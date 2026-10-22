# NixOS module: the hydrometer server (collector, API, UI) and, optionally,
# an agent for the host itself.  Meant for the hydra server: the data lives
# in a `hydrometer` schema inside hydra's database.
self:
{ config, lib, pkgs, ... }:
let
  inherit (lib) mkEnableOption mkIf mkMerge mkOption types;
  cfg = config.services.hydrometer;
  packages = self.packages.${pkgs.stdenv.hostPlatform.system};
  configFile = pkgs.writeText "hydrometer.json" (builtins.toJSON {
    db = "host=/run/postgresql dbname=${cfg.database} user=hydrometer";
    readOnlyDb = "host=/run/postgresql dbname=${cfg.database} user=hydrometer_ro";
    listen = cfg.listen;
    static = "${packages.hydrometer-site}";
    queueRunner = cfg.queueRunner;
    agents = cfg.agents
      ++ lib.optional cfg.agent.enable { name = cfg.agent.name; url = "http://${cfg.agent.listen}"; };
    retentionDays = cfg.retentionDays;
  });
  # `hydrometer` on PATH with the config baked in, for `hydrometer mcp` and
  # `hydrometer query` (both use the read-only role).
  cli = pkgs.writeShellScriptBin "hydrometer" ''
    cmd="''${1:-}"; shift || true
    exec ${packages.hydrometer}/bin/hydrometer "$cmd" --config ${configFile} "$@"
  '';
  hydraTables = [ "builds" "buildsteps" "buildstepoutputs" "jobsets" "jobsetevals" "jobsetevalmembers" "projects" ];
  # Where database setup SQL runs, and how it reaches psql (see below).
  separateSetup = lib.versionAtLeast config.system.nixos.release "26.05";
  setupUnit = if separateSetup then "postgresql-setup" else "postgresql";
  psql = if separateSetup then "psql" else "$PSQL";
in {
  options.services.hydrometer = {
    enable = mkEnableOption "the hydrometer server";
    database = mkOption { type = types.str; default = "hydra"; description = "Hydra's database; hydrometer adds a schema to it."; };
    listen = mkOption { type = types.str; default = "127.0.0.1:9185"; };
    queueRunner = mkOption { type = types.nullOr types.str; default = "http://[::1]:8080"; description = "The queue runner's REST API."; };
    retentionDays = mkOption { type = types.int; default = 14; description = "Days of raw samples to keep (rollups are kept a year)."; };
    agents = mkOption {
      description = "Agents to pull from. name must match hydra's machine name.";
      default = [ ];
      type = types.listOf (types.submodule {
        options.name = mkOption { type = types.str; };
        options.url = mkOption { type = types.str; example = "http://aarch64-darwin-6.lan:9184"; };
      });
    };
    nginx = {
      enable = mkEnableOption "serving the UI and API from an nginx virtual host";
      virtualHost = mkOption { type = types.str; example = "ci.zw3rk.com"; };
      location = mkOption { type = types.str; default = "/hydrometer/"; };
    };
    agent = {
      enable = mkEnableOption "an agent on this host";
      name = mkOption { type = types.str; default = config.networking.hostName; description = "This machine's name as hydra knows it."; };
      listen = mkOption { type = types.str; default = "127.0.0.1:9184"; };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      users.users.hydrometer = { isSystemUser = true; group = "hydrometer"; };
      users.groups.hydrometer = { };
      environment.systemPackages = [ cli ];

      services.postgresql.ensureUsers = [ { name = "hydrometer"; } { name = "hydrometer_ro"; } ];
      # Any local user may connect as the read-only role (for `hydrometer
      # mcp`); only the service user as the owner.
      services.postgresql.identMap = lib.mkAfter ''
        hydrometer hydrometer hydrometer
        hydrometer hydrometer hydrometer_ro
        hydrometer /^(.*)$ hydrometer_ro
      '';
      services.postgresql.authentication = lib.mkBefore ''
        local ${cfg.database} hydrometer,hydrometer_ro peer map=hydrometer
      '';
      # NixOS 26.05 runs ensureUsers in postgresql-setup and no longer sets
      # $PSQL in postgresql.service; a failing ExecStartPost there would stop
      # the database server.  Earlier releases only have postgresql.service.
      systemd.services.${setupUnit}.postStart = lib.mkAfter ''
        ${psql} -d ${cfg.database} -tAc 'CREATE SCHEMA IF NOT EXISTS hydrometer AUTHORIZATION hydrometer'
        ${psql} -d ${cfg.database} -tAc 'GRANT SELECT ON ${lib.concatStringsSep ", " hydraTables} TO hydrometer, hydrometer_ro'
        ${psql} -d ${cfg.database} -tAc 'GRANT USAGE ON SCHEMA hydrometer TO hydrometer_ro'
        ${psql} -d ${cfg.database} -tAc 'ALTER DEFAULT PRIVILEGES FOR ROLE hydrometer IN SCHEMA hydrometer GRANT SELECT ON TABLES TO hydrometer_ro'
        ${psql} -d ${cfg.database} -tAc 'GRANT SELECT ON ALL TABLES IN SCHEMA hydrometer TO hydrometer_ro'
      '';

      systemd.services.hydrometer = {
        description = "hydrometer collector and API";
        wantedBy = [ "multi-user.target" ];
        # After the setup unit: that is where the roles and schema are made.
        after = [ "${setupUnit}.service" "network-online.target" ];
        requires = [ "${setupUnit}.service" ];
        wants = [ "network-online.target" ];
        serviceConfig = {
          User = "hydrometer";
          ExecStart = "${packages.hydrometer}/bin/hydrometer serve --config ${configFile}";
          Restart = "always";
          RestartSec = 10;
          # The collector is light; never let it compete with hydra.
          Nice = 10;
          MemoryMax = "1G";
        };
      };

      services.nginx.virtualHosts = mkIf cfg.nginx.enable {
        ${cfg.nginx.virtualHost}.locations.${cfg.nginx.location} = {
          proxyPass = "http://${cfg.listen}/";
        };
      };
    })
    (mkIf cfg.agent.enable {
      systemd.services.hydrometer-agent = {
        description = "hydrometer agent";
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
          # Root: per-process accounting of build users needs it.
          ExecStart = "${packages.hydrometer-agent}/bin/hydrometer-agent --listen ${cfg.agent.listen} --name ${cfg.agent.name}";
          Restart = "always";
          RestartSec = 10;
          Nice = 5;
        };
      };
    })
  ];
}
