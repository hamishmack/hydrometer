# nix-darwin module: the hydrometer agent as a launchd daemon.  It runs as
# root (per-process accounting of the build users needs it) and serves its
# sample buffer on the LAN for the server to pull.
self:
{ config, lib, pkgs, ... }:
let
  inherit (lib) mkEnableOption mkIf mkOption types;
  cfg = config.services.hydrometer-agent;
  agent = self.packages.${pkgs.stdenv.hostPlatform.system}.hydrometer-agent;
in {
  options.services.hydrometer-agent = {
    enable = mkEnableOption "the hydrometer agent";
    name = mkOption { type = types.str; description = "This machine's name as hydra knows it (e.g. aarch64-darwin-6)."; };
    listen = mkOption { type = types.str; default = "*:9184"; };
    interval = mkOption { type = types.int; default = 5; description = "Seconds between samples."; };
  };
  config = mkIf cfg.enable {
    launchd.daemons.hydrometer-agent.serviceConfig = {
      ProgramArguments = [ "${agent}/bin/hydrometer-agent" "--listen" cfg.listen "--name" cfg.name "--interval" (toString cfg.interval) ];
      KeepAlive = true;
      RunAtLoad = true;
      ProcessType = "Background";
      StandardErrorPath = "/var/log/hydrometer-agent.log";
      StandardOutPath = "/var/log/hydrometer-agent.log";
    };
  };
}
