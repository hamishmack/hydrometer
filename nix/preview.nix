# `nix run .#preview -- [API_BASE] [PORT]`: serve this revision's UI on
# http://127.0.0.1:PORT/ against a running hydrometer's API, without deploying
# it.  The API allows any origin, so the page can fetch from the real server.
# API_BASE is the URL the deployed page lives at (it must end in /), default
# http://127.0.0.1:9185/ (an ssh tunnel to the server).
{ pkgs, site }:
pkgs.writeShellApplication {
  name = "hydrometer-preview";
  runtimeInputs = [ pkgs.python3 pkgs.coreutils pkgs.gnused ];
  text = ''
    api="''${1:-http://127.0.0.1:9185/}"
    port="''${2:-8000}"
    case "$api" in */) ;; *) api="$api/" ;; esac
    dir=$(mktemp -d)
    trap 'rm -rf "$dir"' EXIT
    cp -r ${site}/. "$dir"
    chmod -R u+w "$dir"
    # The UI prefixes API paths with window.hydrometerApi when it is set.
    sed -i "s|<script src=\"hydrometer-ui.js[^\"]*\">|<script>window.hydrometerApi = '$api';</script>\n  &|" "$dir/index.html"
    echo "hydrometer UI from ${site}"
    echo "API: $api"
    echo "open http://127.0.0.1:$port/"
    python3 -m http.server --bind 127.0.0.1 --directory "$dir" "$port"
  '';
}
