# The static UI: index.html, the GHC JS backend output and vendored ECharts.
{ pkgs, uiJs }:
pkgs.runCommand "hydrometer-site" { } ''
  mkdir -p $out/vendor
  cp ${../hydrometer-ui/static}/index.html $out/
  cp ${../hydrometer-ui/static}/style.css $out/
  cp ${../hydrometer-ui/static}/charts.js $out/
  cp ${../hydrometer-ui/static/vendor}/echarts.min.js $out/vendor/
  # The JS backend links one self-contained script with a node hashbang.
  sed '1{/^#!/d}' ${uiJs}/bin/hydrometer-ui > $out/hydrometer-ui.js
  # Version the asset URLs: Cloudflare (and browsers) cache .js and .css for
  # hours regardless of the server's Cache-Control, so a deploy must change
  # their URLs.
  v=$(cat $out/style.css $out/charts.js $out/hydrometer-ui.js $out/vendor/echarts.min.js | sha256sum | cut -c1-12)
  chmod u+w $out/index.html
  sed -i -E "s/(href|src)=\"(style\.css|charts\.js|hydrometer-ui\.js|vendor\/echarts\.min\.js)\"/\1=\"\2?v=$v\"/" $out/index.html
''
