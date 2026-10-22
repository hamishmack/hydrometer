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
''
