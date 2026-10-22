// Thin glue between the Haskell UI and Apache ECharts.  The Haskell side
// passes JSON strings (an API Series or a list of rows) and a small options
// object; everything chart-specific lives here.
(function () {
  "use strict";
  var dark = window.matchMedia && window.matchMedia("(prefers-color-scheme: dark)").matches;

  function chart(el) {
    var c = echarts.getInstanceByDom(el);
    if (!c) {
      c = echarts.init(el, dark ? "dark" : null, { renderer: "canvas" });
      if (window.ResizeObserver) new ResizeObserver(function () { c.resize(); }).observe(el);
    }
    return c;
  }

  function fmtBytes(v) {
    if (v == null) return "–";
    var u = ["B", "KiB", "MiB", "GiB", "TiB"], i = 0;
    while (Math.abs(v) >= 1024 && i < u.length - 1) { v /= 1024; i++; }
    return (i ? v.toFixed(1) : Math.round(v)) + " " + u[i];
  }
  function fmt(unit) {
    return function (v) {
      if (v == null) return "–";
      switch (unit) {
        case "pct": return (v * 100).toFixed(0) + "%";
        case "bytes": return fmtBytes(v);
        case "rate": return fmtBytes(v) + "/s";
        case "cores": return v.toFixed(1);
        default: return (Math.round(v * 100) / 100).toString();
      }
    };
  }

  // Line/area chart of an API Series: {time: [ms], series: {name: [v]}}.
  // opts: {cols: [name], labels: {name: label}, unit, stack, area, yMax, axes: {name: 1}}
  function line(el, json, opts) {
    var s = JSON.parse(json), o = JSON.parse(opts || "{}");
    var cols = o.cols || Object.keys(s.series || {});
    var series = cols.filter(function (k) { return s.series && s.series[k]; }).map(function (k) {
      return {
        name: (o.labels && o.labels[k]) || k, type: "line", showSymbol: false, connectNulls: false,
        stack: o.stack ? "s" : undefined, areaStyle: o.area || o.stack ? { opacity: 0.35 } : undefined,
        yAxisIndex: (o.axes && o.axes[k]) || 0,
        step: o.step ? "start" : undefined,
        data: s.time.map(function (t, i) { return [t, s.series[k][i]]; })
      };
    });
    var axis = function (unit, max) {
      return { type: "value", max: max, axisLabel: { formatter: fmt(unit) }, splitLine: { lineStyle: { opacity: 0.3 } } };
    };
    chart(el).setOption({
      backgroundColor: "transparent", animation: false,
      grid: { left: 60, right: o.unit2 !== undefined ? 60 : 20, top: 30, bottom: 30 },
      legend: { top: 4, type: "scroll" },
      tooltip: { trigger: "axis", valueFormatter: fmt(o.unit) },
      xAxis: { type: "time" },
      yAxis: o.unit2 !== undefined ? [axis(o.unit, o.yMax), axis(o.unit2)] : axis(o.unit, o.yMax),
      series: series
    }, true);
  }

  function spark(el, json) {
    var v = JSON.parse(json);
    chart(el).setOption({
      backgroundColor: "transparent", animation: false,
      grid: { left: 0, right: 0, top: 2, bottom: 2 },
      xAxis: { type: "category", show: false, data: v.map(function (_, i) { return i; }) },
      yAxis: { type: "value", show: false, min: 0, max: 1 },
      series: [{ type: "line", data: v, showSymbol: false, areaStyle: { opacity: 0.25 }, lineStyle: { width: 1 } }]
    }, true);
  }

  // Horizontal bars: rows = [[label, value], ...]
  function bars(el, json, opts) {
    var rows = JSON.parse(json).slice().reverse(), o = JSON.parse(opts || "{}");
    chart(el).setOption({
      backgroundColor: "transparent", animation: false,
      grid: { left: 10, right: 40, top: 10, bottom: 20, containLabel: true },
      tooltip: { trigger: "axis", axisPointer: { type: "shadow" }, valueFormatter: fmt(o.unit) },
      xAxis: { type: "value", axisLabel: { formatter: fmt(o.unit) } },
      yAxis: { type: "category", data: rows.map(function (r) { return r[0] == null ? "(none)" : String(r[0]); }),
               axisLabel: { width: 260, overflow: "truncate" } },
      series: [{ type: "bar", data: rows.map(function (r) { return r[1]; }), name: o.label || "" }]
    }, true);
  }

  // Gantt chart of steps: rows = [{start, stop, name, status, busy, href}].
  // Lanes are assigned greedily so concurrent steps stack.
  function gantt(el, json) {
    var rows = JSON.parse(json).slice().sort(function (a, b) { return a.start - b.start; });
    var laneEnds = [];
    rows.forEach(function (r) {
      var lane = laneEnds.findIndex(function (e) { return e <= r.start; });
      if (lane < 0) { lane = laneEnds.length; laneEnds.push(0); }
      laneEnds[lane] = r.stop; r.lane = lane;
    });
    var colour = function (r) {
      if (r.stopped == null || r.busy) return "#2f6fde";
      return r.status === 0 ? "#2e9e5b" : "#d4453a";
    };
    var c = chart(el);
    c.setOption({
      backgroundColor: "transparent", animation: false,
      grid: { left: 40, right: 20, top: 10, bottom: 30 },
      tooltip: { formatter: function (p) {
        var r = rows[p.dataIndex];
        return "<b>" + r.name + "</b><br>" + (r.job || "") + "<br>" +
          Math.round((r.stop - r.start) / 1000) + " s" + (r.cpu != null ? ", " + r.cpu.toFixed(0) + " CPU s" : "") +
          (r.rss != null ? ", " + fmtBytes(r.rss) : "");
      } },
      xAxis: { type: "time" },
      yAxis: { type: "value", min: -0.5, max: Math.max(laneEnds.length - 0.5, 0.5), interval: 1, axisLabel: { formatter: function (v) { return "#" + (v + 1); } } },
      series: [{
        type: "custom",
        renderItem: function (params, api) {
          var start = api.coord([api.value(0), api.value(2)]), end = api.coord([api.value(1), api.value(2)]);
          var h = api.size([0, 1])[1] * 0.7;
          return { type: "rect", shape: { x: start[0], y: start[1] - h / 2, width: Math.max(end[0] - start[0], 1), height: h },
                   style: { fill: api.visual("color") } };
        },
        encode: { x: [0, 1], y: 2 },
        data: rows.map(function (r) { return { value: [r.start, r.stop, r.lane], itemStyle: { color: colour(r) } }; })
      }]
    }, true);
    c.off("click");
    c.on("click", function (p) { var r = rows[p.dataIndex]; if (r && r.href) location.hash = r.href; });
  }

  var fns = { line: line, spark: spark, bars: bars, gantt: gantt };
  // Called from Haskell.  Deferred a frame: a freshly built element may not
  // be attached (or sized) yet.
  function draw(fn, el, json, opts) {
    requestAnimationFrame(function () {
      try { fns[fn](el, json, opts); } catch (e) { console.error("hmCharts." + fn, e); }
    });
  }
  window.hmCharts = { draw: draw, line: line, spark: spark, bars: bars, gantt: gantt, fmtBytes: fmtBytes };
})();
