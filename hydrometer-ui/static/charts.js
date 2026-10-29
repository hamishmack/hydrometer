// Thin glue between the Haskell UI and Apache ECharts.  The Haskell side
// passes JSON strings (an API Series or a list of rows) and a small options
// object; everything chart-specific lives here.
(function () {
  "use strict";
  var dark = window.matchMedia && window.matchMedia("(prefers-color-scheme: dark)").matches;

  // Live charts.  Pages rebuild their DOM on refresh (the Now page every
  // 5 s), so instances whose element has left the document are disposed.
  var live = [];
  function sweep() {
    live = live.filter(function (x) {
      if (document.contains(x.el)) return true;
      if (x.ro) x.ro.disconnect();
      x.c.dispose();
      return false;
    });
  }

  function chart(el) {
    var c = echarts.getInstanceByDom(el);
    if (!c) {
      sweep();
      c = echarts.init(el, dark ? "dark" : null, { renderer: "canvas" });
      var ro = window.ResizeObserver ? new ResizeObserver(function () { c.resize(); }) : null;
      if (ro) ro.observe(el);
      live.push({ el: el, c: c, ro: ro });
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
        // Only first-axis series stack: a second-axis line is a different unit.
        stack: o.stack && !(o.axes && o.axes[k]) ? "s" : undefined,
        areaStyle: o.area || (o.stack && !(o.axes && o.axes[k])) ? { opacity: 0.35 } : undefined,
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

  function cssVar(name) {
    return getComputedStyle(document.documentElement).getPropertyValue(name).trim();
  }

  // Half-circle pressure gauge of a 0..1 value, banded like the meter bars
  // (warn above 75%, bad above 90%).
  function gauge(el, json) {
    var v = JSON.parse(json);
    chart(el).setOption({
      backgroundColor: "transparent", animation: false,
      series: [{
        type: "gauge", startAngle: 180, endAngle: 0, min: 0, max: 1,
        center: ["50%", "88%"], radius: "160%",
        axisLine: { lineStyle: { width: 10, color: [[0.75, cssVar("--ok")], [0.9, cssVar("--warn")], [1, cssVar("--bad")]] } },
        pointer: { length: "72%", width: 4, itemStyle: { color: cssVar("--fg") } },
        anchor: { show: true, size: 8, itemStyle: { color: cssVar("--fg") } },
        axisTick: { show: false }, splitLine: { show: false }, axisLabel: { show: false },
        title: { show: false }, detail: { show: false },
        data: [{ value: v == null ? 0 : Math.max(0, Math.min(1, v)) }]
      }]
    }, true);
  }

  // Horizontal bars: rows = [[label, value], ...].  The chart grows with the
  // row count so every category gets a readable label.
  function bars(el, json, opts) {
    var rows = JSON.parse(json).slice().reverse(), o = JSON.parse(opts || "{}");
    el.style.height = Math.max(200, rows.length * 22 + 40) + "px";
    chart(el).setOption({
      backgroundColor: "transparent", animation: false,
      grid: { left: 10, right: 70, top: 10, bottom: 20, containLabel: true },
      tooltip: { trigger: "axis", axisPointer: { type: "shadow" }, valueFormatter: fmt(o.unit) },
      xAxis: { type: "value", axisLabel: { formatter: fmt(o.unit) } },
      yAxis: { type: "category", data: rows.map(function (r) { return r[0] == null ? "(none)" : String(r[0]); }),
               axisLabel: { width: 260, overflow: "truncate", interval: 0 } },
      series: [{ type: "bar", data: rows.map(function (r) { return r[1]; }), name: o.label || "",
                 label: { show: true, position: "right", fontSize: 11, formatter: function (p) { return fmt(o.unit)(p.value); } } }]
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

  var statusNames = { 0: "succeeded", 1: "failed", 2: "dependency failed", 3: "aborted", 4: "cancelled",
    6: "failed with output", 7: "timed out", 8: "cached failure", 9: "unsupported system",
    10: "log limit exceeded", 11: "output size limit exceeded", 12: "not deterministic", 13: "resolved", 100: "busy" };
  function fmtSecs(s) {
    s = Math.round(s);
    if (s < 60) return s + "s";
    if (s < 3600) return Math.floor(s / 60) + "m" + String(s % 60).padStart(2, "0") + "s";
    return Math.floor(s / 3600) + "h" + String(Math.floor(s / 60) % 60).padStart(2, "0") + "m";
  }

  // An evaluation's steps on a time line, one row each in start order, with a
  // line from each dependency's end to the start of the step that waited for
  // it.  The critical path (from the last step to finish, back through each
  // step's last-finishing dependency) is outlined.
  function depgraph(el, json) {
    var g = JSON.parse(json), now = Date.now();
    // Substituted steps' sources and sizes (api/eval/ID/substitutions), by .drv.
    var subst = {};
    if (g.graph) {
      var t = g.subst;
      if (t && t.columns) t.rows.forEach(function (r) {
        var o = {}; t.columns.forEach(function (c, i) { o[c] = r[i]; });
        if (!subst[o.drv]) subst[o.drv] = o;
      });
      g = g.graph;
    }
    var nodes = g.nodes.map(function (n, i) { n.i = i; n.end = n.stop == null ? now : n.stop; n.deps = []; return n; });
    g.edges.forEach(function (e) { nodes[e[1]].deps.push(e[0]); });
    var order = nodes.slice().sort(function (a, b) { return a.start - b.start || a.end - b.end; });
    order.forEach(function (n, row) { n.row = row; });
    var crit = {}, critEdge = {};
    var last = nodes.reduce(function (m, n) { return m == null || n.end > m.end ? n : m; }, null);
    for (var n = last; n; ) {
      crit[n.i] = true;
      var d = n.deps.map(function (j) { return nodes[j]; }).reduce(function (m, x) { return m == null || x.end > m.end ? x : m; }, null);
      if (d) critEdge[d.i + ">" + n.i] = true;
      n = d;
    }
    el.style.height = Math.max(200, order.length * 16 + 50) + "px";
    var colour = function (n) {
      if (n.stop == null || n.busy) return "#2f6fde";
      if (n.kind === "substituted") return dark ? "#5a6170" : "#b4bac6";
      return n.status === 0 ? "#2e9e5b" : "#d4453a";
    };
    var critColour = dark ? "#e3e6ec" : "#1d2330";
    var retryFill = dark ? "rgba(212,138,26,0.18)" : "rgba(212,138,26,0.22)", retryStroke = "#d48a1a";
    // A node's attempts, each ending at its stop, or (lost to a queue-runner
    // restart, so no stop) at the next attempt's start.
    var attempts = function (n) {
      var as = (n.attempts && n.attempts.length) ? n.attempts : [{ start: n.start, stop: n.stop, status: n.status, busy: n.busy, machine: n.machine }];
      return as.map(function (a, k) {
        var next = as[k + 1];
        var lost = a.stop == null && !a.busy;
        return { start: a.start, end: a.stop != null ? a.stop : (lost && next ? next.start : now), status: a.status, busy: a.busy, lost: lost, machine: a.machine };
      });
    };
    var attemptColour = function (n, a) {
      if (a.busy) return "#2f6fde";
      if (a.lost) return "#8e5bd0";
      if (a.status === 0) return n.kind === "substituted" ? (dark ? "#5a6170" : "#b4bac6") : "#2e9e5b";
      if (a.status === 3 || a.status === 4) return "#d48a1a";
      return "#d4453a";
    };
    var lineColour = dark ? "rgba(200,205,215,0.25)" : "rgba(60,70,90,0.22)";
    var lines = [];
    nodes.forEach(function (n) {
      n.deps.forEach(function (j) {
        var d = nodes[j], c = critEdge[j + ">" + n.i];
        lines.push({ coords: [[d.end, d.row], [n.start, n.row]], lineStyle: { color: c ? critColour : lineColour, width: c ? 2 : 1 } });
      });
    });
    var c = chart(el);
    c.resize();
    c.setOption({
      backgroundColor: "transparent", animation: false,
      grid: { left: 190, right: 20, top: 10, bottom: 30 },
      tooltip: { formatter: function (p) {
        if (p.seriesIndex !== 0) return "";
        var n = order[p.dataIndex];
        var ready = n.deps.reduce(function (m, j) { return Math.max(m, nodes[j].end); }, 0);
        return "<b>" + n.name + "</b><br>" + n.kind + (n.machine ? " on " + n.machine : "") + ", " +
          (n.stop == null ? "running" : (statusNames[n.status] || "status " + n.status)) + "<br>" +
          fmtSecs((n.end - n.start) / 1000) +
          (subst[n.drv] ? "<br>from " + (subst[n.drv].source || "?") + ", downloaded " + fmtBytes(subst[n.drv].downloaded_bytes) +
            " (" + subst[n.drv].downloaded_paths + " paths), closure " + fmtBytes(subst[n.drv].closure_bytes) : "") +
          (ready ? (n.start > ready ? ", started " + fmtSecs((n.start - ready) / 1000) + " after its inputs were ready" : "") : "") +
          (function () {
            var atts = attempts(n);
            if (atts.length < 2) return "";
            return atts.map(function (a, k) {
              var what = a.busy ? "running" : a.lost ? "lost (queue runner restarted)" : (statusNames[a.status] || "status " + a.status);
              var gap = atts[k + 1] && atts[k + 1].start > a.end ? ", retried " + fmtSecs((atts[k + 1].start - a.end) / 1000) + " later" : "";
              return "<br>attempt " + (k + 1) + ": " + what + (a.machine ? " on " + a.machine : "") + ", " + fmtSecs((a.end - a.start) / 1000) + gap;
            }).join("");
          })() +
          "<br>" + n.deps.length + " dependencies shown" + (crit[n.i] ? "<br><b>on the critical path</b>" : "");
      } },
      xAxis: { type: "time" },
      yAxis: { type: "category", inverse: true, data: order.map(function (n) { return n.name; }),
               axisLabel: { fontSize: 10, width: 180, overflow: "truncate", interval: 0 } },
      series: [{
        type: "custom",
        // One bar per attempt, and a hatched gap where a failed or lost
        // attempt waited to be retried.
        renderItem: function (params, api) {
          var n = order[params.dataIndex], y = api.value(2);
          var h = api.size([0, 1])[1] * 0.7, kids = [];
          var rect = function (a, b, style) {
            var s = api.coord([a, y]), e = api.coord([b, y]);
            kids.push({ type: "rect", shape: { x: s[0], y: s[1] - h / 2, width: Math.max(e[0] - s[0], 2), height: h }, style: style });
          };
          var atts = attempts(n);
          atts.forEach(function (a, k) {
            rect(a.start, a.end, { fill: attemptColour(n, a) });
            var next = atts[k + 1];
            if (next && next.start > a.end) rect(a.end, next.start, { fill: retryFill, stroke: retryStroke, lineWidth: 1, lineDash: [3, 2] });
          });
          if (crit[n.i]) rect(n.start, n.end, { fill: "none", stroke: critColour, lineWidth: 2 });
          return { type: "group", children: kids };
        },
        encode: { x: [0, 1], y: 2 },
        data: order.map(function (n) { return { value: [n.start, n.end, n.row], itemStyle: { color: colour(n) } }; }),
        z: 2
      }, {
        type: "lines", coordinateSystem: "cartesian2d", silent: true, z: 1,
        data: lines.length > 4000 ? lines.filter(function (l) { return l.lineStyle.width > 1; }) : lines
      }]
    }, true);
    c.off("click");
    c.on("click", function (p) { var n = order[p.dataIndex]; if (p.seriesIndex === 0 && n) location.hash = "#/build/" + n.build; });
  }

  var fns = { line: line, spark: spark, gauge: gauge, bars: bars, gantt: gantt, depgraph: depgraph };
  // Called from Haskell.  Deferred a frame: a freshly built element may not
  // be attached (or sized) yet.
  function draw(fn, el, json, opts, tries) {
    requestAnimationFrame(function () {
      // Not attached yet: try again for a few frames, then give up (it was
      // replaced by a newer build of the page).
      if (!document.contains(el)) { if ((tries || 0) < 10) draw(fn, el, json, opts, (tries || 0) + 1); return; }
      try { fns[fn](el, json, opts); } catch (e) { console.error("hmCharts." + fn, e); }
    });
  }
  window.hmCharts = { draw: draw, line: line, spark: spark, gauge: gauge, bars: bars, gantt: gantt, depgraph: depgraph, fmtBytes: fmtBytes };
})();
