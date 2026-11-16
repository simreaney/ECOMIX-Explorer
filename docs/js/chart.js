// Chart component. Draws line/band charts (with Observable Plot), heat
// grids, stacked small multiples and treemaps (with d3), and gives every one
// the same hover layer: a crosshair that snaps to the nearest x and one
// tooltip listing every series there. Each chart can also flip to a table
// view of the same numbers.

import { EC, FONT, fmtNum, fmtDate } from "./theme.js";
import { el, setChildren, showTip, hideTip, lineKey, swatchKey, dotKey, pagedTable } from "./ui.js";

const Plot = () => window.Plot;
const d3 = () => window.d3;

export class Chart {
  constructor(parent) {
    this.legend = el("div.chart-legend");
    this.plot = el("div.chart-plot");
    this.caption = el("div.chart-caption");
    this.el = el("div.chart", {}, this.legend, this.plot, this.caption);
    parent.append(this.el);
    this.spec = null;
    this.token = 0;
    this.tableMode = false;
    this.tableBtn = null;
    this.ro = new ResizeObserver(() => this.render());
    this.ro.observe(this.plot);
  }

  // Run an async loader; the previous render stays (dimmed) until it lands.
  async load(fn) {
    const t = ++this.token;
    this.el.classList.add("is-stale");
    try {
      const spec = await fn();
      if (t === this.token && spec !== undefined) this._set(spec);
    } catch (err) {
      console.error(err);
      if (t === this.token) this._set({ message: "Could not load this data", sub: String((err && err.message) || err) });
    } finally {
      if (t === this.token) this.el.classList.remove("is-stale");
    }
  }

  draw(spec) { this.token++; this.el.classList.remove("is-stale"); this._set(spec); }

  message(text, sub) { this.draw({ message: text, sub }); }

  // A pill that flips this chart between plot and table view.
  tableToggle() {
    this.tableBtn = el("button.pill", { type: "button", "aria-pressed": "false", text: "Table", title: "Show the numbers behind this chart",
      onclick: () => { this.tableMode = !this.tableMode; this.tableBtn.setAttribute("aria-pressed", String(this.tableMode)); this._drawn = null; this.render(); } });
    return this.tableBtn;
  }

  _set(spec) {
    this.spec = spec;
    this._drawn = null;
    setChildren(this.legend, spec && !spec.message ? legendItems(spec) : []);
    this.caption.textContent = (spec && spec.caption) || "";
    this.render();
  }

  render() {
    const spec = this.spec;
    const w = this.plot.clientWidth, h = this.plot.clientHeight;
    if (!spec || w < 20 || h < 20) return;
    if (this._drawn === spec && w === this._w && h === this._h) return;
    this._drawn = spec; this._w = w; this._h = h;
    hideTip();
    this.plot.replaceChildren();
    if (spec.message) {
      this.plot.append(el("div.chart-msg", {}, el("span", { text: spec.message }), spec.sub ? el("small", { text: spec.sub }) : null));
      return;
    }
    if (this.tableMode) {
      const t = spec.table || autoTable(spec);
      const box = el("div", { style: { position: "absolute", inset: "0", display: "flex", flexDirection: "column" } },
        t ? pagedTable(t.columns, t.rows, { pageSize: 200, compact: true }) : el("div.chart-msg", { text: "No table for this view" }));
      this.plot.append(box);
      return;
    }
    const host = el("div", { style: { position: "absolute", inset: "0" } });
    this.plot.append(host);
    try {
      if (spec.kind === "line") drawLine(host, spec, w, h);
      else if (spec.kind === "stack") drawStack(host, spec, w, h);
      else if (spec.kind === "heat") drawHeat(host, spec, w, h);
      else if (spec.kind === "treemap") drawTreemap(host, spec, w, h);
    } catch (err) {
      console.error(err);
      host.replaceChildren(el("div.chart-msg", { text: "Could not draw this chart" }));
    }
  }
}

// ---- legend ----------------------------------------------------------------

function legendItems(spec) {
  let items = spec.legend;
  if (!items) {
    const series = spec.kind === "line" ? spec.series : [];
    items = series.length >= 2 ? series.filter((s) => s.label && !s.noLegend).map((s) => ({ label: s.label, color: s.color, dash: s.dash, kind: s.line === false ? "dot" : "line" })) : [];
  }
  return items.map((it) => {
    if (it.kind === "sep") return el("span.sep");
    if (it.kind === "gradient") {
      const g = el("span", { style: { display: "inline-block", width: "90px", height: "8px", background: `linear-gradient(90deg, ${it.colors.join(",")})` } });
      return el("span.key", {}, it.from ? el("span", { text: it.from }) : null, g, it.to ? el("span", { text: it.to }) : null);
    }
    const key = it.kind === "rect" ? swatchKey(it.color, it.opacity ?? 1) : it.kind === "dot" ? dotKey(it.color) : lineKey(it.color, it.dash);
    return el("span.key", {}, key, el("span", { text: it.label }));
  });
}

// ---- formatting ------------------------------------------------------------

export function tickFmt(v) {
  if (v == null || !Number.isFinite(v)) return "";
  if (v === 0) return "0";
  const a = Math.abs(v);
  if (a >= 1e6 || a < 1e-3) {
    const e = Math.floor(Math.log10(a));
    const m = v / 10 ** e;
    const ms = Math.abs(Math.abs(m) - 1) < 1e-9 ? (m < 0 ? "−" : "") : `${+m.toPrecision(2)}×`;
    return `${ms}10${superscript(e)}`;
  }
  return (+v.toPrecision(6)).toLocaleString("en-GB", { maximumFractionDigits: 6 }).replace(/^-/, "−");
}

const SUP = { "-": "⁻", 0: "⁰", 1: "¹", 2: "²", 3: "³", 4: "⁴", 5: "⁵", 6: "⁶", 7: "⁷", 8: "⁸", 9: "⁹" };
const superscript = (n) => String(n).split("").map((c) => SUP[c] || c).join("");

// Tick values for a log axis: whole decades when the range spans enough of
// them, otherwise 1-2-5 steps (so the grid stays as sparse as the labels).
function logTicks(domain) {
  const [a, b] = domain;
  const e0 = Math.floor(Math.log10(a)), e1 = Math.ceil(Math.log10(b));
  const pick = (ms) => {
    const out = [];
    for (let e = e0; e <= e1; e++) for (const m of ms) { const v = m * 10 ** e; if (v >= a * (1 - 1e-9) && v <= b * (1 + 1e-9)) out.push(v); }
    return out;
  };
  let t = pick([1]);
  if (t.length < 3) t = pick([1, 3]);
  if (t.length < 3) t = pick([1, 2, 5]);
  if (t.length > 8) { const k = Math.ceil(t.length / 8); t = t.filter((_, i) => i % k === 0); }
  return t;
}

function xFormatter(spec) {
  if (spec.x.format) return spec.x.format;
  if (spec.x.type === "utc") return (v) => fmtDate(v);
  return (v) => tickFmt(v);
}

// ---- line charts -----------------------------------------------------------

function extent(spec) {
  let x0 = Infinity, x1 = -Infinity, y0 = Infinity, y1 = -Infinity;
  const log = spec.y.type === "log";
  for (const s of spec.series) {
    for (let i = 0; i < s.x.length; i++) {
      const xv = s.x[i];
      for (const arr of [s.y, s.lo, s.hi]) {
        if (!arr) continue;
        const v = arr[i];
        if (v == null || !Number.isFinite(v) || (log && v <= 0)) continue;
        if (v < y0) y0 = v; if (v > y1) y1 = v;
        if (xv < x0) x0 = xv; if (xv > x1) x1 = xv;
      }
    }
  }
  for (const r of spec.rules || []) {
    if (Number.isFinite(r.y) && (!log || r.y > 0)) { y0 = Math.min(y0, r.y); y1 = Math.max(y1, r.y); }
  }
  if (spec.y.zero && !log) { y0 = Math.min(0, y0); y1 = Math.max(0, y1); }
  if (!(y1 >= y0)) return null;
  if (y0 === y1) { const d = log ? y0 * 0.5 : Math.abs(y0) * 0.1 || 1; y0 -= log ? d : d; y1 += d; }
  return { x: [x0, x1], y: [y0, y1] };
}

function yTicks(type, domain, h) {
  const n = Math.max(2, Math.round(h / 45));
  if (type === "log") return logTicks(domain).map(tickFmt);
  const scale = d3().scaleLinear().domain(domain).nice(n);
  return scale.ticks(n).map(tickFmt);
}

function textWidth(s, px = 11) { return String(s).length * px * 0.58; }

function drawLine(host, spec, w, h, opts = {}) {
  const ext = extent(spec);
  if (!ext) { host.append(el("div.chart-msg", { text: spec.empty || "No data to show" })); return; }
  const xType = spec.x.type || "linear";
  const yType = spec.y.type || "linear";
  const xDomain = spec.x.domain || ext.x;
  const yDomain = spec.y.domain || ext.y;
  const asX = xType === "utc" ? (v) => new Date(v) : (v) => v;

  const titleH = opts.title ? 18 : 0;
  const mt = (spec.y.label ? 20 : 10) + titleH;
  const mb = opts.hideXAxis ? 8 : spec.x.label ? 34 : xType === "utc" ? 34 : 22;
  const ticks = yTicks(yType, yDomain, h - mt - mb);
  const ml = Math.max(36, Math.min(90, Math.max(...ticks.map((t) => textWidth(t))) + 14));
  let mr = 14;
  const direct = spec.directLabels ? spec.series.filter((s) => s.label && s.y) : [];
  if (direct.length) mr = Math.min(160, Math.max(...direct.map((s) => textWidth(s.directLabel || s.label))) + 18);

  const marks = [];
  const P = Plot();
  for (const s of spec.series) {
    const idx = d3().range(s.x.length);
    const clean = (arr) => (i) => {
      const v = arr[i];
      return v == null || !Number.isFinite(v) || (yType === "log" && v <= 0) ? NaN : v;
    };
    if (s.lo && s.hi) {
      marks.push(P.areaY(idx, { x: (i) => asX(s.x[i]), y1: clean(s.lo), y2: clean(s.hi), fill: s.bandColor || s.color, fillOpacity: s.bandOpacity ?? 0.16, curve: s.curve || "linear" }));
    }
  }
  if (spec.zeroRule && yType !== "log") marks.push(P.ruleY([0], { stroke: EC.ink, strokeWidth: 1 }));
  for (const r of spec.rules || []) {
    if (!Number.isFinite(r.y) || (yType === "log" && r.y <= 0)) continue;
    marks.push(P.ruleY([r.y], { stroke: r.color || EC.ink, strokeWidth: 1.25, strokeDasharray: r.dash || null }));
  }
  for (const s of spec.series) {
    if (!s.y) continue;
    const idx = d3().range(s.x.length);
    const yv = (i) => {
      const v = s.y[i];
      return v == null || !Number.isFinite(v) || (yType === "log" && v <= 0) ? NaN : v;
    };
    if (s.line !== false) {
      marks.push(P.line(idx, { x: (i) => asX(s.x[i]), y: yv, stroke: s.color, strokeWidth: s.width || 2, strokeDasharray: s.dash || null, curve: s.curve || "linear" }));
    }
    if (s.points) {
      marks.push(P.dot(idx.filter((i) => Number.isFinite(yv(i))), {
        x: (i) => asX(s.x[i]), y: yv, r: s.r || 4,
        fill: s.pointColors ? (i) => s.pointColors[i] : s.color, stroke: EC.ground, strokeWidth: 1,
      }));
    }
  }
  if (spec.marker != null && Number.isFinite(spec.marker)) {
    marks.push(P.ruleX([asX(spec.marker)], { stroke: EC.accentText, strokeWidth: 1.25 }));
  }

  const svg = P.plot({
    width: w, height: h,
    marginTop: mt, marginRight: mr, marginBottom: mb, marginLeft: ml,
    style: { fontFamily: FONT, fontSize: "11px", color: EC.inkDim, background: "transparent", overflow: "visible" },
    x: {
      type: xType, domain: xType === "utc" ? xDomain.map((v) => new Date(v)) : xDomain,
      label: opts.hideXAxis ? null : spec.x.label || null, labelArrow: "none", labelAnchor: "right", labelOffset: 30,
      tickSize: 0, ticks: spec.x.ticks, tickFormat: spec.x.tickFormat, axis: opts.hideXAxis ? null : "bottom",
    },
    y: {
      type: yType, domain: yDomain, nice: yType !== "log" && !spec.y.domain,
      label: spec.y.label || null, labelArrow: "none", labelAnchor: "top",
      grid: true, tickSize: 0, tickFormat: tickFmt,
      ticks: yType === "log" ? logTicks(yDomain) : Math.max(2, Math.round((h - mt - mb) / 45)),
    },
    marks,
  });
  svg.style.overflow = "visible";
  host.append(svg);
  const xs = svg.scale("x"), ys = svg.scale("y");
  const sel = d3().select(svg);
  // one strong baseline
  sel.append("line").attr("x1", ml).attr("x2", w - mr).attr("y1", h - mb + 0.5).attr("y2", h - mb + 0.5)
    .attr("stroke", EC.ink).attr("stroke-width", 1.5);
  if (opts.title) {
    // Plot sets text-anchor: middle on its root, so anchor explicitly.
    sel.append("text").attr("x", 0).attr("y", 12).attr("text-anchor", "start").attr("fill", EC.ink).attr("font-weight", 700).attr("font-size", 12).text(opts.title);
  }

  // Direct labels at the end of each line, nudged apart so they never overlap.
  if (direct.length) {
    const labels = [];
    for (const s of direct) {
      for (let i = s.y.length - 1; i >= 0; i--) {
        const v = s.y[i];
        if (v != null && Number.isFinite(v) && (yType !== "log" || v > 0)) {
          labels.push({ text: s.directLabel || s.label, y: ys.apply(v), x: xs.apply(asX(s.x[i])), color: s.color });
          break;
        }
      }
    }
    labels.sort((a, b) => a.y - b.y);
    for (let i = 1; i < labels.length; i++) labels[i].y = Math.max(labels[i].y, labels[i - 1].y + 12);
    for (const l of labels) {
      sel.append("text").attr("x", w - mr + 6).attr("y", l.y).attr("dy", "0.32em").attr("text-anchor", "start").attr("fill", EC.inkMuted).attr("font-size", 11).text(l.text);
    }
  }

  attachHover(host, svg, spec, { xs, ml, mr, mt, mb, w, h, asX });
}

function bisect(arr, v) {
  let lo = 0, hi = arr.length - 1;
  if (hi < 0) return -1;
  while (lo < hi) {
    const mid = (lo + hi) >> 1;
    if (arr[mid] < v) lo = mid + 1; else hi = mid;
  }
  if (lo > 0 && Math.abs(arr[lo - 1] - v) <= Math.abs(arr[lo] - v)) lo--;
  return lo;
}

function attachHover(host, svg, spec, g) {
  const cross = el("div.cross", { hidden: true });
  host.append(cross);
  const fmtX = xFormatter(spec);
  const fmtY = spec.y.format || ((v) => fmtNum(v, 3));
  const xNum = (v) => (v instanceof Date ? v.getTime() : v);
  const leave = () => { cross.hidden = true; hideTip(); if (spec.onLeave) spec.onLeave(); };

  host.addEventListener("pointermove", (e) => {
    const rect = svg.getBoundingClientRect();
    const px = e.clientX - rect.left, py = e.clientY - rect.top;
    if (px < g.ml - 4 || px > g.w - g.mr + 4 || py < g.mt - 4 || py > g.h - g.mb + 4) { leave(); return; }
    const target = xNum(g.xs.invert(px));
    let snap = null, best = Infinity;
    for (const s of spec.series) {
      const i = bisect(s.x, target);
      if (i < 0) continue;
      const d = Math.abs(s.x[i] - target);
      if (d < best) { best = d; snap = s.x[i]; }
    }
    if (snap == null) { leave(); return; }
    const rows = [];
    for (const s of spec.series) {
      if (s.noTip) continue;
      const i = bisect(s.x, snap);
      if (i < 0 || s.x[i] !== snap) continue;
      const v = s.y ? s.y[i] : null;
      const band = s.lo && s.hi && Number.isFinite(s.lo[i]) && Number.isFinite(s.hi[i]) ? `${fmtY(s.lo[i])} – ${fmtY(s.hi[i])}` : null;
      if ((v == null || !Number.isFinite(v)) && !band) continue;
      const label = s.tipLabel ? s.tipLabel(i) : s.label || "";
      rows.push(el("div.tt-row", {},
        s.y && s.line !== false ? lineKey(s.color, s.dash) : s.points ? dotKey(s.pointColors ? s.pointColors[i] : s.color) : swatchKey(s.color, 0.3),
        el("b", { text: v != null && Number.isFinite(v) ? (s.tipValue ? s.tipValue(i) : fmtY(v)) : band }),
        el("span", { text: v != null && Number.isFinite(v) && band ? `${label} · range ${band}` : label })));
    }
    for (const r of spec.rules || []) {
      if (r.label && Number.isFinite(r.y)) rows.push(el("div.tt-row", {}, lineKey(r.color || EC.ink, r.dash), el("b", { text: fmtY(r.y) }), el("span", { text: r.label })));
    }
    cross.hidden = false;
    cross.style.left = `${g.xs.apply(g.asX(snap))}px`;
    cross.style.top = `${g.mt}px`;
    cross.style.height = `${g.h - g.mt - g.mb}px`;
    showTip(e.clientX, e.clientY, [el("div.tt-title", { text: fmtX(snap) }), rows.length ? rows : el("div.tt-row", { text: "No value" })]);
    if (spec.onHover) spec.onHover(snap);
  });
  host.addEventListener("pointerleave", leave);
}

// ---- stacked small multiples ----------------------------------------------

function drawStack(host, spec, w, h) {
  const n = spec.panels.length;
  if (!n) { host.append(el("div.chart-msg", { text: spec.empty || "No data to show" })); return; }
  const axisH = spec.x.type === "utc" ? 34 : 22;
  const ph = (h - axisH) / n;
  spec.panels.forEach((p, i) => {
    const last = i === n - 1;
    const box = el("div", { style: { position: "absolute", left: "0", right: "0", top: `${i * ph}px`, height: `${last ? ph + axisH : ph}px` } });
    host.append(box);
    const panel = { ...p, x: { ...spec.x, ...(p.x || {}) }, y: { ...(p.y || {}) } };
    if (p.empty) {
      box.append(el("div.chart-msg", {}, el("b", { text: p.title, style: { color: EC.ink, fontSize: "12px" } }), el("small", { text: p.empty })));
      return;
    }
    drawLine(box, panel, w, last ? ph + axisH : ph, { title: p.title, hideXAxis: !last });
  });
}

// ---- heat grid ------------------------------------------------------------

function drawHeat(host, spec, w, h) {
  const nx = spec.xKeys.length, ny = spec.yKeys.length;
  if (!nx || !ny) { host.append(el("div.chart-msg", { text: spec.empty || "No data to show" })); return; }
  const ml = Math.min(200, Math.max(...spec.yKeys.map((k) => textWidth(k, 10))) + 12);
  const mt = 4, mr = 8, mb = 46;
  const cw = (w - ml - mr) / nx, ch = (h - mt - mb) / ny;
  const color = d3().scaleLinear().domain(spec.colors.map((_, i) => i / (spec.colors.length - 1))).range(spec.colors).interpolate(d3().interpolateLab).clamp(true);
  const svg = d3().create("svg").attr("width", w).attr("height", h).style("font-family", FONT);
  const g = svg.append("g");
  for (let yi = 0; yi < ny; yi++) {
    for (let xi = 0; xi < nx; xi++) {
      const v = spec.value(xi, yi);
      g.append("rect").attr("x", ml + xi * cw).attr("y", mt + yi * ch).attr("width", Math.max(cw - 1, 0.5)).attr("height", Math.max(ch - 1, 0.5))
        .attr("fill", v == null || !Number.isFinite(v) ? spec.naColor || EC.surface : color(v));
    }
  }
  const fs = Math.max(8, Math.min(10, ch - 1));
  const ylab = svg.append("g").attr("font-size", fs).attr("fill", EC.ink);
  spec.yKeys.forEach((k, yi) => {
    if (ch < 7 && yi % Math.ceil(7 / ch)) return;
    ylab.append("text").attr("x", ml - 6).attr("y", mt + yi * ch + ch / 2).attr("dy", "0.32em").attr("text-anchor", "end").text(k);
  });
  const every = Math.max(1, Math.ceil(16 / cw));
  const xlab = svg.append("g").attr("font-size", 10).attr("fill", EC.inkDim);
  let lastTick = null, lastX = -Infinity;
  spec.xKeys.forEach((k, xi) => {
    const t = spec.xTick ? spec.xTick(k, xi) : k;
    if (!t || t === lastTick || xi - lastX < every) return;
    lastTick = t; lastX = xi;
    const x = ml + xi * cw + cw / 2, y = h - mb + 8;
    xlab.append("text").attr("transform", `translate(${x},${y}) rotate(-45)`).attr("text-anchor", "end").attr("dy", "0.32em").text(t);
  });
  const hover = svg.append("rect").attr("fill", "none").attr("stroke", EC.ink).attr("stroke-width", 2).attr("visibility", "hidden").attr("pointer-events", "none");
  host.append(svg.node());
  host.addEventListener("pointermove", (e) => {
    const r = svg.node().getBoundingClientRect();
    const xi = Math.floor((e.clientX - r.left - ml) / cw), yi = Math.floor((e.clientY - r.top - mt) / ch);
    if (xi < 0 || yi < 0 || xi >= nx || yi >= ny) { hover.attr("visibility", "hidden"); hideTip(); return; }
    hover.attr("visibility", "visible").attr("x", ml + xi * cw - 0.5).attr("y", mt + yi * ch - 0.5).attr("width", cw).attr("height", ch);
    const t = spec.tip(xi, yi);
    showTip(e.clientX, e.clientY, [el("div.tt-title", { text: t.title }), ...t.rows.map(([label, value]) => el("div.tt-row", {}, el("b", { text: value }), el("span", { text: label })))]);
  });
  host.addEventListener("pointerleave", () => { hover.attr("visibility", "hidden"); hideTip(); });
}

// ---- treemap --------------------------------------------------------------

function drawTreemap(host, spec, w, h) {
  const items = spec.items.filter((d) => d.value > 0);
  if (!items.length) { host.append(el("div.chart-msg", { text: spec.empty || "No data" })); return; }
  const root = d3().hierarchy({ children: items }).sum((d) => d.value || 0).sort((a, b) => b.value - a.value);
  d3().treemap().tile(d3().treemapSquarify).size([w, h]).paddingInner(2).round(true)(root);
  const svg = d3().create("svg").attr("width", w).attr("height", h).style("font-family", FONT);
  const total = d3().sum(items, (d) => d.value);
  for (const leaf of root.leaves()) {
    const d = leaf.data;
    const gw = leaf.x1 - leaf.x0, gh = leaf.y1 - leaf.y0;
    const g = svg.append("g").attr("transform", `translate(${leaf.x0},${leaf.y0})`);
    g.append("rect").attr("width", gw).attr("height", gh).attr("fill", d.color);
    if (gw > 70 && gh > 34) {
      const t = g.append("text").attr("x", gw / 2).attr("y", gh / 2).attr("text-anchor", "middle").attr("fill", d.text).attr("font-size", 12);
      t.append("tspan").attr("x", gw / 2).attr("dy", "-0.2em").text(d.label);
      t.append("tspan").attr("x", gw / 2).attr("dy", "1.2em").attr("font-weight", 700).text(`${(100 * d.value / total).toFixed(1)}%`);
    }
    g.on("pointermove", (e) => showTip(e.clientX, e.clientY, [el("div.tt-row", {}, swatchKey(d.color), el("b", { text: `${(100 * d.value / total).toFixed(1)}%` }), el("span", { text: d.label }))]))
      .on("pointerleave", hideTip);
  }
  host.append(svg.node());
}

// ---- table view ------------------------------------------------------------

function autoTable(spec) {
  if (spec.kind === "line") {
    const fmtX = xFormatter(spec);
    const xs = new Set();
    for (const s of spec.series) for (const v of s.x) xs.add(v);
    const xsSorted = [...xs].sort((a, b) => a - b);
    const columns = [{ key: "x", label: spec.x.label || (spec.x.type === "utc" ? "Date" : "x"), format: fmtX }];
    const lookups = [];
    spec.series.forEach((s, k) => {
      const m = new Map();
      s.x.forEach((xv, i) => m.set(xv, i));
      lookups.push(m);
      if (s.y) columns.push({ key: `s${k}`, label: s.label || spec.y.label || "value", format: (v) => fmtNum(v, 4) });
      if (s.lo && s.hi) {
        columns.push({ key: `s${k}lo`, label: `${s.label ? `${s.label} ` : ""}low`, format: (v) => fmtNum(v, 4) });
        columns.push({ key: `s${k}hi`, label: `${s.label ? `${s.label} ` : ""}high`, format: (v) => fmtNum(v, 4) });
      }
    });
    const rows = xsSorted.map((xv) => {
      const r = { x: xv };
      spec.series.forEach((s, k) => {
        const i = lookups[k].get(xv);
        if (i === undefined) return;
        if (s.y) r[`s${k}`] = s.y[i];
        if (s.lo) { r[`s${k}lo`] = s.lo[i]; r[`s${k}hi`] = s.hi[i]; }
      });
      return r;
    });
    return { columns, rows };
  }
  if (spec.kind === "stack") {
    const columns = [{ key: "panel", label: "Panel" }, { key: "x", label: spec.x.type === "utc" ? "Date" : "x", format: spec.x.type === "utc" ? (v) => fmtDate(v) : (v) => v },
      { key: "y", label: "Value", format: (v, r) => (r.text != null ? r.text : fmtNum(v, 4)) }];
    const rows = [];
    for (const p of spec.panels) {
      for (const s of p.series || []) {
        s.x.forEach((xv, i) => rows.push({ panel: p.title, x: xv, y: s.y[i], text: s.tipValue ? s.tipValue(i) : null }));
      }
    }
    return { columns, rows };
  }
  if (spec.kind === "treemap") {
    const total = d3().sum(spec.items, (d) => d.value);
    return { columns: [{ key: "label", label: "Source" }, { key: "share", label: "Share", format: (v) => `${(100 * v).toFixed(1)}%` }],
      rows: spec.items.map((d) => ({ label: d.label, share: d.value / total })) };
  }
  return null;
}
