// Spatial Datasets: any modelled variable as a coloured map layer.

import { D, geo, extremes, chemSummary, wbchem, subbasinFeature, isChemVar, isWbChemVar, chemKeyOf, variableLabel } from "../data.js";
import { state, set, on } from "../state.js";
import { el, field, select, setChildren } from "../ui.js";
import { EC, prettyUnit, fmtNum } from "../theme.js";
import { makeMap, setBasemap, packLayer, legendControl, BASEMAPS } from "../mapkit.js";
import { subbasinCtx, changeLocation } from "../common.js";
import { tickFmt } from "../chart.js";

const L = () => window.L;
// Single-hue sequential ramp (light = low, dark = high).
const RAMP = ["#e3effc", "#b7d3f6", "#86b6ef", "#5598e7", "#2a78d6", "#1c5cab", "#104281", "#0a2d55"];
const STATS = [{ value: "mean", label: "Mean" }, { value: "median", label: "Median" }, { value: "90th", label: "90th percentile" }];
const PCTS = [{ value: "1", label: "Low extreme (P1)" }, { value: "50", label: "Median (P50)" }, { value: "99", label: "High extreme (P99)" }];

export function create() {
  const H = D.meta.hype;
  const ctx = subbasinCtx({ map: true, action: changeLocation() });
  ctx.el.querySelectorAll(".ctx-stat").forEach((n) => n.remove());
  const statsBox = ctx.el.querySelector(".ctx-stats");
  const mk = (label) => { const v = el("div.ctx-value", { text: "—" }); statsBox.append(el("div.ctx-stat", {}, el("div.ctx-label", { text: label }), v)); return v; };
  const valueHere = mk("Value here"), rankHere = mk("Rank"), rangeAll = mk("Study-area range");

  const hypeOk = D.has("extremes");
  const variableOptions = {
    "Hydrology & climate": H.variables.map((v) => ({ value: v.id, label: hypeOk ? v.label : `${v.label} (not published)`, disabled: !hypeOk })),
    "Chemical concentrations · subbasin (daily, ng/L)": D.meta.chem.keys.map((k) => ({ value: `chem_${k}`, label: D.chemLabel[k], disabled: !D.has("chem_daily") })).sort((a, b) => a.label.localeCompare(b.label)),
    "Chemical concentrations · water body (monthly, ng/L)": D.meta.chem.keys.map((k) => ({ value: `wbchem_${k}`, label: D.chemLabel[k], disabled: !D.has("wbchem") })).sort((a, b) => a.label.localeCompare(b.label)),
  };
  let variable = hypeOk ? "discharge" : variableOptions["Chemical concentrations · subbasin (daily, ng/L)"][0].value;
  let period = H.combos.some(([s, p]) => s === "SSP585" && p === "2070-2079") ? "SSP585|2070-2079" : H.combos.length ? H.combos[0].join("|") : "";
  let pct = "99", stat = "median";

  const varSel = select({ options: variableOptions, value: variable, onChange: (v) => { variable = v; refresh(); } });
  const periodSel = select({ options: H.combos.map(([s, p]) => ({ value: `${s}|${p}`, label: `${s} (${p})` })), value: period, onChange: (v) => { period = v; refresh(); } });
  const pctSel = select({ options: PCTS, value: pct, onChange: (v) => { pct = v; refresh(); } });
  const statSel = select({ options: STATS, value: stat, onChange: (v) => { stat = v; refresh(); } });
  const hypeFields = el("div", { style: { display: "flex", flexDirection: "column", gap: "16px" } }, field("Period", periodSel.el), field("Conditions", pctSel.el));
  const chemFields = field("Statistic", statSel.el, "Time-averaged over the whole 2018-2022 record.");
  const basemap = select({ options: BASEMAPS, value: "osm", onChange: (v) => setBasemap(map, v) });
  const sidebar = el("aside.sidebar", { style: { width: "260px" } },
    el("div.sidebar-title", { text: "Map layer" }), field("Model variable", varSel.el), hypeFields, chemFields, field("Basemap", basemap.el));

  const mapBox = el("div");
  const status = el("div.map-status", { hidden: true });
  const page = el("div.page", {}, ctx.el, el("div.layout", {}, sidebar, el("div.main", {}, el("div.map-wrap", {}, mapBox, status))));
  const map = makeMap(mapBox);
  const legend = legendControl("bottomleft");
  legend.addTo(map);
  let layer = null, selection = null;
  let current = null;   // {kind, values: Map(id -> value), unit, label}
  let token = 0;

  // value per subbasin (or water body) for the current selection
  async function values() {
    if (isWbChemVar(variable)) {
      const W = await wbchem();
      const key = chemKeyOf(variable);
      const m = new Map();
      for (const wb of W.waterbodies) {
        const arr = W.series(key, stat, wb);
        if (!arr) continue;
        let s = 0, n = 0;
        for (const v of arr) if (Number.isFinite(v)) { s += v; n++; }
        if (n) m.set(wb, s / n);
      }
      return { kind: "waterbody", values: m, unit: "ng/L", label: `${STATS.find((x) => x.value === stat).label} conc. (ng/L)` };
    }
    if (isChemVar(variable)) {
      const S = await chemSummary();
      // Several modelling sites can share a subbasin: pool their means.
      const acc = new Map();
      for (const r of S.rows(chemKeyOf(variable), stat)) {
        if (!(r.n > 0)) continue;
        const a = acc.get(r.subbasin) || { s: 0, n: 0 };
        a.s += r.mean * r.n; a.n += r.n;
        acc.set(r.subbasin, a);
      }
      return { kind: "subbasin", values: new Map([...acc].map(([k, a]) => [k, a.s / a.n])), unit: "ng/L", label: `${STATS.find((x) => x.value === stat).label} conc. (ng/L)` };
    }
    const X = await extremes(variable);
    const [ssp, per] = period.split("|");
    const arr = X.values(ssp, per, Number(pct));
    const m = new Map();
    if (arr) X.ids.forEach((id, i) => { if (Number.isFinite(arr[i])) m.set(id, arr[i]); });
    return { kind: "subbasin", values: m, unit: prettyUnit(X.unit), label: `P${pct} (${prettyUnit(X.unit)})` };
  }

  function colourScale(vals) {
    const v = Float64Array.from(vals).filter(Number.isFinite).sort();
    if (!v.length) return null;
    // Clip the colour scale to the 2nd-98th percentile so a few extreme
    // subbasins don't wash everything else out; use a log scale when the
    // values are positive and span more than two orders of magnitude.
    const q = (p) => v[Math.min(v.length - 1, Math.max(0, Math.round(p * (v.length - 1))))];
    let lo = q(0.02), hi = q(0.98);
    if (lo === hi) { lo = v[0]; hi = v[v.length - 1]; }
    const posLo = v.find((x) => x > 0);
    const log = posLo != null && hi > 0 && hi / Math.max(lo, posLo) > 100;
    if (log) lo = Math.max(lo, posLo);
    const s = (log ? window.d3.scaleLog() : window.d3.scaleLinear()).domain([lo, hi === lo ? lo + 1 : hi]).clamp(true);
    const interp = window.d3.interpolateRgbBasis(RAMP);
    return { lo, hi, log, min: v[0], max: v[v.length - 1], colour: (x) => (Number.isFinite(x) ? interp(s(log ? Math.max(x, lo) : x)) : null), t: s };
  }

  function drawLegend(cs, label) {
    if (!cs) { setChildren(legend._div, el("div.legend-title", { text: "No data" })); return; }
    const canvas = el("canvas", { width: 14, height: 120 });
    const c2 = canvas.getContext("2d");
    const interp = window.d3.interpolateRgbBasis(RAMP);
    for (let y = 0; y < 120; y++) { c2.fillStyle = interp(1 - y / 119); c2.fillRect(0, y, 14, 1); }
    let ticks = cs.t.ticks(5).filter((t) => t >= cs.lo && t <= cs.hi);
    if (cs.log) {
      const decades = ticks.filter((t) => Math.abs(Math.log10(t) - Math.round(Math.log10(t))) < 1e-9);
      ticks = decades.length >= 2 ? decades : ticks.filter((t) => [1, 2, 5].includes(Math.round(t / 10 ** Math.floor(Math.log10(t) + 1e-9))));
      if (ticks.length > 6) ticks = ticks.filter((_, i) => i % Math.ceil(ticks.length / 6) === 0);
    }
    const tickEls = ticks.map((t) => el("span", { text: tickFmt(t), style: { top: `${(1 - cs.t(t)) * 120}px` } }));
    setChildren(legend._div, el("div.legend-title", { text: label }),
      el("div.legend-ramp", {}, canvas, el("div.ticks", {}, tickEls)),
      el("div", { style: { marginTop: "4px", color: EC.inkDim }, text: `${cs.log ? "Log scale; c" : "C"}olours clipped to the 2nd–98th percentile` }));
  }

  async function refresh() {
    hypeFields.hidden = isChemVar(variable) || isWbChemVar(variable);
    chemFields.hidden = !hypeFields.hidden;
    const t = ++token;
    status.textContent = "Loading…"; status.hidden = false;
    try {
      const cur = await values();
      const g = cur.kind === "waterbody" ? [await geo("waterbodies")] : await Promise.all([geo("subbasins_hype"), geo("subbasins_chem")]);
      if (t !== token) return;
      current = cur;
      const cs = colourScale(cur.values.values());
      if (layer) map.removeLayer(layer);
      layer = L().featureGroup();
      for (const pack of g) {
        const ids = pack.props.id;
        const idx = [];
        ids.forEach((id, i) => { if (cur.values.has(id)) idx.push(i); });
        packLayer(map, pack, idx, (i) => ({ fillColor: cs.colour(cur.values.get(ids[i])), fillOpacity: 0.8, color: EC.ink, weight: 0.6, opacity: 0.7 }))
          .bindTooltip((l) => {
            const id = ids[l.feature.properties.i];
            const name = cur.kind === "waterbody" ? (D.wb.get(id) || {}).name || id : `Subbasin ${id}`;
            return `${name}: ${fmtNum(cur.values.get(id), 4)} ${cur.unit}`;
          }, { sticky: true })
          .on("click", (e) => { if (cur.kind === "subbasin") set({ sb: ids[e.layer.feature.properties.i] }); })
          .addTo(layer);
      }
      layer.addTo(map);
      if (selection) selection.bringToFront();
      drawLegend(cs, `${variableLabel(variable)} · ${cur.label}`);
      rangeAll.textContent = cs ? `${fmtNum(cs.min, 3)} – ${fmtNum(cs.max, 3)}` : "—";
      drawHere();
    } finally {
      if (t === token) status.hidden = true;
    }
  }

  function drawHere() {
    if (!current || current.kind !== "subbasin" || state.sb == null || !current.values.has(state.sb)) {
      valueHere.textContent = current && current.kind === "waterbody" ? "hover a water body" : state.sb == null ? "—" : "no value";
      rankHere.textContent = "—";
      return;
    }
    const v = current.values.get(state.sb);
    setChildren(valueHere, fmtNum(v, 3), el("span.unit", { text: ` ${current.unit}` }));
    let rank = 1;
    for (const x of current.values.values()) if (x > v) rank++;
    rankHere.textContent = `${rank.toLocaleString("en-GB")} of ${current.values.size.toLocaleString("en-GB")}`;
  }

  async function drawSelection() {
    if (selection) { map.removeLayer(selection); selection = null; }
    drawHere();
    if (state.sb == null) return;
    const f = await subbasinFeature(state.sb);
    if (!f) return;
    if (selection) map.removeLayer(selection);
    selection = L().geoJSON(f.feature, { renderer: map._ecRenderer, interactive: false, style: { fill: false, color: EC.accent, weight: 3, opacity: 1 } }).addTo(map);
  }

  return {
    id: "spatial", title: "Spatial Datasets", el: page, deps: ["sb"],
    async init() { await refresh(); },
    update() { drawSelection(); },
  };
}
