// Site Details: chemical occurrence grid and time series for one measured
// monitoring site or one modelled water body.

import { D, measured, siteinfo, wbchem, waterbodyFeature } from "../data.js";
import { state, set, on } from "../state.js";
import { el, field, select, seg, rail, popover, ctxStrip, setChildren, unpublished } from "../ui.js";
import { EC, HEAT, fmtNum, fmtDate } from "../theme.js";
import { miniMap } from "../mapkit.js";
import { Chart } from "../chart.js";

const STAT_OPTIONS = [{ value: "mean", label: "Mean" }, { value: "median", label: "Median" }, { value: "90th", label: "90th percentile" }];

export function create() {
  const ctx = ctxStrip({ map: true, idLabel: "Measured site", stats: [["summary", "Summary"], ["landcover", "Land cover"]] });
  ctx.stats.summary.classList.add("is-text");
  ctx.stats.landcover.classList.add("is-text");
  const mmBox = el("div", { style: { width: "100%", height: "94px" } });
  ctx.map.append(mmBox);
  const mm = miniMap(mmBox);

  // ---- sidebar ----
  const mode = seg({
    options: [{ value: "measured", label: "Measured" }, { value: "modelled", label: "Modelled" }],
    value: state.siteMode, ariaLabel: "Site type",
    onChange: (v) => set({ siteMode: v }),
  });
  const siteRail = rail({ onPick: (id) => set({ site: id }) });
  const wbRail = rail({ onPick: (id) => set({ wb: id }) });
  const railNote = el("div", { style: { padding: "0 16px" } });
  const sidebar = el("aside.sidebar", { style: { width: "240px" } },
    el("div.sidebar-title", { text: "Sites" }), mode.el,
    el("div.fill", {}, railNote, siteRail.el, wbRail.el));

  // ---- cards ----
  let gridStat = "median";
  const statSelect = select({ options: STAT_OPTIONS, value: gridStat, onChange: (v) => { gridStat = v; drawGrid(); drawSeries(); } });
  const statPill = el("button.pill", { type: "button", text: "Statistic: Median ▾" });
  popover(statPill, "Modelled statistic", field("Statistic", statSelect.el));
  const gridNote = el("span.panel-note");
  const gridBody = el("div.card-body");
  const grid = new Chart(gridBody);
  const gridCard = el("section.card", {}, el("div.card-header", {}, el("span.title", { text: "Chemical occurrence" }), statPill, gridNote, el("span.card-tools", {}, grid.tableToggle())), gridBody);

  const panelSelects = [0, 1, 2, 3].map((k) => select({ options: [], onChange: (v) => { panels[k] = v; drawSeries(); } }));
  const panelPill = el("button.pill", { type: "button", text: "Choose panels ▾" });
  popover(panelPill, "Time series panels", el("div", { style: { display: "flex", flexDirection: "column", gap: "10px" } },
    panelSelects.map((s, k) => field(`Panel ${k + 1}`, s.el))));
  const tsBody = el("div.card-body");
  const ts = new Chart(tsBody);
  const tsCard = el("section.card", {}, el("div.card-header", {}, el("span.title", { text: "Time series" }), panelPill,
    el("span.panel-note", { text: "widest concentration range here" }), el("span.card-tools", {}, ts.tableToggle())), tsBody);

  const page = el("div.page", {}, ctx.el, el("div.layout", {}, sidebar, el("div.main", {}, el("div.grid-site", {}, gridCard, tsCard))));

  let M = null, INFO = null, W = null;
  let panels = [];
  let panelsFor = null;

  const modelled = () => state.siteMode === "modelled";

  // ---- context strip ----
  async function drawCtx() {
    INFO = INFO || await siteinfo();
    ctx.idLabel.textContent = modelled() ? "Modelled water body" : "Measured site";
    let info = null;
    if (modelled()) {
      const w = state.wb ? D.wb.get(state.wb) : null;
      ctx.setId(w ? w.name : "No water body");
      ctx.sub.textContent = w ? `${w.id}${w.opcat ? ` · ${w.opcat}` : ""}` : "Pick a water body in the list";
      info = w ? INFO.modelled[w.id] : null;
      ctx.stats.summary.textContent = !w ? "Select a modelled water body to see its background." : info ? info.summary : `No water body information available yet for ${w.id}`;
      if (w) { const f = await waterbodyFeature(w.id); if (f && state.wb === w.id && modelled()) mm.showFeature(f.feature, f.bbox); } else mm.clear();
    } else {
      const s = M && state.site ? M.byId.get(state.site) : null;
      ctx.setId(s ? s.name : "No site");
      ctx.sub.textContent = s ? `${s.id} · ${s.lat.toFixed(4)}, ${s.lon.toFixed(4)}` : "Pick a site in the list";
      info = s ? INFO.measured[s.id] : null;
      ctx.stats.summary.textContent = !s ? "Select a measured site to see its background." : info ? info.summary : `No site information available yet for ${s.id}`;
      if (s) mm.showPoint(s.lat, s.lon); else mm.clear();
    }
    const lc = info && info.landcover ? info.landcover : [];
    if (!lc.length) { ctx.stats.landcover.textContent = "—"; return; }
    const shade = window.d3.interpolateRgb(EC.accentText, EC.accentTint);
    setChildren(ctx.stats.landcover,
      el("div.landcover-bar", {}, lc.map(([name, pct], i) => el("div", { title: `${name} ${pct}%`, style: { width: `${pct}%`, background: shade(lc.length > 1 ? i / (lc.length - 1) : 0) } }))),
      el("div.landcover-key", {}, lc.map(([name, pct]) => el("span", { text: `${name} ${pct}` }))));
  }

  // ---- rails ----
  function drawRails() {
    siteRail.el.hidden = modelled();
    wbRail.el.hidden = !modelled();
    setChildren(railNote, modelled() ? (D.has("wbchem") ? null : unpublished("Modelled water body data")) : (D.has("measured") ? null : unpublished("Measured data")));
    siteRail.setActive(state.site);
    wbRail.setActive(state.wb);
  }

  // ---- measured helpers ----
  function measuredSeries(site, key) {
    const raw = site.values[key] || [];
    return raw.map((v) => (v === "ND" ? 0 : v == null ? NaN : v));
  }

  function measuredTop4(site) {
    const ranked = M.params.filter((p) => p.group === "Organic micropollutant").map((p) => {
      const v = measuredSeries(site, p.key).filter(Number.isFinite);
      return { key: p.key, range: v.length ? Math.max(...v) - Math.min(...v) : NaN };
    }).filter((r) => Number.isFinite(r.range)).sort((a, b) => b.range - a.range).slice(0, 4).map((r) => r.key);
    for (const p of M.params) { if (ranked.length >= 4) break; if (p.group === "Organic micropollutant" && !ranked.includes(p.key)) ranked.push(p.key); }
    return ranked;
  }

  function wbTop4(wb) {
    const ranked = D.meta.chem.keys.map((k) => {
      const v = Array.from(W.series(k, "median", wb) || []).filter(Number.isFinite);
      return { key: k, range: v.length ? Math.max(...v) - Math.min(...v) : NaN };
    }).filter((r) => Number.isFinite(r.range)).sort((a, b) => b.range - a.range).slice(0, 4).map((r) => r.key);
    for (const k of D.meta.chem.keys) { if (ranked.length >= 4) break; if (!ranked.includes(k)) ranked.push(k); }
    return ranked;
  }

  function setPanelOptions() {
    if (modelled()) {
      const opts = { "Modelled chemicals": D.meta.chem.keys.map((k) => ({ value: k, label: D.chemLabel[k] })).sort((a, b) => a.label.localeCompare(b.label)) };
      panelSelects.forEach((s, k) => { s.setOptions(opts, false); s.value = panels[k]; });
    } else if (M) {
      const opts = {
        "Organic micropollutants": M.params.filter((p) => p.group === "Organic micropollutant").map((p) => ({ value: p.key, label: p.label })),
        "Physicochemical parameters": M.params.filter((p) => p.group !== "Organic micropollutant").map((p) => ({ value: p.key, label: p.label })),
      };
      panelSelects.forEach((s, k) => { s.setOptions(opts, false); s.value = panels[k]; });
    }
  }

  // Reset the four panels to the widest-range chemicals whenever the site,
  // water body or mode changes.
  function resetPanels() {
    const key = modelled() ? `m:${state.wb}` : `s:${state.site}`;
    if (key === panelsFor) return;
    panelsFor = key;
    if (modelled() && state.wb && W) panels = wbTop4(state.wb);
    else if (!modelled() && state.site && M && M.byId.has(state.site)) panels = measuredTop4(M.byId.get(state.site));
    setPanelOptions();
  }

  // ---- occurrence grid ----
  function drawGrid() {
    statPill.hidden = !modelled();
    statPill.textContent = `Statistic: ${STAT_OPTIONS.find((o) => o.value === gridStat).label} ▾`;
    if (modelled()) {
      gridNote.textContent = "colour = concentration relative to this water body's own maximum";
      if (!D.has("wbchem")) return grid.message("Modelled water body data not published");
      if (!state.wb) return grid.message("Pick a water body in the list");
      const keys = D.meta.chem.keys.slice().sort((a, b) => D.chemLabel[a].localeCompare(D.chemLabel[b]));
      const series = keys.map((k) => W.series(k, gridStat, state.wb));
      if (!series.some(Boolean)) return grid.message("No modelled data for this water body");
      const scaled = series.map((s) => {
        if (!s) return null;
        let mx = -Infinity; for (const v of s) if (v > mx) mx = v;
        if (!(Number.isFinite(mx) && mx > 0)) mx = 1;
        return Array.from(s, (v) => (Number.isFinite(v) ? Math.log1p(Math.max(v, 0)) / Math.log1p(mx) : null));
      });
      const rows = [];
      keys.forEach((k, yi) => W.dateStrings.forEach((d, xi) => rows.push({ chem: D.chemLabel[k], date: d, value: series[yi] ? series[yi][xi] : null })));
      grid.draw({
        kind: "heat", xKeys: W.dateStrings, yKeys: keys.map((k) => D.chemLabel[k]),
        xTick: (d) => fmtDate(`${d}T00:00:00Z`, "my"),
        value: (xi, yi) => (scaled[yi] ? scaled[yi][xi] : null),
        tip: (xi, yi) => ({ title: D.chemLabel[keys[yi]], rows: [["", fmtDate(`${W.dateStrings[xi]}T00:00:00Z`, "my")], ["ng/L", series[yi] ? fmtNum(series[yi][xi], 3) : "–"]] }),
        colors: HEAT,
        legend: [{ kind: "gradient", colors: HEAT, from: "low", to: "this water body's max" }],
        table: { columns: [{ key: "chem", label: "Chemical" }, { key: "date", label: "Month" }, { key: "value", label: "Concentration (ng/L)", format: (v) => fmtNum(v, 4) }], rows },
      });
    } else {
      gridNote.textContent = "colour = concentration relative to this site's own maximum; grey = no sample";
      if (!D.has("measured")) return grid.message("Measured data not published");
      const site = state.site ? M.byId.get(state.site) : null;
      if (!site) return grid.message("Pick a site in the list");
      const params = M.params.slice().sort((a, b) => a.group.localeCompare(b.group) || a.label.localeCompare(b.label));
      const cells = params.map((p) => {
        const raw = site.values[p.key] || [];
        let mx = -Infinity;
        for (const v of raw) if (typeof v === "number" && v > mx) mx = v;
        if (!(Number.isFinite(mx) && mx > 0)) mx = 1;
        return raw.map((v) => (v == null ? null : Math.log1p(Math.max(v === "ND" ? 0 : v, 0)) / Math.log1p(mx)));
      });
      const status = (v) => (v == null ? "No sample taken" : v === "ND" ? "Not detected (below LOD)" : `Value: ${fmtNum(v, 3)}`);
      const rows = [];
      params.forEach((p) => site.dates.forEach((d, xi) => {
        const v = (site.values[p.key] || [])[xi];
        rows.push({ param: p.label, date: d, value: v == null ? "no sample" : v === "ND" ? "ND" : v });
      }));
      grid.draw({
        kind: "heat", xKeys: site.dates, yKeys: params.map((p) => p.label),
        xTick: (d) => fmtDate(`${d}T00:00:00Z`, "my"),
        value: (xi, yi) => cells[yi][xi],
        tip: (xi, yi) => ({ title: params[yi].label, rows: [["", fmtDate(`${site.dates[xi]}T00:00:00Z`)], ["", status((site.values[params[yi].key] || [])[xi])]] }),
        colors: HEAT,
        legend: [{ kind: "gradient", colors: HEAT, from: "not detected", to: "this site's max" }, { kind: "rect", color: EC.surface, label: "no sample" }],
        table: { columns: [{ key: "param", label: "Parameter" }, { key: "date", label: "Date" }, { key: "value", label: "Value", format: (v) => (typeof v === "number" ? fmtNum(v, 4) : v) }], rows },
      });
    }
  }

  // ---- time series panels ----
  function drawSeries() {
    if (modelled()) {
      if (!D.has("wbchem")) return ts.message("Modelled water body data not published");
      if (!state.wb) return ts.message("Pick a water body in the list");
      const x = W.dates.map((d) => d.getTime());
      ts.draw({
        kind: "stack", x: { type: "utc" }, caption: "Monthly modelled concentration (ng/L), 2018-2022.",
        panels: panels.map((k) => {
          const y = W.series(k, gridStat, state.wb);
          return y ? { title: D.chemLabel[k], y: {}, series: [{ x, y: Array.from(y), color: EC.accent, points: true, label: D.chemLabel[k] }] }
            : { title: D.chemLabel[k], empty: "No data" };
        }),
      });
    } else {
      if (!D.has("measured")) return ts.message("Measured data not published");
      const site = state.site ? M.byId.get(state.site) : null;
      if (!site) return ts.message("Pick a site in the list");
      const x = site.dates.map((d) => Date.parse(`${d}T00:00:00Z`));
      ts.draw({
        kind: "stack", x: { type: "utc" }, caption: "Gaps indicate no sample taken.",
        legend: [{ kind: "dot", color: EC.accent, label: "Detected" }, { kind: "dot", color: EC.inkDim, label: "Non-detect" }],
        panels: panels.map((key) => {
          const p = M.param.get(key);
          const raw = site.values[key] || [];
          return {
            title: p ? p.label : key, y: {},
            series: [{
              x, y: measuredSeries(site, key), color: EC.accent, points: true, label: p ? p.label : key,
              pointColors: raw.map((v) => (v === "ND" ? EC.inkDim : EC.accent)),
              tipValue: (i) => (raw[i] === "ND" ? "ND" : fmtNum(raw[i], 3)),
            }],
          };
        }),
      });
    }
  }

  async function update() {
    mode.value = state.siteMode;
    if (modelled() && D.has("wbchem") && !W) W = await wbchem();
    if (!modelled() && D.has("measured") && !M) {
      M = await measured();
      siteRail.setItems(M.sites.slice().sort((a, b) => a.name.localeCompare(b.name)).map((s) => ({ id: s.id, label: s.name })));
    }
    drawRails();
    resetPanels();
    drawGrid();
    drawSeries();
    drawCtx();
  }

  wbRail.setItems(D.wbModelled.map((w) => ({ id: w.id, label: w.name })));

  return {
    id: "site", title: "Site Details", el: page,
    deps: ["site", "wb", "siteMode"],
    update,
  };
}
