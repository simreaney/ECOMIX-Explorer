// Download: build a tabular or spatial extract and save it in the chosen
// format. Everything is assembled in the browser from the published data.

import { D, subbasin, hype, forcing, chemSubbasin, chemSummary, wbchem, geo, hypeDownloadSummary, forcingDownloadSummary, isChemVar, isWbChemVar, chemKeyOf, variableLabel } from "../data.js";
import { state, on } from "../state.js";
import { el, field, select, setChildren, pagedTable, saveFile } from "../ui.js";
import { fmtNum, dayToDate } from "../theme.js";
import { subbasinCtx, changeLocation } from "../common.js";
import { toCSV, toXLSX, toParquet, toGeoPackage, toShapefileZip, toGeoParquet, toGeoJSON } from "../export.js";

const FORCING_LABEL = "UKCP18 RCP8.5";
const CLIMATE = { precip: "Precipitation", temp: "Temperature" };
const TAB_FORMATS = [{ value: "csv", label: "CSV" }, { value: "xlsx", label: "XLSX" }, { value: "parquet", label: "Parquet" }];
const GEO_FORMATS = [{ value: "gpkg", label: "GeoPackage" }, { value: "shp", label: "Shapefile (.zip)" }, { value: "geoparquet", label: "GeoParquet" }, { value: "geojson", label: "GeoJSON" }];
const EXT = { csv: "csv", xlsx: "xlsx", parquet: "parquet", gpkg: "gpkg", shp: "zip", geoparquet: "parquet", geojson: "geojson" };
const round3 = (v) => (v != null && Number.isFinite(v) ? Math.round(v * 1000) / 1000 : null);
const isoDay = (k) => dayToDate(k).toISOString().slice(0, 10);

export function create() {
  const ctx = subbasinCtx({ map: true, action: changeLocation() });
  ctx.el.querySelectorAll(".ctx-stat").forEach((n) => n.remove());
  const statsBox = ctx.el.querySelector(".ctx-stats");
  const mk = (label) => { const v = el("div.ctx-value", { text: "—" }); statsBox.append(el("div.ctx-stat", {}, el("div.ctx-label", { text: label }), v)); return v; };
  const rowsStat = mk("Rows in selection"), combosStat = mk("Scenarios × periods"), sizeStat = mk("Estimated size");

  const hypeOk = D.has("hype_proj"), forcingOk = D.has("forcing");
  const tag = (ok, label) => (ok ? label : `${label} (not published)`);
  const chemOpts = (prefix, ok) => D.meta.chem.keys.map((k) => ({ value: `${prefix}${k}`, label: tag(ok, D.chemLabel[k]), disabled: !ok })).sort((a, b) => a.label.localeCompare(b.label));
  const varOptions = {
    "Hydrology & climate": [
      { value: "precip", label: tag(forcingOk, "Precipitation"), disabled: !forcingOk },
      { value: "temp", label: tag(forcingOk, "Temperature"), disabled: !forcingOk },
      ...D.meta.hype.variables.map((v) => ({ value: v.id, label: tag(hypeOk, v.label), disabled: !hypeOk })),
    ],
    "Chemical concentrations · subbasin (daily, ng/L)": chemOpts("chem_", D.has("chem_daily")),
    "Chemical concentrations · water body (monthly, ng/L)": chemOpts("wbchem_", D.has("wbchem")),
  };
  const firstEnabled = Object.values(varOptions).flat().find((o) => !o.disabled);
  const s = { variable: firstEnabled ? firstEnabled.value : "precip", stat: "median", type: "tabular", layer: "subbasins", format: "csv" };

  const varSel = select({ options: varOptions, value: s.variable, onChange: (v) => { s.variable = v; if (isWbChemVar(v)) { s.layer = "waterbodies"; layerSel.value = "waterbodies"; } refresh(); } });
  const statSel = select({ options: [{ value: "mean", label: "Mean" }, { value: "median", label: "Median" }, { value: "90th", label: "90th percentile" }], value: s.stat, onChange: (v) => { s.stat = v; refresh(); } });
  const statField = field("Statistic", statSel.el);
  const typeSel = select({ options: [{ value: "tabular", label: "Tabular" }, { value: "spatial", label: "Spatial" }], value: s.type, onChange: (v) => {
    s.type = v; s.format = v === "tabular" ? "csv" : "gpkg";
    formatSel.setOptions(v === "tabular" ? TAB_FORMATS : GEO_FORMATS, false); formatSel.value = s.format; refresh();
  } });
  const layerSel = select({ options: [{ value: "subbasins", label: "Subbasins" }, { value: "catchment", label: "Catchment" }, { value: "waterbodies", label: "Water bodies" }], value: s.layer, onChange: (v) => { s.layer = v; refresh(); } });
  const layerField = field("Spatial layer", layerSel.el);
  const formatSel = select({ options: TAB_FORMATS, value: s.format, onChange: (v) => { s.format = v; drawStats(); } });
  const help = el("p.help");
  const dlBtn = el("button.btn.btn-accent", { type: "button", text: "Download data", onclick: () => download() });
  const dlNote = el("p.help");
  const sidebar = el("aside.sidebar", { style: { width: "290px" } },
    el("div.sidebar-title", { text: "Build a download" }),
    field("Variable", varSel.el), statField, field("Data type", typeSel.el), layerField, field("Format", formatSel.el), help, dlBtn, dlNote);

  const previewNote = el("span.panel-note");
  const previewBody = el("div.card-body");
  const page = el("div.page", {}, ctx.el, el("div.layout", {}, sidebar, el("div.main", {},
    el("section.card", { style: { flex: "1 1 auto" } }, el("div.card-header", {}, el("span.title", { text: "Preview" }), previewNote), previewBody))));

  let built = null;   // {rows, columns} for tabular or {features, rows} for spatial
  let token = 0;

  const chemVar = () => isChemVar(s.variable) || isWbChemVar(s.variable);

  // ---- tabular extract ----
  async function tabularRows() {
    const v = s.variable;
    if (isWbChemVar(v)) {
      const W = await wbchem();
      const key = chemKeyOf(v), rows = [];
      for (const wb of W.waterbodies) {
        const arr = W.series(key, s.stat, wb);
        if (arr) W.dateStrings.forEach((d, i) => { if (Number.isFinite(arr[i])) rows.push({ date: d, waterbody_id: wb, value: arr[i], unit: "ng/L" }); });
      }
      return { rows };
    }
    if (state.sb == null) return { message: "Select a subbasin on the Map tab to build a download." };
    if (isChemVar(v)) {
      if (!D.chemSitesBySb.has(state.sb)) return { message: "No chemical data for this subbasin." };
      const key = chemKeyOf(v), rows = [];
      for (const site of await chemSubbasin(state.sb)) {
        const arr = site.series(key, s.stat);
        if (arr) for (let i = 0; i < site.n; i++) rows.push({ date: isoDay(site.start + i), subbasin: state.sb, site_id: site.site, value: arr[i], unit: "ng/L" });
      }
      return { rows };
    }
    if (v in CLIMATE) {
      const { rows, units } = await forcing(state.sb);
      const out = rows.filter((r) => r.variable === CLIMATE[v] && r.agg === "monthly")
        .map((r) => ({ subbasin: state.sb, scenario: FORCING_LABEL, period: r.period, month: r.month, value: round3(r.p50), unit: units[CLIMATE[v]] }));
      return out.length ? { rows: out } : { message: "No climate data for this subbasin." };
    }
    const sb = subbasin(state.sb);
    if (!sb || !sb.hype) return { message: "No hydrology model data for this subbasin." };
    const data = await hype(state.sb);
    const unit = (D.hypeVar.get(v) || {}).unit;
    return { rows: data.month.filter((r) => r.variable === v).map((r) => ({ subbasin: state.sb, scenario: r.ssp, period: r.period, month: r.month, value: round3(r.p50), unit })) };
  }

  // Per-subbasin (or per-water-body) n / mean / min / max of the variable.
  async function summaries() {
    const v = s.variable;
    if (isWbChemVar(v)) {
      const W = await wbchem();
      const key = chemKeyOf(v), out = new Map();
      for (const wb of W.waterbodies) {
        const arr = W.series(key, s.stat, wb);
        if (!arr) continue;
        let n = 0, sum = 0, mn = Infinity, mx = -Infinity;
        for (const x of arr) if (Number.isFinite(x)) { n++; sum += x; if (x < mn) mn = x; if (x > mx) mx = x; }
        if (n) out.set(wb, { n, mean: sum / n, min: mn, max: mx });
      }
      return { byId: out, unit: "ng/L", round: round3 };
    }
    if (isChemVar(v)) {
      const S = await chemSummary();
      const out = new Map();
      for (const r of S.rows(chemKeyOf(v), s.stat)) {
        if (!(r.n > 0)) continue;
        const a = out.get(r.subbasin) || { n: 0, mean: 0, min: Infinity, max: -Infinity };
        a.mean = (a.mean * a.n + r.mean * r.n) / (a.n + r.n);
        a.n += r.n; a.min = Math.min(a.min, r.min); a.max = Math.max(a.max, r.max);
        out.set(r.subbasin, a);
      }
      return { byId: out, unit: "ng/L", round: (x) => x };
    }
    const table = v in CLIMATE ? await forcingDownloadSummary() : await hypeDownloadSummary();
    const rows = table.rows(v in CLIMATE ? CLIMATE[v] : v);
    const unit = v in CLIMATE ? (v === "precip" ? "mm" : "degC") : (D.hypeVar.get(v) || {}).unit;
    return { byId: new Map(rows.map((r) => [r.subbasin, r])), unit, round: round3 };
  }

  async function spatialFeatures() {
    const wbVar = isWbChemVar(s.variable);
    if ((s.layer === "waterbodies") !== wbVar) {
      return { message: wbVar ? "Water body chemical variables can only be exported on the Water bodies layer." : "The Water bodies layer only carries the water body chemical variables." };
    }
    const sum = await summaries();
    const stats = (a) => ({
      n_records: a ? a.n : 0, value_mean: a ? sum.round(a.mean) : null, value_min: a ? sum.round(a.min) : null, value_max: a ? sum.round(a.max) : null,
      unit: a ? sum.unit : null, spatial_layer: s.layer, variable: s.variable,
    });
    const name = s.layer === "subbasins" ? "dl_subbasins" : s.layer === "catchment" ? "dl_catchments" : "dl_waterbodies";
    const g = await geo(name);
    const keys = Object.keys(g.props);
    let overall = null;
    if (s.layer === "catchment") {
      // As in the Shiny app: one study-area-wide summary of every subbasin's
      // values, attached to each catchment.
      let n = 0, mean = 0, mn = Infinity, mx = -Infinity;
      for (const a of sum.byId.values()) { mean = (mean * n + a.mean * a.n) / (n + a.n); n += a.n; mn = Math.min(mn, a.min); mx = Math.max(mx, a.max); }
      overall = n ? { n, mean, min: mn, max: mx } : null;
    }
    const features = [];
    for (let i = 0; i < g.n; i++) {
      const props = Object.fromEntries(keys.map((k) => [k, g.props[k][i]]));
      const a = s.layer === "subbasins" ? sum.byId.get(props.Id) : s.layer === "waterbodies" ? sum.byId.get(props.water_body) : overall;
      features.push({ props: { ...props, ...stats(a) }, polygons: g.polygons(i) });
    }
    return { features };
  }

  // ---- preview + stats ----
  async function refresh() {
    statField.hidden = !chemVar();
    layerField.hidden = s.type !== "spatial";
    help.textContent = s.type === "tabular"
      ? (chemVar() ? (isWbChemVar(s.variable) ? "Monthly values for every modelled water body (this page has no water body picker)." : "Daily values for every modelling site in the selected subbasin.")
        : "Monthly median (p50) values for the selected subbasin, every scenario and period.")
      : "One feature per polygon with the count, mean, minimum and maximum of the variable; geometry in British National Grid (EPSG:27700).";
    const t = ++token;
    previewBody.style.opacity = "0.45";
    try {
      const res = s.type === "tabular" ? await tabularRows() : await spatialFeatures();
      if (t !== token) return;
      built = res;
      if (res.message) {
        setChildren(previewBody, el("div.chart-msg", { text: res.message }));
        previewNote.textContent = "nothing selected yet";
      } else {
        const rows = res.rows || res.features.map((f) => f.props).filter((p) => !(p.value_mean == null && p.value_min == null && p.value_max == null));
        const cols = rows.length ? Object.keys(rows[0]) : [];
        setChildren(previewBody, rows.length
          ? pagedTable(cols.map((c) => ({ key: c, label: c, format: (v) => (typeof v === "number" ? (Number.isInteger(v) ? String(v) : fmtNum(v, 5)) : v ?? "NA") })), rows, { pageSize: 100 })
          : el("div.chart-msg", { text: "No rows with values for this selection." }));
        const total = res.rows ? res.rows.length : res.features.length;
        previewNote.textContent = res.rows ? `${total.toLocaleString("en-GB")} rows` : `${total.toLocaleString("en-GB")} features · table lists the ${rows.length.toLocaleString("en-GB")} with values`;
      }
    } catch (err) {
      console.error(err);
      if (t === token) { built = { message: String(err.message || err) }; setChildren(previewBody, el("div.chart-msg", { text: "Could not build this extract", title: String(err) })); }
    } finally {
      if (t === token) previewBody.style.opacity = "";
      drawStats();
    }
  }

  function drawStats() {
    const n = !built || built.message ? 0 : built.rows ? built.rows.length : built.features.length;
    rowsStat.textContent = n.toLocaleString("en-GB");
    if (built && built.rows && built.rows.length && "scenario" in built.rows[0]) {
      combosStat.textContent = `${new Set(built.rows.map((r) => r.scenario)).size} × ${new Set(built.rows.map((r) => r.period)).size}`;
    } else combosStat.textContent = "—";
    sizeStat.textContent = `${Math.max(Math.round((n * 48) / 1024), 1).toLocaleString("en-GB")} kB ${s.format.toUpperCase()}`;
    dlBtn.disabled = !n;
  }

  function fileName() {
    const spatial = s.type === "spatial";
    const id = spatial ? (s.layer === "subbasins" ? "all_subbasins" : s.layer === "waterbodies" ? "all_waterbodies" : "catchment")
      : isWbChemVar(s.variable) ? "all_waterbodies" : `subbasin_${state.sb}`;
    const label = chemVar() ? `${chemKeyOf(s.variable)}_${s.stat}` : s.variable;
    return `ecomix_${spatial ? "spatial" : "tabular"}${spatial ? `_${s.layer}` : ""}_${id}_${label}.${EXT[s.format]}`;
  }

  async function download() {
    if (!built || built.message) return;
    dlBtn.disabled = true;
    dlNote.textContent = "Preparing file…";
    try {
      let blob;
      if (s.type === "tabular") {
        const rows = built.rows;
        blob = s.format === "csv" ? new Blob([toCSV(rows)], { type: "text/csv" }) : s.format === "xlsx" ? toXLSX(rows) : await toParquet(rows);
      } else {
        const f = built.features;
        blob = s.format === "gpkg" ? await toGeoPackage(f) : s.format === "shp" ? toShapefileZip(f) : s.format === "geoparquet" ? await toGeoParquet(f) : toGeoJSON(f);
      }
      saveFile(fileName(), blob);
      dlNote.textContent = `${fileName()} · ${(blob.size / 1024).toLocaleString("en-GB", { maximumFractionDigits: 0 })} kB`;
    } catch (err) {
      console.error(err);
      dlNote.textContent = `Could not write the file: ${err.message || err}`;
    } finally {
      dlBtn.disabled = false;
    }
  }

  return {
    id: "download", title: "Download", el: page, deps: ["sb"],
    update() { refresh(); },
  };
}
