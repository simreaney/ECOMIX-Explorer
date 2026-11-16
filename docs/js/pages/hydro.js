// Hydro Explorer: climate, simulated vs observed hydrology, and projections
// (yearly / monthly / distributions / daily) for one subbasin or, for the
// chemical variables, one modelled water body.

import { D, subbasin, hype, forcing, obs, chemSubbasin, wbchem, isChemVar, chemKeyOf, variableLabel } from "../data.js";
import { state, set, on } from "../state.js";
import { el, field, select, seg, chips, combo, popover, setChildren, unpublished, saveFile } from "../ui.js";
import { EC, SERIES, STAT_COLOUR, STAT_LABEL, PCT_DASH, PCT_LABEL, PCT_PROB, scenarioColour, prettyUnit, fmtNum, monthName, dayToDate, DAY, quantilesSorted } from "../theme.js";
import { Chart } from "../chart.js";
import { subbasinCtx, changeLocation } from "../common.js";
import { toCSV } from "../export.js";

const FORCING_LABEL = "UKCP18 RCP8.5";
const WQ_VARIABLES = new Set(["SS", "IN", "SP", "PP", "DOC"]);
const MONTH_DAYS = [31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
const PROBS = Array.from({ length: 99 }, (_, i) => i + 1);
const pctShort = (p) => `P${Number(p.slice(1))}`;
const monthTicks = { ticks: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12], tickFormat: (m) => monthName(m) };
const cfTicks = { ticks: [0, 25, 50, 75, 100], tickFormat: (v) => (v === 50 ? "50 (median)" : String(v)) };

export function create() {
  const H = D.meta.hype;
  const ctx = subbasinCtx({ map: true, action: changeLocation() });

  // ---- sidebar controls ----
  const mode = seg({
    options: [{ value: "subbasin", label: "Subbasin" }, { value: "waterbody", label: "Water body" }],
    value: state.deMode, ariaLabel: "Spatial unit", onChange: (v) => set({ deMode: v }),
  });
  const wbSearch = combo({
    placeholder: "Search water body",
    getItems: () => D.wbModelled.map((w) => ({ label: w.name, value: w.id })),
    onPick: (it) => set({ deWb: it.value }),
  });
  const wbField = el("div", {}, wbSearch.el);

  const scenarioChips = chips({ options: H.scenarios.map((s) => ({ value: s, label: s })), value: state.scenarios, onChange: (v) => set({ scenarios: v }) });
  const periodChips = chips({ options: H.periods.map((p) => ({ value: p, label: p })), value: state.periods, onChange: (v) => set({ periods: v }) });
  const plotType = seg({ options: [{ value: "Absolute", label: "Absolute" }, { value: "Relative", label: "Relative" }], value: state.plotType, onChange: (v) => set({ plotType: v }) });
  const plotTypeField = field("Values shown as", plotType.el);
  const subbasinControls = el("div", { style: { display: "flex", flexDirection: "column", gap: "16px" } },
    field("Scenarios", scenarioChips.el), field("Projection periods", periodChips.el), plotTypeField);

  const pctChips = chips({ options: H.pct.map((p) => ({ value: p, label: pctShort(p), title: PCT_LABEL[p] })), value: state.pct, onChange: (v) => set({ pct: v }) });
  const statChips = chips({ options: ["mean", "median", "90th"].map((s) => ({ value: s, label: STAT_LABEL[s] })), value: state.chemStats, onChange: (v) => set({ chemStats: v }) });
  const condLabel = el("span.label", { text: "Conditions" });
  const condField = el("div", {}, condLabel, pctChips.el, statChips.el);

  const variable = select({ options: [], onChange: (v) => set({ variable: v }) });
  const dlBtn = el("button.btn.btn-accent", { type: "button", text: "Download this selection", onclick: () => downloadSelection() });
  const notes = el("div", { style: { display: "flex", flexDirection: "column", gap: "8px" } });

  const sidebar = el("aside.sidebar", { style: { width: "260px" } },
    el("div.sidebar-title", { text: "Controls · all panels" }),
    mode.el, wbField, subbasinControls, condField, el("hr.hr"), field("Model variable", variable.el), dlBtn, notes);

  // ---- cards ----
  const climateVar = select({ options: [{ value: "Precipitation", label: "Precipitation" }, { value: "Temperature", label: "Temperature" }], value: state.climateVar, onChange: (v) => set({ climateVar: v }) });
  const climateRes = select({ options: [{ value: "monthly", label: "Monthly" }, { value: "annual", label: "Yearly" }], value: state.climateRes, onChange: (v) => set({ climateRes: v }) });
  const climatePill = el("button.pill", { type: "button" });
  popover(climatePill, "Climate panel", el("div", { style: { display: "flex", flexDirection: "column", gap: "10px" } }, field("Climate variable", climateVar.el), field("Resolution", climateRes.el)));
  const climateBody = el("div.card-body");
  const climate = new Chart(climateBody);
  const climateCard = el("section.card", {}, el("div.card-header", {}, el("span.title", { text: "Climate" }), climatePill, el("span.card-tools", {}, climate.tableToggle())), climateBody);

  const obsVar = select({ options: [], onChange: (v) => set({ obsVar: v }) });
  const obsPill = el("button.pill", { type: "button" });
  popover(obsPill, "Observation panel", field("Observed variable", obsVar.el));
  const obsBody = el("div.card-body");
  const obsChart = new Chart(obsBody);
  const obsCard = el("section.card", {}, el("div.card-header", {}, el("span.title", { text: "Simulated vs observed" }), obsPill, el("span.card-tools", {}, obsChart.tableToggle())), obsBody);

  const projTitle = el("span.title");
  const tabs = seg({ options: ["Yearly", "Monthly", "Distributions", "Daily"].map((t) => ({ value: t, label: t })), value: state.projTab, onChange: (v) => set({ projTab: v }) });
  const projBody = el("div.card-body");
  const proj = new Chart(projBody);
  const projCard = el("section.card.span-2", {}, el("div.card-header", {}, projTitle, tabs.el, el("span.card-tools", {}, proj.tableToggle())), projBody);

  const page = el("div.page", {}, ctx.el, el("div.layout", {}, sidebar,
    el("div.main", {}, el("div.grid-hydro", {}, climateCard, obsCard, projCard))));

  const wbMode = () => state.deMode === "waterbody";
  const chemMode = () => isChemVar(state.variable);

  // ---- control sync ----
  function syncControls() {
    mode.value = state.deMode;
    wbField.hidden = !wbMode();
    wbSearch.text = state.deWb && D.wb.get(state.deWb) ? D.wb.get(state.deWb).name : "";
    subbasinControls.hidden = wbMode();
    scenarioChips.value = state.scenarios;
    periodChips.value = state.periods;
    plotType.value = state.plotType;
    plotTypeField.hidden = chemMode();
    pctChips.el.hidden = chemMode();
    statChips.el.hidden = !chemMode();
    pctChips.value = state.pct;
    statChips.value = state.chemStats;
    condLabel.textContent = chemMode() ? "Statistic" : "Conditions";

    const chemOpts = D.meta.chem.keys.map((k) => ({ value: `chem_${k}`, label: D.chemLabel[k] })).sort((a, b) => a.label.localeCompare(b.label));
    const hypeOpts = H.variables.map((v) => ({ value: v.id, label: D.has("hype_proj") ? v.label : `${v.label} (not published)`, disabled: !D.has("hype_proj") }));
    variable.setOptions(wbMode() ? { "Chemical concentrations (ng/L)": chemOpts } : { "Hydrology & climate": hypeOpts, "Chemical concentrations (ng/L)": chemOpts }, false);
    variable.value = state.variable;

    climateVar.value = state.climateVar;
    climateRes.value = state.climateRes;
    climatePill.textContent = `${state.climateVar} · ${state.climateRes === "annual" ? "Yearly" : "Monthly"} ▾`;

    const choices = obsChoices();
    obsVar.setOptions(choices.length ? choices.map((v) => ({ value: v, label: v })) : [{ value: "", label: state.sb == null ? "Please select a subbasin" : "No observations available" }], false);
    obsVar.value = currentObsVar();
    obsPill.textContent = `${currentObsVar() || "Choose a variable"} ▾`;

    tabs.el.querySelectorAll("button")[3].hidden = !chemMode();
    tabs.value = state.projTab;
    projTitle.textContent = chemMode() ? `${variableLabel(state.variable)} concentration` : `Projected ${variableLabel(state.variable).toLowerCase()}`;

    const missing = [];
    if (!D.has("hype_proj")) missing.push(unpublished("Hydrology projections"));
    setChildren(notes, missing);
  }

  const obsChoices = () => (state.sb != null ? D.obsVars.get(state.sb) || [] : []);
  const currentObsVar = () => {
    const c = obsChoices();
    return c.includes(state.obsVar) ? state.obsVar : c[0] || null;
  };

  // Keep the selection valid: a water body mode needs a chemical variable,
  // and "Daily" only exists for chemicals.
  function normalise() {
    const patch = {};
    if (wbMode() && !chemMode()) patch.variable = `chem_${D.meta.chem.keys.slice().sort((a, b) => D.chemLabel[a].localeCompare(D.chemLabel[b]))[0]}`;
    const chem = isChemVar(patch.variable || state.variable);
    if (!chem && state.projTab === "Daily") patch.projTab = "Yearly";
    if (Object.keys(patch).length) { set(patch); return true; }
    return false;
  }

  // ---- climate panel ----
  function drawClimate() {
    if (wbMode()) return climate.message("Not available in Water body mode");
    if (!D.has("forcing")) return climate.message("Climate projections not published", "The UKCP18 ensemble summary (data/DB_Proj_Forcing) was not available when this site was built.");
    if (state.sb == null) return climate.message("Pick a subbasin on the map");
    const sb = state.sb, cv = state.climateVar, res = state.climateRes;
    climate.load(async () => {
      const { rows, units } = await forcing(sb);
      let df = rows.filter((r) => r.variable === cv && r.agg === res);
      if (!df.length) return { message: "No climate model data for this subbasin" };
      const unit = prettyUnit(units[cv]);
      if (res === "monthly") {
        const periods = [...(state.scenarios.includes("Baseline") ? [H.baseline_period] : []), ...state.periods];
        df = df.filter((r) => periods.includes(r.period));
        if (!df.length) return { message: "No climate model data for the selected period" };
        const groups = d3group(df, (r) => r.period);
        const series = [...groups.keys()].sort().map((period) => {
          const g = groups.get(period).sort((a, b) => a.month - b.month);
          const isBase = period === H.baseline_period;
          return {
            label: isBase ? `Baseline (${period})` : `${FORCING_LABEL} (${period})`,
            color: isBase ? EC.ink : scenarioColour(FORCING_LABEL, period, H.periods),
            x: g.map((r) => r.month), y: g.map((r) => r.p50), lo: g.map((r) => r.p10), hi: g.map((r) => r.p90),
          };
        });
        return {
          kind: "line", x: { label: "Month", ...monthTicks, format: (m) => monthName(m) }, y: { label: `${cv} [${unit}]` }, series,
          caption: "Median across the 12 UKCP18 members; band = 10th-90th percentile across years in the period.",
        };
      }
      df.sort((a, b) => a.year - b.year);
      return {
        kind: "line", x: { label: "Year", tickFormat: (v) => String(v), format: (v) => String(v) }, y: { label: `${cv} [${unit}]` },
        series: [{ label: FORCING_LABEL, color: SERIES.red, x: df.map((r) => r.year), y: df.map((r) => r.p50), lo: df.map((r) => r.p10), hi: df.map((r) => r.p90) }],
        caption: "Median of the 12 UKCP18 members; band = 10th-90th percentile across members.",
      };
    });
  }

  // ---- simulated vs observed ----
  function drawObs() {
    if (wbMode()) return obsChart.message("Not available in Water body mode");
    if (state.sb == null) return obsChart.message("Pick a subbasin on the map");
    const v = currentObsVar();
    if (!v) return obsChart.message("No observations at this subbasin", "Turn on the map's “Observed hydrology sites” layer to find subbasins with a gauge.");
    if (!D.has("obs")) return obsChart.message("Observed hydrology not published", "The historical simulation/observation dataset (data/DB_Historical_Sim_Obs) was not available when this site was built.");
    const sb = state.sb;
    obsChart.load(async () => {
      const all = await obs(sb);
      const d = all[v];
      if (!d || !d.day.length) return { message: "No data for this variable" };
      const x = Array.from(d.day, (k) => k * DAY);
      const st = d.stations[0];
      const caption = st ? `${st.label || ""} (Station ${st.id})` : "";
      if (v === "discharge") {
        return {
          kind: "line", x: { type: "utc" }, y: { label: "Discharge [m³/s]", zero: true },
          series: [
            { label: "Simulation", color: SERIES.red, x, y: Array.from(d.sim_P50), lo: Array.from(d.sim_P10), hi: Array.from(d.sim_P90), width: 1.5 },
            { label: "Observation", color: EC.ink, x, y: Array.from(d.obs), lo: Array.from(d.obs_min), hi: Array.from(d.obs_max), width: 1.5, bandOpacity: 0.2 },
          ],
          caption,
        };
      }
      return {
        kind: "line", x: { type: "utc" }, y: { label: `${v}${d.unit ? ` [${prettyUnit(d.unit)}]` : ""}`, zero: true },
        series: [
          { label: "Simulation (median, 10th-90th)", color: SERIES.red, x, y: Array.from(d.sim_P50), lo: Array.from(d.sim_P10), hi: Array.from(d.sim_P90), width: 1.5 },
          { label: "Observation", color: EC.ink, x, y: Array.from(d.obs), line: false, points: true, r: 3 },
        ],
        caption,
      };
    });
  }

  // ---- projections ----
  function drawProjections() {
    if (chemMode()) return drawChemProjection();
    if (wbMode()) return proj.message("Pick a chemical variable");
    if (!D.has("hype_proj")) return proj.message("Hydrology projections not published", "The HYPE projections (data/DB_Proj_Year, data/DB_Proj_Month) were not available when this site was built. Chemical variables are available in the Model variable list.");
    if (state.sb == null) return proj.message("Pick a subbasin on the map");
    const s = subbasin(state.sb);
    if (!s || !s.hype) return proj.message("No hydrology model data for this subbasin", "The HYPE model covers 3,126 of the subbasins; the map's “Subbasins with hydrology” layer shows which.");
    const sb = state.sb, tab = state.projTab;
    proj.load(async () => {
      const data = await hype(sb);
      if (tab === "Yearly") return yearlySpec(data);
      if (tab === "Monthly") return monthlySpec(data);
      return distributionSpec(data);
    });
  }

  const pcts = () => H.pct.filter((p) => state.pct.includes(p));
  const unitOf = (v) => prettyUnit((D.hypeVar.get(v) || {}).unit);

  function pctLegend(scenarioItems) {
    return [...scenarioItems, { kind: "sep" }, ...pcts().map((p) => ({ label: pctShort(p), color: EC.inkDim, dash: PCT_DASH[p] }))];
  }

  function yearlySpec(data) {
    const v = state.variable, scen = state.scenarios, ps = pcts();
    let rows = data.year.filter((r) => r.variable === v && (r.ssp === "Baseline" || scen.includes(r.ssp)));
    if (WQ_VARIABLES.has(v)) rows = rows.filter((r) => r.ssp === "Baseline" || r.year % 10 !== 0);
    if (!rows.length) return { message: "No hydrology model data for this subbasin" };
    const baseYears = rows.filter((r) => r.ssp === "Baseline").map((r) => r.year);
    const lbl = variableLabel(v), unit = unitOf(v);
    const sspOrder = H.scenarios;
    if (state.plotType === "Absolute") {
      const plotRows = rows.filter((r) => scen.includes(r.ssp) && !(r.ssp === "Baseline" && r.year > 2020));
      if (!plotRows.length) return { message: "No data for the selected scenarios" };
      const groups = d3group(plotRows, (r) => r.ssp);
      const series = [];
      const legend = [];
      for (const ssp of sspOrder.filter((s) => groups.has(s))) {
        const g = groups.get(ssp).sort((a, b) => a.year - b.year);
        const color = scenarioColour(ssp, null, null);
        legend.push({ label: ssp, color });
        const x = g.map((r) => r.year);
        series.push({ label: ssp, color, x, lo: g.map((r) => Math.min(...ps.map((p) => r[p]))), hi: g.map((r) => Math.max(...ps.map((p) => r[p]))), bandOpacity: 0.12, noTip: true });
        for (const p of ps) series.push({ label: `${ssp} · ${pctShort(p)}`, color, dash: PCT_DASH[p], x, y: g.map((r) => r[p]) });
      }
      return {
        kind: "line", x: { label: "Year", tickFormat: String, format: String }, y: { label: `${lbl} [${unit}]` }, series, legend: pctLegend(legend),
        caption: "Percentiles of daily values within each year, pooled over the model ensemble.",
      };
    }
    const proj = rows.filter((r) => r.ssp !== "Baseline");
    if (!proj.length) return { message: "Select a future scenario to compare with the baseline" };
    const base = rows.filter((r) => r.ssp === "Baseline");
    const baseMean = Object.fromEntries(ps.map((p) => [p, mean(base.map((r) => r[p]))]));
    const groups = d3group(proj, (r) => r.ssp);
    const series = [], legend = [];
    for (const ssp of sspOrder.filter((s) => groups.has(s))) {
      const g = groups.get(ssp).sort((a, b) => a.year - b.year);
      const color = scenarioColour(ssp, null, null);
      legend.push({ label: ssp, color });
      for (const p of ps) series.push({ label: `${ssp} · ${pctShort(p)}`, color, dash: PCT_DASH[p], x: g.map((r) => r.year), y: g.map((r) => r[p] - baseMean[p]) });
    }
    return {
      kind: "line", x: { label: "Year", tickFormat: String, format: String }, y: { label: `Change to baseline: ${lbl} [${unit}]` }, series, zeroRule: true,
      legend: pctLegend(legend),
      caption: baseYears.length ? `Change against the ${Math.min(...baseYears)}-${Math.max(...baseYears)} baseline mean.` : "",
    };
  }

  function selectedPeriods() {
    return [...(state.scenarios.includes("Baseline") ? [H.baseline_period] : []), ...state.periods];
  }

  function scenarioSeriesLabel(r) { return `${r.ssp} (${r.period})`; }

  function monthlySpec(data) {
    const v = state.variable, scen = state.scenarios, ps = pcts(), periods = selectedPeriods();
    const rows = data.month.filter((r) => r.variable === v && (r.ssp === "Baseline" || scen.includes(r.ssp)) && (r.period === H.baseline_period || periods.includes(r.period)));
    if (!rows.length) return { message: "No hydrology model data for this subbasin" };
    const lbl = variableLabel(v), unit = unitOf(v);
    const order = (a, b) => (a.ssp === "Baseline" ? -1 : b.ssp === "Baseline" ? 1 : a.period.localeCompare(b.period) || a.ssp.localeCompare(b.ssp));
    if (state.plotType === "Absolute") {
      const plotRows = rows.filter((r) => scen.includes(r.ssp) && periods.includes(r.period));
      if (!plotRows.length) return { message: "No data for the selected scenarios/periods" };
      const groups = d3group(plotRows, scenarioSeriesLabel);
      const keys = [...groups.keys()].sort((a, b) => order(groups.get(a)[0], groups.get(b)[0]));
      const series = [], legend = [];
      for (const key of keys) {
        const g = groups.get(key).sort((a, b) => a.month - b.month);
        const color = scenarioColour(g[0].ssp, g[0].period, H.periods);
        legend.push({ label: key, color });
        const x = g.map((r) => r.month);
        series.push({ label: key, color, x, lo: g.map((r) => Math.min(...ps.map((p) => r[p]))), hi: g.map((r) => Math.max(...ps.map((p) => r[p]))), bandOpacity: 0.12, noTip: true });
        for (const p of ps) series.push({ label: `${key} · ${pctShort(p)}`, color, dash: PCT_DASH[p], x, y: g.map((r) => r[p]) });
      }
      return {
        kind: "line", x: { label: "Month", ...monthTicks, format: monthName }, y: { label: `${lbl} [${unit}]` }, series, legend: pctLegend(legend),
        caption: "Percentiles of daily values in each calendar month, pooled over the period and model ensemble.",
      };
    }
    const projRows = rows.filter((r) => r.ssp !== "Baseline" && periods.includes(r.period));
    if (!projRows.length) return { message: "Select a future scenario to compare with the baseline" };
    const base = new Map(rows.filter((r) => r.ssp === "Baseline").map((r) => [r.month, r]));
    const groups = d3group(projRows, scenarioSeriesLabel);
    const keys = [...groups.keys()].sort((a, b) => order(groups.get(a)[0], groups.get(b)[0]));
    const series = [], legend = [];
    for (const key of keys) {
      const g = groups.get(key).sort((a, b) => a.month - b.month);
      const color = scenarioColour(g[0].ssp, g[0].period, H.periods);
      legend.push({ label: key, color });
      for (const p of ps) series.push({ label: `${key} · ${pctShort(p)}`, color, dash: PCT_DASH[p], x: g.map((r) => r.month), y: g.map((r) => (base.has(r.month) ? r[p] - base.get(r.month)[p] : NaN)) });
    }
    return {
      kind: "line", x: { label: "Month", ...monthTicks, format: monthName }, y: { label: `Change to baseline: ${lbl} [${unit}]` }, series, zeroRule: true,
      legend: pctLegend(legend), caption: `Change against the ${H.baseline_period} baseline.`,
    };
  }

  function distributionSpec(data) {
    const v = state.variable, scen = state.scenarios, periods = selectedPeriods();
    const rows = data.month.filter((r) => r.variable === v && scen.includes(r.ssp) && periods.includes(r.period));
    if (!rows.length) return { message: "No hydrology model data for this subbasin" };
    const groups = d3group(rows, scenarioSeriesLabel);
    const keys = [...groups.keys()].sort((a, b) => {
      const ra = groups.get(a)[0], rb = groups.get(b)[0];
      return ra.ssp === "Baseline" ? -1 : rb.ssp === "Baseline" ? 1 : ra.period.localeCompare(rb.period) || ra.ssp.localeCompare(rb.ssp);
    });
    const series = keys.map((key) => {
      const g = groups.get(key);
      return { label: key, color: scenarioColour(g[0].ssp, g[0].period, H.periods), x: PROBS, y: pooledPercentiles(g, PROBS.map((p) => p / 100)) };
    });
    return {
      kind: "line", x: { label: "Cumulative frequency [%]", domain: [0, 100], ...cfTicks, format: (p) => `${p}%` },
      y: { label: `${variableLabel(v)} [${unitOf(v)}]`, type: "log" }, series,
      caption: "Daily values pooled over all months of the period and the model ensemble.",
    };
  }

  // ---- chemical projections (subbasin daily, or water body monthly) ----
  async function chemSeries() {
    const key = chemKeyOf(state.variable);
    const stats = ["mean", "median", "90th"].filter((s) => state.chemStats.includes(s));
    if (wbMode()) {
      const W = await wbchem();
      const x = W.dates.map((d) => d.getTime());
      return { monthly: true, x, stats: stats.map((s) => ({ stat: s, values: [W.series(key, s, state.deWb)].filter(Boolean).map((a) => Array.from(a)) })) };
    }
    const sites = await chemSubbasin(state.sb);
    if (!sites.length) return null;
    const { start, n } = sites[0];
    const x = Array.from({ length: n }, (_, i) => (start + i) * DAY);
    return { monthly: false, x, nSites: sites.length, stats: stats.map((s) => ({ stat: s, values: sites.map((site) => Array.from(site.series(key, s) || [])) })) };
  }

  function drawChemProjection() {
    if (wbMode()) {
      if (!D.has("wbchem")) return proj.message("Modelled water body data not published");
      if (!state.deWb) return proj.message("Search for a water body in the sidebar");
    } else {
      if (!D.has("chem_daily")) return proj.message("Chemical predictions not published");
      if (state.sb == null) return proj.message("Pick a subbasin on the map");
      if (!D.chemSitesBySb.has(state.sb)) return proj.message("No chemical data for this subbasin", "Chemical predictions cover 507 subbasins; the map's “Chemical data coverage” layer shows which.");
    }
    const tab = state.projTab, lbl = variableLabel(state.variable);
    proj.load(async () => {
      const cs = await chemSeries();
      if (!cs || !cs.stats.some((s) => s.values.length)) return { message: "No chemical data here" };
      const ylab = `${lbl} [ng/L]`;
      const dates = cs.x.map((t) => new Date(t));
      const record = cs.monthly ? "monthly" : "daily";
      const siteNote = cs.nSites > 1 ? ` Pooled over the ${cs.nSites} modelling sites in this subbasin.` : "";
      const statSeries = (fn) => cs.stats.map(({ stat, values }) => ({ label: STAT_LABEL[stat], color: STAT_COLOUR[stat], ...fn(values) }));
      if (tab === "Yearly") {
        const years = [...new Set(dates.map((d) => d.getUTCFullYear()))].sort();
        const series = statSeries((values) => ({
          x: years, y: years.map((yr) => mean(values.flatMap((arr) => arr.filter((_, i) => dates[i].getUTCFullYear() === yr)))), points: true,
        }));
        return { kind: "line", x: { label: "Year", ticks: years, tickFormat: String, format: String }, y: { label: ylab }, series, directLabels: series.length > 1,
          caption: `Annual mean of the ${record} 2018-2022 record.${siteNote}` };
      }
      if (tab === "Monthly") {
        const months = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12];
        const series = statSeries((values) => {
          const byMonth = months.map((m) => values.flatMap((arr) => arr.filter((v, i) => dates[i].getUTCMonth() + 1 === m && Number.isFinite(v))));
          return { x: months, y: byMonth.map(mean), lo: byMonth.map((a) => (a.length ? Math.min(...a) : NaN)), hi: byMonth.map((a) => (a.length ? Math.max(...a) : NaN)) };
        });
        return { kind: "line", x: { label: "Month", ...monthTicks, format: monthName }, y: { label: ylab }, series, directLabels: series.length > 1,
          caption: `Monthly climatology across 2018-2022; band = range across ${record} values.${siteNote}` };
      }
      if (tab === "Distributions") {
        const series = statSeries((values) => {
          const sorted = Float64Array.from(values.flat().filter(Number.isFinite)).sort();
          return { x: PROBS, y: quantilesSorted(sorted, PROBS.map((p) => p / 100)) };
        });
        return { kind: "line", x: { label: "Cumulative frequency [%]", domain: [0, 100], ...cfTicks, format: (p) => `${p}%` }, y: { label: ylab, type: "log" }, series,
          directLabels: series.length > 1, caption: `Empirical distribution of the ${record} values.${siteNote}` };
      }
      // Daily
      const series = statSeries((values) => ({
        x: cs.x, width: 1.25, y: cs.x.map((_, i) => mean(values.map((arr) => arr[i]))),
      }));
      return { kind: "line", x: { type: "utc" }, y: { label: ylab }, series,
        caption: cs.monthly ? "Monthly modelled concentration, 2018-2022." : `Daily reconstructed concentration, 2018-2022.${cs.nSites > 1 ? ` Mean of the ${cs.nSites} modelling sites in this subbasin.` : ""}` };
    });
  }

  // ---- download this selection ----
  async function downloadSelection() {
    const v = state.variable;
    if (isChemVar(v)) {
      const key = chemKeyOf(v);
      const label = variableLabel(v);
      const rows = [];
      if (wbMode()) {
        if (!state.deWb || !D.has("wbchem")) return;
        const W = await wbchem();
        for (const s of ["mean", "median", "90th"].filter((x) => state.chemStats.includes(x))) {
          const arr = W.series(key, s, state.deWb);
          if (arr) W.dateStrings.forEach((d, i) => rows.push({ water_body: state.deWb, chemical: label, statistic: s, date: d, value: arr[i], unit: "ng/L" }));
        }
        saveFile(`ecomix_waterbody_${state.deWb}_${key}.csv`, toCSV(rows), "text/csv");
      } else {
        if (state.sb == null || !D.chemSitesBySb.has(state.sb)) return;
        const sites = await chemSubbasin(state.sb);
        for (const site of sites) {
          for (const s of ["mean", "median", "90th"].filter((x) => state.chemStats.includes(x))) {
            const arr = site.series(key, s);
            if (!arr) continue;
            for (let i = 0; i < site.n; i++) rows.push({ subbasin: state.sb, site_id: site.site, chemical: label, statistic: s, date: isoDay(site.start + i), value: arr[i], unit: "ng/L" });
          }
        }
        saveFile(`ecomix_subbasin_${state.sb}_${key}.csv`, toCSV(rows), "text/csv");
      }
      return;
    }
    if (!D.has("hype_proj") || state.sb == null || !subbasin(state.sb) || !subbasin(state.sb).hype) return;
    const data = await hype(state.sb);
    const unit = (D.hypeVar.get(v) || {}).unit;
    const rows = data.month.filter((r) => r.variable === v).map((r) => ({ subbasin: state.sb, scenario: r.ssp, period: r.period, month: r.month, value: round3(r.p50), unit }));
    saveFile(`ecomix_subbasin_${state.sb}_${v}.csv`, toCSV(rows), "text/csv");
  }

  function update() {
    if (normalise()) return;
    syncControls();
    dlBtn.disabled = wbMode() ? !state.deWb : state.sb == null;
    drawClimate();
    drawObs();
    drawProjections();
  }

  return {
    id: "hydro", title: "Hydro Explorer", el: page,
    deps: ["sb", "deMode", "deWb", "variable", "scenarios", "periods", "pct", "chemStats", "plotType", "climateVar", "climateRes", "obsVar", "projTab"],
    update,
  };
}

// ---- helpers ----------------------------------------------------------------

function d3group(rows, key) {
  const m = new Map();
  for (const r of rows) { const k = key(r); if (!m.has(k)) m.set(k, []); m.get(k).push(r); }
  return m;
}

function mean(values) {
  let s = 0, n = 0;
  for (const v of values) if (v != null && Number.isFinite(v)) { s += v; n++; }
  return n ? s / n : NaN;
}

const round3 = (v) => (Number.isFinite(v) ? Math.round(v * 1000) / 1000 : null);
const isoDay = (k) => dayToDate(k).toISOString().slice(0, 10);

// Distribution of daily values pooled over the months of one scenario x
// period: each month linear between its percentile knots, weighted by days
// in the month (port of app.R pooled_percentiles(); the same calculation
// scripts/import_database_update.py uses for the Spatial Datasets extremes).
export function pooledPercentiles(monthRows, probsOut) {
  const keys = Object.keys(PCT_PROB);
  const probs = keys.map((k) => PCT_PROB[k]);
  const q = monthRows.map((r) => keys.map((k) => r[k]).sort((a, b) => a - b));
  let w = monthRows.map((r) => MONTH_DAYS[r.month - 1]);
  const ws = w.reduce((a, b) => a + b, 0);
  w = w.map((x) => x / ws);
  const nk = probs.length;
  const cdfAt = (x, left) => {
    let total = 0;
    for (let m = 0; m < q.length; m++) {
      let k = 0;
      for (const v of q[m]) if (left ? v < x : v <= x) k++;
      let f;
      if (k === 0) f = 0;
      else if (k === nk) f = 1;
      else f = probs[k - 1] + ((x - q[m][k - 1]) / (q[m][k] - q[m][k - 1])) * (probs[k] - probs[k - 1]);
      total += w[m] * f;
    }
    return total;
  };
  const knots = [...new Set(q.flat())].sort((a, b) => a - b);
  const pts = [];
  for (const kv of knots) { pts.push([cdfAt(kv, true), kv]); pts.push([cdfAt(kv, false), kv]); }
  pts.sort((a, b) => a[0] - b[0] || a[1] - b[1]);
  // approx(..., ties = min): collapse equal f to the smallest v.
  const fs = [], vs = [];
  for (const [f, v] of pts) {
    if (fs.length && fs[fs.length - 1] === f) continue;
    fs.push(f); vs.push(v);
  }
  return probsOut.map((p) => {
    if (p <= fs[0]) return vs[0];
    if (p >= fs[fs.length - 1]) return vs[vs.length - 1];
    let i = 1;
    while (fs[i] < p) i++;
    return vs[i - 1] + ((p - fs[i - 1]) / (fs[i] - fs[i - 1])) * (vs[i] - vs[i - 1]);
  });
}
