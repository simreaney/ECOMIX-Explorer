// Chemical Explorer: D3D-modelled daily concentration and ssPAF (10th-90th
// percentile band + median) for one chemical at one site, with acute/chronic
// level-of-concern lines and source apportionment treemaps.

import { D, d3d, d3dIndex, ecorisk } from "../data.js";
import { state, set, on } from "../state.js";
import { el, field, select, seg, combo, setChildren, unpublished } from "../ui.js";
import { EC, SERIES, DAY, fmtDate, quantile } from "../theme.js";
import { Chart } from "../chart.js";

const SRC_STYLE = { dog_swim: [EC.accent, "#ffffff"], pet_CSO: [EC.ink, "#ffffff"], pet_WWTP: ["#9b9797", EC.ink] };

export function create() {
  const meta = D.meta.d3d;
  const chemicals = meta.chemicals;

  const chemSelect = select({ options: chemicals.length ? chemicals.map((c) => ({ value: c.key, label: c.label })) : [{ value: "", label: "Not published", disabled: true }], onChange: (v) => set({ d3dChem: v }) });
  let siteChoices = [];
  const siteSearch = combo({ placeholder: "Search water body", getItems: () => siteChoices, max: 600, onPick: (it) => set({ d3dSite: it.value }) });
  const grouping = seg({ options: [{ value: "daily", label: "Daily" }, { value: "weekly", label: "Weekly" }, { value: "monthly", label: "Monthly" }], value: state.d3dGroup, onChange: (v) => set({ d3dGroup: v }) });
  const aggStat = seg({ options: [{ value: "median", label: "Median" }, { value: "p90", label: "90th percentile" }], value: state.d3dStat, onChange: (v) => set({ d3dStat: v }) });
  const aggField = field("Aggregate using", aggStat.el);
  const note = el("div");

  const sidebar = el("aside.sidebar", { style: { width: "270px" } },
    el("div.sidebar-title", { text: "Controls" }),
    field("Chemical", chemSelect.el), field("Water body", siteSearch.el), el("hr.hr"),
    field("Time step", grouping.el), aggField, note);

  const thresholdNote = el("span.panel-note");
  const concBody = el("div.card-body");
  const conc = new Chart(concBody);
  const sspafBody = el("div.card-body");
  const sspaf = new Chart(sspafBody);
  const concCard = el("section.card", {}, el("div.card-header", {}, el("span.title", { text: "Concentration · 10th–90th percentile range" }), thresholdNote, el("span.card-tools", {}, conc.tableToggle())), concBody);
  const sspafCard = el("section.card", {}, el("div.card-header", {}, el("span.title", { text: "ssPAF (potentially affected fraction of species) · 10th–90th percentile range" }), el("span.card-tools", {}, sspaf.tableToggle())), sspafBody);

  const avgTitle = el("span.title", { text: "Average source apportionment" });
  const dayTitle = el("span.title", { text: "Selected day · hover a graph" });
  const avgBox = el("div.treemap"), dayBox = el("div.treemap");
  const avgChart = new Chart(avgBox), dayChart = new Chart(dayBox);
  const side = el("div.side", {},
    el("section.card", {}, el("div.card-header", {}, avgTitle), el("div.card-body", {}, avgBox)),
    el("section.card", {}, el("div.card-header", {}, dayTitle), el("div.card-body", {}, dayBox)));

  const page = el("div.page", {}, el("div.layout", {}, sidebar, el("div.main", {}, el("div.row-chem", {}, el("div.col", {}, concCard, sspafCard), side))));

  let index = null, raw = null, rawKey = null, grouped = null, thresholds = null, hoverDay = null;
  let pendingCarry = null;

  // Carry a Map / Site Details selection over when this tab is opened; an
  // explicit site (a popup link) wins.
  on(["sb"], () => { if (state.sb != null) pendingCarry = { type: "subbasin", value: state.sb }; });
  on(["wb"], () => { if (state.wb) pendingCarry = { type: "waterbody", value: state.wb }; });
  on(["d3dSite"], () => { pendingCarry = null; });

  function siteLabelList(chem) {
    const sites = index && index[chem] ? index[chem].sites : [];
    const names = sites.map((id) => {
      const s = D.chemSite.get(Number(id));
      return s && s.wb_name ? s.wb_name : `Site ${id}`;
    });
    const count = new Map();
    names.forEach((n) => count.set(n, (count.get(n) || 0) + 1));
    return sites.map((id, i) => ({ value: id, label: count.get(names[i]) > 1 ? `${names[i]} (site ${id})` : names[i] }));
  }

  async function getThresholds(chem, site) {
    if (!D.has("ecorisk")) return null;
    const eco = await ecorisk();
    const entry = Object.entries(eco).find(([k]) => k.toLowerCase() === chem);
    if (!entry) return null;
    const row = entry[1].rows.find((r) => String(r.site_id) === String(site));
    if (!row) return null;
    const na = (v) => v == null || String(v).trim().toLowerCase() === "na" || String(v).trim() === "";
    const ss = !na(row["site_specific hc5"]);
    return {
      acute: Number(ss ? row["site_specific hc5"] : row.generic_hc5),
      chronic: Number(ss ? row["site_specific hc5/10"] : row["generic_hc5_divided by 10"]),
      source: ss ? "site-specific" : "generic",
    };
  }

  function groupSeries(r) {
    const days = Array.from(r.day);
    if (state.d3dGroup === "daily") return { day: days, conc: r.conc.map((a) => Array.from(a)), sspaf: r.sspaf.map((a) => Array.from(a)) };
    // Calendar weeks (starting Monday, like R's cut(x, "week")) or months.
    const binOf = state.d3dGroup === "weekly"
      ? (k) => k - ((((k + 4) % 7) + 6) % 7)
      : (k) => { const d = new Date(k * DAY); return Math.round(Date.UTC(d.getUTCFullYear(), d.getUTCMonth(), 1) / DAY); };
    const bins = [];
    const idx = new Map();
    days.forEach((k, i) => {
      const b = binOf(k);
      if (!idx.has(b)) { idx.set(b, bins.length); bins.push({ day: b, rows: [] }); }
      bins[idx.get(b)].rows.push(i);
    });
    const stat = state.d3dStat === "p90" ? (v) => quantile(v, 0.9) : (v) => quantile(v, 0.5);
    const agg = (arr) => bins.map((b) => stat(b.rows.map((i) => arr[i]).filter(Number.isFinite)));
    return { day: bins.map((b) => b.day), conc: r.conc.map(agg), sspaf: r.sspaf.map(agg) };
  }

  function bandSpec(g, which, opts) {
    const x = g.day.map((k) => k * DAY);
    const [lo, mid, hi] = g[which];
    return {
      kind: "line", x: { type: "utc" }, ...opts,
      marker: hoverDay != null ? hoverDay * DAY : null,
      series: [{ label: "Median (50th percentile)", color: SERIES.red, x, y: mid, lo, hi, width: state.d3dGroup === "daily" ? 1.25 : 2 }],
      onHover: (t) => setHover(Math.round(t / DAY)),
      onLeave: () => { if (hoverDay != null) drawSeries(); },
    };
  }

  function drawSeries() {
    if (!grouped) return;
    const ds = chemicals.find((c) => c.key === state.d3dChem);
    const rules = thresholds ? [
      { y: thresholds.acute, dash: "6 4", label: "Acute HC5" },
      { y: thresholds.chronic, dash: "2 3", label: "Chronic HC5/10" },
    ] : [];
    conc.draw(bandSpec(grouped, "conc", {
      y: { type: "log", label: `${ds.label} concentration [${ds.unit}, log scale]` }, rules,
      legend: [{ label: "Median", color: SERIES.red }, { kind: "rect", label: "10th–90th percentile", color: SERIES.red, opacity: 0.25 }, ...rules.map((r) => ({ label: r.label, color: EC.ink, dash: r.dash }))],
      caption: "Gaps mark exact-zero days (undefined on a log scale).",
    }));
    sspaf.draw(bandSpec(grouped, "sspaf", {
      y: { label: `${ds.label} ssPAF`, zero: true },
      legend: [{ label: "Median", color: SERIES.red }, { kind: "rect", label: "10th–90th percentile", color: SERIES.red, opacity: 0.25 }],
    }));
  }

  // ---- source apportionment ----
  function srcShares(rowsFilter) {
    const src = raw && raw.src;
    if (!src) return null;
    const v = src.shares.map((arr) => {
      let s = 0, n = 0;
      for (let i = 0; i < arr.length; i++) if (rowsFilter(i) && arr[i] !== 65535) { s += arr[i] / 65534; n++; }
      return n ? s / n : NaN;
    });
    const total = v.reduce((a, b) => a + b, 0);
    if (v.some((x) => !Number.isFinite(x)) || !(total > 0)) return null;
    return src.keys.map((k, i) => ({ key: k, value: v[i] / total }));
  }

  function treemapSpec(shares, empty) {
    if (!shares) return { message: empty };
    const items = shares.map((s) => ({ label: meta.src_labels[s.key] || s.key, value: s.value, color: SRC_STYLE[s.key][0], text: SRC_STYLE[s.key][1] }));
    return { kind: "treemap", items, caption: items.slice().sort((a, b) => b.value - a.value).map((d) => `${d.label} ${(100 * d.value).toFixed(1)}%`).join(" · ") };
  }

  let hoverFrame = 0;
  function setHover(k) {
    if (!raw || !raw.src) return;
    const first = raw.src.start, last = raw.src.start + raw.src.n - 1;
    k = Math.min(Math.max(k, first), last);
    if (k === hoverDay) return;
    hoverDay = k;
    cancelAnimationFrame(hoverFrame);
    hoverFrame = requestAnimationFrame(drawDayTreemap);
  }

  function drawDayTreemap() {
    if (!raw || !raw.src) return;
    if (hoverDay == null) {
      dayTitle.textContent = "Selected day · hover a graph";
      dayChart.message("Hover either time series to pick a day");
      return;
    }
    dayTitle.textContent = `Source apportionment · ${fmtDate(new Date(hoverDay * DAY))}`;
    const i = hoverDay - raw.src.start;
    dayChart.draw(treemapSpec(srcShares((j) => j === i), "No source apportionment for this day"));
  }

  async function update(changed) {
    if (!D.has("d3d") || !chemicals.length) {
      chemSelect.value = "";
      setChildren(note, unpublished("D3D chemical percentile series"));
      conc.message("Chemical percentile series not published", "The D3D concentration/ssPAF files (data-chem/7_chemicals) were not available when this site was built.");
      sspaf.message("Chemical percentile series not published");
      side.hidden = true;
      return;
    }
    index = index || await d3dIndex();
    if (!state.d3dChem || !index[state.d3dChem]) { set({ d3dChem: chemicals[0].key }); return; }
    chemSelect.value = state.d3dChem;
    grouping.value = state.d3dGroup;
    aggStat.value = state.d3dStat;
    aggField.hidden = state.d3dGroup === "daily";
    siteChoices = siteLabelList(state.d3dChem);
    const valid = siteChoices.some((c) => c.value === state.d3dSite);
    if (!valid) { set({ d3dSite: siteChoices.length ? siteChoices[0].value : null }); return; }
    siteSearch.text = (siteChoices.find((c) => c.value === state.d3dSite) || {}).label || "";

    const key = `${state.d3dChem}|${state.d3dSite}`;
    if (key !== rawKey) {
      hoverDay = null;
      conc.el.classList.add("is-stale"); sspaf.el.classList.add("is-stale");
      const [r, thr] = await Promise.all([d3d(state.d3dChem, state.d3dSite), getThresholds(state.d3dChem, state.d3dSite)]);
      if (`${state.d3dChem}|${state.d3dSite}` !== key) return;
      raw = r; rawKey = key; thresholds = thr;
    }
    thresholdNote.textContent = thresholds ? `${thresholds.source} HC5 thresholds` : "no threshold data for this chemical/site";
    if (!raw) { conc.message("No data for this site"); sspaf.message("No data for this site"); side.hidden = true; return; }
    grouped = groupSeries(raw);
    drawSeries();
    side.hidden = !raw.src;
    if (raw.src) {
      avgChart.draw(treemapSpec(srcShares(() => true), "No source apportionment for this site"));
      drawDayTreemap();
    }
  }

  return {
    id: "chem", title: "Chemical Explorer", el: page,
    deps: ["d3dChem", "d3dSite", "d3dGroup", "d3dStat"],
    update,
    onShow() {
      if (!pendingCarry || !D.has("d3d")) return;
      const sel = pendingCarry;
      pendingCarry = null;
      const hits = sel.type === "subbasin"
        ? D.meta.chem.sites.filter((s) => s.subbasin === sel.value).map((s) => String(s.site))
        : D.meta.chem.sites.filter((s) => s.wb === sel.value).map((s) => String(s.site));
      const avail = index && state.d3dChem && index[state.d3dChem] ? new Set(index[state.d3dChem].sites) : null;
      const hit = hits.find((h) => !avail || avail.has(h));
      if (hit) { set({ d3dSite: hit }); pendingCarry = null; }
    },
  };
}
