// Ecological Risk: detail for one site/chemical, reached from a point on the
// Map's Ecological risk layer (or picked here).

import { D, ecorisk } from "../data.js";
import { state, set } from "../state.js";
import { el, field, select, combo, setChildren, unpublished } from "../ui.js";
import { EC, RISK_COLOUR, fmtNum } from "../theme.js";
import { riskFlagLines } from "./map.js";
import { go } from "../common.js";

export function create() {
  const chemSelect = select({ options: [], onChange: (v) => set({ eco: `${v}||${currentSite() || ""}` }) });
  let sites = [];
  const siteSearch = combo({ placeholder: "Search site id or water body", getItems: () => sites, max: 600, onPick: (it) => set({ eco: `${currentChem()}||${it.value}` }) });
  const note = el("div");
  const sidebar = el("aside.sidebar", { style: { width: "270px" } },
    el("div.sidebar-title", { text: "Site" }), field("Chemical", chemSelect.el), field("Site", siteSearch.el), note,
    el("p.help", { text: "Or click a point on the Map's Ecological risk layer." }));
  const body = el("div.card-body.detail");
  const page = el("div.page", {}, el("div.layout", {}, sidebar, el("div.main", {},
    el("section.card", { style: { flex: "1 1 auto" } }, el("div.card-header", {}, el("span.title", { text: "Ecological risk · site detail" })), body))));

  let eco = null;
  const parts = () => (state.eco || "").split("||");
  const currentChem = () => parts()[0] || (eco ? Object.keys(eco)[0] : "");
  const currentSite = () => parts()[1] || null;

  // Units stay out of the uppercase label style (an uppercased µ reads as M).
  const statLabel = (label) => {
    const m = label.match(/^(.*)\((.*)\)$/);
    return m ? el("div.ctx-label", { title: label }, m[1], el("span.nocase", { text: `(${m[2]})` })) : el("div.ctx-label", { text: label });
  };
  const stat = (label, value, style) => el("div.ctx-stat", {}, statLabel(label), el("div.ctx-value", { text: value, style }));

  async function update() {
    if (!D.has("ecorisk")) {
      setChildren(note, unpublished("Ecological risk data"));
      setChildren(body, el("div.chart-msg", { text: "Ecological risk data not published" }));
      return;
    }
    eco = eco || await ecorisk();
    chemSelect.setOptions(Object.entries(eco).map(([k, v]) => ({ value: k, label: v.label })), false);
    const chem = currentChem();
    chemSelect.value = chem;
    const entry = eco[chem];
    sites = entry ? entry.rows.map((r) => {
      const s = D.chemSite.get(Number(r.site_id));
      return { value: String(r.site_id), label: `${r.site_id} · ${s && s.wb_name ? s.wb_name : `subbasin ${r.subbasin}`}` };
    }) : [];
    const site = currentSite();
    const row = entry && site ? entry.rows.find((r) => String(r.site_id) === site) : null;
    siteSearch.text = row ? (sites.find((s) => s.value === site) || {}).label : "";
    if (!row) {
      setChildren(body, el("div.chart-msg", { text: "Select a point on the Map's Ecological risk layer, or pick a site in the sidebar, to see its detail here." }));
      return;
    }
    const num = (v) => fmtNum(Number(v), 3);
    const hc5 = (v) => (v == null || String(v).trim().toLowerCase() === "na" ? "Not available" : num(v));
    const s = D.chemSite.get(Number(row.site_id));
    const d3dHas = D.has("d3d") && D.meta.d3d.chemicals.some((c) => c.key === chem.toLowerCase());
    setChildren(body,
      el("div.ctx-stats", {},
        stat("Chemical", entry.label), stat("Site id", String(row.site_id)), stat("Subbasin", String(row.subbasin)),
        el("div.ctx-stat", {}, el("div.ctx-label", { text: "Risk level" }),
          el("div.ctx-value", {}, el("span", { style: { display: "inline-block", width: "14px", height: "14px", background: RISK_COLOUR[row.risk_level], marginRight: "8px", verticalAlign: "-1px", border: "1px solid rgba(32,30,29,.25)" } }), row.risk_level))),
      el("div.ctx-stats", {}, stat("Water body", s && s.wb_name ? s.wb_name : "—", { fontSize: "14px", whiteSpace: "normal" }), stat("Easting", String(row.EASTING)), stat("Northing", String(row.NORTHING)), stat("Days of data", String(row.n_days))),
      el("div.ctx-stats", {},
        stat("Median 90th-pctile conc. (µg/L)", num(row.median_of_p90_ug)), stat("95th-pctile of 90th-pctile conc. (µg/L)", num(row.p95_of_p90_ug)),
        stat("Median conc. (µg/L)", num(row.median_of_median_ug)), stat("95th-pctile of median conc. (µg/L)", num(row.p95_of_median_ug))),
      el("div.ctx-stats", {},
        stat("Generic HC5", num(row.generic_hc5)), stat("Generic HC5/10", num(row["generic_hc5_divided by 10"])),
        stat("Site-specific HC5", hc5(row["site_specific hc5"])), stat("Site-specific HC5/10", hc5(row["site_specific hc5/10"]))),
      el("h6", { text: "Threshold comparisons" }),
      riskFlagLines(row).map((l) => el("div.flag", { text: l })),
      el("div", { style: { margin: "16px", display: "flex", gap: "16px" } },
        el("a", { href: "#", text: "Show on the map →", onclick: (e) => { e.preventDefault(); go("map", { sb: row.subbasin }); } }),
        d3dHas ? el("a", { href: "#", text: "View chemical time series →", onclick: (e) => { e.preventDefault(); go("chem", { d3dChem: chem.toLowerCase(), d3dSite: String(row.site_id) }); } }) : null));
  }

  return { id: "eco", title: "Ecological Risk", el: page, deps: ["eco"], update };
}
