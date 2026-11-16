// Map: find a subbasin, toggle the study layers, jump to the detail pages.

import { D, geo, measured, ecorisk, subbasin, subbasinFeature } from "../data.js";
import { state, set, on } from "../state.js";
import { el, field, select, combo, rail, link, setChildren } from "../ui.js";
import { EC, RISK_COLOUR, RISK_LEVELS, fmtNum } from "../theme.js";
import { makeMap, setBasemap, packLayer, fitBbox, popupBody, legendControl, BASEMAPS } from "../mapkit.js";
import { subbasinCtx, go } from "../common.js";

const L = () => window.L;

const LAYERS = [
  ["subbasins", "Subbasins with hydrology"],
  ["opcat", "Operational catchment boundaries"],
  ["waterbodies", "Waterbodies (modelled)"],
  ["measured_sites", "Measured sites"],
  ["observed_hydro", "Observed hydrology sites"],
  ["chem_coverage", "Chemical data coverage"],
  ["eco_risk", "Ecological risk"],
];

// Draw order, bottom to top.
const ORDER = ["subbasins", "chem_coverage", "waterbodies", "opcat", "selection", "observed_hydro", "measured_sites", "eco_risk"];

export function create() {
  const ctx = subbasinCtx({ map: false, action: link("Open in explorer →", () => go("hydro")) });

  // ---- sidebar ----
  const ids = D.meta.subbasins.id.map((id) => ({ label: String(id), value: id }));
  const search = combo({
    placeholder: "Search subbasin id",
    getItems: () => ids,
    onPick: (it) => { set({ sb: it.value }); zoomToSelection = true; },
  });

  const active = new Set(["subbasins"]);
  const checks = el("div.checks", {}, LAYERS.map(([key, label]) => el("label", {},
    el("input", { type: "checkbox", checked: active.has(key) || null, onchange: (e) => { e.target.checked ? active.add(key) : active.delete(key); syncLayers(); } }),
    el("span", { text: label }))));

  const ecoSelect = select({ options: [], onChange: () => { if (active.has("eco_risk")) drawEcoRisk(); } });
  const ecoField = field("Ecological risk · chemical", ecoSelect.el);
  ecoField.hidden = true;

  const basemap = select({ options: BASEMAPS, value: "osm", onChange: (v) => setBasemap(map, v) });

  const opcatRail = rail({ onPick: (name) => { const o = D.opcat.get(name); if (o && o.bbox) fitBbox(map, o.bbox, 12); } });
  opcatRail.setItems(D.meta.opcats.slice().sort((a, b) => b.n - a.n).map((o) => ({ id: o.name, label: o.name, swatch: o.colour, num: o.n })));

  const sidebar = el("aside.sidebar", { style: { width: "320px" } },
    el("div.sidebar-title", { text: "Find a location" }),
    search.el,
    el("div", {}, el("span.label", { text: "Layers" }), checks),
    ecoField,
    field("Basemap", basemap.el),
    el("div.fill", {}, el("div.rail-head", { text: `Operational catchments · ${D.meta.opcats.length}` }), opcatRail.el));

  // ---- map ----
  const mapBox = el("div");
  const status = el("div.map-status", { hidden: true });
  const main = el("div.main", {}, el("div.map-wrap", {}, mapBox, status,
    el("div.map-note", { text: "Data compiled by York, Durham and Sheffield Universities (2026). Operational catchment boundaries and waterbodies © Environment Agency copyright and/or database right 2026, licensed under the Open Government Licence v3.0" })));
  const page = el("div.page", {}, ctx.el, el("div.layout", {}, sidebar, main));

  const map = makeMap(mapBox);
  const layers = {};
  let ecoLegend = null;
  let zoomToSelection = false;

  const restack = () => {
    for (const key of ORDER) if (layers[key] && map.hasLayer(layers[key])) layers[key].bringToFront();
  };

  const busy = async (msg, fn) => {
    status.textContent = msg; status.hidden = false;
    try { return await fn(); } finally { status.hidden = true; }
  };

  const openHydro = (id) => go("hydro", { sb: id, deMode: "subbasin" });

  // ---- layer builders (each built once, on first use) ----

  const build = {
    async subbasins() {
      const g = await geo("subbasins_hype");
      return packLayer(map, g, [...Array(g.n).keys()], (i) => {
        const s = subbasin(g.props.id[i]);
        const colour = s && s.opcat ? D.opcat.get(s.opcat).colour : "#e0dede";
        return { fillColor: colour, fillOpacity: 0.55, color: EC.ink, opacity: 0.45, weight: 1 };
      }).on("click", (e) => set({ sb: g.props.id[e.layer.feature.properties.i] }))
        .bindPopup((layer) => {
          const id = g.props.id[layer.feature.properties.i];
          const s = subbasin(id);
          return popupBody(`Subbasin ${id}`, [`Operational catchment: ${(s && s.opcat) || "None"}`], [["Open in Hydro Explorer →", () => openHydro(id)]]);
        });
    },
    async opcat() {
      const g = await geo("opcats");
      return packLayer(map, g, [...Array(g.n).keys()], () => ({ fill: false, color: EC.ink, weight: 2.5, opacity: 0.9 }))
        .bindTooltip((layer) => g.props.name[layer.feature.properties.i], { sticky: true });
    },
    async waterbodies() {
      const g = await geo("waterbodies");
      return packLayer(map, g, [...Array(g.n).keys()], (i) => {
        const w = D.wb.get(g.props.id[i]);
        const colour = !w || !w.model ? EC.ruleLight : w.opcat && D.opcat.get(w.opcat) ? D.opcat.get(w.opcat).colour : "#e0dede";
        return { fillColor: colour, fillOpacity: w && w.model ? 0.45 : 0.2, color: EC.ink, weight: 0.75, opacity: 0.5 };
      }).bindTooltip((layer) => {
        const w = D.wb.get(g.props.id[layer.feature.properties.i]);
        return `${w.name}${w.opcat ? ` · ${w.opcat}` : ""}${w.model ? "" : " (no modelled data)"}`;
      }, { sticky: true }).bindPopup((layer) => {
        const w = D.wb.get(g.props.id[layer.feature.properties.i]);
        return w.model
          ? popupBody(w.name, [w.opcat], [["Open in Site Details →", () => go("site", { wb: w.id, siteMode: "modelled" })]])
          : popupBody(w.name, [el("em", { text: "No modelled chemical data available" })]);
      });
    },
    async measured_sites() {
      const m = await measured();
      return L().featureGroup(m.sites.map((s) => L().circleMarker([s.lat, s.lon], {
        renderer: map._ecRenderer, radius: 5, color: "#ffffff", weight: 1.5, fillColor: EC.ink, fillOpacity: 0.95,
      }).bindTooltip(s.name).bindPopup(() => popupBody(s.name, [], [["Open in Site Details →", () => go("site", { site: s.id, siteMode: "measured" })]]))));
    },
    async observed_hydro() {
      return L().featureGroup(D.meta.obs_points.map((p) => L().circleMarker([p.lat, p.lon], {
        renderer: map._ecRenderer, radius: 5, color: EC.ink, weight: 1.5, fillColor: "#0f6659", fillOpacity: 0.9,
      }).bindPopup(() => popupBody(`Subbasin ${p.id}`, [`Observed variables: ${p.vars.join(", ")}`], [["Open in Hydro Explorer →", () => openHydro(p.id)]]))));
    },
    async chem_coverage() {
      const [hype, chem] = await Promise.all([geo("subbasins_hype"), geo("subbasins_chem")]);
      const style = () => ({ fillColor: "#2f6690", fillOpacity: 0.22, color: "#2f6690", weight: 1.5, opacity: 0.85 });
      const group = L().featureGroup();
      for (const g of [hype, chem]) {
        const idx = [];
        g.props.id.forEach((id, i) => { if (D.chemSitesBySb.has(id)) idx.push(i); });
        packLayer(map, g, idx, style)
          .bindTooltip((layer) => `Subbasin ${g.props.id[layer.feature.properties.i]} · chemical data available`, { sticky: true })
          .bindPopup((layer) => {
            const id = g.props.id[layer.feature.properties.i];
            return popupBody(`Subbasin ${id}`, [], [["Open in Hydro Explorer →", () => openHydro(id)]]);
          }).addTo(group);
      }
      return group;
    },
  };

  async function ensure(key) {
    if (!layers[key]) layers[key] = await busy("Loading layer…", build[key]);
    return layers[key];
  }

  async function syncLayers() {
    ecoField.hidden = !active.has("eco_risk");
    for (const [key] of LAYERS) {
      if (key === "eco_risk") continue;
      if (active.has(key)) { const layer = await ensure(key); if (active.has(key) && !map.hasLayer(layer)) layer.addTo(map); }
      else if (layers[key] && map.hasLayer(layers[key])) map.removeLayer(layers[key]);
    }
    await drawEcoRisk();
    restack();
  }

  // ---- ecological risk layer ----
  let ecoData = null;
  async function drawEcoRisk() {
    if (layers.eco_risk) { map.removeLayer(layers.eco_risk); layers.eco_risk = null; }
    if (ecoLegend) { ecoLegend.remove(); ecoLegend = null; }
    if (!active.has("eco_risk")) return;
    if (!D.has("ecorisk")) return;
    ecoData = ecoData || await ecorisk();
    const chem = ecoSelect.value;
    const entry = ecoData[chem];
    if (!entry) return;
    const d3dChems = new Set(D.meta.d3d.chemicals.map((c) => c.key));
    layers.eco_risk = L().featureGroup(entry.rows.map((r) => L().circleMarker([r.lat, r.lon], {
      renderer: map._ecRenderer, radius: 6, color: "#ffffff", weight: 1.5, fillColor: RISK_COLOUR[r.risk_level], fillOpacity: 0.9,
    }).bindTooltip(`${entry.label} · ${r.risk_level} · Site ${r.site_id}`)
      .bindPopup(() => popupBody(`${entry.label} · ecological risk`, [
        `Site ${r.site_id} · Subbasin ${r.subbasin}`,
        ...riskFlagLines(r),
      ], [
        ["View ecological risk detail →", () => go("eco", { eco: `${chem}||${r.site_id}` })],
        D.has("d3d") && d3dChems.has(chem.toLowerCase())
          ? ["View chemical time series →", () => go("chem", { d3dChem: chem.toLowerCase(), d3dSite: String(r.site_id) })] : null,
      ])))).addTo(map);
    ecoLegend = legendControl("bottomleft");
    ecoLegend.addTo(map);
    setChildren(ecoLegend._div, el("div.legend-title", { text: "Ecological risk" }),
      RISK_LEVELS.map((lv) => el("div.legend-row", {}, el("span.legend-swatch", { style: { background: RISK_COLOUR[lv], borderRadius: "50%" } }), el("span", { text: lv }))));
    restack();
  }

  // ---- selection ----
  async function showSelection() {
    if (layers.selection) { map.removeLayer(layers.selection); layers.selection = null; }
    const id = state.sb;
    opcatRail.setActive(id != null && subbasin(id) ? subbasin(id).opcat : null);
    search.text = id != null ? String(id) : "";
    if (id == null) return;
    const f = await subbasinFeature(id);
    if (!f || state.sb !== id) return;
    if (layers.selection) map.removeLayer(layers.selection);
    layers.selection = L().geoJSON(f.feature, { renderer: map._ecRenderer, interactive: false,
      style: { fillColor: EC.accent, fillOpacity: 0.35, color: EC.accent, weight: 3, opacity: 1 } }).addTo(map);
    restack();
    if (zoomToSelection) { fitBbox(map, f.bbox, 12); zoomToSelection = false; }
  }

  on(["sb"], showSelection);

  return {
    id: "map", title: "Map", el: page,
    async init() {
      if (D.has("ecorisk")) {
        const eco = await ecorisk();
        ecoSelect.setOptions(Object.entries(eco).map(([k, v]) => ({ value: k, label: v.label })), false);
      } else {
        ecoSelect.setOptions([{ value: "", label: "Not published", disabled: true }], false);
      }
      await syncLayers();
      await showSelection();
    },
  };
}

const FLAG_LABELS = {
  acute_risk_threshold_p90: "Acute risk (90th percentile exposure)",
  chronic_risk_threshold_p90: "Chronic risk (90th percentile exposure)",
  acute_risk_threshold_average: "Acute risk (average exposure)",
  chronic_risk_threshold_average: "Chronic risk (average exposure)",
};

export function riskFlagLines(r) {
  const naStr = (v) => v == null || String(v).trim().toLowerCase() === "na" || String(v).trim() === "";
  return Object.entries(FLAG_LABELS).filter(([col]) => col in r).map(([col, label]) => {
    const ss = r[`ss_${col}`];
    return `${label}: ${r[col]}${!naStr(ss) ? ` · site-specific: ${ss}` : ""}`;
  });
}
