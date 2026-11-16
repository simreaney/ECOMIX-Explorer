// Pieces shared by several pages: the subbasin context strip and navigation.

import { D, subbasin, subbasinFeature } from "./data.js";
import { state, set, batch, on } from "./state.js";
import { el, ctxStrip, link, setChildren } from "./ui.js";
import { miniMap } from "./mapkit.js";
import { fmtNum } from "./theme.js";

// Switch tab (and optionally apply state at the same time).
export function go(tab, patch = {}) {
  batch(() => set({ ...patch, tab }));
}

const dash = "—";

// The four-number strip about the selected subbasin, repeated across pages.
export function subbasinCtx({ map = true, action = null } = {}) {
  const ctx = ctxStrip({
    map,
    stats: [["area", "Upstream area"], ["precip", "Annual precip."], ["temp", "Mean annual temp."], ["obs", "Observations"]],
    action,
  });
  ctx.stats.obs.classList.add("is-tags");
  let mm = null;
  const update = async () => {
    const s = state.sb != null ? subbasin(state.sb) : null;
    if (!s) {
      ctx.setId(state.sb != null ? String(state.sb) : "No location");
      ctx.sub.textContent = state.sb != null ? "Not a known subbasin id" : "Pick a subbasin on the map";
      for (const v of Object.values(ctx.stats)) v.textContent = dash;
      if (mm) mm.clear();
      return;
    }
    ctx.setId(String(s.id));
    ctx.sub.textContent = s.opcat || "No operational catchment";
    setChildren(ctx.stats.area, s.area_m2 != null ? [fmtNum(s.area_m2 / 1e6, 3), el("span.unit", { text: " km²" })] : dash);
    setChildren(ctx.stats.precip, s.precip != null ? [String(Math.round(s.precip)), el("span.unit", { text: " mm" })] : dash);
    setChildren(ctx.stats.temp, s.maat != null ? [s.maat.toFixed(1), el("span.unit", { text: " °C" })] : dash);
    setChildren(ctx.stats.obs, s.obs.length ? s.obs.map((v) => el("span.ctx-tag", { text: v })) : el("span.dim", { text: "none at this subbasin" }));
    if (mm) {
      const f = await subbasinFeature(s.id);
      if (f && state.sb === s.id) mm.showFeature(f.feature, f.bbox);
    }
  };
  if (map) {
    const box = el("div", { style: { width: "100%", height: "94px" } });
    ctx.map.append(box);
    mm = miniMap(box);
  }
  on(["sb"], update);
  update();
  return ctx;
}

export const changeLocation = () => link("Change location", () => go("map"));

// A chemical's sites that sit in a subbasin, or in a water body.
export function sitesForSubbasin(id) { return D.chemSitesBySb.get(id) || []; }
export function sitesForWaterbody(code) { return D.meta.chem.sites.filter((s) => s.wb === code).map((s) => s.site); }
