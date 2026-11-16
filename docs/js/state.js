// One shared, observable state object. Pages subscribe to the keys they draw
// from; the part of the state worth sharing in a link is mirrored into the
// URL hash, so any view can be bookmarked or sent to someone.

export const state = {
  tab: "map",
  sb: null,            // selected subbasin id
  site: null,          // selected measured site id
  wb: null,            // selected modelled water body (Site Details)
  siteMode: "measured",
  deMode: "subbasin",  // Hydro Explorer: subbasin | waterbody
  deWb: null,          // Hydro Explorer water body
  variable: "discharge",
  scenarios: null,
  periods: null,
  pct: ["p10", "p50", "p90"],
  chemStats: ["median"],
  plotType: "Absolute",
  climateVar: "Precipitation",
  climateRes: "monthly",
  obsVar: null,
  projTab: "Yearly",
  d3dChem: null,
  d3dSite: null,
  d3dGroup: "daily",
  d3dStat: "median",
  eco: null,           // "chemical||site" on the Ecological Risk tab
};

// key in the hash -> [state key, parse]
const num = (v) => (v === "" || Number.isNaN(Number(v)) ? null : Number(v));
const str = (v) => v || null;
const list = (v) => (v ? v.split(",") : []);
const HASH = {
  tab: ["tab", str], sb: ["sb", num], site: ["site", str], wb: ["wb", str],
  mode: ["siteMode", str], demode: ["deMode", str], dewb: ["deWb", str],
  var: ["variable", str], scen: ["scenarios", list], per: ["periods", list],
  pct: ["pct", list], stat: ["chemStats", list], type: ["plotType", str],
  chem: ["d3dChem", str], d3site: ["d3dSite", str], eco: ["eco", str],
};

const subs = [];
let batching = null;

export function on(keys, fn) {
  subs.push({ keys: new Set(keys), fn });
}

function same(a, b) {
  if (Array.isArray(a) && Array.isArray(b)) return a.length === b.length && a.every((v, i) => v === b[i]);
  return a === b;
}

export function set(patch) {
  const changed = new Set();
  for (const [k, v] of Object.entries(patch)) {
    if (!same(state[k], v)) { state[k] = v; changed.add(k); }
  }
  if (!changed.size) return;
  if (batching) { changed.forEach((k) => batching.add(k)); return; }
  flush(changed);
}

function flush(changed) {
  writeHash();
  for (const s of subs) {
    if ([...changed].some((k) => s.keys.has(k))) {
      try { s.fn(changed); } catch (err) { console.error(err); }
    }
  }
}

// Run several set() calls as one notification.
export function batch(fn) {
  if (batching) return fn();
  batching = new Set();
  try { fn(); } finally {
    const changed = batching;
    batching = null;
    if (changed.size) flush(changed);
  }
}

let defaults = null;
let lastHash = "";

function writeHash() {
  if (!defaults) return;
  const parts = [];
  for (const [hk, [sk]] of Object.entries(HASH)) {
    const v = state[sk];
    if (v == null || same(v, defaults[sk])) continue;
    parts.push(`${hk}=${encodeURIComponent(Array.isArray(v) ? v.join(",") : v)}`);
  }
  lastHash = parts.length ? `#${parts.join("&")}` : "";
  if (location.hash !== lastHash) history.replaceState(null, "", lastHash || location.pathname + location.search);
}

function readHash() {
  const patch = {};
  for (const part of location.hash.replace(/^#/, "").split("&")) {
    if (!part) continue;
    const i = part.indexOf("=");
    const hk = i < 0 ? part : part.slice(0, i);
    if (!(hk in HASH)) continue;
    const [sk, parse] = HASH[hk];
    patch[sk] = parse(decodeURIComponent(i < 0 ? "" : part.slice(i + 1)));
  }
  return patch;
}

// Call once the defaults that depend on the data (scenarios, periods...) are set.
export function startRouting() {
  defaults = structuredClone(state);
  set(readHash());
  window.addEventListener("hashchange", () => {
    if (location.hash === lastHash) return;
    set({ ...structuredClone(defaults), ...readHash() });
  });
}
