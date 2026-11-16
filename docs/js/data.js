// Data access layer. Everything the app shows comes from the static files
// under data/ (built by scripts/build_site.py); this module fetches them on
// demand, caches them, and hands back plain rows / typed arrays.

import { fetchPack, fetchJSON } from "./pack.js";
import { GeoPack } from "./geo.js";

const BASE = "data/";

export const D = {
  meta: null,
  has: (name) => Boolean(D.meta && D.meta.datasets[name]),
};

const cache = new Map();

function memo(key, load, max = 0) {
  if (cache.has(key)) {
    const hit = cache.get(key);
    if (max) { cache.delete(key); cache.set(key, hit); }   // refresh recency
    return hit;
  }
  const p = load();
  cache.set(key, p);
  p.catch(() => cache.delete(key));
  if (max) {
    // Evict the oldest entries of this family (keys share the "family:" prefix).
    const family = key.slice(0, key.indexOf(":") + 1);
    const keys = [...cache.keys()].filter((k) => k.startsWith(family));
    for (const k of keys.slice(0, Math.max(0, keys.length - max))) cache.delete(k);
  }
  return p;
}

const pack = (path) => fetchPack(BASE + path);

// ---- startup ---------------------------------------------------------------

export async function init() {
  const meta = await fetchJSON(BASE + "meta.json");
  D.meta = meta;
  const sb = meta.subbasins;
  D.sbIndex = new Map(sb.id.map((id, i) => [id, i]));
  D.opcat = new Map(meta.opcats.map((o) => [o.name, o]));
  D.wb = new Map(meta.waterbodies.map((w) => [w.id, w]));
  D.wbModelled = meta.waterbodies.filter((w) => w.model).sort((a, b) => a.name.localeCompare(b.name));
  D.chemLabel = Object.fromEntries(meta.chem.keys.map((k, i) => [k, meta.chem.labels[i]]));
  D.chemSite = new Map(meta.chem.sites.map((s) => [s.site, s]));
  D.chemSitesBySb = new Map();
  for (const s of meta.chem.sites) {
    if (!D.chemSitesBySb.has(s.subbasin)) D.chemSitesBySb.set(s.subbasin, []);
    D.chemSitesBySb.get(s.subbasin).push(s.site);
  }
  D.obsVars = new Map(meta.obs_points.map((p) => [p.id, p.vars]));
  D.hypeVar = new Map(meta.hype.variables.map((v) => [v.id, v]));
  return meta;
}

export function subbasin(id) {
  const i = D.sbIndex.get(id);
  if (i === undefined) return null;
  const sb = D.meta.subbasins;
  const flags = sb.flags[i];
  return {
    id,
    opcat: sb.opcat[i] >= 0 ? D.meta.opcats[sb.opcat[i]].name : null,
    area_m2: sb.area_m2[i],
    precip: sb.precip[i],
    maat: sb.maat[i],
    hype: Boolean(flags & 1),
    chem: Boolean(flags & 2),
    obs: D.obsVars.get(id) || [],
  };
}

export const isChemVar = (v) => typeof v === "string" && v.startsWith("chem_");
export const isWbChemVar = (v) => typeof v === "string" && v.startsWith("wbchem_");
export const chemKeyOf = (v) => v.replace(/^(wb)?chem_/, "");

export function variableLabel(v) {
  if (isChemVar(v) || isWbChemVar(v)) return D.chemLabel[chemKeyOf(v)] || chemKeyOf(v);
  return (D.hypeVar.get(v) || {}).label || v;
}

// ---- geometry --------------------------------------------------------------

export const geo = (name) => memo(`geo:${name}`, async () => new GeoPack(await pack(`geo/${name}.bin.gz`)));

// Which display layer holds a subbasin's polygon.
export function subbasinLayer(id) {
  const s = subbasin(id);
  if (!s) return null;
  return s.hype ? "subbasins_hype" : s.chem ? "subbasins_chem" : "subbasins_rest";
}

export async function subbasinFeature(id) {
  const layer = subbasinLayer(id);
  if (!layer) return null;
  const g = await geo(layer);
  const i = g.find("id", id);
  return i < 0 ? null : { feature: g.feature(i, { id }), bbox: g.bbox(i) };
}

export async function waterbodyFeature(id) {
  const g = await geo("waterbodies");
  const i = g.find("id", id);
  return i < 0 ? null : { feature: g.feature(i, { id }), bbox: g.bbox(i) };
}

// ---- small JSON datasets ---------------------------------------------------

export const measured = () => memo("measured", async () => {
  const m = await fetchJSON(BASE + "measured.json");
  m.byId = new Map(m.sites.map((s) => [s.id, s]));
  m.param = new Map(m.params.map((p) => [p.key, p]));
  return m;
});

export const siteinfo = () => memo("siteinfo", () => fetchJSON(BASE + "siteinfo.json"));

export const ecorisk = () => memo("ecorisk", () => fetchJSON(BASE + "ecorisk.json"));

// ---- chemical concentrations: daily by site --------------------------------

const chemSitePack = (site) => memo(`chemsite:${site}`, () => pack(`chem/site/${site}.bin.gz`), 16);

// Daily series of every site in a subbasin (usually one; a few subbasins
// hold two or three). series(chem, stat) is a Float32Array view of n days.
export async function chemSubbasin(id) {
  const sites = D.chemSitesBySb.get(id) || [];
  const packs = await Promise.all(sites.map(chemSitePack));
  return packs.map((p) => {
    const { chems, stats, start, n_days: n, site } = p.meta;
    return {
      site, start, n,
      series(chem, stat) {
        const c = chems.indexOf(chem), s = stats.indexOf(stat);
        if (c < 0 || s < 0) return null;
        const off = (c * stats.length + s) * n;
        return p.arrays.v.subarray(off, off + n);
      },
    };
  });
}

// Whole-record n / mean / min / max per chemical x statistic x site.
export const chemSummary = () => memo("chemsummary", async () => {
  const p = await pack("chem/summary.bin.gz");
  const { chems, stats, sites, subbasins } = p.meta;
  const ns = sites.length;
  return {
    sites, subbasins,
    // rows: [{site, subbasin, n, mean, min, max}]
    rows(chem, stat) {
      const c = chems.indexOf(chem), s = stats.indexOf(stat);
      if (c < 0 || s < 0) return [];
      const off = (c * stats.length + s) * ns;
      return sites.map((site, i) => ({
        site, subbasin: subbasins[i], n: p.arrays.n[off + i],
        mean: p.arrays.mean[off + i], min: p.arrays.min[off + i], max: p.arrays.max[off + i],
      }));
    },
  };
});

// ---- chemical concentrations: monthly by water body ------------------------

export const wbchem = () => memo("wbchem", async () => {
  const p = await pack("wbchem.bin.gz");
  const { chems, stats, waterbodies, dates } = p.meta;
  const nw = waterbodies.length, nd = dates.length;
  const wbIndex = new Map(waterbodies.map((w, i) => [w, i]));
  return {
    chems, stats, waterbodies,
    dates: dates.map((d) => new Date(`${d}T00:00:00Z`)),
    dateStrings: dates,
    series(chem, stat, wb) {
      const c = chems.indexOf(chem), s = stats.indexOf(stat), w = wbIndex.get(wb);
      if (c < 0 || s < 0 || w === undefined) return null;
      const off = ((c * stats.length + s) * nw + w) * nd;
      return p.arrays.v.subarray(off, off + nd);
    },
  };
});

// ---- HYPE extremes (Spatial Datasets) --------------------------------------

export const extremes = (variable) => memo(`extremes:${variable}`, async () => {
  const p = await pack(`extremes/${variable}.bin.gz`);
  const { combos, pcts, unit } = p.meta;
  const ids = p.arrays.ids, n = ids.length;
  return {
    unit, ids, combos, pcts,
    // Float32Array aligned with ids (NaN = no data), or null.
    values(ssp, period, pct) {
      const c = combos.findIndex(([s, q]) => s === ssp && q === period);
      const k = pcts.indexOf(pct);
      if (c < 0 || k < 0) return null;
      const off = (c * pcts.length + k) * n;
      return p.arrays.v.subarray(off, off + n);
    },
  };
}, 4);

// ---- HYPE projections per subbasin -----------------------------------------

export const hype = (id) => memo(`hype:${id}`, async () => {
  const p = await pack(`hype/sb/${id}.bin.gz`);
  const { variables, ssps, periods, pct } = p.meta;
  const a = p.arrays;
  const rowsOf = (prefix, extra) => {
    const vals = a[`${prefix}_p`];
    if (!vals) return [];
    const n = a[`${prefix}_var`].length;
    const rows = new Array(n);
    for (let i = 0; i < n; i++) {
      const row = { variable: variables[a[`${prefix}_var`][i]], ssp: ssps[a[`${prefix}_ssp`][i]] };
      extra(row, i);
      for (let k = 0; k < pct.length; k++) row[pct[k]] = vals[i * pct.length + k];
      rows[i] = row;
    }
    return rows;
  };
  return {
    year: rowsOf("y", (row, i) => { row.year = a.y_year[i]; }),
    month: rowsOf("m", (row, i) => { row.period = periods[a.m_period[i]]; row.month = a.m_month[i]; }),
  };
}, 12);

export const hypeDownloadSummary = () => memo("hypedl", async () => {
  const p = await pack("hype/dl_summary.bin.gz");
  return summaryTable(p);
});

function summaryTable(p) {
  const { variables } = p.meta;
  const ids = p.arrays.ids, n = ids.length;
  return {
    ids,
    // rows: [{subbasin, n, mean, min, max}] for one variable
    rows(variable) {
      const v = variables.indexOf(variable);
      if (v < 0) return [];
      const off = v * 4 * n, out = [];
      for (let i = 0; i < n; i++) {
        const count = p.arrays.v[off + i];
        if (!(count > 0)) continue;
        out.push({ subbasin: ids[i], n: count, mean: p.arrays.v[off + n + i], min: p.arrays.v[off + 2 * n + i], max: p.arrays.v[off + 3 * n + i] });
      }
      return out;
    },
  };
}

// ---- climate projections (UKCP18 ensemble summary) -------------------------

const forcingIndex = () => memo("forcingindex", async () => {
  const p = await pack("forcing/index.bin.gz");
  return { ids: p.arrays.ids, block: p.meta.block, index: new Map(Array.from(p.arrays.ids, (id, i) => [id, i])) };
});

export async function forcing(id) {
  const idx = await forcingIndex();
  const i = idx.index.get(id);
  if (i === undefined) return { rows: [], units: {} };
  const p = await memo(`forcing:${Math.floor(i / idx.block)}`, () => pack(`forcing/${Math.floor(i / idx.block)}.bin.gz`), 6);
  const { variables, periods, units } = p.meta;
  const a = p.arrays, rows = [];
  for (let k = 0; k < a.sub.length; k++) {
    if (a.sub[k] !== id) continue;
    rows.push({
      variable: variables[a.var[k]], agg: a.agg[k] ? "monthly" : "annual",
      period: a.period[k] === 255 ? null : periods[a.period[k]],
      year: a.year[k], month: a.month[k], p10: a.p10[k], p50: a.p50[k], p90: a.p90[k],
    });
  }
  return { rows, units };
}

export const forcingDownloadSummary = () => memo("forcingdl", async () => summaryTable(await pack("forcing/dl_summary.bin.gz")));

// ---- historical simulation vs observation ----------------------------------

export const obs = (id) => memo(`obs:${id}`, async () => {
  const p = await pack(`obs/${id}.bin.gz`);
  const { variables, units, stations } = p.meta;
  const a = p.arrays, byVar = {};
  let start = 0;
  for (let v = 0; v < variables.length; v++) {
    let end = start;
    while (end < a.var.length && a.var[end] === v) end++;
    byVar[variables[v]] = {
      unit: units[variables[v]] || null,
      stations: stations.filter((s) => s.variable === variables[v]),
      day: a.day.subarray(start, end),
      sim_P10: a.sim_P10.subarray(start, end), sim_P50: a.sim_P50.subarray(start, end), sim_P90: a.sim_P90.subarray(start, end),
      obs: a.obs.subarray(start, end), obs_min: a.obs_min.subarray(start, end), obs_max: a.obs_max.subarray(start, end),
    };
    start = end;
  }
  return byVar;
}, 8);

// ---- D3D chemical percentile series + source apportionment -----------------

export const d3dIndex = () => memo("d3dindex", () => fetchJSON(BASE + "d3d/index.json"));

export async function d3d(chem, site) {
  const index = await d3dIndex();
  const entry = index[chem];
  const k = entry ? entry.sites.indexOf(String(site)) : -1;
  if (k < 0) return null;
  const block = D.meta.d3d.block;
  const p = await memo(`d3d:${chem}:${Math.floor(k / block)}`, () => pack(`d3d/${chem}/${Math.floor(k / block)}.bin.gz`), 8);
  const row = p.meta.sites.indexOf(String(site));
  const nd = p.arrays.day.length;
  const view = (arr, len, j) => arr.subarray((row * 3 + j) * len, (row * 3 + j + 1) * len);
  const out = {
    day: p.arrays.day,
    conc: [0, 1, 2].map((j) => view(p.arrays.conc, nd, j)),
    sspaf: [0, 1, 2].map((j) => view(p.arrays.sspaf, nd, j)),
    src: null,
  };
  if (p.arrays.src) {
    const ns = p.arrays.src.shape[2];
    const shares = [0, 1, 2].map((j) => view(p.arrays.src, ns, j));
    if (shares[0].some((v) => v !== 65535)) out.src = { keys: p.meta.src_keys, start: p.meta.src_start, n: ns, shares };
  }
  return out;
}
