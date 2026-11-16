#!/usr/bin/env python3
"""Build the data files for the static (GitHub Pages) ECOMIX Explorer.

The web app under docs/ has no server: everything it shows is read from the
pre-built files this script writes to docs/data/. Run it from the repo root
whenever any source dataset changes, then commit docs/data/:

    python3 scripts/build_site.py                 # everything that is present
    python3 scripts/build_site.py --only chem geo # just some steps
    python3 scripts/build_site.py --keepbits 23   # full float32 precision

Source data that is missing (several of the large datasets are gitignored and
only exist on the machine that built them) is skipped, recorded as
unavailable in docs/data/meta.json, and the app shows a "not published" note
in place of the views that need it. Nothing is invented to fill the gap.

Steps (names for --only) and the sources they read:

  core      data/subbasin_*.csv, data/waterbody_operational_catchment.csv,
            gis-data/, data/measured/, data/modelled/      -> meta.json, siteinfo.json
  geo       gis-data/ layers                               -> geo/*.bin.gz
  measured  data/measured/comix_monitoring_data_Dashboard.csv
            (or the copy inside data/measured.zip)         -> measured.json
  ecorisk   data-chem/5_EcologicalRisk/*_risk.csv          -> ecorisk.json
  chem      data-chem/1_DAILY_BY_SITE_conc_ng_L/*.csv      -> chem/site/*.bin.gz, chem/summary.bin.gz
  wbchem    data-chem/2_Modelling_results_.../*.parquet    -> wbchem.bin.gz
  extremes  data/Subbasin_Extremes.gz.parquet              -> extremes/*.bin.gz
  hype      data/DB_Proj_Year, data/DB_Proj_Month          -> hype/sb/*.bin.gz, hype/dl_summary.bin.gz
  forcing   data/DB_Proj_Forcing                           -> forcing/*.bin.gz, forcing/dl_summary.bin.gz
  obs       data/DB_Historical_Sim_Obs                     -> obs/*.bin.gz
  d3d       data-chem/7_chemicals, 6_Source_apportionment  -> d3d/<chemical>/*.bin.gz

File format ("pack", read by docs/js/pack.js): a gzip stream holding
  b"ECXP" | uint32 header length | JSON header | array payload
where the header lists each array's dtype, shape and byte range. Float32
arrays are bit-rounded to --keepbits mantissa bits (default 14, a relative
error below 0.004 %, i.e. 4-5 significant figures) and byte-shuffled, which
is what makes them compress; integers are stored exactly.

Requires: numpy, pandas, pyarrow, geopandas (with pyogrio), shapely, pyproj.
"""

import argparse
import gzip
import json
import math
import re
import shutil
import sys
import time
import zipfile
from datetime import date, datetime, timezone
from pathlib import Path

import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parent.parent
FORMAT_VERSION = 1

STEPS = ["core", "geo", "measured", "ecorisk", "chem", "wbchem", "extremes",
         "hype", "forcing", "obs", "d3d"]

# ---- vocabulary shared with the app (ported from app.R) ---------------------

CHEM_LABELS = {
    "6PPDQ": "6PPD-Q", "Azoxystrobin": "Azoxystrobin", "Cu": "Cu",
    "Metformin": "Metformin", "Zn": "Zn", "anthracene": "Anthracene",
    "azithromycin": "Azithromycin", "benzoapyrene": "Benzo[a]pyrene",
    "clarithromycin": "Clarithromycin", "cypermethrin": "Cypermethrin",
    "deltamethrin": "Deltamethrin", "diclofenac": "Diclofenac",
    "doramectin": "Doramectin", "erithromycin": "Erythromycin",
    "fipronil": "Fipronil", "fluconazole": "Fluconazole",
    "flufenacet": "Flufenacet", "fluoranthene": "Fluoranthene",
    "ibuprofen": "Ibuprofen", "imidacloprid": "Imidacloprid",
    "ivermectin": "Ivermectin", "metazachlor": "Metazachlor",
    "permethrin": "Permethrin", "tebuconazole": "Tebuconazole",
    "triallate": "Triallate", "venlafaxine": "Venlafaxine",
}
CHEM_KEYS = sorted(CHEM_LABELS)          # same order as R's sort() for these names
CHEM_STATS = ["mean", "median", "90th"]

HYPE_VARIABLES = [  # id, label (app.R hype_variable_choices order)
    ("discharge", "Discharge"), ("water_temperature", "Water Temperature"),
    ("SS", "Susp. Sediments"), ("IN", "Inorganic Nitrogen"),
    ("SP", "Soluble Phosphorus"), ("PP", "Particulate Phosphorus"),
    ("DOC", "Dissolved Organic Carbon"),
]
HYPE_PCT = ["p01", "p05", "p10", "p25", "p50", "p75", "p90", "p95", "p99"]

OPCAT_RIVER_GROUPS = {
    "Ure": ["Upper Ure", "Middle and Lower Ure"],
    "Swale": ["Upper Swale", "Middle Swale", "Lower Swale"],
    "Derwent": ["Derwent Upper -  Derbyshire", "Derwent Middle - Derbyshire",
                "Upper Derwent Yorkshire", "Middle Derwent Yorkshire", "Lower Derwent Yorkshire"],
    "Wharfe": ["Upper Wharfe", "Middle Wharfe and Washburn", "Lower Wharfe"],
    "Aire": ["Upper Aire", "Middle Aire", "Lower Aire"],
    "Nidd": ["Upper Nidd", "Middle and Lower Nidd"],
    "Ouse": ["Upper Ouse Yorkshire", "Lower Ouse Yorkshire"],
    "Don": ["Upper Don", "Middle Don", "Lower Don"],
    "Calder": ["Upper Calder", "Middle Calder", "Lower Calder", "Calder"],
}

SRC_KEYS = ["dog_swim", "pet_CSO", "pet_WWTP"]
SRC_FIRST = date(2018, 1, 1)
SRC_LAST = date(2022, 12, 31)

EPOCH = date(1970, 1, 1)


def log(msg):
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


def epoch_days(values):
    """Dates (anything pandas understands) -> int32 days since 1970-01-01."""
    d = pd.to_datetime(pd.Series(values)).to_numpy().astype("datetime64[D]")
    return d.astype("int64").astype("<i4")


# ---- pack writer -------------------------------------------------------------

def bitround(a, keepbits):
    """Round float32 values to `keepbits` mantissa bits (round-to-nearest).
    NaN/inf pass through. keepbits >= 23 leaves the values untouched."""
    a = np.ascontiguousarray(a, dtype="<f4")
    if keepbits >= 23:
        return a
    drop = 23 - keepbits
    bits = a.view("<u4").copy()
    mask = np.uint32((0xFFFFFFFF >> drop) << drop)
    half = np.uint32(1 << (drop - 1))
    finite = np.isfinite(a)
    bits[finite] = (bits[finite] + half) & mask
    return bits.view("<f4")


def _shuffle(a):
    """Byte-shuffle: all first bytes, then all second bytes, ... (the high
    bytes of neighbouring values are alike, so they compress far better)."""
    return np.ascontiguousarray(a.reshape(-1).view(np.uint8).reshape(-1, a.itemsize).T).tobytes()


def write_pack(path, meta, arrays, keepbits=None):
    """arrays: {name: ndarray}. float32 arrays are bit-rounded when keepbits
    is given. Output is deterministic (no timestamps), so an unchanged input
    rewrites byte-identical files and git sees no diff."""
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    index, chunks, offset = {}, [], 0
    for name, arr in arrays.items():
        arr = np.asarray(arr)
        code = {"float32": "f4", "float64": "f8", "int32": "i4", "int16": "i2",
                "uint16": "u2", "uint8": "u1", "int8": "i1", "uint32": "u4"}.get(arr.dtype.name)
        if code is None:
            raise TypeError(f"{path.name}:{name}: unsupported dtype {arr.dtype}")
        arr = np.ascontiguousarray(arr, dtype=arr.dtype.newbyteorder("<"))
        if code == "f4" and keepbits is not None:
            arr = bitround(arr, keepbits)
        shuffle = arr.itemsize > 1
        raw = _shuffle(arr) if shuffle else arr.tobytes()
        pad = (-len(raw)) % 8
        index[name] = {"dtype": code, "shape": list(arr.shape), "offset": offset,
                       "nbytes": len(raw), "shuffle": shuffle}
        chunks.append(raw + b"\0" * pad)
        offset += len(raw) + pad
    header = json.dumps({"meta": meta, "arrays": index}, separators=(",", ":"),
                        ensure_ascii=False).encode("utf-8")
    header += b" " * ((-(8 + len(header))) % 8)
    body = b"ECXP" + np.uint32(len(header)).astype("<u4").tobytes() + header + b"".join(chunks)
    with open(path, "wb") as fh:
        fh.write(gzip.compress(body, compresslevel=9, mtime=0))
    return path.stat().st_size


def write_json(path, obj):
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    text = json.dumps(obj, separators=(",", ":"), ensure_ascii=False, allow_nan=False)
    path.write_text(text, encoding="utf-8")
    return path.stat().st_size


def clean(v):
    """JSON-safe scalar: NaN/None -> None, numpy scalars -> python."""
    if v is None:
        return None
    if isinstance(v, (np.floating, float)):
        return None if not math.isfinite(v) else float(v)
    if isinstance(v, (np.integer,)):
        return int(v)
    if v is pd.NA or v is pd.NaT:
        return None
    return v


def dir_size(path):
    path = Path(path)
    if path.is_file():
        return path.stat().st_size, 1
    files = [p for p in path.rglob("*") if p.is_file()]
    return sum(p.stat().st_size for p in files), len(files)


# ---- geometry ---------------------------------------------------------------

def pack_geometries(geoms, scale):
    """Polygon/MultiPolygon list -> integer delta arrays.

    Coordinates are multiplied by `scale` and rounded; each ring drops its
    closing vertex and any repeated point. Returns arrays
      feat_rings  rings per feature
      ring_len    vertices per ring
      ring_ext    1 = exterior ring (starts a new polygon), 0 = hole
      dx, dy      first differences of all ring vertices, in ring order
    """
    feat_rings, ring_len, ring_ext, xs, ys = [], [], [], [], []
    for geom in geoms:
        n_rings = 0
        if geom is not None and not geom.is_empty:
            polys = list(geom.geoms) if geom.geom_type == "MultiPolygon" else [geom]
            for poly in polys:
                if poly.geom_type != "Polygon":
                    continue
                for k, ring in enumerate([poly.exterior, *poly.interiors]):
                    c = np.rint(np.asarray(ring.coords)[:, :2] * scale).astype(np.int64)
                    if len(c) > 1 and (c[0] == c[-1]).all():
                        c = c[:-1]
                    if len(c) > 1:
                        keep = np.ones(len(c), dtype=bool)
                        keep[1:] = (np.diff(c, axis=0) != 0).any(axis=1)
                        c = c[keep]
                    if len(c) < 3:
                        continue
                    xs.append(c[:, 0]); ys.append(c[:, 1])
                    ring_len.append(len(c)); ring_ext.append(1 if k == 0 else 0)
                    n_rings += 1
        feat_rings.append(n_rings)
    x = np.concatenate(xs) if xs else np.zeros(0, np.int64)
    y = np.concatenate(ys) if ys else np.zeros(0, np.int64)
    return {
        "feat_rings": np.asarray(feat_rings, "<i4"),
        "ring_len": np.asarray(ring_len, "<i4"),
        "ring_ext": np.asarray(ring_ext, np.uint8),
        "dx": np.diff(x, prepend=0).astype("<i4"),
        "dy": np.diff(y, prepend=0).astype("<i4"),
    }


def make_valid_polygons(geoms):
    """Repair invalid polygons and keep only the polygon parts of the result
    (a repair can return a collection that also holds stray lines/points)."""
    import shapely
    geoms = np.asarray(geoms, dtype=object).copy()
    bad = np.flatnonzero(~shapely.is_valid(geoms))
    for i in bad:
        try:
            fixed = shapely.make_valid(geoms[i])
        except shapely.errors.GEOSException:
            # GEOS's default repair can trip over collapsed slivers; the
            # "structure" method handles those.
            fixed = shapely.make_valid(geoms[i], method="structure", keep_collapsed=False)
        if fixed.geom_type not in ("Polygon", "MultiPolygon"):
            polys = []
            for part in getattr(fixed, "geoms", []):
                if part.geom_type == "Polygon":
                    polys.append(part)
                elif part.geom_type == "MultiPolygon":
                    polys.extend(part.geoms)
            fixed = shapely.MultiPolygon(polys)
        geoms[i] = fixed
    return geoms


def write_geo_pack(path, gdf, scale, crs, props):
    """props: {name: list} per-feature attribute columns stored in the header."""
    arrays = pack_geometries(list(gdf.geometry), scale)
    meta = {"kind": "polygons", "crs": crs, "scale": scale, "n": len(gdf), "props": props}
    return write_pack(path, meta, arrays)


def r_hcl(h, c, l):
    """Port of grDevices::hcl() (polar CIE-LUV, D65, sRGB gamma, fixup)."""
    if l <= 0:
        return "#000000"
    wy, wu, wv = 100.0, 0.1978398, 0.4683363
    hr = math.radians(h)
    U, V = c * math.cos(hr), c * math.sin(hr)
    Y = wy * (((l + 16) / 116) ** 3 if l > 7.999592 else l / 903.3)
    u = U / (13 * l) + wu
    v = V / (13 * l) + wv
    X = 9.0 * Y * u / (4 * v)
    Z = -X / 3 - 5 * Y + 3 * Y / v

    def g(t):
        t = 1.055 * t ** (1 / 2.4) - 0.055 if t > 0.00304 else 12.92 * t
        return int(min(max(t, 0.0), 1.0) * 255 + 0.5)

    r = g((3.240479 * X - 1.537150 * Y - 0.498535 * Z) / wy)
    gg = g((-0.969256 * X + 1.875992 * Y + 0.041556 * Z) / wy)
    b = g((0.055648 * X - 0.204043 * Y + 1.057311 * Z) / wy)
    return f"#{r:02X}{gg:02X}{b:02X}"


def opcat_colours(levels):
    """One hue per river system, shaded upstream -> downstream (app.R)."""
    grouped = {n for members in OPCAT_RIVER_GROUPS.values() for n in members}
    groups = dict(OPCAT_RIVER_GROUPS)
    for name in levels:
        if name not in grouped:
            groups[name] = [name]
    names = sorted(groups, key=lambda s: s.casefold())
    hues = np.linspace(40, 340, len(names))
    out = {}
    for hue, name in zip(hues, names):
        members = groups[name]
        n = len(members)
        lum = [50.0] if n == 1 else np.linspace(78, 38, n)
        chroma = [55.0] if n == 1 else np.linspace(45, 70, n)
        for m, c, l in zip(members, chroma, lum):
            out[m] = r_hcl(float(hue), float(c), float(l))
    return out


# ---- builder ----------------------------------------------------------------

class Builder:
    def __init__(self, root, out, keepbits):
        self.root = Path(root)
        self.out = Path(out)
        self.keepbits = keepbits
        self._cache = {}

    def src(self, *parts):
        return self.root.joinpath(*parts)

    # -- shared lookups -------------------------------------------------------

    def gis(self):
        """Display layers in WGS84 plus the tables that hang off them."""
        if "gis" in self._cache:
            return self._cache["gis"]
        import geopandas as gpd
        import shapely

        log("reading GIS layers")
        sub = gpd.read_file(self.src("gis-data", "subbasins_wgs.shp"))[["Id", "geometry"]]
        sub["Id"] = sub["Id"].astype(int)
        sub = sub.sort_values("Id").reset_index(drop=True)
        sub["geometry"] = shapely.set_precision(sub.geometry.values, 1e-5, mode="pointwise")

        def simplified(gdf, tolerance):
            bng = gdf.to_crs(27700)
            geom = make_valid_polygons(bng.geometry.values)
            geom = shapely.simplify(geom, tolerance, preserve_topology=True)
            out = gpd.GeoDataFrame(gdf.drop(columns="geometry"), geometry=geom, crs=27700).to_crs(4326)
            out["geometry"] = shapely.set_precision(out.geometry.values, 1e-5, mode="pointwise")
            return out

        wb_src = gpd.read_file(self.src("gis-data", "EA_Waterbodies_AOI.geojson"))
        wb = simplified(wb_src[["water_body", "water_bod0", "geometry"]], 10)
        opcat = simplified(gpd.read_file(self.src("gis-data", "operational_catchments_wgs.shp")), 10)

        self._cache["gis"] = {"sub": sub, "wb": wb, "wb_src": wb_src, "opcat": opcat}
        return self._cache["gis"]

    def site_key(self):
        key = pd.read_csv(self.src("data-chem", "1_DAILY_BY_SITE_conc_ng_L", "Subbasin_Siteids_Key.csv"),
                          encoding="utf-8-sig")
        key["SITE_ID"] = key["SITE_ID"].astype(int)
        key["subbasin"] = key["subbasin"].astype(int)
        return key

    def chem_site_wb(self):
        """SITE_ID -> subbasin -> EA water body containing the subbasin's
        representative point (app.R chem_site_wb), plus the chemical
        modelling's short water body id."""
        if "site_wb" in self._cache:
            return self._cache["site_wb"]
        import shapely
        g = self.gis()
        key = self.site_key()
        sub = g["sub"].set_index("Id")
        ids = sorted(set(key.subbasin) & set(sub.index))
        pts = shapely.point_on_surface(make_valid_polygons(sub.loc[ids].geometry.values))
        wb_geom = make_valid_polygons(g["wb"].geometry.values)
        tree = shapely.STRtree(wb_geom)
        pt_idx, wb_idx = tree.query(pts, predicate="intersects")
        first = {}
        for p, w in sorted(zip(pt_idx.tolist(), wb_idx.tolist())):
            first.setdefault(ids[p], w)
        codes = g["wb"]["water_body"].tolist()
        names = g["wb"]["water_bod0"].tolist()
        key["wb_code"] = [codes[first[s]] if s in first else None for s in key.subbasin]
        key["wb_name"] = [names[first[s]] if s in first else None for s in key.subbasin]
        wbkey = pd.read_csv(self.src("data-chem", "2_Modelling_results_by_waterbody_monthly_conc_ng_per_L",
                                     "marthaWBid2EAWBid.csv"), encoding="utf-8-sig")
        short = dict(zip(wbkey.water_body, wbkey.id_waterbody.astype(int).astype(str)))
        key["wb_id"] = [short.get(c) for c in key.wb_code]
        self._cache["site_wb"] = key
        return key

    def hive_table(self, name, columns=None):
        """A DB_* folder (variable=*/part-0.parquet, or a flat folder) as a
        pandas frame, or None if it is not on this machine."""
        import pyarrow.dataset as ds
        path = self.src("data", name)
        if not path.is_dir() or not any(path.rglob("*.parquet")):
            return None
        dataset = ds.dataset(str(path), format="parquet", partitioning="hive",
                             ignore_prefixes=[".", "_"])
        cols = [c for c in columns if c in dataset.schema.names] if columns else None
        df = dataset.to_table(columns=cols).to_pandas()
        for c in df.columns:
            if isinstance(df[c].dtype, pd.CategoricalDtype):
                df[c] = df[c].astype(str)
        return df

    # -- steps ----------------------------------------------------------------

    def step_core(self):
        g = self.gis()
        sub = g["sub"]
        ids = sub["Id"].to_numpy()
        pos = {int(s): i for i, s in enumerate(ids)}

        opc = pd.read_csv(self.src("data", "subbasin_operational_catchment.csv"))
        opcat_levels = sorted(opc.opcat_name.dropna().unique(), key=str.casefold)
        colours = opcat_colours(opcat_levels)
        opcat_index = {n: i for i, n in enumerate(opcat_levels)}
        sub_opcat = np.full(len(ids), -1, dtype=int)
        for s, n in zip(opc.subbasin, opc.opcat_name):
            if int(s) in pos and isinstance(n, str):
                sub_opcat[pos[int(s)]] = opcat_index[n]

        clim = pd.read_csv(self.src("data", "subbasin_climate.csv"))
        clim["subbasin"] = clim.subbasin.astype(str).str.replace("X", "", regex=False).astype(int)
        clim = clim.set_index("subbasin").reindex(ids)
        lc = pd.read_csv(self.src("data", "subbasin_lc.csv"))
        lc["subbasin"] = lc.subbasin.astype(str).str.replace("X", "", regex=False).astype(int)
        area = lc[lc.variable == "Upstream area"].drop_duplicates("subbasin").set_index("subbasin").value.reindex(ids)

        datasets = self.dataset_status()

        # Which subbasins have what (bit flags: 1 hydrology, 2 chemistry, 4 observations).
        hype_ids = self.hype_subbasin_ids()
        key = self.chem_site_wb()
        obs = self.observed_variables()
        flags = np.zeros(len(ids), dtype=int)
        for s in hype_ids:
            if s in pos: flags[pos[s]] |= 1
        for s in key.subbasin:
            if s in pos: flags[pos[s]] |= 2
        for s in obs:
            if s in pos: flags[pos[s]] |= 4

        # Operational catchment rail: name, colour, hydrology subbasin count, bbox.
        hype_set = set(hype_ids)
        counts = {}
        for s, n in zip(opc.subbasin, opc.opcat_name):
            if isinstance(n, str):
                counts[n] = counts.get(n, 0) + (1 if int(s) in hype_set else 0)
        bounds = {row.opcat_name: [round(v, 5) for v in row.geometry.bounds]
                  for row in g["opcat"].itertuples()}
        opcats = [{"name": n, "colour": colours.get(n), "n": counts.get(n, 0), "bbox": bounds.get(n)}
                  for n in opcat_levels]

        wbo = pd.read_csv(self.src("data", "waterbody_operational_catchment.csv"))
        wb_opcat = dict(zip(wbo.water_body, wbo.opcat_name))
        wb_model = set(self.wbchem_ids())
        waterbodies = [{"id": r.water_body, "name": r.water_bod0,
                        "opcat": wb_opcat.get(r.water_body) if isinstance(wb_opcat.get(r.water_body), str) else None,
                        "model": r.water_body in wb_model}
                       for r in g["wb"].itertuples()]

        # Centroid of each subbasin with observed hydrology (map marker position).
        obs_points = []
        sub_geom = sub.set_index("Id").geometry
        for s in sorted(obs):
            if s in pos:
                c = sub_geom.loc[s].centroid
                obs_points.append({"id": s, "lon": round(c.x, 5), "lat": round(c.y, 5), "vars": obs[s]})

        hype_meta = self.hype_meta()
        meta = {
            "format": FORMAT_VERSION,
            "built": datetime.now(timezone.utc).strftime("%Y-%m-%d"),
            "keepbits": self.keepbits,
            "datasets": datasets,
            "subbasins": {
                "id": ids.tolist(),
                "opcat": sub_opcat.tolist(),
                "area_m2": [clean(v) for v in area.to_numpy(dtype=float)],
                "precip": [None if not math.isfinite(v) else round(float(v), 1) for v in clim.precip.to_numpy(dtype=float)],
                "maat": [None if not math.isfinite(v) else round(float(v), 2) for v in clim.maat.to_numpy(dtype=float)],
                "flags": flags.tolist(),
            },
            "opcats": opcats,
            "waterbodies": waterbodies,
            "obs_points": obs_points,
            "hype": hype_meta,
            "chem": {
                "keys": CHEM_KEYS,
                "labels": [CHEM_LABELS[k] for k in CHEM_KEYS],
                "stats": CHEM_STATS,
                "sites": [{"site": int(r.SITE_ID), "subbasin": int(r.subbasin), "wb": r.wb_code,
                           "wb_name": r.wb_name, "wb_id": r.wb_id} for r in key.itertuples()],
            },
            "d3d": self.d3d_meta(),
        }
        size = write_json(self.out / "meta.json", meta)
        log(f"meta.json: {len(ids):,} subbasins, {len(opcats)} operational catchments, "
            f"{len(waterbodies)} water bodies ({size / 1e3:.0f} kB)")

        help_src = self.src("help.htm")
        if help_src.is_file():
            shutil.copyfile(help_src, self.out / "help.htm")

        info = {"measured": self.parse_entity_info(self.src("data", "measured", "sitesInfo.txt")),
                "modelled": self.parse_entity_info(self.src("data", "modelled", "waterbodyInfo.txt"))}
        size = write_json(self.out / "siteinfo.json", info)
        log(f"siteinfo.json: {len(info['measured'])} sites, {len(info['modelled'])} water bodies ({size / 1e3:.0f} kB)")

    def dataset_status(self):
        def has_parquet(*parts):
            p = self.src(*parts)
            return p.is_dir() and any(p.rglob("*.parquet"))
        chem_dir = self.src("data-chem", "1_DAILY_BY_SITE_conc_ng_L")
        d3d = self.d3d_files()
        return {
            "chem_daily": any(chem_dir.glob("*_conc_by_SITE_ID.csv")),
            "wbchem": any(self.src("data-chem", "2_Modelling_results_by_waterbody_monthly_conc_ng_per_L").glob("*.parquet")),
            "extremes": self.src("data", "Subbasin_Extremes.gz.parquet").is_file(),
            "measured": self.measured_source() is not None,
            "ecorisk": any(self.src("data-chem", "5_EcologicalRisk").glob("*_risk.csv")),
            "hype_proj": has_parquet("data", "DB_Proj_Year") and has_parquet("data", "DB_Proj_Month"),
            "forcing": has_parquet("data", "DB_Proj_Forcing"),
            "obs": has_parquet("data", "DB_Historical_Sim_Obs"),
            "d3d": bool(d3d),
            "source_apportionment": bool(self.src_files(d3d)),
        }

    def hype_subbasin_ids(self):
        """Subbasins with HYPE output: from the monthly projections when they
        are here, otherwise from the extremes file derived from them."""
        if "hype_ids" in self._cache:
            return self._cache["hype_ids"]
        import pyarrow.parquet as pq
        ids = []
        month = self.hive_table("DB_Proj_Month", ["subbasin"])
        if month is not None:
            ids = sorted(int(s) for s in month.subbasin.unique())
        elif self.src("data", "Subbasin_Extremes.gz.parquet").is_file():
            t = pq.read_table(self.src("data", "Subbasin_Extremes.gz.parquet"), columns=["subbasin"])
            ids = sorted(int(s) for s in np.unique(t["subbasin"].to_numpy()))
        self._cache["hype_ids"] = ids
        return ids

    def observed_variables(self):
        """{subbasin: [observed variables]}. Falls back to the cached list
        the Shiny app writes (data/app_cache) when the observations are not
        on this machine, so the map layer and context strip still know which
        subbasins have a gauge even though the series cannot be published."""
        if "obs_vars" in self._cache:
            return self._cache["obs_vars"]
        out = {}
        df = self.hive_table("DB_Historical_Sim_Obs", ["subbasin", "variable"])
        if df is not None:
            for s, v in df.drop_duplicates().itertuples(index=False):
                out.setdefault(int(s), []).append(str(v))
        else:
            cache = self.src("data", "app_cache", "df_observed_subbasins.rds")
            if cache.is_file():
                try:
                    import warnings
                    import rdata
                    with warnings.catch_warnings():
                        warnings.simplefilter("ignore")
                        value = rdata.read_rds(cache)["value"]
                    for s, v in zip(value["subbasin"], value["variable"]):
                        out.setdefault(int(s), []).append(str(v))
                except Exception as exc:  # optional convenience only
                    log(f"  (could not read {cache.name}: {exc})")
        out = {s: sorted(set(v)) for s, v in out.items()}
        self._cache["obs_vars"] = out
        return out

    def wbchem_ids(self):
        import pyarrow.parquet as pq
        path = self.wbchem_path()
        if path is None:
            return []
        t = pq.read_table(path, columns=["waterbody_id"])
        return sorted(set(t["waterbody_id"].to_pylist()))

    def wbchem_path(self):
        d = self.src("data-chem", "2_Modelling_results_by_waterbody_monthly_conc_ng_per_L")
        p = d / "2_Modelling_results_by_waterbody_monthly_conc_ng_per_L.parquet"
        return p if p.is_file() else None

    def hype_meta(self):
        """Scenario / period vocabulary and units, from the monthly
        projections if present, else from the extremes file."""
        import pyarrow.parquet as pq
        units, combos = {}, []
        month = self.hive_table("DB_Proj_Month", ["variable", "ssp", "period", "unit"])
        src = month
        if src is None and self.src("data", "Subbasin_Extremes.gz.parquet").is_file():
            src = pq.read_table(self.src("data", "Subbasin_Extremes.gz.parquet"),
                                columns=["variable", "ssp", "period", "unit"]).to_pandas()
        if src is not None:
            u = src.drop_duplicates(["variable"])
            units = {str(v): str(x) for v, x in zip(u.variable, u.unit)}
            c = src.drop_duplicates(["ssp", "period"]).sort_values(["period", "ssp"])
            combos = [[str(s), str(p)] for s, p in zip(c.ssp, c.period)]
        ssps = sorted({s for s, _ in combos} - {"Baseline"})
        scenarios = (["Baseline"] if any(s == "Baseline" for s, _ in combos) else []) + ssps
        baseline = next((p for s, p in combos if s == "Baseline"), None)
        periods = sorted({p for s, p in combos if s != "Baseline"})
        return {
            "variables": [{"id": v, "label": l, "unit": units.get(v)} for v, l in HYPE_VARIABLES],
            "pct": HYPE_PCT,
            "scenarios": scenarios,
            "periods": periods,
            "baseline_period": baseline,
            "combos": combos,
        }

    @staticmethod
    def parse_entity_info(path):
        """Port of app.R parse_entity_info(): '### id' blocks of 'Key: Value'."""
        path = Path(path)
        if not path.is_file():
            return {}
        lines = [l.rstrip("\n") for l in path.read_text(encoding="utf-8").splitlines()]
        lines = [l for l in lines if not (re.match(r"^\s*#", l) and not l.startswith("### "))]
        out, current = {}, None
        for line in lines:
            if line.startswith("### "):
                current = {"name": None, "summary": None, "population": None, "opcat": None, "landcover": []}
                out[line[4:].strip()] = current
                continue
            m = re.match(r"^([^:]+):\s*(.*)$", line)
            if current is None or not m:
                continue
            k, v = m.group(1).strip(), m.group(2)
            if k in ("Site", "Water body"):
                current["name"] = current["name"] or v
            elif k == "Summary":
                current["summary"] = v
            elif k == "Population (catchment)":
                current["population"] = v
            elif k == "Operational catchment":
                current["opcat"] = v
            elif k.startswith("Land cover"):
                name = re.sub(r"^Land cover - | \(%\)$", "", k)
                try:
                    current["landcover"].append([name, float(v)])
                except ValueError:
                    pass
        return out

    def step_geo(self):
        import geopandas as gpd
        g = self.gis()
        sub = g["sub"]
        hype = set(self.hype_subbasin_ids())
        chem = set(self.site_key().subbasin)
        parts = {
            "subbasins_hype": sub[sub.Id.isin(hype)],
            "subbasins_chem": sub[sub.Id.isin(chem - hype)],
            "subbasins_rest": sub[~sub.Id.isin(hype | chem)],
        }
        for name, part in parts.items():
            size = write_geo_pack(self.out / "geo" / f"{name}.bin.gz", part, 1e5, "EPSG:4326",
                                  {"id": part.Id.tolist()})
            log(f"geo/{name}: {len(part):,} polygons ({size / 1e3:.0f} kB)")
        size = write_geo_pack(self.out / "geo" / "waterbodies.bin.gz", g["wb"], 1e5, "EPSG:4326",
                              {"id": g["wb"].water_body.tolist()})
        log(f"geo/waterbodies: {len(g['wb'])} polygons ({size / 1e3:.0f} kB)")
        oc = g["opcat"]
        size = write_geo_pack(self.out / "geo" / "opcats.bin.gz", oc, 1e5, "EPSG:4326",
                              {"name": oc.opcat_name.tolist()})
        log(f"geo/opcats: {len(oc)} polygons ({size / 1e3:.0f} kB)")

        # Full-resolution layers for the Download page, in British National
        # Grid at 1 mm precision (downloads are always exported in EPSG:27700;
        # 1 cm rounding made three water body polygons self-intersect).
        sub_bng = gpd.read_file(self.src("gis-data", "subbasins_bng.shp"))
        sub_bng["Id"] = sub_bng["Id"].astype(int)
        sub_bng = sub_bng.sort_values("Id").reset_index(drop=True)
        size = write_geo_pack(self.out / "geo" / "dl_subbasins.bin.gz", sub_bng, 1000, "EPSG:27700",
                              {"Id": sub_bng.Id.tolist(),
                               "gridcode": [clean(v) for v in sub_bng.gridcode],
                               "Area": [clean(v) for v in sub_bng.Area]})
        log(f"geo/dl_subbasins: {len(sub_bng):,} polygons ({size / 1e3:.0f} kB)")

        # The source file name is misspelt ("bgn"); accept either.
        cat_path = next((p for p in (self.src("gis-data", "catchments_bng.shp"),
                                     self.src("gis-data", "catchments_bgn.shp")) if p.is_file()), None)
        cat = gpd.read_file(cat_path) if cat_path else gpd.read_file(self.src("gis-data", "catchments_wgs.shp")).to_crs(27700)
        props = {c: [clean(v) for v in cat[c]] for c in cat.columns if c != "geometry"}
        size = write_geo_pack(self.out / "geo" / "dl_catchments.bin.gz", cat, 1000, "EPSG:27700", props)
        log(f"geo/dl_catchments: {len(cat)} polygons ({size / 1e3:.0f} kB)")

        wb = g["wb_src"].to_crs(27700)
        wbo = pd.read_csv(self.src("data", "waterbody_operational_catchment.csv"))
        wb_opcat = dict(zip(wbo.water_body, wbo.opcat_name))
        size = write_geo_pack(self.out / "geo" / "dl_waterbodies.bin.gz", wb, 1000, "EPSG:27700",
                              {"water_body": wb.water_body.tolist(),
                               "opcat_name": [wb_opcat.get(w) if isinstance(wb_opcat.get(w), str) else None
                                              for w in wb.water_body]})
        log(f"geo/dl_waterbodies: {len(wb)} polygons ({size / 1e3:.0f} kB)")

    def measured_source(self):
        """The monitoring CSV: the unpacked file if present, else the copy
        inside data/measured.zip. Returns a callable that opens it."""
        direct = self.src("data", "measured", "comix_monitoring_data_Dashboard.csv")
        if direct.is_file():
            return lambda: open(direct, "rb")
        z = self.src("data", "measured.zip")
        if z.is_file():
            with zipfile.ZipFile(z) as zf:
                names = [n for n in zf.namelist()
                         if n.endswith("comix_monitoring_data_Dashboard.csv") and "__MACOSX" not in n]
            if names:
                return lambda: zipfile.ZipFile(z).open(names[0])
        return None

    def step_measured(self):
        opener = self.measured_source()
        if opener is None:
            log("measured: source not found, skipped")
            return
        with opener() as fh:
            df = pd.read_csv(fh, dtype=str, keep_default_na=False, encoding="utf-8-sig")
        df = df.rename(columns={df.columns[0]: "Site_id"})
        # The last row is a footnote about the ND/NA convention, not a record.
        df = df[pd.to_numeric(df["Latitude"], errors="coerce").notna()].reset_index(drop=True)
        meta_cols = ["Site_id", "Site_full_name", "Latitude", "Longitude", "Week_Month", "Date"]
        others = [c for c in df.columns if c not in meta_cols]
        physchem, chemical = others[-7:], others[:-7]
        df["Date"] = pd.to_datetime(df["Date"], format="%d/%m/%Y").dt.strftime("%Y-%m-%d")

        def cell(v):
            v = v.strip()
            if v == "" or v.upper() == "NA":
                return None            # no sample
            if v == "ND":
                return "ND"            # non-detect
            try:
                return float(v)
            except ValueError:
                return None

        sites = []
        for site_id, part in df.groupby("Site_id", sort=False):
            part = part.sort_values("Date")
            sites.append({
                "id": site_id,
                "name": part["Site_full_name"].iloc[0],
                "lat": float(part["Latitude"].iloc[0]),
                "lon": float(part["Longitude"].iloc[0]),
                "dates": part["Date"].tolist(),
                "values": {c: [cell(v) for v in part[c]] for c in chemical + physchem},
            })
        params = ([{"key": c, "label": re.sub(r"_ng L⁻¹$", "", c), "group": "Organic micropollutant"} for c in chemical]
                  + [{"key": c, "label": c, "group": "Physicochemical parameter"} for c in physchem])
        size = write_json(self.out / "measured.json", {"params": params, "sites": sites})
        log(f"measured.json: {len(sites)} sites, {len(df):,} samples, {len(params)} parameters ({size / 1e3:.0f} kB)")

    def step_ecorisk(self):
        import pyproj
        files = sorted(self.src("data-chem", "5_EcologicalRisk").glob("*_risk.csv"))
        if not files:
            log("ecorisk: no *_risk.csv files, skipped")
            return
        key = self.site_key()
        to_wgs = pyproj.Transformer.from_crs(27700, 4326, always_xy=True)
        d3d = set(self.d3d_files())

        def na(v):
            return v is None or (isinstance(v, float) and math.isnan(v)) or str(v).strip().lower() == "na"

        def yes(row, col):
            return str(row.get(col, "")).strip().lower() == "yes"

        out = {}
        for f in files:
            chem = f.name[: -len("_risk.csv")]
            df = pd.read_csv(f, encoding="utf-8-sig", dtype=str, keep_default_na=False)
            df["site_id"] = df["site_id"].astype(int)
            df = df.merge(key, left_on="site_id", right_on="SITE_ID", how="inner").drop(columns="SITE_ID")
            lon, lat = to_wgs.transform(df.EASTING.astype(float).to_numpy(), df.NORTHING.astype(float).to_numpy())
            rows = []
            for i, row in enumerate(df.to_dict("records")):
                has_ss = not na(row.get("site_specific hc5"))
                pre = "ss_" if has_ss else ""
                acute = yes(row, f"{pre}acute_risk_threshold_p90") or yes(row, f"{pre}acute_risk_threshold_average")
                chronic = yes(row, f"{pre}chronic_risk_threshold_p90") or yes(row, f"{pre}chronic_risk_threshold_average")
                row["risk_level"] = "Both" if acute and chronic else "Acute" if acute else "Chronic" if chronic else "None"
                row["lon"], row["lat"] = round(float(lon[i]), 5), round(float(lat[i]), 5)
                row["subbasin"] = int(row["subbasin"])
                rows.append(row)
            out[chem] = {"label": chem[:1].upper() + chem[1:], "has_d3d": chem.lower() in d3d, "rows": rows}
        size = write_json(self.out / "ecorisk.json", out)
        log(f"ecorisk.json: {', '.join(f'{k} ({len(v['rows'])} sites)' for k, v in out.items())} ({size / 1e3:.0f} kB)")

    def step_chem(self):
        chem_dir = self.src("data-chem", "1_DAILY_BY_SITE_conc_ng_L")
        pattern = re.compile(r"^(?P<chem>.+)_(?P<stat>90th|mean|median)_conc_by_SITE_ID\.csv$")
        files = {}
        for p in chem_dir.glob("*.csv"):
            m = pattern.match(p.name)
            if m:
                files[(m.group("chem"), m.group("stat"))] = p
        if not files:
            log("chem: no daily concentration CSVs, skipped")
            return
        unknown = sorted({c for c, _ in files} - set(CHEM_KEYS))
        if unknown:
            sys.exit(f"chem: no display label for {unknown} - add them to CHEM_LABELS")
        key = self.site_key()
        site_ids = key.SITE_ID.tolist()

        dates, cube = None, None
        for ci, chem in enumerate(CHEM_KEYS):
            for si, stat in enumerate(CHEM_STATS):
                path = files.get((chem, stat))
                if path is None:
                    continue
                df = pd.read_csv(path)
                d = epoch_days(df.iloc[:, 0])
                df = df.iloc[:, 1:]
                df.columns = [int(c) for c in df.columns]
                if dates is None:
                    dates = d
                    cube = np.full((len(CHEM_KEYS), len(CHEM_STATS), len(site_ids), len(d)), np.nan, dtype=np.float64)
                elif not np.array_equal(d, dates):
                    sys.exit(f"chem: {path.name} has a different date axis from the other files")
                missing = [s for s in site_ids if s not in df.columns]
                if missing:
                    sys.exit(f"chem: {path.name} lacks SITE_IDs {missing[:5]}...")
                cube[ci, si] = df[site_ids].to_numpy(dtype=np.float64).T
            log(f"chem: read {chem}")
        if not (np.diff(dates) == 1).all():
            sys.exit("chem: the daily record has gaps; the app assumes consecutive days")

        # Per-chemical/statistic/site summary over the whole record, from the
        # unrounded values: Spatial Datasets map and spatial downloads.
        with np.errstate(all="ignore"):
            import warnings
            with warnings.catch_warnings():
                warnings.simplefilter("ignore", RuntimeWarning)
                summary = {
                    "n": np.isfinite(cube).sum(axis=3).astype("<i4"),
                    "mean": np.nanmean(cube, axis=3).astype("<f4"),
                    "min": np.nanmin(cube, axis=3).astype("<f4"),
                    "max": np.nanmax(cube, axis=3).astype("<f4"),
                }
        meta = {"chems": CHEM_KEYS, "stats": CHEM_STATS, "sites": site_ids,
                "subbasins": key.subbasin.tolist(), "start": int(dates[0]), "n_days": int(len(dates))}
        write_pack(self.out / "chem" / "summary.bin.gz", meta, summary)

        out_dir = self.out / "chem" / "site"
        if out_dir.exists():
            shutil.rmtree(out_dir)
        total = 0
        for k, site in enumerate(site_ids):
            total += write_pack(out_dir / f"{site}.bin.gz",
                                {"site": site, "subbasin": int(key.subbasin.iloc[k]), "chems": CHEM_KEYS,
                                 "stats": CHEM_STATS, "start": int(dates[0]), "n_days": int(len(dates))},
                                {"v": cube[:, :, k, :].astype("<f4")}, keepbits=self.keepbits)
        log(f"chem: {len(site_ids)} site files, {len(dates):,} days x {len(CHEM_KEYS)} chemicals x "
            f"{len(CHEM_STATS)} statistics ({total / 1e6:.1f} MB)")

    def step_wbchem(self):
        import pyarrow.parquet as pq
        path = self.wbchem_path()
        if path is None:
            log("wbchem: parquet not found, skipped")
            return
        df = pq.read_table(path).to_pandas()
        df["chemical"] = df["chemical"].astype(str)
        df["stat"] = df["stat"].astype(str)
        lower = {k.lower(): k for k in CHEM_KEYS}
        raw = sorted(df.chemical.unique())
        bad = [r for r in raw if r.lower() not in lower]
        if bad:
            sys.exit(f"wbchem: chemicals {bad} do not match the daily dataset's names")
        df["chem_key"] = df.chemical.str.lower().map(lower)
        wbs = sorted(df.waterbody_id.unique())
        dates = sorted(df.date.unique())
        cube = np.full((len(CHEM_KEYS), len(CHEM_STATS), len(wbs), len(dates)), np.nan, dtype=np.float64)
        ci = df.chem_key.map({k: i for i, k in enumerate(CHEM_KEYS)}).to_numpy()
        si = df.stat.map({k: i for i, k in enumerate(CHEM_STATS)}).to_numpy()
        wi = df.waterbody_id.map({k: i for i, k in enumerate(wbs)}).to_numpy()
        di = df.date.map({k: i for i, k in enumerate(dates)}).to_numpy()
        cube[ci, si, wi, di] = df.conc_ng_L.to_numpy()
        meta = {"chems": CHEM_KEYS, "stats": CHEM_STATS, "waterbodies": wbs,
                "dates": [d.isoformat() if hasattr(d, "isoformat") else str(d) for d in dates]}
        size = write_pack(self.out / "wbchem.bin.gz", meta, {"v": cube.astype("<f4")}, keepbits=self.keepbits)
        log(f"wbchem: {len(wbs)} water bodies x {len(dates)} months x {len(raw)} chemicals ({size / 1e6:.1f} MB)")

    def step_extremes(self):
        import pyarrow.parquet as pq
        path = self.src("data", "Subbasin_Extremes.gz.parquet")
        if not path.is_file():
            log("extremes: parquet not found, skipped")
            return
        df = pq.read_table(path).to_pandas()
        ids = np.sort(df.subbasin.unique())
        combos = df.drop_duplicates(["ssp", "period"]).sort_values(["period", "ssp"])
        combos = [(str(s), str(p)) for s, p in zip(combos.ssp, combos.period)]
        pcts = sorted(float(p) for p in df.percentile.unique())
        n_missing = 0
        for variable, part in df.groupby("variable"):
            arr = np.full((len(combos), len(pcts), len(ids)), np.nan, dtype=np.float64)
            ci = pd.Series(list(zip(part.ssp, part.period))).map({c: i for i, c in enumerate(combos)}).to_numpy()
            pi = part.percentile.map({p: i for i, p in enumerate(pcts)}).to_numpy()
            ii = np.searchsorted(ids, part.subbasin.to_numpy())
            v = part.value.to_numpy(dtype=np.float64).copy()
            # HYPE writes -9999 where it has no value; keep that as "no data"
            # rather than letting it into the colour scale as a real number.
            sentinel = v <= -9000
            n_missing += int(sentinel.sum())
            v[sentinel] = np.nan
            arr[ci, pi, ii] = v
            meta = {"variable": str(variable), "unit": str(part.unit.iloc[0]),
                    "combos": [list(c) for c in combos], "pcts": pcts}
            write_pack(self.out / "extremes" / f"{variable}.bin.gz", meta,
                       {"ids": ids.astype("<i4"), "v": arr.astype("<f4")}, keepbits=self.keepbits)
        size, n = dir_size(self.out / "extremes")
        log(f"extremes: {n} variables x {len(combos)} scenario-periods x {len(ids):,} subbasins "
            f"({size / 1e6:.1f} MB; {n_missing} values at the -9999 missing code stored as no-data)")

    def step_hype(self):
        year = self.hive_table("DB_Proj_Year")
        month = self.hive_table("DB_Proj_Month")
        if year is None or month is None:
            log("hype: data/DB_Proj_Year and/or data/DB_Proj_Month not on this machine, skipped")
            return
        var_ids = [v for v, _ in HYPE_VARIABLES]
        extra = sorted(set(month.variable.unique()) - set(var_ids))
        if extra:
            log(f"  (variables {extra} are not offered by the app and are left out)")
        year = year[year.variable.isin(var_ids)].copy()
        month = month[month.variable.isin(var_ids)].copy()
        ssps = sorted(set(year.ssp.unique()) | set(month.ssp.unique()))
        periods = sorted(month.period.unique())
        for df in (year, month):
            df["v"] = df.variable.map({v: i for i, v in enumerate(var_ids)}).astype(np.uint8)
            df["s"] = df.ssp.map({v: i for i, v in enumerate(ssps)}).astype(np.uint8)
        month["p"] = month.period.map({v: i for i, v in enumerate(periods)}).astype(np.uint8)
        year = year.sort_values(["subbasin", "v", "s", "year"])
        month = month.sort_values(["subbasin", "v", "s", "p", "month"])

        out_dir = self.out / "hype" / "sb"
        if out_dir.exists():
            shutil.rmtree(out_dir)
        meta = {"variables": var_ids, "ssps": ssps, "periods": periods, "pct": HYPE_PCT}
        ygroups = {int(k): v for k, v in year.groupby("subbasin", sort=False)}
        total = 0
        ids = []
        for sb, m in month.groupby("subbasin", sort=True):
            sb = int(sb)
            y = ygroups.get(sb)
            arrays = {
                "m_var": m.v.to_numpy(), "m_ssp": m.s.to_numpy(), "m_period": m.p.to_numpy(),
                "m_month": m.month.to_numpy(dtype=np.uint8),
                "m_p": m[HYPE_PCT].to_numpy(dtype="<f4"),
            }
            if y is not None:
                arrays.update({"y_var": y.v.to_numpy(), "y_ssp": y.s.to_numpy(),
                               "y_year": y.year.to_numpy(dtype="<i2"),
                               "y_p": y[HYPE_PCT].to_numpy(dtype="<f4")})
            total += write_pack(out_dir / f"{sb}.bin.gz", dict(meta, subbasin=sb), arrays, keepbits=self.keepbits)
            ids.append(sb)

        # Per-subbasin summary of the monthly p50 rows (the Download page's
        # tabular extract, rounded to 3 decimals as there) for spatial exports.
        month["value"] = month.p50.round(3)
        grp = month.groupby(["v", "subbasin"]).value.agg(["count", "mean", "min", "max"]).reset_index()
        ids_arr = np.asarray(sorted(ids), dtype="<i4")
        summ = np.full((len(var_ids), 4, len(ids_arr)), np.nan, dtype=np.float64)
        vi = grp.v.to_numpy()
        ii = np.searchsorted(ids_arr, grp.subbasin.to_numpy())
        for k, col in enumerate(["count", "mean", "min", "max"]):
            summ[vi, k, ii] = grp[col].to_numpy()
        write_pack(self.out / "hype" / "dl_summary.bin.gz", {"variables": var_ids, "cols": ["n", "mean", "min", "max"]},
                   {"ids": ids_arr, "v": summ.astype("<f8")})
        log(f"hype: {len(ids):,} subbasin files ({total / 1e6:.1f} MB)")

    FORCING_BLOCK = 32

    def step_forcing(self):
        df = self.hive_table("DB_Proj_Forcing")
        if df is None:
            log("forcing: data/DB_Proj_Forcing not on this machine, skipped")
            return
        variables = sorted(df.variable.unique())
        periods = sorted(df.period.dropna().unique())
        df["v"] = df.variable.map({v: i for i, v in enumerate(variables)}).astype(np.uint8)
        df["a"] = (df.time_aggregation == "monthly").astype(np.uint8)
        df["p"] = df.period.map({v: i for i, v in enumerate(periods)}).fillna(255).astype(np.uint8)
        df["yr"] = df.year.fillna(0).astype("int16")
        df["mo"] = df.month.fillna(0).astype(np.uint8)
        df = df.sort_values(["subbasin", "v", "a", "p", "yr", "mo"])
        units = {str(v): str(u) for v, u in df.drop_duplicates("variable")[["variable", "unit"]].itertuples(index=False)}
        ids = np.sort(df.subbasin.unique()).astype(int)
        block_of = {int(s): i // self.FORCING_BLOCK for i, s in enumerate(ids)}
        df["block"] = df.subbasin.map(block_of)

        out_dir = self.out / "forcing"
        if out_dir.exists():
            shutil.rmtree(out_dir)
        meta = {"variables": variables, "periods": periods, "units": units}
        total = 0
        for block, part in df.groupby("block", sort=True):
            total += write_pack(out_dir / f"{int(block)}.bin.gz", meta, {
                "sub": part.subbasin.to_numpy(dtype="<i4"), "var": part.v.to_numpy(), "agg": part.a.to_numpy(),
                "period": part.p.to_numpy(), "year": part.yr.to_numpy(dtype="<i2"), "month": part.mo.to_numpy(),
                "p10": part.p10.to_numpy(dtype="<f4"), "p50": part.p50.to_numpy(dtype="<f4"),
                "p90": part.p90.to_numpy(dtype="<f4"),
            }, keepbits=self.keepbits)
        monthly = df[df.a == 1].copy()
        monthly["value"] = monthly.p50.round(3)
        grp = monthly.groupby(["v", "subbasin"]).value.agg(["count", "mean", "min", "max"]).reset_index()
        summ = np.full((len(variables), 4, len(ids)), np.nan, dtype=np.float64)
        ii = np.searchsorted(ids, grp.subbasin.to_numpy())
        for k, col in enumerate(["count", "mean", "min", "max"]):
            summ[grp.v.to_numpy(), k, ii] = grp[col].to_numpy()
        write_pack(out_dir / "dl_summary.bin.gz", {"variables": variables, "cols": ["n", "mean", "min", "max"]},
                   {"ids": ids.astype("<i4"), "v": summ.astype("<f8")})
        write_pack(out_dir / "index.bin.gz", dict(meta, block=self.FORCING_BLOCK), {"ids": ids.astype("<i4")})
        log(f"forcing: {len(ids):,} subbasins in {df.block.nunique()} files ({total / 1e6:.1f} MB)")

    def step_obs(self):
        df = self.hive_table("DB_Historical_Sim_Obs")
        if df is None:
            log("obs: data/DB_Historical_Sim_Obs not on this machine, skipped")
            return
        out_dir = self.out / "obs"
        if out_dir.exists():
            shutil.rmtree(out_dir)
        value_cols = ["sim_P10", "sim_P50", "sim_P90", "obs", "obs_min", "obs_max"]
        for c in value_cols:
            if c not in df.columns:
                df[c] = np.nan
        df["day"] = epoch_days(df.date)
        total, n = 0, 0
        for sb, part in df.groupby("subbasin", sort=True):
            variables = sorted(part.variable.unique())
            stations, unit = [], {}
            part = part.sort_values(["variable", "day"])
            if "id_station" in part.columns:
                st = part.drop_duplicates(["variable", "id_station"])
                for r in st.to_dict("records"):
                    stations.append({"variable": str(r["variable"]), "id": clean(r.get("id_station")),
                                     "label": clean(r.get("station_label"))})
            if "unit" in part.columns:
                unit = {str(v): str(u) for v, u in part.drop_duplicates("variable")[["variable", "unit"]].itertuples(index=False)}
            arrays = {"var": part.variable.map({v: i for i, v in enumerate(variables)}).to_numpy(dtype=np.uint8),
                      "day": part.day.to_numpy(dtype="<i4")}
            for c in value_cols:
                arrays[c] = part[c].to_numpy(dtype="<f4")
            total += write_pack(out_dir / f"{int(sb)}.bin.gz",
                                {"subbasin": int(sb), "variables": variables, "units": unit, "stations": stations},
                                arrays, keepbits=self.keepbits)
            n += 1
        log(f"obs: {n} subbasin files, {len(df):,} daily rows ({total / 1e6:.1f} MB)")

    # -- D3D chemical percentiles + source apportionment ----------------------

    def d3d_files(self):
        """{chemical: {label, conc, sspaf}} for every complete file pair."""
        if "d3d_files" in self._cache:
            return self._cache["d3d_files"]
        out = {}
        d = self.src("data-chem", "7_chemicals")
        if d.is_dir():
            found = {}
            for p in sorted(d.glob("*.parquet")):
                parts = p.name.split("_")
                if len(parts) < 2:
                    continue
                found.setdefault(parts[0].lower(), {"orig": parts[0]})[parts[1].lower()] = p
            for chem in sorted(found):
                f = found[chem]
                if "conc" in f and "sspaf" in f:
                    orig = f["orig"]
                    out[chem] = {"label": orig.title() if orig == orig.lower() else orig,
                                 "conc": f["conc"], "sspaf": f["sspaf"]}
        self._cache["d3d_files"] = out
        return out

    def src_files(self, d3d=None):
        """{chemical: {source: path}} for chemicals with all three source files."""
        d3d = self.d3d_files() if d3d is None else d3d
        d = self.src("data-chem", "6_Source_apportionment")
        out = {}
        if d.is_dir():
            found = {}
            for p in sorted(d.glob("*_source_apportion_waterbodies.csv")):
                m = re.match(r"^([^_]+)_(.*)_source_apportion_waterbodies\.csv$", p.name)
                if m:
                    found.setdefault(m.group(1).lower(), {})[m.group(2)] = p
            for chem, f in found.items():
                if chem in d3d and all(s in f for s in SRC_KEYS):
                    out[chem] = f
        return out

    D3D_BLOCK = 8

    def d3d_meta(self):
        """Chemical list for the app. The site list per chemical is only
        known once the parquet files are read, so step_d3d() writes it to
        d3d/index.json; this just says which chemicals exist."""
        files = self.d3d_files()
        src = self.src_files(files)
        return {"chemicals": [{"key": k, "label": v["label"], "unit": "µg/L", "src": k in src}
                              for k, v in files.items()],
                "block": self.D3D_BLOCK,
                "src_labels": {"dog_swim": "Dog swimming", "pet_CSO": "Pet CSOs", "pet_WWTP": "Pet WWTP"}}

    def step_d3d(self):
        import pyarrow.parquet as pq
        files = self.d3d_files()
        if not files:
            log("d3d: data-chem/7_chemicals not on this machine, skipped")
            return
        src_files = self.src_files(files)
        site_wb = dict(zip(self.chem_site_wb().SITE_ID.astype(str), self.chem_site_wb().wb_id))
        out_root = self.out / "d3d"
        if out_root.exists():
            shutil.rmtree(out_root)
        index, total = {}, 0
        for chem, f in files.items():
            def load(path, prefix):
                t = pq.read_table(path).to_pandas()
                t["SITE_ID"] = t["SITE_ID"].astype(str).str.replace(r"\.0$", "", regex=True)
                t["day"] = epoch_days(t["Date"])
                cols = [f"{prefix}_p10", f"{prefix}_p50", f"{prefix}_p90"]
                return t[["SITE_ID", "day", *cols]].rename(columns=dict(zip(cols, ["p10", "p50", "p90"])))
            conc = load(f["conc"], "conc_ug_L")
            sspaf = load(f["sspaf"], "ssPAF")
            sites = sorted(set(conc.SITE_ID), key=lambda s: (float(s) if s.replace(".", "", 1).isdigit() else math.inf, s))
            days = np.sort(np.unique(np.concatenate([conc.day.to_numpy(), sspaf.day.to_numpy()])))
            site_pos = {s: i for i, s in enumerate(sites)}

            def cube(t):
                a = np.full((len(sites), 3, len(days)), np.nan, dtype=np.float32)
                t = t[t.SITE_ID.isin(site_pos)]
                si = t.SITE_ID.map(site_pos).to_numpy()
                di = np.searchsorted(days, t.day.to_numpy())
                for k, c in enumerate(["p10", "p50", "p90"]):
                    a[si, k, di] = t[c].to_numpy(dtype=np.float32)
                return a
            conc_a, sspaf_a = cube(conc), cube(sspaf)

            src_a, src_start = None, None
            if chem in src_files:
                mats = {}
                for s in SRC_KEYS:
                    m = pd.read_csv(src_files[chem][s])
                    m.columns = [str(c).strip() for c in m.columns]
                    last = (SRC_LAST - EPOCH).days
                    d = last - np.arange(len(m))[::-1]
                    keep = d >= (SRC_FIRST - EPOCH).days
                    mats[s] = (m[keep].reset_index(drop=True), d[keep])
                src_days = mats[SRC_KEYS[0]][1]
                src_start = int(src_days[0])
                # uint16 fractions (0..65534 = 0..1); 65535 = no value.
                src_a = np.full((len(sites), len(SRC_KEYS), len(src_days)), 65535, dtype=np.uint16)
                for i, site in enumerate(sites):
                    wb = site_wb.get(site)
                    if wb is None or wb not in mats[SRC_KEYS[0]][0].columns:
                        continue
                    for k, s in enumerate(SRC_KEYS):
                        v = mats[s][0][wb].to_numpy(dtype=np.float64)
                        q = np.where(np.isfinite(v), np.rint(np.clip(v, 0, 1) * 65534), 65535)
                        src_a[i, k, : len(q)] = q.astype(np.uint16)

            n_blocks = math.ceil(len(sites) / self.D3D_BLOCK)
            for b in range(n_blocks):
                sl = slice(b * self.D3D_BLOCK, (b + 1) * self.D3D_BLOCK)
                arrays = {"day": days.astype("<i4"), "conc": conc_a[sl], "sspaf": sspaf_a[sl]}
                meta = {"chem": chem, "sites": sites[sl]}
                if src_a is not None:
                    arrays["src"] = src_a[sl]
                    meta.update({"src_keys": SRC_KEYS, "src_start": src_start})
                total += write_pack(out_root / chem / f"{b}.bin.gz", meta, arrays, keepbits=self.keepbits)
            index[chem] = {"sites": sites, "src": chem in src_files}
            log(f"d3d: {chem}: {len(sites)} sites x {len(days):,} days")
        write_json(out_root / "index.json", index)
        log(f"d3d: {len(files)} chemicals ({total / 1e6:.1f} MB)")


def main():
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    parser.add_argument("--root", type=Path, default=ROOT, help="repo root holding data/, data-chem/, gis-data/")
    parser.add_argument("--out", type=Path, default=None, help="output folder (default <root>/docs/data)")
    parser.add_argument("--only", nargs="+", choices=STEPS, help="steps to run (default: all)")
    parser.add_argument("--keepbits", type=int, default=14,
                        help="float32 mantissa bits kept (14 = 4-5 significant figures; 23 = exact float32)")
    args = parser.parse_args()
    out = args.out or (ROOT / "docs" / "data")
    builder = Builder(args.root, out, args.keepbits)
    steps = args.only or STEPS
    t0 = time.time()
    for step in steps:
        getattr(builder, f"step_{step}")()
    if "core" not in steps:
        # meta.json records which datasets are published, so keep it in step.
        builder.step_core()

    size, n = dir_size(out)
    log(f"done in {time.time() - t0:.0f}s: {out} holds {n:,} files, {size / 1e6:.0f} MB")
    site_size, _ = dir_size(out.parent)
    if site_size > 900e6:
        log(f"WARNING: the site is {site_size / 1e6:.0f} MB; GitHub Pages allows 1 GB. "
            f"Re-run with a lower --keepbits (e.g. 12) to shrink it.")
    missing = [k for k, v in builder.dataset_status().items() if not v]
    if missing:
        log("not published (source data not on this machine): " + ", ".join(missing))


if __name__ == "__main__":
    main()
