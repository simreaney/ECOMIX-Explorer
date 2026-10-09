#!/usr/bin/env python3
"""One-off data-prep script (not run as part of the Shiny app itself).

Imports a HYPE / climate database update - delivered as millions of tiny
Hive-partitioned parquet files under data/database_update/ (layout and units
documented in data/database_update/readme.xlsx) - into the DB_* layout that
app.R reads.

Each source dataset is collapsed into one de-duplicated, pre-sorted parquet
file per top-level partition (per HYPE variable / per climate model), so
Arrow has a handful of files to enumerate at app startup rather than ~6
million, and row-group min/max statistics on the leading `subbasin` sort key
prune per-subbasin reads the way the per-subbasin directories used to.
Some source folders hold the same rows twice (a part-0 with a repeated month
plus a stray part-1) - exact duplicate rows are dropped, and any key that is
still duplicated afterwards (i.e. two *different* values for the same key) is
reported, not silently resolved.

Source -> output (all under --out):
  HYPE/projections/by_{year,month,week}   DB_Proj_{Year,Month,Week}/variable=*/
  HYPE/historical/by_{year,month,week,day} DB_Hist_{Year,Month,Week,Day}/variable=*/
  climate/projections/by_{year,month,week} DB_Proj_Forcing_{Year,Month,Week}/cm=*/
  climate/historical/by_{year,month,day,percentiles}
                                         DB_Hist_Forcing_{Year,Month,Day,Percentiles}/
  HYPE/historical/Validation             DB_Historical_Sim_Obs/variable=*/
  readme.xlsx                            DB_readme.xlsx
Derived from the above, for app.R:
  DB_Proj_Forcing/              climate ensemble summary (p10/p50/p90 across
                                the UKCP18 members), annual + monthly
  Subbasin_Extremes.gz.parquet  P1/P50/P99 per subbasin x variable x scenario
                                x period for the Spatial Datasets map

Column conventions follow the existing data/ folders: `id_subbasin` is
renamed `subbasin`, year/month/week are integers, HYPE datasets gain a `unit`
column (from readme.xlsx), and the monthly/weekly HYPE projections gain a
decade `period` label ("2070-2079") alongside the raw `time_slice` - the raw
slices differ by variable (discharge/water_temperature use 2070_2079, the
water quality variables 2071_2079), so `period` is what the app filters on.

Usage (from the repo root):
  python3 scripts/import_database_update.py
  python3 scripts/import_database_update.py --only DB_Proj_Month --resume

Outputs are staged in --out (default data_consolidated/, gitignored), not
written into data/ directly. To install, move the current data/ copies of
the same names aside (e.g. into data/pre_update_hive_backup/, also
gitignored) and move the staged folders/files into data/.
"""

import argparse
import shutil
import sys
import time
from pathlib import Path
from urllib.parse import unquote

import duckdb
import numpy as np
import pyarrow as pa
import pyarrow.parquet as pq

ROOT = Path(__file__).resolve().parent.parent

# Units per HYPE variable, from readme.xlsx. Historical datasets spell water
# temperature "Water temperature", the projections "water_temperature".
HYPE_UNITS = {
    "discharge": "m3/s",
    "DOC": "mg/l",
    "IN": "ug/l",
    "ON": "ug/l",
    "PP": "ug/l",
    "SP": "ug/l",
    "SS": "mg/l",
    "water_temperature": "degC",
    "Water temperature": "degC",
}

# name -> (source folder, top-level partition key or None, kind, time column)
DATASETS = {
    "DB_Proj_Year": ("HYPE/projections/by_year", "variable", "hype_proj", "year"),
    "DB_Proj_Month": ("HYPE/projections/by_month", "variable", "hype_proj", "month"),
    "DB_Proj_Week": ("HYPE/projections/by_week", "variable", "hype_proj", "week"),
    "DB_Hist_Year": ("HYPE/historical/by_year", "variable", "hype_hist", "year"),
    "DB_Hist_Month": ("HYPE/historical/by_month", "variable", "hype_hist", "month"),
    "DB_Hist_Week": ("HYPE/historical/by_week", "variable", "hype_hist", "week"),
    "DB_Hist_Day": ("HYPE/historical/by_day", "variable", "hype_hist", "date"),
    "DB_Proj_Forcing_Year": ("climate/projections/by_year", "cm", "clim_proj", "year"),
    "DB_Proj_Forcing_Month": ("climate/projections/by_month", "cm", "clim_proj", "month"),
    "DB_Proj_Forcing_Week": ("climate/projections/by_week", "cm", "clim_proj", "week"),
    "DB_Hist_Forcing_Year": ("climate/historical/by_year", None, "clim_hist", "year"),
    "DB_Hist_Forcing_Month": ("climate/historical/by_month", None, "clim_hist", "month"),
    "DB_Hist_Forcing_Day": ("climate/historical/by_day", None, "clim_hist", "date"),
    "DB_Hist_Forcing_Percentiles": ("climate/historical/by_percentiles", None, "clim_hist", "percentile"),
    "DB_Historical_Sim_Obs": ("HYPE/historical/Validation", "variable", "hype_obs", "date"),
}

# Hive keys below the top-level partition, per kind (the source files only
# sometimes carry these as columns too, so they're always read from paths).
HIVE_KEYS = {
    "hype_proj": ["ssp", "time_slice", "id_subbasin"],
    "hype_hist": ["id_subbasin"],
    "clim_proj": ["period", "id_subbasin"],
    "clim_hist": ["id_subbasin"],
    "hype_obs": ["subbasin"],
}

# Extra sort/uniqueness keys ahead of the time column, per kind (a subbasin
# can hold more than one observation station).
EXTRA_KEYS = {"hype_obs": ["id_station"]}

# Decade label for a projection time slice; Baseline keeps its own range.
PERIOD_SQL = """CASE WHEN ssp = 'Baseline' THEN replace(time_slice, '_', '-')
  ELSE printf('%d-%d', (CAST(split_part(time_slice, '_', 1) AS INTEGER) // 10) * 10,
                       (CAST(split_part(time_slice, '_', 1) AS INTEGER) // 10) * 10 + 9) END"""

PARQUET_OPTS = "FORMAT parquet, COMPRESSION zstd, ROW_GROUP_SIZE 32768"

# Percentile columns of the HYPE projections and their probabilities.
PCT_COLS = ["p01", "p05", "p10", "p25", "p50", "p75", "p90", "p95", "p99"]
PCT_PROBS = np.array([0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99])
MONTH_DAYS = np.array([31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31])


def log(msg):
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


def hive_types(keys):
    types = {k: "INTEGER" if k in ("id_subbasin", "cm") else "VARCHAR" for k in keys}
    return "{" + ", ".join(f"'{k}': {v}" for k, v in types.items()) + "}"


def consolidate(con, name, src_root, out_root, resume):
    src_rel, top_key, kind, time_col = DATASETS[name]
    src = src_root / src_rel
    keys = HIVE_KEYS[kind]
    parts = sorted(p.name for p in src.iterdir() if p.is_dir()) if top_key else [None]

    for part in parts:
        if part:
            glob = src / part / "**" / "*.parquet"
            out_file = out_root / name / part / "part-0.parquet"
            all_keys = [top_key] + keys
        else:
            glob = src / "**" / "*.parquet"
            out_file = out_root / name / "part-0.parquet"
            all_keys = keys
        if resume and out_file.exists():
            log(f"{name}/{part or ''}: exists, skipped")
            continue

        t0 = time.time()
        source = (f"read_parquet('{glob}', hive_partitioning = true, union_by_name = true, "
                  f"hive_types = {hive_types(all_keys)})")
        cols = [r[0] for r in con.execute(f"DESCRIBE SELECT * FROM {source}").fetchall()]

        sub_key = "id_subbasin" if "id_subbasin" in keys else "subbasin"
        select = [f"CAST({sub_key} AS INTEGER) AS subbasin"]
        select += [k for k in keys if k != sub_key]
        if kind == "hype_proj" and time_col in ("month", "week"):
            select.append(f"{PERIOD_SQL} AS period")
        for c in cols:
            if c in all_keys:
                continue
            if c in ("year", "month", "week"):
                select.append(f"CAST({c} AS INTEGER) AS {c}")
            elif c == "date":
                select.append("CAST(date AS DATE) AS date")
            else:
                select.append(f'"{c}"')
        if kind in ("hype_proj", "hype_hist"):
            select.append(f"'{HYPE_UNITS[unquote(part.split('=', 1)[1])]}' AS unit")

        sort_keys = (["subbasin"] + [k for k in keys if k != sub_key]
                     + EXTRA_KEYS.get(kind, []) + [time_col])
        con.execute(f"CREATE OR REPLACE TEMP TABLE raw AS SELECT {', '.join(select)} FROM {source}")
        n_raw = con.execute("SELECT count(*) FROM raw").fetchone()[0]

        out_file.parent.mkdir(parents=True, exist_ok=True)
        tmp_file = out_file.with_name(".part-0.parquet.tmp")
        con.execute(f"COPY (SELECT DISTINCT * FROM raw ORDER BY {', '.join(sort_keys)}) "
                    f"TO '{tmp_file}' ({PARQUET_OPTS})")
        tmp_file.rename(out_file)

        n_out, n_keys = con.execute(
            f"SELECT count(*), count(DISTINCT ({', '.join(sort_keys)})) FROM read_parquet('{out_file}')"
        ).fetchone()
        con.execute("DROP TABLE raw")
        note = f"{n_raw - n_out:,} duplicate rows dropped" if n_raw != n_out else "no duplicates"
        if n_keys != n_out:
            note += f"; WARNING {n_out - n_keys:,} rows share a key ({', '.join(sort_keys)}) with different values"
        log(f"{name}/{part or ''}: {n_out:,} rows ({note}), "
            f"{out_file.stat().st_size / 1e6:.1f} MB, {time.time() - t0:.0f}s")


def build_forcing_summary(con, out_root):
    """Ensemble p10/p50/p90 across the UKCP18 members, in the shape app.R's
    climate plot and downloads use: annual rows are the spread of each year's
    total/mean across members; monthly rows are the median across members of
    each member's 10th/50th/90th percentile across the years of the period.
    Baseline (2000-2019) pools the 2000s and 2010s decades. The source's
    period folders overlap (2010s runs to 2022, so 2020-2022 also sit in
    2020s, with identical values), so annual rows are de-duplicated per
    member-year first, to count each member once."""
    yr = (f"(SELECT DISTINCT subbasin, cm, year, precipitation, temperature FROM "
          f"read_parquet('{out_root}/DB_Proj_Forcing_Year/*/*.parquet', hive_partitioning = true))")
    mo = f"read_parquet('{out_root}/DB_Proj_Forcing_Month/*/*.parquet', hive_partitioning = true)"
    decade = "printf('%d-%d', {0} // 10 * 10, {0} // 10 * 10 + 9)"
    clim_period = "printf('%s-%d', left(period, 4), CAST(left(period, 4) AS INTEGER) + 9)"
    parts = []
    for var, col, unit in (("Precipitation", "precipitation", "mm"), ("Temperature", "temperature", "degC")):
        parts.append(f"""
          SELECT subbasin, 'annual' AS time_aggregation, {decade.format('year')} AS period,
                 year, CAST(NULL AS INTEGER) AS month, '{var}' AS variable,
                 quantile_cont({col}, 0.1) AS p10, quantile_cont({col}, 0.5) AS p50,
                 quantile_cont({col}, 0.9) AS p90, '{unit}' AS unit, count(*) AS n_members
          FROM {yr} GROUP BY subbasin, year""")
        for period_expr, where in ((clim_period, ""), ("'2000-2019'", "WHERE period IN ('2000s', '2010s')")):
            parts.append(f"""
              SELECT subbasin, 'monthly', {period_expr} AS period, CAST(NULL AS INTEGER), month, '{var}',
                     median({col}_p10), median({col}_p50), median({col}_p90), '{unit}', count(*)
              FROM {mo} {where} GROUP BY subbasin, {period_expr}, month""")
    out_file = out_root / "DB_Proj_Forcing" / "part-0.parquet"
    out_file.parent.mkdir(parents=True, exist_ok=True)
    con.execute(f"COPY (SELECT * FROM ({' UNION ALL '.join(parts)}) "
                f"ORDER BY subbasin, variable, time_aggregation, period, year, month) "
                f"TO '{out_file}' ({PARQUET_OPTS})")
    n = con.execute(f"SELECT count(*) FROM read_parquet('{out_file}')").fetchone()[0]
    log(f"DB_Proj_Forcing (ensemble summary): {n:,} rows")


def mixture_cdf(q, w, v):
    """CDF at v of the day-weighted mixture of per-month distributions, each
    linear between its percentile knots, with each month's mass below p01 /
    above p99 placed at those knots. q: (G, M, K) sorted knots; v: (G,)."""
    k = (q <= v[:, None, None]).sum(-1)
    seg = np.clip(k, 1, len(PCT_PROBS) - 1)
    q_lo = np.take_along_axis(q, (seg - 1)[..., None], -1)[..., 0]
    q_hi = np.take_along_axis(q, seg[..., None], -1)[..., 0]
    with np.errstate(divide="ignore", invalid="ignore"):
        frac = np.where(q_hi > q_lo, (v[:, None] - q_lo) / (q_hi - q_lo), 1.0)
    f = PCT_PROBS[seg - 1] + frac * (PCT_PROBS[seg] - PCT_PROBS[seg - 1])
    f = np.where(k == 0, 0.0, np.where(k == len(PCT_PROBS), 1.0, f))
    return (f * w).sum(-1)


def pooled_quantile(q, w, p, iterations=60):
    """Smallest v with mixture_cdf(v) >= p, by bisection (handles the steps)."""
    lo = q.min(axis=(1, 2)) - 1.0
    hi = q.max(axis=(1, 2))
    for _ in range(iterations):
        mid = (lo + hi) / 2
        above = mixture_cdf(q, w, mid) >= p
        hi = np.where(above, mid, hi)
        lo = np.where(above, lo, mid)
    return hi


def build_extremes(out_root):
    """P1/P50/P99 of daily values pooled over all months of each period, per
    subbasin x variable x scenario x period, from DB_Proj_Month - the same
    pooled distribution the Hydro Explorer's cumulative frequency plot draws."""
    w = MONTH_DAYS / MONTH_DAYS.sum()
    tables = []
    for part in sorted((out_root / "DB_Proj_Month").iterdir()):
        if not part.is_dir():
            continue
        variable = unquote(part.name.split("=", 1)[1])
        t = pq.read_table(part / "part-0.parquet")
        n_groups = t.num_rows // 12
        months = t["month"].to_numpy().reshape(n_groups, 12)
        if t.num_rows % 12 or not (months == np.arange(1, 13)).all():
            sys.exit(f"DB_Proj_Month/{part.name}: groups are not 12 consecutive months")
        q = np.sort(np.stack([t[c].to_numpy() for c in PCT_COLS], -1).reshape(n_groups, 12, len(PCT_COLS)), -1)
        has_nan = np.isnan(q).any(axis=(1, 2))
        first = np.arange(n_groups) * 12
        for pct in (1, 50, 99):
            value = pooled_quantile(np.nan_to_num(q), w, pct / 100)
            value[has_nan] = np.nan
            tables.append(pa.table({
                "variable": pa.array([variable] * n_groups),
                "ssp": t["ssp"].take(first),
                "period": t["period"].take(first),
                "time_slice": t["time_slice"].take(first),
                "subbasin": t["subbasin"].take(first),
                "unit": t["unit"].take(first),
                "percentile": pa.array(np.full(n_groups, float(pct))),
                "value": pa.array(value),
            }))
        log(f"Subbasin_Extremes: {variable} done ({n_groups:,} groups, {int(has_nan.sum())} with missing values)")
    out_file = out_root / "Subbasin_Extremes.gz.parquet"
    pq.write_table(pa.concat_tables(tables), out_file, compression="gzip")
    log(f"Subbasin_Extremes.gz.parquet: {sum(t.num_rows for t in tables):,} rows")


def main():
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    parser.add_argument("--src", type=Path, default=ROOT / "data" / "database_update")
    parser.add_argument("--out", type=Path, default=ROOT / "data_consolidated")
    parser.add_argument("--only", nargs="+", help="dataset names to (re)build; default all")
    parser.add_argument("--resume", action="store_true", help="skip partitions already written")
    args = parser.parse_args()

    names = args.only or list(DATASETS) + ["DB_Proj_Forcing", "Subbasin_Extremes", "DB_readme"]
    args.out.mkdir(parents=True, exist_ok=True)
    tmp_dir = args.out / ".duckdb_tmp"
    con = duckdb.connect()
    con.execute(f"SET temp_directory = '{tmp_dir}'")
    con.execute("SET preserve_insertion_order = true")

    for name in names:
        if name in DATASETS:
            consolidate(con, name, args.src, args.out, args.resume)
        elif name == "DB_Proj_Forcing":
            build_forcing_summary(con, args.out)
        elif name == "Subbasin_Extremes":
            build_extremes(args.out)
        elif name == "DB_readme":
            shutil.copy2(args.src / "readme.xlsx", args.out / "DB_readme.xlsx")
            log("DB_readme.xlsx: copied")
        else:
            sys.exit(f"unknown dataset {name}")

    con.close()
    shutil.rmtree(tmp_dir, ignore_errors=True)
    log("done")


if __name__ == "__main__":
    main()
