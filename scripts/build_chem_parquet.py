#!/usr/bin/env python3
"""Combine the wide-format chemical concentration CSVs into compressed
long-format parquet files.

Two datasets are built:

1. Daily-by-site (`1_DAILY_BY_SITE_conc_ng_L/`): one row per date, one
   column per SITE_ID (e.g. `6PPDQ_mean_conc_by_SITE_ID.csv`). Reshaped to
   long format (date, site_id, subbasin, conc_ng_L) using
   `Subbasin_Siteids_Key.csv` to attach the subbasin, and written as a
   Hive-partitioned dataset with one file per chemical x statistic:

       1_DAILY_BY_SITE_conc_ng_L/daily/chemical=<chem>/stat=<stat>/part-0.parquet

   The app filters every query by chemical and stat, which now just selects
   a directory, and usually by subbasin too. Rows are sorted by
   (subbasin, site_id, date) and written in small row groups (at most
   DAILY_ROW_GROUP_SIZE rows, always ending on a subbasin boundary), so the
   int64 min/max statistics of the leading `subbasin` key let Arrow skip
   all but the one row group holding a subbasin. The previous single-file
   layout (one huge row group per chemical x statistic, chemical/stat
   stored dictionary-encoded) could not be pruned at all - Arrow does not
   filter row groups on dictionary column statistics - so every lookup
   scanned all ~76.5M rows.

2. Monthly-by-waterbody (`2_Modelling_results_by_waterbody_monthly_conc_ng_per_L/`):
   one row per month-end date, one column per short numeric waterbody id
   (e.g. `6PPDQ_mean_conc_by_ds_waterbody.csv`). Reshaped to long format
   (date, waterbody_id, chemical, stat, conc_ng_L), replacing the short id
   with the full EA waterbody code via `marthaWBid2EAWBid.csv`. Small
   enough (~1.6M rows) to stay a single file; each CSV is written as its
   own row group(s), and chemical/stat are plain strings (not dictionary
   encoded) so each row group's min == max string statistics let Arrow
   prune on them. The monthly source CSVs are not kept in the repo: when
   none are present, the existing parquet is re-encoded to the current
   schema instead (same rows, same order) rather than overwritten.

The old single-file daily parquet (`1_DAILY_BY_SITE_conc_ng_L.parquet`) is
no longer written or read by the app.

Each CSV is reshaped and written on its own, so memory use stays bounded
regardless of how many files/rows are combined. Outputs are built under a
temporary name and swapped into place at the end, so an interrupted run
never leaves a half-written dataset where the app would read it.
"""

import re
import shutil
from pathlib import Path

import numpy as np
import pandas as pd
import pyarrow as pa
import pyarrow.parquet as pq

REPO_ROOT = Path(__file__).resolve().parent.parent

DAILY_DIR = REPO_ROOT / "data-chem" / "1_DAILY_BY_SITE_conc_ng_L"
DAILY_DATASET_DIR = DAILY_DIR / "daily"
DAILY_KEY_CSV = DAILY_DIR / "Subbasin_Siteids_Key.csv"
DAILY_FILENAME_RE = re.compile(r"^(?P<chemical>.+)_(?P<stat>90th|mean|median)_conc_by_SITE_ID\.csv$")

# Up to ~17 sites' worth of daily rows (1,826 days each) per row group:
# small enough that a one-subbasin read decodes one row group instead of the
# whole file, large enough that the ~32 row groups per file keep metadata
# and compression overhead negligible. Must stay below Arrow's default scan
# batch size (131,072 rows) so each row group reaches the app as one batch -
# see daily_row_group_bounds().
DAILY_ROW_GROUP_SIZE = 32_768

# chemical/stat are not columns in the files themselves - they come from the
# chemical=<chem>/stat=<stat> directory names (Hive partitioning).
DAILY_SCHEMA = pa.schema(
    [
        ("date", pa.date32()),
        ("site_id", pa.int64()),
        ("subbasin", pa.int64()),
        ("conc_ng_L", pa.float64()),
    ]
)

MONTHLY_DIR = REPO_ROOT / "data-chem" / "2_Modelling_results_by_waterbody_monthly_conc_ng_per_L"
MONTHLY_OUTPUT_PATH = MONTHLY_DIR / "2_Modelling_results_by_waterbody_monthly_conc_ng_per_L.parquet"
MONTHLY_KEY_CSV = MONTHLY_DIR / "marthaWBid2EAWBid.csv"
MONTHLY_FILENAME_RE = re.compile(r"^(?P<chemical>.+)_(?P<stat>90th|mean|median)_conc_by_ds_waterbody\.csv$")

MONTHLY_SCHEMA = pa.schema(
    [
        ("date", pa.date32()),
        ("waterbody_id", pa.string()),
        ("chemical", pa.string()),
        ("stat", pa.string()),
        ("conc_ng_L", pa.float64()),
    ]
)


def load_subbasin_key() -> pd.Series:
    key = pd.read_csv(DAILY_KEY_CSV, encoding="utf-8-sig")
    key["SITE_ID"] = key["SITE_ID"].astype("int64")
    key["subbasin"] = key["subbasin"].astype("int64")
    return key.set_index("SITE_ID")["subbasin"]


def load_waterbody_key() -> pd.Series:
    key = pd.read_csv(MONTHLY_KEY_CSV, encoding="utf-8-sig")
    key["id_waterbody"] = key["id_waterbody"].astype("int64")
    return key.set_index("id_waterbody")["water_body"]


def melt_daily_file(path: Path, subbasin_by_site: pd.Series) -> pd.DataFrame:
    df = pd.read_csv(path)
    date_col = df.columns[0]
    df = df.rename(columns={date_col: "date"})

    long_df = df.melt(id_vars="date", var_name="site_id", value_name="conc_ng_L")
    long_df["date"] = pd.to_datetime(long_df["date"])
    long_df["site_id"] = long_df["site_id"].astype("int64")
    long_df["subbasin"] = long_df["site_id"].map(subbasin_by_site).astype("Int64")
    long_df = long_df.dropna(subset=["conc_ng_L"])
    # Raises if a SITE_ID is missing from Subbasin_Siteids_Key.csv.
    long_df["subbasin"] = long_df["subbasin"].astype("int64")

    # app.R orders its results by (site_id, date) / (date, site_id)
    # explicitly, which only fully determines the order if each site has at
    # most one value per date.
    if long_df.duplicated(["site_id", "date"]).any():
        raise ValueError(f"{path.name}: duplicate (SITE_ID, date) rows")

    long_df = long_df.sort_values(["subbasin", "site_id", "date"])
    long_df["date"] = long_df["date"].dt.date

    return long_df[["date", "site_id", "subbasin", "conc_ng_L"]]


def melt_monthly_file(path: Path, chemical: str, stat: str, waterbody_by_id: pd.Series) -> pd.DataFrame:
    df = pd.read_csv(path)
    date_col = df.columns[0]
    df = df.rename(columns={date_col: "date"})

    long_df = df.melt(id_vars="date", var_name="waterbody_id", value_name="conc_ng_L")
    long_df["date"] = pd.to_datetime(long_df["date"]).dt.date
    long_df["waterbody_id"] = long_df["waterbody_id"].astype("int64").map(waterbody_by_id)
    long_df["chemical"] = chemical
    long_df["stat"] = stat
    long_df = long_df.dropna(subset=["conc_ng_L"])

    return long_df[["date", "waterbody_id", "chemical", "stat", "conc_ng_L"]]


def daily_row_group_bounds(subbasin: pd.Series, max_rows: int = DAILY_ROW_GROUP_SIZE) -> list[tuple[int, int]]:
    """(start, stop) row ranges packing whole subbasins into row groups of
    at most `max_rows` rows (a single subbasin larger than that gets a row
    group of its own). `subbasin` must already be sorted.

    Ending row groups on subbasin boundaries rather than every `max_rows`
    rows means (a) a one-subbasin read touches exactly one row group, and
    (b) no site's series is split across two row groups. (b) is what keeps
    app.R's per-site means (get_chem_spatial_summary(), an Arrow group_by/
    summarise) reproducible: Arrow sums each group within a scan batch in
    row order, but a group split over two batches is summed as two partial
    sums combined in whichever order the threads finish, so its last bits
    varied between runs and with the number of CPU cores.
    """
    values = subbasin.to_numpy()
    # End (exclusive) of every subbasin's run of rows.
    run_ends = [*(np.flatnonzero(np.diff(values) != 0) + 1).tolist(), len(values)]
    bounds = []
    group_start = run_start = 0
    for run_end in run_ends:
        # Close the current row group before a subbasin that would overflow it.
        if run_end - group_start > max_rows and run_start > group_start:
            bounds.append((group_start, run_start))
            group_start = run_start
        run_start = run_end
    if len(values) > group_start:
        bounds.append((group_start, len(values)))
    return bounds


def build_daily_dataset() -> None:
    subbasin_by_site = load_subbasin_key()

    files = sorted(
        (path, m.group("chemical"), m.group("stat"))
        for path in DAILY_DIR.glob("*.csv")
        if (m := DAILY_FILENAME_RE.match(path.name))
    )
    print(f"[daily-by-site] Found {len(files)} chemical concentration CSVs to combine.")
    if not files:
        raise SystemExit(f"[daily-by-site] No source CSVs in {DAILY_DIR} - leaving {DAILY_DATASET_DIR} untouched")

    # Built under a dot-prefixed name (which Arrow dataset discovery ignores)
    # and swapped in at the end; rebuilding from scratch also drops the
    # partition of any CSV that has since been removed.
    tmp_dir = DAILY_DIR / ".daily.tmp"
    if tmp_dir.exists():
        shutil.rmtree(tmp_dir)

    total_rows = 0
    for i, (path, chemical, stat) in enumerate(files, start=1):
        long_df = melt_daily_file(path, subbasin_by_site)

        table = pa.Table.from_pandas(long_df, schema=DAILY_SCHEMA, preserve_index=False)
        table = table.replace_schema_metadata(None)  # no pandas index metadata
        out_file = tmp_dir / f"chemical={chemical}" / f"stat={stat}" / "part-0.parquet"
        out_file.parent.mkdir(parents=True)
        with pq.ParquetWriter(out_file, DAILY_SCHEMA, compression="zstd") as writer:
            for start, stop in daily_row_group_bounds(long_df["subbasin"]):
                writer.write_table(table.slice(start, stop - start), row_group_size=stop - start)

        total_rows += len(long_df)
        print(f"[daily-by-site] [{i}/{len(files)}] {path.name}: {len(long_df):,} rows")

    if DAILY_DATASET_DIR.exists():
        shutil.rmtree(DAILY_DATASET_DIR)
    tmp_dir.rename(DAILY_DATASET_DIR)

    print(f"[daily-by-site] Done. Wrote {total_rows:,} rows to {DAILY_DATASET_DIR}")


def write_monthly_parquet(tables) -> int:
    """Stream `tables` into MONTHLY_OUTPUT_PATH, one write_table() each.

    One write_table() per chemical x statistic keeps every row group to a
    single chemical/stat, so its chemical/stat statistics have min == max.
    """
    tmp_path = MONTHLY_OUTPUT_PATH.with_name("." + MONTHLY_OUTPUT_PATH.name + ".tmp")
    writer = pq.ParquetWriter(tmp_path, MONTHLY_SCHEMA, compression="zstd")
    total_rows = 0
    try:
        for table in tables:
            writer.write_table(table)
            total_rows += table.num_rows
    finally:
        writer.close()
    tmp_path.replace(MONTHLY_OUTPUT_PATH)
    return total_rows


def build_monthly_parquet() -> None:
    waterbody_by_id = load_waterbody_key()

    files = sorted(
        (path, m.group("chemical"), m.group("stat"))
        for path in MONTHLY_DIR.glob("*.csv")
        if (m := MONTHLY_FILENAME_RE.match(path.name))
    )
    print(f"[monthly-by-waterbody] Found {len(files)} chemical concentration CSVs to combine.")

    # The monthly source CSVs are not kept in the repo (only the key CSV
    # is), so a run without them must not replace the existing parquet with
    # an empty one. Instead the existing file is re-encoded to MONTHLY_SCHEMA
    # (same rows, same order), which is all a schema-only change needs.
    if not files:
        if not MONTHLY_OUTPUT_PATH.exists():
            raise SystemExit(f"[monthly-by-waterbody] No source CSVs and no existing {MONTHLY_OUTPUT_PATH.name}")
        reencode_monthly_parquet()
        return

    def tables():
        for i, (path, chemical, stat) in enumerate(files, start=1):
            long_df = melt_monthly_file(path, chemical, stat, waterbody_by_id)
            print(f"[monthly-by-waterbody] [{i}/{len(files)}] {path.name}: {len(long_df):,} rows")
            yield pa.Table.from_pandas(long_df, schema=MONTHLY_SCHEMA, preserve_index=False)

    total_rows = write_monthly_parquet(tables())
    print(f"[monthly-by-waterbody] Done. Wrote {total_rows:,} rows to {MONTHLY_OUTPUT_PATH}")


def reencode_monthly_parquet() -> None:
    print(f"[monthly-by-waterbody] Re-encoding the existing {MONTHLY_OUTPUT_PATH.name} instead.")
    with pq.ParquetFile(MONTHLY_OUTPUT_PATH) as src:
        # Each source row group is already one chemical x statistic (one
        # write_table() per CSV when it was built), so copying row group by
        # row group keeps that property.
        def tables():
            for i in range(src.num_row_groups):
                yield src.read_row_group(i).cast(MONTHLY_SCHEMA)

        total_rows = write_monthly_parquet(tables())
    print(f"[monthly-by-waterbody] Done. Re-encoded {total_rows:,} rows in {MONTHLY_OUTPUT_PATH}")


def main() -> None:
    build_daily_dataset()
    build_monthly_parquet()


if __name__ == "__main__":
    main()
