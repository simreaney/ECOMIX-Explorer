#!/usr/bin/env python3
"""Combine daily-by-site chemical model realizations into parquet.

Looks in this script's own folder for sub-folders, each holding the CSVs for
one chemical's set of model realizations (e.g. `fipronil/` with 25 CSVs,
`imidacloprid/` with its own set, ...). Every CSV in a sub-folder is expected
to share the same grid: sites x days (`subbasin, SITE_ID, POINT, day, OMP`).

Each sub-folder found gets two parquet files, written back into this
script's folder and named after the sub-folder:

1. `<sub-folder>.parquet` - all of that sub-folder's CSVs stacked into one
   long table, tagged with a `realization` column (assigned in file order)
   so any single run's series can still be recovered.

2. `<sub-folder>_percentiles.parquet` - the 10th/50th/90th percentile of OMP
   across that sub-folder's realizations, computed per (subbasin, site_id,
   point, day).

OMP is stored as 32-bit float (source values only carry ~6-7 significant
digits) and both files are written with zstd at its max compression level
(22) to keep them small.

Both steps run in DuckDB so memory use stays bounded regardless of how many
total rows are involved.
"""

from pathlib import Path

import duckdb

SCRIPT_DIR = Path(__file__).resolve().parent


def find_input_groups() -> dict[str, list[Path]]:
    groups = {}
    for entry in sorted(SCRIPT_DIR.iterdir()):
        if not entry.is_dir():
            continue
        csvs = sorted(entry.glob("*.csv"))
        if csvs:
            groups[entry.name] = csvs

    if not groups:
        raise SystemExit(f"No sub-folders containing .csv files found in {SCRIPT_DIR}")

    return groups


def build_combined_parquet(con: duckdb.DuckDBPyConnection, name: str, files: list[Path]) -> Path:
    output_path = SCRIPT_DIR / f"{name}.parquet"
    print(f"[{name}] Combining {len(files)} realization CSVs.")

    per_file_selects = [
        """
        SELECT
            CAST(? AS SMALLINT) AS realization,
            CAST(subbasin AS INTEGER) AS subbasin,
            CAST(SITE_ID AS INTEGER) AS site_id,
            CAST(POINT AS INTEGER) AS point,
            CAST(day AS INTEGER) AS day,
            CAST(OMP AS FLOAT) AS omp
        FROM read_csv_auto(?)
        """
        for _ in files
    ]
    params = [value for i, f in enumerate(files, start=1) for value in (i, str(f))]

    con.execute(
        f"COPY ({' UNION ALL '.join(per_file_selects)}) TO '{output_path}' "
        "(FORMAT PARQUET, COMPRESSION ZSTD, COMPRESSION_LEVEL 22)",
        params,
    )

    total_rows = con.execute("SELECT count(*) FROM read_parquet(?)", [str(output_path)]).fetchone()[0]
    print(f"[{name}] Combined: wrote {total_rows:,} rows to {output_path}")
    return output_path


def build_percentile_parquet(con: duckdb.DuckDBPyConnection, name: str, combined_path: Path) -> None:
    output_path = SCRIPT_DIR / f"{name}_percentiles.parquet"
    print(f"[{name}] Computing 10th/50th/90th percentiles per (site, day) across realizations.")

    con.execute(
        f"""
        COPY (
            SELECT
                subbasin,
                site_id,
                point,
                day,
                count(*) AS n_realizations,
                CAST(percentile_cont(0.1) WITHIN GROUP (ORDER BY omp) AS FLOAT) AS omp_p10,
                CAST(percentile_cont(0.5) WITHIN GROUP (ORDER BY omp) AS FLOAT) AS omp_p50,
                CAST(percentile_cont(0.9) WITHIN GROUP (ORDER BY omp) AS FLOAT) AS omp_p90
            FROM read_parquet('{combined_path}')
            GROUP BY subbasin, site_id, point, day
            ORDER BY site_id, day
        ) TO '{output_path}' (FORMAT PARQUET, COMPRESSION ZSTD, COMPRESSION_LEVEL 22)
        """
    )

    total_rows = con.execute("SELECT count(*) FROM read_parquet(?)", [str(output_path)]).fetchone()[0]
    print(f"[{name}] Percentiles: wrote {total_rows:,} rows to {output_path}")


def main() -> None:
    groups = find_input_groups()
    con = duckdb.connect()
    for name, files in groups.items():
        combined_path = build_combined_parquet(con, name, files)
        build_percentile_parquet(con, name, combined_path)


if __name__ == "__main__":
    main()
