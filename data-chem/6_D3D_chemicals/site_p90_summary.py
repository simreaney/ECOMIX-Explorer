"""
Summarise the daily fipronil P90 time series for each site.

Input:  6_fipronil_daily_by_site_percentiles.parquet
        One row per site per day, with omp_p10 / omp_p50 / omp_p90
        already computed across model realizations for that day.

Output: For each site, across its full daily time series of omp_p90:
          - median_p90 : median of the daily P90 values
          - q1_p90     : 25th percentile of the daily P90 values
          - q3_p90     : 75th percentile of the daily P90 values
          - iqr_p90    : q3_p90 - q1_p90
"""

import pandas as pd

INPUT_PATH = "6_fipronil_daily_by_site_percentiles.parquet"
OUTPUT_PATH = "6_fipronil_site_p90_summary.parquet"
OUTPUT_CSV_PATH = "6_fipronil_site_p90_summary.csv"


def summarise(df: pd.DataFrame) -> pd.DataFrame:
    grouped = df.groupby(["site_id", "subbasin", "point"])["omp_p90"]

    summary = grouped.agg(
        median_p90="median",
        q1_p90=lambda s: s.quantile(0.25),
        q3_p90=lambda s: s.quantile(0.75),
        n_days="count",
    ).reset_index()

    summary["iqr_p90"] = summary["q3_p90"] - summary["q1_p90"]

    return summary[
        ["site_id", "subbasin", "point", "n_days", "median_p90", "q1_p90", "q3_p90", "iqr_p90"]
    ].sort_values("site_id")


def main() -> None:
    df = pd.read_parquet(INPUT_PATH)
    summary = summarise(df)

    summary.to_parquet(OUTPUT_PATH, index=False)
    summary.to_csv(OUTPUT_CSV_PATH, index=False)

    print(f"Summarised {summary.shape[0]} sites")
    print(summary.head())


if __name__ == "__main__":
    main()
