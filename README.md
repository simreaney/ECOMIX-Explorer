# ECOMIX-Explorer
Interactive dashboard for ECOMIX data.

This is the web dashboard code for the [ECOMIX](https://water.leeds.ac.uk/fwq-programme/assessing-and-managing-the-impacts-of-mixtures-of-chemicals-on-uk-freshwater-biodiversity/) project, that is part of the [NERC Freshwater Quality Programme](https://water.leeds.ac.uk/event_category/freshwater-quality-programme/).

## Web version (GitHub Pages)

[docs/](docs/) holds a static version of the dashboard that runs entirely in the browser, so it can be hosted on GitHub Pages with no server. It has the same eight tabs as the Shiny app below. Charts are drawn with [Observable Plot](https://observablehq.com/plot) and d3, maps with [Leaflet](https://leafletjs.com), and downloads (CSV, XLSX, Parquet, GeoPackage, Shapefile, GeoParquet, GeoJSON) are written in the browser. Every library is vendored in [docs/vendor/](docs/vendor/) (see [LICENSES.md](docs/vendor/LICENSES.md)), so the site needs nothing from a CDN.

**Publishing.** In the repository's *Settings → Pages*, set *Source* to *GitHub Actions*. [.github/workflows/pages.yml](.github/workflows/pages.yml) then deploys `docs/` on every push to `main` that changes it. The site is served at `https://<owner>.github.io/ECOMIX-Explorer/`.

**Updating the data.** The site reads compact pre-built files from [docs/data/](docs/data/), produced from the source datasets by [scripts/build_site.py](scripts/build_site.py):

```sh
pip install numpy pandas pyarrow geopandas pyogrio shapely pyproj rdata
python3 scripts/build_site.py          # all steps; a few minutes
python3 scripts/build_site.py --only chem wbchem   # just some steps
```

Commit `docs/data/` afterwards. The build is deterministic, so unchanged inputs produce byte-identical files and no git diff.

Several source datasets are gitignored and only exist where they were built: the HYPE projections (`data/DB_Proj_Year`, `data/DB_Proj_Month`), the UKCP18 climate summary (`data/DB_Proj_Forcing`), simulated vs observed hydrology (`data/DB_Historical_Sim_Obs`), the D3D percentile series (`data-chem/7_chemicals`) and source apportionment (`data-chem/6_Source_apportionment`). Run the build on a machine that has them. Any that are missing are listed at the end of the build, and the site shows a "not published" note in place of the views that need them.

**Precision and size.** Values are stored as float32 rounded to 14 mantissa bits (4–5 significant figures, relative error below 0.004 %), which is also the precision of the site's downloads; integers and identifiers are exact. GitHub Pages sites are limited to 1 GB. The build warns past 900 MB; `--keepbits 12` makes the files smaller, `--keepbits 23` keeps full float32 precision.

**Local preview.** The site loads its data with `fetch`, so it has to be served over HTTP rather than opened from disk:

```sh
python3 -m http.server --directory docs 8000   # then open http://localhost:8000
```

The selected tab, location and main settings are kept in the URL, so any view can be bookmarked or shared.

## Dashboard (R Shiny)

The original dashboard ([app.R](app.R)) is an R Shiny app that lets users explore hydrological and chemical exposure data for the study catchments, both as they were observed/simulated historically and as they are projected to change under future climate scenarios. It is organised into the following tabs:

* **Map** — search for or browse subbasins on an interactive map, with toggleable layers for subbasin/operational catchment boundaries, waterbodies (modelled), measured sites, observed hydrology sites and chemical data coverage.
* **Site Details** — for a selected measured site or modelled water body (switchable via a toggle in the sidebar), view a chemical occurrence/concentration grid and configurable time-series panels for up to four determinands.
* **Hydro Explorer** — for a selected subbasin (or, in Water body mode, a selected modelled water body), compare climate variables, simulated vs. observed hydrology, and projections (yearly, monthly, distribution/cumulative-frequency, and daily for chemicals) across scenarios and future periods.
* **Chemical Explorer** — D3D-modelled daily percentile chemical concentration time series for a selected chemical and water body, with acute/chronic level-of-concern threshold lines.
* **Ecological Risk** — detail view for a site/chemical reached by clicking a point on the Map's Ecological risk layer.
* **Spatial Datasets** — view any modelled hydrological, subbasin chemical, or water body chemical variable as a coloured map layer, for a chosen period/percentile or chemical statistic.
* **Download** — build and preview a custom data extract (variable, data type, spatial layer, format) and download it as a file.

Underlying data (HYPE hydrological model output, chemical concentration predictions for both measured sites and modelled water bodies, and supporting GIS layers) lives in [data/](data/), [data-chem/](data-chem/) and [gis-data/](gis-data/), with data preparation scripts in [scripts/](scripts/).

This project will develop a novel assessment framework for assessing the real impacts of chemical pollution in UK rivers. It will identify and manage hotspots of risk, helping to halt the decline in freshwater biodiversity.

The framework will be developed not only to assess currently chemical impacts but also future impacts resulting from climate change, urbanisation and population growth. It will allow mitigation and adaptation approaches to be targeted where they will have the greatest benefit. The framework will also deliver models for assessing the impacts of chemical mixtures and co-stressors on biodiversity.

This project will:

* investigate the most damaging chemicals being emitted into UK freshwaters
* characterise current (2002-2022) and future (2061-2080) chemical exposure and general water quality parameter profiles for the study catchments
* estimate the effects of chemicals on UK-relevant species
* predict the current and future effects of chemical mixtures on biodiversity and ecosystem function
* identify interventions to mitigate the impacts of chemicals on biodiversity now and under future climate and catchment change.

The modelling tools developed during this project will inform the development of better plans for adaptation and mitigation of risks associated with declining water quality now and in the future. Led by Professor Alistair Boxall, University of York, with partners University of Sheffield and Durham University. 

