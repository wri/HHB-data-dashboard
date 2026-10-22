# Heat & Health Dashboard — Belo Horizonte & Campinas

A Shiny app for exploring heat-related mortality risk by neighborhood in Belo
Horizonte and Campinas.

## Running the app

Run `app.R` (or click "Run App" in RStudio). It loads `global.R`, `ui.R` and
`server.R`.

The app reads `data/app_data.rds`, which is built from the raw files by
`prepare_data.R`. Re-run it whenever any raw data changes:

```
Rscript prepare_data.R      # from the HHB-data-dashboard folder
```

## Editing text and translations

All text in the app (tab names, labels, legends, chart axes, the welcome
pop-up, the Information tab) lives in `data/translations.xlsx`, sheet `text`:
one row per piece of text, with `english` and `portuguese` columns. The app
reads it at startup; no R code needs changing to reword or translate anything.

- Edit only the `english` and `portuguese` columns. Never change `key`.
- Keep `{placeholders}` (e.g. `{difference} above`): the app fills them in.
- A blank `portuguese` cell shows the English text.
- For long text, a blank line in the cell starts a new paragraph.
- Save, then redeploy the app. The `how to use` sheet repeats these rules.

Users switch language with the **EN | PT** links at the right of the tab bar
(they reload the page with `?lang=en` / `?lang=pt`). The language the app opens
in is `default_language` in `global.R`. In Portuguese, numbers use a decimal
comma (12,5%). City names, barrio names and age groups like `65+` come from the
data and are not translated.

To add a new piece of text in code, add a row with a new key to the
spreadsheet and use `tr("your_key")` in `ui.R` / `server.R`. A key missing from
the spreadsheet shows on screen as the key itself, with a warning in the R
console.

## Deploying

The running app only needs `app.R`, `global.R`, `ui.R`, `server.R`, `www/`,
`data/app_data.rds` (about 3 MB) and `data/translations.xlsx`. The `.rscignore` files in this folder and
in `data/` keep the raw data, `prepare_data.R` and `sandbox.R` out of an
`rsconnect` deployment (e.g. shinyapps.io).

## Source data (`data/`)

Only `data/app_data.rds`, `data/translations.xlsx` and `data/data_dictionary.xlsx` are in the git repo.
The raw data files below are kept locally (`data/*` is in `.gitignore`) and are only needed to re-run
`prepare_data.R`; get them from the project team.

| File | What it is |
|---|---|
| `neighborhood_temperature_citylimits.geojson` | Health polygons: one row per polygon × age group (`all`, `<65`, `65+`), with EDF, relative risk, temperature and neighborhood characteristics. 103 polygons across both cities. |
| `barrios_belohorizonte.geojson`, `barrios_campinas.geojson` | Barrio (neighborhood) boundaries, used for names and outlines only. |
| `curves.csv.gz` | Temperature–mortality curve table: `campinas_bh_nbhd_summer_curves.parquet` from the curve package, saved as a gzipped CSV so no parquet package is needed. |
| `data_dictionary.xlsx` | Column descriptions for the health data. |

## Archive (`archive/`, not in git)

Original files kept for reference only. Neither the app nor `prepare_data.R`
uses them, and `archive/` is in `.gitignore` and `.rscignore`.

| File | What it is |
|---|---|
| `curves/` | The full temperature–mortality curve package as delivered (geodatabase, parquet curve table, figures; see `archive/curves/README.md`). |
| `data.zip` | The original data download. |

## Data processing

All processing happens in `prepare_data.R`, except the display-only steps
listed at the end.

### Health polygons

- Belo Horizonte and Campinas columns have city prefixes in the source
  (`bh_` / `bh_bh_`, `cam_` / `cam_cam_`). These are renamed to shared names
  (`population`, `perc_informal`, `perc_tree`, `perc_imperv`,
  `deprivation_group`) and the two cities are stacked into one table.
- Geometries are repaired with `st_make_valid()`.
- **Population density** is calculated: 2010 census population ÷ polygon area,
  in people per hectare (area measured in the source's metric projection).
- **No health values are modified, averaged or moved onto barrios.** All health
  numbers stay on the health polygons.

### Barrios

- The two cities' barrio files are combined, reduced to `city` and
  `barrio_name`, repaired with `st_make_valid()`, and given a `barrio_id`.

### Linking barrios to health polygons (majority coverage)

Barrios and health polygons have different boundaries, so each barrio is
linked to **one** health polygon: the one that covers the largest share of the
barrio's area. Each barrio is intersected with the health polygons of the same
city, overlap areas are measured, and the largest overlap wins.

This adds two columns to the barrios:

- `majority_health_id` — the `L2_5LOCALID` of that health polygon.
- `majority_health_share` — the fraction of the barrio's area it covers.
  Most barrios sit almost entirely inside one polygon (median ≈ 100%); the
  lowest is about 37%, so for a few barrios the data shown describes only part
  of the barrio.

**In the app:** clicking a barrio shows the data and curve for its majority
health polygon, not the polygon directly under the cursor. A click outside
every barrio falls back to the health polygon under the cursor.

### Temperature–mortality curves

- Read from `data/curves.csv.gz`. Columns kept: `city`,
  `age_group`, `L2_5LOCALID`, `percentile`, `temperature`, `rr`, `rr_lci`,
  `rr_uci`.
- **Filtered to the polygons on the map.** The curve file covers more
  neighborhoods (174 in Belo Horizonte, 109 in Campinas) than the health
  polygons on the map. Only curves whose `L2_5LOCALID` matches a health polygon
  are kept, plus the city-average curve (`L2_5LOCALID = "city"`). That leaves
  67 neighborhood curves for Belo Horizonte and 36 for Campinas, per age group.
  Every health polygon has a curve.
- The city-average curve is used exactly as supplied. It is the average over
  **all** neighborhoods in the curve file, including those not on the map, so
  it is not recalculated from the 67 / 36 curves shown.
- The dashed reference lines on the chart are the 75th, 90th, 95th and 99th
  percentile temperatures, taken from the city-average curve rows. Relative
  risk is 1 at the 75th percentile (the model's centering point).

### Display-only steps (`global.R`)

- Health polygons and barrios are reprojected to latitude/longitude
  (EPSG:4326) for the web maps.
- `deprivation_group` (social inclusion index, 1–5) is turned into labelled
  categories ("1-Very low" … "5-Very high").

### Social inclusion index direction

`deprivation_group` comes from `bh_PC1g` / `cam_PC1g`, which the data
dictionary describes only as "pca 1 group: 1=very low; 2=low; 3=medium;
4=high; 5=very high". It doesn't say which end is high risk.

The dashboard treats **5 as most inclusive (lowest risk) and 1 as least
inclusive (highest risk)**, and colours the maps that way. 
- EDF values are stored as fractions and shown as percentages.
