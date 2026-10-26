# UiOPregnancyAlgorithm

The **UiO pregnancy algorithm** as an R package. It identifies pregnancies, their outcomes and
their start dates in the Norwegian health registries:

| Registry | What it contributes |
|---|---|
| Medical Birth Registry of Norway (MBRN) | births from 12 weeks: live births, stillbirths, late miscarriages, induced abortions, with recorded gestational age |
| Norwegian Patient Registry (NPR) | hospital diagnoses: miscarriage (O02–O03), induced abortion (O04), ectopic (O00) and molar pregnancy (O01), and pregnancy markers |
| Primary-care claims (KUHR) | miscarriages (ICD-10 O02–O03, ICPC-2 W82) and pregnancy markers |

Outcomes: live birth, stillbirth, miscarriage, induced abortion, molar pregnancy and ectopic
pregnancy. Pregnancies that are not in the MBRN are dated with pregnancy markers (for example
Z32.1 *pregnancy confirmed*) or, without markers, with outcome-specific default lengths.

The algorithm was developed and validated by Nordeng et al. (2024). Version 2.0.0 is the version
used in the comparison of four pregnancy algorithms in Norwegian registry data (Nordeng, Hayati,
van Gelder; manuscript in preparation).

**Authors:** Hedvig M. E. Nordeng, Saeed Hayati (maintainer), Marleen M. H. J. van Gelder.

## Installation

```r
# install.packages("remotes")
remotes::install_github("<owner>/UiOPregnancyAlgorithm")
```

Three small C++ helpers are compiled with `Rcpp::sourceCpp()` when the algorithm runs, so a C++
compiler is needed at run time (Rtools on Windows; on TSD it is available in the R installations).

## Quick start

```r
library(UiOPregnancyAlgorithm)

src <- uio_source_files(raw_dir = "N:/durable/Raw_data")   # where the registry extracts are
res <- run_uio_pa(workdir = "N:/durable/work/uio",          # folder for the cached extracts
                  source_files = src)

res[, .N, by = outcome_lable]
attr(res, "settings")                                       # the settings used
```

`res` has one row per pregnancy: the woman (`ID`), the outcome (`outcome_final`,
`outcome_lable`), the outcome date (`date_outcome`), the start (`pr_start_final`), the
gestational age at the outcome (`GA_event_days_final`, `GA_event_week_final`), how the pregnancy
was dated (`dating_method`) and the data sources (`MC_source`, `TOP_source`). See `?run_uio_pa`.

## Time conventions

* The pregnancy start date is **day 0**. The gestational age at the outcome is
  `GA_event_days_final = date_outcome - pr_start_final`, in days.
* Completed weeks are `floor(days / 7)`: day 154 is 22+0 weeks.
* The Norwegian registries' statistics count weeks *ordinally* (week *n* = days 7(*n*−1) to
  7*n*−1). A pregnancy of 56 days (8+0 weeks) is therefore in "week 9", and "before week 9" means
  55 days or fewer.
* "Within *d* days before the outcome" includes both the outcome date and the date *d* days before.

## Settings

Every rule that has a number is an argument of `run_uio_pa()`. The defaults are the settings of
the main analysis of the four-algorithm comparison. `?uio_default_settings` describes each one.

| Argument | Default | Meaning |
|---|---|---|
| `stillbirth_min_days` | `154` | an MBRN fetal death from this gestational age (days) is a stillbirth, below it a miscarriage (154 = 22+0 weeks, the MBRN definition); `NULL` = rule of version 1.3 (completed weeks > 20, i.e. from 147 days) |
| `stillbirth_min_weight_g` | `NULL` | birth weight (g) that also decides stillbirth vs miscarriage, usually `500` |
| `stillbirth_weight_rule` | `"or"` | `"or"`: 22 weeks **or** 500 g; `"and"`: 22 weeks **and** 500 g (weight missing: weeks only); `"weight_first"`: weight when recorded, else weeks |
| `marker_rejection_days` | `119` | a marker-based estimate of 119 days (17 weeks) or more is rejected and the next marker tried; if all are rejected the pregnancy gets 119 days |
| `marker_lookback_days` | `70` | markers are searched from this many days before the outcome |
| `same_event_days` | `56` | records less than 56 days apart are the same event (repeated contacts for one miscarriage or abortion; records shortly after a birth) |
| `induced_abortion_default_days` | `56` | length of an induced abortion without dating information (8+0 weeks); one number or a table by year |
| `miscarriage_default_days` | `70` | idem, miscarriage (10+0 weeks) |
| `molar_default_days` | `77` | idem, molar pregnancy (11+0 weeks) |
| `ectopic_default_days` | `56` | idem, ectopic pregnancy (8+0 weeks) |
| `o35_induced_days` | `80` | length of an induced abortion with an O35 code (prenatal diagnosis) |
| `o35_window_days` | `27` | the O35 code must be recorded 0–27 days after the abortion |
| `induced_abortion_default_distribution` | `NULL` | draw the length of undated induced abortions from a distribution instead of one value |
| `random_seed` | `1` | seed for those draws |
| `normalize_codes` | `TRUE` | match diagnosis codes after removing dots and spaces and converting to upper case (`"Z32.1"` = `"Z321"`) |

```r
# sensitivity analyses
run_uio_pa(workdir = wd, source_files = src, induced_abortion_default_days = 49)
run_uio_pa(workdir = wd, source_files = src, stillbirth_min_weight_g = 500, stillbirth_weight_rule = "and")
```

## Pregnancy markers

`uio_hospital_codes()` (NPR, 15 ICD-10 codes) and `uio_primary_care_codes()` (KUHR, the same 15
codes and 6 ICPC-2 codes) list the markers in priority order with the days from the marker to the
pregnancy start. Nordeng et al. (2024, Supplementary Table 1) considered 45 ICD-10 and 9 ICPC-2
codes and kept those recorded at a median gestational age below 84 days.

## Data

`uio_source_files()` describes the extracts: file paths, the columns used, and dates stored as
day offsets from a reference date. The defaults are the layout of the extracts of the project in
which the package was developed; change them for other deliveries. The extracts are read once
and cached in `workdir`; delete the cached files to read them again.

The package includes all women in the registries, with no age limit. Restrict the study
population (for example to women aged 12–55 years) after the run.

## Citation

Nordeng HME, Lupattelli A, Engjom HM, van Gelder MMHJ. Detecting and dating early non-live
pregnancy outcomes: generation of a novel pregnancy algorithm from Norwegian linked health
registries. *Pharmacoepidemiol Drug Saf.* 2024;33(9):e70002.
[doi:10.1002/pds.70002](https://doi.org/10.1002/pds.70002)

For the package: `citation("UiOPregnancyAlgorithm")`.

## License

Apache License 2.0.
