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
remotes::install_github("PharmaSafeUiO/UiOPregnancyAlgorithm")
```

Three small C++ helpers are compiled with `Rcpp::sourceCpp()` when the algorithm runs, so a C++
compiler is needed at run time (Rtools on Windows; on TSD it is available in the R installations).

## Quick start

```r
library(UiOPregnancyAlgorithm)

src <- uio_project_sources("p1111")                          # the registry extracts (see below)
res <- run_uio_pa(workdir = "N:/durable/work/uio",           # an existing folder for the cached extracts
                  source_files = src)

```

At the end of the run a summary is printed: which pregnancy codes are found in NPR and KUHR (with
each marker's days to the start), the settings, and the number of pregnancies of each outcome.
The results are saved in `workdir/output` (choose another folder with `output_dir`):

| File | Content |
|---|---|
| `uio_pregnancies.rds` | the pregnancies (also returned by `run_uio_pa()`) |
| `uio_report.html` | report: outcomes, by year, by the woman's age, gestational age per outcome, dating, sources, codes, settings |
| `uio_outcomes.csv` | number of pregnancies and gestational age per outcome |
| `uio_code_availability.csv` | records and women for each pregnancy marker and outcome code in NPR and KUHR |

In the report and the CSV files, counts of 1–4 are shown as `<5`, so they can be exported from TSD.

Each pregnancy has:

| Column | Content |
|---|---|
| `ID`, `preg_id` | the woman and the pregnancy |
| `outcome_final`, `outcome_lable` | 1 live birth, 2 miscarriage, 3 induced abortion, 4 stillbirth, 5 molar, 6 ectopic pregnancy |
| `date_outcome`, `year_outcome`, `pr_start_final` | outcome date and start (day 0) |
| `GA_event_days_final`, `GA_event_week_final` | gestational age at the outcome, days and completed weeks |
| `dating_class` | `recorded` (MBRN), `estimated` (pregnancy marker) or `fixed` (default length, marker limit, O35 rule) |
| `dating_method`, `dating_source`, `dating_code` | how the start was set, from which registry, and the marker or O35 code |
| `outcome_source`, `outcome_code` | the registry that decided the outcome (MBRN, then NPR, then KUHR) and its code (`DODKAT 7`, `O039`, `W82`) |
| `codes_mbrn`, `codes_npr`, `codes_kuhr` | all pregnancy codes of the episode in each registry |
| `MC_source`, `TOP_source` | registries that recorded a miscarriage / an induced abortion |
| `mother_age` | the woman's age at the outcome |

## Time conventions

* The pregnancy start date is **day 0**. The gestational age at the outcome is
  `GA_event_days_final = date_outcome - pr_start_final`, in days.
* Completed weeks are `floor(days / 7)`: day 154 is 22+0 weeks.
* The Norwegian registries' statistics count weeks *ordinally* (week *n* = days 7(*n*−1) to
  7*n*−1). A pregnancy of 49 days (7+0 weeks) is therefore in "week 8", and "before week 9" means
  55 days or fewer.
* "Within *d* days before the outcome" includes both the outcome date and the date *d* days before.

## Settings

Every rule that has a number is an argument of `run_uio_pa()`. The defaults are the settings of
the main analysis of the four-algorithm comparison, with birth weight added to the stillbirth rule
(the MBRN definition). `?uio_default_settings` describes each one.

| Argument | Default | Meaning |
|---|---|---|
| `stillbirth_min_days` | `154` | an MBRN fetal death from this gestational age (days) is a stillbirth, below it a miscarriage (154 = 22+0 weeks, the MBRN definition); `NULL` = rule of version 1.3 (completed weeks > 20, i.e. from 147 days) |
| `stillbirth_min_weight_g` | `500` | birth weight (g) that also decides stillbirth vs miscarriage; used when the birth weight is mapped, `NULL` turns it off |
| `stillbirth_weight_rule` | `"and"` | `"and"`: 22 weeks **and** 500 g, weeks only when the weight is missing (the MBRN definition); `"or"`: 22 weeks **or** 500 g; `"weight_first"`: weight when recorded, else weeks |
| `marker_rejection_days` | `119` | a marker-based estimate of 119 days (17 weeks) or more is rejected and the next marker tried; if all are rejected the pregnancy gets 119 days |
| `marker_lookback_days` | `70` | markers are searched from this many days before the outcome |
| `same_event_days` | `56` | records less than 56 days apart are the same event (repeated contacts for one miscarriage or abortion; records shortly after a birth) |
| `induced_abortion_default_days` | `NULL` | length of an induced abortion without dating information; `NULL` = the median of the outcome year in the Registry of Pregnancy Termination (44–48 days, see below), one number = a fixed length (for example `49`, 7+0 weeks), or your own table by year |
| `miscarriage_default_days` | `70` | idem, miscarriage (10+0 weeks) |
| `molar_default_days` | `77` | idem, molar pregnancy (11+0 weeks) |
| `ectopic_default_days` | `56` | idem, ectopic pregnancy (8+0 weeks) |
| `o35_induced_days` | `80` | length of an induced abortion with an O35 code (prenatal diagnosis) |
| `o35_window_days` | `27` | the O35 code must be recorded 0–27 days after the abortion |
| `normalize_codes` | `TRUE` | match diagnosis codes after removing dots and spaces and converting to upper case (`"Z32.1"` = `"Z321"`) |

```r
# sensitivity analyses
run_uio_pa(workdir = wd, source_files = src, induced_abortion_default_days = 49)   # one fixed length
run_uio_pa(workdir = wd, source_files = src, stillbirth_min_weight_g = NULL)        # gestational age only
```

### Year-specific default for induced abortions

`uio_termination_days_by_year()` gives the median gestational age of induced abortions in Norway for
each year 2008–2024 (44–48 days), from the Registry of Pregnancy Termination (FHI, Tabell 6 of the
reports for 2024, 2018 and 2017; interpolated within weeks 7–8). It is the default
(`induced_abortion_default_days = NULL`): an undated induced abortion gets the median of the year of
its outcome.

Outcome years before 2008 or after 2024 have no registry value. Their values are **extrapolated**
from the linear trend of the table, rounded and kept within 44–48 days. The run then gives a warning
that lists these years and values, and the same note appears in the printed summary and the HTML
report. To give every undated induced abortion one fixed length instead, give
`induced_abortion_default_days` one number of days (for example `49`).

## Pregnancy markers

`uio_hospital_codes()` (NPR, 15 ICD-10 codes) and `uio_primary_care_codes()` (KUHR, the same 15
codes and 6 ICPC-2 codes) list the markers in priority order with the days from the marker to the
pregnancy start. Nordeng et al. (2024, Supplementary Table 1) considered 45 ICD-10 and 9 ICPC-2
codes and kept those recorded at a median gestational age below 84 days.

## Connecting the source data

The algorithm needs three extracts: the MBRN (one row per child), the NPR (diagnoses) and KUHR
(diagnoses). They are read once and cached in `workdir`; delete the cached `*_full.csv` files there
to read them again (for example after a new delivery).

### Projects P1111, P704 and P1380

One line. The file names, columns and date forms of these projects are built in:

```r
src <- uio_project_sources("p1111")   # MBRN, NPR (7 extracts), KUHR; day offsets from 25 Dec 1975
src <- uio_project_sources("p704")    # MBRN, NPR_mor (tilstand_1, ...), KUHR; day offsets from 24 Sep 1997
src <- uio_project_sources("p1380")   # MBRN (.sav), NPR (6 contact + code files), KUHR (.dsv); real dates

res <- run_uio_pa(workdir = "N:/durable/<your folder>/uio", source_files = src)
```

If the extracts are in another folder than on TSD, give it: `uio_project_sources("p704", raw_dir = "...")`.

### Other projects

Describe each extract with `uio_source()`: the file, which column holds what, and how its dates are
stored (`uio_dates()`).

```r
src <- uio_source_files(
  birth_registry = uio_source("N:/durable/data/mbrn.csv",
    columns = c(mother_id = "MOR_ID", mother_age = "MORS_ALDER", event_date = "FDATO",
                child_order = "PLUREK", gestational_days = "SVLEN_DG", death_cat = "DODKAT",
                apgar5 = "APGAR5"),
    dates = uio_dates(format = "dmy")),
  hospital = uio_source("N:/durable/data/npr_diagnoses.csv",            # one row per diagnosis
    columns = c(mother_id = "ID", diagnose_code = "ICD10", visit_date = "INNDATO", gender = "KJONN"),
    dates = uio_dates(ref_date = "2000-01-01")),
  primary_care = uio_source("N:/durable/data/kuhr/.*[.]csv",            # all matching files
    columns = c(mother_id = "ID", diagnose_code = "DIAGNOSE", vocabulary = "KODEVERK",
                visit_date = "DATO", gender = "KJONN"),
    dates = uio_dates()))
```

#### The MBRN in current deliveries

FHI now delivers the MBRN with lower-case names and recommends less identifying variables: the vital
status `leve_dodstatus_fodsel` (1 live born, 2 stillborn or spontaneous abortion, 3 abortion approved
by a board) instead of `dodkat`, and the gestational age in completed weeks `svlen` instead of
`svlen_dg` (days). `uio_mbrn_columns()` gives these names; keep those your delivery has:

```r
mbrn <- uio_source("N:/durable/data/mfr.csv",
  columns = uio_mbrn_columns(event_date = "fdato",                 # or the project's day difference
                             vital_status = "leve_dodstatus_fodsel", # or "dodkat"
                             gestational_age = "svlen_dg",           # or "svlen" (weeks)
                             optional = c("child_order", "mother_age", "birth_weight")))
```

With `svlen`, the gestational age is the middle of the completed week (weeks × 7 + 3 days). With
`dodkat`, both the old coding and the coding of 2024 work (live born 0–4, earlier also 5, 6, 11, 12;
stillborn 7–9; abortion approved by a board 10).

#### Dates

Dates can be stored in three ways, and each extract can use its own:

| How the dates are stored | `dates =` |
|---|---|
| real dates (or ISO text, `2015-06-15`) | `uio_dates()` |
| text in another form (`15.06.2015`, `06/15/2015 00:00:00`) | `uio_dates(format = "dmy")`, `uio_dates(format = "mdy HMS")` |
| days from one reference date | `uio_dates(ref_date = "1997-09-24")` |
| days from a reference date that differs per woman | `uio_dates(ref_dates = "reference_dates.csv")` |

For the last form, give a file with one row per woman and two columns, `person_id` (her ID, as in
the extracts) and `ref_date` (her reference date):

```
person_id,ref_date
1001,1989-03-17
1002,1992-11-02
```

Other column names: `uio_dates(ref_dates = "file.csv", person_id = "LOPENR", ref_column = "REFDATO")`.

Files can be `.csv`, `.txt`, tab-separated `.dsv`/`.tsv`, Stata `.dta` or SPSS `.sav`. The NPR must
be one row per diagnosis (`mother_id`, `diagnose_code`, `visit_date`). In the NPR and KUHR extracts,
a `gender` column keeps only women (`female_value = 2` by default, set per extract in `uio_source()`);
without it, all records are taken as women's. The MBRN extract has no gender filter. `?uio_source`
lists all columns.

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
