# UiOPregnancyAlgorithm 2.0.0

The version used in the comparison of four pregnancy algorithms (Nordeng, Hayati, van Gelder).

## Settings as arguments

* Every setting is now an argument of `run_uio_pa()` with its default, documented in
  `?run_uio_pa` and `?uio_default_settings`. They can still be given together as a list
  (`settings =`); a setting given both ways is an error.
* The defaults are the settings of the main analysis of the comparison: `stillbirth_min_days = 154`
  (22+0 weeks, the MBRN definition), `marker_rejection_days = 119` (17 weeks, as in Nordeng et al.
  2024), `normalize_codes = TRUE`, `induced_abortion_default_days = NULL` (the median gestational age
  of induced abortions in the year of the outcome, from the Norwegian Registry of Pregnancy Termination,
  see `uio_termination_days_by_year()`; one number gives a fixed length), and the other values of
  version 1.3; in addition the birth weight
  takes part in the stillbirth rule when it is mapped (`stillbirth_min_weight_g = 500`): a fetal death is a
  stillbirth from 22 weeks **or** 500 g, the MBRN definition. The setting `stillbirth_weight_rule` of the
  earlier 2.0.0 builds ("and", "or", "weight_first") is removed: the rule is always "or".
* New function `uio_v13_settings()`: the settings of the R script version 1.3.
* New function `uio_termination_days_by_year()`: the median gestational age of induced abortions in
  Norway per year 2008-2024 (Registry of Pregnancy Termination, Tabell 6 of the FHI reports for 2024,
  2018 and 2017): the default of `induced_abortion_default_days`. Outcome years outside 2008-2024 are
  extrapolated from the trend of the table (kept within its range); the run warns which years and
  values, with a note in the summary and the report, and explains how to use one fixed length instead.
* `workdir` has no default any more (it was a project folder on TSD).

## Results: description of each pregnancy, summary, output folder

* Each pregnancy now has `year_outcome`, `dating_class` (recorded, estimated, fixed), `dating_source`
  and `dating_code` (the marker or O35 code that set the start), `outcome_source` and `outcome_code`
  (the registry and code that decided the outcome), `codes_mbrn`, `codes_npr`, `codes_kuhr` (all
  pregnancy codes of the episode) and `mother_age`.
* At the end of the run a summary is printed: the availability of the codes in NPR and KUHR (with
  each marker's days to the start), the settings, and the number of pregnancies of each outcome.
* `output_dir` (default `workdir/output`) receives the pregnancies (`.rds`), the outcome table and the
  code availability (`.csv`) and an HTML report (by year, by age, gestational age per outcome, dating,
  sources, codes, settings). Counts of 1-4 are shown as `<5` (`mask_below`).

## Source data

* `uio_project_sources("p1111")`, `"p704"` and `"p1380"` return the complete definition of the
  extracts of these projects (file names, columns, date forms, and the assembly of the NPR from its
  raw extracts: seven extracts in P1111, the mothers' extracts with one column per diagnosis in P704,
  contact and code files in P1380).
* For other projects, `uio_source()` describes one extract (file or data frame, columns, dates) and
  `uio_source_files()` combines the three; missing required columns are reported.
* `uio_dates()`: real dates (`format` for text dates, parsed with `lubridate::parse_date_time()`),
  day offsets from one reference date (`ref_date`), or day offsets from a reference date per woman
  (`ref_dates`, a file with `person_id` and `ref_date`).
* SPSS `.sav` and `.tsv` files are read; value labels are removed. The NPR `stay_id` and the KUHR
  `gender` column are optional (without `gender`, the extract is taken to hold women only), as is
  `birth_year` (a birth-year limit without the column was an error). `female_value` gives the code
  of women in the `gender` column.
* `source_files` has no default in `run_uio_pa()`.
* The columns of each source file are checked before it is read: a column whose name differs only in
  upper/lower case is used (with a message), and a missing column is an error that names the file and
  lists its columns (before, the column was skipped with a warning and the run failed later in `setnames`).
* `.dsv` and `.tsv` files are read as tab-separated, as before; a file whose header has no tab is read with
  the separator that `fread` detects. The P1380 KUHR files of the 2024 delivery are separated by `;`, so
  they could not be read before.
* Diagnosis codes and code systems read from Latin-1 files (for example the letter O-slash in the P1380
  KUHR files) are converted to UTF-8 in memory; before, the code normalisation stopped with "input string
  ... is invalid" in a UTF-8 session.
* Pregnancies without an outcome (`outcome_final` NA, kept as in version 1.3) no longer stop the HTML report:
  they are left out of the tables and charts, and a note and a warning give their number. They are MBRN
  multiple births whose combination of child outcomes the algorithm does not list (e.g. a triplet with two
  live births and a miscarriage; any quadruplet other than four live births, or two miscarriages and two
  terminations), or an MBRN `DODKAT` that is not a known code.
* Memory: right after reading, NPR and KUHR are reduced to the records whose code the algorithm uses (the
  markers and the outcome codes), in blocks; large projects (P704) ran out of memory before.
* Pregnancy ids: the pregnancies of NPR only and of KUHR only were numbered from 1,000,001 and 2,000,001 (as in
  version 1.3); with more than a million NPR records these numbers could be equal to other pregnancies' ids, and
  records of different pregnancies (e.g. a KUHR miscarriage and an NPR termination) were merged. The numbering now
  starts above the largest id in use, and a check stops the run if two pregnancies share an id.
* The MBRN of current deliveries: the vital status `vital_status` (`leve_dodstatus_fodsel`: 1, 2, 3) can
  replace `death_cat` (`DODKAT`, old or 2024 coding), the gestational age in weeks `gestational_weeks`
  (`svlen`, used as weeks x 7 + 3 days) can replace `gestational_days`, and `abortion_weeks`
  (`svlen_abr`) fills it for abortions approved by a board. `apgar5`, `mother_age` and `child_order`
  are optional. `uio_mbrn_columns()` gives the names of the current MBRN variables.
* P1380: all six NPR parts (SOM, AVTSOM, PHBU, PHV, AVTPHV, TSB). The birth weight (`VEKT`) is mapped in
  the three project definitions.
* Without a mapped birth weight the stillbirth rule uses gestational age alone, with a message.

## New settings (defaults = the values of version 1.3)

* `marker_lookback_days` (70): markers are collected from this many days before the outcome.
* `same_event_days` (56): records less than this many days apart are the same event. Version 1.3
  computed the episode thresholds 1-60 days and used only 55; only the one that is used is now
  computed (same result, faster).
* `o35_window_days` (27): an O35 code from the abortion date up to this many days after it.

## Other

* The hospital `vocabulary` variable of `uio_source_files()` is now found when it is given (it was
  looked up as "vocabulay"); with the default source files nothing changes.
* Help pages describe how the algorithm works, the time conventions (the start is day 0; gestational
  age = outcome date - start), the output columns and the source files.
* The original v1.3 script is included for the regression test (`tests/regression/reference/`).
* `induced_abortion_default_distribution` and `random_seed` (version 1.4.0) are removed; the default of
  `induced_abortion_default_days` is now the year table of the Registry of Pregnancy Termination.

# UiOPregnancyAlgorithm 1.4.0

Two new options. Both are off by default, so the defaults give the same results as 1.3.1.

## Default lengths of undated induced abortions

* `induced_abortion_default_days` (and the other outcome-specific defaults: `miscarriage_default_days`,
  `molar_default_days`, `ectopic_default_days`) accepts a data frame with columns `year` and `days`, for a
  default that depends on the year of the outcome. A year that is not listed takes the value of the closest
  earlier listed year; years before the first listed year take the first value.
* `induced_abortion_default_distribution`: a data frame with columns `days_min`, `days_max`, `prob` and
  optionally `year`. Each induced abortion without other dating information gets a length drawn from it
  (a row with probability `prob`, rescaled within the year, then a whole number of days uniformly in
  `days_min`-`days_max`), so that one fixed value does not create an artificial spike. The draws are
  reproducible with `random_seed` (default 1); the caller's random-number state is restored. These
  pregnancies have `dating_method` "fixed outcome-specific default (drawn from distribution)".

## Birth weight for stillbirth

* `stillbirth_min_weight_g` (grams, e.g. 500) uses the MBRN birth weight (`VEKT`, new element
  `birth_weight` of `uio_source_files()$birth_registry$variables`) for fetal deaths (DODKAT 7-9).
  `stillbirth_weight_rule = "or"` (default): stillbirth if the gestational-age rule (`stillbirth_min_days`)
  says stillbirth or the weight is at least the limit; `"weight_first"`: the weight decides when recorded,
  the gestational-age rule otherwise. Weights of 0 or below count as missing. MBRN records without
  gestational age are still dropped, as in 1.3.
* The weight column is read only when `stillbirth_min_weight_g` is set. A cached
  `birth_registry_full.csv` without it is then read again from the source and cached with the weight.

# UiOPregnancyAlgorithm 1.3.1

First release as an installable R package. The algorithm is version 1.3
(`revised_codes/UiO_PA_v1.3/UiO_preg_Algo_v1.3.R`). Changes from 1.3:

## Packaging (no change in results)

* The script is now the function `run_uio_pa()`. The top-level preparation and caching of the NPR
  extract (`npr_mothers_full.csv`), formerly run when the script was sourced, is the internal
  function `uio_prepare_sources()`. `run_uio_pa()` sets the working directory to `workdir` and
  restores the previous one on exit.
* `uio_source_files()` returns the v1.3 `source_files` list (same TSD paths and variable mappings).
  Its arguments override the paths: `raw_dir`, `mbrn_file`, `npr_files`, `kuhr_file`. The seven
  raw NPR paths, previously hard-coded in the script, are in `source_files$hospital$raw_files`.
  If `source_files$hospital$data` is supplied, it is used as is.
* `uio_hospital_codes()` and `uio_primary_care_codes()` return the v1.3 marker lists.
* The three C++ helpers are still compiled at run time with `Rcpp::sourceCpp(code = ...)` (same
  C++ code). They are now created in the function's own environment (`env = environment()`)
  instead of the global environment.
* Unqualified calls resolve to the same functions as in the script run with
  `library(Rcpp); library(data.table); library(lubridate); library(stringr)`, for example
  `lubridate::year`. The package does this through NAMESPACE imports, so it no longer depends on
  the order of the packages on the user's search path.
* The result carries the settings used, as `attr(result, "settings")`.
* data.table compatibility (same assignment, no change in results): v1.3 lines 481-482,
  `MBRN[, var_1:=NA_Date_, with = FALSE]`, are now `MBRN[, (var_1):=NA_Date_]`. Combining
  `with = FALSE` with `:=` warns from data.table 1.15.0 and is an error in 1.17.x. The lines run
  only when the MBRN data have no quadruplets (or no triplets), so on such data v1.3 fails with
  data.table >= 1.17.

## New settings (`settings =` argument, see `uio_default_settings()`)

The defaults reproduce v1.3:

* `induced_abortion_default_days`, `miscarriage_default_days = 70`,
  `molar_default_days = 77`, `ectopic_default_days = 56`: the outcome-specific default pregnancy
  lengths.
* `o35_induced_days = 80`: pregnancy length of an induced abortion with an O35 prenatal-diagnosis
  code.
* `stillbirth_min_days = NULL`: `NULL` keeps the v1.3 split of MBRN fetal deaths (DODKAT 7-9):
  stillbirth if `floor(SVLEN_DG/7) > 20`. A number, e.g. `154`, makes it a stillbirth if
  `SVLEN_DG >= 154` and a miscarriage otherwise (as in `revised_codes_SB_22`).
* The `dating_method` label of the O35 length follows the setting. With the default it is
  unchanged: `"fixed 80 d (TOP with O35)"`.

## Diagnosis-code normalisation (`normalize_codes = TRUE` by default)

* After NPR and KUHR are loaded (from the raw files or the cached CSVs), the diagnosis codes
  (NPR `tilst`, KUHR `diagnose`) are normalised in memory to `toupper(gsub("[^A-Za-z0-9]", "", x))`.
  The cached CSV files keep the raw codes. Marker and miscarriage matching uses patterns without
  dots ("Z321", "W82", ...), and markers are compared by exact equality. Without normalisation,
  codes stored as e.g. "Z32.1", "w82" or "W 82" were missed.
* The `code` column of the marker lists is normalised the same way. The default lists are already
  in this form, so this changes nothing for them.
* The coding-system labels (`"ICD-10"`, `"ICPC-2"`) are not modified.
* `normalize_codes = FALSE` reproduces v1.3 exactly. On data whose codes are already upper case
  without separators, `TRUE` and `FALSE` give the same result.
