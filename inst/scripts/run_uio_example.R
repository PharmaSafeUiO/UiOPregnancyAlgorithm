## Run the UiO pregnancy algorithm on registry extracts
## ====================================================
## A template: copy it, fill in PART 1 and run it (source() or Rscript).
## The installed copy is system.file("scripts", "run_uio_example.R", package = "UiOPregnancyAlgorithm").
##
## With use_example_data <- TRUE it runs on the synthetic extracts of the package (?uio_example_data), so you can
## try it first. The output folder then shows what you will get for your own data.

library(UiOPregnancyAlgorithm)

## ---- PART 1: your settings -------------------------------------------------------------------------------------
use_example_data <- TRUE                      # FALSE: read your own extracts (describe them below)
workdir     <- "uio_work"                     # cached copies of the extracts (case level: keep with the raw data)
output_dir  <- "uio_output"                   # pregnancies (case level) and the report, tables (counts of 1-4 masked)
years_range <- c(2008, 2023)                  # first and last year of the records used

if (use_example_data) {
  src <- uio_example_data(file.path(workdir, "example_extracts"))
} else {
  ## Describe the three extracts. `columns` maps what the algorithm needs (left) to the column names of your file
  ## (right); ?uio_source lists every column. `file` is a path whose file-name part is a regular expression: all
  ## matching files are read and stacked (csv, txt, tab-separated dsv/tsv, Stata dta, SPSS sav). Instead of `file`,
  ## `data =` takes a data frame already in memory (for example an NPR assembled from several files, one row per
  ## diagnosis).
  ##
  ## Dates (?uio_dates), per extract:
  ##   uio_dates()                                   real dates, or ISO text such as 2015-06-15
  ##   uio_dates(format = "dmy")                     text in another order ("15.06.2015"; "mdy HMS" for "06/15/2015 00:00:00")
  ##   uio_dates(ref_date = "1997-09-24")            days since one reference date
  ##   uio_dates(ref_dates = "reference_dates.csv")  days since a reference date per woman (columns person_id, ref_date)
  dates <- uio_dates(format = "ymd")
  src <- uio_source_files(
    ## Medical Birth Registry of Norway, one row per child. Current deliveries (lower-case names, leve_dodstatus_fodsel,
    ## svlen in weeks): build `columns` with uio_mbrn_columns().
    birth_registry = uio_source("data/mbrn.csv", dates = dates,
      columns = c(mother_id = "LOPENR_MOR", event_date = "FDATO", gestational_days = "SVLEN_DG", death_cat = "DODKAT",
                  child_order = "PLUREK", apgar5 = "APGAR5", mother_age = "MORS_ALDER", birth_weight = "VEKT")),
    ## Norwegian Patient Registry, one row per diagnosis (ICD-10). `gender` keeps women only (value 2; female_value =)
    hospital = uio_source("data/npr/npr_.*[.]csv", dates = dates,
      columns = c(mother_id = "LOPENR", diagnose_code = "DIAGNOSE", visit_date = "INNDATO", gender = "KJONN")),
    ## Primary-care claims (KUHR), one row per diagnosis, ICD-10 or ICPC-2 with the coding system in `vocabulary`
    primary_care = uio_source("data/kuhr/.*[.]csv", dates = dates,
      columns = c(mother_id = "LOPENR", diagnose_code = "DIAGNOSE_KODE", vocabulary = "DIAGNOSEKODEVERK",
                  visit_date = "DATO", gender = "PASIENT_KJONN")))
}

## ---- PART 2: run -------------------------------------------------------------------------------------------------
## The defaults are the settings of the main analysis (?uio_default_settings): stillbirth from 22+0 weeks or 500 g,
## undated induced abortions dated by the median of their year in the Registry of Pregnancy Termination, MBRN single
## births without gestational age imputed. Sensitivity analyses: e.g. induced_abortion_default_days = 49.
dir.create(workdir, showWarnings = FALSE, recursive = TRUE)
res <- run_uio_pa(workdir = workdir, source_files = src, years_range = years_range, output_dir = output_dir)

## The pregnancies (one row per pregnancy) are also in output_dir/uio_pregnancies.rds; the report is
## output_dir/uio_report.html. A Stata copy, if needed:
## haven::write_dta(as.data.frame(res), file.path(output_dir, "uio_pregnancies.dta"))
print(table(res$outcome_lable_new, useNA = "ifany"))
