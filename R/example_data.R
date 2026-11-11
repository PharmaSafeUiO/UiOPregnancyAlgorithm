#' Synthetic example data
#'
#' Writes three small synthetic registry extracts to `dir` and returns their definition for
#' [run_uio_pa()]. Use it to try the package, or as a template for the description of your own
#' extracts with [uio_source_files()]. The data are invented; each woman tests one rule:
#'
#' \describe{
#'   \item{`mbrn.csv`}{Medical Birth Registry of Norway, one row per child: `mother_id`, `child_id`,
#'     `mother_age`, `birth_date`, `child_order`, `gestational_days`, `dodkat`, `apgar5`,
#'     `birth_weight` (g).}
#'   \item{`npr.csv`}{Norwegian Patient Registry, one row per diagnosis: `mother_id`, `sex`
#'     (2 = woman), `visit_date`, `code_system`, `diagnosis`.}
#'   \item{`kuhr.csv`}{primary-care claims (KUHR), one row per diagnosis: `mother_id`, `sex`,
#'     `visit_date`, `code_system` (`"ICD-10"` or `"ICPC-2"`), `diagnosis`.}
#' }
#' Dates are ISO dates (`2015-06-15`), read with `uio_dates(format = "ymd")`.
#'
#' Women: 1 live birth with an earlier birth in 2005 and markers; 2 fetal death at 150 days and
#' 560 g (a stillbirth by weight); 3 NPR miscarriage with a marker 20 days before; 4 NPR induced
#' abortion without marker; 5 and 6 KUHR miscarriages coded `"w82"` and `"W 82"`; 7 induced
#' abortion with O35 the same day; 8 KUHR miscarriage with a KUHR marker; 9 dotted codes
#' (`"O03.9"`, `"Z32.1"`); 10 twins; 11 live birth with KUHR codes during the pregnancy; 12 a man
#' in KUHR and 99 a man in the NPR (excluded); 13 NPR and KUHR records of one miscarriage; 14 MBRN
#' miscarriage also in the NPR; 15 ectopic and 16 molar pregnancy; 17 quadruplets.
#'
#' @param dir Folder to write to (created if needed).
#' @return The `source_files` list for [run_uio_pa()].
#' @examples
#' src <- uio_example_data(file.path(tempdir(), "uio_example"))
#' src$birth_registry$variables
#' \dontrun{
#' wd <- file.path(tempdir(), "uio_work"); dir.create(wd)
#' res <- run_uio_pa(workdir = wd, source_files = src, output_dir = NULL)
#' }
#' @export
uio_example_data <- function(dir = file.path(tempdir(), "uio_example")) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir <- normalizePath(dir, winslash = "/")
  mbrn <- data.table::data.table(
    mother_id        = c(1L, 1L, 2L, 10L, 10L, 11L, 14L, 17L, 17L, 17L, 17L),
    child_id         = 101:111,
    mother_age       = c(20L, 30L, 28L, 33L, 33L, 27L, 31L, 35L, 35L, 35L, 35L),
    birth_date       = as.Date(c("2005-03-01", "2015-06-15", "2016-03-10", "2014-11-11", "2014-11-11", "2012-01-10",
                                 "2011-07-07", "2019-12-12", "2019-12-12", "2019-12-12", "2019-12-12")),
    child_order      = c(1L, 1L, 1L, 1L, 2L, 1L, 1L, 1L, 2L, 3L, 4L),
    gestational_days = c(270L, 280L, 150L, 259L, 259L, 275L, 120L, 230L, 230L, 230L, 230L),
    dodkat           = c(0L, 0L, 7L, 0L, 0L, 0L, 8L, 0L, 0L, 0L, 0L),
    apgar5           = c(9L, 9L, 0L, 8L, 7L, 10L, 0L, 7L, 8L, 7L, 6L),
    birth_weight     = c(3300L, 3500L, 560L, 2700L, 2600L, 3400L, 300L, 1500L, 1450L, 1400L, 1350L))
  npr <- data.table::rbindlist(list(
    list(1L,  2L, "2014-11-10", "ICD-10", "Z321"),
    list(1L,  2L, "2014-11-10", "NCSP",   "MBA00"),     # a procedure: not read (code system)
    list(3L,  2L, "2017-03-31", "ICD-10", "Z321"),
    list(3L,  2L, "2017-04-20", "ICD-10", "O039"),
    list(4L,  2L, "2018-02-14", "ICD-10", "O049"),
    list(7L,  2L, "2020-01-15", "ICD-10", "O049"),
    list(7L,  2L, "2020-01-15", "ICD-10", "O359"),
    list(9L,  2L, "2018-08-30", "ICD-10", "Z32.1"),
    list(9L,  2L, "2018-09-09", "ICD-10", "O03.9"),
    list(13L, 2L, "2013-05-05", "ICD-10", "O021"),
    list(14L, 2L, "2011-07-07", "ICD-10", "O039"),
    list(99L, 1L, "2015-01-01", "ICD-10", "O039"),      # a man: excluded
    list(15L, 2L, "2016-10-10", "ICD-10", "O009"),
    list(16L, 2L, "2022-02-02", "ICD-10", "O010"),
    list(1L,  2L, "2016-01-01", "ICD-10", "F329"),
    list(1L,  2L, "2016-02-01", "ICD-10", "F419"),
    list(11L, 2L, "2013-03-03", "ICD-10", "Z509"),
    list(11L, 2L, "2013-04-04", "ICD-10", "F102")))
  data.table::setnames(npr, c("mother_id", "sex", "visit_date", "code_system", "diagnosis"))
  kuhr <- data.table::rbindlist(list(
    list(11L, 2L, "2011-05-15", "ICD-10", "Z321"),
    list(11L, 2L, "2011-06-01", "ICPC-2", "W78"),
    list(11L, 2L, "2011-09-01", "ICD-10", "O359"),
    list(13L, 2L, "2013-05-08", "ICPC-2", "W82"),
    list(1L,  2L, "2015-01-20", "ICPC-2", "W78"),
    list(5L,  2L, "2019-05-05", "ICPC-2", "w82"),
    list(6L,  2L, "2019-08-08", "ICPC-2", "W 82"),
    list(8L,  2L, "2021-02-16", "ICPC-2", "W78"),
    list(8L,  2L, "2021-03-03", "ICPC-2", "W82"),
    list(12L, 1L, "2020-06-06", "ICPC-2", "W82")))      # a man: excluded
  data.table::setnames(kuhr, c("mother_id", "sex", "visit_date", "code_system", "diagnosis"))
  data.table::fwrite(mbrn, file.path(dir, "mbrn.csv"))
  data.table::fwrite(npr, file.path(dir, "npr.csv"))
  data.table::fwrite(kuhr, file.path(dir, "kuhr.csv"))

  ymd <- uio_dates(format = "ymd")
  uio_source_files(
    birth_registry = uio_source(file.path(dir, "mbrn.csv"), dates = ymd,
      columns = c(mother_id = "mother_id", mother_age = "mother_age", event_date = "birth_date",
                  child_order = "child_order", gestational_days = "gestational_days", death_cat = "dodkat",
                  apgar5 = "apgar5", birth_weight = "birth_weight")),
    hospital = uio_source(file.path(dir, "npr.csv"), dates = ymd,
      columns = c(mother_id = "mother_id", diagnose_code = "diagnosis", vocabulary = "code_system",
                  visit_date = "visit_date", gender = "sex")),
    primary_care = uio_source(file.path(dir, "kuhr.csv"), dates = ymd,
      columns = c(mother_id = "mother_id", diagnose_code = "diagnosis", vocabulary = "code_system",
                  visit_date = "visit_date", gender = "sex")))
}
