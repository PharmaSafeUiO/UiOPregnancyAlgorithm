#' Source data of the UiO pregnancy algorithm
#'
#' The algorithm reads three registry extracts: the Medical Birth Registry of Norway
#' (`birth_registry`, one row per child), the Norwegian Patient Registry (`hospital`, one row per
#' diagnosis) and primary-care claims (`primary_care`, KUHR, one row per diagnosis).
#'
#' * For the projects in which the algorithm has been run, `uio_project_sources()` returns the
#'   complete definition: `uio_project_sources("p1111")`, `"p704"` or `"p1380"`.
#' * For other projects, describe each extract with `uio_source()` and its dates with
#'   `uio_dates()`, and combine them with `uio_source_files()`.
#'
#' @section Columns:
#' `columns` maps the information the algorithm needs (names) to the column names of the extract
#' (values). Required:
#' \describe{
#'   \item{`birth_registry`}{`mother_id`, `mother_age`, `event_date` (birth date), `child_order`
#'     (`PLUREK`), `gestational_days` (`SVLEN_DG`), `death_cat` (vital status `DODKAT`), `apgar5`.
#'     Optional: `birth_year` (used with `birth_year_limit`) and `birth_weight` (grams; read only
#'     with `stillbirth_min_weight_g`).}
#'   \item{`hospital`}{`mother_id`, `diagnose_code` (ICD-10, e.g. `"O039"` or `"O03.9"`),
#'     `visit_date`. Optional: `vocabulary` (coding system; all `"ICD-10"` if not given),
#'     `stay_id`, `gender`, `birth_year`.}
#'   \item{`primary_care`}{`mother_id`, `diagnose_code`, `vocabulary` (`"ICD-10"` or
#'     `"ICPC-2"`), `visit_date`. Optional: `gender` (all women if not given), `birth_year`.}
#' }
#' The woman's ID must be the same in the three extracts. With `gender`, only women are kept
#' (`female_value`, default 2 as in the Norwegian registries).
#'
#' @section Files:
#' `file` is a path whose file-name part is a regular expression: all matching files in the folder
#' are read and stacked. Supported: `.csv`, `.txt`, tab-separated `.dsv`/`.tsv`, Stata `.dta` and
#' SPSS `.sav`. Instead of a file, `data` can be a data frame already in memory. The extracts are read
#' once and cached in the work folder of [run_uio_pa()]; delete the cached `*_full.csv` files to
#' read them again.
#'
#' @param file Path of the extract (the file-name part is a regular expression).
#' @param data A data frame, instead of `file`.
#' @param columns Named character vector: information = column name, see the section Columns.
#' @param dates How the dates are stored, see [uio_dates()].
#' @param birth_year_limit `NULL` or `c(first, last)`: keep women born in these years (needs the
#'   `birth_year` column).
#' @param female_value Value of the `gender` column for women. Default `2`.
#' @return `uio_source()`: the definition of one extract. `uio_source_files()` and
#'   `uio_project_sources()`: a list with `birth_registry`, `hospital` and `primary_care`, for the
#'   argument `source_files` of [run_uio_pa()].
#' @examples
#' # a project with real dates in the MBRN and day offsets from a per-person reference date in NPR and KUHR
#' ref <- uio_dates(ref_dates = "N:/durable/data/reference_dates.csv")
#' src <- uio_source_files(
#'   birth_registry = uio_source("N:/durable/data/mbrn.csv",
#'     columns = c(mother_id = "MOR_ID", mother_age = "MORS_ALDER", event_date = "FDATO",
#'                 child_order = "PLUREK", gestational_days = "SVLEN_DG", death_cat = "DODKAT",
#'                 apgar5 = "APGAR5"),
#'     dates = uio_dates(format = "ymd")),
#'   hospital = uio_source("N:/durable/data/npr_diagnoses.csv",
#'     columns = c(mother_id = "ID", diagnose_code = "KODE", visit_date = "DAGER"), dates = ref),
#'   primary_care = uio_source("N:/durable/data/kuhr.csv",
#'     columns = c(mother_id = "ID", diagnose_code = "DIAGNOSE", vocabulary = "KODEVERK",
#'                 visit_date = "DAGER"), dates = ref))
#' @export
uio_source <- function(file = NULL, data = NULL, columns, dates = uio_dates(), birth_year_limit = NULL,
                       female_value = 2) {
  if (is.null(file) == is.null(data)) stop("Give either `file` or `data`.", call. = FALSE)
  if (!is.character(columns) || is.null(names(columns)) || any(names(columns) == ""))
    stop("`columns` must be a named character vector (information = column name).", call. = FALSE)
  if (!inherits(dates, "uio_dates")) stop("`dates` must be made with uio_dates().", call. = FALSE)
  if (!is.null(birth_year_limit) && (length(birth_year_limit) != 2 || !"birth_year" %in% names(columns)))
    stop("`birth_year_limit` must be c(first, last) and needs a `birth_year` column.", call. = FALSE)
  list(file = file, data = data, ref_date = dates$ref_date, date_format = dates$format,
       ref_dates = dates$ref_dates, birth_year_limit = birth_year_limit, variables = columns,
       female_value = female_value)
}

#' @rdname uio_source
#' @param birth_registry,hospital,primary_care Extracts defined with `uio_source()`.
#' @export
uio_source_files <- function(birth_registry, hospital, primary_care) {
  need <- list(birth_registry = c("mother_id", "mother_age", "event_date", "child_order", "gestational_days", "death_cat", "apgar5"),
               hospital = c("mother_id", "diagnose_code", "visit_date"),
               primary_care = c("mother_id", "diagnose_code", "vocabulary", "visit_date"))
  src <- list(birth_registry = birth_registry, hospital = hospital, primary_care = primary_care)
  for (nm in names(need)) {
    miss <- setdiff(need[[nm]], names(src[[nm]]$variables))
    if (length(miss)) stop("`", nm, "` lacks the column(s): ", paste(miss, collapse = ", "), call. = FALSE)
  }
  src
}

#' How dates are stored in an extract
#'
#' The registries deliver dates in three ways, and each extract can use its own:
#' \describe{
#'   \item{Real dates}{`uio_dates()` for dates read as dates or as ISO text (`"2015-06-15"`);
#'     `uio_dates(format = "dmy")` (or `"ymd"`, `"mdy HMS"`, ...) for other text formats, given as
#'     [lubridate::parse_date_time()] orders.}
#'   \item{Day offsets from one reference date}{`uio_dates(ref_date = "1997-09-24")`: the column
#'     holds a number of days, and the date is `ref_date + days`.}
#'   \item{Day offsets from a reference date per person}{`uio_dates(ref_dates = "file.csv")`: a
#'     file (or data frame) with one row per woman, with her ID (`person_id`) and her reference date
#'     (`ref_date`); the date of each record is the woman's reference date plus the number of days.
#'     The IDs are those of the `mother_id` column of the extract. The reference dates are real
#'     dates, in ISO form or in `ref_format`.}
#' }
#'
#' @param format `NULL` or the date format of text dates, as [lubridate::parse_date_time()] orders.
#' @param ref_date One reference date (`Date` or `"YYYY-MM-DD"`).
#' @param ref_dates A file (csv, txt, dsv/tsv, dta or sav) or a data frame with the woman's ID and
#'   her reference date.
#' @param person_id,ref_column Names of the two columns of `ref_dates`. Defaults `"person_id"` and
#'   `"ref_date"`.
#' @param ref_format `NULL` or the format of the reference dates in `ref_dates`.
#' @return An object of class `uio_dates`, for [uio_source()].
#' @examples
#' uio_dates()                          # real dates
#' uio_dates(format = "dmy")            # text such as "15.06.2015"
#' uio_dates(ref_date = "1997-09-24")   # days since 24 September 1997
#' @export
uio_dates <- function(format = NULL, ref_date = NULL, ref_dates = NULL, person_id = "person_id",
                      ref_column = "ref_date", ref_format = NULL) {
  if (sum(!is.null(format), !is.null(ref_date), !is.null(ref_dates)) > 1)
    stop("Give at most one of `format`, `ref_date` and `ref_dates`.", call. = FALSE)
  if (!is.null(ref_date)) {
    ref_date <- as.Date(ref_date)
    if (length(ref_date) != 1L || is.na(ref_date)) stop("`ref_date` must be one date.", call. = FALSE)
  }
  if (!is.null(ref_dates))
    ref_dates <- list(source = ref_dates, person_id = person_id, ref_column = ref_column, format = ref_format)
  structure(list(format = format, ref_date = ref_date, ref_dates = ref_dates), class = "uio_dates")
}

#' @rdname uio_source
#' @param project `"p1111"`, `"p704"` or `"p1380"`.
#' @param raw_dir Folder of the raw extracts on TSD. Defaults: P1111 `"N:/durable/Raw_data"`,
#'   P704 `"N:/durable/Updated_files_2020/Raw files/Data"`, P1380
#'   `"N:/durable/Raw_data/Data_delivery_2024"`.
#' @details `uio_project_sources()` knows the extracts of three projects:
#' \describe{
#'   \item{P1111}{MBRN and KUHR as csv with day offsets from 25 December 1975; NPR as seven extracts
#'     (SOM, AVTSOM, ASP, BUP, PHV, REH, TSB) with day offsets, from which the ICD-10 diagnoses of
#'     women are taken (the linkage key by column position: 22 in SOM, 16 in AVTSOM, ASP and BUP, 18
#'     in PHV, REH and TSB).}
#'   \item{P704}{MBRN, NPR and KUHR as csv with day offsets from 24 September 1997; NPR as the
#'     mothers' extracts (`NPR_mor`) with one column per diagnosis (`tilstand_1`, `tilstand_2`, ...);
#'     KUHR without sex (the extract holds mothers only).}
#'   \item{P1380}{MBRN as SPSS file with dates as text (`"mdy HMS"`); NPR as contact files with
#'     separate code files linked by `kobl_nokkel` (SOM and AVTSOM), ICD-10 diagnoses of women, with
#'     dates; KUHR as tab-separated `.dsv` with dates (`"ymd"`).}
#' }
#' @export
uio_project_sources <- function(project = c("p1111", "p704", "p1380"), raw_dir = NULL) {
  project <- tolower(match.arg(tolower(project), c("p1111", "p704", "p1380")))
  f <- function(...) file.path(raw_dir, ...)
  if (project == "p1111") {
    if (is.null(raw_dir)) raw_dir <- "N:/durable/Raw_data"
    d <- uio_dates(ref_date = lubridate::ymd("2004-01-01") - 10234)        # 1975-12-25
    by <- c(1, 10000)
    npr <- c(som    = f("NPR/PDB2774_NPR_19_45084_SOMdata_til_utlevering_redNorPD_202209.csv"),
             avtsom = f("NPR/PDB2774_NPR_19_45084_AVTSOMdata_til_utlevering_redNorPD_202209.csv"),
             asp    = f("NPR/PDB2774_NPR_19_45084_ASPdata_til_utlevering_redNorPD_202209.csv"),
             bup    = f("NPR/PDB2774_NPR_19_45084_BUPdata_til_utlevering_redNorPD_202209.csv"),
             phv    = f("NPR/PDB2774_NPR_19_45084_PHVdata_til_utlevering_redNorPD_202209.csv"),
             reh    = f("NPR/PDB2774_NPR_19_45084_REHdata_til_utlevering_redNorPD_202209.csv"),
             tsb    = f("NPR/PDB2774_NPR_19_45084_TSBdata_til_utlevering_redNorPD_202209.csv"))
    src <- list(
      birth_registry = list(file = f("MFR/PDB2774_MFR_p202207_mfr_2022Q1_202209.csv"), ref_date = d$ref_date, birth_year_limit = by,
                            variables = c(mother_id = "MOR_PasientLopeNr_PDB2774", mother_age = "MORS_ALDER", event_date = "FDATO_DIFF",
                                          child_order = "PLUREK", gestational_days = "SVLEN_DG", death_cat = "DODKAT", apgar5 = "APGAR5",
                                          birth_year = "MOR_FAAR", birth_weight = "VEKT")),
      hospital = list(data = NULL, prepare = "p1111", raw_files = npr, ref_date = d$ref_date, birth_year_limit = by,
                      variables = c(mother_id = "lopeNr_1_PasientLopeNr_PDB2774", diagnose_code = "KodeVerdi", stay_id = "visit_id",
                                    visit_date = "diffdato", birth_year = "fodselsar")),
      primary_care = list(file = f("KUHR/.*.csv"), ref_date = d$ref_date, birth_year_limit = by,
                          variables = c(mother_id = "PasientLopeNr_PDB2774", diagnose_code = "DIAGNOSE_KODE", vocabulary = "DIAGNOSEKODEVERK",
                                        visit_date = "DIFFERANSEDAGER", gender = "PASIENT_KJONN", birth_year = "FXDT")))
  } else if (project == "p704") {
    if (is.null(raw_dir)) raw_dir <- "N:/durable/Updated_files_2020/Raw files/Data"
    d <- uio_dates(ref_date = "1997-09-24")
    src <- list(
      birth_registry = list(file = f("PDB 2424_MFRdata_182029_2020.09.csv"), ref_date = d$ref_date,
                            variables = c(mother_id = "PasientLopeNr_MOR", mother_age = "MORS_ALDER", event_date = "DIFF_DAGER_FDATO",
                                          child_order = "PLUREK", gestational_days = "SVLEN_DG", death_cat = "DODKAT", apgar5 = "APGAR5",
                                          birth_year = "FAAR")),
      hospital = list(data = NULL, prepare = "p704", raw_files = c(npr_mor = f("PDB 2424_NPR_20_505_NPR_mor.*")), ref_date = d$ref_date,
                      variables = c(mother_id = "pasientlopenr_MOR", diagnose_code = "tilstand", stay_id = "visit_id",
                                    visit_date = "inndato_diff")),
      primary_care = list(file = f("PDB 2424_.* Data fra KUHR.*.csv"), ref_date = d$ref_date,
                          variables = c(mother_id = "pasientlopenr_MOR", diagnose_code = "diagnose", vocabulary = "diagnosekodeverk",
                                        visit_date = "ant_dager")))
  } else {
    if (is.null(raw_dir)) raw_dir <- "N:/durable/Raw_data/Data_delivery_2024"
    src <- list(
      birth_registry = list(file = f("MBRN/p202274_mfr_2024_11.sav"), date_format = "mdy HMS",
                            variables = c(mother_id = "LOPENR_MOR", mother_age = "MORS_ALDER", event_date = "FDATO", child_order = "PLUREK",
                                          gestational_days = "SVLEN_DG", death_cat = "DODKAT", apgar5 = "APGAR5")),
      hospital = list(data = NULL, prepare = "p1380",
                      raw_files = c(som = f("NPR/SOM/SOM_2018_2023.csv"), som_codes = f("NPR/SOM/SOM_ICD10.csv"),
                                    avtsom = f("NPR/AVTSOM/AVTSOM_2018_2023.csv"), avtsom_codes = f("NPR/AVTSOM/AVTSOM_koder.csv")),
                      variables = c(mother_id = "lopenr", diagnose_code = "kodeverdi", stay_id = "kobl_nokkel", visit_date = "inndato")),
      primary_care = list(file = f("KUHR/.*.dsv"), date_format = "ymd",
                          variables = c(mother_id = "PASIENTLOPENUMMER", diagnose_code = "DIAGNOSE_KODE", vocabulary = "DIAGNOSEKODEVERK",
                                        visit_date = "DATO", gender = "PASIENT_KJONN")))
  }
  src
}

#' Pregnancy markers of the UiO pregnancy algorithm
#'
#' Early-pregnancy marker codes used to date pregnancies without MBRN gestational age, with the
#' number of days from the marker to the pregnancy start (`days`): the start is the marker date
#' minus `days`. `uio_hospital_codes()` gives the 15 ICD-10 markers used in the NPR,
#' `uio_primary_care_codes()` the 21 markers used in KUHR (the same 15 ICD-10 codes and 6 ICPC-2
#' codes). Row order is the priority order: the first marker in the list that is found and accepted
#' dates the pregnancy.
#'
#' Nordeng et al. (2024, Supplementary Table 1) considered 45 ICD-10 and 9 ICPC-2 codes and kept
#' those with a median gestational age at the first record below 84 days in pregnancies of the
#' MBRN; `days` is that median, separately for the NPR and KUHR. Codes are matched without the dot
#' (`"Z321"` for Z32.1); `W781` is a Norwegian ICPC-2 extension code.
#'
#' Other lists can be passed to [run_uio_pa()] (same columns; `vocab` `"ICD-10"` or `"ICPC-2"`).
#'
#' @references Nordeng HME, Lupattelli A, Engjom HM, van Gelder MMHJ. Pharmacoepidemiol Drug Saf.
#'   2024;33(9):e70002. \doi{10.1002/pds.70002}
#'
#' @return A tibble with columns `vocab`, `code` and `days`.
#' @examples
#' uio_hospital_codes()
#' uio_primary_care_codes()
#' @export
uio_hospital_codes <- function() {
  # v1.3 lines 107-126
  # Select appropriate codes from NPR for dating the early non-live births
  # Order matters. Higher codes are prioritized over the lower codes for imputing event dates
  hospital_codes = dplyr::tribble(
    ~vocab, ~code, ~days,
    "ICD-10","Z313",15,
    "ICD-10","Z311",12,
    "ICD-10","Z312",15,
    "ICD-10","Z351",65,
    "ICD-10","Z321",64,
    "ICD-10","O262",57,
    "ICD-10","Z320",48,
    "ICD-10","Z350",60,
    "ICD-10","O211",66,
    "ICD-10","O200",64,
    "ICD-10","O208",73,
    "ICD-10","O210",72,
    "ICD-10","O219",74,
    "ICD-10","O209",81,
    "ICD-10","O263",70
  )
  hospital_codes
}

#' @rdname uio_hospital_codes
#' @export
uio_primary_care_codes <- function() {
  # v1.3 lines 129-153
  # Select appropriate codes from KUHR
  primary_care_codes = dplyr::tribble(
    ~vocab, ~code, ~days,
    "ICD-10","Z313",15,
    "ICD-10","Z311",15,
    "ICD-10","Z312",15,
    "ICD-10","Z351",63,
    "ICD-10","O262",56,
    "ICPC-2","W79",53,
    "ICD-10","Z321",63,
    "ICD-10","Z350",57,
    "ICD-10","Z320",45,
    "ICPC-2","W78",57,
    "ICPC-2","W781",83,
    "ICPC-2","W01",57,
    "ICPC-2","W05",60,
    "ICD-10","O200",62,
    "ICD-10","O211",73,
    "ICD-10","O208",72,
    "ICD-10","O219",74,
    "ICD-10","O210",76,
    "ICD-10","O209",77,
    "ICPC-2","W03",71,
    "ICD-10","O263",62
  )
  primary_care_codes
}

#' Prepare the NPR source table of a project (internal)
#'
#' For the projects of [uio_project_sources()], the NPR is assembled from several raw extracts into
#' one table with one row per diagnosis and cached as `npr_mothers_full.csv` in the work folder
#' (the working directory). If that file is cached, it is read only when `hospital_full.csv` is not.
#' Extracts defined with [uio_source()] need no preparation.
#'
#' @param source_files See [uio_source_files()].
#' @return `source_files` with `hospital$data` set (possibly `NULL` when `hospital_full.csv` is cached).
#' @keywords internal
#' @noRd
uio_prepare_sources <- function(source_files) {
  h <- source_files$hospital
  if (!is.null(h$data) || is.null(h$prepare)) return(source_files)
  if (file.exists("npr_mothers_full.csv")) {
    # Cached: read it only if hospital_full.csv still has to be built from it
    npr0 <- if (!file.exists("hospital_full.csv")) fread("npr_mothers_full.csv") else NULL
  } else {
    npr0 <- switch(h$prepare, p1111 = .uio_npr_p1111(h$raw_files), p704 = .uio_npr_p704(h$raw_files),
                   p1380 = .uio_npr_p1380(h$raw_files),
                   stop("Unknown NPR preparation: ", h$prepare, call. = FALSE))
    fwrite(npr0, "npr_mothers_full.csv")
  }
  source_files$hospital["data"] <- list(npr0)
  source_files
}

# P1111 (v1.3 lines 24-75): seven extracts; linkage key by position; ICD-10 diagnoses of women
.uio_npr_p1111 <- function(npr_files) {
  needed <- c("som", "avtsom", "asp", "bup", "phv", "reh", "tsb")
  if (!all(needed %in% names(npr_files)))
    stop("The P1111 NPR needs the extracts: ", paste(needed, collapse = ", "), call. = FALSE)
  som = fread(npr_files[["som"]], encoding = "UTF-8")
  avtsom = fread(npr_files[["avtsom"]])
  asp = fread(npr_files[["asp"]], encoding = "UTF-8")
  bup = fread(npr_files[["bup"]], encoding = "UTF-8")
  phv = fread(npr_files[["phv"]], encoding = "UTF-8")
  reh = fread(npr_files[["reh"]], encoding = "UTF-8")
  tsb = fread(npr_files[["tsb"]], encoding = "UTF-8")

  colnames(som)[22]="kobl_nokkel"
  colnames(avtsom)[16]="kobl_nokkel"
  colnames(asp)[16]="kobl_nokkel"
  colnames(bup)[16]="kobl_nokkel"
  colnames(phv)[18]="kobl_nokkel"
  colnames(reh)[18]="kobl_nokkel"
  colnames(tsb)[18]="kobl_nokkel"

  som=som[KodeNavn=="ICD-10" & kjonn==2 ,.(kobl_nokkel,lopeNr_1_PasientLopeNr_PDB2774,KodeVerdi,diffdato,fodselsar)]
  avtsom=avtsom[KodeNavn=="ICD-10" & kjonn==2 ,.(kobl_nokkel,lopeNr_1_PasientLopeNr_PDB2774,KodeVerdi,diffdato,fodselsar)]
  asp=asp[KodeNavn=="ICD-10" & kjonn==2 ,.(kobl_nokkel,lopeNr_1_PasientLopeNr_PDB2774,KodeVerdi,diffdato,fodselsar)]
  bup[,KodeNavn:="ICD-10"]
  bup=bup[KodeNavn=="ICD-10" & kjonn==2 ,.(kobl_nokkel,lopeNr_1_PasientLopeNr_PDB2774,KodeVerdi,diffdato,fodselsar)]
  phv=phv[KodeNavn=="ICD-10" & kjonn==2 ,.(kobl_nokkel,lopeNr_1_PasientLopeNr_PDB2774,KodeVerdi,diffdato,fodselsar)]
  reh=reh[KodeNavn=="ICD-10" & kjonn==2 ,.(kobl_nokkel,lopeNr_1_PasientLopeNr_PDB2774,KodeVerdi,diffdato,fodselsar)]
  tsb=tsb[KodeNavn=="ICD-10" & kjonn==2 ,.(kobl_nokkel,lopeNr_1_PasientLopeNr_PDB2774,KodeVerdi,diffdato,fodselsar)]

  som[,visit_type:="som"]
  avtsom[,visit_type:="avtsom"]
  asp[,visit_type:="asp"]
  bup[,visit_type:="bup"]
  phv[,visit_type:="phv"]
  reh[,visit_type:="reh"]
  tsb[,visit_type:="tsb"]

  npr0=rbindlist(list(som,avtsom,asp,bup,phv,reh,tsb))
  setorder(npr0, lopeNr_1_PasientLopeNr_PDB2774, kobl_nokkel,visit_type, diffdato,na.last = TRUE)
  setkey(npr0, lopeNr_1_PasientLopeNr_PDB2774, kobl_nokkel,visit_type)
  npr0[,visit_id:=.GRP,by=.(lopeNr_1_PasientLopeNr_PDB2774,kobl_nokkel,visit_type)]
  npr0
}

# P704: the mothers' NPR extracts, one column per diagnosis (tilstand_1, tilstand_2, ...)
.uio_npr_p704 <- function(npr_files) {
  pat <- npr_files[["npr_mor"]]
  files <- fs::dir_ls(path = dirname(pat), regexp = basename(pat))
  if (!length(files)) stop("No NPR files match ", pat, call. = FALSE)
  npr <- rbindlist(lapply(files, fread, encoding = "UTF-8"), fill = TRUE)
  npr0 <- data.table::melt(npr, measure.vars = data.table::patterns("tilstand_"), value.name = "tilstand",
                           id.vars = c("pasientlopenr_MOR", "inndato_diff"))
  setorder(npr0, pasientlopenr_MOR, inndato_diff, na.last = TRUE)
  setkey(npr0, pasientlopenr_MOR)
  npr0[, visit_id := seq_len(.N), by = .(pasientlopenr_MOR)]
  npr0
}

# P1380: contact files (woman, date, sex, kobl_nokkel) with separate code files (kobl_nokkel, code system, code)
.uio_npr_p1380 <- function(npr_files) {
  part <- function(contacts, codes, sex_col, type) {
    k <- fread(npr_files[[contacts]], encoding = "UTF-8")
    colnames(k)[sex_col] <- "kjonn"
    k <- k[kjonn == 2, .(kobl_nokkel, lopenr, inndato)]          # women
    k[, visit_type := type]
    cd <- fread(npr_files[[codes]])
    names(cd) <- tolower(names(cd))
    cd <- cd[kodenavn == "ICD-10", .(kobl_nokkel, kodeverdi)]
    merge(k, cd, by = "kobl_nokkel", all.x = TRUE)
  }
  npr0 <- rbindlist(list(part("som", "som_codes", 7, "som"), part("avtsom", "avtsom_codes", 6, "avtsom")))
  setorder(npr0, lopenr, kobl_nokkel, visit_type, inndato, na.last = TRUE)
  setkey(npr0, lopenr, kobl_nokkel, visit_type)
  npr0[, visit_id := .GRP, by = .(lopenr, kobl_nokkel, visit_type)]
  npr0
}

# Dates of one extract (internal): real dates, ref_date + days, or the woman's reference date + days.
.uio_as_date <- function(x, id, src) {
  if (!is.null(src$ref_dates)) {
    rd <- .uio_read_ref_dates(src$ref_dates)
    ref <- rd$ref_date[match(as.character(id), rd$person_id)]
    if (any(is.na(ref) & !is.na(x)))
      warning(sum(is.na(ref) & !is.na(x)), " record(s) of women without a reference date get no date.", call. = FALSE)
    return(ref + as.numeric(x))
  }
  if (!is.null(src$ref_date)) return(src$ref_date + x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.null(src$date_format)) return(lubridate::as_date(x))
  as.Date(lubridate::parse_date_time(as.character(x), orders = src$date_format, quiet = TRUE))
}

.uio_read_ref_dates <- function(spec) {
  d <- spec$source
  if (is.character(d)) {
    ext <- tolower(tools::file_ext(d))
    d <- switch(ext, dta = haven::read_dta(d), sav = haven::read_sav(d),
                dsv = , tsv = fread(d, sep = "\t"), fread(d))
  }
  d <- as.data.table(d)
  if (!all(c(spec$person_id, spec$ref_column) %in% names(d)))
    stop("The reference-date table needs the columns ", spec$person_id, " and ", spec$ref_column, ".", call. = FALSE)
  r <- d[[spec$ref_column]]
  r <- if (inherits(r, "Date")) as.Date(r) else if (inherits(r, "POSIXt")) as.Date(r) else
    if (is.null(spec$format)) lubridate::as_date(r) else as.Date(lubridate::parse_date_time(as.character(r), orders = spec$format, quiet = TRUE))
  out <- data.table(person_id = as.character(d[[spec$person_id]]), ref_date = r)
  if (anyDuplicated(out$person_id)) stop("The reference-date table has more than one row for some women.", call. = FALSE)
  out
}
