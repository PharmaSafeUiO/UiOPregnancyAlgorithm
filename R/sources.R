#' Source files of the UiO pregnancy algorithm (MBRN, NPR, KUHR)
#'
#' Returns the list that tells [run_uio_pa()] where the registry extracts are and which columns
#' hold which information. The defaults describe the extracts of the project used to develop the
#' package (file names, column names, dates as day offsets). For other extracts, change the paths
#' with the arguments and, if needed, the elements of the returned list.
#'
#' Each of the three elements (`birth_registry`, `hospital`, `primary_care`) has:
#' \describe{
#'   \item{`file`}{The extract (`csv`, `txt`, tab-separated `dsv`, or Stata `dta`). The file name
#'     part is a regular expression; all matching files in the folder are read and row-bound.
#'     For `hospital`, `raw_files` holds the seven NPR extracts instead (see below).}
#'   \item{`data`}{Optional: the extract as a data frame, used instead of `file`.}
#'   \item{`ref_date`}{Dates are stored as day offsets; the date is `ref_date + offset`. Set it to
#'     `NULL` for real dates (parsed with [lubridate::as_date()]).}
#'   \item{`birth_year_limit`}{`c(first, last)` birth years of the women to keep (variable
#'     `birth_year`).}
#'   \item{`variables`}{Named character vector: role = column name in the extract.}
#' }
#' The roles are, for the MBRN (one row per child): `mother_id`, `mother_age`, `event_date` (birth
#' date), `child_order` (`PLUREK`), `gestational_days` (`SVLEN_DG`, gestational age in days),
#' `death_cat` (vital status `DODKAT`), `apgar5`, `birth_year` (mother's) and `birth_weight` (grams;
#' read only with `stillbirth_min_weight_g`); for the NPR: `mother_id`, `diagnose_code`, `stay_id`,
#' `visit_date`, `birth_year`; for KUHR: `mother_id`, `diagnose_code`, `vocabulary` (`"ICD-10"` or
#' `"ICPC-2"`), `visit_date`, `gender` (2 = female) and `birth_year`.
#'
#' The NPR is prepared from seven raw extracts (`raw_files`): ICD-10 diagnoses of women are kept and
#' cached as `npr_mothers_full.csv` in the work folder. The linkage-key column is taken by
#' position (column 22 in SOM, 16 in AVTSOM, ASP and BUP, 18 in PHV, REH and TSB).
#'
#' @param raw_dir Root folder of the raw registry extracts. Default `"N:/durable/Raw_data"` (TSD).
#' @param mbrn_file Medical Birth Registry (MBRN/MFR) extract (csv).
#' @param npr_files Named character vector with the seven NPR extracts (`som`, `avtsom`, `asp`,
#'   `bup`, `phv`, `reh`, `tsb`). They are read only when `npr_mothers_full.csv` is not cached in
#'   the working directory. The linkage-key column is taken by position, as in v1.3 (column 22 in
#'   SOM, 16 in AVTSOM/ASP/BUP, 18 in PHV/REH/TSB).
#' @param kuhr_file KUHR extract(s). The directory part is a folder, and the file name part is a
#'   regular expression matched against the files in it (all matches are row-bound).
#'   The extracts are read only when the cached files are not in the work folder.
#' @return A named list with elements `birth_registry`, `hospital` and `primary_care`.
#' @examples
#' src <- uio_source_files()
#' src$birth_registry$variables
#' # all raw extracts under another root folder
#' src2 <- uio_source_files(raw_dir = "D:/data/Raw_data")
#' @export
uio_source_files <- function(raw_dir = "N:/durable/Raw_data",
                             mbrn_file = file.path(raw_dir, "MFR/PDB2774_MFR_p202207_mfr_2022Q1_202209.csv"),
                             npr_files = c(
                               som    = file.path(raw_dir, "NPR/PDB2774_NPR_19_45084_SOMdata_til_utlevering_redNorPD_202209.csv"),
                               avtsom = file.path(raw_dir, "NPR/PDB2774_NPR_19_45084_AVTSOMdata_til_utlevering_redNorPD_202209.csv"),
                               asp    = file.path(raw_dir, "NPR/PDB2774_NPR_19_45084_ASPdata_til_utlevering_redNorPD_202209.csv"),
                               bup    = file.path(raw_dir, "NPR/PDB2774_NPR_19_45084_BUPdata_til_utlevering_redNorPD_202209.csv"),
                               phv    = file.path(raw_dir, "NPR/PDB2774_NPR_19_45084_PHVdata_til_utlevering_redNorPD_202209.csv"),
                               reh    = file.path(raw_dir, "NPR/PDB2774_NPR_19_45084_REHdata_til_utlevering_redNorPD_202209.csv"),
                               tsb    = file.path(raw_dir, "NPR/PDB2774_NPR_19_45084_TSBdata_til_utlevering_redNorPD_202209.csv")),
                             kuhr_file = file.path(raw_dir, "KUHR/.*.csv")) {
  needed <- c("som", "avtsom", "asp", "bup", "phv", "reh", "tsb")
  if (!is.character(npr_files) || !all(needed %in% names(npr_files)))
    stop("`npr_files` must be a named character vector with elements: ", paste(needed, collapse = ", "), call. = FALSE)

  # v1.3 lines 78-105 (hospital: data = NULL here, raw_files added)
  source_files = list(
    birth_registry = list(file = mbrn_file,
                          ref_date = ymd("2004-01-01") - 10234,birth_year_limit = c(1,10000),#birth_year_limit = c(1965,2004),
                          variables = c(mother_id = "MOR_PasientLopeNr_PDB2774",
                                        mother_age = "MORS_ALDER",
                                        event_date = "FDATO_DIFF"     ,
                                        child_order = "PLUREK",
                                        gestational_days = "SVLEN_DG",
                                        death_cat = "DODKAT",
                                        apgar5 = "APGAR5",
                                        birth_year = "MOR_FAAR",
                                        birth_weight = "VEKT") ),   # [v1.4.0] read only when stillbirth_min_weight_g is set
    hospital  = list(data = NULL,
                     raw_files = npr_files[needed],
                     ref_date = ymd("2004-01-01") - 10234, birth_year_limit = c(1,10000),#birth_year_limit = c(1965,2004),
                     variables = c(mother_id = "lopeNr_1_PasientLopeNr_PDB2774",
                                   diagnose_code = "KodeVerdi",
                                   stay_id = "visit_id",
                                   visit_date = "diffdato",
                                   birth_year = "fodselsar",
                                   vocabulay = NULL)),
    primary_care = list(file = kuhr_file,
                        ref_date = ymd("2004-01-01") - 10234, birth_year_limit = c(1,10000),#birth_year_limit = c(1965,2004),
                        variables = c(mother_id = "PasientLopeNr_PDB2774",
                                      diagnose_code = "DIAGNOSE_KODE",
                                      vocabulary = "DIAGNOSEKODEVERK",
                                      visit_date = "DIFFERANSEDAGER",
                                      gender = "PASIENT_KJONN",
                                      birth_year = "FXDT"))
  )
  source_files
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

#' Prepare the NPR source table (internal)
#'
#' The top-level preparation of the v1.3 script (lines 24-75), moved into a function. Must be
#' called with the working directory set to the algorithm's work folder. If
#' `npr_mothers_full.csv` is not cached there, the seven raw NPR extracts are read, restricted to
#' ICD-10 diagnoses of women, row-bound, given a `visit_id`, and cached as
#' `npr_mothers_full.csv`. Otherwise the cached file is read, but only if `hospital_full.csv` is
#' not cached as well. The result is stored in `source_files$hospital$data`.
#'
#' If `source_files$hospital$data` is already supplied (not `NULL`), it is used as is and nothing
#' is read or written here.
#'
#' @param source_files See [uio_source_files()].
#' @return `source_files` with `hospital$data` set (possibly `NULL` when `hospital_full.csv` is cached).
#' @keywords internal
#' @noRd
uio_prepare_sources <- function(source_files) {
  if (!is.null(source_files$hospital$data)) return(source_files)
  npr_files = source_files$hospital$raw_files
  if (!file.exists("npr_mothers_full.csv") &&
      (is.null(npr_files) || !all(c("som", "avtsom", "asp", "bup", "phv", "reh", "tsb") %in% names(npr_files))))
    stop("npr_mothers_full.csv is not cached in the work folder and source_files$hospital$raw_files ",
         "does not name the seven raw NPR files; see uio_source_files().", call. = FALSE)

  ####### Load NPR data
  # Not cached: read the seven NPR files, name the linkage-key column (by position), keep ICD-10 diagnoses of
  # women, row-bind them and number the contacts (visit_id = woman, linkage key and file); then cache the result.
  if(!file.exists("npr_mothers_full.csv")){
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

    fwrite(npr0,"npr_mothers_full.csv")
  }else{
    # Cached: read it only if hospital_full.csv still has to be built from it
    if(!file.exists("hospital_full.csv")){
      npr0 = fread("npr_mothers_full.csv")
    }else{
      npr0 = NULL #no need to load it
    }

  }

  source_files$hospital["data"] = list(npr0)
  source_files
}
