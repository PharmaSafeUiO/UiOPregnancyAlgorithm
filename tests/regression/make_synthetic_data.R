## Synthetic MBRN / NPR / KUHR extracts in the exact formats read by UiO_preg_Algo_v1.3.R
##
## Layout under raw_dir (mirrors N:/durable/Raw_data on TSD):
##   MFR/PDB2774_MFR_p202207_mfr_2022Q1_202209.csv                  read by smart_load(select = ...)
##   NPR/PDB2774_NPR_19_45084_<X>data_til_utlevering_redNorPD_202209.csv, X = SOM, AVTSOM, ASP, BUP,
##       PHV, REH, TSB. The linkage key is renamed BY POSITION in the script: column 22 (SOM),
##       16 (AVTSOM, ASP, BUP), 18 (PHV, REH, TSB). BUP has no KodeNavn (the script sets it).
##   KUHR/*.csv                                                     all files matching ".*.csv"
## Dates are day offsets: date = ymd("2004-01-01") - 10234 + offset (reference date 1975-12-25).
##
## Women (ID) and what they test:
##   1  live birth with GA (MBRN) + an earlier birth in 2005 (dropped by years_range) + markers
##      during the pregnancy; F-diagnoses in BUP and PHV
##   2  MBRN fetal death (DODKAT 7) at 150 days: stillbirth with the v1.3 rule (floor(150/7) = 21 > 20),
##      miscarriage with stillbirth_min_days = 154. Birth weight 560 g (v1.4.0 tests: stillbirth with
##      154 days or 500 g; miscarriage with weight_first and 600 g)
##   3  NPR miscarriage O039 with marker Z321 20 days before (implied GA 20 + 64 = 84 days: rejected by
##      the 84-day rule, so dated 84 d; accepted with marker_rejection_days = 91)
##   4  NPR induced abortion O049 without marker (56 d in v1.3; 49 d with induced_abortion_default_days = 49;
##      46 d with the default, the registry median of 2018)
##   5  KUHR miscarriage coded "w82" (lower case): missed by v1.3, found with normalize_codes = TRUE
##   6  KUHR miscarriage coded "W 82" (space): idem
##   7  NPR induced abortion O049 with O359 the same day (O35 rule: 80 d)
##   8  KUHR miscarriage W82 with KUHR marker W78 15 days before (pregnancy-marker dating, 15 + 57 = 72 d)
##   9  NPR miscarriage "O03.9" with marker "Z32.1" 10 days before (dotted codes): v1.3 finds the
##      outcome (unanchored "O03") but not the marker (70 d default); normalised: marker, 10 + 64 = 74 d
##  10  twin live birth (needed: BLOCK 2 of the script refers to livebirth2 etc.)
##  11  live birth with KUHR Z321, W78 and O359 (ICD-10) during the pregnancy; Z-diagnoses in REH, TSB
##  12  man (PASIENT_KJONN 1) with W82 in KUHR: excluded
##  13  NPR miscarriage O021 + KUHR W82 3 days later (KUHR record dropped: within 56 days after)
##  14  MBRN miscarriage (DODKAT 8, 120 days, 300 g) also in NPR (O039 the same day)
##  15  NPR ectopic pregnancy O009 (AVTSOM file)
##  16  NPR molar pregnancy O010 (ASP file)
##  99  man in NPR (kjonn 1) with O039: excluded
##  17  quadruplet live birth (only if with_quadruplets = TRUE). With quadruplets in the data the
##      dcast in BLOCK 2 creates DAGER_FDATO2-4 / SVLEN_DG2-4 itself, so v1.3 lines 481-482
##      (`MBRN[, var_1:=NA_Date_, with = FALSE]`, an error from data.table 1.17) are not executed and
##      the unmodified v1.3 script runs on current data.table. Without quadruplets those lines run.
make_uio_synthetic_data <- function(raw_dir, with_quadruplets = TRUE) {
  stopifnot(requireNamespace("data.table", quietly = TRUE))
  ref_date <- as.Date("2004-01-01") - 10234                     # 1975-12-25, as in the script
  off <- function(d) as.integer(as.Date(d) - ref_date)
  for (d in c("MFR", "NPR", "KUHR")) dir.create(file.path(raw_dir, d), recursive = TRUE, showWarnings = FALSE)

  ## ---- MBRN (MFR) -------------------------------------------------------------------------
  mbrn <- data.table::data.table(
    MOR_PasientLopeNr_PDB2774 = c(1L, 1L, 2L, 10L, 10L, 11L, 14L),
    BARN_LOPENR               = 101:107,                                  # extra column (not selected)
    MORS_ALDER                = c(20L, 30L, 28L, 33L, 33L, 27L, 31L),
    FDATO_DIFF                = off(c("2005-03-01", "2015-06-15", "2016-03-10", "2014-11-11",
                                      "2014-11-11", "2012-01-10", "2011-07-07")),
    PLUREK                    = c(1L, 1L, 1L, 1L, 2L, 1L, 1L),
    SVLEN_DG                  = c(270L, 280L, 150L, 259L, 259L, 275L, 120L),
    DODKAT                    = c(0L, 0L, 7L, 0L, 0L, 0L, 8L),
    APGAR5                    = c(9L, 9L, 0L, 8L, 7L, 10L, 0L),
    MOR_FAAR                  = c(1985L, 1985L, 1988L, 1981L, 1981L, 1985L, 1980L),
    VEKT                      = c(3300L, 3500L, 560L, 2700L, 2600L, 3400L, 300L))   # birth weight, g (v1.4.0)
  if (with_quadruplets)
    mbrn <- rbind(mbrn, data.table::data.table(
      MOR_PasientLopeNr_PDB2774 = 17L, BARN_LOPENR = 108:111, MORS_ALDER = 35L,
      FDATO_DIFF = off("2019-12-12"), PLUREK = 1:4, SVLEN_DG = 230L, DODKAT = 0L,
      APGAR5 = c(7L, 8L, 7L, 6L), MOR_FAAR = 1984L, VEKT = c(1500L, 1450L, 1400L, 1350L)))
  data.table::fwrite(mbrn, file.path(raw_dir, "MFR", "PDB2774_MFR_p202207_mfr_2022Q1_202209.csv"))

  ## ---- NPR (seven files) --------------------------------------------------------------------
  # rows: file, ID, date, code system, code, sex, linkage key
  npr <- data.table::rbindlist(list(
    list("SOM",    1L,  "2014-11-10", "ICD-10", "Z321",  2L, 5001L),
    list("SOM",    1L,  "2014-11-10", "NCSP",   "MBA00", 2L, 5001L),     # procedure: dropped (KodeNavn)
    list("SOM",    3L,  "2017-03-31", "ICD-10", "Z321",  2L, 5003L),
    list("SOM",    3L,  "2017-04-20", "ICD-10", "O039",  2L, 5004L),
    list("SOM",    4L,  "2018-02-14", "ICD-10", "O049",  2L, 5005L),
    list("SOM",    7L,  "2020-01-15", "ICD-10", "O049",  2L, 5007L),
    list("SOM",    7L,  "2020-01-15", "ICD-10", "O359",  2L, 5007L),
    list("SOM",    9L,  "2018-08-30", "ICD-10", "Z32.1", 2L, 5009L),
    list("SOM",    9L,  "2018-09-09", "ICD-10", "O03.9", 2L, 5010L),
    list("SOM",    13L, "2013-05-05", "ICD-10", "O021",  2L, 5013L),
    list("SOM",    14L, "2011-07-07", "ICD-10", "O039",  2L, 5014L),
    list("SOM",    99L, "2015-01-01", "ICD-10", "O039",  1L, 5099L),     # man: dropped
    list("AVTSOM", 15L, "2016-10-10", "ICD-10", "O009",  2L, 6015L),
    list("ASP",    16L, "2022-02-02", "ICD-10", "O010",  2L, 7016L),
    list("BUP",    1L,  "2016-01-01", "ICD-10", "F329",  2L, 8001L),
    list("PHV",    1L,  "2016-02-01", "ICD-10", "F419",  2L, 9001L),
    list("REH",    11L, "2013-03-03", "ICD-10", "Z509",  2L, 9101L),
    list("TSB",    11L, "2013-04-04", "ICD-10", "F102",  2L, 9201L)))
  data.table::setnames(npr, c("file", "id", "date", "system", "code", "sex", "key"))
  key_pos <- c(SOM = 22L, AVTSOM = 16L, ASP = 16L, BUP = 16L, PHV = 18L, REH = 18L, TSB = 18L)
  for (f in names(key_pos)) {
    r <- npr[npr$file == f]
    d <- data.table::data.table(lopeNr_1_PasientLopeNr_PDB2774 = r$id,
                                kjonn = r$sex,
                                fodselsar = 1985L,
                                diffdato = off(r$date))
    if (f != "BUP") d[["KodeNavn"]] <- r$system                        # BUP: no KodeNavn column
    d[["KodeVerdi"]] <- r$code
    n_fill <- key_pos[[f]] - 1L - ncol(d)                               # filler columns before the key
    for (k in seq_len(n_fill)) d[[sprintf("FILL%02d", k)]] <- k
    d[[paste0("aggr_key_", tolower(f))]] <- r$key                       # renamed to kobl_nokkel by position
    d[["after_key"]] <- "x"                                             # a column after the key
    stopifnot(names(d)[key_pos[[f]]] == paste0("aggr_key_", tolower(f)))
    data.table::fwrite(d, file.path(raw_dir, "NPR", sprintf("PDB2774_NPR_19_45084_%sdata_til_utlevering_redNorPD_202209.csv", f)))
  }

  ## ---- KUHR (two files) ---------------------------------------------------------------------
  kuhr <- data.table::rbindlist(list(
    list("a", 11L, "2011-05-15", "Z321", "ICD-10", 2L),
    list("a", 11L, "2011-06-01", "W78",  "ICPC-2", 2L),
    list("a", 11L, "2011-09-01", "O359", "ICD-10", 2L),
    list("a", 13L, "2013-05-08", "W82",  "ICPC-2", 2L),
    list("a", 1L,  "2015-01-20", "W78",  "ICPC-2", 2L),
    list("b", 5L,  "2019-05-05", "w82",  "ICPC-2", 2L),
    list("b", 6L,  "2019-08-08", "W 82", "ICPC-2", 2L),
    list("b", 8L,  "2021-02-16", "W78",  "ICPC-2", 2L),
    list("b", 8L,  "2021-03-03", "W82",  "ICPC-2", 2L),
    list("b", 12L, "2020-06-06", "W82",  "ICPC-2", 1L)))
  data.table::setnames(kuhr, c("file", "id", "date", "code", "system", "sex"))
  for (f in c("a", "b")) {
    r <- kuhr[kuhr$file == f]
    d <- data.table::data.table(PasientLopeNr_PDB2774 = r$id,
                                DIAGNOSE_KODE = r$code,
                                DIAGNOSEKODEVERK = r$system,
                                DIFFERANSEDAGER = off(r$date),
                                PASIENT_KJONN = r$sex,
                                FXDT = 1985L,
                                TAKST = "2ad")                          # extra column (not selected)
    data.table::fwrite(d, file.path(raw_dir, "KUHR", paste0("kuhr_", f, ".csv")))
  }
  invisible(raw_dir)
}
