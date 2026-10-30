## [experimental] Test of the missing-GA report on synthetic data with known answers.
##
## Usage: Rscript tests/missing_ga/test_missing_ga.R --lib=<library holding UiOPregnancyAlgorithm> [--out=<folder>]
## The synthetic data of the regression test (tests/regression/make_synthetic_data.R) plus these women,
## all MBRN births without gestational age (SVLEN_DG missing) unless stated:
##   20  fetal death (DODKAT 7), 400 g, 2016-05-10; NPR O021 2 days before, KUHR W82 20 days before
##   21  fetal death (DODKAT 8), 800 g, 2017-06-01; NPR O049 the same day, NPR O359 3 days after (O35 rule)
##   22  fetal death (DODKAT 9), weight missing, 2018-03-03; nothing in NPR or KUHR
##   23  fetal death (DODKAT 7), 350 g, 2019-02-02; KUHR W82 3 days before; NPR O039 100 days before (outside);
##       NPR Z321 154 days before (outside the marker window)
##   24  live birth (DODKAT 0), 3000 g, 2015-09-09; NPR O034 3 days before
##   25  DODKAT 13 with Apgar 8 (a live birth), 2014-04-04; nothing
##   26  DODKAT 13 with Apgar 0 (unknown), 2014-05-05; nothing
##   27  fetal death (DODKAT 7), 600 g, 2020-10-10; NPR Z321 30 days before (start: 30 + 64 = 94 days), NPR O039 5 days after
##   28  twins 2021-01-01: live birth with GA 250 days and a fetal death (DODKAT 7, 900 g) without GA
##   29  fetal death (DODKAT 7), weight 0 (= missing), 2022-06-06; NPR O021 the same day, KUHR W83 10 days before
##   30  fetal death (DODKAT 7), 2024-05-05: outside years_range c(2008, 2023), not counted
##   31  fetal death (DODKAT 7), 700 g, 2019-09-09; NPR Z321 60 days before (implies 60 + 64 = 124 days: rejected)
##   32  live birth (DODKAT 0), 2016-01-15; NPR O021 120 days before (previous outcome: 92 days left, below 154: not imputed)
##   33  fetal death (DODKAT 9), weight missing, 2017-03-03; NPR O039 100 days before and again 73 days before (one event,
##       dated by its first record: start 2016-12-21, 72 days; a stillbirth below 154 days becomes a late spontaneous abortion)
##   34  live birth (DODKAT 0), 2018-06-01; an earlier MBRN live birth WITH GA (270 days) on 2017-12-01, and NPR O021 19 days
##       after it (part of that birth, not a previous outcome): start 2017-12-29, 154 days, imputed
## W23's NPR O039 100 days before is its previous outcome: start 28 days after it (72 days). W29's KUHR W83 makes it a termination.
args <- commandArgs(TRUE)
get_arg <- function(k, default = NULL) { a <- grep(paste0("^--", k, "="), args, value = TRUE); if (length(a)) sub(paste0("^--", k, "="), "", a[1]) else default }
lib <- get_arg("lib", .libPaths()[1])
this_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
pkg_dir <- normalizePath(file.path(dirname(this_file), "..", ".."))
out_root <- get_arg("out", file.path(tempdir(), "missing_ga_test"))
unlink(out_root, recursive = TRUE); dir.create(out_root, recursive = TRUE)

library(data.table)
library(UiOPregnancyAlgorithm, lib.loc = lib)
source(file.path(pkg_dir, "tests", "regression", "make_synthetic_data.R"))
raw_dir <- file.path(out_root, "Raw_data"); make_uio_synthetic_data(raw_dir)

ref_date <- as.Date("2004-01-01") - 10234
off <- function(d) as.integer(as.Date(d) - ref_date)
# MBRN
f_mfr <- list.files(file.path(raw_dir, "MFR"), full.names = TRUE)
mfr <- fread(f_mfr)
add <- data.table(
  MOR_PasientLopeNr_PDB2774 = c(20L, 21L, 22L, 23L, 24L, 25L, 26L, 27L, 28L, 28L, 29L, 30L, 31L, 32L, 33L, 34L, 34L),
  BARN_LOPENR = 201:217, MORS_ALDER = 30L,
  FDATO_DIFF = off(c("2016-05-10", "2017-06-01", "2018-03-03", "2019-02-02", "2015-09-09", "2014-04-04", "2014-05-05",
                     "2020-10-10", "2021-01-01", "2021-01-01", "2022-06-06", "2024-05-05", "2019-09-09", "2016-01-15", "2017-03-03",
                     "2017-12-01", "2018-06-01")),
  PLUREK = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L),
  SVLEN_DG = c(NA, NA, NA, NA, NA, NA, NA, NA, 250L, NA, NA, NA, NA, NA, NA, 270L, NA),
  DODKAT = c(7L, 8L, 9L, 7L, 0L, 13L, 13L, 7L, 0L, 7L, 7L, 7L, 7L, 0L, 9L, 0L, 0L),
  APGAR5 = c(0L, 0L, 0L, 0L, 9L, 8L, 0L, 0L, 8L, 0L, 0L, 0L, 0L, 9L, 0L, 9L, 9L),
  MOR_FAAR = 1986L,
  VEKT = c(400L, 800L, NA, 350L, 3000L, 3200L, 3100L, 600L, 2500L, 900L, 0L, 700L, 700L, 3300L, NA, 3400L, 3500L))
fwrite(rbind(mfr, add), f_mfr)
# NPR (SOM file; the linkage key is column 22)
f_som <- list.files(file.path(raw_dir, "NPR"), pattern = "_SOMdata", full.names = TRUE)
som <- fread(f_som)
tmpl <- som[1]
npr_add <- rbindlist(lapply(list(
  list(20L, "2016-05-08", "O021"), list(21L, "2017-06-01", "O049"), list(21L, "2017-06-04", "O359"), list(23L, "2018-10-25", "O039"), list(23L, "2018-09-01", "Z321"),
  list(24L, "2015-09-06", "O034"), list(27L, "2020-09-10", "Z321"), list(27L, "2020-10-15", "O039"),
  list(29L, "2022-06-06", "O021"), list(31L, "2019-07-11", "Z321"), list(32L, "2015-09-17", "O021"),
  list(33L, "2016-11-23", "O039"), list(33L, "2016-12-20", "O039"), list(34L, "2017-12-20", "O021")), function(r) {
    x <- copy(tmpl); x[, lopeNr_1_PasientLopeNr_PDB2774 := r[[1]]][, diffdato := off(r[[2]])][, KodeNavn := "ICD-10"][, KodeVerdi := r[[3]]]
    x }))
npr_add[, aggr_key_som := 20000L + .I]
fwrite(rbind(som, npr_add), f_som)
# KUHR
f_k <- list.files(file.path(raw_dir, "KUHR"), full.names = TRUE)[1]
ku <- fread(f_k)
ku_add <- data.table(PasientLopeNr_PDB2774 = c(20L, 23L, 29L), DIAGNOSE_KODE = c("W82", "W82", "W83"),
                     DIAGNOSEKODEVERK = "ICPC-2", DIFFERANSEDAGER = off(c("2016-04-20", "2019-01-30", "2022-05-27")),
                     PASIENT_KJONN = 2L, FXDT = 1986L, TAKST = "2ad")
fwrite(rbind(ku, ku_add), f_k)

src <- uio_project_sources("p1111", raw_dir = raw_dir)
wd <- file.path(out_root, "work"); dir.create(wd)
res0 <- run_uio_pa(workdir = wd, source_files = src, years_range = c(2008, 2023), output_dir = file.path(out_root, "output_unmasked"),
                   mask_below = 0, verbose = FALSE, report = FALSE)
res5 <- run_uio_pa(workdir = wd, source_files = src, years_range = c(2008, 2023), output_dir = file.path(out_root, "output"),
                   verbose = FALSE)

n_fail <- 0L
check <- function(label, ok) { cat(if (isTRUE(ok)) "[PASS]" else { n_fail <<- n_fail + 1L; "[FAIL]" }, label, "\n") }
T <- attr(res0, "missing_ga")$tables
U <- attr(res0, "missing_ga")$units
key <- function(df, cols) paste(apply(as.data.frame(df)[, cols, drop = FALSE], 1, function(r) paste(trimws(r), collapse = "/")), collapse = "; ")
cell <- function(tab, g, cat) { x <- as.data.frame(tab); x$n[x$group == g & x$category == cat] }
for (nm in names(T)) { cat("\n--", nm, "--\n"); print(as.data.frame(T[[nm]]), row.names = FALSE) }
cat("\n")
FD <- "Fetal deaths, all"; LT <- "Fetal deaths, < 500 g"; GE <- "Fetal deaths, >= 500 g"; WM <- "Fetal deaths, weight missing"; LB <- "Live births"

check("groups: fetal deaths 9 (< 500 g 2, >= 500 g 4, weight missing 3), live births 4",
      identical(as.integer(T$groups$n), c(9L, 2L, 4L, 3L, 4L)))
check("A: outcome codes in the window: fetal deaths 5 (NPR 4, KUHR 2, both 1); live births 1",
      cell(T$A_summary, FD, "with an outcome code") == 5 && cell(T$A_summary, FD, "  in NPR") == 4 &&
        cell(T$A_summary, FD, "  in KUHR") == 2 && cell(T$A_summary, FD, "  in both") == 1 && cell(T$A_summary, LB, "with an outcome code") == 1)
check("A: confirmed outcome, fetal deaths: miscarriage 4 (NPR only 2, KUHR only 1, both 1), termination 1, no outcome code 4 (W33's codes are 73 and 100 days before)",
      key(T$A_outcome[T$A_outcome$group == FD & T$A_outcome$n > 0, ], c("outcome_confirmed", "n", "NPR_only", "KUHR_only", "NPR_and_KUHR")) ==
        "Miscarriage/4/2/1/1; Termination of pregnancy/1/1/0/0; No outcome code/4/0/0/0")
check("A: by weight: < 500 g miscarriage 2; >= 500 g miscarriage 1, termination 1, none 2; weight missing miscarriage 1, none 2",
      cell(T$glance_outcome, LT, "Miscarriage") == 2 && cell(T$glance_outcome, GE, "Miscarriage") == 1 &&
        cell(T$glance_outcome, GE, "Termination of pregnancy") == 1 && cell(T$glance_outcome, GE, "No outcome code") == 2 &&
        cell(T$glance_outcome, WM, "Miscarriage") == 1 && cell(T$glance_outcome, WM, "No outcome code") == 2)
check("A: birth weight by confirmed outcome: < 500 g miscarriage 2 (350, 400 g: median 375); all fetal deaths termination 1 (W21, 800 g)",
      key(T$A_weight[T$A_weight$group == LT, ], c("outcome_confirmed", "n", "weight_median", "weight_min", "weight_max")) == "Miscarriage/2/375/350/400" &&
        key(T$A_weight[T$A_weight$group == FD & T$A_weight$outcome_confirmed == "Termination of pregnancy", ], c("n", "weight_median")) == "1/800")
check("A: W23's NPR O039 100 days before is outside the window (only KUHR W82)", U[ID == "23", oc_source] == "KUHR only")
check("A: first outcome code: W20 20 days before (KUHR), W27 5 days after",
      U[ID == "20", oc_first] == -20 && U[ID == "27", oc_first] == 5)
check("A: codes in the window, fetal deaths: NPR O021 2, O039 1, O049 1; KUHR W82 2, W83 1 (W83 not an outcome code)",
      key(T$A_codes[T$A_codes$group == FD, ], c("source", "code", "n")) == "NPR/O021/2; NPR/O039/1; NPR/O049/1; KUHR/W82/2; KUHR/W83/1" &&
        T$A_codes$outcome_code_of_algorithm[T$A_codes$group == FD & T$A_codes$code == "W83"] == "no" &&
        T$A_codes$meaning[T$A_codes$group == FD & T$A_codes$code == "O021"] == "Other abnormal products of conception: missed abortion")
check("B: markers, fetal deaths: 2 with a marker; 1 accepted (W27, 94 days), 1 all rejected (W31, 124 days); 7 none",
      cell(T$B_summary, FD, "with a marker in the window") == 2 && cell(T$B_summary, FD, "  marker accepted (start estimated)") == 1 &&
        cell(T$B_summary, FD, "  markers found, all implying 119 days or more") == 1 && cell(T$B_summary, FD, "no marker in the window") == 7 &&
        U[ID == "27", ga_marker] == 94 && U[ID == "31", ga_rejected] == 124)
check("B: dating method, fetal deaths: marker 1 (GA 94), rejected 1 (implied 124), O35 rule 1 (W21), no marker 6",
      key(T$B_method[T$B_method$group == FD, ], c("dating_method", "n", "GA_median")) ==
        "Pregnancy marker (start estimated)/1/94; Markers found, all imply >= 119 days/1/124; O35 rule (termination, fixed length)/1/NA; No marker in the window/6/NA")
check("B: the marker that set the start: NPR Z321 (pregnancy confirmed, 64 days), 1 unit, in fetal deaths and >= 500 g",
      key(T$B_markers, c("group", "source", "code", "days_to_start", "n")) == "Fetal deaths, all/NPR/Z321/64/1; Fetal deaths, >= 500 g/NPR/Z321/64/1" &&
        T$B_markers$meaning[1] == "Pregnancy examination and test: pregnancy confirmed")
check("C: captured, fetal deaths: outcome and start 1, outcome only 4, start only 0, neither 4; live births: outcome only 1, neither 3",
      cell(T$captured, FD, "Outcome and start") == 1 && cell(T$captured, FD, "Outcome only") == 4 && cell(T$captured, FD, "Start only") == 0 &&
        cell(T$captured, FD, "Neither") == 4 && cell(T$captured, LB, "Outcome only") == 1 && cell(T$captured, LB, "Neither") == 3)
check("D: the algorithm now keeps every single fetal death without GA as an MBRN pregnancy: late spontaneous abortion 3 (W20, W23, W33), termination 2 (W21 O04, W29 W83), stillbirth 3 (imputed GA); the twin W28 is the MBRN live birth",
      key(T$C_algorithm[T$C_algorithm$group == FD, ], c("decided_by", "outcome", "n", "MBRN_GA", "MBRN_GA_imputed")) ==
        "MBRN/Live birth/1/1/0; MBRN/Late spontaneous abortion/3/0/3; MBRN/Termination of pregnancy/2/0/2; MBRN/Stillbirth/3/0/3")

## [experimental] imputation of the gestational age of MBRN single fetal deaths without GA
P <- as.data.table(res0)
fd_dates <- data.table(ID = c(20L, 21L, 22L, 23L, 27L, 29L, 31L, 33L),
                       d = as.Date(c("2016-05-10", "2017-06-01", "2018-03-03", "2019-02-02", "2020-10-10", "2022-06-06", "2019-09-09", "2017-03-03")),
                       kind = c("Late spontaneous abortion", "Termination of pregnancy", "Stillbirth", "Late spontaneous abortion", "Stillbirth",
                                "Termination of pregnancy", "Stillbirth", "Late spontaneous abortion"),
                       ga = c(120, 119, 154, 72, 154, 119, 154, 72),
                       dm = c("late spontaneous abortions", "terminations", "stillbirths", "late spontaneous abortions, start 28 d after the previous outcome",
                              "stillbirths", "terminations", "stillbirths", "stillbirths, start 28 d after the previous outcome"))
got <- P[fd_dates, on = .(ID, date_outcome = d), nomatch = NA]
check("imputation: W20 (400 g), W23 (350 g) late spontaneous abortions; W21 (800 g, NPR O049 the same day) and W29 (weight 0 = missing, KUHR W83 10 days before) terminations; W22 (no weight, no code), W27 (600 g), W31 (700 g) stillbirths; W33 (no weight, no code in the window) a stillbirth by the rule, a late spontaneous abortion after its start was moved (72 days)",
      identical(got$outcome_lable, fd_dates$kind) && all(got$outcome_source == "MBRN"))
check("imputation: GA = data median (late spontaneous abortions 120 days, stillbirths 154 days: the median 150 of the synthetic stillbirths raised to 154, terminations 119 days: no MBRN termination with GA, fallback), but W23 and W33 72 days (start 28 days after the previous outcome); dating_method and ga_imputed say so",
      identical(as.numeric(got$GA_event_days_final), fd_dates$ga) && all(got$ga_imputed) &&
        identical(got$dating_method, paste0("MBRN (imputed GA: median of ", fd_dates$dm, ")")) &&
        all(got$dating_class == "fixed") && all(got$dating_source == "MBRN median"))
IM <- attr(res0, "imputed_missing_ga")
check("W83: W29 is a termination by its KUHR W83 alone (no O04); W21 by NPR O04",
      { a <- IM[ID == "29"]; b <- IM[ID == "21"]
        a$kind == "termination" && a$w83 && !a$o04_npr && !a$o04_kuhr && b$kind == "termination" && b$o04_npr && !b$w83 })
check("previous outcome: W23's NPR O039 100 days before; W33's event of two O039 records dated by the first (100 days before); W34's earlier MBRN birth (its NPR O021 19 days after that birth belongs to it); W24's O034 3 days before is its own code (start not moved)",
      IM[ID == "23", prev_outcome] == as.integer(as.Date("2018-10-25")) && IM[ID == "33", prev_outcome] == as.integer(as.Date("2016-11-23")) &&
        IM[ID == "34", prev_outcome] == as.integer(as.Date("2017-12-01")) && is.na(IM[ID == "24", prev_outcome]) && !IM[ID == "24", capped])
check("live births: W34 imputed with 154 days (start 28 days after its previous birth, exactly the limit); W32 not imputed (92 days left): no pregnancy at its birth date, its NPR O021 is a spontaneous abortion",
      { w34 <- P[ID == 34L & date_outcome == as.Date("2018-06-01")]
        nrow(w34) == 1L && w34$GA_event_days_final == 154 && w34$ga_imputed && !IM[ID == "32", imputed] && IM[ID == "32", ga] == 92 &&
          nrow(P[ID == 32L & date_outcome == as.Date("2016-01-15")]) == 0L &&
          P[ID == 32L & date_outcome == as.Date("2015-09-17"), outcome_lable] == "Spontaneous abortion" })
check("the earlier outcomes are kept as their own pregnancies once the start is moved: W23's O039 (2018-10-25) and W33's O039 (2016-11-23) are spontaneous abortions",
      P[ID == 23L & date_outcome == as.Date("2018-10-25"), outcome_lable] == "Spontaneous abortion" &&
        P[ID == 33L & date_outcome == as.Date("2016-11-23"), outcome_lable] == "Spontaneous abortion")
check("missing_ga_min_gap_days = 0: W23's start is the previous outcome date (100 days)",
      { r <- as.data.table(run_uio_pa(workdir = wd, source_files = src, years_range = c(2008, 2023), output_dir = NULL, verbose = FALSE,
                                      missing_ga_report = FALSE, missing_ga_min_gap_days = 0))
        r[ID == 23L & date_outcome == as.Date("2019-02-02"), GA_event_days_final] == 100 })
check("O04 rule: W21's termination is the MBRN pregnancy (TOP_source MBRN+NPR, outcome_final 3); codes_mbrn keeps the registered DODKAT 8",
      { w <- P[ID == 21L & date_outcome == as.Date("2017-06-01")]
        nrow(w) == 1L && w$outcome_final == 3L && w$TOP_source %in% 2 && grepl("DODKAT 8", w$codes_mbrn, fixed = TRUE) })
check("no double counting: one pregnancy of the algorithm within 55 days of each of these birth dates (the NPR/KUHR codes around them belong to the MBRN pregnancy)",
      all(U[ID %in% as.character(fd_dates$ID) & group == "SB", n_candidates] == 1L))
check("the twin W28 (fetal death without GA) is not imputed; the pregnancy outside years_range (W30, 2024) is kept by the algorithm as MBRN births after the last year are",
      !any(P[ID == 28L, ga_imputed]) && P[ID == 30L, outcome_lable] == "Stillbirth")
check("outcome_lable: NPR/KUHR miscarriages are 'Spontaneous abortion', MBRN ones 'Late spontaneous abortion'; outcome_final stays 2 for both",
      all(P[outcome_final == 2 & MC_source %in% 5:7, outcome_lable] == "Spontaneous abortion") &&
        all(P[outcome_final == 2 & MC_source %in% 1:4, outcome_lable] == "Late spontaneous abortion") &&
        nrow(P[outcome_final == 2 & MC_source %in% 5:7]) > 0 && setequal(unique(P$outcome_lable[!is.na(P$outcome_lable)]),
        c("Live birth", "Spontaneous abortion", "Late spontaneous abortion", "Termination of pregnancy", "Stillbirth", "Molar pregnancy", "Ectopic pregnancy")[
          c("Live birth", "Spontaneous abortion", "Late spontaneous abortion", "Termination of pregnancy", "Stillbirth", "Molar pregnancy", "Ectopic pregnancy") %in% P$outcome_lable]))
check("live births without GA: W24 (DODKAT 0) and W25 (DODKAT 13, Apgar 8) kept as live births with 280 days; W26 (DODKAT 13, Apgar 0) not",
      { lb <- P[ID %in% c(24L, 25L)]
        nrow(lb) == 2L && all(lb$outcome_final == 1L) && all(lb$GA_event_days_final == 280) && all(lb$ga_imputed) &&
          all(lb$dating_method == "MBRN (imputed GA: median of live births)") && nrow(P[ID == 26L]) == 0L })
check("missing_ga_impute = FALSE: no imputed pregnancy, the fetal deaths without GA are dropped (W22 has no pregnancy)",
      { r <- as.data.table(run_uio_pa(workdir = wd, source_files = src, years_range = c(2008, 2023), output_dir = NULL, verbose = FALSE,
                                      missing_ga_report = FALSE, missing_ga_impute = FALSE))
        !any(r$ga_imputed) && nrow(r[ID == 22L]) == 0L })

od <- file.path(out_root, "output", "missing_ga")
fl <- list.files(od)
check("report files written (md, html, docx, chart png) and 18 CSV tables; no sensitivity analysis",
      all(c("missing_ga_report.md", "missing_ga_report.html", "missing_ga_report.docx", "missing_ga_glance.png") %in% fl) &&
        sum(grepl("^table_.*csv$", fl)) == 18L && !any(grepl("sensitivity", fl)) && is.null(attr(res0, "missing_ga")$sensitivity))
html <- readLines(file.path(od, "missing_ga_report.html"), encoding = "UTF-8")
check("HTML: chart embedded; hover text on n and on the group columns; parts A-D present",
      any(grepl("data:image/png;base64,", html, fixed = TRUE)) && any(grepl('<th title="Units: one mother and birth date', html, fixed = TRUE)) &&
        any(grepl('<th title="n (% of the units of this group)">Fetal deaths, &lt; 500 g</th>', html, fixed = TRUE)) &&
        all(vapply(c("A. Outcome confirmed by NPR or KUHR", "B. Start estimated from pregnancy markers", "C. Outcome and start together",
                     "D. For comparison"), function(s) any(grepl(s, html, fixed = TRUE)), TRUE)))
md <- readLines(file.path(od, "missing_ga_report.md"), encoding = "UTF-8")
check("masked report: small counts shown as <5, no small number in a table", any(grepl("| <5 |", md, fixed = TRUE)) &&
        !any(grepl("\\| [1-4] \\|", md)) && !any(grepl("\\| [1-4] \\(", md)))
dx <- file.path(od, "missing_ga_report.docx")
zl <- utils::unzip(dx, list = TRUE)$Name
check("docx is a valid zip with document.xml, styles.xml and the chart", all(c("word/document.xml", "word/styles.xml", "[Content_Types].xml", "word/media/image1.png") %in% zl))
if (requireNamespace("xml2", quietly = TRUE)) {
  td <- tempfile(); utils::unzip(dx, exdir = td)
  check("document.xml is well-formed XML", inherits(tryCatch(xml2::read_xml(file.path(td, "word", "document.xml")), error = function(e) e), "xml_document"))
}
check("the result without the report has no attribute and the same pregnancies",
      { r <- run_uio_pa(workdir = wd, source_files = src, years_range = c(2008, 2023), output_dir = NULL, verbose = FALSE, missing_ga_report = FALSE)
        is.null(attr(r, "missing_ga")) && isTRUE(all.equal(r, res0, check.attributes = FALSE)) })
check("a longer marker look-back (200 days, no rejection limit) dates W31 from its marker (124 days)",
      { r <- run_uio_pa(workdir = wd, source_files = src, years_range = c(2008, 2023), output_dir = NULL, verbose = FALSE,
                        missing_ga_marker_days = 200, missing_ga_reject_days = 400)
        attr(r, "missing_ga")$units[ID == "31", ga_marker] == 124 })
check("main analysis: W23's early marker (154 days before) is outside the 70-day window", !U[ID == "23", marker_any])
check("HTML: one analysis, no switch", !any(grepl('<select', html, fixed = TRUE)) && !any(grepl("sensitivity", html, ignore.case = TRUE)))
check("Word: one chart only", !"word/media/image2.png" %in% zl)
cat("\nOutput:", od, "\n")
cat(if (n_fail) paste("FAILED:", n_fail) else "ALL CHECKS PASSED", "\n")
