## Regression test: original UiO_preg_Algo_v1.3.R versus the package UiOPregnancyAlgorithm 2.0.0,
## on synthetic MBRN/NPR/KUHR extracts (see make_synthetic_data.R).
##
## Usage (from any folder):
##   Rscript tests/regression/run_regression.R --lib=<library holding UiOPregnancyAlgorithm> \
##           [--script=<path to the original UiO_preg_Algo_v1.3.R; default tests/regression/reference/>]
## Output: printed, and written to tests/regression/results/regression_log.txt (+ result .rds files).
##
## Checks
##  1. the package resolves every free symbol to the same package as the script does
##  2. uio_project_sources("p1111"), uio_hospital_codes(), uio_primary_care_codes() equal the v1.3 objects
##  3. original v1.3 (sys.source, as run_uio.R) == package with uio_v13_settings() (identical data.table)
##  4. package rerun from the cached CSV files == first run; settings never change the cache;
##     the working directory is restored (also after an error)
##  5. the settings change exactly the expected pregnancies, with the expected values
##  6. on data without quadruplets (where v1.3 lines 481-482 run) the package works and gives the same
##     results; the original v1.3 fails there on data.table >= 1.17 (with=FALSE + := is an error)
##  7. [v1.4.0] birth weight for stillbirth, year-specific and drawn induced-abortion defaults
##  8. [v2.0.0] settings as arguments, the defaults, the "and" weight rule, and the former fixed values
args <- commandArgs(TRUE)
getarg <- function(name, default) {
  a <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(a)) sub(paste0("^--", name, "="), "", a[1]) else default
}
this_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
this_dir  <- dirname(normalizePath(this_file, winslash = "/"))
lib       <- normalizePath(getarg("lib", .libPaths()[1]), winslash = "/")
script    <- normalizePath(getarg("script", file.path(this_dir, "reference", "UiO_preg_Algo_v1.3.R")), winslash = "/")
results_dir <- file.path(this_dir, "results"); dir.create(results_dir, showWarnings = FALSE)
rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")

suppressPackageStartupMessages(library(data.table))
library(UiOPregnancyAlgorithm, lib.loc = lib)

log_con <- file(file.path(results_dir, "regression_log.txt"), open = "wt")
sink(log_con, split = TRUE)
ok_all <- TRUE
check <- function(label, ok) {
  ok <- isTRUE(ok); ok_all <<- ok_all && ok
  cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", label))
  invisible(ok)
}

cat("UiO regression test,", format(Sys.time()), "\n")
cat("R:", R.version.string, "| data.table", format(packageVersion("data.table")), "| lubridate",
    format(packageVersion("lubridate")), "| stringr", format(packageVersion("stringr")), "| Rcpp",
    format(packageVersion("Rcpp")), "\n")
cat("package:", format(packageVersion("UiOPregnancyAlgorithm", lib.loc = lib)), "from", lib, "\n")
cat("original script:", script, " md5", tools::md5sum(script), "\n\n")

tmp <- normalizePath(tempfile("uio_regression_"), winslash = "/", mustWork = FALSE); dir.create(tmp)
raw_dir <- file.path(tmp, "Raw_data")
source(file.path(this_dir, "make_synthetic_data.R"))
make_uio_synthetic_data(raw_dir)
cat("synthetic raw extracts in", raw_dir, ":\n"); print(list.files(raw_dir, recursive = TRUE))

run_child <- function(child, params, tag, allow_fail = FALSE) {
  pf <- file.path(tmp, paste0("params_", tag, ".rds")); saveRDS(params, pf)
  out <- suppressWarnings(system2(rscript, c(shQuote(file.path(this_dir, child)), shQuote(pf)),
                                  stdout = TRUE, stderr = TRUE))
  writeLines(out, file.path(results_dir, paste0("child_", tag, ".log")))
  status <- attr(out, "status"); if (is.null(status)) status <- 0L
  if (status != 0L && !allow_fail) { cat(tail(out, 30), sep = "\n"); stop("child '", tag, "' failed (status ", status, ")") }
  invisible(structure(out, status = status))
}

## ---- 1. symbol resolution --------------------------------------------------------------------
cat("\n== 1. symbol resolution (script session vs package namespace) ==\n")
run_child("check_symbol_resolution.R", list(mode = "script", script = script, out = file.path(tmp, "sym_script.rds")), "symbols_script")
run_child("check_symbol_resolution.R", list(mode = "package", lib = lib, out = file.path(tmp, "sym_pkg.rds")), "symbols_package")
sym_s <- readRDS(file.path(tmp, "sym_script.rds")); sym_p <- readRDS(file.path(tmp, "sym_pkg.rds"))
cat("package session search path:", sym_p$search, "\n")
for (part in c("detect", "prepare")) {
  o_s <- sym_s$origins[[part]]; o_p <- sym_p$origins[[part]]
  found <- names(o_s)[!is.na(o_s)]
  # a symbol that the package calls qualified (pkg::name) instead of unqualified counts with that package
  q_p <- sym_p$qualified[[part]]
  o_p_eff <- o_p[found]; names(o_p_eff) <- found
  use_q <- is.na(o_p_eff) & found %in% names(q_p)
  o_p_eff[use_q] <- q_p[found[use_q]]
  if (any(use_q)) cat("  called qualified in the package:", paste0(q_p[found[use_q]], "::", found[use_q]), "\n")
  mism <- found[is.na(o_p_eff) | o_s[found] != o_p_eff]
  # [v2.0.0] sample(), sort() and table() were used only by the v1.3 guessing of text-date formats, replaced by
  # lubridate::parse_date_time() (dates of the P1111 extracts are day offsets, so that code did not run there)
  gone <- if (part == "detect") intersect(mism, c("sample", "sort", "table")) else character(0)
  if (length(gone)) cat("  no longer used (v1.3 text-date guessing):", gone, "\n")
  mism <- setdiff(mism, gone)
  nonbase <- found[o_s[found] != "base"]
  cat(sprintf("%s: %d free symbols resolve to functions/objects in the script; non-base ones: %s\n", part,
              length(found), paste(sprintf("%s(%s)", nonbase, o_s[nonbase]), collapse = " ")))
  check(sprintf("%s: every one of them resolves to the same package in the package namespace", part), length(mism) == 0)
  if (length(mism)) print(rbind(script = o_s[mism], package = o_p_eff[mism]))
  new_syms <- setdiff(names(o_p), names(o_s))
  cat("  symbols new in the package version:", paste(sprintf("%s(%s)", new_syms, o_p[new_syms]), collapse = " "), "\n")
  newly_found <- names(o_p)[!is.na(o_p) & names(o_p) %in% names(o_s)[is.na(o_s)]]
  check(sprintf("%s: no column-name symbol of the script resolves to an object in the package", part), length(newly_found) == 0)
}

## ---- 2. source definitions and marker lists -----------------------------------------------------
cat("\n== 2. uio_project_sources(\"p1111\") / marker lists versus the v1.3 top-level objects ==\n")
wd_obj <- file.path(tmp, "wd_objects"); dir.create(wd_obj)
writeLines("x", file.path(wd_obj, "npr_mothers_full.csv")); writeLines("x", file.path(wd_obj, "hospital_full.csv"))  # dummies: nothing is read
run_child("child_run_original.R", list(mode = "objects", script = script, workdir = wd_obj, out = file.path(tmp, "orig_objects.rds")), "original_objects")
oo <- readRDS(file.path(tmp, "orig_objects.rds"))
src_p <- uio_project_sources("p1111")
br_p <- src_p$birth_registry; br_p$variables <- br_p$variables[names(br_p$variables) != "birth_weight"]
check("birth_registry element identical (apart from the birth_weight variable added in 1.4.0)", identical(oo$source_files$birth_registry, br_p))
check("birth_weight variable is VEKT", identical(unname(src_p$birth_registry$variables["birth_weight"]), "VEKT"))
check("primary_care element identical", identical(oo$source_files$primary_care, src_p$primary_care))
h_o <- oo$source_files$hospital[c("ref_date", "birth_year_limit", "variables")]
h_p <- src_p$hospital[c("ref_date", "birth_year_limit", "variables")]
check("hospital element identical (dates, birth-year limits, variables)", identical(h_o, h_p))
s_lines <- readLines(script)
npr_paths <- sub('^.*fread\\("([^"]+)".*$', "\\1", grep('fread\\("N:/durable/Raw_data/NPR/', s_lines, value = TRUE))
check("raw NPR paths identical to the script's fread() paths (same order som..tsb)", identical(npr_paths, unname(src_p$hospital$raw_files)))
check("uio_hospital_codes() identical to v1.3 hospital_codes", identical(oo$hospital_codes, uio_hospital_codes()))
check("uio_primary_care_codes() identical to v1.3 primary_care_codes", identical(oo$primary_care_codes, uio_primary_care_codes()))
check("v1.3 years_range is c(2008, 2023), the run_uio_pa() default", identical(oo$years_range, eval(formals(run_uio_pa)$years_range)))
nc <- UiOPregnancyAlgorithm:::.uio_normalize_code
check("default marker codes are unchanged by normalisation",
      identical(nc(uio_hospital_codes()$code), uio_hospital_codes()$code) && identical(nc(uio_primary_care_codes()$code), uio_primary_care_codes()$code))
lits <- c("O00", "O01", "O02", "O03", "O04", "O35", "W82")
check("literal patterns O00-O04, O35, W82 are unchanged by normalisation", identical(nc(lits), lits))
cat("examples: ", paste(sprintf('"%s" -> "%s"', c("Z32.1", "w82", "W 82", "o03.9", "O039"), nc(c("Z32.1", "w82", "W 82", "o03.9", "O039"))), collapse = ", "), "\n")

## ---- 3. original v1.3 versus package ----------------------------------------------------------
cat("\n== 3. original v1.3 script versus package (uio_v13_settings()) ==\n")
orig_lines <- readLines(script)
sub_lines  <- gsub("N:/durable/Raw_data/", paste0(raw_dir, "/"), orig_lines, fixed = TRUE)
changed <- which(orig_lines != sub_lines)
cat("for the synthetic run only the raw-data path prefix N:/durable/Raw_data/ is replaced, on script lines:", changed, "\n")
check("only raw-data path lines differ in the synthetic-path copy of the script",
      all(grepl("N:/durable/Raw_data/", orig_lines[changed], fixed = TRUE)) &&
        identical(gsub(paste0(raw_dir, "/"), "N:/durable/Raw_data/", sub_lines, fixed = TRUE), orig_lines))
script_syn <- file.path(tmp, "UiO_preg_Algo_v1.3_synthetic_paths.R"); writeLines(sub_lines, script_syn)

wd_orig <- file.path(tmp, "wd_original"); dir.create(wd_orig)
wd_pkg  <- file.path(tmp, "wd_package");  dir.create(wd_pkg)
empty   <- file.path(tmp, "wd_empty");    dir.create(empty)
run_child("child_run_original.R", list(mode = "run", script = script_syn, workdir = wd_orig, out = file.path(tmp, "orig.rds")), "original_run")
run_child("child_run_package.R", list(lib = lib, raw_dir = raw_dir, workdir = wd_pkg, empty_dir = empty, out = file.path(tmp, "pkg.rds")), "package_runs")
orig <- readRDS(file.path(tmp, "orig.rds")); pk <- readRDS(file.path(tmp, "pkg.rds"))
runs <- lapply(pk$runs, function(x) { x <- copy(x); x })
v13_cols <- c("outcome_final", "ID", "preg_id", "date_outcome", "MC_source", "TOP_source", "pr_start_final", "GA_event_week_final",
              "GA_event_days_final", "dating_method", "truncated_at_previous_outcome", "outcome_lable")
strip <- function(x) {          # the columns of v1.3, in one order (2.0.0 adds the description of each pregnancy)
  x <- copy(x)[, intersect(v13_cols, names(x)), with = FALSE]
  for (a in c("settings", "code_availability", "uio_capture")) setattr(x, a, NULL)
  setkey(x, NULL); setorder(x, outcome_final, ID, date_outcome, preg_id); x
}
same_dt <- function(a, b) {
  a <- strip(a); b <- strip(b)
  c(all_equal       = isTRUE(all.equal(a, b, check.attributes = TRUE)),
    identical_cols  = identical(as.list(a), as.list(b)),
    identical_class = identical(lapply(a, class), lapply(b, class)),
    identical_key   = identical(key(a), key(b)),
    same_after_ordering = isTRUE(all.equal(setorder(copy(a), ID, date_outcome, preg_id), setorder(copy(b), ID, date_outcome, preg_id))))
}
cat("original v1.3 result:", nrow(orig), "rows; columns:", paste(names(orig), collapse = ", "), "\n")
r <- same_dt(orig, runs$b); print(r)
check("original v1.3 == package with uio_v13_settings(): identical data.table", all(r))
check("files cached by the package equal those cached by the original script",
      identical(unname(tools::md5sum(sort(list.files(wd_orig, full.names = TRUE)))), unname(pk$cache_before)) &&
        identical(sort(list.files(wd_orig)), basename(names(pk$cache_before))))
cat("cached files:", basename(names(pk$cache_before)), "\n")

## ---- 4. cache / working directory ----------------------------------------------------------
cat("\n== 4. cache and working directory ==\n")
check("rerun from the cached CSV files == first run", all(same_dt(runs$b, runs$b2)))
check("cached CSV files unchanged by runs with other settings / normalize_codes = TRUE", identical(pk$cache_before, pk$cache_after))
check("working directory restored after an error inside run_uio_pa()", pk$wd_restored_after_error)
cat("  (error was: ", pk$error_message, ")\n", sep = "")
cat("  unknown setting gives: ", pk$bad_setting_message, "\n", sep = "")
check("attr(result, 'settings') holds the full settings",
      identical(attr(runs$c, "settings")$stillbirth_min_days, 154) && identical(attr(runs$c, "settings")$induced_abortion_default_days, 49) &&
        identical(names(attr(runs$c, "settings")), names(uio_default_settings())))

## ---- 5. effect of the settings --------------------------------------------------------------
cat("\n== 5. effect of the settings ==\n")
summ <- function(x) {
  x <- strip(x)
  x[, .(ID, date_outcome = as.character(date_outcome), outcome = outcome_final,
        GA_days = GA_event_days_final, dating_method)][order(ID, date_outcome)]
}
S <- lapply(c(list(original = orig), runs), summ)
cat("original v1.3 = package (b), per pregnancy:\n"); print(S$original)

expected_b <- data.table(
  ID           = c(1L, 2L, 3L, 4L, 7L, 8L, 9L, 10L, 11L, 13L, 14L, 15L, 16L, 17L),
  date_outcome = c("2015-06-15", "2016-03-10", "2017-04-20", "2018-02-14", "2020-01-15", "2021-03-03", "2018-09-09",
                   "2014-11-11", "2012-01-10", "2013-05-05", "2011-07-07", "2016-10-10", "2022-02-02", "2019-12-12"),
  outcome      = c(1, 4, 2, 3, 3, 2, 2, 1, 1, 2, 2, 6, 5, 1),
  GA_days      = c(280, 150, 84, 56, 80, 72, 70, 259, 275, 70, 120, 56, 77, 230),
  dating_method = c("MBRN (recorded GA)", "MBRN (recorded GA)", "fixed 84 d (marker >= 12 wk rejected)",
                    "fixed outcome-specific default", "fixed 80 d (TOP with O35)", "pregnancy marker",
                    "fixed outcome-specific default", "MBRN (recorded GA)", "MBRN (recorded GA)",
                    "fixed outcome-specific default", "MBRN (recorded GA)", "fixed outcome-specific default",
                    "fixed outcome-specific default", "MBRN (recorded GA)"))
setorder(expected_b, ID, date_outcome)
cmp_expected <- function(obs, exp, label) {
  o <- copy(obs)[, outcome := as.numeric(outcome)][, GA_days := as.numeric(GA_days)]
  ok <- isTRUE(all.equal(o, exp, check.attributes = FALSE))
  check(label, ok)
  if (!ok) { cat("observed:\n"); print(o); cat("expected:\n"); print(exp) }
}
cmp_expected(S$b, expected_b, "v1.3 results equal the hand-derived expectations for every synthetic woman")

same <- function(a, b) (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)
changed_rows <- function(base, new, tag) {   # pregnancies that differ between two runs
  sfx <- paste0(".", tag)
  m <- merge(base, new, by = c("ID", "date_outcome"), all = TRUE, suffixes = c(".v13", sfx))
  keep <- !(same(m[["outcome.v13"]], m[[paste0("outcome", sfx)]]) &
              same(m[["GA_days.v13"]], m[[paste0("GA_days", sfx)]]) &
              same(m[["dating_method.v13"]], m[[paste0("dating_method", sfx)]]))
  m[keep]
}
modify <- function(base, rows) {        # rows: data.table with ID, date_outcome, outcome, GA_days, dating_method
  out <- rbind(base[!rows, on = .(ID, date_outcome)], rows)
  setorder(out, ID, date_outcome)[]
}
exp_c <- modify(expected_b, data.table(
  ID = c(2L, 4L, 5L, 6L, 9L),
  date_outcome = c("2016-03-10", "2018-02-14", "2019-05-05", "2019-08-08", "2018-09-09"),
  outcome = c(2, 3, 2, 2, 2), GA_days = c(150, 49, 70, 70, 74),
  dating_method = c("MBRN (recorded GA)", "fixed outcome-specific default", "fixed outcome-specific default",
                    "fixed outcome-specific default", "pregnancy marker")))
cat("\n(c) v1.3 settings with stillbirth_min_days = 154, induced_abortion_default_days = 49, normalize_codes = TRUE\n")
cat("    expected changes vs v1.3: W2 150-day fetal death stillbirth(4) -> miscarriage(2); W4 abortion without\n",
    "   marker 56 -> 49 d; W5 'w82' and W6 'W 82' now found (KUHR miscarriages, 70 d); W9 dotted marker 'Z32.1'\n",
    "   now used (70 d default -> 74 d marker dating). Everything else unchanged.\n")
print(changed_rows(S$b, S$c, "c"))
cmp_expected(S$c, exp_c, "(c) differs from v1.3 exactly as expected")

exp_d <- modify(expected_b, data.table(
  ID = c(3L, 7L, 9L, 13L),
  date_outcome = c("2017-04-20", "2020-01-15", "2018-09-09", "2013-05-05"),
  outcome = c(2, 3, 2, 2), GA_days = c(84, 90, 63, 63),
  dating_method = c("pregnancy marker", "fixed 90 d (TOP with O35)", "fixed outcome-specific default", "fixed outcome-specific default")))
cat("\n(d) v1.3 settings with marker_rejection_days = 91, o35_induced_days = 90, miscarriage_default_days = 63\n")
cat("    expected changes vs v1.3: W3 marker-implied 84 d now accepted (< 91); W7 O35 abortion 80 -> 90 d;\n",
    "   W9 and W13 miscarriages without usable marker 70 -> 63 d.\n")
print(changed_rows(S$b, S$d, "d"))
cmp_expected(S$d, exp_d, "(d) differs from v1.3 exactly as expected")

exp_e <- modify(expected_b, data.table(
  ID = c(2L, 3L, 4L, 5L, 6L, 9L), date_outcome = c("2016-03-10", "2017-04-20", "2018-02-14", "2019-05-05", "2019-08-08", "2018-09-09"),
  outcome = c(2, 2, 3, 2, 2, 2), GA_days = c(150, 84, 49, 70, 70, 74),
  dating_method = c("MBRN (recorded GA)", "pregnancy marker", "fixed outcome-specific default", "fixed outcome-specific default", "fixed outcome-specific default", "pregnancy marker")))
cat("\n(e) all defaults of version 2.0.0 (stillbirth from 154 days, marker rule 119 days, induced-abortion default 49 days, normalize_codes = TRUE)\n")
cat("    expected changes vs v1.3: W2 150-day fetal death -> miscarriage; W3 marker-implied 84 d accepted (< 119);\n",
    "   W4 abortion without marker 49 d; W5, W6, W9 through the normalised codes.\n")
print(changed_rows(S$b, S$e, "e"))
cmp_expected(S$e, exp_e, "(e) the 2.0.0 defaults differ from v1.3 exactly as expected")

## ---- 6. data without quadruplets (v1.3 lines 481-482 are executed) -----------------------------
cat("\n== 6. synthetic data WITHOUT the quadruplet pregnancy (W17) ==\n")
cat("Here the dcast in BLOCK 2 does not create DAGER_FDATO3/4 and SVLEN_DG3/4, so v1.3 lines 481-482\n",
    "(`MBRN[, var_1:=NA_Date_, with = FALSE]`) run; the package uses `MBRN[, (var_1):=NA_Date_]` instead.\n")
raw_dir_nq <- file.path(tmp, "Raw_data_noquads"); make_uio_synthetic_data(raw_dir_nq, with_quadruplets = FALSE)
wd_pkg_nq <- file.path(tmp, "wd_package_noquads"); dir.create(wd_pkg_nq)
wd_orig_nq <- file.path(tmp, "wd_original_noquads"); dir.create(wd_orig_nq)
run_child("child_run_package.R", list(lib = lib, raw_dir = raw_dir_nq, workdir = wd_pkg_nq, single = TRUE,
                                      out = file.path(tmp, "pkg_nq.rds")), "package_run_noquads")
pkg_nq <- readRDS(file.path(tmp, "pkg_nq.rds"))
cols_np <- setdiff(v13_cols, "preg_id")          # preg_id numbering depends on the rows present
a <- setorder(strip(runs$b)[ID != 17L, ..cols_np], ID, date_outcome)
b <- setorder(strip(pkg_nq)[, ..cols_np], ID, date_outcome)
check("package runs without quadruplets; results equal those of the data with quadruplets (W17 and preg_id aside)",
      isTRUE(all.equal(a, b, check.attributes = FALSE)))
sub_nq <- gsub("N:/durable/Raw_data/", paste0(raw_dir_nq, "/"), orig_lines, fixed = TRUE)
script_syn_nq <- file.path(tmp, "UiO_preg_Algo_v1.3_synthetic_paths_noquads.R"); writeLines(sub_nq, script_syn_nq)
o_nq <- run_child("child_run_original.R", list(mode = "run", script = script_syn_nq, workdir = wd_orig_nq,
                                               out = file.path(tmp, "orig_nq.rds")), "original_run_noquads", allow_fail = TRUE)
if (attr(o_nq, "status") == 0L) {
  orig_nq <- readRDS(file.path(tmp, "orig_nq.rds"))
  check("original v1.3 also runs on these data (data.table < 1.17); identical to the package", all(same_dt(orig_nq, pkg_nq)))
} else {
  msg <- grep("with=FALSE together with :=", o_nq, value = TRUE, fixed = TRUE)
  cat("original v1.3 FAILS on these data with data.table", format(packageVersion("data.table")), ":\n  ", msg[1], "\n")
  check("the original's failure is the with=FALSE + := error that the package fixes (data.table >= 1.17)",
        length(msg) > 0 && packageVersion("data.table") >= "1.17.0")
}

## ---- 7. new settings of version 1.4.0 ------------------------------------------------------------
cat("\n== 7. version 1.4.0 settings (all with normalize_codes = FALSE) ==\n")
check("the birth-weight run adds VEKT to the cached MBRN file", "VEKT" %in% pk$mbrn_cols_after_f)
cat("(f) stillbirth_min_days = 154, stillbirth_min_weight_g = 500 (rule \"or\"): W2 (150 days, 560 g) stays a stillbirth;\n",
    "   W14 (120 days, 300 g) stays a miscarriage; so (f) equals v1.3.\n")
cmp_expected(S$f, expected_b, "(f) equals the v1.3 expectations (W2 stillbirth by weight, although below 154 days)")
exp_g <- modify(expected_b, data.table(ID = 2L, date_outcome = "2016-03-10", outcome = 2, GA_days = 150, dating_method = "MBRN (recorded GA)"))
cat("(g) stillbirth_min_weight_g = 600, rule \"weight_first\": W2 (560 g) becomes a miscarriage although 21 completed weeks.\n")
print(changed_rows(S$b, S$g, "g"))
cmp_expected(S$g, exp_g, "(g) differs from v1.3 exactly as expected")
exp_h <- modify(expected_b, data.table(ID = 4L, date_outcome = "2018-02-14", outcome = 3, GA_days = 47, dating_method = "fixed outcome-specific default"))
cat("(h) induced_abortion_default_days = table 2015: 47, 2019: 45 -> W4 (2018) gets 47 days (closest earlier year).\n")
print(changed_rows(S$b, S$h, "h"))
cmp_expected(S$h, exp_h, "(h) differs from v1.3 exactly as expected")
w4 <- S$i[ID == 4L]
cat("(i) drawn default (42-55 days p = 0.8, 56-69 days p = 0.2, seed 7): W4 gets", w4$GA_days, "days\n")
check("(i) only W4 changes; its length is within 42-69 days and labelled as drawn",
      nrow(changed_rows(S$b, S$i, "i")) == 1L && w4$GA_days >= 42 && w4$GA_days <= 69 &&
        w4$dating_method == "fixed outcome-specific default (drawn from distribution)")
check("(i) repeated with the same seed gives the identical result", all(same_dt(runs$i, runs$i2)))
check("(i) the caller's random-number state is restored", isTRUE(pk$rng_restored))
d <- pk$draws
cat(sprintf("    20,000 draws: %.1f%% in 42-55 days (expected 80%%), range %d-%d, all whole numbers: %s\n",
            100 * mean(d <= 55), min(d), max(d), all(d == round(d))))
check("draws follow the distribution (share in 42-55 days within 79-81%; range 42-69; whole numbers)",
      abs(mean(d <= 55) - 0.8) < 0.01 && min(d) == 42 && max(d) == 69 && all(d == round(d)))
cat("  invalid distribution gives: ", pk$bad_dist_message, "\n", sep = "")
check("(j) v1.3 settings after the cache gained the weight column == first run", all(same_dt(runs$b, runs$j)))

## ---- 8. version 2.0.0 ---------------------------------------------------------------------------
cat("\n== 8. version 2.0.0 ==\n")
check("run_uio_pa() argument defaults equal uio_default_settings()", identical(pk$formals_defaults, uio_default_settings()))
check("settings given as arguments == the same settings in a list ((k) vs (c))", all(same_dt(runs$c, runs$k)))
cat("  argument and list together give: ", pk$both_message, "\n", sep = "")
check("a setting given both as an argument and in `settings` is an error", grepl("both as an argument", pk$both_message, fixed = TRUE))
exp_m <- modify(expected_b, data.table(ID = 2L, date_outcome = "2016-03-10", outcome = 2, GA_days = 150, dating_method = "MBRN (recorded GA)"))
cat("(m) stillbirth_min_days = 154, stillbirth_min_weight_g = 500, rule \"and\": W2 (150 days, 560 g) becomes a miscarriage.\n")
print(changed_rows(S$b, S$m, "m"))
cmp_expected(S$m, exp_m, "(m) differs from v1.3 exactly as expected")
check("(n0) same_event_days = 56, marker_lookback_days = 70, o35_window_days = 27 given explicitly == v1.3", all(same_dt(runs$b, runs$n0)))
cat("(n) same_event_days = 30, marker_lookback_days = 40, o35_window_days = 5: changed pregnancies vs v1.3\n")
print(changed_rows(S$b, S$n, "n"))
tt <- pk$term_table
cat("  uio_termination_days_by_year():\n"); print(tt)
check("registry table 2015-2024: days 47, 47, 46, 46, 45, 44, 44, 44, 45, 45, marked for extrapolation",
      identical(tt$year, 2015:2024) && identical(tt$days, c(47, 47, 46, 46, 45, 44, 44, 44, 45, 45)) && isTRUE(attr(tt, "extrapolate")))
exp_p <- modify(expected_b, data.table(ID = 4L, date_outcome = "2018-02-14", outcome = 3, GA_days = 46, dating_method = "fixed outcome-specific default"))
cat("(p) induced_abortion_default_days = uio_termination_days_by_year(): W4 (2018) gets 46 days.\n")
cmp_expected(S$p, exp_p, "(p) differs from v1.3 exactly as expected")
cat("  extrapolated 2008, 2014, 2018 (listed), 2025, 2040:", pk$extrap_days, "\n")
check("years outside 2015-2024 follow the trend, kept within 44-47 days (47, 47, 46, 44, 44)", identical(pk$extrap_days, c(47, 47, 46, 44, 44)))
check("a table without the attribute is not extrapolated (closest earlier year; first year before)", identical(pk$own_table_days, c(47, 45)))
exp_q <- modify(expected_b, data.table(ID = 4L, date_outcome = "2018-02-14", outcome = 3, GA_days = 44, dating_method = "fixed outcome-specific default"))
cat("(q) table 2019-2024 only: W4 (2018) is extrapolated (44 days). Warning:\n ", pk$warn_q, "\n")
cmp_expected(S$q, exp_q, "(q) the extrapolated year gets the trend value")
check("(q) the run warns that 2018 is extrapolated and how to return to the fixed default",
      length(pk$warn_q) == 1L && grepl("2018: 44 days", pk$warn_q, fixed = TRUE) && grepl("EXTRAPOLATED", pk$warn_q, fixed = TRUE) &&
        grepl("leave out `induced_abortion_default_days` (49 days)", pk$warn_q, fixed = TRUE))
check("(n) the changed values are stored in the settings",
      identical(attr(runs$n, "settings")[c("same_event_days", "marker_lookback_days", "o35_window_days")],
                list(same_event_days = 30, marker_lookback_days = 40, o35_window_days = 5)))

## ---- 9. source layouts of the projects and date forms ---------------------------------------------
cat("\n== 9. the same data in the layouts of P704, P1380 and a generic layout (uio_source_files(), uio_dates()) ==\n")
run_child("child_run_layouts.R", list(lib = lib, raw_dir = raw_dir, tmp = tmp, out = file.path(tmp, "layouts.rds")), "package_layouts")
lay <- readRDS(file.path(tmp, "layouts.rds"))
key_cols <- function(x) {
  x <- strip(x)
  x[, .(ID = as.integer(ID), date_outcome = as.Date(date_outcome), outcome_final, MC_source, TOP_source,
        pr_start_final = as.Date(pr_start_final), GA_event_days_final, dating_method, truncated_at_previous_outcome)][order(ID, date_outcome)]
}
check("P1111 layout via uio_project_sources(\"p1111\") == run (b)", isTRUE(all.equal(key_cols(lay$res$p1111), key_cols(runs$b), check.attributes = FALSE)))
for (lt in c("p704", "p1380", "generic", "new_mbrn")) {
  ok <- isTRUE(all.equal(key_cols(lay$res[[lt]]), key_cols(lay$res$p1111), check.attributes = FALSE))
  check(sprintf("%s layout gives the same pregnancies, outcomes, dates and dating methods as the P1111 layout", lt), ok)
  if (!ok) { cat("observed:\n"); print(key_cols(lay$res[[lt]])); cat("P1111:\n"); print(key_cols(lay$res$p1111)) }
}
cat("  P704: day offsets from 1997-09-24, NPR with tilstand_1..n, KUHR without sex\n",
    " P1380: MBRN SPSS with text dates (mdy HMS), NPR contact + code files, KUHR .dsv with yyyymmdd\n",
    " generic: MBRN dates dd.mm.yyyy, NPR and KUHR with offsets from a per-woman reference date, sex coded K/M\n")
cat("  new_mbrn: id_mor, fdato, plurek, svlen_dg, leve_dodstatus_fodsel (1/2/3) instead of DODKAT, mors_alder\n")
kw <- key_cols(lay$res$new_mbrn_weeks); k1 <- key_cols(lay$res$p1111)
same_but_ga <- isTRUE(all.equal(kw[, .(ID, date_outcome, outcome_final, dating_method)], k1[, .(ID, date_outcome, outcome_final, dating_method)], check.attributes = FALSE))
ga_mbrn <- merge(kw[dating_method == "MBRN (recorded GA)", .(ID, date_outcome, gw = GA_event_days_final)],
                 k1[dating_method == "MBRN (recorded GA)", .(ID, date_outcome, gd = GA_event_days_final)], by = c("ID", "date_outcome"))
check("svlen (weeks): same pregnancies and outcomes; MBRN gestational age = completed weeks x 7 + 3 days",
      same_but_ga && nrow(ga_mbrn) > 0 && all(ga_mbrn$gw == (ga_mbrn$gd %/% 7) * 7 + 3))
cat("  a missing column gives: ", lay$missing_cols, "\n", sep = "")
check("uio_source_files() reports missing required columns", grepl("lacks the column", lay$missing_cols, fixed = TRUE))

## ---- 10. description of the pregnancies, summary, output folder ------------------------------------------
cat("\n== 10. description of the pregnancies, printed summary and output folder (run (o), defaults) ==\n")
o_ <- runs$o
need <- c("year_outcome", "dating_class", "dating_source", "dating_code", "outcome_source", "outcome_code",
          "codes_mbrn", "codes_npr", "codes_kuhr", "mother_age")
check("the result has the description columns", all(need %in% names(o_)))
print(o_[, .(ID, date_outcome, outcome_lable, GA = GA_event_days_final, dating_class, dating_source, dating_code,
             outcome_source, outcome_code, codes_npr, codes_kuhr, mother_age)])
w <- function(id, d) o_[ID == id & as.character(date_outcome) == d]
check("W2 (MBRN fetal death, 150 days): source MBRN, code DODKAT 7", w(2L, "2016-03-10")$outcome_source == "MBRN" && w(2L, "2016-03-10")$outcome_code == "DODKAT 7")
check("W4 (NPR O049, no marker): source NPR, code O049, dating default", w(4L, "2018-02-14")$outcome_code == "O049" && w(4L, "2018-02-14")$dating_source == "default")
check("W7 (O049 with O359): dating source NPR, code O359; episode codes O049, O359",
      w(7L, "2020-01-15")$dating_code == "O359" && w(7L, "2020-01-15")$codes_npr == "O049, O359")
check("W8 (KUHR W82, marker W78 15 days before): source KUHR, code W82; dated by KUHR marker W78",
      w(8L, "2021-03-03")$outcome_source == "KUHR" && w(8L, "2021-03-03")$outcome_code == "W82" &&
        w(8L, "2021-03-03")$dating_source == "KUHR" && w(8L, "2021-03-03")$dating_code == "W78")
check("W9 (O03.9 with Z32.1, normalised): dated by NPR marker Z321", w(9L, "2018-09-09")$dating_code == "Z321" && w(9L, "2018-09-09")$dating_source == "NPR")
check("MBRN pregnancies have the mother's age from the MBRN", all(!is.na(o_[outcome_source == "MBRN", mother_age])))
check("output folder: rds, outcome and code-availability csv, html report", all(c("uio_pregnancies.rds", "uio_outcomes.csv", "uio_code_availability.csv", "uio_report.html") %in% pk$out_files))
check("the saved rds equals the returned result", isTRUE(pk$out_rds_equal))
check("outcome table has all six outcomes (also those with no pregnancies)", nrow(pk$outcomes_csv) == 6 && "Molar pregnancy" %in% pk$outcomes_csv$outcome)
check("counts 1-4 are masked in the csv files", any(pk$outcomes_csv$n == "<5") && !any(grepl("^[1-4]$", c(pk$outcomes_csv$n, pk$avail_csv$n_records))))
check("code availability lists the 15 NPR and 21 KUHR markers and the outcome codes",
      sum(pk$avail_csv$type == "marker" & pk$avail_csv$registry == "NPR") == 15 && sum(pk$avail_csv$type == "marker" & pk$avail_csv$registry == "KUHR") == 21 &&
        all(c("O00", "O01", "O04", "W82") %in% pk$avail_csv$code))
check("HTML report has the sections and inline SVG charts, no external resources",
      all(sapply(c('id="year"', 'id="age"', 'id="ga"', 'id="dating"', 'id="codes"', "<svg"), function(z) any(grepl(z, pk$html, fixed = TRUE)))) &&
        !any(grepl("https?://(?!doi)", pk$html, perl = TRUE)))
cat("printed summary:\n"); cat(pk$summary_text, sep = "\n")
check("printed summary: code availability, then settings, then the outcome table with all six outcomes",
      {i1 <- grep("Codes available", pk$summary_text); i2 <- grep("== Settings", pk$summary_text); i3 <- grep("Pregnancy outcomes", pk$summary_text)
       length(i1) && length(i2) && length(i3) && i1 < i2 && i2 < i3 && all(sapply(c("Livebirth", "Molar pregnancy", "Ectopic pregnancy"), function(z) any(grepl(z, pk$summary_text[i3:length(pk$summary_text)]))))})

saveRDS(list(original = orig, package = runs, package_noquads = pkg_nq), file.path(results_dir, "regression_results.rds"))
cat("\nOVERALL:", if (ok_all) "ALL CHECKS PASSED" else "SOME CHECKS FAILED", "\n")
sink(); close(log_con)
if (!ok_all) quit(status = 1)
