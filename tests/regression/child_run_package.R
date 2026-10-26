## Child process: run the PACKAGE (only the package is attached; data.table, lubridate, stringr and
## Rcpp are NOT attached, to show that the package does not depend on the search path).
## Argument: path to an .rds with list(lib, raw_dir, workdir, empty_dir, out)
p <- readRDS(commandArgs(TRUE)[1])
library(UiOPregnancyAlgorithm, lib.loc = p$lib)
cat("package version:", format(packageVersion("UiOPregnancyAlgorithm", lib.loc = p$lib)), "\n")
cat("search path:", search(), "\n")

src <- uio_project_sources("p1111", raw_dir = p$raw_dir)
owd <- getwd()
rp <- function(...) run_uio_pa(..., output_dir = NULL, verbose = FALSE)   # no output folder (it would be in the cache folder)
v13 <- uio_v13_settings()
if (isTRUE(p$single)) {     # one run with the v1.3 settings only
  res <- rp(workdir = p$workdir, source_files = src, settings = v13,
                    years_range = c(2008, 2023))
  saveRDS(res, p$out)
  quit(save = "no")
}
runs <- list()
md5 <- function() tools::md5sum(sort(list.files(p$workdir, full.names = TRUE)))

t0 <- Sys.time()
# (b) v1.3 settings (uio_v13_settings()); fresh workdir -> reads the raw extracts
runs$b <- rp(workdir = p$workdir, source_files = src,
                     settings = v13, years_range = c(2008, 2023))
cat("\npackage run (b) time:", format(Sys.time() - t0), "\n")
stopifnot(identical(getwd(), owd))
cache_before <- md5()

# (b2) same settings again: now from the cached CSV files in workdir
runs$b2 <- rp(workdir = p$workdir, source_files = src,
                      settings = v13, years_range = c(2008, 2023))
# (c) changed settings (from v1.3)
runs$c <- rp(workdir = p$workdir, source_files = src,
                     settings = modifyList(v13, list(stillbirth_min_days = 154, induced_abortion_default_days = 49,
                                                     normalize_codes = TRUE)),
                     years_range = c(2008, 2023))
# (d) the other settings (from v1.3)
runs$d <- rp(workdir = p$workdir, source_files = src,
                     settings = modifyList(v13, list(marker_rejection_days = 91, o35_induced_days = 90,
                                                     miscarriage_default_days = 63)),
                     years_range = c(2008, 2023))
# (k) [v2.0.0] the same settings as (c), given as arguments instead of a list
runs$k <- rp(workdir = p$workdir, source_files = src, years_range = c(2008, 2023),
                     stillbirth_min_days = 154, induced_abortion_default_days = 49, normalize_codes = TRUE,
                     marker_rejection_days = 84, stillbirth_min_weight_g = NULL)
cache_after <- md5()
stopifnot(identical(getwd(), owd))

# [v1.4.0] new settings (after cache_after: the birth-weight runs add the weight column to the MBRN cache)
runs$f <- rp(workdir = p$workdir, source_files = src, years_range = c(2008, 2023),
                     settings = modifyList(v13, list(stillbirth_min_days = 154, stillbirth_min_weight_g = 500, stillbirth_weight_rule = "or")))
mbrn_cols_after_f <- names(data.table::fread(file.path(p$workdir, "birth_registry_full.csv"), nrows = 0))
# (e) all defaults (version 2.0.0: stillbirth from 154 days and 500 g, marker rule 119 days, normalize_codes = TRUE)
runs$e <- rp(workdir = p$workdir, source_files = src, years_range = c(2008, 2023))
# (o) defaults with the output folder and the printed summary
out_dir <- file.path(dirname(p$workdir), "output_o")
runs$o <- run_uio_pa(workdir = p$workdir, source_files = src, years_range = c(2008, 2023), output_dir = out_dir)
out_files <- list.files(out_dir)
out_rds_equal <- isTRUE(all.equal(readRDS(file.path(out_dir, "uio_pregnancies.rds")), runs$o))
outcomes_csv <- data.table::fread(file.path(out_dir, "uio_outcomes.csv"), colClasses = "character")
avail_csv <- data.table::fread(file.path(out_dir, "uio_code_availability.csv"), colClasses = "character")
html <- readLines(file.path(out_dir, "uio_report.html"), warn = FALSE, encoding = "UTF-8")
summary_text <- capture.output(invisible(run_uio_pa(workdir = p$workdir, source_files = src, years_range = c(2008, 2023), output_dir = NULL)))
runs$g <- rp(workdir = p$workdir, source_files = src, years_range = c(2008, 2023),
                     settings = modifyList(v13, list(stillbirth_min_weight_g = 600, stillbirth_weight_rule = "weight_first")))
# (m) [v2.0.0] rule "and": stillbirth only from 154 days and 500 g
runs$m <- rp(workdir = p$workdir, source_files = src, years_range = c(2008, 2023),
                     settings = modifyList(v13, list(stillbirth_min_days = 154, stillbirth_min_weight_g = 500, stillbirth_weight_rule = "and")))
# (n) [v2.0.0] the fixed values of v1.3 that are now settings, at their v1.3 values and changed
runs$n0 <- rp(workdir = p$workdir, source_files = src, years_range = c(2008, 2023),
                      settings = modifyList(v13, list(same_event_days = 56, marker_lookback_days = 70, o35_window_days = 27)))
runs$n <- rp(workdir = p$workdir, source_files = src, years_range = c(2008, 2023),
                     settings = modifyList(v13, list(same_event_days = 30, marker_lookback_days = 40, o35_window_days = 5)))
runs$h <- rp(workdir = p$workdir, source_files = src, years_range = c(2008, 2023),
                     settings = modifyList(v13, list(induced_abortion_default_days = data.frame(year = c(2015, 2019), days = c(47, 45)))))
dist <- data.frame(days_min = c(42, 56), days_max = c(55, 69), prob = c(0.8, 0.2))
set.seed(99); seed_before <- .Random.seed
runs$i <- rp(workdir = p$workdir, source_files = src, years_range = c(2008, 2023),
                     settings = modifyList(v13, list(induced_abortion_default_distribution = dist, random_seed = 7)))
rng_restored <- identical(.Random.seed, seed_before)
runs$i2 <- rp(workdir = p$workdir, source_files = src, years_range = c(2008, 2023),
                      settings = modifyList(v13, list(induced_abortion_default_distribution = dist, random_seed = 7)))
runs$j <- rp(workdir = p$workdir, source_files = src, settings = v13, years_range = c(2008, 2023))   # v1.3 settings, cache with weight
draws <- UiOPregnancyAlgorithm:::.uio_draw_days(dist, rep(2018, 20000), 7)
bad_dist <- tryCatch(rp(workdir = p$empty_dir, source_files = src, settings = list(induced_abortion_default_distribution = data.frame(days_min = 50, days_max = 40, prob = 1))),
                     error = function(e) conditionMessage(e))

# working directory is restored also when the run fails
err <- tryCatch(rp(workdir = p$empty_dir, source_files = uio_project_sources("p1111", raw_dir = file.path(p$empty_dir, "nope"))),
                error = function(e) conditionMessage(e))
wd_restored_after_error <- identical(getwd(), owd)
# invalid settings are rejected
bad <- tryCatch(rp(workdir = p$empty_dir, source_files = src, settings = list(induced_abortion_days = 49)),
                error = function(e) conditionMessage(e))
# a setting given both as an argument and in `settings` is rejected
both <- tryCatch(rp(workdir = p$empty_dir, source_files = src, induced_abortion_default_days = 49, settings = list(induced_abortion_default_days = 56)),
                 error = function(e) conditionMessage(e))

saveRDS(list(runs = runs, cache_before = cache_before, cache_after = cache_after,
             error_message = err, wd_restored_after_error = wd_restored_after_error,
             bad_setting_message = bad, both_message = both,
             formals_defaults = lapply(formals(run_uio_pa)[names(uio_default_settings())], eval),
             uio_v13_settings = uio_v13_settings(),
             mbrn_cols_after_f = mbrn_cols_after_f, out_files = out_files, out_rds_equal = out_rds_equal,
             outcomes_csv = outcomes_csv, avail_csv = avail_csv, html = html, summary_text = summary_text, rng_restored = rng_restored, draws = draws, bad_dist_message = bad_dist,
             uio_source_files_default = uio_project_sources("p1111"),
             uio_hospital_codes = uio_hospital_codes(),
             uio_primary_care_codes = uio_primary_care_codes(),
             uio_default_settings = uio_default_settings()),
        p$out)
cat("search path at end:", search(), "\n")
