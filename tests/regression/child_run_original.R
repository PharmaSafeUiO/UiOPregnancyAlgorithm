## Child process: run the ORIGINAL script UiO_preg_Algo_v1.3.R the way revised_codes/scripts/run_uio.R does.
## Argument: path to an .rds with list(script, workdir, out, mode)
##   mode = "run"     : sys.source() the script and call detect_pregnancy_episodes()
##   mode = "objects" : sys.source() only (top-level objects); the workdir must hold dummy
##                      npr_mothers_full.csv and hospital_full.csv so that nothing is read
p <- readRDS(commandArgs(TRUE)[1])

suppressPackageStartupMessages(library(data.table))          # as in run_uio.R
owd <- getwd(); uio_env <- new.env()
uio_env$uio_workdir <- p$workdir
if (p$mode == "run") {
  t0 <- Sys.time()
  enb <- tryCatch({
    sys.source(p$script, envir = uio_env)
    uio_env$detect_pregnancy_episodes(uio_env$source_files, uio_env$hospital_codes, uio_env$primary_care_codes,
                                      years_range = uio_env$years_range)
  }, finally = setwd(owd))
  cat("\noriginal v1.3 run time:", format(Sys.time() - t0), "\n")
  saveRDS(enb, p$out)
} else {
  tryCatch(sys.source(p$script, envir = uio_env), finally = setwd(owd))
  saveRDS(list(source_files = uio_env$source_files, hospital_codes = uio_env$hospital_codes,
               primary_care_codes = uio_env$primary_care_codes, years_range = uio_env$years_range),
          p$out)
}
cat("search path:", search(), "\n")
