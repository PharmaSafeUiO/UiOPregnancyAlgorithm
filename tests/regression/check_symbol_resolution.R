## Child process: record where every free symbol of the algorithm resolves.
## Argument: path to an .rds with list(mode, script, lib, out)
##   mode = "script" : session set up as run_uio.R + the script's library() calls; the original
##                     detect_pregnancy_episodes() and the NPR preparation block are taken from the
##                     script and resolved from the global environment (their enclosure)
##   mode = "package": only the package namespace is loaded (nothing attached); the package's
##                     detect_pregnancy_episodes() and uio_prepare_sources() are resolved from it
## For each symbol: the package whose namespace holds the identical object.
p <- readRDS(commandArgs(TRUE)[1])
cands <- c("stringr", "lubridate", "data.table", "Rcpp", "stats", "graphics", "grDevices", "utils",
           "datasets", "methods", "base")

if (p$mode == "script") {
  suppressPackageStartupMessages({
    library(data.table)                                                  # run_uio.R
    library(Rcpp); library(data.table); library(lubridate); library(stringr)   # script lines 2-5
  })
  ex <- parse(p$script, keep.source = FALSE)
  i_fun <- which(vapply(ex, function(e) is.call(e) && identical(e[[1]], as.name("=")) &&
                          identical(e[[2]], as.name("detect_pregnancy_episodes")), TRUE))
  i_prep <- which(vapply(ex, function(e) is.call(e) && identical(e[[1]], as.name("if")) &&
                           grepl("npr_mothers_full.csv", paste(deparse(e[[2]]), collapse = ""), fixed = TRUE), TRUE))
  stopifnot(length(i_fun) == 1, length(i_prep) == 1)
  e <- new.env(); eval(ex[[i_fun]], e)
  f_detect <- e$detect_pregnancy_episodes
  f_prep <- function() NULL; body(f_prep) <- ex[[i_prep]]
  where <- globalenv()
} else {
  ns <- loadNamespace("UiOPregnancyAlgorithm", lib.loc = p$lib)
  f_detect <- get("detect_pregnancy_episodes", envir = ns)
  f_prep <- get("uio_prepare_sources", envir = ns)
  where <- ns
  cands <- c("UiOPregnancyAlgorithm", cands)
}

origin_of <- function(s) {
  if (!exists(s, envir = where)) return(NA_character_)              # column names (data.table NSE)
  obj <- get(s, envir = where)
  for (pk in cands) {
    nse <- asNamespace(pk)
    if (exists(s, envir = nse, inherits = FALSE) && identical(get(s, envir = nse, inherits = FALSE), obj)) return(pk)
  }
  "other"
}
globs <- function(f) { g <- codetools::findGlobals(f, merge = FALSE); sort(unique(c(g$functions, g$variables))) }
# explicitly qualified references pkg::name (e.g. Rcpp::sourceCpp), restricted to installed R packages
# (this skips C++ names such as std::string inside the sourceCpp code strings)
qualified <- function(f) {
  txt <- paste(deparse(f), collapse = "\n")
  m <- regmatches(txt, gregexpr("[A-Za-z][A-Za-z0-9.]*:::?[A-Za-z._][A-Za-z0-9._]*", txt))[[1]]
  pk <- sub(":::?.*$", "", m); nm <- sub("^.*:::?", "", m)
  keep <- pk %in% rownames(installed.packages()) & !duplicated(nm)
  setNames(pk[keep], nm[keep])
}
res <- list(detect = globs(f_detect), prepare = globs(f_prep))
res <- lapply(res, function(s) vapply(s, origin_of, ""))
saveRDS(list(origins = res, qualified = list(detect = qualified(f_detect), prepare = qualified(f_prep)),
             search = search()), p$out)
cat("mode:", p$mode, " search path:", search(), "\n")
