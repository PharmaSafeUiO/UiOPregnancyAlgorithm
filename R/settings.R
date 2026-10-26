#' Settings of the UiO pregnancy algorithm
#'
#' `uio_default_settings()` returns the named list of settings of [run_uio_pa()] with their
#' default values. The defaults are the settings of the main analysis of the four-algorithm
#' comparison (Nordeng, Hayati and van Gelder). `uio_v13_settings()` returns the settings that
#' differ from these defaults in version 1.3 of the algorithm (the R translation of the STATA
#' script): `run_uio_pa(..., settings = uio_v13_settings())` runs the algorithm with the rules of
#' version 1.3.
#'
#' Each setting is also an argument of [run_uio_pa()] with the same name; see there for how
#' they are combined.
#'
#' @section Time conventions:
#' All lengths are whole days. The pregnancy start date is **day 0** and the outcome date is
#' day `GA`, where `GA = outcome date - start date` (`GA_event_days_final`); for example, a
#' pregnancy that starts on 1 January and ends on 8 January has `GA = 7` (1+0 weeks).
#' Completed weeks are `floor(GA / 7)` (`GA_event_week_final`): days 0-6 are week 0, and day 154
#' is 22+0 weeks. The Norwegian registries' statistics count weeks ordinally (week `n` = days `7(n-1)` to
#' `7n-1`), so "before week 9" in the Registry of Pregnancy Termination means `GA <= 55` days.
#' "Within `d` days before the outcome" means `0 <= outcome date - record date <= d`.
#'
#' @section Settings:
#' \describe{
#'   \item{`stillbirth_min_days`}{Gestational age (days) from which an MBRN fetal death
#'     (`DODKAT` 7-9) is a stillbirth; below it, it is a miscarriage. Default `154` (22+0 weeks,
#'     the MBRN definition). `NULL` gives the rule of version 1.3: stillbirth if the completed
#'     weeks `floor(SVLEN_DG / 7) > 20`, i.e. from 147 days (21+0 weeks).}
#'   \item{`stillbirth_min_weight_g`}{`NULL` (default: birth weight not used) or a birth weight
#'     in grams, usually `500`. The MBRN birth weight (`birth_weight` in [uio_source_files()],
#'     column `VEKT`) of a fetal death then also decides between stillbirth and miscarriage, as set
#'     by `stillbirth_weight_rule`. Weights of 0 or below count as missing. MBRN records without
#'     gestational age are dropped whatever the weight, as in version 1.3.}
#'   \item{`stillbirth_weight_rule`}{How birth weight and gestational age are combined when
#'     `stillbirth_min_weight_g` is set:
#'     `"or"` (default): stillbirth if the gestational-age rule says stillbirth **or** the weight is
#'     at least `stillbirth_min_weight_g` (22 weeks or 500 g, the definition of the Norwegian
#'     Institute of Public Health's statistics);
#'     `"and"`: stillbirth only if the gestational-age rule says stillbirth **and** the weight is at
#'     least the limit, the gestational-age rule alone when the weight is missing (a fetus born
#'     before 22 weeks, or below 500 g, is a miscarriage: the MBRN notification guide);
#'     `"weight_first"`: the weight decides when it is recorded, the gestational-age rule when it is
#'     missing.}
#'   \item{`marker_rejection_days`}{The marker rule for pregnancies that are not dated by the
#'     MBRN. A pregnancy marker is used only if it is recorded within this many days before the
#'     outcome, and a marker that implies a pregnancy length (outcome date - marker date + the
#'     marker's days) of this many days or more is rejected; the next marker in priority order is
#'     then tried. If markers were found but all were rejected, the pregnancy gets this length.
#'     Default `119` (17+0 weeks): estimates of 17 weeks or more are not plausible for an early
#'     non-live outcome (Nordeng et al. 2024). Version 1.3 used `84` (12 weeks).
#'     Markers are also limited by `marker_lookback_days`.}
#'   \item{`marker_lookback_days`}{Pregnancy markers of a pregnancy without MBRN gestational age
#'     are collected from this many days before the outcome up to the outcome date. Default `70`,
#'     as in version 1.3. With the defaults the look-back is the binding limit (70 < 119).}
#'   \item{`same_event_days`}{Records of a woman less than this many days apart belong to the same
#'     event. Used to merge repeated NPR and KUHR records of one miscarriage or termination (a
#'     record within `same_event_days - 1` days of the first record of an episode joins the
#'     episode), and to drop NPR and KUHR non-live outcomes recorded within this many days after
#'     an MBRN birth or before or after another non-live outcome. Default `56` (8 weeks), as in
#'     version 1.3.}
#'   \item{`induced_abortion_default_days`}{Pregnancy length of an induced abortion (outcome 3)
#'     without any dating information (no MBRN gestational age, no accepted marker, no rejected
#'     marker, no O35 code). Default `56` (8+0 weeks). Either one number for all years, or a data
#'     frame with columns `year` and `days` (one row per year of the outcome) for year-specific
#'     defaults: a year that is not listed takes the value of the closest earlier listed year, and
#'     years before the first listed year take the first value.}
#'   \item{`miscarriage_default_days`}{Same for a miscarriage (outcome 2). Default `70` (10+0 weeks).}
#'   \item{`molar_default_days`}{Same for a molar pregnancy (outcome 5). Default `77` (11+0 weeks).}
#'   \item{`ectopic_default_days`}{Same for an ectopic pregnancy (outcome 6). Default `56` (8+0 weeks).}
#'   \item{`o35_induced_days`}{Pregnancy length of an induced abortion without MBRN gestational age
#'     that has an O35 code (maternal care for known or suspected fetal abnormality) in NPR or KUHR
#'     recorded from the abortion date up to `o35_window_days` days after it. Default `80`. It
#'     replaces a marker-based start.}
#'   \item{`o35_window_days`}{See `o35_induced_days`. Default `27` (O35 recorded 0-27 days after the
#'     abortion), as in version 1.3.}
#'   \item{`induced_abortion_default_distribution`}{`NULL` (default) or a data frame with columns
#'     `days_min`, `days_max` and `prob`, and optionally `year`. If given, it replaces
#'     `induced_abortion_default_days`: each induced abortion without dating information gets a
#'     pregnancy length drawn from it (a row with probability `prob`, then a whole number of days
#'     uniformly from `days_min` to `days_max`), so that one fixed value does not create a spike.
#'     With a `year` column the rows of the outcome's year are used (a year that is not listed takes
#'     the closest earlier listed year); `prob` is rescaled to sum to 1 within each year. These
#'     pregnancies have `dating_method` `"fixed outcome-specific default (drawn from distribution)"`.}
#'   \item{`random_seed`}{Seed of the draws from `induced_abortion_default_distribution`, so that a
#'     run can be repeated exactly. The caller's random-number state is restored afterwards.
#'     Default `1`.}
#'   \item{`normalize_codes`}{If `TRUE` (default), the NPR (`tilst`) and KUHR (`diagnose`) diagnosis
#'     codes are normalised in memory after loading, as `toupper(gsub("[^A-Za-z0-9]", "", code))`,
#'     so that `"Z32.1"`, `"z321"` or `"Z32 1"` all match `"Z321"`. The marker lists are normalised
#'     the same way. The cached CSV files keep the raw codes, and the coding-system labels
#'     (`"ICD-10"`, `"ICPC-2"`) are not changed. `FALSE` is the behaviour of version 1.3.}
#' }
#'
#' Fixed rules that are not settings: the outcome hierarchy, the marker lists and their order
#' (see [uio_hospital_codes()]), the grouping of MBRN children into one pregnancy (starts less than
#' 13 days apart, or the same birth date), and the live birth assumed for an MBRN record with
#' unknown vital status and an Apgar score above 0.
#'
#' @return A named list.
#' @examples
#' str(uio_default_settings())
#' uio_v13_settings()
#' @export
uio_default_settings <- function() {
  list(
    stillbirth_min_days           = 154,
    stillbirth_min_weight_g       = NULL,
    stillbirth_weight_rule        = "or",
    marker_rejection_days         = 119,
    marker_lookback_days          = 70,
    same_event_days               = 56,
    induced_abortion_default_days = 56,
    miscarriage_default_days      = 70,
    molar_default_days            = 77,
    ectopic_default_days          = 56,
    o35_induced_days              = 80,
    o35_window_days               = 27,
    induced_abortion_default_distribution = NULL,
    random_seed                   = 1,
    normalize_codes               = TRUE
  )
}

#' @rdname uio_default_settings
#' @export
uio_v13_settings <- function() {
  list(stillbirth_min_days = NULL, marker_rejection_days = 84, normalize_codes = FALSE)
}

# Merge user settings with the defaults and validate them (internal).
# Numeric day settings are stored as double, the type of the literals they replace in v1.3.
.uio_resolve_settings <- function(settings = list()) {
  defaults <- uio_default_settings()
  if (is.null(settings)) settings <- list()
  if (!is.list(settings)) stop("`settings` must be a named list, see uio_default_settings().", call. = FALSE)
  if (length(settings) > 0L && (is.null(names(settings)) || any(is.na(names(settings))) || any(names(settings) == "")))
    stop("All elements of `settings` must be named, see uio_default_settings().", call. = FALSE)
  if (anyDuplicated(names(settings)))
    stop("Duplicated names in `settings`: ", paste(unique(names(settings)[duplicated(names(settings))]), collapse = ", "), call. = FALSE)
  unknown <- setdiff(names(settings), names(defaults))
  if (length(unknown))
    stop("Unknown setting(s): ", paste(unknown, collapse = ", "),
         ". Valid settings: ", paste(names(defaults), collapse = ", "), ".", call. = FALSE)

  out <- defaults
  for (nm in names(settings)) out[nm] <- list(settings[[nm]])   # keeps NULL values

  is_whole_days <- function(v) is.numeric(v) && length(v) == 1L && !is.na(v) && is.finite(v) && v >= 0 && v == round(v)
  is_whole_vec  <- function(v) is.numeric(v) && length(v) > 0L && all(!is.na(v) & is.finite(v) & v >= 0 & v == round(v))
  # the outcome-specific defaults may also be year tables (data frame with year and days)
  year_settings <- c("induced_abortion_default_days", "miscarriage_default_days", "molar_default_days", "ectopic_default_days")
  for (nm in year_settings) {
    v <- out[[nm]]
    if (is.data.frame(v)) {
      if (!all(c("year", "days") %in% names(v)) || !nrow(v) || !is_whole_vec(v$year) || !is_whole_vec(v$days) || anyDuplicated(v$year))
        stop("Setting `", nm, "` as a table must be a data frame with columns `year` and `days` (whole numbers, one row per year).", call. = FALSE)
      v <- data.frame(year = as.numeric(v$year), days = as.numeric(v$days))
      out[[nm]] <- v[order(v$year), , drop = FALSE]
    } else if (!is_whole_days(v)) {
      stop("Setting `", nm, "` must be a single non-negative whole number of days, or a data frame with columns `year` and `days`.", call. = FALSE)
    } else out[[nm]] <- as.numeric(v)
  }
  for (nm in c("o35_induced_days", "o35_window_days", "marker_rejection_days", "marker_lookback_days", "same_event_days")) {
    if (!is_whole_days(out[[nm]]))
      stop("Setting `", nm, "` must be a single non-negative whole number of days.", call. = FALSE)
    out[[nm]] <- as.numeric(out[[nm]])
  }
  if (out$same_event_days < 2)
    stop("Setting `same_event_days` must be at least 2.", call. = FALSE)
  dd <- out$induced_abortion_default_distribution
  if (!is.null(dd)) {
    if (!is.data.frame(dd) || !all(c("days_min", "days_max", "prob") %in% names(dd)) || !nrow(dd) ||
        !is_whole_vec(dd$days_min) || !is_whole_vec(dd$days_max) || any(dd$days_max < dd$days_min) ||
        !is.numeric(dd$prob) || any(is.na(dd$prob) | dd$prob < 0) || ("year" %in% names(dd) && !all(is.na(dd$year)) && !is_whole_vec(dd$year)))
      stop("Setting `induced_abortion_default_distribution` must be NULL or a data frame with columns `days_min`, `days_max` ",
           "(whole numbers, days_max >= days_min), `prob` (non-negative) and optionally `year`.", call. = FALSE)
    yr <- if ("year" %in% names(dd)) as.numeric(dd$year) else rep(NA_real_, nrow(dd))
    tot <- tapply(dd$prob, ifelse(is.na(yr), -1, yr), sum)
    if (any(tot <= 0))
      stop("Setting `induced_abortion_default_distribution`: `prob` must have a positive sum (in each year).", call. = FALSE)
    out["induced_abortion_default_distribution"] <- list(data.frame(year = yr, days_min = as.numeric(dd$days_min),
                                                                    days_max = as.numeric(dd$days_max), prob = as.numeric(dd$prob)))
  }
  if (!is_whole_days(out$random_seed))
    stop("Setting `random_seed` must be a single non-negative whole number.", call. = FALSE)
  out$random_seed <- as.numeric(out$random_seed)
  if (!is.null(out$stillbirth_min_weight_g)) {
    v <- out$stillbirth_min_weight_g
    if (!is.numeric(v) || length(v) != 1L || is.na(v) || !is.finite(v) || v <= 0)
      stop("Setting `stillbirth_min_weight_g` must be NULL or a single positive number of grams.", call. = FALSE)
    out["stillbirth_min_weight_g"] <- list(as.numeric(v))
  }
  if (!is.character(out$stillbirth_weight_rule) || length(out$stillbirth_weight_rule) != 1L ||
      !out$stillbirth_weight_rule %in% c("or", "and", "weight_first"))
    stop("Setting `stillbirth_weight_rule` must be \"or\", \"and\" or \"weight_first\".", call. = FALSE)
  if (!is.null(out$stillbirth_min_days)) {
    if (!is_whole_days(out$stillbirth_min_days))
      stop("Setting `stillbirth_min_days` must be NULL or a single non-negative whole number of days.", call. = FALSE)
    out["stillbirth_min_days"] <- list(as.numeric(out$stillbirth_min_days))
  }
  for (nm in c("normalize_codes"))
    if (!is.logical(out[[nm]]) || length(out[[nm]]) != 1L || is.na(out[[nm]]))
      stop("Setting `", nm, "` must be TRUE or FALSE.", call. = FALSE)
  out[names(defaults)]
}

# Normalise diagnosis code strings (internal): keep letters and digits only, upper case.
# Non-character input (e.g. an all-NA logical column) is returned unchanged.
.uio_normalize_code <- function(x) {
  if (!is.character(x)) return(x)
  toupper(gsub("[^A-Za-z0-9]", "", x))
}

# Value of an outcome-specific default for each outcome year (internal). `x` is one number or
# a year table (data frame year, days, sorted by year): a year that is not listed takes the closest
# earlier listed year, a year before the first listed year takes the first.
.uio_days_for_years <- function(x, years) {
  if (!is.data.frame(x)) return(rep(x, length(years)))
  i <- findInterval(years, x$year)
  i[i == 0L] <- 1L
  x$days[i]
}

# Pregnancy lengths drawn from a distribution (internal). `dist` has year (NA = all years),
# days_min, days_max and prob. For each pregnancy one row is drawn (probability prob, rescaled within
# the year), then a whole number of days uniformly in [days_min, days_max]. The draws follow the order
# of `years` after set.seed(seed); the caller's random-number state is restored on exit.
.uio_draw_days <- function(dist, years, seed) {
  n <- length(years)
  if (n == 0L) return(numeric(0))
  genv <- globalenv()
  had_seed <- exists(".Random.seed", envir = genv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = genv, inherits = FALSE)
  on.exit(if (had_seed) assign(".Random.seed", old_seed, envir = genv) else
    if (exists(".Random.seed", envir = genv, inherits = FALSE)) rm(".Random.seed", envir = genv), add = TRUE)
  set.seed(seed)
  if (all(is.na(dist$year))) {
    grp <- rep(1L, n); tabs <- list(dist)
  } else {
    yrs <- sort(unique(dist$year))
    grp <- findInterval(years, yrs); grp[grp == 0L] <- 1L
    tabs <- lapply(yrs, function(y) dist[which(dist$year == y), , drop = FALSE])
  }
  out <- numeric(n)
  for (g in sort(unique(grp))) {
    w <- which(grp == g); d <- tabs[[g]]
    k <- if (nrow(d) == 1L) rep(1L, length(w)) else sample.int(nrow(d), length(w), replace = TRUE, prob = d$prob / sum(d$prob))
    width <- d$days_max[k] - d$days_min[k] + 1
    out[w] <- d$days_min[k] + pmin(floor(stats::runif(length(w)) * width), width - 1)
  }
  out
}
