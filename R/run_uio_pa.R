#' Run the UiO pregnancy algorithm
#'
#' Identifies pregnancies, their outcomes and their start dates in the Medical Birth Registry of
#' Norway (MBRN), the Norwegian Patient Registry (NPR) and the primary-care claims database
#' (KUHR), with the UiO pregnancy algorithm (Nordeng et al. 2024).
#'
#' @section How the algorithm works:
#' \enumerate{
#'   \item **MBRN.** Each child record gets an outcome from the vital-status variable `DODKAT`:
#'     live birth (0-6, 11-12), fetal death (7-9; stillbirth or miscarriage, see
#'     `stillbirth_min_days`), induced abortion (10) or unknown (13; a live birth if the 5-minute
#'     Apgar score is above 0). Children of one mother with starts less than 13 days apart, or the
#'     same birth date, form one pregnancy; multiple pregnancies with discordant outcomes get one
#'     outcome (an induced abortion of any fetus gives an induced abortion; a stillbirth together
#'     with live births or miscarriages gives a stillbirth; live births and miscarriages give a
#'     miscarriage). Records without the mother's ID or gestational age are dropped. The start is
#'     the birth date minus the recorded gestational age in days (`SVLEN_DG`).
#'   \item **NPR.** Records with O00 (ectopic), O01 (molar), O02-O03 (miscarriage) or O04 (induced
#'     abortion) are outcomes. On one day, molar > ectopic > miscarriage > induced abortion.
#'     Repeated records of one event (less than `same_event_days` apart) are merged, and records on
#'     the date of an MBRN outcome are linked to it. Records less than `same_event_days` after an
#'     MBRN outcome, or between the start and the end of an MBRN pregnancy, are dropped.
#'   \item **KUHR.** ICD-10 O02-O03 and ICPC-2 W82 records are miscarriages, merged and dropped by
#'     the same rules, and also dropped when they are less than `same_event_days` before or after
#'     an MBRN or NPR non-live outcome. KUHR adds miscarriages only.
#'   \item **Dating.** Pregnancies with MBRN gestational age keep the MBRN start. The others are
#'     dated, in this order: (a) induced abortions with an O35 code recorded within
#'     `o35_window_days` after the abortion get `o35_induced_days`; (b) pregnancy markers, NPR
#'     first, then KUHR (see [uio_hospital_codes()]): start = marker date minus the marker's days,
#'     for the first marker in priority order that is recorded within `marker_lookback_days` (and
#'     `marker_rejection_days`) before the outcome and implies a length below
#'     `marker_rejection_days`; (c) if markers were found but
#'     all were rejected, `marker_rejection_days`; (d) otherwise the outcome-specific default
#'     (`induced_abortion_default_days`, `miscarriage_default_days`, `molar_default_days`,
#'     `ectopic_default_days`, or a draw from `induced_abortion_default_distribution`). A start
#'     before the woman's previous outcome is moved to that outcome date
#'     (`truncated_at_previous_outcome`).
#' }
#'
#' @section Time conventions:
#' The start date is **day 0**; the gestational age at the outcome is
#' `GA_event_days_final = date_outcome - pr_start_final` (days) and
#' `GA_event_week_final = floor(GA_event_days_final / 7)` (completed weeks). A default of 56 days
#' therefore puts the outcome at 8+0 weeks, which the Norwegian registries count as week 9
#' (ordinal weeks: week `n` = days `7(n-1)` to `7n-1`). All windows are in whole days; "within
#' `d` days before the outcome" includes the outcome date and the day `d` days before it.
#'
#' @section Settings:
#' Every setting is an argument with a default (the main analysis of the four-algorithm
#' comparison). They can also be passed together as a named list in `settings`, for example
#' `settings = uio_v13_settings()` for the rules of version 1.3. A setting may be given either as an
#' argument or in `settings`, not both. Details of each setting: [uio_default_settings()].
#'
#' @section Cached files:
#' The source extracts are read once and cached in `workdir` (`npr_mothers_full.csv`,
#' `birth_registry_full.csv`, `hospital_full.csv`, `primary_care_full.csv`); later runs read the
#' cached files. Delete them to read the extracts again, for example after changing
#' `source_files`. Settings never change the cached files, except that with
#' `stillbirth_min_weight_g` a cached `birth_registry_full.csv` without the birth weight is read
#' again from the source and cached with it.
#'
#' Three small C++ helpers are compiled with [Rcpp::sourceCpp()] when the algorithm runs, so a
#' C++ compiler is needed at run time (Rtools on Windows).
#'
#' @param workdir Existing folder for the cached source extracts. The working directory is set to
#'   it during the run and restored afterwards.
#' @param source_files The registry extracts: `uio_project_sources("p1111")` (or `"p704"`,
#'   `"p1380"`) for those projects, otherwise made with [uio_source_files()].
#' @param hospital_codes,primary_care_codes Pregnancy markers (`vocab`, `code`, `days`; row order =
#'   priority), see [uio_hospital_codes()].
#' @param years_range `c(first_year, last_year)` or `NULL` (no limit). Records (MBRN, NPR, KUHR)
#'   before `first_year` are not read; NPR and KUHR outcome records after `last_year` are not
#'   used (MBRN births after `last_year` are kept). Default `c(2008, 2023)`.
#' @param stillbirth_min_days Gestational age in days from which an MBRN fetal death is a
#'   stillbirth. Default `154` (22+0 weeks); `NULL` = version 1.3 rule (from 147 days).
#' @param stillbirth_min_weight_g `NULL` (default) or a birth weight in grams (usually `500`) that
#'   also decides between stillbirth and miscarriage.
#' @param stillbirth_weight_rule `"or"` (default), `"and"` or `"weight_first"`: how the weight and
#'   the gestational-age rule are combined.
#' @param marker_rejection_days A marker is used only within this many days before the outcome,
#'   and a marker-implied length of this many days or more is rejected; if all markers are
#'   rejected, the pregnancy gets this length. Default `119` (17+0 weeks).
#' @param marker_lookback_days Markers are collected from this many days before the outcome.
#'   Default `70`.
#' @param same_event_days Records less than this many days apart are the same event. Default `56`.
#' @param induced_abortion_default_days,miscarriage_default_days,molar_default_days,ectopic_default_days
#'   Pregnancy length in days when nothing else dates the pregnancy. Defaults `56`, `70`, `77`,
#'   `56`. One number, or a data frame with columns `year` and `days`.
#' @param o35_induced_days Length of an induced abortion with an O35 code. Default `80`.
#' @param o35_window_days The O35 code must be recorded from the abortion date up to this many
#'   days after it. Default `27`.
#' @param induced_abortion_default_distribution `NULL` (default) or a data frame (`days_min`,
#'   `days_max`, `prob`, optional `year`) from which the length of undated induced abortions is
#'   drawn.
#' @param random_seed Seed for those draws. Default `1`.
#' @param normalize_codes Normalise diagnosis codes (upper case, letters and digits only) before
#'   matching. Default `TRUE`.
#' @param settings `NULL` or a named list of settings (names as the arguments above).
#'
#' @return A `data.table`, one row per pregnancy:
#' \describe{
#'   \item{`outcome_final`, `outcome_lable`}{1 Livebirth, 2 Miscarriage, 3 Termination of
#'     pregnancy (induced abortion), 4 Stillbirth, 5 Molar pregnancy, 6 Ectopic pregnancy.}
#'   \item{`ID`}{The woman's ID (the mother ID of the source files).}
#'   \item{`preg_id`}{Pregnancy number (internal).}
#'   \item{`date_outcome`}{Date of the outcome (MBRN birth date, or the first record of the event).}
#'   \item{`pr_start_final`}{Start of the pregnancy (day 0).}
#'   \item{`GA_event_days_final`, `GA_event_week_final`}{Gestational age at the outcome in days and
#'     in completed weeks.}
#'   \item{`MC_source`}{Sources of a miscarriage: 1 MBRN+NPR+KUHR, 2 MBRN+NPR, 3 MBRN+KUHR, 4 MBRN,
#'     5 NPR+KUHR, 6 NPR, 7 KUHR.}
#'   \item{`TOP_source`}{Sources of an induced abortion: 2 MBRN+NPR, 4 MBRN, 6 NPR.}
#'   \item{`dating_method`}{`"MBRN (recorded GA)"`, `"pregnancy marker"`,
#'     `"fixed 80 d (TOP with O35)"`, `"fixed 119 d (marker >= 17 wk rejected)"`,
#'     `"fixed outcome-specific default"` or
#'     `"fixed outcome-specific default (drawn from distribution)"` (the numbers follow the settings).}
#'   \item{`truncated_at_previous_outcome`}{`TRUE` if the start was moved to the woman's previous
#'     outcome date.}
#' }
#' The complete settings used are attached as `attr(result, "settings")`.
#'
#' @references Nordeng HME, Lupattelli A, Engjom HM, van Gelder MMHJ. Detecting and dating early
#'   non-live pregnancy outcomes: generation of a novel pregnancy algorithm from Norwegian linked
#'   health registries. Pharmacoepidemiol Drug Saf. 2024;33(9):e70002. \doi{10.1002/pds.70002}
#' @examples
#' \dontrun{
#' library(UiOPregnancyAlgorithm)
#' src <- uio_project_sources("p1111")
#'
#' # main analysis (the defaults)
#' res <- run_uio_pa(workdir = "N:/durable/work/uio", source_files = src)
#'
#' # sensitivity analyses
#' res49 <- run_uio_pa(workdir = "N:/durable/work/uio", source_files = src,
#'                     induced_abortion_default_days = 49)
#' res_w <- run_uio_pa(workdir = "N:/durable/work/uio", source_files = src,
#'                     stillbirth_min_weight_g = 500, stillbirth_weight_rule = "and")
#'
#' # version 1.3
#' res13 <- run_uio_pa(workdir = "N:/durable/work/uio", source_files = src,
#'                     settings = uio_v13_settings())
#' attr(res13, "settings")
#' }
#' @export
run_uio_pa <- function(workdir,
                       source_files,
                       hospital_codes = uio_hospital_codes(),
                       primary_care_codes = uio_primary_care_codes(),
                       years_range = c(2008, 2023),
                       stillbirth_min_days = 154,
                       stillbirth_min_weight_g = NULL,
                       stillbirth_weight_rule = "or",
                       marker_rejection_days = 119,
                       marker_lookback_days = 70,
                       same_event_days = 56,
                       induced_abortion_default_days = 56,
                       miscarriage_default_days = 70,
                       molar_default_days = 77,
                       ectopic_default_days = 56,
                       o35_induced_days = 80,
                       o35_window_days = 27,
                       induced_abortion_default_distribution = NULL,
                       random_seed = 1,
                       normalize_codes = TRUE,
                       settings = NULL) {
  if (missing(workdir) || !is.character(workdir) || length(workdir) != 1L || !dir.exists(workdir))
    stop("`workdir` must be an existing folder.", call. = FALSE)
  if (missing(source_files))
    stop("`source_files` is missing: use uio_project_sources(\"p1111\") (or \"p704\", \"p1380\"), or uio_source_files().", call. = FALSE)
  # settings given as arguments (explicitly, in this call) and in the `settings` list
  nms <- names(uio_default_settings())
  given <- intersect(names(match.call())[-1], nms)
  if (!is.null(settings) && !is.list(settings))
    stop("`settings` must be NULL or a named list, see uio_default_settings().", call. = FALSE)
  both <- intersect(given, names(settings))
  if (length(both))
    stop("Setting(s) given both as an argument and in `settings`: ", paste(both, collapse = ", "), call. = FALSE)
  settings <- .uio_resolve_settings(c(settings, mget(given, envir = environment())))
  force(source_files); force(hospital_codes); force(primary_care_codes)

  old <- setwd(workdir)
  on.exit(setwd(old), add = TRUE)

  source_files <- uio_prepare_sources(source_files)
  res <- detect_pregnancy_episodes(source_files, hospital_codes, primary_care_codes,
                                   years_range = years_range, settings = settings)
  setattr(res, "settings", settings)
  res
}
