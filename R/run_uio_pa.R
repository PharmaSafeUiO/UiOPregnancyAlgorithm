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
#'     `ectopic_default_days`; for induced abortions by default the median of the year of the outcome
#'     in the Registry of Pregnancy Termination, see [uio_termination_days_by_year()]). A start
#'     before the woman's previous outcome is moved to that outcome date
#'     (`truncated_at_previous_outcome`).
#' }
#'
#' @section Time conventions:
#' The start date is **day 0**; the gestational age at the outcome is
#' `GA_event_days_final = date_outcome - pr_start_final` (days) and
#' `GA_event_week_final = floor(GA_event_days_final / 7)` (completed weeks). A length of 49 days
#' therefore puts the outcome at 7+0 weeks, which the Norwegian registries count as week 8
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
#' @param stillbirth_min_weight_g Birth weight in grams from which an MBRN fetal death is a stillbirth
#'   whatever its gestational age (22 weeks or 500 g). Default `500`; used when the birth weight is
#'   mapped, `NULL` turns it off.
#' @param marker_rejection_days A marker is used only within this many days before the outcome,
#'   and a marker-implied length of this many days or more is rejected; if all markers are
#'   rejected, the pregnancy gets this length. Default `119` (17+0 weeks).
#' @param marker_lookback_days Markers are collected from this many days before the outcome.
#'   Default `70`.
#' @param same_event_days Records less than this many days apart are the same event. Default `56`.
#' @param induced_abortion_default_days,miscarriage_default_days,molar_default_days,ectopic_default_days
#'   Pregnancy length in days when nothing else dates the pregnancy. Defaults `NULL` (induced
#'   abortions: the median of the outcome year, [uio_termination_days_by_year()]), `70`, `77`, `56`.
#'   One number, or a data frame with columns `year` and `days`.
#' @param o35_induced_days Length of an induced abortion with an O35 code. Default `80`.
#' @param o35_window_days The O35 code must be recorded from the abortion date up to this many
#'   days after it. Default `27`.
#' @param normalize_codes Normalise diagnosis codes (upper case, letters and digits only) before
#'   matching. Default `TRUE`.
#' @param settings `NULL` or a named list of settings (names as the arguments above).
#' @param output_dir Folder for the results (created if needed): `uio_pregnancies.rds`,
#'   `uio_outcomes.csv`, `uio_code_availability.csv` and `uio_report.html`. Default
#'   `file.path(workdir, "output")`; `NULL` writes nothing.
#' @param report Write the HTML report. Default `TRUE`.
#' @param mask_below In the CSV files and the report, counts from 1 to `mask_below - 1` are shown
#'   as `"<5"` (default `5`), so that they can be exported from TSD. `0` shows all counts.
#' @param verbose Print the summary at the end of the run. Default `TRUE`.
#' @param missing_ga_report \[experimental\] Describe the MBRN records without gestational age (which
#'   the algorithm drops): the NPR/KUHR outcome codes around the birth date (does NPR/KUHR confirm the
#'   outcome?), the pregnancy markers before it (can the start be estimated?), and the pregnancy that the
#'   current algorithm builds, for fetal deaths by birth weight and for live births. The report is
#'   written to `output_dir/missing_ga` as `missing_ga_report.md`, `.html` and `.docx`, with the tables as
#'   CSV files (counts masked with `mask_below`), and attached to the result as `attr(result, "missing_ga")`
#'   (unmasked, with the women's IDs: keep it inside TSD). Default `TRUE`.
#' @param missing_ga_outcome_days \[experimental\] `c(before =, after =)`: outcome codes (and the pregnancy
#'   of the current algorithm) are searched from `before` days before to `after` days after the MBRN birth
#'   date. Default `c(before = 55, after = 55)` (`same_event_days - 1`: the algorithm's same-event window).
#' @param missing_ga_marker_days,missing_ga_reject_days \[experimental\] Pregnancy markers are searched from
#'   `missing_ga_marker_days` days before the birth date up to it, and a marker that implies
#'   `missing_ga_reject_days` days or more is rejected. `NULL` (default): the run's `marker_lookback_days`
#'   and `marker_rejection_days` (70 and 119), as in the algorithm.
#' @param missing_ga_impute \[experimental\] If `TRUE` (default), MBRN single births with a fetal death
#'   (`DODKAT` 7-9) and without gestational age are kept: each is decided as a termination (an NPR/KUHR O04 code or a
#'   KUHR W83 code around the birth date, whatever the weight), a stillbirth (weight at least `stillbirth_min_weight_g`; or weight missing and
#'   no NPR/KUHR miscarriage code around the birth date) or a late spontaneous abortion (weight below the limit; or weight
#'   missing and such a code), and gets the median gestational age of the MBRN records of that kind (terminations:
#'   DODKAT 10) with a recorded gestational age. MBRN single live
#'   births without gestational age are kept with `missing_ga_livebirth_days`. `dating_method` then says
#'   "MBRN (imputed GA: ...)" and `ga_imputed` is `TRUE`. `FALSE` drops them, as the main package.
#' @param missing_ga_confirm_days \[experimental\] `c(before =, after =)`: the window around the birth date for the
#'   NPR/KUHR codes of a fetal death without gestational age: ICD-10 O04 or ICPC-2 W83 (termination), and ICD-10 O02, O03
#'   or ICPC-2 W82 (late spontaneous abortion when the weight is missing). Default `c(before = 28, after = 7)`. Records in
#'   the `before` days are this pregnancy's own codes, not a previous outcome (`missing_ga_min_gap_days`).
#' @param missing_ga_livebirth_days \[experimental\] Gestational age (days) given to MBRN single live births without
#'   gestational age when `missing_ga_impute = TRUE` (they are then kept). Default `280` (40+0 weeks, the median of live
#'   births).
#' @param missing_ga_min_gap_days \[experimental, may be dropped\] The start of an imputed pregnancy is not earlier than
#'   this many days after the woman's previous outcome (an MBRN birth, or an NPR/KUHR outcome code of the algorithm, dated
#'   as the algorithm dates repeated records, more than `missing_ga_confirm_days["before"]` days before the birth date);
#'   otherwise its gestational age is shortened. Default `28`. The rest of the algorithm only moves a start to the previous
#'   outcome date.
#' @param missing_ga_livebirth_min_days \[experimental, may be dropped\] A live birth whose gestational age is shortened
#'   below this (by `missing_ga_min_gap_days`) is not imputed (dropped, as in the main package). Default `154` (22+0 weeks).
#' @param nonlive_codes \[experimental\] The non-live outcome codes of the report, see
#'   [uio_nonlive_codes()].
#'
#' @section Summary and output folder:
#' At the end of the run the summary is printed: the availability of the codes in NPR and KUHR
#' (records and women for each pregnancy marker, with its days to the start, and for each outcome
#' code), the settings, and the number of pregnancies of each outcome (all six, also those with
#' none) with the median gestational age. The output folder receives the pregnancies (`.rds`), the
#' outcome table and the code availability (`.csv`) and an HTML report with the results by year,
#' by the woman's age, the gestational age per outcome, the dating and the sources.
#'
#' @return A `data.table`, one row per pregnancy:
#' \describe{
#'   \item{`ID`}{The woman's ID (the mother ID of the source files).}
#'   \item{`preg_id`}{Pregnancy number (internal).}
#'   \item{`outcome_final`, `outcome_lable`}{1 Livebirth, 2 Miscarriage, 3 Termination of
#'     pregnancy (induced abortion), 4 Stillbirth, 5 Molar pregnancy, 6 Ectopic pregnancy.}
#'   \item{`outcome_lable_new`}{\[experimental\] The same outcome with new labels: Live birth, Late
#'     spontaneous abortion (a miscarriage decided by the MBRN, `MC_source` 1-4) or Spontaneous abortion
#'     (decided by NPR or KUHR), Termination of pregnancy, Stillbirth, Molar pregnancy, Ectopic pregnancy.}
#'   \item{`date_outcome`, `year_outcome`}{Date (and year) of the outcome: the MBRN birth date, or
#'     the first record of the event.}
#'   \item{`pr_start_final`}{Start of the pregnancy (day 0).}
#'   \item{`GA_event_days_final`, `GA_event_week_final`}{Gestational age at the outcome in days and
#'     in completed weeks.}
#'   \item{`dating_class`}{`"recorded"` (MBRN gestational age), `"estimated"` (pregnancy marker) or
#'     `"fixed"` (default length, marker limit or O35 rule).}
#'   \item{`dating_source`, `dating_code`}{Where the start came from (`"MBRN"`, `"NPR"`, `"KUHR"`,
#'     `"default"`) and the marker or O35 code that set it.}
#'   \item{`outcome_source`, `outcome_code`}{The registry that decided the outcome (MBRN before NPR
#'     before KUHR) and its code on the outcome date (`"DODKAT 7"`, `"O039"`, `"W82"`, ...).}
#'   \item{`codes_mbrn`, `codes_npr`, `codes_kuhr`}{All pregnancy codes of the episode in each
#'     source: the MBRN vital status of the children, and the NPR and KUHR outcome, O35 and marker
#'     codes recorded from the start up to `same_event_days - 1` days after the outcome.}
#'   \item{`mother_age`}{The woman's age at the outcome: from the MBRN at the birth, otherwise from
#'     her birth year estimated from her MBRN records (`NA` without an MBRN record).}
#'   \item{`MC_source`}{Sources of a miscarriage: 1 MBRN+NPR+KUHR, 2 MBRN+NPR, 3 MBRN+KUHR, 4 MBRN,
#'     5 NPR+KUHR, 6 NPR, 7 KUHR.}
#'   \item{`TOP_source`}{Sources of an induced abortion: 2 MBRN+NPR, 4 MBRN, 6 NPR.}
#'   \item{`dating_method`}{`"MBRN (recorded GA)"`, `"pregnancy marker"`,
#'     `"fixed 80 d (TOP with O35)"`, `"fixed 119 d (marker >= 17 wk rejected)"` or
#'     `"fixed outcome-specific default"` (the numbers follow the settings).}
#'   \item{`truncated_at_previous_outcome`}{`TRUE` if the start was moved to the woman's previous
#'     outcome date.}
#' }
#' The complete settings used are attached as `attr(result, "settings")`, and the code availability
#' as `attr(result, "code_availability")`.
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
#' res_ga <- run_uio_pa(workdir = "N:/durable/work/uio", source_files = src,
#'                      stillbirth_min_weight_g = NULL)    # stillbirth by gestational age only
#' res_49 <- run_uio_pa(workdir = "N:/durable/work/uio", source_files = src,
#'                      induced_abortion_default_days = 49) # one fixed length for undated abortions
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
                       stillbirth_min_weight_g = 500,
                       marker_rejection_days = 119,
                       marker_lookback_days = 70,
                       same_event_days = 56,
                       induced_abortion_default_days = NULL,
                       miscarriage_default_days = 70,
                       molar_default_days = 77,
                       ectopic_default_days = 56,
                       o35_induced_days = 80,
                       o35_window_days = 27,
                       normalize_codes = TRUE,
                       settings = NULL,
                       output_dir = file.path(workdir, "output"),
                       report = TRUE,
                       mask_below = 5,
                       verbose = TRUE,
                       missing_ga_report = TRUE,
                       missing_ga_outcome_days = c(before = 55, after = 55),
                       missing_ga_marker_days = NULL,
                       missing_ga_reject_days = NULL,
                       missing_ga_impute = TRUE,
                       missing_ga_confirm_days = c(before = 28, after = 7),
                       missing_ga_livebirth_days = 280,
                       missing_ga_min_gap_days = 28,
                       missing_ga_livebirth_min_days = 154,
                       nonlive_codes = uio_nonlive_codes()) {
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
  if (!is.null(output_dir)) output_dir <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)

  old <- setwd(workdir)
  on.exit(setwd(old), add = TRUE)

  source_files <- uio_prepare_sources(source_files)
  res <- detect_pregnancy_episodes(source_files, hospital_codes, primary_care_codes,
                                   years_range = years_range, settings = settings, nonlive_codes = nonlive_codes,
                                   missing_ga_impute = missing_ga_impute, missing_ga_confirm_days = missing_ga_confirm_days,
                                   missing_ga_livebirth_days = missing_ga_livebirth_days,
                                   missing_ga_min_gap_days = missing_ga_min_gap_days,
                                   missing_ga_livebirth_min_days = missing_ga_livebirth_min_days)
  cap <- attr(res, "uio_capture")
  notes <- character(0)
  if (!is.null(settings$stillbirth_min_weight_g) && !isTRUE(cap$weight_used))
    notes <- c(notes, "Birth weight not mapped: stillbirth decided by gestational age only.")
  notes <- c(notes, .uio_extrapolation_notes(cap$extrapolated))
  imp_all <- cap$missing_ga$imputed   # [experimental]
  imp <- if (!is.null(imp_all) && "imputed" %in% names(imp_all)) imp_all[imputed == TRUE] else imp_all
  if (!is.null(imp) && nrow(imp)) {
    mkn <- function(x) .uio_mask(x, max(mask_below, 1))
    top <- imp[kind == "termination"]
    notes <- c(notes, paste0("[experimental] MBRN single births without gestational age kept with an imputed gestational age: ",
                             mkn(nrow(top)), " terminations (NPR O04 only ", mkn(sum(top$o04_npr & !top$o04_kuhr & !top$w83)),
                             ", KUHR W83 only ", mkn(sum(top$w83 & !top$o04_npr & !top$o04_kuhr)), ", KUHR O04 only ",
                             mkn(sum(top$o04_kuhr & !top$o04_npr & !top$w83)), ", more than one of these ",
                             mkn(sum((top$o04_npr + top$o04_kuhr + top$w83) > 1)), "; ", cap$missing_ga$medians[["termination"]],
                             " days, the median of the MBRN terminations with a recorded gestational age), ",
                             mkn(sum(imp$kind == "stillbirth")), " stillbirths (", cap$missing_ga$medians[["stillbirth"]],
                             " days, the median of the MBRN stillbirths with a recorded gestational age) and ",
                             mkn(sum(imp$kind == "late_spontaneous_abortion")), " late spontaneous abortions (", cap$missing_ga$medians[["late_spontaneous_abortion"]],
                             " days, the median of the MBRN late spontaneous abortions with a recorded gestational age)",
                             if (any(imp$kind == "live_birth")) paste0("; MBRN single live births without gestational age: ",
                                                                       mkn(sum(imp$kind == "live_birth")), " (", cap$missing_ga$medians[["live_birth"]], " days)") else "", ". ",
                             "Start not earlier than ", missing_ga_min_gap_days, " days after the woman's previous outcome: ", mkn(sum(imp$capped)),
                             " starts moved; ", mkn(sum(!imp_all$imputed)), " records not imputed because the gestational age left was too short (live births below ",
                             missing_ga_livebirth_min_days, " days)."))
  }
  out <- .uio_describe(res, cap, settings, hospital_codes, primary_care_codes)
  # pregnancies without an outcome (kept, as in v1.3): MBRN multiple births whose combination of child outcomes is not
  # one of those the algorithm lists (e.g. a triplet with two live births and a miscarriage; quadruplets other than
  # four live births or two miscarriages and two terminations), or an MBRN DODKAT that is not a known code
  n_na <- sum(is.na(out$outcome_final))
  if (n_na) {
    notes <- c(notes, paste0(.uio_mask(n_na, max(mask_below, 1)), " pregnancies have no outcome (outcome_final NA): MBRN multiple births ",
                             "whose combination of child outcomes is not one the algorithm lists (e.g. a triplet with two live births ",
                             "and a miscarriage), or an MBRN DODKAT that is not a known code. They are kept in the result, as in version ",
                             "1.3, but are not in the outcome tables and charts; codes_mbrn shows their DODKAT values."))
    cli::cli_alert_warning("{n_na} pregnanc{?y has/ies have} no outcome (MBRN multiple birth with a combination of outcomes the algorithm does not list, or an unknown DODKAT): kept with outcome_final NA, not in the tables. See res[is.na(outcome_final)].")
  }
  avail <- .uio_code_availability(cap, settings, hospital_codes, primary_care_codes)
  # [experimental] MBRN records without gestational age
  mg <- if (isTRUE(missing_ga_report))
    .uio_missing_ga(out, cap, settings, hospital_codes, primary_care_codes, outcome_days = missing_ga_outcome_days,
                    marker_days = missing_ga_marker_days, reject_days = missing_ga_reject_days,
                    nonlive_codes = nonlive_codes, years_range = years_range)
  rm(res, cap)
  setattr(out, "settings", settings)
  setattr(out, "code_availability", avail)

  if (!is.null(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    mk <- function(x) if (mask_below > 0) .uio_mask(x, mask_below) else x
    ot <- .uio_outcome_table(out)
    for (cc in c("n", "recorded", "estimated", "fixed")) ot[, (cc) := mk(get(cc))]
    data.table::fwrite(ot, file.path(output_dir, "uio_outcomes.csv"))
    av <- data.table::copy(avail)[, `:=`(n_records = mk(n_records), n_women = mk(n_women))]
    data.table::fwrite(av, file.path(output_dir, "uio_code_availability.csv"))
    if (isTRUE(report)) .uio_html_report(out, avail, settings, notes, file.path(output_dir, "uio_report.html"), mask = max(mask_below, 1))
    data.table::setindex(out, NULL)     # indices that data.table adds when the report filters the table
    saveRDS(out, file.path(output_dir, "uio_pregnancies.rds"))
    if (!is.null(mg)) {
      mg_dir <- .uio_missing_ga_write(mg, settings, output_dir, mask_below)
      if (isTRUE(verbose)) cli::cli_alert_success("Missing-GA report in {mg_dir}")
    }
  }
  if (!is.null(mg)) setattr(out, "missing_ga", mg)
  # [experimental] every MBRN single birth without gestational age that was considered for imputation (with the women's IDs:
  # keep inside TSD): the decision, the codes, the previous outcome and whether the start was moved or the record not imputed
  if (!is.null(imp_all) && nrow(imp_all)) setattr(out, "imputed_missing_ga", imp_all)
  if (isTRUE(verbose)) {
    .uio_print_summary(out, avail, settings, notes)
    if (!is.null(output_dir)) cli::cli_alert_success("Results in {output_dir}")
  }
  data.table::setindex(out, NULL)
  out
}
