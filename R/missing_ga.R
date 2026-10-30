# [experimental] MBRN records without gestational age: can NPR and KUHR capture them?
#
# The algorithm drops MBRN records without gestational age (SVLEN_DG) before anything else. For each such
# birth this file looks around the MBRN birth date (FDATO) for
#   A. outcome codes of the algorithm (NPR O00-O04; KUHR O02-O03, W82): would NPR/KUHR confirm the outcome?
#   B. pregnancy markers (the algorithm's marker lists and rules): could the start be estimated?
# separately for fetal deaths by birth weight and for live births, and writes a Markdown, HTML and Word
# report. It also shows, for comparison, the pregnancy that the current algorithm builds. The algorithm
# itself is not changed.

#' Non-live outcome codes for the missing-GA report (experimental)
#'
#' Regular expressions (matched with `grepl(perl = TRUE)` after the code normalisation of the run) for
#' the NPR and KUHR codes listed in the code tables of the missing-GA report of [run_uio_pa()]: ICD-10
#' O00-O08 (pregnancy with abortive outcome) in NPR and KUHR, and ICPC-2 W82 (spontaneous abortion) and
#' W83 (induced abortion) in KUHR. The outcome confirmation itself uses the algorithm's outcome codes
#' (NPR O00-O04; KUHR O02-O03 and W82); the code tables say which codes those are.
#'
#' @return A list with `icd10` and `icpc2`.
#' @examples
#' uio_nonlive_codes()
#' @export
uio_nonlive_codes <- function() list(icd10 = "^O0[0-8]", icpc2 = "^W8[23]")

.mg_groups <- c(FD = "Fetal deaths, all", FD_lt500 = "Fetal deaths, < 500 g", FD_ge500 = "Fetal deaths, >= 500 g",
                FD_wmiss = "Fetal deaths, weight missing", LB = "Live births")
.mg_outcome_cats <- c("Miscarriage", "Termination of pregnancy", "Molar pregnancy", "Ectopic pregnancy",
                      "More than one outcome type", "No outcome code")
.mg_captured_cats <- c("Outcome and start", "Outcome only", "Start only", "Neither")

# ---- analysis ------------------------------------------------------------------------------------------
# out: the described pregnancies (.uio_describe()); cap: the records used by the run (npr, kuhr, missing_ga).
.uio_missing_ga <- function(out, cap, settings, hospital_codes, primary_care_codes, outcome_days = c(before = 55, after = 55),
                            marker_days = NULL, reject_days = NULL, nonlive_codes = uio_nonlive_codes(), years_range = NULL) {
  mg <- cap$missing_ga
  if (is.null(marker_days)) marker_days <- settings$marker_lookback_days
  if (is.null(reject_days)) reject_days <- settings$marker_rejection_days
  ob <- as.integer(outcome_days[["before"]]); oa <- as.integer(outcome_days[["after"]])
  m <- data.table::copy(mg$mbrn)
  data.table::setnames(m, "PasientLopeNr_MOR", "ID")
  m[, ID := as.character(ID)]
  m[, d := as.integer(as.Date(DAGER_FDATO))]
  dk <- suppressWarnings(as.integer(m$DODKAT))
  wt <- if ("VEKT" %in% names(m)) suppressWarnings(as.numeric(m$VEKT)) else rep(NA_real_, nrow(m))
  wt[!is.na(wt) & wt <= 0] <- NA                                      # as in the stillbirth rule
  apgar_ok <- !(trimws(m$APGAR5) %in% c("0", ""))                     # as in the algorithm (NA counts as > 0)
  grp_lab <- c(SB = "Fetal death (DODKAT 7-9)", LB = "Live birth (DODKAT 0-6, 11-12; 13 with Apgar > 0)",
               TOP = "Induced abortion (DODKAT 10)", UK = "Unknown (DODKAT 13, Apgar 0 or blank)",
               OT = "Other or missing DODKAT")
  m[, group := data.table::fcase(dk %in% 7:9, "SB", dk %in% c(0:6, 11:12), "LB", dk %in% 13L & apgar_ok, "LB",
                                 dk %in% 10L, "TOP", dk %in% 13L, "UK", default = "OT")]
  m[, wt := wt]

  # unit = one mother and birth date within a group (children of a multiple birth count once)
  u <- m[, .(n_records = .N, wt_max = if (all(is.na(wt))) NA_real_ else max(wt, na.rm = TRUE)), by = .(ID, d, group)]
  u[, weight_class := data.table::fcase(is.na(wt_max), "unknown", wt_max < 500, "known, < 500 g", default = "known, >= 500 g")]
  u[, uid := .I]
  u[, year := as.integer(format(as.Date(d, origin = "1970-01-01"), "%Y"))]
  u[, wgroup := data.table::fcase(group == "SB" & weight_class == "known, < 500 g", "FD_lt500",
                                  group == "SB" & weight_class == "known, >= 500 g", "FD_ge500",
                                  group == "SB", "FD_wmiss", group == "LB", "LB", default = NA_character_)]
  u[, `:=`(lo = d - ob, hi = d + oa, mlo = d - as.integer(marker_days))]
  ua <- u[!is.na(wgroup)]

  # A. outcome codes of the algorithm from `before` days before to `after` days after the birth date
  oc <- rbind(cap$npr[grepl("O00|O01|O02|O03|O04", tilst), .(ID = as.character(ID), source = "NPR", code = as.character(tilst), dn = as.integer(inndato))],
              cap$kuhr[(diagnosekodeverk == "ICD-10" & grepl("O02|O03", diagnose)) | (diagnosekodeverk == "ICPC-2" & grepl("W82", diagnose, fixed = TRUE)),
                       .(ID = as.character(ID), source = "KUHR", code = as.character(diagnose), dn = as.integer(ant_dager))])
  oc[, type := data.table::fcase(source == "NPR" & grepl("O01", code), "Molar pregnancy", source == "NPR" & grepl("O00", code), "Ectopic pregnancy",
                                 source == "NPR" & grepl("O04", code), "Termination of pregnancy", default = "Miscarriage")]
  ah <- oc[ua, on = .(ID, dn >= lo, dn <= hi), nomatch = 0L, .(uid = i.uid, source = x.source, type = x.type, diff = x.dn - i.d)]
  # (no outcome code in any window: data.table evaluates j once on the empty table, and min() would warn)
  if (nrow(ah)) {
    a <- ah[, .(types = paste(sort(unique(type)), collapse = " + "), npr = any(source == "NPR"), kuhr = any(source == "KUHR"),
                first = min(diff)), by = uid]
    u[a, on = "uid", `:=`(types = i.types, oc_npr = i.npr, oc_kuhr = i.kuhr, oc_first = i.first)]
  }
  for (cc in c("types")) if (!cc %in% names(u)) u[, (cc) := NA_character_]
  for (cc in c("oc_npr", "oc_kuhr")) if (!cc %in% names(u)) u[, (cc) := NA]
  if (!"oc_first" %in% names(u)) u[, oc_first := NA_real_]
  # (nested fifelse: fcase() accepts only a single value as default in older data.table versions)
  u[, outcome_conf := data.table::fifelse(is.na(types), "No outcome code",
                                          data.table::fifelse(grepl(" + ", types, fixed = TRUE), "More than one outcome type", types))]
  u[, oc_source := data.table::fcase(oc_npr %in% TRUE & oc_kuhr %in% TRUE, "NPR and KUHR", oc_npr %in% TRUE, "NPR only",
                                     oc_kuhr %in% TRUE, "KUHR only", default = NA_character_)]
  # all non-live codes (uio_nonlive_codes()) in the same window, for the code tables
  nl <- data.table::copy(mg$nonlive)[, dn := as.integer(date)]
  nh <- if (nrow(nl) && nrow(ua)) unique(nl[ua, on = .(ID, dn >= lo, dn <= hi), nomatch = 0L,
                                            .(uid = i.uid, source = x.source, vocab = x.vocab, code = x.code)]) else
    data.table::data.table(uid = integer(0), source = character(0), vocab = character(0), code = character(0))
  nh[, use := data.table::fcase(source == "NPR" & grepl("O00|O01|O02|O03|O04", code), "yes (NPR outcome)",
                                source == "KUHR" & vocab == "ICD-10" & grepl("O02|O03", code), "yes (KUHR outcome)",
                                source == "KUHR" & vocab == "ICPC-2" & grepl("W82", code, fixed = TRUE), "yes (KUHR outcome)",
                                default = "no")]

  # B. pregnancy markers (the algorithm's lists, priority and rules) from marker_days before the birth date up to
  # it: the earliest record of each code; implied length = (birth date - marker date) + the marker's days; the first
  # marker in priority order (NPR list, then KUHR list) with an implied length below reject_days sets the start
  norm <- if (isTRUE(settings$normalize_codes)) .uio_normalize_code else identity
  hc <- data.table::as.data.table(hospital_codes)[, .(code = norm(code), days, prio = .I, msource = "NPR")]
  pc <- data.table::as.data.table(primary_care_codes)[, .(code = norm(code), days, prio = .I + 1000L, msource = "KUHR")]
  mk <- rbind(cap$npr[, .(ID = as.character(ID), code = as.character(tilst), dn = as.integer(inndato))][hc, on = "code", nomatch = 0L],
              cap$kuhr[, .(ID = as.character(ID), code = as.character(diagnose), dn = as.integer(ant_dager))][pc, on = "code", nomatch = 0L])
  mh <- mk[ua, on = .(ID, dn >= mlo, dn <= d), nomatch = 0L,
           .(uid = i.uid, d = i.d, code = x.code, dn = x.dn, days = x.days, prio = x.prio, msource = x.msource)]
  mh <- mh[order(uid, prio, dn)][, .SD[1], by = .(uid, prio)]
  mh[, implied := (d - dn) + days]
  acc <- mh[implied < reject_days][order(uid, prio)][, .SD[1], by = uid]
  rej <- mh[!uid %in% acc$uid][order(uid, prio)][, .SD[1], by = uid]
  u[, marker_any := uid %in% mh$uid]
  u[, `:=`(marker_code = NA_character_, marker_source = NA_character_, ga_marker = NA_real_, ga_rejected = NA_real_)]
  if (nrow(acc)) u[acc, on = "uid", `:=`(marker_code = i.code, marker_source = i.msource, ga_marker = as.numeric(i.implied))]
  if (nrow(rej)) u[rej, on = "uid", ga_rejected := as.numeric(i.implied)]
  # O35 rule (terminations): an O35 code from the birth date up to o35_window_days after it gives a fixed length
  o35 <- rbind(cap$npr[grepl("O35", tilst, fixed = TRUE), .(ID = as.character(ID), dn = as.integer(inndato))],
               cap$kuhr[diagnosekodeverk == "ICD-10" & grepl("O35", diagnose, fixed = TRUE), .(ID = as.character(ID), dn = as.integer(ant_dager))])
  u[, o35hi := d + as.integer(settings$o35_window_days)]
  oh <- o35[u[outcome_conf == "Termination of pregnancy"], on = .(ID, dn >= d, dn <= o35hi), nomatch = 0L, .(uid = i.uid)]
  lab_mk <- "Pregnancy marker (start estimated)"
  lab_rej <- paste0("Markers found, all imply >= ", reject_days, " days")
  u[, dating := data.table::fcase(uid %in% oh$uid, "O35 rule (termination, fixed length)", !is.na(ga_marker), lab_mk,
                                  marker_any, lab_rej, default = "No marker in the window")]
  u[, captured := data.table::fcase(outcome_conf != "No outcome code" & dating == lab_mk, "Outcome and start",
                                    outcome_conf != "No outcome code", "Outcome only", dating == lab_mk, "Start only", default = "Neither")]

  # C. for comparison: the pregnancy that the current algorithm built (same window; the closest one)
  p <- out[, .(ID = as.character(ID), dout = as.integer(date_outcome), outcome_final, outcome = outcome_lable,
               outcome_source, outcome_code, dating_method, GA = GA_event_days_final)]
  p[is.na(outcome), outcome := "(no outcome: multiple birth not listed, or unknown DODKAT)"]
  cand <- p[u, on = .(ID, dout >= lo, dout <= hi), nomatch = 0L,
            .(uid = i.uid, d = i.d, dout = x.dout, outcome_final = x.outcome_final, outcome = x.outcome,
              outcome_source = x.outcome_source, dating_method = x.dating_method, GA = x.GA)]
  cand[, diff := dout - d]
  cand[, n_candidates := .N, by = uid]
  best <- cand[order(uid, abs(diff), diff)][, .SD[1], by = uid]
  u[, `:=`(decided_by = NA_character_, outcome_final = NA_integer_, outcome = NA_character_, dating_method = NA_character_,
           GA = NA_real_, n_candidates = NA_integer_)]
  if (nrow(best)) u[best, on = "uid", `:=`(decided_by = i.outcome_source, outcome_final = as.integer(i.outcome_final), outcome = i.outcome,
                                           dating_method = i.dating_method, GA = as.numeric(i.GA), n_candidates = i.n_candidates)]
  u[is.na(decided_by), `:=`(decided_by = "none", outcome = "(no pregnancy found)")]

  # ---- tables (raw counts; masked when written) ----
  tabs <- list()
  pct <- function(n, N) if (N > 0) round(100 * n / N, 1) else NA_real_
  sel <- function(g) if (g == "FD") u[group == "SB"] else u[wgroup %in% g]
  G <- names(.mg_groups)
  long <- function(f, cats) data.table::rbindlist(lapply(G, function(g) {
    x <- sel(g); N <- nrow(x); v <- f(x)
    data.frame(group = g, category = cats, n = vapply(cats, function(cc) sum(v == cc, na.rm = TRUE), 0L),
               pct = vapply(cats, function(cc) pct(sum(v == cc, na.rm = TRUE), N), 0), stringsAsFactors = FALSE)
  }))
  ga_stats <- function(g) list(GA_median = if (length(g)) as.numeric(stats::median(g)) else NA_real_, GA_mean = if (length(g)) mean(g) else NA_real_,
                               GA_SD = if (length(g) > 1) stats::sd(g) else NA_real_, GA_min = if (length(g)) min(g) else NA_real_,
                               GA_max = if (length(g)) max(g) else NA_real_)
  ng <- vapply(G, function(g) nrow(sel(g)), 0L)

  ov <- u[, .(units = .N, records = sum(n_records), women = data.table::uniqueN(ID)), by = group]
  ov <- ov[match(intersect(names(grp_lab), ov$group), group)]
  tabs$overview <- data.frame(group = unname(grp_lab[ov$group]), n = ov$units, records = ov$records, women = ov$women, stringsAsFactors = FALSE)
  tabs$groups <- data.frame(group = unname(.mg_groups), n = unname(ng), stringsAsFactors = FALSE)

  # at a glance
  tabs$glance_outcome <- long(function(x) x$outcome_conf, .mg_outcome_cats)
  dat_cats <- c(lab_mk, lab_rej, "O35 rule (termination, fixed length)", "No marker in the window")
  tabs$glance_dating <- long(function(x) x$dating, dat_cats)
  tabs$glance_captured <- long(function(x) x$captured, .mg_captured_cats)

  # A. outcome confirmation
  sumA <- c("with an outcome code", "  in NPR", "  in KUHR", "  in both")
  tabs$A_summary <- long(function(x) c(rep(sumA[1], sum(!is.na(x$oc_source))), rep(sumA[2], sum(x$oc_npr %in% TRUE)),
                                       rep(sumA[3], sum(x$oc_kuhr %in% TRUE)), rep(sumA[4], sum(x$oc_source %in% "NPR and KUHR"))), sumA)
  tabs$A_outcome <- data.table::rbindlist(lapply(G, function(g) {
    x <- sel(g); N <- nrow(x)
    data.frame(group = .mg_groups[[g]], outcome_confirmed = .mg_outcome_cats,
               n = vapply(.mg_outcome_cats, function(cc) sum(x$outcome_conf == cc), 0L),
               pct = vapply(.mg_outcome_cats, function(cc) pct(sum(x$outcome_conf == cc), N), 0),
               NPR_only = vapply(.mg_outcome_cats, function(cc) sum(x$outcome_conf == cc & x$oc_source %in% "NPR only"), 0L),
               KUHR_only = vapply(.mg_outcome_cats, function(cc) sum(x$outcome_conf == cc & x$oc_source %in% "KUHR only"), 0L),
               NPR_and_KUHR = vapply(.mg_outcome_cats, function(cc) sum(x$outcome_conf == cc & x$oc_source %in% "NPR and KUHR"), 0L),
               stringsAsFactors = FALSE)
  }))
  tabs$A_outcome <- tabs$A_outcome[n > 0]
  # birth weight (VEKT; for a multiple birth the highest) of the fetal deaths with a recorded weight, by confirmed outcome
  tabs$A_weight <- data.table::rbindlist(lapply(c("FD", "FD_lt500", "FD_ge500"), function(g) {
    x <- sel(g)[!is.na(wt_max)]
    data.table::rbindlist(lapply(.mg_outcome_cats, function(cc) {
      w <- x[outcome_conf == cc, wt_max]
      if (!length(w)) return(NULL)
      q <- stats::quantile(w, c(0.25, 0.5, 0.75), names = FALSE, type = 7)
      list(group = .mg_groups[[g]], outcome_confirmed = cc, n = length(w), weight_median = q[2], weight_Q1 = q[1], weight_Q3 = q[3],
           weight_mean = mean(w), weight_SD = if (length(w) > 1) stats::sd(w) else NA_real_, weight_min = min(w), weight_max = max(w))
    }))
  }))
  tabs$A_codes <- data.table::rbindlist(lapply(G, function(g) {
    x <- sel(g); h <- nh[uid %in% x$uid]
    if (!nrow(h)) return(NULL)
    f <- h[, .(n = data.table::uniqueN(uid)), by = .(source, vocab, code, use)][order(match(source, c("NPR", "KUHR")), vocab, code)]
    data.frame(group = .mg_groups[[g]], source = f$source, coding = f$vocab, code = f$code, meaning = .mg_code_label(f$code),
               outcome_code_of_algorithm = f$use, n = f$n, pct = vapply(f$n, pct, 0, N = nrow(x)), stringsAsFactors = FALSE)
  }))
  cm <- u[!is.na(wgroup) & outcome_conf == "More than one outcome type"]
  tabs$A_combinations <- if (nrow(cm)) { z <- cm[, .(n = .N), by = .(wgroup, types)][order(match(wgroup, G), -n)]
    data.frame(group = unname(.mg_groups[z$wgroup]), combination = z$types, n = z$n, stringsAsFactors = FALSE) } else
    data.frame(group = character(0), combination = character(0), n = integer(0))
  blab <- c("29-55 days before", "8-28 days before", "1-7 days before", "same day", "1-7 days after", "8-28 days after", "29-55 days after")
  bin <- function(v) blab[findInterval(v, c(-Inf, -28.5, -7.5, -0.5, 0.5, 7.5, 28.5))]
  tabs$A_timing <- long(function(x) bin(x$oc_first[!is.na(x$oc_first)]), blab)
  # the timing table is relative to the units with an outcome code: recompute the percentages
  tabs$A_timing$pct <- NA_real_

  # B. dating by pregnancy markers
  sumB <- c("with a marker in the window", "  marker accepted (start estimated)", paste0("  markers found, all implying ", reject_days, " days or more"),
            "no marker in the window")
  tabs$B_summary <- long(function(x) c(rep(sumB[1], sum(x$marker_any)), rep(sumB[2], sum(!is.na(x$ga_marker))),
                                       rep(sumB[3], sum(x$marker_any & is.na(x$ga_marker))), rep(sumB[4], sum(!x$marker_any))), sumB)
  tabs$B_method <- data.table::rbindlist(lapply(G, function(g) {
    x <- sel(g); N <- nrow(x)
    data.table::rbindlist(lapply(dat_cats, function(cc) {
      y <- x[dating == cc]
      gv <- if (cc == lab_mk) y$ga_marker else if (cc == lab_rej) y$ga_rejected else numeric(0)
      c(list(group = .mg_groups[[g]], dating_method = cc, n = nrow(y), pct = pct(nrow(y), N)), ga_stats(gv))
    }))
  }))
  tabs$B_by_outcome <- data.table::rbindlist(lapply(G, function(g) {
    x <- sel(g)
    z <- x[, .(n = .N), by = .(outcome_conf, dating)][order(match(outcome_conf, .mg_outcome_cats), match(dating, dat_cats))]
    if (!nrow(z)) return(NULL)
    data.frame(group = .mg_groups[[g]], outcome_confirmed = z$outcome_conf, dating_method = z$dating, n = z$n, stringsAsFactors = FALSE)
  }))
  tabs$B_markers <- data.table::rbindlist(lapply(G, function(g) {
    x <- sel(g)[!is.na(marker_code)]
    if (!nrow(x)) return(NULL)
    z <- x[, .(n = .N), by = .(marker_source, marker_code)][order(match(marker_source, c("NPR", "KUHR")), -n)]
    data.frame(group = .mg_groups[[g]], source = z$marker_source, code = z$marker_code, meaning = .mg_code_label(z$marker_code),
               days_to_start = rbind(hc, pc)$days[match(paste(z$marker_source, z$marker_code), paste(rbind(hc, pc)$msource, rbind(hc, pc)$code))],
               n = z$n, stringsAsFactors = FALSE)
  }))

  # captured (outcome and start)
  tabs$captured <- long(function(x) x$captured, .mg_captured_cats)

  # C. the current algorithm, for comparison
  dat5 <- function(dm) data.table::fcase(is.na(dm), NA_character_, dm == "MBRN (recorded GA)", "MBRN_GA",
                                         startsWith(dm, "MBRN (imputed"), "MBRN_GA_imputed", dm == "pregnancy marker", "by_marker",
                                         grepl("O35", dm, fixed = TRUE), "fixed_O35", grepl("rejected", dm, fixed = TRUE), "fixed_rejected_marker",
                                         default = "fixed_default")
  u[, dating_alg := dat5(dating_method)]
  dcols <- c("by_marker", "fixed_default", "fixed_O35", "fixed_rejected_marker", "MBRN_GA", "MBRN_GA_imputed")
  src_order <- c("NPR", "KUHR", "MBRN", "none")
  tabs$C_algorithm <- data.table::rbindlist(lapply(G, function(g) {
    x <- sel(g); N <- nrow(x)
    if (!N) return(NULL)
    t <- x[, c(list(n = .N), lapply(stats::setNames(dcols, dcols), function(k) sum(dating_alg == k, na.rm = TRUE)), ga_stats(GA[decided_by %in% c("NPR", "KUHR")])),
           by = .(decided_by, outcome_final, outcome)][order(match(decided_by, src_order), outcome_final)]
    z <- data.frame(group = .mg_groups[[g]], decided_by = t$decided_by, outcome = t$outcome, n = t$n,
                    pct = vapply(t$n, pct, 0, N = N), stringsAsFactors = FALSE)
    for (k in dcols) z[[k]] <- ifelse(t$decided_by == "none", NA, t[[k]])
    for (k in c("GA_median", "GA_mean", "GA_SD", "GA_min", "GA_max")) z[[k]] <- ifelse(t$decided_by %in% c("NPR", "KUHR"), t[[k]], NA)
    z
  }))
  yr <- u[, .(n = .N), by = .(year, group)][order(year, match(group, names(grp_lab)))]
  tabs$by_year <- data.frame(year = yr$year, group = unname(grp_lab[yr$group]), n = yr$n, stringsAsFactors = FALSE)

  # timing percentages: of the units with an outcome code in each group
  nwo <- vapply(G, function(g) sum(!is.na(sel(g)$oc_first)), 0L)
  tabs$A_timing$pct <- round(100 * tabs$A_timing$n / pmax(nwo[tabs$A_timing$group], 1), 1)
  tabs$A_timing$pct[nwo[tabs$A_timing$group] == 0] <- NA
  for (nm in c("glance_outcome", "glance_dating", "glance_captured", "A_summary", "B_summary", "captured", "A_timing"))
    tabs[[nm]]$group <- unname(.mg_groups[tabs[[nm]]$group])

  list(units = u, tables = tabs,
       meta = list(outcome_days = c(before = ob, after = oa), marker_days = marker_days, reject_days = reject_days,
                   o35_days = settings$o35_window_days, nonlive_codes = nonlive_codes, years_range = years_range,
                   weight_mapped = "VEKT" %in% names(mg$mbrn), n_multi_candidates = sum(u$n_candidates > 1, na.rm = TRUE),
                   n_group = ng, lab_mk = lab_mk, lab_rej = lab_rej, dat_cats = dat_cats))
}

# ---- code titles ---------------------------------------------------------------------------------------------
# Short titles of the ICD-10 (WHO) codes O00-O08, O20, O21, O26, O35, Z31, Z32, Z35 (three and four characters),
# of the ICPC-2 codes W01, W03, W05, W78, W79, W82, W83, and of MBRN DODKAT values. Other codes get "".
.mg_code_label <- function(code) {
  t3 <- c(O00 = "Ectopic pregnancy", O01 = "Hydatidiform mole", O02 = "Other abnormal products of conception",
          O03 = "Spontaneous abortion", O04 = "Medical abortion", O05 = "Other abortion", O06 = "Unspecified abortion",
          O07 = "Failed attempted abortion", O08 = "Complications following abortion, ectopic or molar pregnancy",
          O20 = "Haemorrhage in early pregnancy", O21 = "Excessive vomiting in pregnancy",
          O26 = "Maternal care for other conditions predominantly related to pregnancy",
          O35 = "Maternal care for known or suspected fetal abnormality and damage",
          Z31 = "Procreative management", Z32 = "Pregnancy examination and test", Z35 = "Supervision of high-risk pregnancy",
          W01 = "Question of pregnancy (ICPC-2)", W03 = "Antepartum bleeding (ICPC-2)", W05 = "Pregnancy vomiting or nausea (ICPC-2)",
          W78 = "Pregnancy (ICPC-2)", W79 = "Unwanted pregnancy (ICPC-2)",
          W82 = "Spontaneous abortion (ICPC-2)", W83 = "Induced abortion (ICPC-2)")
  ab <- c("incomplete, with infection", "incomplete, with haemorrhage", "incomplete, with embolism",
          "incomplete, other or unspecified complications", "incomplete, without complication",
          "complete or unspecified, with infection", "complete or unspecified, with haemorrhage",
          "complete or unspecified, with embolism", "complete or unspecified, other or unspecified complications",
          "complete or unspecified, without complication")
  t4 <- list(O00 = c(`0` = "abdominal pregnancy", `1` = "tubal pregnancy", `2` = "ovarian pregnancy", `8` = "other", `9` = "unspecified"),
             O01 = c(`0` = "classical", `1` = "incomplete and partial", `9` = "unspecified"),
             O02 = c(`0` = "blighted ovum and nonhydatidiform mole", `1` = "missed abortion", `8` = "other specified", `9` = "unspecified"),
             O03 = stats::setNames(ab, 0:9), O04 = stats::setNames(ab, 0:9), O05 = stats::setNames(ab, 0:9), O06 = stats::setNames(ab, 0:9),
             O07 = stats::setNames(c("failed medical abortion, with infection", "failed medical abortion, with haemorrhage",
                                     "failed medical abortion, with embolism", "failed medical abortion, other or unspecified complications",
                                     "failed medical abortion, without complication", "other failed attempt, with infection",
                                     "other failed attempt, with haemorrhage", "other failed attempt, with embolism",
                                     "other failed attempt, other or unspecified complications", "other failed attempt, without complication"), 0:9),
             O08 = stats::setNames(c("genital tract and pelvic infection", "delayed or excessive haemorrhage", "embolism", "shock",
                                     "renal failure", "metabolic disorders", "damage to pelvic organs and tissues",
                                     "other venous complications", "other complications", "unspecified"), 0:9),
             O20 = c(`0` = "threatened abortion", `8` = "other", `9` = "unspecified"),
             O21 = c(`0` = "mild hyperemesis gravidarum", `1` = "hyperemesis gravidarum with metabolic disturbance",
                     `2` = "late vomiting of pregnancy", `8` = "other vomiting", `9` = "unspecified"),
             O26 = c(`2` = "pregnancy care of habitual aborter", `3` = "retained intrauterine contraceptive device in pregnancy"),
             Z31 = c(`1` = "artificial insemination", `2` = "in vitro fertilization", `3` = "other assisted fertilization methods"),
             Z32 = c(`0` = "pregnancy, not (yet) confirmed", `1` = "pregnancy confirmed"),
             Z35 = c(`0` = "with history of infertility", `1` = "with history of abortive outcome"),
             W78 = c(`1` = "Norwegian sub-code"))
  vapply(as.character(code), function(k) {
    if (is.na(k) || !nzchar(k)) return("")
    if (grepl("^DODKAT", k)) {
      v <- suppressWarnings(as.integer(strsplit(sub("^DODKAT ?", "", k), "/", fixed = TRUE)[[1]]))
      g <- ifelse(v %in% c(0:6, 11:12), "live born", ifelse(v %in% 7:9, "fetal death", ifelse(v %in% 10L, "induced abortion", "unknown")))
      return(paste0("MBRN vital status: ", paste(unique(g), collapse = ", ")))
    }
    c0 <- toupper(gsub("[^A-Za-z0-9]", "", k)); c3 <- substr(c0, 1, 3)
    if (!c3 %in% names(t3)) return("")
    if (nchar(c0) == 3L || !c3 %in% names(t4)) return(unname(t3[c3]))
    s4 <- t4[[c3]][substr(c0, 4, 4)]
    if (is.na(s4)) unname(t3[c3]) else paste0(t3[[c3]], ": ", s4)
  }, "", USE.NAMES = FALSE)
}

# ---- masking -------------------------------------------------------------------------------------------------
.mg_count_cols <- c("n", "records", "women", "NPR_only", "KUHR_only", "NPR_and_KUHR",
                    "by_marker", "fixed_default", "fixed_O35", "fixed_rejected_marker", "MBRN_GA", "MBRN_GA_imputed")
.mg_stat_cols <- c("GA_median", "GA_mean", "GA_SD", "GA_min", "GA_max",
                   "weight_median", "weight_Q1", "weight_Q3", "weight_mean", "weight_SD", "weight_min", "weight_max")

# Mask counts 1..k-1; when n is masked, its percentage and the gestational-age and weight statistics of the row are left out.
.mg_mask_df <- function(df, k) {
  out <- as.data.frame(df, stringsAsFactors = FALSE)
  if (nrow(out)) {
    small <- if ("n" %in% names(out)) !is.na(out$n) & out$n > 0 & out$n < k else rep(FALSE, nrow(out))
    fmt_n <- function(v) ifelse(is.na(v), "", ifelse(k > 1 & v > 0 & v < k, paste0("<", k), formatC(v, format = "d", big.mark = ",")))
    for (cc in intersect(.mg_count_cols, names(out))) out[[cc]] <- fmt_n(out[[cc]])
    if ("pct" %in% names(out)) out$pct <- ifelse(small | is.na(out$pct), "", formatC(out$pct, format = "f", digits = 1))
    for (cc in intersect(.mg_stat_cols, names(out)))
      out[[cc]] <- ifelse(small | is.na(out[[cc]]), "",
                          if (cc %in% c("GA_mean", "GA_SD", "weight_mean", "weight_SD")) formatC(out[[cc]], format = "f", digits = 1) else as.character(round(out[[cc]], 1)))
  }
  names(out)[names(out) == "pct"] <- "%"
  names(out) <- gsub("_", " ", names(out), fixed = TRUE)
  out
}

# A long table (group, category, n, pct) as categories x groups, each cell "n (pct%)", masked.
.mg_wide <- function(df, k, first = "") {
  cats <- unique(df$category); grps <- unique(df$group)
  out <- data.frame(x = cats, stringsAsFactors = FALSE); names(out) <- first
  for (g in grps) out[[g]] <- vapply(cats, function(cc) {
    r <- df[df$group == g & df$category == cc, ]
    n <- r$n[1]; p <- r$pct[1]
    if (is.na(n)) return("")
    if (k > 1 && n > 0 && n < k) return(paste0("<", k))
    paste0(formatC(n, format = "d", big.mark = ","), if (!is.na(p) && n > 0) paste0(" (", formatC(p, format = "f", digits = 1), "%)") else "")
  }, "", USE.NAMES = FALSE)
  out
}

# Short explanation of each column (HTML: shown when the pointer is on the column header or a count; Word and
# Markdown: the column guide)
.mg_col_help <- c(
  "n" = "Units: one mother and birth date (the children of one birth count once)",
  "records" = "MBRN child records without gestational age",
  "women" = "Different mothers",
  "%" = "Percent of the units of the group",
  "group" = "Fetal deaths (all, and by birth weight) or live births without gestational age",
  "outcome confirmed" = "Type of the NPR/KUHR outcome codes found around the birth date",
  "NPR only" = "Units whose outcome codes in the window are all from NPR",
  "KUHR only" = "Units whose outcome codes in the window are all from KUHR",
  "NPR and KUHR" = "Units with outcome codes in the window from both NPR and KUHR",
  "combination" = "The different outcome types found for one unit",
  "code" = "Diagnosis code as recorded (after the code normalisation)",
  "meaning" = "Short ICD-10 (WHO) or ICPC-2 title of the code",
  "coding" = "Code system",
  "source" = "Registry of the record",
  "outcome code of algorithm" = "Whether the algorithm uses this code as an outcome code",
  "dating method" = "How the start of the pregnancy could be set from the codes around the birth date",
  "days to start" = "Days from the marker to the pregnancy start, as in the algorithm's marker list",
  "decided by" = "Registry that decided the outcome of the pregnancy the algorithm built",
  "outcome" = "Outcome of the pregnancy the algorithm built",
  "by marker" = "Start set from a pregnancy marker",
  "fixed default" = "No usable marker: fixed length for the outcome",
  "fixed O35" = "Termination with an O35 code: fixed length",
  "fixed rejected marker" = "Markers found but all implied too long a pregnancy: fixed length",
  "MBRN GA" = "Start from the gestational age of another MBRN record of the birth",
  "MBRN GA imputed" = "This MBRN record, kept with the median gestational age of its kind (stillbirth or late spontaneous abortion)",
  "GA median" = "Gestational age in days (see the text above the table): median",
  "GA mean" = "Gestational age in days (see the text above the table): mean",
  "GA SD" = "Gestational age in days (see the text above the table): standard deviation",
  "GA min" = "Gestational age in days (see the text above the table): shortest",
  "GA max" = "Gestational age in days (see the text above the table): longest",
  "weight median" = "Birth weight in grams: median",
  "weight Q1" = "Birth weight in grams: first quartile (25% weigh less)",
  "weight Q3" = "Birth weight in grams: third quartile (75% weigh less)",
  "weight mean" = "Birth weight in grams: mean",
  "weight SD" = "Birth weight in grams: standard deviation",
  "weight min" = "Birth weight in grams: lowest",
  "weight max" = "Birth weight in grams: highest")
.mg_col_help <- c(.mg_col_help, stats::setNames(rep("n (% of the units of this group)", length(.mg_groups)), unname(.mg_groups)))

# ---- report text ---------------------------------------------------------------------------------------------
.mg_report_blocks <- function(mg, settings, mask, chart = NULL) {
  mt <- mg$meta; tb <- mg$tables; T <- lapply(tb, .mg_mask_df, k = mask)
  ob <- mt$outcome_days[["before"]]; oa <- mt$outcome_days[["after"]]
  yr <- if (is.null(mt$years_range)) "all years" else paste0(mt$years_range[1], "-", mt$years_range[2])
  mk <- function(n) if (mask > 1 && n > 0 && n < mask) paste0("<", mask) else format(n, big.mark = ",")
  W <- function(nm, first = "") .mg_wide(tb[[nm]], mask, first)
  B <- list()
  cur <- "intro"
  add <- function(type, x = NULL, ...) B[[length(B) + 1L]] <<- list(type = type, x = x, part = cur, ...)
  sec <- function(part, title) { cur <<- part; add("h1", title) }
  add("title", "MBRN births without gestational age: can NPR and KUHR capture them?")
  add("p", paste0("Experimental report of UiOPregnancyAlgorithm ",
                  tryCatch(format(utils::packageVersion("UiOPregnancyAlgorithm")), error = function(e) ""),
                  ", ", format(Sys.time(), "%Y-%m-%d %H:%M"), ". The algorithm drops MBRN records without gestational age (SVLEN_DG). ",
                  "For each such birth, this report looks in NPR and KUHR around the MBRN birth date (FDATO) for two things: ",
                  "(A) an outcome code that confirms how the pregnancy ended, and (B) a pregnancy marker from which the start of the ",
                  "pregnancy can be estimated. Fetal deaths are shown in total and by birth weight, because birth weight is going ",
                  "into the stillbirth rule, and DODKAT with birth weight still does not separate stillbirths from miscarriages when ",
                  "the weight is missing."))
  sec("def", "Definitions")
  add("ul", c(
    paste0("MBRN births: records with the mother's ID and without gestational age, birth year in ", yr, " (years_range of the run)."),
    "Unit (n): one mother and birth date within a group; the children of a multiple birth count once. All counts are units.",
    "Groups: fetal deaths (DODKAT 7-9), all and by birth weight (VEKT: below 500 g, 500 g or more, missing or 0 g or less; for a multiple birth the highest weight); live births (DODKAT 0-6, 11-12, and 13 with a 5-minute Apgar score that is not 0 or blank, as in the algorithm).",
    paste0("Outcome codes say how a pregnancy ended. They are the algorithm's outcome codes: NPR O00 (ectopic), O01 (molar), O02-O03 (miscarriage), O04 (termination); KUHR O02-O03 and W82 (miscarriage). They are searched from ",
           ob, " days before to ", oa, " days after the birth date: in the algorithm, records less than 56 days apart belong to the same event, and NPR/KUHR outcome records less than 56 days after an MBRN birth are part of that birth."),
    paste0("Pregnancy markers say the woman was pregnant at a time (for example Z32.1 pregnancy confirmed, W78 pregnancy). They are searched as in the algorithm: from ",
           mt$marker_days, " days before the birth date up to the birth date; the earliest record of each marker code; NPR list first, then KUHR, in list order. The first marker whose implied length, (birth date - marker date) + the marker's days, is below ",
           mt$reject_days, " days sets the start; that implied length is the estimated gestational age."),
    paste0("O35 rule: a termination (confirmed by O04) with an O35 code from the birth date up to ", mt$o35_days, " days after it gets the algorithm's fixed length, whether or not there is a marker."),
    "Outcome confirmed: the type of the outcome codes found; 'More than one outcome type' when codes of different types are found (the combinations are listed).",
    "Captured: 'Outcome and start' = an outcome code and a marker-estimated start; 'Outcome only'; 'Start only'; 'Neither'.",
    paste0("Units with more than one pregnancy of the current algorithm in the window (Part D): ", mk(mt$n_multi_candidates), "."),
    if (!mt$weight_mapped) "THE BIRTH WEIGHT WAS NOT READ IN THIS RUN (not mapped, or stillbirth_min_weight_g = NULL): all fetal deaths are in 'weight missing'." else NULL,
    if (mask > 1) paste0("Counts from 1 to ", mask - 1, " are shown as <", mask, "; then the percentage and the gestational-age and weight statistics of that row are left out.")
    else "No counts are masked (mask_below = 0): do not export this report from TSD."))
  add("p", "Column guide (in the HTML report, point at a column header or a count to see this):")
  used <- unique(unlist(lapply(T, names)))
  hl <- .mg_col_help[names(.mg_col_help) %in% used & nzchar(.mg_col_help) & !names(.mg_col_help) %in% unname(.mg_groups)]
  add("ul", c(paste0(names(hl), ": ", unname(hl)), "group columns (Fetal deaths, all; ...; Live births): n (% of the units of this group)"))

  sec("overview", "Overview")
  add("p", "MBRN records without gestational age, by DODKAT group (n = units).")
  add("table", T$overview)
  add("p", "The groups of this report (n = units).")
  add("table", T$groups)

  sec("glance", "At a glance")
  if (!is.null(chart) && file.exists(chart))
    add("img", chart, alt = paste0("Three panels, one column per group (fetal deaths all, below 500 g, 500 g or more, weight missing; live births): ",
                                   "the share with each confirmed outcome, with a start estimated from a marker, and with outcome and start together."))
  else add("p", "(The chart could not be drawn in this R session; the tables below have the same numbers.)")
  add("p", "Outcome confirmed by an NPR or KUHR outcome code (n, % of the group).")
  add("table", W("glance_outcome", "outcome confirmed"))
  add("p", "Start of the pregnancy from the codes before the birth date (n, % of the group).")
  add("table", W("glance_dating", "dating method"))
  add("p", "Outcome and start together (n, % of the group).")
  add("table", W("glance_captured", "captured"))

  sec("A", "A. Outcome confirmed by NPR or KUHR")
  add("p", paste0("Units with at least one of the algorithm's outcome codes from ", ob, " days before to ", oa, " days after the birth date (n, % of the group)."))
  add("table", W("A_summary", "item"))
  add("p", "Confirmed outcome, and the registries the outcome codes came from (% of the group).")
  add("table", T$A_outcome)
  add("p", paste0("Birth weight in grams (VEKT; for a multiple birth the highest) of the fetal deaths with a recorded weight, by confirmed outcome ",
                  "(n = units with a weight; median, quartiles, mean, SD, lowest and highest)."))
  add("table", T$A_weight)
  add("p", "Units with more than one outcome type: the combinations.")
  add("table", T$A_combinations)
  add("p", "Date of the first outcome code relative to the birth date (n, % of the units of the group with an outcome code).")
  add("table", W("A_timing", "first outcome code"))
  add("p", paste0("All non-live codes (ICD-10 '", mt$nonlive_codes$icd10, "', ICPC-2 '", mt$nonlive_codes$icpc2, "') in the same window, with their meaning ",
                  "and whether the algorithm uses them as outcome codes (number of units with the code at least once; % of the group)."))
  add("table", T$A_codes)

  sec("B", "B. Start estimated from pregnancy markers")
  add("p", paste0("Note: with the algorithm's rules a marker is used only within ", mt$marker_days, " days before the birth date and only when it implies fewer than ",
                  mt$reject_days, " days, so a marker can only give a gestational age below ", mt$reject_days, " days (", floor(mt$reject_days / 7), "+", mt$reject_days %% 7,
                  " weeks). Later fetal deaths usually have their early markers further back, so for them 'no marker' or 'markers found, all imply >= ",
                  mt$reject_days, " days' is expected. The limits are arguments of run_uio_pa() (missing_ga_marker_days, missing_ga_reject_days)."))
  add("p", "Units with a marker in the window, and whether a start could be estimated (n, % of the group).")
  add("table", W("B_summary", "item"))
  add("p", paste0("Dating method, with the gestational age in days: for 'pregnancy marker' the estimated gestational age; for 'markers found, all imply >= ",
                  mt$reject_days, " days' the length implied by the first marker in priority order."))
  add("table", T$B_method)
  add("p", "Dating method by confirmed outcome.")
  add("table", T$B_by_outcome)
  add("p", "The marker that set the start (n units).")
  add("table", T$B_markers)

  sec("C", "C. Outcome and start together")
  add("p", "Captured by NPR/KUHR: an outcome code around the birth date and a start estimated from a marker (n, % of the group).")
  add("table", W("captured", "captured"))

  sec("D", "D. For comparison: the pregnancy the current algorithm builds")
  add("p", paste0("The pregnancy of the current algorithm (which drops these MBRN records) for the same woman with its outcome date from ", ob,
                  " days before to ", oa, " days after the birth date (the closest one): the registry that decided it, its outcome, how it was dated ",
                  "(counts of units) and, for the pregnancies decided by NPR or KUHR, the gestational age in days that the algorithm gave them. ",
                  "Unlike Parts A-C, the algorithm dates from its own outcome date, uses its own rules for repeated records, and may drop records."))
  add("table", T$C_algorithm)

  sec("supp", "Supplement")
  add("p", "MBRN units without gestational age by year of birth and DODKAT group.")
  add("table", T$by_year)
  add("p", paste0("Settings of the run: ", paste(paste0(.uio_settings_table(settings)$setting, " = ", .uio_settings_table(settings)$value), collapse = "; "), "."))
  B
}

# ---- chart: three panels (confirmed outcome, start, both), one column per group (PNG, base graphics) -------------
.mg_chart <- function(mg, file, mask) {
  tb <- mg$tables; ng <- mg$meta$n_group
  gl <- unname(.mg_groups); hdr <- c("Fetal deaths\nall", "Fetal deaths\n< 500 g", "Fetal deaths\n>= 500 g", "Fetal deaths\nweight missing", "Live births")
  panels <- list(
    list(title = paste0("A. Outcome confirmed by an NPR/KUHR outcome code (", mg$meta$outcome_days[["before"]], " days before to ",
                        mg$meta$outcome_days[["after"]], " days after the birth date)"), d = tb$glance_outcome, col = "#2a78d6"),
    list(title = paste0("B. Start estimated from a pregnancy marker (", mg$meta$marker_days, " days before the birth date up to it)"),
         d = tb$glance_dating, col = "#3f9b6b"),
    list(title = "C. Outcome and start together", d = tb$glance_captured, col = "#7b5ea7"))
  nrow_p <- vapply(panels, function(p) length(unique(p$d$category)), 0L)
  top <- 3.4; total <- top + sum(1.4 + 1.9 + nrow_p + 0.9)
  w_in <- 9; h_in <- 0.25 * total
  args <- list(filename = file, width = w_in, height = h_in, units = "in", res = 200, bg = "white")
  if (isTRUE(capabilities("cairo"))) args$type <- "cairo"
  ok <- tryCatch({ do.call(grDevices::png, args); TRUE }, error = function(e) FALSE)
  if (!ok) return(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::par(mar = c(0, 0, 0, 0), xpd = NA, family = "sans")
  graphics::plot.new(); graphics::plot.window(xlim = c(0, 1), ylim = c(total, 0), xaxs = "i", yaxs = "i")
  ink <- "#1b1b19"; muted <- "#6b6a64"; grid <- "#e6e5e0"
  lab_w <- 0.29; cw <- (1 - lab_w - 0.01) / length(gl); bmax <- cw * 0.42
  graphics::text(0.012, 0.95, "MBRN births without gestational age: what NPR and KUHR can add", adj = c(0, 0.5), font = 2, cex = 1.3, col = ink)
  graphics::text(0.012, 2.15, paste0("The algorithm's marker rules. ",
                                     "Each bar: % of the births (units) of the group. Counts below the masking limit are shown as text only."),
                 adj = c(0, 0.5), cex = 0.75, col = muted)
  y <- top
  for (p in panels) {
    graphics::text(0.012, y + 0.7, p$title, adj = c(0, 0.5), font = 2, cex = 0.85, col = ink)
    y <- y + 1.4
    for (j in seq_along(gl)) {
      xc <- lab_w + (j - 1) * cw
      N <- ng[[names(.mg_groups)[j]]]
      graphics::text(xc + 0.006, y + 0.45, hdr[j], adj = c(0, 0.5), cex = 0.68, col = ink, font = 2)
      graphics::text(xc + 0.006, y + 1.45, paste0("n = ", if (mask > 1 && N > 0 && N < mask) paste0("<", mask) else format(N, big.mark = ",")),
                     adj = c(0, 0.5), cex = 0.66, col = muted)
    }
    y <- y + 1.9
    cats <- unique(p$d$category)
    for (i in seq_along(cats)) {
      yc <- y + i - 0.5
      if (i %% 2 == 0) graphics::rect(0.005, yc - 0.5, 0.995, yc + 0.5, col = "#f6f5f2", border = NA)
      lab <- cats[i]
      while (graphics::strwidth(lab, cex = 0.74) > lab_w - 0.02 && nchar(lab) > 10) lab <- paste0(substr(lab, 1, nchar(lab) - 4), "...")
      graphics::text(0.012, yc, lab, adj = c(0, 0.5), cex = 0.74, col = ink)
      for (j in seq_along(gl)) {
        r <- p$d[p$d$group == gl[j] & p$d$category == cats[i], ]
        n <- if (nrow(r)) r$n[1] else 0; pc <- if (nrow(r)) r$pct[1] else NA
        xc <- lab_w + (j - 1) * cw + 0.006
        small <- mask > 1 && n > 0 && n < mask
        if (!small && !is.na(pc) && n > 0) graphics::rect(xc, yc - 0.3, xc + max(pc / 100 * bmax, 0.002), yc + 0.3, col = p$col, border = NA)
        txt <- if (small) paste0("<", mask) else if (is.na(pc)) "" else if (n == 0) "0" else paste0(formatC(pc, format = "f", digits = 0), "% (", format(n, big.mark = ","), ")")
        graphics::text(xc + (if (!small && !is.na(pc) && n > 0) pc / 100 * bmax else 0) + 0.006, yc, txt, adj = c(0, 0.5), cex = 0.66, col = ink)
      }
    }
    y <- y + length(cats) + 0.9
  }
  c(w_in, h_in)
}
# base64 of a file (for the image in the self-contained HTML)
.mg_b64 <- function(path) {
  r <- as.integer(readBin(path, "raw", file.info(path)$size))
  pad <- (3L - length(r) %% 3L) %% 3L
  r <- c(r, rep(0L, pad))
  m <- matrix(r, nrow = 3L)
  v <- m[1, ] * 65536L + m[2, ] * 256L + m[3, ]
  idx <- rbind(v %/% 262144L, (v %/% 4096L) %% 64L, (v %/% 64L) %% 64L, v %% 64L)
  s <- c(LETTERS, letters, 0:9, "+", "/")[as.vector(idx) + 1L]
  if (pad) s[(length(s) - pad + 1L):length(s)] <- "="
  paste(s, collapse = "")
}

# Markdown
.mg_md <- function(B) {
  esc <- function(x) gsub("|", "\\|", as.character(x), fixed = TRUE)
  out <- character(0)
  for (b in B) {
    out <- c(out, switch(b$type,
      title = c(paste0("# ", b$x), ""),
      h1 = c(paste0("## ", b$x), ""),
      p = c(b$x, ""),
      ul = c(paste0("- ", b$x), ""),
      img = c(paste0("![", b$alt, "](", basename(b$x), ")"), ""),
      table = if (!nrow(b$x)) c("*(none)*", "") else
        c(paste0("| ", paste(esc(names(b$x)), collapse = " | "), " |"),
          paste0("|", paste(rep("---", ncol(b$x)), collapse = "|"), "|"),
          apply(b$x, 1, function(r) paste0("| ", paste(esc(r), collapse = " | "), " |")), "")))
  }
  out
}

# HTML table with the column explanation on the header and on the counts (title attribute)
.mg_html_table <- function(df) {
  tip <- function(nm) { t <- .mg_col_help[nm]; if (is.na(t) || !nzchar(t)) "" else paste0(' title="', .h(t), '"') }
  hd <- paste0(vapply(names(df), function(nm) paste0("<th", tip(nm), ">", .h(nm), "</th>"), ""), collapse = "")
  cnt <- names(df) %in% c(gsub("_", " ", .mg_count_cols, fixed = TRUE), unname(.mg_groups))
  rows <- vapply(seq_len(nrow(df)), function(i) {
    v <- vapply(df[i, ], as.character, "")
    paste0("<tr>", paste0(ifelse(cnt, paste0('<td class="num"', vapply(names(df), tip, ""), ">"), "<td>"), .h(v), "</td>", collapse = ""), "</tr>")
  }, "")
  paste0('<div class="tw"><table><thead><tr>', hd, "</tr></thead><tbody>", paste(rows, collapse = ""), "</tbody></table></div>")
}

# HTML (self-contained, light and dark)
.mg_html <- function(B) {
  blocks <- function(B) vapply(seq_along(B), function(i) {
    b <- B[[i]]
    inner <- switch(b$type,
      title = paste0("<h1>", .h(b$x), "</h1>"),
      h1 = paste0("<h2>", .h(b$x), "</h2>"),
      p = paste0("<p>", .h(b$x), "</p>"),
      ul = paste0("<ul>", paste0("<li>", .h(b$x), "</li>", collapse = ""), "</ul>"),
      img = paste0('<figure class="chart"><img src="data:image/png;base64,', .mg_b64(b$x), '" alt="', .h(b$alt), '"></figure>'),
      table = if (!nrow(b$x)) '<p class="muted">(none)</p>' else .mg_html_table(b$x))
    paste0('<div class="blk" data-k="', i, '">', inner, "</div>")
  }, "")
  body <- blocks(B)
  hdr <- '<header class="top"><div class="hin"><span class="ttl">MBRN births without gestational age</span></div></header>'
  css <- paste(
    ":root{--bg:#fbfbf9;--fg:#1b1b19;--muted:#6b6a64;--line:#e2e1dc;--head:#f1f0ec;--accent:#7a3b2e;color-scheme:light}",
    "@media (prefers-color-scheme:dark){:root:not([data-theme=light]){--bg:#1a1a18;--fg:#ecebe6;--muted:#a3a29a;--line:#34332f;--head:#25241f;--accent:#e0907e;color-scheme:dark}}",
    ":root[data-theme=dark]{--bg:#1a1a18;--fg:#ecebe6;--muted:#a3a29a;--line:#34332f;--head:#25241f;--accent:#e0907e;color-scheme:dark}",
    "body{margin:0;background:var(--bg);color:var(--fg);font:15px/1.55 system-ui,-apple-system,'Segoe UI',sans-serif;overflow-y:scroll}",
    "body::before{content:'';position:fixed;inset:0 0 auto;height:env(safe-area-inset-top,0px);background:var(--bg);z-index:11}",
    ".gut{padding-left:max(16px,env(safe-area-inset-left,0px));padding-right:max(16px,env(safe-area-inset-right,0px))}",
    "header.top{position:sticky;top:env(safe-area-inset-top,0px);z-index:10;background:var(--bg);border-bottom:1px solid var(--line)}",
    ".hin{max-width:1000px;margin:0 auto;padding-block:10px;padding-left:max(16px,env(safe-area-inset-left,0px));padding-right:max(16px,env(safe-area-inset-right,0px))}",
    ".ttl{font-weight:650}",
    "main{max-width:1000px;margin:0 auto;padding-block:8px 40px}h1{font-size:24px;text-wrap:balance}h2{font-size:18px;margin-top:32px;color:var(--accent);text-wrap:balance}",
    "p,li{max-width:80ch}.muted{color:var(--muted)}.tw{overflow-x:auto}",
    "table{border-collapse:collapse;font-size:13.5px;font-variant-numeric:tabular-nums;margin:8px 0 16px}",
    "th,td{border-bottom:1px solid var(--line);padding:5px 10px;text-align:left;vertical-align:top}th{background:var(--head);font-weight:600}",
    "th[title],td[title]{cursor:help}th[title]{text-decoration:underline dotted;text-underline-offset:3px}td.num{text-align:right}",
    "figure.chart{margin:12px 0 20px;background:#fff;border:1px solid var(--line);border-radius:8px;padding:8px;max-width:100%}",
    "figure.chart img{display:block;width:100%;height:auto}")
  c("<!doctype html>", '<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">',
    "<title>Missing-GA MBRN records</title>", paste0("<style>", css, "</style></head><body>"),
    hdr, '<div class="gut"><main>', body, "</main></div>", "</body></html>")
}

# Word (.docx) written directly as Office Open XML, so no extra package is needed (zip is used when
# installed, otherwise utils::zip)
.mg_docx <- function(B, file) {
  x <- function(s) { s <- gsub("&", "&amp;", as.character(s), fixed = TRUE); s <- gsub("<", "&lt;", s, fixed = TRUE); gsub(">", "&gt;", s, fixed = TRUE) }
  par <- function(s, style = NULL, bold = FALSE)
    paste0("<w:p>", if (!is.null(style)) paste0('<w:pPr><w:pStyle w:val="', style, '"/></w:pPr>'),
           '<w:r>', if (bold) "<w:rPr><w:b/></w:rPr>", '<w:t xml:space="preserve">', x(s), "</w:t></w:r></w:p>")
  cell <- function(s, head) paste0('<w:tc><w:tcPr><w:tcW w:w="0" w:type="auto"/>', if (head) '<w:shd w:val="clear" w:color="auto" w:fill="EDEDE8"/>',
                                   "</w:tcPr>", par(s, "TableText", bold = head), "</w:tc>")
  tbl <- function(df) {
    if (!nrow(df)) return(par("(none)"))
    bd <- paste0('<w:', c("top", "left", "bottom", "right", "insideH", "insideV"), ' w:val="single" w:sz="4" w:space="0" w:color="BFBFBF"/>', collapse = "")
    rows <- c(paste0("<w:tr><w:trPr><w:tblHeader/></w:trPr>", paste0(vapply(names(df), cell, "", head = TRUE), collapse = ""), "</w:tr>"),
              vapply(seq_len(nrow(df)), function(i) paste0("<w:tr>", paste0(vapply(unlist(lapply(df[i, ], as.character)), cell, "", head = FALSE), collapse = ""), "</w:tr>"), ""))
    paste0('<w:tbl><w:tblPr><w:tblW w:w="0" w:type="auto"/><w:tblBorders>', bd, "</w:tblBorders>",
           '<w:tblCellMar><w:left w:w="70" w:type="dxa"/><w:right w:w="70" w:type="dxa"/></w:tblCellMar></w:tblPr>',
           paste(rows, collapse = ""), "</w:tbl>", par(""))
  }
  imgs <- character(0)
  pic <- function(path, alt) {
    imgs <<- c(imgs, path); k <- length(imgs)
    dim <- attr(path, "size_in"); if (is.null(dim)) dim <- c(8, 6)
    w <- min(dim[1], 7.0); h <- dim[2] * w / dim[1]
    cx <- round(w * 914400); cy <- round(h * 914400)
    paste0('<w:p><w:r><w:drawing><wp:inline distT="0" distB="0" distL="0" distR="0"><wp:extent cx="', cx, '" cy="', cy, '"/>',
           '<wp:docPr id="', k, '" name="Chart ', k, '" descr="', x(alt), '"/>',
           '<a:graphic><a:graphicData uri="http://schemas.openxmlformats.org/drawingml/2006/picture"><pic:pic>',
           '<pic:nvPicPr><pic:cNvPr id="', k, '" name="image', k, '.png"/><pic:cNvPicPr/></pic:nvPicPr>',
           '<pic:blipFill><a:blip r:embed="rIdImg', k, '"/><a:stretch><a:fillRect/></a:stretch></pic:blipFill>',
           '<pic:spPr><a:xfrm><a:off x="0" y="0"/><a:ext cx="', cx, '" cy="', cy, '"/></a:xfrm><a:prstGeom prst="rect"><a:avLst/></a:prstGeom></pic:spPr>',
           "</pic:pic></a:graphicData></a:graphic></wp:inline></w:drawing></w:r></w:p>")
  }
  body <- character(0)
  for (b in B) body <- c(body, switch(b$type,
    title = par(b$x, "Title"), h1 = par(b$x, "Heading1"), p = par(b$x),
    ul = paste(vapply(b$x, function(s) par(paste0(intToUtf8(8226L), " ", s), "ListText"), ""), collapse = ""),
    img = pic(b$x, b$alt),
    table = tbl(b$x)))
  ns <- paste('xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"',
              'xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"',
              'xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"',
              'xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"',
              'xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture"')
  doc <- paste0('<?xml version="1.0" encoding="UTF-8" standalone="yes"?><w:document ', ns, "><w:body>", paste(body, collapse = ""),
                '<w:sectPr><w:pgSz w:w="11906" w:h="16838"/><w:pgMar w:top="1000" w:right="900" w:bottom="1000" w:left="900" w:header="567" w:footer="567" w:gutter="0"/></w:sectPr></w:body></w:document>')
  sty <- function(id, name, sz, bold = FALSE, before = 0, after = 120, color = NULL, ind = NULL)
    paste0('<w:style w:type="paragraph" w:styleId="', id, '"><w:name w:val="', name, '"/><w:basedOn w:val="Normal"/><w:qFormat/>',
           '<w:pPr><w:spacing w:before="', before, '" w:after="', after, '"/>', if (!is.null(ind)) paste0('<w:ind w:left="', ind, '" w:hanging="200"/>'), "</w:pPr>",
           "<w:rPr>", if (bold) "<w:b/>", if (!is.null(color)) paste0('<w:color w:val="', color, '"/>'), '<w:sz w:val="', sz, '"/></w:rPr></w:style>')
  wns <- 'xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"'
  styles <- paste0('<?xml version="1.0" encoding="UTF-8" standalone="yes"?><w:styles ', wns, ">",
                   '<w:docDefaults><w:rPrDefault><w:rPr><w:rFonts w:ascii="Calibri" w:hAnsi="Calibri" w:cs="Calibri"/><w:sz w:val="21"/></w:rPr></w:rPrDefault>',
                   '<w:pPrDefault><w:pPr><w:spacing w:after="120" w:line="264" w:lineRule="auto"/></w:pPr></w:pPrDefault></w:docDefaults>',
                   '<w:style w:type="paragraph" w:default="1" w:styleId="Normal"><w:name w:val="Normal"/><w:qFormat/></w:style>',
                   sty("Title", "Title", 36, TRUE, 0, 240), sty("Heading1", "heading 1", 28, TRUE, 360, 120, "7A3B2E"),
                   sty("TableText", "Table Text", 16, FALSE, 0, 0), sty("ListText", "List Text", 20, FALSE, 0, 60, ind = 360), "</w:styles>")
  ct <- paste0('<?xml version="1.0" encoding="UTF-8" standalone="yes"?><Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">',
               '<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>',
               '<Default Extension="xml" ContentType="application/xml"/><Default Extension="png" ContentType="image/png"/>',
               '<Override PartName="/word/document.xml" ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml"/>',
               '<Override PartName="/word/styles.xml" ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.styles+xml"/></Types>')
  rels <- paste0('<?xml version="1.0" encoding="UTF-8" standalone="yes"?><Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
                 '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="word/document.xml"/></Relationships>')
  drels <- paste0('<?xml version="1.0" encoding="UTF-8" standalone="yes"?><Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
                  '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles" Target="styles.xml"/>',
                  paste0('<Relationship Id="rIdImg', seq_along(imgs), '" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/image" Target="media/image',
                         seq_along(imgs), '.png"/>', collapse = "")[length(imgs) > 0], "</Relationships>")
  tmp <- tempfile("docx"); dir.create(file.path(tmp, "_rels"), recursive = TRUE); dir.create(file.path(tmp, "word", "_rels"), recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  wr <- function(s, f) { con <- file(file.path(tmp, f), "wb"); writeBin(charToRaw(enc2utf8(s)), con); close(con) }
  wr(ct, "[Content_Types].xml"); wr(rels, "_rels/.rels"); wr(doc, "word/document.xml"); wr(styles, "word/styles.xml"); wr(drels, "word/_rels/document.xml.rels")
  files <- c("[Content_Types].xml", "_rels/.rels", "word/document.xml", "word/styles.xml", "word/_rels/document.xml.rels")
  if (length(imgs)) {
    dir.create(file.path(tmp, "word", "media"))
    for (k in seq_along(imgs)) file.copy(imgs[k], file.path(tmp, "word", "media", paste0("image", k, ".png")))
    files <- c(files, paste0("word/media/image", seq_along(imgs), ".png"))
  }
  file <- normalizePath(file, winslash = "/", mustWork = FALSE)
  if (file.exists(file)) file.remove(file)
  if (requireNamespace("zip", quietly = TRUE)) {
    zip::zip(file, files, root = tmp, mode = "mirror")
  } else {
    old <- setwd(tmp); on.exit(setwd(old), add = TRUE)
    utils::zip(file, files, flags = "-r9Xq")
  }
  invisible(file.exists(file))
}

# Write the report (md, html, docx), the chart (png) and the masked tables (csv) to output_dir/missing_ga.
.uio_missing_ga_write <- function(mg, settings, output_dir, mask) {
  dir <- file.path(output_dir, "missing_ga"); dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  draw <- function(m, f) {
    size <- tryCatch(.mg_chart(m, f, mask), error = function(e) {
      cli::cli_alert_warning("The chart could not be drawn: {conditionMessage(e)}"); NULL })
    if (!is.null(size) && file.exists(f)) structure(f, size_in = size) else NULL
  }
  B <- .mg_report_blocks(mg, settings, mask, chart = draw(mg, file.path(dir, "missing_ga_glance.png")))
  writeLines(enc2utf8(.mg_md(B)), file.path(dir, "missing_ga_report.md"), useBytes = TRUE)
  writeLines(enc2utf8(.mg_html(B)), file.path(dir, "missing_ga_report.html"), useBytes = TRUE)
  tryCatch(.mg_docx(B, file.path(dir, "missing_ga_report.docx")), error = function(e) {
    cli::cli_alert_warning("The Word report could not be written: {conditionMessage(e)}"); FALSE })
  for (nm in names(mg$tables)) data.table::fwrite(.mg_mask_df(mg$tables[[nm]], mask), file.path(dir, paste0("table_", nm, ".csv")))
  invisible(dir)
}

utils::globalVariables(c("DAGER_FDATO", "group", "wt", "wt_max", "weight_class", "uid", "wgroup", "lo", "hi", "mlo", "o35hi",
                         "x.source", "x.type", "x.dn", "x.code", "x.days", "x.prio", "x.msource", "x.vocab", "x.dout", "x.outcome_final",
                         "x.outcome", "x.outcome_source", "x.dating_method", "x.GA", "i.uid", "i.d", "i.types", "i.npr", "i.kuhr", "i.first",
                         "i.code", "i.msource", "i.implied", "i.outcome_source", "i.outcome_final", "i.outcome", "i.dating_method", "i.GA",
                         "i.n_candidates", "types", "oc_npr", "oc_kuhr", "oc_first", "outcome_conf", "oc_source", "type", "dn", "use",
                         "implied", "prio", "msource", "marker_any", "marker_code", "marker_source", "ga_marker", "ga_rejected", "dating",
                         "captured", "decided_by", "n_candidates", "dating_alg", "GA", "diff", "dout", "date", "n_records", "days", "confirmed", "kind", "ga", "w", "i.i", "ga_imputed", "o04", "DODKAT_registered", "o04_npr", "o04_kuhr", "w83", "fdeath", "ga_median", "kind_rule", "prev_outcome", "prev_key", "capped", "imputed", "blo", "bhi", "x.d", "i.d", "first", "row", "basis", "main", "i.lab", "x.lab", "i.row", "i.dn"))
