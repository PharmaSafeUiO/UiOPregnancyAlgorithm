# Description of the identified pregnancies, code availability, run summary and HTML report (internal).

.uio_outcomes <- data.table::data.table(
  outcome_final = 1:6,
  outcome_lable = c("Livebirth", "Miscarriage", "Termination of pregnancy", "Stillbirth", "Molar pregnancy", "Ectopic pregnancy"),
  npr_pattern   = c(NA, "O02|O03", "O04", NA, "O01", "O00"))

.uio_mask <- function(n, k) ifelse(!is.na(n) & n > 0 & n < k, paste0("<", k), format(n, big.mark = ","))

# ---- the pregnancies with their source, codes, dating and the woman's age ------------------------------------
# Joins use whole day numbers (as.integer of the dates), so Date and IDate columns mix safely.
.uio_describe <- function(res, cap, settings, hospital_codes, primary_care_codes) {
  r <- data.table::copy(res)
  data.table::setattr(r, "uio_capture", NULL)
  r[, `:=`(rid = .I, dout = as.integer(date_outcome), dstart = as.integer(pr_start_final))]
  npr <- cap$npr[, .(ID, code = tilst, d = as.integer(inndato))]
  kuhr <- cap$kuhr[, .(ID, code = diagnose, vocab = diagnosekodeverk, d = as.integer(ant_dager))]
  mbrn <- data.table::copy(cap$mbrn)
  data.table::setnames(mbrn, "PasientLopeNr_MOR", "ID")
  mbrn[, d := as.integer(DAGER_FDATO)]
  norm <- if (isTRUE(settings$normalize_codes)) .uio_normalize_code else identity
  hc <- data.table::as.data.table(hospital_codes)[, `:=`(code = norm(code), prio = .I, source = "NPR")]
  pc <- data.table::as.data.table(primary_care_codes)[, `:=`(code = norm(code), prio = .I + 1000L, source = "KUHR")]
  outc <- "O00|O01|O02|O03|O04|O35"

  # source that decided the outcome (MBRN > NPR > KUHR; KUHR adds miscarriages only)
  r[, outcome_source := data.table::fcase(dating_method == "MBRN (recorded GA)", "MBRN",
                                          outcome_final %in% c(3, 5, 6), "NPR",
                                          MC_source %in% 7L, "KUHR",
                                          default = "NPR")]
  r[, dating_class := data.table::fcase(dating_method == "MBRN (recorded GA)", "recorded",
                                        dating_method == "pregnancy marker", "estimated",
                                        default = "fixed")]
  r[, year_outcome := as.integer(data.table::year(date_outcome))]

  # MBRN: vital status (DODKAT) of the children born on the outcome date, and the mother's age then
  mb <- mbrn[r[outcome_source == "MBRN", .(rid, ID, d = dout)], on = .(ID, d), nomatch = 0L]
  if (nrow(mb)) {
    mb_codes <- mb[, .(codes_mbrn = paste0("DODKAT ", paste(sort(unique(DODKAT), na.last = TRUE), collapse = "/")),
                       age_mbrn = suppressWarnings(as.numeric(MORS_ALDER[1]))), by = rid]
    r[mb_codes, on = "rid", `:=`(codes_mbrn = i.codes_mbrn, age_mbrn = i.age_mbrn)]
  }
  if (!"age_mbrn" %in% names(r)) r[, age_mbrn := NA_real_]

  # codes of the episode: NPR and KUHR records from the start up to same_event_days - 1 days after the outcome
  win <- r[, .(rid, ID, lo = dstart, hi = dout + as.integer(settings$same_event_days) - 1L, dout, outcome_final)]
  rel_npr <- npr[grepl(outc, code, perl = TRUE) | code %in% hc$code]
  rel_kuhr <- kuhr[(vocab == "ICD-10" & (grepl(outc, code, perl = TRUE) | code %in% pc[vocab == "ICD-10", code])) |
                   (vocab == "ICPC-2" & (grepl("W82", code, fixed = TRUE) | code %in% pc[vocab == "ICPC-2", code]))]
  en <- rel_npr[win, on = .(ID, d >= lo, d <= hi), nomatch = 0L, .(rid = i.rid, code = x.code, d = x.d, dout = i.dout, outcome_final = i.outcome_final)]
  ek <- rel_kuhr[win, on = .(ID, d >= lo, d <= hi), nomatch = 0L,
                 .(rid = i.rid, code = x.code, vocab = x.vocab, d = x.d, dout = i.dout, outcome_final = i.outcome_final)]
  if (nrow(en)) r[en[, .(v = paste(sort(unique(code)), collapse = ", ")), by = rid], on = "rid", codes_npr := i.v]
  if (nrow(ek)) r[ek[, .(v = paste(sort(unique(code)), collapse = ", ")), by = rid], on = "rid", codes_kuhr := i.v]

  # code that decided the outcome: the outcome record on the outcome date
  pat <- c(`2` = "O02|O03", `3` = "O04", `5` = "O01", `6` = "O00")
  dn <- en[d == dout & as.character(outcome_final) %in% names(pat)]
  if (nrow(dn)) {
    dn <- dn[mapply(function(p, x) grepl(p, x, perl = TRUE), pat[as.character(outcome_final)], code)]
    if (nrow(dn)) r[dn[, .(v = min(code)), by = rid], on = "rid", oc_npr := i.v]
  }
  dk <- ek[d == dout & outcome_final == 2 & ((vocab == "ICD-10" & grepl("O02|O03", code)) | (vocab == "ICPC-2" & grepl("W82", code, fixed = TRUE)))]
  if (nrow(dk)) r[dk[, .(v = min(code)), by = rid], on = "rid", oc_kuhr := i.v]
  for (cc in c("codes_mbrn", "codes_npr", "codes_kuhr", "oc_npr", "oc_kuhr", "dating_source", "dating_code"))
    if (!cc %in% names(r)) r[, (cc) := NA_character_]
  r[, outcome_code := data.table::fcase(outcome_source == "MBRN", codes_mbrn, outcome_source == "NPR", oc_npr,
                                        outcome_source == "KUHR", oc_kuhr, default = NA_character_)]
  r[, c("oc_npr", "oc_kuhr") := NULL]

  # dating source and code: the marker that set the start (NPR list first, then KUHR; list order), or the O35 record
  mk <- r[dating_method == "pregnancy marker", .(rid, ID, dout, lo = dout - as.integer(settings$marker_lookback_days))]
  cand <- rbind(npr[hc, on = "code", nomatch = 0L, .(ID, code, d, days, prio, source)],
                kuhr[pc, on = .(code, vocab), nomatch = 0L, .(ID, code, d, days, prio, source)])
  if (nrow(mk) && nrow(cand)) {
    m <- cand[mk, on = .(ID, d >= lo, d <= dout), nomatch = 0L, .(rid = i.rid, code = x.code, d = x.d, days, prio, source, dout = i.dout)]
    m <- m[d >= dout - settings$marker_rejection_days]
    m <- m[order(rid, code, d)][, .SD[1], by = .(rid, code)]                     # earliest record of each code
    m <- m[(dout - (d - days)) < settings$marker_rejection_days]                    # accepted
    m <- m[order(rid, prio)][, .SD[1], by = rid]
    if (nrow(m)) r[m, on = "rid", `:=`(dating_source = i.source, dating_code = i.code)]
  }
  o35 <- r[grepl("O35", dating_method, fixed = TRUE), .(rid, ID, dout, hi = dout + as.integer(settings$o35_window_days))]
  if (nrow(o35)) {
    c35 <- rbind(npr[grepl("O35", code, fixed = TRUE), .(ID, code, d, source = "NPR", p = 1L)],
                 kuhr[vocab == "ICD-10" & grepl("O35", code, fixed = TRUE), .(ID, code, d, source = "KUHR", p = 2L)])
    if (nrow(c35)) {
      m <- c35[o35, on = .(ID, d >= dout, d <= hi), nomatch = 0L, .(rid = i.rid, code = x.code, source, p, d = x.d)]
      m <- m[order(rid, p, d)][, .SD[1], by = rid]
      if (nrow(m)) r[m, on = "rid", `:=`(dating_source = i.source, dating_code = i.code)]
    }
  }
  r[dating_method == "MBRN (recorded GA)", dating_source := "MBRN"]
  r[is.na(dating_source) & dating_class == "fixed", dating_source := "default"]

  # age of the woman: MBRN age at this birth; otherwise from her birth year estimated from her MBRN records
  mbrn[, .age := suppressWarnings(as.numeric(MORS_ALDER))]
  by_ <- mbrn[!is.na(.age) & .age > 0, .(birth_year_est = stats::median(data.table::year(DAGER_FDATO) - .age)), by = ID]
  if (nrow(by_)) r[by_, on = "ID", birth_year_est := i.birth_year_est]
  if (!"birth_year_est" %in% names(r)) r[, birth_year_est := NA_real_]
  r[, mother_age := data.table::fifelse(!is.na(age_mbrn), age_mbrn, round(year_outcome - birth_year_est))]
  r[, c("age_mbrn", "birth_year_est", "rid", "dout", "dstart") := NULL]
  data.table::setcolorder(r, c("ID", "preg_id", "outcome_final", "outcome_lable", "date_outcome", "year_outcome", "pr_start_final",
                               "GA_event_days_final", "GA_event_week_final", "dating_class", "dating_method", "dating_source", "dating_code",
                               "truncated_at_previous_outcome", "outcome_source", "outcome_code", "codes_mbrn", "codes_npr", "codes_kuhr",
                               "MC_source", "TOP_source", "mother_age"))
  data.table::setorder(r, ID, date_outcome)
  r[]
}

# ---- availability of the codes in NPR and KUHR -----------------------------------------------------------------
.uio_code_availability <- function(cap, settings, hospital_codes, primary_care_codes) {
  norm <- if (isTRUE(settings$normalize_codes)) .uio_normalize_code else identity
  npr <- cap$npr; kuhr <- cap$kuhr
  cnt <- function(d, sel) data.table::data.table(n_records = sum(sel), n_women = data.table::uniqueN(d$ID[sel]))
  out <- list()
  hc <- data.table::as.data.table(hospital_codes); pc <- data.table::as.data.table(primary_care_codes)
  for (i in seq_len(nrow(hc))) out[[length(out) + 1]] <- cbind(data.table::data.table(registry = "NPR", type = "marker", vocab = hc$vocab[i],
    code = hc$code[i], priority = i, days = hc$days[i]), cnt(npr, npr$tilst == norm(hc$code[i])))
  for (i in seq_len(nrow(pc))) out[[length(out) + 1]] <- cbind(data.table::data.table(registry = "KUHR", type = "marker", vocab = pc$vocab[i],
    code = pc$code[i], priority = i, days = pc$days[i]), cnt(kuhr, kuhr$diagnosekodeverk == pc$vocab[i] & kuhr$diagnose == norm(pc$code[i])))
  oc <- data.table::data.table(code = c("O00", "O01", "O02", "O03", "O04", "O35"),
                               meaning = c("ectopic pregnancy", "molar pregnancy", "missed abortion", "spontaneous abortion",
                                           "induced abortion", "maternal care for fetal abnormality (O35 rule)"))
  for (i in seq_len(nrow(oc))) {
    out[[length(out) + 1]] <- cbind(data.table::data.table(registry = "NPR", type = "outcome", vocab = "ICD-10", code = oc$code[i],
      priority = NA_integer_, days = NA_real_), cnt(npr, grepl(oc$code[i], npr$tilst, fixed = TRUE)))
    if (oc$code[i] %in% c("O02", "O03", "O35"))
      out[[length(out) + 1]] <- cbind(data.table::data.table(registry = "KUHR", type = "outcome", vocab = "ICD-10", code = oc$code[i],
        priority = NA_integer_, days = NA_real_), cnt(kuhr, kuhr$diagnosekodeverk == "ICD-10" & grepl(oc$code[i], kuhr$diagnose, fixed = TRUE)))
  }
  out[[length(out) + 1]] <- cbind(data.table::data.table(registry = "KUHR", type = "outcome", vocab = "ICPC-2", code = "W82",
    priority = NA_integer_, days = NA_real_), cnt(kuhr, kuhr$diagnosekodeverk == "ICPC-2" & grepl("W82", kuhr$diagnose, fixed = TRUE)))
  a <- data.table::rbindlist(out)
  a[type == "outcome", meaning := c(oc$meaning, "spontaneous abortion")[match(code, c(oc$code, "W82"))]]
  a[type == "marker", meaning := paste0("start = record date - ", days, " days")]
  a[]
}

# ---- tables used by the summary and the report --------------------------------------------------------------------
.uio_outcome_table <- function(d) {
  o <- data.table::copy(.uio_outcomes)[, .(outcome_final, outcome = outcome_lable)]
  s <- d[!is.na(outcome_final), .(n = .N, ga_median = as.numeric(stats::median(GA_event_days_final)),
             ga_q1 = round(as.numeric(stats::quantile(GA_event_days_final, 0.25, names = FALSE))),
             ga_q3 = round(as.numeric(stats::quantile(GA_event_days_final, 0.75, names = FALSE))),
             ga_min = as.numeric(min(GA_event_days_final)), ga_max = as.numeric(max(GA_event_days_final)),
             recorded = sum(dating_class == "recorded"), estimated = sum(dating_class == "estimated"), fixed = sum(dating_class == "fixed")),
         by = outcome_final]
  o <- merge(o, s, by = "outcome_final", all.x = TRUE)
  o[is.na(n), `:=`(n = 0L, recorded = 0L, estimated = 0L, fixed = 0L)]
  o[, pct := if (sum(n)) 100 * n / sum(n) else 0]
  o[order(outcome_final)]
}

.uio_settings_table <- function(settings) {
  val <- vapply(settings, function(v) {
    if (is.null(v)) "NULL" else if (is.data.frame(v)) paste0("table by year ", min(v$year), "-", max(v$year),
      if (isTRUE(attr(v, "extrapolate"))) " (extrapolated outside)" else "") else paste(format(v), collapse = ", ")
  }, "")
  data.table::data.table(setting = names(settings), value = unname(val))
}

# ---- summary printed at the end of the run ----------------------------------------------------------------------
.uio_print_summary <- function(d, avail, settings, notes) {
  line <- function(...) cat(..., "\n", sep = "")
  rule <- function(t) line("\n== ", t, " ", strrep("=", max(3, 70 - nchar(t))))
  rule("Codes available in NPR and KUHR (records / women)")
  a <- data.table::copy(avail)
  a[, txt := sprintf("%-5s %-8s %-7s %-6s %12s %10s  %s", registry, type, vocab, code, format(n_records, big.mark = ","),
                     format(n_women, big.mark = ","), meaning)]
  line(sprintf("%-5s %-8s %-7s %-6s %12s %10s  %s", "", "", "", "code", "records", "women", "meaning / days to start"))
  for (x in a$txt) line(x)
  rule("Settings")
  s <- .uio_settings_table(settings)
  for (i in seq_len(nrow(s))) line(sprintf("  %-38s %s", s$setting[i], s$value[i]))
  for (n in notes) line("  note: ", n)
  rule("Pregnancy outcomes")
  o <- .uio_outcome_table(d)
  line(sprintf("  %-26s %10s %7s %12s %15s", "outcome", "n", "%", "median GA", "IQR (days)"))
  for (i in seq_len(nrow(o))) line(sprintf("  %-26s %10s %6.1f%% %12s %15s", o$outcome[i], format(o$n[i], big.mark = ","), o$pct[i],
    if (o$n[i]) format(o$ga_median[i]) else "-", if (o$n[i]) paste0(o$ga_q1[i], "-", o$ga_q3[i]) else "-"))
  line(sprintf("  %-26s %10s", "total", format(sum(o$n), big.mark = ",")))
  invisible(o)
}
