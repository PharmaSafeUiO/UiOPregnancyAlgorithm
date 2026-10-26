# Self-contained HTML report of a run (internal). Charts are inline SVG; no internet access is needed (TSD).

.h <- function(x) { x <- as.character(x); x <- gsub("&", "&amp;", x, fixed = TRUE); x <- gsub("<", "&lt;", x, fixed = TRUE); gsub(">", "&gt;", x, fixed = TRUE) }
.fmt <- function(x, d = 0) ifelse(is.na(x), "\u2013", formatC(x, format = "f", digits = d, big.mark = ","))

.html_table <- function(df, cls = "") {
  hd <- paste0("<th>", .h(names(df)), "</th>", collapse = "")
  rows <- vapply(seq_len(nrow(df)), function(i) paste0("<tr>", paste0("<td>", .h(unlist(lapply(df[i, ], as.character))), "</td>", collapse = ""), "</tr>"), "")
  paste0('<div class="tw"><table class="', cls, '"><thead><tr>', hd, "</tr></thead><tbody>", paste(rows, collapse = ""), "</tbody></table></div>")
}

# small line chart (one outcome by year)
.svg_line <- function(x, y, lab_y, col, mask) {
  w <- 300; h <- 150; l <- 44; r <- 10; t <- 12; b <- 26
  if (!length(x)) return('<p class="muted">No pregnancies.</p>')
  ymax <- max(1, y); xr <- range(x); if (xr[1] == xr[2]) xr <- xr + c(-0.5, 0.5)
  sx <- function(v) l + (v - xr[1]) / (xr[2] - xr[1]) * (w - l - r)
  sy <- function(v) h - b - v / ymax * (h - t - b)
  ticks <- pretty(c(0, ymax), 3); ticks <- ticks[ticks <= ymax * 1.001 & ticks == round(ticks)]
  grid <- paste0(sprintf('<line class="grid" x1="%d" x2="%d" y1="%.1f" y2="%.1f"/><text class="ax" x="%d" y="%.1f" text-anchor="end">%s</text>',
                         l, w - r, sy(ticks), sy(ticks), l - 4, sy(ticks) + 3, formatC(ticks, format = "d", big.mark = ",")), collapse = "")
  xt <- pretty(xr, 5); xt <- xt[xt >= xr[1] & xt <= xr[2] & xt == round(xt)]
  xax <- paste0(sprintf('<text class="ax" x="%.1f" y="%d" text-anchor="middle">%d</text>', sx(xt), h - 8, as.integer(xt)), collapse = "")
  pts <- paste(sprintf("%.1f,%.1f", sx(x), sy(y)), collapse = " ")
  dots <- paste0(sprintf('<circle cx="%.1f" cy="%.1f" r="2.6" style="fill:%s"><title>%d: %s</title></circle>', sx(x), sy(y), col, as.integer(x), .uio_mask(y, mask)), collapse = "")
  sprintf('<svg viewBox="0 0 %d %d" role="img" aria-label="%s">%s%s<polyline points="%s" style="fill:none;stroke:%s;stroke-width:2"/>%s</svg>',
          w, h, .h(lab_y), grid, xax, pts, col, dots)
}

# histogram of gestational age by completed week, with the median
.svg_hist <- function(ga, col, mask) {
  w <- 300; h <- 150; l <- 44; r <- 10; t <- 12; b <- 26
  if (!length(ga)) return('<p class="muted">No pregnancies.</p>')
  wk <- floor(ga / 7); lo <- 0; hi <- max(44, max(wk))
  n <- tabulate(wk - lo + 1, nbins = hi - lo + 1)
  ymax <- max(1, n); bw <- (w - l - r) / length(n)
  sx <- function(v) l + (v - lo) * bw
  sy <- function(v) h - b - v / ymax * (h - t - b)
  ticks <- pretty(c(0, ymax), 3); ticks <- ticks[ticks <= ymax * 1.001 & ticks == round(ticks)]
  grid <- paste0(sprintf('<line class="grid" x1="%d" x2="%d" y1="%.1f" y2="%.1f"/><text class="ax" x="%d" y="%.1f" text-anchor="end">%s</text>',
                         l, w - r, sy(ticks), sy(ticks), l - 4, sy(ticks) + 3, formatC(ticks, format = "d", big.mark = ",")), collapse = "")
  bars <- paste0(sprintf('<rect x="%.1f" y="%.1f" width="%.1f" height="%.1f" style="fill:%s"><title>week %d: %s</title></rect>',
                         sx(seq_along(n) - 1 + lo), sy(n), max(bw - 0.6, 0.6), h - b - sy(n), col, seq_along(n) - 1 + lo, .uio_mask(n, mask))[n > 0], collapse = "")
  xt <- c(0, 8, 12, 22, 28, 37, 42); xt <- xt[xt <= hi]
  xax <- paste0(sprintf('<text class="ax" x="%.1f" y="%d" text-anchor="middle">%d</text>', sx(xt) + bw / 2, h - 8, xt), collapse = "")
  md <- stats::median(ga) / 7
  med <- sprintf('<line x1="%.1f" x2="%.1f" y1="%d" y2="%d" class="med"/>', sx(md) + bw / 2, sx(md) + bw / 2, t, h - b)
  sprintf('<svg viewBox="0 0 %d %d" role="img" aria-label="Gestational age histogram">%s%s%s%s</svg>', w, h, grid, bars, xax, med)
}

# horizontal 100 % stacked bars: one row per label, categories with colours
.svg_stack <- function(m, cols, mask) {
  # m: matrix rows = labels, columns = categories (counts)
  w <- 760; rowh <- 24; l <- 170; r <- 70; t <- 6
  h <- t + nrow(m) * rowh + 6
  out <- character(0)
  for (i in seq_len(nrow(m))) {
    tot <- sum(m[i, ]); y <- t + (i - 1) * rowh
    out <- c(out, sprintf('<text class="lab" x="%d" y="%.1f" text-anchor="end">%s</text>', l - 8, y + 15, .h(rownames(m)[i])))
    x <- l
    if (tot > 0) for (j in seq_len(ncol(m))) {
      wd <- m[i, j] / tot * (w - l - r)
      if (wd > 0) out <- c(out, sprintf('<rect x="%.1f" y="%.1f" width="%.1f" height="%d" style="fill:%s"><title>%s, %s: %s (%.1f%%)</title></rect>',
                                        x, y + 3, wd, rowh - 8, cols[j], .h(rownames(m)[i]), .h(colnames(m)[j]), .uio_mask(m[i, j], mask), 100 * m[i, j] / tot))
      x <- x + wd
    }
    out <- c(out, sprintf('<text class="ax" x="%d" y="%.1f">n = %s</text>', w - r + 6, y + 15, .uio_mask(tot, mask)))
  }
  leg <- paste0(sprintf('<span class="key"><i style="background:%s"></i>%s</span>', cols, .h(colnames(m))), collapse = "")
  sprintf('<div class="legend">%s</div><svg viewBox="0 0 %d %d" role="img" aria-label="Stacked bars">%s</svg>', leg, w, h, paste(out, collapse = ""))
}

.uio_html_report <- function(d, avail, settings, notes, file, mask = 5) {
  oc <- .uio_outcome_table(d)
  cols <- c("var(--o1)", "var(--o2)", "var(--o3)", "var(--o4)", "var(--o5)", "var(--o6)")
  version <- tryCatch(format(utils::packageVersion("UiOPregnancyAlgorithm")), error = function(e) "")
  yrs <- range(d$year_outcome, na.rm = TRUE)
  m <- function(x) .uio_mask(x, mask)

  # tiles
  tiles <- paste0(sprintf('<div class="tile"><div class="tn">%s</div><div class="tl"><i style="background:%s"></i>%s</div></div>',
                          m(oc$n), cols, .h(oc$outcome)), collapse = "")

  # outcome table
  ot <- oc[, .(Outcome = outcome, n = m(n), `%` = .fmt(pct, 1), `Median GA (days)` = .fmt(ga_median), `IQR (days)` = ifelse(n > 0, paste0(ga_q1, "\u2013", ga_q3), "\u2013"),
               `Range (days)` = ifelse(n > 0, paste0(ga_min, "\u2013", ga_max), "\u2013"), `Median GA (weeks+days)` = ifelse(n > 0, paste0(ga_median %/% 7, "+", round(ga_median %% 7)), "\u2013"),
               Recorded = m(recorded), Estimated = m(estimated), Fixed = m(fixed))]

  # by year
  by_year <- d[, .N, by = .(year_outcome, outcome_final)]
  lines <- paste0(vapply(1:6, function(k) {
    z <- by_year[outcome_final == k][order(year_outcome)]
    allx <- seq(yrs[1], yrs[2]); y <- z$N[match(allx, z$year_outcome)]; y[is.na(y)] <- 0
    sprintf('<figure class="sm"><figcaption><i style="background:%s"></i>%s</figcaption>%s</figure>', cols[k], .h(oc$outcome[k]),
            if (sum(y)) .svg_line(allx, y, oc$outcome[k], cols[k], mask) else '<p class="muted">No pregnancies.</p>')
  }, ""), collapse = "")
  yt <- data.table::dcast(by_year, year_outcome ~ outcome_final, value.var = "N", fill = 0L)
  for (k in 1:6) if (!as.character(k) %in% names(yt)) yt[, (as.character(k)) := 0L]
  yt <- yt[, c("year_outcome", as.character(1:6)), with = FALSE]
  data.table::setnames(yt, c("Year", oc$outcome))
  yt[, Total := rowSums(.SD), .SDcols = oc$outcome]
  for (cc in setdiff(names(yt), "Year")) yt[, (cc) := m(get(cc))]

  # by maternal age
  brk <- c(-Inf, 19, 24, 29, 34, 39, 44, Inf); labs <- c("<20", "20\u201324", "25\u201329", "30\u201334", "35\u201339", "40\u201344", "45+")
  d2 <- data.table::copy(d)[, age_group := as.character(cut(mother_age, brk, labs))][is.na(age_group), age_group := "unknown"]
  am <- data.table::dcast(d2[, .N, by = .(age_group, outcome_final)], age_group ~ outcome_final, value.var = "N", fill = 0L)
  for (k in 1:6) if (!as.character(k) %in% names(am)) am[, (as.character(k)) := 0L]
  am <- am[match(intersect(c(labs, "unknown"), am$age_group), am$age_group)]
  amat <- as.matrix(am[, as.character(1:6), with = FALSE]); rownames(amat) <- am$age_group; colnames(amat) <- oc$outcome
  at <- data.table::as.data.table(amat, keep.rownames = "Age group")
  at[, Total := rowSums(amat)]
  for (cc in setdiff(names(at), "Age group")) at[, (cc) := m(get(cc))]

  # gestational age
  hists <- paste0(vapply(1:6, function(k) {
    ga <- d[outcome_final == k, GA_event_days_final]
    sprintf('<figure class="sm"><figcaption><i style="background:%s"></i>%s%s</figcaption>%s</figure>', cols[k], .h(oc$outcome[k]),
            if (length(ga)) sprintf('<span class="cap-r">median %s days</span>', format(stats::median(ga))) else "", .svg_hist(ga, cols[k], mask))
  }, ""), collapse = "")

  # dating
  dmat <- as.matrix(oc[, .(recorded, estimated, fixed)]); rownames(dmat) <- oc$outcome; colnames(dmat) <- c("recorded (MBRN)", "estimated (marker)", "fixed")
  dmat <- dmat[rowSums(dmat) > 0, , drop = FALSE]
  dm <- d[, .N, by = .(Outcome = outcome_lable, `Dating method` = dating_method)][order(Outcome, -N)][, n := m(N)][, N := NULL]
  mk <- d[!is.na(dating_code) & dating_class == "estimated", .N, by = .(Source = dating_source, Marker = dating_code)][order(-N)][, n := m(N)][, N := NULL]

  # sources
  smat <- as.matrix(data.table::dcast(d[, .N, by = .(outcome_final, outcome_source)], outcome_final ~ outcome_source, value.var = "N", fill = 0L)[, -1, with = FALSE])
  sl <- sort(unique(d$outcome_final)); rownames(smat) <- oc$outcome[sl]
  smat <- smat[, intersect(c("MBRN", "NPR", "KUHR"), colnames(smat)), drop = FALSE]
  oc_codes <- d[!is.na(outcome_code), .N, by = .(Outcome = outcome_lable, Source = outcome_source, Code = outcome_code)][order(Outcome, -N)][, n := m(N)][, N := NULL]

  # code availability
  av <- data.table::copy(avail)[, .(Registry = registry, Type = type, `Code system` = vocab, Code = code, Priority = ifelse(is.na(priority), "", priority),
                                    `Days to start` = ifelse(is.na(days), "", days), Records = m(n_records), Women = m(n_women), Meaning = meaning)]

  st <- .uio_settings_table(settings)
  css <- '
:root{--bg:#f7f8fa;--card:#ffffff;--fg:#1d2430;--muted:#5f6b7a;--line:#dde2e8;--accent:#2f5d8a;
--o1:#3a78c2;--o2:#e07b39;--o3:#8a5cb8;--o4:#55606e;--o5:#c79a1c;--o6:#cf4459;--c1:#3a78c2;--c2:#3d9a73;--c3:#c2c8d0}
@media (prefers-color-scheme:dark){:root:not([data-theme="light"]){--bg:#11151b;--card:#181e26;--fg:#e6eaf0;--muted:#9aa6b4;--line:#2b3440;--accent:#8db7e6;
--o1:#6ea4e6;--o2:#f0995e;--o3:#b18ae0;--o4:#a3adb9;--o5:#e3bd4f;--o6:#ee6f82;color-scheme:dark}}
*{box-sizing:border-box}body{margin:0;background:var(--bg);color:var(--fg);font:15px/1.5 -apple-system,"Segoe UI",Roboto,"Helvetica Neue",Arial,sans-serif}
.wrap{max-width:1100px;margin:0 auto;padding:28px 18px 60px}
header{border-bottom:1px solid var(--line);padding-bottom:14px;margin-bottom:22px}
h1{font-size:26px;margin:0 0 4px;letter-spacing:-.01em}h2{font-size:19px;margin:34px 0 10px}h3{font-size:15px;margin:18px 0 8px}
.muted{color:var(--muted)}.meta{color:var(--muted);font-size:13.5px}
nav{display:flex;flex-wrap:wrap;gap:6px 14px;font-size:13.5px;margin-top:10px}nav a{color:var(--accent);text-decoration:none}
.tiles{display:grid;grid-template-columns:repeat(auto-fit,minmax(150px,1fr));gap:10px}
.tile{background:var(--card);border:1px solid var(--line);border-radius:8px;padding:12px 14px}
.tn{font-size:24px;font-weight:650;font-variant-numeric:tabular-nums}.tl{font-size:13px;color:var(--muted)}
i{display:inline-block;width:10px;height:10px;border-radius:2px;margin-right:6px;vertical-align:baseline}
.card{background:var(--card);border:1px solid var(--line);border-radius:8px;padding:14px 16px;margin:10px 0}
.grid6{display:grid;grid-template-columns:repeat(auto-fit,minmax(300px,1fr));gap:12px}
figure.sm{margin:0;background:var(--card);border:1px solid var(--line);border-radius:8px;padding:10px 10px 4px}
figcaption{font-size:13.5px;font-weight:600;margin-bottom:2px;display:flex;align-items:center}.cap-r{margin-left:auto;font-weight:400;color:var(--muted);font-size:12.5px}
svg{width:100%;height:auto;display:block}
.grid{stroke:var(--line);stroke-width:1}.ax{fill:var(--muted);font-size:10px;font-variant-numeric:tabular-nums}.lab{fill:var(--fg);font-size:12.5px}
.med{stroke:var(--fg);stroke-dasharray:3 3;stroke-width:1}.med-t{fill:var(--fg)}
.legend{display:flex;flex-wrap:wrap;gap:4px 14px;font-size:12.5px;margin:4px 0 6px}.key{white-space:nowrap}
.tw{overflow-x:auto}table{border-collapse:collapse;width:100%;font-size:13px;font-variant-numeric:tabular-nums}
th,td{padding:5px 8px;border-bottom:1px solid var(--line);text-align:left;vertical-align:top}th{font-weight:600;color:var(--muted);white-space:nowrap}
td:not(:first-child){white-space:nowrap}
.note{font-size:13px;color:var(--muted)}
'
  sec <- function(id, title, body, intro = "") sprintf('<section id="%s"><h2>%s</h2>%s%s</section>', id, title, if (nzchar(intro)) paste0('<p class="muted">', intro, "</p>") else "", body)
  html <- paste0(
    '<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">',
    "<title>UiO pregnancy algorithm report</title><style>", css, "</style></head><body><div class=\"wrap\">",
    "<header><h1>UiO pregnancy algorithm: results</h1>",
    sprintf('<div class="meta">Package version %s \u00b7 run %s \u00b7 outcomes %d\u2013%d \u00b7 %s pregnancies in %s women</div>',
            .h(version), format(Sys.time(), "%Y-%m-%d %H:%M"), yrs[1], yrs[2], m(nrow(d)), m(data.table::uniqueN(d$ID))),
    '<nav><a href="#outcomes">Outcomes</a><a href="#year">By year</a><a href="#age">By age</a><a href="#ga">Gestational age</a>',
    '<a href="#dating">Dating</a><a href="#sources">Sources</a><a href="#codes">Codes</a><a href="#settings">Settings</a></nav></header>',
    '<div class="tiles">', tiles, "</div>",
    sec("outcomes", "Pregnancy outcomes", .html_table(ot),
        paste0("Gestational age (GA) = outcome date \u2212 start date, in days; the start is day 0.",
               if (mask > 1) sprintf(" Counts below %d are shown as &lt;%d.", mask, mask) else "")),
    sec("year", "Pregnancies by year of the outcome", paste0('<div class="grid6">', lines, "</div>", "<h3>Table</h3>", .html_table(yt)),
        "Each panel has its own scale."),
    sec("age", "Pregnancies by the woman's age at the outcome", paste0('<div class="card">', .svg_stack(amat, cols, mask), "</div>", .html_table(at)),
        "Share of each outcome within each age group. Age: from the MBRN at the birth, otherwise from the woman's birth year estimated from her MBRN records; unknown for women without an MBRN record."),
    sec("ga", "Gestational age at the outcome", paste0('<div class="grid6">', hists, "</div>"),
        "Pregnancies by completed week of gestation (days / 7, rounded down); the dashed line is the median."),
    sec("dating", "How the pregnancies were dated", paste0('<div class="card">',
        .svg_stack(dmat, c("var(--c1)", "var(--c2)", "var(--c3)"), mask), "</div><h3>Dating methods</h3>", .html_table(dm),
        if (nrow(mk)) paste0("<h3>Pregnancy markers that set the start</h3>", .html_table(mk)) else ""),
        "Recorded: gestational age in the MBRN. Estimated: a pregnancy marker. Fixed: an outcome-specific default, the marker limit, or the O35 rule."),
    sec("sources", "Sources of the outcomes", paste0('<div class="card">', .svg_stack(smat, c("var(--c1)", "var(--c2)", "var(--o2)"), mask), "</div>",
        "<h3>Codes that decided the outcome</h3>", .html_table(oc_codes)),
        "The registry that decided each outcome (MBRN before NPR before KUHR), and the code on the outcome date."),
    sec("codes", "Codes in NPR and KUHR", .html_table(av),
        "Records of the pregnancy markers (with the days from the record to the pregnancy start) and of the outcome codes, from the first year of the run."),
    sec("settings", "Settings", paste0(.html_table(st), if (length(notes)) paste0('<p class="note">', paste(.h(notes), collapse = "<br>"), "</p>") else "")),
    '<p class="note">Nordeng HME, Lupattelli A, Engjom HM, van Gelder MMHJ. Pharmacoepidemiol Drug Saf. 2024;33(9):e70002.</p>',
    "</div></body></html>")
  writeLines(html, file, useBytes = TRUE)
  invisible(file)
}
