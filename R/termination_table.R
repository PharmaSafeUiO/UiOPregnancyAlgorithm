#' Gestational age of induced abortions in Norway by year
#'
#' A year table for `induced_abortion_default_days` (see [uio_default_settings()]): the median
#' gestational age of induced abortions in Norway in each year 2015-2024, from the Registry of
#' Pregnancy Termination. It is not the default: by default every undated induced abortion gets one
#' fixed length (`induced_abortion_default_days = 49`).
#'
#' **Source.** Norwegian Institute of Public Health (FHI), *Rapport om svangerskapsavbrot for 2024*
#' (corrected May 2026), Tabell 6: induced abortions by gestational week group (0-6, 7-8, 9-10, ...
#' weeks). The registry counts weeks ordinally (week `n` = days `7(n-1)` to `7n-1`), so weeks 7-8
#' are days 42-55.
#'
#' **Method.** In every year the median lies in weeks 7-8. It is interpolated linearly within that
#' group, `42 + 14 * (N / 2 - n[0-6]) / n[7-8]`, where `N` excludes abortions with unknown gestational
#' age, and rounded to whole days (`days`; the unrounded value is in `median_days`). The interpolation
#' assumes that abortions are spread evenly over days 42-55, so the values are approximate.
#'
#' **Years outside 2015-2024 are extrapolated.** The table carries the attribute
#' `extrapolate = TRUE`. For an outcome year before 2015 or after 2024, the value comes from the
#' linear trend of `days` over the listed years (least squares), rounded to whole days and kept within
#' the range of the table (44-47 days), so that the trend does not run away. [run_uio_pa()] then
#' warns which years were extrapolated and with which values, and adds a note to the summary and the
#' report. An extrapolated value is an assumption, not registry data.
#'
#' To go back to the fixed default, leave out `induced_abortion_default_days` (49 days) or give it
#' one number of days. A table of your own (data frame with `year` and `days`) is used without
#' extrapolation (a year that is not listed takes the closest earlier listed year), unless you set
#' `attr(tab, "extrapolate") <- TRUE`.
#'
#' @return A data frame with columns `year`, `days` (whole days) and `median_days`, and the
#'   attribute `extrapolate = TRUE`.
#' @examples
#' uio_termination_days_by_year()
#' \dontrun{
#' run_uio_pa(workdir = wd, source_files = src,
#'            induced_abortion_default_days = uio_termination_days_by_year())
#' }
#' @export
uio_termination_days_by_year <- function() {
  n <- data.frame(year    = 2015:2024,
                  total   = c(14033, 13193, 12750, 12391, 11734, 11089, 10874, 12009, 12842, 12840),
                  w0_6    = c( 5202,  4866,  4890,  4984,  4810,  4924,  4802,  5317,  5485,  5309),
                  w7_8    = c( 5590,  5296,  5012,  4777,  4499,  4026,  3944,  4274,  4737,  4786),
                  unknown = c(    0,    11,     0,     0,     1,     0,     2,     7,   101,    83))
  med <- 42 + 14 * ((n$total - n$unknown) / 2 - n$w0_6) / n$w7_8
  out <- data.frame(year = n$year, days = floor(med + 0.5), median_days = round(med, 1))
  attr(out, "extrapolate") <- TRUE
  out
}

# Values of a year table for years outside its range (internal): linear trend of `days` over the
# listed years, rounded to whole days, kept within the range of the listed values.
.uio_extrapolate_days <- function(x, years) {
  if (nrow(x) < 2L) return(rep(x$days[1], length(years)))
  xm <- mean(x$year); ym <- mean(x$days)
  slope <- sum((x$year - xm) * (x$days - ym)) / sum((x$year - xm)^2)
  v <- floor(ym + slope * (years - xm) + 0.5)
  pmin(pmax(v, min(x$days)), max(x$days))
}

# Notes on extrapolated years (internal): one per setting, years with the same value as ranges.
.uio_extrapolation_notes <- function(extrapolated) {
  if (!length(extrapolated)) return(character(0))
  vapply(names(extrapolated), function(nm) {
    e <- extrapolated[[nm]]
    grp <- cumsum(c(TRUE, diff(e$year) != 1 | diff(e$days) != 0))
    parts <- vapply(split(e, grp), function(g)
      paste0(if (nrow(g) > 1) paste0(min(g$year), "-", max(g$year)) else g$year, ": ", g$days[1], " days"), "")
    fixed <- uio_default_settings()[[nm]]
    paste0("`", nm, "`: the outcome years ", paste(parts, collapse = "; "), " are outside the year table, so ",
           "their values are EXTRAPOLATED from its trend (an assumption, not registry data). To use the fixed default ",
           "instead, leave out `", nm, "` (", fixed, " days) or give it one number of days.")
  }, "", USE.NAMES = FALSE)
}
