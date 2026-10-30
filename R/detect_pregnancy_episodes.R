# UiO pregnancy algorithm, version 2.0.0 (R package UiOPregnancyAlgorithm)
#
# The additions for MBRN records without gestational age are marked "[experimental]":
# MBRN single births without gestational age are kept with an imputed gestational age (the data median of
# their kind: termination, stillbirth or late spontaneous abortion; live births 280 days; not earlier than
# missing_ga_min_gap_days after the woman's previous outcome; missing_ga_impute), outcome_lable_new separates
# late spontaneous abortions (MBRN) from spontaneous abortions (NPR/KUHR), and the MBRN records without
# gestational age and the NPR/KUHR non-live records are kept for the missing-GA report (R/missing_ga.R).
#
# This file holds the algorithm function `detect_pregnancy_episodes()` copied verbatim from
# version 1.3 (tests/regression/reference/UiO_preg_Algo_v1.3.R, lines 158-1510). The only edits are
# marked "[v1.3.1]", "[v1.4.0]" and "[v2.0.0]"; uio_v13_settings() gives the settings of v1.3.
# In addition, the three `sourceCpp(code = ...)` calls are now `Rcpp::sourceCpp(env = environment(),
# code = ...)` (same C++ code): the compiled helpers are created in this function's environment
# instead of the user's global environment. Short comments describing each block were added in Round 3
# (comments only; the code is unchanged).
#
# Symbol resolution: the v1.3 script was run with library(Rcpp); library(data.table);
# library(lubridate); library(stringr) attached in that order. The package NAMESPACE imports
# exactly the objects the function resolved through those packages (e.g. year() from lubridate,
# which masks data.table::year), so the package does not depend on the user's search path.

#' Detect pregnancy episodes (UiO algorithm v1.3; internal)
#'
#' Internal workhorse called by [run_uio_pa()]. It must be called with the working directory set
#' to the folder that holds (or will hold) the cached source extracts `birth_registry_full.csv`,
#' `hospital_full.csv` and `primary_care_full.csv`.
#'
#' @param source_files List of source definitions, see [uio_source_files()]; `hospital$data` must
#'   already hold the prepared NPR table (see `uio_prepare_sources()`) unless `hospital_full.csv`
#'   is cached.
#' @param hospital_codes,primary_care_codes Dating markers (vocab, code, days), see
#'   [uio_hospital_codes()] and [uio_primary_care_codes()]. Row order is the priority order.
#' @param years_range Numeric vector `c(first_year, last_year)` or `NULL`.
#' @param settings Named list of settings, see [uio_default_settings()].
#' @return A data.table, one row per pregnancy.
#' @keywords internal
#' @noRd
detect_pregnancy_episodes = function(source_files, hospital_codes, primary_care_codes, years_range = NULL, settings = uio_default_settings(),
                                     nonlive_codes = uio_nonlive_codes(),     # [experimental] nonlive_codes,
                                     missing_ga_impute = TRUE, missing_ga_confirm_days = c(before = 28, after = 7),
                                     missing_ga_livebirth_days = 280,
                                     missing_ga_min_gap_days = 28, missing_ga_livebirth_min_days = 154){   # [experimental]

  # settings (see uio_default_settings(); uio_v13_settings() reproduces v1.3)
  settings = .uio_resolve_settings(settings)
  cfg_induced_abortion_default_days = settings$induced_abortion_default_days
  cfg_miscarriage_default_days      = settings$miscarriage_default_days
  cfg_molar_default_days            = settings$molar_default_days
  cfg_ectopic_default_days          = settings$ectopic_default_days
  cfg_o35_induced_days              = settings$o35_induced_days
  cfg_marker_rejection_days         = settings$marker_rejection_days
  cfg_stillbirth_min_days           = settings$stillbirth_min_days
  cfg_normalize_codes               = settings$normalize_codes
  # [v1.4.0] birth weight for stillbirth
  cfg_stillbirth_min_weight_g       = settings$stillbirth_min_weight_g
  # [v2.0.0] fixed values of v1.3 that are now settings (defaults = v1.3 values)
  cfg_marker_lookback_days          = settings$marker_lookback_days    # v1.3: 70
  cfg_same_event_days               = settings$same_event_days         # v1.3: 56
  cfg_o35_window_days               = settings$o35_window_days         # v1.3: 27
  same_event_episode_days           = cfg_same_event_days - 1          # v1.3: episode_count55
  use_weight = !is.null(cfg_stillbirth_min_weight_g)
  # [v2.0.0] without a mapped birth weight, the stillbirth rule uses gestational age alone
  if(use_weight && !"birth_weight" %in% names(source_files$birth_registry$variables)){
    cli::cli_alert_warning("Birth weight is not mapped (`birth_weight` in uio_source()): stillbirth by gestational age only.")
    use_weight = FALSE
  }
  # [v1.3.1] normalize_codes also normalises the code column of the marker lists (a no-op for the
  # default lists, which are already upper case without separators)
  if(isTRUE(cfg_normalize_codes)){
    hospital_codes$code = .uio_normalize_code(hospital_codes$code)
    primary_care_codes$code = .uio_normalize_code(primary_care_codes$code)
  }

  # Years of the outcomes kept (years_range); NULL = no restriction
  min_year = -Inf
  max_year = +Inf

  if(!is.null(years_range))
    min_year = years_range[1]

  if(!is.null(years_range))
    max_year = years_range[2]

  # Avoiding 'by = ID' computations when a variable is updated by its lagged value
  shift_wrt_id <-function(x,id, ...){
    #shift(x) by id
    #data assumed to be sorted already by id
    y = shift(x, ...)
    y[!equal(id,shift(id, ...))] = NA
    y
  }

  # Copy the previous value wherever cond is TRUE (used to give the children of one pregnancy the same preg_id)
  shift_with_condition <- function(cond, values){
    #Assuming cond[1] is FALSE
    for(i in seq_along(cond))
      values[i] = ifelse(is.na(cond[i]) || !cond[i], values[i],values[i-1])
    values
  }


  # #Mimic STATA behavior in == and != operators
  # The difference between STATA and R is currently handled thoroughly in the R script.
  equal <- function(x, y) {
    fcase(
      is.na(x) & is.na(y), TRUE,
      is.na(x) | is.na(y), FALSE,
      rep(TRUE,length(x)), x == y
    )
  }

  # Read every file matching the pattern (dta, sav, csv/txt or tab-separated dsv/tsv) and row-bind them
  smart_load <- function(file,variables=NULL,...){
    files= fs::dir_ls(path=dirname(file),regexp = basename(file))
    if(!length(files)) stop("No file matches ", file, call. = FALSE)   # [v2.0.0]
    # [v2.0.0] the requested columns are checked in each file before it is read: a column whose name differs
    # only in upper/lower case is used (and renamed to the requested name); a missing column is an error that
    # names the file and lists its columns (v1.3: fread warned and skipped the column, and setnames failed later)
    match_vars = function(have, f){
      out = variables
      miss = !variables %in% have
      for(j in which(miss)){
        k = which(tolower(have) == tolower(variables[j]))
        if(length(k) == 1L){ out[j] = have[k]; miss[j] = FALSE }
      }
      if(any(miss))
        stop("Column(s) ", paste(variables[miss], collapse = ", "), " not found in ", f, " (", length(files), " file(s) match ",
             file, "). Columns of this file: ", paste(utils::head(have, 80), collapse = ", "), if(length(have) > 80) ", ...",
             ". Change the column names in source_files (uio_source()).", call. = FALSE)
      if(any(out != variables))
        cli::cli_alert_info("{basename(f)}: column{?s} {.val {out[out != variables]}} used for {.val {variables[out != variables]}}.")
      out
    }
    ds =list()
    for(i in seq_along(files)){
      file_i=files[i]
      ext = tolower(tools::file_ext(file_i))
      if(ext%in%c("dta","sav")){
        # [v2.0.0] SPSS files; value labels are removed (codes kept)
        d = if(ext=="dta") haven::read_dta(file_i,...) else haven::read_sav(file_i,...)
        d = as.data.table(haven::zap_labels(d))
        if(!is.null(variables)){
          v = match_vars(names(d), file_i)
          d = d[, v, with = FALSE]
          setnames(d, v, variables)
        }

      }else if(ext%in%c("csv","txt")){
        if(!is.null(variables)){
          v = match_vars(names(data.table::fread(file_i, nrows = 0, ...)), file_i)
          d = data.table::fread(file_i,select = v,...)
          setnames(d, v, variables)
        }else{
          d = data.table::fread(file_i,...)
        }

      }else if(ext%in%c("dsv","tsv")){
        # [v2.0.0] tab-separated as in v1.3; a file whose header has no tab (e.g. the P1380 KUHR files, separated
        # by ";") is read with the separator that fread detects
        sep_i = "\t"
        hdr = names(data.table::fread(file_i, nrows = 0, sep = "\t", ...))
        if(length(hdr) == 1L){
          sep_i = "auto"
          hdr = names(data.table::fread(file_i, nrows = 0, ...))
        }
        if(!is.null(variables)){
          v = match_vars(hdr, file_i)
          d = data.table::fread(file_i,select = v,sep = sep_i,...)
          setnames(d, v, variables)
        }else{
          d = data.table::fread(file_i,sep = sep_i,...)
        }

      }else{
        stop("file type is not supported!")
      }
      ds[[i]]=d
    }

    d=rbindlist(ds,fill = TRUE)
    return(d)
  }

  npr_codes = hospital_codes
  kuhr_codes = primary_care_codes
  if(!"data.table"%in%class(npr_codes))
    npr_codes = as.data.table(npr_codes)

  if(!"data.table"%in%class(kuhr_codes))
    kuhr_codes = as.data.table(kuhr_codes)

  # Code patterns of the records used: dating markers and the outcome codes O00-O04 and O35 (ICD-10),
  # plus the KUHR ICPC-2 markers and W82 (miscarriage)
  npr_pattern = paste0(c(npr_codes[vocab=="ICD-10", code], c("O00","O01", "O02", "O03", "O04","O35")), collapse = "|")
  kuhr_pattern = c("ICD-10" = paste0(c(kuhr_codes[vocab=="ICD-10", code], c("O00","O01", "O02", "O03", "O04","O35")), collapse = "|"),
                   "ICPC-2" = paste0(c(kuhr_codes[vocab=="ICPC-2", code], "W82"), collapse = "|"))

  # [v2.0.0] text read from Latin-1 files (e.g. the letter O-slash, byte D8, in the P1380 KUHR files) is not valid in a UTF-8 session, and the
  # regular expressions below fail on it ("input string ... is invalid"): such strings are converted from Latin-1 to
  # UTF-8 (in memory; the cached CSV files keep the bytes as read)
  fix_encoding = function(v){
    if(!is.character(v)) return(v)
    bad = !is.na(v) & !validUTF8(v)
    if(any(bad)) v[bad] = iconv(v[bad], from = "latin1", to = "UTF-8")
    v
  }

  # [v2.0.0] memory: an NPR or KUHR extract can hold every diagnosis of every woman (P704: about 100 million NPR
  # rows), and each full-length step below (encoding, normalisation, dates, year limit) made a copy of it. Right
  # after reading, only the rows whose code matches a pattern of the code filter below are kept (the steps before
  # that filter work row by row, so the result is the same). The codes are tested in blocks of 5 million rows, on
  # the distinct codes of each block.
  code_rows = function(code, vocab, patterns){
    keep = logical(length(code))
    for(s in seq.int(1, by = 5e6, length.out = ceiling(length(code) / 5e6))){
      i = s:min(length(code), s + 5e6 - 1)
      cd = fix_encoding(code[i])
      if(isTRUE(cfg_normalize_codes)) cd = .uio_normalize_code(cd)
      cd = as.character(cd)
      v = fix_encoding(as.character(vocab[i]))
      u = unique(cd)
      k = logical(length(i))
      for(nm in names(patterns))
        k = k | (!is.na(v) & v == nm & cd %in% u[grepl(patterns[[nm]], u, perl = TRUE)])
      keep[i] = k
    }
    keep
  }
  cli::cli_alert_info("Loading source data")

  ################# Preloading Data ####################
  # Reloading data to speed up running the script
  # MBRN, NPR and KUHR are read once, renamed to the v1.3 column names, limited by birth year and cached
  # as *_full.csv in the work folder; later runs read the cached files.
  # [v1.4.0] the birth weight is read only when stillbirth_min_weight_g is set; a cached file without
  # it is then read again from the source and cached with the weight column
  mbrn_var_map = source_files$birth_registry$variables
  if(!use_weight) mbrn_var_map = mbrn_var_map[names(mbrn_var_map) != "birth_weight"]
  mbrn_cache_ok = file.exists("birth_registry_full.csv") &&
    (!use_weight || "VEKT" %in% names(fread("birth_registry_full.csv", nrows = 0)))
  if(!mbrn_cache_ok){
    mbrn_vars = unname(mbrn_var_map)
    if(is.null(source_files$birth_registry$data)){
      mbrn = smart_load(source_files$birth_registry$file, variables = mbrn_vars)
    }else{
      mbrn = source_files$birth_registry$data
      if(!"data.table" %in% class(mbrn))
        mbrn = as.data.table(mbrn)
      mbrn = mbrn[,..mbrn_vars]
    }


    # [v2.0.0] the columns are renamed to the v1.3 names; the current MBRN variables (vital status
    # leve_dodstatus_fodsel, gestational age in weeks) are converted, see uio_source()
    mbrn = .uio_standardise_mbrn(mbrn, mbrn_var_map)

    # [v2.0.0] && (a NULL limit was an error with &), and the birth year is applied only when it is a column
    if(!is.null(source_files$birth_registry$birth_year_limit) && !is.na(source_files$birth_registry$birth_year_limit[1]) &&
       !is.na(source_files$birth_registry$variables["birth_year"])){
      setnames(mbrn, source_files$birth_registry$variables["birth_year"], "birth_year")
      mbrn=mbrn[birth_year>=source_files$birth_registry$birth_year_limit[1] & birth_year<=source_files$birth_registry$birth_year_limit[2],]
    }

    fwrite(mbrn,"birth_registry_full.csv")
  }else{
    mbrn = fread("birth_registry_full.csv")
  }

  if(!file.exists("hospital_full.csv")){
    npr_vars = unname(source_files$hospital$variables)
    if(is.null(source_files$hospital$data)){
      npr = smart_load(source_files$hospital$file, variables = npr_vars)
    }else{
      npr = source_files$hospital$data
      if(!"data.table" %in% class(npr))
        npr = as.data.table(npr)
        npr = npr[,..npr_vars]
    }

    setnames(npr, source_files$hospital$variables["mother_id"], "ID")
    setnames(npr, source_files$hospital$variables["diagnose_code"], "tilst")
    setnames(npr, source_files$hospital$variables["visit_date"], "inndato")
    # [v2.0.0] stay_id is optional (it is rebuilt from the dates below); with a gender column only women are kept
    if(!is.na(source_files$hospital$variables["stay_id"])) setnames(npr, source_files$hospital$variables["stay_id"], "stay_id") else npr[, stay_id := NA_integer_]
    if(!is.na(source_files$hospital$variables["gender"])){
      fv = if(is.null(source_files$hospital$female_value)) 2 else source_files$hospital$female_value
      npr = npr[as.character(get(source_files$hospital$variables["gender"])) == as.character(fv)]
      npr[, (source_files$hospital$variables["gender"]) := NULL]
    }

    if(!is.null(source_files$hospital$variables["vocabulary"]) &&
       !is.na(source_files$hospital$variables["vocabulary"])){   # [v2.0.0] spelling (v1.3: "vocabulay", never found)
      setnames(npr, source_files$hospital$variables["vocabulary"], "vocab")
    }else{
      npr[, vocab:=npr_codes$vocab[1]]
    }

    if(!is.null(source_files$hospital$birth_year_limit) && !is.na(source_files$hospital$birth_year_limit[1]) &&
       !is.na(source_files$hospital$variables["birth_year"])){
      setnames(npr, source_files$hospital$variables["birth_year"], "birth_year")
      npr=npr[birth_year>=source_files$hospital$birth_year_limit[1] & birth_year<=source_files$hospital$birth_year_limit[2],]
    }



    fwrite(npr,"hospital_full.csv")
  }else{
    npr = fread("hospital_full.csv")
  }
  npr = npr[code_rows(tilst, vocab, c("ICD-10" = paste(npr_pattern, nonlive_codes$icd10, sep = "|")))]   # [v2.0.0] memory, see code_rows();
                                                                    # [experimental] also the non-live codes


  if(!file.exists("primary_care_full.csv")){
    kuhr_vars = unname(source_files$primary_care$variables)
    if(is.null(source_files$primary_care$data)){
      kuhr = smart_load(source_files$primary_care$file, variables = kuhr_vars)
    }else{
      kuhr = source_files$primary_care$data
      if(!"data.table" %in% class(kuhr))
        kuhr = as.data.table(kuhr)
      kuhr = kuhr[,..kuhr_vars]
    }

    setnames(kuhr, source_files$primary_care$variables["mother_id"], "ID")
    setnames(kuhr, source_files$primary_care$variables["diagnose_code"], "diagnose")
    setnames(kuhr, source_files$primary_care$variables["vocabulary"], "diagnosekodeverk")
    setnames(kuhr, source_files$primary_care$variables["visit_date"], "ant_dager")
    # [v2.0.0] without a gender column all records are taken as women's (an extract of mothers); female_value
    # other than 2 is recoded to 2
    if(!is.na(source_files$primary_care$variables["gender"])){
      setnames(kuhr, source_files$primary_care$variables["gender"], "kjonn")
      fv = source_files$primary_care$female_value
      if(!is.null(fv) && !identical(as.character(fv), "2")) kuhr[, kjonn := fifelse(as.character(kjonn) == as.character(fv), 2L, 1L)]
    }else{
      kuhr[, kjonn := 2L]
    }

    if(!is.null(source_files$primary_care$birth_year_limit) && !is.na(source_files$primary_care$birth_year_limit[1]) &&
       !is.na(source_files$primary_care$variables["birth_year"])){
      setnames(kuhr, source_files$primary_care$variables["birth_year"], "birth_year")
      kuhr=kuhr[birth_year>=source_files$primary_care$birth_year_limit[1] & birth_year<=source_files$primary_care$birth_year_limit[2],]
    }

    fwrite(kuhr, "primary_care_full.csv")

  }else{
    kuhr = fread("primary_care_full.csv")
  }
  kuhr = kuhr[code_rows(diagnose, diagnosekodeverk, c("ICD-10" = paste(kuhr_pattern[["ICD-10"]], nonlive_codes$icd10, sep = "|"),
                                                    "ICPC-2" = paste(kuhr_pattern[["ICPC-2"]], nonlive_codes$icpc2, sep = "|")))]   # [experimental]

  # [v2.0.0] Latin-1 text to UTF-8 (fix_encoding() above)
  npr[, tilst := fix_encoding(tilst)]
  if("vocab" %in% names(npr)) npr[, vocab := fix_encoding(vocab)]
  kuhr[, `:=`(diagnose = fix_encoding(diagnose), diagnosekodeverk = fix_encoding(diagnosekodeverk))]

  # [v1.3.1] normalise diagnosis codes in memory (never in the cached CSV files, written above):
  # drop every character that is not a letter or digit and convert to upper case, so that
  # e.g. "Z32.1", "w82" or "W 82" match the marker / outcome patterns ("Z321", "W82").
  # The coding-system labels (vocab, diagnosekodeverk) are not touched.
  if(isTRUE(cfg_normalize_codes)){
    npr[, tilst := .uio_normalize_code(tilst)]
    kuhr[, diagnose := .uio_normalize_code(diagnose)]
  }

  # ref_date = ymd("2004-01-01") - 10234
  # range(mbrn$DAGER_FDATO)+ref_date

  # Dates: real dates, day offsets from one reference date, or day offsets from the woman's own reference
  # date (see uio_dates()). [v2.0.0] text dates are parsed with lubridate::parse_date_time() (v1.3 guessed the
  # format from a random sample of 10,000 values, and used the MBRN table for the NPR)
  if(!"Date"%in%class(npr[,inndato]))
    npr[, inndato := .uio_as_date(inndato, ID, source_files$hospital)]
  if(!"Date"%in%class(mbrn[,DAGER_FDATO]))
    mbrn[, DAGER_FDATO := .uio_as_date(DAGER_FDATO, PasientLopeNr_MOR, source_files$birth_registry)]
  if(!"Date"%in%class(kuhr[,ant_dager]))
    kuhr[, ant_dager := .uio_as_date(ant_dager, ID, source_files$primary_care)]

  # SVLEN = completed gestational weeks; keep records from min_year (MBRN: with mother ID and GA; KUHR: women)
  mbrn[,SVLEN:=floor(SVLEN_DG/7)]

  # [experimental] MBRN records without gestational age (dropped on the next line, as in the algorithm),
  # births in years_range, with the mother's ID
  exp_mbrn_missing_ga = mbrn[year(DAGER_FDATO) >= min_year & year(DAGER_FDATO) <= max_year & !is.na(PasientLopeNr_MOR) & is.na(SVLEN_DG),
                             intersect(c("PasientLopeNr_MOR", "DAGER_FDATO", "PLUREK", "DODKAT", "VEKT", "APGAR5"), names(mbrn)), with = FALSE]

  # [experimental] MBRN single births with a fetal death (DODKAT 7-9) and without gestational age are kept: each is decided
  # as a termination, a stillbirth or a late spontaneous abortion, and SVLEN_DG is set to the median gestational age of
  # the MBRN records of that kind WITH a recorded gestational age (all years from min_year). The algorithm then runs
  # unchanged and gives the same decision (a termination is given DODKAT 10 in memory; cap$mbrn keeps the registered value):
  #   NPR or KUHR has a termination code (ICD-10 O04; KUHR also ICPC-2 W83) from missing_ga_confirm_days["before"] days
  #   before to ["after"] days after the birth date: termination (whatever the weight);
  #   otherwise weight >= stillbirth_min_weight_g: stillbirth; weight below it: late spontaneous abortion;
  #   weight missing: late spontaneous abortion if NPR or KUHR has a miscarriage code (ICD-10 O02, O03; ICPC-2 W82) in the
  #   same window, otherwise stillbirth.
  # Stillbirth median: at least stillbirth_min_days (147 with the v1.3 rule). Late spontaneous abortion median: from 84
  # days (MBRN registers from 12 weeks) to stillbirth_min_days - 1. Termination median (MBRN DODKAT 10): at least 84 days.
  # Without such records: stillbirth_min_days, 119 and 119.
  # MBRN single live births (DODKAT 0-6, 11-12; 13 with a 5-minute Apgar score that is not 0 or blank, as in the algorithm)
  # without gestational age are kept too, with SVLEN_DG = missing_ga_livebirth_days (280: the median of live births).
  # [experimental, may be dropped] Floor of an imputed start: not earlier than the woman's previous outcome + missing_ga_min_gap_days
  # (28); otherwise the gestational age is shortened to that. Previous outcome: the latest, more than
  # missing_ga_confirm_days["before"] days before the birth date (closer records are this pregnancy's own codes), of
  #   an MBRN birth of the woman (any record), and
  #   an NPR (ICD-10 O00-O04) or KUHR (ICD-10 O02, O03; ICPC-2 W82) outcome code (the algorithm's outcome codes), dated by
  #   the first record of its event (records less than same_event_days apart are one event) and not less than
  #   same_event_days after an MBRN birth (such records belong to that birth), as in the algorithm.
  # A live birth whose shortened gestational age is below missing_ga_livebirth_min_days (154), or any record left with less
  # than 1 day, is not imputed (dropped as before). A stillbirth without weight shortened below stillbirth_min_days is a
  # late spontaneous abortion (the algorithm's gestational-age rule).
  # Multiple births (another MBRN record of the mother on the same date, or PLUREK > 1), induced abortions and other DODKAT
  # values keep the missing gestational age and are dropped as before.
  exp_imputed = data.table(ID = character(0), dn = integer(0), kind = character(0), ga = numeric(0))
  exp_medians = c(stillbirth = NA_real_, late_spontaneous_abortion = NA_real_, termination = NA_real_, live_birth = NA_real_)
  if(isTRUE(missing_ga_impute)){
    sb_days = if(is.null(cfg_stillbirth_min_days)) 147 else cfg_stillbirth_min_days
    wlim = if(use_weight) cfg_stillbirth_min_weight_g else Inf
    wt_all = if(use_weight) suppressWarnings(as.numeric(mbrn$VEKT)) else rep(NA_real_, nrow(mbrn))
    wt_all[!is.na(wt_all) & wt_all <= 0] = NA
    fd = mbrn$DODKAT %in% 7:9 & !is.na(mbrn$PasientLopeNr_MOR) & !is.na(mbrn$DAGER_FDATO) & year(mbrn$DAGER_FDATO) >= min_year
    rec = fd & !is.na(mbrn$SVLEN_DG)
    rec_sb = rec & (mbrn$SVLEN_DG >= sb_days | (!is.na(wt_all) & wt_all >= wlim))
    med = function(x, lo, hi, fallback){ x = x[!is.na(x)]; v = if(length(x)) round(stats::median(x)) else fallback; min(max(v, lo), hi) }
    rec_top = mbrn$DODKAT %in% 10 & !is.na(mbrn$SVLEN_DG) & !is.na(mbrn$DAGER_FDATO) & year(mbrn$DAGER_FDATO) >= min_year
    exp_medians = c(stillbirth = med(mbrn$SVLEN_DG[rec_sb], sb_days, Inf, sb_days),
                    late_spontaneous_abortion = med(mbrn$SVLEN_DG[rec & !rec_sb], 84, sb_days - 1, 119),
                    termination = med(mbrn$SVLEN_DG[rec_top], 84, Inf, 119),
                    live_birth = as.numeric(missing_ga_livebirth_days))
    key_md = paste(mbrn$PasientLopeNr_MOR, as.integer(mbrn$DAGER_FDATO))
    single = !key_md %in% key_md[duplicated(key_md)] & (if("PLUREK" %in% names(mbrn)) is.na(mbrn$PLUREK) | mbrn$PLUREK <= 1 else TRUE)
    miss = which(fd & is.na(mbrn$SVLEN_DG) & single)
    ap = if("APGAR5" %in% names(mbrn)) trimws(as.character(mbrn$APGAR5)) else rep("", nrow(mbrn))
    lb = (mbrn$DODKAT %in% c(0:6, 11:12) | (mbrn$DODKAT %in% 13 & !ap %in% c("0", ""))) & !is.na(mbrn$PasientLopeNr_MOR) &
      !is.na(mbrn$DAGER_FDATO) & year(mbrn$DAGER_FDATO) >= min_year
    miss_lb = which(lb & is.na(mbrn$SVLEN_DG) & single)
    cand = data.table(i = c(miss, miss_lb), fdeath = rep(c(TRUE, FALSE), c(length(miss), length(miss_lb))))
    if(nrow(cand)){
      before = as.integer(missing_ga_confirm_days[["before"]])
      cand[, `:=`(ID = as.character(mbrn$PasientLopeNr_MOR[i]), dn = as.integer(mbrn$DAGER_FDATO[i]), w = wt_all[i])]
      cand[, `:=`(lo = dn - before, hi = dn + as.integer(missing_ga_confirm_days[["after"]]))]
      in_window = function(codes) unique(codes[cand, on = .(ID, d >= lo, d <= hi), nomatch = 0L, .(i = i.i)]$i)
      ## termination codes, by registry and code (the summary counts the pregnancies each code switched)
      o04_npr  = npr[vocab == "ICD-10" & grepl("O04", tilst, fixed = TRUE), .(ID = as.character(ID), d = as.integer(inndato))]
      o04_kuhr = kuhr[diagnosekodeverk == "ICD-10" & grepl("O04", diagnose, fixed = TRUE), .(ID = as.character(ID), d = as.integer(ant_dager))]
      w83_kuhr = kuhr[diagnosekodeverk == "ICPC-2" & grepl("W83", diagnose, fixed = TRUE), .(ID = as.character(ID), d = as.integer(ant_dager))]
      sa_codes = rbind(npr[vocab == "ICD-10" & grepl("O02|O03", tilst), .(ID = as.character(ID), d = as.integer(inndato))],
                       kuhr[(diagnosekodeverk == "ICD-10" & grepl("O02|O03", diagnose)) |
                              (diagnosekodeverk == "ICPC-2" & grepl("W82", diagnose, fixed = TRUE)), .(ID = as.character(ID), d = as.integer(ant_dager))])
      cand[, `:=`(o04_npr = i %in% in_window(o04_npr), o04_kuhr = i %in% in_window(o04_kuhr), w83 = i %in% in_window(w83_kuhr),
                  confirmed = i %in% in_window(sa_codes))]
      cand[fdeath == FALSE, `:=`(o04_npr = NA, o04_kuhr = NA, w83 = NA, confirmed = NA)]
      cand[, kind := data.table::fifelse(!fdeath, "live_birth",
                       data.table::fifelse(o04_npr | o04_kuhr | w83, "termination",
                         data.table::fifelse(!is.na(w), data.table::fifelse(w >= wlim, "stillbirth", "late_spontaneous_abortion"),
                                             data.table::fifelse(confirmed, "late_spontaneous_abortion", "stillbirth"))))]
      cand[, ga_median := unname(exp_medians[kind])]

      ## [experimental, may be dropped] the woman's previous outcome (see above) and the floor of the start
      ids = unique(cand$ID)
      births = unique(data.table(ID = as.character(mbrn$PasientLopeNr_MOR), d = as.integer(mbrn$DAGER_FDATO))[ID %in% ids & !is.na(d)])
      ep = unique(rbind(npr[vocab == "ICD-10" & grepl("O00|O01|O02|O03|O04", tilst), .(ID = as.character(ID), d = as.integer(inndato))],
                        kuhr[kjonn == 2 & ((diagnosekodeverk == "ICD-10" & grepl("O02|O03", diagnose)) |
                                             (diagnosekodeverk == "ICPC-2" & grepl("W82", diagnose, fixed = TRUE))),
                             .(ID = as.character(ID), d = as.integer(ant_dager))])[ID %in% ids & !is.na(d)])
      setorder(ep, ID, d)
      ## first record of each event: a record more than same_event_days - 1 days after the event's first record opens a new one
      if(nrow(ep)){
        ep[, first := { f = logical(.N); s = d[1]; f[1] = TRUE
                        for(k in seq_len(.N)[-1]) if(d[k] - s > same_event_episode_days){ s = d[k]; f[k] = TRUE }
                        f }, by = ID]
        ep = ep[first == TRUE, .(ID, d)]
      }
      ## an event less than same_event_days after an MBRN birth belongs to that birth
      ep[, `:=`(blo = d - same_event_episode_days, bhi = d)]
      near = unique(births[ep, on = .(ID, d >= blo, d <= bhi), nomatch = 0L, .(ID, d = i.d)])
      ep = ep[!near, on = .(ID, d), .(ID, d)]
      prev = unique(rbind(births, ep))
      cand[, prev_key := dn - before - 1L]
      cand[, prev_outcome := prev[cand, on = .(ID, d = prev_key), roll = Inf, x.d]]
      gap = as.integer(missing_ga_min_gap_days)
      cand[, capped := !is.na(prev_outcome) & dn - ga_median < prev_outcome + gap]
      cand[, ga := data.table::fifelse(capped, as.numeric(dn - (prev_outcome + gap)), ga_median)]
      cand[, kind_rule := kind]
      cand[kind == "stillbirth" & is.na(w) & ga < sb_days, kind := "late_spontaneous_abortion"]
      cand[, imputed := ga >= 1 & (kind != "live_birth" | ga >= missing_ga_livebirth_min_days)]

      ok = cand[imputed == TRUE]
      mbrn[ok$i, SVLEN_DG := if(is.integer(mbrn$SVLEN_DG)) as.integer(ok$ga) else as.numeric(ok$ga)]
      mbrn[ok$i, SVLEN := floor(SVLEN_DG/7)]
      ## a termination: DODKAT 10 in memory, so that the algorithm takes it as an MBRN induced abortion (registered value kept)
      mbrn[, DODKAT_registered := DODKAT]
      top_i = ok$i[ok$kind == "termination"]
      if(length(top_i)) mbrn[top_i, DODKAT := if(is.integer(mbrn$DODKAT)) 10L else 10]
      exp_imputed = cand[, .(ID, dn, kind, kind_rule, ga, ga_median, weight = w, confirmed, o04 = o04_npr | o04_kuhr, o04_npr, o04_kuhr, w83,
                             prev_outcome, capped, imputed)]
      n_k = function(k) sum(ok$kind == k)
      cli::cli_alert_info(paste0("[experimental] MBRN single births without gestational age kept: ",
                                 n_k("termination"), " termination(s) (O04 or W83; GA ", exp_medians[["termination"]], " days), ",
                                 n_k("stillbirth"), " stillbirth(s) (GA ", exp_medians[["stillbirth"]], " days), ",
                                 n_k("late_spontaneous_abortion"), " late spontaneous abortion(s) (GA ", exp_medians[["late_spontaneous_abortion"]], " days), ",
                                 n_k("live_birth"), " live birth(s) (GA ", exp_medians[["live_birth"]], " days); ",
                                 sum(ok$capped), " of them with the start moved to ", gap, " days after the previous outcome; ",
                                 sum(!cand$imputed), " not imputed (too short after the previous outcome)."))
    }
  }

  mbrn <- mbrn[year(DAGER_FDATO) >= min_year & !is.na(PasientLopeNr_MOR) & !is.na(SVLEN_DG)]
  npr  <- npr[year(inndato) >= min_year]
  kuhr <- kuhr[year(ant_dager) >= min_year & kjonn==2]

  # [experimental] NPR and KUHR (women) records with a non-live outcome code (uio_nonlive_codes()), before the
  # code filter below
  exp_nonlive = rbind(
    npr[vocab == "ICD-10" & grepl(nonlive_codes$icd10, tilst, perl = TRUE),
        .(ID = as.character(ID), source = "NPR", vocab, code = as.character(tilst), date = as.Date(inndato))],
    kuhr[(diagnosekodeverk == "ICD-10" & grepl(nonlive_codes$icd10, diagnose, perl = TRUE)) |
           (diagnosekodeverk == "ICPC-2" & grepl(nonlive_codes$icpc2, diagnose, perl = TRUE)),
         .(ID = as.character(ID), source = "KUHR", vocab = diagnosekodeverk, code = as.character(diagnose), date = as.Date(ant_dager))])

  hospital_codes = as.data.table(hospital_codes)
  primary_care_codes = as.data.table(primary_care_codes)
  # Keep only the codes used later (npr_pattern, kuhr_pattern)
  suppressWarnings(npr[, stay_id:=NULL])
  npr = unique(npr[(vocab == "ICD-10" & grepl(npr_pattern, tilst, perl = TRUE))])
  kuhr = unique(kuhr[(diagnosekodeverk == "ICD-10" & grepl(kuhr_pattern[["ICD-10"]], diagnose, perl = TRUE)) |
                        (diagnosekodeverk == "ICPC-2" & grepl(kuhr_pattern[["ICPC-2"]], diagnose, perl = TRUE)) ])

  # [v2.0.0] the records used, kept for the description of the pregnancies and the code report (run_uio_pa())
  uio_capture = list(npr = npr[, .(ID, tilst, inndato)], kuhr = kuhr[, .(ID, diagnose, diagnosekodeverk, ant_dager)],
                     mbrn = mbrn[, intersect(c("PasientLopeNr_MOR", "DAGER_FDATO", "DODKAT", "SVLEN_DG", "MORS_ALDER", "VEKT"), names(mbrn)), with = FALSE],
                     weight_used = use_weight,
                     missing_ga = list(mbrn = exp_mbrn_missing_ga, nonlive = exp_nonlive,      # [experimental]
                                       imputed = exp_imputed, medians = exp_medians))
  # [experimental] the registered DODKAT in the records used for the description (codes_mbrn), not the 10 of an imputed termination
  if("DODKAT_registered" %in% names(mbrn)){
    uio_capture$mbrn[, DODKAT := mbrn$DODKAT_registered]
    mbrn[, DODKAT_registered := NULL]
  }


  # NPR stay: consecutive records of a woman with the same date; diagno numbers the diagnoses within a stay
  npr[, stay_id := as.integer(rleid(inndato)), by = ID]
  setorder(npr, ID, stay_id, inndato, na.last = TRUE)
  npr[,diagno:=seq_len(.N),by=.(ID, stay_id)]

  cli::cli_progress_bar(
    total = 16,
    format = "{cli::pb_bar} {cli::pb_percent} [{cli::pb_elapsed}]"
  )

  cli::cli_alert_info("Categorise outcome of the offspring's vital status (DODKAT)")
  cli::cli_progress_update()

  ##################### BLOCK 1 ########################
  ######################################################
  # MBRN, one row per child: group children into pregnancies (preg_id) and flag each child's outcome
  # from DODKAT (live birth, miscarriage/stillbirth by the stillbirth limit, induced abortion, unknown).

  # Load data (assuming it's a .dta file, the native Stata format)
  MBRN <- copy(mbrn)

  mbrn[, PasientLopeNr_MOR:=as.integer(PasientLopeNr_MOR)]

  MBRN[, start := DAGER_FDATO - SVLEN_DG]
  MBRN[, start := as.IDate(start, origin = "1970-01-01")]

  # Sort by Birth date and for multiplets by birth order, creating a unique pregnancy ID
  # Assuming 'PasientLopeNr_MOR', 'DAGER_FDATO', and 'PLUREK' are columns in the MBRN data
  setorder(MBRN, PasientLopeNr_MOR, DAGER_FDATO, PLUREK)
  setkey(MBRN, PasientLopeNr_MOR)
  # Unique pregnancy ID for each pregnancy
  MBRN[, preg_id := .I]

  # Create a lag column and replace the preg_id based on the conditions given
  # (a child whose start is < 13 days after the previous child's start, or born the same day, joins its pregnancy)
  MBRN[, lag := start - shift_wrt_id(start, PasientLopeNr_MOR)]
  MBRN[, preg_id := shift_with_condition(lag < 13 | DAGER_FDATO == shift(DAGER_FDATO, 1), preg_id), by=PasientLopeNr_MOR]

  # Create a new birth order and number of multiplets
  setorder(MBRN, PasientLopeNr_MOR, preg_id, DAGER_FDATO, PLUREK)
  MBRN[, PLUREK_new := seq_len(.N), by = .(PasientLopeNr_MOR, preg_id)]
  MBRN[, PLURAL_new := max(PLUREK_new), by = .(PasientLopeNr_MOR, preg_id)]

  # Exclude pregnancies before 2008
  MBRN <- MBRN[year(DAGER_FDATO) >= min_year]

  # Exclude women with missing ID
  MBRN <- MBRN[!is.na(PasientLopeNr_MOR)]

  # Exclude pregnancies with missing gestational length
  MBRN <- MBRN[!is.na(SVLEN_DG)]

  # Categorise outcome of the offspring's vital status
  MBRN[, livebirth := ifelse(DODKAT %in% c(0:6,11:12), 1, NA)]
  if(use_weight){
    # [v1.4.0] birth weight also decides: stillbirth if the gestational-age rule says so or the weight is at least
    # stillbirth_min_weight_g (22 weeks or 500 g, the MBRN definition; weights <= 0 count as missing)
    ga_sb = if(is.null(cfg_stillbirth_min_days)) MBRN$SVLEN > 20 else MBRN$SVLEN_DG >= cfg_stillbirth_min_days
    wt = suppressWarnings(as.numeric(MBRN$VEKT))
    wt[!is.na(wt) & wt <= 0] = NA
    sb = ga_sb | (!is.na(wt) & wt >= cfg_stillbirth_min_weight_g)
    MBRN[, miscar_3 := ifelse(DODKAT %in% c(7:9) & !sb, 1, NA)]
    MBRN[, stillbirth := ifelse((DODKAT %in% c(7:9)) & sb, 1, NA)]
  }else if(is.null(cfg_stillbirth_min_days)){
    # [v1.3.1] stillbirth_min_days = NULL: the v1.3 rule (completed weeks SVLEN > 20)
    MBRN[, miscar_3 := ifelse(DODKAT %in% c(7:9) & SVLEN <= 20, 1, NA)]
    MBRN[, stillbirth := ifelse((DODKAT %in% c(7:9)) & SVLEN > 20, 1, NA)]
  }else{
    # [v1.3.1] fetal death from stillbirth_min_days gestational days = stillbirth (e.g. 154 = 22+0 weeks, as in revised_codes_SB_22)
    MBRN[, miscar_3 := ifelse(DODKAT %in% c(7:9) & SVLEN_DG < cfg_stillbirth_min_days, 1, NA)]
    MBRN[, stillbirth := ifelse((DODKAT %in% c(7:9)) & SVLEN_DG >= cfg_stillbirth_min_days, 1, NA)]
  }
  MBRN[, unknown := ifelse(DODKAT %in% c(13) , 1, NA)]
  MBRN[, induced_ab := ifelse(DODKAT %in% c(10) , 1, NA)]

  replace_na = function(x,val) ifelse(is.na(x),val,x)
  # Recode created variables where . = NA to 0
  MBRN[, `:=`(livebirth = replace_na(livebirth, 0),
              miscar_3 = replace_na(miscar_3, 0),
              stillbirth = replace_na(stillbirth, 0),
              induced_ab = replace_na(induced_ab, 0),
              unknown = replace_na(unknown, 0))]

  # Many pregnancies with unknown outcome, assume liveborn if APGAR score > 0
  MBRN[livebirth == 0 & unknown == 1 & !(trimws(APGAR5) %in% c("0", "")), livebirth:=1]

  MBRN[unknown == 1 & !(trimws(APGAR5) %in% c("0", "")), unknown:=0]

  # Save the final dataset

  MBRN_for_analysis = copy(MBRN)

  ##################### BLOCK 2 ########################
  ######################################################
  cli::cli_alert_info("Handle multifetal gestations with discordant outcomes")
  cli::cli_progress_update()

  # Handle multifetal gestations with discordant outcomes
  # One row per pregnancy with the children's flags side by side (livebirth1, livebirth2, ...); the
  # combinations below give one outcome per multiple pregnancy (any induced abortion -> TOP, etc.).
  MBRN <- MBRN[, .(preg_id,PLUREK_new,PLURAL_new,livebirth,miscar_3,stillbirth,unknown,induced_ab, DAGER_FDATO, SVLEN_DG)]

  # Reshape wide livebirth miscar_3 stillbirth unknown induced_ab
  MBRN <- dcast(MBRN, PLURAL_new+preg_id ~ PLUREK_new, value.var = c("livebirth", "miscar_3", "stillbirth", "unknown", "induced_ab", "DAGER_FDATO", "SVLEN_DG"),
                sep = "")

  for(ij1 in 2:4){
    var_1 = paste0("DAGER_FDATO", ij1)
    var_2 = paste0("SVLEN_DG", ij1)
    # [v1.3.1] `MBRN[, var_1:=NA_Date_, with = FALSE]` -> `MBRN[, (var_1):=NA_Date_]`: same assignment (column
    # named by the value of var_1), but `with = FALSE` together with `:=` is an error from data.table 1.17
    if(!var_1%in%colnames(MBRN)) MBRN[, (var_1):=NA_Date_]
    if(!var_2%in%colnames(MBRN)) MBRN[, (var_2):=NA_Date_]
  }

  # rev 1.2
  # keep multiple pregnancies only (a second child exists); check_ga_* flag children born on different dates
  MBRN = MBRN[!is.na(DAGER_FDATO2)]
  MBRN[DAGER_FDATO1 != DAGER_FDATO2 & PLURAL_new == 2, check_ga_twin := 1]

  # Generate twin_LL to twin_disc
  MBRN[PLURAL_new == 2 & livebirth1 == 1 & livebirth2 == 1, twin_LL := 1]
  MBRN[PLURAL_new == 2 & miscar_31 == 1 & miscar_32 == 1, twin_MM := 1]
  MBRN[PLURAL_new == 2 & stillbirth1 == 1 & stillbirth2 == 1, twin_SB := 1]
  MBRN[PLURAL_new == 2 & unknown1 == 1 & unknown2 == 1, twin_UK := 1]
  MBRN[PLURAL_new == 2 & induced_ab1 == 1 & induced_ab2 == 1, twin_TT := 1]
  MBRN[PLURAL_new == 2 & is.na(twin_LL) & is.na(twin_MM) & is.na(twin_SB) & is.na(twin_UK) & is.na(twin_TT), twin_disc := 1]

  # Generate twin_LS to twin_UC
  MBRN[PLURAL_new == 2 & ((livebirth1 == 1 & stillbirth2 == 1) | (stillbirth1 == 1 & livebirth2 == 1)), twin_LS := 1]
  MBRN[PLURAL_new == 2 & ((livebirth1 == 1 & miscar_32 == 1) | (miscar_31 == 1 & livebirth2 == 1)), twin_LM := 1]
  MBRN[PLURAL_new == 2 & ((livebirth1 == 1 & induced_ab2 == 1) | (induced_ab1 == 1 & livebirth2 == 1)), twin_LT := 1]
  MBRN[PLURAL_new == 2 & ((miscar_31 == 1 & stillbirth2 == 1) | (stillbirth1 == 1 & miscar_32 == 1)), twin_MS := 1]
  MBRN[PLURAL_new == 2 & ((miscar_31 == 1 & induced_ab2 == 1) | (induced_ab1 == 1 & miscar_32 == 1)), twin_MT := 1]
  MBRN[PLURAL_new == 2 & ((stillbirth1 == 1 & induced_ab2 == 1) | (induced_ab1 == 1 & stillbirth2 == 1)), twin_ST := 1]
  MBRN[PLURAL_new == 2 & is.na(twin_UK) & (unknown1 == 1 | unknown2 == 1), twin_UC := 1]


  # rev 1.2
  MBRN[((DAGER_FDATO1 != DAGER_FDATO2) | (DAGER_FDATO1 != DAGER_FDATO3)) & PLURAL_new == 3 & !is.na(DAGER_FDATO3), check_ga_triplet := 1]

  if(!"livebirth3"%in%colnames(MBRN))MBRN[, livebirth3:=0]
  if(!"miscar_33"%in%colnames(MBRN))MBRN[, miscar_33:=0]
  if(!"stillbirth3"%in%colnames(MBRN))MBRN[, stillbirth3:=0]
  if(!"induced_ab3"%in%colnames(MBRN))MBRN[, induced_ab3:=0]
  if(!"unknown3"%in%colnames(MBRN))MBRN[, unknown3:=0]
  if(!"livebirth4"%in%colnames(MBRN))MBRN[, livebirth4:=0]
  if(!"miscar_34"%in%colnames(MBRN))MBRN[, miscar_34:=0]
  if(!"stillbirth4"%in%colnames(MBRN))MBRN[, stillbirth4:=0]
  if(!"induced_ab4"%in%colnames(MBRN))MBRN[, induced_ab4:=0]
  if(!"unknown4"%in%colnames(MBRN))MBRN[, unknown4:=0]

  # Generate trip_LLL to trip_disc
  MBRN[PLURAL_new == 3 & livebirth1 == 1 & livebirth2 == 1 & livebirth3 == 1, trip_LLL := 1]
  MBRN[PLURAL_new == 3 & miscar_31 == 1 & miscar_32 == 1 & miscar_33 == 1, trip_MMM := 1]
  MBRN[PLURAL_new == 3 & stillbirth1 == 1 & stillbirth2 == 1 & stillbirth3 == 1, trip_SSS := 1]
  MBRN[PLURAL_new == 3 & unknown1 == 1 & unknown2 == 1 & unknown3 == 1, trip_UK := 1]
  MBRN[PLURAL_new == 3 & induced_ab1 == 1 & induced_ab2 == 1 & induced_ab3 == 1, trip_TTT := 1]
  MBRN[PLURAL_new == 3 & is.na(trip_LLL) & is.na(trip_MMM) & is.na(trip_SSS) & is.na(trip_UK) & is.na(trip_TTT), trip_disc := 1]

  MBRN[PLURAL_new == 3 ,trip_n_LB := livebirth1+livebirth2+livebirth3]
  MBRN[PLURAL_new == 3 ,trip_n_MC := miscar_31+miscar_32+miscar_33]
  MBRN[PLURAL_new == 3 ,trip_n_SB := stillbirth1+stillbirth2+stillbirth3]
  MBRN[PLURAL_new == 3 ,trip_n_TOP := induced_ab1+induced_ab2+induced_ab3]


  # Generate trip_UC to trip_STT
  MBRN[PLURAL_new == 3 & (unknown1 == 1 | unknown2 == 1 | unknown3 == 1) & trip_disc == 1, trip_UC := 1]
  MBRN[trip_disc == 1 & trip_n_LB == 2 & trip_n_SB == 1, trip_LLS := 1]  #6 pregnancies
  MBRN[trip_disc == 1 & trip_n_LB == 2 & trip_n_TOP == 1, trip_LLT := 1] #2 pregnancies
  MBRN[trip_disc == 1 & trip_n_LB == 1 & trip_n_SB == 2, trip_LSS := 1]  #3 pregnancies
  MBRN[trip_disc == 1 & trip_n_LB == 1 & trip_n_TOP == 2, trip_LTT := 1] #3 pregnancies
  MBRN[trip_disc == 1 & trip_n_SB == 1 & trip_n_TOP == 2, trip_STT := 1] #1 pregnancies

  # rev 1.2
  MBRN[((DAGER_FDATO1 != DAGER_FDATO2) | (DAGER_FDATO1 != DAGER_FDATO3) | (DAGER_FDATO1 != DAGER_FDATO4)) & PLURAL_new == 4 & is.na(DAGER_FDATO4), check_ga_quad := 1]

  MBRN[PLURAL_new == 4,qua_n_LB := livebirth1+livebirth2+livebirth3+livebirth4]
  MBRN[PLURAL_new == 4,qua_n_MC := miscar_31+miscar_32+miscar_33+miscar_34]
  MBRN[PLURAL_new == 4,qua_n_SB := stillbirth1+stillbirth2+stillbirth3+stillbirth4]
  MBRN[PLURAL_new == 4,qua_n_TOP := induced_ab1+induced_ab2+induced_ab3+induced_ab4]
  MBRN[PLURAL_new == 4,qua_n_UK := unknown1+unknown2+unknown3+unknown4]
  MBRN[PLURAL_new == 4 & qua_n_LB == 4,qua_LLLL := 1]
  MBRN[PLURAL_new == 4& qua_n_MC == 2 & qua_n_TOP == 2, qua_MMTT := 1]

  # Pregnancy outcome of the multiples: all live -> live birth; live + miscarriage -> miscarriage; a stillbirth
  # with live births/miscarriages -> stillbirth; any induced abortion -> TOP; unknown child(ren) -> unknown
  MBRN[twin_LL == 1 | trip_LLL == 1 | qua_LLLL == 1, MBRN_livebirth := 1]
  MBRN[twin_MM == 1 | twin_LM == 1 | trip_MMM == 1, MBRN_miscarriage := 1]
  MBRN[twin_SB == 1 | twin_LS == 1 | twin_MS == 1 | trip_SSS == 1 | trip_LLS == 1 | trip_LSS == 1, MBRN_stillbirth := 1]
  MBRN[twin_TT == 1 | twin_LT == 1 | twin_MT == 1 | twin_ST == 1 | trip_LLT == 1 | trip_LTT == 1 | trip_STT == 1 | qua_MMTT == 1, MBRN_TOP := 1]
  MBRN[twin_UK == 1 | twin_UC == 1 | (PLURAL_new == 2 & miscar_31 == 1 & is.na(livebirth2) & is.na(miscar_32) & is.na(stillbirth2) & is.na(unknown2) & is.na(induced_ab2)) | trip_UK == 1, MBRN_unknown := 1]

  ### Rev 1.2
  ### Assignment of gestational age at pregnancy outcome (DAGER_FDATO AND SVLEN_DG!!)
  ### [START]
  # Outcome date (DAGER_FDATO_MF) and GA (SVLEN_DG_MF) of the multiple pregnancy: child 1's values when all
  # children share the date; otherwise mainly those of the child with the deciding outcome (TOP, stillbirth, miscarriage).

  MBRN[is.na(check_ga_twin) & is.na(check_ga_triplet) & is.na(check_ga_quad),  DAGER_FDATO_MF := DAGER_FDATO1]
  MBRN[is.na(DAGER_FDATO_MF) & twin_disc == 0 & (DAGER_FDATO1 > DAGER_FDATO2), DAGER_FDATO_MF:= DAGER_FDATO1]
  MBRN[is.na(DAGER_FDATO_MF) & twin_disc == 0 & (DAGER_FDATO1 < DAGER_FDATO2), DAGER_FDATO_MF:= DAGER_FDATO2]
  MBRN[is.na(DAGER_FDATO_MF) & trip_disc == 0 & (DAGER_FDATO1 > DAGER_FDATO2) & (DAGER_FDATO1 > DAGER_FDATO3), DAGER_FDATO_MF:= DAGER_FDATO1]
  MBRN[is.na(DAGER_FDATO_MF) & trip_disc == 0 & (DAGER_FDATO1 < DAGER_FDATO2) & (DAGER_FDATO2 > DAGER_FDATO3), DAGER_FDATO_MF:= DAGER_FDATO2]
  MBRN[is.na(DAGER_FDATO_MF) & trip_disc == 0 & (DAGER_FDATO1 < DAGER_FDATO3) & (DAGER_FDATO2 < DAGER_FDATO3), DAGER_FDATO_MF:= DAGER_FDATO3]
  MBRN[is.na(DAGER_FDATO_MF) & twin_disc == 1 & induced_ab1 == 1, DAGER_FDATO_MF:= DAGER_FDATO1]
  MBRN[is.na(DAGER_FDATO_MF) & twin_disc == 1 & induced_ab2 == 1, DAGER_FDATO_MF:= DAGER_FDATO2]
  MBRN[is.na(DAGER_FDATO_MF) & twin_disc == 1 & stillbirth1 == 1, DAGER_FDATO_MF:= DAGER_FDATO1]
  MBRN[is.na(DAGER_FDATO_MF) & twin_disc == 1 & stillbirth2 == 1, DAGER_FDATO_MF:= DAGER_FDATO2]
  MBRN[is.na(DAGER_FDATO_MF) & twin_disc == 1 & miscar_31 == 1, DAGER_FDATO_MF:= DAGER_FDATO1]
  MBRN[is.na(DAGER_FDATO_MF) & twin_disc == 1 & miscar_32 == 1, DAGER_FDATO_MF:= DAGER_FDATO2]
  MBRN[is.na(DAGER_FDATO_MF) & trip_disc == 1 & induced_ab1 == 1,DAGER_FDATO_MF:= DAGER_FDATO1]
  MBRN[(is.na(DAGER_FDATO_MF) & trip_disc == 1 & induced_ab2 == 1) | (trip_disc == 1 & induced_ab2 == 1 & induced_ab1 == 1 & DAGER_FDATO2 > DAGER_FDATO_MF), DAGER_FDATO_MF:= DAGER_FDATO2]
  MBRN[(is.na(DAGER_FDATO_MF) & trip_disc == 1 & induced_ab3 == 1) | (trip_disc == 1 & induced_ab3 == 1 & (induced_ab1 == 1 | induced_ab2 == 1) & DAGER_FDATO3 > DAGER_FDATO_MF), DAGER_FDATO_MF:= DAGER_FDATO3]
  MBRN[is.na(DAGER_FDATO_MF) & trip_disc == 1 & stillbirth1 == 1 & MBRN_TOP != 1, DAGER_FDATO_MF:= DAGER_FDATO1]
  MBRN[(is.na(DAGER_FDATO_MF) & trip_disc == 1 & stillbirth2 == 1) | (trip_disc == 1 & stillbirth2 == 1 & stillbirth1 == 1 & DAGER_FDATO2 > DAGER_FDATO_MF) & MBRN_TOP != 1, DAGER_FDATO_MF:= DAGER_FDATO2]
  MBRN[(is.na(DAGER_FDATO_MF) & trip_disc == 1 & stillbirth3 == 1) | (trip_disc == 1 & stillbirth3 == 1 & (stillbirth1 == 1 | stillbirth2 == 1) & DAGER_FDATO3 > DAGER_FDATO_MF) & MBRN_TOP != 1, DAGER_FDATO_MF:= DAGER_FDATO3]
  MBRN[is.na(DAGER_FDATO_MF) & trip_disc == 1 & miscar_31 == 1 & MBRN_TOP != 1 & MBRN_stillbirth != 1, DAGER_FDATO_MF:= DAGER_FDATO1]
  MBRN[(is.na(DAGER_FDATO_MF) & trip_disc == 1 & miscar_32 == 1) | (trip_disc == 1 & miscar_32 == 1 & miscar_31 == 1 & DAGER_FDATO2 > DAGER_FDATO_MF) & MBRN_TOP != 1 & MBRN_stillbirth != 1, DAGER_FDATO_MF:= DAGER_FDATO2]
  MBRN[(is.na(DAGER_FDATO_MF) & trip_disc == 1 & miscar_33 == 1) | (trip_disc == 1 & miscar_33 == 1 & (miscar_31 == 1 | miscar_32 == 1) & DAGER_FDATO3 > DAGER_FDATO_MF) & MBRN_TOP != 1 & MBRN_stillbirth != 1, DAGER_FDATO_MF:= DAGER_FDATO3]
  MBRN[is.na(DAGER_FDATO_MF) & qua_MMTT == 1 & induced_ab1 == 1, DAGER_FDATO_MF:= DAGER_FDATO1]
  MBRN[(is.na(DAGER_FDATO_MF) & qua_MMTT == 1 & induced_ab2 == 1) | (qua_MMTT == 1 & induced_ab2 == 1 & induced_ab1 == 1 & DAGER_FDATO2 > DAGER_FDATO_MF), DAGER_FDATO_MF:= DAGER_FDATO2]
  MBRN[(is.na(DAGER_FDATO_MF) & qua_MMTT == 1 & induced_ab3 == 1) | (qua_MMTT == 1 & induced_ab3 == 1 & (induced_ab1 == 1 | induced_ab2 == 1) & DAGER_FDATO3 > DAGER_FDATO_MF), DAGER_FDATO_MF:= DAGER_FDATO3]
  MBRN[(is.na(DAGER_FDATO_MF) & qua_MMTT == 1 & induced_ab4 == 1) | (qua_MMTT == 1 & induced_ab4 == 1 & (induced_ab1 == 1 | induced_ab2 == 1 | induced_ab3 == 1) & DAGER_FDATO4 > DAGER_FDATO_MF), DAGER_FDATO_MF:= DAGER_FDATO4]

  MBRN[is.na(check_ga_twin) & is.na(check_ga_triplet) & is.na(check_ga_quad), SVLEN_DG_MF:= SVLEN_DG1]
  MBRN[is.na(SVLEN_DG_MF) & twin_disc == 0 & (SVLEN_DG1 > SVLEN_DG2), SVLEN_DG_MF:= SVLEN_DG1]
  MBRN[is.na(SVLEN_DG_MF) & twin_disc == 0 & (SVLEN_DG1 < SVLEN_DG2), SVLEN_DG_MF:= SVLEN_DG2]
  MBRN[is.na(SVLEN_DG_MF) & trip_disc == 0 & (SVLEN_DG1 > SVLEN_DG2) & (SVLEN_DG1 > SVLEN_DG3), SVLEN_DG_MF:= SVLEN_DG1]
  MBRN[is.na(SVLEN_DG_MF) & trip_disc == 0 & (SVLEN_DG1 < SVLEN_DG2) & (SVLEN_DG2 > SVLEN_DG3), SVLEN_DG_MF:= SVLEN_DG2]
  MBRN[is.na(SVLEN_DG_MF) & trip_disc == 0 & (SVLEN_DG1 < SVLEN_DG3) & (SVLEN_DG2 < SVLEN_DG3), SVLEN_DG_MF:= SVLEN_DG3]
  MBRN[is.na(SVLEN_DG_MF) & twin_disc == 1 & induced_ab1 == 1,SVLEN_DG_MF:= SVLEN_DG1]
  MBRN[is.na(SVLEN_DG_MF) & twin_disc == 1 & induced_ab2 == 1, SVLEN_DG_MF:= SVLEN_DG2]
  MBRN[is.na(SVLEN_DG_MF) & twin_disc == 1 & stillbirth1 == 1, SVLEN_DG_MF:= SVLEN_DG1]
  MBRN[is.na(SVLEN_DG_MF) & twin_disc == 1 & stillbirth2 == 1, SVLEN_DG_MF:= SVLEN_DG2]
  MBRN[is.na(SVLEN_DG_MF) & twin_disc == 1 & miscar_31 == 1, SVLEN_DG_MF:= SVLEN_DG1]
  MBRN[is.na(SVLEN_DG_MF) & twin_disc == 1 & miscar_32 == 1, SVLEN_DG_MF:= SVLEN_DG2]
  MBRN[is.na(SVLEN_DG_MF) & trip_disc == 1 & induced_ab1 == 1, SVLEN_DG_MF:= SVLEN_DG1]
  MBRN[(is.na(SVLEN_DG_MF) & trip_disc == 1 & induced_ab2 == 1) | (trip_disc == 1 & induced_ab2 == 1 & induced_ab1 == 1 & SVLEN_DG2 > SVLEN_DG_MF), SVLEN_DG_MF:= SVLEN_DG2]
  MBRN[(is.na(SVLEN_DG_MF) & trip_disc == 1 & induced_ab3 == 1) | (trip_disc == 1 & induced_ab3 == 1 & (induced_ab1 == 1 | induced_ab2 == 1) & SVLEN_DG3 > SVLEN_DG_MF),SVLEN_DG_MF:= SVLEN_DG3]
  MBRN[is.na(SVLEN_DG_MF) & trip_disc == 1 & stillbirth1 == 1 & MBRN_TOP != 1, SVLEN_DG_MF:= SVLEN_DG1]
  MBRN[(is.na(SVLEN_DG_MF) & trip_disc == 1 & stillbirth2 == 1) | (trip_disc == 1 & stillbirth2 == 1 & stillbirth1 == 1 & SVLEN_DG2 > SVLEN_DG_MF) & MBRN_TOP != 1, SVLEN_DG_MF:= SVLEN_DG2]
  MBRN[(is.na(SVLEN_DG_MF) & trip_disc == 1 & stillbirth3 == 1) | (trip_disc == 1 & stillbirth3 == 1 & (stillbirth1 == 1 | stillbirth2 == 1) & SVLEN_DG3 > SVLEN_DG_MF) & MBRN_TOP != 1, SVLEN_DG_MF:= SVLEN_DG3]
  MBRN[is.na(SVLEN_DG_MF) & trip_disc == 1 & miscar_31 == 1 & MBRN_TOP != 1 & MBRN_stillbirth != 1, SVLEN_DG_MF:= SVLEN_DG1]
  MBRN[(is.na(SVLEN_DG_MF) & trip_disc == 1 & miscar_32 == 1) | (trip_disc == 1 & miscar_32 == 1 & miscar_31 == 1 & SVLEN_DG2 > SVLEN_DG_MF) & MBRN_TOP != 1 & MBRN_stillbirth != 1, SVLEN_DG_MF:= SVLEN_DG2]
  MBRN[(is.na(SVLEN_DG_MF) & trip_disc == 1 & miscar_33 == 1) | (trip_disc == 1 & miscar_33 == 1 & (miscar_31 == 1 | miscar_32 == 1) & SVLEN_DG3 > SVLEN_DG_MF) & MBRN_TOP != 1 & MBRN_stillbirth != 1, SVLEN_DG_MF:= SVLEN_DG3]
  MBRN[is.na(SVLEN_DG_MF) & qua_MMTT == 1 & induced_ab1 == 1, SVLEN_DG_MF:= SVLEN_DG1]
  MBRN[(is.na(SVLEN_DG_MF) & qua_MMTT == 1 & induced_ab2 == 1) | (qua_MMTT == 1 & induced_ab2 == 1 & induced_ab1 == 1 & SVLEN_DG2 > SVLEN_DG_MF), SVLEN_DG_MF:= SVLEN_DG2]
  MBRN[(is.na(SVLEN_DG_MF) & qua_MMTT == 1 & induced_ab3 == 1) | (qua_MMTT == 1 & induced_ab3 == 1 & (induced_ab1 == 1 | induced_ab2 == 1) & SVLEN_DG3 > SVLEN_DG_MF), SVLEN_DG_MF:= SVLEN_DG3]
  MBRN[(is.na(SVLEN_DG_MF) & qua_MMTT == 1 & induced_ab4 == 1) | (qua_MMTT == 1 & induced_ab4 == 1 & (induced_ab1 == 1 | induced_ab2 == 1 | induced_ab3 == 1) & SVLEN_DG4 > SVLEN_DG_MF), SVLEN_DG_MF:= SVLEN_DG4]

  ### [END]


  # Drop livebirth1 to qua_MMTT
  MBRN <- MBRN[, .(preg_id, MBRN_livebirth, MBRN_miscarriage, MBRN_stillbirth, MBRN_TOP, MBRN_unknown, DAGER_FDATO_MF, SVLEN_DG_MF)]

  MBRN[, DAGER_FDATO_MF:=as.Date(DAGER_FDATO_MF)]

  # Sort by preg_id
  setorder(MBRN, preg_id)

  # Save the data
  MBRN_check_multifetal = copy(MBRN)

  ##################### BLOCK 3 ########################
  ######################################################
  # Back to the child-level MBRN table: add the multiple-pregnancy outcome, keep the first child of each
  # pregnancy with the combined date/GA, use the child's own flags for singletons, drop unknown outcomes.
  cli::cli_progress_update()

  # Load MBRN_for_analysis dataset
  MBRN_for_analysis[, DAGER_FDATO := as.Date(DAGER_FDATO)]
  MBRN_for_analysis[, start := as.Date(start)]
  MBRN <- MBRN_for_analysis
  setorder(MBRN, preg_id)
  setkey(MBRN, preg_id)
  setkey(MBRN_check_multifetal, preg_id)

  # Merge with MBRN_check_multifetal
  MBRN <- merge(MBRN, MBRN_check_multifetal, by = "preg_id", all.y = TRUE, all.x = TRUE)


  # Now, we can drop multifetal pregnancies, keeping the first child of multiplets
  MBRN <- MBRN[PLUREK_new == 1]

  # rev 1.2
  MBRN[, DAGER_FDATO_MF := as.Date(DAGER_FDATO_MF)]
  MBRN[, DAGER_FDATO := as.Date(DAGER_FDATO)]
  MBRN[!is.na(DAGER_FDATO_MF) & DAGER_FDATO != DAGER_FDATO_MF, DAGER_FDATO := DAGER_FDATO_MF]
  MBRN[!is.na(SVLEN_DG_MF) & SVLEN_DG != SVLEN_DG_MF, SVLEN_DG := SVLEN_DG_MF]
  MBRN[, start := DAGER_FDATO-SVLEN_DG]
  MBRN[, SVLEN_DG_MF:=NULL][,DAGER_FDATO_MF:=NULL]
  MBRN_check_multifetal[, SVLEN_DG_MF:=NULL][,DAGER_FDATO_MF:=NULL]
  # Replace MBRN_livebirth to MBRN_unknown
  MBRN[, `:=`(MBRN_livebirth = ifelse(is.na(MBRN_livebirth) & livebirth == 1 & PLURAL_new == 1, 1, MBRN_livebirth),
              MBRN_miscarriage = ifelse(is.na(MBRN_miscarriage) & miscar_3 == 1 & PLURAL_new == 1, 1, MBRN_miscarriage),
              MBRN_stillbirth = ifelse(is.na(MBRN_stillbirth) & stillbirth == 1 & PLURAL_new == 1, 1, MBRN_stillbirth),
              MBRN_TOP = ifelse(is.na(MBRN_TOP) & induced_ab == 1 & PLURAL_new == 1, 1, MBRN_TOP),
              MBRN_unknown = ifelse(is.na(MBRN_unknown) & unknown == 1 & PLURAL_new == 1, 1, MBRN_unknown))]

  # Replace NA with 0
  MBRN[, `:=`(MBRN_livebirth = replace_na(MBRN_livebirth, 0),
              MBRN_miscarriage = replace_na(MBRN_miscarriage, 0),
              MBRN_stillbirth = replace_na(MBRN_stillbirth, 0),
              MBRN_TOP = replace_na(MBRN_TOP, 0),
              MBRN_unknown = replace_na(MBRN_unknown, 0))]

  # Exclude pregnancies with unknown outcome of pregnancy
  MBRN <- MBRN[MBRN_unknown != 1]

  # Drop MBRN_unknown
  MBRN[, MBRN_unknown := NULL]

  setorder(MBRN, preg_id)
  # Save the data
  MBRN_for_analysis = copy(MBRN)


  # Now we create a smaller file, only for merging purposes
  # Get rid of variables unnecessary for outcome classification
  MBRN <- MBRN[, .(preg_id, SVLEN_DG, DAGER_FDATO, PasientLopeNr_MOR, MBRN_livebirth, MBRN_miscarriage, MBRN_stillbirth, MBRN_TOP)]

  # Rename the ID variable
  setnames(MBRN, "PasientLopeNr_MOR", "ID")

  # Duplicate DAGER_FDATO from MBRN to save it from merging processes
  MBRN[, date_outcome := DAGER_FDATO]


  ##################### BLOCK 4 ########################
  ######################################################
  # NPR outcome records (O00 ectopic, O01 molar, O02-O03 miscarriage, O04 induced abortion) in the study
  # years, merged with the MBRN pregnancies of the same woman ending on the same date.

  ################# NPR_stata2020_mother_full ###########################
  cli::cli_alert_info("Analyzing hospital data: restricted to early non-live births, miscarriage, elective termination, molar pregnancy, ectopic pregnancy.")
  cli::cli_progress_update()

  # Load NPR_stata2020_mother_full dataset
  NPR <-copy(npr)

  # Restrict NPR to early non-live births (miscarriage, elective termination, molar pregnancy, ectopic pregnancy)
  NPR[, include := 0]
  NPR[grepl("O00|O01|O02|O03|O04",tilst,perl = TRUE,ignore.case = TRUE), include := 1]
  NPR <- NPR[include == 1]
  NPR[ ,include := NULL]

  # Drop records before 2008 and after 2018
  NPR <- NPR[year(inndato) >= min_year & year(inndato) <= max_year]

  # Generate new date of birth variable for merging purposes
  NPR[, date_outcome := as.Date(inndato)]

  # Sort by ID and inndato
  setorder(NPR, ID, inndato, stay_id)
  NPR[,inndato:=as.Date(inndato)]

  # Save the data
  MBRN[, date_outcome:=as.Date(date_outcome)]
  MBRN_for_analysis[, DAGER_FDATO:=as.Date(DAGER_FDATO)]
  NPR  <- merge(MBRN, NPR, by = c("ID", "date_outcome"), all.y = TRUE, all.x = TRUE)
  # Stata:
  # use MBRN_for_merge, clear
  # merge 1:m ID date_outcome using NPR_stata2020_mother_reduced

  setorder(NPR, ID, date_outcome, stay_id)
  #NPR <- NPR[order(ID, date_outcome)]
  NPR[is.na(tilst),tilst:=""]

  ##################### BLOCK 5 ########################
  ######################################################
  cli::cli_alert_info("Hospital data: handling multiple admissions.")
  cli::cli_progress_update()

  #We keep one admission per day, prioritizing molar pregnancies >> ectopic pregnancies >> miscarriages >> terminations
  NPR <- NPR[order(ID, date_outcome, stay_id)]

  # NPR_outcome = priority (1 molar, 2 ectopic, 3 miscarriage, 4 TOP); a record is dropped when a record of
  # the same date within 5 rows has a higher priority (equal priority: the earlier row is dropped)
  NPR[str_detect(tilst, "O01") , NPR_outcome :=1]
  NPR[str_detect(tilst, "O00") , NPR_outcome :=2]
  NPR[str_detect(tilst, "O02") | str_detect(tilst, "O03") , NPR_outcome :=3]
  NPR[str_detect(tilst, "O04") , NPR_outcome :=4]

  #Reordering
  NPR <- NPR[order(ID, stay_id, inndato, NPR_outcome, SVLEN_DG)]
  NPR[, drop:=NA_integer_]


  for (i in 1:5) {
    NPR[, drop:=fifelse(!is.na(fifelse(((inndato == shift_wrt_id(inndato, ID, +i) & NPR_outcome >= shift_wrt_id(NPR_outcome, ID, +i) ) |
                                          (inndato == shift_wrt_id(inndato, ID, -i) & NPR_outcome > shift_wrt_id(NPR_outcome, ID, -i))), 1L, NA_integer_)),1L,drop)]
  }

  NPR <- NPR[is.na(drop) | drop == 0]
  NPR[, drop := NULL]

  NPR <- NPR[order(ID, date_outcome, stay_id)]

  NPR[, NPR_outcome := NULL]

  # We first identify miscarriages that have been identified in both MBRN & NPR
  NPR[, sp_MBRN_NPR := ifelse((str_detect(tilst, "O02") | str_detect(tilst, "O03")) & MBRN_miscarriage == 1, 1, NA)]

  # We then identify TOPs that have been identified in both MBRN & NPR/MBRN
  NPR[, top_MBRN_NPR := ifelse(str_detect(tilst, "O04") & MBRN_TOP == 1, 1, NA)]

  # We then identify miscarriages that have been only identified in NPR
  NPR[, sp_NPR_only := ifelse(is.na(sp_MBRN_NPR) & (str_detect(tilst, "O02") | str_detect(tilst, "O03")), 1, NA)]

  # Identify TOPs that have been identified in NPR only
  NPR[, top_NPR_only := ifelse(is.na(top_MBRN_NPR) & str_detect(tilst, "O04"), 1, NA)]

  # We now create our variable for all outcomes identified in NPR
  NPR[, `:=`(count_sp_N = ifelse(sp_MBRN_NPR==1 | sp_NPR_only==1, 1, NA),
             count_top_N = ifelse(top_MBRN_NPR==1 | top_NPR_only==1, 1, NA),
             count_molar = ifelse(str_detect(tilst, "O01"), 1, NA),
             count_ectopic = ifelse(str_detect(tilst, "O00"), 1, NA))]


  ##################### BLOCK 6 ########################
  ######################################################
  # Repeated NPR records of one event: a woman's records form episodes that start at a record and include
  # the records up to same_event_days - 1 (v1.3: 55) days later. A record inside such an episode (not its first)
  # and not linked to an MBRN pregnancy is dropped.
  cli::cli_alert_info("Merging Birth registry and Hospital data: Calculate difference between outcome dates.")
  cli::cli_progress_update()
  setkey(NPR, ID)

  setorder(NPR, ID, date_outcome, na.last = TRUE)
  NPR[, gap_previous := as.numeric(date_outcome - shift_wrt_id(date_outcome, ID))]

  # Based on NPR only
  setorder(NPR, ID, inndato, na.last = TRUE)
  NPR[, gap_previous_NPR := as.numeric(inndato - shift_wrt_id(inndato, ID))]

  NPR[, episode_no := fifelse(!is.na(inndato),seq_len(.N),NA_integer_), by = ID]

  ### TODO: OPTIMISE CODE (Done)
  # C++ helpers: update_episode_start() gives each record the start of its episode (a record more than d days
  # after the current start opens a new episode); update_episode_no_gap() numbers the episodes of a woman
  Rcpp::sourceCpp(env = environment(), code = "
          #include <Rcpp.h>
          // [[Rcpp::export]]
          Rcpp::DateVector update_episode_start(const Rcpp::DateVector dato, const Rcpp::DateVector es, const Rcpp::NumericVector ID, const Rcpp::NumericVector dv) {
          Rcpp::DateVector res = es;
          double d = dv[0];
          res(0) = es(0);
          for (int i=1; i<es.length(); i++) {
            if ( ID(i) == ID(i-1) && (dato(i) - res(i-1)) <= d && R_IsNA(res(i)) && !R_IsNA(res(i-1))) res(i) = res(i-1);
            if ( ID(i) == ID(i-1) && (dato(i) - res(i-1)) >  d && R_IsNA(res(i)) && !R_IsNA(res(i-1))) res(i) = dato(i);
          }
          return res;
          }
          ")

  Rcpp::sourceCpp(env = environment(), code = "
          #include <Rcpp.h>
          // [[Rcpp::export]]
          Rcpp::NumericVector update_episode_no_gap(const Rcpp::DateVector es, const Rcpp::NumericVector ec, const Rcpp::NumericVector en, const Rcpp::NumericVector ID) {
          Rcpp::NumericVector res = en;
          for (int i=1; i<en.length(); i++) {
             if ( ID(i) == ID(i-1) && R_IsNA(res(i)) && !R_IsNA(res(i-1)) && !R_IsNA(es(i))) res(i) = ec(i)+res(i-1);
          }
          return res;
          }
          ")

  # [v2.0.0] v1.3 computed the thresholds 1..60 days and used only 55 (same_event_days - 1); only that one is computed now
  for (i in same_event_episode_days) {
    es = paste0("episode_start", i)
    #NPR[, (es) := NA_Date_]
    NPR[episode_no == 1, (es) := inndato]
    NPR[, (es) := update_episode_start(inndato, get(es), ID, i)]

    # Calculate episode_count and episode_no_gap
    ec = paste0("episode_count", i)
    NPR[, (ec) := fifelse(episode_no == 1, 1, NA_integer_)]
    NPR[, (ec) := fifelse(!is.na(get(es)),
                          fifelse(equal(shift_wrt_id(get(es), ID),get(es)), 0, 1),
                          get(ec))]


    en = paste0("episode_no_gap", i)
    NPR[ , (en) := NA_integer_]
    NPR[episode_no == 1, (en) := 1]
    NPR[, (en) := update_episode_no_gap(get(es), get(ec), get(en), ID)]

  }


  # Drop if episode_count55 == 0 & preg_id == .   [v2.0.0] 55 = same_event_days - 1
  NPR <- NPR[!(get(paste0("episode_count", same_event_episode_days)) == 0 & is.na(preg_id))]

  NPR = NPR[,c("ID", "date_outcome", "preg_id", "SVLEN_DG", "DAGER_FDATO", "MBRN_livebirth", "MBRN_miscarriage",
               "MBRN_stillbirth", "MBRN_TOP", "stay_id", "diagno", "tilst", "inndato", "sp_MBRN_NPR",
               "top_MBRN_NPR", "sp_NPR_only", "top_NPR_only", "count_sp_N", "count_top_N", "count_molar", "count_ectopic" ),
            with = FALSE]

  ##################### BLOCK 7 ########################
  ######################################################
  # Remove NPR events that belong to an MBRN pregnancy: within same_event_days (default 56) days after an MBRN outcome, or between
  # the start and the outcome date of an MBRN pregnancy (dates carried from neighbouring rows of the woman).
  cli::cli_progress_update()

  # We drop miscarriages and TOPs that occurred within same_event_days (default 56) days after a registered pregnancy in MBRN
  NPR[, MBRN_date_ref := DAGER_FDATO]
  NPR[,inndato:=as.Date(inndato)]
  setorder(NPR,ID, date_outcome, na.last=TRUE)
  NPR[, MBRN_date_ref := fifelse(is.na(MBRN_date_ref),shift_wrt_id(MBRN_date_ref, ID),MBRN_date_ref)]
  NPR[, within_56day_preg := as.numeric(fifelse((inndato - MBRN_date_ref) > 0 &
                                                  (inndato - MBRN_date_ref) < cfg_same_event_days, 1, NA_integer_))]   # [v2.0.0] v1.3: 56
  NPR <- NPR[is.na(within_56day_preg)]

  # Drop MBRN_date_ref and within_56day_preg
  NPR[, c("MBRN_date_ref", "within_56day_preg") := NULL]

  # We drop events that occurred during a registered pregnancy in MBRN.
  NPR[, MBRN_date_ref := DAGER_FDATO]
  setorder(NPR,ID, -date_outcome)
  setkey(NPR, ID)
  for(j in 1:5){
    NPR[, MBRN_date_ref := fifelse(is.na(MBRN_date_ref),shift_wrt_id(MBRN_date_ref,ID),MBRN_date_ref)]
  }

  NPR[, MBRN_preg_start := DAGER_FDATO - SVLEN_DG]

  for(j in 1:5){
    NPR[, MBRN_preg_start := fifelse(is.na(MBRN_preg_start),shift_wrt_id(MBRN_preg_start,ID),MBRN_preg_start)]
  }

  NPR[(inndato > MBRN_preg_start) & (inndato < MBRN_date_ref), within_preg := 1]

  cli::cli_alert_info("Explore gestational age distribution of excluded events.")


  NPR[, DAGER_FDATO:=as.Date(DAGER_FDATO)]

  # Explore gestational age distribution of excluded events
  NPR[within_preg == 1, GA_days_event := as.numeric(inndato - MBRN_preg_start)]
  NPR[ , GA_weeks_event := floor(GA_days_event / 7)]
  NPR[within_preg == 1, days_before := as.numeric(MBRN_date_ref - inndato)]
  NPR[ , weeks_before := floor(days_before / 7)]

  # Drop if within_preg == 1
  NPR <- NPR[is.na(within_preg)  |  within_preg!= 1]

  # Drop MBRN_date_ref to GA_weeks_event
  NPR <- NPR[, c("SVLEN_DG","ID","DAGER_FDATO","preg_id","MBRN_livebirth","MBRN_miscarriage","MBRN_stillbirth",
                 "MBRN_TOP","date_outcome","stay_id","diagno","tilst" , "inndato", "sp_MBRN_NPR",
                 "top_MBRN_NPR","sp_NPR_only", "top_NPR_only", "count_sp_N", "count_top_N", "count_molar","count_ectopic",
                 "days_before", "weeks_before")]

  ##################### BLOCK 8 ########################
  ######################################################
  cli::cli_progress_update()
  # Identify events already identified in MBRN
  # MC/TOP_source_NPR: 1 NPR only, 2 MBRN and NPR. "Misclassified": an NPR outcome on the date of an MBRN
  # record with a different outcome. Then the NPR events are merged back to the full MBRN table.

  NPR[sp_NPR_only == 1, MC_source_NPR:=1]
  NPR[sp_MBRN_NPR == 1, MC_source_NPR:=2]

  NPR[top_NPR_only == 1, TOP_source_NPR:=1]
  NPR[top_MBRN_NPR == 1, TOP_source_NPR:=2]


  # Identify misclassified events
  NPR[, `:=`(NPR_molar_misclassified = fifelse(count_molar == 1 & (MBRN_livebirth == 1 | MBRN_stillbirth == 1 | MBRN_miscarriage == 1 | MBRN_TOP == 1), 1, NA_integer_),
             NPR_ectopic_misclassified = fifelse(count_ectopic == 1 & (MBRN_livebirth == 1 | MBRN_stillbirth == 1 | MBRN_miscarriage == 1 | MBRN_TOP == 1), 1, NA_integer_),
             NPR_MC_misclassified = fifelse(sp_NPR_only == 1 & (MBRN_livebirth == 1 | MBRN_stillbirth == 1 | MBRN_TOP == 1), 1, NA_integer_),
             NPR_TOP_misclassified = fifelse(top_NPR_only == 1 & (MBRN_livebirth == 1 | MBRN_miscarriage == 1 | MBRN_stillbirth == 1), 1, NA_integer_))]

  # Assign a new pregnancy ID for events only identified in NPR (as they would not be identified in MBRN)
  # [v2.0.0] v1.3 added 1000000 to the row number: with more than 1 million MBRN pregnancies (ids = row numbers) the ids
  # collided, and with more than 1 million rows here the NPR ids reached the KUHR ids (2000000 + row number, BLOCK 11),
  # so the merge by preg_id joined a KUHR-only miscarriage of one woman to an NPR pregnancy of another (the KUHR
  # miscarriage was lost and the other woman got a second pregnancy on her NPR outcome date). The offset is now
  # 1000000 or the largest id already given, whichever is larger (the same ids as v1.3 when there is no collision).
  npr_id_offset = max(1000000, suppressWarnings(max(NPR$preg_id, na.rm = TRUE)), suppressWarnings(max(MBRN_for_analysis$preg_id, na.rm = TRUE)))
  NPR[, preg_id := fifelse(is.na(preg_id), .I + npr_id_offset, preg_id)]

  # Drop unnecessary variables
  NPR <- NPR[, !c("SVLEN_DG", "MBRN_livebirth", "MBRN_miscarriage", "MBRN_stillbirth", "MBRN_TOP", "stay_id", "diagno", "tilst"), with = FALSE]

  # Rename the ID variable
  setnames(NPR,  "ID", "PasientLopeNr_MOR")

  # Merge to the full MBRN data
  MBRN_for_analysis <- MBRN_for_analysis[,-c("DAGER_FDATO", "PasientLopeNr_MOR")]
  MBRN <- merge(NPR,MBRN_for_analysis, by = "preg_id", all.x = TRUE)

  # Sort by PasientLopeNr_MOR and date_outcome
  setorder(MBRN, PasientLopeNr_MOR, date_outcome, na.last=TRUE)

  MBRN[,date_outcome:=NULL]
  # Create a new date of outcome variable
  MBRN[, date_outcome := DAGER_FDATO]
  MBRN[is.na(date_outcome), date_outcome := inndato]

  # Save the data
  MBRN_NPR_for_analysis = copy(MBRN)

  # Prepare for KUHR merging

  # Get rid of variables unnecessary for outcome classification
  MBRN <- MBRN[, .(PasientLopeNr_MOR, count_sp_N, count_top_N,preg_id, SVLEN_DG,
                   inndato,
                   MBRN_livebirth, MBRN_miscarriage, MBRN_stillbirth, MBRN_TOP, sp_MBRN_NPR, count_molar, count_ectopic, date_outcome,
                   top_MBRN_NPR, sp_NPR_only, top_NPR_only,DAGER_FDATO)]

  # Rename the ID variable and count_sp_N and count_top_N
  setnames(MBRN, c("PasientLopeNr_MOR", "count_sp_N", "count_top_N"), c("ID", "count_NPR_sp", "count_NPR_top"))


  # Create outcome_after_NPR
  # (the MBRN outcome first; otherwise NPR miscarriage, TOP, molar, ectopic, in this order)
  #MBRN[, outcome_after_NPR := ifelse(MBRN_livebirth == 1, 1, ifelse(MBRN_miscarriage == 1, 2, ifelse(MBRN_TOP == 1, 3, ifelse(MBRN_stillbirth == 1, 4, ifelse(count_NPR_sp == 1 & is.na(outcome_after_NPR), 2, ifelse(count_NPR_top == 1 & is.na(outcome_after_NPR), 3, ifelse(count_molar == 1 & is.na(outcome_after_NPR), 5, ifelse(count_ectopic == 1 & is.na(outcome_after_NPR), 6, NA))))))))]
  MBRN[MBRN_livebirth == 1, outcome_after_NPR := 1]
  MBRN[MBRN_miscarriage == 1, outcome_after_NPR := 2]
  MBRN[MBRN_TOP == 1, outcome_after_NPR := 3]
  MBRN[MBRN_stillbirth == 1, outcome_after_NPR := 4]

  MBRN[count_NPR_sp == 1 & is.na(outcome_after_NPR), outcome_after_NPR := 2]
  MBRN[count_NPR_top == 1 & is.na(outcome_after_NPR), outcome_after_NPR := 3]
  MBRN[count_molar == 1 & is.na(outcome_after_NPR), outcome_after_NPR := 5]
  MBRN[count_ectopic == 1 & is.na(outcome_after_NPR), outcome_after_NPR := 6]

  #labels:
  # 1 "Livebirth" 2 "Miscarriage" 3 "Termination of pregnancy" 4 "Stillbirth" 5 "Molar pregnancy" 6 "Ectopic pregnancy"



  ##################### BLOCK 9 ########################
  ######################################################

  ############### KUHR_mother_stata_full ####################
  # Load KUHR_mother_stata_full dataset
  # KUHR miscarriages (ICD-10 O02/O03, ICPC-2 W82) merged with the MBRN+NPR outcomes by woman and date,
  # one record per day; repeated records are then grouped into episodes of same_event_days - 1 days as in block 6.
  cli::cli_alert_info("Analyzing primary care data: restricted to early non-live births, miscarriage, elective termination, molar pregnancy, and ectopic pregnancy.")
  cli::cli_progress_update()
  KUHR <- copy(kuhr)

  # Restrict KUHR to early non-live births (miscarriage, elective termination, molar pregnancy, ectopic pregnancy)
  KUHR[, include := 0]
  KUHR[grepl("O02|O03",diagnose,perl = TRUE,ignore.case = TRUE) & diagnosekodeverk=="ICD-10", include := 1]
  KUHR[grepl("W82",diagnose,perl = TRUE,ignore.case = TRUE) & diagnosekodeverk=="ICPC-2", include := 1]

  KUHR <- KUHR[include == 1]
  KUHR[,include:=NULL]

  # Drop records before 2008 and after 2018
  KUHR <- KUHR[year(ant_dager) >= min_year & year(ant_dager) <= max_year]

  # Generate new date of outcome variable for merging purposes
  KUHR[, date_outcome := as.Date(ant_dager)]

  # Sort by ID and date_outcome
  setorder(KUHR, ID, date_outcome, diagnose, na.last = TRUE)

  # Merge with MBRN
  MBRN <- merge(MBRN, KUHR, by = c("ID", "date_outcome"), all = TRUE)

  # Sort by ID and date_outcome
  setorder(MBRN, ID, date_outcome, na.last = TRUE)

  # change order of rows to match with STATA (Not part of the cdoe. Remove it later)
  setorder(MBRN, ID, date_outcome, diagnose, na.last = TRUE)

  # We keep one admission per day
  setkey(MBRN,ID)
  setorder(MBRN, ID, date_outcome, na.last = TRUE)
  MBRN[,drop:=0]
  MBRN[!is.na(ant_dager) & ant_dager==shift(ant_dager) & ID == shift(ID),drop:=1]
  MBRN <- MBRN[drop!=1]
  MBRN[,drop:=NULL]

  # Identify miscarriages
  MBRN[, miscar := ifelse(grepl("O02|O03|W82",diagnose,perl = TRUE,ignore.case = TRUE), 1, NA)]

  # We first identify miscarriages that have been already identified in MBRN only
  MBRN[, sp_MBRN_KUHR := fifelse(miscar == 1 & MBRN_miscarriage == 1, 1, NA_integer_)]

  # We then identify miscarriages that have been only identified in KUHR
  MBRN[, sp_KUHR_only := fifelse(miscar == 1 & is.na(sp_MBRN_KUHR), 1, NA_integer_)]

  # We now create our marker for miscarriages
  MBRN[, count_KUHR_sp := fifelse(!is.na(sp_MBRN_KUHR) | !is.na(sp_KUHR_only), 1, NA_integer_)]

  # Drop miscar
  MBRN[, miscar := NULL]

  # # Calculate difference between outcome dates
  # setorder(MBRN,ID, date_outcome,na.last = TRUE)

  setkey(MBRN,ID)
  setorder(MBRN, ID, date_outcome, DAGER_FDATO)
  MBRN[, gap_previous := as.numeric(date_outcome - shift_wrt_id(date_outcome, ID))]

  # Based on KUHR only
  setorder(MBRN,ID, ant_dager,na.last = TRUE)
  MBRN[, gap_previous_KUHR := as.numeric(ant_dager - shift_wrt_id(ant_dager, ID))]

  # Days since first episode within KUHR
  MBRN[!is.na(ant_dager), episode_no := seq_along(.I), by = ID]

  ## TODO: OPTIMISE the code for the loop (Done)
  MBRN[,ant_dager:=as.Date(ant_dager)]
  for (i in same_event_episode_days) {   # [v2.0.0] v1.3: 1:60, of which only 55 was used
    es = paste0("episode_start", i)
    MBRN[, (es) := fifelse(episode_no == 1, ant_dager, NA_Date_)]
    MBRN[, (es) := update_episode_start(ant_dager, get(es), ID, i)]


    ec = paste0("episode_count", i)
    MBRN[, (ec) := fifelse(episode_no == 1, 1, NA_integer_)]
    MBRN[, (ec) := fifelse(!is.na(get(es)),
                           fifelse(equal(shift_wrt_id(get(es), ID),get(es)), 0, 1),
                           get(ec))]

    en = paste0("episode_no_gap", i)

    # MBRN[, (en) := fifelse(episode_no == 1, 1, NA_integer_)]
    MBRN[ , (en) := NA_integer_]
    MBRN[episode_no == 1, (en) := 1]
    MBRN[, (en) := update_episode_no_gap(get(es), get(ec), get(en), ID)]
  }

  # Drop if episode_count55 == 0 & preg_id == .
  MBRN[,drop:=fifelse(get(paste0("episode_count", same_event_episode_days)) == 0 & is.na(preg_id),1,0,0)]   # [v2.0.0] 55 = same_event_days - 1
  MBRN <- MBRN[drop==0]
  MBRN[,drop:=NULL]


  #################### BLOCK 10 ########################
  ######################################################
  # Remove KUHR miscarriage records that belong to another event: within same_event_days (default 56) days before or after a non-birth
  # outcome of MBRN/NPR, within same_event_days (default 56) days after an MBRN outcome, or during an MBRN pregnancy.
  cli::cli_progress_update()
  # Exclude if within same_event_days (default 56) days before an event registered in MBRN or NPR
  setorder(MBRN, ID, date_outcome, na.last = TRUE)
  MBRN[, earlier_diagnosis := fifelse(is.na(preg_id) &
                                        (shift_wrt_id(date_outcome, ID, -1) - ant_dager < cfg_same_event_days) &   # [v2.0.0] v1.3: 56
                                        (shift_wrt_id(outcome_after_NPR, ID, -1) %in% c(2, 3, 5, 6)), 1, NA_integer_)]
  MBRN <- MBRN[is.na(earlier_diagnosis)]

  # Exclude if within same_event_days (default 56) days after a miscarriage / TOP registered in MBRN or NPR
  MBRN[, later_diagnosis := fifelse(is.na(preg_id) &
                                      (ant_dager - shift_wrt_id(date_outcome, ID) < cfg_same_event_days) & (ant_dager - shift_wrt_id(date_outcome, ID) > 0) &   # [v2.0.0] v1.3: 56
                                      (shift_wrt_id(outcome_after_NPR, ID) %in% c(2, 3, 5, 6)), 1, NA_integer_)]
  MBRN <- MBRN[is.na(later_diagnosis)]

  # Drop gap_previous to later_diagnosis
  MBRN <- MBRN[, !c(paste0(c("episode_no_gap", "episode_start", "episode_count"), same_event_episode_days),
                    "earlier_diagnosis", "later_diagnosis",
                    "gap_previous", "gap_previous_KUHR", "episode_no"), with = FALSE]

  # We drop miscarriages and TOPs that occurred within same_event_days (default 56) days after a registered pregnancy in MBRN
  MBRN[, MBRN_date_ref := DAGER_FDATO]
  setorder(MBRN, ID, date_outcome, na.last = TRUE)
  for(i in 1:20)
    MBRN[, MBRN_date_ref := fifelse(is.na(MBRN_date_ref), shift_wrt_id(MBRN_date_ref, ID), MBRN_date_ref)]

  MBRN[, within_56day_preg := fifelse((ant_dager - MBRN_date_ref > 0) & (ant_dager - MBRN_date_ref < cfg_same_event_days), 1, NA_integer_)]   # [v2.0.0] v1.3: 56
  MBRN <- MBRN[is.na(within_56day_preg)]

  # Drop MBRN_date_ref and within_56day_preg
  MBRN[, c("MBRN_date_ref", "within_56day_preg") := NULL]

  # We drop miscarriages and TOPs that occurred during a registered pregnancy in MBRN.
  MBRN[, MBRN_date_ref := DAGER_FDATO]
  setorder(MBRN, ID, -date_outcome, na.last = TRUE)
  for(i in 1:20)
    MBRN[, MBRN_date_ref := fifelse(is.na(MBRN_date_ref), shift_wrt_id(MBRN_date_ref, ID), MBRN_date_ref)]

  MBRN[, MBRN_preg_start := DAGER_FDATO - SVLEN_DG]

  for(i in 1:20)
    MBRN[, MBRN_preg_start := fifelse(is.na(MBRN_preg_start), shift_wrt_id(MBRN_preg_start, ID), MBRN_preg_start)]

  MBRN[, within_preg := ifelse((ant_dager > MBRN_preg_start) & (ant_dager < MBRN_date_ref), 1, NA_integer_)]

  # Explore gestational age distribution of excluded events
  MBRN[within_preg == 1, GA_days_event := as.numeric(ant_dager - MBRN_preg_start)]
  MBRN[, GA_weeks_event := floor(GA_days_event / 7)]
  MBRN[within_preg == 1, days_before :=  as.numeric(MBRN_date_ref - ant_dager)]
  MBRN[, weeks_before := floor(days_before / 7)]

  # Drop if within_preg == 1
  MBRN[,drop:=0]
  MBRN[within_preg == 1, drop:=1]
  MBRN <- MBRN[drop == 0]
  MBRN[,drop:=NULL]

  # Drop MBRN_date_ref to GA_weeks_event
  MBRN <- MBRN[, !c("MBRN_date_ref", "MBRN_preg_start", "within_preg", "GA_days_event", "GA_weeks_event"), with = FALSE]

  # Identify events already identified in MBRN
  MBRN[sp_KUHR_only == 1 & is.na(outcome_after_NPR), MC_source_KUHR:= 1]
  MBRN[count_KUHR_sp == 1 & outcome_after_NPR == 2 & is.na(MC_source_KUHR), MC_source_KUHR:= 2]
  MBRN[count_KUHR_sp == 1 & outcome_after_NPR != 2 & !is.na(outcome_after_NPR), KUHR_MC_misclassified:= 1]

  # Identify final outcome
  # (a KUHR-only miscarriage where MBRN/NPR give no outcome; otherwise the MBRN/NPR outcome)
  MBRN[, outcome_final := fifelse(sp_KUHR_only == 1 & is.na(outcome_after_NPR), 2, outcome_after_NPR)]

  ##################### BLOCK 11 ########################
  ######################################################
  # Merge the KUHR results back into the MBRN+NPR table and code the data sources of each outcome.
  cli::cli_progress_update()
  setorder(MBRN, ID, -date_outcome, na.last = TRUE)
  # Assign a new pregnancy ID for miscarriages identified in KUHR
  # [v2.0.0] offset 2000000 or the largest id of the MBRN and NPR pregnancies, whichever is larger (see the NPR ids above)
  kuhr_id_offset = max(2000000, suppressWarnings(max(MBRN$preg_id, na.rm = TRUE)), suppressWarnings(max(MBRN_NPR_for_analysis$preg_id, na.rm = TRUE)))
  MBRN[, preg_id := fifelse(is.na(preg_id), .I + kuhr_id_offset, preg_id)]
  if(anyDuplicated(MBRN$preg_id[MBRN$preg_id > kuhr_id_offset]) || any(MBRN$preg_id[MBRN$preg_id > kuhr_id_offset] %in% MBRN_NPR_for_analysis$preg_id))
    stop("Internal error: the KUHR pregnancy ids are not unique.", call. = FALSE)

  # Drop unnecessary variables
  MBRN <- MBRN[, !c("SVLEN_DG", "MBRN_livebirth", "date_outcome", "MBRN_miscarriage", "MBRN_stillbirth",
                    "MBRN_TOP",
                    "diagnose", "diagnosekodeverk"), with = FALSE]

  # Rename the ID variable
  setnames(MBRN, "ID", "PasientLopeNr_MOR")

  # Save the data

  # Merge to the full MBRN_NPR data
  setorder(MBRN_NPR_for_analysis, PasientLopeNr_MOR, na.last = TRUE)
  MBRN <- merge(MBRN_NPR_for_analysis, MBRN, by = "preg_id", all = TRUE, suffixes = c("","__y__y"))
  MBRN[,PasientLopeNr_MOR:=fifelse(is.na(PasientLopeNr_MOR),PasientLopeNr_MOR__y__y,PasientLopeNr_MOR)]
  cns = colnames(MBRN)[grepl("__y__y$",colnames(MBRN),perl = TRUE)]
  MBRN = MBRN[,!cns, with = FALSE]

  setorder(MBRN, PasientLopeNr_MOR, na.last = TRUE)

  # Create a new date of outcome variable
  MBRN[, date_outcome := fifelse(is.na(date_outcome), ant_dager, date_outcome)]

  # Create variable to indicate source of outcome
  # MC_source: 1 MBRN+NPR+KUHR, 2 MBRN+NPR, 3 MBRN+KUHR, 4 MBRN, 5 NPR+KUHR, 6 NPR, 7 KUHR;
  # TOP_source: 2 MBRN+NPR, 4 MBRN, 6 NPR (misclassified NPR/KUHR events get no source)
  setnames(MBRN, "PasientLopeNr_MOR", "ID")

  MBRN[, MC_source:=NA_integer_]
  MBRN[MBRN_miscarriage == 1 & count_sp_N == 1 & count_KUHR_sp == 1, MC_source:=1]
  MBRN[MBRN_miscarriage == 1 & count_sp_N == 1 & is.na(count_KUHR_sp), MC_source:=2]
  MBRN[MBRN_miscarriage == 1 & is.na(count_sp_N) & count_KUHR_sp == 1, MC_source:=3]
  MBRN[MBRN_miscarriage == 1 & is.na(count_sp_N) & is.na(count_KUHR_sp), MC_source:=4]
  MBRN[is.na(MBRN_miscarriage) & count_sp_N == 1 & count_KUHR_sp == 1 & is.na(NPR_MC_misclassified) & is.na(KUHR_MC_misclassified), MC_source:=5]
  MBRN[is.na(MBRN_miscarriage) & count_sp_N == 1 & is.na(count_KUHR_sp) & is.na(NPR_MC_misclassified), MC_source:=6]
  MBRN[is.na(MBRN_miscarriage) & is.na(count_sp_N) & count_KUHR_sp == 1 & is.na(KUHR_MC_misclassified), MC_source:=7]

  MBRN[MBRN_TOP == 1 & count_top_N == 1, TOP_source:=2]
  MBRN[MBRN_TOP == 1 & is.na(count_top_N), TOP_source:=4]
  MBRN[is.na(MBRN_TOP) & count_top_N == 1 & is.na(NPR_TOP_misclassified), TOP_source:=6]

  # Save the data
  # Only include variables necessary for analytical studies (although this may need a tailored approach for specific sensitivity analyses)
  MBRN <- MBRN[, c("ID", "MORS_ALDER", "preg_id", "DAGER_FDATO", "start", "date_outcome", "outcome_final", "MC_source", "TOP_source"), with = FALSE]

  setorder(MBRN, ID, date_outcome, preg_id, na.last = TRUE)
  # Save the data


  ##################### BLOCK 12 ########################
  #######################################################
  cli::cli_alert_info("Dating of the early non-live births")
  cli::cli_progress_update()

  # Dating of the early non-live births
  # Pregnancies without MBRN gestational age are dated by early-pregnancy markers, first NPR (here), then
  # KUHR (block 13); MBRN pregnancies keep their MBRN start (pr_start_new = start).
  # Select appropriate codes from NPR
  NPR <- copy(npr)
  NPR[,tilst:=trimws(tilst)]
  NPR <- NPR[tilst != ""]
  NPR <- NPR[grepl(paste0(npr_codes$code,collapse = "|"), tilst, perl = TRUE)]
  NPR <- NPR[, c("stay_id", "diagno") := NULL]
  setorder(NPR, ID, inndato, na.last = TRUE)
  NPR[, diagno := seq_len(.N), by = ID]
  dlen = max(NPR[,diagno])
  NPR <- dcast(NPR, ID ~ diagno, value.var = c("tilst", "inndato"), sep = "")

  # Link to identified outcomes
  enb <- copy(MBRN)

  early_nonlive_births <- merge(enb, NPR, by = "ID", all.x = TRUE)

  # Calculate gestational age of selected NPR events in MBRN detected pregnancies
  codes <- npr_codes$code


  ## TODO: OPTIMISE the code for the loop (Done)
  # update_code_dates(): for one marker code, <code>_date = the woman's earliest record of that code dated within
  # the MBRN pregnancy (start..outcome) or from marker_lookback_days (v1.3: 70) before the outcome to the outcome date
  Rcpp::sourceCpp(env = environment(), code = '
          #include <Rcpp.h>
          using namespace Rcpp;
          // [[Rcpp::export]]
void update_code_dates(DataFrame &early_nonlive_births, CharacterVector codes, std::string event_col_prefix, std::string evet_date_col_prefix, const int dlen, const int lookback) {
    int n = early_nonlive_births.nrows();
    //NumericVector MORS_ALDER = Rcpp::as<NumericVector>(early_nonlive_births["MORS_ALDER"]);
    //DateVector start = Rcpp::as<DateVector>(early_nonlive_births["start"]);
    //DateVector date_outcome = Rcpp::as<DateVector>(early_nonlive_births["date_outcome"]);
    //DateVector DAGER_FDATO = Rcpp::as<DateVector>(early_nonlive_births["DAGER_FDATO"]);
    NumericVector MORS_ALDER = early_nonlive_births["MORS_ALDER"];
    DateVector start = early_nonlive_births["start"];
    DateVector date_outcome = early_nonlive_births["date_outcome"];
    DateVector DAGER_FDATO = early_nonlive_births["DAGER_FDATO"];

    for (int i = 0; i < codes.size(); ++i) {
        std::string code = Rcpp::as<std::string>(codes[i]);
        std::string vname = code + "_date";

        //DateVector vdate = Rcpp::as<DateVector>(early_nonlive_births[vname]);
        DateVector vdate = early_nonlive_births[vname];
        for (int j = 1; j <= dlen; ++j) {
            std::string inndi = evet_date_col_prefix + std::to_string(j);
            std::string tilsti = event_col_prefix + std::to_string(j);

            //DateVector indiv = Rcpp::as<DateVector>(early_nonlive_births[inndi]);
            //StringVector tidiv = Rcpp::as<StringVector>(early_nonlive_births[tilsti]);
            DateVector indiv = early_nonlive_births[inndi];
            StringVector tidiv = early_nonlive_births[tilsti];

            for (int k = 0; k < n; ++k) {
            auto indi = indiv(k);
            auto tildi = tidiv(k);
            auto mini = MORS_ALDER(k);
            auto si = start(k);
            auto doi = date_outcome(k);
            auto dfi = DAGER_FDATO(k);
                if (R_IsNA(vdate(k)) &&
                    tildi == code &&
                    ((indi >= si && indi <= doi && !R_IsNA(mini)) || (((indi-doi)>=-lookback) && indi <= doi))
                    ){
                    vdate(k) = 1*indi;
                }
            }
        }

    }
}
          ')



  pb=txtProgressBar(0,length(codes),style = 3)
  k = 0
  for (code in codes) {
    k = k+1
    setTxtProgressBar(pb,k)
    vname = paste0(code, "_date")
    early_nonlive_births[, (vname) := Date(.N)]
    update_code_dates(early_nonlive_births, code, event_col_prefix = "tilst", evet_date_col_prefix = "inndato", dlen = dlen,
                      lookback = cfg_marker_lookback_days)   # [v2.0.0] v1.3: 70
  }
  cat("\n")

  # Impute pregnancy start date based on early pregnancy markers
  # Codes in list order (= priority): start = marker date - days, if the marker lies within
  # marker_rejection_days before the outcome. Without MBRN GA, a start implying a length of marker_rejection_days
  # or more is rejected (flag_over12weeks) and the next code is tried.
  codes <- npr_codes$code
  days <- npr_codes$days
  early_nonlive_births[, pr_start_new := as.Date(start)]
  early_nonlive_births[, flag_over12weeks := NA_integer_]
  for (i in seq_along(codes)) {
    code = codes[i]
    dday = days[i]
    vname = paste0(code, "_date")
    early_nonlive_births[is.na(pr_start_new) & get(vname) >= (date_outcome - cfg_marker_rejection_days) & get(vname) <= date_outcome, pr_start_new := get(vname) - dday]
    early_nonlive_births[is.na(start) & ((date_outcome - pr_start_new) >= cfg_marker_rejection_days) & !is.na(pr_start_new), flag_over12weeks := 1]
    early_nonlive_births[is.na(start) & ((date_outcome - pr_start_new) >= cfg_marker_rejection_days), pr_start_new := NA_Date_]
  }


  # Calculate number of repaired flags
  # (repaired: a marker was rejected but another one was accepted)
  early_nonlive_births[flag_over12weeks == 1 & !is.na(pr_start_new), repaired_flag := 1]

  # Drop unnecessary variables
  early_nonlive_births = early_nonlive_births[, c("MORS_ALDER", "ID", "DAGER_FDATO", "start", "preg_id",
                                                  "date_outcome", "outcome_final", "MC_source" , "TOP_source",
                                                  "pr_start_new", "flag_over12weeks", "repaired_flag"), with = FALSE]

  ##################### BLOCK 13 ########################
  #######################################################
  # The same marker dating with the KUHR markers (ICD-10 and ICPC-2), for pregnancies still undated; only the
  # first min(60, 99.8th percentile) marker records of each woman are used.

  cli::cli_progress_update()

  KUHR <- copy(kuhr)

  KUHR[, filter := FALSE]
  vocabs = unique(kuhr_codes[,vocab])
  for(j in seq_along(vocabs)){
    voc = vocabs[j]
    vcodes = kuhr_codes[vocab == voc, code]
    pat = paste0(kuhr_codes$code, collapse = "|")
    KUHR[, filter := filter |
           grepl(voc,diagnosekodeverk,fixed = TRUE) &
           grepl(pat,diagnose, perl = TRUE)]
  }
  KUHR = KUHR[filter==TRUE]
  KUHR[,filter:=NULL]
  KUHR = KUHR[, .(diagnose, ID, ant_dager)]
  setkey(KUHR, ID)
  setorder(KUHR, ID, ant_dager, diagnose, na.last = TRUE)
  KUHR[, diagno := seq_len(.N), by = ID]
  dlen = max(KUHR[, diagno],na.rm = TRUE)
  dmax = min(60, ceiling(quantile(KUHR[, diagno], 0.998, na.rm = TRUE)))
  KUHR = KUHR[diagno<=dmax]
  dlen = max(KUHR[, diagno],na.rm = TRUE)
  KUHR <- dcast(KUHR, ID ~ diagno, value.var = c("diagnose", "ant_dager"), sep = "")

  # Link to identified outcomes
  enb <- copy(early_nonlive_births)

  # Add an indicator column to each data.table
  early_nonlive_births <- merge(enb, KUHR, by = "ID", all.x = TRUE)

  # Calculate gestational age of selected KUHR events in MBRN detected pregnancies
  codes <- kuhr_codes$code

  #### TODO: OPTIMISE the code for the loop (Done)
  pb=txtProgressBar(0,length(codes),style = 3)
  k = 0
  for (code in codes) {
    k = k+1
    setTxtProgressBar(pb,k)
    vname = paste0(code, "_date")
    early_nonlive_births[, (vname) := Date(.N)]
    update_code_dates(early_nonlive_births, code, event_col_prefix = "diagnose", evet_date_col_prefix = "ant_dager", dlen = dlen,
                      lookback = cfg_marker_lookback_days)   # [v2.0.0] v1.3: 70
  }
  cat("\n")

  # Impute pregnancy start date based on early pregnancy markers
  days <- kuhr_codes$days
  for (i in seq_along(codes)) {
    code = codes[i]
    dday = days[i]
    vname = paste0(code, "_date")
    early_nonlive_births[is.na(pr_start_new) & get(vname) >= (date_outcome - cfg_marker_rejection_days) & get(vname) <= date_outcome, pr_start_new := get(vname) - dday]
    early_nonlive_births[is.na(start) & ((date_outcome - pr_start_new) >= cfg_marker_rejection_days) & !is.na(pr_start_new), flag_over12weeks := 1]
    early_nonlive_births[is.na(start) & ((date_outcome - pr_start_new) >= cfg_marker_rejection_days), pr_start_new := NA_Date_]
  }

  # Calculate number of repaired flags
  early_nonlive_births[!is.na(flag_over12weeks) & !is.na(pr_start_new), repaired_flag := 1]

  # Drop unnecessary variables
  early_nonlive_births= early_nonlive_births[, .(MORS_ALDER,ID,DAGER_FDATO,start,preg_id,
                                                 date_outcome,outcome_final,MC_source,
                                                 TOP_source,pr_start_new,flag_over12weeks,repaired_flag)]

  ##################### BLOCK 14 ########################
  #######################################################
  cli::cli_progress_update()
  # Identify pregnancy with prenatal diagnosis code O35
  # prenatal_diagnosis = 1: an induced abortion with an O35 record in NPR or KUHR (ICD-10) dated 0-o35_window_days (v1.3: 27) days after it

  # Select appropriate codes from NPR
  NPR <- copy(npr)
  NPR <- NPR[!is.na(tilst)]
  NPR <- NPR[tilst != ""]
  NPR <- NPR[str_detect(tilst, "O35")]
  if(nrow(NPR)>0){

    NPR[, c("stay_id", "diagno") := NULL]
    setorder(NPR, ID, inndato, na.last = TRUE)
    NPR[, diagno := seq_len(.N), by = ID]
    dlen_npr = max(NPR[,diagno])
    NPR <- dcast(NPR, ID ~ diagno, value.var = c("tilst", "inndato"), sep = "")
    # Link to identified outcomes
    enb <- copy(early_nonlive_births)
    early_nonlive_births <- merge(enb, NPR, by = "ID", all.x = TRUE)
  }

  # Select appropriate codes from KUHR
  KUHR <- copy(kuhr)
  KUHR <- KUHR[diagnosekodeverk == "ICD-10" & str_detect(diagnose, "O35")]
  KUHR = KUHR[, .(diagnose, ID, ant_dager)]
  setorder(KUHR, ID, ant_dager, na.last = TRUE)
  KUHR[, diagno := seq_len(.N), by = ID]
  dlen_kuhr = max(KUHR[,diagno],na.rm = TRUE)
  if(dlen_kuhr>60){
    dmax = min(60, ceiling(quantile(KUHR[, diagno], 0.998, na.rm = TRUE)))
    KUHR = KUHR[diagno<=dmax]
    dlen_kuhr = max(KUHR[, diagno],na.rm = TRUE)
  }
  KUHR <- dcast(KUHR, ID ~ diagno, value.var = c("diagnose", "ant_dager"), sep = "")

  # Link to identified outcomes
  enb <- copy(early_nonlive_births)
  early_nonlive_births <- merge(enb, KUHR, by = "ID", all.x = TRUE)

  # Link to elective terminations
  early_nonlive_births[, prenatal_diagnosis := NA_integer_]

  if(nrow(NPR)>0){
    for (i in 1:dlen_npr) {
      early_nonlive_births[outcome_final == 3 & (date_outcome - get(paste0("inndato",   i)) >= -cfg_o35_window_days) & (date_outcome - get(paste0("inndato",   i)) <= 0), prenatal_diagnosis := 1]   # [v2.0.0] v1.3: > -28
    }
  }
  if(dlen_kuhr>0)
    for (i in 1:dlen_kuhr) {
      early_nonlive_births[outcome_final == 3 & (date_outcome - get(paste0("ant_dager", i)) >= -cfg_o35_window_days) & (date_outcome - get(paste0("ant_dager", i)) <= 0), prenatal_diagnosis := 1]   # [v2.0.0] v1.3: > -28
    }

  # Drop unnecessary variables
  early_nonlive_births = early_nonlive_births[,.(MORS_ALDER, ID, DAGER_FDATO, start, preg_id, date_outcome ,
                                                 outcome_final, MC_source, TOP_source, pr_start_new, flag_over12weeks,
                                                 repaired_flag, prenatal_diagnosis)]

  #########################
  # Actual (final) dating
  cli::cli_alert_info("Final coding and dating of the detected events")
  cli::cli_progress_update()

  early_nonlive_births[,start:=as.Date(start)]
  early_nonlive_births[, GA_event_week_MBRN := floor(as.numeric(date_outcome - start) / 7)]
  early_nonlive_births[, GA_event_days_MBRN := as.numeric(date_outcome - start)]

  # Start: MBRN or marker start; TOP with O35 and no MBRN GA -> outcome - o35_induced_days (replaces a marker start);
  # only rejected markers -> outcome - marker_rejection_days; a start before the woman's previous outcome is moved to it
  early_nonlive_births[, pr_start_final := as.Date(pr_start_new)]
  early_nonlive_births[outcome_final == 3 & is.na(start) & !is.na(prenatal_diagnosis), pr_start_final := date_outcome - cfg_o35_induced_days]   # [v1.3.1] setting (v1.3: 80)
  early_nonlive_births[is.na(pr_start_final) & !is.na(flag_over12weeks) & is.na(repaired_flag), pr_start_final := date_outcome - cfg_marker_rejection_days]   # [v1.3.1] setting (v1.3: 84)

  early_nonlive_births[, pr_start_new_previous := NA_Date_]
  setorder(early_nonlive_births, ID, date_outcome, na.last = TRUE)

  early_nonlive_births[, pr_start_new_previous := fifelse(pr_start_final < shift_wrt_id(date_outcome, ID), shift_wrt_id(date_outcome,ID), pr_start_new_previous)]
  early_nonlive_births[!is.na(pr_start_new_previous), pr_start_final := pr_start_new_previous]

  early_nonlive_births[, GA_event_week_imput := floor(as.numeric(date_outcome - pr_start_final) / 7)]
  early_nonlive_births[, GA_event_days_imput := as.numeric(date_outcome - pr_start_final)]

  # Still undated: outcome-specific default length, again not before the previous outcome
  # [v1.3.1] outcome-specific default lengths are settings (miscarriage, induced abortion, molar, ectopic)
  # [v1.4.0] a default may be a year table (year of the outcome)
  # [v2.0.0] a table with attribute extrapolate (uio_termination_days_by_year(), the default for induced
  # abortions) extrapolates years outside its range; those years are reported (warning, summary, report)
  extrapolated = list()
  set_default = function(oc, x, nm = ""){
    if(is.data.frame(x)){
      i = early_nonlive_births[, which(is.na(pr_start_final) & outcome_final == oc)]
      yrs = early_nonlive_births[i, year(date_outcome)]
      early_nonlive_births[i, pr_start_final := date_outcome - .uio_days_for_years(x, yrs)]
      if(isTRUE(attr(x, "extrapolate"))){
        out = sort(unique(yrs[yrs < min(x$year) | yrs > max(x$year)]))
        if(length(out)) extrapolated[[nm]] <<- data.frame(year = out, days = .uio_extrapolate_days(x, out))
      }
    }else{
      early_nonlive_births[is.na(pr_start_final) & outcome_final == oc, pr_start_final := date_outcome - x]
    }
  }
  set_default(2, cfg_miscarriage_default_days, "miscarriage_default_days")
  set_default(3, cfg_induced_abortion_default_days, "induced_abortion_default_days")
  set_default(5, cfg_molar_default_days, "molar_default_days")
  set_default(6, cfg_ectopic_default_days, "ectopic_default_days")
  for(msg in .uio_extrapolation_notes(extrapolated)) warning(msg, call. = FALSE)
  early_nonlive_births[, pr_start_previous := NA_Date_]
  # early_nonlive_births[, pr_start_previous := fifelse(pr_start_final < shift(date_outcome),
  #                                                     shift(date_outcome), pr_start_previous), by =ID]

  early_nonlive_births[, pr_start_previous := fifelse(pr_start_final < shift_wrt_id(date_outcome,ID),
                                                      shift_wrt_id(date_outcome,ID), pr_start_previous)]

  # early_nonlive_births[, pr_start_previous := fifelse(pr_start_final < shift_wrt_id(date_outcome, ID),
  #                                                     shift_wrt_id(date_outcome,ID), pr_start_previous, NA_Date_)]
  early_nonlive_births[!is.na(pr_start_previous), pr_start_final := pr_start_previous]

  early_nonlive_births[, GA_event_week_final := floor(as.numeric(date_outcome - pr_start_final) / 7)]
  early_nonlive_births[, GA_event_days_final := as.numeric(date_outcome - pr_start_final)]


  # [revised_codes] dating method of each pregnancy (no change to the dating itself)
  # [v1.3.1] the two fixed-length labels follow the settings (identical to v1.3 with the defaults 80 and 84)
  cfg_label_o35 = paste0("fixed ", cfg_o35_induced_days, " d (TOP with O35)")
  cfg_label_rejected = paste0("fixed ", cfg_marker_rejection_days, " d (marker >= ",
                              if(cfg_marker_rejection_days %% 7 == 0) paste0(cfg_marker_rejection_days %/% 7, " wk") else paste0(cfg_marker_rejection_days, " d"),
                              " rejected)")
  early_nonlive_births[, dating_method := fcase(
    !is.na(start), "MBRN (recorded GA)",
    outcome_final == 3 & !is.na(prenatal_diagnosis), cfg_label_o35,
    !is.na(pr_start_new), "pregnancy marker",
    !is.na(flag_over12weeks) & is.na(repaired_flag), cfg_label_rejected,
    default = "fixed outcome-specific default")]
  early_nonlive_births[, truncated_at_previous_outcome := !is.na(pr_start_previous) | !is.na(pr_start_new_previous)]

  # Drop unnecessary variables for final file
  early_nonlive_births = early_nonlive_births[,.(ID, preg_id, date_outcome, outcome_final,
                                                 MC_source, TOP_source, pr_start_final,
                                                 GA_event_week_final, GA_event_days_final,
                                                 dating_method, truncated_at_previous_outcome)]

  # early_nonlive_births2 <<- copy(early_nonlive_births)
  cli::cli_progress_update()
  cli::cli_alert_info("Done")
  ############################### (DONE) #################################

  outcome_lables = data.table(outcome_final = 1:6, outcome_lable= c("Livebirth", "Miscarriage", "Termination of pregnancy",
                                                                    "Stillbirth", "Molar pregnancy", "Ectopic pregnancy"),
                              # [experimental] outcome_lable_new: outcome_lable is kept as in v1.3; in outcome_lable_new a
                              # miscarriage decided by the MBRN (MC_source 1-4) is a "Late spontaneous abortion", one decided by
                              # NPR or KUHR a "Spontaneous abortion" (outcome_final stays 2 for both)
                              outcome_lable_new = c("Live birth", "Spontaneous abortion", "Termination of pregnancy",
                                                    "Stillbirth", "Molar pregnancy", "Ectopic pregnancy"))

  early_nonlive_births = merge(early_nonlive_births, outcome_lables, by = "outcome_final", all.x = TRUE)
  early_nonlive_births[outcome_final == 2 & MC_source %in% 1:4, outcome_lable_new := "Late spontaneous abortion"]   # [experimental]

  # [experimental] pregnancies of the MBRN records whose gestational age was imputed (see above)
  early_nonlive_births[, ga_imputed := FALSE]
  imp_used = if(nrow(exp_imputed) && "imputed" %in% names(exp_imputed)) exp_imputed[imputed == TRUE] else exp_imputed
  if(nrow(imp_used)){
    k = match(paste(as.integer(early_nonlive_births$ID), as.integer(as.Date(early_nonlive_births$date_outcome))),
              paste(as.integer(imp_used$ID), imp_used$dn))
    hit = !is.na(k) & early_nonlive_births$dating_method == "MBRN (recorded GA)"
    early_nonlive_births[hit, `:=`(ga_imputed = TRUE,
                                   dating_method = paste0("MBRN (imputed GA: median of ", gsub("_", " ", imp_used$kind_rule[k[hit]]), "s",
                                                          data.table::fifelse(imp_used$capped[k[hit]],
                                                                              paste0(", start ", missing_ga_min_gap_days, " d after the previous outcome"), ""), ")"))]
  }

  uio_capture$extrapolated = extrapolated
  setattr(early_nonlive_births, "uio_capture", uio_capture)   # [v2.0.0] used and removed by run_uio_pa()
  early_nonlive_births
}
