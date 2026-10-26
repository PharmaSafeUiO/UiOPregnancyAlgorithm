#' UiOPregnancyAlgorithm: the UiO pregnancy algorithm for the Norwegian health registries
#'
#' Identifies pregnancies, their outcomes (live birth, stillbirth, miscarriage, induced abortion,
#' molar and ectopic pregnancy) and their start dates in the Medical Birth Registry of Norway
#' (MBRN), the Norwegian Patient Registry (NPR) and the primary-care claims database (KUHR), with
#' the UiO pregnancy algorithm (Nordeng et al., Pharmacoepidemiol Drug Saf 2024;33(9):e70002).
#'
#' Start with [run_uio_pa()]. The settings and their defaults are described in
#' [uio_default_settings()], the source files in [uio_source_files()] and the pregnancy markers in
#' [uio_hospital_codes()]. The pregnancy start is day 0 and the gestational age at the outcome is
#' the number of days from the start to the outcome.
#'
#' @keywords internal
"_PACKAGE"

# data.table is used through importFrom() in NAMESPACE, which already makes the package
# "data.table aware"; this flag documents that intent and keeps it if the imports change.
.datatable.aware <- TRUE
# data.table's `..var` prefix (columns named in a variable) is not a global variable
utils::globalVariables(c("..variables", "..mbrn_vars", "..npr_vars", "..kuhr_vars"))
# column names of the data.table code of the algorithm (non-standard evaluation), and the C++ helpers
utils::globalVariables(c(
  ".", "ant_dager", "APGAR5", "birth_year", "check_ga_quad", "check_ga_triplet", "check_ga_twin",
  "count_ectopic", "count_KUHR_sp", "count_molar", "count_NPR_sp", "count_NPR_top", "count_sp_N",
  "count_top_N", "DAGER_FDATO", "DAGER_FDATO_MF", "DAGER_FDATO1", "DAGER_FDATO2", "DAGER_FDATO3",
  "DAGER_FDATO4", "date_outcome", "dating_method", "days_before", "default_drawn", "diagno", "diagnose",
  "diagnosekodeverk", "diffdato", "DODKAT", "earlier_diagnosis", "episode_no", "filter", "flag_over12weeks",
  "fodselsar", "GA_days_event", "GA_event_days_final", "GA_event_days_imput", "GA_event_days_MBRN",
  "GA_event_week_final", "GA_event_week_imput", "GA_event_week_MBRN", "GA_weeks_event", "gap_previous",
  "gap_previous_KUHR", "gap_previous_NPR", "ID", "include", "induced_ab", "induced_ab1", "induced_ab2",
  "induced_ab3", "induced_ab4", "inndato", "inndato_diff", "kjonn", "kobl_nokkel", "kodenavn", "KodeNavn",
  "kodeverdi", "KodeVerdi", "KUHR_MC_misclassified", "lag", "later_diagnosis", "livebirth", "livebirth1",
  "livebirth2", "livebirth3", "livebirth4", "lopenr", "lopeNr_1_PasientLopeNr_PDB2774", "MBRN_date_ref",
  "MBRN_livebirth", "MBRN_miscarriage", "MBRN_preg_start", "MBRN_stillbirth", "MBRN_TOP", "MBRN_unknown",
  "MC_source", "MC_source_KUHR", "MC_source_NPR", "miscar", "miscar_3", "miscar_31", "miscar_32",
  "miscar_33", "miscar_34", "MORS_ALDER", "NPR_MC_misclassified", "NPR_outcome", "NPR_TOP_misclassified",
  "outcome_after_NPR", "outcome_final", "pasientlopenr_MOR", "PasientLopeNr_MOR", "PasientLopeNr_MOR__y__y",
  "PLURAL_new", "PLUREK", "PLUREK_new", "pr_start_final", "pr_start_new", "pr_start_new_previous",
  "pr_start_previous", "preg_id", "prenatal_diagnosis", "qua_LLLL", "qua_MMTT", "qua_n_LB", "qua_n_MC",
  "qua_n_SB", "qua_n_TOP", "qua_n_UK", "repaired_flag", "sp_KUHR_only", "sp_MBRN_KUHR", "sp_MBRN_NPR",
  "sp_NPR_only", "start", "stay_id", "stillbirth", "stillbirth1", "stillbirth2", "stillbirth3",
  "stillbirth4", "SVLEN", "SVLEN_DG", "SVLEN_DG_MF", "SVLEN_DG1", "SVLEN_DG2", "SVLEN_DG3", "SVLEN_DG4",
  "tilst", "top_MBRN_NPR", "top_NPR_only", "TOP_source", "TOP_source_NPR", "trip_disc", "trip_LLL",
  "trip_LLS", "trip_LLT", "trip_LSS", "trip_LTT", "trip_MMM", "trip_n_LB", "trip_n_MC", "trip_n_SB",
  "trip_n_TOP", "trip_SSS", "trip_STT", "trip_TTT", "trip_UC", "trip_UK", "truncated_at_previous_outcome",
  "twin_disc", "twin_LL", "twin_LM", "twin_LS", "twin_LT", "twin_MM", "twin_MS", "twin_MT", "twin_SB",
  "twin_ST", "twin_TT", "twin_UC", "twin_UK", "unknown", "unknown1", "unknown2", "unknown3", "unknown4",
  "update_code_dates", "update_episode_no_gap", "update_episode_start", "visit_id", "visit_type", "vocab",
  "weeks_before", "within_56day_preg", "within_preg"))
utils::globalVariables(c(
  ".age", "LEVE_DODSTATUS_FODSEL", "N", "Outcome", "SVLEN_ABR", "Total", "age_group", "age_mbrn", "birth_year_est", "code", "codes_kuhr", "codes_mbrn", "codes_npr", "d", "dating_class", "dating_code", "dating_source", "days", "dout", "dstart", "estimated", "fixed", "ga_max", "ga_median", "ga_min", "ga_q1", "ga_q3", "hi", "i.age_mbrn", "i.birth_year_est", "i.code", "i.codes_mbrn", "i.outcome_final", "i.rid", "i.source", "i.v", "i.dout", "lo", "meaning", "mother_age", "n", "n_records", "n_women", "oc_kuhr", "oc_npr", "outcome", "outcome_code", "outcome_lable", "outcome_source", "p", "pct", "prio", "priority", "recorded", "registry", "rid", "txt", "type", "x.code", "x.d", "x.vocab", "year_outcome", "source", "vocab"))
