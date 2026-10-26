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
