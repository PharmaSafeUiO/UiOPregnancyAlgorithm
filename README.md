# UiO pregnancy algorithm, version 1.0

`UiO_preg_Algo_v1.0.R` is version 1.0 of the **UiO pregnancy algorithm**: the R translation of the
STATA script of the algorithm developed and validated by Nordeng et al. (2024). It identifies
pregnancies, their outcomes and their start dates in the Norwegian health registries: the Medical
Birth Registry of Norway (MBRN), the Norwegian Patient Registry (NPR) and the primary-care claims
(KUHR).

Version 2.0 is an installable R package (`run_uio_pa()`), with every rule as a documented setting;
`run_uio_pa(..., settings = uio_v1_settings())` gives the results of this script. Use version 2.0
for new work. See the [v2.0.0 release](https://github.com/PharmaSafeUiO/UiOPregnancyAlgorithm/releases/tag/v2.0.0).

**Authors:** Hedvig M. E. Nordeng, Saeed Hayati (maintainer), Marleen M. H. J. van Gelder.

## Rules

* **Outcomes:** live birth, stillbirth, miscarriage, induced abortion, molar pregnancy and ectopic
  pregnancy. An MBRN fetal death (`DODKAT` 7-9) is a stillbirth from 21 completed weeks
  (`SVLEN` > 20) and a miscarriage before.
* **Dating:** MBRN births have their recorded gestational age. Other pregnancies are dated with
  pregnancy markers: the 15 ICD-10 and 6 ICPC-2 codes, of 45 ICD-10 and 9 ICPC-2 candidate codes,
  with a median gestational age below 12 weeks in MBRN pregnancies, in priority order. Markers
  recorded up to 70 days before the outcome are used. An estimate of 119 days (17 weeks) or more is
  rejected and the next marker tried; if all are rejected, the gestational age is 119 days.
* **O35:** an induced abortion with an O35 code (prenatal diagnosis of fetal anomaly) recorded 0-28
  days before it gets 80 days.
* **Without markers:** miscarriage 70 days, induced abortion 56 days, molar pregnancy 77 days,
  ectopic pregnancy 56 days.
* The start of a pregnancy is not set before the woman's previous outcome.

## Use

The data-loading part at the top of the script reads the extracts of one project: the seven NPR
files (`NPR/SOMdata.csv`, `AVTSOMdata.csv`, `ASPdata.csv`, `BUPdata.csv`, `PHVdata.csv`,
`REHdata.csv`, `TSBdata.csv`), the MBRN file (`MFR/MFR.csv`) and the KUHR files (`KUHR/*.csv`),
with dates as day differences from a reference date. Adapt the file names, column names
(`source_files`) and reference date (`ref_date`) to your extracts. Then:

```r
raw_data_dir <- "path/to/Raw_data/"   # folder with the subfolders NPR/, MFR/ and KUHR/
uio_workdir  <- "path/to/workdir"     # cached extracts are written here
source("UiO_preg_Algo_v1.0.R")
pregnancies <- detect_pregnancy_episodes(source_files, hospital_codes, primary_care_codes,
                                         years_range = years_range)
```

Requires R with the packages Rcpp (and a C++ compiler), data.table, lubridate, stringr, dplyr, cli,
fs and haven. Lines 481-482 combine `with = FALSE` with `:=`, which is an error from data.table 1.17
when the MBRN data have no quadruplets (or no triplets); version 2.0 corrects this.

## Reference

Nordeng HME, Lupattelli A, Engjom HM, van Gelder MMHJ. Detecting and Dating Early Non-live
Pregnancy Outcomes: Generation of a Novel Pregnancy Algorithm From Norwegian Linked Health
Registries. *Pharmacoepidemiol Drug Saf.* 2024;33(9):e70002.
[doi:10.1002/pds.70002](https://doi.org/10.1002/pds.70002)

## License

Apache License 2.0 (see `LICENSE`).
