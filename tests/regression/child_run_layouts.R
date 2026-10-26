## Child process: the same synthetic registry data in the layouts of P704 and P1380 and in a generic layout
## with real dates and per-person reference dates (uio_source_files() / uio_dates()). Every run should give
## the pregnancies of the P1111 layout.
## Argument: path to an .rds with list(lib, raw_dir, tmp, out)
p <- readRDS(commandArgs(TRUE)[1])
suppressPackageStartupMessages(library(data.table))
library(UiOPregnancyAlgorithm, lib.loc = p$lib)
v13 <- uio_v13_settings()
ref0 <- as.Date("2004-01-01") - 10234                           # reference date of the P1111 layout
wd <- function(tag) { d <- file.path(p$tmp, paste0("wd_layout_", tag)); dir.create(d, showWarnings = FALSE); d }
run <- function(tag, src) run_uio_pa(workdir = wd(tag), source_files = src, settings = v13, years_range = c(2008, 2023))

## P1111 layout (the reference); its prepared NPR table is reused below
res <- list(p1111 = run("p1111", uio_project_sources("p1111", raw_dir = p$raw_dir)))
npr <- fread(file.path(wd("p1111"), "npr_mothers_full.csv"))       # ICD-10 diagnoses of women, one row per diagnosis
mbrn <- fread(file.path(p$raw_dir, "MFR", "PDB2774_MFR_p202207_mfr_2022Q1_202209.csv"))
kuhr <- rbindlist(lapply(list.files(file.path(p$raw_dir, "KUHR"), full.names = TRUE), fread), fill = TRUE)

## P704 layout: offsets from 1997-09-24; NPR of mothers with one column per diagnosis; KUHR without sex (mothers only)
r704 <- file.path(p$tmp, "raw_p704"); dir.create(r704, showWarnings = FALSE)
shift <- as.integer(as.Date("1997-09-24") - ref0)
fwrite(mbrn[, .(PasientLopeNr_MOR = MOR_PasientLopeNr_PDB2774, MORS_ALDER, DIFF_DAGER_FDATO = FDATO_DIFF - shift, PLUREK,
                SVLEN_DG, DODKAT, APGAR5, FAAR = MOR_FAAR)], file.path(r704, "PDB 2424_MFRdata_182029_2020.09.csv"))
w <- npr[, .(pasientlopenr_MOR = lopeNr_1_PasientLopeNr_PDB2774, inndato_diff = diffdato - shift, KodeVerdi, visit_id)]
w[, k := paste0("tilstand_", seq_len(.N)), by = visit_id]
w <- dcast(w, pasientlopenr_MOR + inndato_diff + visit_id ~ k, value.var = "KodeVerdi", fill = "")[, visit_id := NULL]
fwrite(w, file.path(r704, "PDB 2424_NPR_20_505_NPR_mor_1.csv"))
fwrite(kuhr[PASIENT_KJONN == 2, .(pasientlopenr_MOR = PasientLopeNr_PDB2774, diagnose = DIAGNOSE_KODE,
                                  diagnosekodeverk = DIAGNOSEKODEVERK, ant_dager = DIFFERANSEDAGER - shift)],
       file.path(r704, "PDB 2424_1 Data fra KUHR 2020.csv"))
res$p704 <- run("p704", uio_project_sources("p704", raw_dir = r704))

## P1380 layout: MBRN as SPSS with text dates ("mdy HMS"); NPR as contact files (sex in column 7 / 6) and code
## files; KUHR tab-separated with dates as yyyymmdd. All NPR diagnoses go to SOM, except AVTSOM's own.
r1380 <- file.path(p$tmp, "raw_p1380"); for (d in c("MBRN", "NPR/SOM", "NPR/AVTSOM", "KUHR")) dir.create(file.path(r1380, d), recursive = TRUE, showWarnings = FALSE)
haven::write_sav(as.data.frame(mbrn[, .(LOPENR_MOR = MOR_PasientLopeNr_PDB2774, MORS_ALDER,
                                        FDATO = format(ref0 + FDATO_DIFF, "%m/%d/%Y 00:00:00"), PLUREK, SVLEN_DG, DODKAT, APGAR5)]),
                 file.path(r1380, "MBRN", "p202274_mfr_2024_11.sav"))
n2 <- copy(npr)[, inndato := ref0 + diffdato][, part := fifelse(visit_type == "avtsom", "avtsom", "som")]
for (pt in c("som", "avtsom")) {
  x <- unique(n2[part == pt, .(kobl_nokkel = visit_id, lopenr = lopeNr_1_PasientLopeNr_PDB2774, inndato)])
  x <- if (pt == "som") x[, .(kobl_nokkel, lopenr, inndato, a = 1L, b = 1L, c = 1L, kjonn = 2L)] else
    x[, .(kobl_nokkel, lopenr, inndato, a = 1L, b = 1L, kjonn = 2L)]
  up <- toupper(pt)
  fwrite(x, file.path(r1380, "NPR", up, paste0(up, "_2018_2023.csv")))
  fwrite(n2[part == pt, .(kobl_nokkel = visit_id, kodenavn = "ICD-10", kodeverdi = KodeVerdi)],
         file.path(r1380, "NPR", up, if (pt == "som") "SOM_ICD10.csv" else "AVTSOM_koder.csv"))
}
fwrite(kuhr[, .(PASIENTLOPENUMMER = PasientLopeNr_PDB2774, DIAGNOSE_KODE, DIAGNOSEKODEVERK,
                DATO = format(ref0 + DIFFERANSEDAGER, "%Y%m%d"), PASIENT_KJONN)], file.path(r1380, "KUHR", "kuhr_2024.dsv"), sep = "\t")
res$p1380 <- run("p1380", uio_project_sources("p1380", raw_dir = r1380))

## Generic layout: MBRN with real dates as text ("dmy"); NPR as one long file and KUHR, both with day offsets from
## a reference date that differs per woman (file person_id, ref_date)
rg <- file.path(p$tmp, "raw_generic"); dir.create(rg, showWarnings = FALSE)
ids <- unique(c(mbrn$MOR_PasientLopeNr_PDB2774, npr$lopeNr_1_PasientLopeNr_PDB2774, kuhr$PasientLopeNr_PDB2774))
refs <- data.table(person_id = ids, ref_date = as.Date("1990-01-01") + 37L * seq_along(ids))
fwrite(refs, file.path(rg, "reference_dates.csv"))
own <- function(id, offset0) as.integer(ref0 + offset0 - refs$ref_date[match(id, refs$person_id)])
fwrite(mbrn[, .(MOR = MOR_PasientLopeNr_PDB2774, ALDER = MORS_ALDER, FDATO = format(ref0 + FDATO_DIFF, "%d.%m.%Y"), PLUREK,
                SVLEN_DG, DODKAT, APGAR5)], file.path(rg, "mbrn.csv"))
fwrite(npr[, .(ID = lopeNr_1_PasientLopeNr_PDB2774, KODE = KodeVerdi, DAGER = own(lopeNr_1_PasientLopeNr_PDB2774, diffdato))],
       file.path(rg, "npr_diagnoses.csv"))
fwrite(kuhr[, .(ID = PasientLopeNr_PDB2774, DIAGNOSE = DIAGNOSE_KODE, KODEVERK = DIAGNOSEKODEVERK,
                DAGER = own(PasientLopeNr_PDB2774, DIFFERANSEDAGER), KJONN = ifelse(PASIENT_KJONN == 2, "K", "M"))],
       file.path(rg, "kuhr.csv"))
ref <- uio_dates(ref_dates = file.path(rg, "reference_dates.csv"))
src_g <- uio_source_files(
  birth_registry = uio_source(file.path(rg, "mbrn.csv"),
    columns = c(mother_id = "MOR", mother_age = "ALDER", event_date = "FDATO", child_order = "PLUREK",
                gestational_days = "SVLEN_DG", death_cat = "DODKAT", apgar5 = "APGAR5"),
    dates = uio_dates(format = "dmy")),
  hospital = uio_source(file.path(rg, "npr_diagnoses.csv"),
    columns = c(mother_id = "ID", diagnose_code = "KODE", visit_date = "DAGER"), dates = ref),
  primary_care = uio_source(file.path(rg, "kuhr.csv"),
    columns = c(mother_id = "ID", diagnose_code = "DIAGNOSE", vocabulary = "KODEVERK", visit_date = "DAGER", gender = "KJONN"),
    dates = ref, female_value = "K"))
res$generic <- run("generic", src_g)
missing_cols <- tryCatch(uio_source_files(birth_registry = src_g$birth_registry, hospital = uio_source("x.csv", columns = c(mother_id = "ID")),
                                          primary_care = src_g$primary_care), error = function(e) conditionMessage(e))
saveRDS(list(res = res, missing_cols = missing_cols), p$out)
