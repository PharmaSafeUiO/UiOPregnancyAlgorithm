# Load necessary libraries
library(Rcpp)
library(data.table)
library(lubridate)
library(stringr)

# early_nonlive_births2 = detect_pregnancy_episodes(source_files, hospital_codes, primary_care_codes, years_range = years_range)
#saveRDS(early_nonlive_births, "early_nonlive_births.rds", compress = FALSE)

plot_outputs = FALSE
years_range = c(2008,2023)
##TODO: Derive the list of crucial variables in each data source (Partially done)
##TODO: Extract all hard coded diagnose codes (Partially done)
##TODO: Remove unnecessary intermediate file savings (Done)
##TODO: Optimize the code for memory usage
##TODO: Replace WIDE format with LONG format in BLOCK12 and BLOCK13 for better performance


# Set working directory
# [revised_codes] working folder (holds the cached source extracts) can be set by the caller
if (!exists("uio_workdir")) uio_workdir <- "N:/durable/Saeed/UiO_preg_algo/"
setwd(uio_workdir)

####### Load NPR data
if(!file.exists("npr_mothers_full.csv")){
  som = fread("N:/durable/Raw_data/NPR/PDB2774_NPR_19_45084_SOMdata_til_utlevering_redNorPD_202209.csv", encoding = "UTF-8")
  avtsom = fread("N:/durable/Raw_data/NPR/PDB2774_NPR_19_45084_AVTSOMdata_til_utlevering_redNorPD_202209.csv")
  asp = fread("N:/durable/Raw_data/NPR/PDB2774_NPR_19_45084_ASPdata_til_utlevering_redNorPD_202209.csv", encoding = "UTF-8")
  bup = fread("N:/durable/Raw_data/NPR/PDB2774_NPR_19_45084_BUPdata_til_utlevering_redNorPD_202209.csv", encoding = "UTF-8")
  phv = fread("N:/durable/Raw_data/NPR/PDB2774_NPR_19_45084_PHVdata_til_utlevering_redNorPD_202209.csv", encoding = "UTF-8")
  reh = fread("N:/durable/Raw_data/NPR/PDB2774_NPR_19_45084_REHdata_til_utlevering_redNorPD_202209.csv", encoding = "UTF-8")
  tsb = fread("N:/durable/Raw_data/NPR/PDB2774_NPR_19_45084_TSBdata_til_utlevering_redNorPD_202209.csv", encoding = "UTF-8")

  colnames(som)[22]="kobl_nokkel"
  colnames(avtsom)[16]="kobl_nokkel"
  colnames(asp)[16]="kobl_nokkel"
  colnames(bup)[16]="kobl_nokkel"
  colnames(phv)[18]="kobl_nokkel"
  colnames(reh)[18]="kobl_nokkel"
  colnames(tsb)[18]="kobl_nokkel"

  som=som[KodeNavn=="ICD-10" & kjonn==2 ,.(kobl_nokkel,lopeNr_1_PasientLopeNr_PDB2774,KodeVerdi,diffdato,fodselsar)]
  avtsom=avtsom[KodeNavn=="ICD-10" & kjonn==2 ,.(kobl_nokkel,lopeNr_1_PasientLopeNr_PDB2774,KodeVerdi,diffdato,fodselsar)]
  asp=asp[KodeNavn=="ICD-10" & kjonn==2 ,.(kobl_nokkel,lopeNr_1_PasientLopeNr_PDB2774,KodeVerdi,diffdato,fodselsar)]
  bup[,KodeNavn:="ICD-10"]
  bup=bup[KodeNavn=="ICD-10" & kjonn==2 ,.(kobl_nokkel,lopeNr_1_PasientLopeNr_PDB2774,KodeVerdi,diffdato,fodselsar)]
  phv=phv[KodeNavn=="ICD-10" & kjonn==2 ,.(kobl_nokkel,lopeNr_1_PasientLopeNr_PDB2774,KodeVerdi,diffdato,fodselsar)]
  reh=reh[KodeNavn=="ICD-10" & kjonn==2 ,.(kobl_nokkel,lopeNr_1_PasientLopeNr_PDB2774,KodeVerdi,diffdato,fodselsar)]
  tsb=tsb[KodeNavn=="ICD-10" & kjonn==2 ,.(kobl_nokkel,lopeNr_1_PasientLopeNr_PDB2774,KodeVerdi,diffdato,fodselsar)]


  som[,visit_type:="som"]
  avtsom[,visit_type:="avtsom"]
  asp[,visit_type:="asp"]
  bup[,visit_type:="bup"]
  phv[,visit_type:="phv"]
  reh[,visit_type:="reh"]
  tsb[,visit_type:="tsb"]

  npr0=rbindlist(list(som,avtsom,asp,bup,phv,reh,tsb))

  setorder(npr0, lopeNr_1_PasientLopeNr_PDB2774, kobl_nokkel,visit_type, diffdato,na.last = TRUE)
  setkey(npr0, lopeNr_1_PasientLopeNr_PDB2774, kobl_nokkel,visit_type)

  npr0[,visit_id:=.GRP,by=.(lopeNr_1_PasientLopeNr_PDB2774,kobl_nokkel,visit_type)]

  fwrite(npr0,"npr_mothers_full.csv")
}else{
  if(!file.exists("hospital_full.csv")){
    npr0 = fread("npr_mothers_full.csv")
  }else{
    npr0 = NULL #no need to load it
  }

}


source_files = list(
  birth_registry = list(file = "N:/durable/Raw_data/MFR/PDB2774_MFR_p202207_mfr_2022Q1_202209.csv",
                        ref_date = ymd("2004-01-01") - 10234,birth_year_limit = c(1,10000),#birth_year_limit = c(1965,2004),
              variables = c(mother_id = "MOR_PasientLopeNr_PDB2774",
                            mother_age = "MORS_ALDER",
                            event_date = "FDATO_DIFF"     ,
                            child_order = "PLUREK",
                            gestational_days = "SVLEN_DG",
                            death_cat = "DODKAT",
                            apgar5 = "APGAR5",
                            birth_year = "MOR_FAAR") ),
  hospital  = list(data = npr0,
                   ref_date = ymd("2004-01-01") - 10234, birth_year_limit = c(1,10000),#birth_year_limit = c(1965,2004),
                   variables = c(mother_id = "lopeNr_1_PasientLopeNr_PDB2774",
                                 diagnose_code = "KodeVerdi",
                                 stay_id = "visit_id",
                                 visit_date = "diffdato",
                                 birth_year = "fodselsar",
                                 vocabulay = NULL)),
  primary_care = list(file ="N:/durable/Raw_data/KUHR/.*.csv",
                      ref_date = ymd("2004-01-01") - 10234, birth_year_limit = c(1,10000),#birth_year_limit = c(1965,2004),
                      variables = c(mother_id = "PasientLopeNr_PDB2774",
                                    diagnose_code = "DIAGNOSE_KODE",
                                    vocabulary = "DIAGNOSEKODEVERK",
                                    visit_date = "DIFFERANSEDAGER",
                                    gender = "PASIENT_KJONN",
                                    birth_year = "FXDT"))
)

# Select appropriate codes from NPR for dating the early non-live births
# Order matters. Higher codes are prioritized over the lower codes for imputing event dates
hospital_codes = dplyr::tribble(
  ~vocab, ~code, ~days,
  "ICD-10","Z313",15,
  "ICD-10","Z311",12,
  "ICD-10","Z312",15,
  "ICD-10","Z351",65,
  "ICD-10","Z321",64,
  "ICD-10","O262",57,
  "ICD-10","Z320",48,
  "ICD-10","Z350",60,
  "ICD-10","O211",66,
  "ICD-10","O200",64,
  "ICD-10","O208",73,
  "ICD-10","O210",72,
  "ICD-10","O219",74,
  "ICD-10","O209",81,
  "ICD-10","O263",70
)


# Select appropriate codes from KUHR
primary_care_codes = dplyr::tribble(
  ~vocab, ~code, ~days,
  "ICD-10","Z313",15,
  "ICD-10","Z311",15,
  "ICD-10","Z312",15,
  "ICD-10","Z351",63,
  "ICD-10","O262",56,
  "ICPC-2","W79",53,
  "ICD-10","Z321",63,
  "ICD-10","Z350",57,
  "ICD-10","Z320",45,
  "ICPC-2","W78",57,
  "ICPC-2","W781",83,
  "ICPC-2","W01",57,
  "ICPC-2","W05",60,
  "ICD-10","O200",62,
  "ICD-10","O211",73,
  "ICD-10","O208",72,
  "ICD-10","O219",74,
  "ICD-10","O210",76,
  "ICD-10","O209",77,
  "ICPC-2","W03",71,
  "ICD-10","O263",62
)




detect_pregnancy_episodes = function(source_files, hospital_codes, primary_care_codes, years_range = NULL){

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

  smart_load <- function(file,variables=NULL,...){
    files= fs::dir_ls(path=dirname(file),regexp = basename(file))
    ds =list()
    for(i in seq_along(files)){
      file=files[i]
      ext = tools::file_ext(file)
      if(ext=="dta"){
        if(!is.null(variables)){
          d = as.data.table(haven::read_dta(file,...))
          d=d[,..variables]
        }else{
          d = as.data.table(haven::read_dta(file,...))
        }

      }else if(ext%in%c("csv","txt")){
        if(!is.null(variables)){
          d = data.table::fread(file,select = variables,...)
        }else{
          d = data.table::fread(file,...)
        }

      }else if(ext%in%c("dsv")){
        if(!is.null(variables)){
          d = data.table::fread(file,select = variables,sep = "\t",...)
        }else{
          d = data.table::fread(file,sep = "\t",...)
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
  cli::cli_alert_info("Loading source data")

  ################# Preloading Data ####################
  # Reloading data to speed up running the script
  if(!file.exists("birth_registry_full.csv")){
    mbrn_vars = unname(source_files$birth_registry$variables)
    if(is.null(source_files$birth_registry$data)){
      mbrn = smart_load(source_files$birth_registry$file, variables = mbrn_vars)
    }else{
      mbrn = source_files$birth_registry$data
      if(!"data.table" %in% class(mbrn))
        mbrn = as.data.table(mbrn)
      mbrn = mbrn[,..mbrn_vars]
    }


    setnames(mbrn, source_files$birth_registry$variables["mother_id"], "PasientLopeNr_MOR")
    setnames(mbrn, source_files$birth_registry$variables["event_date"], "DAGER_FDATO")
    setnames(mbrn, source_files$birth_registry$variables["child_order"], "PLUREK")
    setnames(mbrn, source_files$birth_registry$variables["gestational_days"], "SVLEN_DG")
    setnames(mbrn, source_files$birth_registry$variables["death_cat"], "DODKAT")
    setnames(mbrn, source_files$birth_registry$variables["apgar5"], "APGAR5")
    setnames(mbrn, source_files$birth_registry$variables["mother_age"], "MORS_ALDER")

    if(!is.null(source_files$birth_registry$birth_year_limit) & !is.na(source_files$birth_registry$birth_year_limit[1]) & length(source_files$birth_registry$birth_year_limit)>0){
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
    setnames(npr, source_files$hospital$variables["stay_id"], "stay_id")

    if(!is.null(source_files$hospital$variables["vocabulay"]) &&
       !is.na(source_files$hospital$variables["vocabulay"])){
      setnames(npr, source_files$hospital$variables["vocabulary"], "vocab")
    }else{
      npr[, vocab:=npr_codes$vocab[1]]
    }

    if(!is.null(source_files$hospital$birth_year_limit) & !is.na(source_files$hospital$birth_year_limit[1]) & length(source_files$hospital$birth_year_limit)>0){
      setnames(npr, source_files$hospital$variables["birth_year"], "birth_year")
      npr=npr[birth_year>=source_files$hospital$birth_year_limit[1] & birth_year<=source_files$hospital$birth_year_limit[2],]
    }



    fwrite(npr,"hospital_full.csv")
  }else{
    npr = fread("hospital_full.csv")
  }


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
    setnames(kuhr, source_files$primary_care$variables["gender"], "kjonn")

    if(!is.null(source_files$primary_care$birth_year_limit) & !is.na(source_files$primary_care$birth_year_limit[1]) & length(source_files$primary_care$birth_year_limit)>0){
      setnames(kuhr, source_files$primary_care$variables["birth_year"], "birth_year")
      kuhr=kuhr[birth_year>=source_files$primary_care$birth_year_limit[1] & birth_year<=source_files$primary_care$birth_year_limit[2],]
    }

    fwrite(kuhr, "primary_care_full.csv")

  }else{
    kuhr = fread("primary_care_full.csv")
  }

  # ref_date = ymd("2004-01-01") - 10234
  # range(mbrn$DAGER_FDATO)+ref_date

  if(!"Date"%in%class(npr[,inndato])){
    if(!is.null(source_files$hospital$ref_date)){
      npr[,inndato:=source_files$hospital$ref_date+inndato]
    }else if(is.null(source_files$hospital$date_format)){
      npr[,inndato:=lubridate::as_date(inndato)]
    }else{
      inds=sample(1:nrow(npr),10000)
      date_formats=names(sort(table(lubridate::guess_formats(mbrn[inds,inndato],source_files$hospital$date_format))))
      npr[,inndato:=lubridate::as_date(inndato,format=date_formats)]
    }
  }


  if(!"Date"%in%class(mbrn[,DAGER_FDATO])){
    if(!is.null(source_files$birth_registry$ref_date)){
      mbrn[,DAGER_FDATO:=source_files$birth_registry$ref_date+DAGER_FDATO]
    }else if(is.null(source_files$birth_registry$date_format)){
      mbrn[,DAGER_FDATO:=lubridate::as_date(DAGER_FDATO)]
    }else{
      inds=sample(1:nrow(mbrn),10000)
      date_formats=names(sort(table(lubridate::guess_formats(mbrn[inds,DAGER_FDATO],source_files$birth_registry$date_format))))
      mbrn[,DAGER_FDATO:=lubridate::as_date(DAGER_FDATO,format=date_formats)]
    }
  }


  if(!"Date"%in%class(kuhr[,ant_dager])){
    if(!is.null(source_files$primary_care$ref_date)){
      kuhr[,ant_dager:=source_files$primary_care$ref_date+ant_dager]
    }else if(is.null(source_files$primary_care$date_format)){
      kuhr[,ant_dager:=lubridate::as_date(ant_dager)]
    }else{
      inds=sample(1:nrow(kuhr),10000)
      date_formats=names(sort(table(lubridate::guess_formats(kuhr[inds,ant_dager],source_files$primary_care$date_format))))
      kuhr[,ant_dager:=lubridate::as_date(ant_dager,format=date_formats)]
    }
  }

  mbrn[,SVLEN:=floor(SVLEN_DG/7)]

  mbrn <- mbrn[year(DAGER_FDATO) >= min_year & !is.na(PasientLopeNr_MOR) & !is.na(SVLEN_DG)]
  npr  <- npr[year(inndato) >= min_year]
  kuhr <- kuhr[year(ant_dager) >= min_year & kjonn==2]

  hospital_codes = as.data.table(hospital_codes)
  primary_care_codes = as.data.table(primary_care_codes)
  suppressWarnings(npr[, stay_id:=NULL])
  npr = unique(npr[(vocab == "ICD-10" & grepl(paste0(c(hospital_codes[vocab=="ICD-10", code],c("O00","O01", "O02", "O03", "O04","O35")), collapse = "|"), tilst, perl = TRUE))])
  kuhr = unique(kuhr[(diagnosekodeverk == "ICD-10" & grepl(paste0(c(primary_care_codes[vocab=="ICD-10", code],c("O00","O01", "O02", "O03", "O04","O35")), collapse = "|"), diagnose, perl = TRUE)) |
                        (diagnosekodeverk == "ICPC-2" & grepl(paste0(c(primary_care_codes[vocab=="ICPC-2", code],"W82"), collapse = "|"), diagnose, perl = TRUE)) ])


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
  MBRN[, miscar_3 := ifelse(DODKAT %in% c(7:9) & SVLEN <= 20, 1, NA)]
  MBRN[, stillbirth := ifelse((DODKAT %in% c(7:9)) & SVLEN > 20, 1, NA)]
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
  MBRN <- MBRN[, .(preg_id,PLUREK_new,PLURAL_new,livebirth,miscar_3,stillbirth,unknown,induced_ab, DAGER_FDATO, SVLEN_DG)]

  # Reshape wide livebirth miscar_3 stillbirth unknown induced_ab
  MBRN <- dcast(MBRN, PLURAL_new+preg_id ~ PLUREK_new, value.var = c("livebirth", "miscar_3", "stillbirth", "unknown", "induced_ab", "DAGER_FDATO", "SVLEN_DG"),
                sep = "")

  for(ij1 in 2:4){
    var_1 = paste0("DAGER_FDATO", ij1)
    var_2 = paste0("SVLEN_DG", ij1)
    if(!var_1%in%colnames(MBRN)) MBRN[, var_1:=NA_Date_, with = FALSE]
    if(!var_2%in%colnames(MBRN)) MBRN[, var_2:=NA_Date_, with = FALSE]
  }

  # rev 1.2
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

  MBRN[twin_LL == 1 | trip_LLL == 1 | qua_LLLL == 1, MBRN_livebirth := 1]
  MBRN[twin_MM == 1 | twin_LM == 1 | trip_MMM == 1, MBRN_miscarriage := 1]
  MBRN[twin_SB == 1 | twin_LS == 1 | twin_MS == 1 | trip_SSS == 1 | trip_LLS == 1 | trip_LSS == 1, MBRN_stillbirth := 1]
  MBRN[twin_TT == 1 | twin_LT == 1 | twin_MT == 1 | twin_ST == 1 | trip_LLT == 1 | trip_LTT == 1 | trip_STT == 1 | qua_MMTT == 1, MBRN_TOP := 1]
  MBRN[twin_UK == 1 | twin_UC == 1 | (PLURAL_new == 2 & miscar_31 == 1 & is.na(livebirth2) & is.na(miscar_32) & is.na(stillbirth2) & is.na(unknown2) & is.na(induced_ab2)) | trip_UK == 1, MBRN_unknown := 1]

  ### Rev 1.2
  ### Assignment of gestational age at pregnancy outcome (DAGER_FDATO AND SVLEN_DG!!)
  ### [START]

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

  # Manually checked preg_id 581171 (miscarriage + missing) does not appear to be multifetal gestation
  MBRN[preg_id == 581171, `:=`(MBRN_miscarriage = 1, MBRN_unknown = NA, PLURAL_new = 1)]

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
  sourceCpp(code = "
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

  sourceCpp(code = "
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

  pb=txtProgressBar(0,60,style = 3)
  for (i in 1:60) {
    setTxtProgressBar(pb,i)
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
  cat("\n")


  # Drop if episode_count55 == 0 & preg_id == .
  NPR <- NPR[!(episode_count55 == 0 & is.na(preg_id))]

  NPR = NPR[,c("ID", "date_outcome", "preg_id", "SVLEN_DG", "DAGER_FDATO", "MBRN_livebirth", "MBRN_miscarriage",
               "MBRN_stillbirth", "MBRN_TOP", "stay_id", "diagno", "tilst", "inndato", "sp_MBRN_NPR",
               "top_MBRN_NPR", "sp_NPR_only", "top_NPR_only", "count_sp_N", "count_top_N", "count_molar", "count_ectopic" ),
            with = FALSE]

  ##################### BLOCK 7 ########################
  ######################################################
  cli::cli_progress_update()

  # We drop miscarriages and TOPs that occurred within 56 days after a registered pregnancy in MBRN
  NPR[, MBRN_date_ref := DAGER_FDATO]
  NPR[,inndato:=as.Date(inndato)]
  setorder(NPR,ID, date_outcome, na.last=TRUE)
  NPR[, MBRN_date_ref := fifelse(is.na(MBRN_date_ref),shift_wrt_id(MBRN_date_ref, ID),MBRN_date_ref)]
  NPR[, within_56day_preg := as.numeric(fifelse((inndato - MBRN_date_ref) > 0 &
                                                  (inndato - MBRN_date_ref) < 56, 1, NA_integer_))]
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
  NPR[, preg_id := fifelse(is.na(preg_id), .I + 1000000, preg_id)] #TODO: Change 1000000 with max(preg_id)

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
  for (i in 1:60) {
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
  MBRN[,drop:=fifelse(episode_count55 == 0 & is.na(preg_id),1,0,0)]
  MBRN <- MBRN[drop==0]
  MBRN[,drop:=NULL]


  #################### BLOCK 10 ########################
  ######################################################
  cli::cli_progress_update()
  # Exclude if within 56 days before an event registered in MBRN or NPR
  setorder(MBRN, ID, date_outcome, na.last = TRUE)
  MBRN[, earlier_diagnosis := fifelse(is.na(preg_id) &
                                        (shift_wrt_id(date_outcome, ID, -1) - ant_dager < 56) &
                                        (shift_wrt_id(outcome_after_NPR, ID, -1) %in% c(2, 3, 5, 6)), 1, NA_integer_)]
  MBRN <- MBRN[is.na(earlier_diagnosis)]

  # Exclude if within 56 days after a miscarriage / TOP registered in MBRN or NPR
  MBRN[, later_diagnosis := fifelse(is.na(preg_id) &
                                      (ant_dager - shift_wrt_id(date_outcome, ID) < 56) & (ant_dager - shift_wrt_id(date_outcome, ID) > 0) &
                                      (shift_wrt_id(outcome_after_NPR, ID) %in% c(2, 3, 5, 6)), 1, NA_integer_)]
  MBRN <- MBRN[is.na(later_diagnosis)]

  # Drop gap_previous to later_diagnosis
  MBRN <- MBRN[, !c(paste0("episode_no_gap", 1:60),paste0("episode_start", 1:60),paste0("episode_count", 1:60),
                    "earlier_diagnosis", "later_diagnosis",
                    "gap_previous", "gap_previous_KUHR", "episode_no"), with = FALSE]

  # We drop miscarriages and TOPs that occurred within 56 days after a registered pregnancy in MBRN
  MBRN[, MBRN_date_ref := DAGER_FDATO]
  setorder(MBRN, ID, date_outcome, na.last = TRUE)
  for(i in 1:20)
    MBRN[, MBRN_date_ref := fifelse(is.na(MBRN_date_ref), shift_wrt_id(MBRN_date_ref, ID), MBRN_date_ref)]

  MBRN[, within_56day_preg := fifelse((ant_dager - MBRN_date_ref > 0) & (ant_dager - MBRN_date_ref < 56), 1, NA_integer_)]
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
  MBRN[, outcome_final := fifelse(sp_KUHR_only == 1 & is.na(outcome_after_NPR), 2, outcome_after_NPR)]

  ##################### BLOCK 11 ########################
  ######################################################
  cli::cli_progress_update()
  setorder(MBRN, ID, -date_outcome, na.last = TRUE)
  # Assign a new pregnancy ID for miscarriages identified in KUHR
  MBRN[, preg_id := fifelse(is.na(preg_id), .I + 2000000,preg_id)]

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
  sourceCpp(code = '
          #include <Rcpp.h>
          using namespace Rcpp;
          // [[Rcpp::export]]
void update_code_dates(DataFrame &early_nonlive_births, CharacterVector codes, std::string event_col_prefix, std::string evet_date_col_prefix, const int dlen) {
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
                    ((indi >= si && indi <= doi && !R_IsNA(mini)) || (((indi-doi)>=-70) && indi <= doi))
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
    update_code_dates(early_nonlive_births, code, event_col_prefix = "tilst", evet_date_col_prefix = "inndato", dlen = dlen)
  }
  cat("\n")

  # Impute pregnancy start date based on early pregnancy markers
  codes <- npr_codes$code
  days <- npr_codes$days
  early_nonlive_births[, pr_start_new := as.Date(start)]
  early_nonlive_births[, flag_over12weeks := NA_integer_]
  for (i in seq_along(codes)) {
    code = codes[i]
    dday = days[i]
    vname = paste0(code, "_date")
    early_nonlive_births[is.na(pr_start_new) & get(vname) >= (date_outcome - 84) & get(vname) <= date_outcome, pr_start_new := get(vname) - dday]
    early_nonlive_births[is.na(start) & ((date_outcome - pr_start_new) >= 84) & !is.na(pr_start_new), flag_over12weeks := 1]
    early_nonlive_births[is.na(start) & ((date_outcome - pr_start_new) >= 84), pr_start_new := NA_Date_]
  }


  # Calculate number of repaired flags
  early_nonlive_births[flag_over12weeks == 1 & !is.na(pr_start_new), repaired_flag := 1]

  # Drop unnecessary variables
  early_nonlive_births = early_nonlive_births[, c("MORS_ALDER", "ID", "DAGER_FDATO", "start", "preg_id",
                                                  "date_outcome", "outcome_final", "MC_source" , "TOP_source",
                                                  "pr_start_new", "flag_over12weeks", "repaired_flag"), with = FALSE]

  ##################### BLOCK 13 ########################
  #######################################################

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
    update_code_dates(early_nonlive_births, code, event_col_prefix = "diagnose", evet_date_col_prefix = "ant_dager", dlen = dlen)
  }
  cat("\n")

  # Impute pregnancy start date based on early pregnancy markers
  days <- kuhr_codes$days
  for (i in seq_along(codes)) {
    code = codes[i]
    dday = days[i]
    vname = paste0(code, "_date")
    early_nonlive_births[is.na(pr_start_new) & get(vname) >= (date_outcome - 84) & get(vname) <= date_outcome, pr_start_new := get(vname) - dday]
    early_nonlive_births[is.na(start) & ((date_outcome - pr_start_new) >= 84) & !is.na(pr_start_new), flag_over12weeks := 1]
    early_nonlive_births[is.na(start) & ((date_outcome - pr_start_new) >= 84), pr_start_new := NA_Date_]
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
      early_nonlive_births[outcome_final == 3 & (date_outcome - get(paste0("inndato",   i)) > -28) & (date_outcome - get(paste0("inndato",   i)) <= 0), prenatal_diagnosis := 1]
    }
  }
  if(dlen_kuhr>0)
    for (i in 1:dlen_kuhr) {
      early_nonlive_births[outcome_final == 3 & (date_outcome - get(paste0("ant_dager", i)) > -28) & (date_outcome - get(paste0("ant_dager", i)) <= 0), prenatal_diagnosis := 1]
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

  early_nonlive_births[, pr_start_final := as.Date(pr_start_new)]
  early_nonlive_births[outcome_final == 3 & is.na(start) & !is.na(prenatal_diagnosis), pr_start_final := date_outcome - 80]
  early_nonlive_births[is.na(pr_start_final) & !is.na(flag_over12weeks) & is.na(repaired_flag), pr_start_final := date_outcome - 84]

  early_nonlive_births[, pr_start_new_previous := NA_Date_]
  setorder(early_nonlive_births, ID, date_outcome, na.last = TRUE)

  early_nonlive_births[, pr_start_new_previous := fifelse(pr_start_final < shift_wrt_id(date_outcome, ID), shift_wrt_id(date_outcome,ID), pr_start_new_previous)]
  early_nonlive_births[!is.na(pr_start_new_previous), pr_start_final := pr_start_new_previous]

  early_nonlive_births[, GA_event_week_imput := floor(as.numeric(date_outcome - pr_start_final) / 7)]
  early_nonlive_births[, GA_event_days_imput := as.numeric(date_outcome - pr_start_final)]

  early_nonlive_births[is.na(pr_start_final) & outcome_final == 2, pr_start_final := date_outcome - 70]
  early_nonlive_births[is.na(pr_start_final) & outcome_final == 3, pr_start_final := date_outcome - 56]
  early_nonlive_births[is.na(pr_start_final) & outcome_final == 5, pr_start_final := date_outcome - 77]
  early_nonlive_births[is.na(pr_start_final) & outcome_final == 6, pr_start_final := date_outcome - 56]
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
  early_nonlive_births[, dating_method := fcase(
    !is.na(start), "MBRN (recorded GA)",
    outcome_final == 3 & !is.na(prenatal_diagnosis), "fixed 80 d (TOP with O35)",
    !is.na(pr_start_new), "pregnancy marker",
    !is.na(flag_over12weeks) & is.na(repaired_flag), "fixed 84 d (marker >= 12 wk rejected)",
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
                                                                    "Stillbirth", "Molar pregnancy", "Ectopic pregnancy"))

  early_nonlive_births = merge(early_nonlive_births, outcome_lables, by = "outcome_final", all.x = TRUE)

  early_nonlive_births
}




