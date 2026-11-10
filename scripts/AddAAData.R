## Curates Velmu-LG data
## Louise Forsblom (Syke)

library(plyr)
library(sf)
library(readxl)
library(data.table)
options(encoding = "ISO-8859-1")

## LG
load("../output/LG_combined_202409.RData")


## Add 0.001 (presence) value for species that are present but have no abundance
LG_combined[(is.na(LG_combined$PEITTAVYYS_PROS) &
               LG_combined$HAVAITSIJAN_ANTAMA_NIMI != "Ei lajihavaintoa"), "PEITTAVYYS_PROS"] <- 0.001

## Add presence value to not observed category
LG_combined[LG_combined$HAVAITSIJAN_ANTAMA_NIMI %in% "Ei lajihavaintoa", "PEITTAVYYS_PROS"] <- 0.001

## Remove uncertain observations
LG_combined <- LG_combined[!LG_combined$TARKISTUSTARVE %in% c(20, 30, 31), ]

## Adjust sediment data
names(LG_combined)[names(LG_combined) == "JARJESTYSNUMERO"] <- "Sedimentti"
LG_combined$Sedimentti <- LG_combined$Sedimentti - 1

LG_combined$PVM <- as.Date(LG_combined$PVM_ALKU)
LG_combined$YEAR <- as.numeric(format(LG_combined$PVM, "%Y"))
LG_combined$PVM_ALKU <- NULL

## Projects with the project code NA are reassigned
LG_combined <- LG_combined[!(is.na(LG_combined$HANKE_NIMI) & is.na(LG_combined$HAV_TLAHDE_KUVAUS)), ]
project_na <- LG_combined[is.na(LG_combined$HANKE_NIMI), ]
HAV_TLAHDE_KUVAUS <- tolower(unique(project_na$HAV_TLAHDE_KUVAUS))
velmu_projects <- grep("velmu", HAV_TLAHDE_KUVAUS, value = TRUE)
perustyot_31 <- grep("31", HAV_TLAHDE_KUVAUS, value = TRUE)
LG_combined <- LG_combined[!LG_combined$HANKE_NIMI %in% c("YVA"), ]

LG_combined[tolower(LG_combined$HAV_TLAHDE_KUVAUS) %in% velmu_projects, "HANKE_NIMI"] <- "Vedenalaisen meriluonnon monimuotoisuus (VELMU)"
LG_combined[tolower(LG_combined$HAV_TLAHDE_KUVAUS) %in% perustyot_31, "HANKE_NIMI"] <- "Perusrahoitus (31-työt)"
LG_combined <- LG_combined[!is.na(LG_combined$HANKE_NIMI), ]

## Choose columns to keep
columns_to_choose <- c("LG_KOHDEID", "LG_KERTAID", "PVM", "YEAR", "PAAKOHDE_ID", "SYVYYDEN_KORJAUS",
                       "RUUDUN_SYVYYS", "VIDEOKOHDE", "MENETELMA", "OTANTAMENETELMA",
                       "HAVAITSIJAN_ANTAMA_NIMI", "PEITTAVYYS_PROS", "MAARA", "EI_HAVAINTOJA",
                       "LAJIN_KESKIM_KORKEUS", "Sedimentti", "EPIFYYTTI",
                       "KALLIO", "LOHKARE3000", "LOHKARE1200", "LOHKARE600",
                       "GLASIAALISAVI", "KIVI100", "LIIKKUMATON",
                       "KIVI60", "SORA", "HIEKKA", "SILTTI", "EL_LAJI_ID",
                       "LIIKKUVA", "MUTA", "SAVI", "KONKRETIO",
                       "HIEKKAKIVI", "KEINOTEKOINEN", "TURVE",
                       "PUU", "X_KOORD", "Y_KOORD", "HANKE_NIMI",
                       "PINTA_ALAN_KOKOLK")

LG_combined <- LG_combined[, names(LG_combined) %in% columns_to_choose]

## Rename species
specieslist <- read.csv2("../input/Species_lookup.csv")
specieslist_LG <- unique(specieslist[!is.na(specieslist$EL_LAJI_ID),
                                     names(specieslist) %in% c("NIMI", "EL_LAJI_ID")])
specieslist_LG <- specieslist_LG[specieslist_LG$EL_LAJI_ID %in% LG_combined$EL_LAJI_ID, ]

## Match based on EL_LAJI_ID
LG_combined$IS_ALAND_DATA <- NA
LG_combined$`Aland ID` <- NA

LG_combined <- merge(LG_combined, specieslist_LG, by = "EL_LAJI_ID")

# 
## Adds Aland
load("../input/aland_LG_format_09_2024.RData")
ahv$EI_HAVAINTOJA <- NA
ahv$PVM <- as.Date(ahv$PVM, "%Y.%m.%d")

## Fix naming in the AA data according to list
ahv_part <- ahv[ahv$HAVAITSIJAN_ANTAMA_NIMI %in% specieslist_LG$NIMI, ]
ahv_part$NIMI <- ahv_part$HAVAITSIJAN_ANTAMA_NIMI
ahv_part <- merge(ahv_part, specieslist_LG, by = "NIMI")

specieslist_ahv <- specieslist[!is.na(specieslist$HAVAITSIJAN_ANTAMA_NIMI_AA),
                               names(specieslist) %in% c("NIMI", "EL_LAJI_ID", "HAVAITSIJAN_ANTAMA_NIMI_AA")]
ahv_part_rename <- ahv[ahv$HAVAITSIJAN_ANTAMA_NIMI %in% specieslist_ahv$HAVAITSIJAN_ANTAMA_NIMI_AA, ]
ahv_part_rename$HAVAITSIJAN_ANTAMA_NIMI_AA <- ahv_part_rename$HAVAITSIJAN_ANTAMA_NIMI
ahv_part_rename <- merge(ahv_part_rename, specieslist_ahv,
                         by = "HAVAITSIJAN_ANTAMA_NIMI_AA")
ahv_part_rename$HAVAITSIJAN_ANTAMA_NIMI_AA <- NULL
ahv_part_rename <- ahv_part_rename[!duplicated(ahv_part_rename), ]


## Combine data
LG_combined_new <- rbind(LG_combined, ahv_part, ahv_part_rename)
LG_combined_new$RUUDUN_SYVYYS <- abs(LG_combined_new$RUUDUN_SYVYYS) * -1

### Change to wide
dt <- setDT(LG_combined_new)
dt$ID <- dt$LG_KERTAID
dt[dt$HANKE_NIMI %in% c("Husö specialarbete, rapport nr 152",
                        "Husö specialarbete, rapport nr 155",
                        "Kartering av marina Natura habitat kring Åland",
                        "ÅlandSeaMap"), "ID"] <-
  dt[dt$HANKE_NIMI %in% c("Husö specialarbete, rapport nr 152",
                          "Husö specialarbete, rapport nr 155",
                          "Kartering av marina Natura habitat kring Åland",
                          "ÅlandSeaMap"), "Aland ID"]


other.variables <- c(columns_to_choose[!columns_to_choose %in% c("HAVAITSIJAN_ANTAMA_NIMI",
                                                                 "PEITTAVYYS_PROS", "EL_LAJI_ID",
                                                                 "LAJIN_KESKIM_KORKEUS",
                                                                 "MAARA")], "ID")
dt.aggregated <- dt[, sum(PEITTAVYYS_PROS, na.rm = TRUE), by = c("NIMI", "ID")]
dt.wide <- data.table::dcast(dt.aggregated, ... ~ NIMI, value.var = "V1", fill = 0)
dt.other.variables <- unique(dt[, ..other.variables])
dt.combined <- merge(dt.wide, dt.other.variables)
dt.combined[, DROP:= NULL]
data_wide <- setDF(dt.combined)

## Remove at risk duplicates
to_choose <- c("X_KOORD", "Y_KOORD", "MENETELMA", "PVM",
               "EPIFYYTTI", "RUUDUN_SYVYYS")
lajit <- unique(specieslist$NIMI)

part <- data_wide[, names(data_wide) %in% c(to_choose, lajit)]
data_wide <- data_wide[!duplicated(part), ]

## Remove epiphytic observation duplicates
data_wide <- data_wide[!duplicated(data_wide[, !names(data_wide) %in% "EPIFYYTTI"]), ]
data_wide$EPIFYYTTI <- NULL

## Edit project names
data_wide[data_wide$HANKE_NIMI == "Kartering av marina Natura habitat kring Åland", "HANKE_NIMI"] <- "Mapping Marine Natura 2000 habitats in Åland"
data_wide[data_wide$HANKE_NIMI == "Husö specialarbete, rapport nr 152", "HANKE_NIMI"] <- "Mapping and habitat classification of the underwater environment in Lumparn report 152"
data_wide[data_wide$HANKE_NIMI == "Husö specialarbete, rapport nr 155", "HANKE_NIMI"] <- "Mapping and habitat classification of the underwater environment in Geta, report 155"
data_wide[data_wide$HANKE_NIMI == "Nature and Nurture of the Northern Baltic Sea", "HANKE_NIMI"] <- "NANNUT"
data_wide[data_wide$HANKE_NIMI == "Perusrahoitus (31-työt)", "HANKE_NIMI"] <- "Parks and Wildlife Finland"
data_wide[data_wide$HANKE_NIMI == "Seamless maps and management of the Northern Bothn", "HANKE_NIMI"] <- "SEAmBOTH"
data_wide[data_wide$HANKE_NIMI == "Suomen mereisen NATURA 2000 -verkoston inventointi", "HANKE_NIMI"] <- "FINMARINET"
data_wide[data_wide$HANKE_NIMI == "Vedenalaisen meriluonnon monimuotoisuus (VELMU)", "HANKE_NIMI"] <- "Velmu"


## Save data
save(data_wide, file = paste0("../output/LG_AHV_aineistot_", Sys.Date(), ".RData"))

## GPKG version
both_gpkg <- st_as_sf(data_wide, coords = c("X_KOORD", "Y_KOORD"), crs = 3067)
write_sf(both_gpkg, paste0("../output/LG_AHV_aineistot_", Sys.Date(), ".gpkg"))

