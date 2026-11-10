## Louise Forsblom Syke
## Prepares Aland data for merging with the other files

library(readxl)
library(dplyr)
library(reshape2)


options(encoding = "ISO-8859-1")

## Dive transects
col_type <- c(rep("guess", 14), "date", rep("guess", 34), rep("numeric", 440), rep("guess", 4))
dat <- read_xlsx("../input/Aland_Vegetation_data_Velmu_ed2.xlsx",
                 guess_max = 6158, col_types = col_type)
dat$Sedimentti <- dat$Sediment
dat[dat$Sedimentti == 4 & !is.na(dat$Sedimentti), "Sedimentti"] <- 3
dat$RUUDUN_SYVYYS <- as.numeric(gsub(",", ".", dat$`Depth of the point`)) * -1

## REMOVE EMPTY
dat[, 50:489] <- apply(dat[, 50:489], 2, FUN = as.numeric)
dat[, 50:489][is.na(dat[, 50:489])] <- 0

dat$`Potamogeton compressus` <- rowSums(dat[, names(dat) %in% c("Potamogeton compressus...326",
                                                                "Potamogeton compressus...485")],
                                        na.rm = TRUE)
dat$`Potamogeton friesii` <- rowSums(dat[ , names(dat) %in% c("Potamogeton friesii...328",
                                                              "Potamogeton friesii...484")],
                                     na.rm = TRUE)
dat <- select(dat, -c(`Potamogeton compressus...326`, `Potamogeton compressus...485`,
                      `Potamogeton friesii...328`, `Potamogeton friesii...484`))

colSums(dat[, c(50:485,492, 493)], na.rm = TRUE) -> to_drop
names(to_drop)[to_drop > 0] -> to_keep
names(to_drop)[to_drop == 0] -> to_drop
sp_to_keep <- gsub(" sp.$", "", to_keep)
write.csv(sp_to_keep, row.names = FALSE, "../input/aa_observation_names.csv")
dat <- dat[, !names(dat) %in% to_drop]

dat$PVM <- format(dat$`Sampling date`, "%Y.%m.%d")
dat <- as.data.frame(dat)

dat$PAAKOHDE_ID <- dat$`Site name`
dat[dat$Method == 36, "PAAKOHDE_ID"] <- NA

names(dat)[names(dat) == "Method"] <- "MENETELMA"
names(dat)[names(dat) == "Rock"] <- "KALLIO"
names(dat)[names(dat) == "Boulder  > 300 cm"] <- "LOHKARE3000"
names(dat)[names(dat) == "Boulder 120-300 cm"] <- "LOHKARE1200"
names(dat)[names(dat) == "Boulder 60-120 cm"] <- "LOHKARE600"
names(dat)[names(dat) == "Hard clay"] <- "GLASIAALISAVI"
names(dat)[names(dat) == "Stones 10-60 cm"] <- "KIVI100"
names(dat)[names(dat) == "Non-mobile substrate"] <- "LIIKKUMATON"
names(dat)[names(dat) == "Stones   6-10 cm"] <- "KIVI60"
names(dat)[names(dat) == "Gravel 0,2-6 cm"] <- "SORA"
names(dat)[names(dat) == "Sand 0,06-2 mm"] <- "HIEKKA"
names(dat)[names(dat) == "Silt 0,002-0,06"] <- "SILTTI"
names(dat)[names(dat) == "Mobile substrate"] <- "LIIKKUVA"
names(dat)[names(dat) == "Mud"] <- "MUTA"
names(dat)[names(dat) == "Clay < 0,002 mm"] <- "SAVI"
names(dat)[names(dat) == "Concretes"] <- "KONKRETIO"
names(dat)[names(dat) == "Sandstone"] <- "HIEKKAKIVI"
names(dat)[names(dat) == "Man-made structures"] <- "KEINOTEKOINEN"
names(dat)[names(dat) == "Peat"] <- "TURVE"
names(dat)[names(dat) == "Tree trunks/branches"] <- "PUU"
names(dat)[names(dat) == "Point coordinate E (Euref Fin)"] <- "X_KOORD"
names(dat)[names(dat) == "Point coordinate N (Euref Fin)"] <- "Y_KOORD"
names(dat)[names(dat) == "Sampling year"] <- "YEAR"
dat$LG_KOHDEID <- NA
dat$LG_KERTAID <- NA
dat$SYVYYDEN_KORJAUS <- NA
dat$VIDEOKOHDE <- NA
dat$OTANTAMENETELMA <- NA
dat$EPIFYYTTI <- NA
dat$HANKE_NIMI <- dat$Project
dat$LAJIN_KESKIM_KORKEUS <- NA
dat$MAARA <- NA

## Vaihda nimet
dat <- dat %>%
  tidyr::replace_na(list(KALLIO = 0,
                         LOHKARE3000 = 0,
                         LOHKARE1200 = 0,
                         LOHKARE600 = 0,
                         GLASIAALISAVI = 0,
                         KIVI100 = 0,
                         LIIKKUMATON = 0,
                         KIVI60 = 0,
                         SORA = 0,
                         HIEKKA = 0,
                         SILTTI = 0,
                         LIIKKUVA = 0,
                         MUTA = 0,
                         SAVI = 0,
                         KONKRETIO = 0,
                         HIEKKAKIVI = 0,
                         KEINOTEKOINEN = 0,
                         TURVE = 0,
                         PUU = 0)) %>%
  dplyr::mutate(MENETELMA = replace(MENETELMA, MENETELMA == 25, 21), ## Snorklauslinja
                MENETELMA = replace(MENETELMA, MENETELMA == 36, 42)) ## Video

dat$`Ei lajihavaintoa` <- as.numeric(rowSums(dat[, names(dat) %in% to_keep],
                                        na.rm = TRUE) == 0)

columns_to_choose <- c("LG_KOHDEID", "LG_KERTAID", "PVM", "YEAR", "PAAKOHDE_ID", "SYVYYDEN_KORJAUS",
                       "RUUDUN_SYVYYS", "VIDEOKOHDE", "MENETELMA", "OTANTAMENETELMA", "MAARA",
                       "LAJIN_KESKIM_KORKEUS", "Sedimentti", "EPIFYYTTI",
                       "KALLIO", "LOHKARE3000", "LOHKARE1200", "LOHKARE600",
                       "GLASIAALISAVI", "KIVI100", "LIIKKUMATON",
                       "KIVI60", "SORA", "HIEKKA", "SILTTI",
                       "LIIKKUVA", "MUTA", "SAVI", "KONKRETIO",
                       "HIEKKAKIVI", "KEINOTEKOINEN", "TURVE",
                       "PUU", "X_KOORD", "Y_KOORD", "HANKE_NIMI", "Aland ID")

aland_fin <- dat[, names(dat) %in% c(columns_to_choose,
                                     "Ei lajihavaintoa",
                                     to_keep)]

ahv <- melt(aland_fin, id.vars = columns_to_choose,
            value.name = "PEITTAVYYS_PROS",
            variable.name = "HAVAITSIJAN_ANTAMA_NIMI")
ahv <- ahv[ahv$PEITTAVYYS_PROS > 0, ]

ahv$IS_ALAND_DATA <- "YES"

## Add observation area
ahv[ahv$MENETELMA %in% c(21), "PINTA_ALAN_KOKOLK"] <- 14
ahv[ahv$MENETELMA %in% c(21) &
      ahv$HANKE_NIMI == "Husö specialarbete, rapport nr 155", "PINTA_ALAN_KOKOLK"] <- 16

ahv$HAVAITSIJAN_ANTAMA_NIMI <- gsub(" sp.$", "", ahv$HAVAITSIJAN_ANTAMA_NIMI)

save(ahv, file = "../input/aland_LG_format_09_2024.RData")
