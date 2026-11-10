## Louise Forsblom Syke

library(sf)

options(encoding = "ISO-8859-1")
dat_id <- read.csv2("../curated_data/Velmu_sample_ID_POHJE.csv")
dat_r <- read.csv2("../../Data/Benthos/Velmu_data_paper/all_benthos.csv")
species_key <- read.csv2("../curated_data/species_lookup_pohje.csv")


dat <- dat_r[dat_r$samplingID %in% dat_id$Naytteenotto_Id, ]
dat[dat$unitName == "ind/mÂ²", "unitName"] <- "ind/m²"
dat <- dat[dat$unitName == "ind/m²" | dat$samplerName == "Fucus bag", ]
names(dat)[names(dat) == "ï..benthosStationID"] <- "benthosStationID"

dat <- dat[, names(dat) %in% c("benthosStationID", "samplingID", "scientificName", "Time",
                               "samplerName", "samplingArea_cm2", "sieveSizeInMm", "lifeStage",
                               "replicates", "unitName", "meanValue", "intNorthFIN", "intEastFIN")]

dat$meanValue <- as.numeric(gsub(",", ".", dat$meanValue))

pohje_species <- species_key[species_key$Source == "POHJE",
                             names(species_key) %in% c("Group", "scientificName")]

dat <- merge(dat, pohje_species)
dat$MethodCategory <- "Benthic faunal sampling"
dat$Source <- "POHJE"


dat <- dat[, c(2, 3, 4, 12, 13, 5, 6, 7, 1, 8, 9, 10, 11, 14, 15, 16)]

## Remove PV
pv <- read_sf("../input/PV_suoja_alueet_EUREF.shp") 
data_sf <- st_as_sf(dat[, names(dat) %in% c("intEastFIN", "intNorthFIN")],
                    coords = c("intEastFIN", "intNorthFIN"), crs = 3067)

dat <- dat[!lengths(st_intersects(data_sf, pv)) > 0, ]


write.csv2(dat, row.names = FALSE, "../curated_data/POHJE_benthic_faunal_sampling_observations.csv")

length(unique(dat$samplingID))
