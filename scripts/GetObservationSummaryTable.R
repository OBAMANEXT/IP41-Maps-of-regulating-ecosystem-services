## Louise Forsblom Syke

library(plyr)

options(encoding = "ISO-8859-1")
dat <- read.csv2("../../Velmu/input/Species_lookup.csv")

## Change Finnish names
finnish_names <- unique(dat[dat$Translation != "",
                                  names(dat) %in% c("Translation", "NIMI")])
for (i in 1:nrow(finnish_names)) {
  old_name <- finnish_names[i, "NIMI"]
  new_name <- finnish_names[i, "Translation"]
  dat[dat$NIMI %in% old_name, "NIMI"] <- new_name
  
}


## Table
load("../curated_data/LajiGIS_AA_vegetation_mapping_video_observations.RData")
dat <- dat[dat$NIMI %in% dat_long$verbatimIdentification, ]

for (i in 1:nrow(dat)) {
  species <- dat[i, "NIMI"]
  N <- nrow(dat_long[dat_long$verbatimIdentification == species, ])
  dat[i, "N"] <- N
}

dat$EL_LAJI_ID <- NULL
dat$ELIORYHMA <- NULL
dat$HAVAITSIJAN_ANTAMA_NIMI_AA <- NULL

names(dat)[names(dat) == "NIMI"] <- "verbatimIdentification"
names(dat)[names(dat) == "genus"] <- "Genus"
names(dat)[names(dat) == "IncludeForSpeciesrichness"] <- "IncludeForAquaticVegSpeciesRichness"
names(dat)[names(dat) == "LAJI_URI"] <- "Species_URI"

dat$scientificName <- dat$verbatimIdentification
dat[dat$Species_URI %in% "MXkoodi puuttuu", "scientificName"] <- NA
dat[dat$Species_URI %in% "MXkoodi puuttuu", "Species_URI"] <- NA

dat$Source <- "LajiGIS & AA"

## Benthic
poh <- read.csv2("../curated_data/POHJE_benthic_faunal_sampling_observations.csv")
type_poh <- read.csv2("../curated_data/species_lookup_pohje.csv")

poh_dat <- data.frame(scientificName = unique(poh$scientificName),
                      Species_URI = "",
                      Translation = "")

poh_dat$verbatimIdentification <- poh_dat$scientificName

poh_dat <- merge(poh_dat, type_poh, all.x = TRUE)
names(poh_dat)[names(poh_dat) == "IncludeForSpeciesrichness"] <- "IncludeForAquaticVegSpeciesRichness"


poh_dat_s <- ddply(poh, c("scientificName"), summarise,
                   N = length(samplingID))

poh_dat_n <- merge(poh_dat, poh_dat_s, by = "scientificName")

dat_both <- rbind(dat, poh_dat_n)
dat_both[is.na(dat_both)] <- ""

dat_both <- dat_both[, c(9, 1, 4, 2, 3, 5, 6, 8, 10, 7)]

write.csv2(dat_both, row.names = FALSE, "../curated_data/observation_summary_table.csv")


length(unique(dat$RecordID)) + length(unique(poh$samplingID))
length(unique(dat_both$Genus))
length(unique(dat_both[dat_both$TerrestrialVegetation == "", "Genus"]))

##
to_paper <- dat_both[dat_both$IncludeForAquaticVegSpeciesRichness != "", ]
table(to_paper$IncludeForAquaticVegSpeciesRichness)
sum(to_paper$IncludeForAquaticVegSpeciesRichness == "x") +
  length(unique(to_paper$IncludeForAquaticVegSpeciesRichness)) - 1
