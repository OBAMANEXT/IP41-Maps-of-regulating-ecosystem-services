## Louise Forsblom Syke

options(encoding = "ISO-8859-1")
library(reshape2)
library(plyr)
library(sf)

## Loads the data from lajiGIS
load("../../Velmu/output/LG_AHV_aineistot_2024-09-23.RData")

## Removes sensitive areas
pv <- read_sf("../input/PV_suoja_alueet_EUREF.shp") 
data_sf <- st_as_sf(data_wide[, names(data_wide) %in% c("X_KOORD", "Y_KOORD")],
                    coords = c("X_KOORD", "Y_KOORD"), crs = 3067)
data_wide <- data_wide[!lengths(st_intersects(data_sf, pv)) > 0, ]

species_key <- read.csv2("../../Velmu/input/Species_lookup.csv")

## Rename methods
data_wide <- data_wide[data_wide$YEAR >= 2004, ]
data_wide <- data_wide[!is.na(data_wide$MENETELMA), ]
data_wide <- data_wide[!data_wide$MENETELMA %in% c(24, 28, 36, 39), ]
data_wide[data_wide$MENETELMA %in% c(44), "Method"] <- "Video + rake"
data_wide[data_wide$MENETELMA %in% c(21), "Method"] <- "Transect"
data_wide[data_wide$MENETELMA %in% c(25), "Method"] <- "Dive"
data_wide[data_wide$MENETELMA %in% 22, "Method"] <- "Rake"
data_wide[data_wide$MENETELMA %in% 23, "Method"] <- "Aquascope"
data_wide[data_wide$MENETELMA %in% 20, "Method"] <- "Snorkelling"
data_wide[data_wide$MENETELMA %in% 42, "Method"] <- "Video"
data_wide[data_wide$MENETELMA %in% 43, "Method"] <- "ROV"
data_wide <- data_wide[!is.na(data_wide$Method), ]
data_wide[data_wide$Method != "Transect", "PAAKOHDE_ID"] <- NA

## Defines the area
data_wide$Area <- NA
data_wide[data_wide$PINTA_ALAN_KOKOLK == 14 &
            !is.na(data_wide$PINTA_ALAN_KOKOLK), "Area"] <- "1m2"
data_wide[data_wide$PINTA_ALAN_KOKOLK == 16 &
            !is.na(data_wide$PINTA_ALAN_KOKOLK), "Area"] <- "4m2"
data_wide[data_wide$PINTA_ALAN_KOKOLK %in% c(1, 4, 15, 99, 100,
                                             250, 400, 2678) &
            !is.na(data_wide$PINTA_ALAN_KOKOLK), "Area"] <- NA
data_wide[data_wide$HANKE_NIMI == "Kvarken Flada", "Area"] <- "0.25m2"

## Change Finnish names
finnish_names <- unique(species_key[species_key$Translation != "",
                                    names(species_key) %in% c("Translation", "NIMI")])
for (i in 1:nrow(finnish_names)) {
  old_name <- finnish_names[i, "NIMI"]
  new_name <- finnish_names[i, "Translation"]
  names(data_wide)[names(data_wide) %in% old_name] <- new_name
  
}

species_key[species_key$Translation != "", "NIMI"] <-
  species_key[species_key$Translation != "", "Translation"]


## Remove species with no observations
all_species <- unique(species_key$NIMI)
sp_in_data <- all_species[all_species %in% colnames(data_wide)]
sp_in_data_n <- colSums(data_wide[, names(data_wide) %in% sp_in_data] > 0, na.rm = TRUE)
sp_to_drop <- names(sp_in_data_n)[sp_in_data_n == 0]
data_wide <- data_wide[, !names(data_wide) %in% sp_to_drop]

## Drops sensitive species
data_wide$`Natrix natrix` <- NULL
data_wide$`Lutra lutra` <- NULL
data_wide$`Bufo bufo` <- NULL
data_wide$`Rana arvalis` <- NULL


## Renames columns
names(data_wide)[names(data_wide) == "OTANTAMENETELMA"] <- "SamplingDesign"
names(data_wide)[names(data_wide) == "HANKE_NIMI"] <- "Project"
names(data_wide)[names(data_wide) == "PVM"] <- "Date"
names(data_wide)[names(data_wide) == "X_KOORD"] <- "intEastFIN"
names(data_wide)[names(data_wide) == "Y_KOORD"] <- "intNorthFIN"
names(data_wide)[names(data_wide) == "Sedimentti"] <- "LooseSurfaceSediment"
names(data_wide)[names(data_wide) == "PAAKOHDE_ID"] <- "TransectId"
names(data_wide)[names(data_wide) == "ID"] <- "RecordID"

## Change data to long format
names(data_wide)[names(data_wide) == "Ei lajihavaintoa"] <- "No species recorded"
data_wide[rowSums(data_wide[, names(data_wide) %in% unique(species_key$NIMI)],
                  na.rm = TRUE) == 0, "No species recorded"] <- 1

dat_long <- data_wide[, names(data_wide) %in% c("RecordID", unique(species_key$NIMI),
                                                "No species recorded",
                                                "Method", "Date", "Project", "SamplingDesign",
                                                "Area", "TransectId",
                                                "intEastFIN", "intNorthFIN",
                                                "LooseSurfaceSediment")]
dat_long <- melt(dat_long,
                 id.vars = c("RecordID", "Method", "Date", "Project",
                             "SamplingDesign", "Area",
                             "intEastFIN", "intNorthFIN",
                             "LooseSurfaceSediment", "TransectId"),
                 variable.name = "scientificName",
                 value.name = "Cover", na.rm = TRUE)
dat_long <- dat_long[dat_long$Cover > 0, ]

## Add source
dat_long$Source <- "LajiGIS"
dat_long[dat_long$Project %in% c("ÅlandSeaMap",
                                 "Mapping Marine Natura 2000 habitats in Åland",
                                 "Mapping and habitat classification of the underwater environment in Lumparn report 152",
                                 "Mapping and habitat classification of the underwater environment in Geta, report 155"),
         "Source"] <- "Abo Akemdemi"

## Drop sensitive observations
dat_long <- dat_long[!(dat_long$Source %in% "Abo Akemdemi" &
                         dat_long$scientificName %in% "Chara horrida"), ]

dat_long <- dat_long[!(dat_long$Source %in% "Abo Akemdemi" &
                         dat_long$scientificName %in% "Chara connivens"), ]

## Match species lists
species_listing <- unique(species_key[, names(species_key) %in% c("NIMI", "Group", "LAJI_URI")])
names(species_listing) <- c("scientificName", "Species_URI", "Group")

dat_long <- merge(dat_long, species_listing, all.x = TRUE, by = "scientificName")
dat_long$verbatimIdentification <- dat_long$scientificName
dat_long[dat_long$Species_URI %in% "MXkoodi puuttuu", "scientificName"] <- NA
dat_long[dat_long$Species_URI %in% "MXkoodi puuttuu", "Species_URI"] <- NA
dat_long[dat_long$Source == "Abo Akademi", "Species_URI"] <- NA

dat_long$MethodCategory <- "Vegetation mapping"
dat_long[dat_long$Method %in% c("Video", "ROV"), "MethodCategory"] <- "Video methods"

## Cover to NA for species that have not been consistently recorded as cover
## Cover to NA for cover values below the lowest defined by the method guide (0.1)
sp_drop <- species_listing[species_listing$Group %in% c("Crustacea",  "Fish", "Insecta",
                                                        "Polychaeta", "Other taxa"), "scientificName"]
sp_drop <- sp_drop[!sp_drop %in% c("Amphibalanus improvisus",
                                   "Drifting unidentified vegetal material",
                                   "Drifting macrophyte")]

as_pa <- unique(c("Anodonta", "Anodonta anatina", "Aurelia aurita", "Crangon crangon", "Eriocheir sinensis",
                  "Galerucella nymphaeae", "Macroplea", "Macroplea appendiculata", "Macroplea mutica", "Macroplea pubipennis",
                  "Palaemon", "Palaemon adspersus", "Palaemon elegans", "Rhithropanopeus harrisii", "Saduria entomon", "Unio",
                  "Unio pictorum", "Unio tumidus", "Unionidae", "Viviparus", "Viviparus contectus", "Hydrobia",
                  "Theodoxus fluviatilis", sp_drop, "Pisces"))

dat_long[dat_long$verbatimIdentification %in% as_pa, "Cover"] <- 9999
dat_long[dat_long$Cover < 0.1, "Cover"] <- 9999
dat_long[dat_long$verbatimIdentification %in% "No species recorded", "Cover"] <- 0
dat_long <- dat_long[!dat_long$RecordID %in% 1017663, ]


dat_long <- dat_long[, c(2, 9, 8, 3, 11, 17, 6,
                         7, 4, 1, 16, 12, 10, 15, 5, 13)]

save(dat_long, file = "../curated_data/LajiGIS_AA_vegetation_mapping_video_observations.RData")
write.csv2(dat_long, row.names = FALSE,
           file = "../curated_data/LajiGIS_AA_vegetation_mapping_video_observations.csv")


## Values used for paper
month <- data.frame(month = as.numeric(format(dat_long$Date, "%m")),
                    ID = dat_long$RecordID)
month <- unique(month)

table(month$month)
(sum(month$month %in% c(6, 7, 8, 9))) / nrow(month)

length(unique(dat_long$RecordID))
