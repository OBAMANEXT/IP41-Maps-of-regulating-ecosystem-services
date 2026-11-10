## Louise Forsblom Syke
setwd("D:/Users/e1002963/Local/Velmu_data_paper/scripts")

library(sf)
library(plyr)

#### Load data
sgrid <- read_sf("../grids/Grid_10km.shp")
load("../curated_data/LajiGIS_AA_vegetation_mapping_video_observations.RData")
dat_long <- dat_long[, names(dat_long) %in% c("RecordID", "MethodCategory",
                                              "intEastFIN", "intNorthFIN",
                                              "verbatimIdentification")]

### Add genus
species_key <- read.csv2("../curated_data/observation_summary_table.csv")
species_key <- species_key[species_key$TerrestrialVegetation != "x", ]
species_key <- unique(species_key[, names(species_key) %in% c("verbatimIdentification",
                                                              "Genus", "Group")])
species_key <- species_key[species_key$Genus != "", ]

dat_gen <- dat_long[dat_long$verbatimIdentification %in% species_key$verbatimIdentification, ]
dat_gen <- merge(dat_gen, species_key)

dat_points <- st_as_sf(dat_gen, coords = c("intEastFIN", "intNorthFIN"), crs = 3067)
dat.sf <- st_drop_geometry(st_join(dat_points, sgrid))

## Algae
dat.number <- ddply(dat.sf[dat.sf$Group == "Algae", ],
                    c("square"),
                    summarise,
                    N = length(unique(Genus)))

dat.num <- merge(sgrid, dat.number[!is.na(dat.number$square), ])
write_sf(dat.num, "../grids/GRID_GENUS_ALGAE.shp")

## Mosses
dat.number <- ddply(dat.sf[dat.sf$Group == "Bryophyta", ],
                    c("square"),
                    summarise,
                    N = length(unique(Genus)))

dat.num <- merge(sgrid, dat.number[!is.na(dat.number$square), ])
write_sf(dat.num, "../grids/GRID_GENUS_MOSSES.shp")

## Aquatic vascular plants
dat.number <- ddply(dat.sf[dat.sf$Group == "Vascular plants", ],
                    c("square"),
                    summarise,
                    N = length(unique(Genus)))

dat.num <- merge(sgrid, dat.number[!is.na(dat.number$square), ])
write_sf(dat.num, "../grids/GRID_GENUS_AQUATIC_PLANTS.shp")


## Endangered species based on Kuusela et al. 2022
## http://hdl.handle.net/10138/344399

keep <- c("Alisma wahlenbergii",
          "Ceramium virgatum",
          "Chara braunii",
          "Chara horrida",
          "Crassula aquatica",
          "Dictyosiphon chordaria",
          "Fucus radicans",
          "Fucus vesiculosus",
          "Fucus",
          "Hippuris lanceolata",
          "Hippuris tetraphylla",
          "Najas tenuissima",
          "Nitella hyalina",
          "Nitellopsis obtusa",
          "Persicaria foliosa",
          "Potamogeton friesii",
          "Rhodomela confervoides",
          "Ruppia maritima",
          "Ruppia spiralis",
          "Zostera marina")

dat.end <- dat.sf[dat.sf$verbatimIdentification %in% keep, ]
dat.end[dat.end$verbatimIdentification %in% c("Fucus vesiculosus", "Fucus"),
        "scientificName"] <- "Fucus"

dat.number <- ddply(dat.end,
                    c("square"),
                    summarise,
                    N = length(unique(verbatimIdentification)))

dat.num <- merge(sgrid, dat.number[!is.na(dat.number$square), ])

write_sf(dat.num, "../grids/GRID_GENUS_ENDANGERED.shp")
