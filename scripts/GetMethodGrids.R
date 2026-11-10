setwd("D:/Users/e1002963/Local/Velmu_data_paper/scripts")

library(sf)
library(ggplot2)

#### Load data
sgrid <- read_sf("../grids/Grid_10km.shp")
dat_long <- read.csv2("../curated_data/LajiGIS_AA_vegetation_mapping_video_observations.csv")
dat_long <- dat_long[!duplicated(dat_long$RecordID), ]
dat_long <- dat_long[, names(dat_long) %in% c("RecordID", "MethodCategory",
                                              "intEastFIN", "intNorthFIN",
                                              "Date")]

poh <- read.csv2("../curated_data/POHJE_benthic_faunal_sampling_observations.csv")
poh <- poh[, names(poh) %in% c("samplingID", "MethodCategory",
                               "intEastFIN", "intNorthFIN", "Time")]
poh <- unique(poh)

names(poh)[names(poh) == "samplingID"] <- "RecordID"
dat_long$Year <- as.numeric(format(as.Date(dat_long$Date), "%Y"))
poh$Year <- as.numeric(format(as.Date(poh$Time), "%Y"))
poh$Time <- NULL
dat_long$Date <- NULL

both <- rbind(dat_long, poh)

dat_points <- st_as_sf(both, coords = c("intEastFIN", "intNorthFIN"), crs = 3067)
dat.sf <- st_drop_geometry(st_join(dat_points, sgrid))

library(plyr)
dat.number <- ddply(dat.sf[dat.sf$MethodCategory == "Benthic faunal sampling", ],
                    c("square", "MethodCategory"),
                    summarise,
                    N = length(square))

dat.num <- merge(sgrid, dat.number[!is.na(dat.number$square), ])
# write_sf(dat.num, "../grids/Fauna_observation_grids.shp")


dat.number <- ddply(dat.sf[dat.sf$MethodCategory == "Video methods", ],
                    c("square", "MethodCategory"),
                    summarise,
                    N = length(square))

dat.num <- merge(sgrid, dat.number[!is.na(dat.number$square), ])
# write_sf(dat.num, "../grids/Video_observation_grids.shp")


dat.number <- ddply(dat.sf[dat.sf$MethodCategory == "Vegetation mapping", ],
                    c("square", "MethodCategory"),
                    summarise,
                    N = length(square))

dat.num <- merge(sgrid, dat.number[!is.na(dat.number$square), ])
# write_sf(dat.num, "../grids/Diving_observation_grids.shp")



dat_plot <- ddply(dat.sf,
                    c("Year", "MethodCategory"),
                    summarise,
                    N = length(square))


# dat_plot[dat_plot$MethodCategory == "Benthic faunal sampling", "MethodCategory"] <- "Benthic faunal\nsampling"

velmu_col <- c("#e4f1e1", "#63a6a0", "#0d585f")
png(file = "../graphs/Forsblom_Virtanen_et_al_map_graph.png", width =  12,
    height = 9, res = 600, units = "cm")

g <- ggplot(dat_plot, aes(Year, N, fill = MethodCategory)) + geom_col(col = "black")
g <- g + ylab("Observations")
g <- g + scale_fill_manual(values = velmu_col)
g <- g + scale_x_continuous(expand = c(0, 0))
g <- g + scale_y_continuous(expand = c(0, 0), limits = c(0, 30000),
                            breaks = c(0, 5000, 15000, 25000))
g <- g + theme_classic() + xlab("")
g <- g + theme(legend.title = element_blank(),
               legend.position = c(0.82, 0.85),
               legend.background = element_blank(),
                     axis.title.y = element_text(size = 13, color = "black"), 
                     axis.title.x = element_text(size = 13, color = "black"),
                     axis.text.x = element_text(size = 12, color = "black"), 
                     axis.text.y = element_text(size = 12, color = "black"),
                     legend.text = element_text(size = 12, color = "black"),
                     plot.title = element_text(size = 14),
               plot.margin = margin(t = 5.5, r = 40,
                                    b = 5.5, l = 5.5, unit = "pt"))
g

dev.off()
