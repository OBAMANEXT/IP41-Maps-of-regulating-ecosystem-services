## Louise Forsblom Syke

library(ggplot2)
library(reshape2)
library(plyr)
options(scipen = 999)

## Vegetation data
dat_long <- read.csv2("../curated_data/LajiGIS_AA_vegetation_mapping_video_observations.csv")

species_to_drop <- read.csv2("../curated_data/observation_summary_table.csv")
length(unique(species_to_drop$Genus))

species_to_drop <- species_to_drop[species_to_drop$TerrestrialVegetation == "x", "verbatimIdentification"]
length(unique(species_to_drop))
dat_long <- dat_long[!dat_long$verbatimIdentification %in% species_to_drop, ]

## Benthic data
poh <- read.csv2("../curated_data/POHJE_benthic_faunal_sampling_observations.csv")

names(poh)[names(poh) == "samplingID"] <- "RecordID"
poh <- poh[!is.na(poh$meanValue), ]
poh$verbatimIdentification <- poh$scientificName

poh_dat_s <- ddply(poh, c("RecordID", "verbatimIdentification", "MethodCategory", "Group"), summarise,
                   PresenceAbsence = as.numeric(meanValue > 0))

dat_long$PresenceAbsence <- 1
dat_long <- dat_long[, names(dat_long) %in% names(poh_dat_s)]

### Combine and plot
poh_dat_s <- poh_dat_s[poh_dat_s$PresenceAbsence > 0, ]

dat_long$Source <- "LajiGIS"
poh_dat_s$Source <- "POHJE"
dat_both <- rbind(dat_long, poh_dat_s)

## Rorder method category
dat_both[dat_both$MethodCategory == "Benthic faunal sampling", "MethodCategory"] <- "Faunal sampling"
dat_both$MethodCategory <- factor(dat_both$MethodCategory,
                                  levels = c("Faunal sampling",
                                             "Video methods",
                                             "Vegetation mapping"))

dat_cond <- ddply(dat_both, c("Group", "MethodCategory"),
                  summarise, Observations = length(verbatimIdentification))
dat_cond[is.na(dat_cond$Group), "Group"] <- "No species recorded"

species_key <- read.csv2("../curated_data/observation_summary_table.csv")
species_key <- unique(species_key[, names(species_key) %in% c("verbatimIdentification", "Genus")])
species_key <- species_key[species_key$Genus != "", ]

dat_gen <- dat_both[dat_both$verbatimIdentification %in% species_key$verbatimIdentification, ]
dat_gen <- merge(dat_gen, species_key)

dat_plot <- ddply(dat_gen, c("Group", "MethodCategory"),
                  summarise, N = length(unique(Genus)))


plot_col <- c("#e4f1e1", "#63a6a0", "#0d585f")
g <- ggplot(dat_cond, aes(reorder(Group, Observations),
                          Observations, fill = MethodCategory)) +
  geom_bar(stat = "Identity", col = "black")
g <- g + coord_flip()
g <- g + theme_classic() + xlab("") + labs(tag = "(a)")
# g <- g + facet_wrap(~Source, ncol = 2)
g <- g + scale_fill_manual(values = plot_col)
g <- g + ylab("Number of observations") + scale_y_continuous(expand = c(0, 0), limits = c(0, 300000))
g <- g + theme(axis.text = element_text(color = "black", size = 16),
               axis.title = element_text(size = 16),
               legend.title = element_blank(),
               legend.position = "none",
               axis.title.y = element_text(size = 16, color = "black"), 
               axis.title.x = element_text(size = 16, color = "black"),
               axis.text.x = element_text(size = 16, color = "black"), 
               axis.text.y = element_text(size = 16, color = "black"),
               legend.text = element_text(size = 16, color = "black"))
g


## Fill for plot
dat_expand <- expand.grid(Group = unique(dat_plot$Group),
                          MethodCategory = unique(dat_plot$MethodCategory),
                          Zero = 0)

dat_plot_g <- merge(dat_plot, dat_expand, by = c("Group", "MethodCategory"), all.y = TRUE)
dat_plot_g[is.na(dat_plot_g$N), "N"] <- 0

p <- ggplot(dat_plot_g, aes(reorder(Group, N), N, fill = MethodCategory)) +
  geom_bar(stat = "Identity", col = "black", position = position_dodge(0.7))
p <- p + coord_flip() + labs(tag = "(b)")
p <- p + theme_classic() + xlab("")
p <- p + scale_fill_manual(values = plot_col,
                           breaks = c("Vegetation mapping",
                                      "Video methods",
                                      "Faunal sampling"))
p <- p + ylab("Number of genera") + scale_y_continuous(expand = c(0, 0), limits = c(0, 55))
p <- p + theme(axis.text = element_text(color = "black", size = 16),
               axis.title = element_text(size = 16), legend.title = element_blank(),
               legend.position = c(0.7, 0.3),
               axis.title.y = element_text(size = 16, color = "black"), 
               axis.title.x = element_text(size = 16, color = "black"),
               axis.text.x = element_text(size = 16, color = "black"), 
               axis.text.y = element_text(size = 16, color = "black"),
               legend.text = element_text(size = 16, color = "black"))
p


library(ggrepel)
library(grid)
jpeg("../graphs/figure_6.jpeg", width = 35, height = 15,
    res = 600, units = "cm")
gg <- ggplotGrob(g)
dg <- ggplotGrob(p)
grid::grid.newpage()
grid::grid.draw(cbind(gg, dg))
dev.off()


svg("../graphs/figure_6.svg", width = 20, height = 10)
gg <- ggplotGrob(g)
dg <- ggplotGrob(p)
grid::grid.newpage()
grid::grid.draw(cbind(gg, dg))
dev.off()

