## Louise Forsblom Syke
# Creates species lookup based on the LajiGIS tables LG_LAJINIMI and LG_LAJI
# Retains columns for scientificName, verbatimIdentification and scientificNameAuthorship
# Subsequent manual match of Åbo Akademi species names to list 


library(readxl)
library(sf)

options(encoding = "ISO-8859-1")

ID <- read_sf("../Meriaineistot_helmikuu_2024.gdb/", "LG_LAJINIMI")
ID <- ID[ID$KIELI == 1, ]
ID <- ID[ID$ASEMA == 1, ]
ID <- ID[, names(ID) %in% c("NIMI", "EL_LAJI_ID")]

ID_LAJIGIS <- read_sf("../Meriaineistot_helmikuu_2024.gdb/", "LG_LAJI")
ID_LAJIGIS <- ID_LAJIGIS[, names(ID_LAJIGIS) %in% c("ELIORYHMA", "SALATTAVA",
                                                    "EL_LAJI_ID", "LAJI_URI")]
all_names <- merge(ID_LAJIGIS, ID, all.x = TRUE)

load("../output/LG_combined_202409.RData")
observed_taxa <- unique(LG_combined[, names(LG_combined) %in% c("EL_LAJI_ID",
                                                                "HAVAITSIJAN_ANTAMA_NIMI")])
all_names <- all_names[all_names$EL_LAJI_ID %in% observed_taxa$EL_LAJI_ID, ]

## Identify names that could not be matched directly to AA data
## Edits added to list manually for AA
## Columns other columns for manuscript added producing the file
## Species_lookup.csv
aa_names <- read.csv("../input/aa_observation_names.csv")
aa_names <- aa_names[!aa_names$x %in% all_names$NIMI, "x"]
aa_missing <- data.frame(HAVAITSIJAN_ANTAMA_NIMI_AA = aa_names,
                         EL_LAJI_ID = "",
                         NIMI = "",
                         ELIORYHMA = "",
                         SALATTAVA = "",
                         LAJI_URI = "")

all_names <- merge(aa_missing, all_names, all = TRUE)
write.csv2(all_names, row.names = FALSE, "../input/LG_AA_species_list.csv", fileEncoding = "ISO-8859-1")

