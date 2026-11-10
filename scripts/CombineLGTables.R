## Combine LajiGIS tables
## Louise Forsblom (Syke), Tytti Turkia (MH), Rasmus Boman (MH)

library(sf)
library(dplyr)

options(encoding = "ISO-8859-1")

## Species observations
LG_LAJIHAV_V <- st_read("../Meriaineistot_helmikuu_2024.gdb", layer = "LG_LAJIHAV_V")

lajihavainnot <- LG_LAJIHAV_V %>%
  select(LG_LAJIHAVID, HAVAITSIJAN_ANTAMA_NIMI,
         PEITTAVYYS_PROS, LAJIN_KESKIM_KORKEUS,
         LAJIN_KESKIM_KORKEUS, EL_LAJI_ID,
         HAVAINNON_LAATU, HAVAINNON_KUVAUS,
         HAVAITSIJA, LUOJA, EPIFYYTTI,
         MAARA, MAARAN_YKSIKKO,
         TARKISTUSTARVE)

## Combine with kertaIDhen
LG_LAJIHAV_TO_KERTA <- st_read("../Meriaineistot_helmikuu_2024.gdb", layer = "LG_LAJIHAV_TO_KERTA")
lajihavainnot <- merge(lajihavainnot, LG_LAJIHAV_TO_KERTA,
                       by = "LG_LAJIHAVID", all.x = TRUE)

## Depth from video data
LG_VIDEO <- st_read("../Meriaineistot_helmikuu_2024.gdb", layer = "LG_VIDEO")
video_syvyys <- LG_VIDEO %>%
  select(LG_KERTAID, SYVYYS_VIDEON_ALUSSA) %>%
  mutate(VIDEOKOHDE = 1)
lajihavainnot <- merge(lajihavainnot, video_syvyys,
                       by = "LG_KERTAID", all.x = TRUE)

## Substrate
LG_KERTA_VESI <- st_read("../Meriaineistot_helmikuu_2024.gdb", layer = "LG_KERTA_VESI")
pohjanlaadut <- LG_KERTA_VESI %>%
  select(LG_KERTAID, MERI_ID, KENTTAHENKILOT,
         SYVYYDEN_KORJAUS, KALLIO:PUU, PL_ARVIO_EPAVARMA)

lajihavainnot <- merge(lajihavainnot, pohjanlaadut,
                       by = "LG_KERTAID", all.x = TRUE)


## Unique visits
LG_KERTA <- st_read("../Meriaineistot_helmikuu_2024.gdb", layer = "LG_KERTA_v")
kerrat <- LG_KERTA %>%
  select(LG_KERTAID, LG_KOHDEID, EI_HAVAINTOJA, HUOMAUTUKSET,
         KARTOITUSKERTA, HAV_TLAHDE_KUVAUS, OLOTILA, PVM_ALKU)
kerrat <- kerrat[kerrat$OLOTILA == 1, ]

## Remove errors
errors <- read.csv("../input/Poistettavat_KAIKKI.csv")
kerrat <- kerrat[!kerrat$LG_KOHDEID %in% errors$KohdeID, ]

## Add sediment
## Link in LG_YMPTIEDOT2
LG_YMPT_ARVO <- st_read("../Meriaineistot_helmikuu_2024.gdb", layer = "LG_YMPT_ARVO")
LG_YMPTIEDOT_KEY <- read.csv2("../input/LG_YMPTIEDOT.csv")
ymp_yht <- merge(LG_YMPT_ARVO, LG_YMPTIEDOT_KEY,
                 by.x = "LG_YMPTIEDOTID",
                 by.y = "ï..OBJECTID")

ymp_yht <- ymp_yht[ , !names(ymp_yht) %in% c("LG_YMPTIEDOTID", "ARVO")]

ymp_yht <- ymp_yht[ymp_yht$KOODISTONUMERO %in% c("Sedimentin mÃ¤Ã¤rÃ¤, MH",
                                                 "Sedimentin mÃ¤Ã¤rÃ¤, Alleco"), ]

ymp_wide <- ymp_yht  %>% 
  tidyr::pivot_wider( names_from = "KOODISTONUMERO", # nimet Lajihavainto-nimiseen sarakkeeseen
                      values_from = "SELITE", # arvot taulukosta Peittavyys-nimiseen sarakkeeseen
                      values_fill = NA )

kerrat <- merge(kerrat, ymp_wide, by = "LG_KERTAID", all.x = TRUE)


## Coordinates
LG_KOHDE_P <- st_read("../Meriaineistot_helmikuu_2024.gdb", layer = "LG_KOHDE_P_20_21")
kohde_sf <- LG_KOHDE_P %>% select(LG_KOHDEID)

## Additional information
LG_KOHDE <- st_read("../Meriaineistot_helmikuu_2024.gdb", layer = "LG_KOHDE_20_21")
kohde_koords <- LG_KOHDE %>%
  select(LG_KOHDEID, KOHDE_TYYPPI, KOHDE_TASO,
         MENETELMA, RUNSAUSARVIOINNIN_MENETELMA,
         PINTA_ALAN_KOKOLK, ORIG_Y_KOORD, ORIG_X_KOORD,
         PAAKOHDE_ID)
kohde_koords <- kohde_koords[kohde_koords$KOHDE_TASO == 63, ]


## Project information
LG_HANKE <- st_read("../Meriaineistot_helmikuu_2024.gdb", layer = "LG_HANKE")
LG_HANKE <- LG_HANKE %>%
  select(HANKE_NIMI, LG_HANKEID)

LG_KERTA_TO_HANKE <- st_read("../Meriaineistot_helmikuu_2024.gdb", layer = "LG_KERTA_TO_HANKE")
LG_HANKE <- merge(LG_KERTA_TO_HANKE, LG_HANKE, by = "LG_HANKEID", all.x = TRUE)
LG_HANKE <- LG_HANKE %>%
  select(HANKE_NIMI, LG_KERTAID)

## Depth from dives
LG_KOHDE_VESI <- st_read("../Meriaineistot_helmikuu_2024.gdb", layer = "LG_KOHDE_VESI")
syvyys <- LG_KOHDE_VESI %>%
  select(LG_KOHDEID, RUUDUN_SYVYYS, MENETELMA_TARK_VESI, OTANTAMENETELMA)

## Combine tables
# Removes multiple coordinates for the same location 
kohde_sf <- kohde_sf[!kohde_sf$LG_KOHDEID %in% 358383, ]
kerrat <- kerrat[!kerrat$LG_KOHDEID %in% 358383, ]

kohde_sf <- kohde_sf[!kohde_sf$LG_KOHDEID %in% 375721, ]
kerrat <- kerrat[!kerrat$LG_KOHDEID %in% 375721, ]


kohde_kerta <- kohde_sf %>%
  left_join(kohde_koords, by = "LG_KOHDEID") %>%
  left_join(syvyys, by = "LG_KOHDEID")

kohde_kerta <- kerrat %>%
  inner_join(kohde_kerta, by = "LG_KOHDEID") %>%
  mutate(X_KOORD = sf::st_coordinates(SHAPE)[, 1], # Muokataan koordinaateista omat sarakkeet, muista taulukoista puuttui arvoja
         Y_KOORD = sf::st_coordinates(SHAPE)[, 2])

## Add species
full_sf <- kohde_kerta %>%
  left_join(lajihavainnot, by = "LG_KERTAID") %>%
  left_join(LG_HANKE, by = "LG_KERTAID") %>%
  mutate(RUUDUN_SYVYYS = case_when(VIDEOKOHDE == 1 ~ SYVYYS_VIDEON_ALUSSA,
                                   .default = RUUDUN_SYVYYS)) %>% 
  select(LG_KOHDEID, LG_KERTAID, HAVAITSIJAN_ANTAMA_NIMI,
         PEITTAVYYS_PROS, everything())# Järjestetään loogisemmin

# Drops geometries
LG_combined <- st_drop_geometry(full_sf)

## Remove lakes
LG_combined <- LG_combined[!(LG_combined$X_KOORD > 405727 &
                               LG_combined$X_KOORD < 676305 &
                               LG_combined$Y_KOORD > 6778780 &
                               LG_combined$Y_KOORD < 6976387), ]

## Make edits based on missing EL_LAJI_ID
LG_combined[is.na(LG_combined$HAVAITSIJAN_ANTAMA_NIMI) &
              LG_combined$EI_HAVAINTOJA %in% 1, "HAVAITSIJAN_ANTAMA_NIMI"] <- "Ei lajihavaintoa"
LG_combined[LG_combined$HAVAITSIJAN_ANTAMA_NIMI %in% "Ei lajihavaintoa", "EL_LAJI_ID"] <- 99999


## Remove trash
LG_combined <- LG_combined[LG_combined$HAVAITSIJAN_ANTAMA_NIMI != "Roska", ]

## Remove rows with missing EL_LAJI_ID
LG_combined <- LG_combined[!is.na(LG_combined$LG_KOHDEID), ]
LG_combined <- LG_combined[!is.na(LG_combined$EL_LAJI_ID), ]

# Tallennetaan Rdata-muodossa
save(LG_combined, file = paste0("../output/LG_combined_", format(Sys.Date(), "%Y%m"), ".RData"))
