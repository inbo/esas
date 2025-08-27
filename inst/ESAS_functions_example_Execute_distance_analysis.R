#Source
source("./Scripts/ESAS functions.R")

#Read 4 ESAS tables:
ESAS_TABLES_LIST <- Read_ESAS_Tables(path = "./Data/ESAS download 2024 04 30",
                                     file_encoding = "UTF-8")

#Create an ESAS master-table:
ESAS_TABLE <- Create_ESAS_Table(esas_tables_list = ESAS_TABLES_LIST)


#Execute distance analysis on selection of species:
PROBABILITIES <- Calculate_Detection_P_Ship_Based_Surveys(esas_table_2_analyse = ESAS_TABLE,
                                                          species_2_analyse = c(720,6020))

#check outcome of probabilities:
PROBABILITIES
TEST <- read.csv("./Output/ESAS_probabilities.csv")
TEST %>% filter(Species %in% c(720,6020))

#Create a cross-table with distance-corrected seabird densities
ESAS_DENSITIES <- Create_Seabird_Density_Cross_Table(esas_table = ESAS_TABLE,
                                                     probabilities = PROBABILITIES,
                                                     species_selection = c(720,6020))
head(ESAS_DENSITIES)
summary(ESAS_DENSITIES)

#check number of rows:
TEST1 <- ESAS_TABLE %>% filter(DistanceBins %in% c("0|50|100|200|300"), PlatformClass %in% c("30"), Area > 0)
length(unique(TEST1$PositionID))
length(unique(TEST1$PositionID)) == nrow(ESAS_DENSITIES)

#check outcome of densities:
BASE_TEST <- ESAS_TABLE %>% filter(
  DistanceBins %in% c("0|50|100|200|300"),
  PlatformClass %in% c(30),
  Area > 0,
  Transect %in% c("True"),
  !Behaviour %in% c(99))

FLYING <- BASE_TEST %>% filter(
  ObservationDistance %in% c("F"),
  SpeciesCode == 6020) %>%
  mutate(DENS = Count / Area)

SWIMMING <- BASE_TEST %>% filter(
  ObservationDistance %in% c("A","B","C","D"),
  SpeciesCode == 6020) %>%
  mutate(DENS = Count / (Area*0.54))

TEST2 <- rbind(FLYING, SWIMMING) %>%
  group_by(PositionID) %>%
  summarise(DENS = sum(DENS))

TEST2$DENS <- round(TEST2$DENS, digits = 2)

sum(ESAS_DENSITIES$`6020`)
sum(TEST2$DENS)
sum(ESAS_DENSITIES$`6020`) == sum(TEST2$DENS)

#graphical check
library(sf)
ESAS_DENSITIES_SF <- st_as_sf(ESAS_DENSITIES[ESAS_DENSITIES$Date > "2014-01-01",],
                              coords = c("Longitude","Latitude"),
                              crs = 4326)

BPNS <- st_read("./Shapes/BPNS_polygon.shp")
ggplot() + geom_sf(data = BPNS)
BPNS <- st_transform(BPNS, crs = 4326)

ESAS_DENSITIES_SF_BPNS <-  ESAS_DENSITIES_SF[BPNS,]

ggplot() +
  geom_sf(data = BPNS, fill = NA) +
  geom_sf(data = ESAS_DENSITIES_SF_BPNS, col = "grey", size = 1) +
  geom_sf(data = ESAS_DENSITIES_SF_BPNS %>% filter(`6020` > 0), col = "red3", aes(size = `6020`)) +
  labs(title = "Black-legged kittiwake", size = "Density (n/km²)")

ggplot() +
  geom_sf(data = BPNS, fill = NA) +
  geom_sf(data = ESAS_DENSITIES_SF_BPNS, col = "grey", size = 1) +
  geom_sf(data = ESAS_DENSITIES_SF_BPNS %>% filter(`720` > 0), col = "blue3", aes(size = `720`)) +
  labs(title = "Great cormorant", size = "Density (n/km²)")
