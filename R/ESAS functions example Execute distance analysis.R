#Source
source("./Scripts/ESAS functions.R")

#Read 4 ESAS tables:
ESAS_TABLES_LIST <- Read_ESAS_Tables(pathway = "./Data/ESAS download 2024 04 30",
                                     file_encoding = "UTF-8")

#Create an ESAS master-table:
ESAS_TABLE <- Create_ESAS_Table(esas_tables_list = ESAS_TABLES_LIST)


#Execute distance analysis on selection of species:
PROBABILITIES <- Calculate_Detection_P_Ship_Based_Surveys(esas_table_2_analyse = ESAS_TABLE,
                                                                        species_2_analyse = c(720,6020))

PROBABILITIES
TEST <- read.csv("./Output/ESAS_probabilities.csv")
TEST %>% filter(Species %in% c(720,6020))

#Create a cross-table with distance-corrected seabird densities
ESAS_DENSITIES <- Create_Seabird_Density_Cross_Table(esas_table = ESAS_TABLE, 
                                                     probabilities = PROBABILITIES, 
                                                     species_selection = c(720,6020))
head(ESAS_DENSITIES)
summary(ESAS_DENSITIES)

#test:
test <- ESAS_TABLE %>% filter(DistanceBins == "0|50|100|200|300", PlatformClass == 30)
length(unique(test$PositionID))
