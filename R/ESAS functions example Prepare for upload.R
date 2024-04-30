#Source
source("./Scripts/ESAS functions.R")

#Read 4 ESAS tables:
ESAS_TABLES_LIST <- Read_ESAS_Tables(pathway = "./Data/", 
                                file_encoding = "UTF-8")

#Create ESAS master-table:
ESAS_TABLE <- Create_ESAS_Table(ESAS_TABLES_LIST)
str(ESAS_TABLES_LIST$OBSERVATIONS)

#Convert tables to upload format:
ESAS_4_UPLOAD <- Convert_ESAS_Tables_4_Upload(campaigns_tbl = ESAS_TABLES_LIST$CAMPAIGNS, 
                                              samples_tbl = ESAS_TABLES_LIST$SAMPLES, 
                                              positions_tbl = ESAS_TABLES_LIST$POSITIONS, 
                                              observations_tbl = ESAS_TABLES_LIST$OBSERVATIONS, 
                                              data_provider = "202", 
                                              country = "BE")

#write upload table:
Export_ESAS_Upload_Matrix(table = ESAS_4_UPLOAD, 
                          pathway = "./Output/", 
                          export_name = "ESAS_4_upload", 
                          file_encoding = "UTF-8")

#test by comparing old and new matrix:
test1 <- read.csv("./Output/ESAS_INBO_2024_01_31.csv", sep = "\t", header = F)
test2 <- read.csv("./Output/ESAS_4_upload.csv", sep = "\t", header = F)

library(arsenal)
summary(comparedf(test1, test2))
rm(test1, test2)

# #download all observations of Gavia immer from ICES DC ESAS database 
# Observations_Gavia_immer <- Download_ESAS_data(species = 40)
