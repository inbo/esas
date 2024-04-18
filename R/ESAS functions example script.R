#Source
source("./Scripts/ESAS functions.R")

#read tables:
ESAS_tables <- Read_ESAS_tables(pathway = "./Data/", 
                                file_encoding = "UTF-8")
Campaigns <- ESAS_tables[[1]]
Samples <- ESAS_tables[[2]]
Positions <- ESAS_tables[[3]]
Observations <- ESAS_tables[[4]]
rm(ESAS_tables)

#convert tables:
ESAS_4_upload <- Convert_ESAS_tables_4_upload(campaigns_tbl = Campaigns, 
                                              samples_tbl = Samples, 
                                              positions_tbl = Positions, 
                                              observations_tbl = Observations, 
                                              data_provider = "202", 
                                              country = "BE")

#write upload table:
Export_ESAS_upload_matrix(table = ESAS_4_upload, 
                          pathway = "./Output/", 
                          export_name = "ESAS_4_upload", 
                          file_encoding = "UTF-8")

#test by comparing old and new matrix:
test1 <- read.csv("./Output/ESAS_4_upload_TEST.csv", sep = "\t", header = F)
test2 <- read.csv("./Output/ESAS_4_upload.csv", sep = "\t", header = F)

library(arsenal)
summary(comparedf(test1, test2))
rm(test1, test2)

#download all observations of Gavia immer from ICES DC ESAS database 
Observations_Gavia_immer <- Download_ESAS_data(species = 40)
