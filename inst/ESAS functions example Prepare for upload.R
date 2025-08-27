#Source
source("./Scripts/ESAS functions.R")

#Read 4 ESAS tables:
ESAS_TABLES_LIST <- Read_ESAS_Tables(path = "./Data/",
                                     file_encoding = "UTF-8")

#Convert tables to upload format:
ESAS_4_UPLOAD <- Transform_ESAS_Tables_4_Upload(campaigns_tbl = ESAS_TABLES_LIST$CAMPAIGNS,
                                              samples_tbl = ESAS_TABLES_LIST$SAMPLES,
                                              positions_tbl = ESAS_TABLES_LIST$POSITIONS,
                                              observations_tbl = ESAS_TABLES_LIST$OBSERVATIONS,
                                              data_provider = "202",
                                              country = "BE")

#Export upload table:
Export_ESAS_Upload_Matrix(table = ESAS_4_UPLOAD,
                          path = "./Output/",
                          export_name = "ESAS_4_upload",
                          file_encoding = "UTF-8")

#Test by comparing old and new matrix:
test1 <- read.csv("./Output/ESAS_INBO_2024_01_31.csv", sep = "\t", header = F)
test2 <- read.csv("./Output/ESAS_4_upload.csv", sep = "\t", header = F)

library(arsenal)
summary(comparedf(test1, test2))


