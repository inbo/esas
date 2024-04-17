#Example data available at: 
https://drive.google.com/drive/folders/1mX26tLMC9qZpAIOeAMsS9ng5wPsL1_vH?usp=drive_link

#read tables:
Campaigns <- Read_ESAS_table(unique_part_of_table_name = "camp", pathway = "./Data/", fileEnc = "UTF-8")
Samples <- Read_ESAS_table(unique_part_of_table_name = "samp", pathway = "./Data/", fileEnc = "UTF-8")
Positions <- Read_ESAS_table(unique_part_of_table_name = "pos", pathway = "./Data/", fileEnc = "UTF-8")
Observations <- Read_ESAS_table(unique_part_of_table_name = "obs", pathway = "./Data/", fileEnc = "UTF-8")

#convert tables:
ESAS_4_upload <- Convert_ESAS_tables_4_upload(CampaignsTable = Campaigns, 
                                              SamplesTable = Samples, 
                                              PositionsTable = Positions, 
                                              ObservationsTable = Observations, 
                                              Data_provider = "202", 
                                              Country = "BE")

#write upload table:
Export_ESAS_upload_matrix(table = ESAS_4_upload, 
                          pathway = "./Output/", 
                          export_name = "ESAS_4_upload", 
                          fileEnc = "UTF-8")

#test by comparing old and new matrix:
test1 <- read.csv("./Output/ESAS_4_upload_TEST_functions.csv",
                 sep = "\t", header = F)

test2 <- read.csv("./Output/ESAS_4_upload.csv",
                  sep = "\t", header = F)
library(arsenal)
summary(comparedf(test1, test2))

#download observations of Gavia immer from ICES DC ESAS database 
Observations_Gavia_immer <- Download_ESAS_data(Species=40)
