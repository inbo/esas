#Source
source("./Scripts/ESAS functions.R")

#Read 4 ESAS tables:
ESAS_TABLES_LIST <- Read_ESAS_Tables(pathway = "./Data/ESAS download 2024 04 30",
                                     file_encoding = "UTF-8")

#Create an ESAS master-table:
ESAS_TABLE <- Create_ESAS_Table(esas_tables_list = ESAS_TABLES_LIST)


#Execute distance analysis on selection of species:
PROBABILITIES <- Detection_Probabilities_Ship_Based_Surveys(esas_table_2_analyse = ESAS_TABLE,
                                                            species_2_analyse = c(720,6020))

