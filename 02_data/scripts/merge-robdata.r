library(readxl)
library(tidyverse)
library(writexl)
library(ggplot2)
library(dplyr)
library(purrr)
library(data.table)
library(readxl)
library(stringr)

#Problem: Riccardo put the different studies next to each other instead of under each other thats why i only read in subframes so that i can merge them afterwards

#riccardo rob cohort data healthy population symptoms
Rob_HP_S_R_Cohort <- as.data.table(read_xlsx("C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/rawdata/Rob_Assesment_Riccardo_Johanna21.09.2023.xlsx", sheet=1, range=cell_cols("A:L")))

# Remove completely empty rows
Rob_HP_S_R_Cohort <- Rob_HP_S_R_Cohort [!apply(is.na(Rob_HP_S_R_Cohort), 1, all), ]

View(Rob_HP_S_R_Cohort)


#riccardo rob cross-sectional data healthy population symptoms
Rob_HP_S_R_CS<- as.data.table(read_xlsx("C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/rawdata/Rob_Assesment_Riccardo_Johanna21.09.2023.xlsx", sheet=1, range=cell_cols("N:X")))

# Remove completely empty rows
Rob_HP_S_R_CS <- Rob_HP_S_R_CS [!apply(is.na(Rob_HP_S_R_CS), 1, all), ]

View(Rob_HP_S_R_CS)

#riccardo rob cohort data healthy population diagnosis
Rob_HP_D_R_Cohort <- as.data.table(read_xlsx("C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/rawdata/Rob_Assesment_Riccardo_Johanna21.09.2023.xlsx", sheet=2, range=cell_cols("A:L")))


Rob_HP_D_R_Cohort <- Rob_HP_D_R_Cohort [!apply(is.na(Rob_HP_D_R_Cohort), 1, all), ]

View(Rob_HP_D_R_Cohort)

#riccardo rob cross-sectional data healthy population diagnosis
Rob_HP_D_R_CS<- as.data.table(read_xlsx("C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/rawdata/Rob_Assesment_Riccardo_Johanna21.09.2023.xlsx", sheet=2, range=cell_cols("N:X")))

# Remove completely empty rows
Rob_HP_D_R_CS <- Rob_HP_D_R_CS [!apply(is.na(Rob_HP_D_R_CS), 1, all), ]

View(Rob_HP_D_R_CS)


#riccardo rob cohort data CHR population symptoms

Rob_CHR_S_R_Cohort <- as.data.table(read_xlsx("C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/rawdata/Rob_Assesment_Riccardo_Johanna21.09.2023.xlsx", sheet=3,range=cell_cols("A:L")))

# Remove completely empty rows
Rob_CHR_S_R_Cohort <- Rob_CHR_S_R_Cohort [!apply(is.na(Rob_CHR_S_R_Cohort ), 1, all), ]

View(Rob_CHR_S_R_Cohort)

#riccardo rob case-control data CHR population transition 

Rob_CHR_S_R_CS <- as.data.table(read_xlsx("C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/rawdata/Rob_Assesment_Riccardo_Johanna21.09.2023.xlsx", sheet=3, range=cell_cols("N:X")))

# Remove completely empty rows
Rob_CHR_S_R_CS  <- Rob_CHR_S_R_CS  [!apply(is.na(Rob_CHR_S_R_CS  ), 1, all), ]

View(Rob_CHR_S_R_CS)
#riccardo rob case-control data CHR population transition 

Rob_CHR_S_R_CC <- as.data.table(read_xlsx("C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/rawdata/Rob_Assesment_Riccardo_Johanna21.09.2023.xlsx", sheet=3, range=cell_cols("Z:AK")))

# Remove completely empty rows
Rob_CHR_S_R_CC  <- Rob_CHR_S_R_CC  [!apply(is.na(Rob_CHR_S_R_CC ), 1, all), ]

View(Rob_CHR_S_R_CC)

#riccardo rob cohort data CHR population transition 

Rob_CHR_T_R_Cohort <- as.data.table(read_xlsx("C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/rawdata/Rob_Assesment_Riccardo_Johanna21.09.2023.xlsx", sheet=4,range=cell_cols("A:L") ))

# Remove completely empty rows
Rob_CHR_T_R_Cohort <- Rob_CHR_T_R_Cohort [!apply(is.na(Rob_CHR_T_R_Cohort), 1, all), ]

View(Rob_CHR_T_R_Cohort)

#riccardo rob case-control data CHR population transition 

Rob_CHR_T_R_CC <- as.data.table(read_xlsx("C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/rawdata/Rob_Assesment_Riccardo_Johanna21.09.2023.xlsx", sheet=4,range=cell_cols("N:Y")))

# Remove completely empty rows
Rob_CHR_T_R_CC <- Rob_CHR_T_R_CC [!apply(is.na(Rob_CHR_T_R_CC), 1, all), ]

View(Rob_CHR_T_R_CC)

#rob riccardo psychosis population cross-sectional
Rob_P_R_CS <- as.data.table(read_xlsx("C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/rawdata/Rob_Assesment_Riccardo_Johanna21.09.2023.xlsx", sheet=5,range=cell_cols("A:K")))

# Remove completely empty rows
Rob_P_R_CS <- Rob_P_R_CS[!apply(is.na(Rob_P_R_CS), 1, all), ]

View(Rob_P_R_CS)

#rob riccardo psychosis population case-control
Rob_P_R_CC <- as.data.table(read_xlsx("C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/rawdata/Rob_Assesment_Riccardo_Johanna21.09.2023.xlsx", sheet=5,range=cell_cols("M:X")))

# Remove completely empty rows
Rob_P_R_CC <- Rob_P_R_CC[!apply(is.na(Rob_P_R_CC), 1, all), ]


View(Rob_P_R_CC)

#Rob Riccardo psychosis population cohort
Rob_P_R_C <- as.data.table(read_xlsx("C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/rawdata/Rob_Assesment_Riccardo_Johanna21.09.2023.xlsx", sheet=5,range=cell_cols("AA:AK")))

# Remove completely empty rows
Rob_P_R_C <- Rob_P_R_C[!apply(is.na(Rob_P_R_C), 1, all), ]


#Rob carolina psychosis
Rob_P_C <- as.data.table(read_xlsx("C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/rawdata/New_excel_Carolina_09.10_20.05.2024.xlsx", sheet=3,skip=1,))

View(Rob_P_C)

Rob_P_C <- Rob_P_C %>%
rename(Primary = "Study type")

dt_list_rob <- list(Rob_HP_S_R_CS,Rob_HP_S_R_Cohort, Rob_HP_D_R_CS, Rob_HP_D_R_Cohort,Rob_CHR_T_R_Cohort, Rob_CHR_T_R_CC, Rob_CHR_S_R_CS, Rob_P_R_CC, Rob_P_R_CS, Rob_P_C, Rob_P_R_C)

# Merge all data tables
merged_dt_rob <- rbindlist(dt_list_rob, fill = TRUE)

# View the merged data table
View(merged_dt_rob)

write_xlsx(merged_dt_rob, "C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/mergeddata/merged_dt_rob.xlsx")

