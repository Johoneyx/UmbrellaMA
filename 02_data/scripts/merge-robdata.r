library(readxl)
library(tidyverse)
library(writexl)
library(ggplot2)
library(dplyr)
library(purrr)
library(data.table)
library(readxl)
library(stringr)

# Read the data using fread from data.table package and skip the first rows as the real headers are in the second row
Rob_HP_S_R <- as.data.table(read_xlsx("C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/rawdata/Rob_Assesment_Riccardo_Johanna21.09.2023.xlsx", sheet=1,skip=1))

# Remove completely empty rows
Rob_HP_S_R <- Rob_HP_S_R[!apply(is.na(Rob_HP_S_R), 1, all), ]


View(Rob_HP_S_R)
# Shorten the variable names
names(Rob_HP_S_R) <- sub("\\..*$", "", names(Rob_HP_S_R))

View(Rob_HP_S_R)


#Problem: Riccardo put the different studies next to each other instead of under each other, grouped by studytype, I need to split the dataframes and merge them afterwards 

# Get the column names
col_names <- names(Rob_HP_S_R)

# Find the indices of the columns that start with "Review"
review_indices <- grep("^Review", col_names)

# Get the index of the second "Review" column
second_review_index <- review_indices[2]

# Split the dataframe into two
df1 <- Rob_HP_S_R[, 1:(second_review_index - 1)]
df2 <- Rob_HP_S_R[, second_review_index:ncol(Rob_HP_S_R)]

View(df1)
View(df2)




Rob_CHR_T_R <- as.data.table(read_xlsx("C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/rawdata/Rob_Assesment_Riccardo_Johanna21.09.2023.xlsx", sheet=4,skip = 1))

# Remove completely empty rows
Rob_CHR_T_R <- Rob_CHR_T_R [!apply(is.na(Rob_CHR_T_R ), 1, all), ]

# Shorten the variable names
names(Rob_CHR_T_R) <- sub("\\..*$", "", names(Rob_CHR_T_R))

Rob_P_R <- as.data.table(read_xlsx("C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/rawdata/Rob_Assesment_Riccardo_Johanna21.09.2023.xlsx", sheet=5,skip = 1))

# Remove completely empty rows
Rob_P_R <- Rob_P_R[!apply(is.na(Rob_P_R), 1, all), ]

# Shorten the variable names
names(Rob_P_R) <- sub("\\..*$", "", names(Rob_P_R))

# Get the column names
col_names <- names(Rob_P_R)

# Find the indices of the columns that start with "Review"
review_indices <- grep("^Review", col_names)


lapply(split_dfs, View)

#
Rob_P_C <- as.data.table(read_xlsx("C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/rawdata/New_excel_Carolina_09.10_20.05.2024.xlsx", sheet=3,skip = 1))

# Remove completely empty rows
Rob_P_C <- Rob_P_C [!apply(is.na(Rob_P_C ), 1, all), ]







