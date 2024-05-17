library(readxl)
library(tidyverse)
library(writexl)
library(ggplot2)
library(dplyr)
library(purrr)

HP_S_KD <- as.data.frame(read_xlsx("C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/rawdata/Umbrella_MA_23.09.2023.xlsx", sheet=1))

HP_D_KR <- as.data.frame(read_xlsx("C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/rawdata/Umbrella_MA_23.09.2023.xlsx", sheet =2))

CHR_T_KD <- as.data.frame(read_xlsx("C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/rawdata/Umbrella_MA_23.09.2023.xlsx", sheet =3))

CHR_S_KR <-as.data.frame(read_xlsx("C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/rawdata/Umbrella_MA_23.09.2023.xlsx", sheet =4))

CHR_S_KD <-as.data.frame(read_xlsx("C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/rawdata/Umbrella_MA_23.09.2023.xlsx", sheet =5))

CHR_AR <-as.data.frame(read_xlsx("C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/rawdata/Umbrella_MA_23.09.2023.xlsx", sheet =6))

P_J <-as.data.frame(read_xlsx("C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/rawdata/Umbrella_MA_23.09.2023.xlsx", sheet =7))
HP_J <-as.data.frame(read_xlsx("C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/rawdata/Umbrella_MA_23.09.2023.xlsx", sheet =8))

CHR_J <-as.data.frame(read_xlsx("C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/rawdata/Umbrella_MA_23.09.2023.xlsx", sheet=9))

P_M <-as.data.frame(read_xlsx("C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/rawdata/Umbrella_MA_23.09.2023.xlsx", sheet=10))

P_CJ <-as.data.frame(read_xlsx("02_data/rawdata/Dataextraction&Rob_Carolina_04.10.2023_JMG_05.10.2023.xlsx", sheet =1))

P_KC <-as.data.frame(read_xlsx("02_data/rawdata/Dataextraction&Rob_Carolina_04.10.2023_JMG_05.10.2023.xlsx", sheet =2))



# Create a list of all dataframes
df_list <- list(HP_S_KD, HP_D_KR, CHR_T_KD, CHR_S_KR, CHR_S_KD, CHR_AR, P_J, HP_J, CHR_J, P_M, P_CJ, P_KC)

# Convert all variables in all dataframes to character type
df_list <- lapply(df_list, function(df) mutate_all(df, as.character))

df_list <- lapply(df_list, function(df) {
  names(df) <- tolower(names(df))
  return(df)
})

# Use reduce and full_join to merge all dataframes
merged_df <- reduce(df_list, full_join, by = NULL)

view(merged_df)

names(merged_df)
str(merged_df)