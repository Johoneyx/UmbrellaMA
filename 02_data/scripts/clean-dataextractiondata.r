

merged_df$firstauthor <- ifelse(is.na(str_extract(merged_df$author, ".*(?= \\()")), merged_df$authoronly, str_extract(merged_df$author, ".*(?= \\()"))


# Replace "L.Clausen" with "Clausen"
merged_df$firstauthor <- sub("L.Clausen", "Clausen", merged_df$firstauthor)

# Replace "Jennifer L. Seddon" with "Seddon"
merged_df$firstauthor <- sub("Jennifer L. Seddon", "Seddon", merged_df$firstauthor)


library(dplyr)


merged_df$firstauthor

merged_df$publicationyear



merged_df$publicationyear <- ifelse(is.na(merged_df$year), 
                                    str_extract(merged_df$author, "\\((\\d+)\\)"), 
                                    merged_df$year)


# Remove all brackets
merged_df$publicationyear <- gsub("\\(|\\)", "", merged_df$publicationyear)


# Define the new names for the values you want to rename
new_names <- c("ferdindand"="ferdinand", "bergé(2016)" = "bergé", "shimmelmann" = "schimmelmann", "j.m.stone"   =" stone", "r. emsley"= "emsley",   "kristinerømerthomsen"=  "romerthomsen", "isaacetal" ="isaac", "vandijk" ="vandjik", "gonzalez-into" ="gonzalez-pinto")

# Rename the values
merged_df$firstauthor <- recode(merged_df$firstauthor, !!!new_names)

merged_df$publicationyear 




merged_df$pop <- str_extract(merged_df$dt_name, "^[^_]*")

merged_df$pop

#clean whitespaces
merged_df$firstauthor <- gsub("\\s", "", merged_df$firstauthor)
merged_df$publicationyear <- gsub("\\s", "", merged_df$publicationyear)

merged_df$key <- paste(merged_df$firstauthor, merged_df$publicationyear, merged_df$pop, sep = "_")

#clean whitespaces
merged_df$firstauthor <- gsub("\\s", "", merged_df$firstauthor)
merged_df$publicationyear <- gsub("\\s", "", merged_df$publicationyear)


#rename 
new_names <- c( "ferdindand"="ferdinand", "bergé(2016)" = "bergé", "shimmelmann" = "schimmelmann", "j.m.stone"   =" stone", "r. emsley"= "emsley",   "kristinerømerthomsen"=  "romerthomsen", "isaacetal" ="isaac", "vandijk" ="vandjik", "gonzalez-into" ="gonzalez-pinto")

keys_unique <- unique(merged_df$key)




# Write to an Excel file
# Write the dataframe to an Excel file
write.xlsx(merged_df, "merged_df.xlsx")
write_xlsx(keys_unique_df, "studylist_extracteddata.xlsx")
































































































