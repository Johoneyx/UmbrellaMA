# Create df_studylist
library(ggplot)

emap_studylist_cohort <- as.data.frame(read.xlsx("02_data/cleandata/df_studylist_cohort.xlsx"))


table(emap_studylist_cohort$studydesign)

typecount <- aggregate(population ~  )

barplot(table(emap_studylist_cohort$studydesign))


# Create a bar plot of the population variable
ggplot(emap_studylist_cohort, aes(x = population)) +
  geom_bar() +
  theme_minimal() +
  labs(x = "Population", y = "Count", title = "Count of each level of the population variable")

  table(emap_studylist_cohort$population)

  barplot(table(emap_studylist_cohort$population))

library(qreport)



