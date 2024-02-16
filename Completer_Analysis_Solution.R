## 
## R script to read in a file and create some visualizations
##


## tidyverse library allows easy wrangling of data
library(tidyverse)

## read csv file into memory
completers <- read.csv('SampleDataCOM.csv')

## Recode Sex into Men and Women so the data are easier to read when graphing

completers$Gender <- recode(as.character(completers$Sex), `1` = "Men", `2` = "Women")

## Simple bar graph of new variable 'Gender'

ggplot(data = completers) +
  geom_bar(mapping = aes(x = Gender)
           )

## Adding some color to the graph using 'fill' for contrast.

ggplot(data = completers) +
  geom_bar(mapping = aes(x = Gender, fill = Gender)
        )

## Recoding RaceEthnicity to definitions for easier analysis

completers$Ethnicity <- recode(as.character(completers$RaceEthnicity), 
                    '1' = "Noresident allien",
                    '2' = "Hispanic/Latino",
                    '3' = "Americian Indian or Alaska Native",
                    '4' = "Asian",
                    '5' = "Black or African American",
                    '6' = "Native Hawaiian or Other Pacific Islander",
                    '7' = "White",
                    '8' = "Two or more races",
                    '9' = "Race and ethnicity unknown")

## Plotting Race/Ethnicity
ggplot(data = completers) +
  geom_bar(mapping = aes(x = Ethnicity, fill = Ethnicity))

## Need to remove labels since they overlap and we have the legend at the right
ggplot(data = completers) +
  geom_bar(mapping = aes(x = Ethnicity, fill = Ethnicity)) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

## Plotting a stacked bar chart of race/ethnicity by gender
ggplot(data = completers) +
  geom_bar(mapping = aes(x = Gender, fill = Ethnicity)
  )

## Recode degree type into descriptions for analysis
completers$Degree_type <- recode(as.character(completers$DegreeLevel), 
                                 '2' = "Diploma", 
                                 '3' = "Associate", 
                                 '5' = "Bacehlor",
                                 '6' = "Post Bach Cert",
                                 '7' = "Masters",
                                 '17' = "Doc-Research",
                                 '18' = "Doc-Prof",
                                 '19' = "Doc-Other")

## Find the average number of earned number of credits by degree type
time_to_degree <- completers %>%
  group_by(Degree_type) %>%
  summarize(
    Avg_Credits = mean(CreditsEarned)
  )

## Plot results
ggplot(data = time_to_degree,
       mapping = aes(Degree_type, Avg_Credits)) +
  geom_point()

## Use geom_col to create a bar chart and have the height represent the value of y as 
## opposed to count of x

ggplot(data = time_to_degree)+
  geom_col(mapping = aes(Degree_type, Avg_Credits))

## Let's make it a little more interesting and add some color using "fill". Notice what happens to 
## the labels on the x axis. They overlap because of the legend added to the side.
ggplot(data = time_to_degree)+
  geom_col(mapping = aes(Degree_type, Avg_Credits, fill = Degree_type))

## Now remove the labels across the bottom to clean it up.
ggplot(data = time_to_degree)+
  geom_col(mapping = aes(Degree_type, Avg_Credits, fill = Degree_type)) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

