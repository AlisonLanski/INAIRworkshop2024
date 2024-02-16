#######################################################
###
### Sample Processing of Completions Prep File
###
### "Solution" file by Alison Lanski
###
### Note:
###   There is more than one way to write this code
###
#######################################################

#####
###
## Step 0: Load the packages we'll need

library(readr)  #read data into R
library(dplyr)  #manipulate data


#####
###
## Step 1: Read in the data and check it out

#package to load the data


#you will need your to have your own personal path
dat <- read_csv("C:/Users/alanski/Downloads/SampleDataPrepCOM.csv")

#how is the data put together?

glimpse(dat)
#can see all column names on the left margin
#can see all datatypes in light italics
### dbl means a number, chr means text
# then get a data preview


#look at the first six rows
head(dat)


#ok, some idea of what's in here
#check the final data requirements at this link:
# https://alisonlanski.github.io/IPEDSuploadables/articles/setup_for_completions.html

#List of updates:
# add Unitid
# rename ID to StudentId
# recode RaceEthnicity
# compute Sex & GenderDetail from Gender
# compute DegreeLevel from Degree
# compute MajorCip from CipSubj and CipDtl
# compute DistanceEd31 and DistanceEd32 from Location
# remove non-graduated students

#####
###
## Step 2: Rename column

dat <- dat %>% rename(StudentId = ID)


#####
###
## Step 3: Remove extra rows

#first, look at the info by getting single-column value counts
dat %>% count(Graduated)
#ok, we need to keep the 1s and lose the 0s
#use == to produce True or False values: R will only keep the rows that are True
dat <- dat %>% filter(Graduated == 1)

#check it -- do we still have 45 rows?
dat %>% count(Graduated)


#####
###
## Step 4: Single Column Recode/Computes

#check values here vs at the link above (for requirements)
dat %>% count(RaceEthnicity)

#ok, we need Hisp/Lat to be 2, Multiple to be 8, White to be 7
#NA means "missing" so we need that to end up as Unknown 9

#adding new lines after punctuation to make it easier to read
#must add the line break AFTER %>% or , unless you like error messages

#it is safter to recode first into a new column, then delete/rename to fix the column names later
dat <- dat %>%
  mutate(RaceEthnicityUpdate = case_when(RaceEthnicity == 'Hisp/Lat' ~ 2,
                                   RaceEthnicity == 'White' ~ 7,
                                   RaceEthnicity == 'Multiple' ~ 8,
                                   is.na(RaceEthnicity) ~ 9,
                                   TRUE ~ 100)
         )
 #is.na() returns True if the cell is empty
 # the final TRUE ~ 100 will code any data not captured by the previous values as 100
 # I like to use a final condition like this to validate my data process
 
#check results -- before & after
dat %>% count(RaceEthnicity, RaceEthnicityUpdate)


#check values for the next one
dat %>% count(Degree)

dat <- dat %>% mutate(DegreeLevel = case_when(Degree == 'BA' ~ 5,
                                              Degree == 'MA' ~ 7,
                                              Degree == 'PhD' ~ 17,
                                              TRUE ~ 100))
#check results before & after
dat %>% count(Degree, DegreeLevel)


#keep going
dat %>% count(Gender)
#NB for non-binary, U for unknown

#for Sex, have to impute. OUr majority is F so we'll use F
#for Gender Detail, want to track the NB and Unknown
#also need to convert to the numeric codes on the website

dat <- dat %>% mutate(Sex = ifelse(Gender == 'M', 1, 2))
dat %>% count(Sex)

#then we want to be more specific in the detail column
dat <- dat %>% mutate(GenderDetail = case_when(Gender == 'NB' ~ 4,
                                               Gender == 'U' ~ 3,
                                               TRUE ~ Sex))

dat %>% count(Gender, Sex, GenderDetail)



#####
###
## Step 5: Two-column logic

dat %>% count(DistanceEd, Location)
#this one only works if you have the codebook 
#you need to know that DistanceEd is already using the required numbers
#you need to know that Location 1 means "mandatory on-site, nothing non-mandatory on site"

#to account for the IPEDS rules, we want our logic to check values in both columns
#it's also hard to assign a value of null (NA) within a case_when because it has to match datatype
#so we explicitly convert the type of the NA value
dat <- dat %>% 
  mutate(DistanceEd31 = case_when(DistanceEd == 3 & Location == 1 ~ 1,
                                  DistanceEd == 3 & Location == 3 ~ 1,
                                  DistanceEd == 3 ~ 0,
                                 TRUE ~ as.numeric(NA)),
         DistanceEd32 = case_when(DistanceEd == 3 & Location == 2 ~ 1,
                                  DistanceEd == 3 & Location == 3 ~ 1,
                                  DistanceEd == 3 ~ 0,
                                  TRUE ~ as.numeric(NA))
         )

dat %>% count(DistanceEd, DistanceEd31, DistanceEd32)


#check next
dat %>% count(CipSubj, CipDtl)

#need to combine with a . inbetween
dat <- dat %>%
  mutate(MajorCip = paste0(CipSubj, ".", CipDtl))

#check next
dat %>% count(CipSubj, CipDtl, MajorCip)
#we could use more complicated logic to add 0 in front of 9. but we don't need to
#the package can handle this format, according to the instructions


#####
###
## Step 6: Final Cleanup

#what's left?
glimpse(dat)

#need to add Unitid
#need to rename RaceEthnicityUpdate to RaceEthnicity
#can remove extra columns (not required)

final_dat <- dat %>%
  mutate(Unitid = 999999,
         OldRaceEthnicity = RaceEthnicity,
         RaceEthnicity = RaceEthnicityUpdate)

glimpse(final_dat)

#optional cleanup: only the columns we want, in the order we want them
final_dat <- final_dat %>% 
  select(Unitid, StudentId, RaceEthnicity, Sex, GenderDetail, DegreeLevel,
         MajorNumber, MajorCip, DistanceEd, DistanceEd31, DistanceEd32, Age)

glimpse(final_dat)


#####
###
## Step 7: Ready to produce the report

library(IPEDSuploadables)
produce_com_report(final_dat)

#The file produced by this code should match the file produced by your code.