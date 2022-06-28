rm(list=ls())

setwd("R:/bellih01lab/home/JanikaPeyasena")

Data <- read.csv("HTN_ALTA_Project_BP_Control_TY_Nov_2020_w_PDC_Scores_20210305.csv")

#Data with city
Race <- Data[,c("RACE","CALC_PDC_SCORE")]

Race<-na.omit(Race)

install.packages("dplyr")
library(dplyr)

#Removing zero 

Race <- Race %>% filter(Race$CALC_PDC_SCORE !=0)

Race$RACE=tolower(Race$RACE)

Race_split<- split(Race, Race$RACE)


# only including the first race
Race$RACE = gsub(";.*","", Race$RACE)

#African American
Black_or_African_American_1 <- Race %>%
  filter(grepl('black', RACE))

Black_or_African_American_2 <- Race %>%
  filter(grepl('cambodian', RACE))

#Includes all african american patients
Black_or_African_American <- rbind(Black_or_African_American_1, Black_or_African_American_2)

#white
white <- Race %>%
  filter(grepl('white', RACE))


Native_Hawaiian_and_Other_Pacific_Islander <-Race %>%
  filter(grepl('other', RACE))

American_Indian_and_Alaska_Native <-Race %>%
  filter(grepl('native', RACE))

#Subset Of Unkown 
Unknown_1 <- Race %>%
  filter(grepl('unknown', RACE))

Unknown_2 <- Race %>%
  filter(grepl('refused', RACE))

#Includes all unknown patients
Unknown <- rbind(Unknown_1, Unknown_2)

#Subset of asian
Asian_1 <- Race %>%
  filter(grepl('asian', RACE))

Asian_2 <- Race %>%
  filter(grepl('bangladeshi', RACE))

Asian_3 <- Race %>%
  filter(grepl('chinese', RACE))

Asian_4 <- Race %>%
  filter(grepl('filipino', RACE))

Asian_5 <- Race %>%
  filter(grepl('guamanian', RACE))

Asian_6 <- Race %>%
  filter(grepl('indonesian', RACE))

Asian_7 <- Race %>%
  filter(grepl('japanese', RACE))

Asian_8 <- Race %>%
  filter(grepl('korean', RACE))

Asian_9 <- Race %>%
  filter(grepl('laotian', RACE))

Asian_10 <- Race %>%
  filter(grepl('pakistani', RACE))

Asian_11 <- Race %>%
  filter(grepl('sri lankan', RACE))

Asian_12 <- Race %>%
  filter(grepl('thai', RACE))

Asian_13 <- Race %>%
  filter(grepl('vietnamese', RACE))

# All Asian Groups Combined
Asian <- rbind(Asian_1,Asian_2,Asian_3,Asian_4,Asian_5,Asian_6,Asian_7,Asian_8,Asian_9,Asian_10,Asian_11,Asian_12,Asian_13)

#Boxplot
boxplot(Asian$CALC_PDC_SCORE, Unknown$CALC_PDC_SCORE, white$CALC_PDC_SCORE, Native_Hawaiian_and_Other_Pacific_Islander$CALC_PDC_SCORE,Black_or_African_American$CALC_PDC_SCORE, horizontal = TRUE, col = blues9, names =c("A", "U", "W", "NH", "B"), xlab="PDC Score", ylab="Race", main= "Race vs PDC Score")