rm(list=ls())

setwd("R:/bellih01lab/home/JanikaPeyasena")

Data <- read.csv("HTN_ALTA_Project_BP_Control_TY_Nov_2020_w_PDC_Scores_20210305.csv")

Location <- Data[,c("QUALIFYING_ENCOUNTER_LOCATION","CALC_PDC_SCORE")]

#Removing missing and 0 values
Location <- na.omit(Location)
Location <- Location %>% filter(Location$CALC_PDC_SCORE !=0)

Location$QUALIFYING_ENCOUNTER_LOCATION=tolower(Location$QUALIFYING_ENCOUNTER_LOCATION)

Location_split<- split(Location,Location$QUALIFYING_ENCOUNTER_LOCATION)

#Each Location 

Adult_Medicine <- subset(Location, Location$QUALIFYING_ENCOUNTER_LOCATION=="family health centers at nyu langone - adult medicine")

Family_physician <- subset(Location, Location$QUALIFYING_ENCOUNTER_LOCATION=="family health centers at nyu langone - family physician")

Flatbush <-subset(Location, Location$QUALIFYING_ENCOUNTER_LOCATION=="family health centers at nyu langone - flatbush")

Park_Ridge <- subset(Location, Location$QUALIFYING_ENCOUNTER_LOCATION=="family health centers at nyu langone - park ridge")

Park_Slope <- subset(Location, Location$QUALIFYING_ENCOUNTER_LOCATION=="family health centers at nyu langone - park slope")

Sunset_Terrace <- subset(Location, Location$QUALIFYING_ENCOUNTER_LOCATION=="family health centers at nyu langone - sunset terrace medical")

Huntington <-subset(Location, Location$QUALIFYING_ENCOUNTER_LOCATION=="nyu langone huntington medical group")

Primary_care <- subset(Location, Location$QUALIFYING_ENCOUNTER_LOCATION=="nyu winthrop primary care")

#Boxplot with all of the Loaction
boxplot(Adult_Medicine$CALC_PDC_SCORE, Family_physician$CALC_PDC_SCORE,Flatbush$CALC_PDC_SCORE, Park_Ridge$CALC_PDC_SCORE, Park_Slope$CALC_PDC_SCORE,Sunset_Terrace$CALC_PDC_SCORE, Huntington$CALC_PDC_SCORE, Primary_care$CALC_PDC_SCORE , horizontal = TRUE, col = blues9, names =c("1", "2", "3","4", "5", "6", "7", "8"), xlab="PDC Score", ylab="Location", main= "Location vs PDC Score")