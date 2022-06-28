rm(list=ls())

setwd("R:/bellih01lab/home/JanikaPeyasena")

Data <- read.csv("HTN_ALTA_Project_BP_Control_TY_Nov_2020_w_PDC_Scores_20210305.csv")

Data<- na.omit(Data)

Data <- Data[,c("BP_DIASTOLIC","BP_SYSTOLIC","PRIMARY_CARE_YN","CURRENT_AGE","CALC_PDC_SCORE","RACE","ETHNIC_BACKGROUND","QUALIFYING_ENCOUNTER_LOCATION")]

#Removing zero values
Data <- subset(Data,Data$CALC_PDC_SCORE >=1 & Data$CALC_PDC_SCORE <101)

#Yes=1 and No =0 in Primary care column
Data$PRIMARY_CARE_YN<-ifelse(Data$PRIMARY_CARE_YN=="Y",1,0)

#Removing extra values
Data$RACE = gsub(";.*","", Data$RACE)

Data$RACE = gsub("-.*","", Data$RACE)

Data$ETHNIC_BACKGROUND = gsub(";.*","", Data$ETHNIC_BACKGROUND)

Data$ETHNIC_BACKGROUND = gsub("\\?.*","", Data$ETHNIC_BACKGROUND)

Data$ETHNIC_BACKGROUND = gsub("\\,.*","", Data$ETHNIC_BACKGROUND)


#Replacing variables in Race
Data$RACE[Data$RACE=="Patient Refused"] <-"Unknown"
Data$RACE[Data$RACE=="unknown"] <-"Unknown"
Data$RACE[Data$RACE=="Other Race"] <-"Unknown"
Data$RACE[Data$RACE==""] <-"Unknown"

Data$RACE[Data$RACE=="Asian "] <-"Asian"
Data$RACE[Data$RACE=="Vietnamese"] <-"Asian"
Data$RACE[Data$RACE=="Thai"] <-"Asian"
Data$RACE[Data$RACE=="Sri Lankan"] <-"Asian"
Data$RACE[Data$RACE=="Pakistani"] <-"Asian"
Data$RACE[Data$RACE=="Korean"] <-"Asian"
Data$RACE[Data$RACE=="Asian Indian"] <-"Asian"
Data$RACE[Data$RACE=="Japanese"] <-"Asian"
Data$RACE[Data$RACE=="Indonesian"] <-"Asian"
Data$RACE[Data$RACE=="Filipino"] <-"Asian"
Data$RACE[Data$RACE=="Chinese"] <-"Asian"
Data$RACE[Data$RACE=="Bangladeshi"] <-"Asian"
Data$RACE[Data$RACE=="Laotian"] <-"Asian"
Data$RACE[Data$RACE=="Guamanian or Chamorro"] <-"Asian"

Data$RACE[Data$RACE=="Other Pacific Islander"] <-"Native Hawaiian and Other Pacific Islander"
Data$RACE[Data$RACE=="Native Hawaiian"] <-"Native Hawaiian and Other Pacific Islander"
Data$RACE[Data$RACE=="Native Hawaiian"] <-"Native Hawaiian and Other Pacific Islander"

Data$RACE[Data$RACE=="Native American (American Indian/Eskimo/Aleutian)"] <-"American Indian and Alaska Native"

Data$RACE[Data$RACE=="Cambodian"] <-"African American (Black)"

Race_split <- split(Data, Data$RACE)

#Replacing Ethnic values
Data$ETHNIC_BACKGROUND[Data$ETHNIC_BACKGROUND=="Costa Rican"]<-"Hispanic or latino"
Data$ETHNIC_BACKGROUND[Data$ETHNIC_BACKGROUND=="Cuban"]<-"Hispanic or latino"
Data$ETHNIC_BACKGROUND[Data$ETHNIC_BACKGROUND=="Guatemalan"]<-"Hispanic or latino"
Data$ETHNIC_BACKGROUND[Data$ETHNIC_BACKGROUND=="Honduran"]<-"Hispanic or latino"
Data$ETHNIC_BACKGROUND[Data$ETHNIC_BACKGROUND=="Dominican"]<-"Hispanic or latino"
Data$ETHNIC_BACKGROUND[Data$ETHNIC_BACKGROUND=="Mexican"]<-"Hispanic or latino"
Data$ETHNIC_BACKGROUND[Data$ETHNIC_BACKGROUND=="Salvadoran"]<-"Hispanic or latino"
Data$ETHNIC_BACKGROUND[Data$ETHNIC_BACKGROUND=="Spaniard"]<-"Hispanic or latino "
Data$ETHNIC_BACKGROUND[Data$ETHNIC_BACKGROUND=="Puerto Rican"]<-"Hispanic or latino"
Data$ETHNIC_BACKGROUND[Data$ETHNIC_BACKGROUND=="South American"]<-"Hispanic or latino"
Data$ETHNIC_BACKGROUND[Data$ETHNIC_BACKGROUND=="Panamanian"]<-"Hispanic or latino"
Data$ETHNIC_BACKGROUND[Data$ETHNIC_BACKGROUND=="Nicaraguan"]<-"Hispanic or latino"
Data$ETHNIC_BACKGROUND[Data$ETHNIC_BACKGROUND=="Hispanic/Latino "]<-"Hispanic or latino"
Data$ETHNIC_BACKGROUND[Data$ETHNIC_BACKGROUND=="Hispanic"]<-"Hispanic or latino"

Data$ETHNIC_BACKGROUND[Data$ETHNIC_BACKGROUND==""]<-"Unknown"
Data$ETHNIC_BACKGROUND[Data$ETHNIC_BACKGROUND=="Patient Refused"]<-"Unknown"

Data$ETHNIC_BACKGROUND[Data$ETHNIC_BACKGROUND=="Not of Spanish/Hispanic Origin"]<-"Not Hispanic or latino"

Ethnic_split <- split(Data, Data$ETHNIC_BACKGROUND)

install.packages("GGally")
library("GGally")

ggpairs(data=Data, columns=1:8, title="")

#Ethnicity + Age
Ethnicity_Age <-aov(Data$CALC_PDC_SCORE ~ Data$ETHNIC_BACKGROUND*Data$CURRENT_AGE, data = Data)
summary(Ethnicity_Age)

#Ethnicity + Location
Ethnicity_Location <- aov(Data$CALC_PDC_SCORE ~ Data$QUALIFYING_ENCOUNTER_LOCATION*Data$ETHNIC_BACKGROUND, data = Data)
summary(Ethnicity_Location)
  
#Ethnicity+ Race ***
Ethnicity_Race <- aov(Data$CALC_PDC_SCORE ~ Data$ETHNIC_BACKGROUND*Data$RACE, data = Data)
summary(Ethnicity_Race)
  
#Race + Age *
Race_Age<-aov(Data$CALC_PDC_SCORE ~ Data$RACE*Data$CURRENT_AGE, data = Data)
summary(Race_Age)


#Race + Location 
Race_Location <-aov(Data$CALC_PDC_SCORE ~ Data$RACE*Data$QUALIFYING_ENCOUNTER_LOCATION, data = Data)
summary(Race_Location)
 
#Location + Age ****
Location_Age <-aov(Data$CALC_PDC_SCORE ~ Data$QUALIFYING_ENCOUNTER_LOCATION*Data$CURRENT_AGE, data = Data)
summary(Location_Age)



  