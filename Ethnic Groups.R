rm(list=ls())

setwd("R:/bellih01lab/home/JanikaPeyasena")

Data <- read.csv("HTN_ALTA_Project_BP_Control_TY_Nov_2020_w_PDC_Scores_20210305.csv")

Ethnic_bg <- Data[,c("ETHNIC_BACKGROUND","CALC_PDC_SCORE")]

Ethnic_bg <- na.omit(Ethnic_bg)

Ethnic_bg_split <- split(Ethnic_bg, Ethnic_bg$ETHNIC_BACKGROUND)

grouped <- Ethnic_bg %>% group_by(ETHNIC_BACKGROUND)

install.packages("dplyr")
library(dplyr)

Ethnic_bg$ETHNIC_BACKGROUND = gsub(";.*","", Ethnic_bg$ETHNIC_BACKGROUND)

#Removing zero 

Ethnic_bg <- Ethnic_bg %>% filter(Ethnic_bg$CALC_PDC_SCORE !=0)

#Subset of Non Hispanic
Non_Hispanic <- Ethnic_bg %>%
  filter(grepl('Not', ETHNIC_BACKGROUND))

#Subset Unknown
Unknown_1 <- Ethnic_bg %>%
  filter(grepl('Patient Refused', ETHNIC_BACKGROUND))

Unknown_2 <- Ethnic_bg %>%
  filter(grepl('Unknown', ETHNIC_BACKGROUND))

# All of the unknown 
Unknown <- rbind(Unknown_1,Unknown_2)

#Subset of Hispanic
Hispanic_1<- Ethnic_bg %>%
  filter(grepl('Costa', ETHNIC_BACKGROUND))

Hispanic_2<- Ethnic_bg %>%
  filter(grepl('Cuban', ETHNIC_BACKGROUND))

Hispanic_3<- Ethnic_bg %>%
  filter(grepl('Dominican', ETHNIC_BACKGROUND))

Hispanic_4<- Ethnic_bg %>%
  filter(grepl('Guatemalan', ETHNIC_BACKGROUND))

Hispanic_5<- Ethnic_bg %>%
  filter(grepl('Hispanic/Latino ?', ETHNIC_BACKGROUND))

Hispanic_6<- Ethnic_bg %>%
  filter(grepl('Honduran', ETHNIC_BACKGROUND))

Hispanic_7<- Ethnic_bg %>%
  filter(grepl('Mexican', ETHNIC_BACKGROUND))

Hispanic_8<- Ethnic_bg %>%
  filter(grepl('Nicaraguan', ETHNIC_BACKGROUND))

Hispanic_9<- Ethnic_bg %>%
  filter(grepl('Panamanian', ETHNIC_BACKGROUND))

Hispanic_10<- Ethnic_bg %>%
  filter(grepl('Puerto Rican', ETHNIC_BACKGROUND))

Hispanic_11<- Ethnic_bg %>%
  filter(grepl('Salvadoran', ETHNIC_BACKGROUND))

Hispanic_12<- Ethnic_bg %>%
  filter(grepl('South American', ETHNIC_BACKGROUND))

Hispanic_13<- Ethnic_bg %>%
  filter(grepl('Spaniard', ETHNIC_BACKGROUND))

#All Hispanic
Hispanic <- rbind(Hispanic_1,Hispanic_2,Hispanic_3,Hispanic_4, Hispanic_5, Hispanic_6, Hispanic_7, Hispanic_8, Hispanic_10, Hispanic_11, Hispanic_12, Hispanic_13)

boxplot(Hispanic$CALC_PDC_SCORE, Non_Hispanic$CALC_PDC_SCORE, Unknown$CALC_PDC_SCORE, horizontal = TRUE, col = blues9, names =c("H", "N", "U"), xlab="PDC Score", ylab="Ethnicity", main= "Ethnicity vs PDC Score")