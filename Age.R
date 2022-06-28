rm(list=ls())

setwd("R:/bellih01lab/home/JanikaPeyasena")

Data <- read.csv("HTN_ALTA_Project_BP_Control_TY_Nov_2020_w_PDC_Scores_20210305.csv")

age <- Data[,c("CURRENT_AGE","CALC_PDC_SCORE")]

age <- na.omit(age)

#Removing 0 value fo pdc 
age_0 <- subset(age,age$CALC_PDC_SCORE >=1 & age$CALC_PDC_SCORE <101)

#Subset of ages
Over_65 <- subset(age_0, age_0$CURRENT_AGE >=65 & age_0$CURRENT_AGE <87)

over_40 <- subset(age_0, age_0$CURRENT_AGE >=40 & age_0$CURRENT_AGE <65)

under_40 <-subset(age_0, age_0$CURRENT_AGE >=0 & age_0$CURRENT_AGE <40)


#Boxplot 
boxplot(under_40$CALC_PDC_SCORE, over_40$CALC_PDC_SCORE,Over_65$CALC_PDC_SCORE, horizontal = TRUE,names =c("0-39","40-64","65+"), col = blues9, xlab="PDC Score", ylab="Age", main= "Age vs PDC Score")

# Pearson's test
cor.test(age_0$CALC_PDC_SCORE, age_0$CURRENT_AGE, method = "pearson")


