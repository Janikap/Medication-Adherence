rm(list=ls())

setwd("R:/bellih01lab/home/JanikaPeyasena")

Data <- read.csv("HTN_ALTA_Project_BP_Control_TY_Nov_2020_w_PDC_Scores_20210305.csv")


#Data with just Primary care
Primary_setting <- Data[,c("PRIMARY_CARE_YN","CALC_PDC_SCORE","CURRENT_AGE")]

#Primary setting without N/A
Primary_setting_omit <- na.omit(Primary_setting)


#Subset by age
Over_65 <- subset(Primary_setting_omit, Primary_setting_omit$CURRENT_AGE >=65 & Primary_setting_omit$CURRENT_AGE <87)

over_40 <- subset(Primary_setting_omit, Primary_setting_omit$CURRENT_AGE >=40 & Primary_setting_omit$CURRENT_AGE <65)

under_40 <-subset(Primary_setting_omit, Primary_setting_omit$CURRENT_AGE >=0 & Primary_setting_omit$CURRENT_AGE <40)


all <- rbind(Over_65, over_40, under_40)

all_p <- subset(all,all$PRIMARY_CARE_YN==1)

all_s <- subset(all, all$PRIMARY_CARE_YN==0)

#removing the 0 value
Over_65_0 <- subset(Over_65,Over_65$CALC_PDC_SCORE >=1 & Over_65$CALC_PDC_SCORE <101)

Over_40_0 <- subset(over_40,over_40$CALC_PDC_SCORE >=1 & over_40$CALC_PDC_SCORE <101)

Under_40_0 <- subset(under_40,under_40$CALC_PDC_SCORE >=1 & under_40$CALC_PDC_SCORE <101)


#Yes -> 1 and NO -> 0 for Primary care
Over_65$PRIMARY_CARE_YN<-ifelse(Over_65$PRIMARY_CARE_YN=="Y",1,0)

over_40$PRIMARY_CARE_YN<-ifelse(over_40$PRIMARY_CARE_YN=="Y",1,0)

under_40$PRIMARY_CARE_YN<-ifelse(under_40$PRIMARY_CARE_YN=="Y",1,0)


#ggplot
ggplot(data = Over_65, mapping= aes(x=Over_65$PRIMARY_CARE_YN, y=Over_65$CALC_PDC_SCORE)) +geom_point()+labs(x= "Primary Care", y= "PDC Scores")+ ggtitle("Over 65 Primary Care vs PDC Score")

ggplot(data = over_40, mapping= aes(x=over_40$PRIMARY_CARE_YN, y=over_40$CALC_PDC_SCORE)) +geom_point()+labs(x= "Primary Care", y= "PDC Scores")+ ggtitle("Over 40 Primary Care vs PDC Score")


s<- split(Primary_setting_omit, Primary_setting_omit$PRIMARY_CARE_YN)

#boxplot
boxplot(all_p$CALC_PDC_SCORE, all_s$CALC_PDC_SCORE,names = c("Primary"," Specialty"), horizontal = TRUE, col = blues9, xlab="PDC Score", ylab="Race", main= "Primary/ Specialty vs PDC Score")

