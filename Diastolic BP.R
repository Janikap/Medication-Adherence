rm(list=ls())

setwd("R:/bellih01lab/home/JanikaPeyasena")

Data <- read.csv("HTN_ALTA_Project_BP_Control_TY_Nov_2020_w_PDC_Scores_20210305.csv")


#Data with just diastolic BP
diastolic <- Data[,c("BP_DIASTOLIC","CALC_PDC_SCORE")]

#Diastolic BP without N/A
diastolic_omit <- na.omit(diastolic)

# Paired T test- diastolic
t.test(diastolic_omit$BP_DIASTOLIC,diastolic_omit$CALC_PDC_SCORE,paired = T)
#t = 39.292, df = 14822, p-value < 2.2e-16

#diastolic with age
diastolic_age <- Data[,c("CURRENT_AGE","BP_DIASTOLIC","CALC_PDC_SCORE")]
diastolic_age_omit <- na.omit(diastolic_age)


#split by age
Over_65 <- subset(diastolic_age_omit, diastolic_age_omit$CURRENT_AGE >=65 & diastolic_age_omit$CURRENT_AGE <87)

over_40 <- subset(diastolic_age_omit, diastolic_age_omit$CURRENT_AGE >=40 & diastolic_age_omit$CURRENT_AGE <65)

under_40 <-subset(diastolic_age_omit, diastolic_age_omit$CURRENT_AGE >=0 & diastolic_age_omit$CURRENT_AGE <40)


#without 0 value
Over_65_0 <- subset(Over_65,Over_65$CALC_PDC_SCORE >=1 & Over_65$CALC_PDC_SCORE <101)

Over_40_0 <- subset(over_40,over_40$CALC_PDC_SCORE >=1 & over_40$CALC_PDC_SCORE <101)

Under_40_0 <- subset(under_40,under_40$CALC_PDC_SCORE >=1 & under_40$CALC_PDC_SCORE <101)




#ggplot
ggplot(over_40, aes(x=BP_DIASTOLIC, y=CALC_PDC_SCORE,color=CURRENT_AGE)) + ggtitle("Age 40-64 Diastolic BP VS PDC Score") + xlab("Diastolic BP") + ylab("PDC Score") + geom_point() + theme_light()+ theme(plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(legend.title=element_blank())

ggplot(under_40, aes(x=BP_DIASTOLIC, y=CALC_PDC_SCORE,color=CURRENT_AGE)) + ggtitle("Age Under 40 Diastolic BP VS PDC Score") + xlab("Diastolic BP") + ylab("PDC Score") + geom_point() + theme_light()+ theme(plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(legend.title=element_blank())

ggplot(Over_65, aes(x=BP_DIASTOLIC, y=CALC_PDC_SCORE,color=CURRENT_AGE)) + ggtitle("Age Over 65 Diastolic BP VS PDC Score") + xlab("Diastolic BP") + ylab("PDC Score") + geom_point() + theme_light()+ theme(plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(legend.title=element_blank())


#with regression line

ggplot(Over_65_0, aes(x=BP_DIASTOLIC, y=CALC_PDC_SCORE,color=CURRENT_AGE)) + ggtitle("Age Over 65 Diastolic BP VS PDC Score") + xlab("Diastolic BP") + ylab("PDC Score") + geom_point() + theme_light()+ theme(plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(legend.title=element_blank())+geom_smooth(method='lm', formula= y~x)

ggplot(Under_40_0, aes(x=BP_DIASTOLIC, y=CALC_PDC_SCORE,color=CURRENT_AGE)) + ggtitle("Age Under 40 Diastolic BP VS PDC Score") + xlab("Diastolic BP") + ylab("PDC Score") + geom_point() + theme_light()+ theme(plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(legend.title=element_blank())+geom_smooth(method='lm', formula= y~x)

ggplot(Over_40_0, aes(x=BP_DIASTOLIC, y=CALC_PDC_SCORE,color=CURRENT_AGE)) + ggtitle("Age 40-64 Diastolic BP VS PDC Score") + xlab("Diastolic BP") + ylab("PDC Score") + geom_point() + theme_light()+ theme(plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(legend.title=element_blank())+geom_smooth(method='lm', formula= y~x)


# get the slope of the regression line
results <- lm(BP_DIASTOLIC ~CALC_PDC_SCORE , data = Under_40_0)
results <- lm(BP_DIASTOLIC ~CALC_PDC_SCORE , data = Over_40_0)
results <- lm(BP_DIASTOLIC ~CALC_PDC_SCORE , data = Over_65_0)
      
