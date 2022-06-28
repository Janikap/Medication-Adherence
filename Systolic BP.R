rm(list=ls())

setwd("R:/bellih01lab/home/JanikaPeyasena")

Data <- read.csv("HTN_ALTA_Project_BP_Control_TY_Nov_2020_w_PDC_Scores_20210305.csv")

#Data with just systolic BP
systolic <- Data[,c("BP_SYSTOLIC","CALC_PDC_SCORE")]

#Systolic BP without N/A
systolic_omit <- na.omit(systolic)


#with age
systolic_age <- Data[,c("CURRENT_AGE","BP_SYSTOLIC","CALC_PDC_SCORE")]
systolic_age_omit <- na.omit(systolic_age)

#splits by each age
s <- split(systolic_age_omit, systolic_age_omit$CURRENT_AGE)

Over_65 <- subset(systolic_age_omit, systolic_age_omit$CURRENT_AGE >=65 & systolic_age_omit$CURRENT_AGE <87)

over_40 <- subset(systolic_age_omit, systolic_age_omit$CURRENT_AGE >=40 & systolic_age_omit$CURRENT_AGE <65)

under_40 <-subset(systolic_age_omit, systolic_age_omit$CURRENT_AGE >=0 & systolic_age_omit$CURRENT_AGE <40)

plot(x=Over_65$BP_SYSTOLIC, y=Over_65$CALC_PDC_SCORE, main = "BP", xlab = "BP", ylab = "PDC Score")

#Scattered Plot
ggplot(Over_65, aes(x=BP_SYSTOLIC, y=CALC_PDC_SCORE,color=CURRENT_AGE)) + geom_point()
ggplot(over_40, aes(x=BP_SYSTOLIC, y=CALC_PDC_SCORE,color=CURRENT_AGE)) + geom_point()
ggplot(under_40, aes(x=BP_SYSTOLIC, y=CALC_PDC_SCORE,color=CURRENT_AGE)) + geom_point()



#without grid lines

ggplot(over_40, aes(x=BP_SYSTOLIC, y=CALC_PDC_SCORE,color=CURRENT_AGE)) +geom_point() + theme_light()+ theme(plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())


# With labels
ggplot(over_40, aes(x=BP_SYSTOLIC, y=CALC_PDC_SCORE,color=CURRENT_AGE)) + ggtitle("Age 40-64 Systolic BP VS PDC Score") + xlab("Systolic BP") + ylab("PDC Score") + geom_point() + theme_light()+ theme(plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(legend.title=element_blank())

ggplot(under_40, aes(x=BP_SYSTOLIC, y=CALC_PDC_SCORE,color=CURRENT_AGE)) + ggtitle("Age Under 40 Systolic BP VS PDC Score") + xlab("Systolic BP") + ylab("PDC Score") + geom_point() + theme_light()+ theme(plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(legend.title=element_blank())

ggplot(Over_65, aes(x=BP_SYSTOLIC, y=CALC_PDC_SCORE,color=CURRENT_AGE)) + ggtitle("Age Over 65 Systolic BP VS PDC Score") + xlab("Systolic BP") + ylab("PDC Score") + geom_point() + theme_light()+ theme(plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(legend.title=element_blank())


# with regression line 

ggplot(Under_40_0, aes(x=BP_SYSTOLIC, y=CALC_PDC_SCORE,color=CURRENT_AGE)) + ggtitle("Age Over 65 Systolic BP VS PDC Score") + xlab("Systolic BP") + ylab("PDC Score") + geom_point() + theme_light()+ theme(plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(legend.title=element_blank())+geom_smooth(method='lm', formula= y~x)


#without 0 value
Over_65_0 <- subset(Over_65,Over_65$CALC_PDC_SCORE >=1 & Over_65$CALC_PDC_SCORE <101)

Over_40_0 <- subset(over_40,over_40$CALC_PDC_SCORE >=1 & over_40$CALC_PDC_SCORE <101)

Under_40_0 <- subset(under_40,under_40$CALC_PDC_SCORE >=1 & under_40$CALC_PDC_SCORE <101)


#Regression line slope
results <- lm(BP_SYSTOLIC ~CALC_PDC_SCORE , data = Over_65_0)
results <- lm(BP_SYSTOLIC ~CALC_PDC_SCORE , data = Over_40_0)
results <- lm(BP_SYSTOLIC ~CALC_PDC_SCORE , data = Under_40_0)
