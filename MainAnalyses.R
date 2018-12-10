
library(plyr)
library(Hmisc)
library(MASS)
library(ggplot2)


# Risky health choices and the Balloon Economic Risk Protocol 

# First run: Questionnaire.R
# Run RA.R and get RiskAmb as dataframe

cd <- "/Users/Kimmie/Dropbox/Papers/BART/PaperVersions/EconBARTalone/JOEP/DataFiles/" ### Put your own current directory here.
q2="BERPdata.csv" #leave name intact
q3="BBdis.txt" #leave name intact

### File extensions
file2=paste(cd, q2, sep = "")
file3=paste(cd, q3, sep = "")

# Upload BART Experimental data
#Mean behaviors:
Experiment=read.csv(file2)
#Upload Belief distribution
BBdis=read.csv(file3, header=FALSE)
  #rename some variables:
  BBdis <- rename(BBdis, c(V1="Points"))
  BBdis <- rename(BBdis, c(V2="Bin"))
  BBdis <- rename(BBdis, c(V3="Participant"))

#Create additional variable
Experiment$RiskBelief=Experiment$Alone-Experiment$BeliefBalloon

# Merge all files

merge.all <- function(by, ...) {
  frames <- list(...)
  df <- Reduce(function(x, y) { merge(x, y, by = by) }, frames)
  return(df)
}

# 
Alldata=merge.all(by = "Participant", AuditA, AuditS, CDCrisk, Druguse, Sex, Demographics, Experiment, RiskAmb) 
names(Alldata) # double check all variables

# ----------------------------------------------------------

### Analyses below are in the order of which they are discussed in the manuscript.

# Population sample
summary(Alldata$Age)
fem=with(Alldata, c(sum(Gender==1)))
fem/nrow(Alldata) #ratio female

# Test mean histogram against uniform distribution
aggregate(BBdis$Points~BBdis$Bin, FUN=mean)
testdistribution=c(1,2,2,3,3,3,3,4,4,4,4,4,5,5,5,5,5,5,6,6,6,6,6,6,6,7,7,7,7,7,7,7,8,8,8,8,8,8,8,8,9,9,9,9,9,9,9,9,9,10,10,10,10,10,10,10,10,10,10,11,11,11,11,11,11,11,11,12,12,12,12,12,12,12,13,13,13,13,13,13,14,14,14,14,15,15,15,15,16,16,16,17,17,18,18,19,19,20,20,20)

hist(testdistribution,breaks=c(0:20),plot=TRUE)
ks.test(testdistribution,"punif",1,20) #p<.05, so we reject the null that the sample comes from a population which has a uniform distribution

# Create boxplot graph per balloon size bin
BBdis$Bin=factor(BBdis$Bin)
boxplot(BBdis$Points ~ BBdis$Bin,main="Participants' distributions") #Fig. 4
boxplot(BBdis$Points ~ BBdis$Bin,ylim=c(0,0.5),main="Participants' distributions") #Appendix figure

#Mean Beliefs
summary(Alldata$BeliefBalloon) #participants' mean belief balloon

# Fig. 5
ggplot(data=Alldata, aes(x=BeliefBalloon)) + geom_histogram(colour="black", fill="darkgrey") +
  geom_vline(aes(xintercept=mean(BeliefBalloon)),   
             color="blue", linetype="dashed", size=1) +
  coord_cartesian(xlim=c(0, 80)) + 
  scale_x_continuous(breaks = seq(0, 80,10)) +
  scale_y_continuous(breaks = seq(0, 30,5))

# ----------------------------------------------------------

# Risk preferences in the BERP

summary(Experiment$Alone)
sd(Experiment$Alone)
range(Experiment$Alone)
breaks = seq(9, 47, by=0.5)
Alone.cut = cut(Experiment$Alone, breaks)
Alone.freq = table(Alone.cut)
sum(Alone.freq[1:45]) #n=109
sum(Alone.freq[46:47]) #n=8
sum(Alone.freq[48:76]) #n=41

#Gender and BERP
Fem_Male=subset(Alldata, Gender >0) # 1 participant who did not indicate gender
Fem_Male$Gender=factor(Fem_Male$Gender)
aggregate(Fem_Male$Alone, by=list(group=Fem_Male$Gender),mean) #1=female, 2=male
wilcox.test(Fem_Male$Alone ~ Fem_Male$Gender)

# firstcash
summary(Alldata$firstcash)

#Risk-belief
summary(Experiment$RiskBelief)  
sd(Experiment$RiskBelief)  
range(Experiment$RiskBelief)  

riskaverse=-0.5
nrow(subset(Experiment, Experiment$RiskBelief < riskaverse)) #99
nrow(subset(Experiment, Experiment$RiskBelief >= riskaverse & Experiment$RiskBelief <=0.5)) #8
nrow(subset(Experiment, Experiment$RiskBelief >0.5)) #51

#Correlations between all experimental variables
x=Alldata[,c("Alone","Age","Education","firstcash","BeliefBalloon","RA","AA","RiskBelief")] #Not related to age or education
rc=(rcorr(as.matrix(x),type="pearson"))  
rc

# Fig 7
ggplot(Alldata, aes(x=BeliefBalloon, y=Alone)) +
  coord_cartesian(xlim=c(10, 75),ylim=c(10,75)) +
  scale_x_continuous(breaks=seq(10, 75, 10)) +
  scale_y_continuous(breaks=seq(10, 75, 10)) +
  geom_point(shape=1) + 
  geom_smooth(method=lm) +
  geom_abline(intercept = 0, slope = 1)

Figeq=lm(Alone ~ BeliefBalloon, data=Alldata)
summary(Figeq)

# ----------------------------------------------------------

# Descriptives risk/ambiguity preferences

summary(Alldata$RA) #0.473
wilcox.test (RiskAmb$RA, mu=0.5) #p=0.46
summary(RiskAmb$AA) #0.034
wilcox.test (RiskAmb$AA, mu=0) #p=0.03
histogram(RiskAmb$AA) #not normally distributed, so make sense to perform non parametric test.

#Women are more risk and ambiguity averse
aggregate(Fem_Male$RA, by=list(group=Fem_Male$Gender),mean) 
aggregate(Fem_Male$AA, by=list(group=Fem_Male$Gender),mean)
wilcox.test(Fem_Male$RA ~ Fem_Male$Gender) #p=non-sig
wilcox.test(Fem_Male$AA ~ Fem_Male$Gender) #p=non-sig

# How many switchers

#The current NA's did not switch. Give these participants score of 0.
Alldata$SwitchRisk[is.na(Alldata$SwitchRisk)] <- 0
Alldata$SwitchAmb[is.na(Alldata$SwitchAmb)] <- 0

# Total number of switchers
switchers_risk=with(Alldata, c(sum(SwitchRisk>1))) #68
switchers_amb=with(Alldata, c(sum(Alldata$SwitchAmb>1))) #72
switchers=with(Alldata, c(sum(SwitchAmb>1 | SwitchRisk>1))) #92

# Check if behavior is different between switchers and non-switchers: 
Alldata$oneswitch_risk=ifelse(Alldata$SwitchRisk<2,1,0)
Alldata$oneswitch_amb=ifelse(Alldata$SwitchAmb<2,1,0)
Alldata$oneswitch=ifelse(Alldata$oneswitch_risk==1 & Alldata$oneswitch_amb==1,1,0)

# Number of switchers amongst those participants who switch multiple times
aggregate(Alldata$SwitchRisk, by=list(group=Alldata$oneswitch_risk),mean) 
aggregate(Alldata$SwitchAmb, by=list(group=Alldata$oneswitch_amb),mean) 

#Risk preferences in switchers vs non-switchers
aggregate(Alldata$Alone, by=list(group=Alldata$oneswitch),mean)
wilcox.test(Alldata$Alone ~ Alldata$oneswitch) #p=sig

# ----------------------------------------------------------

# summary statistics outside measures

# Alcohol
# A score of 8 or more is associated with harmful or hazardous drinking, 
#a score of 13 or more in women, and 15 or more in men, is likely to indicate alcohol dependence.
summary(Alldata$ASscore)
aggregate(Alldata$ASscore, by=list(group=Alldata$Gender),mean)

# Drugs: number of drugs used (0-7)
summary(Alldata$Drugstotal_O)
aggregate(Alldata$Drugstotal_O, by=list(group=Alldata$Gender),mean)
table(Alldata$Marijuana_O)
table(Alldata$Stimulants_O)
table(Alldata$Cocaine_O)
table(Alldata$Hallucinogens_O)
table(Alldata$Opiates_O)
table(Alldata$Sedatives_O)
table(Alldata$hitup_O)

#Sex: number of sex partners
summary(Alldata$Sexpartners)
aggregate(Alldata$Sexpartners, by=list(group=Alldata$Gender),mean)

#Smoking: does participants smoke or not
table(Alldata$CDCrisk1) #smoking
smoke_fem=with(Alldata, c(sum(CDCrisk1 == 1 & Gender==1)))
smoke_male=with(Alldata, c(sum(CDCrisk1 == 1 & Gender==2)))

#Stealing: did participant steal something last year
table(Alldata$CDCrisk6) #stealing
steal_fem=with(Alldata, c(sum(CDCrisk6 == 1 & Gender==1)))
steal_male=with(Alldata, c(sum(CDCrisk6 == 1 & Gender==2)))

#No seatbelt use: did participant drive without seatbelt
table(Alldata$CDCrisk9) #driving without seatbelt
seatbelt_fem=with(Alldata, c(sum(CDCrisk9 == 1 & Gender==1)))
seatbelt_male=with(Alldata, c(sum(CDCrisk9 == 1 & Gender==2)))

# ----------------------------------------------------------

# Bivariate correlations experimental data and continuous outside measures
# BART Alone correlates with Alcohol use (p=0.04) 
x=Alldata[,c("Alone","RA","AA","firstcash","RiskBelief","ASscore","Sexpartners")] 
rc=rcorr(as.matrix(x),type="pearson")  
rc

# Bivariate correlations experimental data and nominal and ordinal outside measures
x=Alldata[,c("Alone","RA","AA","firstcash","RiskBelief","Drugstotal_O","CDCrisk1","CDCrisk6","CDCrisk9")] 
rc=rcorr(as.matrix(x),type="spearman")  
rc

# ----------------------------------------------------------

# OLS models

#Make Gender a factor
Alldata$Gender=ifelse(Alldata$Gender==0,3,Alldata$Gender)
Alldata$Gender=factor(Alldata$Gender) #fem=1,male=2, unidentified=3

#Make repeated switchers in economic risk/ambiguity task a factor
Alldata$oneswitch=factor(Alldata$oneswitch)

#Make MRI participants a factor
Alldata$MRI=factor(Alldata$MRI)

#Alcohol

Alc1=lm(ASscore ~ RA + AA + Gender + Age + Education, data=Alldata)
summary(Alc1)

Alc2=lm(ASscore ~ Alone + RA + AA + Gender + Age + Education, data=Alldata)
summary(Alc2)

#robustness analyses

# MRI
Alc_R1=lm(ASscore ~ Alone + RA + AA + Gender + Age + Education + MRI, data=Alldata)
summary(Alc_R1)

# non-switchers vs multiswitchers in risk/ambiguity task

Alc_R2=lm(ASscore ~ Alone + RA + AA + Gender + Age + Education + MRI + oneswitch, data=Alldata)
summary(Alc_R2)

#Drugs
Drugs1=lm(Drugstotal_O ~ RA + AA + Gender + Age + Education, data=Alldata)
summary(Drugs1)

Drugs2=lm(Drugstotal_O ~ Alone + RA + AA + Gender + Age + Education, data=Alldata)
summary(Drugs2)

# Robustness analyses

Drugs_R1=lm(Drugstotal_O ~ Alone + RA + AA + Gender + Age + Education + MRI, data=Alldata)
summary(Drugs_R1)

Drugs_R2=lm(Drugstotal_O ~ Alone + RA + AA + Gender + Age + Education + MRI + oneswitch, data=Alldata)
summary(Drugs_R2)

#ordered ordinal regression
Alldata$Drugstotal_O=factor(Alldata$Drugstotal_O)
DrugsOL <- polr(Drugstotal_O ~ Alone + RA + AA + Gender + Age + Education + MRI + oneswitch, data=Alldata, Hess=TRUE)
summary(DrugsOL)

ci <- confint(DrugsOL) # ci for Alone does not include 0, so sig!
confint.default(DrugsOL)

#Smoking

Smoking1 <- glm(CDCrisk1 ~ RA + AA + Gender + Age + Education, family=binomial(link='logit'),data=Alldata)
summary(Smoking1)

Smoking2 <- glm(CDCrisk1 ~ Alone + RA + AA + Gender + Age + Education, family=binomial(link='logit'),data=Alldata)
summary(Smoking2)

Smoking_R1 <- glm(CDCrisk1 ~ Alone + RA + AA + Gender + Age + Education + MRI, family=binomial(link='logit'),data=Alldata)
summary(Smoking_R1)

Smoking_R2 <- glm(CDCrisk1 ~ Alone + RA + AA + Gender + Age + Education + MRI + oneswitch, family=binomial(link='logit'),data=Alldata)
summary(Smoking_R2)

# Risk belief and outside measures

#Alcohol

Alc3=lm(ASscore ~ RA + AA + Gender + Age + Education, data=Alldata)
summary(Alc3)

Alc4=lm(ASscore ~ RiskBelief + RA + AA + Gender + Age + Education, data=Alldata)
summary(Alc4)

# Drug use
#ordered ordinal regression
Alldata$Drugstotal_O=factor(Alldata$Drugstotal_O)
DrugsOL2 <- polr(Drugstotal_O ~ RiskBelief + RA + AA + Gender + Age + Education, data=Alldata, Hess=TRUE)
summary(DrugsOL2)

ci <- confint(DrugsOL2) # ci for Alone does not include 0, so sig!
confint.default(DrugsOL2)
