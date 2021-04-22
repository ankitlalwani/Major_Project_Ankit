setwd("/Users/Ankitlalwani/Desktop/Final Project")
#converting all empty and ? to NA
diab_data<- read.csv("10kDiabetes.csv", header = TRUE, na.strings = c("","?","NA")) 
head(diab_data)
str(diab_data)


#Part 1
#removing weight, since they are too sparse to process 
diab_data[["weight"]]<-NULL

#Part 2
#CORR RCORR
diab_cordata <- subset(diab_data, select = c("time_in_hospital","num_lab_procedures","num_procedures","num_medications","number_ou tpatient","number_emergency","number_inpatient","number_diagnoses","readmitted")) 
cor_data<- cor(diab_data$time_in_hospital,diab_data$num_lab_procedures,use="complete.obs",method="p earson")
cor_data

cor_cordata<-cor(diab_cordata,use="complete.obs",method="pearson") 
cor_cordata

#rcorr
install.packages("Hmisc", dependencies = TRUE)
library(Hmisc)
tnlp<-rcorr(diab_data$time_in_hospital,diab_data$num_lab_procedures, type = "pearson") 
tnlp

install.packages("plyr", dependencies = TRUE) 
library('plyr')
count(diab_data,'race') 
count(diab_data,'gender')

#chi-square analysis
counts1 <- ddply(diab_data,.(diab_data$race,diab_data$readmitted),nrow) 
counts1

names(counts1)<-c("Race","Readmitted","Freq")
african<-c(725,1361) 
caucasian<-c(3115,4442) 
asian<-c(19,36) 
hispanic<-c(65,116) 
other<-c(41,80)

install.packages("gmodels")
library(gmodels)
CrossTable(race_readmitted, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
install.packages("mlogit") 
library(mlogit)
model1 <- glm(diab_data$readmitted ~ diab_data$number_inpatient+diab_data$number_outpatient+diab_data$num_lab_procedures,data = diab_data, family = binomial())
model2 <- glm(diab_data$readmitted ~ diab_data$number_inpatient+diab_data$number_outpatient+diab_data$number_diagnoses,data = diab_data, family = binomial())
summary(model1) 
summary(model2)
anova(model1,model2)