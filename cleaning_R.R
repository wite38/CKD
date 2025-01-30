
#load tidyverse
library(readr, tidyverse)

#import data set to R
kidney_disease_raw <-read.csv("kidney_disease_raw.csv")

#view data structure
View(kidney_disease_raw)
str(kidney_disease_raw)
dim(kidney_disease_raw)
colnames(kidney_disease_raw)
unique(kidney_disease_raw)
class(kidney_disease_raw$htn)
unique(kidney_disease_raw$htn)
levels(kidney_disease_raw$htn)

#cleaning data set
sum(is.na(kidney_disease_raw)) #checking missing values
kidney_disease_raw <- na.omit(kidney_disease_raw) #omiting missing values

#renaming columns
library(dplyr)
kidney_disease_raw<-rename(kidney_disease_raw, urine_albumin=al, blood_urea=bu, serum_creatine=sc, hgb=hemo)

# deleting irrelevant columns
kidney_disease_raw<-kidney_disease_raw%>% select(-c(4, 6, 7, 8, 9, 10, 11, 14, 15, 17, 18, 19, 23, 24, 25))

#changing data types
kidney_disease_raw$htn<- as.factor(kidney_disease_raw$htn)
kidney_disease_raw$dm<- as.factor(kidney_disease_raw$dm)
kidney_disease_raw$cad<-as.factor(kidney_disease_raw$cad)
kidney_disease_raw$classification<- as.factor(kidney_disease_raw$classification)

#sampling and resizing the data set
set.seed(123)
sampled_kidney_disease_raw<-kidney_disease_raw %>%
  slice_sample(n=30)
#preview the data
str(sampled_kidney_disease_raw)
sum(is.na(kidney_disease_raw)) #checking missing values

#renaming columns
library(dplyr)
sampled_kidney_disease_raw<-rename(sampled_kidney_disease_raw, hypertension=htn, diabetes=dm, coronay_arterial_disease=cad)

#saving the cleaned data set as CSV
write.csv(sampled_kidney_disease_raw, "sampled_kidney_disease_raw.csv", row.names=(FALSE))

class(sampled_kidney_disease_raw$htn)
unique(sampled_kidney_disease_raw$htn)
levels(sampled_kidney_disease_raw$htn)
