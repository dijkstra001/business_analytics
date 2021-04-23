setwd("D:/DATA SCIENCE/CURSOS E TREINAMENTOS/BUSINESS-ANALYTICS/hr_analysis")
getwd()

library(caret)
library(ggplot2)
library(gridExtra)
library(data.table)
library(car)
library(caTools)
library(corrplot)
library(rpart)
library(rpart.plot)


# Loading dataset:
dataset <- fread("dataset.csv")
View(dataset)

# ETL:

dataset$Attrition <- as.factor(dataset$Attrition)
dataset$BusinessTravel <- as.factor(dataset$BusinessTravel)
dataset$Department <- as.factor(dataset$Department)
dataset$Education <- as.factor(dataset$Education)
dataset$EducationField <- as.factor(dataset$EducationField)
dataset$`Employee Source` <- as.factor(dataset$`Employee Source`)
dataset$EnvironmentSatisfaction <- as.factor(dataset$EnvironmentSatisfaction)
dataset$Gender <- as.factor(dataset$Gender)
dataset$JobInvolvement <- as.factor(dataset$JobInvolvement)
dataset$JobLevel <- as.factor(dataset$JobLevel)
dataset$JobRole <- as.factor(dataset$JobRole)
dataset$JobSatisfaction <- as.factor(dataset$JobSatisfaction)
dataset$MaritalStatus <- as.factor(dataset$MaritalStatus)
dataset$OverTime <- as.factor(dataset$OverTime)
dataset$PerformanceRating <- as.factor(dataset$PerformanceRating)
dataset$RelationshipSatisfaction <- as.factor(dataset$RelationshipSatisfaction)
dataset$StockOptionLevel <- as.factor(dataset$StockOptionLevel)
dataset$WorkLifeBalance <- as.factor(dataset$WorkLifeBalance)

dataset$DistanceFromHome <- as.integer(dataset$DistanceFromHome)
dataset$MonthlyIncome <- as.integer(dataset$MonthlyIncome)
dataset$PercentSalaryHike <- as.integer(dataset$PercentSalaryHike)

dataset <- droplevels(dataset)

# Attribute Engineering:

# Create column PriorYearsOfExperience:
dataset$PriorYearsOfExperience <- dataset$TotalWorkingYears - dataset$YearsAtCompany

# Create column AverageTenure:
dataset$AverageTenure <- dataset$PriorYearsOfExperience / dataset$NumCompaniesWorked
dataset$AverageTenure[!is.finite(dataset$AverageTenure)] <- 0

# Create dataset without Attrition = Termination
withoutTermination <- dataset[dataset$Attrition != 'Termination']
withoutTermination <- droplevels(withoutTermination)
dim(withoutTermination)

# Create dataset without Attrition = Voluntary Resignation
withoutVoluntaryResignation <- dataset[dataset$Attrition != 'Voluntary Resignation']
withoutVoluntaryResignation <- droplevels(withoutVoluntaryResignation)
dim(withoutVoluntaryResignation)

# EXPLORATORY ANALYSIS






