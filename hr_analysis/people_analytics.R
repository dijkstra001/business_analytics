setwd("D:/DATA SCIENCE/CURSOS E TREINAMENTOS/BUSINESS-ANALYTICS/hr_analysis")
getwd()
library(plyr)
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

ggplot(dataset) + geom_bar(aes(x = Gender))
ggplot(dataset) + geom_density(aes(x = Age))
ggplot(dataset) + geom_bar(aes(x = Attrition))
ggplot(dataset) + geom_bar(aes(x = Department))
ggplot(dataset) + geom_bar(aes(x = JobRole))
ggplot(dataset) + geom_bar(aes(x = Education)) + facet_grid(~EducationField)

vTotalWorkingYears <- ggplot(dataset) + geom_density(aes(x = TotalWorkingYears))
vYearsAtCompany <- ggplot(dataset) + geom_density(aes(x = YearsAtCompany))
vYearsSinceLastPromotion <- ggplot(dataset) + geom_density(aes(x = YearsSinceLastPromotion))
vYearsWithCurrManager <- ggplot(dataset) + geom_density(aes(x = YearsWithCurrManager))
vYearsInCurrentRole <- ggplot(dataset) + geom_density(aes(x = YearsInCurrentRole))
vPriorYearsOfExperience <- ggplot(dataset) + geom_density(aes(x = PriorYearsOfExperience))

grid.arrange(vTotalWorkingYears,
             vYearsAtCompany,
             vYearsSinceLastPromotion,
             vYearsWithCurrManager,
             vYearsInCurrentRole,
             vPriorYearsOfExperience,
             nrow = 2,
             ncol = 3)

# % employee per years of experience (1, 3, 5, 7, 10):
length(which(dataset$PriorYearsOfExperience < 1)) / length(dataset$PriorYearsOfExperience)
length(which(dataset$PriorYearsOfExperience < 3)) / length(dataset$PriorYearsOfExperience)
length(which(dataset$PriorYearsOfExperience < 5)) / length(dataset$PriorYearsOfExperience)
length(which(dataset$PriorYearsOfExperience < 7)) / length(dataset$PriorYearsOfExperience)
length(which(dataset$PriorYearsOfExperience < 10)) / length(dataset$PriorYearsOfExperience)

# % employee with < 30 years old:
length(which(dataset$Age < 30)) / length(dataset$Age)

# % employee with graduation and master degree:
length(which(dataset$Education == 3)) / length(dataset$Education)
length(which(dataset$Education == 4)) / length(dataset$Education)

# Showing salary per month by satisfaction work level:
ggplot(data = subset(dataset, !is.na(JobSatisfaction)), aes(x = JobSatisfaction, MonthlyIncome)) +
  geom_boxplot()

# Calculating the corr between variables:
cor(dataset$TotalWorkingYears, dataset$YearsAtCompany, use = 'complete.obs')
cor(dataset$YearsAtCompany, dataset$YearsInCurrentRole, use = 'complete.obs')
cor(dataset$YearsAtCompany, dataset$YearsSinceLastPromotion, use = 'complete.obs')
cor(dataset$YearsAtCompany, dataset$YearsWithCurrManager, use = 'complete.obs')
cor(dataset$YearsAtCompany, dataset$MonthlyIncome, use = 'complete.obs')
cor(dataset$TotalWorkingYears, dataset$MonthlyIncome, use = 'complete.obs')

ggplot(dataset) + geom_point(aes(TotalWorkingYears, MonthlyIncome))
ggplot(dataset) + geom_point(aes(YearsAtCompany, MonthlyIncome))

ggplot(data = subset(dataset, !is.na(WorkLifeBalance)), aes(WorkLifeBalance, MonthlyIncome)) +
  geom_boxplot()

# Salary diff per Gender:
ggplot(data = subset(dataset, !is.na(Gender)), aes(Gender, MonthlyIncome, fill = Gender)) +
  geom_boxplot() +
  theme(legend.position = 'none', plot.title = element_text(hjust = 0.5, size = 10)) +
  labs(x = 'Gender', y = 'Monthly Income', title = 'Monthly income by Gender') +
  coord_flip()

# Job role insights:
ggplot(data = subset(dataset, !is.na(JobRole))) + geom_boxplot(aes(JobRole, MonthlyIncome)) +
  ggtitle('Monthly income per Job role')

ggplot(data = subset(dataset, !is.na(JobRole))) + geom_boxplot(aes(JobRole, AgeStartedWorking)) +
  ggtitle('Age starting working per Job role')

ggplot(data = subset(dataset, !is.na(JobRole))) + geom_boxplot(aes(JobRole, Age)) +
  ggtitle('Age per Job role')

ggplot(data = subset(dataset, !is.na(JobRole))) + geom_boxplot(aes(JobRole, YearsAtCompany)) +
  ggtitle('Years at company per Job role')

ggplot(data = na.omit(dataset)) + geom_bar(aes(JobRole, fill = Education), position = 'fill') +
  ggtitle('Education level per Job role') +
  ylab('Proportion')

# Multivariable analysis - hiring process:
ggplot(data = withoutTermination) + geom_bar(aes(x = Education, fill = Attrition), position = 'fill') +
  facet_grid(.~Department)

ggplot(data = withoutTermination) + geom_bar(aes(x = Education, fill = Attrition), position = 'fill') +
  facet_grid(.~JobRole)

ggplot(data = withoutTermination) + geom_bar(aes(x = EducationField, fill = Attrition), position = 'fill') +
  facet_grid(.~JobRole) +
  theme(axis.text.x = element_text(angle = -90, hjust = 0))


# MODELING DATA FOR ML ALGORITHMS:
