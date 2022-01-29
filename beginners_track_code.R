#Importing Libraries
library(dplyr)
library(psych)
library('fastDummies')
library(QuantPsyc) ###RUN THIS CODE
library(mctest)
library(leaps)
library(MASS)

#Importing Data
loan_data <- read.csv("C:/Users/Acer/Documents/Spring 2022/datathon/beginners_track/track.csv")
View(loan_data)

#Coding categorical variables
loan_dummy <- dummy_cols(loan_data, select_columns = c('race', 'denial_reason', 'occupancy_type', 'sex', 'ethnicity', 'debt_to_income_ratio', 
                                                       'applicant_age', 'conforming_loan_limit', 'construction_method', 'loan_type'))

#Create column of loan-to-income ratio
loan_dummy$loan_income_ratio <- (loan_dummy$loan_amount/((loan_dummy$income)*1000))

#Subset all numeric columns from dataframe
numeric_loan_dummy <- unlist(lapply(loan_dummy, is.numeric))
regression_loan_dummy <- loan_dummy[ , numeric_loan_dummy]

#Remove columns with excessive NAs
regression_loan_dummy <- subset(regression_loan_dummy, select = -c(discount_points, lender_credits))
regression_loan_dummy <- subset(regression_loan_dummy, select = -c(total_loan_costs, property_value))

#Replace INF with NA in each column
regression_loan_dummy <- do.call(data.frame,                    
                                 lapply(regression_loan_dummy,
                                        function(x) replace(x, is.infinite(x), NA)))

#Remove rows that contain any NA values
regression_loan_dummy <- na.omit(regression_loan_dummy)

#Reset row names
rownames(regression_loan_dummy) = seq(length=nrow(regression_loan_dummy))

#Model formulation
pre_dummy_lin_reg <- lm(interest_rate ~ ., data = regression_loan_dummy)
summary(pre_dummy_lin_reg)

#Testing linear regression assumptions
par(mfrow = c(2, 2))
plot(pre_dummy_lin_reg)

#Removal of first outliers
regression_loan_dummy <- regression_loan_dummy[-c(6585, 6494), ]

#Reset row names
rownames(regression_loan_dummy) = seq(length=nrow(regression_loan_dummy))

#Model formulation2
pre_dummy_lin_reg <- lm(interest_rate ~ ., data = regression_loan_dummy)
summary(pre_dummy_lin_reg)

#Testing linear regression assumptions2
par(mfrow = c(2, 2))
plot(pre_dummy_lin_reg)

#Removal of second outliers (cook's distance)
regression_loan_dummy <- regression_loan_dummy[-c(32173, 6055), ]

#Reset row names2
rownames(regression_loan_dummy) = seq(length=nrow(regression_loan_dummy))

#Model formulation3
pre_dummy_lin_reg <- lm(interest_rate ~ ., data = regression_loan_dummy)
summary(pre_dummy_lin_reg)

#Testing linear regression assumptions3
par(mfrow = c(2, 2))
plot(pre_dummy_lin_reg)

#Removal of third outlier (cook's distance)
regression_loan_dummy <- regression_loan_dummy[-c(32170), ]

#Reset row names3
rownames(regression_loan_dummy) = seq(length=nrow(regression_loan_dummy))

#Model formulation4
pre_dummy_lin_reg <- lm(interest_rate ~ ., data = regression_loan_dummy)
summary(pre_dummy_lin_reg)

#Testing linear regression assumptions4
par(mfrow = c(2, 2))
plot(pre_dummy_lin_reg)

#Removal of fourth outlier (cook's distance)
regression_loan_dummy <- regression_loan_dummy[-c(63380), ]

#Reset row names3
rownames(regression_loan_dummy) = seq(length=nrow(regression_loan_dummy))

#Model formulation4
pre_dummy_lin_reg <- lm(interest_rate ~ ., data = regression_loan_dummy)
summary(pre_dummy_lin_reg)

#Testing linear regression assumptions4
par(mfrow = c(2, 2))
plot(pre_dummy_lin_reg)

#Removal of fifth outlier (cook's distance)
regression_loan_dummy <- regression_loan_dummy[-c(95923), ]

#Reset row names4
rownames(regression_loan_dummy) = seq(length=nrow(regression_loan_dummy))

#Model formulation5
pre_dummy_lin_reg <- lm(interest_rate ~ ., data = regression_loan_dummy)
summary(pre_dummy_lin_reg)

#Testing linear regression assumptions5
par(mfrow = c(2, 2))
plot(pre_dummy_lin_reg)

#Removing insignificant predictors
regression_loan_dummy <- subset(regression_loan_dummy, select = -c(county_code, msa, applicant_age_above_62, income, tract_one_to_four_family_homes, tract_population,
                                                                   total_units, accepted, race_2.or.more.minority.races, race_Black.or.African.American,
                                                                   race_Free.Form.Text.Only, race_Native.Hawaiian.or.Other.Pacific.Islander, race_White, denial_reason_,
                                                                   denial_reason_Collateral, denial_reason_Credit.application.incomplete, denial_reason_Credit.history,
                                                                   denial_reason_Debt.to.income.ratio, denial_reason_Employment.history, denial_reason_Insufficient.cash..downpayment..closing.costs.,
                                                                   denial_reason_Mortgage.insurance.denied, denial_reason_Other, denial_reason_Unverifiable.information, occupancy_type_Second.residence,
                                                                   sex_Male, ethnicity_Free.Form.Text.Only, ethnicity_Joint, ethnicity_Not.Hispanic.or.Latino, debt_to_income_ratio_, 
                                                                   applicant_age_.74, applicant_age_35.44, applicant_age_45.54, applicant_age_55.64, applicant_age_65.74, conforming_loan_limit_NC,
                                                                   construction_method_Site.built, loan_type_RHS.FSA, loan_type_VA, loan_income_ratio))
#Model formulation6
pre_dummy_lin_reg <- lm(interest_rate ~ ., data = regression_loan_dummy)
summary(pre_dummy_lin_reg)

#Standardizing Coefficients
colnames(regression_loan_dummy)

modelformula <- interest_rate ~ year+ tract_median_age_of_hu+ tract_to_msa_income_percentage+ balloon_payment+ combined_loan_to_value_ratio+ loan_amount+ race_Asian+ occupancy_type_Investment.property+
                                sex_Female+ ethnicity_Hispanic.or.Latino+ debt_to_income_ratio_.60.+ debt_to_income_ratio_30...36.+ debt_to_income_ratio_41...46.+ debt_to_income_ratio_50..60.+ applicant_age_25.34+
                                construction_method_Manufactured+ loan_type_FHA+ census_tract+ tract_minority_pop_percent+ ffiec_msa_md_median_fam_income+ business_or_commercial_purpose+ race_American.Indian.or.Alaska.Native+
                                race_Joint+ occupancy_type_Principal.residence+ sex_Joint+ debt_to_income_ratio_.20.+ debt_to_income_ratio_20...30.+ debt_to_income_ratio_36...41.+ debt_to_income_ratio_46...50.+ applicant_age_.25+
                                conforming_loan_limit_C+loan_type_Conventional

regression_loan_standardized <- lapply(regression_loan_dummy[, all.vars(modelformula)], scale)

regression_standardized_model <- lm(interest_rate ~ year+ tract_median_age_of_hu+ tract_to_msa_income_percentage+ balloon_payment+ combined_loan_to_value_ratio+ loan_amount+ race_Asian+ occupancy_type_Investment.property+
                                      sex_Female+ ethnicity_Hispanic.or.Latino+ debt_to_income_ratio_.60.+ debt_to_income_ratio_30...36.+ debt_to_income_ratio_41...46.+ debt_to_income_ratio_50..60.+ applicant_age_25.34+
                                      construction_method_Manufactured+ loan_type_FHA+ census_tract+ tract_minority_pop_percent+ ffiec_msa_md_median_fam_income+ business_or_commercial_purpose+ race_American.Indian.or.Alaska.Native+
                                      race_Joint+ occupancy_type_Principal.residence+ sex_Joint+ debt_to_income_ratio_.20.+ debt_to_income_ratio_20...30.+ debt_to_income_ratio_36...41.+ debt_to_income_ratio_46...50.+ applicant_age_.25+
                                      conforming_loan_limit_C+loan_type_Conventional, data = regression_loan_standardized)

summary(regression_standardized_model)



regression_loan_standardized <- lapply(regression_loan_dummy[+ all.vars(interest_rate ~ .)], scale)

regression_loan_standardized <- lm(scale(interest_rate) ~ scale(.), data = regression_loan_dummy)

mod <- lm(interest_rate ~ ., data = regression_loan_dummy)
coef_upd <- lm.beta(mod)


#Optimizing adjusted R^2
X = subset(regression_loan_dummy, select = -c(interest_rate))
View(X)

regsubsetsObj <- regsubsets(x = X, y = regression_loan_dummy$interest_rate, nbest = 2, really.big = T)
plot(regsubsetsObj, scale = "adjr2")







###LINEAR REGRESSION###
#Subset all numeric columns from dataframe
numeric_loan_dummy <- unlist(lapply(loan_dummy, is.numeric))
regression_loan_dummy <- loan_dummy[ , numeric_loan_dummy]

#Sum NAs in each column
colSums(is.na(regression_loan_dummy))

#Remove columns with excessive NAs
regression_loan_dummy <- subset(regression_loan_dummy, select = -c(discount_points, lender_credits))
regression_loan_dummy <- subset(regression_loan_dummy, select = -c(total_loan_costs, property_value))

#Remove rows that contain any NA values
regression_loan_dummy <- na.omit(regression_loan_dummy)

#Replace INF with NA in each column
regression_loan_dummy <- do.call(data.frame,                    
                   lapply(regression_loan_dummy,
                          function(x) replace(x, is.infinite(x), NA)))

#Remove rows that contain any NA values
regression_loan_dummy <- na.omit(regression_loan_dummy)



three_var <- subset(regression_loan_dummy, select = c(income, accepted, interest_rate, loan_income_ratio, loan_amount))
View(three_var)






###START FROM HERE AFTER RUNNING QUANTPSYC LIBRARY
#Creation of initial MLR model (CHOOSE VARIABLES WITH STRONG PEARSON CORRELATION COEFFICIENT WITH INTEREST RATE)
model_loan <- lm(interest_rate ~ ., data = regression_loan_dummy) 
summary(model_loan)

#Checking for multicollinearity
imcdiag(model_loan)

#Removing columns with multicollinearity
regression_loan_dummy <- subset(regression_loan_dummy, select = -c(year, tract_one_to_four_family_homes, tract_population, ffiec_msa_md_median_fam_income))

#Recreate MLR model with removed multicollinearity variables
model_loan <- lm(interest_rate ~ ., data = regression_loan_dummy)
summary(model_loan)

#Optimizing R^2 values
X = subset(regression_loan_dummy, select = -c(interest_rate))

regsubsetsObj <- regsubsets(x = X, y = regression_loan_dummy$interest_rate, nbest = 2, really.big = T)
plot(regsubsetsObj, scale = "adjr2")

#Removing predictors for graph clarity
X = subset(X, select = -c(applicant_age_25.34, debt_to_income_ratio_50..60., debt_to_income_ratio_41...46., debt_to_income_ratio_30...36., debt_to_income_ratio_.60., sex_Female))

#Optimizing R^2 values2
regsubsetsObj <- regsubsets(x = X, y = regression_loan_dummy$interest_rate, nbest = 2, really.big = T)
plot(regsubsetsObj, scale = "adjr2")

#Removing predictors for graph clarity
X = subset(X, select = -c(debt_to_income_ratio_36...41., debt_to_income_ratio_20...30., applicant_age_.25, tract_median_age_of_hu))

#Optimizing R^2 values2
regsubsetsObj <- regsubsets(x = X, y = regression_loan_dummy$interest_rate, nbest = 2, really.big = T)
plot(regsubsetsObj, scale = "adjr2")

#Recreating smaller MLR model based off adjusted r^2 value
pre_dummy_lin_reg <- lm(interest_rate ~ balloon_payment + occupancy_type_Investment.property + ffiec_msa_md_median_fam_income + loan_amount + ethnicity_Hispanic.or.Latino + loan_type_FHA, data = regression_loan_dummy)
summary(pre_dummy_lin_reg)

#Testing for multicollinearity
imcdiag(pre_dummy_lin_reg)
View(regression_loan_dummy)

#Recreating smaller MLR model based off of standardized coefficients
pre_dummy_lin_reg <- lm(interest_rate ~ year + occupancy_type_Investment.property + loan_type_FHA + ffiec_msa_md_median_fam_income + debt_to_income_ratio_20...30., data = regression_loan_dummy)
summary(pre_dummy_lin_reg)
?vif()


#Optimizing R^2 values with pre dummy variables
loan_data <- read.csv("C:/Users/Acer/Documents/Spring 2022/datathon/beginners_track/track.csv")




X = subset(regression_loan_dummy, select = -c(interest_rate))
View(X)

regsubsetsObj <- regsubsets(x = X, y = regression_loan_dummy$interest_rate, nbest = 2, really.big = T)
plot(regsubsetsObj, scale = "adjr2")


View(regression_loan_dummy)




View(regression_loan_dummy)




summary(regression_loan_dummy)




#MLR Model with removed insignificant predictors
model_loan_new <- lm(interest_rate ~ VARIABLES, data = loan_dummy) ###CHANGE VARIABLES
summary(model_loan_new)

#Standardizing coefficients
standardized_loan <- lapply(loan_dummy[, all.vars(interest_rate ~ THE VARIABLES THAT MATTER)], scale) ###CHANGE VARIABLES
standardized_model <- lm(interest_rate ~ VARIABLES, data = standardized_loan)
summary(standardized_model)





###LOGISTIC REGRESSION###
#Remove interest_rate for logistic regression
loan_data <- read.csv("C:/Users/Acer/Documents/Spring 2022/datathon/beginners_track/track.csv")

#Coding categorical variables
loan_data <- dummy_cols(loan_data, select_columns = c('race', 'occupancy_type', 'sex', 'ethnicity', 'debt_to_income_ratio', 
                                                       'applicant_age', 'conforming_loan_limit', 'construction_method', 'loan_type'))
View(loan_data)

regression_loan_dummy_log <- subset(loan_data, select = -c(interest_rate, discount_points, lender_credits, total_loan_costs, property_value, county_code, msa, denial_reason))

#Create column of loan-to-income ratio
regression_loan_dummy_log$loan_income_ratio <- (regression_loan_dummy_log$loan_amount/((regression_loan_dummy_log$income)*1000))

#Subset all numeric columns from dataframe
numeric_regression_loan_dummy_log <- unlist(lapply(regression_loan_dummy_log, is.numeric))
regression_loan_dummy_log <- regression_loan_dummy_log[ , numeric_regression_loan_dummy_log]


#Check NAs in columns
colSums(is.na(regression_loan_dummy_log))

View(regression_loan_dummy_log)


#Remove rows that contain any NA values
regression_loan_dummy_log <- na.omit(regression_loan_dummy_log)

#Replace INF with NA in each columndebt_to_income_ratio_
regression_loan_dummy_log <- do.call(data.frame,                    
                                 lapply(regression_loan_dummy_log,
                                        function(x) replace(x, is.infinite(x), NA)))


#Remove rows that contain any NA values
regression_loan_dummy_log <- na.omit(regression_loan_dummy_log)

#Formula Logistic Regression model
model <- glm(formula = accepted ~ ., family = binomial(link = "logit"), data = regression_loan_dummy_log)

#Remove insignificant predictors
regression_loan_dummy_log <- subset(regression_loan_dummy_log, select = -c(census_tract, applicant_age_above_62, income, combined_loan_to_value_ratio, race_Free.Form.Text.Only, race_White, occupancy_type_Investment.property, 
                                                                           occupancy_type_Principal.residence, occupancy_type_Second.residence, sex_Male, ethnicity_Joint, ethnicity_Not.Hispanic.or.Latino, debt_to_income_ratio_50..60., 
                                                                           applicant_age_.25, applicant_age_.74, applicant_age_35.44, applicant_age_45.54, applicant_age_65.74, construction_method_Manufactured, construction_method_Site.built, loan_type_VA,
                                                                           loan_income_ratio))

#Reformulate Logistic Regression Model
model <- glm(formula = accepted ~ ., family = binomial(link = "logit"), data = regression_loan_dummy_log)
summary(model)

#Remove insignificant predictors
regression_loan_dummy_log <- subset(regression_loan_dummy_log, select = -c(tract_population, tract_to_msa_income_percentage, balloon_payment, conforming_loan_limit_NC))


#Reformulate Logistic Regression Model
model <- glm(formula = accepted ~ ., family = binomial(link = "logit"), data = regression_loan_dummy_log)
summary(model)

#Testing logistic regression model assumptions
par(mfrow = c(2, 2))
plot(regression_loan_dummy_log)

#Checking for multicollinearity
imcdiag(model)

#Removing year, reformulate model, & checking for multicollinearity
regression_loan_dummy_log <- subset(regression_loan_dummy_log, select = -c(year))
model <- glm(formula = accepted ~ ., family = binomial(link = "logit"), data = regression_loan_dummy_log)
summary(model)
regression_loan_dummy_log <- subset(regression_loan_dummy_log, select = -c(ffiec_msa_md_median_fam_income))

model <- glm(formula = accepted ~ ., family = binomial(link = "logit"), data = regression_loan_dummy_log)
summary(model)
imcdiag(model)
regression_loan_dummy_log <- subset(regression_loan_dummy_log, select = -c(loan_type_Conventional))

model <- glm(formula = accepted ~ ., family = binomial(link = "logit"), data = regression_loan_dummy_log)
summary(model)
imcdiag(model)


#Standardized Coefficients
modelformula <- accepted ~ tract_one_to_four_family_homes + tract_median_age_of_hu + tract_minority_pop_percent + business_or_commercial_purpose + loan_amount + total_units + race_2.or.more.minority.races + race_American.Indian.or.Alaska.Native + race_Asian
                            + race_Black.or.African.American + race_joint + race_Native.Hawaiin.or.Other.Pacific.Islander + sex_Female + sex_Joint + ethnicity_Free.Form.Text.Only + ethnicity_Hispanic.or.Latino + debt_to_income_ratio_ + debt_to_income_ratio_.20. 
                            + debt_to_income_ratio_.60. + debt_to_income_ratio_20...30. + debt_to_income_ratio_30...36. + debt_to_income_ratio36...41. + debt_to_income_ratio41...46. + debt_to_income_ratio_46...50. + applicant_age_25.34 + applicant_age_55.64 +
                            conforming_loan_limit_C + loan_type_FHA + loan_type_RHS.FSA

logistic_reg_stand <- lapply(regression_loan_dummy_log[, all.vars(modelformula)], scale)

model <- glm(formula = modelformula, family = binomial(link = "logit"), data = regression_loan_dummy_log)
summary(model)

#Final logistic formulation with highest impact coefficients
regression_loan_dummy_log <- subset(regression_loan_dummy_log, select = c(race_Asian, race_American.Indian.or.Alaska.Native, race_2.or.more.minority.races, total_units, business_or_commercial_purpose, accepted))
model <- glm(formula = accepted ~ ., family = binomial(link = "logit"), data = regression_loan_dummy_log)
summary(model)

imcdiag(model)




#Standardize


summary(model)

subsetted_loans <- select(loan_data, c("income","SEX255214", "RHI125214", "RHI225214", "EDU635213", "EDU685213", "VET605213", "INC110213", "PVY020213", "POP060210", "percentage"))



##Hardcoding debt values/Logistic Regression for debt to income ratio & acceptance
hard_coded_debt <- read.csv("C:/Users/Acer/Documents/Spring 2022/datathon/beginners_track/track.csv")
View(hard_coded_debt)
hard_coded_debt <- subset(hard_coded_debt, select = c(debt_to_income_ratio, accepted))
hard_coded_debt["debt_to_income_ratio"][hard_coded_debt["debt_to_income_ratio"] == ">60%"] <- 80
hard_coded_debt["debt_to_income_ratio"][hard_coded_debt["debt_to_income_ratio"] == "41%-<46%"] <- 43.5
hard_coded_debt["debt_to_income_ratio"][hard_coded_debt["debt_to_income_ratio"] == "36%-<41%"] <- 38.5
hard_coded_debt["debt_to_income_ratio"][hard_coded_debt["debt_to_income_ratio"] == "20%-<30%"] <- 25
hard_coded_debt["debt_to_income_ratio"][hard_coded_debt["debt_to_income_ratio"] == "46%-<50%"] <- 48
hard_coded_debt["debt_to_income_ratio"][hard_coded_debt["debt_to_income_ratio"] == "30%-<36%"] <- 33
hard_coded_debt["debt_to_income_ratio"][hard_coded_debt["debt_to_income_ratio"] == "<20%"] <- 10
hard_coded_debt["debt_to_income_ratio"][hard_coded_debt["debt_to_income_ratio"] == "50%-60%"] <- 55

#Convert all values to numeric
hard_coded_debt <- lapply(hard_coded_debt$debt_to_income_ratio, as.numeric)

#Remove rows that contain any NA values
hard_coded_debt <- na.omit(hard_coded_debt)
View(hard_coded_debt)
class(hard_coded_debt)

hard_coded_debt[!(is.na(hard_coded_debt$debt_to_income_ratio) | hard_coded_debt$debt_to_income_ratio==""), ]

#Logistic Regression model
model <- glm(formula = accepted ~ ., family = binomial(link = "logit"), data = hard_coded_debt)
summary(model)

#Linear Regression Model with debt to income ratio & interest_rate
hard_coded_debt_lin <- read.csv("C:/Users/Acer/Documents/Spring 2022/datathon/beginners_track/track.csv")
View(hard_coded_debt)
hard_coded_debt <- subset(hard_coded_debt, select = c(debt_to_income_ratio, interest_rate))
hard_coded_debt["debt_to_income_ratio"][hard_coded_debt["debt_to_income_ratio"] == ">60%"] <- 80
hard_coded_debt["debt_to_income_ratio"][hard_coded_debt["debt_to_income_ratio"] == "41%-<46%"] <- 43.5
hard_coded_debt["debt_to_income_ratio"][hard_coded_debt["debt_to_income_ratio"] == "36%-<41%"] <- 38.5
hard_coded_debt["debt_to_income_ratio"][hard_coded_debt["debt_to_income_ratio"] == "20%-<30%"] <- 25
hard_coded_debt["debt_to_income_ratio"][hard_coded_debt["debt_to_income_ratio"] == "46%-<50%"] <- 48
hard_coded_debt["debt_to_income_ratio"][hard_coded_debt["debt_to_income_ratio"] == "30%-<36%"] <- 33
hard_coded_debt["debt_to_income_ratio"][hard_coded_debt["debt_to_income_ratio"] == "<20%"] <- 10
hard_coded_debt["debt_to_income_ratio"][hard_coded_debt["debt_to_income_ratio"] == "50%-60%"] <- 55

#Convert all values to numeric
hard_coded_debt <- lapply(hard_coded_debt$debt_to_income_ratio, as.numeric)

#Remove rows that contain any NA values
hard_coded_debt <- na.omit(hard_coded_debt)
View(hard_coded_debt)
class(hard_coded_debt)

hard_coded_debt[!(is.na(hard_coded_debt$debt_to_income_ratio) | hard_coded_debt$debt_to_income_ratio==""), ]

#Creation of initial MLR model (CHOOSE VARIABLES WITH STRONG PEARSON CORRELATION COEFFICIENT WITH INTEREST RATE)
model_loan <- lm(interest_rate ~ ., data = hard_coded_debt)
summary(model_loan)













