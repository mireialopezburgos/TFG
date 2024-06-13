## 0) Script description:##############################################
#
# Date: 08/05/2023
# Author: Mireia Lopez Burgos
# Topic: TFG
# Objective: Analyze data TFG
##

##
# version: v.1.0
# latest version date: 29/05/2023
##

##
# changelist: nous models amb complete cases sense financial management 
##

## 1) Settings:########################################################

# To clean, to set working directory

rm(list=ls())

setwd("//fs01.isglobal.lan/estudiants/Mireia Lopez")
dir()

## 2) Read db:#########################################################
#load("db/Dades Mireia Lopez BiSC_24_016/Dades Mireia Lopez BiSC_24_016/2024_04_25_Mireia_Lopez_BiSC_24_016.RData")
load("db/2024_05_15/2024_05_15_Mireia_Lopez_BiSC_24_016.RData")
load("db/2024_05_15/bw_ML.RData")

## 3) New variables :#########################################################
 
## s11 (Language)
mireia_lopez_2024_05_15$S11_new <- factor(x = mireia_lopez_2024_05_15$S11, levels = c("1","2","3", "4"), 
                                          labels = c("One","Two","More than two", "I don't know / I would rather not answer")
) 

## s11_1 (Language specification) 
# Harmonize:
# All to uppercase and remove accents 
library(stringi)
mireia_lopez_2024_05_15$S11_1_new <- toupper(stri_trans_general(mireia_lopez_2024_05_15$S11_1,"Latin-ASCII"))

# Regular Expressions and other changes
# Blank spaces at the beggining, change to NA
which(mireia_lopez_2024_05_15$S11_1_new=="")
mireia_lopez_2024_05_15$S11_1_new[mireia_lopez_2024_05_15$S11_1_new==""] <- NA
which(is.na(mireia_lopez_2024_05_15$S11_1_new))
sum(is.na(mireia_lopez_2024_05_15$S11_1_new)) # 21 NAs

## s12 (Predominant language)
# Harmonize:
# All to uppercase and remove accents
library(stringi)
mireia_lopez_2024_05_15$S12_new <- toupper(stri_trans_general(mireia_lopez_2024_05_15$S12,"Latin-ASCII"))
table(mireia_lopez_2024_05_15$S12_new)

# Regular Expressions and other changes
# Blank spaces at the beggining, change to NA
which(mireia_lopez_2024_05_15$S12_new=="")
mireia_lopez_2024_05_15$S12_new[mireia_lopez_2024_05_15$S12_new==""] <- NA
which(is.na(mireia_lopez_2024_05_15$S12_new))
sum(is.na(mireia_lopez_2024_05_15$S12_new)) # 501 NAs

## s15 (Partner sex)
mireia_lopez_2024_05_15$S15_new <- factor(x = mireia_lopez_2024_05_15$S15, levels = c("1","2","9"), 
                                          labels = c("Female","Male","I would rather not answer"))

## s21 (Conditional from S20, if they answered "Employee", how many people work for their boss?)
mireia_lopez_2024_05_15$S21_new <- as.numeric(gsub(",", ".", mireia_lopez_2024_05_15$S21))

## s22 (If they answered "Freelance with employees" in s20, how many people they have as employees)
mireia_lopez_2024_05_15$S22_new <- factor(x = mireia_lopez_2024_05_15$S22, levels = c("1","2"), 
                                          labels = c("1 to 24","25 or more"))

## s23 (Do you supervise other workers?)
mireia_lopez_2024_05_15$S23_new <- factor(x = mireia_lopez_2024_05_15$S23, levels = c("1","2"), 
                                          labels = c("Yes","No"))

## S10.32w (How would you say that you or your spouse are managing financially today?)
mireia_lopez_2024_05_15$S10.32w_new <- factor(x = mireia_lopez_2024_05_15$S10.32w, levels = c("1","2","3","4","5","9"), 
                                              labels = c("We live comfortably","We are fine","We are as good as we can",
                                                         "It turns out to be quite difficult","It is very difficult",
                                                         "I would rather not answer"))

# transformar categoria others s11_new en NA i treure-la
mireia_lopez_2024_05_15$S11_new[mireia_lopez_2024_05_15$S11_new %in% c("I don't know / I would rather not answer")] <- NA
mireia_lopez_2024_05_15$S11_new <- droplevels(mireia_lopez_2024_05_15$S11_new)

# merged parity
mireia_lopez_2024_05_15$merged_parity <- ifelse(mireia_lopez_2024_05_15$parity_m >= 3, "3 or more", as.character(mireia_lopez_2024_05_15$parity_m))

# imputem "home" en tots els casos en que el pare biologic sigui que si
mireia_lopez_2024_05_15$partner_sex <- ifelse(mireia_lopez_2024_05_15$pare_bio == "yes" & is.na(mireia_lopez_2024_05_15$S15_new), "Male", mireia_lopez_2024_05_15$S15_new)
mireia_lopez_2024_05_15$partner_sex <- ifelse(mireia_lopez_2024_05_15$partner_sex == "1", "Female", 
                                              ifelse(mireia_lopez_2024_05_15$partner_sex == "2", "Male", mireia_lopez_2024_05_15$partner_sex))

# new s17 employment situation variable
mireia_lopez_2024_05_15$active_worker <- ifelse(mireia_lopez_2024_05_15$S17 == "Active worker (Paid jobs including scholarship students)", 1, 0)
mireia_lopez_2024_05_15$unemployed <- ifelse(mireia_lopez_2024_05_15$S17 == "Unemployed", 1, 0)
mireia_lopez_2024_05_15$other_workers_s17 <- ifelse(mireia_lopez_2024_05_15$S17 %in% c("Student (no scholarship)", "Housewife", "On leave", "Volunteer (unpaid work)"), 1, 0)

# new s20 employment type of active workers variable
mireia_lopez_2024_05_15$employee <- ifelse(mireia_lopez_2024_05_15$S20 %in% c("Employee"), 1, 0)
mireia_lopez_2024_05_15$self_employee <- ifelse(mireia_lopez_2024_05_15$S20 %in% c("Self-employed / Freelance without employees", "Self-employed with employees"), 1, 0)
mireia_lopez_2024_05_15$other_employees_s20 <- ifelse(mireia_lopez_2024_05_15$S20 %in% c("Others") | is.na(mireia_lopez_2024_05_15$S20), 1, 0)

table(mireia_lopez_2024_05_15$employee)

# new s21_new missings = 0
mireia_lopez_2024_05_15$employees_boss <- ifelse(is.na(mireia_lopez_2024_05_15$S21_new), 0, mireia_lopez_2024_05_15$S21_new)

# new s23_new missings = No 
table(mireia_lopez_2024_05_15$S23_new, useNA = "ifany")
mireia_lopez_2024_05_15$S23_new[is.na(mireia_lopez_2024_05_15$S23_new)] <- "No"

# new s10.32w_new financial management 
table(mireia_lopez_2024_05_15$financial_management)

#install.packages("dplyr")
library(dplyr)
mireia_lopez_2024_05_15 <- mireia_lopez_2024_05_15 %>%
  mutate(financial_management = recode(S10.32w_new,
                                       "It turns out to be quite difficult" = "It is difficult",
                                       "It is very difficult" = "It is difficult",
                                       "I would rather not answer" = NA_character_))
# combine dataframes 
bw_grams <- bw_ML[["b_peso"]]
combined_df <- cbind(mireia_lopez_2024_05_15, bw_grams)

## 4) Nous descriptius :######################################################### 

library(dplyr)

#dataset with financial management 
data_subset <- select(combined_df, id_mother, S11_new, partner_sex, active_worker, unemployed, other_workers_s17, employee, self_employee, other_employees_s20, employees_boss, S23_new, financial_management, bmi_m_32w, edad, educ_level_m_3cat, merged_parity, ethnicity_m_3cat, covid_pandemic_m, bw_bcnatal_c_4cat, birthweight_c_3cat, bw_grams, sga_bcnatal_0y_c, gestage_0y_c_weeks, gestage_0y_c_4cat)
complete_data_subset <- data_subset[complete.cases(data_subset), ]

bw_subset <- select(combined_df, S11_new, partner_sex, active_worker, unemployed, other_workers_s17, employee, self_employee, other_employees_s20, employees_boss, S23_new, financial_management, edad, educ_level_m_3cat, merged_parity, ethnicity_m_3cat, bmi_m_32w, covid_pandemic_m, bw_bcnatal_c_4cat, birthweight_c_3cat, bw_grams)
ga_subset <- select(combined_df, S11_new, partner_sex, active_worker, unemployed, other_workers_s17, employee, self_employee, other_employees_s20, employees_boss, S23_new, financial_management, edad, educ_level_m_3cat, merged_parity, ethnicity_m_3cat, bmi_m_32w, covid_pandemic_m, sga_bcnatal_0y_c, gestage_0y_c_weeks, gestage_0y_c_4cat)

bw_complete <- bw_subset[complete.cases(bw_subset), ]
ga_complete <- ga_subset[complete.cases(ga_subset), ]

#dataset without financial management
data_subset_2 <- select(combined_df, id_mother, S11_new, partner_sex, active_worker, unemployed, other_workers_s17, employee, self_employee, other_employees_s20, employees_boss, S23_new, edad, educ_level_m_3cat, merged_parity, ethnicity_m_3cat, bmi_m_32w, covid_pandemic_m, bw_bcnatal_c_4cat, birthweight_c_3cat, bw_grams, sga_bcnatal_0y_c, gestage_0y_c_weeks, gestage_0y_c_4cat)
complete_data_subset_2 <- data_subset_2[complete.cases(data_subset_2), ]

bw_subset_2 <- select(combined_df, S11_new, partner_sex, active_worker, unemployed, other_workers_s17, employee, self_employee, other_employees_s20, employees_boss, S23_new, edad, educ_level_m_3cat, merged_parity, ethnicity_m_3cat, bmi_m_32w, covid_pandemic_m, bw_bcnatal_c_4cat, birthweight_c_3cat, bw_grams)
ga_subset_2 <- select(combined_df, S11_new, partner_sex, active_worker, unemployed, other_workers_s17, employee, self_employee, other_employees_s20, employees_boss, S23_new, edad, educ_level_m_3cat, merged_parity, ethnicity_m_3cat, bmi_m_32w, covid_pandemic_m, sga_bcnatal_0y_c, gestage_0y_c_weeks, gestage_0y_c_4cat)

bw_complete_2 <- bw_subset_2[complete.cases(bw_subset_2), ]
ga_complete_2 <- ga_subset_2[complete.cases(ga_subset_2), ]

# dif hospitals (10: st pau, 11: maternitat, 12: st joan)

# Subset for IDs starting with 10 (St. Paul Hospital)
st_pau <- complete_data_subset[grep("^10", complete_data_subset$id_mother), ]

# Subset for IDs starting with 11 (Maternity Hospital)
maternity <- complete_data_subset[grep("^11", complete_data_subset$id_mother), ]

# Subset for IDs starting with 12 (St. Joan Hospital)
st_joan <- complete_data_subset[grep("^12", complete_data_subset$id_mother), ]

# How many languages are spoken in your home?  #######
summary(complete_data_subset$S11_new)
table(complete_data_subset$S11_new, useNA = "ifany")

value_counts <- table(complete_data_subset$S11_new, useNA = "ifany")
total_participants <- sum(value_counts)
percentages <- (value_counts / total_participants) * 100
print(percentages)

# What is the sex of your current partner?  #######
summary(complete_data_subset$partner_sex)
table(complete_data_subset$partner_sex, useNA = "ifany")

value_counts <- table(complete_data_subset$partner_sex, useNA = "ifany")
total_participants <- sum(value_counts)
percentages <- (value_counts / total_participants) * 100
print(percentages)

# Indicate your current employment situation  #######
#active worker 
summary(complete_data_subset$active_worker)
table(complete_data_subset$active_worker, useNA = "ifany")

value_counts <- table(complete_data_subset$active_worker)
total_participants <- sum(value_counts)
percentages <- (value_counts / total_participants) * 100
print(percentages)

#unemployed
summary(complete_data_subset$unemployed)
table(complete_data_subset$unemployed, useNA = "ifany")

value_counts <- table(complete_data_subset$unemployed)
total_participants <- sum(value_counts)
percentages <- (value_counts / total_participants) * 100
print(percentages)

#other situation 
summary(complete_data_subset$other_workers_s17)
table(complete_data_subset$other_workers_s17, useNA = "ifany")

value_counts <- table(complete_data_subset$other_workers_s17)
total_participants <- sum(value_counts)
percentages <- (value_counts / total_participants) * 100
print(percentages)

# If your answer is active worker, what kind of worker are you?  #######
# employees
summary(complete_data_subset$employee)
table(complete_data_subset$employee, useNA = "ifany")

value_counts <- table(complete_data_subset$employee)
total_participants <- sum(value_counts)
percentages <- (value_counts / total_participants) * 100
print(percentages)

#self-employees
summary(complete_data_subset$self_employee)
table(complete_data_subset$self_employee, useNA = "ifany")

value_counts <- table(complete_data_subset$self_employee)
total_participants <- sum(value_counts)
percentages <- (value_counts / total_participants) * 100
print(percentages)

#other employees
summary(complete_data_subset$other_employees_s20)
table(complete_data_subset$other_employees_s20, useNA = "ifany")

value_counts <- table(complete_data_subset$other_employees_s20)
total_participants <- sum(value_counts)
percentages <- (value_counts / total_participants) * 100
print(percentages)

# If your answer is employee, indicate how many people work for your boss  #######
summary(complete_data_subset$employees_boss)
table(complete_data_subset$employees_boss, useNA = "ifany")
sd_value <- sd(complete_data_subset$employees_boss, na.rm = TRUE)
print(sd_value)

# Calculate quartiles and IQR
q1 <- quantile(complete_data_subset$employees_boss, 0.25, na.rm = TRUE)
q3 <- quantile(complete_data_subset$employees_boss, 0.75, na.rm = TRUE)
iqr <- q3 - q1

# Identify outliers
outliers <- complete_data_subset$employees_boss < (q1 - 1.5 * iqr) | complete_data_subset$employees_boss > (q3 + 1.5 * iqr)

# Filter outliers
outlier_values <- complete_data_subset$employees_boss[outliers]

summary(outliers)
table(outlier_values)

# Define lower and upper bounds for outliers
lower_bound <- q1 - 1.5 * iqr
upper_bound <- q3 + 1.5 * iqr

# Filter out outliers
data_no_outliers <- subset(complete_data_subset, employees_boss >= lower_bound & employees_boss <= upper_bound)

# Calculate mean and standard deviation without outliers
mean_no_outliers <- mean(data_no_outliers$employees_boss)
median_no_outliers <- median(data_no_outliers$employees_boss)
sd_no_outliers <- sd(data_no_outliers$employees_boss)
print(mean_no_outliers)
print(median_no_outliers)
print(sd_no_outliers)
summary(data_no_outliers$employees_boss)

# new variable s22 if self-employed in s20 and na in s22: put a 0 
mireia_lopez_2024_05_15$S22_new <- ifelse(is.na(mireia_lopez_2024_05_15$S22_new) & mireia_lopez_2024_05_15$self_employee == 1, 0, mireia_lopez_2024_05_15$S22_new)

mireia_lopez_2024_05_15$S22_new_cat <- cut(mireia_lopez_2024_05_15$S22_new,
                        breaks = c(-Inf, 0, 24, Inf),
                        labels = c("0", "1 to 24", "25 or more"),
                        right = TRUE,
                        include.lowest = TRUE)

combined_df_2 <- cbind(mireia_lopez_2024_05_15, bw_grams)

# If your answer is freelance with employees, indicate how many people you have as employees  #######
data_subset_3 <- select(combined_df_2, S22_new_cat, id_mother, S11_new, partner_sex, active_worker, unemployed, other_workers_s17, employee, self_employee, other_employees_s20, employees_boss, S23_new, financial_management, bmi_m_32w, edad, educ_level_m_3cat, merged_parity, ethnicity_m_3cat, covid_pandemic_m, bw_bcnatal_c_4cat, birthweight_c_3cat, bw_grams, sga_bcnatal_0y_c, gestage_0y_c_weeks, gestage_0y_c_4cat)
complete_data_subset_3 <- data_subset_3[complete.cases(data_subset_3), ]

summary(complete_data_subset_3$S22_new_cat)
table(complete_data_subset_3$S22_new_cat, useNA = "ifany")

value_counts <- table(complete_data_subset_3$S22_new_cat, useNA = "ifany")
total_participants <- sum(value_counts)
percentages <- (value_counts / total_participants) * 100
print(percentages)

# Do you supervise other workers?  #######
summary(complete_data_subset$S23_new)
table(complete_data_subset$S23_new, useNA = "ifany")

value_counts <- table(complete_data_subset$S23_new, useNA = "ifany")
total_participants <- sum(value_counts)
percentages <- (value_counts / total_participants) * 100
print(percentages)

# How would you say that you or your spouse are managing financially today? Would you say that ...? Check only one box  #######
summary(complete_data_subset$financial_management)
table(complete_data_subset$financial_management, useNA = "ifany")

value_counts <- table(complete_data_subset$financial_management, useNA = "ifany")
total_participants <- sum(value_counts)
percentages <- (value_counts / total_participants) * 100
print(percentages)

# Ntotal 
summary(complete_data_subset)
summary(st_pau)
summary(maternity)
summary(st_joan)

# Age  #######
summary(complete_data_subset$edad)
sd_value <- sd(complete_data_subset$edad)
print(sd_value)

summary(st_pau$edad)
sd_value <- sd(st_pau$edad)
print(sd_value)

summary(maternity$edad)
sd_value <- sd(maternity$edad)
print(sd_value)

summary(st_joan$edad)
sd_value <- sd(st_joan$edad)
print(sd_value)

# Ethnicity  #######
summary(complete_data_subset$ethnicity_m_3cat)
table(complete_data_subset$ethnicity_m_3cat, useNA = "ifany")

table(st_pau$ethnicity_m_3cat, useNA = "ifany")
table(maternity$ethnicity_m_3cat, useNA = "ifany")
table(st_joan$ethnicity_m_3cat, useNA = "ifany")

value_counts <- table(complete_data_subset$ethnicity_m_3cat)
total_participants <- sum(value_counts)
percentages <- (value_counts / total_participants) * 100
print(percentages)

value_counts <- table(st_pau$ethnicity_m_3cat)
total_participants <- sum(value_counts)
percentages <- (value_counts / total_participants) * 100
print(percentages)

value_counts <- table(maternity$ethnicity_m_3cat)
total_participants <- sum(value_counts)
percentages <- (value_counts / total_participants) * 100
print(percentages)

value_counts <- table(st_joan$ethnicity_m_3cat)
total_participants <- sum(value_counts)
percentages <- (value_counts / total_participants) * 100
print(percentages)

# Educational level  #######
table(complete_data_subset$educ_level_m_3cat, useNA = "ifany")

value_counts <- table(complete_data_subset$educ_level_m_3cat, useNA = "ifany")
total_participants <- sum(value_counts)
percentages <- (value_counts / total_participants) * 100
print(percentages)

# st pau
summary(st_pau$educ_level_m_3cat)
table(st_pau$educ_level_m_3cat, useNA = "ifany")

value_counts <- table(st_pau$educ_level_m_3cat, useNA = "ifany")
total_participants <- sum(value_counts)
percentages <- (value_counts / total_participants) * 100
print(percentages)

# maternity
summary(maternity$educ_level_m_3cat)
table(maternity$educ_level_m_3cat, useNA = "ifany")

value_counts <- table(maternity$educ_level_m_3cat, useNA = "ifany")
total_participants <- sum(value_counts)
percentages <- (value_counts / total_participants) * 100
print(percentages)

# st joan 
summary(st_joan$educ_level_m_3cat)
table(st_joan$educ_level_m_3cat, useNA = "ifany")

value_counts <- table(st_joan$educ_level_m_3cat, useNA = "ifany")
total_participants <- sum(value_counts)
percentages <- (value_counts / total_participants) * 100
print(percentages)

# Parity  #######
summary(complete_data_subset$merged_parity)
table(complete_data_subset$merged_parity, useNA = "ifany")
value_counts <- table(complete_data_subset$merged_parity)
total_participants <- sum(value_counts)
percentages <- (value_counts / total_participants) * 100
print(percentages)

# st pau
summary(st_pau$merged_parity)
table(st_pau$merged_parity, useNA = "ifany")
value_counts <- table(st_pau$merged_parity)
total_participants <- sum(value_counts)
percentages <- (value_counts / total_participants) * 100
print(percentages)

# maternity
summary(maternity$merged_parity)
table(maternity$merged_parity, useNA = "ifany")
value_counts <- table(maternity$merged_parity)
total_participants <- sum(value_counts)
percentages <- (value_counts / total_participants) * 100
print(percentages)

# st joan
summary(st_joan$merged_parity)
table(st_joan$merged_parity, useNA = "ifany")
value_counts <- table(st_joan$merged_parity)
total_participants <- sum(value_counts)
percentages <- (value_counts / total_participants) * 100
print(percentages)

# BMI  #######
summary(complete_data_subset$bmi_m_32w)
summary(st_pau$bmi_m_32w)
summary(maternity$bmi_m_32w)
summary(st_joan$bmi_m_32w)

sd_value <- sd(complete_data_subset$bmi_m_32w, na.rm = TRUE)
print(sd_value)

sd_value <- sd(st_pau$bmi_m_32w, na.rm = TRUE)
print(sd_value)

sd_value <- sd(maternity$bmi_m_32w, na.rm = TRUE)
print(sd_value)

sd_value <- sd(st_joan$bmi_m_32w, na.rm = TRUE)
print(sd_value)

# Whether pandemic started before, during of after pregnancy  #######
summary(complete_data_subset$covid_pandemic_m)
table(complete_data_subset$covid_pandemic_m, useNA = "ifany")

value_counts <- table(complete_data_subset$covid_pandemic_m, useNA = "ifany")
total_participants <- sum(value_counts)
percentages <- (value_counts / total_participants) * 100
print(percentages)

# st pau 
summary(st_pau$covid_pandemic_m)
table(st_pau$covid_pandemic_m, useNA = "ifany")

value_counts <- table(st_pau$covid_pandemic_m, useNA = "ifany")
total_participants <- sum(value_counts)
percentages <- (value_counts / total_participants) * 100
print(percentages)

# maternity 
summary(maternity$covid_pandemic_m)
table(maternity$covid_pandemic_m, useNA = "ifany")

value_counts <- table(maternity$covid_pandemic_m, useNA = "ifany")
total_participants <- sum(value_counts)
percentages <- (value_counts / total_participants) * 100
print(percentages)

# st joan 
summary(st_joan$covid_pandemic_m)
table(st_joan$covid_pandemic_m, useNA = "ifany")

value_counts <- table(st_joan$covid_pandemic_m, useNA = "ifany")
total_participants <- sum(value_counts)
percentages <- (value_counts / total_participants) * 100
print(percentages)

# Gestational age at birth, in weeks  #######
summary(complete_data_subset$gestage_0y_c_weeks, na.omit = TRUE)

sd_value <- sd(complete_data_subset$gestage_0y_c_weeks, na.rm = TRUE)
print(sd_value)

summary(st_pau$gestage_0y_c_weeks)
summary(maternity$gestage_0y_c_weeks)
summary(st_joan$gestage_0y_c_weeks)

sd_value <- sd(st_pau$gestage_0y_c_weeks, na.rm = TRUE)
print(sd_value)
sd_value <- sd(maternity$gestage_0y_c_weeks, na.rm = TRUE)
print(sd_value)
sd_value <- sd(st_joan$gestage_0y_c_weeks, na.rm = TRUE)
print(sd_value) 

# Gestational age at birth, in 4 categories  #######
summary(complete_data_subset$gestage_0y_c_4cat)
table(complete_data_subset$gestage_0y_c_4cat, useNA = "ifany")

value_counts <- table(complete_data_subset$gestage_0y_c_4cat, useNA = "ifany")
total_participants <- sum(value_counts)
percentages <- (value_counts / total_participants) * 100
print(percentages)

# st pau 
summary(st_pau$gestage_0y_c_4cat)
table(st_pau$gestage_0y_c_4cat, useNA = "ifany")

value_counts <- table(st_pau$gestage_0y_c_4cat, useNA = "ifany")
total_participants <- sum(value_counts)
percentages <- (value_counts / total_participants) * 100
print(percentages)

# maternity 
summary(maternity$gestage_0y_c_4cat)
table(maternity$gestage_0y_c_4cat, useNA = "ifany")

value_counts <- table(maternity$gestage_0y_c_4cat, useNA = "ifany")
total_participants <- sum(value_counts)
percentages <- (value_counts / total_participants) * 100
print(percentages)

# st joan 
summary(st_joan$gestage_0y_c_4cat)
table(st_joan$gestage_0y_c_4cat, useNA = "ifany")

value_counts <- table(st_joan$gestage_0y_c_4cat, useNA = "ifany")
total_participants <- sum(value_counts)
percentages <- (value_counts / total_participants) * 100
print(percentages)

# Birthweight in 3 categories  #######
summary(complete_data_subset$birthweight_c_3cat)
table(complete_data_subset$birthweight_c_3cat, useNA = "ifany")

value_counts <- table(complete_data_subset$birthweight_c_3cat, useNA = "ifany")
total_participants <- sum(value_counts)
percentages <- (value_counts / total_participants) * 100
print(percentages)

# st pau 
summary(st_pau$birthweight_c_3cat)
table(st_pau$birthweight_c_3cat, useNA = "ifany")

value_counts <- table(st_pau$birthweight_c_3cat, useNA = "ifany")
total_participants <- sum(value_counts)
percentages <- (value_counts / total_participants) * 100
print(percentages)

# maternity 
summary(maternity$birthweight_c_3cat)
table(maternity$birthweight_c_3cat, useNA = "ifany")

value_counts <- table(maternity$birthweight_c_3cat, useNA = "ifany")
total_participants <- sum(value_counts)
percentages <- (value_counts / total_participants) * 100
print(percentages)

# st joan 
summary(st_joan$birthweight_c_3cat)
table(st_joan$birthweight_c_3cat, useNA = "ifany")

value_counts <- table(st_joan$birthweight_c_3cat, useNA = "ifany")
total_participants <- sum(value_counts)
percentages <- (value_counts / total_participants) * 100
print(percentages)

# Birth weight classification based on birthweight centiles of BCNatal  #######
summary(complete_data_subset$bw_bcnatal_c_4cat)
table(complete_data_subset$bw_bcnatal_c_4cat, useNA = "ifany")

value_counts <- table(complete_data_subset$bw_bcnatal_c_4cat, useNA = "ifany")
total_participants <- sum(value_counts)
percentages <- (value_counts / total_participants) * 100
print(percentages)

# st pau 
summary(st_pau$bw_bcnatal_c_4cat)
table(st_pau$bw_bcnatal_c_4cat, useNA = "ifany")

value_counts <- table(st_pau$bw_bcnatal_c_4cat, useNA = "ifany")
total_participants <- sum(value_counts)
percentages <- (value_counts / total_participants) * 100
print(percentages)

# maternity 
summary(maternity$bw_bcnatal_c_4cat)
table(maternity$bw_bcnatal_c_4cat, useNA = "ifany")

value_counts <- table(maternity$bw_bcnatal_c_4cat, useNA = "ifany")
total_participants <- sum(value_counts)
percentages <- (value_counts / total_participants) * 100
print(percentages)

# st joan 
summary(st_joan$bw_bcnatal_c_4cat)
table(st_joan$bw_bcnatal_c_4cat, useNA = "ifany")

value_counts <- table(st_joan$bw_bcnatal_c_4cat, useNA = "ifany")
total_participants <- sum(value_counts)
percentages <- (value_counts / total_participants) * 100
print(percentages)

# Small for gestational age (SGA) or small fetus according to BCNatal BW centiles  #######
summary(complete_data_subset$sga_bcnatal_0y_c)
table(complete_data_subset$sga_bcnatal_0y_c, useNA = "ifany")

value_counts <- table(complete_data_subset$sga_bcnatal_0y_c, useNA = "ifany")
total_participants <- sum(value_counts)
percentages <- (value_counts / total_participants) * 100
print(percentages)

# st pau 
summary(st_pau$sga_bcnatal_0y_c)
table(st_pau$sga_bcnatal_0y_c, useNA = "ifany")

value_counts <- table(st_pau$sga_bcnatal_0y_c, useNA = "ifany")
total_participants <- sum(value_counts)
percentages <- (value_counts / total_participants) * 100
print(percentages)

# maternity 
summary(maternity$sga_bcnatal_0y_c)
table(maternity$sga_bcnatal_0y_c, useNA = "ifany")

value_counts <- table(maternity$sga_bcnatal_0y_c, useNA = "ifany")
total_participants <- sum(value_counts)
percentages <- (value_counts / total_participants) * 100
print(percentages)

# st joan 
sga <- na.omit(st_joan$sga_bcnatal_0y_c) 
summary(st_joan$sga_bcnatal_0y_c)
table(st_joan$sga_bcnatal_0y_c, useNA = "ifany")

value_counts <- table(st_joan$sga_bcnatal_0y_c, useNA = "ifany")
total_participants <- sum(value_counts)
percentages <- (value_counts / total_participants) * 100
print(percentages)

# BW in grams  #######
summary(complete_data_subset$bw_grams, na.omit = TRUE)

sd_value <- sd(complete_data_subset$bw_grams, na.rm = TRUE)
print(sd_value)

summary(st_pau$bw_grams)
summary(maternity$bw_grams)
summary(st_joan$bw_grams)

sd_value <- sd(st_pau$bw_grams, na.rm = TRUE)
print(sd_value)
sd_value <- sd(maternity$bw_grams, na.rm = TRUE)
print(sd_value)
sd_value <- sd(st_joan$bw_grams, na.rm = TRUE)
print(sd_value) 

## 5) GLM:#########################################################

#install.packages("glmtoolbox")
library(glmtoolbox)

# gestational age at birth in 4 cat --> 2 cat: preterm / non preterm

ga_complete_2 <- ga_complete_2 %>%
  mutate(gestage_2cat = case_when(
    gestage_0y_c_4cat %in% c("1", "2") ~ "preterm",
    gestage_0y_c_4cat %in% c("3", "4") ~ "non-preterm"
  ))

# birth weight in 3 cat --> 2 cat: LBW / non LBW

bw_complete_2 <- bw_complete_2 %>%
  mutate(bw_2cat = case_when(
    birthweight_c_3cat %in% c("low_bw") ~ "LBW",
    birthweight_c_3cat %in% c("high_bw", "normal_bw") ~ "non-LBW"
  ))

# gestational age at birth in 4 cat --> 2 cat: preterm / non preterm

ga_complete <- ga_complete %>%
  mutate(gestage_2cat = case_when(
    gestage_0y_c_4cat %in% c("1", "2") ~ "preterm",
    gestage_0y_c_4cat %in% c("3", "4") ~ "non-preterm"
  ))

# birth weight in 3 cat --> 2 cat: LBW / non LBW

bw_complete <- bw_complete %>%
  mutate(bw_2cat = case_when(
    birthweight_c_3cat %in% c("low_bw") ~ "LBW",
    birthweight_c_3cat %in% c("high_bw", "normal_bw") ~ "non-LBW"
  ))

# bw_complete_2 chr --> factor / num 
bw_complete_2$bw_2cat <- as.factor(bw_complete_2$bw_2cat)
bw_complete_2$partner_sex <- as.factor(bw_complete_2$partner_sex)
bw_complete_2$merged_parity <- as.factor(bw_complete_2$merged_parity) 
bw_complete_2$educ_level_m_3cat <- as.factor(bw_complete_2$educ_level_m_3cat) 
bw_complete_2$covid_pandemic_m <- as.factor(bw_complete_2$covid_pandemic_m) 
bw_complete_2$ethnicity_m_3cat <- as.factor(bw_complete_2$ethnicity_m_3cat)

bw_complete_2$active_worker <- as.factor(bw_complete_2$active_worker)
bw_complete_2$unemployed <- as.factor(bw_complete_2$unemployed)
bw_complete_2$other_workers_s17 <- as.factor(bw_complete_2$other_workers_s17)
bw_complete_2$employee <- as.factor(bw_complete_2$employee)
bw_complete_2$self_employee <- as.factor(bw_complete_2$self_employee)
bw_complete_2$other_employees_s20 <- as.factor(bw_complete_2$other_employees_s20)

str(bw_complete_2)

# ga_complete_2 chr --> factor / num 
ga_complete_2$gestage_2cat <- as.factor(ga_complete_2$gestage_2cat) 
ga_complete_2$sga_bcnatal_0y_c <- as.factor(ga_complete_2$sga_bcnatal_0y_c) 
ga_complete_2$partner_sex <- as.factor(ga_complete_2$partner_sex) 
ga_complete_2$merged_parity <- as.factor(ga_complete_2$merged_parity) 
ga_complete_2$educ_level_m_3cat <- as.factor(ga_complete_2$educ_level_m_3cat) 
ga_complete_2$covid_pandemic_m <- as.factor(ga_complete_2$covid_pandemic_m) 
ga_complete_2$ethnicity_m_3cat <- as.factor(ga_complete_2$ethnicity_m_3cat)

ga_complete_2$active_worker <- as.factor(ga_complete_2$active_worker)
ga_complete_2$unemployed <- as.factor(ga_complete_2$unemployed)
ga_complete_2$other_workers_s17 <- as.factor(ga_complete_2$other_workers_s17)
ga_complete_2$employee <- as.factor(ga_complete_2$employee)
ga_complete_2$self_employee <- as.factor(ga_complete_2$self_employee)
ga_complete_2$other_employees_s20 <- as.factor(ga_complete_2$other_employees_s20)

str(ga_complete_2)

# bw_complete chr --> factor / num 
bw_complete$bw_2cat <- as.factor(bw_complete$bw_2cat)
bw_complete$partner_sex <- as.factor(bw_complete$partner_sex)
bw_complete$merged_parity <- as.factor(bw_complete$merged_parity) 
bw_complete$educ_level_m_3cat <- as.factor(bw_complete$educ_level_m_3cat) 
bw_complete$covid_pandemic_m <- as.factor(bw_complete$covid_pandemic_m) 
bw_complete$ethnicity_m_3cat <- as.factor(bw_complete$ethnicity_m_3cat)

bw_complete$active_worker <- as.factor(bw_complete$active_worker)
bw_complete$unemployed <- as.factor(bw_complete$unemployed)
bw_complete$other_workers_s17 <- as.factor(bw_complete$other_workers_s17)
bw_complete$employee <- as.factor(bw_complete$employee)
bw_complete$self_employee <- as.factor(bw_complete$self_employee)
bw_complete$other_employees_s20 <- as.factor(bw_complete$other_employees_s20)

str(bw_complete)

# ga_complete chr --> factor / num 
ga_complete$gestage_2cat <- as.factor(ga_complete$gestage_2cat) 
ga_complete$sga_bcnatal_0y_c <- as.factor(ga_complete$sga_bcnatal_0y_c) 
ga_complete$partner_sex <- as.factor(ga_complete$partner_sex) 
ga_complete$merged_parity <- as.factor(ga_complete$merged_parity) 
ga_complete$educ_level_m_3cat <- as.factor(ga_complete$educ_level_m_3cat) 
ga_complete$covid_pandemic_m <- as.factor(ga_complete$covid_pandemic_m) 
ga_complete$ethnicity_m_3cat <- as.factor(ga_complete$ethnicity_m_3cat)

ga_complete$active_worker <- as.factor(ga_complete$active_worker)
ga_complete$unemployed <- as.factor(ga_complete$unemployed)
ga_complete$other_workers_s17 <- as.factor(ga_complete$other_workers_s17)
ga_complete$employee <- as.factor(ga_complete$employee)
ga_complete$self_employee <- as.factor(ga_complete$self_employee)
ga_complete$other_employees_s20 <- as.factor(ga_complete$other_employees_s20)

str(ga_complete)

# birthweight in 3cat: low, normal, high  --> in 2cat ####### 

#educational level 
bi_bw_2cat_education <- glm(bw_2cat ~ educ_level_m_3cat, data = bw_complete_2, family = binomial(link = "logit"))
summary(bi_bw_2cat_education)
adjR2(bi_bw_2cat_education)

#S11_new num of lang
bi_bw_2cat_s11_new <- glm(bw_2cat ~ S11_new, data = bw_complete_2, family = binomial(link = "logit"))
summary(bi_bw_2cat_s11_new)
adjR2(bi_bw_2cat_s11_new)

bw_3cat_s11_new <- glm(bw_2cat ~ S11_new + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = bw_complete_2, family = binomial(link = "logit"))
summary(bw_3cat_s11_new)
adjR2(bw_3cat_s11_new)

#S17 employment situation
# active worker 
bi_bw_3cat_activeworker <- glm(bw_2cat ~ active_worker, data = bw_complete_2, family = binomial(link = "logit"))
summary(bi_bw_3cat_activeworker)
adjR2(bi_bw_3cat_activeworker)

bw_3cat_activeworker <- glm(bw_2cat ~ active_worker + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = bw_complete_2, family = binomial(link = "logit"))
summary(bw_3cat_activeworker)
adjR2(bw_3cat_activeworker)

# unemployed 
bi_bw_3cat_unemployed <- glm(bw_2cat ~ unemployed, data = bw_complete_2, family = binomial(link = "logit"))
summary(bi_bw_3cat_unemployed)
adjR2(bi_bw_3cat_unemployed)

bw_3cat_unemployed <- glm(bw_2cat ~ unemployed + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = bw_complete_2, family = binomial(link = "logit"))
summary(bw_3cat_unemployed)
adjR2(bw_3cat_unemployed)

# others 
bi_bw_3cat_otherworkers <- glm(bw_2cat ~ other_workers_s17, data = bw_complete_2, family = binomial(link = "logit"))
summary(bi_bw_3cat_otherworkers)
adjR2(bi_bw_3cat_otherworkers)

bw_3cat_otherworkers <- glm(bw_2cat ~ other_workers_s17 + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = bw_complete_2, family = binomial(link = "logit"))
summary(bw_3cat_otherworkers)
adjR2(bw_3cat_otherworkers)

# merged   
bi_bw_3cat_employment_type <- glm(bw_2cat ~ active_worker + unemployed + other_workers_s17, data = bw_complete_2, family = binomial(link = "logit"))
summary(bi_bw_3cat_employment_type)
adjR2(bi_bw_3cat_employment_type)

bw_3cat_employment_type <- glm(bw_2cat ~ active_worker + unemployed + other_workers_s17 + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = bw_complete_2, family = binomial(link = "logit"))
summary(bw_3cat_employment_type)
adjR2(bw_3cat_employment_type)

#s20 employment type
# employee
bi_bw_3cat_employee <- glm(bw_2cat ~ employee, data = bw_complete_2, family = binomial(link = "logit"))
summary(bi_bw_3cat_employee)
adjR2(bi_bw_3cat_employee)

bw_3cat_employee <- glm(bw_2cat ~ employee + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = bw_complete_2, family = binomial(link = "logit"))
summary(bw_3cat_employee)
adjR2(bw_3cat_employee)

# self-employee
bi_bw_3cat_selfemployee <- glm(bw_2cat ~ self_employee, data = bw_complete_2, family = binomial(link = "logit"))
summary(bi_bw_3cat_selfemployee)
adjR2(bi_bw_3cat_selfemployee)

bw_3cat_selfemployee <- glm(bw_2cat ~ self_employee + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = bw_complete_2, family = binomial(link = "logit"))

summary(bw_3cat_selfemployee)
adjR2(bw_3cat_selfemployee)

# other employees
bi_bw_3cat_otheremployees <- glm(bw_2cat ~ other_employees_s20, data = bw_complete_2, family = binomial(link = "logit"))
summary(bi_bw_3cat_otheremployees)
adjR2(bi_bw_3cat_otheremployees)

bw_3cat_otheremployees <- glm(bw_2cat ~ other_employees_s20 + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = bw_complete_2, family = binomial(link = "logit"))
summary(bw_3cat_otheremployees)
adjR2(bw_3cat_otheremployees)

# merged 
bw_3cat_activeworker_type <- glm(bw_2cat ~ employee + other_employees_s20 + self_employee + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = bw_complete_2, family = binomial(link = "logit"))
summary(bw_3cat_activeworker_type)
adjR2(bw_3cat_activeworker_type)

# S21_new people that work for their boss (if employees)
bi_bw_3cat_employeesboss <- glm(bw_2cat ~ employees_boss, data = bw_complete_2, family = binomial(link = "logit"))
summary(bi_bw_3cat_employeesboss)
adjR2(bi_bw_3cat_employeesboss)

bw_3cat_employeesboss <- glm(bw_2cat ~ employees_boss + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = bw_complete_2, family = binomial(link = "logit"))
summary(bw_3cat_employeesboss)
adjR2(bw_3cat_employeesboss)

# S23_new supervision of other workers
bi_bw_3cat_s23 <- glm(bw_2cat ~ S23_new, data = bw_complete_2, family = binomial(link = "logit"))
summary(bi_bw_3cat_s23)
adjR2(bi_bw_3cat_s23)

bw_3cat_s23 <- glm(bw_2cat ~ S23_new + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = bw_complete_2, family = binomial(link = "logit"))
summary(bw_3cat_s23)
adjR2(bw_3cat_s23)

# S10.32w_new financial management
bi_bw_3cat_financialmanagement <- glm(bw_2cat ~ financial_management, data = bw_complete, family = binomial(link = "logit"))
summary(bi_bw_3cat_financialmanagement)
adjR2(bi_bw_3cat_financialmanagement)

bw_3cat_financialmanagement <- glm(bw_2cat ~ financial_management + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = bw_complete, family = binomial(link = "logit"))
summary(bw_3cat_financialmanagement)
adjR2(bw_3cat_financialmanagement)

# S15_new sex of the partner 
bi_bw_3cat_partner_sex <- glm(bw_2cat ~ partner_sex, data = bw_complete_2, family = binomial(link = "logit"))
summary(bi_bw_3cat_partner_sex)
adjR2(bi_bw_3cat_partner_sex)

bw_3cat_partner_sex <- glm(bw_2cat ~ partner_sex + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = bw_complete_2, family = binomial(link = "logit"))
summary(bw_3cat_partner_sex)
adjR2(bw_3cat_partner_sex)

# gestational age in 4 cat --> in 2 cat ####### 
#educational level 
bi_ga_2cat_education <- glm(gestage_2cat ~ educ_level_m_3cat, data = ga_complete_2, family = binomial(link = "logit"))
summary(bi_ga_2cat_education)
adjR2(bi_ga_2cat_education)

#S11_new num of lang
bi_ga_2cat_s11_new <- glm(gestage_2cat ~ S11_new, data = ga_complete_2, family = binomial(link = "logit"))
summary(bi_ga_2cat_s11_new)
adjR2(bi_ga_2cat_s11_new)

ga_2cat_s11_new <- glm(gestage_2cat ~ S11_new + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = ga_complete_2, family = binomial(link = "logit"))
summary(ga_2cat_s11_new)
adjR2(ga_2cat_s11_new)

#S17 employment situation
# active worker 
bi_ga_2cat_activeworker <- glm(gestage_2cat ~ active_worker, data = ga_complete_2, family = binomial(link = "logit"))
summary(bi_ga_2cat_activeworker)
adjR2(bi_ga_2cat_activeworker)

ga_2cat_activeworker <- glm(gestage_2cat ~ active_worker + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = ga_complete_2, family = binomial(link = "logit"))
summary(ga_2cat_activeworker)
adjR2(ga_2cat_activeworker)

# unemployed 
bi_ga_2cat_unemployed <- glm(gestage_2cat ~ unemployed, data = ga_complete_2, family = binomial(link = "logit"))
summary(bi_ga_2cat_unemployed)
adjR2(bi_ga_2cat_unemployed)

ga_2cat_unemployed <- glm(gestage_2cat ~ unemployed + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = ga_complete_2, family = binomial(link = "logit"))
summary(ga_2cat_unemployed)
adjR2(ga_2cat_unemployed)

# others 
bi_ga_2cat_otherworkers <- glm(gestage_2cat ~ other_workers_s17, data = ga_complete_2, family = binomial(link = "logit"))
summary(bi_ga_2cat_otherworkers)
adjR2(bi_ga_2cat_otherworkers)

ga_2cat_otherworkers <- glm(gestage_2cat ~ other_workers_s17 + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = ga_complete_2, family = binomial(link = "logit"))
summary(ga_2cat_otherworkers)
adjR2(ga_2cat_otherworkers)

#s20 employment type
# employee
bi_ga_2cat_employee <- glm(gestage_2cat ~ employee, data = ga_complete_2, family = binomial(link = "logit"))
summary(bi_ga_2cat_employee)
adjR2(bi_ga_2cat_employee)

ga_2cat_employee <- glm(gestage_2cat ~ employee + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = ga_complete_2, family = binomial(link = "logit"))
summary(ga_2cat_employee)
adjR2(ga_2cat_employee)

# self-employee
bi_ga_2cat_selfemployee <- glm(gestage_2cat ~ self_employee, data = ga_complete_2, family = binomial(link = "logit"))
summary(bi_ga_2cat_selfemployee)
adjR2(bi_ga_2cat_selfemployee)

ga_2cat_selfemployee <- glm(gestage_2cat ~ self_employee + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = ga_complete_2, family = binomial(link = "logit"))
summary(ga_2cat_selfemployee)
adjR2(ga_2cat_selfemployee)

# other employees
bi_ga_2cat_otheremployees <- glm(gestage_2cat ~ other_employees_s20, data = ga_complete_2, family = binomial(link = "logit"))
summary(bi_ga_2cat_otheremployees)
adjR2(bi_ga_2cat_otheremployees)

ga_2cat_otheremployees <- glm(gestage_2cat ~ other_employees_s20 + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = ga_complete_2, family = binomial(link = "logit"))
summary(ga_2cat_otheremployees)
adjR2(ga_2cat_otheremployees)

# S21_new people that work for their boss (if employees)
bi_ga_2cat_employeesboss <- glm(gestage_2cat ~ employees_boss, data = ga_complete_2, family = binomial(link = "logit"))
summary(bi_ga_2cat_employeesboss)
adjR2(bi_ga_2cat_employeesboss)

ga_2cat_employeesboss <- glm(gestage_2cat ~ employees_boss + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = ga_complete_2, family = binomial(link = "logit"))
summary(ga_2cat_employeesboss)
adjR2(ga_2cat_employeesboss)

# S23_new supervision of other workers
bi_ga_2cat_s23 <- glm(gestage_2cat ~ S23_new, data = ga_complete_2, family = binomial(link = "logit"))
summary(bi_ga_2cat_s23)
adjR2(bi_ga_2cat_s23)

ga_2cat_s23 <- glm(gestage_2cat ~ S23_new + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = ga_complete_2, family = binomial(link = "logit"))
summary(ga_2cat_s23)
adjR2(ga_2cat_s23)

# S10.32w_new financial management
bi_ga_2cat_financialmanagement <- glm(gestage_2cat ~ financial_management, data = ga_complete, family = binomial(link = "logit"))
summary(bi_ga_2cat_financialmanagement)
adjR2(bi_ga_2cat_financialmanagement)

ga_2cat_financialmanagement <- glm(gestage_2cat ~ financial_management + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = ga_complete, family = binomial(link = "logit"))
summary(ga_2cat_financialmanagement)
adjR2(ga_2cat_financialmanagement)

# S15_new sex of the partner 
bi_ga_2cat_partner_sex <- glm(gestage_2cat ~ partner_sex, data = ga_complete_2, family = binomial(link = "logit"))
summary(bi_ga_2cat_partner_sex)
adjR2(bi_ga_2cat_partner_sex)

ga_2cat_partner_sex <- glm(gestage_2cat ~ partner_sex + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = ga_complete_2, family = binomial(link = "logit"))
summary(ga_2cat_partner_sex)
adjR2(ga_2cat_partner_sex)

# SGA ####### 
# educational level 
bi_sga_education <- glm(sga_bcnatal_0y_c ~ educ_level_m_3cat, data = ga_complete_2, family = binomial(link = "logit"))
summary(bi_sga_education)
adjR2(bi_sga_education)

#S11_new num of lang
bi_sga_s11_new <- glm(sga_bcnatal_0y_c ~ S11_new, data = ga_complete_2, family = binomial(link = "logit"))
summary(bi_sga_s11_new)
adjR2(bi_sga_s11_new)

sga_s11_new <- glm(sga_bcnatal_0y_c ~ S11_new + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = ga_complete_2, family = binomial(link = "logit"))
summary(sga_s11_new)
adjR2(sga_s11_new)

#S17 employment situation
# active worker 
bi_sga_activeworker <- glm(sga_bcnatal_0y_c ~ active_worker, data = ga_complete_2, family = binomial(link = "logit"))
summary(bi_sga_activeworker)
adjR2(bi_sga_activeworker)

sga_activeworker <- glm(sga_bcnatal_0y_c ~ active_worker + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = ga_complete_2, family = binomial(link = "logit"))
summary(sga_activeworker)
adjR2(sga_activeworker)

# unemployed 
bi_sga_unemployed <- glm(sga_bcnatal_0y_c ~ unemployed, data = ga_complete_2, family = binomial(link = "logit"))
summary(bi_sga_unemployed)
adjR2(bi_sga_unemployed)

sga_unemployed <- glm(sga_bcnatal_0y_c ~ unemployed + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = ga_complete_2, family = binomial(link = "logit"))
summary(sga_unemployed)
adjR2(sga_unemployed)

# others 
bi_sga_otherworkers <- glm(sga_bcnatal_0y_c ~ other_workers_s17, data = ga_complete_2, family = binomial(link = "logit"))
summary(bi_sga_otherworkers)
adjR2(bi_sga_otherworkers)

sga_otherworkers <- glm(sga_bcnatal_0y_c ~ other_workers_s17 + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = ga_complete_2, family = binomial(link = "logit"))
summary(sga_otherworkers)
adjR2(sga_otherworkers)

#s20 employment type
# employee
bi_sga_employee <- glm(sga_bcnatal_0y_c ~ employee, data = ga_complete_2, family = binomial(link = "logit"))
summary(bi_sga_employee)
adjR2(bi_sga_employee)

sga_employee <- glm(sga_bcnatal_0y_c ~ employee + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = ga_complete_2, family = binomial(link = "logit"))
summary(sga_employee)
adjR2(sga_employee)

# self-employee
bi_sga_selfemployee <- glm(sga_bcnatal_0y_c ~ self_employee, data = ga_complete_2, family = binomial(link = "logit"))
summary(bi_sga_selfemployee)
adjR2(bi_sga_selfemployee)

sga_selfemployee <- glm(sga_bcnatal_0y_c ~ self_employee + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = ga_complete_2, family = binomial(link = "logit"))
summary(sga_selfemployee)
adjR2(sga_selfemployee)

# other employees
bi_sga_otheremployees <- glm(sga_bcnatal_0y_c ~ other_employees_s20, data = ga_complete_2, family = binomial(link = "logit"))
summary(bi_sga_otheremployees)
adjR2(bi_sga_otheremployees)

sga_otheremployees <- glm(sga_bcnatal_0y_c ~ other_employees_s20 + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = ga_complete_2, family = binomial(link = "logit"))
summary(sga_otheremployees)
adjR2(sga_otheremployees)

# S21_new people that work for their boss (if employees)
bi_sga_employeesboss <- glm(sga_bcnatal_0y_c ~ employees_boss, data = ga_complete_2, family = binomial(link = "logit"))
summary(bi_sga_employeesboss)
adjR2(bi_sga_employeesboss)

sga_employeesboss <- glm(sga_bcnatal_0y_c ~ employees_boss + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = ga_complete_2, family = binomial(link = "logit"))
summary(sga_employeesboss)
adjR2(sga_employeesboss)

# S23_new supervision of other workers
bi_sga_s23 <- glm(sga_bcnatal_0y_c ~ S23_new, data = ga_complete_2, family = binomial(link = "logit"))
summary(bi_sga_s23)
adjR2(bi_sga_s23)

sga_s23 <- glm(sga_bcnatal_0y_c ~ S23_new + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = ga_complete_2, family = binomial(link = "logit"))
summary(sga_s23)
adjR2(sga_s23)

# S10.32w_new financial management
bi_sga_financialmanagement <- glm(sga_bcnatal_0y_c ~ financial_management, data = ga_complete, family = binomial(link = "logit"))
summary(bi_sga_financialmanagement)
adjR2(bi_sga_financialmanagement)

sga_financialmanagement <- glm(sga_bcnatal_0y_c ~ financial_management + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = ga_complete, family = binomial(link = "logit"))
summary(sga_financialmanagement)
adjR2(sga_financialmanagement)

# S15_new sex of the partner 
bi_sga_partner_sex <- glm(sga_bcnatal_0y_c ~ partner_sex, data = ga_complete_2, family = binomial(link = "logit"))
summary(bi_sga_partner_sex)
adjR2(bi_sga_partner_sex)

sga_partner_sex <- glm(sga_bcnatal_0y_c ~ partner_sex + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = ga_complete_2, family = binomial(link = "logit"))
summary(sga_partner_sex)
adjR2(sga_partner_sex)

## 6) LM :#########################################################

#gestational age in weeks ############

#educational level
bi_ga_weeks_education <- lm(gestage_0y_c_weeks ~ educ_level_m_3cat, data = ga_complete_2)
summary(bi_ga_weeks_education)
adjR2(bi_ga_weeks_education)

#S11_new num of lang
bi_ga_weeks_s11_new <- lm(gestage_0y_c_weeks ~ S11_new, data = ga_complete_2)
summary(bi_ga_weeks_s11_new)
adjR2(bi_ga_weeks_s11_new)

ga_weeks_s11_new <- lm(gestage_0y_c_weeks ~ S11_new + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = ga_complete_2)
summary(ga_weeks_s11_new)
adjR2(ga_weeks_s11_new)

#S17 employment situation
# active worker 
bi_ga_weeks_activeworker <- lm(gestage_0y_c_weeks ~ active_worker, data = ga_complete_2)

summary(bi_ga_weeks_activeworker)
adjR2(bi_ga_weeks_activeworker)

ga_weeks_activeworker <- lm(gestage_0y_c_weeks ~ active_worker + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = ga_complete_2)
summary(ga_weeks_activeworker)
adjR2(ga_weeks_activeworker)

# unemployed 
bi_ga_weeks_unemployed <- lm(gestage_0y_c_weeks ~ unemployed, data = ga_complete_2)
summary(bi_ga_weeks_unemployed)
adjR2(bi_ga_weeks_unemployed)

ga_weeks_unemployed <- lm(gestage_0y_c_weeks ~ unemployed + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = ga_complete_2)
summary(ga_weeks_unemployed)
adjR2(ga_weeks_unemployed)

# others 
bi_ga_weeks_otherworkers <- lm(gestage_0y_c_weeks ~ other_workers_s17, data = ga_complete_2)
summary(bi_ga_weeks_otherworkers)
adjR2(bi_ga_weeks_otherworkers)

ga_weeks_otherworkers <- lm(gestage_0y_c_weeks ~ other_workers_s17 + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = ga_complete_2)
summary(ga_weeks_otherworkers)
adjR2(ga_weeks_otherworkers)

#s20 employment type
# employee
bi_ga_weeks_employee <- lm(gestage_0y_c_weeks ~ employee, data = ga_complete_2)
summary(bi_ga_weeks_employee)
adjR2(bi_ga_weeks_employee)

ga_weeks_employee <- lm(gestage_0y_c_weeks ~ employee + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = ga_complete_2)
summary(ga_weeks_employee)
adjR2(ga_weeks_employee)

# self-employee
bi_ga_weeks_selfemployee <- lm(gestage_0y_c_weeks ~ self_employee, data = ga_complete_2)
summary(bi_ga_weeks_selfemployee)
adjR2(bi_ga_weeks_selfemployee)

ga_weeks_selfemployee <- lm(gestage_0y_c_weeks ~ self_employee + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = ga_complete_2)
summary(ga_weeks_selfemployee)
adjR2(ga_weeks_selfemployee)

# other employees
bi_ga_weeks_otheremployees <- lm(gestage_0y_c_weeks ~ other_employees_s20, data = ga_complete_2)
summary(bi_ga_weeks_otheremployees)
adjR2(bi_ga_weeks_otheremployees)

ga_weeks_otheremployees <- lm(gestage_0y_c_weeks ~ other_employees_s20 + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = ga_complete_2)
summary(ga_weeks_otheremployees)
adjR2(ga_weeks_otheremployees)

# S21_new people that work for their boss (if employees)
bi_ga_weeks_employeesboss <- lm(gestage_0y_c_weeks ~ employees_boss, data = ga_complete_2)
summary(bi_ga_weeks_employeesboss)
adjR2(bi_ga_weeks_employeesboss)

ga_weeks_employeesboss <- lm(gestage_0y_c_weeks ~ employees_boss + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = ga_complete_2)
summary(ga_weeks_employeesboss)
adjR2(ga_weeks_employeesboss)

# S23_new supervision of other workers
bi_ga_weeks_s23 <- lm(gestage_0y_c_weeks ~ S23_new, data = ga_complete_2)
summary(bi_ga_weeks_s23)
adjR2(bi_ga_weeks_s23)

ga_weeks_s23 <- lm(gestage_0y_c_weeks ~ S23_new + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = ga_complete_2)
summary(ga_weeks_s23)
adjR2(ga_weeks_s23)

# S10.32w_new financial management
bi_ga_weeks_financialmanagement <- lm(gestage_0y_c_weeks ~ financial_management, data = ga_complete)
summary(bi_ga_weeks_financialmanagement)
adjR2(bi_ga_weeks_financialmanagement)

ga_weeks_financialmanagement <- lm(gestage_0y_c_weeks ~ financial_management + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = ga_complete)
summary(ga_weeks_financialmanagement)
adjR2(ga_weeks_financialmanagement)

# S15_new sex of the partner 
bi_ga_weeks_partner_sex <- lm(gestage_0y_c_weeks ~ partner_sex, data = ga_complete_2)
summary(bi_ga_weeks_partner_sex)
adjR2(bi_ga_weeks_partner_sex)

ga_weeks_partner_sex <- lm(gestage_0y_c_weeks ~ partner_sex + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = ga_complete_2)
summary(ga_weeks_partner_sex)
adjR2(ga_weeks_partner_sex)

# birth weight in grams ############

#educational level
bi_bw_g_education <- lm(bw_grams ~ educ_level_m_3cat, data = bw_complete_2)
summary(bi_bw_g_education)
adjR2(bi_bw_g_education)

#S11_new num of lang
bi_bw_g_s11_new <- lm(bw_grams ~ S11_new, data = bw_complete_2)
summary(bi_bw_g_s11_new)
adjR2(bi_bw_g_s11_new)

bw_g_s11_new <- lm(bw_grams ~ S11_new + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = bw_complete_2)
summary(bw_g_s11_new)
adjR2(bw_g_s11_new)

#S17 employment situation
# active worker 
bi_bw_g_activeworker <- lm(bw_grams ~ active_worker, data = bw_complete_2)
summary(bi_bw_g_activeworker)
adjR2(bi_bw_g_activeworker)

bw_g_activeworker <- lm(bw_grams ~ active_worker + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = bw_complete_2)
summary(bw_g_activeworker)
adjR2(bw_g_activeworker)

# unemployed 
bi_bw_g_unemployed <- lm(bw_grams ~ unemployed, data = bw_complete_2)
summary(bi_bw_g_unemployed)
adjR2(bi_bw_g_unemployed)

bw_g_unemployed <- lm(bw_grams ~ unemployed + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = bw_complete_2)
summary(bw_g_unemployed)
adjR2(bw_g_unemployed)

# others 
bi_bw_g_otherworkers <- lm(bw_grams ~ other_workers_s17, data = bw_complete_2)
summary(bi_bw_g_otherworkers)
adjR2(bi_bw_g_otherworkers)

bw_g_otherworkers <- lm(bw_grams ~ other_workers_s17 + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = bw_complete_2)
summary(bw_g_otherworkers)
adjR2(bw_g_otherworkers)

#s20 employment type
# employee
bi_bw_g_employee <- lm(bw_grams ~ employee, data = bw_complete_2)
summary(bi_bw_g_employee)
adjR2(bi_bw_g_employee)

bw_g_employee <- lm(bw_grams ~ employee + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = bw_complete_2)
summary(bw_g_employee)
adjR2(bw_g_employee)

# self-employee
bi_bw_g_selfemployee <- lm(bw_grams ~ self_employee, data = bw_complete_2)
summary(bi_bw_g_selfemployee)
adjR2(bi_bw_g_selfemployee)

bw_g_selfemployee <- lm(bw_grams ~ self_employee + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = bw_complete_2)
summary(bw_g_selfemployee)
adjR2(bw_g_selfemployee)

# other employees
bi_bw_g_otheremployees <- lm(bw_grams ~ other_employees_s20, data = bw_complete_2)
summary(bi_bw_g_otheremployees)
adjR2(bi_bw_g_otheremployees)

bw_g_otheremployees <- lm(bw_grams ~ other_employees_s20 + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = bw_complete_2)
summary(bw_g_otheremployees)
adjR2(bw_g_otheremployees)

# S21_new people that work for their boss (if employees)
bi_bw_g_employeesboss <- lm(bw_grams ~ employees_boss, data = bw_complete_2)
summary(bi_bw_g_employeesboss)
adjR2(bi_bw_g_employeesboss)

bw_g_employeesboss <- lm(bw_grams ~ employees_boss + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = bw_complete_2)
summary(bw_g_employeesboss)
adjR2(bw_g_employeesboss)

# S23_new supervision of other workers
bi_bw_g_s23 <- lm(bw_grams ~ S23_new, data = bw_complete_2)
summary(bi_bw_g_s23)
adjR2(bi_bw_g_s23)

bw_g_s23 <- lm(bw_grams ~ S23_new + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = bw_complete_2)
summary(bw_g_s23)
adjR2(bw_g_s23)

# S10.32w_new financial management
bi_bw_g_financialmanagement <- lm(bw_grams ~ financial_management, data = bw_complete)
summary(bi_bw_g_financialmanagement)
adjR2(bi_bw_g_financialmanagement)

bw_g_financialmanagement <- lm(bw_grams ~ financial_management + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = bw_complete)
summary(bw_g_financialmanagement)
adjR2(bw_g_financialmanagement)

# S15_new sex of the partner 
bi_bw_g_partner_sex <- lm(bw_grams ~ partner_sex, data = bw_complete_2)
summary(bi_bw_g_partner_sex)
adjR2(bi_bw_g_partner_sex)

bw_g_partner_sex <- lm(bw_grams ~ partner_sex + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = bw_complete_2)
summary(bw_g_partner_sex)
adjR2(bw_g_partner_sex)

#7) Results table 3: ########
# means and proportions for all bivariate models 

# GA in weeks ##########
# Educational level 
ed_df <- subset(ga_complete_2, educ_level_m_3cat == "Primary or less")
mean_ed_ga <- mean(ed_df$gestage_0y_c_weeks)
mean_ed_ga

ed_df <- subset(ga_complete_2, educ_level_m_3cat == "Secondary")
mean_ed_ga <- mean(ed_df$gestage_0y_c_weeks)
mean_ed_ga

ed_df <- subset(ga_complete_2, educ_level_m_3cat == "University")
mean_ed_ga <- mean(ed_df$gestage_0y_c_weeks)
mean_ed_ga

# Number of languages 
lang_df <- subset(ga_complete_2, S11_new == "One")
mean_language_ga <- mean(lang_df$gestage_0y_c_weeks)
mean_language_ga

lang_df <- subset(ga_complete_2, S11_new == "Two")
mean_language_ga <- mean(lang_df$gestage_0y_c_weeks)
mean_language_ga

lang_df <- subset(ga_complete_2, S11_new == "More than two")
mean_language_ga <- mean(lang_df$gestage_0y_c_weeks)
mean_language_ga

#Employment situation
empl_df <- subset(ga_complete_2, active_worker == "1")
mean_empl_ga <- mean(empl_df$gestage_0y_c_weeks)
mean_empl_ga

empl_df <- subset(ga_complete_2, unemployed == "1")
mean_empl_ga <- mean(empl_df$gestage_0y_c_weeks)
mean_empl_ga

empl_df <- subset(ga_complete_2, other_workers_s17 == "1")
mean_empl_ga <- mean(empl_df$gestage_0y_c_weeks)
mean_empl_ga

#Employment type of active workers
empl_type_df <- subset(ga_complete_2, employee == "1")
mean_empl_type_ga <- mean(empl_type_df$gestage_0y_c_weeks)
mean_empl_type_ga

empl_df <- subset(ga_complete_2, self_employee == "1")
mean_empl_ga <- mean(empl_df$gestage_0y_c_weeks)
mean_empl_ga

empl_df <- subset(ga_complete_2, other_employees_s20 == "1")
mean_empl_ga <- mean(empl_df$gestage_0y_c_weeks)
mean_empl_ga

#Supervision of other workers 
sup_workers_df <- subset(ga_complete_2, S23_new == "Yes")
mean_sup_workers_ga <- mean(sup_workers_df$gestage_0y_c_weeks)
mean_sup_workers_ga

sup_workers_df <- subset(ga_complete_2, S23_new == "No")
mean_sup_workers_ga <- mean(sup_workers_df$gestage_0y_c_weeks)
mean_sup_workers_ga

#employees boss 
mean_gestational_age_by_num_people <- aggregate(gestage_0y_c_weeks ~ employees_boss, data = ga_complete_2, FUN = mean)
mean_gestational_age_by_num_people
summary(mean_gestational_age_by_num_people)

#financial management 
fin_man_df <- subset(ga_complete, financial_management == "We live comfortably")
mean_fin_man_ga <- mean(fin_man_df$gestage_0y_c_weeks)
mean_fin_man_ga

fin_man_df <- subset(ga_complete, financial_management == "We are fine")
mean_fin_man_ga <- mean(fin_man_df$gestage_0y_c_weeks)
mean_fin_man_ga

fin_man_df <- subset(ga_complete, financial_management == "We are as good as we can")
mean_fin_man_ga <- mean(fin_man_df$gestage_0y_c_weeks)
mean_fin_man_ga

fin_man_df <- subset(ga_complete, financial_management == "It is difficult")
mean_fin_man_ga <- mean(fin_man_df$gestage_0y_c_weeks)
mean_fin_man_ga

# bw in grams ##########
# Educational level 
ed_df <- subset(bw_complete_2, educ_level_m_3cat == "Primary or less")
mean_ed_bw <- mean(ed_df$bw_grams)
mean_ed_bw

ed_df <- subset(bw_complete_2, educ_level_m_3cat == "Secondary")
mean_ed_bw <- mean(ed_df$bw_grams)
mean_ed_bw

ed_df <- subset(bw_complete_2, educ_level_m_3cat == "University")
mean_ed_bw <- mean(ed_df$bw_grams)
mean_ed_bw

# Number of languages 
lang_df <- subset(bw_complete_2, S11_new == "One")
mean_language_bw <- mean(lang_df$bw_grams)
mean_language_bw

lang_df <- subset(bw_complete_2, S11_new == "Two")
mean_language_bw <- mean(lang_df$bw_grams)
mean_language_bw

lang_df <- subset(bw_complete_2, S11_new == "More than two")
mean_language_bw <- mean(lang_df$bw_grams)
mean_language_bw

#Employment situation
empl_df <- subset(bw_complete_2, active_worker == "1")
mean_empl_bw <- mean(empl_df$bw_grams)
mean_empl_bw

empl_df <- subset(bw_complete_2, unemployed == "1")
mean_empl_bw <- mean(empl_df$bw_grams)
mean_empl_bw

empl_df <- subset(bw_complete_2, other_workers_s17 == "1")
mean_empl_bw <- mean(empl_df$bw_grams)
mean_empl_bw

#Employment type of active workers
empl_type_df <- subset(bw_complete_2, employee == "1")
mean_empl_type_bw <- mean(empl_type_df$bw_grams)
mean_empl_type_bw

empl_df <- subset(bw_complete_2, self_employee == "1")
mean_empl_bw <- mean(empl_df$bw_grams)
mean_empl_bw

empl_df <- subset(bw_complete_2, other_employees_s20 == "1")
mean_empl_bw <- mean(empl_df$bw_grams)
mean_empl_bw

#Supervision of other workers 
sup_workers_df <- subset(bw_complete_2, S23_new == "Yes")
mean_sup_workers_bw <- mean(sup_workers_df$bw_grams)
mean_sup_workers_bw

sup_workers_df <- subset(bw_complete_2, S23_new == "No")
mean_sup_workers_bw <- mean(sup_workers_df$bw_grams)
mean_sup_workers_bw

#employees boss 
mean_gestational_age_by_num_people <- aggregate(bw_grams ~ employees_boss, data = bw_complete_2, FUN = mean)
mean_gestational_age_by_num_people
summary(mean_gestational_age_by_num_people)

#financial management 
fin_man_df <- subset(bw_complete, financial_management == "We live comfortably")
mean_fin_man_bw <- mean(fin_man_df$bw_grams)
mean_fin_man_bw

fin_man_df <- subset(bw_complete, financial_management == "We are fine")
mean_fin_man_bw <- mean(fin_man_df$bw_grams)
mean_fin_man_bw

fin_man_df <- subset(bw_complete, financial_management == "We are as good as we can")
mean_fin_man_bw <- mean(fin_man_df$bw_grams)
mean_fin_man_bw

fin_man_df <- subset(bw_complete, financial_management == "It is difficult")
mean_fin_man_bw <- mean(fin_man_df$bw_grams)
mean_fin_man_bw

# preterm / non preterm #########
# Educational level 
ed_df <- subset(ga_complete_2, educ_level_m_3cat == "Primary or less")
mean_ed_preterm <- mean(ed_df$gestage_2cat == "preterm")
mean_ed_preterm

ed_df <- subset(ga_complete_2, educ_level_m_3cat == "Secondary")
mean_ed_preterm <- mean(ed_df$gestage_2cat == "preterm")
mean_ed_preterm

ed_df <- subset(ga_complete_2, educ_level_m_3cat == "University")
mean_ed_preterm <- mean(ed_df$gestage_2cat == "preterm")
mean_ed_preterm

# language 
language_df <- subset(ga_complete_2, S11_new == "One")
mean_preterm_language <- mean(language_df$gestage_2cat == "preterm")
mean_preterm_language

language_df <- subset(ga_complete_2, S11_new == "Two")
mean_preterm_language <- mean(language_df$gestage_2cat == "preterm")
mean_preterm_language

language_df <- subset(ga_complete_2, S11_new == "More than two")
mean_preterm_language <- mean(language_df$gestage_2cat == "preterm")
mean_preterm_language

# employment situation 
empl_df <- subset(ga_complete_2, active_worker == "1")
mean_preterm_empl <- mean(empl_df$gestage_2cat == "preterm")
mean_preterm_empl

empl_df <- subset(ga_complete_2, unemployed == "1")
mean_preterm_empl <- mean(empl_df$gestage_2cat == "preterm")
mean_preterm_empl

empl_df <- subset(ga_complete_2, other_workers_s17 == "1")
mean_preterm_empl <- mean(empl_df$gestage_2cat == "preterm")
mean_preterm_empl

# employment type of active workers  
empl_type_df <- subset(ga_complete_2, employee == "1")
mean_preterm_empl_type <- mean(empl_type_df$gestage_2cat == "preterm")
mean_preterm_empl_type

empl_type_df <- subset(ga_complete_2, self_employee == "1")
mean_preterm_empl_type <- mean(empl_type_df$gestage_2cat == "preterm")
mean_preterm_empl_type

empl_type_df <- subset(ga_complete_2, other_employees_s20 == "1")
mean_preterm_empl_type <- mean(empl_type_df$gestage_2cat == "preterm")
mean_preterm_empl_type

# supervision of workers   
sup_workers_df <- subset(ga_complete_2, S23_new == "Yes")
mean_preterm_sup_workers <- mean(sup_workers_df$gestage_2cat == "preterm")
mean_preterm_sup_workers

sup_workers_df <- subset(ga_complete_2, S23_new == "No")
mean_preterm_sup_workers <- mean(sup_workers_df$gestage_2cat == "preterm")
mean_preterm_sup_workers

#employees boss 
mean_preterm_by_num_people <- aggregate(gestage_2cat ~ employees_boss, data = ga_complete_2, FUN = mean)
mean_preterm_by_num_people
summary(mean_preterm_by_num_people)

#financial management 
fin_man_df <- subset(ga_complete, financial_management == "We live comfortably")
mean_fin_man_bw <- mean(fin_man_df$gestage_2cat == "preterm")
mean_fin_man_bw

fin_man_df <- subset(ga_complete, financial_management == "We are fine")
mean_fin_man_bw <- mean(fin_man_df$gestage_2cat == "preterm")
mean_fin_man_bw

fin_man_df <- subset(ga_complete, financial_management == "We are as good as we can")
mean_fin_man_bw <- mean(fin_man_df$gestage_2cat == "preterm")
mean_fin_man_bw

fin_man_df <- subset(ga_complete, financial_management == "It is difficult")
mean_fin_man_bw <- mean(fin_man_df$gestage_2cat == "preterm")
mean_fin_man_bw

# sga / non sga #########
# Educational level 
ed_df <- subset(ga_complete_2, educ_level_m_3cat == "Primary or less")
mean_edsga <- mean(ed_df$sga_bcnatal_0y_c == "sga")
mean_edsga

ed_df <- subset(ga_complete_2, educ_level_m_3cat == "Secondary")
mean_edsga <- mean(ed_df$sga_bcnatal_0y_c == "sga")
mean_edsga

ed_df <- subset(ga_complete_2, educ_level_m_3cat == "University")
mean_edsga <- mean(ed_df$sga_bcnatal_0y_c == "sga")
mean_edsga

# language 
ed_df <- subset(ga_complete_2, S11_new == "One")
mean_sga_language <- mean(language_df$sga_bcnatal_0y_c == "sga")
mean_sga_language

language_df <- subset(ga_complete_2, S11_new == "Two")
mean_sga_language <- mean(language_df$sga_bcnatal_0y_c == "sga")
mean_sga_language

language_df <- subset(ga_complete_2, S11_new == "More than two")
mean_sga_language <- mean(language_df$sga_bcnatal_0y_c == "sga")
mean_sga_language

# employment situation 
empl_df <- subset(ga_complete_2, active_worker == "1")
mean_sga_empl <- mean(empl_df$sga_bcnatal_0y_c == "sga")
mean_sga_empl

empl_df <- subset(ga_complete_2, unemployed == "1")
mean_sga_empl <- mean(empl_df$sga_bcnatal_0y_c == "sga")
mean_sga_empl

empl_df <- subset(ga_complete_2, other_workers_s17 == "1")
mean_sga_empl <- mean(empl_df$sga_bcnatal_0y_c == "sga")
mean_sga_empl

# employment type of active workers  
empl_type_df <- subset(ga_complete_2, employee == "1")
mean_sga_empl_type <- mean(empl_type_df$sga_bcnatal_0y_c == "sga")
mean_sga_empl_type

empl_type_df <- subset(ga_complete_2, self_employee == "1")
mean_sga_empl_type <- mean(empl_type_df$sga_bcnatal_0y_c == "sga")
mean_sga_empl_type

empl_type_df <- subset(ga_complete_2, other_employees_s20 == "1")
mean_sga_empl_type <- mean(empl_type_df$sga_bcnatal_0y_c == "sga")
mean_sga_empl_type

# supervision of workers   
sup_workers_df <- subset(ga_complete_2, S23_new == "Yes")
mean_sga_sup_workers <- mean(sup_workers_df$sga_bcnatal_0y_c == "sga")
mean_sga_sup_workers

sup_workers_df <- subset(ga_complete_2, S23_new == "No")
mean_sga_sup_workers <- mean(sup_workers_df$sga_bcnatal_0y_c == "sga")
mean_sga_sup_workers

#employees boss 
mean_sga_by_num_people <- aggregate(sga_bcnatal_0y_c ~ employees_boss, data = ga_complete_2, FUN = mean)
mean_sga_by_num_people
summary(mean_sga_by_num_people)

#financial management 
fin_man_df <- subset(ga_complete, financial_management == "We live comfortably")
mean_fin_man_bw <- mean(fin_man_df$sga_bcnatal_0y_c == "sga")
mean_fin_man_bw

fin_man_df <- subset(ga_complete, financial_management == "We are fine")
mean_fin_man_bw <- mean(fin_man_df$sga_bcnatal_0y_c == "sga")
mean_fin_man_bw

fin_man_df <- subset(ga_complete, financial_management == "We are as good as we can")
mean_fin_man_bw <- mean(fin_man_df$sga_bcnatal_0y_c == "sga")
mean_fin_man_bw

fin_man_df <- subset(ga_complete, financial_management == "It is difficult")
mean_fin_man_bw <- mean(fin_man_df$sga_bcnatal_0y_c == "sga")
mean_fin_man_bw

# LBW / non LBW #########
# language 
language_df <- subset(bw_complete_2, S11_new == "One")
mean_LBW_language <- mean(language_df$bw_2cat == "LBW")
mean_LBW_language

language_df <- subset(bw_complete_2, S11_new == "Two")
mean_LBW_language <- mean(language_df$bw_2cat == "LBW")
mean_LBW_language

language_df <- subset(bw_complete_2, S11_new == "More than two")
mean_LBW_language <- mean(language_df$bw_2cat == "LBW")
mean_LBW_language

# employment situation 
empl_df <- subset(bw_complete_2, active_worker == "1")
mean_LBW_empl <- mean(empl_df$bw_2cat == "LBW")
mean_LBW_empl

empl_df <- subset(bw_complete_2, unemployed == "1")
mean_LBW_empl <- mean(empl_df$bw_2cat == "LBW")
mean_LBW_empl

empl_df <- subset(bw_complete_2, other_workers_s17 == "1")
mean_LBW_empl <- mean(empl_df$bw_2cat == "LBW")
mean_LBW_empl

# employment type of active workers  
empl_type_df <- subset(bw_complete_2, employee == "1")
mean_LBW_empl_type <- mean(empl_type_df$bw_2cat == "LBW")
mean_LBW_empl_type

empl_type_df <- subset(bw_complete_2, self_employee == "1")
mean_LBW_empl_type <- mean(empl_type_df$bw_2cat == "LBW")
mean_LBW_empl_type

empl_type_df <- subset(bw_complete_2, other_employees_s20 == "1")
mean_LBW_empl_type <- mean(empl_type_df$bw_2cat == "LBW")
mean_LBW_empl_type

# supervision of workers   
sup_workers_df <- subset(bw_complete_2, S23_new == "Yes")
mean_LBW_sup_workers <- mean(sup_workers_df$bw_2cat == "LBW")
mean_LBW_sup_workers

sup_workers_df <- subset(bw_complete_2, S23_new == "No")
mean_LBW_sup_workers <- mean(sup_workers_df$bw_2cat == "LBW")
mean_LBW_sup_workers

#employees boss 
mean_LBW_by_num_people <- aggregate(bw_2cat ~ employees_boss, data = bw_complete_2, FUN = mean)
mean_LBW_by_num_people
summary(mean_LBW_by_num_people)

#financial management 
fin_man_df <- subset(bw_complete, financial_management == "We live comfortably")
mean_fin_man_bw <- mean(fin_man_df$bw_2cat == "LBW")
mean_fin_man_bw

fin_man_df <- subset(bw_complete, financial_management == "We are fine")
mean_fin_man_bw <- mean(fin_man_df$bw_2cat == "LBW")
mean_fin_man_bw

fin_man_df <- subset(bw_complete, financial_management == "We are as good as we can")
mean_fin_man_bw <- mean(fin_man_df$bw_2cat == "LBW")
mean_fin_man_bw

fin_man_df <- subset(bw_complete, financial_management == "It is difficult")
mean_fin_man_bw <- mean(fin_man_df$bw_2cat == "LBW")
mean_fin_man_bw

#8) Results table 4: #########

# b coefficient and 95% CI calculation for all multivariate models
#install.packages("broom")
library(broom)
library(dplyr)

# 8.1) bw in 2 cat ######
# num of languages #########
results <- tidy(bw_3cat_s11_new, conf.int = TRUE)

variables_of_interest <- c("S11_newTwo", "S11_newMore than two", "edad", "merged_parity1", 
                           "merged_parity2", "merged_parity3 or more", "ethnicity_m_3catlat_amr", 
                           "ethnicity_m_3catother", "educ_level_m_3catSecondary", 
                           "educ_level_m_3catUniversity", "bmi_m_32w", 
                           "covid_pandemic_mbefore_pandemic", "covid_pandemic_mmid_pandemic_1", 
                           "covid_pandemic_mmid_pandemic_2")

formatted_results <- results %>%
  filter(term %in% variables_of_interest) %>%
  mutate(`Coefficient (95% CI)` = sprintf("%.2f (%.2f, %.2f)", estimate, conf.low, conf.high)) %>%
  select(term, `Coefficient (95% CI)`) %>%
  rename(Variable = term)

print(formatted_results)

summary(bw_3cat_s11_new)
adjR2(bw_3cat_s11_new)

# employment situation #########
# merged   
bi_bw_3cat_employment_type <- glm(bw_2cat ~ active_worker + unemployed + other_workers_s17, data = bw_complete_2, family = binomial(link = "logit"))
bw_3cat_employment_type <- glm(bw_2cat ~ active_worker + unemployed + other_workers_s17 + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = bw_complete_2, family = binomial(link = "logit"))

results <- tidy(bw_3cat_employment_type, conf.int = TRUE)

variables_of_interest <- c("active_worker1", "unemployed1", "edad", "merged_parity1", 
                           "merged_parity2", "merged_parity3 or more", "ethnicity_m_3catlat_amr", 
                           "ethnicity_m_3catother", "educ_level_m_3catSecondary", 
                           "educ_level_m_3catUniversity", "bmi_m_32w", 
                           "covid_pandemic_mbefore_pandemic", "covid_pandemic_mmid_pandemic_1", 
                           "covid_pandemic_mmid_pandemic_2")

formatted_results <- results %>%
  filter(term %in% variables_of_interest) %>%
  mutate(`Coefficient (95% CI)` = sprintf("%.2f (%.2f, %.2f)", estimate, conf.low, conf.high)) %>%
  select(term, `Coefficient (95% CI)`) %>%
  rename(Variable = term)

print(formatted_results)

summary(bw_3cat_employment_type)
adjR2(bw_3cat_employment_type)

# employment type of active workers #########
# merged 
bi_bw_3cat_activeworker_type <- glm(bw_2cat ~ employee + self_employee + other_employees_s20, data = bw_complete_2, family = binomial(link = "logit"))
bw_3cat_activeworker_type <- glm(bw_2cat ~ employee + self_employee + other_employees_s20 + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = bw_complete_2, family = binomial(link = "logit"))

results <- tidy(bw_3cat_activeworker_type, conf.int = TRUE)

variables_of_interest <- c("employee1", "self_employee1", "edad", "merged_parity1", 
                           "merged_parity2", "merged_parity3 or more", "ethnicity_m_3catlat_amr", 
                           "ethnicity_m_3catother", "educ_level_m_3catSecondary", 
                           "educ_level_m_3catUniversity", "bmi_m_32w", 
                           "covid_pandemic_mbefore_pandemic", "covid_pandemic_mmid_pandemic_1", 
                           "covid_pandemic_mmid_pandemic_2")

formatted_results <- results %>%
  filter(term %in% variables_of_interest) %>%
  mutate(`Coefficient (95% CI)` = sprintf("%.2f (%.2f, %.2f)", estimate, conf.low, conf.high)) %>%
  select(term, `Coefficient (95% CI)`) %>%
  rename(Variable = term)

print(formatted_results)

summary(bw_3cat_activeworker_type)
adjR2(bw_3cat_activeworker_type)

# num of people that work for their boss if employees #########

results <- tidy(bw_3cat_employeesboss, conf.int = TRUE)

variables_of_interest <- c("employees_boss", "edad", "merged_parity1", 
                           "merged_parity2", "merged_parity3 or more", "ethnicity_m_3catlat_amr", 
                           "ethnicity_m_3catother", "educ_level_m_3catSecondary", 
                           "educ_level_m_3catUniversity", "bmi_m_32w", 
                           "covid_pandemic_mbefore_pandemic", "covid_pandemic_mmid_pandemic_1", 
                           "covid_pandemic_mmid_pandemic_2")

formatted_results <- results %>%
  filter(term %in% variables_of_interest) %>%
  mutate(`Coefficient (95% CI)` = sprintf("%.2f (%.2f, %.2f)", estimate, conf.low, conf.high)) %>%
  select(term, `Coefficient (95% CI)`) %>%
  rename(Variable = term)

print(formatted_results)

summary(bw_3cat_employeesboss)
adjR2(bw_3cat_employeesboss)


# S23_new supervision of other workers #########
results <- tidy(bw_3cat_s23, conf.int = TRUE)

variables_of_interest <- c("S23_newNo", "edad", "merged_parity1", 
                           "merged_parity2", "merged_parity3 or more", "ethnicity_m_3catlat_amr", 
                           "ethnicity_m_3catother", "educ_level_m_3catSecondary", 
                           "educ_level_m_3catUniversity", "bmi_m_32w", 
                           "covid_pandemic_mbefore_pandemic", "covid_pandemic_mmid_pandemic_1", 
                           "covid_pandemic_mmid_pandemic_2")

formatted_results <- results %>%
  filter(term %in% variables_of_interest) %>%
  mutate(`Coefficient (95% CI)` = sprintf("%.2f (%.2f, %.2f)", estimate, conf.low, conf.high)) %>%
  select(term, `Coefficient (95% CI)`) %>%
  rename(Variable = term)

print(formatted_results)

summary(bw_3cat_s23)
adjR2(bw_3cat_s23)

# financial management #########
results <- tidy(bw_3cat_financialmanagement, conf.int = TRUE)

variables_of_interest <- c("financial_managementWe are fine", "financial_managementWe are as good as we can", "financial_managementIt is difficult", "edad", "merged_parity1", 
                           "merged_parity2", "merged_parity3 or more", "ethnicity_m_3catlat_amr", 
                           "ethnicity_m_3catother", "educ_level_m_3catSecondary", 
                           "educ_level_m_3catUniversity", "bmi_m_32w", 
                           "covid_pandemic_mbefore_pandemic", "covid_pandemic_mmid_pandemic_1", 
                           "covid_pandemic_mmid_pandemic_2")

formatted_results <- results %>%
  filter(term %in% variables_of_interest) %>%
  mutate(`Coefficient (95% CI)` = sprintf("%.2f (%.2f, %.2f)", estimate, conf.low, conf.high)) %>%
  select(term, `Coefficient (95% CI)`) %>%
  rename(Variable = term)

print(formatted_results)

summary(bw_3cat_financialmanagement)
adjR2(bw_3cat_financialmanagement)

# 8.2) ga in 2 cat ######
# num of languages #########
results <- tidy(ga_2cat_s11_new, conf.int = TRUE)

variables_of_interest <- c("S11_newTwo", "S11_newMore than two", "edad", "merged_parity1", 
                           "merged_parity2", "merged_parity3 or more", "ethnicity_m_3catlat_amr", 
                           "ethnicity_m_3catother", "educ_level_m_3catSecondary", 
                           "educ_level_m_3catUniversity", "bmi_m_32w", 
                           "covid_pandemic_mbefore_pandemic", "covid_pandemic_mmid_pandemic_1", 
                           "covid_pandemic_mmid_pandemic_2")

formatted_results <- results %>%
  filter(term %in% variables_of_interest) %>%
  mutate(`Coefficient (95% CI)` = sprintf("%.2f (%.2f, %.2f)", estimate, conf.low, conf.high)) %>%
  select(term, `Coefficient (95% CI)`) %>%
  rename(Variable = term)

print(formatted_results)

summary(ga_2cat_s11_new)
adjR2(ga_2cat_s11_new)

# employment situation #########
# merged   
bi_ga_2cat_employment_type <- glm(gestage_2cat ~ active_worker + unemployed + other_workers_s17, data = ga_complete_2, family = binomial(link = "logit"))
ga_2cat_employment_type <- glm(gestage_2cat ~ active_worker + unemployed + other_workers_s17 + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = ga_complete_2, family = binomial(link = "logit"))

results <- tidy(ga_2cat_employment_type, conf.int = TRUE)

variables_of_interest <- c("active_worker1", "unemployed1", "edad", "merged_parity1", 
                           "merged_parity2", "merged_parity3 or more", "ethnicity_m_3catlat_amr", 
                           "ethnicity_m_3catother", "educ_level_m_3catSecondary", 
                           "educ_level_m_3catUniversity", "bmi_m_32w", 
                           "covid_pandemic_mbefore_pandemic", "covid_pandemic_mmid_pandemic_1", 
                           "covid_pandemic_mmid_pandemic_2")

formatted_results <- results %>%
  filter(term %in% variables_of_interest) %>%
  mutate(`Coefficient (95% CI)` = sprintf("%.2f (%.2f, %.2f)", estimate, conf.low, conf.high)) %>%
  select(term, `Coefficient (95% CI)`) %>%
  rename(Variable = term)

print(formatted_results)

summary(ga_2cat_employment_type)
adjR2(ga_2cat_employment_type)

# employment type of active workers #########
# merged 
bi_ga_2cat_activeworker_type <- glm(gestage_2cat ~ employee + self_employee + other_employees_s20, data = ga_complete_2, family = binomial(link = "logit"))
ga_2cat_activeworker_type <- glm(gestage_2cat ~ employee + self_employee + other_employees_s20 + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = ga_complete_2, family = binomial(link = "logit"))

results <- tidy(ga_2cat_activeworker_type, conf.int = TRUE)

variables_of_interest <- c("employee1", "self_employee1", "edad", "merged_parity1", 
                           "merged_parity2", "merged_parity3 or more", "ethnicity_m_3catlat_amr", 
                           "ethnicity_m_3catother", "educ_level_m_3catSecondary", 
                           "educ_level_m_3catUniversity", "bmi_m_32w", 
                           "covid_pandemic_mbefore_pandemic", "covid_pandemic_mmid_pandemic_1", 
                           "covid_pandemic_mmid_pandemic_2")

formatted_results <- results %>%
  filter(term %in% variables_of_interest) %>%
  mutate(`Coefficient (95% CI)` = sprintf("%.2f (%.2f, %.2f)", estimate, conf.low, conf.high)) %>%
  select(term, `Coefficient (95% CI)`) %>%
  rename(Variable = term)

print(formatted_results)

summary(ga_2cat_activeworker_type)
adjR2(ga_2cat_activeworker_type)

# num of people that work for their boss if employees #########

results <- tidy(ga_2cat_employeesboss, conf.int = TRUE)

variables_of_interest <- c("employees_boss", "edad", "merged_parity1", 
                           "merged_parity2", "merged_parity3 or more", "ethnicity_m_3catlat_amr", 
                           "ethnicity_m_3catother", "educ_level_m_3catSecondary", 
                           "educ_level_m_3catUniversity", "bmi_m_32w", 
                           "covid_pandemic_mbefore_pandemic", "covid_pandemic_mmid_pandemic_1", 
                           "covid_pandemic_mmid_pandemic_2")

formatted_results <- results %>%
  filter(term %in% variables_of_interest) %>%
  mutate(`Coefficient (95% CI)` = sprintf("%.2f (%.2f, %.2f)", estimate, conf.low, conf.high)) %>%
  select(term, `Coefficient (95% CI)`) %>%
  rename(Variable = term)

print(formatted_results)

summary(ga_2cat_employeesboss)
adjR2(ga_2cat_employeesboss)


# S23_new supervision of other workers #########
results <- tidy(ga_2cat_s23, conf.int = TRUE)

variables_of_interest <- c("S23_newNo", "edad", "merged_parity1", 
                           "merged_parity2", "merged_parity3 or more", "ethnicity_m_3catlat_amr", 
                           "ethnicity_m_3catother", "educ_level_m_3catSecondary", 
                           "educ_level_m_3catUniversity", "bmi_m_32w", 
                           "covid_pandemic_mbefore_pandemic", "covid_pandemic_mmid_pandemic_1", 
                           "covid_pandemic_mmid_pandemic_2")

formatted_results <- results %>%
  filter(term %in% variables_of_interest) %>%
  mutate(`Coefficient (95% CI)` = sprintf("%.2f (%.2f, %.2f)", estimate, conf.low, conf.high)) %>%
  select(term, `Coefficient (95% CI)`) %>%
  rename(Variable = term)

print(formatted_results)

summary(ga_2cat_s23)
adjR2(ga_2cat_s23)

# financial management #########
results <- tidy(ga_2cat_financialmanagement, conf.int = TRUE)

variables_of_interest <- c("financial_managementWe are fine", "financial_managementWe are as good as we can", "financial_managementIt is difficult", "edad", "merged_parity1", 
                           "merged_parity2", "merged_parity3 or more", "ethnicity_m_3catlat_amr", 
                           "ethnicity_m_3catother", "educ_level_m_3catSecondary", 
                           "educ_level_m_3catUniversity", "bmi_m_32w", 
                           "covid_pandemic_mbefore_pandemic", "covid_pandemic_mmid_pandemic_1", 
                           "covid_pandemic_mmid_pandemic_2")

formatted_results <- results %>%
  filter(term %in% variables_of_interest) %>%
  mutate(`Coefficient (95% CI)` = sprintf("%.2f (%.2f, %.2f)", estimate, conf.low, conf.high)) %>%
  select(term, `Coefficient (95% CI)`) %>%
  rename(Variable = term)

print(formatted_results)

summary(ga_2cat_financialmanagement)
adjR2(ga_2cat_financialmanagement)

# 8.3) sga ######
# num of languages #########
results <- tidy(sga_s11_new, conf.int = TRUE)

variables_of_interest <- c("S11_newTwo", "S11_newMore than two", "edad", "merged_parity1", 
                           "merged_parity2", "merged_parity3 or more", "ethnicity_m_3catlat_amr", 
                           "ethnicity_m_3catother", "educ_level_m_3catSecondary", 
                           "educ_level_m_3catUniversity", "bmi_m_32w", 
                           "covid_pandemic_mbefore_pandemic", "covid_pandemic_mmid_pandemic_1", 
                           "covid_pandemic_mmid_pandemic_2")

formatted_results <- results %>%
  filter(term %in% variables_of_interest) %>%
  mutate(`Coefficient (95% CI)` = sprintf("%.2f (%.2f, %.2f)", estimate, conf.low, conf.high)) %>%
  select(term, `Coefficient (95% CI)`) %>%
  rename(Variable = term)

print(formatted_results)

summary(sga_s11_new)
adjR2(sga_s11_new)

# employment situation #########
# merged   
bi_sga_employment_type <- glm(sga_bcnatal_0y_c ~ active_worker + unemployed + other_workers_s17, data = ga_complete_2, family = binomial(link = "logit"))
sga_employment_type <- glm(sga_bcnatal_0y_c ~ active_worker + unemployed + other_workers_s17 + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = ga_complete_2, family = binomial(link = "logit"))

results <- tidy(sga_employment_type, conf.int = TRUE)

variables_of_interest <- c("active_worker1", "unemployed1", "edad", "merged_parity1", 
                           "merged_parity2", "merged_parity3 or more", "ethnicity_m_3catlat_amr", 
                           "ethnicity_m_3catother", "educ_level_m_3catSecondary", 
                           "educ_level_m_3catUniversity", "bmi_m_32w", 
                           "covid_pandemic_mbefore_pandemic", "covid_pandemic_mmid_pandemic_1", 
                           "covid_pandemic_mmid_pandemic_2")

formatted_results <- results %>%
  filter(term %in% variables_of_interest) %>%
  mutate(`Coefficient (95% CI)` = sprintf("%.2f (%.2f, %.2f)", estimate, conf.low, conf.high)) %>%
  select(term, `Coefficient (95% CI)`) %>%
  rename(Variable = term)

print(formatted_results)

summary(sga_employment_type)
adjR2(sga_employment_type)

# employment type of active workers #########
# merged 
bi_sga_activeworker_type <- glm(sga_bcnatal_0y_c ~ employee + self_employee + other_employees_s20, data = ga_complete_2, family = binomial(link = "logit"))
sga_activeworker_type <- glm(sga_bcnatal_0y_c ~ employee + self_employee + other_employees_s20 + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = ga_complete_2, family = binomial(link = "logit"))

results <- tidy(sga_activeworker_type, conf.int = TRUE)

variables_of_interest <- c("employee1", "self_employee1", "edad", "merged_parity1", 
                           "merged_parity2", "merged_parity3 or more", "ethnicity_m_3catlat_amr", 
                           "ethnicity_m_3catother", "educ_level_m_3catSecondary", 
                           "educ_level_m_3catUniversity", "bmi_m_32w", 
                           "covid_pandemic_mbefore_pandemic", "covid_pandemic_mmid_pandemic_1", 
                           "covid_pandemic_mmid_pandemic_2")

formatted_results <- results %>%
  filter(term %in% variables_of_interest) %>%
  mutate(`Coefficient (95% CI)` = sprintf("%.2f (%.2f, %.2f)", estimate, conf.low, conf.high)) %>%
  select(term, `Coefficient (95% CI)`) %>%
  rename(Variable = term)

print(formatted_results)

summary(sga_activeworker_type)
adjR2(sga_activeworker_type)

# num of people that work for their boss if employees #########

results <- tidy(sga_employeesboss, conf.int = TRUE)

variables_of_interest <- c("employees_boss", "edad", "merged_parity1", 
                           "merged_parity2", "merged_parity3 or more", "ethnicity_m_3catlat_amr", 
                           "ethnicity_m_3catother", "educ_level_m_3catSecondary", 
                           "educ_level_m_3catUniversity", "bmi_m_32w", 
                           "covid_pandemic_mbefore_pandemic", "covid_pandemic_mmid_pandemic_1", 
                           "covid_pandemic_mmid_pandemic_2")

formatted_results <- results %>%
  filter(term %in% variables_of_interest) %>%
  mutate(`Coefficient (95% CI)` = sprintf("%.2f (%.2f, %.2f)", estimate, conf.low, conf.high)) %>%
  select(term, `Coefficient (95% CI)`) %>%
  rename(Variable = term)

print(formatted_results)

summary(sga_employeesboss)
adjR2(sga_employeesboss)

# S23_new supervision of other workers #########
summary(sga_s23)

results <- tidy(sga_s23, conf.int = TRUE)

variables_of_interest <- c("S23_newNo", "edad", "merged_parity1", 
                           "merged_parity2", "merged_parity3 or more", "ethnicity_m_3catlat_amr", 
                           "ethnicity_m_3catother", "educ_level_m_3catSecondary", 
                           "educ_level_m_3catUniversity", "bmi_m_32w", 
                           "covid_pandemic_mbefore_pandemic", "covid_pandemic_mmid_pandemic_1", 
                           "covid_pandemic_mmid_pandemic_2")

formatted_results <- results %>%
  filter(term %in% variables_of_interest) %>%
  mutate(`Coefficient (95% CI)` = sprintf("%.2f (%.2f, %.2f)", estimate, conf.low, conf.high)) %>%
  select(term, `Coefficient (95% CI)`) %>%
  rename(Variable = term)

print(formatted_results)

summary(sga_s23)
adjR2(sga_s23)

# financial management #########
results <- tidy(sga_financialmanagement, conf.int = TRUE)

variables_of_interest <- c("financial_managementWe are fine", "financial_managementWe are as good as we can", "financial_managementIt is difficult", "edad", "merged_parity1", 
                           "merged_parity2", "merged_parity3 or more", "ethnicity_m_3catlat_amr", 
                           "ethnicity_m_3catother", "educ_level_m_3catSecondary", 
                           "educ_level_m_3catUniversity", "bmi_m_32w", 
                           "covid_pandemic_mbefore_pandemic", "covid_pandemic_mmid_pandemic_1", 
                           "covid_pandemic_mmid_pandemic_2")

formatted_results <- results %>%
  filter(term %in% variables_of_interest) %>%
  mutate(`Coefficient (95% CI)` = sprintf("%.2f (%.2f, %.2f)", estimate, conf.low, conf.high)) %>%
  select(term, `Coefficient (95% CI)`) %>%
  rename(Variable = term)

print(formatted_results)

summary(sga_financialmanagement)
adjR2(sga_financialmanagement)

# 8.4) ga in weeks ######

# num of languages #########
results <- tidy(ga_weeks_s11_new, conf.int = TRUE)

variables_of_interest <- c("S11_newTwo", "S11_newMore than two", "edad", "merged_parity1", 
                           "merged_parity2", "merged_parity3 or more", "ethnicity_m_3catlat_amr", 
                           "ethnicity_m_3catother", "educ_level_m_3catSecondary", 
                           "educ_level_m_3catUniversity", "bmi_m_32w", 
                           "covid_pandemic_mbefore_pandemic", "covid_pandemic_mmid_pandemic_1", 
                           "covid_pandemic_mmid_pandemic_2")

formatted_results <- results %>%
  filter(term %in% variables_of_interest) %>%
  mutate(`Coefficient (95% CI)` = sprintf("%.2f (%.2f, %.2f)", estimate, conf.low, conf.high)) %>%
  select(term, `Coefficient (95% CI)`) %>%
  rename(Variable = term)

print(formatted_results)

summary(ga_weeks_s11_new)
adjR2(ga_weeks_s11_new)

# employment situation #########
# merged   
bi_ga_weeks_employment_type <- lm(gestage_0y_c_weeks ~ active_worker + unemployed + other_workers_s17, data = ga_complete_2)
ga_weeks_employment_type <- lm(gestage_0y_c_weeks ~ active_worker + unemployed + other_workers_s17 + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = ga_complete_2)

results <- tidy(ga_weeks_employment_type, conf.int = TRUE)

variables_of_interest <- c("active_worker1", "unemployed1", "edad", "merged_parity1", 
                           "merged_parity2", "merged_parity3 or more", "ethnicity_m_3catlat_amr", 
                           "ethnicity_m_3catother", "educ_level_m_3catSecondary", 
                           "educ_level_m_3catUniversity", "bmi_m_32w", 
                           "covid_pandemic_mbefore_pandemic", "covid_pandemic_mmid_pandemic_1", 
                           "covid_pandemic_mmid_pandemic_2")

formatted_results <- results %>%
  filter(term %in% variables_of_interest) %>%
  mutate(`Coefficient (95% CI)` = sprintf("%.2f (%.2f, %.2f)", estimate, conf.low, conf.high)) %>%
  select(term, `Coefficient (95% CI)`) %>%
  rename(Variable = term)

print(formatted_results)

summary(ga_weeks_employment_type)
adjR2(ga_weeks_employment_type)

# employment type of active workers #########
# merged 
bi_ga_weeks_activeworker_type <- lm(gestage_0y_c_weeks ~ employee + self_employee + other_employees_s20, data = ga_complete_2)
ga_weeks_activeworker_type <- lm(gestage_0y_c_weeks ~ employee + self_employee + other_employees_s20 + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = ga_complete_2)

results <- tidy(ga_weeks_activeworker_type, conf.int = TRUE)

variables_of_interest <- c("employee1", "self_employee1", "edad", "merged_parity1", 
                           "merged_parity2", "merged_parity3 or more", "ethnicity_m_3catlat_amr", 
                           "ethnicity_m_3catother", "educ_level_m_3catSecondary", 
                           "educ_level_m_3catUniversity", "bmi_m_32w", 
                           "covid_pandemic_mbefore_pandemic", "covid_pandemic_mmid_pandemic_1", 
                           "covid_pandemic_mmid_pandemic_2")

formatted_results <- results %>%
  filter(term %in% variables_of_interest) %>%
  mutate(`Coefficient (95% CI)` = sprintf("%.2f (%.2f, %.2f)", estimate, conf.low, conf.high)) %>%
  select(term, `Coefficient (95% CI)`) %>%
  rename(Variable = term)

print(formatted_results)

summary(ga_weeks_activeworker_type)
adjR2(ga_weeks_activeworker_type)

# num of people that work for their boss if employees #########

results <- tidy(ga_weeks_employeesboss, conf.int = TRUE)

variables_of_interest <- c("employees_boss", "edad", "merged_parity1", 
                           "merged_parity2", "merged_parity3 or more", "ethnicity_m_3catlat_amr", 
                           "ethnicity_m_3catother", "educ_level_m_3catSecondary", 
                           "educ_level_m_3catUniversity", "bmi_m_32w", 
                           "covid_pandemic_mbefore_pandemic", "covid_pandemic_mmid_pandemic_1", 
                           "covid_pandemic_mmid_pandemic_2")

formatted_results <- results %>%
  filter(term %in% variables_of_interest) %>%
  mutate(`Coefficient (95% CI)` = sprintf("%.2f (%.2f, %.2f)", estimate, conf.low, conf.high)) %>%
  select(term, `Coefficient (95% CI)`) %>%
  rename(Variable = term)

print(formatted_results)

summary(ga_weeks_employeesboss)
adjR2(ga_weeks_employeesboss)

# S23_new supervision of other workers #########
results <- tidy(ga_weeks_s23, conf.int = TRUE)

variables_of_interest <- c("S23_newNo", "edad", "merged_parity1", 
                           "merged_parity2", "merged_parity3 or more", "ethnicity_m_3catlat_amr", 
                           "ethnicity_m_3catother", "educ_level_m_3catSecondary", 
                           "educ_level_m_3catUniversity", "bmi_m_32w", 
                           "covid_pandemic_mbefore_pandemic", "covid_pandemic_mmid_pandemic_1", 
                           "covid_pandemic_mmid_pandemic_2")

formatted_results <- results %>%
  filter(term %in% variables_of_interest) %>%
  mutate(`Coefficient (95% CI)` = sprintf("%.2f (%.2f, %.2f)", estimate, conf.low, conf.high)) %>%
  select(term, `Coefficient (95% CI)`) %>%
  rename(Variable = term)

print(formatted_results)

summary(ga_weeks_s23)
adjR2(ga_weeks_s23)

# financial management #########
results <- tidy(ga_weeks_financialmanagement, conf.int = TRUE)

variables_of_interest <- c("financial_managementWe are fine", "financial_managementWe are as good as we can", "financial_managementIt is difficult", "edad", "merged_parity1", 
                           "merged_parity2", "merged_parity3 or more", "ethnicity_m_3catlat_amr", 
                           "ethnicity_m_3catother", "educ_level_m_3catSecondary", 
                           "educ_level_m_3catUniversity", "bmi_m_32w", 
                           "covid_pandemic_mbefore_pandemic", "covid_pandemic_mmid_pandemic_1", 
                           "covid_pandemic_mmid_pandemic_2")

formatted_results <- results %>%
  filter(term %in% variables_of_interest) %>%
  mutate(`Coefficient (95% CI)` = sprintf("%.2f (%.2f, %.2f)", estimate, conf.low, conf.high)) %>%
  select(term, `Coefficient (95% CI)`) %>%
  rename(Variable = term)

print(formatted_results)

summary(ga_weeks_financialmanagement)
adjR2(ga_weeks_financialmanagement)

# 8.5) bw in grams ######
# num of languages #########
results <- tidy(bw_g_s11_new, conf.int = TRUE)

variables_of_interest <- c("S11_newTwo", "S11_newMore than two", "edad", "merged_parity1", 
                           "merged_parity2", "merged_parity3 or more", "ethnicity_m_3catlat_amr", 
                           "ethnicity_m_3catother", "educ_level_m_3catSecondary", 
                           "educ_level_m_3catUniversity", "bmi_m_32w", 
                           "covid_pandemic_mbefore_pandemic", "covid_pandemic_mmid_pandemic_1", 
                           "covid_pandemic_mmid_pandemic_2")

formatted_results <- results %>%
  filter(term %in% variables_of_interest) %>%
  mutate(`Coefficient (95% CI)` = sprintf("%.2f (%.2f, %.2f)", estimate, conf.low, conf.high)) %>%
  select(term, `Coefficient (95% CI)`) %>%
  rename(Variable = term)

print(formatted_results)

summary(bw_g_s11_new)
adjR2(bw_g_s11_new)

# employment situation #########
# merged   
bi_bw_g_employment_type <- lm(bw_grams ~ active_worker + unemployed + other_workers_s17, data = bw_complete_2)
bw_g_employment_type <- lm(bw_grams ~ active_worker + unemployed + other_workers_s17 + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = bw_complete_2)

results <- tidy(bw_g_employment_type, conf.int = TRUE)

variables_of_interest <- c("active_worker1", "unemployed1", "edad", "merged_parity1", 
                           "merged_parity2", "merged_parity3 or more", "ethnicity_m_3catlat_amr", 
                           "ethnicity_m_3catother", "educ_level_m_3catSecondary", 
                           "educ_level_m_3catUniversity", "bmi_m_32w", 
                           "covid_pandemic_mbefore_pandemic", "covid_pandemic_mmid_pandemic_1", 
                           "covid_pandemic_mmid_pandemic_2")

formatted_results <- results %>%
  filter(term %in% variables_of_interest) %>%
  mutate(`Coefficient (95% CI)` = sprintf("%.2f (%.2f, %.2f)", estimate, conf.low, conf.high)) %>%
  select(term, `Coefficient (95% CI)`) %>%
  rename(Variable = term)

print(formatted_results)

summary(bw_g_employment_type)
adjR2(bw_g_employment_type)

# employment type of active workers #########
# merged 
bi_bw_g_activeworker_type <- lm(bw_grams ~ employee + self_employee + other_employees_s20, data = bw_complete_2)
bw_g_activeworker_type <- lm(bw_grams ~ employee + self_employee + other_employees_s20 + educ_level_m_3cat + merged_parity + ethnicity_m_3cat + bmi_m_32w + edad + covid_pandemic_m, data = bw_complete_2)

results <- tidy(bw_g_activeworker_type, conf.int = TRUE)

variables_of_interest <- c("employee1", "self_employee1", "edad", "merged_parity1", 
                           "merged_parity2", "merged_parity3 or more", "ethnicity_m_3catlat_amr", 
                           "ethnicity_m_3catother", "educ_level_m_3catSecondary", 
                           "educ_level_m_3catUniversity", "bmi_m_32w", 
                           "covid_pandemic_mbefore_pandemic", "covid_pandemic_mmid_pandemic_1", 
                           "covid_pandemic_mmid_pandemic_2")

formatted_results <- results %>%
  filter(term %in% variables_of_interest) %>%
  mutate(`Coefficient (95% CI)` = sprintf("%.2f (%.2f, %.2f)", estimate, conf.low, conf.high)) %>%
  select(term, `Coefficient (95% CI)`) %>%
  rename(Variable = term)

print(formatted_results)

summary(bw_g_activeworker_type)
adjR2(bw_g_activeworker_type)

# num of people that work for their boss if employees #########

results <- tidy(bw_g_employeesboss, conf.int = TRUE)

variables_of_interest <- c("employees_boss", "edad", "merged_parity1", 
                           "merged_parity2", "merged_parity3 or more", "ethnicity_m_3catlat_amr", 
                           "ethnicity_m_3catother", "educ_level_m_3catSecondary", 
                           "educ_level_m_3catUniversity", "bmi_m_32w", 
                           "covid_pandemic_mbefore_pandemic", "covid_pandemic_mmid_pandemic_1", 
                           "covid_pandemic_mmid_pandemic_2")

formatted_results <- results %>%
  filter(term %in% variables_of_interest) %>%
  mutate(`Coefficient (95% CI)` = sprintf("%.2f (%.2f, %.2f)", estimate, conf.low, conf.high)) %>%
  select(term, `Coefficient (95% CI)`) %>%
  rename(Variable = term)

print(formatted_results)

summary(bw_g_employeesboss)
adjR2(bw_g_employeesboss)


# S23_new supervision of other workers #########
results <- tidy(bw_g_s23, conf.int = TRUE)

variables_of_interest <- c("S23_newNo", "edad", "merged_parity1", 
                           "merged_parity2", "merged_parity3 or more", "ethnicity_m_3catlat_amr", 
                           "ethnicity_m_3catother", "educ_level_m_3catSecondary", 
                           "educ_level_m_3catUniversity", "bmi_m_32w", 
                           "covid_pandemic_mbefore_pandemic", "covid_pandemic_mmid_pandemic_1", 
                           "covid_pandemic_mmid_pandemic_2")

formatted_results <- results %>%
  filter(term %in% variables_of_interest) %>%
  mutate(`Coefficient (95% CI)` = sprintf("%.2f (%.2f, %.2f)", estimate, conf.low, conf.high)) %>%
  select(term, `Coefficient (95% CI)`) %>%
  rename(Variable = term)

print(formatted_results)

summary(bw_g_s23)
adjR2(bw_g_s23)

# financial management #########
results <- tidy(bw_g_financialmanagement, conf.int = TRUE)

variables_of_interest <- c("financial_managementWe are fine", "financial_managementWe are as good as we can", "financial_managementIt is difficult", "edad", "merged_parity1", 
                           "merged_parity2", "merged_parity3 or more", "ethnicity_m_3catlat_amr", 
                           "ethnicity_m_3catother", "educ_level_m_3catSecondary", 
                           "educ_level_m_3catUniversity", "bmi_m_32w", 
                           "covid_pandemic_mbefore_pandemic", "covid_pandemic_mmid_pandemic_1", 
                           "covid_pandemic_mmid_pandemic_2")

formatted_results <- results %>%
  filter(term %in% variables_of_interest) %>%
  mutate(`Coefficient (95% CI)` = sprintf("%.2f (%.2f, %.2f)", estimate, conf.low, conf.high)) %>%
  select(term, `Coefficient (95% CI)`) %>%
  rename(Variable = term)

print(formatted_results)

summary(bw_g_financialmanagement)
adjR2(bw_g_financialmanagement)
