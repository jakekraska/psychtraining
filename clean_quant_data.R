### load libraries

library(tidyr)
library(dplyr)
library(stringr)

###  load data

data <- read.csv("quant_data_raw.csv", stringsAsFactors = FALSE, na.strings = c("", "NA")) ### load raw data into a data frame
data <- data[-c(1:2),-c(1:17)] ### remove first two rows
data <- data[!apply(is.na(data) | data == "", 1, all),] # remove empty rows
data <- droplevels(data) ### reset the factor levels

### label questions

names(data) <- c("age", "gender", "currentRegistration", "aope", "trainingPathway",
                 "yearsExperience", "supervisor", "workSetting", "clientAge", 
                 paste0("trainingPathway_",1:29), paste0("endorsements_",1:13),
                 paste0("betterAccess_",1:17))

### redo agreement

data[data == "Strongly disagree"] <- 1
data[data == "Somewhat disagree"] <- 2
data[data == "Neither agree nor disagree"] <- 3
data[data == "Somewhat agree"] <- 4
data[data == "Strongly agree"] <- 5

# Make strings shorter and recode data
data$aope <- str_replace(data$aope,"Educational and Developmental","Ed&Dev") # make string shorter
data$aope <- str_replace(data$aope,"Sport and Exercise","S&E") # make string shorter
data$yearsExperience <- str_replace(data$yearsExperience, "N/A \\(still Provisionally registered\\)", "Provisional") # make string shorter
data$currentRegistration <- str_replace(data$currentRegistration, " Registration","")
data$trainingPathway <- recode(data$trainingPathway,
                               "4+2 Internship" = "4+2",
                               "5+1 Internship" = "5+1",
                               "Combined Masters/PhD (MPsych/PhD)" = "MPsych/PhD",
                               "N/A (still Provisionally registered)" = "Ongoing",
                               "Professional Doctorate (DPsych)" = "DPsych",
                               "Professional Masters Degree (MPsych)" = "MPsych")

### write data

write.csv(x = data, file = "quant_data_clean.csv", row.names = FALSE)
