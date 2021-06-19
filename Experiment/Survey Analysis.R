library(data.table)
library(ggplot2)
library(fixest)
library(tidyverse)
library(caret)
library(broom)
library(lfe)
df_raw <- fread("C:/Users/YEET/Documents/BU/Classes/BA 830/Project/results_raw.csv")
df_clean <- fread("C:/Users/YEET/Documents/BU/Classes/BA 830/Project/results_clean.csv")


head(df_clean)


#Randomization Checks

#For Age
t.test(df_clean[Treated == 0, Age], df_clean[Treated == 1, Age])

#Creating one hot encoding for Gender
table(df_clean[,Gender])
dummy <- dummyVars(" ~ Gender", data=df_clean)
newdata <- data.frame(predict(dummy, newdata = df_clean)) 

gender_df <- as.data.table(cbind(newdata, Treated = df_clean[,Treated])) # Combine the columns

t.test(gender_df[Treated == 0, GenderWoman], gender_df[Treated == 1, GenderWoman])
t.test(gender_df[Treated == 0, GenderMan], gender_df[Treated == 1, GenderMan])
t.test(gender_df[Treated == 0, GenderNon.binary], gender_df[Treated == 1, GenderNon.binary])
t.test(gender_df[Treated == 0, GenderAgender], gender_df[Treated == 1, GenderAgender]) # Only one observation
t.test(gender_df[Treated == 0, GenderPrefer.not.to.say], gender_df[Treated == 1, GenderPrefer.not.to.say]) # Only one observation


#English as a first language
df_clean[,Eng_First_Lang_Num := ifelse(df_clean[,English_First_Language] == "Yes", 1, 0)]
t.test(df_clean[Treated == 0, Eng_First_Lang_Num], df_clean[Treated == 1, Eng_First_Lang_Num])

#Students
df_clean[,Student_Num := ifelse(df_clean[, Student] == "Yes", 1, 0)]
t.test(df_clean[Treated == 0, Student_Num], df_clean[Treated == 1, Student_Num])

#Treatment Effect
feols(total_correct ~ Treated, data = df_clean, se = 'white')

df_clean[, Score := total_correct/4]
ITT <- feols(Score ~ Treated, data = df_clean, se = 'white')

#Treatment Effect with Compliers
df_compliers <- subset(df_clean, (Treated == 0 | Treated == 1 & CP_or_T == 'CP'))

feols(Score ~ Treated, data = df_compliers, se = 'white')


df_clean[, compliance := ifelse((df_clean[, Treated] == 1 & CP_or_T == 'T'), 0, 1)]

felm(Score ~ 1 | 0 | (compliance ~ Treated), data = df_clean)

feols(Score ~ 1 | 0 | compliance ~ Treated, data = df_clean, se = "white")

alpha <- mean(df_clean[Treated == 1,compliance])
alpha

tidy_reg <- tidy(ITT)

coefficient <- tidy_reg[2,2]
SE <- tidy_reg$std.error[2]
  
CACE <- coefficient / alpha
CACE_SE <- SE / alpha


