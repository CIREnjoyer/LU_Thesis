library(ggplot2)
library(readxl)
library(ggResidpanel)
library(broom)
library(pwrss)
library(modelsummary)
library(dplyr)
library(httr)
library(performance)
library(psych)
library(correlation)
library(marginaleffects)
library(lme4)
library(tableone)
library(expss)

###### Data Management

dta0 <- read.csv("https://raw.githubusercontent.com/CIREnjoyer/LU_Thesis/refs/heads/main/Thesis/Data.csv")
nats <- read.csv("https://gist.githubusercontent.com/marijn/274449/raw/0045fb5f54f9ad357e301cf30e23d9834058618a/nationalities.csv")

dta <- dta0[-c(1:2),]

names <- colnames(dta[,c(14:23, 26:30, 37:43, 47)])
natio <- colnames(nats)

for (i in names) {
  
  dta[[i]] <- as.numeric(dta[[i]])
  
}

dta <- dta |> #recoding variables
  mutate(open = ((q5 + (8 - q10_orig))/2),
         cons = ((q3 + (8 - q8_orig))/2),
         extr = ((q1 + (8 - q6_orig))/2),
         agr = (((8 - q2_orig) + q7)/2),
         neur = ((q4 + (8 - q9_orig))/2),
         q11 = coalesce(c11, t11),
         q12 = coalesce(c12, t12),
         q13 = coalesce(c13, t13),
         q14 = coalesce(c14, t14),
         q15 = coalesce(c15, t15),
         ethn = rowMeans(across(c(q11,q13)), na.rm = T),
         civ = rowMeans(across(c(q12,q14,q15)), na.rm = T),
         group = factor(group),
         gndr = factor(gndr, levels = c(1:4), labels = c("Male", "Female", "Non-binary", "Prefer")),
         psychback = factor(psychback, labels = c("No", "Yes")),
         nationality = factor(nationality, levels = c(1:194), labels = c(paste(natio))),
         educ = as.numeric(educ),
         educ = (educ - 1),
         educ = case_when(
           educ == 1 ~ 2,
           .default = educ
         ),
         educ_f = factor(educ, levels = c(1:5), labels = c("Less than High School", "High School", "Bachelor's", "Master's", "Doctorate")),
         region = case_when(
         nationality %in% c("Dutch", "French", "German", "Italian", "Austrian",
                              "Belgian", "Finnish", "Irish", "Portuguese", "Spanish",
                              "Danish", "Swedish", "Norwegian", "British", 
                              "Cypriot") ~ "Western Europe",
         nationality %in% c("Ukrainian", "Romanian", "Bulgarian", "Polish",
                              "Russian", "Czech", "Croatian", "Bosnian", "Estonian",
                              "Hungarian", "Albanian", "Armenian", "Azerbaijani",
                              "Belarusian", "Georgian", "Herzegovinian") ~ "Eastern Europe",
         TRUE ~ "Other"),
         region = factor(region, levels = c("Western Europe", 
                                            "Eastern Europe", 
                                            "Other"))
         
  ) |>
  rename(grp = group)

dta1 <- dta |>
  filter(check <= 3 | is.na(check)) #retain the cases that pass the treatment check

summary(lm(civ ~ as.factor(educ), data = dta1)) #checking if there is difference in factor or numeric for educ
summary(lm(civ ~ educ, data = dta1))

summary(lm(ethn ~ as.factor(educ), data = dta1))
summary(lm(ethn ~ educ, data = dta1))

###### Data Presentation

#predictors
datasummary_skim(dta1[, c("open", "cons", "extr", "agr", "neur", "age", "lrscale_1")], type = "numeric",
                 fun_numeric = list(Min = min, Mean = mean, SD = sd, Max = max, "Total N" = length))

#cat predictors
datasummary_skim(dta1[, c("educ_f", "gndr")])

#outcomes
datasummary_skim(dta1[, c("civ", "ethn")], type = "numeric",
                 fun_numeric = list(Min = min, Mean = mean, SD = sd, Max = max, "Total N" = length))

#visualise separation by group
CreateTableOne(vars = c("open", "cons", "extr", "agr", "neur", "age", "lrscale_1", "educ_f", "gndr", "region"),
               strata = "grp",
               data = dta1)

#correlations
x <- correlation(dta1[, 14:23],
                 ci = 0.95)

datasummary_correlation(x,
                        stars = T)


###### Models

#robusteness checks for cons
summary(lm(civ ~ grp, data = dta1))
summary(lm(civ ~ grp + cons, data = dta1))

summary(lm(ethn ~ grp, data = dta1))
summary(lm(ethn ~ grp + cons, data = dta1))

#check for dependencies in the treatment effectivenss
summary(lm(check ~ open + cons + extr + agr + neur + psychback + lrscale_1 + age + gndr + educ + region, data = dta1))

#clean models with interactions
model1 <- lm(civ ~ open*grp + cons*grp + extr*grp + agr*grp + neur*grp, data = dta1)

model2 <- lm(ethn ~ open*grp + cons*grp + extr*grp + agr*grp + neur*grp, data = dta1)

modelsummary(list(model1, model2),
             stars = T)

#assumptions check
check_collinearity(model1)
check_heteroscedasticity(model1)
resid_panel(model1, plots = c("resid"))
resid_panel(model1, plots = c("hist", "qq"))
resid_panel(model1, plots = "cookd")
model1_aug <- augment(model1)
summary(model1_aug$.std.resid)
values <- c(1.96, 2.56, 3.29)

for (i in values) {
  
  model1_aug <- model1_aug |>
    mutate(err = case_when(
      abs(.std.resid) > i ~ 1,
      .default = 0
    ))
  
  print(
    
    fre(model1_aug$err)
    
  )
  
}

check_collinearity(model2)
check_heteroscedasticity(model2)
resid_panel(model2, plots = c("resid"))
resid_panel(model2, plots = c("hist", "qq"))
resid_panel(model2, plots = "cookd")
model2_aug <- augment(model2)
summary(model2_aug$.std.resid)
values <- c(1.96, 2.56, 3.29)

for (i in values) {
  
  model2_aug <- model2_aug |>
    mutate(err = case_when(
      abs(.std.resid) > i ~ 1,
      .default = 0
    ))
  
  print(
    
    fre(model2_aug$err)
    
  )
  
}

#full models with interactions
model3 <- lm(civ ~ open*grp + cons*grp + extr*grp + agr*grp + neur*grp + age + region + educ + gndr + lrscale_1 + psychback, data = dta1)

model4 <- lm(ethn ~ open*grp + cons*grp + extr*grp + agr*grp + neur*grp + age + region + educ + gndr + lrscale_1 + psychback, data = dta1)

modelsummary(list(model3, model4),
             stars = T)

#assumptions checks
check_collinearity(model3)
check_heteroscedasticity(model3)
resid_panel(model3, plots = c("resid"))
resid_panel(model3, plots = c("hist", "qq"))
resid_panel(model3, plots = "cookd")
model3_aug <- augment(model3)
summary(model3_aug$.std.resid)
values <- c(1.96, 2.56, 3.29)

for (i in values) {
  
  model3_aug <- model3_aug |>
    mutate(err = case_when(
      abs(.std.resid) > i ~ 1,
      .default = 0
    ))
  
  print(
    
    fre(model3_aug$err)
    
  )
  
}

check_collinearity(model4)
check_heteroscedasticity(model4)
resid_panel(model4, plots = c("resid"))
resid_panel(model4, plots = c("hist", "qq"))
resid_panel(model4, plots = "cookd")
model4_aug <- augment(model4)
summary(model4_aug$.std.resid)
values <- c(1.96, 2.56, 3.29)

for (i in values) {
  
  model4_aug <- model4_aug |>
    mutate(err = case_when(
      abs(.std.resid) > i ~ 1,
      .default = 0
    ))
  
  print(
    
    fre(model4_aug$err)
    
  )
  
}

#overview of interactions for civ

plot_predictions(model3,
                 condition = list("open" = 1:7,
                                  "grp" = c("control", "treatment")),
                 newdata = datagrid(open = 1:7,
                                    grp = c("control", "treatment")),
                 conf_level = 0.9)

plot_predictions(model3,
                 condition = list("cons" = 1:7,
                                  "grp" = c("control", "treatment")),
                 newdata = datagrid(cons = 1:7,
                                    grp = c("control", "treatment")),
                 conf_level = 0.9)

plot_predictions(model3,
                 condition = list("extr" = 1:7,
                                  "grp" = c("control", "treatment")),
                 newdata = datagrid(extr = 1:7,
                                    grp = c("control", "treatment")),
                 conf_level = 0.9)

plot_predictions(model3,
                 condition = list("agr" = 1:7,
                                  "grp" = c("control", "treatment")),
                 newdata = datagrid(agr = 1:7,
                                    grp = c("control", "treatment")),
                 conf_level = 0.9)

plot_predictions(model3,
                 condition = list("neur" = 1:7,
                                  "grp" = c("control", "treatment")),
                 newdata = datagrid(neur = 1:7,
                                    grp = c("control", "treatment")),
                 conf_level = 0.9)

#overview of interactions for ethn

plot_predictions(model4,
                 condition = list("open" = 1:7,
                                  "grp" = c("control", "treatment")),
                 newdata = datagrid(open = 1:7,
                                    grp = c("control", "treatment")),
                 conf_level = 0.9)

plot_predictions(model4,
                 condition = list("cons" = 1:7,
                                  "grp" = c("control", "treatment")),
                 newdata = datagrid(cons = 1:7,
                                    grp = c("control", "treatment")),
                 conf_level = 0.9)

plot_predictions(model4,
                 condition = list("extr" = 1:7,
                                  "grp" = c("control", "treatment")),
                 newdata = datagrid(extr = 1:7,
                                    grp = c("control", "treatment")),
                 conf_level = 0.9)

plot_predictions(model3,
                 condition = list("agr" = 1:7,
                                  "grp" = c("control", "treatment")),
                 newdata = datagrid(agr = 1:7,
                                    grp = c("control", "treatment")),
                 conf_level = 0.9)

plot_predictions(model4,
                 condition = list("neur" = 1:7,
                                  "grp" = c("control", "treatment")),
                 newdata = datagrid(neur = 1:7,
                                    grp = c("control", "treatment")),
                 conf_level = 0.9)
