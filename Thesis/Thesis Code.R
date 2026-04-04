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
library(emmeans)
library(patchwork)
library(kableExtra)

setwd("C:/Users/skots/Desktop/Master PoliSci/Master's Thesis/Thesis/latex")
options("modelsummary_format_numeric_latex" = "plain")

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
t1 <- CreateTableOne(vars = c("open", "cons", "extr", "agr", "neur", "civ", "ethn", "age", "lrscale_1", "educ_f", "gndr", "region"),
               strata = "grp",
               data = dta1)

print(t1, printToggle = FALSE) |> 
  kableone(format = "latex")

#correlations
x <- correlation(dta[, 14:23],
                 ci = 0.95)

datasummary_correlation(x,
                        stars = T,
                        output = "latex")


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

#civ models by group
model3.1 <- lm(civ ~ open + cons + extr + agr + neur + age + region + educ + gndr + lrscale_1 + psychback, data = subset(dta1, grp == "control"))
model3.2 <- lm(civ ~ open + cons + extr + agr + neur + age + region + educ + gndr + lrscale_1 + psychback, data = subset(dta1, grp == "treatment"))

modelsummary(list(model3.1, model3.2),
             stars = T)

#ethn models by group
model4.1 <- lm(ethn ~ open + cons + extr + agr + neur + age + region + educ + gndr + lrscale_1 + psychback, data = subset(dta1, grp == "control"))
model4.2 <- lm(ethn ~ open + cons + extr + agr + neur + age + region + educ + gndr + lrscale_1 + psychback, data = subset(dta1, grp == "treatment"))

modelsummary(list(model4.1, model4.2),
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

preds <- emmeans(model3, ~ open * grp, at = list(open = c(1:7)), level = 0.9)
tidy(preds)
predictions <- as.data.frame(preds)

civ_o <- ggplot(predictions, aes(x = open, y = emmean, colour = grp)) + 
  geom_line(size = 1) + 
  geom_ribbon(data = predictions, aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.2) + 
  scale_colour_manual(values = c("darkred", "darkblue")) +
  theme_minimal(20) +
  labs(
    x = "Openness to Experience",
    y = "Importance of Civic Boundary",
    colour = "Group")

preds <- emmeans(model3, ~ cons * grp, at = list(cons = c(1:7)), level = 0.9)
tidy(preds)
predictions <- as.data.frame(preds)

civ_c <- ggplot(predictions, aes(x = cons, y = emmean, colour = grp)) + 
  geom_line(size = 1) + 
  geom_ribbon(data = predictions, aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.2) + 
  scale_colour_manual(values = c("darkred", "darkblue")) +
  theme_minimal(20) +
  labs(
    x = "Conscientiousness",
    y = "Importance of Civic Boundary",
    colour = "Group")

plot_predictions(model3,
                 condition = list("cons" = 1:7,
                                  "grp" = c("control", "treatment")),
                 newdata = datagrid(cons = 1:7,
                                    grp = c("control", "treatment")),
                 conf_level = 0.9)

preds <- emmeans(model3, ~ extr * grp, at = list(extr = c(1:7)), level = 0.9)
tidy(preds)
predictions <- as.data.frame(preds)

civ_e <- ggplot(predictions, aes(x = extr, y = emmean, colour = grp)) + 
  geom_line(size = 1) + 
  geom_ribbon(data = predictions, aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.2) + 
  scale_colour_manual(values = c("darkred", "darkblue")) +
  theme_minimal(20) +
  labs(
    x = "Extraversion",
    y = "Importance of Civic Boundary",
    colour = "Group")

plot_predictions(model3,
                 condition = list("extr" = 1:7,
                                  "grp" = c("control", "treatment")),
                 newdata = datagrid(extr = 1:7,
                                    grp = c("control", "treatment")),
                 conf_level = 0.9)

preds <- emmeans(model3, ~ agr * grp, at = list(agr = c(1:7)), level = 0.9)
tidy(preds)
predictions <- as.data.frame(preds)

civ_a <- ggplot(predictions, aes(x = agr, y = emmean, colour = grp)) + 
  geom_line(size = 1) + 
  geom_ribbon(data = predictions, aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.2) + 
  scale_colour_manual(values = c("darkred", "darkblue")) +
  theme_minimal(20) +
  labs(
    x = "Agreeableness",
    y = "Importance of Civic Boundary",
    colour = "Group")

plot_predictions(model3,
                 condition = list("agr" = 1:7,
                                  "grp" = c("control", "treatment")),
                 newdata = datagrid(agr = 1:7,
                                    grp = c("control", "treatment")),
                 conf_level = 0.9)

preds <- emmeans(model3, ~ neur * grp, at = list(neur = c(1:7)), level = 0.9)
tidy(preds)
predictions <- as.data.frame(preds)

civ_n <- ggplot(predictions, aes(x = neur, y = emmean, colour = grp)) + 
  geom_line(size = 1) + 
  geom_ribbon(data = predictions, aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.2) + 
  scale_colour_manual(values = c("darkred", "darkblue")) +
  theme_minimal(20) +
  labs(
    x = "Neuroticism",
    y = "Importance of Civic Boundary",
    colour = "Group")

plot_predictions(model3,
                 condition = list("neur" = 1:7,
                                  "grp" = c("control", "treatment")),
                 newdata = datagrid(neur = 1:7,
                                    grp = c("control", "treatment")),
                 conf_level = 0.9)

dev.new(width = 12, height = 8)
(civ_o | civ_e) / (civ_a | civ_n)

#overview of interactions for ethn
preds <- emmeans(model4, ~ open * grp, at = list(open = c(1:7)), level = 0.9)
tidy(preds)
predictions <- as.data.frame(preds)

ethn_o <- ggplot(predictions, aes(x = open, y = emmean, colour = grp)) + 
  geom_line(size = 1) + 
  geom_ribbon(data = predictions, aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.2) + 
  scale_colour_manual(values = c("darkred", "darkblue")) +
  theme_minimal(20) +
  labs(
    x = "Openness to Experience",
    y = "Importance of Ethnic Boundary",
    colour = "Group")

plot_predictions(model4,
                 condition = list("open" = 1:7,
                                  "grp" = c("control", "treatment")),
                 newdata = datagrid(open = 1:7,
                                    grp = c("control", "treatment")),
                 conf_level = 0.9)

preds <- emmeans(model4, ~ cons * grp, at = list(cons = c(1:7)), level = 0.9)
tidy(preds)
predictions <- as.data.frame(preds)

ethn_c <- ggplot(predictions, aes(x = cons, y = emmean, colour = grp)) + 
  geom_line(size = 1) + 
  geom_ribbon(data = predictions, aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.2) + 
  scale_colour_manual(values = c("darkred", "darkblue")) +
  theme_minimal(20) +
  labs(
    x = "Conscientiousness",
    y = "Importance of Ethnic Boundary",
    colour = "Group")

plot_predictions(model4,
                 condition = list("cons" = 1:7,
                                  "grp" = c("control", "treatment")),
                 newdata = datagrid(cons = 1:7,
                                    grp = c("control", "treatment")),
                 conf_level = 0.9)

preds <- emmeans(model4, ~ extr * grp, at = list(extr = c(1:7)), level = 0.9)
tidy(preds)
predictions <- as.data.frame(preds)

ethn_e <- ggplot(predictions, aes(x = extr, y = emmean, colour = grp)) + 
  geom_line(size = 1) + 
  geom_ribbon(data = predictions, aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.2) + 
  scale_colour_manual(values = c("darkred", "darkblue")) +
  theme_minimal(20) +
  labs(
    x = "Extraversion",
    y = "Importance of Ethnic Boundary",
    colour = "Group")

plot_predictions(model4,
                 condition = list("extr" = 1:7,
                                  "grp" = c("control", "treatment")),
                 newdata = datagrid(extr = 1:7,
                                    grp = c("control", "treatment")),
                 conf_level = 0.9)

preds <- emmeans(model4, ~ agr * grp, at = list(agr = c(1:7)), level = 0.9)
tidy(preds)
predictions <- as.data.frame(preds)

ethn_a <- ggplot(predictions, aes(x = agr, y = emmean, colour = grp)) + 
  geom_line(size = 1) + 
  geom_ribbon(data = predictions, aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.2) + 
  scale_colour_manual(values = c("darkred", "darkblue")) +
  theme_minimal(20) +
  labs(
    x = "Agreeableness",
    y = "Importance of Ethnic Boundary",
    colour = "Group")

plot_predictions(model3,
                 condition = list("agr" = 1:7,
                                  "grp" = c("control", "treatment")),
                 newdata = datagrid(agr = 1:7,
                                    grp = c("control", "treatment")),
                 conf_level = 0.9)

preds <- emmeans(model4, ~ neur * grp, at = list(neur = c(1:7)), level = 0.9)
tidy(preds)
predictions <- as.data.frame(preds)

ethn_n <- ggplot(predictions, aes(x = neur, y = emmean, colour = grp)) + 
  geom_line(size = 1) + 
  geom_ribbon(data = predictions, aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.2) + 
  scale_colour_manual(values = c("darkred", "darkblue")) +
  theme_minimal(20) +
  labs(
    x = "Neuroticism",
    y = "Importance of Civic Boundary",
    colour = "Group")

plot_predictions(model4,
                 condition = list("neur" = 1:7,
                                  "grp" = c("control", "treatment")),
                 newdata = datagrid(neur = 1:7,
                                    grp = c("control", "treatment")),
                 conf_level = 0.9)

(ethn_o | ethn_c) / (ethn_e | ethn_a)

(ethn_n)

###### Fine Grained Models

#Born in the country
model5 <- lm(q11 ~ open*grp + cons*grp + extr*grp + agr*grp + neur*grp + age + region + educ + gndr + lrscale_1 + psychback, data = dta1)
modelsummary(model5, stars = T)

#assumptions check
check_collinearity(model5)
check_heteroscedasticity(model5)
resid_panel(model5, plots = c("resid"))
resid_panel(model5, plots = c("hist", "qq"))
resid_panel(model5, plots = "cookd")
model5_aug <- augment(model5)
summary(model5_aug$.std.resid)
values <- c(1.96, 2.56, 3.29)

for (i in values) {
  
  model5_aug <- model5_aug |>
    mutate(err = case_when(
      abs(.std.resid) > i ~ 1,
      .default = 0
    ))
  
  print(
    
    fre(model5_aug$err)
    
  )
  
}

#overview of interactions for Born in the country
preds <- emmeans(model5, ~ open * grp, at = list(open = c(1:7)), level = 0.9)
tidy(preds)
predictions <- as.data.frame(preds)

ggplot(predictions, aes(x = open, y = emmean, colour = grp)) + 
  geom_line(size = 1) + 
  geom_ribbon(data = predictions, aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.2) + 
  scale_colour_manual(values = c("darkred", "darkblue")) +
  theme_minimal(20) +
  labs(
    x = "Opennes to Experience",
    y = "Imprtance of Being Born in the Country",
    colour = "Group")

preds <- emmeans(model5, ~ cons * grp, at = list(cons = c(1:7)), level = 0.9)
tidy(preds)
predictions <- as.data.frame(preds)

ggplot(predictions, aes(x = cons, y = emmean, colour = grp)) + 
  geom_line(size = 1) + 
  geom_ribbon(data = predictions, aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.2) + 
  scale_colour_manual(values = c("darkred", "darkblue")) +
  theme_minimal(20) +
  labs(
    x = "Conscientiousness",
    y = "Imprtance of Being Born in the Country",
    colour = "Group")

preds <- emmeans(model5, ~ agr * grp, at = list(agr = c(1:7)), level = 0.9)
tidy(preds)
predictions <- as.data.frame(preds)

ggplot(predictions, aes(x = agr, y = emmean, colour = grp)) + 
  geom_line(size = 1) + 
  geom_ribbon(data = predictions, aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.2) + 
  scale_colour_manual(values = c("darkred", "darkblue")) +
  theme_minimal(20) +
  labs(
    x = "Agreeableness",
    y = "Imprtance of Being Born in the Country",
    colour = "Group")

preds <- emmeans(model5, ~ extr * grp, at = list(extr = c(1:7)), level = 0.9)
tidy(preds)
predictions <- as.data.frame(preds)

ggplot(predictions, aes(x = extr, y = emmean, colour = grp)) + 
  geom_line(size = 1) + 
  geom_ribbon(data = predictions, aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.2) + 
  scale_colour_manual(values = c("darkred", "darkblue")) +
  theme_minimal(20) +
  labs(
    x = "Extraversion",
    y = "Imprtance of Being Born in the Country",
    colour = "Group")

preds <- emmeans(model5, ~ neur * grp, at = list(neur = c(1:7)), level = 0.9)
tidy(preds)
predictions <- as.data.frame(preds)

ggplot(predictions, aes(x = neur, y = emmean, colour = grp)) + 
  geom_line(size = 1) + 
  geom_ribbon(data = predictions, aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.2) + 
  scale_colour_manual(values = c("darkred", "darkblue")) +
  theme_minimal(20) +
  labs(
    x = "Neuroticism",
    y = "Imprtance of Being Born in the Country",
    colour = "Group")

#Language
model6 <- lm(q12 ~ open*grp + cons*grp + extr*grp + agr*grp + neur*grp + age + region + educ + gndr + lrscale_1 + psychback, data = dta1)
modelsummary(model6, stars = T)

#assumetions checks
check_collinearity(model6)
check_heteroscedasticity(model6)
resid_panel(model6, plots = c("resid"))
resid_panel(model6, plots = c("hist", "qq"))
resid_panel(model6, plots = "cookd")
model6_aug <- augment(model6)
summary(model6_aug$.std.resid)
values <- c(1.96, 2.56, 3.29)

for (i in values) {
  
  model6_aug <- model6_aug |>
    mutate(err = case_when(
      abs(.std.resid) > i ~ 1,
      .default = 0
    ))
  
  print(
    
    fre(model6_aug$err)
    
  )
  
}

#overview of interactions for Language
preds <- emmeans(model6, ~ open * grp, at = list(open = c(1:7)), level = 0.9)
tidy(preds)
predictions <- as.data.frame(preds)

ggplot(predictions, aes(x = open, y = emmean, colour = grp)) + 
  geom_line(size = 1) + 
  geom_ribbon(data = predictions, aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.2) + 
  scale_colour_manual(values = c("darkred", "darkblue")) +
  theme_minimal(20) +
  labs(
    x = "Openness to Experience",
    y = "Importance of Language",
    colour = "Group")

preds <- emmeans(model6, ~ cons * grp, at = list(cons = c(1:7)), level = 0.9)
tidy(preds)
predictions <- as.data.frame(preds)

ggplot(predictions, aes(x = cons, y = emmean, colour = grp)) + 
  geom_line(size = 1) + 
  geom_ribbon(data = predictions, aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.2) + 
  scale_colour_manual(values = c("darkred", "darkblue")) +
  theme_minimal(20) +
  labs(
    x = "Conscientiousness",
    y = "Importance of Language",
    colour = "Group")

preds <- emmeans(model6, ~ extr * grp, at = list(extr = c(1:7)), level = 0.9)
tidy(preds)
predictions <- as.data.frame(preds)

ggplot(predictions, aes(x = extr, y = emmean, colour = grp)) + 
  geom_line(size = 1) + 
  geom_ribbon(data = predictions, aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.2) + 
  scale_colour_manual(values = c("darkred", "darkblue")) +
  theme_minimal(20) +
  labs(
    x = "Extraversion",
    y = "Importance of Language",
    colour = "Group")

preds <- emmeans(model6, ~ agr * grp, at = list(agr = c(1:7)), level = 0.9)
tidy(preds)
predictions <- as.data.frame(preds)

ggplot(predictions, aes(x = agr, y = emmean, colour = grp)) + 
  geom_line(size = 1) + 
  geom_ribbon(data = predictions, aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.2) + 
  scale_colour_manual(values = c("darkred", "darkblue")) +
  theme_minimal(20) +
  labs(
    x = "Agreeableness",
    y = "Importance of Language",
    colour = "Group")

preds <- emmeans(model6, ~ neur * grp, at = list(neur = c(1:7)), level = 0.9)
tidy(preds)
predictions <- as.data.frame(preds)

ggplot(predictions, aes(x = neur, y = emmean, colour = grp)) + 
  geom_line(size = 1) + 
  geom_ribbon(data = predictions, aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.2) + 
  scale_colour_manual(values = c("darkred", "darkblue")) +
  theme_minimal(20) +
  labs(
    x = "Neuroticism",
    y = "Importance of Language",
    colour = "Group")

#Ancestry
model7 <- lm(q13 ~ open*grp + cons*grp + extr*grp + agr*grp + neur*grp + age + region + educ + gndr + lrscale_1 + psychback, data = dta1)
modelsummary(model7, stars = T)

#assumptions checks
check_collinearity(model7)
check_heteroscedasticity(model7)
resid_panel(model7, plots = c("resid"))
resid_panel(model7, plots = c("hist", "qq"))
resid_panel(model7, plots = "cookd")
model7_aug <- augment(model7)
summary(model7_aug$.std.resid)
values <- c(1.96, 2.56, 3.29)

for (i in values) {
  
  model7_aug <- model7_aug |>
    mutate(err = case_when(
      abs(.std.resid) > i ~ 1,
      .default = 0
    ))
  
  print(
    
    fre(model7_aug$err)
    
  )
  
}

#overview of interactions for Ancestry
preds <- emmeans(model7, ~ open * grp, at = list(open = c(1:7)), level = 0.9)
tidy(preds)
predictions <- as.data.frame(preds)

ggplot(predictions, aes(x = open, y = emmean, colour = grp)) + 
  geom_line(size = 1) + 
  geom_ribbon(data = predictions, aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.2) + 
  scale_colour_manual(values = c("darkred", "darkblue")) +
  theme_minimal(20) +
  labs(
    x = "Openness to Experience",
    y = "Importance of Ancestry",
    colour = "Group")

preds <- emmeans(model7, ~ cons * grp, at = list(cons = c(1:7)), level = 0.9)
tidy(preds)
predictions <- as.data.frame(preds)

ggplot(predictions, aes(x = cons, y = emmean, colour = grp)) + 
  geom_line(size = 1) + 
  geom_ribbon(data = predictions, aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.2) + 
  scale_colour_manual(values = c("darkred", "darkblue")) +
  theme_minimal(20) +
  labs(
    x = "Conscientiousness",
    y = "Importance of Ancestry",
    colour = "Group")

preds <- emmeans(model7, ~ extr * grp, at = list(extr = c(1:7)), level = 0.9)
tidy(preds)
predictions <- as.data.frame(preds)

ggplot(predictions, aes(x = extr, y = emmean, colour = grp)) + 
  geom_line(size = 1) + 
  geom_ribbon(data = predictions, aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.2) + 
  scale_colour_manual(values = c("darkred", "darkblue")) +
  theme_minimal(20) +
  labs(
    x = "Extraversion",
    y = "Importance of Ancestry",
    colour = "Group")

preds <- emmeans(model7, ~ agr * grp, at = list(agr = c(1:7)), level = 0.9)
tidy(preds)
predictions <- as.data.frame(preds)

ggplot(predictions, aes(x = agr, y = emmean, colour = grp)) + 
  geom_line(size = 1) + 
  geom_ribbon(data = predictions, aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.2) + 
  scale_colour_manual(values = c("darkred", "darkblue")) +
  theme_minimal(20) +
  labs(
    x = "Agreeableness",
    y = "Importance of Ancestry",
    colour = "Group")

preds <- emmeans(model7, ~ neur * grp, at = list(neur = c(1:7)), level = 0.9)
tidy(preds)
predictions <- as.data.frame(preds)

ggplot(predictions, aes(x = neur, y = emmean, colour = grp)) + 
  geom_line(size = 1) + 
  geom_ribbon(data = predictions, aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.2) + 
  scale_colour_manual(values = c("darkred", "darkblue")) +
  theme_minimal(20) +
  labs(
    x = "Neuroticism",
    y = "Importance of Ancestry",
    colour = "Group")

#Institutions
model8 <- lm(q14 ~ open*grp + cons*grp + extr*grp + agr*grp + neur*grp + age + region + educ + gndr + lrscale_1 + psychback, data = dta1)
modelsummary(model8, stars = T)

#assumptions checks
check_collinearity(model8)
check_heteroscedasticity(model8)
resid_panel(model8, plots = c("resid"))
resid_panel(model8, plots = c("hist", "qq"))
resid_panel(model8, plots = "cookd")
model8_aug <- augment(model8)
summary(model8_aug$.std.resid)
values <- c(1.96, 2.56, 3.29)

for (i in values) {
  
  model8_aug <- model8_aug |>
    mutate(err = case_when(
      abs(.std.resid) > i ~ 1,
      .default = 0
    ))
  
  print(
    
    fre(model8_aug$err)
    
  )
  
}

#overvies of interactions for Institutions
preds <- emmeans(model8, ~ open * grp, at = list(open = c(1:7)), level = 0.9)
tidy(preds)
predictions <- as.data.frame(preds)

ggplot(predictions, aes(x = open, y = emmean, colour = grp)) + 
  geom_line(size = 1) + 
  geom_ribbon(data = predictions, aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.2) + 
  scale_colour_manual(values = c("darkred", "darkblue")) +
  theme_minimal(20) +
  labs(
    x = "Openness to Experience",
    y = "Importance of Respecting Institutions",
    colour = "Group")

preds <- emmeans(model8, ~ cons * grp, at = list(cons = c(1:7)), level = 0.9)
tidy(preds)
predictions <- as.data.frame(preds)

ggplot(predictions, aes(x = cons, y = emmean, colour = grp)) + 
  geom_line(size = 1) + 
  geom_ribbon(data = predictions, aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.2) + 
  scale_colour_manual(values = c("darkred", "darkblue")) +
  theme_minimal(20) +
  labs(
    x = "Conscientiousness",
    y = "Importance of Respecting Institutions",
    colour = "Group")

preds <- emmeans(model8, ~ extr * grp, at = list(extr = c(1:7)), level = 0.9)
tidy(preds)
predictions <- as.data.frame(preds)

ggplot(predictions, aes(x = extr, y = emmean, colour = grp)) + 
  geom_line(size = 1) + 
  geom_ribbon(data = predictions, aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.2) + 
  scale_colour_manual(values = c("darkred", "darkblue")) +
  theme_minimal(20) +
  labs(
    x = "Extraversion",
    y = "Importance of Respecting Institutions",
    colour = "Group")

preds <- emmeans(model8, ~ agr * grp, at = list(agr = c(1:7)), level = 0.9)
tidy(preds)
predictions <- as.data.frame(preds)

ggplot(predictions, aes(x = agr, y = emmean, colour = grp)) + 
  geom_line(size = 1) + 
  geom_ribbon(data = predictions, aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.2) + 
  scale_colour_manual(values = c("darkred", "darkblue")) +
  theme_minimal(20) +
  labs(
    x = "Agreeableness",
    y = "Importance of Respecting Institutions",
    colour = "Group")

preds <- emmeans(model8, ~ neur * grp, at = list(neur = c(1:7)), level = 0.9)
tidy(preds)
predictions <- as.data.frame(preds)

ggplot(predictions, aes(x = neur, y = emmean, colour = grp)) + 
  geom_line(size = 1) + 
  geom_ribbon(data = predictions, aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.2) + 
  scale_colour_manual(values = c("darkred", "darkblue")) +
  theme_minimal(20) +
  labs(
    x = "Neuroticism",
    y = "Importance of Respecting Institutions",
    colour = "Group")

#Feel
model9 <- lm(q15 ~ open*grp + cons*grp + extr*grp + agr*grp + neur*grp + age + region + educ + gndr + lrscale_1 + psychback, data = dta1)
modelsummary(model9, stars = T)

#assumptions checks
check_collinearity(model9)
check_heteroscedasticity(model9)
resid_panel(model9, plots = c("resid"))
resid_panel(model9, plots = c("hist", "qq"))
resid_panel(model9, plots = "cookd")
model9_aug <- augment(model9)
summary(model9_aug$.std.resid)
values <- c(1.96, 2.56, 3.29)

for (i in values) {
  
  model9_aug <- model9_aug |>
    mutate(err = case_when(
      abs(.std.resid) > i ~ 1,
      .default = 0
    ))
  
  print(
    
    fre(model9_aug$err)
    
  )
  
}

#overview of interactions for Feel
preds <- emmeans(model9, ~ open * grp, at = list(open = c(1:7)), level = 0.9)
tidy(preds)
predictions <- as.data.frame(preds)

ggplot(predictions, aes(x = open, y = emmean, colour = grp)) + 
  geom_line(size = 1) + 
  geom_ribbon(data = predictions, aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.2) + 
  scale_colour_manual(values = c("darkred", "darkblue")) +
  theme_minimal(20) +
  labs(
    x = "Openness to Experience",
    y = "Importance of Feeling",
    colour = "Group")

preds <- emmeans(model9, ~ cons * grp, at = list(cons = c(1:7)), level = 0.9)
tidy(preds)
predictions <- as.data.frame(preds)

ggplot(predictions, aes(x = cons, y = emmean, colour = grp)) + 
  geom_line(size = 1) + 
  geom_ribbon(data = predictions, aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.2) + 
  scale_colour_manual(values = c("darkred", "darkblue")) +
  theme_minimal(20) +
  labs(
    x = "Conscientiousness",
    y = "Importance of Feeling",
    colour = "Group")

preds <- emmeans(model9, ~ extr * grp, at = list(extr = c(1:7)), level = 0.9)
tidy(preds)
predictions <- as.data.frame(preds)

ggplot(predictions, aes(x = extr, y = emmean, colour = grp)) + 
  geom_line(size = 1) + 
  geom_ribbon(data = predictions, aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.2) + 
  scale_colour_manual(values = c("darkred", "darkblue")) +
  theme_minimal(20) +
  labs(
    x = "Extraversion",
    y = "Importance of Feeling",
    colour = "Group")

preds <- emmeans(model9, ~ agr * grp, at = list(agr = c(1:7)), level = 0.9)
tidy(preds)
predictions <- as.data.frame(preds)

ggplot(predictions, aes(x = agr, y = emmean, colour = grp)) + 
  geom_line(size = 1) + 
  geom_ribbon(data = predictions, aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.2) + 
  scale_colour_manual(values = c("darkred", "darkblue")) +
  theme_minimal(20) +
  labs(
    x = "Agreeableness",
    y = "Importance of Feeling",
    colour = "Group")

preds <- emmeans(model9, ~ neur * grp, at = list(neur = c(1:7)), level = 0.9)
tidy(preds)
predictions <- as.data.frame(preds)

ggplot(predictions, aes(x = neur, y = emmean, colour = grp)) + 
  geom_line(size = 1) + 
  geom_ribbon(data = predictions, aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.2) + 
  scale_colour_manual(values = c("darkred", "darkblue")) +
  theme_minimal(20) +
  labs(
    x = "Neuroticism",
    y = "Importance of Feeling",
    colour = "Group")

###### Presentation

#civ
modelsummary(model3, #full model
             stars = T,
             coef_map = c("(Intercept)" = "Intercept",
                          "open" = "Openness",
                          "cons" = "Conscientiousness",
                          "extr" = "Extraversion",
                          "agr" = "Agreeableness",
                          "neur" = "Neuroticism",
                          "grptreatment" = "Treatment Group",
                          "open:grptreatment" = "Openness * Treatment",
                          "grptreatment:cons" = "Conscientiousness * Treatment",
                          "grptreatment:extr" = "Extraversion * Treatment",
                          "grptreatment:agr" = "Agreeableness * Treatment",
                          "grptreatment:neur" = "Neuroticism * Treatment",
                          "age" = "Age",
                          "regionEastern Europe" = "Eastern Europe",
                          "regionOther" = "Other Region",
                          "educ" = "Education",
                          "gndrFemale" = "Gender (Female)",
                          "gndrNon-binary" = "Gender (Non-binary)",
                          "gndrPrefer" = "Gender (Prefer Not to Say)",
                          "lrscale_1" = "Left-Right",
                          "psychbackYes" = "Psychological Background (Yes)"),
             gof_map = c("nobs", "r.squared", "adj.r.squared"),
             #output = "latex",
                         notes = ("OLS coefficients with standard errors in parentheses. The reference groups are Western Europe for region and Male for gender"))

modelsummary(model3, #only significant
             stars = T,
             coef_map = c("(Intercept)" = "Intercept",
                          "open" = "Openness",
                          "cons" = "Conscientiousness",
                          "extr" = "Extraversion",
                          "agr" = "Agreeableness",
                          "neur" = "Neuroticism",
                          "grptreatment" = "Treatment Group",
                          "open:grptreatment" = "Openness * Treatment",
                          "grptreatment:cons" = "Conscientiousness * Treatment",
                          "grptreatment:extr" = "Extraversion * Treatment",
                          "grptreatment:agr" = "Agreeableness * Treatment",
                          "grptreatment:neur" = "Neuroticism * Treatment",
                          "lrscale_1" = "Left-Right",
                          "psychbackYes" = "Psychological Background (Yes)"),
             gof_map = c("nobs", "r.squared", "adj.r.squared"),
             output = "latex",
             notes = ("OLS coefficients with standard errors in parentheses"))

modelsummary(list(model3.1, model3.2), #model by groups
             stars = T,
             coef_map = c("(Intercept)" = "Intercept",
                          "open" = "Openness",
                          "cons" = "Conscientiousness",
                          "extr" = "Extraversion",
                          "agr" = "Agreeableness",
                          "neur" = "Neuroticism",
                          "age" = "Age",
                          "regionEastern Europe" = "Eastern Europe",
                          "regionOther" = "Other Region",
                          "educ" = "Education",
                          "gndrFemale" = "Gender (Female)",
                          "gndrNon-binary" = "Gender (Non-binary)",
                          "gndrPrefer" = "Gender (Prefer Not to Say)",
                          "lrscale_1" = "Left-Right",
                          "psychbackYes" = "Psychological Background (Yes)"),
             gof_map = c("nobs", "r.squared", "adj.r.squared"),
             #output = "latex",
             notes = ("OLS coefficients with standard errors in parentheses. The reference groups are Western Europe for region and Male for gender"))

#ethn
modelsummary(model4, #full model
             stars = T,
             coef_map = c("(Intercept)" = "Intercept",
                          "open" = "Openness",
                          "cons" = "Conscientiousness",
                          "extr" = "Extraversion",
                          "agr" = "Agreeableness",
                          "neur" = "Neuroticism",
                          "grptreatment" = "Treatment Group",
                          "open:grptreatment" = "Openness * Treatment",
                          "grptreatment:cons" = "Conscientiousness * Treatment",
                          "grptreatment:extr" = "Extraversion * Treatment",
                          "grptreatment:agr" = "Agreeableness * Treatment",
                          "grptreatment:neur" = "Neuroticism * Treatment",
                          "age" = "Age",
                          "regionEastern Europe" = "Eastern Europe",
                          "regionOther" = "Other Region",
                          "educ" = "Education",
                          "gndrFemale" = "Gender (Female)",
                          "gndrNon-binary" = "Gender (Non-binary)",
                          "gndrPrefer" = "Gender (Prefer Not to Say)",
                          "lrscale_1" = "Left-Right",
                          "psychbackYes" = "Psychological Background (Yes)"),
             gof_map = c("nobs", "r.squared", "adj.r.squared"),
             #output = "latex",
             notes = ("OLS coefficients with standard errors in parentheses. The reference groups are Western Europe for region and Male for gender"))

modelsummary(model4, #only significant
             stars = T,
             coef_map = c("(Intercept)" = "Intercept",
                          "open" = "Openness",
                          "cons" = "Conscientiousness",
                          "extr" = "Extraversion",
                          "agr" = "Agreeableness",
                          "neur" = "Neuroticism",
                          "grptreatment" = "Treatment Group",
                          "open:grptreatment" = "Openness * Treatment",
                          "grptreatment:cons" = "Conscientiousness * Treatment",
                          "grptreatment:extr" = "Extraversion * Treatment",
                          "grptreatment:agr" = "Agreeableness * Treatment",
                          "grptreatment:neur" = "Neuroticism * Treatment",
                          "lrscale_1" = "Left-Right"),
             gof_map = c("nobs", "r.squared", "adj.r.squared"),
             output = "latex",
             notes = ("OLS coefficients with standard errors in parentheses"))

modelsummary(list(model4.1, model4.2), #models by group
             stars = T,
             coef_map = c("(Intercept)" = "Intercept",
                          "open" = "Openness",
                          "cons" = "Conscientiousness",
                          "extr" = "Extraversion",
                          "agr" = "Agreeableness",
                          "neur" = "Neuroticism",
                          "age" = "Age",
                          "regionEastern Europe" = "Eastern Europe",
                          "regionOther" = "Other Region",
                          "educ" = "Education",
                          "gndrFemale" = "Gender (Female)",
                          "gndrNon-binary" = "Gender (Non-binary)",
                          "gndrPrefer" = "Gender (Prefer Not to Say)",
                          "lrscale_1" = "Left-Right",
                          "psychbackYes" = "Psychological Background (Yes)"),
             gof_map = c("nobs", "r.squared", "adj.r.squared"),
             #output = "latex",
             notes = ("OLS coefficients with standard errors in parentheses. The reference groups are Western Europe for region and Male for gender"))


#Being Born
modelsummary(model5, #full model
             stars = T,
             coef_map = c("(Intercept)" = "Intercept",
                          "open" = "Openness",
                          "cons" = "Conscientiousness",
                          "extr" = "Extraversion",
                          "agr" = "Agreeableness",
                          "neur" = "Neuroticism",
                          "grptreatment" = "Treatment Group",
                          "open:grptreatment" = "Openness * Treatment",
                          "grptreatment:cons" = "Conscientiousness * Treatment",
                          "grptreatment:extr" = "Extraversion * Treatment",
                          "grptreatment:agr" = "Agreeableness * Treatment",
                          "grptreatment:neur" = "Neuroticism * Treatment",
                          "age" = "Age",
                          "regionEastern Europe" = "Eastern Europe",
                          "regionOther" = "Other Region",
                          "educ" = "Education",
                          "gndrFemale" = "Gender (Female)",
                          "gndrNon-binary" = "Gender (Non-binary)",
                          "gndrPrefer" = "Gender (Prefer Not to Say)",
                          "lrscale_1" = "Left-Right",
                          "psychbackYes" = "Psychological Background (Yes)"),
             gof_map = c("nobs", "r.squared", "adj.r.squared"),
             #output = "latex",
             notes = ("OLS coefficients with standard errors in parentheses. The reference groups are Western Europe for region and Male for gender"))

modelsummary(model5, #only significant
             stars = T,
             coef_map = c("(Intercept)" = "Intercept",
                          "open" = "Openness",
                          "cons" = "Conscientiousness",
                          "extr" = "Extraversion",
                          "agr" = "Agreeableness",
                          "neur" = "Neuroticism",
                          "grptreatment" = "Treatment Group",
                          "open:grptreatment" = "Openness * Treatment",
                          "grptreatment:cons" = "Conscientiousness * Treatment",
                          "grptreatment:extr" = "Extraversion * Treatment",
                          "grptreatment:agr" = "Agreeableness * Treatment",
                          "grptreatment:neur" = "Neuroticism * Treatment",
                          "age" = "Age",
                          "lrscale_1" = "Left-Right"),
             gof_map = c("nobs", "r.squared", "adj.r.squared"),
             output = "latex",
             notes = ("OLS coefficients with standard errors in parentheses"))

#Language
modelsummary(model6, #full model
             stars = T,
             coef_map = c("(Intercept)" = "Intercept",
                          "open" = "Openness",
                          "cons" = "Conscientiousness",
                          "extr" = "Extraversion",
                          "agr" = "Agreeableness",
                          "neur" = "Neuroticism",
                          "grptreatment" = "Treatment Group",
                          "open:grptreatment" = "Openness * Treatment",
                          "grptreatment:cons" = "Conscientiousness * Treatment",
                          "grptreatment:extr" = "Extraversion * Treatment",
                          "grptreatment:agr" = "Agreeableness * Treatment",
                          "grptreatment:neur" = "Neuroticism * Treatment",
                          "age" = "Age",
                          "regionEastern Europe" = "Eastern Europe",
                          "regionOther" = "Other Region",
                          "educ" = "Education",
                          "gndrFemale" = "Gender (Female)",
                          "gndrNon-binary" = "Gender (Non-binary)",
                          "gndrPrefer" = "Gender (Prefer Not to Say)",
                          "lrscale_1" = "Left-Right",
                          "psychbackYes" = "Psychological Background (Yes)"),
             gof_map = c("nobs", "r.squared", "adj.r.squared"),
             #output = "latex",
             notes = ("OLS coefficients with standard errors in parentheses. The reference groups are Western Europe for region and Male for gender"))


modelsummary(model6, #only significant
             stars = T,
             coef_map = c("(Intercept)" = "Intercept",
                          "open" = "Openness",
                          "cons" = "Conscientiousness",
                          "extr" = "Extraversion",
                          "agr" = "Agreeableness",
                          "neur" = "Neuroticism",
                          "grptreatment" = "Treatment Group",
                          "open:grptreatment" = "Openness * Treatment",
                          "grptreatment:cons" = "Conscientiousness * Treatment",
                          "grptreatment:extr" = "Extraversion * Treatment",
                          "grptreatment:agr" = "Agreeableness * Treatment",
                          "grptreatment:neur" = "Neuroticism * Treatment",
                          "lrscale_1" = "Left-Right"),
             gof_map = c("nobs", "r.squared", "adj.r.squared"),
             output = "latex",
             notes = ("OLS coefficients with standard errors in parentheses"))

#Ancestry
modelsummary(model7, #full model
             stars = T,
             coef_map = c("(Intercept)" = "Intercept",
                          "open" = "Openness",
                          "cons" = "Conscientiousness",
                          "extr" = "Extraversion",
                          "agr" = "Agreeableness",
                          "neur" = "Neuroticism",
                          "grptreatment" = "Treatment Group",
                          "open:grptreatment" = "Openness * Treatment",
                          "grptreatment:cons" = "Conscientiousness * Treatment",
                          "grptreatment:extr" = "Extraversion * Treatment",
                          "grptreatment:agr" = "Agreeableness * Treatment",
                          "grptreatment:neur" = "Neuroticism * Treatment",
                          "age" = "Age",
                          "regionEastern Europe" = "Eastern Europe",
                          "regionOther" = "Other Region",
                          "educ" = "Education",
                          "gndrFemale" = "Gender (Female)",
                          "gndrNon-binary" = "Gender (Non-binary)",
                          "gndrPrefer" = "Gender (Prefer Not to Say)",
                          "lrscale_1" = "Left-Right",
                          "psychbackYes" = "Psychological Background (Yes)"),
             gof_map = c("nobs", "r.squared", "adj.r.squared"),
             #output = "latex",
             notes = ("OLS coefficients with standard errors in parentheses. The reference groups are Western Europe for region and Male for gender"))

modelsummary(model7, #only significant
             stars = T,
             coef_map = c("(Intercept)" = "Intercept",
                          "open" = "Openness",
                          "cons" = "Conscientiousness",
                          "extr" = "Extraversion",
                          "agr" = "Agreeableness",
                          "neur" = "Neuroticism",
                          "grptreatment" = "Treatment Group",
                          "open:grptreatment" = "Openness * Treatment",
                          "grptreatment:cons" = "Conscientiousness * Treatment",
                          "grptreatment:extr" = "Extraversion * Treatment",
                          "grptreatment:agr" = "Agreeableness * Treatment",
                          "grptreatment:neur" = "Neuroticism * Treatment",
                          "lrscale_1" = "Left-Right"),
             gof_map = c("nobs", "r.squared", "adj.r.squared"),
             output = "latex",
             notes = ("OLS coefficients with standard errors in parentheses"))

#Institutions
modelsummary(model8, #full model
             stars = T,
             coef_map = c("(Intercept)" = "Intercept",
                          "open" = "Openness",
                          "cons" = "Conscientiousness",
                          "extr" = "Extraversion",
                          "agr" = "Agreeableness",
                          "neur" = "Neuroticism",
                          "grptreatment" = "Treatment Group",
                          "open:grptreatment" = "Openness * Treatment",
                          "grptreatment:cons" = "Conscientiousness * Treatment",
                          "grptreatment:extr" = "Extraversion * Treatment",
                          "grptreatment:agr" = "Agreeableness * Treatment",
                          "grptreatment:neur" = "Neuroticism * Treatment",
                          "age" = "Age",
                          "regionEastern Europe" = "Eastern Europe",
                          "regionOther" = "Other Region",
                          "educ" = "Education",
                          "gndrFemale" = "Gender (Female)",
                          "gndrNon-binary" = "Gender (Non-binary)",
                          "gndrPrefer" = "Gender (Prefer Not to Say)",
                          "lrscale_1" = "Left-Right",
                          "psychbackYes" = "Psychological Background (Yes)"),
             gof_map = c("nobs", "r.squared", "adj.r.squared"),
             #output = "latex",
             notes = ("OLS coefficients with standard errors in parentheses. The reference groups are Western Europe for region and Male for gender"))

modelsummary(model8, #only significant
             stars = T,
             coef_map = c("(Intercept)" = "Intercept",
                          "open" = "Openness",
                          "cons" = "Conscientiousness",
                          "extr" = "Extraversion",
                          "agr" = "Agreeableness",
                          "neur" = "Neuroticism",
                          "grptreatment" = "Treatment Group",
                          "open:grptreatment" = "Openness * Treatment",
                          "grptreatment:cons" = "Conscientiousness * Treatment",
                          "grptreatment:extr" = "Extraversion * Treatment",
                          "grptreatment:agr" = "Agreeableness * Treatment",
                          "grptreatment:neur" = "Neuroticism * Treatment",
                          "lrscale_1" = "Left-Right"),
             gof_map = c("nobs", "r.squared", "adj.r.squared"),
             output = "latex",
             notes = ("OLS coefficients with standard errors in parentheses"))

#Feel
modelsummary(model9, #only significant
             stars = T,
             coef_map = c("(Intercept)" = "Intercept",
                          "open" = "Openness",
                          "cons" = "Conscientiousness",
                          "extr" = "Extraversion",
                          "agr" = "Agreeableness",
                          "neur" = "Neuroticism",
                          "grptreatment" = "Treatment Group",
                          "open:grptreatment" = "Openness * Treatment",
                          "grptreatment:cons" = "Conscientiousness * Treatment",
                          "grptreatment:extr" = "Extraversion * Treatment",
                          "grptreatment:agr" = "Agreeableness * Treatment",
                          "grptreatment:neur" = "Neuroticism * Treatment",
                          "psychbackYes" = "Psychological Background (Yes)"),
             gof_map = c("nobs", "r.squared", "adj.r.squared"),
             output = "latex",
             notes = ("OLS coefficients with standard errors in parentheses. The reference groups are Western Europe for region and Male for gender"))





