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

dta0 <- read.csv("https://raw.githubusercontent.com/CIREnjoyer/LU_Thesis/refs/heads/main/Pilot/Pilot1.csv")
nats <- read.csv("https://gist.githubusercontent.com/marijn/274449/raw/0045fb5f54f9ad357e301cf30e23d9834058618a/nationalities.csv")

dta <- dta0[-c(1:2),]

names <- colnames(dta[,c(14:23, 26:30, 37:43, 47)])
natio <- colnames(nats)

for (i in names) {
  
  dta[[i]] <- as.numeric(dta[[i]])
  
}

dta <- dta |>
  mutate(open = ((q5 + (7 - q10_orig))/2),
         cons = ((q3 + (7 - q8_orig))/2),
         extr = ((q1 + (7 - q6_orig))/2),
         agr = (((7 - q2_orig) + q7)/2),
         neur = ((q4 + (7 - q9_orig))/2),
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

datasummary_skim(dta, type = "numeric",
                 fun_numeric = list(Min = min, Mean = mean, SD = sd, Max = max, "Total N" = length))

x <- correlation(dta[, 14:23],
                 ci = 0.95)

datasummary_correlation(x,
                        stars = T)


model1 <- lm(ethn ~ open + cons + extr + agr + neur, data = subset(dta1, group == "treatment" & check <= 3))

model2 <- lm(ethn ~ open + cons + extr + agr + neur, data = subset(dta1, group == "control"))

model3 <- lm(civ ~ open + cons + extr + agr + neur, data = subset(dta1, group == "treatment" & check <= 3))

model4 <- lm(civ ~ open + cons + extr + agr + neur , data = subset(dta1, group == "control"))

modelsummary(list(model1, model2, model3, model4),
             stars = T)

dta1 <- dta |>
  filter(check <= 3 | is.na(check))

model5 <- lm(civ ~ open*grp + cons*grp + extr*grp + agr*grp + neur*grp, data = dta1)

model6 <- lm(ethn ~ open*grp + cons*grp + extr*grp + agr*grp + neur*grp, data = dta1)

model7 <- lm(civ ~ open*grp + cons*grp + extr*grp + agr*grp + neur*grp + age + region + educ + gndr + lrscale_1 + psychback, data = dta1)

model8 <- lm(ethn ~ open*grp + cons*grp + extr*grp + agr*grp + neur*grp + age + region + educ + gndr + lrscale_1 + psychback, data = dta1)

modelsummary(list(model5, model6, model7, model8),
             stars = T)


model9 <- lm(check ~ open + cons + extr + agr + neur + psychback + lrscale_1 + age + gndr, data = dta)
summary(model9)


model10 <- lm(q11 ~ open*grp + cons*grp + extr*grp + agr*grp + neur*grp + age + region + educ + gndr + lrscale_1 + psychback, data = dta1)
model11 <- lm(q12 ~ open*grp + cons*grp + extr*grp + agr*grp + neur*grp + age + region + educ + gndr + lrscale_1 + psychback, data = dta1)
model12 <- lm(q13 ~ open*grp + cons*grp + extr*grp + agr*grp + neur*grp + age + region + educ + gndr + lrscale_1 + psychback, data = dta1) 
model13 <- lm(q14 ~ open*grp + cons*grp + extr*grp + agr*grp + neur*grp + age + region + educ + gndr + lrscale_1 + psychback, data = dta1)
model14 <- lm(q15 ~ open*grp + cons*grp + extr*grp + agr*grp + neur*grp + age + region + educ + gndr + lrscale_1 + psychback, data = dta1)

modelsummary(list(model10, model11, model12, model13, model14),
             stars = T)

pwrss.f.reg(r2 = 0.3,
            k = 17,
            power = 0.8,
            alpha = 0.05)


alpha(dta[, c("q11","q13")])
alpha(dta[, c("q12", "q14", "q15")],  use = "pairwise.complete.obs")

alpha(dta[, c("c12", "c14", "c15")])
alpha(dta[, c("t12", "t14", "t15")])

pca <- principal(dta[, c("q12", "q14", "q15")], nfactors = 2, rotate = "varimax")
print(pca)

check_collinearity(model7)

slopes(model8,
       variables = "cons",
       by = "grp",
       newdata = datagrid(cons = c(1:7)),
       conf_level = 0.9) |>
  ggplot(aes(x = grp, y = estimate)) + 
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high))

plot_predictions(model8,
                 condition = list("cons" = 1:7,
                                  "grp" = c("control", "treatment")),
                 newdata = datagrid(cons = 1:7,
                                    grp = c("control", "treatment")),
                 conf_level = 0.95)

plot_predictions(model8,
                 condition = list("agr" = 1:7,
                                  "grp" = c("control", "treatment")),
                 newdata = datagrid(agr = 1:7,
                                    grp = c("control", "treatment")),
                 conf_level = 0.95)

plot_predictions(model8,
                 condition = list("neur" = 1:7,
                                  "grp" = c("control", "treatment")),
                 newdata = datagrid(neur = 1:7,
                                    grp = c("control", "treatment")),
                 conf_level = 0.95)

plot_predictions(model13,
                 condition = list("extr" = 1:7,
                                  "grp" = c("control", "treatment")),
                 newdata = datagrid(extr = 1:7,
                                    grp = c("control", "treatment")),
                 conf_level = 0.9)

emmeans(model8, ~cons * grp, at = list(cons = c(1:7)),
        type = "response",
        level = 0.9)








