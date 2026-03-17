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

dta0 <- read.csv("https://raw.githubusercontent.com/CIREnjoyer/LU_Thesis/refs/heads/main/Pilot/Pilot.csv")

dta <- dta0[-c(1:2),]

names <- colnames(dta[,c(14:23, 26:30, 37:43, 47)])

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
         nationality = factor(nationality),
         educ = factor(educ),
         
  )

datasummary_skim(dta, type = "numeric",
                 fun_numeric = list(Min = min, Mean = mean, SD = sd, Max = max, "Total N" = length))



model1 <- lm(ethn ~ open + cons + extr + agr + neur, data = subset(dta, group == "treatment" & check <= 2))

model2 <- lm(ethn ~ open + cons + extr + agr + neur, data = subset(dta, group == "control"))

model3 <- lm(civ ~ open + cons + extr + agr + neur, data = subset(dta, group == "treatment" & check <= 2))

model4 <- lm(civ ~ open + cons + extr + agr + neur, data = subset(dta, group == "control"))

modelsummary(list(model1, model2, model3, model4),
             stars = T)

model5 <- lm(civ ~ open*group + cons*group + extr*group + agr*group + neur*group, data = dta)

model6 <- lm(ethn ~ open*group + cons*group + extr*group + agr*group + neur*group, data = dta)

model7 <- lm(civ ~ open*group + cons*group + extr*group + agr*group + neur*group + age + nationality + educ + gndr + lrscale_1 + psychback, data = dta)

model8 <- lm(ethn ~ open*group + cons*group + extr*group + agr*group + neur*group + age + nationality + educ + gndr + lrscale_1 + psychback, data = dta)

modelsummary(list(model5, model6, model7, model8),
             stars = T)


model9 <- lm(check ~ open + cons + extr + agr + neur + psychback, data = dta)
summary(model9)


model10 <- lm(q11 ~ open*group + cons*group + extr*group + agr*group + neur*group, data = dta)
model11 <- lm(q12 ~ open*group + cons*group + extr*group + agr*group + neur*group, data = dta)
model12 <- lm(q13 ~ open*group + cons*group + extr*group + agr*group + neur*group, data = dta) 
model13 <- lm(q14 ~ open*group + cons*group + extr*group + agr*group + neur*group, data = dta)
model14 <- lm(q15 ~ open*group + cons*group + extr*group + agr*group + neur*group, data = dta)

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




