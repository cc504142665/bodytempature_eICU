# body temperature abnormalities and critically ill patients
# eICU-CRD

# install packages 
#  rgenoud, MatchIt, MASS, car, relaimpo
library(pacman)
pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, 
               ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, glmnet, leaps, bestglm, dummies, QuantPsyc,
               stringr, tidyr, Hmisc, arsenal, survminer, survival)
temp <- import("C:/File/Study/Temperature+eICU/eICU.temp.allcoreperi.nonsepsis.antipyretic.csv")
load(file = "eicutemp.nonsepsis.Rda")
temp_nonsepsis <- temp
load(file = "eicutemp.sepsis.Rda")
temp_sep <- temp

# data cleanse & classification
# gender
temp$gender_trans <- temp$gender
temp$gender_trans[temp$gender_trans==0] = 'Female'
temp$gender_trans[temp$gender_trans==1] = 'Male'
temp$gender_trans <- as.factor(temp$gender_trans)
temp <- temp[!is.na(temp$gender_trans),]

# age
temp$age_trans <- temp$age
temp$age_trans[temp$age_trans=='> 89'] = '91.5'
temp$age_trans <- as.numeric(temp$age_trans)
temp <- temp[!is.na(temp$age_trans),]

# APACHE
temp$apache_iv[temp$apache_iv<0] <- NA

# BMI
# Remove heights above 3 meter and below .5 meter.
temp$admissionheight[temp$admissionheight > 300 | temp$admissionheight < 50] <- NA
# Remove weights below 20kg and above 600kg
temp$admissionweight[temp$admissionweight < 20 | temp$admissionweight > 600] <- NA
# Compute the body mass index
temp$BMI <- temp$admissionweight / (temp$admissionheight/100)^2
# Remove BMIs below 15 or above 100
temp$BMI[temp$BMI < 15 | temp$BMI > 100] <- NA

# BMI category
#temp$BMI_category <- cut(temp$BMI,
                             #breaks=c(-Inf,18.5,25,30,Inf),right = FALSE)
#temp$BMI_category <- factor(temp$BMI_category, 
                                #levels=c("[-Inf,18.5)", "[18.5,25)", "[25,30)", "[30, Inf)"),
                                #labels=c("Underweight", "Normal weight", "Overweight", "Obese"))

# ICU type
temp$ICU = as.factor(temp$unittype)
#classify into one CCU-CTICU
levels(temp$ICU)[levels(temp$ICU) %in% c("Cardiac ICU", "CSICU", "CTICU", "CCU-CTICU")] = "Cardiac ICU"

# race (add native american into other/unknown)
temp$race = as.factor(temp$ethnicity)
levels(temp$race)[levels(temp$race) %in% "Native American"] = "Other/Unknown"
levels(temp$race)[levels(temp$race) %in% ""] = "Other/Unknown"

# region
temp$region <- as.factor(temp$region)

# flag change
temp$hypertension_flag <- as.factor(temp$hypertension_flag)
temp$diabetes_flag <- as.factor(temp$diabetes_flag)
temp$CKD_flag <- as.factor(temp$CKD_flag)
temp$COPD_flag <- as.factor(temp$COPD_flag)
temp$heartfailure_flag <- as.factor(temp$heartfailure_flag)
temp$cancer_flag <- as.factor(temp$cancer_flag)

temp$ventilation_flag <- as.factor(temp$ventilation_flag)
temp$tracheostomy_flag <- as.factor(temp$tracheostomy_flag)
temp$dialysis_flag <- as.factor(temp$dialysis_flag)
temp$vasopressor_flag <- as.factor(temp$vasopressor_flag)
temp$antibiotics_flag <- as.factor(temp$antibiotics_flag)

temp$acetaminophen_flag <- as.factor(temp$acetaminophen_flag)
temp$nsaid_flag <- as.factor(temp$nsaid_flag)
temp$antipyretic_flag <- as.factor(if_else(temp$acetaminophen_flag == 1 | temp$nsaid_flag == 1, 1, 0))

temp$hosp_mortality <- as.factor(temp$hosp_mortality)
temp$icu_mortality <- as.factor(temp$icu_mortality)
# If someone expires in the ICU, then they expire in the hospital.
temp$hosp_mortality[which(temp$icu_mortality == 1)] <- 1

temp$T_class <- cut(temp$T_max,
                    breaks=c(-Inf,36,37.2,38.3,39,Inf),right = FALSE)
temp_sep$T_class <- factor(temp_sep$T_class,
                       levels = c("[-Inf,36)","[36,37.2)","[37.2,38.3)","[38.3,39)","[39, Inf)"),
                       labels = c("< 36","36 ~ 37.1","37.2 ~ 38.2","38.3 ~ 38.9",">= 39"))

save(temp, file = "eicutemp.nonsepsis.Rda")

# exclusion 
temp_sep <- temp_sep[temp_sep$age_trans >= 18,]
temp_nonsepsis <- temp_nonsepsis[temp_nonsepsis$age_trans >= 18,]

temp_sep <- temp_sep[temp_sep$unitvisitnumber == 1,]
temp_nonsepsis <- temp_nonsepsis[temp_nonsepsis$unitvisitnumber == 1,]

save(temp_sep, file = "eicutemp.sepsis.Rda")
save(temp_nonsepsis, file = "eicutemp.nonsepsis.Rda")

# table 1: baseline of sepsis/non-sepsis
my_controls <- tableby.control(
  test = T,
  total = T,
  numeric.test = "kwt", cat.test = "chisq",
  numeric.stats = c("meansd", "medianq1q3", "Nmiss2"),
  cat.stats = c("countpct", "Nmiss2"),
  stats.labels = list(
    meansd = "Mean (SD)",
    medianq1q3 = "Median (Q1, Q3)",
    Nmiss2 = "Missing")
)


my_labels <- list(
  age_trans = "Age, years", 
  gender_trans = "Male, %", 
  BMI = "BMI, kg/m2", 
  race = "Ethnicity, %",
  ICU = "Admitted ICU type, %", 
  region = "Region, %",
  apache_iv = "APACHE IV score", 
  sofa = "SOFA score",
  hypertension_flag = "Hypertension, %",
  diabetes_flag = "Diabetes mellitus, %", 
  CKD_flag = "CKD, %", 
  COPD_flag = "COPD, %", 
  heartfailure_flag="Congestive heart failure, %",
  cancer_flag = "Cancer, %", 
  ventilation_flag="Mechanical Ventilation, %", 
  tracheostomy_flag = "Tracheostomy, %",
  dialysis_flag="Dialysis, %",
  vasopressor_flag="Vasopressor, %", 
  ene_less_flag = "(Nor)epinephrine <= 0.1 micrograms/kg/min, %",
  ene_over_flag = "(Nor)epinephrine > 0.1 micrograms/kg/min, %",
  dop_over5_flag = "Dopamine > 5 micrograms/kg/min, %",
  dop_less5_flag = "Dopamine <= 5 micrograms/kg/min, %",
  antibiotics_flag="Antibiotics, %", 
  hosp_mortality="In-hospital mortality, %", 
  icu_mortality="ICU mortality, %", 
  hosp_los_hours="Hospital LOS, hours", 
  icu_los_hours="ICU LOS, hours", 
  HR_max="Max heart rate, bpm", 
  RR_max ="Max respiratory rate, bpm",
  SpO2_min="Min SPO2, %", 
  MAP_min="Min MAP, mmHg",
  T_max = "Tmax",
  T_max_peri = "Tmax",
  comorbidites_flag = "Comorbidities, %",
  WBC_max = "Max WBC, 10^9/L",
  HGB_min = "Min hemoglobin, g/L",
  HCT_min = "Min hemocrit, %",
  PLT_min = "Min platelet, 10^9/L",
  LAC_max = "Max lactate, mmol/L",
  ALT_max = "Max ALT, U/L",
  AST_max = "Max AST, U/L",
  BIL_max = "Max total bilirubin, mg/dL",
  BUN_max = "Max BUN, mg/dL",
  SCR_max = "Max cretitine, mg/dL",
  NLR_max = "Max NLR",
  uo_24h = "Urine output, mL",
  OI_min = "Min OI, mmHg",
  septicshock_flag = "Septic shock, %",
  acetaminophen_flag = "Acetaminophen, %",
  nsaid_flag = "NSAIDs, %"
)

temp_all <- rbind(temp_sep, temp_nonsepsis)
temp_all$sepsis_flag <- as.factor(temp_all$sepsis_flag)

table1 <- tableby(sepsis_flag ~ age_trans + gender_trans +  BMI + ICU + race + region
                  + apache_iv + hypertension_flag + diabetes_flag + CKD_flag + COPD_flag + heartfailure_flag
                  + cancer_flag + ventilation_flag + dialysis_flag + vasopressor_flag
                  + HR_max + RR_max + SpO2_min + MAP_min + T_max
                  + WBC_max + LAC_max + NLR_max + uo_24h,
                  data = temp_all, control = my_controls)

summary(table1, labelTranslations = my_labels, text = TRUE)   

temp_all$race1[temp_all$race %in% c("Other/Unknown","African American","Asian","Caucasian")] = "Non-Region"
temp_all$race1[temp_all$race %in% c("Hispanic")] = "Yes-Region"
temp_all$race1 <- as.factor(temp_all$race1)

h <- table(temp_all$race1, temp_all$sepsis_flag)
chisq.test(h)

# normality
qqnorm(temp_all$HR_max)


# table 2 & 3: baseline table (all temp)
# table temperature outcome table
table2 <- tableby(T_class ~ age_trans + gender_trans +  BMI + apache_iv + sofa
                  + HR_max + RR_max + SpO2_min + MAP_min
                  + WBC_max + LAC_max + NLR_max + acetaminophen_flag + nsaid_flag
                  + hosp_los_hours + icu_los_hours + icu_mortality + hosp_mortality,
                  data = temp_sep, control = my_controls)

summary(table2, labelTranslations = my_labels, text = TRUE)


table3 <- tableby(T_class ~ age_trans + gender_trans +  BMI + apache_iv + sofa
                  + HR_max + RR_max + SpO2_min + MAP_min
                  + WBC_max + LAC_max + NLR_max + acetaminophen_flag + nsaid_flag
                  + hosp_los_hours + icu_los_hours + icu_mortality + hosp_mortality,
                  data = temp_nonsepsis, control = my_controls)

summary(table3, labelTranslations = my_labels, text = TRUE)   


# pairwise
pairwise.t.test(temp_nonsepsis$icu_los_hours, temp_nonsepsis$T_class)

h <- table(temp_nonsepsis$T_class, temp_nonsepsis$icu_mortality)

pacman::p_load(rcompanion)
pairwiseNominalIndependence(as.matrix(h),
                            fisher = FALSE,
                            gtest  = FALSE,
                            chisq  = TRUE,
                            method = "fdr")

# normality
qqnorm(temp$NLR_max)


# logistics regression

# glm/all temperatures
model1 <- glm(hosp_mortality ~ relevel(T_class,"36 ~ 37.1") + age_trans + gender_trans 
              + BMI + apache_iv, data = temp_sep, family = binomial)
exp(cbind("Odds ratio" = coef(model1), confint.default(model1, level = 0.95)))  
summary(model1)


model2 <- glm(hosp_mortality ~ relevel(T_class,"36 ~ 37.1") + age_trans + gender_trans 
              + BMI + apache_iv, data = temp_nonsepsis, family = binomial)
exp(cbind("Odds ratio" = coef(model2), confint.default(model2, level = 0.95)))  
summary(model2)



# antipyretic therapy 
# PSM
fever <- temp_nonsepsis[temp_nonsepsis$T_class %in% c("38.3 ~ 38.9",">= 39"),]

pacman::p_load(MatchIt, lmtest, sandwich, optmatch)

fever <- fever[!is.na(fever$apache_iv),]
fever_match2 <- matchit(antipyretic_flag ~ age_trans + gender_trans + apache_iv,
                        data = fever, method = "nearest", distance ="glm",
                        ratio = 1, replace = FALSE)

fever_matched <- match.data(fever_match2)

# after matching
table4 <- tableby(antipyretic_flag ~ age_trans + gender_trans + BMI + apache_iv 
                  + hosp_los_hours + icu_los_hours + icu_mortality + hosp_mortality
                  + ventilation_flag + dialysis_flag + vasopressor_flag
                  + HR_max + RR_max + SpO2_min + MAP_min + T_max
                  + WBC_max + LAC_max + NLR_max + uo_24h,
                  data = fever_matched, control = my_controls)

summary(table4, labelTranslations = my_labels, text = TRUE)


# Kaplan Meier Curve
temp_sep$hosp_los_days <- temp_sep$hosp_los_hours / 24
temp_nonsepsis$hosp_los_days <- temp_nonsepsis$hosp_los_hours / 24


survival_sep <- temp_sep[,"hosp_los_days"]
survival_nsep <- temp_nonsepsis[,"hosp_los_days"]

death_sep <- as.numeric(temp_sep[,"hosp_mortality"])
death_nsep <- as.numeric(temp_nonsepsis[,"hosp_mortality"])

groups_sep <- temp_sep[,"T_class"]
groups_nsep <- temp_nonsepsis[,"T_class"]

fit_sep <- survfit(Surv(survival_sep, death_sep) ~ groups_sep, data = temp_sep)
fit_nsep <- survfit(Surv(survival_nsep, death_nsep) ~ groups_nsep, data = temp_nonsepsis)

h1 <- ggsurvplot(
  fit_sep,                     # survfit object with calculated statistics.
  data = temp_sep,            
  pval = TRUE, fun = "pct",        
  xlim = c(0,28), break.time.by = 7, linetype = "strata", size = 1,
  risk.table = TRUE, risk.table.y.text.col = T, risk.table.y.text = FALSE, 
  xlab = "Days after ICU admission", legend = "bottom", 
  legend.title = "Temperature category, °C",
  legend.labs = c("< 36", "36 ~ 37.1", "37.2 ~ 38.2", "38.3 ~ 38.9", ">= 39"), 
  censor=FALSE,
  ggtheme = theme_gray(), title = "(A) 28-day survival curve in sepsis patients")   

h2 <- ggsurvplot(
  fit_nsep,                     # survfit object with calculated statistics.
  data = temp_nonsepsis,            
  pval = TRUE, fun = "pct",        
  xlim = c(0,28), break.time.by = 7, linetype = "strata", size = 1,
  risk.table = TRUE, risk.table.y.text.col = T, risk.table.y.text = FALSE, 
  legend = "bottom", 
  legend.title = "Temperature category, °C",
  legend.labs = c("< 36", "36 ~ 37.1", "37.2 ~ 38.2", "38.3 ~ 38.9", ">= 39"), 
  censor=FALSE,
  ggtheme = theme_gray(), title = "(B) 28-day survival curve in non-sepsis patients")  

h <- arrange_ggsurvplots(list(h1, h2), ncol = 2, nrow = 1)
               

# log-rank test for each
tnonseptic <- as.data.frame(temp_csv_twoflag[[1]])
#lrt1.1 <- survdiff(Surv(hosp_los_days, hosp_mortality) ~ age_category, tnonseptic)
lrt1.2 <- pairwise_survdiff(Surv(survival_time2, death2) ~ fivegroups2, data = temp_csv_twoflag[[2]])


