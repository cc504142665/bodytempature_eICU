# body temperature abnormalities and critically ill patients
# eICU-CRD

# install packages 
library(pacman)
pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, 
               ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, glmnet, leaps, bestglm, dummies, QuantPsyc,
               stringr, tidyr, Hmisc, arsenal, survminer, rgenoud, MatchIt, MASS, car, relaimpo, ggpubr)
temp_csv <- import("C:/SDcard/Study/1 temperature/temp_sepsis analysis/temp_sepsis_final2(1st_stay only+shock+site).csv")

# data cleanse & classification
# temp site
temp_csv$temp_site[temp_csv$temp_site == 0] = "Other"
temp_csv$temp_site <- as.factor(temp_csv$temp_site)
levels(temp_csv$temp_site)[levels(temp_csv$temp_site) %in% c("oral", "ORAL", "po", "PO", "OR", ".po", "orally", ".OR", ".PO",
                                                             "Gastric Tube, Oral", "oal", "or", "oraL")] = "Oral"
levels(temp_csv$temp_site)[levels(temp_csv$temp_site) %in% c("axillary", "AXILLARY", "Axilla", "ax", "Ax", "AX", "axilla", "Axllry",
                                                             ".AX", "AdultAxillary", "axil", "axilalry", "axill", "AXL")] = "Axillary"
levels(temp_csv$temp_site)[levels(temp_csv$temp_site) %in% c("Temporal scan", "temporal", "TEMPORAL ARTERY", "Temp Artery",
                                                             "TA", "Temporal Artery Scan", ".ta", ".TA", "TEMPORAL", "Temporal Artery",
                                                             "temp art", "ta", "TEMPOROL", "Temporal artery scan", "Tempor", ".Ta",
                                                             "TEMP ART", ",TA", ".tA", "TA-", "tem", "temopral", "Temp art", "tempart",
                                                             "Tpl")] = "Temporal"
levels(temp_csv$temp_site)[levels(temp_csv$temp_site) %in% c("tympanic", "TYMPANIC", "tym")] = "Tympanic"
levels(temp_csv$temp_site)[levels(temp_csv$temp_site) %in% c("core",  "CORE", "Core Temperature", 
                                                             "rectal", "RECTAL", ".rec", "rec", "REC", ".REC", "RA", "Rec", "rect",
                                                             "rectaal", "rectally", "Rectal",
                                                             "PA CATHETER", "Intravascular (swan)", "Blood", "Core central line", 
                                                             "femoral", "pa", "pa line", "Vascular", "Eso", "ESOPHAGEAL", "esoph", "Esophageal Probe", 
                                                             "Esophageal")] = "Core"
levels(temp_csv$temp_site)[levels(temp_csv$temp_site) %in% c("bladder", "BLADDER", "Bladr", "FOLEY", "Temperature Sensing Urinary Catheter",
                                                             "Core urinary catheter", "Foley")] = "Bladder"

levels(temp_csv$temp_site)[levels(temp_csv$temp_site) %in% c("1", "undocumented", "4", "temp", "Temprl", "102", "3", "103",
                                                             "T.", "O", "101", "0", "O.", "100.7", "89", "98.8", "99.1", "(Other)", "SB",
                                                             "SR", "BOLT", "Skin Sensor", "Forehead", "Groin", "skin", "groin", "SKIN", 
                                                             "Skin Temp Probe", "Skin")] = "Other"

# gender
temp_csv$gender_trans <- temp_csv$gender
temp_csv$gender_trans[temp_csv$gender_trans==0] = 'Female'
temp_csv$gender_trans[temp_csv$gender_trans==1] = 'Male'
temp_csv$gender_trans <- as.factor(temp_csv$gender_trans)
summary(temp_csv$gender_trans)

# age
temp_csv$age_trans <- temp_csv$age
temp_csv$age_trans[temp_csv$age_trans=='> 89'] = '91.5'
temp_csv$age_trans <- as.numeric(temp_csv$age_trans)
summary(temp_csv$age_trans)
# age category
temp_csv$age_category <- cut(temp_csv$age_trans,
                             breaks=c(-Inf,40,60,80,Inf),right = FALSE)

# APACHE
temp_csv$apache_iv[temp_csv$apache_iv<0] <- NA
temp_csv$apache_notemp <- temp_csv$apache_iv - temp_csv$apacheiv_temp
summary(temp_csv$apache_iv)
summary(temp_csv$apache_notemp)

# BMI
# Remove heights above 3 meter and below .5 meter.
temp_csv$admissionheight[temp_csv$admissionheight > 300 | temp_csv$admissionheight < 50] <- NA
# Remove weights below 20kg and above 600kg
temp_csv$admissionweight[temp_csv$admissionweight < 20 | temp_csv$admissionweight > 600] <- NA
# Compute the body mass index
temp_csv$BMI <- temp_csv$admissionweight / (temp_csv$admissionheight/100)^2
# Remove BMIs below 15 or above 100
temp_csv$BMI[temp_csv$BMI < 15 | temp_csv$BMI > 100] <- NA
summary(temp_csv$BMI)
# BMI category
temp_csv$BMI_category <- cut(temp_csv$BMI,
                             breaks=c(-Inf,18.5,25,30,Inf),right = FALSE)
temp_csv$BMI_category <- factor(temp_csv$BMI_category, 
                                levels=c("[-Inf,18.5)", "[18.5,25)", "[25,30)", "[30, Inf)"),
                                labels=c("Underweight", "Normal weight", "Overweight", "Obese"))

# ICU type
temp_csv$ICU = as.factor(temp_csv$unittype)
#classify into one CCU-CTICU
levels(temp_csv$ICU)[levels(temp_csv$ICU) %in% c("Cardiac ICU", "CSICU", "CTICU", "CCU-CTICU")] = "Cardiac ICU"
summary(temp_csv$ICU)

# race (add native american into other/unknown)
temp_csv$race = as.factor(temp_csv$ethnicity)
levels(temp_csv$race)[levels(temp_csv$race) %in% "Native American"] = "Other/Unknown"
levels(temp_csv$race)[levels(temp_csv$race) %in% ""] = "Other/Unknown"
summary(temp_csv$race)

# septic / non-septic
temp_csv$sepsis_flag[temp_csv$sepsis_flag==0] = 'Non-septic'
temp_csv$sepsis_flag[temp_csv$sepsis_flag==1] = 'Septic'
temp_csv$sepsis_flag <- as.factor(temp_csv$sepsis_flag)
summary(temp_csv$sepsis_flag)

# temperature grouping
temp_csv$tmax_icu_category <- cut(temp_csv$temp_max,
                          breaks=c(-Inf,36.5,37.5,38.5,39.5,Inf),right = FALSE)
summary(temp_csv$tmax_icu_category)

# flag change
temp_csv$hypertension_flag <- as.factor(temp_csv$hypertension_flag)
temp_csv$diabetes_flag <- as.factor(temp_csv$diabetes_flag)
temp_csv$ckd_flag <- as.factor(temp_csv$ckd_flag)
temp_csv$copd_flag <- as.factor(temp_csv$copd_flag)
temp_csv$heartfailure_flag <- as.factor(temp_csv$heartfailure_flag)
temp_csv$cancer_flag <- as.factor(temp_csv$cancer_flag)
temp_csv$aids <- as.factor(temp_csv$aids)
temp_csv$ventilation_flag <- as.factor(temp_csv$ventilation_flag)
temp_csv$dialysis_flag <- as.factor(temp_csv$dialysis_flag)
temp_csv$vasopressor_flag <- as.factor(temp_csv$vasopressor_flag)
temp_csv$antibiotics_flag <- as.factor(temp_csv$antibiotics_flag)
temp_csv$warming_flag <- as.factor(temp_csv$warming_flag)
temp_csv$antipyretics_flag <- as.factor(temp_csv$antipyretics_flag)
temp_csv$septicshock_flag <- as.factor(temp_csv$septicshock_flag)
temp_csv$hosp_mortality <- as.factor(temp_csv$hosp_mortality)
temp_csv$icu_mortality <- as.factor(temp_csv$icu_mortality)
# If someone expires in the ICU, then they expire in the hospital.
temp_csv$hosp_mortality[which(temp_csv$icu_mortality == 1)] <- 1

# presence of comorbidities
temp_csv$comorbidites_flag <- if_else(temp_csv$hypertension_flag == 1 | temp_csv$diabetes_flag == 1
                                      | temp_csv$ckd_flag == 1 | temp_csv$copd_flag == 1 | temp_csv$heartfailure_flag ==1
                                      | temp_csv$cancer_flag == 1, 1, 0)
temp_csv$comorbidites_flag <- as.factor(temp_csv$comorbidites_flag)




# histogram according to temp site
t.temporal <- temp_csv[temp_csv$temp_site %in% "Temporal", ]
t.axillary <- temp_csv[temp_csv$temp_site %in% "Axillary", ]
t.oral <- temp_csv[temp_csv$temp_site %in% "Oral", ]
t.core <- temp_csv[temp_csv$temp_site %in% "Core", ]
t.other <- temp_csv[temp_csv$temp_site %in% "Other", ]
t.bladder <- temp_csv[temp_csv$temp_site %in% "Bladder", ]
t.tympanic <- temp_csv[temp_csv$temp_site %in% "Tympanic", ]

h1 <- qplot(t.oral$temp_max,
            geom = "histogram",
            binwidth = 0.2,
            xlim = c(36,40),
            main = "(A) Oral",
            xlab = "Maximum body temperature(°„C)")
h2 <- qplot(t.temporal$temp_max,
            geom = "histogram",
            binwidth = 0.2,
            xlim = c(36,40),
            main = "(B) Temporal",
            xlab = "Maximum body temperature(°„C)")
h3 <- qplot(t.axillary$temp_max,
            geom = "histogram",
            binwidth = 0.2,
            xlim = c(36,40),
            main = "(C) Axillary",
            xlab = "Maximum body temperature(°„C)")
h4 <- qplot(t.tympanic$temp_max,
            geom = "histogram",
            binwidth = 0.2,
            xlim = c(36,40),
            main = "(D) Tympanic",
            xlab = "Maximum body temperature(°„C)")
h5 <- qplot(t.core$temp_max,
            geom = "histogram",
            binwidth = 0.2,
            xlim = c(36,40),
            main = "(E) Core",
            xlab = "Maximum body temperature(°„C)")
h6 <- qplot(t.bladder$temp_max,
            geom = "histogram",
            binwidth = 0.2,
            xlim = c(36,40),
            main = "(F) Bladder",
            xlab = "Maximum body temperature(°„C)")
h7 <- qplot(t.other$temp_max,
            geom = "histogram",
            binwidth = 0.2,
            xlim = c(36,40),
            main = "(G) Other",
            xlab = "Maximum body temperature(°„C)")
ggarrange(h1,h2,h3,h4,h5,h6,h7 + rremove("x.text"),
          ncol=4, nrow=2)



# table 1 septic/non-septic baseline table
my_controls <- tableby.control(
  test = T,
  total = T,
  numeric.test = "kwt", cat.test = "chisq",
  numeric.stats = c("meansd", "medianq1q3", "range", "Nmiss2"),
  cat.stats = c("countpct", "Nmiss2"),
  stats.labels = list(
    meansd = "Mean (SD)",
    medianq1q3 = "Median (Q1, Q3)",
    range = "Min - Max",
    Nmiss2 = "Missing"
  )
)

my_labels <- list(
  age_trans = "Age, years", gender_trans = "Male, %", BMI = "BMI, kg/m2", race = "Ethnicity, %",
  ICU = "Admitted ICU type, %", apache_iv = "APACHE IV score", apache_notemp = "APACHE IV score nt", hypertension_flag = "Hypertension, %",
  diabetes_flag = "Diabetes mellitus, %", ckd_flag = "CKD, %", copd_flag = "COPD, %", heartfailure_flag="Congestive heart failure, %",
  cancer_flag = "Cancer, %", aids="AIDS, %", ventilation_flag="Ventilation, %", dialysis_flag="Dialysis, %",
  vasopressor_flag="Vasopressor, %", antibiotics_flag="Antimicrobial therapy, %", hosp_mortality="In-hospital mortality, %", 
  icu_mortality="ICU mortality, %", hosp_los_hours="Hospital LOS, hours", icu_los_hours="ICU LOS, hours", 
  temp_max="Tmax, °„C" , hr_avg_24hours="Heart rate, bpm", rr_avg_24hours="Respiratory rate, bpm",
  spo2_avg_24hours="Peripheral oxygen saturation, %", map_avg_24hours="Mean arterial pressure, mmHg", 
  wbc_max="Peak white blood cell counts, *10^9/L", nlr_max="Peak NLR", hgb_avg="Hemoglobin, g/dL", 
  hct_avg="Hematocrit, %", plt_avg="Platelets, *10^9/L"
)


table1.1 <- tableby(sepsis_flag ~ age_trans + gender_trans + BMI + race + ICU
                      + apache_iv + hypertension_flag + diabetes_flag + ckd_flag
                      + copd_flag + heartfailure_flag + cancer_flag + aids, 
                      data = temp_csv, control = my_controls)

summary(table1.1, labelTranslations = my_labels, text = TRUE)




# table 2 temperature outcome table
temp_csv_twoflag = split(temp_csv, temp_csv$sepsis_flag)

my_controls2 <- tableby.control(
  test = T,
  total = T,
  numeric.test = "anova", cat.test = "chisq",
  numeric.stats = c("meansd", "medianq1q3", "range", "Nmiss2"),
  cat.stats = c("countpct", "Nmiss2"),
  stats.labels = list(
    meansd = "Mean (SD)",
    medianq1q3 = "Median (Q1, Q3)",
    range = "Min - Max",
    Nmiss2 = "Missing"
  )
)

table2.1 <- tableby(tmax_icu_category ~ age_trans + gender_trans + BMI
                    + apache_notemp + comorbidites_flag + ventilation_flag + dialysis_flag + vasopressor_flag + antibiotics_flag
                    + temp_min + t_change + t_max_24hours
                    + hr_avg_24hours + rr_avg_24hours + spo2_avg_24hours + map_avg_24hours + wbc_max + nlr_max
                    + hosp_mortality + icu_mortality, 
                    data = temp_csv_twoflag[[1]], control = my_controls2)

table2.2 <- tableby(tmax_icu_category ~ age_trans + gender_trans + BMI
                    + apache_notemp + comorbidites_flag + ventilation_flag + dialysis_flag + vasopressor_flag + antibiotics_flag
                    + temp_min + t_change + t_max_24hours
                    + hr_avg_24hours + rr_avg_24hours + spo2_avg_24hours + map_avg_24hours + wbc_max + nlr_max
                    + hosp_mortality + icu_mortality, 
                    data = temp_csv_twoflag[[2]], control = my_controls2)

summary(table2.1, labelTranslations = my_labels, text = TRUE)
summary(table2.2, labelTranslations = my_labels, text = TRUE)


# KM survival plot
temp_csv$hosp_los_days <- temp_csv$hosp_los_hours/24
temp_csv_twoflag = split(temp_csv, temp_csv$sepsis_flag)

survival_time1 <- temp_csv_twoflag[[1]][,"hosp_los_days"]
survival_time2 <- temp_csv_twoflag[[2]][,"hosp_los_days"]
death1 <- as.numeric(temp_csv_twoflag[[1]][,"hosp_mortality"])
death2 <- as.numeric(temp_csv_twoflag[[2]][,"hosp_mortality"])
fivegroups1 <- temp_csv_twoflag[[1]][,"tmax_icu_category"]
fivegroups2 <- temp_csv_twoflag[[2]][,"tmax_icu_category"]

fit1.1 <- survfit(Surv(survival_time1, death1) ~ fivegroups1, data = temp_csv_twoflag[[1]])
fit1.2 <- survfit(Surv(survival_time2, death2) ~ fivegroups2, data = temp_csv_twoflag[[2]])

ggsurvplot(
  fit1.1,                     # survfit object with calculated statistics.
  data = temp_csv_twoflag[[1]],            
  pval = TRUE, fun = "pct",        
  xlim = c(0,28), break.time.by = 7, linetype = "strata", size = 1,
  risk.table = TRUE, risk.table.y.text.col = T, risk.table.y.text = FALSE, 
  xlab = "Days after ICU admission", legend = "bottom", legend.title = "Temperature category, °„C",
  legend.labs = c("<36.5", "[36.5,37.5)", "[37.5,38.5)", "[38.5,39.5)", "°›39.5"), censor=FALSE,
  ggtheme = theme_gray(), title = "(A) 28-day survival curve in non-septic patients")   

ggsurvplot(
  fit1.2,                     # survfit object with calculated statistics.
  data = temp_csv_twoflag[[2]],            
  pval = TRUE, fun = "pct",        
  xlim = c(0,28), break.time.by = 7, linetype = "strata", size = 1,
  risk.table = TRUE, risk.table.y.text.col = T, risk.table.y.text = FALSE, 
  xlab = "Days after ICU admission", legend = "bottom", legend.title = "Temperature category, °„C",
  legend.labs = c("<36.5", "[36.5,37.5)", "[37.5,38.5)", "[38.5,39.5)", "°›39.5"), censor=FALSE,
  ggtheme = theme_gray(), title = "(B) 28-day survival curve in septic patients")  




# Multivariate logistics regression
# 1 non-septic cohort
model1 <- glm(hosp_mortality ~ relevel(tmax_icu_category, "[36.5,37.5)") + relevel(age_category, "[-Inf,40)") + gender_trans + relevel(BMI_category, "Normal weight")
                + apache_notemp + relevel(ICU, "Med-Surg ICU") + hypertension_flag + diabetes_flag 
                + ckd_flag + copd_flag + heartfailure_flag + cancer_flag + ventilation_flag + dialysis_flag 
                + vasopressor_flag + antibiotics_flag + relevel(temp_site,"Oral"), data = temp_csv_twoflag[[1]], family = binomial)

exp(cbind("Odds ratio" = coef(model1), confint.default(model1, level = 0.95)))
summary(model1)


# 2 septic cohort 
model2 <- glm(hosp_mortality ~ relevel(tmax_icu_category, "[36.5,37.5)") + relevel(age_category, "[-Inf,40)") + gender_trans + relevel(BMI_category, "Normal weight")
                + apache_notemp + relevel(ICU, "Med-Surg ICU")  + hypertension_flag + diabetes_flag 
                + ckd_flag + copd_flag + heartfailure_flag + cancer_flag + ventilation_flag + dialysis_flag 
                + vasopressor_flag + antibiotics_flag + relevel(temp_site,"Oral"), 
                data = temp_csv_twoflag[[2]], family = binomial)

exp(cbind("Odds ratio" = coef(model2), confint.default(model2, level = 0.95)))
summary(model2)





# subgroup analysis 
# antimicrobial therapy
# septic group
t4s.1 <- temp_csv_twoflag[[2]][temp_csv_twoflag[[2]]$tmax_icu_category %in% "[-Inf,36.5)", ]
t4s.2 <- temp_csv_twoflag[[2]][temp_csv_twoflag[[2]]$tmax_icu_category %in% "[36.5,37.5)", ]
t4s.3 <- temp_csv_twoflag[[2]][temp_csv_twoflag[[2]]$tmax_icu_category %in% "[37.5,38.5)", ]
t4s.4 <- temp_csv_twoflag[[2]][temp_csv_twoflag[[2]]$tmax_icu_category %in% "[38.5,39.5)", ]
t4s.5 <- temp_csv_twoflag[[2]][temp_csv_twoflag[[2]]$tmax_icu_category %in% "[39.5, Inf)", ]

models_abs1 <- glm(hosp_mortality ~ antibiotics_flag + age_trans + gender_trans + apache_notemp, data = t4s.1, family = binomial)
models_abs2 <- glm(hosp_mortality ~ antibiotics_flag + age_trans + gender_trans + apache_notemp, data = t4s.2, family = binomial)
models_abs3 <- glm(hosp_mortality ~ antibiotics_flag + age_trans + gender_trans + apache_notemp, data = t4s.3, family = binomial)
models_abs4 <- glm(hosp_mortality ~ antibiotics_flag + age_trans + gender_trans + apache_notemp, data = t4s.4, family = binomial)
models_abs5 <- glm(hosp_mortality ~ antibiotics_flag + age_trans + gender_trans + apache_notemp, data = t4s.5, family = binomial)

# non-septic group
t4ns.1 <- temp_csv_twoflag[[1]][temp_csv_twoflag[[1]]$tmax_icu_category %in% "[-Inf,36.5)", ]
t4ns.2 <- temp_csv_twoflag[[1]][temp_csv_twoflag[[1]]$tmax_icu_category %in% "[36.5,37.5)", ]
t4ns.3 <- temp_csv_twoflag[[1]][temp_csv_twoflag[[1]]$tmax_icu_category %in% "[37.5,38.5)", ]
t4ns.4 <- temp_csv_twoflag[[1]][temp_csv_twoflag[[1]]$tmax_icu_category %in% "[38.5,39.5)", ]
t4ns.5 <- temp_csv_twoflag[[1]][temp_csv_twoflag[[1]]$tmax_icu_category %in% "[39.5, Inf)", ]

modelns_ab1 <- glm(hosp_mortality ~ antibiotics_flag + age_trans + gender_trans + apache_notemp, data = t4ns.1, family = binomial)
modelns_ab2 <- glm(hosp_mortality ~ antibiotics_flag + age_trans + gender_trans + apache_notemp, data = t4ns.2, family = binomial)
modelns_ab3 <- glm(hosp_mortality ~ antibiotics_flag + age_trans + gender_trans + apache_notemp, data = t4ns.3, family = binomial)
modelns_ab4 <- glm(hosp_mortality ~ antibiotics_flag + age_trans + gender_trans + apache_notemp, data = t4ns.4, family = binomial)
modelns_ab5 <- glm(hosp_mortality ~ antibiotics_flag + age_trans + gender_trans + apache_notemp, data = t4ns.5, family = binomial)


# bar plot of antibiotics
anti.ns <- data.frame(Groups=factor(c("<36.5","[36.5,37.5)","[37.5,38.5)","[38.5,39.5)", ">=39.5")),
                      anti=c(20.8, 16.5, 22.2, 28.4, 33.7))
anti.ns$Groups <- factor(anti.ns$Groups, 
                       levels = c("<36.5","[36.5,37.5)","[37.5,38.5)","[38.5,39.5)",">=39.5"))

ggplot(data=anti.ns,aes(x=Groups, y=anti, fill=Groups))+
  geom_bar(stat = "identity")+
  labs(x="Temperature Categories, °„C", y="Antibiotics usage, %")+theme(text = element_text(size=15))
  


anti.s <- data.frame(Groups=factor(c("<36.5","[36.5,37.5)","[37.5,38.5)","[38.5,39.5)", ">=39.5")),
                     anti=c(67.2, 57.2, 55.3, 54.4, 55.5))
anti.s$Groups <- factor(anti.s$Groups, 
                         levels = c("<36.5","[36.5,37.5)","[37.5,38.5)","[38.5,39.5)",">=39.5"))

ggplot(data=anti.s,aes(x=Groups, y=anti, fill=Groups))+
  geom_bar(stat = "identity")+
  labs(x="Temperature Categories, °„C", y="Antibiotics usage, %")+theme(text = element_text(size=15))



# forrest plot of antibiotics
pacman::p_load(forestplot, haven, gridExtra)

forplot <- import("C:/SDcard/Study/1 temperature/figures/temp_nonseptic.csv")
attach(forplot)
forplot$variables[forplot$variables %in% "°›39.5"] = ">=39.5"
forplot$variables[forplot$variables %in% "Temperature categories"] = "Temperature categories, °„C"

tiff(file="C:/SDcard/Study/1 temperature/figures/test.tiff", width=688, height=764, res=300)
forestplot(dt=forplot,as.matrix(forplot[,1:3]), OR, LowerCI, UpperCI, 
           graph.pos=2, zero=1, graphwidth=unit(50,"mm"), lineheight="auto",
           boxsize=0.1, xticks=(c(0.25,0.5,0.75,1.0,1.25,1.5)),col = fpColors(lines="blue", box="black"),
           ci.vertices=TRUE, ci.vertices.height = 0.15,
           xlab="<---Favors antibiotics---                           ---Opposes antibiotics--->", txt_gp = fpTxtGp(cex=1.5))


forplot.septic <- import("C:/SDcard/Study/1 temperature/figures/temp_septic.csv")
attach(forplot.septic)
forplot.septic$variables[forplot.septic$variables %in% "°›39.5"] = ">=39.5"
forplot.septic$variables[forplot.septic$variables %in% "Temperature categories"] = "Temperature categories, °„C"

tiff(file="C:/SDcard/Study/1 temperature/figures/test.tiff", width=688, height=764, res=300)
forestplot(dt=forplot.septic,as.matrix(forplot[,1:3]), OR, LowerCI, UpperCI, 
           graph.pos=2, zero=1, graphwidth=unit(50,"mm"), lineheight="auto",
           boxsize=0.1, xticks=(c(0.25,0.5,0.75,1.0,1.25,1.5)),col = fpColors(lines="blue", box="black"),
           ci.vertices=TRUE, ci.vertices.height = 0.15,
           xlab="<---Favors antibiotics---                           ---Opposes antibiotics--->", txt_gp = fpTxtGp(cex=1.5))


