
# ==============================================================================
# Maternal Mental Health and Birth Outcomes Project | Modeling Code
# March 2026
# Author: Joey Lee
# ==============================================================================
#
#   
#   This script was developed as part of a client-facing consulting project
#   completed by a five-person student team. I was responsible for developing,
#   implementing, and interpreting the regression modeling workflow shown here.
#
#   
#   This includes fitting linear regression models for gestational age and logistic
#   regression models for NICU placement using the cleaned ChatterBaby survey data.
#   Models compare alternative maternal mental health predictor definitions and
#   include subgroup/sensitivity analyses for singleton births, U.S.-only
#   respondents, and maternal trauma.
#
# ==============================================================================

# =========================
# 1. MODEL PREPROCESSING
# =========================

# Load cleaned data
data <- readRDS("mmh_cleaned_data.rds")

# Drop NAs in country_US
data <- data[!is.na(data$country_US), ]

# US-only data
data_us <- data[data$country_US == 1, ] # ~2000 observations

# Singleton-only data
data_singleton <- subset(data, twins_binary == 0)

# =========================
# 2. CORE GESTATIONAL AGE MODELS

# Method: MLR
# Outcome variable: Gestation weeks
# Dataset: Full data
# =========================

# ---- MODEL 1: Any mental health problem reported before/during pregnancy (0/1) ----

# Model 1a
# Mom's delivery age: linear
m1a <- lm(
  babygestation ~
    any_mh +
    momsdeliveryage + # linear moms age term
    dadsdeliveryage +
    maternalpregnancyproblems___17 +
    momsedu + 
    dadsedu +
    race_ai_an + race_asian + race_black + race_hispanic +
    race_pi + race_mena + race_white +
    tobacco_binary +
    marriage +
    twins_binary +
    sex_male,
  data = data
)
summary(m1a)

# Model 1a: Diagnostic plots 
par(mfrow = c(2,2))
plot(m1a)

# Model 1b
# Mom's delivery age: quadratic 
m1b <- lm(
  babygestation ~
    sex_male +
    maternalpregnancyproblems___17 +
    tobacco_binary +
    momsdeliveryage +
    I(momsdeliveryage^2) + # quadratic moms age term
    dadsdeliveryage +
    momsedu +
    dadsedu +
    race_ai_an + race_asian + race_black + race_hispanic +
    race_pi + race_mena + race_white +
    marriage +
    twins_binary +
    any_mh,
  data = data
)
summary(m1b)

# Model 1b: Diagnostic plots
par(mfrow = c(2,2))
plot(m1b)

# Model 1c
# Mom's delivery age: Over 35 (1), 35 or under (0)
m1c <- lm(
  babygestation ~
    sex_male +
    maternalpregnancyproblems___17 +
    tobacco_binary +
    momsage_over35 + # binary moms age
    dadsdeliveryage +
    momsedu +
    dadsedu +
    race_ai_an + race_asian + race_black + race_hispanic +
    race_pi + race_mena + race_white +
    marriage +
    twins_binary +
    any_mh,
  data = data
)
summary(m1c)

# Model 1c: Diagnostic plots
par(mfrow = c(2,2))
plot(m1c)

# ---- MODEL 2: Mental Health Count Categories (0, 1, 2, or 3+ MH conditions) ----

# Model 2a
# Mom's delivery age: linear 
m2a <- lm(
  babygestation ~
    mh_count_cat +
    momsdeliveryage + # linear moms age term
    dadsdeliveryage +
    maternalpregnancyproblems___17 +
    momsedu + 
    dadsedu +
    race_ai_an + race_asian + race_black + race_hispanic +
    race_pi + race_mena + race_white +
    tobacco_binary +
    marriage +
    twins_binary +
    sex_male,
  data = data
)
summary(m2a)

# Model 2a: Diagnostic plots
par(mfrow = c(2,2))
plot(m2a)

# Model 2b
# Mom's delivery age: quadratic
m2b <- lm(
  babygestation ~
    mh_count_cat +
    momsdeliveryage +
    I(momsdeliveryage^2) + # quadratic moms age term
    dadsdeliveryage +
    maternalpregnancyproblems___17 +
    momsedu + 
    dadsedu +
    race_ai_an + race_asian + race_black + race_hispanic +
    race_pi + race_mena + race_white +
    tobacco_binary +
    marriage +
    twins_binary +
    sex_male,
  data = data
)
summary(m2b)

# Model 2b: Diagnostic plots
par(mfrow = c(2,2))
plot(m2b)

# Model 2c 
# Mom's delivery age: Over 35 (1), 35 or under (0)
m2c <- lm(
  babygestation ~
    mh_count_cat +
    momsage_over35 + 
    dadsdeliveryage +
    maternalpregnancyproblems___17 +
    momsedu + 
    dadsedu +
    race_ai_an + race_asian + race_black + race_hispanic +
    race_pi + race_mena + race_white +
    tobacco_binary +
    marriage +
    twins_binary +
    sex_male,
  data = data
)
summary(m2c)

# Model 2c: Diagnostic plots
par(mfrow = c(2,2))
plot(m2c)

# ---- MODEL 3: Continuous MH Predictor (Total MH Conditions Reported) ----

# Model 3a
# Mom's delivery age: linear
m3a <- lm(
  babygestation ~
    mh_count +
    momsdeliveryage + # linear moms age term
    dadsdeliveryage +
    maternalpregnancyproblems___17 +
    momsedu + 
    dadsedu +
    race_ai_an + race_asian + race_black + race_hispanic +
    race_pi + race_mena + race_white +
    tobacco_binary +
    marriage +
    twins_binary +
    sex_male,
  data = data
)
summary(m3a)

# Model 3a: Diagnostic Plots
par(mfrow = c(2,2))
plot(m3a)

# Model 3b 
# Mom's delivery age: quadratic
m3b <- lm(
  babygestation ~
    mh_count +
    momsdeliveryage +
    I(momsdeliveryage^2) + # quadratic moms age term
    dadsdeliveryage +
    maternalpregnancyproblems___17 +
    momsedu + 
    dadsedu +
    race_ai_an + race_asian + race_black + race_hispanic +
    race_pi + race_mena + race_white +
    tobacco_binary +
    marriage +
    twins_binary +
    sex_male,
  data = data
)
summary(m3b)

# Model 3b: Diagnostic Plots
par(mfrow = c(2,2))
plot(m3b)

# Model 3c
# Mom's delivery age: Over 35 (1), 35 or under (0)
m3c <- lm(
  babygestation ~
    mh_count +
    momsage_over35 + 
    dadsdeliveryage +
    maternalpregnancyproblems___17 +
    momsedu +
    dadsedu +
    race_ai_an + race_asian + race_black + race_hispanic +
    race_pi + race_mena + race_white +
    tobacco_binary +
    marriage +
    twins_binary +
    sex_male,
  data = data
)
summary(m3c)

# Model 3c: Diagnostic Plots
par(mfrow = c(2,2))
plot(m3c)

# ---- MODEL 4: Individual Binary MH Indicators ----

# Model 4a
# Mom's delivery age: linear
m4a <- lm(
  babygestation ~
    sex_male +
    maternalpregnancyproblems___17 +
    tobacco_binary +
    momsdeliveryage +
    dadsdeliveryage +
    momsedu +
    dadsedu +
    race_ai_an + race_asian + race_black + race_hispanic +
    race_pi + race_mena + race_white +
    marriage +
    twins_binary +
    mat_dep_before_during +
    mat_anx_before_during +
    mat_ocd_before_during +
    mat_bp_before_during +
    mat_adhd_before_during +
    mat_pssd_before_during,
  data = data
)
summary(m4a)

# Model 4a: Diagnostic Plots
par(mfrow = c(2,2))
plot(m4a)

# Model 4b
# Mom's delivery age: quadratic 
m4b <- lm(
  babygestation ~
    sex_male +
    maternalpregnancyproblems___17 +
    tobacco_binary +
    momsdeliveryage +
    I(momsdeliveryage^2) + # quadratic moms age term
    dadsdeliveryage +
    momsedu +
    dadsedu +
    race_ai_an + race_asian + race_black + race_hispanic +
    race_pi + race_mena + race_white +
    marriage +
    twins_binary +
    mat_dep_before_during +
    mat_anx_before_during +
    mat_ocd_before_during +
    mat_bp_before_during +
    mat_adhd_before_during +
    mat_pssd_before_during,
  data = data
)
summary(m4b)

# Model 4b: Diagnostic Plots
par(mfrow = c(2,2))
plot(m4b)

# Model 4c
# Mom's delivery age: Over 35 (1), 35 or under (0)
m4c <- lm(
  babygestation ~
    sex_male +
    maternalpregnancyproblems___17 +
    tobacco_binary +
    momsage_over35 + # binary moms age
    dadsdeliveryage +
    momsedu +
    dadsedu +
    race_ai_an + race_asian + race_black + race_hispanic +
    race_pi + race_mena + race_white +
    marriage +
    twins_binary +
    mat_dep_before_during +
    mat_anx_before_during +
    mat_ocd_before_during +
    mat_bp_before_during +
    mat_adhd_before_during +
    mat_pssd_before_during,
  data = data
)
summary(m4c)

# Model 4c: Diagnostic Plots
par(mfrow = c(2,2))
plot(m4c)

# =========================
# 3. SUPPLEMENTARY GESTATIONAL AGE MODELS: SINGLE BIRTHS ONLY

# Method: MLR
# Outcome variable: Gestation weeks
# Dataset: Singleton births only
# Mom's delivery age: linear
# =========================

# Model s1
# Mental Health Predictors: Any mental health problem reported before/during pregnancy (0/1)
ms1 <- lm(
  babygestation ~
    any_mh +
    momsdeliveryage + # linear moms age term
    dadsdeliveryage +
    maternalpregnancyproblems___17 +
    momsedu + 
    dadsedu +
    race_ai_an + race_asian + race_black + race_hispanic +
    race_pi + race_mena + race_white +
    tobacco_binary +
    marriage +
    sex_male,
  data = data_singleton
)

summary(ms1)

# Model s1: Diagnostic Plots
par(mfrow = c(2,2))
plot(ms1)

# Model s2
# Mental Health Predictors: Count Categories (0,1,2,3+ MH problems)
ms2 <- lm(
  babygestation ~
    mh_count_cat +
    momsdeliveryage + # linear moms age term
    dadsdeliveryage + 
    maternalpregnancyproblems___17 +
    momsedu + 
    dadsedu +
    race_ai_an + race_asian + race_black + race_hispanic +
    race_pi + race_mena + race_white +
    tobacco_binary +
    marriage +
    sex_male,
  data = data_singleton
)
summary(ms2)

# Model s2: Diagnostic Plots
par(mfrow = c(2,2))
plot(ms2)

# Model s3
# Mental Health Predictors: Continuous MH Count (Total MH Conditions)
ms3 <- lm(
  babygestation ~
    mh_count +
    momsdeliveryage + # linear moms age term
    dadsdeliveryage +
    maternalpregnancyproblems___17 +
    momsedu + 
    dadsedu +
    race_ai_an + race_asian + race_black + race_hispanic +
    race_pi + race_mena + race_white +
    tobacco_binary +
    marriage +
    sex_male,
  data = data_singleton
)
summary(ms3)

# Model s3: Diagnostic Plots
par(mfrow = c(2,2))
plot(ms3)

# Model s4
# Mental Health Predictors: Individual binary indicators
ms4 <- lm(
  babygestation ~
    mat_dep_before_during + # binary mental health predictors (only before and during pregnancy)
    mat_anx_before_during +
    mat_ocd_before_during +
    mat_bp_before_during +
    mat_adhd_before_during +
    mat_pssd_before_during + 
    momsdeliveryage + # linear moms age term
    dadsdeliveryage +
    maternalpregnancyproblems___17 +
    momsedu + 
    dadsedu +
    race_ai_an + race_asian + race_black + race_hispanic +
    race_pi + race_mena + race_white +
    tobacco_binary +
    marriage +
    sex_male,
  data = data_singleton
)
summary(ms4)

# Model s4: Diagnostic Plots
par(mfrow = c(2,2))
plot(ms4)

# =========================
# 4. SUPPLEMENTARY GESTATIONAL AGE MODELS: US ONLY

# Method: MLR
# Outcome variable: Gestation weeks
# Dataset: US only
# =========================

# Model u1
# Mental Health Predictors: Individual binary indicators
# Mom's delivery age: linear
mu1 <- lm(
  babygestation ~
    mat_dep_before_during +
    mat_anx_before_during +
    mat_ocd_before_during +
    mat_bp_before_during +
    mat_adhd_before_during +
    mat_pssd_before_during +
    momsdeliveryage +
    dadsdeliveryage +
    maternalpregnancyproblems___17 +
    momsedu + 
    dadsedu +
    race_ai_an + race_asian + race_black + race_hispanic +
    race_pi + race_mena + race_white +
    tobacco_binary +
    marriage +
    twins_binary +
    sex_male,
  data = data_us
)
summary(mu1)

# Model u1: Diagnostic Plots
par(mfrow = c(2,2))
plot(mu1)

# Model u2
# Mental Health Predictors: Individual binary indicators
# Mom's delivery age: quadratic
mu2 <- lm(
  babygestation ~
    sex_male +
    maternalpregnancyproblems___17 +
    tobacco_binary +
    momsdeliveryage +
    I(momsdeliveryage^2) +
    dadsdeliveryage +
    momsedu +
    dadsedu +
    race_ai_an + race_asian + race_black + race_hispanic +
    race_pi + race_mena + race_white +
    marriage +
    twins_binary +
    mat_dep_before_during +
    mat_anx_before_during +
    mat_ocd_before_during +
    mat_bp_before_during +
    mat_adhd_before_during +
    mat_pssd_before_during,
  data = data_us
)
summary(mu2)

# Model u2: Diagnostic Plots
par(mfrow = c(2,2))
plot(mu2)

# Model u3
# Mental Health Predictors: Individual binary indicators
# Mom's delivery age: Over 35 (1), 35 or under (0)
mu3 <- lm(
  babygestation ~
    sex_male +
    maternalpregnancyproblems___17 +
    tobacco_binary +
    momsage_over35 +
    dadsdeliveryage +
    momsedu +
    dadsedu +
    race_ai_an + race_asian + race_black + race_hispanic +
    race_pi + race_mena + race_white +
    marriage +
    twins_binary +
    mat_dep_before_during +
    mat_anx_before_during +
    mat_ocd_before_during +
    mat_bp_before_during +
    mat_adhd_before_during +
    mat_pssd_before_during,
  data = data_us
)
summary(mu3)

# Model u3: Diagnostic Plots
par(mfrow = c(2,2))
plot(mu3)

# Model u4
# Mental Health Predictors: Any Mental Health Condition 
# Mom's delivery age: linear
mu4 <- lm(
  babygestation ~
    any_mh +
    momsdeliveryage +
    dadsdeliveryage +
    maternalpregnancyproblems___17 +
    momsedu + 
    dadsedu +
    race_ai_an + race_asian + race_black + race_hispanic +
    race_pi + race_mena + race_white +
    tobacco_binary +
    marriage +
    twins_binary +
    sex_male,
  data = data_us
)

summary(mu4)

# Model u4: Diagnostic Plots
par(mfrow = c(2,2))
plot(mu4)

# Model u5
# Mental Health Predictors: Any Mental Health Condition 
# Mom's delivery age: quadratic
mu5 <- lm(
  babygestation ~
    sex_male +
    maternalpregnancyproblems___17 +
    tobacco_binary +
    momsdeliveryage +
    I(momsdeliveryage^2) +
    dadsdeliveryage +
    momsedu +
    dadsedu +
    race_ai_an + race_asian + race_black + race_hispanic +
    race_pi + race_mena + race_white +
    marriage +
    twins_binary +
    any_mh,
  data = data_us
)
summary(mu5)

# Model u5: Diagnostic Plots
par(mfrow = c(2,2))
plot(mu5)

# Model u6
# Mental Health Predictors: Any Mental Health Condition 
# Mom's delivery age: Over 35 (1) vs 35 or under (0)
mu6 <- lm(
  babygestation ~
    sex_male +
    maternalpregnancyproblems___17 +
    tobacco_binary +
    momsage_over35 +
    dadsdeliveryage +
    momsedu +
    dadsedu +
    race_ai_an + race_asian + race_black + race_hispanic +
    race_pi + race_mena + race_white +
    marriage +
    twins_binary +
    any_mh,
  data = data_us
)
summary(mu6)

# Model u6: Diagnostic Plots
par(mfrow = c(2,2))
plot(mu6)

# Model u7
# Mental Health Predictors: Continuous MH Count (Total MH Conditions)
# Mom's delivery age: linear
mu7 <- lm(
  babygestation ~
    mh_count +
    momsdeliveryage +
    dadsdeliveryage +
    maternalpregnancyproblems___17 +
    momsedu + 
    dadsedu +
    race_ai_an + race_asian + race_black + race_hispanic +
    race_pi + race_mena + race_white +
    tobacco_binary +
    marriage +
    twins_binary +
    sex_male,
  data = data_us
)
summary(mu7)

# Model u7: Diagnostic Plots
par(mfrow = c(2,2))
plot(mu7)

# Model u8
# Mental Health Predictors: Continuous MH Count (Total MH Conditions)
# Mom's delivery age: quadratic
mu8 <- lm(
  babygestation ~
    mh_count +
    momsdeliveryage +
    I(momsdeliveryage^2) +
    dadsdeliveryage +
    maternalpregnancyproblems___17 +
    momsedu + 
    dadsedu +
    race_ai_an + race_asian + race_black + race_hispanic +
    race_pi + race_mena + race_white +
    tobacco_binary +
    marriage +
    twins_binary +
    sex_male,
  data = data_us
)
summary(mu8)

# Model u8: Diagnostic Plots
par(mfrow = c(2,2))
plot(mu8)

# Model u9
# Mental Health Predictors: Continuous MH Count (Total MH Conditions)
# Mom's delivery age: Over 35 (1) vs 35 or under (0)
mu9 <- lm(
  babygestation ~
    mh_count +
    momsage_over35 + 
    dadsdeliveryage +
    maternalpregnancyproblems___17 +
    momsedu + 
    dadsedu +
    race_ai_an + race_asian + race_black + race_hispanic +
    race_pi + race_mena + race_white +
    tobacco_binary +
    marriage +
    twins_binary +
    sex_male,
  data = data_us
)
summary(mu9)

# Model u9: Diagnostic Plots
par(mfrow = c(2,2))
plot(mu9)

# Model u10
# Mental Health Predictors: MH Condition Count Categories (0, 1, 2, 3+)
# Mom's delivery age: linear
mu10 <- lm(
  babygestation ~
    mh_count_cat +
    momsdeliveryage +
    dadsdeliveryage +
    maternalpregnancyproblems___17 +
    momsedu + 
    dadsedu +
    race_ai_an + race_asian + race_black + race_hispanic +
    race_pi + race_mena + race_white +
    tobacco_binary +
    marriage +
    twins_binary +
    sex_male,
  data = data_us
)
summary(mu10)

# Model u10: Diagnostic Plots
par(mfrow = c(2,2))
plot(mu10)

# =========================
# 5. SUPPLEMENTARY GESTATIONAL AGE MODELS: + MATERNAL TRAUMA

# Method: MLR
# Outcome variable: Gestation weeks
# Dataset: Full data
# =========================

# Model t1 
# Mental Health Predictors: Any MH condition (binary)
# Mom's delivery age: linear
# Trauma: Any trauma (1) vs no trauma (0)
mt1 <- lm(
  babygestation ~
    any_mh +
    momsdeliveryage + # linear moms age term
    dadsdeliveryage +
    maternalpregnancyproblems___17 +
    momsedu + 
    dadsedu +
    race_ai_an + race_asian + race_black + race_hispanic +
    race_pi + race_mena + race_white +
    tobacco_binary +
    marriage +
    twins_binary +
    sex_male +
    mom_trauma,
  data = data
)
summary(mt1)

# Model t1: Diagnostic Plots
par(mfrow = c(2,2))
plot(mt1)

# Model t2
# Mental Health Predictors: Any MH condition (binary)
# Mom's delivery age: linear
# Trauma: Any trauma + Interaction of any MH condition and any trauma
mt2 <- lm(
  babygestation ~
    any_mh +
    momsdeliveryage + # linear moms age term
    dadsdeliveryage +
    maternalpregnancyproblems___17 +
    momsedu + 
    dadsedu +
    race_ai_an + race_asian + race_black + race_hispanic +
    race_pi + race_mena + race_white +
    tobacco_binary +
    marriage +
    twins_binary +
    sex_male +
    mom_trauma + 
    any_mh:mom_trauma,
  data = data
)
summary(mt2)

# Model t2: Diagnostic Plots
par(mfrow = c(2,2))
plot(mt2)

# Model t3
# Mental Health Predictors: Any MH condition (binary)
# Mom's delivery age: linear
# Trauma: Categorized trauma types
# Dataset: Full data 
mt3 <- lm(
  babygestation ~
    any_mh +
    momsdeliveryage + # linear moms age term
    dadsdeliveryage +
    maternalpregnancyproblems___17 +
    momsedu + 
    dadsedu +
    race_ai_an + race_asian + race_black + race_hispanic +
    race_pi + race_mena + race_white +
    tobacco_binary +
    marriage +
    twins_binary +
    sex_male +
    trauma_community + trauma_maltreatment + trauma_collective + trauma_loss_medical + trauma_interpersonal + trauma_other,
  data = data
)
summary(mt3)

# Model t3: Diagnostic Plots
par(mfrow = c(2,2))
plot(mt3)

# =========================
# 6. CORE NICU PLACEMENT MODELS

# Method: Logistic Regression
# Outcome variable: NICU placement
# Dataset: US-only
# =========================

# Model 5
# Mom's delivery age: linear
# Mental Health Predictor: Any mental health problem reported before/during pregnancy (0/1)
m5 <- glm(nicuplacement~
             babygestation + 
             sex_male + 
             maternalpregnancyproblems___17 + 
             tobacco_binary + 
             momsdeliveryage + 
             dadsdeliveryage + 
             momsedu + 
             dadsedu + 
             race_ai_an + 
             race_asian + 
             race_black + 
             race_hispanic + 
             race_pi + 
             race_mena + 
             race_white + 
             twins_binary + 
             marriage + 
             any_mh, family = binomial, data = data_us)
summary(m5)

# Model 5: Diagnostic Plots
plot(fitted(m5), residuals(m5, type = "deviance"))
abline(h = 0, col = "red")

# Model 6 (babygestation removed)
# Mom's delivery age: linear
# Mental Health Predictor: Any mental health problem reported before/during pregnancy (0/1)
m6 <- glm(nicuplacement~
             sex_male + 
             maternalpregnancyproblems___17 + 
             tobacco_binary + 
             momsdeliveryage + 
             dadsdeliveryage + 
             momsedu + 
             dadsedu + 
             race_ai_an + 
             race_asian + 
             race_black + 
             race_hispanic + 
             race_pi + 
             race_mena + 
             race_white + 
             twins_binary + 
             marriage + 
             any_mh, family = binomial, data = data_us)
summary(m6)

# Model 6: Diagnostic plots
plot(fitted(m6), residuals(m6, type = "deviance"))
abline(h = 0, col = "red")

# Model 7 (babygestation removed)
# Mom's delivery age: linear
# Mental Health Predictor: Mental Health Count Categories (0, 1, 2, or 3+ MH conditions)
m7 <- glm(nicuplacement~
             sex_male + 
             maternalpregnancyproblems___17 + 
             tobacco_binary + 
             momsdeliveryage + 
             dadsdeliveryage + 
             momsedu + 
             dadsedu + 
             race_ai_an + 
             race_asian + 
             race_black + 
             race_hispanic + 
             race_pi + 
             race_mena + 
             race_white + 
             twins_binary + 
             marriage + 
             mh_count_cat, family = binomial, data = data_us)
summary(m7)

# Model 7: Diagnostic Plots
plot(fitted(m7), residuals(m7, type = "deviance"))
abline(h = 0, col = "red")

# Model 8 (babygestation removed)
# Mom's delivery age: linear
# Mental Health Predictor: Continuous MH Predictor (Total MH Conditions)
m8 <- glm(nicuplacement~
             sex_male + 
             maternalpregnancyproblems___17 + 
             tobacco_binary + 
             momsdeliveryage + 
             dadsdeliveryage + 
             momsedu + 
             dadsedu + 
             race_ai_an + 
             race_asian + 
             race_black + 
             race_hispanic + 
             race_pi + 
             race_mena + 
             race_white + 
             twins_binary + 
             marriage + 
             mh_count, family = binomial, data = data_us)
summary(m8)

# Model 8: Diagnostic Plots
plot(fitted(m8), residuals(m8, type = "deviance"))
abline(h = 0, col = "red")

# Model 9 (babygestation removed)
# Mom's delivery age: linear
# Mental Health Predictor: Individual MH indicators
m9 <- glm(nicuplacement~
             sex_male + 
             maternalpregnancyproblems___17 + 
             tobacco_binary + 
             momsdeliveryage + 
             dadsdeliveryage + 
             momsedu + 
             dadsedu + 
             race_ai_an + 
             race_asian + 
             race_black + 
             race_hispanic + 
             race_pi + 
             race_mena + 
             race_white + 
             twins_binary + 
             marriage + 
             mat_dep_before_during + # binary mental health predictors (only before and during pregnancy)
             mat_anx_before_during +
             mat_ocd_before_during +
             mat_bp_before_during +
             mat_adhd_before_during +
             mat_pssd_before_during, family = binomial, data = data_us)
summary(m9)

# Model 9: Diagnostic Plots
plot(fitted(m9), residuals(m9, type = "deviance"))
abline(h = 0, col = "red")

# =========================
# 7. SUPPLEMENTARY NICU PLACEMENT MODEL: MATERNAL TRAUMA

# Method: Logistic Regression
# Outcome variable: NICU placement
# Mom's delivery age: linear
# Dataset: US-only
# Trauma: Any trauma (1) vs No trauma (0)
# =========================

# Model t4
mt4 <- glm(nicuplacement~
             sex_male + 
             maternalpregnancyproblems___17 + 
             tobacco_binary + 
             momsdeliveryage + 
             dadsdeliveryage + 
             momsedu + 
             dadsedu + 
             race_ai_an + 
             race_asian + 
             race_black + 
             race_hispanic + 
             race_pi + 
             race_mena + 
             race_white + 
             twins_binary + 
             marriage + 
             mom_trauma, family = binomial, data = data_us)
summary(mt4)

# Model t4: Diagnostic Plots
plot(fitted(mt4), residuals(mt4, type = "deviance"))
abline(h = 0, col = "red")

# =========================
# 8. SUPPLEMENTARY ANALYSES
# =========================

# QUESTION: Are non-NICU respondents/NAs for NICU are more likely to be outside the US?

data_raw <- read.csv("ChatterBaby_2026_DID.csv") # load raw data

data_raw$country_US <- ifelse(data_raw$country == 1, 1, 0) # create column for US status

data_raw$nicu_missing <- is.na(data_raw$nicuplacement)

# table of respondents by US/non-US status and NICU missingness
table(data_raw$country_US, data_raw$nicu_missing)

# perform chi-sq. test to check for association between country and missing NICU
chisq.test(table(data_raw$country_US, data_raw$nicu_missing)) # p < 0.05 => Non-NICU respondents significantly more likely to be outside the US

# QUESTION: Does shorter gestation increase risk of postpartum depression?

m_ppd <- glm(mat_dep_after ~ 
               babygestation +
               momsdeliveryage +
               dadsdeliveryage +
               maternalpregnancyproblems___17 +
               momsedu +
               dadsedu + 
               race_ai_an + 
               race_asian + 
               race_black + 
               race_hispanic +
               race_pi + 
               race_mena + 
               race_white +
               tobacco_binary + 
               marriage + 
               twins_binary + 
               sex_male,
             family = binomial,
             data = data
)
summary(m_ppd)

# QUESTION: Does NICU admission increase risk of postpartum depression?

m_ppd2 <- glm(mat_dep_after ~ 
                nicuplacement +
                momsdeliveryage +
                dadsdeliveryage +
                momsedu +
                dadsedu + 
                maternalpregnancyproblems___17 +
                race_ai_an + 
                race_asian + 
                race_black + 
                race_hispanic +
                race_pi + 
                race_mena + 
                race_white +
                tobacco_binary + 
                marriage + 
                twins_binary + 
                sex_male,
              family = binomial,
              data = data
)
summary(m_ppd2)

