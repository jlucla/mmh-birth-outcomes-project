# ==============================================================================
# Maternal Mental Health and Birth Outcomes Project | Data Preprocessing and Feature Engineering
# March 2026
# ==============================================================================
# 
#   Client-facing consulting project completed by a five-person student team.
#
#   This script cleans raw survey data collected from the ChatterBaby app, removing 
#   post-outcome variables, recoding predictors, handling missingness/skip logic, and 
#   constructing analysis-ready maternal mental health, trauma, demographic, family 
#   history, and substance-use variables.
#
#   I contributed to preprocessing, variable recoding, feature
#   engineering, missing-data handling, and preparation of the analysis-ready dataset.
#
# ==============================================================================

# Load libraries
library(dplyr)
library(stringr)

# Load raw dataset
data <- read.csv("ChatterBaby_2026_DID.csv")

# Remove biologically implausible observations
data <- data[-c(3329, 3894), ] # Gestation weeks = 25, NICU = 0 

# Remove incomplete surveys
data <- data %>%
  filter(en_demographics_timestamp != "[not completed]")

# Remove post-outcome and administrative variables
data <- data[, !(names(data) %in% c(
  "babyage",
  "babysweightlbs",
  "weightkg",
  "cryfrequency",
  "colic",
  "medicalhistorybaby___1",
  "medicalhistorybaby___2",
  "medicalhistorybaby___25",
  "medicalhistorybaby___23",
  "medicalhistorybaby___3",
  "medicalhistorybaby___20",
  "medicalhistorybaby___21",
  "medicalhistorybaby___4",
  "medicalhistorybaby___22",
  "medicalhistorybaby___14",
  "medicalhistorybaby___5",
  "medicalhistorybaby___6",
  "medicalhistorybaby___7",
  "medicalhistorybaby___8",
  "medicalhistorybaby___9",
  "medicalhistorybaby___10",
  "medicalhistorybaby___11",
  "medicalhistorybaby___12",
  "medicalhistorybaby___13",
  "medicalhistorybaby___15",
  "medicalhistorybaby___16",
  "medicalhistorybaby___17",
  "medicalhistorybaby___18",
  "medicalhistorybaby___24",
  "medicalhistorybaby___19",
  "medicalhistorybaby_other",
  "babymeds",
  "whichmedsforbaby",
  "asdbehaviors___1",
  "asdbehaviors___2",
  "asdbehaviors___3",
  "asdbehaviors___4",
  "asdbehaviors___5",
  "asdbehaviors___6",
  "asdbehaviors___7",
  "asdbehaviors___8",
  "asdbehaviors___9",
  "asdbehaviors___10",
  "asdbehaviors___11",
  "asdbehaviors___12",
  "asdbehaviors___13",
  "asdbehaviors___14",
  "asdbehaviors___15",
  "asdbehaviors___16",
  "asdbehaviors___17",
  "asdbehaviors___18",
  "asdbehaviors___19",
  "asdbehaviors___20",
  "asdbehaviors___21",
  "asdbehaviors___22",
  "lostskillsdemographics",
  "lostskillsdescriptiondemographics",
  "breastfedattempt",
  "babyfood",
  "babyfedother",
  "unusualbehaviors",
  "unusualtext",
  "otherbehaviors",
  "nicustay",
  "adoption",
  "en_demographics_timestamp",
  "consent",
  "language_number",
  "primaryhomelanguage",
  "language_other",
  "percent_language",
  "secondarylanguage",
  "secondlanguagetime",
  "language_other_2"
))]

# Convert nicuplacement to factor
data$nicuplacement <- as.factor(data$nicuplacement)

# -------------------------
# Demographic Variables
# -------------------------

# Convert sex to factor
data$sex <- as.factor(data$sex)
names(data)[names(data) == "sex"] <- "sex_male"

# Convert country to binary categorical US vs non-US
data$country <- ifelse(data$country == 1, 1, 0)
data$country <- as.factor(data$country)
names(data)[names(data) == "country"] <- "country_US"

# Set row with 1-year-old mother to NA
data$momsdeliveryage[data$momsdeliveryage == 1] <- NA

# Set implausible father delivery age values to NA
data$dadsdeliveryage[data$dadsdeliveryage == 1] <- NA

# Map free-text race responses from otherrace into binary race indicator variables.
# If no race category is identified, race indicators are set to NA.

## --- 1) Mapping 
label_to_col <- c(
  "American Indian/Alaskan Native"      = "race___0",
  "Asian"                               = "race___1",
  "Black"                               = "race___2",
  "Hispanic, Latino, Spanish"           = "race___3",
  "Native Hawaiian/Pacific Islander"    = "race___4",
  "Middle Eastern/North African"        = "race___5",
  "White"                               = "race___6"
)
race_cols <- unname(label_to_col)

# Safety check: make sure columns exist
missing_cols <- setdiff(race_cols, names(data))
if (length(missing_cols) > 0) {
  stop("Missing race columns in data: ", paste(missing_cols, collapse = ", "))
}

## --- 2) Corrections table ---
corrections <- data.frame(
  row_index = c(
    23, 81, 86, 169, 198, 265, 274, 288, 297, 339,
    354, 404, 456, 461, 467, 544, 640, 760, 873, 887,
    906, 976, 1018, 1025, 1130, 1165, 1190, 1205, 1220, 1232,
    1288, 1296, 1320, 1371, 1413, 1430, 1431, 1442, 1445, 1595
  ),
  race_text = c(
    "Asian, White",
    "Asian, White",
    "Asian",
    "Asian, White",
    "White",
    "Asian",
    NA,
    "White",
    NA,
    "Asian, White",
    "White",
    "Black, White",
    "Middle Eastern/North African",
    "White",
    NA,
    "White, Black",
    NA,
    "White",
    "Black, White",
    "White",
    "American Indian/Alaskan Native, White, Black",
    "Asian",
    "White",
    NA,
    "American Indian/Alaskan Native",
    "Black, Native Hawaiian/Pacific Islander, White",
    "Hispanic, Latino, Spanish",
    "White, Asian",
    "Asian",
    "White",
    "White",
    "Asian",
    "White",
    "Black",
    "White",
    "Asian",
    "White",
    "Black",
    NA,
    "White"
  ),
  stringsAsFactors = FALSE
)

## --- 3) Split race_text safely (preserve "Hispanic, Latino, Spanish") ---
split_races <- function(x) {
  if (is.na(x) || !nzchar(trimws(x))) return(character(0))
  
  # Protect the only label containing commas
  x2 <- gsub("Hispanic, Latino, Spanish", "Hispanic|Latino|Spanish", x, fixed = TRUE)
  
  parts <- trimws(strsplit(x2, ",")[[1]])
  parts <- gsub("Hispanic\\|Latino\\|Spanish", "Hispanic, Latino, Spanish", parts)
  parts <- parts[nzchar(parts)]
  parts
}

## --- 4) Apply overwrite corrections ---
for (k in seq_len(nrow(corrections))) {
  i <- corrections$row_index[k]
  races <- split_races(corrections$race_text[k])
  
  # Skip invalid indices or NA race_text
  if (length(races) == 0) next
  if (is.na(i) || i < 1 || i > nrow(data)) next
  
  # Set all mapped race columns to 0
  data[i, race_cols] <- 0L
  
  # Set listed races to 1
  cols_to_set <- unname(label_to_col[races])
  cols_to_set <- cols_to_set[!is.na(cols_to_set)]
  if (length(cols_to_set) > 0) data[i, cols_to_set] <- 1L
}


# --- Add these rows onto existing corrections ---
# Note: Row-level race corrections were manually validated from free-text survey responses
# and mapped into binary race indicator variables.
additional_corrections <- data.frame(
  row_index = c(
    1601,1614,1627,1650,1659,1676,1691,1695,1717,1723,
    1733,1821,1837,1857,1858,1863,1871,1873,1910,1924,
    1963,1972,1973,1994,2021,2079,2087,2214,2234,2236,2242,2246,2260,2272,2334,2375,2497,2512,2539,2557,
    2579,2581,2587,2591,2600,2629,2631,2634,2682,2684,2740,2760,2764,2788,2795,2822,2876,2913,2930,2953,
    2958,2981,2984,3023,3024,3034,3122,3130,3134,3174,3175,3177,3179,3207,3209,3211,3213,3257,3271,3288,
    3302,3342,3361,3385,3403,3493,3515,3521,3577,3625,3629,3651,3654,3680,3751,3772,3776,3853,3867,3869,
    3907,3913,3922,3956,4031,4047,4051,4102,4112,4158,4188,4195,4215,4216,4231,4234,4238,4262,4303,4307,
    4314,4342,4350,4427,4495,4533,4539
  ),
  race_text = c(
    "Asian","White","Black",NA,NA,"Hispanic, Latino, Spanish, Asian","White","White, Middle Eastern/North African","White","Asian",
    "White","American Indian/Alaskan Native","White, Asian","White","White","Asian","White, Hispanic, Latino, Spanish","White, Asian","White","Asian",
    "White","Asian, White",NA,"Asian",NA,"Asian, White","White, Asian","White","White","Hispanic, Latino, Spanish","White","White","Asian",NA,"Black","White","Black",NA,NA,"White",
    "White","White","Black","White",NA,"White, Black",NA,"White",NA,"Middle Eastern/North African","White",NA,NA,"Asian","Middle Eastern/North African",NA,NA,NA,"White","Black",
    "Black","Black","Black",NA,NA,"Black, White","Asian","Middle Eastern/North African","White, Asian","Asian, White",NA,"Black",NA,"White","White","Asian","White",NA,"Black","Hispanic, Latino, Spanish",
    "Black, White","Black","Asian","Black","White","Black, White","Asian","Native Hawaiian/Pacific Islander, White","White, Asian",NA,NA,"White, Asian",NA,"Black","White","American Indian/Alaskan Native","Native Hawaiian/Pacific Islander, White","Asian","White, Black","White, Black",
    "White","White","White",NA,"White",NA,NA,"Asian","Asian","Middle Eastern/North African","American Indian/Alaskan Native",NA,"Black","Asian","Asian",NA,"White, Hispanic, Latino, Spanish","Asian, White","Middle Eastern/North African","Black, White",
    "White","Asian, White","Middle Eastern/North African","Asian","Asian, White","Asian, White","Asian, White"
  ),
  stringsAsFactors = FALSE
)

# Append
corrections <- rbind(corrections, additional_corrections)

# Remove duplicates 
corrections <- corrections[!duplicated(corrections$row_index), ]

for (k in seq_len(nrow(corrections))) {
  i <- corrections$row_index[k]
  races <- split_races(corrections$race_text[k])
  
  if (length(races) == 0) next
  if (is.na(i) || i < 1 || i > nrow(data)) next
  
  data[i, race_cols] <- 0L
  cols_to_set <- unname(label_to_col[races])
  cols_to_set <- cols_to_set[!is.na(cols_to_set)]
  if (length(cols_to_set) > 0) data[i, cols_to_set] <- 1L
}


# Drop otherrace and race___7 after recoding
data <- data %>%
  select(-any_of(c("otherrace", "race___7")))

race_cols <- paste0("race___", 0:6)

# Identify rows where all race columns are 0 
rows_all_zero <- rowSums(data[, race_cols] == 1, na.rm = TRUE) == 0

# Set all race columns to NA for those rows
data[rows_all_zero, race_cols] <- NA

data$race___0 <- as.factor(data$race___0)
data$race___1 <- as.factor(data$race___1)
data$race___2 <- as.factor(data$race___2)
data$race___3 <- as.factor(data$race___3)
data$race___4 <- as.factor(data$race___4)
data$race___5 <- as.factor(data$race___5)
data$race___6 <- as.factor(data$race___6)

names(data)[names(data) == "race___0"] <- "race_ai_an"
names(data)[names(data) == "race___1"] <- "race_asian"
names(data)[names(data) == "race___2"] <- "race_black"
names(data)[names(data) == "race___3"] <- "race_hispanic"
names(data)[names(data) == "race___4"] <- "race_pi"
names(data)[names(data) == "race___5"] <- "race_mena"
names(data)[names(data) == "race___6"] <- "race_white"

# Mom's Education: convert category 6 (unknown/not sure) to NA

data <- data %>%
  mutate(
    momsedu = ifelse(momsedu == 6, NA, momsedu)
  )

# Dad's Education: convert category 6 (unknown/not sure) to NA

data <- data %>%
  mutate(
    dadsedu = ifelse(dadsedu == 6, NA, dadsedu)
  )

# Mom's Education: Convert to factor
data$momsedu <- as.factor(data$momsedu)

levels(data$momsedu) <- c(
  "less_than_hs",
  "hs_grad",
  "some_college",
  "bachelors",
  "graduate_degree"
)

# Dad's Education: Convert to factor
data$dadsedu <- as.factor(data$dadsedu)

levels(data$dadsedu) <- c(
  "less_than_hs",
  "hs_grad",
  "some_college",
  "bachelors",
  "graduate_degree"
)


# Marital Status: Collapse marriage categories 2/3/4 into "Previously Married", 6 = NA

data <- data %>%
  mutate(
    marriage = case_when(
      marriage == 1 ~ "Married",
      marriage %in% c(2, 3, 4) ~ "Previously Married",
      marriage == 5 ~ "Never Married",   
      marriage == 6 ~ NA_character_,
      TRUE ~ as.character(marriage)
    ),
    marriage = factor(marriage)
  )

# Birth Type: convert twinsormore to binary "Single Birth" vs "Multiple Births"
data$twins_binary <- ifelse(data$twinsormore >= 2, 1, 0)
data$twins_binary <- as.factor(data$twins_binary)

data$twinsormore <- NULL

# -------------------------
# Mental Health and Trauma Variables
# -------------------------

three_level_vars <- c("mat_dep", "mat_anx", "mat_ocd", "mat_bp", "mat_adhd", "mat_pssd")

# Loop through and create the two-category transformation
for (var in three_level_vars) {
  
  # Grouping 1 (Before) and 2 (During) into one column
  # Returns 1 if the value is 1 OR 2; otherwise 0 (including NAs)
  data[[paste0(var, "_before_during")]] <- ifelse(data[[var]] %in% c(1, 2), 1, 0)
  
  # Keeping 3 (After) as its own column 
  # Returns 1 if the value is 3; otherwise 0
  data[[paste0(var, "_after")]] <- ifelse(data[[var]] %in% 3, 1, 0)
}

# Trauma Variables (Specific Groupings)
data <- data |>
  mutate(
    # Interpersonal violence (direct victimization)
    trauma_interpersonal = as.integer(rowSums(across(c(what_trauma___1, what_trauma___2, 
                                                       what_trauma___6, what_trauma___13)), na.rm = TRUE) > 0),
    
    # maltreatment
    trauma_maltreatment = as.integer(rowSums(across(c(what_trauma___3, what_trauma___4)), na.rm = TRUE) > 0),
    
    # Community/societal violence (witnessed or experienced at broader scale)
    trauma_community = as.integer(rowSums(across(c(what_trauma___7, what_trauma___8, 
                                                   what_trauma___9)), na.rm = TRUE) > 0),
    
    # Collective/political trauma
    trauma_collective = as.integer(rowSums(across(c(what_trauma___war, what_trauma___10, 
                                                    what_trauma___11, what_trauma___12)), na.rm = TRUE) > 0),
    
    # Loss and medical
    trauma_loss_medical = as.integer(rowSums(across(c(what_trauma___5, what_trauma___14)), na.rm = TRUE) > 0),
    
    # Other
    trauma_other = what_trauma___15
  )

# Trauma Variables (Broader Groupings)
data <- data |>
  mutate(
    # direct physical violence/physical changes 
    trauma_physical = as.integer(rowSums(across(c(what_trauma___1, what_trauma___2, what_trauma___6, what_trauma___13, what_trauma___5, what_trauma___10)), na.rm = TRUE) > 0),
    # emotional factors
    trauma_emotional = as.integer(rowSums(across(c(what_trauma___3, what_trauma___4, what_trauma___14)), na.rm = TRUE) > 0),
    # community/societal violence, political/war trauma, other
    trauma_other = as.integer(rowSums(across(c(what_trauma___7, what_trauma___8, what_trauma___9, what_trauma___war, what_trauma___11, what_trauma___12, what_trauma___15)), na.rm = TRUE) > 0)) |>
  select(-starts_with("what_trauma___"), -trauma_optional)

# Family History Variables
# Test if the ddfamily_sibling and szbpfamily_sibling variables are significant in the model; drop them if they’re insignificant 
data <- data |> 
  select(-matches("family___[1238]$"),  # Drops anything ending in ___1, ___2, ___3, or ___8
         -ends_with("family_other"),    # Drops deaffamily_other, asdfamily_other, ddfamily_other
         -asd_genetic, 
         -numberfullsiblings, 
         -olderhalfsiblings, 
         -birthordertotal,
         -otherszbpfamilymember) |>
  mutate(deaffamily_sibling = if_else(if_any(num_range("deaffamily___", 4:7), ~ .x == 1), 1, 0), # Create the _sibling indicators
         asdfamily_sibling  = if_else(if_any(num_range("asdfamily___", 4:7), ~ .x == 1), 1, 0),
         ddfamily_sibling   = if_else(if_any(num_range("ddfamily___", 4:7), ~ .x == 1), 1, 0),
         szbpfamily_sibling = if_else(if_any(num_range("szbpfamily___", 4:7), ~ .x == 1), 1, 0) ) |>
  select(-matches("family___[4-7]$"), # Drop the leftover ___4 to ___7 columns
         -deaffamily_sibling,
         -asdfamily_sibling)

# -------------------------
# Substance Use Variables
# -------------------------

data$tobacco_binary <- data$tobacco_pregnancy_when___1 | 
  data$tobacco_pregnancy_when___2 | 
  data$tobacco_pregnancy_when___3 | 
  data$vaping_pregnancy___1 | 
  data$vaping_pregnancy___2 | 
  data$vaping_pregnancy___3

data$drugs_alcohol_tobacco_binary <- data$drugsalcoholtobacco___1 | 
  data$drugsalcoholtobacco___2 | 
  data$drugsalcoholtobacco___3 | 
  data$drugsalcoholtobacco___4 | 
  data$drugsalcoholtobacco___5 | 
  data$drugsalcoholtobacco___6 | 
  data$drugsalcoholtobacco___7 | 
  data$drugsalcoholtobacco___8

data$cannabis_or_alcohol <- data$alcohol_pregnancy_when___1 | 
  data$alcohol_pregnancy_when___2 | 
  data$alcohol_pregnancy_when___3 | 
  data$cannabis_pregnancy_when___1 | 
  data$cannabis_pregnancy_when___2 | 
  data$cannabis_pregnancy_when___3

data$other_drugs <- data$stimulants_pregnancy_when___1 | 
  data$stimulants_pregnancy_when___2 | 
  data$stimulants_pregnancy_when___3 | 
  data$opioids_pregnancy_when___1 | 
  data$opioids_pregnancy_when___2 | 
  data$opioids_pregnancy_when___3 | 
  data$psychoactive_pregnancy___1 | 
  data$psychoactive_pregnancy___2 | 
  data$psychoactive_pregnancy___3 | 
  data$other_pregnancy_drug___1 | 
  data$other_pregnancy_drug___2 | 
  data$other_pregnancy_drug___3

data <- subset(data, select = -c(farmwork, farmmomwhen___1,farmmomwhen___2,farmmomwhen___3,farmmomwhen___4,farmdadwhen___1,farmdadwhen___2,farmdadwhen___3,farmdadwhen___4,mompesticides,dadpesticides,drugsalcoholtobacco___1,drugsalcoholtobacco___8,drugsalcoholtobacco___2,drugsalcoholtobacco___3,drugsalcoholtobacco___4,drugsalcoholtobacco___5,drugsalcoholtobacco___6,drugsalcoholtobacco___7,otherdrug,tobacco_pregnancy_when___1,tobacco_pregnancy_when___2,tobacco_pregnancy_when___3,vaping_pregnancy___1,vaping_pregnancy___2,vaping_pregnancy___3,alcohol_pregnancy_when___1,alcohol_pregnancy_when___2,alcohol_pregnancy_when___3,cannabis_pregnancy_when___1,cannabis_pregnancy_when___2,cannabis_pregnancy_when___3,stimulants_pregnancy_when___1,stimulants_pregnancy_when___2,stimulants_pregnancy_when___3,opioids_pregnancy_when___1,opioids_pregnancy_when___2,opioids_pregnancy_when___3,psychoactive_pregnancy___1,psychoactive_pregnancy___2,psychoactive_pregnancy___3,other_pregnancy_drug___1,other_pregnancy_drug___2,other_pregnancy_drug___3))

# -------------------------
# ART Variables
# -------------------------

# 0) convert blank strings to NA
data <- data %>%
  mutate(across(where(is.character), ~ na_if(str_squish(.), "")))

# 1) Make sure ART columns are numeric 0/1 (if they came in as "0"/"1" strings)
art_method_cols <- paste0("artmethod___", 1:7)
art_method_cols <- art_method_cols[art_method_cols %in% names(data)]

data <- data %>%
  mutate(
    art = suppressWarnings(as.integer(art)),
    across(all_of(art_method_cols), ~ suppressWarnings(as.integer(.x))),
    otherart = if ("otherart" %in% names(data)) otherart else NA_character_
  )

# 2) Create ART indicator
data <- data %>%
  mutate(art_any = art)

# 3) Flag: ART=Yes but no method selected (missing follow-up)
data <- data %>%
  mutate(
    art_method_missing = ifelse(
      art_any == 1 &
        rowSums(across(all_of(art_method_cols), ~ .x == 1), na.rm = TRUE) == 0,
      1L, 0L
    )
  )

# 4) Enforce skip-logic + handle missing follow-up correctly
data <- data %>%
  mutate(
    # If ART = No, all methods must be 0
    across(all_of(art_method_cols), ~ ifelse(art_any == 0, 0L, .x)),
    
    # If ART = Yes but method missing, set methods to NA (unknown)
    across(all_of(art_method_cols), ~ ifelse(art_method_missing == 1, NA_integer_, .x)),
    
    # otherart only allowed if "Other" checked (artmethod___5 == 1)
    otherart = if ("otherart" %in% names(data) && "artmethod___5" %in% names(data)) {
      ifelse(data$artmethod___5 == 1, data$otherart, NA_character_)
    } else {
      data$otherart
    }
  )

# -------------------------
# Maternal Pregnancy Problems Variables
# -------------------------

# Convert blank strings to NA 
data <- data %>%
  mutate(across(where(is.character), ~ na_if(str_squish(.), "")))

# Ensure pregnancy problem checkboxes are integer 0/1
mpp_cols <- grep("^maternalpregnancyproblems___", names(data), value = TRUE)

data <- data %>%
  mutate(across(all_of(mpp_cols), ~ suppressWarnings(as.integer(.x))))

# Keep free text only when ___18 (Other) is checked
if (all(c("maternalpregnancyproblems___18", "maternalpregnancyproblems_other") %in% names(data))) {
  data <- data %>%
    mutate(maternalpregnancyproblems_other =
             ifelse(maternalpregnancyproblems___18 == 1,
                    maternalpregnancyproblems_other, NA_character_))
}

# -------------------------
# Fever Variables
# -------------------------

fever_cols <- grep("^whenfever___", names(data), value = TRUE)

data <- data %>%
  mutate(
    momfever = suppressWarnings(as.integer(momfever)),
    across(all_of(fever_cols), ~ suppressWarnings(as.integer(.x)))
  ) %>%
  mutate(
    # flag fever=1 but no trimester selected
    fever_when_missing = ifelse(
      momfever == 1 &
        rowSums(across(all_of(fever_cols), ~ .x == 1), na.rm = TRUE) == 0,
      1L, 0L
    ),
    # if fever=0, force trimester flags to 0
    across(all_of(fever_cols), ~ ifelse(momfever == 0, 0L, .x)),
    # if fever follow-up missing, set trimester flags to NA (unknown)
    across(all_of(fever_cols), ~ ifelse(fever_when_missing == 1, NA_integer_, .x)),
    fever_any = momfever
  )

# -------------------------
# Additional Feature Engineering
# -------------------------

# Create binary advanced maternal delivery age variable
data$momsage_over35 <- factor(
  ifelse(data$momsdeliveryage >= 35, 1, 0),
  levels = c(0,1),
  labels = c("Under35","35plus")
)

# Create pregnancy-related mental health burden (raw counts) variable
data$mh_count <- rowSums(
  sapply(
    data[, c(
      "mat_dep_before_during",
      "mat_anx_before_during",
      "mat_ocd_before_during",
      "mat_bp_before_during",
      "mat_adhd_before_during",
      "mat_pssd_before_during"
    )],
    function(x) as.numeric(as.character(x))
  ),
  na.rm = TRUE
)

# Create pregnancy-related mental health burden
# Levels: One condition, two conditions, three or more conditions
data$mh_count_cat <- factor(
  pmin(data$mh_count, 3),
  levels = c(0, 1, 2, 3),
  labels = c(
    "No Conditions",
    "One Condition",
    "Two Conditions",
    "Three or More Conditions"
  )
)

# Create pregnancy-related mental health burden (binary) variable
# Any mental health condition = 1
data$any_mh <- as.integer(data$mh_count > 0)

# Drop text-based columns
data <- data[, !(names(data) %in% c(
  "otherart",
  "maternalpregnancyproblems_other",
  "mat_mental_health_other",
  "mat_other",
  "otherdrug"
))]

# -------------------------
# Additional Cleaning (Recoding, dropping irrelevant columns and missing rows)
# -------------------------

vars_to_factor <- c(
  "art",
  "artmethod___1","artmethod___7","artmethod___6","artmethod___2",
  "artmethod___3","artmethod___4","artmethod___5",
  "maternalpregnancyproblems___1","maternalpregnancyproblems___2",
  "maternalpregnancyproblems___3","maternalpregnancyproblems___15",
  "maternalpregnancyproblems___21","maternalpregnancyproblems___16",
  "maternalpregnancyproblems___4","maternalpregnancyproblems___5",
  "maternalpregnancyproblems___6","maternalpregnancyproblems___7",
  "maternalpregnancyproblems___8","maternalpregnancyproblems___9",
  "maternalpregnancyproblems___10","maternalpregnancyproblems___11",
  "maternalpregnancyproblems___12","maternalpregnancyproblems___13",
  "maternalpregnancyproblems___14","maternalpregnancyproblems___17",
  "maternalpregnancyproblems___19","maternalpregnancyproblems___20",
  "maternalpregnancyproblems___18",
  "mom_trauma",
  "matmentalhealth___1","matmentalhealth___2","matmentalhealth___3",
  "matmentalhealth___4","matmentalhealth___5","matmentalhealth___6",
  "matmentalhealth___7",
  "mat_dep","mat_anx","mat_ocd","mat_bp","mat_adhd","mat_pssd",
  "momfever",
  "whenfever___1","whenfever___2","whenfever___3",
  "momcovid", "covidvaccine", "covidvaccinewhich", "covidbaby",
  "art_any", "art_method_missing",
  "fever_when_missing", "fever_any",
  "cannabis_or_alcohol", "drugs_alcohol_tobacco_binary", "tobacco_binary", "other_drugs",
  "mat_dep_before_during", "mat_dep_after", "mat_anx_before_during", "mat_anx_after", "mat_ocd_before_during",
  "mat_ocd_after", "mat_bp_before_during", "mat_bp_after", "mat_adhd_before_during", "mat_adhd_after",
  "mat_pssd_before_during", "mat_pssd_after", 
  "trauma_interpersonal", 
  "trauma_maltreatment",
  "trauma_community",
  "trauma_collective",
  "trauma_loss_medical",
  "trauma_other",
  "ddfamily_sibling", "szbpfamily_sibling",
  "delivery"
)

# Convert vars to factor
vars_to_factor <- intersect(vars_to_factor, names(data)) # prevents errors if variable is missing
data[vars_to_factor] <- lapply(data[vars_to_factor], factor)

# Remove columns
data$momcoviddiagnosesdate <- NULL
data$covidvaccinedate <- NULL
data$covidbabywhen <- NULL

# Drop rows where babygestation or nicuplacement are NA
data <- data[!is.na(data$babygestation) & 
               !is.na(data$nicuplacement), ]

# Save cleaned data
#saveRDS(data, "mmh_cleaned_data.rds")