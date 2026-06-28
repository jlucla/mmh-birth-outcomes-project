#Impact of Maternal Mental Health on Birth Outcomes
##Overview

This project was completed as part of a UCLA capstone statistical consulting course (February–March 2026) for an on-campus client. Our team analyzed survey data collected through the ChatterBaby mobile application to investigate the relationship between maternal mental health conditions during pregnancy and birth outcomes.

The primary outcomes of interest were gestational age and NICU admission, while maternal mental health was measured using self-reported diagnoses including depression, anxiety, OCD, ADHD, bipolar disorder, and related conditions.

##Data Preparation

The original survey dataset contained over 250 variables per respondent and required extensive preprocessing prior to analysis. Data cleaning included:

Removing variables not relevant to the research questions
Identifying and correcting biologically implausible values
Excluding incomplete survey responses
Recoding and consolidating sparse categorical variables
Creating derived variables for maternal mental health, demographic characteristics, and pregnancy-related factors

Following the client's specifications, complete-case analysis was performed for the primary outcome variables.

##Methods

Statistical analyses were conducted in R using packages including tidyverse, ggplot2, dplyr, stringr, corrplot, and scales.

Analytical methods included:

Multiple linear regression for gestational age
Logistic regression for NICU admission
Sensitivity analyses using alternative specifications of maternal age

Maternal mental health was modeled using multiple operationalizations, including binary indicators, condition counts, and individual diagnoses to evaluate overall, dose-response, and condition-specific associations.

##Key Findings
Maternal mental health conditions reported before or during pregnancy were significantly associated with shorter gestational age.
A dose-response relationship was observed, with increasing numbers of reported mental health conditions associated with progressively shorter gestational age.
Depression was the individual condition most strongly associated with reduced gestational age.
Maternal mental health was not independently associated with NICU admission after adjusting for gestational age, suggesting gestational age may mediate much of the observed relationship.

###Repository Contents
- Data preprocessing code
- Statistical modeling code
- Variable dictionary
- Final project report

Note: The original survey data are not included in this repository because they were provided for a client consulting project and cannot be publicly redistributed. The repository focuses on the analytical workflow and reproducible statistical methods.

