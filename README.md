# Impact of Maternal Mental Health on Birth Outcomes

## Overview

This project was completed as part of a UCLA capstone statistical consulting course for an on-campus client. Our team analyzed survey data collected through the ChatterBaby mobile application to examine the relationship between maternal mental health conditions and birth outcomes.

The primary outcomes were:

- Gestational age
- NICU admission

Maternal mental health was measured using self-reported conditions before or during pregnancy, including depression, anxiety, OCD, ADHD, bipolar disorder, and related conditions.

## Data Preparation

The original survey dataset contained over 250 variables per respondent and required extensive cleaning before analysis. The preprocessing workflow included:

- Removing variables not relevant to the research questions
- Identifying biologically implausible values
- Excluding incomplete survey responses
- Recoding and consolidating sparse categorical variables
- Creating derived variables for maternal mental health, demographics, and pregnancy-related factors

Following the client's specifications, complete-case analysis was used for the primary outcome variables.

## Methods

All analyses were performed in R. Methods included:

- Multiple linear regression for gestational age
- Logistic regression for NICU admission
- Sensitivity analyses using alternative specifications of maternal age

Maternal mental health was modeled using several approaches, including binary indicators, condition counts, and individual diagnoses.

## Key Findings

- Maternal mental health conditions before or during pregnancy were significantly associated with shorter gestational age.
- A dose-response relationship was observed, with more reported mental health conditions associated with progressively shorter gestational age.
- Depression was the individual condition most strongly associated with reduced gestational age.
- Maternal mental health was not independently associated with NICU admission after adjusting for gestational age.

## Repository Contents

- Data cleaning and preprocessing scripts
- Statistical modeling workflows
- Variable dictionary
- Final project report

## Data Availability

Out of caution for subject confidentiality, the raw and cleaned survey datasets are not available for redistribution.
