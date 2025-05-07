
<!-- README.md is generated from README.Rmd. Please edit that file -->

## 📋 Overview

This repository contains R scripts for reproducing all analyses related
to the CSKT AWARE project. It currently includes analyses for project
years 2 and 3.

## 🔄 Analysis Workflow

For each data source:

1.  Raw data is processed using dedicated R scripts
2.  These scripts generate demographic variables and survey constructs
3.  Processed data is then merged for comprehensive analyses
4.  Results are compiled into structured Excel reports

## 📅️ Year 2

#### 🔍 Data sources

1.  🌍 Culture survey
2.  🧘 Health/wellness survey

#### 🛠️ Processing scripts

These scripts generate condensed data sets with demographic variables
and survey constructs necessary for analysis.

-   `CreateCultureSurveyConstructs_Year2_022624.R`
-   `CreateHealthWellnessConstructs_Year2_022624.R`

#### 📈 Analysis Script

Subsequent analyses were performed using:​

`DescriptiveAnalysis_Year2_022624.R`

This script produces a single 📊 Excel file
(`DescriptiveTablesYear2_022624.xlsx`) containing the following outputs:

1.  Basic demographic table by adult/youth
2.  Item-level frequencies by adult/youth
3.  Scale scores from both surveys by adult/youth
4.  Correlation tables for scale scores by adult/youth

  All output files are stored on 📦 Box in: *Year 2 Data Analyses
Datasets – Adam -&gt; Analysis results*

## 📅️️ Year 3

#### 🔍 Data sources

1.  🌍 Culture survey
2.  🧘 Health/wellness survey
3.  🔥 Encampment
4.  🥁 Activity data

#### 🛠️ Processing scripts

-   `CreateDemosAcrossAllSources_041024.R` – this combines demographic
    data across all sources
-   `CreateCultureSurveyConstructs_Year3_160924.R`
-   `CreateHealthWellnessConstructs_Year3_151124.R`
-   `CreateEncampmentConstructs_Year3_121124.R`
-   `CreateActivityConstructs_Year3_280824.R`

These scripts generate condensed data sets with demographic variables
and survey constructs necessary for analysis.

#### 📈 Analysis Script

Subsequent analyses were performed using:​

`DescriptiveAnalysis_Year3_151124.R`

These analysis focus on 240 kids with activity data.

This script produces a single 📊 Excel file
(`DescriptiveAnalyses_Year3_191124.xlsx`) containing the following
outputs:

1.  Basic demographic table by district
2.  Means (SD) for each outcome, by district
3.  Correlation table for all outcomes

Analyses were performed separately for 🔥️ encampment using
`DescriptivesEncampment_311024.R`

This script produces an 📊 Excel file
(`EncampmentResultsYr3_110424.xlsx`) containing the following outputs:

1.  Descriptives
2.  Outcome descriptives by pre and post
3.  Outcomes descriptives for youth
4.  Correlations of outcome scores among adults
5.  Correlations of outcomes scores among youth
6.  Item frequencies for adults
7.  Item frequencies for youth

  All output files are stored on 📦 Box in: *Year 3 Data Analyses
Datasets – Adam -&gt; Analysis results*
