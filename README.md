
<!-- README.md is generated from README.Rmd. Please edit that file -->
<p style="display: flex; align-items: center;">
<img src="C:/Users/vonbe/OneDrive/Adam/images.jpg" width="70" style="margin-right: 10px;">
<span style="font-size: 30px; font-weight: bold;">CSKT</span>
</p>

## 📝 Overview

This repository includes R coding files for reproducing all analyses for
CSKT AWARE. Currently, the repo includes analyses for project years 2
and year 3.

For each data type (e.g. survey), the data is first processed using a
single R coding file. Mainly, this is for creating demographic variables
and survey constructs, which are then combined and uses in anaysis. See
the breakdown by year below

## 📅️ Year 2

#### 🔍 Data sources

1.  🌍 Culture survey
2.  🧘 Health/wellness survey

#### 🛠️ Processing scripts

-   `CreateCultureSurveyConstructs_Year2_022624.R`

-   `CreateHealthWellnessConstructs_Year2_022624.R`

These scripts generate condensed data sets with demographic variables
and survey constructs necessary for analysis.

#### 📈 Analysis Script

Subsequent analyses were performed using:​

`DescriptiveAnalysis_Year2_022624.R`

This script produces a single 📊 Excel file containing the following
outputs:

1.  Basic demographic table by adult/youth
2.  Item-level frequencies by adult/youth
3.  Scale scores from both surveys by adult/youth
4.  Correlation tables for scale scores by adult/youth

## 📅️️ Year 3
