# Disaster Casuality Analysis Using Regression and Classification

This project aims to **analyze disaster impacts** and **group similar cases** by analyzing features using regression and classification models.

---

## 📌 Project Description

The disaster-related data from **13 countries worldwide** is analyzed, to explore the **predictive potential** of parameters such as **deaths and monetary losses**. It also aims to **classify countries** and **disaster types** based on similarity traits.

---

## 🚀 Getting Started

### Prerequisites

Ensure the following are installed:

- **R** and **RStudio** (preferably RStudio 2024.09.0+)
- Required R libraries:

Some basic Git commands are:
```
readxl, readr, openxlsx, dplyr, ggplot2, tidyr, caret, ggpubr, 
reshape2, gridExtra, ggcorrplot, Metrics, rpart, rpart.plot, 
purr, factoextra, glmnet, scales, FactoMineR, randomForest, xgboost
```
### Instructions

1. Download the zipped project folder and unzip it to a location of your choice.

2. Open RStudio and set the working directory to the unzipped folder.

3. Run the R scripts one-by-one, starting from datacleaning_and_featureEngineering.R.

4. Each script:

    - Reads a file (.xlsx/.csv)

    - Saves the output to ensure progress

5. Review outputs in the Console and plots in the Plot window.

---

### 📁 Code Scripts
- Data Cleaning and Feature Engineering:
datacleaning_and_featureEngineering.R
→ Cleans raw dataset and performs feature engineering

- Exploratory Data Analysis:
EDA_Code.R
→ Generates visual insights using the final dataset

- Regression:
Regression_Code.R
→ Feature selection, model tuning, regression analysis, and plots

- PCA and Classification:
classification_and_pca.R
→ Runs PCA and classification using both original and principal components

### 🔄 Order of Execution
Follow this order for executing the scripts:

1. Data Cleaning and Feature Engineering
→ Run datacleaning_and_featureEngineering.R
Output: df_clean.csv

2. Exploratory Data Analysis
→ Run EDA_Code.R using eda_data.xlsx
Output: Visuals and statistical insights

3. Regression Analysis
→ Run Regression_Code.R using model_data.csv
Output: Model summaries, metrics, regression plots

4. PCA and Classification
→ Run classification_and_pca.R using classification-related files
Output: PCA results, classification accuracy and confusion matrix

### 📊 Visualized Findings
The insights and findings are summarised and visualized using various charts in the ``` Summarized Findings.pptx ``` file

#### Additional Information:
- Raw data files are included in the country-wise-data folder
- Cleaned versions of the data are included in clean-data folder

