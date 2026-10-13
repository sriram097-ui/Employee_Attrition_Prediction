# Employee Attrition Prediction — R Shiny Dashboard

A professional multi-page dashboard for employee workforce exploration and attrition prediction.

## Features
- Executive KPIs: employee count, attrition rate, retention count, average age
- Attrition distribution and department/category comparisons
- Workforce exploration charts and numeric summaries
- Attrition insights and numeric correlation heatmap
- Random Forest prediction lab with feature importance and employee-profile scoring
- Searchable/filterable employee records with CSV download
- Demo data fallback so the dashboard can launch before your real CSV is added

## Project structure
```
Employee_Attrition_Prediction/
├── app.R
├── data/
│   └── employee_attrition.csv   # add your dataset here
├── www/
├── README.md
└── requirements.txt
```

## 1. Install R
Install R for Windows from https://cran.r-project.org/ if it is not installed.

## 2. Open in VS Code
Open the extracted `Employee_Attrition_Prediction` folder in VS Code. Install the **R** extension by REditorSupport if desired.

## 3. Install packages
In the R Interactive terminal, run:
```r
install.packages(c(
  "shiny", "shinydashboard", "tidyverse", "ggplot2",
  "DT", "randomForest", "scales"
), repos = "https://cloud.r-project.org")
```

If Windows reports `Permission denied` for a DLL, close all R/RStudio/VS Code R sessions and retry. Do not install a package while another R session is using it.

## 4. Add your dataset
Copy your CSV to:
`data/employee_attrition.csv`

The dashboard works best with the common IBM HR attrition dataset, including an `Attrition` column whose values are `Yes` and `No`. Column names are made syntactically safe by R. If the file is missing, demo data is used and clearly labelled in the dashboard.

## 5. Run the dashboard
Open the R Interactive terminal in VS Code and run:
```r
setwd("C:/path/to/Employee_Attrition_Prediction")
shiny::runApp()
```
Replace the example path with the actual folder location. Alternatively, in the terminal, change directory to the project folder and run `R -e "shiny::runApp()"`.

The app opens in your default browser.

## Notes
- The model is an exploratory Random Forest classifier using out-of-bag error/accuracy and variable importance.
- Attrition predictions should not be used as the sole basis for employment decisions.
- For meaningful model evaluation, use a larger dataset and consider class imbalance, calibration, fairness, and a held-out test set.
