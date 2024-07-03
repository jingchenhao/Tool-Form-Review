# Test Form Review Tool

This tool is created to support test form review. 

## Description

* The tool checks test form compliance with test blueprints by verifying the number of items in each reporting category for each standard.

  ![image](https://github.com/jingchenhao/Tool-Form-Review/assets/71888017/91c1dee4-4637-4de7-979e-8533b5b359b5)

* It checks the item difficulty distribution and item p-values of each form.

  ![image](https://github.com/jingchenhao/Tool-Form-Review/assets/71888017/e13f8b63-e9cc-4f61-9cc6-fd8589eacb49)

* It evaluates test forms by examining test information curves and test characteristic curves.

  ![image](https://github.com/jingchenhao/Tool-Form-Review/assets/71888017/8167430a-d0a9-4224-b05c-72f795cdf9fe)


## Getting Started

* Inputs:
  - items_simulated.csv file is the simulated item parameter file for test forms
  - targets.xlsx file contains targeted item difficulty ranges for different forms. 


* Process file:
  - form_review_app.R is the R shiny program to run the tool. 


## Installation 

R libraries 
shiny, dplyr, readxl, tidyr, plotly, shinydashboard, shinythemes, readr


