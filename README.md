# **Replication Package for Suicide Prevention Overview**

**Date of Release:** 4/8/2026  
**Title:** Effectiveness of school-based suicide prevention interventions: An overview of systematic reviews  
**OSF Component for Publication:** <https://osf.io/b7dhj/> <!-- TODO: update with correct OSF component for OVERVIEW specifically -->
**Package Author:** Shaina Trevino 

<!-- TODO: ADD LINK TO PRE-PRINT SOMEWHERE IN THE BEGINNING -->

## **🔹 Overview**
This repository contains the replication materials for the following publication:  

Chinn, L. K., Trevino, S. D., Steinka-Fry, K., Day, E., Tanner-Smith, E. E., & Grant, S. (2026). Effectiveness of school-based suicide prevention interventions: An overview of systematic reviews <!-- TODO: update with correct DOI for OVERVIEW publication specifically -->

This replication package follows **[AEA Data and Code Availability Standards](https://datacodestandard.org/)** and includes:
- Datasets used to generate reported results.
- Code necessary to reproduce quantitative results reported.
- Computational environment details to ensure reproducibility.



## **🔹 Data and Code Availability Statement**
### **Data Sources**
The data used in this publication were derived from a larger [living systematic review on school-based suicide prevention](https://github.com/HEDCO-Institute/Suicide_Prevention_Overview). <!-- TODO: update with LIVING repo once made AND/OR is this still accurate are they still derived from each other? -->
- Datasets used for this publication reflect a fixed version of the data, captured during analysis.
- Data for this publication were collected in DistillerSR. Some files, such as the citation matrix, were constructed in excel.
- Metadata (variable names and descriptions) for all data files are provided in corresponding tabs in the `data/Suicide_Prevention_Data_Dictionary.xlsx` file. 

The following datasets used for analyses are available in the `data` subfolder:

| Data File | Description | Data Structure |
|-----------|-------------|-----------| 
| `SPO_screening_eligibility.xlsx` | Abstract and full-text screening decisions | One row per report/citation | 
| `Suicide_citation_matrix_reconciled.xlsx` | Primary study overlap across reviews and primary study eligibility decisions| One row per primary study included in eligible reviews | 
| `SPO_review_level.xlsx` | Extracted descriptive data, study quality assessment (AMSTAR), and risk of bias assessment (ROBIS) for eligible reviews | One row per eligible review |
| `SPO_linked_references.csv` | Reference information for additional reports of studies/reviews | One row per main reference and linked reference combo | 
| `SPO_duplicate_refs.csv` | Reference information for duplicate citations | One row per duplicate citation |
<br>

### **Analysis Script and Reproducibility Workflow**
The `code` subfolder contains the R script (`analysis_script.R`) to generate quantitative results for this publication. It does the following: 
1. Imports raw data
1. Transforms data
1. Runs all analyses and summaries for results
1. Creates all tables, figures, and appendices
1. Saves all outputs and exports a bundled `analysis_script_objects.rds` object (`outputs/objects`)

The `manuscript` subfolder contains the Quarto files to generate the pre-print of our paper<!-- TODO: INSERT LINK TO PREPRINT -->. The Quarto manuscript does not rerun analyses; it relies entirely on the precomputed objects stored in the `analysis_script_objects.rds` file. These files:
1. Load precomputed results from the `.rds`
1. Generate a fully reproducible Quarto pre-print
1. Render to a website in the `docs` folder for publishing online through GitHub Pages

### **Data Citation**
Please cite this version of the data as follows:

Trevino, S. D., Chinn, L. K., Steinka-Fry, K., Day, E., Tanner-Smith, E. E., & Grant, S. (2026). Data for "Effectiveness of school-based suicide prevention interventions: An overview of systematic reviews." [OSF](https://osf.io/b7dhj/). <!-- doi:10.17605/OSF.IO/KG57Y --> <!-- TODO: update with correct DOI for OVERVIEW publication specifically -->


### **Handling of Missing Data**
- Missing values in the datasets are coded as `-999`, `Not Reported`, or `NA` and indicate those values were not reported in studies/reviews.



## **🔹 Computational Requirements**
### **Software Environment**
- **R Version:** 4.5.2  
- **Operating System:** Windows 11 Enterprise (64-bit operating system)  

### **Reproducing the Environment**
First, open the Rproject (`.Rproj` file) which should automatically activate the `renv` environment. If not, you can follow the steps outlined below and in the `analysis_script.R` file to activate the `renv` environment. While in your Rproject within RStudio, open the `code/analysis_script.R` file which contains commented-out code to set up the environment using the `renv` package and install the correct package versions, also listed here:

1. Install `renv` (if not already installed):
```
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
```
2. Restore any missing packages (you should only need to run this code once):
```
renv::restore()
```

Once the environment is restored, load the necessary packages:
```
library(rio)
library(here)
library(readxl)
library(janitor)
library(tidyverse)
library(openxlsx)
library(lubridate)
library(gt)
library(webshot2)
library(ccaR)
library(glue)
```

If those don't work, you can try 
```
if (!require("pacman")) install.packages("pacman")
pacman::p_load(devtools, rio, here, readxl, janitor, tidyverse, openxlsx, lubridate, gt, webshot2, glue)
pacman::p_load_gh("thdiakon/ccaR", "mcguinlu/robvis") #from github
```

## **🔹 Instructions for Replication**

### **Data Preparation and Analysis**
To replicate our results: 

**If you have Rstudio and Git installed and connected to your GitHub account:**

1. Clone the [main repository](https://github.com/HEDCO-Institute/Suicide_Prevention_Overview) to your local machine ([click for help](https://book.cds101.com/using-rstudio-server-to-clone-a-github-repo-as-a-new-project.html#step---2))
1. Open the `Suicide_Prevention_Overview` R project in R Studio (this should automatically activate the `renv` environment, if not follow steps above)
1. In the R Console, run `quarto::quarto_render()` to generate the reproducible manuscript
  - Successful replication will produce a rendered HTML manuscript (`index.html`) in the `docs/` folder.
1. Optional: Run `analysis_script.R` in the `code` subfolder to reproduce all analytic steps and generate `outputs` used in the pre-print. 

**If you need to install or connect R, Rstudio, Git, and/or GitHub:**

1. [Create a GitHub account](https://happygitwithr.com/github-acct.html#github-acct)
1. [Install R and RStudio](https://happygitwithr.com/install-r-rstudio.html)
1. [Install Git](https://happygitwithr.com/install-git.html)
1. [Link Git to your GitHub account](https://happygitwithr.com/hello-git.html)
1. [Sign into GitHub in Rstudio](https://happygitwithr.com/https-pat.html)

**To reproduce our results without using Git and GitHub, you may use the following steps:** 

1. Download the ZIP file from the [main repository](https://github.com/HEDCO-Institute/Suicide_Prevention_Overview) 
1. Open the `Suicide_Prevention_Overview` R project in R Studio (this should automatically activate the `renv` environment, if not follow steps above)
1. In the R Console, run `quarto::quarto_render()` to generate the reproducible manuscript
  - Successful replication will produce a rendered HTML manuscript (`index.html`) in the `docs/` folder.
1. Optional: Run `analysis_script.R` in the `code` subfolder to reproduce all analytic steps and generate `outputs` used in the pre-print. 


### **Notes on Reproducibility**
- All file paths are relative using `here::here()`; no hardcoded paths are used. By default, the script does not overwrite any outputs to preserve the integrity of the replication package.
- Data cleaning, analyses, and visualizations are created in the provided `analysis_script.R` file. This file is provided for transparency and verification of the analytic pipeline and does not overwrite results by default. 
- All generated results and outputs from the `analysis_script.R` file are saved to `outputs/objects/analysis_script_objects.rds`. The various `.qmd` files in the `manuscript` subfolder call these objects to present in the pre-print <!-- TODO: INCLUDE LINK-->.
- All generated tables, figures, and appendices are saved in the `outputs` subfolder. If you prefer not to rerun the scripts, you can directly access these files .
- Saved `outputs` reflect a fixed version of the analytic results at the time of publication and is not overwritten when running the analysis script unless explicitly specified by the user.
- Some results were removed or reformatted in the final published manuscript following reviewer recommendations. Thus, generated outputs may not exactly match the final published tables or figures.


### **Non-Reproducible Elements**
Some components cannot be reproduced using the analysis script:
- Narrative summaries of each review findings are not produced by the analysis script
- Supplement 1 were manually created in Word and are not part of the outputs generated by the analysis script
<!-- TODO: is this correct we used to have search string appendix/supplement - what other tables are not produced here -->

### **Known Discrepancies**
There are some discrepancies between outputs from the analysis script and those reported in the publication since data package was created after publication:
<!-- TODO: anything here? -->


## **🔹 Folder Structure**
<!-- TODO: is this correct after all changes? -->
```
📁 Suicide_Prevention_Overview/
│── 📁 code/                        # Analysis script for reproducibility
│    └── analysis_script.R
│
│── 📁 data/                        # Datasets used for this publication
│    ├── SPO_duplicate_refs.csv
│    ├── SPO_linked_references.csv
│    ├── SPO_review_level.xlsx
│    ├── SPO_screening_eligibility.xlsx
│    ├── Suicide_citation_matrix_reconciled.xlsx
│    └── Suicide_Prevention_Data_Dictionary.xlsx  # Codebook for all data files
│
│── 📁 docs/                        # Rendered Quarto pre-print for GitHub Pages
│
│── 📁 manuscript/                  # Quarto files to build pre-print
│
│── 📁 outputs/                     # Generated tables, figures, and appendices
│    ├── 📁 appendices/             # Generated appendices
│    ├── 📁 figures/                # Generated figures
│    ├── 📁 objects/                # Exported objects from analysis_script.R
│    └── 📁 tables/                 # Generated tables
│
│── 📁 renv/                        # Renv environment for reproducibility
│── 📄 renv.lock                    # Package versions and dependencies
│── 📄 .Rprofile                    # Renv configuration file
│── 📄 README.md                    # This README document
```




## **🔹 Licensing**
The code and data in this replication package are licensed under the Creative Commons Attribution 4.0 International License (CC BY 4.0); see the LICENSE file in the main repository root directory for full terms.



## **🔹 Contact Information**
For questions about this replication package, contact:  
✉️ **Shaina Trevino** (strevino@uoregon.edu)  


