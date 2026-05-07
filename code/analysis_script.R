
# MAIN ANALYSIS SCRIPT
# Paper: Effectiveness of school-based suicide prevention interventions: An overview of systematic reviews
# Purpose: Calculate and reproduce analytic objects, results, tables, and figures reported in the paper
#===============================================================================

# 1. Renv ----------------------------------------------------------------

## If renv environment does not load automatically, uncomment and run the below code:

# if (!require("renv)) install.packages("renv")
# renv::restore()

# 2. Load packages --------------------------------------------------------

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

## If library didn't work, try un-commenting and running the below code:

# if (!require("pacman")) install.packages("pacman") #installs and loads pacman package
# 
# pacman::p_load(devtools, rio, here, readxl, janitor, tidyverse, openxlsx, lubridate, gt, webshot2, glue) #install and load required packages
# pacman::p_load_gh("thdiakon/ccaR", "mcguinlu/robvis") #from github


# 3. Import data ----------------------------------------------------------

# Eligibility (with linked refs; datarama report)
spo_eligibility_raw <- import(here("data", "SPO_screening_eligibility.xlsx")) %>% 
  janitor::clean_names()
# Check unique IDs
length(unique(spo_eligibility_raw$refid)) == nrow(spo_eligibility_raw)

# Import Quarantine Duplicates (Download from DistillerSR Quarantine)
spo_duplicates <- import(here("data", "SPO_duplicate_refs.csv")) %>% 
  janitor::clean_names()

# Import linked citations for all references
linked_ref_df <- import(here("data", "SPO_linked_references.csv")) %>%
  janitor::clean_names()

# Import Review Level Data
spo_review_level <- import(here("data", "SPO_review_level.xlsx")) %>% 
  janitor::clean_names()

# Import Review Meta-Analytic Data
spo_ma_estimates <- import(here("data", "SPO_review_ma_estimates.xlsx")) %>% 
  janitor::clean_names() %>% 
  distinct(across(-user), .keep_all = TRUE) 
  


# Import citation matrix
elig_path <- here("data", "Suicide_citation_matrix_reconciled.xlsx")

citation_matrix <- read_excel(elig_path, sheet = "Citation Matrix", col_names = FALSE)

# 4. Helper functions -----------------------------------------------------

# Function to clean citations with missing info
clean_citation_text <- function(x) {
  x %>%
    #Work with character safely
    as.character() %>%
    
    #Turn NA-like strings into NA if needed
    na_if("") %>%
    
    #Remove repeated punctuation patterns caused by empty fields
    stringr::str_replace_all("\\(\\s*\\)", "") %>%      #()
    stringr::str_replace_all("\\[\\s*\\]", "") %>%      #[]
    stringr::str_replace_all("\\{\\s*\\}", "") %>%      #{}
    stringr::str_replace_all("\\s*\\.\\s*\\.\\s*", ". ") %>%   #..
    stringr::str_replace_all("\\s*,\\s*,\\s*", ", ") %>%       #,,
    stringr::str_replace_all("\\s*;\\s*;\\s*", "; ") %>%       #;;
    stringr::str_replace_all("\\s*:\\s*:\\s*", ": ") %>%       #::
    
    #Clean mixed punctuation leftovers
    stringr::str_replace_all(",\\s*\\)", ")") %>%       #, )
    stringr::str_replace_all("\\(\\s*,", "(") %>%       #( ,
    stringr::str_replace_all("\\s+\\)", ")") %>%        #space before )
    stringr::str_replace_all("\\(\\s+", "(") %>%        #space after (
    stringr::str_replace_all("\\s+,", ",") %>%          #space before comma
    stringr::str_replace_all("\\s+\\.", ".") %>%        #space before period
    
    #Replace ,. with just .
    stringr::str_replace_all(",\\.", ".") %>%
    stringr::str_replace_all("/+\\)", ")") %>%
    
    #Remove punctuation at start/end if export left it dangling
    stringr::str_replace_all("^[,;:.\\s]+", "") %>%
    stringr::str_replace_all("[,;:\\s]+$", "") %>%
    
    #Collapse multiple spaces
    stringr::str_replace_all("\\s{2,}", " ") %>%
    stringr::str_squish()
}

# Write functions for formatting inline code
format_thousands <- function(x) {
  big_num <- format(x, big.mark = ",")
  return(big_num)
}

format_percent <- function(x, digits, force_decimal = FALSE) {
  if (force_decimal) {
    percentage <- paste0(format(round(x, digits), nsmall = digits), "%")
  } else {
    percentage <- paste0(round(x, digits), "%")
  }
  return(percentage)
}

# 5. Clean and prepare data -----------------------------------------------

# Eligibility
colnames(citation_matrix) <- citation_matrix[1,]
td_cm <- citation_matrix %>% 
  slice(-1)

spo_eligibility <- spo_eligibility_raw %>% 
  mutate(
    eligibility_exclude_reason = if_else(
      !is.na(eligibility_notes) &
        str_detect(eligibility_notes, fixed("; Study")) &
        eligibility_exclude_reason == "Unclear (provide reason in comments)",
      "Ineligible study design (not a systematic review or primary study)",
      eligibility_exclude_reason
    ),
    bibliography = clean_citation_text(bibliography)
  )

# Review level
review_td <- spo_review_level %>% 
  select(-starts_with("amstar"), -starts_with("robis")) %>% 
  mutate(review_publication_year = as.numeric(review_publication_year))

amstar_rating_td <- spo_review_level %>% 
  select(refid, review_author_year, starts_with("amstar")) %>% 
  select(refid, review_author_year, ends_with("rating"))

robis_rating_td <- spo_review_level %>% 
  select(refid, review_author_year, starts_with("robis")) %>% 
  select(refid, review_author_year, ends_with("decision"), robis_overall_a, robis_overall_b, robis_overall_c, 
         robis_overall_rating)

  
# 6. Eligibility Results --------------------------------------------------

# Calculate eligibility counts for PRISMA reporting
overview_counts <- list(
  records_identified = nrow(spo_eligibility) + nrow(spo_duplicates),
  records_screened = nrow(spo_eligibility),
  records_kept = sum(spo_eligibility$screening_decision == "Keep", na.rm = TRUE),
  reports_not_retrieved = sum(spo_eligibility$pdf_retrieved == "No", na.rm = TRUE),
  reports_assessed = sum(spo_eligibility$screening_decision == "Keep", na.rm = TRUE) - 
    sum(spo_eligibility$pdf_retrieved == "No", na.rm = TRUE),
  reviews_included = nrow(spo_review_level),
  reports_included = sum(
    spo_eligibility$eligibility_decision == "Eligible" &
      spo_eligibility$eligibility_reference_type == "Review",
    na.rm = TRUE
  )
)

# Percentages
overview_counts$prop_kept <- overview_counts$records_kept / overview_counts$records_screened * 100
overview_counts$prop_ref_assessed <- overview_counts$reports_assessed / overview_counts$records_screened * 100
overview_counts$prop_reviews_assessed <- overview_counts$reviews_included / overview_counts$reports_assessed * 100
overview_counts$prop_reviews_identified <- overview_counts$reviews_included / overview_counts$records_identified * 100

# 7. Review Descriptive Results -------------------------------------------

# Calculate most commonly searched databases
#change string to lowercase and create flag if each database was searched
rev_df <- review_td %>% 
  mutate(review_databases_searched = str_to_lower(review_databases_searched),
         pubmed = str_detect(review_databases_searched, "pubmed"),
         psycinfo = str_detect(review_databases_searched, "psycinfo"),
         medline = str_detect(review_databases_searched, "medline|ovid"),
         eric = str_detect(review_databases_searched, "eric|education resources information center"),
         ebsco = str_detect(review_databases_searched, "ebsco"),
         proquest_dt = str_detect(review_databases_searched, "dissertation and theses|dissertation results on proquest"),
         psycarticles = str_detect(review_databases_searched, "psycarticles"),
         psycextra = str_detect(review_databases_searched, "psycextra|psychextra"),
         cochrance_central = str_detect(review_databases_searched, "cochrane central"),
         cochrane_library = str_detect(review_databases_searched, "cochrane library"),
         wos = str_detect(review_databases_searched, "web of science"),
         academic_ap = str_detect(review_databases_searched, "academic search premier"),
         pbs_collection = str_detect(review_databases_searched, "psychology and behavioral sciences collection|psychology and behavioralsciences"),
         cinahl = str_detect(review_databases_searched, "cinahl"),
         embase = str_detect(review_databases_searched, "embase"),
         science_cit_index = str_detect(review_databases_searched, "science citation index"),
         sci_direct = str_detect(review_databases_searched, "science direct|sciencedirect"),
         scopus = str_detect(review_databases_searched, "scopus"),
         gscholar = str_detect(review_databases_searched, "google scholar|googlescholar"),
         british_ni = str_detect(review_databases_searched, "british nursing index"),
         britich_ei = str_detect(review_databases_searched, "british education index"),
         assia = str_detect(review_databases_searched, "assia"),
         diss_abs = str_detect(review_databases_searched, "dissertation abstracts"),
         socio_abs = str_detect(review_databases_searched, "sociological abstracts"),
         won = str_detect(review_databases_searched, "web of knowledge"),
         ctg = str_detect(review_databases_searched, "clinicaltrials.gov"),
         prosp = str_detect(review_databases_searched, "prospero"),
         optn  = str_detect(review_databases_searched, "opentrials.net"), #Added for suicide
         edrc = str_detect(review_databases_searched, "education research complete"), #Added for suicide
         edsrc = str_detect(review_databases_searched, "education source"), #Added for suicide
         epist = str_detect(review_databases_searched, "epistemonikos"))

#create long dataframe
review_databases <- rev_df %>% 
  select(refid, pubmed:epist) %>% 
  pivot_longer(cols = pubmed:epist) %>% 
  mutate(value = as.character(value))

#calculate most common databases searched and 
#remove cases where reviews said both pubmed and medline but only searched one
mode_databases <- review_databases %>% 
  group_by(name) %>% 
  summarize(num_db = sum(value == "TRUE"),
            percent = sum(value == "TRUE") / nrow(review_td) * 100) %>% 
  arrange(desc(num_db))

# Year of last database search
#extract review year from search data
review_df <- review_td %>% 
  mutate(db_search_year = as.numeric(str_extract(review_search_date, "[0-9]{4}")))

# Transparency
#number of reviews with flow diagrams
num_prisma <- sum(review_df$review_flow_diagram == "Yes", na.rm = TRUE) 

#number of reviews with registration number
num_reg <- sum(review_df$review_registration_number != "-999") 

#number of reviews with availability statements
num_avail <- sum(review_df$review_availability_statement != "-999")

# 8. Risk of Bias and Quality Assessment Results --------------------------

# AMSTAR Results
# Number/% of high confidence
amstar_high <- sum(amstar_rating_td$amstar_overall_rating == "HIGH") 
amstar_high_per <- amstar_high  / nrow(amstar_rating_td) * 100 

# Number/% of moderate confidence
amstar_mod <- sum(amstar_rating_td$amstar_overall_rating == "MODERATE")
amstar_mod_per <- amstar_mod  / nrow(amstar_rating_td) * 100 

# Number/% of low confidence
amstar_low <- sum(amstar_rating_td$amstar_overall_rating == "LOW")
amstar_low_per <- amstar_low  / nrow(amstar_rating_td) * 100 

# Number/% of critically low confidence
amstar_clow <- sum(amstar_rating_td$amstar_overall_rating == "CRITICALLY LOW")
amstar_clow_per <- amstar_clow  / nrow(amstar_rating_td) * 100 

# Calculate most common critical weaknesses in AMSTAR ratings among reviews
# Filter for critical domains
amstar_crit <- amstar_rating_td %>% 
  select(refid, amstar_2_rating, amstar_4_rating, amstar_7_rating, amstar_9rct_rating, amstar_9nrsi_rating,
         amstar_11rct_rating, amstar_11rct_rating, amstar_13_rating, amstar_15_rating) %>% 
  pivot_longer(cols = -refid)

# Calculate frequency for "No"s per critical domains
amstar_crit_mode <- amstar_crit %>% 
  group_by(name) %>% 
  summarize(n = sum(value == "No"),
            percent = sum(value == "No") / nrow(amstar_rating_td) * 100) %>% 
  arrange(desc(n))

# Calculate most common non-critical weaknesses in AMSTAR ratings among reviews
# Filter for non-critical domains
amstar_noncrit <- amstar_rating_td %>% 
  select(refid, amstar_1_rating, amstar_3_rating, amstar_5_rating, amstar_6_rating, amstar_8_rating, 
         amstar_10_rating, amstar_12_rating, amstar_14_rating, amstar_16_rating) %>% 
  pivot_longer(cols = -refid)

# Calculate frequency for "No"s per non-critical domains
amstar_noncrit_mode <- amstar_noncrit %>% 
  group_by(name) %>% 
  summarize(n = sum(value == "No"),
            percent = sum(value == "No") / nrow(amstar_rating_td) * 100) %>% 
  arrange(desc(n))

# ROBIS results
# Number/% of low risk
robis_low <- sum(robis_rating_td$robis_overall_rating == "LOW")
robis_low_per <- robis_low  / nrow(robis_rating_td) * 100 

# Number/% of unclear risk
robis_unclear <- sum(robis_rating_td$robis_overall_rating == "UNCLEAR") 
robis_unclear_per <- robis_unclear  / nrow(robis_rating_td) * 100 

# Number/% of high risk
robis_high <- sum(robis_rating_td$robis_overall_rating == "HIGH") 
robis_high_per <- robis_high  / nrow(robis_rating_td) * 100 

# Select ratings, transform to long format, calculate percent with high risk
robis_concerns <- robis_rating_td %>% 
  select(refid, ends_with("decision"), robis_overall_a, 
         robis_overall_b, robis_overall_c) %>% 
  pivot_longer(cols = 2:8) %>% 
  group_by(name) %>% 
  summarize(n_decision = sum(value == "HIGH"),
            per_decision = sum(value == "HIGH") / nrow(robis_rating_td) * 100,
            n_overall = sum(value == "No"),
            per_overall = sum(value == "No") / nrow(robis_rating_td) * 100) %>% 
  arrange(desc(n_decision))

# 9. Overlap of Studies in Reviews Results --------------------------------

# Using citation matrix without headers, transpose data 
cmt <- as.data.frame(t(citation_matrix))

# Remove first row of column names and second row with current study inclusions, and
# Create new variable to calculate number of studies in each review
df_overlap <- cmt %>% 
  slice(-1) %>% 
  mutate(num_stud = rowSums(. == "Yes")) 

# Range and median of number of studies in each review
range(df_overlap$num_stud) 
median(df_overlap$num_stud) 

# Number and percentage of primary studies included in more than one review
#create flag for if study is in more than one review
cm_overlap <- td_cm %>% 
  mutate(morethanone = ifelse(rowSums(. == "Yes") > 1, "yes", "no"))

# Number and % included in more than one review
num_inc_overlap <- sum(cm_overlap$morethanone == "yes")
num_inc_overlap_per <- sum(cm_overlap$morethanone == "yes") / nrow(cm_overlap) * 100

# Overall CCA percentage for all primary studies included across reviews
#create dataframe for ccaR input (1 = included; 0 = excluded)
ccar_input <- td_cm %>% 
  mutate_at(vars(-study), ~ifelse(. == "Yes", 1, 0))  

#calculate overall CCA
cca_included <- cca(ccar_input) 


# 10. Create tables -------------------------------------------------------

## Table 2. Descriptive characteristics per each included systematic review----
# Select info for table1
t1_info <- review_td %>% 
  select(refid, review_author_year, review_databases_searched, review_search_date)

# Transform to long to calculate # of included/eligible studies per review
cm_long <- td_cm %>% 
  pivot_longer(cols = -study,
               names_to = "review_author_year") 

# Count number of included studies per review
numinc_perreview <- cm_long %>% 
  group_by(review_author_year) %>% 
  summarize(n_included = sum(value == "Yes", na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(review_author_year = str_remove(review_author_year, "\\*$")) 

# Merge and format values
t1 <- left_join(t1_info, numinc_perreview) %>% 
  mutate(search_date_format = ifelse(nchar(review_search_date) == 10, format(ymd(review_search_date), "%B %d, %Y"), review_search_date)) %>% 
  mutate(search_date_format = ifelse(nchar(review_search_date) == 7, format(ym(review_search_date), "%B %Y"), search_date_format)) %>% 
  mutate_all(~ replace(., . == -999, "Not Reported")) %>% 
  select(review_author_year, review_databases_searched, search_date_format, n_included) %>% 
  arrange(str_to_lower(review_author_year))

# Create gt table and format
table1_formatted <- t1 %>% 
  gt() %>% 
  cols_align(columns = c("n_included"), align = "center") %>% 
  tab_style(style = cell_text(align = "left"), locations = cells_column_labels(columns = c("n_included"))) %>% 
  cols_width(review_author_year ~ px(125), review_databases_searched ~ px(275), search_date_format ~ px(150), starts_with("n") ~ px(75)) %>% 
  cols_label(review_author_year = "Review", 
             review_databases_searched = "Databases Searched",
             search_date_format = "Search Date",
             n_included = "Included Studies"
  ) %>% 
  tab_style(style = list(cell_borders(sides = "all", color = "black", weight = px(1))),
            locations = cells_body(columns = everything())) %>% 
  tab_style(style = list(cell_borders(sides = "all", color = "black", weight = px(1)),
                         cell_text(weight = "bold")),
            locations = cells_column_labels()) %>% 
  tab_options(column_labels.border.top.color = "black",
              column_labels.border.bottom.color = "black",
              table_body.border.bottom.color = "black",
              table.border.top.color = "white",
              heading.border.bottom.color = "black",
              table_body.hlines.color = "white",
              table.font.names = "Times New Roman")

# # Save table
# # Save as word doc
# gtsave(table1_formatted, filename = "table2_word.docx", device = "word", path = here("outputs", "tables"), landscape = TRUE) #landscape doesn't work
# 
# # Save as html with larger column widths
# table1_html <- table1_formatted %>%
#   cols_width(review_author_year ~ px(175), review_databases_searched ~ px(700), search_date_format ~ px(150), starts_with("n") ~ px(75))
# 
#  gtsave(table1_html, filename = "table2_descriptives.html", path = here("outputs", "tables"))

## Table 3. AMSTAR-2 Ratings per Question ----

# Re-format ratings to report in table 2
t2 <- amstar_rating_td %>% 
  mutate(review_author_year = as.factor(review_author_year)) %>% 
  mutate_if(is.character, ~case_when(. == "HIGH" ~ "H",
                                     . == "MODERATE" ~ "M",
                                     . == "LOW" ~ "L",
                                     . == "CRITICALLY LOW" ~ "CL",
                                     . == "Yes" ~ "Y",
                                     . == "Partial Yes" ~ "PY",
                                     . == "No" ~ "N",
                                     TRUE ~ .)) %>% 
  select(-refid, -amstar_11nrsi_rating, -amstar_9nrsi_rating) %>% 
  arrange(str_to_lower(review_author_year))

# Create gt table and format
t2_formatted <- t2 %>% 
  gt() %>% 
  cols_label(review_author_year = "Review", 
             amstar_overall_rating = "Overall",
             amstar_1_rating = "1",
             amstar_2_rating = "2",
             amstar_3_rating = "3",
             amstar_4_rating = "4",
             amstar_5_rating = "5",
             amstar_6_rating = "6",
             amstar_7_rating = "7",
             amstar_8_rating = "8",
             amstar_9rct_rating = "9",
             amstar_10_rating = "10",
             amstar_11rct_rating = "11",
             amstar_12_rating = "12",
             amstar_13_rating = "13",
             amstar_14_rating = "14",
             amstar_15_rating = "15",
             amstar_16_rating = "16") %>% 
  tab_style(style = list(cell_borders(sides = "all", color = "black", weight = px(1))),
            locations = cells_body(columns = everything())) %>% 
  tab_style(style = list(cell_borders(sides = "all", color = "black", weight = px(1)),
                         cell_text(weight = "bold")),
            locations = cells_column_labels()) %>% 
  tab_options(column_labels.border.top.color = "black",
              column_labels.border.bottom.color = "black",
              table_body.border.bottom.color = "black",
              table.border.top.color = "white",
              heading.border.bottom.color = "black",
              table_body.hlines.color = "white",
              table.border.bottom.color = "white",
              table.font.names = "Times New Roman") %>% 
  cols_width(review_author_year ~ px(175),
             amstar_overall_rating ~ px(75),
             everything() ~ px(35)) %>% 
  tab_style(style = cell_text(align = "center"), locations = cells_column_labels(columns = everything())) %>% 
  tab_style(style = cell_text(align = "left"), locations = cells_column_labels(columns = "review_author_year")) %>% 
  tab_style(style = cell_text(align = "center"), locations = cells_body(columns = everything())) %>% 
  tab_style(style = cell_text(align = "left"), locations = cells_body(columns = "review_author_year")) %>% 
  tab_footnote(footnote = "N = No; PY = Partial Yes; Y = Yes; CL = Critically Low; L = Low; M = Moderate; H = High") %>% 
  data_color(columns = 2:18,
             colors = scales::col_factor(palette = c("#ab1d1a", "#8ace7e", "#e03531", "#ffda66", "#e03531", "#b2dfa8", "#8ace7e"),
                                         domain = c("CL", "H", "L", "M", "N", "PY", "Y")))

# # Save table
# # Save as HTML
# gtsave(t2_formatted, filename = "table3_amstar.html", path = here("outputs", "tables"))
# 
# # Save as word doc
# gtsave(t2_formatted, filename = "table3_word.docx", path = here("outputs", "tables"))

## Table 4. Risk of Bias in Included Systematic Reviews (ROBIS) ----

# Remove leading numbers, select variables
t3 <- robis_rating_td %>% 
  mutate_if(is.character, ~case_when(. == "HIGH" ~ "High",
                                     . == "LOW" ~ "Low",
                                     . == "UNCLEAR" ~ "Unclear",
                                     TRUE ~ .)) %>% 
  select(review_author_year, ends_with("decision"), robis_overall_a, robis_overall_b, robis_overall_c, robis_overall_rating) %>% 
  arrange(str_to_lower(review_author_year))

# Create gt table
t3_formatted <- t3 %>% 
  gt() %>% 
  cols_label(review_author_year = "Review", 
             robis_1_decision = "Domain 1",
             robis_2_decision = "Domain 2",
             robis_3_decision = "Domain 3",
             robis_4_decision = "Domain 4",
             robis_overall_a = "Interpretation",
             robis_overall_b = "Relevance",
             robis_overall_c = "Spin",
             robis_overall_rating = "Overall") %>% 
  tab_style(style = list(cell_borders(sides = "all", color = "black", weight = px(1))),
            locations = cells_body(columns = everything())) %>% 
  tab_style(style = list(cell_borders(sides = "all", color = "black", weight = px(1)),
                         cell_text(weight = "bold")),
            locations = cells_column_labels()) %>% 
  tab_options(column_labels.border.top.color = "black",
              column_labels.border.bottom.color = "black",
              table_body.border.bottom.color = "black",
              table.border.top.color = "white",
              heading.border.bottom.color = "black",
              table_body.hlines.color = "white",
              table.border.bottom.color = "white",
              table.font.names = "Times New Roman") %>% 
  cols_width(review_author_year ~ px(165),
             robis_overall_a ~ px(110),
             robis_overall_b ~ px(100),
             robis_overall_c ~ px(100),
             everything() ~ px(100),
             robis_overall_rating ~ px(50)) %>% 
  data_color(columns = 2:9,
             colors = scales::col_factor(palette = c("#e03531", "#8ace7e", "#e03531", "gray60", "#ef9997", "#b2dfa8", "#ffda66", "#8ace7e"),
                                         domain = c("High", "Low", "No", "No Information", "Probably No", "Probably Yes", "Unclear", "Yes")))


# # Save table
# # Save as html
# gtsave(t3_formatted, filename = "table4_robis.html", path = here("outputs", "tables"))
# 
# # Save as word doc
# gtsave(t3_formatted, filename = "table4_word.docx", path = here("outputs", "tables"))

## Table 5. Overall Meta-Analytic Estimates ----

# Subset data for table
t5 <- spo_ma_estimates %>% 
  filter(effect_tbl_type == "Main") %>% 
  left_join(review_td %>% select(refid, review_author_year), by = "refid") %>% 
  select(
    effect_outcome,
    effect_timing,
    effect_tbl_comparator,
    effect_tbl_studies,
    effect_tbl_ma_estimate,
    effect_tbl_heterogeneity,
    review_author_year
  ) %>% 
  mutate(
    across(everything(), ~ ifelse(is.na(.x) | .x == "", "NR", as.character(.x))),
    effect_timing_sort = as.numeric(effect_timing)
  ) %>% 
  arrange(str_to_lower(effect_outcome), effect_timing_sort)

# Add outcome group rows inside the table
t5_with_groups <- t5 %>% 
  group_split(effect_outcome) %>% 
  map_dfr(function(df) {
    bind_rows(
      tibble(
        effect_outcome = first(df$effect_outcome),
        effect_timing = "",
        effect_timing_sort = NA_real_,
        effect_tbl_comparator = first(df$effect_outcome),
        effect_tbl_studies = "",
        effect_tbl_ma_estimate = "",
        effect_tbl_heterogeneity = "",
        review_author_year = "",
        row_type = "group"
      ),
      df %>% mutate(row_type = "data")
    )
  }) %>% 
  select(-effect_outcome, -effect_timing, -effect_timing_sort)

# Format for table
t5_formatted <- t5_with_groups %>% 
  gt() %>% 
  cols_hide(row_type) %>% 
  cols_label(
    effect_tbl_comparator = "Comparator",
    effect_tbl_studies = "Studies",
    effect_tbl_ma_estimate = "Meta-analytic effect estimate",
    effect_tbl_heterogeneity = "Heterogeneity",
    review_author_year = "Review"
  ) %>% 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = row_type == "group")
  ) %>% 
  tab_style(
    style = cell_borders(sides = c("top", "bottom"), color = "black", weight = px(1)),
    locations = cells_body(columns = everything())
  ) %>% 
  tab_style(
    style = list(
      cell_borders(sides = c("top", "bottom"), color = "black", weight = px(1)),
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels()
  ) %>% 
  tab_options(
    column_labels.border.top.color = "black",
    column_labels.border.bottom.color = "black",
    table_body.border.bottom.color = "black",
    table_body.hlines.color = "black",
    table_body.vlines.color = "white",
    table.border.top.color = "black",
    table.border.bottom.color = "white",
    table.border.left.color = "black",
    table.border.right.color = "black",
    heading.border.bottom.color = "black",
    table.font.names = "Times New Roman"
  ) %>% 
  tab_source_note(
    source_note = md(
      "*Note:* CI = confidence interval, NR = not reported. NNT = number needed to treat, SMD = standardized mean difference."
    )
  ) %>% 
  cols_width(
    effect_tbl_comparator ~ pct(15),
    effect_tbl_studies ~ pct(15),
    effect_tbl_ma_estimate ~ pct(40),
    effect_tbl_heterogeneity ~ pct(20),
    review_author_year ~ pct(10)
  )

# # Save table
# # Save as html
# gtsave(t5_formatted, filename = "table5_estimates.html", path = here("outputs", "tables"))
# 
# # Save as word doc
# gtsave(t5_formatted, filename = "table5_word.docx", path = here("outputs", "tables"))

## Table 6. Meta-analytic estimates by intervention type, contrasts, or non-primary outcomes ----

# Subset for table
t6_base <- spo_ma_estimates %>% 
  filter(effect_tbl_type != "Main") %>% 
  filter(effect_tbl_2_group != "Knowledge-based outcomes") %>% 
  mutate(
    effect_tbl_2_group = factor(
      effect_tbl_2_group,
      levels = c(
        "Effect of targeting STBs",
        "Educational interventions",
        "Universal Prevention, effect of school level",
        "Follow-up time point contrasts"
      )
    )
  ) %>% 
  left_join(review_td %>% select(refid, review_author_year), by = "refid") %>%
  mutate(effect_timing_sort = as.numeric(effect_timing))

# Main rows
t6_main <- t6_base %>% 
  transmute(
    effect_tbl_2_group,
    effect_timing_sort,
    effect_tbl_2_name,
    effect_tbl_comparator,
    effect_tbl_studies,
    effect_tbl_ma_estimate,
    effect_tbl_heterogeneity,
    review_author_year,
    row_type = "data"
  )

# Subgroup rows, only when subgroup information exists
t6_subgroup <- t6_base %>% 
  filter(
    !is.na(effect_tbl_2_name_subgroup) |
      !is.na(effect_tbl_studies_subgroup) |
      !is.na(effect_tbl_ma_estimate_subgroup) |
      !is.na(effect_tbl_heterogeneity_subgroup)
  ) %>% 
  transmute(
    effect_tbl_2_group,
    effect_timing_sort = effect_timing_sort + 0.1,
    effect_tbl_2_name = paste0("   ", effect_tbl_2_name_subgroup),
    effect_tbl_comparator = effect_tbl_comparator,
    effect_tbl_studies = effect_tbl_studies_subgroup,
    effect_tbl_ma_estimate = effect_tbl_ma_estimate_subgroup,
    effect_tbl_heterogeneity = effect_tbl_heterogeneity_subgroup,
    review_author_year,
    row_type = "subgroup"
  )

# Combine main + subgroup rows
t6 <- bind_rows(t6_main, t6_subgroup) %>% 
  mutate(
    across(
      c(
        effect_tbl_2_name,
        effect_tbl_comparator,
        effect_tbl_studies,
        effect_tbl_ma_estimate,
        effect_tbl_heterogeneity,
        review_author_year
      ),
      ~ ifelse(is.na(.x) | .x == "", "NR", as.character(.x))
    )
  ) %>% 
  mutate(across(where(is.character), stringr::str_trim)) %>% 
  arrange(effect_tbl_2_group, effect_tbl_2_name, effect_timing_sort)

# Add group header rows inside the table
t6_with_groups <- t6 %>% 
  group_split(effect_tbl_2_group) %>% 
  map_dfr(function(df) {
    bind_rows(
      tibble(
        effect_tbl_2_group = first(df$effect_tbl_2_group),
        effect_timing_sort = NA_real_,
        effect_tbl_2_name = as.character(first(df$effect_tbl_2_group)),
        effect_tbl_comparator = "",
        effect_tbl_studies = "",
        effect_tbl_ma_estimate = "",
        effect_tbl_heterogeneity = "",
        review_author_year = "",
        row_type = "group"
      ),
      df
    )
  }) %>% 
  select(-effect_tbl_2_group, -effect_timing_sort)

# Format table
t6_formatted <- t6_with_groups %>% 
  gt() %>% 
  cols_hide(row_type) %>% 
  cols_label(
    effect_tbl_2_name = md("Outcome:<br>Intervention"),
    effect_tbl_comparator = "Comparator",
    effect_tbl_studies = "Studies",
    effect_tbl_ma_estimate = "Meta-analytic effect estimate",
    effect_tbl_heterogeneity = "Heterogeneity",
    review_author_year = "Review"
  ) %>% 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      rows = row_type == "group"
    )
  ) %>% 
  # tab_style(
  #   style = list(
  #     cell_borders(sides = "all", color = "black", weight = px(1))
  #   ),
  #   locations = cells_body(columns = everything())
  # ) %>% 
  # tab_style(
  #   style = list(
  #     cell_borders(sides = "all", color = "black", weight = px(1)),
  #     cell_text(weight = "bold")
  #   ),
  #   locations = cells_column_labels()
  # ) %>% 
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),  # ONLY horizontal lines
      color = "black",
      weight = px(1)
    ),
    locations = cells_body(columns = everything())
  ) %>%
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),  # header horizontal lines only
      color = "black",
      weight = px(1)
    ),
    locations = cells_column_labels()
  ) %>% 
  tab_source_note(
    source_note = md(
      "**Note.** NR = not reported, SMD = standardized mean difference, CI = confidence interval, STBs = suicidal thoughts and behaviors, OR = odds ratio."
    )
  ) %>% 
  tab_options(
    column_labels.border.top.color = "black",
    column_labels.border.bottom.color = "black",
    table_body.border.bottom.color = "black",
    table.border.top.color = "white",
    table.border.bottom.color = "white",
    heading.border.bottom.color = "black",
    table_body.hlines.color = "white",
    table.font.names = "Times New Roman"
  )

# # Save table
# # Save as html
# gtsave(t6_formatted, filename = "table6_estimates.html", path = here("outputs", "tables"))
# 
# # Save as word doc
# gtsave(t6_formatted, filename = "table6_word.docx", path = here("outputs", "tables"))

# 11. Create figures ------------------------------------------------------

## Figure 1. PRISMA Flow Diagram ----

# Function for PRISMA for Overviews - from make_prisma_diagrams.R (HEDCO R project)
make_prisma_overview_plot <- function(elig_df, dupe_df, review_df) {
  
  # Create objects needed from variables
  number_of_duplicates <- nrow(dupe_df)
  number_search_results <- nrow(elig_df) + number_of_duplicates
  number_dropped <- sum(elig_df$screening_decision == "Drop")
  number_kept <- sum(elig_df$screening_decision == "Keep")
  number_not_retrieved <- sum(elig_df$pdf_retrieved == "No", na.rm = TRUE)
  number_ineligible <- sum(elig_df$eligibility_decision == "Not Eligible" | elig_df$eligibility_reference_type == "Study", na.rm = TRUE)
  number_eligible <- nrow(review_df)
  number_eligible_reports <- sum(elig_df$eligibility_decision == "Eligible" & elig_df$eligibility_reference_type == "Review", na.rm = TRUE)
  
  # Calculate exclusion counts
  exclusion_reasons_summary <- elig_df %>% 
    mutate(reason = dplyr::case_when(
      eligibility_reference_type == "Study" ~ "Ineligible study design",
      TRUE ~ as.character(eligibility_exclude_reason))) %>% 
    filter(!is.na(reason)) %>% 
    count(reason, name = "n") %>% 
    arrange(desc(n))
  
  # Define PRISMA Counts
  records_identified <- number_search_results
  duplicates_removed <- number_of_duplicates
  records_screened <- records_identified - duplicates_removed
  records_excluded <- number_dropped
  reports_sought <- number_kept
  reports_not_retrieved <- number_not_retrieved
  reports_assessed <- reports_sought - reports_not_retrieved
  reports_excluded_total <- number_ineligible
  studies_included <- number_eligible
  reports_included <- number_eligible_reports
  
  
  # Expose as a named vector for easy glue’ing
  exclusion_counts <- setNames(
    exclusion_reasons_summary$n,
    exclusion_reasons_summary$reason
  )
  
  # Collapse into markdown bullets
  bullet_text <- glue_collapse(
    glue("- {names(exclusion_counts)}: {exclusion_counts}"),
    sep = "\n"
  )
  
  cat(bullet_text)
  
  ## PLOT 
  
  # Define box content: x, y = center positions; label = box text
  boxes <- tibble(
    x = c(3, 6.5, 3, 6.5, 3, 6.5, 3, 6.5, 3),
    y = c(9, 9, 7.5, 7.5, 6, 6, 4.5, 3.9, 3),
    height = c(rep(1, 7), 2.8, 1),
    width  = c(rep(2.6, 7), 3.5, 2.6),
    label = c(
      glue("Records identified\n(n = {formatC(records_identified, format = 'd', big.mark = ',')})"),
      glue("Duplicates removed\n(n = {formatC(duplicates_removed, format = 'd', big.mark = ',')})"),
      glue("Records screened\n(n = {formatC(records_screened, format = 'd', big.mark = ',')})"),
      glue("Records excluded\n(n = {formatC(records_excluded, format = 'd', big.mark = ',')})"),
      glue("Reports sought\n(n = {formatC(reports_sought, format = 'd', big.mark = ',')})"),
      glue("Not retrieved\n(n = {formatC(reports_not_retrieved, format = 'd', big.mark = ',')})"),
      glue("Assessed for eligibility\n(n = {formatC(reports_assessed, format = 'd', big.mark = ',')})"),
      NA_character_,
      glue("Studies included\n(n = {formatC(studies_included, format = 'd', big.mark = ',')})\nReports = {formatC(reports_included, format = 'd', big.mark = ',')}")
    ),
    label_top = NA_character_,
    label_bullets = NA_character_
  )
  
  # Centered top line for “Reports excluded”
  boxes$label_top[8] <- glue(
    "Reports excluded\n(n = {formatC(reports_excluded_total, format = 'd', big.mark = ',')})\n"
  )
  
  
  # Dynamically inject the bullets for row 8
  boxes$label_bullets[8] <- bullet_text
  
  
  # Define arrow segments (from_x, from_y, to_x, to_y)
  arrows <- tribble(
    ~x,  ~y,   ~xend, ~yend,
    3,   8.5,    3,     8,       # Records identified → Records screened
    4.3,   9,    5.2,     9,     # Records identified → Duplicates removed
    3,     7,    3,     6.5,     # Records screened → Reports sought
    4.3,   7.5,  5.2,     7.5,   # Records screened → Records excluded
    3,     5.5,    3,     5,     # Reports sought → Assessed for eligibility
    4.3,   6,    5.2,     6,     # Reports sought → Not retrieved
    3,     4,    3,     3.5,     # Assessed → Included
    4.3,   4.5,  4.75,   4.5   # Assessed → Reports excluded
  )
  
  # Define section headers
  sections <- data.frame(
    label = c("Identification", "Screening", "Included"),
    x = c(1, 1, 1),
    y = c(9, 6, 3),
    height = c(1.6, 4.1, 1.5)
  )
  
  
  # Plot
  ggplot() +
    # Boxes
    geom_rect(data = boxes, aes(xmin = x - width/2, xmax = x + width/2,
                                ymin = y - height/2, ymax = y + height/2),
              fill = "white", color = "black") +
    
    # Normal text
    geom_text(data = boxes, aes(x = x, y = y, label = label), size = 3.2, lineheight = 1.1) +
    
    # Centered top line for reports excluded
    geom_text(data = boxes[8, ], aes(x = x, y = y + 1.1, label = label_top),
              size = 3.2, hjust = 0.5, lineheight = 1.1) +
    
    # Left-aligned bullets
    geom_text(data = boxes[8, ], aes(x = x - 1.55, y = y - 0.25, label = label_bullets),
              size = 3, hjust = 0, lineheight = 1.1) +
    
    
    # Arrows
    geom_segment(data = arrows,
                 aes(x = x, y = y, xend = xend, yend = yend),
                 arrow = arrow(length = unit(0.15, "inches")),
                 lineend = "round") +
    
    # Section header background
    geom_rect(data = sections,
              aes(xmin = x - 0.25, xmax = x + 0.25,
                  ymin = y - height / 2, ymax = y + height / 2),
              fill = "lightblue", color = NA) +
    
    # Section header text
    geom_text(data = sections,
              aes(x = x, y = y, label = label),
              angle = 90, size = 4) +
    
    coord_equal() +
    theme_void()
  
}

 

# Clean exclusion reasons
spo_eligibility <- spo_eligibility %>%
  mutate(
    eligibility_exclude_reason = eligibility_exclude_reason %>%
      stringr::str_remove_all("\\s*\\([^)]*\\)") %>%  #remove " (anything)"
      stringr::str_trim()                             #remove leading/trailing spaces
  )

# Use function
spo_overview_prisma <- make_prisma_overview_plot(elig_df = spo_eligibility, dupe_df = spo_duplicates, review_df = spo_review_level)

# # Save
# ggsave(filename = "outputs/figures/figure1_prisma.png", spo_overview_prisma)


## Figure 2. Overlap of included studies across systematic reviews ----

# Check if there are any studies that need specific note
asterisk_studies <- td_cm %>%
  filter(str_detect(study, "\\*$")) %>%
  pull(study) %>%
  str_remove("\\*$")

# Helper function to buid caption text
asterisk_caption <- if(length(asterisk_studies) == 0){
  NULL
} else if(length(asterisk_studies) == 1){
  glue("Only meta-analyzed studies were reported for {asterisk_studies}.")
} else {
  study_text <- toString(asterisk_studies)
  study_text <- sub(", ([^,]+)$", ", and \\1", study_text)
  glue("Only meta-analyzed studies were reported for {study_text}")
}

# Identify review columns with trailing asterisks
starred_reviews <- names(td_cm) %>%
  .[. != "study"] %>%
  .[str_detect(., "\\*$")] %>%
  str_remove("\\*$")

# Create sentence for caption
star_note <- if(length(starred_reviews) == 0){
  NULL
} else if(length(starred_reviews) == 1){
  glue("Only meta-analyzed studies were reported for {starred_reviews}")
} else if(length(starred_reviews) == 2){
  glue("Only meta-analyzed studies were reported for {starred_reviews[1]} and {starred_reviews[2]}")
} else {
  glue(
    "Only meta-analyzed studies were reported for {paste(starred_reviews[-length(starred_reviews)], collapse = ', ')}, and {starred_reviews[length(starred_reviews)]}"
  )
}

# Remove trailing asterisks from review column names
td_cm_clean <- td_cm
names(td_cm_clean) <- names(td_cm_clean) %>%
  str_replace("\\*$", "")

# Format citation matrix including our review
cca_inc <- td_cm_clean %>% 
  mutate_at(vars(-study), ~ifelse(. == "Yes", 1, 0)) 

# Create heatmap first
f2 <- cca_heatmap(cca_inc, decimal_digits = 0, fontsize = 12, fontsize_diag = 8)

# Grab existing caption from cca_heatmap and append note
existing_caption <- f2$labels$caption
combined_caption <- paste(
  c(existing_caption, star_note),
  collapse = "\n"
)

# Apply updated caption and theme
f2 <- f2 +
  ggplot2::labs(caption = combined_caption) +
  ggplot2::theme(
    plot.caption = ggplot2::element_text(size = 20, margin = ggplot2::margin(30,0,0,0)),
    legend.title = ggplot2::element_text(size = 20, face = "bold", vjust = 4),
    legend.text = ggplot2::element_text(size = 20),
    legend.key.size = ggplot2::unit(1.0, "cm"),
    legend.title.align = 0.5,
    legend.text.align = 0.5,
    axis.text.x = ggplot2::element_text(size = 26),
    axis.text.y = ggplot2::element_text(size = 26),
    axis.title = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_line(colour = "grey80", linetype = "dashed")
  )

# # Save figure
# # Set image dimensions and file path to save
# png(here("outputs", "figures", "figure2_heatmap.png"), width = 1000, height = 1000)
# 
# # Show plot
# f2
# 
# # Save png of plot
# dev.off()


# 12. Create appendices ---------------------------------------------------

## Appendix 1. List of excluded reviews ----

# Import citation info 
review_elig_citations <- spo_eligibility

# Create df of excluded reviews and citations
a1_info <- review_elig_citations %>% 
  filter(eligibility_decision == "Not Eligible") %>% 
  filter(eligibility_exclude_reason != "Unclear") %>% 
  rename(citation = bibliography) %>% 
  select(citation, eligibility_exclude_reason) %>% 
  arrange(str_to_lower(stringi::stri_trans_general(citation, "Latin-ASCII")))

# Create formatted gt table
a1_formatted <- a1_info %>% 
  gt() %>% 
  cols_label(
    citation = "Excluded Review", 
    eligibility_exclude_reason = "Reason for Exclusion"
  ) %>% 
  tab_style(
    style = list(
      cell_borders(sides = "all", color = "black", weight = px(1))
    ),
    locations = cells_body(columns = everything())
  ) %>% 
  tab_style(
    style = list(
      cell_borders(sides = "all", color = "black", weight = px(1)),
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels()
  ) %>% 
  tab_style(
    style = cell_text(align = "left"),
    locations = cells_column_labels(columns = citation)
  ) %>% 
  tab_style(
    style = cell_text(align = "left"),
    locations = cells_body(columns = citation)
  ) %>% 
  tab_options(
    column_labels.border.top.color = "black",
    column_labels.border.bottom.color = "black",
    table_body.border.bottom.color = "black",
    table.border.top.color = "white",
    heading.border.bottom.color = "black",
    table_body.hlines.color = "white",
    table.border.bottom.color = "white",
    table.font.names = "Times New Roman"
  ) %>% 
  tab_header(
    title = md("**<div style='text-align: center;'>Appendix 1</div>**<div style='text-align: center; margin-top:10px; margin-bottom:20px;'>**List of Reviews Excluded at Full-Text Eligibility Assessment**<br></div>")
  ) %>% 
  tab_style(
    style = list(cell_text(align = "center")),
    locations = cells_title(groups = "title")
  )


# # Save as word
# gtsave(a1_formatted, filename = "appendix1_word.docx", path = here("outputs", "appendices"))
# 
# # Save as html
# gtsave(a1_formatted, filename = "appendix_1.html", path = here("outputs", "appendices"))


## Appendix 2. Reviews awaiting classification ----

# Create df with studies awaiting classification
a3_info <- review_elig_citations %>% 
  filter(eligibility_exclude_reason == "Unclear") %>% 
  mutate(eligibility_notes = str_remove_all(eligibility_notes, "\\(.*?\\)")) %>% 
  mutate(eligibility_notes = str_remove_all(eligibility_notes, "; Review")) %>% 
  rename(citation = bibliography) %>% 
  select(citation, eligibility_notes) %>% 
  arrange(str_to_lower(stringi::stri_trans_general(citation, "Latin-ASCII")))



# Create formatted gt table
a3_formatted <- a3_info %>% 
  gt()  %>% 
  cols_label(citation = "Review", 
             eligibility_notes = "Reason") %>% 
  tab_style(style = list(cell_borders(sides = "all", color = "black", weight = px(1))),
            locations = cells_body(columns = everything())) %>% 
  tab_style(style = list(cell_borders(sides = "all", color = "black", weight = px(1)),
                         cell_text(weight = "bold")),
            locations = cells_column_labels()) %>% 
  tab_options(column_labels.border.top.color = "black",
              column_labels.border.bottom.color = "black",
              table_body.border.bottom.color = "black",
              table.border.top.color = "white",
              heading.border.bottom.color = "black",
              table_body.hlines.color = "white",
              table.border.bottom.color = "white",
              table.font.names = "Times New Roman") %>% 
  tab_header(title = md("**<div style='text-align: center;'>Appendix 2</div>**<div style='text-align: center; margin-top:10px; margin-bottom:20px;'>**List of Reviews Awaiting Classification**<br></div>")) %>% 
  tab_style(style = list(cell_text(align = "center")),
            locations = cells_title(groups = "title"))


# # Save as word
# gtsave(a3_formatted, filename = "appendix2_word.docx", path = here("outputs", "appendices"))
# 
# # Save as html
# gtsave(a3_formatted, filename = "appendix_2.html", path = here("outputs", "appendices"))


## Appendix 3. Descriptive characteristics for each included systematic review ----

# Get meta-analytic summary info
spo_ma_summary <- spo_ma_estimates %>% 
  filter(!is.na(effect_tbl_summary)) %>% 
  mutate(
    # Keep colon at end
    before_colon = stringr::str_extract(effect_tbl_summary, "^[^:]+:"),
    
    # Everything AFTER first colon
    after_colon  = stringr::str_replace(effect_tbl_summary, "^[^:]+:\\s*", "")
  ) %>% 
  group_by(refid, before_colon) %>% 
  summarize(
    bullets = paste0(
      "<ul>",
      paste0("<li>", after_colon, "</li>", collapse = ""),
      "</ul>"
    ),
    .groups = "drop"
  ) %>% 
  group_by(refid) %>% 
  summarize(
    review_ma_findings = paste0(
      before_colon,
      bullets,
      collapse = "<br><br>"
    ),
    .groups = "drop"
  )

# Create COI variable and Critical Appraisal section
spo_review_info <- spo_review_level %>%
  mutate(
    review_coi = case_when(
      amstar_16_rating == "No" ~ "Lacks reporting on funding and/or conflicts of interest",
      
      !is.na(amstar_16_coi_the_authors_reported_no_competing_interests_or) &
        is.na(amstar_16_coi_the_authors_described_their_funding_sources_and_how_they_managed_potential_conflicts_of_interest) ~
        "The authors declared no competing interests",
      
      is.na(amstar_16_coi_the_authors_reported_no_competing_interests_or) &
        !is.na(amstar_16_coi_the_authors_described_their_funding_sources_and_how_they_managed_potential_conflicts_of_interest) ~
        "Disclosed funding sources and management of any potential conflicts of interest",
      
      !is.na(amstar_16_coi_the_authors_reported_no_competing_interests_or) &
        !is.na(amstar_16_coi_the_authors_described_their_funding_sources_and_how_they_managed_potential_conflicts_of_interest) ~
        "The authors declared no competing interests and/or disclosed funding sources and management of any potential conflicts",
      
      TRUE ~ NA_character_
    ),
    review_critical_appraisal = paste0(
      "<ul>",
      "<li><strong>AMSTAR-2:</strong> ", str_to_sentence(amstar_overall_rating), "</li>",
      "<li><strong>ROBIS:</strong> ", str_to_sentence(robis_overall_rating), "</li>",
      "</ul>"
    )
  ) %>%
  select(refid, review_coi, review_critical_appraisal)

# Select relevant appendix 4 info
a4_info <- review_td %>%
  left_join(numinc_perreview) %>%
  left_join(spo_review_info, by = "refid") %>%
  mutate(search_date_format = ifelse(nchar(review_search_date) == 10, format(ymd(review_search_date), "%B %d, %Y"), review_search_date)) %>%
  mutate(search_date_format = ifelse(nchar(review_search_date) == 7, format(ym(review_search_date), "%B %Y"), search_date_format)) %>%
  mutate(across(everything(), ~ifelse(.x == -999, "Not reported", .x)),
         n_included = as.character(n_included)
  ) %>%
  select(refid, review_author_year, starts_with("review_main_focus"), starts_with("review_eligibility"), review_databases_searched,
         search_date_format, n_included, review_flow_diagram, review_registration_number, review_availability_statement, 
         review_coi, review_critical_appraisal) %>%
  arrange(str_to_lower(review_author_year))

# Create df of review_author_year and refid for merging
review_idbyname <- review_td %>%
  select(refid, review_author_year)

# Select citation info for eligible reviews
review_citations <- review_elig_citations %>%
  janitor::clean_names() %>%
  left_join(review_idbyname) %>%
  filter(!is.na(review_author_year)) %>%
  rename(citation = bibliography) %>% 
  select(refid, citation, title)

# Pull list of included refids
inc_rev <- review_citations$refid

# Transform for merging
linked_ref_td <- linked_ref_df %>%
  filter(refid %in% inc_rev) %>%
  select(refid, linked_refid) %>%
  left_join(review_citations) %>%
  left_join(review_elig_citations, by = c("linked_refid" = "refid")) %>%
  rename(main_citation = citation,
         linked_citation = bibliography) %>%
  select(refid, main_citation, starts_with("linked")) %>%
  pivot_longer(cols = c(main_citation, linked_citation),
               names_to = "citation_type",
               values_to = "citation",
               values_drop_na = TRUE) %>%
  mutate(citation_refid = case_when(citation_type == "main_citation" ~ as.character(refid),
                                    citation_type == "linked_citation" ~ as.character(linked_refid))) %>%
  select(-c(linked_refid, citation_type, linked_references)) %>%
  filter(refid != citation_refid)

# Create variable that combines all references for a review into one cell
review_reports <- review_citations %>%
  select(refid, citation) %>%
  mutate(is_main = TRUE) %>%
  mutate(citation_refid = "") %>%
  #rbind(linked_ref_td) %>%
  rbind(
    linked_ref_td %>% mutate(is_main = FALSE)
  ) %>%
  group_by(refid) %>%
  # summarize(all_reports = paste(citation, collapse = " <br><br> ")) %>%
  # mutate(all_reports = stringi::stri_trans_general(all_reports, "Latin-ASCII"),
  #        all_reports = clean_citation_text(all_reports)) %>%
  # ungroup()
  summarize(
    all_reports = {
      main <- citation[is_main][1]
      others <- citation[!is_main]
      
      if(length(others) > 0){
        paste0(
          main,
          "<ul>",
          paste0("<li>", others, "</li>", collapse = ""),
          "</ul>"
        )
      } else {
        main
      }
    }
  ) %>%
  mutate(
    all_reports = stringi::stri_trans_general(all_reports, "Latin-ASCII"),
    all_reports = clean_citation_text(all_reports)
  ) %>%
  ungroup()


# Extract titles from citation df
rev_titles <- review_citations %>%
  select(refid, title)

# Merge titles and citations with other a4 info and format eligibility cells
a4 <- a4_info %>%
  left_join(rev_titles, by = "refid") %>%
  left_join(review_reports, by = "refid") %>%
  left_join(spo_ma_summary, by = "refid") %>%
  select(review_author_year, title, starts_with("review_main_focus"), review_eligibility_participant:review_availability_statement, review_coi,
         review_critical_appraisal, all_reports, review_ma_findings) %>%
  # mutate_at(vars(starts_with("review_eligibility"), review_availability_statement), list(~ str_replace_all(., "\\s+-\\s+(?=[A-Z])", "<br> - "))) %>%
  # mutate_at(vars(starts_with("review_eligibility"), review_availability_statement), ~ str_replace_all(., "- +([\"'a-zA-Z])", " <li>\\1")) %>%
  # mutate_at(vars(starts_with("review_eligibility")), ~ paste0("<ul>", ., "</ul>")) %>%
  # mutate(review_availability_statement = ifelse(str_detect(review_availability_statement, "<li>"),
  #                                               paste0("<ul>", review_availability_statement, "</ul>"),
  #                                               review_availability_statement))
  mutate(
    across(everything(), ~ ifelse(is.na(.x) | .x == "", "Not reported", as.character(.x))),
    
    main_focus = paste0(
      "<ul>",
      "<li><strong>Suicide:</strong> ", if_else(!is.na(review_main_focus_suicide), "yes", "no"), "</li>",
      "<li><strong>Prevention:</strong> ", if_else(!is.na(review_main_focus_prevention), "yes", "no"), "</li>",
      "<li><strong>School-aged youth:</strong> ", if_else(!is.na(review_main_focus_school_aged_youth), "yes", "no"), "</li>",
      "<li><strong>School settings:</strong> ", if_else(!is.na(review_main_focus_school_settings), "yes", "no"), "</li>",
      "</ul>"
    ),
    
    eligibility_criteria = paste0(
      "<ul>",
      "<li><strong>Participants:</strong> ", review_eligibility_participant, "</li>",
      "<li><strong>Interventions:</strong> ", review_eligibility_intervention, "</li>",
      "<li><strong>Comparisons:</strong> ", review_eligibility_comparator, "</li>",
      "<li><strong>Outcomes:</strong> ", review_eligibility_outcome, "</li>",
      "<li><strong>Timing:</strong> ", review_eligibility_timing, "</li>",
      "<li><strong>Setting:</strong> ", review_eligibility_setting, "</li>",
      "<li><strong>Design:</strong> ", review_eligibility_design, "</li>",
      "</ul>"
    ),
    
    search_strategy = paste0(
      "<ul>",
      "<li><strong>Databases searched:</strong> ", review_databases_searched, "</li>",
      "<li><strong>Search date:</strong> ", search_date_format, "</li>",
      "<li><strong>Included studies:</strong> ", n_included, "</li>",
      "</ul>"
    ),
    
    methodological_transparency = paste0(
      "<ul>",
      "<li><strong>PRISMA diagram:</strong> ", review_flow_diagram, "</li>",
      "<li><strong>Registration:</strong> ", review_registration_number, "</li>",
      "<li><strong>Data availability statement:</strong> ", review_availability_statement, "</li>",
      "<li><strong>Conflict of interest:</strong> ", review_coi, "</li>",
      "</ul>"
    )
  ) %>%
  select(
    review_author_year,
    title,
    main_focus,
    eligibility_criteria,
    search_strategy,
    methodological_transparency,
    review_critical_appraisal,
    review_ma_findings,
    all_reports
  )


# Split data frame into list of data frames
a4_dflist <- map(1:nrow(a4), ~a4[.x, ])

# Transfer to long format
df_list_long <- map(a4_dflist, ~ .x %>%
                      pivot_longer(cols = everything(),
                                   names_to = "variable",
                                   values_to = "value") %>%
                      # mutate(variable = case_when(variable == "review_author_year" ~ "Review",
                      #                             variable == "title" ~ "Title",
                      #                             variable == "review_eligibility_participant" ~ "Participants",
                      #                             variable == "review_eligibility_intervention" ~ "Interventions",
                      #                             variable == "review_eligibility_comparator" ~ "Comparisons",
                      #                             variable == "review_eligibility_outcome" ~ "Outcomes",
                      #                             variable == "review_eligibility_timing" ~ "Timing",
                      #                             variable == "review_eligibility_setting" ~ "Setting",
                      #                             variable == "review_eligibility_design" ~ "Design",
                      #                             variable == "review_databases_searched" ~ "Databases Searched",
                      #                             variable == "search_date_format" ~ "Search Date",
                      #                             variable == "review_flow_diagram" ~ "Prisma Diagram",
                      #                             variable == "review_registration_number" ~ "Registration",
                      #                             variable == "review_availability_statement" ~ "Data Availability Statement",
                      #                             variable == "review_coi" ~ "Conflict of Interest",
                      #                             variable == "review_critical_appraisal" ~ "Critical Appraisal",
                      #                             variable == "n_included" ~ "Included Studies",
                      #                             variable == "all_reports" ~ "References",
                      #                             TRUE ~ variable)))
                      mutate(variable = case_when(
                        variable == "review_author_year" ~ "Review",
                        variable == "title" ~ "Title",
                        variable == "main_focus" ~ "Main Focus",
                        variable == "eligibility_criteria" ~ "Eligibility Criteria",
                        variable == "search_strategy" ~ "Search Strategy",
                        variable == "methodological_transparency" ~ "Methodological Transparency",
                        variable == "review_critical_appraisal" ~ "Critical Appraisal",
                        variable == "review_ma_findings" ~ "Meta-Analytic Findings",
                        variable == "all_reports" ~ "References",
                        TRUE ~ variable
                      )))

# Combine into one single data frame
combined_a4 <- bind_rows(df_list_long, .id = "source") %>%
  mutate(source = as.numeric(source))

# Create gt table for each for each review
list_gt <- lapply(split(combined_a4, combined_a4$source), function(x) {
  gt(x) %>%
    cols_hide("source") %>%
    cols_label(variable = "",
               value = "") %>%
    tab_style(style = list(cell_text(weight = "bold")),
              locations = cells_body(columns = "variable")) %>%
    # tab_row_group(rows = 3:9, label = "Eligibility Criteria:") %>%
    # tab_row_group(rows = 1:2, label = "", id = "group1") %>%
    tab_options(table.font.names = "Times New Roman",
                table.width = pct(100)) %>%
    # tab_style(style = cell_text(weight = "bold"), locations = cells_row_groups()) %>%
    # tab_style(style = cell_text(align = "right"), locations = cells_body(columns = "variable", rows = 3:9)) %>%
    fmt_markdown(columns = everything())
})

# Add title
a4_title_html <- '<h2 style="text-align:center;font-family:Times New Roman;font-size:20px;margin-bottom:5px;margin-top:25px;">
  <div>Appendix 3</div>
  <div style="margin-top:10px; margin-bottom:10px;">Characteristics of Included Reviews</div>
</h2>'


# Extract HTML code of table
html_code <- list_gt %>%
  map(as_raw_html) %>%
  reduce(paste) %>%
  paste(a4_title_html, ., sep = "\n")

# # Save as HMTL
 writeLines(html_code, here("outputs", "appendices", "appendix_3.html"))
# 
# # Export in word
# a4_word <- combined_a4 %>%
#   mutate(
#     value = value %>%
#       str_replace_all("<ul>", "") %>%
#       str_replace_all("</ul>", "") %>%
#       str_replace_all("<li>", "• ") %>%
#       str_replace_all("</li>", "\n")
#   ) %>%
#   gt(groupname_col = "source") %>%
#   cols_hide("source") %>%
#   cols_label(
#     variable = "",
#     value = ""
#   ) %>%
#   tab_style(
#     style = cell_text(weight = "bold"),
#     locations = cells_body(columns = "variable")
#   ) %>%
#   tab_options(
#     table.font.names = "Times New Roman",
#     table.width = pct(100)
#   )#%>%
#   #fmt_markdown(columns = everything())
# 
# gtsave(
#   a4_word,
#   filename = here::here("outputs", "appendices", "appendix3_word.docx")
# )

# 13. Save objects for manuscript reporting ------------------------

# # Get names of objects currently in the environment
# bundle_names <- ls()
# 
# # Create named list of all remaining objects
# bundle <- mget(bundle_names, inherits = TRUE)
# 
# # Save one file
# saveRDS(bundle, here("outputs", "objects", "analysis_script_objects.rds"))


