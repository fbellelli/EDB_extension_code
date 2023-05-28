# Written by Francesco S. Bellelli - May 2021
# For queries contact: F.S.Bellelli@gmail.com
# R version 4.03 - Windows 

#PRELIMINARIES ------------------------------------

#set working directory -> make sure it correspond to location of this file
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  

#load and install required libraries
memory.limit(64000)
source("functions/check packages.R")
check_packages("tidyverse","data.table","stopwords","udpipe","fastmatch","stringdist","Matrix")
#__________________________________________________


#IMPORT EDB DATA ----------------------------------

#import EDB data (picks first csv file in the "raw EDB data" folder)
EDB_file_path <- list.files(path="./EDB raw data", pattern="*.csv", full.names=TRUE, recursive=FALSE)[1]
data <- fread(EDB_file_path, keepLeadingZeros = TRUE)
#__________________________________________________


#1) CHECK COLUMN NAMES ----------------------------

#check the names of the columns in the dataset in order to allow for a correct execution of the script
#if not exact match is found, the function will try to look for a close match
source("functions/check column names.R")
data <- check_colnames(data)
#__________________________________________________


#2) CHECK COUNTRY NAMES ---------------------------

#This function creates a new columns "COUNTRIES" in which notifying Members' countries are listed with their official WTO names.
#This will check for any inconsistency in country naming and unify it across dataset.
#Moreover, it will split EU into individual countries to allow merging with country-level data. All measures from the EU are assumed to have been adopted by every EU Members, and by the UK for measures notified up to 2019.
#[execution time ~10 seconds]
source("functions/country names.R")
data <- extract_countries(data,
                          brexit_year = 2020)   #for all EU notification from this year on, UK is not listed among countries associated with the measures 
#__________________________________________________


#3) EXTRACT IMPLEMENTATION YEARS ------------------

#the function below creates a column "START" and "END" containing the extracted initial and final (if any is detected) implementation years.
#[execution time ~30 seconds]
source("functions/extract implementation years.R")
data <- extract_implementation_years(data)
#__________________________________________________


#4) ASSOCIATE ISIC CODES --------------------------

#the function below creates a column "ISIC" with 2-digits ISIC codes associated to the "sectors" variable of the EDB
#[execution time ~10 seconds]
source("functions/associate ISIC codes.R")
data <- associate_ISIC(data)
#__________________________________________________


#5) TOKENISE & ANNOTATE DESCRIPTIONS --------------

#the following function uses a machine learning algorithm to extract, lemmatise, and annotate words from the columns "Measure description", "Environment-related objective" and "Coverage of the measure"
#the table created by this function will be used in step 6, 7 and 8
#[execution time ~15 MINUTES!!]
source("functions/tokenise descriptions.R")
measures_keywords <- tokenise_description(measures_data = data,
                                          udpipe_model_path = "programme files/english-ewt-ud-2.4-190531.udpipe")
#__________________________________________________


#6) LOOK FOR SIMILAR MEASURES ---------------------

#the following function adds two columns containing a list of similar measures notified by the same country, and a measure of similarity
#[execution time ~ 5 MINUTES]
source("functions/measure similarity.R")
data <- measure_similarity(data,
                           measures_keywords,
                           cut_off = 0.5,    #Cut-off value for the similarity index (a value between 0 and 1)
                           match_country=TRUE) #if set to TRUE, similarity is checked only for measures sharing at least one country in common. (execution time does not significantly change if set to FALSE)
#__________________________________________________


#7) MATCHING HS CODES -----------------------------

#The function below matches HS chapters (2-digits HS codes) to the mesures in the EDB and identifies HS / ICS codes reported by members
#The function outpts a table with the HS chapters associated to each measure, as well as a 
#[execution time ~15 MINUTES]
source("functions/match HS chapters.R")
matched_codes <- match_HS_chapters(measures_data = data,
                                   measures_keywords = measures_keywords,
                                   udpipe_model_path = "programme files/english-ewt-ud-2.4-190531.udpipe",   #path to the model containing ML model for lemmatisation and annotation of keywords
                                   path_HS = "programme files/HS_cleaned_description.csv",     #path to the file containing HS code descriptions
                                   path_ICS_conversion_table = "programme files/ICS_HS_conc.csv",   #path to conversion table HS-ICS codes (Razi's table)
                                   path_OECD_env_goods = "programme files/table_OECD_env_goals_chapters.csv",   #path to the correspondence table between harmonised objectives and environmental chapters (based on OECD environmental goods list)
                                   keyword_threshold = 20, #size-reduction parameters (see methodology note for details) - all keywords that are found in more than this specified number of chapters will be ignored for matching
                                   cut_off_absolute = 0.7, #size-reduction parameters (see methodology note for details) -  between 0 and 1: percentile of the absolute link strength (L-tilde) distribution which is set as cut-off value. For instance, 0.1 means that the weakest 10% of the links are eliminated. 
                                   cut_off_relative = 0.1, #size-reduction parameters (see methodology note for details) - between 0 and 1: FOR EACH MEASURE, only the links with relative strength (L-bar) above this value are retained. 
                                   weight_sector_yes = 1 ,  #Step 3 weights (see methodology note) - weight to be applied to sectors which match with the HS code. (Notice that measures labelled as "Service" are applied a weight of 0, no matter what)
                                   weight_sector_no = 0.5,  #Step 3 weights (see methodology note) - weight to be applied to sectors which DO NOT match with the HS code.
                                   weight_env_obj_yes = 1,  #Step 3 weights (see methodology note) - weight applied to links with consistent environmental objective
                                   weight_env_obj_no = 0.9)
#__________________________________________________


#8) SCORE MEASURES --------------------------------

#The function below adds 9 new columns corresponding to the the measure strength score and all its components. (MEASURE SCORE, DEPTH, BREADTH, DEPTH_measures_instrument, DEPTH_measures_variety, DEPTH_wording, BREADTH_env_objectives, BREADTH_env_keywords, BREADTH_economic_sectors)
#For details on calculations refer to Appendix of the paper
#[execution time ~2 minutes]
source("functions/score measures.R")
data <- score_measures(measures_data = data,
                       measures_keywords = measures_keywords)
#__________________________________________________



#SAVE RESULTS -------------------------------------

#save results in tables at different levels of aggregation

#MEASURE
condensed_matched_HS <- matched_codes$HS_matches %>%
  group_by(measure_nr) %>%
  summarise(`Matched HS chapters` = paste0(`Tentative HS chapters match`, collapse=";"),
            `Absolute link strength (L_tilde)` = paste0(`Absolute link strength (L_tilde)`, collapse=";"),
            `Relative link strength (L_bar)` = paste0(`Relative link strength (L_bar)`, collapse=";"),.groups="drop")

final_m <- left_join(data, matched_codes$info_reported_codes, by="Nr")  #import info on reported codes
final_m <- left_join(final_m, condensed_matched_HS, by=c("Nr"="measure_nr") ) #import matched HS chapters
fwrite(final_m, "final output/Extended EDB (by measure).csv")

#MEASURE - COUNTRY
final_mc <- separate_rows(final_m, COUNTRIES, sep=";")  #split by country
fwrite(final_mc, "final output/Extended EDB (by measure-country).csv" )

#MEASURE - COUNTRY - HS chapter
final_mcs <- left_join(data, matched_codes$info_reported_codes, by="Nr")  #import info on reported codes
final_mcs <- full_join(final_mcs, matched_codes$HS_matches, by=c("Nr"="measure_nr"))
final_mcs <- separate_rows(final_mcs, COUNTRIES, sep=";")  #split by country
fwrite(final_mcs, "final output/Extended EDB (by measure-country-HS chapter).csv" )
#__________________________________________________




