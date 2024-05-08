# You will use the data_download.R file to download your data from the Google
# Sheet that you have established for your capstone project. Use the googlesheets4
# package to access the data. Do not clean the data, but save it in the data/raw
# folder using the write_csv() function. Give the the a meaningful name that
# describes your project.

library(readr)
library(googlesheets4)

# Download survey results -------------------------------------------------

survey_WTW_waste_sep <- read_sheet("https://docs.google.com/spreadsheets/d/1x81KExjLqDFTdzK4o5LB8jCje8vmpPNKv_7iIMrF3l4/edit?usp=sharing")

write_csv(survey_WTW_waste_sep, "data/raw/01-WTW_waste_sep.csv")


# Download data dictionary ------------------------------------------------

# data_dictionary <- read_sheet("https://docs.google.com/spreadsheets/d/1LRp-MsZ1wtMXfVMmWwlQ_G4bnLvGPUh4nbI28j2DE8k/edit?usp=sharing")
# 
# write_csv(data_dictionary, "data/processed/00-data_dictionary.csv")