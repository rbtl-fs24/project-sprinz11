# You will use the data_cleaning.R file to process your data into a state that
# is ready for analysis. Do not access the data from Google Drive, but rather
# use the read_csv() function to access your data from the data/raw folder. Save
# the processed, analysis-ready data in the data/processed folder using the
# write_csv() function. Again, give the file a meaningful name that describes
# your project.


# Setup -------------------------------------------------------------------

library(tidyverse)


# Import raw data ---------------------------------------------------------

raw_data <- read_csv("data/raw/01-WTW_waste_sep.csv")
# data_dictionary <- read_csv("data/processed/00-data_dictionary.csv")

# Data cleaning -----------------------------------------------------------

preprocess <- raw_data |> 
  janitor::clean_names()

consent_col_name <- "your_responses_to_this_questionnaire_may_be_used_for_research_purposes_and_will_be_stored_anonymously_without_any_data_being_stored_that_can_be_associated_with_you_as_an_individual"

count(preprocess, !!sym(consent_col_name))

preprocess <-  preprocess |> 
  filter(!str_detect(!!sym(consent_col_name), "I do not agree")) |> # https://www.perplexity.ai/search/Using-the-filter-EamBK_pYRmucZrXnpBhYDQ
  select(-c("timestamp", !!sym(consent_col_name)))

col_names_wide = c("id", "institution", "level_of_studies", "main_campus", "avail_residual",
                   "avail_organic", "avail_pet", "avail_plastics", "avail_paper", "avail_cans",
                   "avail_glass", "avail_batteries", "avail_electronic", "avail_storage_media","avail_metal",
                   "avail_wood", "avail_liquids", "avail_cartridges", "avail_coffee_capsules",
                   "avail_textiles", "add_types", "wtw_organic", "wtw_pet", "wtw_plastics",
                   "wtw_paper", "wtw_cans", "wtw_glass", "wtw_batteries", "wtw_electronic",
                   "wtw_storage_media", "wtw_metal", "wtw_wood", "wtw_liquids", "wtw_cartridges",
                   "wtw_coffee_capsules", "wtw_textiles", "improvements")

data_wide <- preprocess |> 
  mutate(id = seq(1:n()), .before = starts_with("what_institution")) |> 
  rename_all(~ col_names_wide)


# I know the following code is not necessary because the long names are attached
# further below again. But I decided to leave it in for learning reasons.
data_wide <- data_wide |> 
  mutate(across(starts_with("avail_"), ~ case_when(
    . == "I know where the nearest bin is located." ~ "nb",
    . == "I know it exists but I am not sure where." ~ "ebw",
    . == "I do not know of any but I would like for it to be available." ~ "wl",
    . == "I do not know any and I would not use it." ~ "nu",
    . == "No answer" ~ "noa"
  ))) |> 
  mutate(across(starts_with("wtw_"), ~ case_when(
    . == "In the room I am currently in" ~ "cr",
    . == "Within the same building" ~ "sb",
    . == "The building next door" ~ "nb",
    . == "Anywhere on the campus" ~ "ac",
    . == "Off campus" ~ "oc",
    . == "No answer" ~ "noa"
  )))            # https://www.perplexity.ai/search/In-an-R-zSpzF9tWTUOpT8NO0PdLAA


processed_open_answers <- data_wide |> 
  select(-starts_with("avail"), -starts_with("wtw")) |> 
  filter(!(is.na(add_types) & is.na(improvements)))


# Pivot wide to long ------------------------------------------------------

#data_long <- data_wide |> 
#  pivot_longer(cols = starts_with("avail"),
#               names_to = "avail_waste_type",
#               values_to = "avail_resp_short") |> 
#  pivot_longer(cols = starts_with("wtw"),
#               names_to = "wtw_waste_type",
#               values_to = "wtw_resp_short")

## Is there a more elegant way to do this? -->

data_long_avail <- data_wide |> 
  select(-starts_with("wtw")) |> 
  pivot_longer(cols = starts_with("avail"),
               names_to = "waste_type",
               values_to = "avail_resp_short") |> 
  mutate(waste_type = str_remove(waste_type, pattern = "avail_"))

data_long_wtw <- data_wide |> 
  select(-starts_with("avail")) |> 
  mutate(wtw_residual = NA,
         .before = wtw_organic) |> 
  pivot_longer(cols = starts_with("wtw"),
               names_to = "waste_type",
               values_to = "wtw_resp_short") |> 
  mutate(waste_type = str_remove(waste_type, pattern = "wtw_"))

data_long <- left_join(data_long_avail, data_long_wtw) |> 
  select(-add_types, -improvements)

data_long <- data_long |> 
  mutate(avail_resp_long = case_when(avail_resp_short == "nb" ~ "I know where the nearest bin is located.",
                                     avail_resp_short == "ebw" ~ "I know it exists but I am not sure where.",
                                     avail_resp_short == "wl" ~ "I do not know of any but I would like for it to be available.",
                                     avail_resp_short == "nu" ~ "I do not know any and I would not use it.",
                                     avail_resp_short == "noa" ~ "No answer"),
         .after = avail_resp_short) |> 
  mutate(wtw_resp_long = case_when(wtw_resp_short == "cr" ~ "In the room I am currently in",
                                   wtw_resp_short == "sb" ~ "Within the same building",
                                   wtw_resp_short == "nb" ~ "The building next door",
                                   wtw_resp_short == "ac" ~ "Anywhere on the campus",
                                   wtw_resp_short == "oc" ~ "Off campus",
                                   wtw_resp_short == "noa" ~ "No answer"),
         .after = wtw_resp_short)


lvl_studies <- c("BSc", "MSc", "PhD", "Other", "Prefer not to say")
lvl_avail <- c("nb", "ebw", "wl", "nu", "noa")
lvl_wtw <- c("cr", "sb", "nb", "ac", "oc", "noa")

data_long_fct <- data_long |> 
  mutate(level_of_studies = factor(level_of_studies, levels = lvl_studies)) |> 
  mutate(avail_resp_short = factor(avail_resp_short, levels = lvl_avail)) |> 
  mutate(wtw_resp_short = factor(wtw_resp_short, levels = lvl_wtw))

processed_data <- data_long_fct

# Export processed data ---------------------------------------------------

write_rds(processed_data, "data/processed/01-data_avail_wtw.rds")

write_rds(processed_open_answers, "data/processed/02-open_answers.rds")
