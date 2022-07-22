# implement cleaning log

library(tidyverse)
library(lubridate)

# read the data
df_cleaning_log <- readxl::read_excel("inputs/20220720_combined_logic_spatial_and_others_checks.xlsx") %>% 
  filter(!adjust_log %in% c("delete_log"), !is.na(value), !is.na(uuid)) %>%  
  mutate(sheet = NA, index = NA, relevant = NA)
df_raw_data <- readxl::read_excel("inputs/REACH_UGA_Phone_inventory_tool_Finalized_Data.xlsx")
df_survey <- readxl::read_excel("inputs/REACH_UGA_Phone_inventory_tool_Finalized.xlsx", sheet = "survey") 
df_choices <- readxl::read_excel("inputs/REACH_UGA_Phone_inventory_tool_Finalized.xlsx", sheet = "choices") 

# find all new choices to add to choices sheet ----------------------------

# gather choice options based on unique choices list
df_grouped_choices<- df_choices %>% 
  group_by(list_name) %>% 
  summarise(choice_options = paste(name, collapse = " : "))
# get new name and ad_option pairs to add to the choices sheet
new_vars <- df_cleaning_log %>% 
  filter(type %in% c("change_response", "add_option")) %>% 
  left_join(df_survey, by = "name") %>% 
  filter(str_detect(string = type.y, pattern = "select_one|select one|select_multiple|select multiple")) %>% 
  separate(col = type.y, into = c("select_type", "list_name"), sep =" ", remove = TRUE, extra = "drop") %>% 
  left_join(df_grouped_choices, by = "list_name") %>%
  filter(!str_detect(string = choice_options, pattern = value ) ) %>%
  rename(choice = value ) %>%
  select(name, choice) %>%
  distinct() %>% # to make sure there are no duplicates
  arrange(name)


# create kobold object ----------------------------------------------------

kbo <- kobold::kobold(survey = df_survey, 
                      choices = df_choices, 
                      data = df_survey, 
                      cleaning = df_cleaning_log)

# modified choices for the survey tool --------------------------------------
df_choises_modified <- butteR:::xlsform_add_choices(kobold = kbo, new_choices = new_vars)

# special treat for variables for select_multiple, we need to add the columns to the data itself
df_survey_sm <- df_survey %>% 
  mutate(q_type = case_when(str_detect(string = type, pattern = "select_multiple|select multiple") ~ "sm",
                            str_detect(string = type, pattern = "select_one|select one") ~ "so",
                            TRUE ~ type)) %>% 
  filter(q_type=="sm") %>% 
  select(name, q_type)
# construct new columns for select multiple
new_vars_sm <- new_vars %>% 
  left_join(df_survey_sm, by = "name") %>% 
  mutate(new_cols=paste0(name,"/",choice))


# add new columns to the raw data -----------------------------------------

df_raw_data_modified <- df_raw_data %>% 
  butteR:::mutate_batch(nm = new_vars_sm$new_cols, value = F )

# make some cleanup -------------------------------------------------------
kbo_modified <- kobold::kobold(survey = df_survey, 
                               choices = df_choises_modified, 
                               data = df_raw_data_modified, 
                               cleaning = df_cleaning_log )
kbo_cleaned<- kobold::kobold_cleaner(kbo_modified)
  

df_cleaned_data <- kbo_cleaned$data %>% 
  mutate(
    usability_status = case_when(screen_touch == "yes" & battery == "yes" & internet == "yes" & maps_me_funct == "yes" & gps_function == "yes" ~ "good",
                                 TRUE ~ "bad")
  )

# write final modified data -----------------------------------------------------

write_csv(df_cleaned_data, file = paste0("outputs/", butteR::date_file_prefix(), "_clean_data.csv"))
