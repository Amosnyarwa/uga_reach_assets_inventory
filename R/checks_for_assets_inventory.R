# script: logical and others checks

library(tidyverse)
library(lubridate)
library(glue)
source("R/checks_for_other_responses.R")

# read data

df_assets_data <- readxl::read_excel("inputs/REACH_UGA_Phone_inventory_tool_Finalized_Data.xlsx") %>% 
  mutate(i.check.uuid = `_uuid`,
         i.check.start_date = as_date(start),
         i.check.gps_precision = `_geopoint_precision`,
         i.check.start = as_datetime(start),
         i.check.end = as_datetime(end))
  

df_survey <- readxl::read_excel("inputs/REACH_UGA_Phone_inventory_tool_Finalized.xlsx", sheet = "survey")
df_choices <- readxl::read_excel("inputs/REACH_UGA_Phone_inventory_tool_Finalized.xlsx", sheet = "choices")


# output holder -----------------------------------------------------------

logic_output <- list()


# GPS precision checks ----------------------------------------------------

df_gps_function <- df_assets_data %>%
  filter(`_geopoint_precision` < 10, gps_function == "no") %>%
  mutate(i.check.type = "change_response",
         i.check.name = "gps_function",
         i.check.current_value = as.character(gps_function),
         i.check.value = "",
         i.check.issue_id = "logic_m_requirement_wrong_gps_function",
         i.check.issue = "wrong_gps_function",
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "1",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_gps_function")){
  if(nrow(df_gps_function) > 0){
    logic_output$df_gps_function <- df_gps_function
  }
}
    
# combine checks ----------------------------------------------------------

df_logic_checks <- bind_rows(logic_output)

# others checks

df_other_response_data <- extract_other_data(input_assets_data = df_assets_data, input_survey = df_survey, input_choices = df_choices)


# combine logic and others checks
df_combined_checks <- bind_rows(df_logic_checks, df_other_response_data)

# output the resulting data frame
write_csv(x = df_combined_checks, file = paste0("outputs/", butteR::date_file_prefix(), "_combined_logic_spatial_and_others_checks.csv"), na = "")

    
  
  



