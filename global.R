# Copyright (C) 2023 ICF
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Affero General Public License for more details at
# <https://www.gnu.org/licenses/>. 

library(openxlsx)
library(lubridate)
library(bsicons)
library(colourpicker)
library(shinyjs)
library(stringr)
library(janitor)
library(bslib)
# library(thematic)
library(DT)
library(zip)
# library(archive)
library(tidyverse)
library(shinydashboard)
library(shiny)

options(shiny.maxRequestSize = 30*1024^2)

source("https://raw.githubusercontent.com/HUD-Data-Lab/DataLab/main/datalab_functions.R")
source("https://raw.githubusercontent.com/HUD-Data-Lab/DataLab/main/DataLab_Lists.R")


# source("https://raw.githubusercontent.com/abtassociates/eva/main/helper_functions.R")
logSessionData <- function() {return()}
logMetadata <- function() {return()}
evachecks <- read_csv("https://raw.githubusercontent.com/abtassociates/eva/9d4ba5c637134e7ebc72cfd337891feffe8b2a6b/public-resources/EvaChecks.csv",
                      show_col_types = FALSE)
cols_and_data_types <- read_csv("https://raw.githubusercontent.com/abtassociates/eva/main/public-resources/columns.csv", 
                                col_types = cols()) %>%
  filter(!(File %in% c("Affiliation",
                       "AssessmentResults",
                       "AssessmentQuestions")))
data_type_mapping <- c(
  character = "c", 
  numeric = "n", 
  date = "D",
  datetime = "T"
)
get_col_types <- function(file) {
  # get the column data types expected for the given file
  col_types <- cols_and_data_types %>%
    filter(File == file) %>%
    mutate(DataType = data_type_mapping[as.character(DataType)]) %>%
    pull(DataType) %>%
    paste0(collapse = "")
  return(col_types)
}

convert_list_1.8 <- function(column_name) {
  # replace_na(column_name, 99)
  names_for_1.8[match(column_name %>%
                        replace_na(99), values_for_1.8)]
}
names_for_1.8 <- c("Yes", "No", "Client doesn't know",
                   "Client prefers not to answer", "Data not collected")
values_for_1.8 <- c(1, 0, 8, 9, 99)

##  variable assignment
{
  homeless_situations <- c(1, 2, 16, 18)
  housed_situations <- c(3, 10, 11, 14, 19, 20, 21, 22, 23, 26, 28, 29, 31, 32, 33, 34, 35, 36)
  homeless_program_types <- c(0, 1, 2, 4, 8)
  housing_program_types <- c(3, 9, 10, 55, 13)
  file_count <- 9 
  service_list <- data.frame(
    RecordType = c("141", "142", "143", "144", "151", "152", "161", "200", 
                   "210", "300"),
    List = c("P1.2", "R14.2", "W1.2", "V2.2", "W2.2", "V3.3", "P2.2", "4.14",
             "V8.2", "C2.2")) %>%
    # left_join(read.xlsx("https://github.com/HUD-Data-Lab/DataLab/raw/main/CSV%20Specifications%20Machine-Readable_FY2024.xlsx", 
    # left_join(read.xlsx("https://files.hudexchange.info/resources/documents/HMIS-CSV-Machine-Readable-Specifications.xlsx", 
    left_join(read.xlsx("github.com/HUD-Data-Lab/DataLab/raw/main/Machine Readable Documentation/DRAFT - CSV Specifications Machine-Readable_FY2026.xlsx", 
                        sheet = "CSV Lists"), 
              by = "List") %>%
    mutate(Text = if_else(RecordType == 200, "Bed night", Text),
           Value = if_else(RecordType == 200, RecordType, Value)) %>%
    rename(TypeProvided = Value) %>%
    dplyr::mutate(ServiceType = case_when(
      str_detect(Text, fixed("outreach", ignore_case=TRUE)) |
        Text == "Bed night" ~ "Homeless",
      str_detect(Text, fixed("rental", ignore_case=TRUE)) |
        str_detect(Text, fixed("eviction prevention", ignore_case=TRUE)) |
        str_detect(Text, fixed("security deposit", ignore_case=TRUE))~ "Housed"
    ))
}

bnl_table <- function(provided_data,
                      ces_color, shelter_color, housing_color) {
  DT::datatable(
    provided_data %>%
      `colnames<-`( gsub("([a-z])([A-Z])", "\\1 \\2", colnames(provided_data))),
    options = list(
      pageLength = 50,
      columnDefs = list(list(targets = 1:3, visible = FALSE)),
      initComplete = JS(
        "function(settings, json) {",
        "$('th').css({'text-align': 'center'});",
        "$('td').css({'text-align': 'center'});",
        "}")),
    selection = "single",
    rownames = FALSE) %>%
    formatStyle("Personal ID", `text-align` = 'center') %>% 
    formatStyle(
      'Personal ID', 'In CES',
      backgroundColor = styleEqual(1, ces_color)) %>% 
    formatStyle(
      'Personal ID', 'Sheltered',
      backgroundColor = styleEqual(1, shelter_color)) %>% 
    formatStyle(
      'Personal ID', 'In Housing Program',
      backgroundColor = styleEqual(1, housing_color))
}

event_table <- function(provided_data) {
  DT::datatable(
    provided_data %>%
      `colnames<-`(gsub("([a-z])([A-Z])", "\\1 \\2", colnames(provided_data))) %>%
      select(c("Personal ID", "Effective Date", "Event Type", "Information Source", "before_inactive_date"))
    ,
    options = list(
      pageLength = 5,
      columnDefs = list(list(targets = 4, visible = FALSE))
    ),
    rownames = FALSE) %>%
    formatStyle(
      c("Personal ID", "Effective Date", "Event Type", "Information Source"),
      'before_inactive_date',
      # target = 'row',
      # backgroundColor = styleEqual(c(0, 1), c('White', 'WhiteSmoke'))
      fontWeight = styleEqual(c(0, 1), c('bold', 'normal'))
    )
}

