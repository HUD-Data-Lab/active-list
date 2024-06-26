# Copyright (C) 2022 Gwen Beebe
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


library(shiny)
library(shinydashboard)
library(shinyWidgets)
# library(dashboardthemes)
library(bslib)
library(tidyverse)
library(lubridate)
library(DT)
library(shinydashboardPlus)
library(colourpicker)

options(shiny.maxRequestSize = 30*1024^2)
`%nin%` = Negate(`%in%`)

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

hud_service_data <- 
  # read.csv("https://raw.githubusercontent.com/gwenbeebe/CHIP_HMIS/main/Publishing/NHSDC_ByNameList/SupplementalData_ServiceGroups.csv") %>%
  # left_join(read.csv("https://raw.githubusercontent.com/gwenbeebe/CHIP_HMIS/main/Publishing/NHSDC_ByNameList/SupplementalData_Services.csv"), 
  read.csv("C:/Users/57695/OneDrive - ICF/ICF Homeless Services Team/HCC/Code/active-list/SupplementalData_ServiceGroups.csv") %>%
  left_join(read.csv("C:/Users/57695/OneDrive - ICF/ICF Homeless Services Team/HCC/Code/active-list/SupplementalData_Services.csv"),
            by = "RecordType") %>%
  dplyr::mutate(ServiceType = case_when(
    str_detect(Description, fixed("outreach", ignore_case=TRUE)) |
      Description == "Bed night" ~ "Homeless",
    str_detect(Description, fixed("rental", ignore_case=TRUE)) |
      str_detect(Description, fixed("eviction prevention", ignore_case=TRUE)) |
      str_detect(Description, fixed("security deposit", ignore_case=TRUE))~ "Housed"
  ))

##  variable assignment
{
  homeless_situations <- c(1, 2, 16, 18)
  housed_situations <- c(3, 10, 11, 14, 19, 20, 21, 22, 23, 26, 28, 29, 31, 32, 33, 34, 35, 36)
  homeless_program_types <- c(1, 2, 4, 8)
  housing_program_types <- c(3, 9, 10, 55, 13)
  file_count <- 9 
  }