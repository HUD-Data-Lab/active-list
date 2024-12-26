# Copyright (C) 2024 ICF
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


function(input, output, session) {
  
  get_statuses <- function(event_list) {
    
    event_list %>%
      select(PersonalID, EffectiveDate, ClientStatus, before_inactive_date) %>%
      group_by(PersonalID) %>%
      dplyr::mutate(PriorDate = dplyr::lead(EffectiveDate),
                    PriorStatus = dplyr::lead(ClientStatus),
                    HomelessPrior90 = ClientStatus == "Homeless" &
                      PriorStatus == "Homeless" &
                      EffectiveDate - ddays(input$days_to_inactive) <= PriorDate &
                      !is.na(PriorStatus),
                    IdentificationDate = suppressWarnings(max(case_when(
                      ClientStatus == "Homeless" &
                        !HomelessPrior90 ~ EffectiveDate), na.rm = TRUE)),
                    HomelessEventInPeriod = suppressWarnings(max(
                      ClientStatus == "Homeless" &
                        before_inactive_date == 0, na.rm = TRUE)),
                    HomelessEventBeforePeriod = suppressWarnings(max(
                      ClientStatus == "Homeless" &
                        before_inactive_date == 1, na.rm = TRUE)),
                    HousedBefore = suppressWarnings(max(
                      ClientStatus == "Homeless" &
                        PriorStatus == "Housed" &
                        before_inactive_date == 0, na.rm = TRUE))) %>%
      slice(1L) %>%
      ungroup() %>%
      mutate(CurrentStatus = case_when(
        ClientStatus == "Housed" ~ "Housed",
        HomelessEventInPeriod &
          !HomelessEventBeforePeriod ~ "New to List",
        !HomelessEventInPeriod ~ "Inactive",
        HousedBefore == 1 ~ "Return From Housed",
        IdentificationDate >= effective_date() - ddays(input$days_to_inactive) ~ "Return From Inactive",
        TRUE ~ "Active"
      )) %>%
      filter(CurrentStatus %nin% c("Housed", "Inactive")) %>%
      select(PersonalID, CurrentStatus, IdentificationDate) %>%
      left_join(client_information(),
                by = "PersonalID") %>%
      select(c(PersonalID, InHousingProgram, Sheltered, InCES,
               CurrentStatus, IdentificationDate,
               setdiff(colnames(client_information()), "PersonalID")))
  }
  
  valid_file <- reactiveVal(0)
  # file_list <- reactiveValues()
  # file_list$file <- list()
  # rv <- reactiveValues()
  
  notify <- function(msg, id = NULL) {
    showNotification(msg, id = id, duration = NULL, closeButton = FALSE)
  }
  
  importFile <- function(csvFile, guess_max = 1000) {
    filename = str_glue("{csvFile}.csv")
    data <- read_csv(utils::unzip(zipfile = input$imported$datapath, files = filename)
                     ,col_types = get_col_types(csvFile)
                     ,na = ""
    )
    file.remove(filename)
    return(data)
  }
  
  observeEvent(input$timeOut, {
    reset("imported")
    session$reload()
  })
  
  csv_files <- reactive({
    if (is.null(input$imported)) {
      return ()
    }
    # observeEvent(input$imported, {
    valid_file(0)
    csv_files <- list()
    source("https://raw.githubusercontent.com/HUD-Data-Lab/DataLab/main/local_initially_valid_import.R", 
           local = TRUE)
    # extract file names from their uploaded zip
    if(tolower(tools::file_ext(input$imported$datapath)) != "zip") {
      show_invalid_popup(127)
      print("Unsuccessful upload - zip file not .zip")
    } else {
      
      zipContents <- utils::unzip(zipfile = input$imported$datapath, list=TRUE)
      
      zipFiles <- zipContents$Name %>% str_replace(".csv", "")
      
      # expected files
      expected_files <- unique(cols_and_data_types$File)
      
      # get missing files by comparing what we expect with what we got
      missing_files <- expected_files[!(expected_files %in% zipFiles)]
      
      ### Now check whether the file is hashed, has the expected structure, and contains
      # the expected csv files
      if(grepl("/", zipContents$Name[1])) {
        show_invalid_popup(122)
        # logMetadata("Unsuccessful upload - zip file was misstructured")
      } else if("Export" %in% missing_files) {
        show_invalid_popup(123)
        # logMetadata("Unsuccessful upload - not an HMIS CSV Export")
      } else if(!isFY2024Export()) {
        show_invalid_popup(124)
        # logMetadata("Unsuccessful upload - out of date HMIS CSV Export")
      } else if(length(missing_files)) {
        evachecks <- evachecks %>% filter(ID == 125) %>% 
          mutate(Guidance = HTML(str_glue(
            "Your zip file appears to be missing the following files:<br/><br/>
      
        {paste(missing_files, collapse = ', ')}<br/><br/>
        
        You either uploaded something other than an HMIS CSV export or your export 
        does not contain all the files outlined in the HMIS CSV Export specifications.
        If you are not sure how to run the hashed HMIS CSV Export in your HMIS,
        please contact your HMIS vendor."))
          )
        show_invalid_popup(125)
        # logMetadata("Unsuccessful upload - incomplete dataset")
      } else if(!is_hashed()) {
        ## reset before publishing!!
        # show_invalid_popup(126)
        initially_valid_import <- 1
        # logMetadata("Unsuccessful upload - not hashed")
      } 
    }
    
    if(initially_valid_import == 1) {
      
      withProgress({
        setProgress(message = "Processing...", value = .15)
        setProgress(detail = "Reading your files..", value = .2)})
      
      for (file in unique(cols_and_data_types$File)) {
        # print(file)
        #import the csv and save it as a data frame
        assign(file, importFile(file))
        # csv_files <- c(csv_files, list(get(file)))
      }
    }
    default_report_start_date <- Export %>% pull(ExportStartDate)
    default_report_end_date <- Export %>% pull(ExportEndDate)
    
    # rv$Enrollment <- Enrollment
    # valid_file(1)
    # choices <<- Organization$OrganizationName
    # list(Organization = Organization, Client = Client)
    # file_list$file$Organization <- Organization
    list(Client = Client, 
         CurrentLivingSituation = CurrentLivingSituation,
         Enrollment = Enrollment,
         Event = Event,
         Exit = Exit,
         Funder = Funder,
         Organization = Organization,
         Project = Project,
         Services = Services,
         IncomeBenefits = IncomeBenefits,
         Disabilities = Disabilities,
         HealthAndDV = HealthAndDV,
         default_report_start_date = default_report_start_date,
         default_report_end_date = default_report_end_date)
    # Organization
    # names(csv_files) <- unique(cols_and_data_types$File)
    # csv_files
  })
  ##########################
  
  output$report_date_picker <- renderUI({
    if(is.null(input$imported)){return ()}
    dateInput("report_date_picker",
              "Effective Date",
              value = csv_files()$default_report_end_date,
              min = csv_files()$default_report_start_date + ddays(input$days_to_inactive),
              max = csv_files()$default_report_end_date
              )
  })
  
  effective_date <- reactive({
    if (is.null(input$imported)) {return ()}
    else if (is.null(input$report_date_picker)) {csv_files()$default_report_end_date}
    else {input$report_date_picker}
  })
  
  joined_enrollments <- reactive({
    if(is.null(input$imported)){return ()}
    
    csv_files()$Enrollment %>%
      filter(EntryDate <= effective_date()) %>%
      select(EnrollmentID, PersonalID, EntryDate, HouseholdID, RelationshipToHoH, ProjectID,
             LivingSituation, DateToStreetESSH, DisablingCondition, TimesHomelessPastThreeYears,
             MonthsHomelessPastThreeYears, MoveInDate) %>%
      left_join(csv_files()$Project %>%
                  select(ProjectID, ProjectType, ProjectName, OrganizationID), by = "ProjectID") %>%
      left_join(csv_files()$Organization %>%
                  select(OrganizationID, OrganizationName), by = "OrganizationID") %>%
      left_join(csv_files()$Exit %>%
                  select(EnrollmentID, ExitDate, Destination), by = "EnrollmentID") %>%
      left_join(csv_files()$HealthAndDV %>%
                  filter(DataCollectionStage == 1) %>%
                  select(EnrollmentID, CurrentlyFleeing), by = "EnrollmentID") %>%
      dplyr::mutate(EntryDate = ymd(EntryDate),
                    MoveInDate = ymd(MoveInDate),
                    ExitDate = ymd(ExitDate),
                    LivingSituation = floor(LivingSituation / 100))
  })
  
  client_information <- reactive({
    if(is.null(input$imported)){return ()}
    
    base_client_data <- csv_files()$Client %>%
      # mutate(Name = paste(str_to_title(FirstName), str_to_title(LastName))) %>%
      # select(-c(FirstName, LastName, DOB, VeteranStatus)) %>%
      select(PersonalID) %>%
      left_join(joined_enrollments() %>%
                  filter(is.na(ExitDate)) %>%
                  group_by(PersonalID) %>%
                  summarise(ActiveEnrollments = paste(unique(ProjectName), collapse=", "),
                            InHousingProgram = max(if_else(ProjectType %in% housing_program_types, 1, 0),  na.rm = TRUE),
                            Sheltered = max(if_else(ProjectType %in% c(0, 1, 2, 8), 1, 0),  na.rm = TRUE),
                            InCES = max(if_else(ProjectType == 14, 1, 0),  na.rm = TRUE)),
                by = "PersonalID")
    
    # if(is.null(input$additional_client_info)){
      return (base_client_data)
    #   }
    # else {return(base_client_data %>%
    #                left_join(read_csv(input$additional_client_info$datapath) %>%
    #                            distinct(PersonalID, .keep_all = TRUE) %>%
    #                            mutate(PersonalID = as.character(PersonalID)),
    #                          by = "PersonalID"))}
  })
  
  ########################
  
  ##  run event calculations
  
  ##  current living situation events
  cls_events <- reactive({
    if(is.null(input$imported)){return ()}
    
    csv_files()$CurrentLivingSituation %>%
      mutate(CurrentLivingSituation = floor(CurrentLivingSituation / 100)) %>%
      filter(CurrentLivingSituation %in% c(1, 4)) %>%
      left_join(joined_enrollments() %>%
                  select(EnrollmentID, ProjectName), by = "EnrollmentID") %>%
      dplyr::mutate(
        ClientStatus = case_when(
          CurrentLivingSituation == 1 ~ "Homeless",
          CurrentLivingSituation == 4 ~ "Housed"),
        EventType = case_when(
          CurrentLivingSituation == 1 ~ "Literally Homeless CLS",
          CurrentLivingSituation == 4 ~ "Housed CLS"),
        InformationSource = if_else(is.na(VerifiedBy), ProjectName, VerifiedBy)) %>%
      rename(EffectiveDate = InformationDate) %>%
      select(PersonalID, EffectiveDate, ClientStatus, EventType, InformationSource)
  })
  
  ##  homeless enrollment events
  homeless_enrollment_events <- reactive({
    if(is.null(input$imported)){return ()}
    
    joined_enrollments() %>%
      filter(LivingSituation == 1 |
               CurrentlyFleeing == 1 |
               ProjectType %in% homeless_program_types) %>%
      dplyr::mutate(ClientStatus = "Homeless",
                    EventType = "Literally Homeless Enrollment") %>%
      rename(EffectiveDate = EntryDate,
             InformationSource = ProjectName) %>%
      select(PersonalID, EffectiveDate, ClientStatus, EventType, InformationSource)
  })
  
  ##  housing move-in date events
  move_in_date_events <- reactive({
    if(is.null(input$imported)){return ()}
    
    joined_enrollments() %>%
      filter(MoveInDate >= csv_files()$default_report_start_date &
               MoveInDate <= effective_date() &
               ProjectType %in% housing_program_types) %>%
      dplyr::mutate(ClientStatus = "Housed",
                    EventType = "Housing Move-In Date") %>%
      rename(EffectiveDate = MoveInDate,
             InformationSource = ProjectName) %>%
      select(PersonalID, EffectiveDate, ClientStatus, EventType, InformationSource)
  })
  
  ##  service events
  service_events <- reactive({
    if(is.null(input$imported)){return ()}
    
    service_list %>%
      inner_join(csv_files()$Services %>%
                   mutate(RecordType = as.character(RecordType),
                          TypeProvided = as.character(TypeProvided)) %>%
                   select(PersonalID, EnrollmentID, DateProvided, RecordType, TypeProvided) %>%
                   filter(DateProvided >= csv_files()$default_report_start_date),
                  by = c("RecordType", "TypeProvided")) %>%
      left_join(joined_enrollments() %>%
                  select(EnrollmentID, ProjectName, ProjectType), by = "EnrollmentID") %>%
      filter(!is.na(ServiceType) |
               ProjectType %in% homeless_program_types) %>%
      dplyr::mutate(
        EventType = case_when(
          ServiceType == "Housed" ~ "Rent or Deposit Service",
          ServiceType == "Homeless" ~ "Outreach Contact Service",
          TRUE ~ "Service From Homeless-Only Program"),
        ClientStatus = if_else(is.na(ServiceType), "Homeless", ServiceType)) %>%
      rename(EffectiveDate = DateProvided,
             InformationSource = ProjectName)%>%
      select(PersonalID, EffectiveDate, ClientStatus, EventType, InformationSource)
  })
  
  ##  exits/residence events
  other_enrollment_events <- reactive({
    if(is.null(input$imported)){return ()}
    
    joined_enrollments() %>%
      filter(
        (is.na(ExitDate) &
           ProjectType %in% c(1, 2, 8, housing_program_types)) |
          Destination %in% c(1, 4)) %>%
      dplyr::mutate(EffectiveDate = if_else(is.na(ExitDate), effective_date(), ExitDate),
                    ClientStatus = case_when(
                      Destination == 4 |
                        (ProjectType %in% housing_program_types &
                           is.na(ExitDate) &
                           MoveInDate <= effective_date()) ~ "Housed",
                      TRUE ~ "Homeless"),
                    EventType = case_when(
                      Destination == 1 ~ "Homeless Exit From Program",
                      Destination == 4 ~ "Housed Exit From Program",
                      TRUE ~ "Still Enrolled In Program")) %>%
      rename(InformationSource = ProjectName) %>%
      select(PersonalID, EffectiveDate, ClientStatus, EventType, InformationSource)
  })
  
  all_events <- reactive({
    if(is.null(input$imported)){return ()}
    id <- notify("Combining everything you uploaded...")
    on.exit(removeNotification(id), add = TRUE)
    
    cls_events() %>%
      union(homeless_enrollment_events()) %>%
      union(move_in_date_events()) %>%
      union(service_events()) %>%
      union(other_enrollment_events()) %>%
      filter(EffectiveDate >= csv_files()$default_report_start_date &
               EffectiveDate <= effective_date()) %>%
      arrange(PersonalID, desc(EffectiveDate), desc(ClientStatus)) %>%
      distinct(PersonalID, EffectiveDate, .keep_all = TRUE) %>%
      mutate(before_inactive_date = 
               if_else(EffectiveDate < effective_date() - ddays(input$days_to_inactive), 1, 0))
  })
  
  client_statuses <- reactive({
    if(is.null(input$imported)){return ()}
    id <- notify("Determining statuses for every person...")
    on.exit(removeNotification(id), add = TRUE)
    
    get_statuses(all_events())
  }) 
  
  recent_enrollments <- reactive({
    if(is.null(input$imported)){return ()}
    csv_files()$Enrollment %>%
      filter(EntryDate <= effective_date()) %>%
      arrange(desc(EntryDate)) %>%
      group_by(PersonalID) %>%
      slice(1:3) %>%
      ungroup() 
  })
  
  chronic_folks <- reactive({
    if(is.null(input$imported)){return ()}
    id <- notify("Finding chronic folx...")
    on.exit(removeNotification(id), add = TRUE)
    
    recent_enrollments() %>%
      inner_join(csv_files()$Client %>%
                   filter(DOB <= effective_date() - years(18)) %>%
                   select(PersonalID), by = "PersonalID") %>%
      filter(DisablingCondition == 1) %>%
      dplyr::mutate(SinglyChronic =
                      if_else(((ymd(DateToStreetESSH) + days(365) <= ymd(EntryDate) &
                                  !is.na(DateToStreetESSH)) |
                                 (
                                   MonthsHomelessPastThreeYears %in% c(112, 113) &
                                     TimesHomelessPastThreeYears == 4 &
                                     !is.na(MonthsHomelessPastThreeYears) &
                                     !is.na(TimesHomelessPastThreeYears)
                                 )
                      ), 1, 0)) %>%
      select(PersonalID, SinglyChronic) %>%
      group_by(PersonalID) %>%
      summarise(SinglyChronic = max(SinglyChronic)) %>%
      filter(SinglyChronic == 1)
  })
  
  youth <- reactive({
    if(is.null(input$imported)){return ()}
    id <- notify("Finding youth...")
    on.exit(removeNotification(id), add = TRUE)
    
    reunified <- recent_enrollments() %>%
      filter(EntryDate >= effective_date() - ddays(input$days_to_inactive)) %>%
      group_by(PersonalID) %>%
      slice(1L) %>%
      ungroup() %>%
      filter(RelationshipToHoH == 2)
    
    recent_enrollments() %>%
      inner_join(csv_files()$Client %>%
                   mutate(Age = trunc((DOB %--% effective_date()) / years(1))) %>%
                   # filter(DOB <= effective_date() - years(12) &
                   #          DOB > effective_date() - years(25)) %>%
                   filter(Age >= 12 & Age < 25), 
                 by = "PersonalID") %>%
      filter(RelationshipToHoH %in% c(1, 3) 
             & PersonalID %nin% reunified$PersonalID) %>%
      # dplyr::mutate(YouthFlag = 1) %>%
      # select(PersonalID, YouthFlag) %>%
      select(PersonalID, Age) %>%
      group_by(PersonalID) %>%
      slice(1L) %>%
      ungroup()
    

  })
  
  family_sizes <- reactive({
    if(is.null(input$imported)){return ()}
    id <- notify("Calculating family sizes...")
    on.exit(removeNotification(id), add = TRUE)
    
    csv_files()$Enrollment %>%
      inner_join(csv_files()$Enrollment %>%
                   filter(EntryDate <= effective_date()) %>%
                   distinct(PersonalID, HouseholdID, .keep_all = TRUE) %>%
                   inner_join(csv_files()$Client %>%
                                filter(DOB > effective_date() - years(18)) %>%
                                select(PersonalID), by = "PersonalID") %>%
                   group_by(HouseholdID) %>%
                   summarise(NumberOfChildren = n()) %>%
                   filter(NumberOfChildren > 0),
                 by = "HouseholdID") %>%
      distinct(PersonalID, HouseholdID, .keep_all = TRUE) %>%
      group_by(HouseholdID, NumberOfChildren) %>% 
      summarise(HouseholdSize = n()) %>%
      filter(NumberOfChildren < HouseholdSize)
  })
  
  families <- reactive({
    if(is.null(input$imported)){return ()}
    recent_enrollments() %>%
      inner_join(family_sizes(), by = "HouseholdID") %>%
      filter(RelationshipToHoH == 1) %>%
      arrange(desc(EntryDate)) %>%
      select(PersonalID, NumberOfChildren, HouseholdSize) %>%
      group_by(PersonalID) %>%
      slice(1L) %>%
      ungroup()
  })
  
  #########################
  output$effective_date_v <- renderUI({
    h4(
      if(is.null(input$imported)){"No data uploaded"}
      else{
        paste0(
          "Effective ", format(effective_date(), "%m-%d-%Y"), " (", input$days_to_inactive, " days to inactive)"
        )})
  })
  
  vet_statuses <- reactive({
    if(is.null(input$imported)){return ()}
    client_statuses() %>%
      inner_join(csv_files()$Client %>%
                   filter(VeteranStatus == 1) %>%
                   select(PersonalID), by = "PersonalID") %>%
      mutate(PersonalID = as.integer(PersonalID),
             IdentificationDate = ymd(IdentificationDate)) %>%
      arrange(PersonalID) 
  })
  
  output$VBNL_active <- renderText({
      if(is.null(input$imported)){"---"}
      else{nrow(vet_statuses() %>%
                  filter(CurrentStatus == "Active"))}
    })
  
  output$VBNL_newly <- renderText({
    if(is.null(input$imported)){"---"}
    else{nrow(vet_statuses() %>%
                filter(CurrentStatus == "New to List"))}
  })
  
  output$VBNL_return_h <- renderText({
    if(is.null(input$imported)){"---"}
    else{nrow(vet_statuses() %>%
                filter(CurrentStatus == "Return From Housed"))}
  })
  
  output$VBNL_return_i <- renderText({
    if(is.null(input$imported)){"---"}
    else{nrow(vet_statuses() %>%
                filter(CurrentStatus == "Return From Inactive"))}
  })
  
  output$veteran_by_name_list <- renderDataTable({
    if(is.null(input$imported)){return ()}
    bnl_table(vet_statuses(),
              input$ces_color, input$shelter_color, input$housing_color)
  })
  
  VBNL_events <- reactive({
    all_events()[which(vet_statuses()[[input$veteran_by_name_list_rows_selected,1]]==all_events()$PersonalID),]
  })
  
  observeEvent(input$veteran_by_name_list_rows_selected,{
    showModal(
      modalDialog(
        renderDataTable({
          event_table(VBNL_events())
        })
      ))
  })
  
  #########################
  
  output$effective_date_c <- renderUI({
    h4(
      if(is.null(input$imported)){"No data uploaded"}
      else{
        paste0(
          "Effective ", format(effective_date(), "%m-%d-%Y"), " (", input$days_to_inactive, " days to inactive)"
        )})
  })
  
  chronic_statuses <- reactive({
    if(is.null(input$imported)){return ()}
    client_statuses() %>%
      inner_join(chronic_folks() %>%
                   select(PersonalID), by = "PersonalID") %>%
      mutate(PersonalID = as.integer(PersonalID),
             IdentificationDate = ymd(IdentificationDate)) %>%
      arrange(PersonalID)
  })
  
  output$CBNL_active <- renderText({
    if(is.null(input$imported)){"---"}
    else{nrow(chronic_statuses() %>%
                filter(CurrentStatus == "Active"))}
  })
  
  output$CBNL_newly <- renderText({
    if(is.null(input$imported)){"---"}
    else{nrow(chronic_statuses() %>%
                filter(CurrentStatus == "New to List"))}
  })
  
  output$CBNL_return_h <- renderText({
    if(is.null(input$imported)){"---"}
    else{nrow(chronic_statuses() %>%
                filter(CurrentStatus == "Return From Housed"))}
  })
  
  output$CBNL_return_i <- renderText({
    if(is.null(input$imported)){"---"}
    else{nrow(chronic_statuses() %>%
                filter(CurrentStatus == "Return From Inactive"))}
  })
    
  output$chronic_by_name_list <- renderDataTable({
    if(is.null(input$imported)){return ()}
    bnl_table(chronic_statuses(),
              input$ces_color, input$shelter_color, input$housing_color)
  })
  
  CBNL_events <- reactive({
    all_events()[which(chronic_statuses()[[input$chronic_by_name_list_rows_selected,1]]==all_events()$PersonalID),]
  })
  
  observeEvent(input$chronic_by_name_list_rows_selected,{
    showModal(
      modalDialog(
        renderDataTable({
          event_table(CBNL_events())
        })
      ))
  })
  
  #########################
  
  output$effective_date_y <- renderUI({
    h4(
      if(is.null(input$imported)){"No data uploaded"}
      else{
        paste0(
          "Effective ", format(effective_date(), "%m-%d-%Y"), " (", input$days_to_inactive, " days to inactive)"
        )})
  })
  
  youth_statuses <- reactive({
    if(is.null(input$imported)){return ()}
    client_statuses() %>%
      inner_join(youth(), 
                 by = "PersonalID") %>%
      mutate(PersonalID = as.integer(PersonalID),
             IdentificationDate = ymd(IdentificationDate)) %>%
      arrange(PersonalID)
  })
  
  output$YBNL_active <- renderText({
    if(is.null(input$imported)){"---"}
    else{nrow(youth_statuses() %>%
                filter(CurrentStatus == "Active"))}
  })
  
  output$YBNL_newly <- renderText({
    if(is.null(input$imported)){"---"}
    else{nrow(youth_statuses() %>%
                filter(CurrentStatus == "New to List"))}
  })
  
  output$YBNL_return_h <- renderText({
    if(is.null(input$imported)){"---"}
    else{nrow(youth_statuses() %>%
                filter(CurrentStatus == "Return From Housed"))}
  })
  
  output$YBNL_return_i <- renderText({
    if(is.null(input$imported)){"---"}
    else{nrow(youth_statuses() %>%
                filter(CurrentStatus == "Return From Inactive"))}
  })
  
  output$youth_by_name_list <- renderDataTable({
    if(is.null(input$imported)){return ()}
    bnl_table(youth_statuses(),
              input$ces_color, input$shelter_color, input$housing_color)
  })
  
  YBNL_events <- reactive({
    all_events()[which(youth_statuses()[[input$youth_by_name_list_rows_selected,1]]==all_events()$PersonalID),]
  })
  
  observeEvent(input$youth_by_name_list_rows_selected,{
    showModal(
      modalDialog(
        renderDataTable({
          event_table(YBNL_events())
        })
      ))
  })
  
  #########################
  
  output$effective_date_f <- renderUI({
    h4(
      if(is.null(input$imported)){"No data uploaded"}
      else{
        paste0(
          "Effective ", format(effective_date(), "%m-%d-%Y"), " (", input$days_to_inactive, " days to inactive)"
        )})
  })
  
  family_statuses <- reactive({
    if(is.null(input$imported)){return ()}
    client_statuses() %>%
      inner_join(families() %>%
                   select(PersonalID, NumberOfChildren, HouseholdSize), 
                 by = "PersonalID") %>%
      mutate(PersonalID = as.integer(PersonalID),
             IdentificationDate = ymd(IdentificationDate)) %>%
      arrange(PersonalID)
  })
  
  output$FBNL_active <- renderText({
    if(is.null(input$imported)){"---"}
    else{nrow(family_statuses() %>%
                filter(CurrentStatus == "Active"))}
  })
  
  output$FBNL_newly <- renderText({
    if(is.null(input$imported)){"---"}
    else{nrow(family_statuses() %>%
                filter(CurrentStatus == "New to List"))}
  })
  
  output$FBNL_return_h <- renderText({
    if(is.null(input$imported)){"---"}
    else{nrow(family_statuses() %>%
                filter(CurrentStatus == "Return From Housed"))}
  })
  
  output$FBNL_return_i <- renderText({
    if(is.null(input$imported)){"---"}
    else{nrow(family_statuses() %>%
                filter(CurrentStatus == "Return From Inactive"))}
  })
  
  output$family_by_name_list <- renderDataTable({
    if(is.null(input$imported)){return ()}
    bnl_table(family_statuses(),
              input$ces_color, input$shelter_color, input$housing_color)
  })
  
  FBNL_events <- reactive({
    all_events()[which(family_statuses()[[input$family_by_name_list_rows_selected,1]]==all_events()$PersonalID),]
  })
  
  observeEvent(input$family_by_name_list_rows_selected,{
    showModal(
      modalDialog(
        renderDataTable({
          event_table(FBNL_events())
        })
      ))
  })
  
  #########################
  
  output$effective_date_a <- renderUI({
    h4(
      if(is.null(input$imported)){"No data uploaded"}
      else{
        paste0(
          "Effective ", format(effective_date(), "%m-%d-%Y"), " (", input$days_to_inactive, " days to inactive)"
        )})
  })
  
  all_statuses <- reactive({
    if(is.null(input$imported)){return ()}
    client_statuses() %>%
      inner_join(csv_files()$Client %>%
                   filter(DOB <= effective_date() - years(18)) %>%
                   select(PersonalID), by = "PersonalID") %>%
      mutate(PersonalID = as.integer(PersonalID),
             IdentificationDate = ymd(IdentificationDate)) %>%
      arrange(PersonalID)
  })
  
  output$BNL_active <- renderText({
    if(is.null(input$imported)){"---"}
    else{nrow(all_statuses() %>%
                filter(CurrentStatus == "Active"))}
  })
  
  output$BNL_newly <- renderText({
    if(is.null(input$imported)){"---"}
    else{nrow(all_statuses() %>%
                filter(CurrentStatus == "New to List"))}
  })
  
  output$BNL_return_h <- renderText({
    if(is.null(input$imported)){"---"}
    else{nrow(all_statuses() %>%
                filter(CurrentStatus == "Return From Housed"))}
  })
  
  output$BNL_return_i <- renderText({
    if(is.null(input$imported)){"---"}
    else{nrow(all_statuses() %>%
                filter(CurrentStatus == "Return From Inactive"))}
  })
  
  output$by_name_list <- renderDataTable({
    if(is.null(input$imported)){return ()}
    bnl_table(all_statuses(),
              input$ces_color, input$shelter_color, input$housing_color)
  })
  
  BNL_events <- reactive({
    all_events()[which(all_statuses()[[input$by_name_list_rows_selected,1]]==all_events()$PersonalID),]
  })
  
  observeEvent(input$by_name_list_rows_selected,{
    showModal(
      modalDialog(
        renderDataTable({
          event_table(BNL_events())
        })
      ))
  })
  
}
