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

server <- function(input, output, session) {
  
  # ##  load in all files
  # export_data <- reactive({
  #   if(is.null(input$imported)){return ()}
  #   withProgress(
  #     read_csv(unzip(input$imported$datapath, "Export.csv"),
  #              col_types = "cicccciiiTTTccciii"))
  # })
  # 
  # start_date <- reactive({
  #   if(is.null(input$imported)){return ()}
  #   as.Date(export_data()$ExportStartDate)
  # })
  # 
  # end_date <- reactive({
  #   if(is.null(input$imported)){return ()}
  #   as.Date(export_data()$ExportEndDate)
  # })
  # 
  # project_data <- reactive({
  #   if(is.null(input$imported)){return ()}
  #   read_csv(unzip(input$imported$datapath, "Project.csv"),
  #            col_types = "ccccDDnnnnnnnnnTTcTc") 
  # })  
  # 
  # organization_data <- reactive({
  #   if(is.null(input$imported)){return ()}
  #   read_csv(unzip(input$imported$datapath, "Organization.csv"),
  #            col_types = "ccncTTcTn")
  # })
  # 
  # exit_data <- reactive({
  #   if(is.null(input$imported)){return ()}
  #   read_csv(unzip(input$imported$datapath, "Exit.csv"),
  #            col_types = "cccDiciiiiiiiiiiiiiiiiiiiiiiiiiDiiiiiiTTcTc")
  # })
  # 
  # dv_data <- reactive({
  #   if(is.null(input$imported)){return ()}
  #   read_csv(unzip(input$imported$datapath, "HealthAndDV.csv"),
  #            col_types = "cccDiiiiiiiDiiiiiTTcTc") %>%
  #     filter(CurrentlyFleeing == 1 &
  #              DataCollectionStage == 1) %>%
  #     select(EnrollmentID, CurrentlyFleeing) 
  # })
  # 
  # enrollment_data <- reactive({
  #   if(is.null(input$imported)){return ()}
  #   read_csv(unzip(input$imported$datapath, "Enrollment.csv"),
  #            col_types = "cccDciiiiiDiiiDDDiiiicccciiiDiiiiciiiiiiiiiiiiciiiiiiiiiiiiiiiiiiiiTTcTc")
  # })
  # 
  # 
  # 
  # service_data <- reactive({
  #   if(is.null(input$imported)){return ()}
  #   read_csv(unzip(input$imported$datapath, "Services.csv"),
  #            col_types = "cccDiicciciTTcTc")
  # })
  # 
  # cls_data <- reactive({
  #   if(is.null(input$imported)){return ()}
  #   read_csv(unzip(input$imported$datapath, "CurrentLivingSituation.csv"),
  #            col_types = "cccDiciiiiicTTcTc")
  # })
  # 
  # client_data <- reactive({
  #   if(is.null(input$imported)){return ()}
  #   read_csv(unzip(input$imported$datapath, "Client.csv"),
  #            col_types = "ccccciciDiiiiiiiiiiiiiciiiiiiiiiiiiiTTcTc") %>%
  #     select(PersonalID, DOB, VeteranStatus, FirstName, LastName)
  # })
  
  #########################
  
  valid_file <- reactiveVal(0)
  # file_list <- reactiveValues()
  # file_list$file <- list()
  # rv <- reactiveValues()
  
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
        show_invalid_popup(126)
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
  
  #########################
  
  joined_enrollments <- reactive({
    if(is.null(input$imported)){return ()}
    csv_files()$Enrollment %>%
      left_join(csv_files()$Project %>%
                  select(ProjectID, ProjectType, ProjectName, OrganizationID), by = "ProjectID") %>%
      left_join(csv_files()$Organization %>%
                  select(OrganizationID, OrganizationName), by = "OrganizationID") %>%
      left_join(csv_files()$Exit %>%
                  select(EnrollmentID, ExitDate, Destination), by = "EnrollmentID") %>%
      left_join(csv_files()$HealthAndDV, by = "EnrollmentID") %>%
      dplyr::mutate(EntryDate = ymd(EntryDate),
                    MoveInDate = ymd(MoveInDate),
                    ExitDate = ymd(ExitDate)) %>%
      select(EnrollmentID, PersonalID, EntryDate, HouseholdID, RelationshipToHoH, ProjectID,
             LivingSituation, DateToStreetESSH, DisablingCondition, TimesHomelessPastThreeYears, 
             MonthsHomelessPastThreeYears, MoveInDate, CurrentlyFleeing, ProjectType,
             OrganizationName, ProjectName, ExitDate, Destination)
  })
  
  client_information <- reactive({
    if(is.null(input$imported)){return ()}
    base_client_data <- csv_files()$Client %>%
      mutate(Name = paste(str_to_title(FirstName), str_to_title(LastName))) %>%
      select(-c(FirstName, LastName, DOB, VeteranStatus)) %>%
      left_join(joined_enrollments() %>%
                  filter(is.na(ExitDate)) %>%
                  group_by(PersonalID) %>%
                  summarise(ActiveEnrollments = paste(unique(ProjectName), collapse=", "),
                            InHousingProgram = max(if_else(ProjectType %in% housing_program_types, 1, 0),  na.rm = TRUE),
                            Sheltered = max(if_else(ProjectType %in% c(1, 2, 8), 1, 0),  na.rm = TRUE),
                            InCES = max(if_else(ProjectType == 14, 1, 0),  na.rm = TRUE)), 
                by = "PersonalID")
    
    if(is.null(input$additional_client_info)){return (base_client_data)}
    else {return(base_client_data %>%
                   left_join(read_csv(input$additional_client_info$datapath) %>%
                               distinct(PersonalID, .keep_all = TRUE) %>%
                               mutate(PersonalID = as.character(PersonalID)),
                             by = "PersonalID"))}
  })
  
  ########################
  
  ##  run event calculations
  
  ##  current living situation events
  cls_events <- reactive({
    if(is.null(input$imported)){return ()}
    csv_files()$CurrentLivingSituation %>%
      filter(CurrentLivingSituation %in% c(homeless_situations, housed_situations)) %>%
      left_join(joined_enrollments() %>%
                  select(EnrollmentID, ProjectName), by = "EnrollmentID") %>%
      dplyr::mutate(
        ClientStatus = case_when(
          CurrentLivingSituation %in% homeless_situations ~ "Homeless",
          CurrentLivingSituation %in% housed_situations ~ "Housed"),
        EventType = case_when(
          CurrentLivingSituation %in% homeless_situations ~ "Literally Homeless CLS",
          CurrentLivingSituation %in% housed_situations ~ "Housed CLS"),
        InformationSource = if_else(is.na(VerifiedBy), ProjectName, VerifiedBy)) %>%
      rename(EffectiveDate = InformationDate) %>%
      select(PersonalID, EffectiveDate, ClientStatus, EventType, InformationSource)
  })
  
  ##  homeless enrollment events
  homeless_enrollment_events <- reactive({
    if(is.null(input$imported)){return ()}
    joined_enrollments() %>%
      filter(LivingSituation %in% homeless_situations |
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
               MoveInDate <= csv_files()$default_report_end_date &
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
    csv_files()$Services %>%
      filter(DateProvided >= csv_files()$default_report_start_date) %>%
      left_join(hud_service_data, by = c("RecordType", "TypeProvided")) %>%
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
          Destination %in% c(homeless_situations, housed_situations)) %>%
      dplyr::mutate(EffectiveDate = if_else(is.na(ExitDate), csv_files()$default_report_end_date, ExitDate),
                    ClientStatus = case_when(
                      Destination %in% housed_situations |
                        (ProjectType %in% housing_program_types &
                           is.na(ExitDate) &
                           MoveInDate <= csv_files()$default_report_end_date) ~ "Housed",
                      TRUE ~ "Homeless"),
                    EventType = case_when(
                      Destination %in% homeless_situations ~ "Homeless Exit From Program",
                      Destination %in% housed_situations ~ "Housed Exit From Program",
                      TRUE ~ "Still Enrolled In Program")) %>%
      rename(InformationSource = ProjectName) %>%
      select(PersonalID, EffectiveDate, ClientStatus, EventType, InformationSource)
  })
  
  all_events <- reactive({
    if(is.null(input$imported)){return ()}
    cls_events() %>%
      union(homeless_enrollment_events()) %>%
      union(move_in_date_events()) %>%
      union(service_events()) %>%
      union(other_enrollment_events()) %>%
      filter(EffectiveDate >= csv_files()$default_report_start_date) %>%
      arrange(PersonalID, desc(EffectiveDate), desc(ClientStatus)) %>%
      distinct(PersonalID, EffectiveDate, .keep_all = TRUE) %>%
      mutate(before_inactive_date = 
               if_else(EffectiveDate < csv_files()$default_report_end_date - ddays(input$days_to_inactive), 1, 0))
  })
  
  client_statuses <- reactive({
    if(is.null(input$imported)){return ()}
    all_events() %>%
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
        IdentificationDate >= csv_files()$default_report_end_date - ddays(input$days_to_inactive) ~ "Return From Inactive",
        TRUE ~ "Active"
      )) %>%
      filter(CurrentStatus %nin% c("Housed", "Inactive")) %>%
      select(PersonalID, CurrentStatus, IdentificationDate) %>%
      left_join(client_information(),
                by = "PersonalID") %>%
      select(c(PersonalID, InHousingProgram, Sheltered, InCES,
               Name, CurrentStatus, IdentificationDate,
               setdiff(colnames(client_information()), "Name")))
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
  
  chronic_folks <- reactive({
    if(is.null(input$imported)){return ()}
    csv_files()$Enrollment %>%
      inner_join(csv_files()$Client %>%
                   filter(DOB <= csv_files()$default_report_end_date - years(18)) %>%
                   select(PersonalID), by = "PersonalID") %>%
      arrange(desc(EntryDate)) %>%
      group_by(PersonalID) %>%
      slice(1:3) %>%
      ungroup() %>%
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

  chronic_statuses <- reactive({
    if(is.null(input$imported)){return ()}
    client_statuses() %>%
      inner_join(chronic_folks() %>%
                   select(PersonalID), by = "PersonalID") %>%
      mutate(PersonalID = as.integer(PersonalID),
             IdentificationDate = ymd(IdentificationDate)) %>%
      arrange(PersonalID)
  })
  
  recent_enrollments <- reactive({
    if(is.null(input$imported)){return ()}
    csv_files()$Enrollment %>%
      arrange(desc(EntryDate)) %>%
      group_by(PersonalID) %>%
      slice(1:3) %>%
      ungroup() 
  })
      
  youth <- reactive({
    if(is.null(input$imported)){return ()}
    recent_enrollments() %>%
      inner_join(csv_files()$Client %>%
                   filter(DOB <= csv_files()$default_report_end_date - years(18) &
                            DOB > csv_files()$default_report_end_date - years(25)) %>%
                   select(PersonalID), by = "PersonalID") %>%
      filter(RelationshipToHoH %in% c(1, 3)) %>%
      dplyr::mutate(YouthFlag = 1) %>%
      select(PersonalID, YouthFlag) %>%
      group_by(PersonalID) %>%
      slice(1L) %>%
      ungroup()
  })
  
  youth_statuses <- reactive({
    if(is.null(input$imported)){return ()}
    client_statuses() %>%
      inner_join(youth() %>%
                   select(PersonalID), by = "PersonalID") %>%
      mutate(PersonalID = as.integer(PersonalID),
             IdentificationDate = ymd(IdentificationDate)) %>%
      arrange(PersonalID)
  })
  
  family_sizes <- reactive({
    if(is.null(input$imported)){return ()}
    csv_files()$Enrollment %>%
      inner_join(csv_files()$Enrollment %>%
                   distinct(PersonalID, HouseholdID, .keep_all = TRUE) %>%
                   inner_join(csv_files()$Client %>%
                                filter(DOB > csv_files()$default_report_end_date - years(18)) %>%
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
  
  all_statuses <- reactive({
    if(is.null(input$imported)){return ()}
    client_statuses() %>%
      inner_join(csv_files()$Client %>%
                   filter(DOB <= csv_files()$default_report_end_date - years(18)) %>%
                   select(PersonalID), by = "PersonalID") %>%
      mutate(PersonalID = as.integer(PersonalID),
             IdentificationDate = ymd(IdentificationDate)) %>%
      arrange(PersonalID)
  })
  
  #########################
  output$effective_date_v <- renderUI({
    h4(
      if(is.null(input$imported)){"No data uploaded"}
      else{
        paste(
          "Effective", format(csv_files()$default_report_end_date, "%m-%d-%Y")
        )})
  })
  
  output$VBNL_active <- renderValueBox({
    valueBox(paste(
      if(is.null(input$imported)){"---"}
      else{nrow(vet_statuses() %>%
               filter(CurrentStatus == "Active"))}, 
      "Veterans"),  
      "are actively homeless", icon = icon("campground"),
      color = "olive")})
  
  output$VBNL_newly <- renderValueBox({
    valueBox(paste(
      if(is.null(input$imported)){"---"}
      else{nrow(vet_statuses() %>%
               filter(CurrentStatus == "New to List"))}, 
      "Veterans"),  
      "are newly homeless", icon = icon("car-side"),
      color = "olive")})
  
  output$VBNL_return_h <- renderValueBox({
    valueBox(paste(
      if(is.null(input$imported)){"---"}
      else{nrow(vet_statuses() %>%
               filter(CurrentStatus == "Return From Housed"))}, 
      "Veterans"),  
      "have returned from housing", icon = icon("house-damage"),
      color = "olive")})
  
  output$VBNL_return_i <- renderValueBox({
    valueBox(paste(
      if(is.null(input$imported)){"---"}
      else{nrow(vet_statuses() %>%
               filter(CurrentStatus == "Return From Inactive"))}, 
      "Veterans"),  
      "have returned from inactive", icon = icon("undo"),
      color = "olive")})
  
  
  ##############################
  
  output$veteran_by_name_list <- renderDataTable({
    if(is.null(input$imported)){return ()}
    DT::datatable(
      vet_statuses(),
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
      formatStyle("PersonalID", `text-align` = 'center') %>% 
      formatStyle(
        'PersonalID', 'InCES',
        backgroundColor = styleEqual(1, input$ces_color)) %>% 
      formatStyle(
        'PersonalID', 'Sheltered',
        backgroundColor = styleEqual(1, input$shelter_color)) %>% 
      formatStyle(
        'PersonalID', 'InHousingProgram',
        backgroundColor = styleEqual(1, input$housing_color))
  })
  
  VBNL_events <- reactive({
    all_events()[which(vet_statuses()[[input$veteran_by_name_list_rows_selected,1]]==all_events()$PersonalID),]
  })
  
  observeEvent(input$veteran_by_name_list_rows_selected,{
    showModal(
      modalDialog(
        renderDataTable({
          DT::datatable(
            VBNL_events() %>%
              select(PersonalID, EffectiveDate, EventType, InformationSource, before_inactive_date),
            options = list(
              pageLength = 5,
              columnDefs = list(list(targets = 4, visible = FALSE))
              ),
            rownames = FALSE) %>% 
            formatStyle(
              c("PersonalID", "EffectiveDate", "EventType", "InformationSource"),
              'before_inactive_date',
              # target = 'row',
              backgroundColor = styleEqual(c(0, 1), c('White', 'WhiteSmoke'))
            )
        })
      ))
  })
  
  ###############
  output$effective_date_c <- renderUI({
    h4(
      if(is.null(input$imported)){"No data uploaded"}
      else{
        paste(
          "Effective", format(csv_files()$default_report_end_date, "%m-%d-%Y")
        )})
  })
  
  output$chronic_by_name_list <- renderDataTable({
    if(is.null(input$imported)){return ()}
    DT::datatable(
      chronic_statuses(),
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
      formatStyle("PersonalID", `text-align` = 'center') %>% 
      formatStyle(
        'PersonalID', 'InCES',
        backgroundColor = styleEqual(1, input$ces_color)) %>% 
      formatStyle(
        'PersonalID', 'Sheltered',
        backgroundColor = styleEqual(1, input$shelter_color)) %>% 
      formatStyle(
        'PersonalID', 'InHousingProgram',
        backgroundColor = styleEqual(1, input$housing_color))
  })
  
  output$CBNL_active <- renderValueBox({
    valueBox(paste(
      if(is.null(input$imported)){"---"}
      else{nrow(chronic_statuses() %>%
                  filter(CurrentStatus == "Active"))}, 
      "People"),  
      "stayed active on the chronic list", icon = icon("campground"),
      color = "red")})
  
  output$CBNL_newly <- renderValueBox({
    valueBox(paste(
      if(is.null(input$imported)){"---"}
      else{nrow(chronic_statuses() %>%
                  filter(CurrentStatus == "New to List"))}, 
      "People"),  
      "are new to the chronic list", icon = icon("car-side"),
      color = "red")})
  
  output$CBNL_return_h <- renderValueBox({
    valueBox(paste(
      if(is.null(input$imported)){"---"}
      else{nrow(chronic_statuses() %>%
                  filter(CurrentStatus == "Return From Housed"))}, 
      "Chronic People"),  
      "have returned from housing", icon = icon("house-damage"),
      color = "red")})
  
  output$CBNL_return_i <- renderValueBox({
    valueBox(paste(
      if(is.null(input$imported)){"---"}
      else{nrow(chronic_statuses() %>%
                  filter(CurrentStatus == "Return From Inactive"))}, 
      "Chronic People"),  
      "have returned from inactive", icon = icon("undo"),
      color = "red")})
  
  CBNL_events <- reactive({
    all_events()[which(chronic_statuses()[[input$chronic_by_name_list_rows_selected,1]]==all_events()$PersonalID),]
  })
  
  observeEvent(input$chronic_by_name_list_rows_selected,{
    showModal(
      modalDialog(
        renderDataTable({
          DT::datatable(
            CBNL_events() %>%
              select(PersonalID, EffectiveDate, EventType, InformationSource, before_inactive_date),
            options = list(
              pageLength = 5,
              columnDefs = list(list(targets = 4, visible = FALSE))
            ),
            rownames = FALSE) %>% 
            formatStyle(
              c("PersonalID", "EffectiveDate", "EventType", "InformationSource"),
              'before_inactive_date',
              # target = 'row',
              backgroundColor = styleEqual(c(0, 1), c('White', 'WhiteSmoke'))
            )
        })
      ))
  })
  
  ###############
  output$effective_date_y <- renderUI({
    h4(
      if(is.null(input$imported)){"No data uploaded"}
      else{
        paste(
          "Effective", format(csv_files()$default_report_end_date, "%m-%d-%Y")
        )})
  })
  
  output$youth_by_name_list <- renderDataTable({
    if(is.null(input$imported)){return ()}
    DT::datatable(
      youth_statuses(),
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
      formatStyle("PersonalID", `text-align` = 'center') %>% 
      formatStyle(
        'PersonalID', 'InCES',
        backgroundColor = styleEqual(1, input$ces_color)) %>% 
      formatStyle(
        'PersonalID', 'Sheltered',
        backgroundColor = styleEqual(1, input$shelter_color)) %>% 
      formatStyle(
        'PersonalID', 'InHousingProgram',
        backgroundColor = styleEqual(1, input$housing_color))
  })
  
  output$YBNL_active <- renderValueBox({
    valueBox(paste(
      if(is.null(input$imported)){"---"}
      else{nrow(youth_statuses() %>%
                  filter(CurrentStatus == "Active"))}, 
      "Youth"),  
      "are actively homeless", icon = icon("campground"),
      color = "yellow")})
  
  output$YBNL_newly <- renderValueBox({
    valueBox(paste(
      if(is.null(input$imported)){"---"}
      else{nrow(youth_statuses() %>%
                  filter(CurrentStatus == "New to List"))}, 
      "Youth"),  
      "are newly homeless", icon = icon("car-side"),
      color = "yellow")})
  
  output$YBNL_return_h <- renderValueBox({
    valueBox(paste(
      if(is.null(input$imported)){"---"}
      else{nrow(youth_statuses() %>%
                  filter(CurrentStatus == "Return From Housed"))}, 
      "Youth"),  
      "have returned from housing", icon = icon("house-damage"),
      color = "yellow")})
  
  output$YBNL_return_i <- renderValueBox({
    valueBox(paste(
      if(is.null(input$imported)){"---"}
      else{nrow(youth_statuses() %>%
                  filter(CurrentStatus == "Return From Inactive"))}, 
      "Youth"),  
      "have returned from inactive", icon = icon("undo"),
      color = "yellow")})
  
  YBNL_events <- reactive({
    all_events()[which(youth_statuses()[[input$youth_by_name_list_rows_selected,1]]==all_events()$PersonalID),]
  })
  
  observeEvent(input$youth_by_name_list_rows_selected,{
    showModal(
      modalDialog(
        renderDataTable({
          DT::datatable(
            YBNL_events() %>%
              select(PersonalID, EffectiveDate, EventType, InformationSource, before_inactive_date),
            options = list(
              pageLength = 5,
              columnDefs = list(list(targets = 4, visible = FALSE))
            ),
            rownames = FALSE) %>% 
            formatStyle(
              c("PersonalID", "EffectiveDate", "EventType", "InformationSource"),
              'before_inactive_date',
              # target = 'row',
              backgroundColor = styleEqual(c(0, 1), c('White', 'WhiteSmoke'))
            )
        })
      ))
  })
  
  
  ###############
  output$effective_date_f <- renderUI({
    h4(
      if(is.null(input$imported)){"No data uploaded"}
      else{
        paste(
          "Effective", format(csv_files()$default_report_end_date, "%m-%d-%Y")
        )})
  })
  
  output$family_by_name_list <- renderDataTable({
    if(is.null(input$imported)){return ()}
    DT::datatable(
      family_statuses(),
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
      formatStyle("PersonalID", `text-align` = 'center') %>% 
      formatStyle(
        'PersonalID', 'InCES',
        backgroundColor = styleEqual(1, input$ces_color)) %>% 
      formatStyle(
        'PersonalID', 'Sheltered',
        backgroundColor = styleEqual(1, input$shelter_color)) %>% 
      formatStyle(
        'PersonalID', 'InHousingProgram',
        backgroundColor = styleEqual(1, input$housing_color))
  })
  
  output$FBNL_active <- renderValueBox({
    valueBox(paste(
      if(is.null(input$imported)){"---"}
      else{nrow(family_statuses() %>%
                  filter(CurrentStatus == "Active"))}, 
      "Families"),  
      "are actively homeless", icon = icon("campground"),
      color = "purple")})
  
  output$FBNL_newly <- renderValueBox({
    valueBox(paste(
      if(is.null(input$imported)){"---"}
      else{nrow(family_statuses() %>%
                  filter(CurrentStatus == "New to List"))}, 
      "Families"),  
      "are newly homeless", icon = icon("car-side"),
      color = "purple")})
  
  output$FBNL_return_h <- renderValueBox({
    valueBox(paste(
      if(is.null(input$imported)){"---"}
      else{nrow(family_statuses() %>%
                  filter(CurrentStatus == "Return From Housed"))}, 
      "Families"),  
      "have returned from housing", icon = icon("house-damage"),
      color = "purple")})
  
  output$FBNL_return_i <- renderValueBox({
    valueBox(paste(
      if(is.null(input$imported)){"---"}
      else{nrow(family_statuses() %>%
                  filter(CurrentStatus == "Return From Inactive"))}, 
      "Families"),  
      "have returned from inactive", icon = icon("undo"),
      color = "purple")})
  
  FBNL_events <- reactive({
    all_events()[which(family_statuses()[[input$family_by_name_list_rows_selected,1]]==all_events()$PersonalID),]
  })
  
  observeEvent(input$family_by_name_list_rows_selected,{
    showModal(
      modalDialog(
        renderDataTable({
          DT::datatable(
            FBNL_events() %>%
              select(PersonalID, EffectiveDate, EventType, InformationSource, before_inactive_date),
            options = list(
              pageLength = 5,
              columnDefs = list(list(targets = 4, visible = FALSE))
            ),
            rownames = FALSE) %>% 
            formatStyle(
              c("PersonalID", "EffectiveDate", "EventType", "InformationSource"),
              'before_inactive_date',
              # target = 'row',
              backgroundColor = styleEqual(c(0, 1), c('White', 'WhiteSmoke'))
            )
        })
      ))
  })
  
  ###############
  output$effective_date <- renderUI({
    h4(
      if(is.null(input$imported)){"No data uploaded"}
      else{
        paste(
          "Effective", format(csv_files()$default_report_end_date, "%m-%d-%Y")
        )})
  })

  output$by_name_list <- renderDataTable({
    if(is.null(input$imported)){return ()}
    DT::datatable(
      all_statuses(),
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
      formatStyle("PersonalID", `text-align` = 'center') %>% 
      formatStyle(
        'PersonalID', 'InCES',
        backgroundColor = styleEqual(1, input$ces_color)) %>% 
      formatStyle(
        'PersonalID', 'Sheltered',
        backgroundColor = styleEqual(1, input$shelter_color)) %>% 
      formatStyle(
        'PersonalID', 'InHousingProgram',
        backgroundColor = styleEqual(1, input$housing_color))
  })

  output$BNL_active <- renderValueBox({
    valueBox(paste(
      if(is.null(input$imported)){"---"}
      else{nrow(all_statuses() %>%
                  filter(CurrentStatus == "Active"))},
      "People"),
      "are actively homeless", icon = icon("campground"),
      color = "teal")})

  output$BNL_newly <- renderValueBox({
    valueBox(paste(
      if(is.null(input$imported)){"---"}
      else{nrow(all_statuses() %>%
                  filter(CurrentStatus == "New to List"))},
      "People"),
      "are newly homeless", icon = icon("car-side"),
      color = "teal")})

  output$BNL_return_h <- renderValueBox({
    valueBox(paste(
      if(is.null(input$imported)){"---"}
      else{nrow(all_statuses() %>%
                  filter(CurrentStatus == "Return From Housed"))},
      "People"),
      "have returned from housing", icon = icon("house-damage"),
      color = "teal")})

  output$BNL_return_i <- renderValueBox({
    valueBox(paste(
      if(is.null(input$imported)){"---"}
      else{nrow(all_statuses() %>%
                  filter(CurrentStatus == "Return From Inactive"))},
      "People"),
      "have returned from inactive", icon = icon("undo"),
      color = "teal")})

  BNL_events <- reactive({
    all_events()[which(all_statuses()[[input$by_name_list_rows_selected,1]]==all_events()$PersonalID),]
  })

  observeEvent(input$by_name_list_rows_selected,{
    showModal(
      modalDialog(
        renderDataTable({
          DT::datatable(
            BNL_events() %>%
              select(PersonalID, EffectiveDate, EventType, InformationSource, before_inactive_date),
            options = list(
              pageLength = 5,
              columnDefs = list(list(targets = 4, visible = FALSE))
            ),
            rownames = FALSE) %>%
            formatStyle(
              c("PersonalID", "EffectiveDate", "EventType", "InformationSource"),
              'before_inactive_date',
              # target = 'row',
              backgroundColor = styleEqual(c(0, 1), c('White', 'WhiteSmoke'))
            )
        })
      ))
  })
}