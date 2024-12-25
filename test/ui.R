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

# credits
{
  credits <- bslib::accordion(multiple = FALSE,
                              accordion_panel("Why would I use this?", 
                                              HTML("<p>The idea behind a by-name list is to help a CoC identify who is experiencing homelessness. Many by-name lists are based on open enrollments, but this by-name list is designed to incorporate other kinds of client interactions with HMIS such as exit destinations, services provided, and current living situation records.",  
                                                   "<p>Many CoCs have developed by-name lists in-house or through their HMIS vendor that work well for their communities. This tool was developed and published in hopes that it can help other CoCs that do not have capacity to create a list internally but are interested in using one for case conferencing or other resource targeting.")
                              ),
                              accordion_panel("What do these client statuses mean?",
                                              HTML("<p>This tool calculates six different client statuses: Active, Inactive, Return From Inactive, Housed, Return From Housed, and New To List. These statuses are calculated from the distribution of client events compared to a user-specified period of time required for inactivity (default is 90 days). Inactive and housed clients are not displayed on the tool.", 
                                                   "<ul>
      <li>Active: Homeless events in the last inactive period and earlier, not returning from housed
      <li>Inactive: Most recent event indicated homelessness, but it is older than the inactive period
      <li>Return From Inactive: Client has homeless events in the inactive period and earlier, but has a gap between them of at least as long as the inactive period
      <li>Housed: Most recent event was a housed event
      <li>Return From Housed: At least one homeless event in the last inactive period that was immediately preceded by a housed event
      <li>New To List: They only have homeless events, none before the inactive period"
                                              )),
                              accordion_panel("What are homeless and housed events?",
                                              HTML("Homeless Events",
                                                   "<ul>
      <li>Enrollment dates into shelter, safe haven, transitional housing, or street outreach
      <li>Any enrollment date with a literally homeless prior living situation (includes actively fleeing DV)
      <li>Exit dates from shelter, safe haven, and transitional housing
      <li>Any exit date with a literally homeless destination
      <li>Open enrollments in shelter, safe have, transitional housing, or housing programs with no move-in dates
      <li>Current living situations indicating literal homelessness
      <li>Street outreach contact services
      <li>Any service from a homeless-specific program
    </ul>
    Housed Events
    <ul>
      <li>Any exit date to a housed destination
      <li>Open enrollment in a housing program with a move-in date
      <li>Housed current living situations
      <li>Rental assistance services
      <li>Deposit assistance services
      <li>Housing move-in dates
    </ul>"
                                              )),
                              accordion_panel("Is this secure?",
                                              HTML("The safety and security of client information always comes first. This tool runs on the same platform as Eva. Based on the following information provided by RStudio (the owner of shinyapps.io), I believe this is secure, but I am not a lawyer and final responsibility for our clients' information security lies with each of us. No client data is stored anywhere by this app; it exists only within the context of a given session.
    <p><blockquote>
      Each app is deployed into its own container, and the network access between containers is tightly controlled. All access to the apps is over SSL, and you can configure your app to require authentication prior to anyone accessing it if you have the Standard plan or above.
      The design of the system is for every account to have its own sub-directory structure, and to enforce the security at the file system and operating system levels. The storage for each container is not permanent, so if you need to store data, our strong recommendation is for you to push that data into your own data store.
      shinyapps.io is currently hosted on Amazon's Web Services (AWS) infrastructure in the us-east-1 region.
    </blockquote>
    <a href='https://docs.rstudio.com/shinyapps.io/security-and-compliance.html'>RStudio: Security and Compliance</a>
    <p><blockquote>
      Each application deployed to shinyapps.io creates storage in its own private file system when the application starts to run. The application only has access to the data that was uploaded with the application at the time of deployment.
    </blockquote>
    <a href='https://docs.rstudio.com/shinyapps.io/Storage.html'>RStudio: Storage</a>
    <br><br>If you would prefer to run this tool on your local machine, you can download the code for the full project from
    <a href='https://github.com/HUD-Data-Lab/active-list'>Github</a>
    and run the whole thing completely offline. If you have any trouble at all getting set up, just let me know and we will work through it together."
                                              )),
                              accordion_panel("How can I distribute and modify this?",
                                              HTML("This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or any later version.
    <p>This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
    <a href='https://www.gnu.org/licenses/'>GNU Affero General Public License</a> for more details.
    <p>This license does technically mean you <a href='https://www.gnu.org/philosophy/selling.en.html'>could sell it</a> but...the whole point is increasing capacity for CoCs that cannot make this kind of technical investment in-house. Just be cool, man."
                                              )),
                              accordion_panel("How can I help?",
                                              HTML("First and foremost, if you would like to modify and/or improve upon this tool (and I hope you do!) please feel free to create a Github branch and let me know what you learn. If you want to submit issues or merge branches, even better. Like anything else, this tool is a work in progress and can only benefit from having more eyes on it!
    Beyond that...if any of this has been helpful to you, please consider donating to the agencies that supported its development:
    <ul>
      <li><a href='https://www.councilforthehomeless.org/donate-online/'>Council for the Homeless</a> for believing this was a worthwhile way to build by-name lists,
      <li><a href='https://www.chipindy.org/donate.html'>CHIP</a> for first letting me implement and improve this passion project,
      <li>or <a href='http://cohhio.org/donate/'>COHHIO</a> for showing us all what could be done with HMIS and R.")),
                              accordion_panel("How do I send feedback?",
                                              HTML("If you're the techie type and you find an issue, go ahead and flag it on Github! If that sounds confusing and/or intimidating (I promise it isn't as bad as it sounds), you can always email me at
    <a href='mailto:gwen.beebe@icf.com'>gwen.beebe@icf.com</a>
    Also please feel free to shoot me a note if this is even remotely useful to you! I'd love to hear about it.")),
  )
}

attribution <- layout_columns(
  value_box(
    title = "Author",
    value = "Gwen Beebe",
    showcase = bs_icon("feather"),
    theme = "text-danger"),
  value_box(
    title = "Last Updated",
    value = "4/22/24",
    showcase = bs_icon("calendar-event"),
    theme = "text-danger"))

import_text <- card(card_header(
  class = "bg-danger",
  "Instructions"),
  HTML("<p>To generate an active list, you will need a hashed 
              <a href='https://files.hudexchange.info/resources/documents/HMIS-CSV-Format-Specifications-2024.pdf'
              target= '_blank' rel='noopener noreferrer'>HMIS CSV Export</a>.
              </p>
              <p>Generate a hashed HMIS CSV Export from your local HMIS and store
              it in a secure location that you can easily find again. It must be
              a .zip file with 24 csv files in it.
              <ul>
              <li>A hashed export means that the personal identifiers are obscured
              when the export is generated.</li>
              <li>The HMIS CSV Export has client-level data in it, so it must be
              stored in a secure location per HUD, state, and local rules and
              regulations.</li>
              <li>If you are unsure how to generate your hashed HMIS CSV Export,
              please contact your vendor.</li>
              </ul>
              Once you have exported the correct file from your HMIS, you are
              ready to generate a PATH report. Begin by uploading your zip file below."))

import_button <- card(
  fileInput("imported",
            label = NULL,
            multiple = FALSE,
            accept = ".zip")#,
  # uiOutput("fileInfo")
)

settings_row_one <- layout_columns(
  card(card_header("Days to Inactive"),
       numericInput("days_to_inactive", "", 90)
  ),
  card(card_header("Open Enrollments to Include")
  )
  ,
  card(title = "Color Controls",
       colourpicker::colourInput("housing_color", "In Housing Program", "LightBlue"),
       colourpicker::colourInput("shelter_color", "Sheltered", "Thistle"),
       colourpicker::colourInput("ces_color", "In Coordinated Entry", "MistyRose"),
       status = "primary", width = 4, solidHeader = TRUE
  )
)


ui <- 
  page_navbar(
    theme =
      bs_theme(
        # bootswatch = "lux", version = 5,
        # bg = "#ffffff", fg = "#000000", primary = "#0785F2",
        # secondary = "#031D40", success = "#30F298", info = "#5BCBF5",
        bg = "#ffffff", fg = "#000000", primary = "#5BCBF5",
        secondary = "#031D40", success = "#30F298", info = "#0785F2",
        warning = "#FFC628", danger = "#414042", base_font = font_google("DM Sans"),
        code_font = font_google("DM Mono"), heading_font = "DM Sans Black",
        # "nav-link-font-size" = "24px",
        "nav-link-font" = "DM Sans Black",
        `enable-shadows` = TRUE
        , preset = "spacelab"
      ),
    title = "Active List Maker",
    nav_panel(title = "Tool Setup",
              navset_card_tab(
                # height = 450,
                full_screen = TRUE,
                nav_panel(
                  card_title("Import HUD CSV"),
                  import_text,
                  import_button,
                  card(
                    DT::dataTableOutput("debug_table")
                  )
                ),
                nav_panel(
                  card_title("Optional Settings"),
                  settings_row_one
                ),
                # nav_panel(
                #   shiny::icon("circle-info"),
                #   markdown("Learn more about [htmlwidgets](http://www.htmlwidgets.org/)")
                # )
              )
    ),
    nav_panel(title = "By-Name Lists", 
              navset_card_tab(
                full_screen = TRUE,
                nav_panel(
                  card_title(div(icon("flag-usa"), " Veteran BNL")),
                  card(htmlOutput("effective_date_v")),
                  layout_column_wrap(
                    width = 1/2, height = 300,
                    value_box(
                      title = "There are",
                      value = textOutput("VBNL_active"),
                      showcase = icon("campground"),
                      p("actively homeless veterans")),
                    value_box(
                      title = "There are",
                      value = textOutput("VBNL_newly"),
                      showcase = icon("car-side"),
                      p("newly homeless veterans")),
                    value_box(
                      title = "There are",
                      value = textOutput("VBNL_return_h"),
                      showcase = icon("house-damage"),
                      p("veterans who have returned from housing")),
                    value_box(
                      title = "There are",
                      value = textOutput("VBNL_return_i"),
                      showcase = icon("undo"),
                      p("veterans who have returned from inactive"))),
                  card(
                    DT::dataTableOutput("veteran_by_name_list")
                  )
                ),
                nav_panel(
                  card_title(div(icon("hourglass"), " Chronic BNL")),
                  card(htmlOutput("effective_date_c")),
                  layout_column_wrap(
                    width = 1/2, height = 300,
                    value_box(
                      title = "There are",
                      value = textOutput("CBNL_active"),
                      showcase = icon("campground"),
                      p("actively chronically homeless people")),
                    value_box(
                      title = "There are",
                      value = textOutput("CBNL_newly"),
                      showcase = icon("car-side"),
                      p("people new to the chronic list")),
                    value_box(
                      title = "There are",
                      value = textOutput("CBNL_return_h"),
                      showcase = icon("house-damage"),
                      p("chronically homeless folks who have returned from housing")),
                    value_box(
                      title = "There are",
                      value = textOutput("CBNL_return_i"),
                      showcase = icon("undo"),
                      p("chronically homeless folks who have returned from inactive"))),
                  card(
                    DT::dataTableOutput("chronic_by_name_list")
                  )
                ),
                nav_panel(
                  card_title(div(icon("user"), " Youth BNL")),
                  card(htmlOutput("effective_date_y")),
                  layout_column_wrap(
                    width = 1/2, height = 300,
                    value_box(
                      title = "There are",
                      value = textOutput("YBNL_active"),
                      showcase = icon("campground"),
                      p("actively homeless youth")),
                    value_box(
                      title = "There are",
                      value = textOutput("YBNL_newly"),
                      showcase = icon("car-side"),
                      p("newly homeless youth")),
                    value_box(
                      title = "There are",
                      value = textOutput("YBNL_return_h"),
                      showcase = icon("house-damage"),
                      p("youth who have returned from housing")),
                    value_box(
                      title = "There are",
                      value = textOutput("YBNL_return_i"),
                      showcase = icon("undo"),
                      p("youth who have returned from inactive"))),
                  card(
                    DT::dataTableOutput("youth_by_name_list")
                  )
                ),
                nav_panel(
                  card_title(div(icon("child"), " Family BNL")),
                  card(htmlOutput("effective_date_y")),
                  layout_column_wrap(
                    width = 1/2, height = 300,
                    value_box(
                      title = "There are",
                      value = textOutput("FBNL_active"),
                      showcase = icon("campground"),
                      p("actively homeless families")),
                    value_box(
                      title = "There are",
                      value = textOutput("FBNL_newly"),
                      showcase = icon("car-side"),
                      p("newly homeless families")),
                    value_box(
                      title = "There are",
                      value = textOutput("FBNL_return_h"),
                      showcase = icon("house-damage"),
                      p("families who have returned from housing")),
                    value_box(
                      title = "There are",
                      value = textOutput("FBNL_return_i"),
                      showcase = icon("undo"),
                      p("families who have returned from inactive"))),
                  card(
                    DT::dataTableOutput("family_by_name_list")
                  )
                ),
                nav_panel(
                  # card_title(div(icon("bar-chart", style = "color:blue;"), " Full BNL"))
                  card_title(div(icon("users"), " Full BNL")),
                  card(htmlOutput("effective_date_a")),
                  layout_column_wrap(
                    width = 1/2, height = 300,
                    value_box(
                      title = "There are",
                      value = textOutput("BNL_active"),
                      showcase = icon("campground"),
                      p("actively homeless adults")),
                    value_box(
                      title = "There are",
                      value = textOutput("BNL_newly"),
                      showcase = icon("car-side"),
                      p("newly homeless adults")),
                    value_box(
                      title = "There are",
                      value = textOutput("BNL_return_h"),
                      showcase = icon("house-damage"),
                      p("adults who have returned from housing")),
                    value_box(
                      title = "There are",
                      value = textOutput("BNL_return_i"),
                      showcase = icon("undo"),
                      p("adults who have returned from inactive"))),
                  card(
                    DT::dataTableOutput("by_name_list")
                  )
                )
              )
    ),
    nav_panel("Other Information", 
              credits,
              attribution)
  )