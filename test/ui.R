link_shiny <- tags$a(
  shiny::icon("github"), "Shiny",
  href = "https://github.com/rstudio/shiny",
  target = "_blank"
)
link_posit <- tags$a(
  shiny::icon("r-project"), "Posit",
  href = "https://posit.co",
  target = "_blank"
)

import_text <- card(card_header(
    class = "bg-danger",
    "Instructions"),
    HTML("<p>To generate a <a href='https://www.hudexchange.info/resource/5212/final-hmis-programming-specifications-path-annual-report/'
              target= '_blank' rel='noopener noreferrer'>PATH report</a>, you will need a hashed 
              <a href='https://files.hudexchange.info/resources/documents/HMIS-CSV-Format-Specifications-2024.pdf'
              target= '_blank' rel='noopener noreferrer'>HMIS CSV Export</a>.
              </p>
              <p>Generate a hashed HMIS CSV Export from your local HMIS and store
              it in a secure location that you can easily find again. It must be
              a .zip file with 23 csv files in it.
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
  # card(title = "Color Controls",
  #     colourInput("housing_color", "In Housing Program", "LightBlue"),
  #     colourInput("shelter_color", "Sheltered", "Thistle"),
  #     colourInput("ces_color", "In Coordinated Entry", "MistyRose"),
  #     status = "primary", width = 4, solidHeader = TRUE
  # )
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
        "nav-link-font-size" = "24px",
        "nav-link-font" = "DM Sans Black",
        `enable-shadows` = TRUE
        , preset = "spacelab"
      ),
  title = "By-Name List Maker",
  nav_panel(title = "Tool Setup",
            navset_card_tab(
              # height = 450,
              full_screen = TRUE,
              nav_panel(
                card_title("Import HUD CSV"),
                import_text,
                import_button
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
                card_title(div(icon("flag-usa"), " Veteran BNL"))
              ),
              nav_panel(
                card_title(div(icon("hourglass"), " Chronic BNL"))
              ),
              nav_panel(
                card_title(div(icon("user"), " Youth BNL"))
              ),
              nav_panel(
                card_title(div(icon("child"), " Family BNL"))
              ),
              nav_panel(
                # card_title(div(icon("bar-chart", style = "color:blue;"), " Full BNL"))
                card_title(div(icon("users"), " Full BNL"))
              )
            )
            ),
  nav_panel("Other Information", p("Third page content."))
)