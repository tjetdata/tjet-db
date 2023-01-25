shinyUI(
  fluidPage(
    theme = bslib::bs_theme(version = 5, bootswatch = "cosmo"),
    titlePanel(""),
    # titlePanel("Transitional Justice Evaluation Tools (TJET) Database"),
    tags$head(tags$style(
      HTML(
        "#div_id .selectize-control.single .selectize-input:after{content: none;}
        .leaflet-container { background: #FFF;}"
      )
    )),
    fluidRow(
      column(
        3,
        selectizeInput(
          "samples",
          multiple = TRUE,
          width = "100%",
          label = NULL,
          options = list(placeholder = "select one or more contexts"),
          choices = c(
            "Countries that experienced democratic transitions",
            "Countries that experienced armed internal conflicts",
            "Stable authoritarian regimes",
            "Long-established democratic regimes"
          )
        )
      ),
      column(
        2,
        selectizeInput(
          "mechs",
          multiple = TRUE,
          width = "100%",
          label = NULL,
          options = list(placeholder = "select one or more TJ policies"),
          choices = c(
            "Amnesties",
            "Criminal trials ",
            "Reparations",
            "Truth commissions",
            "Vettings", 
            "only policies addressing SGBV"
          )
        )
      ),
      column(
        2,
        selectizeInput(
          "country",
          label = NULL,
          multiple = TRUE,
          width = "100%",
          options = list(placeholder = "countries"),
          choices = country_names
        )
      ),
      column(
        1,
        selectizeInput(
          "regions",
          label = NULL,
          multiple = TRUE,
          width = "100%",
          options = list(placeholder = "regions"),
          choices = c(
            "all regions",
            "Africa",
            "Americas",
            "Asia",
            "Europe",
            "MENA",
            "Oceania"
          )
        )
      ),
      column(
        1,
        selectizeInput(
          "start_year",
          label = NULL,
          width = "100%",
          options = list(placeholder = "select"),
          choices = 1970:2020,
          selected = 1970
        )
      ),
      column(
        1,
        selectizeInput(
          "last_year",
          label = NULL,
          width = "100%",
          options = list(placeholder = "select"),
          choices = 1970:2020,
          selected = 2020
        )
      ),
      column(2, uiOutput("download_button"))
      # tags$div(id = "div_id",
      #          selectizeInput("sgbv", label = NULL, width = "100%",
      #                         options = list(placeholder = "select"),
      #                         choices = c("All available data",
      #                                     "Only mechanisms addressing SGBV") ) ),
    ),
    tabsetPanel(
      type = "pill",
      tabPanel("Map",
        tags$style(type = "text/css", "#worldmap {height: calc(100vh) !important;}"),
        leafletOutput("worldmap", height = "auto")
      ),
      tabPanel("Table", DT::dataTableOutput("table"))
    )
  )
)