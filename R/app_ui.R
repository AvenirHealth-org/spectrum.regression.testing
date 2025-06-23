#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  meta.data = setup.meta.data()

  orderly_spectrum_versions <- list_spectrum_versions(ORDERLY_ROOT)

  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      titlePanel(h1("AIM: HIV Estimates Regression Testing"), windowTitle="AIM Regression Testing"),
      sidebarLayout(
        sidebarPanel(
          selectInput("EstimatesRound",
                      label = "HIV estimates round",
                      choices = sort(unique(meta.data$version$Round), decreasing=TRUE),
                      selected = max(meta.data$version$Round)),
          tabsetPanel(id="Navigator",
                      type="tabs",
                      tabPanel(
                        "Version",
                        br(),
                        h4(strong("Spectrum version 1")),
                        checkboxInput("useOrderlyData1", "Use orderly data", FALSE),
                        conditionalPanel(
                          condition = "!input.useOrderlyData1",
                          selectInput("version1", label = NULL, choices = NULL),
                        ),
                        conditionalPanel(
                          condition = "input.useOrderlyData1",
                          selectInput("orderlySpectrum1", label = "Spectrum version", choices = orderly_spectrum_versions),
                          checkboxInput("orderlyLeapfrog1", label = "Leapfrog run", FALSE),
                          selectInput("orderlyVersion1", label = "Version", choices = NULL)
                        ),
                        h4(strong("Spectrum version 2")),
                        checkboxInput("useOrderlyData2", "Use orderly data", FALSE),
                        conditionalPanel(
                          condition = "!input.useOrderlyData2",
                          selectInput("version2", label = NULL, choices = NULL),
                        ),
                        conditionalPanel(
                          condition = "input.useOrderlyData2",
                          selectInput("orderlySpectrum2", label = "Spectrum version", choices = orderly_spectrum_versions),
                          checkboxInput("orderlyLeapfrog2", label = "Leapfrog run", FALSE),
                          selectInput("orderlyVersion2", label = "Version", choices = NULL)
                        ),
                      ),
                      tabPanel("Location", br(), shinyTree::shinyTree("locationTree", checkbox=TRUE, multiple=TRUE, sort=TRUE, themeIcons=FALSE)),
                      tabPanel("Indicator",
                               checkboxGroupInput("indicators", label="", choices=var.levels$ind, selected=var.levels$ind[4:6]),
                               br(),
                               p("Births and deaths are only displayed if all ages are selected."),
                               p("MTCT and PMTCT indicators are only displayed when indicators for 15-49 females are selected."))
          ),
          width=3),
        mainPanel(
          wellPanel(
            fluidRow(column(1, radioButtons("area",    label=h3("Level"),   choices=var.levels$area, selected=var.levels$area[1])),
                     column(1, radioButtons("sex",     label=h3("Sex"),     choices=var.levels$sex,  selected=var.levels$sex[1])),
                     column(1, radioButtons("age",     label=h3("Age"),     choices=var.levels$age,  selected=var.levels$age[1])),
                     column(2, radioButtons("display", label=h3("Display"), choices=c("Plot trends", "Plot differences", "Table"), selected="Plot trends")),
                     column(3, sliderInput("years", label=h3("Years"), min=1970, max=2030, value=c(1970, 2030), sep="")))),
          conditionalPanel(condition = "input.display == 'Plot trends'",      plotOutput("plotTrend")),
          conditionalPanel(condition = "input.display == 'Plot differences'", plotOutput("plotDiffs")),
          conditionalPanel(condition = "input.display == 'Table'",            DT::DTOutput("table"))
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "spectrum.regression.testing"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
