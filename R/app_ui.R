#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @noRd
#' @importFrom htmltools h2 h3 span tagList
#' @importFrom shiny actionButton fluidPage fluidRow mainPanel numericInput plotOutput selectInput sidebarLayout sidebarPanel sliderInput tableOutput textOutput uiOutput
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          fluidRow(
            h2("CRM parameters"),
            numericInput(inputId = "target", "Target toxicity rate", value = 0.25),
            sliderInput(inputId = "nb_levels", label = "Number of dose levels", value = 2, min = 2, max = 12, step = 1, ticks = F),
            selectInput(inputId = "start_dose", label = "Starting dose", c(1,2)),
            h3("Skeleton"),
            uiOutput("skeleton"),
            actionButton(inputId = "start_button", label = "Start model", style = "font-weight: bold;")
          ),
          fluidRow(
            h2("Next patient included"),
            actionButton(inputId = "tox_button", label = "Toxicity", style = "background-color: #fa5f67; font-weight: bold;"),
            actionButton(inputId = "notox_button", label = "No toxicity", style = "background-color: #a0faa3; font-weight: bold;")
          )
        ),
        mainPanel(
          span(textOutput("error_message"), style = " font-weight: bold; color: red;"),
          plotOutput("plot"),
          tableOutput("data"),
          textOutput("recommended_dose")
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
#' @importFrom golem add_resource_path bundle_resources favicon
#' @importFrom utils head
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
      app_title = "crmsimulation"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
