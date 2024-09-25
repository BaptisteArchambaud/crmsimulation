#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @noRd
#' @importFrom dplyr bind_rows
#' @importFrom escalation fit get_dfcrm recommended_dose
#' @importFrom purrr map map_vec
#' @importFrom shiny numericInput observeEvent reactiveVal renderPlot renderTable renderText renderUI req updateSelectInput
#' @importFrom tibble tibble
#' @importFrom utils data
app_server <- function(input, output, session) {

  #input skeleton depending on input number of levels
  output$skeleton <- renderUI({
    (1:input$nb_levels) %>% map(
      ~ numericInput(inputId = paste0("skeleton", .x), label = paste("Toxicity probability for dose level", .x),
                     value = min(.x / 20, 1), step = 0.05, min = 0, max = 1)
    )
  })

  #input choice of starting dose depending on input number of levels
  observeEvent(input$nb_levels, {
    updateSelectInput(session, inputId = "start_dose", label = "Starting dose", 1:input$nb_levels)
  })

  #simulation database of all included patients
  data <- reactiveVal(tibble(subjid = character(), dose_level = character(), tox = character()))
  #skeleton
  skeleton <- reactiveVal()
  #CRM model
  crm_model_fit <- reactiveVal()
  #dose level for next patient included
  next_dose <- reactiveVal()
  #target toxicity rate
  target <- reactiveVal()
  #plot credible intervals
  CI <- reactiveVal(F)
  #boolean to control inputs before running CRM and simulating data
  crm_run_ok <- reactiveVal(FALSE)

  observeEvent(input$start_button, {
    crm_run_ok(FALSE)
    #clear history of previous data
    output$data <- renderTable(NULL)
    #initialize parameters
    output$recommended_dose <- renderText("")
    skeleton(
      (1:input$nb_levels) %>% map_vec(~input[[paste0("skeleton", .x)]])
    )
    target(input$target)
    #error messages if input controls were not validated
    if(!(target() <=1 & target() >= 0) | is.na(target())){
      output$error_message <- renderText("Target toxicity rate must be >=0 and <=1")
      output$plot <- renderPlot(NULL)
    } else if(!(sum(skeleton() <= 0) == 0 & sum(skeleton() > 1) == 0) | sum(is.na(skeleton())) > 0){
      output$error_message <- renderText("Toxicity probabilities must be >0 and <=1")
      output$plot <- renderPlot(NULL)
      #otherwise initialize CRM and plot skeleton graph
    } else{
      crm_run_ok(TRUE)
      data(tibble(subjid = character(), dose_level = character(), tox = character()))
      crm_model_fit(
        get_dfcrm(skeleton = skeleton(), target = target(), model = "empiric", method = "bayes", scale = sqrt(1.34)) %>%
          fit("")
      )
      next_dose(input$start_dose)
      output$recommended_dose <- renderText(paste0("First patient will receive dose level ", next_dose()))
      output$error_message <- renderText("")
      output$plot <- renderPlot(ggplot_tox_probas(modelfit = crm_model_fit(), target = target()))
    }
  })

  #add new patient in simulation database and update graph depending on button selection
  observeEvent(input$tox_button, {
    req(crm_run_ok())
    data(data() %>% bind_rows(tibble(subjid = paste0("#", nrow(data()) + 1), dose_level = as.character(next_dose()), tox = "1")))
    crm_model_fit(
      get_dfcrm(skeleton = skeleton(), target = target(), model = "empiric", method = "bayes", scale = sqrt(1.34)) %>%
        fit(tox_df_to_vector(data(), "dose_level", "tox"))
    )
    next_dose(crm_model_fit() %>% recommended_dose())
    output$data <- renderTable(data())
    output$recommended_dose <- renderText(paste0("Recommended dose for next patient is dose level ", next_dose()))
    output$plot <- renderPlot(ggplot_tox_probas(modelfit = crm_model_fit(), target = target(), CI = CI()))
  })
  observeEvent(input$notox_button, {
    req(crm_run_ok())
    data(data() %>% bind_rows(tibble(subjid = paste0("#", nrow(data()) + 1), dose_level = as.character(next_dose()), tox = "0")))
    crm_model_fit(
      get_dfcrm(skeleton = skeleton(), target = target(), model = "empiric", method = "bayes", scale = sqrt(1.34)) %>%
        fit(tox_df_to_vector(data(), "dose_level", "tox"))
    )
    next_dose(crm_model_fit() %>% recommended_dose())
    output$data <- renderTable(data())
    output$recommended_dose <- renderText(paste0("Recommended dose for next patient is dose level ", next_dose()))
    output$plot <- renderPlot(ggplot_tox_probas(modelfit = crm_model_fit(), target = target(), CI = CI()))
  })

  #plot credible intervals when clicking on the graph
  observeEvent(input$plot_click, CI(!CI()))

}
