#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

message("here::here is ", here::here())


if (file.exists(here::here("apps", "HealthScenarioExplorer", "assets", "mod_phmh.rds"))){
  message("model found: loading it")
  mod_phmh <- readRDS(here::here("apps", "HealthScenarioExplorer", "assets", "mod_phmh.rds"))
} else {
  message("model not found: creating")
  # source(here::here("apps", "HealthScenarioExplorer", "R", "create_assets.R"))
}
#

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Model Predictions"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          # create a labeled subtabsetPanel
          wellPanel(
            h4("Baseline conditions"),
            # create a slider to select an age between 16 and 64 inclusive
            sliderInput(
              "ageSelection",
              "Select an age",
              min = 16, max = 64, value = 30
            ),
            # create a selector to select either male or female
            selectInput(
              "sexSelection",
              "Select a sex",
              choices = c("female", "male"),
              selected = "female"
            ),
            selectInput(
              "currentStateSelection",
              "Select a prior activity state",
              choices = c("Employed", "Unemployed", "Inactive care",
                          "Inactive student", "Inactive other", "Inactive retired",
                          "Inactive long term sick"),
              selected = "Employed"
            ),
            sliderInput(
              "sf12mcsSelection",
              "Select a standardised baseline mental health score",
              min = -2.5, max = 2.4, value = 0,
              step = 0.01
            ),
            sliderInput(
              "sf12pcsSelection",
              "Select a standardised baseline physical health score",
              min = -2.5, max = 2.4, value = 0,
              step = 0.01
            )

          ),
          wellPanel(
            h4("Counterfactual scenario"),
            sliderInput(
              "healthChangeCounterfactual",
              "Select how much total health change",
              min = -1.5, max = 2.5, value = 1, step = 0.01
            )
          )
        ),

        mainPanel(
          h2("Baseline model predictions"),
          # Create a table output to display the model predictions
          tableOutput("modelPredictions"),
          plotOutput("modelPredictionsPlot"),
          p("The following shows how the baseline and counterfactual compositions would differ for 10,000 persons with the specified characteristics"),
          tableOutput("scenarioComparisons")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  get_predictor_predictions <- reactive({
    vignette_baseline <- tibble(
      age = input$ageSelection,
      sex = input$sexSelection,
      sf12mcs_dv = input$sf12mcsSelection,
      sf12pcs_dv = input$sf12pcsSelection,
      this_status = input$currentStateSelection
    )
    predictions_baseline <- predict(mod_phmh, vignette_baseline, type = "probs")
    preds_predictors_baseline <- bind_cols(vignette_baseline, t(predictions_baseline))

    vignette_counterfactual <-
      vignette_baseline |>
      mutate(
        sf12mcs_dv = sf12mcs_dv + 1 * input$healthChangeCounterfactual / sqrt(5),
        sf12pcs_dv = sf12pcs_dv + 2 * input$healthChangeCounterfactual / sqrt(5)
      )

    predictions_counterfactual <- predict(mod_phmh, vignette_counterfactual, type = "probs")
    preds_predictors_counterfactual <- bind_cols(vignette_counterfactual, t(predictions_counterfactual))
    preds_predictors <-
      bind_rows(
        preds_predictors_baseline |>
          mutate(scenario = "baseline"),
        preds_predictors_counterfactual |>
          mutate(scenario = "counterfactual")
      )

    preds_predictors
  })

  output$modelPredictionsPlot <- renderPlot({
    df <- get_predictor_predictions() |>
      pivot_longer(cols = Employed:Unemployed, names_to = "pred_status", values_to = "prob")

    gg <- ggplot(df, aes(x = I(1), fill = pred_status, y = prob)) +
      geom_col() +
      coord_polar(theta = "y") +
      facet_wrap(~scenario) +
      labs(x = "", y = "Proportion in state", fill = "Activity state") +
      theme_void() +
      theme(legend.position = "bottom")
    gg
  })

  output$modelPredictions <- renderTable({
    get_predictor_predictions()
  })

  output$scenarioComparisons <- renderTable({
    df <- get_predictor_predictions() |>
      select(scenario, Employed:Unemployed) |>
      pivot_longer(cols = Employed:Unemployed, names_to = "pred_status", values_to = "prob") |>
      mutate(per10k = 10000 * prob) |>
      select(-prob) |>
      pivot_wider(names_from = scenario, values_from = per10k) |>
      mutate(
        abs_diff = counterfactual - baseline,
        rel_diff = abs_diff / baseline
      ) |>
      select(pred_status, baseline, counterfactual, abs_diff, rel_diff)
    df
  })

}

# Run the application
shinyApp(ui = ui, server = server)
