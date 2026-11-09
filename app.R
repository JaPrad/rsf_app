# Author: Jyotiraditya Pradhan
# Date: November 8, 2026

# Load Packages ----------------------------------------------------------

rm(list = ls())
if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  shiny,
  here,
  tidyverse,
  randomForestSRC
)
rsf <- readRDS(here("data", "competing_rsf.rds"))

religion_choices <- c(
  "Hindu" = "hindu",
  "Muslim" = "muslim",
  "Other" = "other"
)
caste_choices <- c(
  "Upper Caste" = "uc",
  "OBC/Intermediate Caste" = "obc_ic",
  "SC" = "sc",
  "ST" = "st",
  "Not Stated" = "not_stated"
)
edu_choices <- c(
  "No Schooling" = "no_edu",
  "Primary School" = "primary_school",
  "Middle School" = "middle_school",
  "High School" = "high_school",
  "Vocational" = "vocational",
  "Higher Education" = "higher_edu"
)
relation_choices <- c(
  "Self" = "self",
  "Son" = "son",
  "Other" = "other"
)

ui <- fluidPage(
  titlePanel("RSF: Unemployment Duration for Males in Urban India"),

  sidebarLayout(
    sidebarPanel(
      h3("Input"),

      fluidRow(
        # First input column
        column(
          width = 6,

          h4("Individual 1", style = "color: red;"),

          numericInput(
            "age_1",
            "Age",
            value = 35
          ),

          numericInput(
            "family_income_1",
            "Family Income (in 1000 INR)",
            value = 9
          ),

          selectInput(
            "religion_1",
            "Religion",
            religion_choices
          ),

          selectInput(
            "caste_1",
            "Caste",
            caste_choices
          ),

          selectInput(
            "edu_1",
            "Education Level",
            edu_choices
          ),

          selectInput(
            "relation_1",
            "Relation to Head of Household",
            relation_choices
          )
        ),

        # Second input column
        column(
          width = 6,

          h4("Individual 2", style = "color: blue;"),

          numericInput(
            "age_2",
            "Age",
            value = 35
          ),

          numericInput(
            "family_income_2",
            "Family Income (in 1000 INR)",
            value = 9
          ),

          selectInput(
            "religion_2",
            "Religion",
            religion_choices,
            selected = "muslim"
          ),

          selectInput(
            "caste_2",
            "Caste",
            caste_choices,
            selected = "obc_ic"
          ),

          selectInput(
            "edu_2",
            "Education Level",
            edu_choices
          ),

          selectInput(
            "relation_2",
            "Relation to Head of Household",
            relation_choices
          )
        )
      ),

      h4("Exit Type"),

      fluidRow(
        column(
          width = 5,
          checkboxInput(
            "re_employment",
            "Re-Employment",
            value = TRUE
          )
        ),
        column(
          width = 6,
          checkboxInput(
            "quitting",
            "Quitting Labour Force"
          )
        )
      ),

      actionButton(
        "predict",
        "Generate Prediction",
        class = "btn-primary"
      )
    ),

    mainPanel(
      br(),
      br(),
      br(),
      h3("CIF"),
      plotOutput("prediction_plot", height = "400px")
    )
  )
)


server <- function(input, output, session) {
  # Generate prediction
  prediction <- eventReactive(input$predict, {
    # Create input dataframe
    data_1 <- data.frame(
      age = input$age_1,
      family_income_ten_thousands = (input$family_income_1) / 10,
      religion = input$religion_1,
      caste = input$caste_1,
      edu = input$edu_1,
      relation = input$relation_1
    )

    data_2 <- data.frame(
      age = input$age_2,
      family_income_ten_thousands = (input$family_income_2) / 10,
      religion = input$religion_2,
      caste = input$caste_2,
      edu = input$edu_2,
      relation = input$relation_2
    )

    # Predict using loaded model
    prediction_1 <- predict(rsf, data_1)
    prediction_2 <- predict(rsf, data_2)

    tibble(
      time = prediction_1$time.interest,
      cif = prediction_1$cif[1, , "CIF.1"]
    ) |>
      mutate(Individual = "1", `Exit Type` = "Re-Employment") |>
      add_row(
        tibble(
          time = prediction_1$time.interest,
          cif = prediction_1$cif[1, , "CIF.2"]
        ) |>
          mutate(Individual = "1", `Exit Type` = "Quitting")
      ) |>
      add_row(
        tibble(
          time = prediction_2$time.interest,
          cif = prediction_2$cif[1, , "CIF.1"]
        ) |>
          mutate(Individual = "2", `Exit Type` = "Re-Employment")
      ) |>
      add_row(
        tibble(
          time = prediction_2$time.interest,
          cif = prediction_2$cif[1, , "CIF.2"]
        ) |>
          mutate(Individual = "2", `Exit Type` = "Quitting")
      ) |>
      filter(time <= 24)
  })

  # Plot prediction
  output$prediction_plot <- renderPlot({
    req(prediction())

    prediction_table <- prediction()

    if (input$re_employment & !input$quitting) {
      prediction_table |>
        filter(`Exit Type` == "Re-Employment") |>
        ggplot(aes(
          x = time,
          y = cif,
          colour = Individual
        )) +
        ylab("P(Re-Employment)") -> p
    } else if (!input$re_employment & input$quitting) {
      prediction_table |>
        filter(`Exit Type` == "Quitting") |>
        ggplot(aes(
          x = time,
          y = cif,
          colour = Individual
        )) +
        ylab("P(Quitting)") -> p
    } else if (input$re_employment & input$quitting) {
      prediction_table |>
        ggplot(aes(
          x = time,
          y = cif,
          colour = Individual,
          linetype = `Exit Type`
        )) +
        ylab("P(Outcome)") -> p
    }

    p +
      xlab("Time (Months)") +
      geom_line() +
      coord_cartesian(xlim = c(0, 24)) +
      theme_classic(base_size = 20)
  })
}


shinyApp(ui = ui, server = server)
