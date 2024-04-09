# ---
# title: "Stocks"
# author: "Trey Sholes"
# format: Shiny App
# ---

# load libraries
library(shiny)
library(shinythemes)
library(tidyverse)
library(dplyr)
library(readxl)
library(forcats)
library(treemap)
library(plotly)
library(forcats)
library(MASS)
library(DT)

# Read datasets
Persons <- read_excel("Persons.xlsx")
Property <- read_excel("Property.xlsx")
Society <- read_excel("Society.xlsx")
Population <- read_excel("Population.xlsx")
Urban <- read_excel("Urban.xlsx")
Household <- read.csv("Household.csv")
Divorce <- read.csv("Divorce.csv")
Police <- read.csv("Police.csv")

# Define Midwest states
midwest_states <-
  c(
    "Illinois",
    "Indiana",
    "Iowa",
    "Kansas",
    "Michigan",
    "Minnesota",
    "Missouri",
    "Nebraska",
    "North Dakota",
    "Ohio",
    "South Dakota",
    "Wisconsin"
  )

# Filter datasets to include only Midwest states where applicable
Persons <- Persons %>% filter(State %in% midwest_states)
Property <- Property %>% filter(State %in% midwest_states)
Society <- Society %>% filter(State %in% midwest_states)
Population <- Population %>% filter(State %in% midwest_states)
Urban <- Urban %>% filter(State %in% midwest_states)
Household <- Household %>% filter(State %in% midwest_states)
Divorce <- Divorce %>% filter(State %in% midwest_states)
Police <- Police %>% filter(State %in% midwest_states)

# Merge filtered datasets
Crime <- merge(Persons, Property, by = "State")
Crime <- merge(Crime, Society, by = "State")
Crime <- merge(Crime, Population, by = "State")

C_Vars <- merge(Urban, Household, by = "State")
C_Vars <- merge(C_Vars, Divorce, by = "State")
C_Vars <- merge(C_Vars, Police, by = "State")

# Remove unused column
C_Vars <- dplyr::select(C_Vars,-2)

# Merge all
C_All <- merge(Crime, C_Vars, by = "State")

# Clean up column names
clean_column_names <- function(names) {
  names <- gsub(" ", "_", names)
  names <- make.names(names)
  return(names)
}

colnames(C_All) <- gsub(" ", "_", colnames(C_All))

# Define UI for application for Shiny App
ui <- fluidPage(
  theme = shinytheme("superhero"),
  navbarPage(
    "United States Crime Data",
    tabPanel(
      "Understanding the Amount of Crime",
      sidebarPanel(
        titlePanel("Inputs"),
        tags$h1(""),
        selectInput(
          "state",
          "Choose State(s):",
          choice = unique(Crime$State),
          multiple = TRUE,
          selected = c("South Dakota", "North Dakota")
        ),
        selectInput(
          "crime",
          "Choose Crime:",
          choice = names(Crime[, c(2:24)]),
          selected = "Arson"
        ),
        checkboxInput("normalize",
                      "Normalize by Population",
                      value = FALSE)
      ),
      mainPanel(
        title = "Bar Plot & Tree Map",
        width = 12,
        height = 12,
        splitLayout(
          cellWidths = c("50%", "50%"),
          plotOutput("bar_plot", height = 400, width = 700),
          plotOutput("tree_plot", height = 400, width = 700)
        )
      )
    ),
    tabPanel(
      "Possible Crime Variables",
      fluidRow(
        style = "height:50px",
        column(
          width = 6,
          selectInput(
            "states",
            "Choose States(s):",
            choice = unique(C_Vars$State),
            multiple = TRUE,
            selected = c("South Dakota", "North Dakota")
          )
        ),
        column(
          width = 6,
          selectInput(
            "doughnut_var",
            "Choose Possible Crime Variable:",
            choices = c(
              "URBAN POP",
              "RURAL POP",
              "Median.Household.Income",
              "Police",
              "First.Line.Police.Detectives",
              "Tranzit.Police",
              "Divorce.Rate"
            ),
            selected = "URBAN POP",
            multiple = FALSE
          )
        )
      ),
      fluidRow(
        style = "height:800px",
        column(width = 7,
               plotlyOutput("doughnut_plot")),
        column(
          width = 5,
          plotOutput("bar2_plot", height = 500, width = 500),
          verbatimTextOutput("linear_regression_output")
        )
      )
    ),
    tabPanel(
      "Linear Regression",
      sidebarPanel(
        selectInput(
          "dependent_var",
          "Dependent Variable",
          choices = clean_column_names(names(C_All[, c(2:24)])),
          selected = "Arson"
        ),
        selectInput(
          "independent_vars",
          "Independent Variables",
          choices = clean_column_names(names(C_All[, c(25:33)])),
          multiple = TRUE
        ),
        actionButton("run_analysis", "Run Analysis"),
        textInput("linear_formula",
                  "Paste Final Model Here:")
      ),
      mainPanel(
        title = "Linear Model",
        width = 12,
        height = 12,
        verbatimTextOutput("anova_output"),
        plotOutput("diagnostic_plots")
        
      )
    )
  )
)

# Define server logic required for Shiny App
server <- function(input, output) {
  output$bar_plot <- renderPlot({
    selected_states <- input$state
    
    if (input$normalize) {
      ggplot(Crime %>% filter(State %in% selected_states),
             aes(x = State, y = (get(input$crime) / Population) * 100000)) +
        geom_segment(aes(
          x = State,
          xend = State,
          y = 0,
          yend = (get(input$crime) / Population) * 100000
        )) +
        geom_point(
          size = 5,
          color = "red",
          fill = alpha("orange", 0.3),
          alpha = 0.7,
          shape = 21,
          stroke = 2
        ) + theme(text = element_text(size = 15)) + labs(y = paste(input$crime, " (Normalized by Population)"))
    } else {
      ggplot(Crime %>% filter(State %in% selected_states),
             aes(x = State, y = get(input$crime))) +
        geom_segment(aes(
          x = State,
          xend = State,
          y = 0,
          yend = get(input$crime)
        )) +
        geom_point(
          size = 5,
          color =           "red",
          fill = alpha("orange", 0.3),
          alpha = 0.7,
          shape = 21,
          stroke = 2
        ) + theme(text = element_text(size = 15)) + labs(y = paste(input$crime))
    }
  })
  
  output$tree_plot <- renderPlot({
    selected_states <- input$state
    
    if (input$normalize) {
      Crime_normalized <- Crime %>% filter(State %in% selected_states) %>%
        mutate(crime_rate = (get(input$crime) / Population) * 100000)
      
      treemap(
        Crime_normalized,
        index = "State",
        vSize = "crime_rate",
        type = "index"
      )
    } else {
      treemap(
        Crime %>% filter(State %in% selected_states),
        index = "State",
        vSize = input$crime,
        type = "index"
      )
    }
  })
  
  output$doughnut_plot <- renderPlotly({
    selected_states <- input$states
    selected_vars <- input$doughnut_var
    
    fig <-
      plot_ly(data = C_Vars %>% filter(State %in% selected_states))
    
    for (var in selected_vars) {
      fig <- fig %>% add_trace(
        labels = ~ State,
        values = ~ get(var),
        type = 'pie',
        name = var,
        domain = list(
          row = 0,
          column = which(selected_vars == var) - 1
        )
      )
    }
    
    fig <- fig %>% layout(
      title = "Pie Chart",
      showlegend = TRUE,
      grid = list(rows = 1, columns = length(selected_vars)),
      xaxis = list(
        showgrid = FALSE,
        zeroline = FALSE,
        showticklabels = FALSE
      ),
      yaxis = list(
        showgrid = FALSE,
        zeroline = FALSE,
        showticklabels = FALSE
      ),
      height = 500,
      width = 500,
      margin = list(
        t = 50,
        l = 50,
        r = 50,
        b = 50
      )
    )
    
    fig
  })
  
  output$bar2_plot <- renderPlot({
    selected_states <- input$states
    C_Vars <- C_Vars %>% filter(State %in% selected_states)
    
    C_Vars %>%
      ggplot(aes(
        x = State,
        y = get(input$doughnut_var),
        fill = State
      )) +
      geom_bar(stat = "identity",
               alpha = .6,
               width = .4) +
      coord_flip() +
      xlab("State(s)") +
      ylab(paste(input$doughnut_var)) +
      theme_bw() +
      theme(text = element_text(size = 15))
  })
  
  # Linear Regression
  results_reactive <- reactiveVal(NULL)
  
  observeEvent(input$run_analysis, {
    dependent_var <- input$dependent_var
    independent_vars <- input$independent_vars
    
    # Check at least one independent variable is selected
    if (is.null(independent_vars) ||
        length(independent_vars) == 0) {
      return()
    }
    
    # Clean column names
    C_All <- C_All[-c(45), ]
    C_All_cleaned <- C_All
    names(C_All_cleaned) <- clean_column_names(names(C_All_cleaned))
    
    formula <-
      as.formula(paste(dependent_var, "~", paste(independent_vars, collapse = " + ")))
    fit <- lm(formula, data = C_All_cleaned)
    
    step <- stepAIC(fit, direction = "both")
    
    results_reactive(list(anova = step$anova, coefficients = coef(step)))
  })
  
  
  output$anova_output <- renderPrint({
    results <- results_reactive()
    if (!is.null(results)) {
      print(results)
    }
  })
  
  output$diagnostic_plots <- renderPlot({
    linear_formula <- as.formula(input$linear_formula)
    
    C_All <- C_All[-c(45), ]
    
    # Fit the linear model
    fit <- lm(linear_formula, data = C_All)
    
    # Create diagnostic plots
    par(mfrow = c(2, 2))
    plot(fit, which = 1)
    plot(fit, which = 2)
    plot(fit, which = 3)
    plot(fit, which = 4)
    par(mfrow = c(1, 1))
  })
}

shinyApp(ui = ui, server = server)

