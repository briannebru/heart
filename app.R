library(shiny)
library(bslib) # For theming with Bootstrap
library(DT) # For rendering data tables
library(ggplot2) # For plotting
library(plotly) # For interactive plots


source("R/helpers.R")
source("R/mod_download_plot.R")

heart <- readRDS("data/heart.rds")

ui <- page_sidebar(
  
  title = tags$span(
    tags$img(src = "logo.png", height = "30px", style = "margin-right: 10px; filter: invert(1);"),
    "Heart Attack Dashboard"
    ),
  
  theme = bs_theme(bootswatch = "pulse"),
  
  sidebar = sidebar(
    selectInput(
      inputId = "outcome",
      label = "Outcome:",
      choices = c("All", "Survived", "Died")
    ),
    
    selectInput(
      inputId = "diagnosis",
      label = "Diagnosis:",
      choices = c("All", sort(unique(as.character(heart$DIAGNOSIS)))),
      selected = "All"
    ),
    
    selectInput(
      inputId = "drg",
      label = "DRG:",
      choices = c("All", sort(unique(as.character(heart$DRG)))),
      selected = "All"
    ),
    
    sliderInput(
      inputId = "age_range",
      label = "Age Range:",
      min = min(heart$AGE),
      max = max(heart$AGE),
      value = c(min(heart$AGE), max(heart$AGE))
    ),
    
    actionButton(
      inputId = "reset",
      label = "Reset",
      icon = bsicons::bs_icon("arrow-counterclockwise")
    )
  ),
  
  #----------------------------------------------------------
  
  # tabs
  
  navset_tab(
    
    nav_panel(
      "Overview", 
      layout_column_wrap(
        width = 1/2,
       
        value_box(
          title = "Female Mortality",
          value = textOutput("f_mortality"),
          theme = "danger",
          showcase = bsicons::bs_icon("gender-female")
        ),
        value_box(
          title = "Male Mortality",
          value = textOutput("m_mortality"),
          theme = "primary",
          showcase = bsicons::bs_icon("gender-male")
        )
      ),
      card(
        card_header("Age Distribution"),
        plotOutput("age_hist"),
        mod_download_plot_ui("dl_age", label = "Download")
      ),
    ),
    
    nav_panel(
      "Explore", 
      plotlyOutput("scatter_plot"),
      mod_download_plot_ui("dl_scatter", label = "Download")
    ),
    
    nav_panel(
      "Charges",
      layout_column_wrap(
        width = 1/3,
        value_box(
          title = "Avg Charges (per stay)",
          value = textOutput("avg_charges"),
          theme = "success",
          showcase = bsicons::bs_icon("currency-dollar")
        ),
        value_box(
          title = "Avg Length of Stay",
          value = textOutput("avg_los"),
          theme = "primary",
          showcase = bsicons::bs_icon("clock")
        ),
        value_box(
          title = "Avg Cost per Day",
          value = textOutput("cost_per_day"),
          theme = "warning",
          showcase = bsicons::bs_icon("receipt")
        )
      ),
      card(
        card_header("Daily Charges by Sex and DRG"),
        plotlyOutput("daily_charges_boxplot"),
        mod_download_plot_ui("dl_daily_charges", label = "Download")
      )
     ),
    
    nav_panel(
      "Length of Stay",
      layout_column_wrap(
        width = 1/2,
        value_box(
          title = "Female Avg LOS",
          value = textOutput("f_avg_los"),
          theme = "danger",
          showcase = bsicons::bs_icon("gender-female")
        ),
        value_box(
          title = "Male Avg LOS",
          value = textOutput("m_avg_los"),
          theme = "primary",
          showcase = bsicons::bs_icon("gender-male")
        )
      ),
      card(
        card_header("Length of Stay Distribution"),
        plotOutput("los_density"),
        mod_download_plot_ui("dl_los", label = "Download")
      )
    ),
    
    nav_panel(
      "Data", 
      # Add the download button
      downloadButton(
        outputId = "download_filtered_data",
        label = "Download Filtered Data"
      ),
      DT::dataTableOutput("data_table")
    ),

    nav_panel(
      "About",
      card(
        card_header("About This Dashboard"),
        tags$p(
          "This dashboard explores outcomes, length of stay, and charges for",
          "12,844 heart attack patients from New York State (1993)."
        ),
        tags$p(
          "Key findings on why women may have higher mortality after heart attack:"
        ),
        tags$ul(
          tags$li("Women often present at older ages and with more comorbidities."),
          tags$li("Delays in seeking and receiving care can worsen outcomes."),
          tags$li("Symptoms can be less classic, increasing under-recognition."),
          tags$li("Underlying mechanisms can differ (e.g., non-obstructive disease)."),
          tags$li("The mortality gap is often larger among younger women.")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    d <- heart
    if (input$outcome != "All") {
      d <- d[d$DIED == input$outcome, ]
    }
    if (input$diagnosis != "All") {
      d <- d[as.character(d$DIAGNOSIS) == input$diagnosis, ]
    }
    if (input$drg != "All") {
      d <- d[as.character(d$DRG) == input$drg, ]
    }
    d <- d[d$AGE >= input$age_range[1] & d$AGE <= input$age_range[2], ]
    d
  })

  output$data_table <- DT::renderDataTable({
    filtered_data()
  })

  # Female stats
  
  output$f_mortality <- renderText({
    compute_mortality(filtered_data()[filtered_data()$SEX == "Female", ])
  })

  # Male stats
  
  output$m_mortality <- renderText({
    compute_mortality(filtered_data()[filtered_data()$SEX == "Male", ])
  })

  # Female avg LOS
  
  output$f_avg_los <- renderText({
    df <- filtered_data()
    x <- mean(df$LOS[df$SEX == "Female"], na.rm = TRUE)
    paste0(format_num(x, digits = 1), " days")
  })

  # Male avg LOS
  
  output$m_avg_los <- renderText({
    df <- filtered_data()
    x <- mean(df$LOS[df$SEX == "Male"], na.rm = TRUE)
    paste0(format_num(x, digits = 1), " days")
  })
  
# Create the age plot as a reactive (reusable)
  
  age_plot <- reactive({
    req(nrow(filtered_data()) >= 2)
    ggplot(filtered_data(), aes(x = AGE, fill = DIED)) +
      geom_density(alpha = 0.5) +
      labs(x = "Age", y = "Density", fill = "DIED") +
      facet_wrap(~ SEX) +
      theme_minimal() +
      theme(
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)
      )
  })
  
  # Display the plot
  
  output$age_hist <- renderPlot({
    age_plot()
  })

  mod_download_plot_server("dl_age", filename = "age_distribution", figure = age_plot)

  # Length of stay density plot
  
  los_plot <- reactive({
    req(nrow(filtered_data()) >= 2)
    ggplot(filtered_data(), aes(x = LOS, fill = DIED)) +
      geom_density(alpha = 0.5) +
      labs(x = "Length of Stay (days)", y = "Density", fill = "DIED") +
      facet_wrap(~ SEX) +
      theme_minimal() +
      theme(
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)
      )
  })
  
  output$los_density <- renderPlot({
    los_plot()
  })

  mod_download_plot_server("dl_los", filename = "length_of_stay_distribution", figure = los_plot)

  scatter_plot <- reactive({
    df <- filtered_data()
    req(nrow(df) >= 1)
    if (nrow(df) > 1000) {
      df <- df[sample(nrow(df), 1000), ]
    }

    ggplot(df, aes(x = AGE, y = LOS, color = SEX)) +
      geom_point(alpha = 0.3) +
      labs(x = "Age", y = "Length of Stay (days)", color = "Sex") +
      geom_smooth(method = "lm", se = FALSE) +
      theme_minimal()
  })

  output$scatter_plot <- renderPlotly({
    ggplotly(scatter_plot())
  })

  mod_download_plot_server("dl_scatter", filename = "age_vs_los_scatter", figure = scatter_plot)

  observeEvent(input$reset, {
    updateSelectInput(session, "outcome", selected = "All")
    updateSelectInput(session, "diagnosis", selected = "All")
    updateSelectInput(session, "drg", selected = "All")
    updateSliderInput(session, "age_range",
    value = c(min(heart$AGE), max(heart$AGE)))
  })

  format_money <- function(x) {
    if (is.na(x) || is.nan(x) || is.infinite(x)) return("—")
    paste0("$", formatC(x, format = "f", digits = 0, big.mark = ","))
  }

  format_num <- function(x, digits = 1) {
    if (is.na(x) || is.nan(x) || is.infinite(x)) return("—")
    formatC(x, format = "f", digits = digits, big.mark = ",")
  }

  # Avg Charges (ignore missing charges)
  output$avg_charges <- renderText({
    df <- filtered_data()
    x <- mean(df$CHARGES, na.rm = TRUE)
    format_money(x)
  })

  # Avg LOS
  output$avg_los <- renderText({
    df <- filtered_data()
    x <- mean(df$LOS, na.rm = TRUE)
    paste0(format_num(x, digits = 1), " days")
  })

  # Avg Cost per Day: sum(CHARGES) / sum(LOS) using LOS > 0 and non-missing charges
  
  output$cost_per_day <- renderText({
    df <- filtered_data()
    df <- df[!is.na(df$CHARGES) & df$LOS > 0, ]
    if (nrow(df) < 1) return("—")
    x <- sum(df$CHARGES) / sum(df$LOS)
    format_money(x)
  })
  
  # ----------------- Server -----------------
  daily_charges_plot <- reactive({
    df <- filtered_data()

    # Remove missing charges and LOS <= 0
    df <- df[!is.na(df$CHARGES) & df$LOS > 0, ]

    req(nrow(df) >= 2)

    # Calculate cost per day
    df$COST_PER_DAY <- df$CHARGES / df$LOS

    ggplot(df, aes(x = SEX, y = COST_PER_DAY, fill = SEX)) +
      geom_boxplot(alpha = 0.7, outlier.alpha = 0.5) +
      facet_wrap(~ DRG, scales = "free_y") +
      labs(
        x = "Sex",
        y = "Cost per Day ($)",
        title = "Daily Charges by Sex and DRG"
      ) +
      theme_minimal() +
      theme(
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "none"
      )
  })

  output$daily_charges_boxplot <- renderPlotly({
    ggplotly(daily_charges_plot(), tooltip = c("x", "y")) # makes it interactive
  })
  
  mod_download_plot_server("dl_daily_charges", filename = "daily_charges", figure = daily_charges_plot)

  output$download_filtered_data <- downloadHandler(
    filename = function() {
      paste0("heart_filtered_", Sys.Date(), ".csv")
    },
    content = function(file) {
      df <- filtered_data()  # use your reactive filtered data
      write.csv(df, file, row.names = FALSE)
    }
  )
  
}

shinyApp(ui = ui, server = server)
