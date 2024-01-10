# Load paket
library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)

# Dataset default
data_default <- data.frame(
  Day = 1:10,
  Left_Sidebar = c(2.5, 2.7, 2.8, 2.6, 3.0, 2.4, 2.9, 2.5, 2.6, 2.7),
  Center_Page = c(3.8, 3.5, 4.0, 3.7, 3.9, 3.6, 4.1, 3.4, 3.8, 3.9),
  Right_Sidebar = c(3.1, 2.9, 3.0, 3.2, 3.3, 2.8, 3.4, 3.1, 3.2, 3.5)
)
View(data_default)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "CTR Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home"),
      menuItem("Data Input", tabName = "data_input"),
      menuItem("CTR Analysis", tabName = "ctr_analysis"),
      menuItem("Summary and Conclusion", tabName = "summary_and_conclusion")
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .conclusion-box {
          background-color: #FFD700; /* Warna kuning untuk conclusion yang signifikan */
          padding: 10px;
          border-radius: 5px;
          margin-bottom: 10px;
        }
      "))
    ),
    tabItems(
      tabItem(
        tabName = "home",
        fluidRow(
          box(
            title = "CTR Data Overview",
            status = "primary",
            solidHeader = TRUE,
            DTOutput("overview_table")
          )
        )
      ),
      tabItem(
        tabName = "data_input",
        fluidRow(
          box(
            title = "Enter CTR Data",
            status = "primary",
            fileInput("file", "Upload CSV file"),
            textInput("left_sidebar", "Left Sidebar CTR", value = "2.5, 2.7, 2.8, 2.6, 3.0, 2.4, 2.9, 2.5, 2.6, 2.7"),
            textInput("center_page", "Center Page CTR", value = "3.8, 3.5, 4.0, 3.7, 3.9, 3.6, 4.1, 3.4, 3.8, 3.9"),
            textInput("right_sidebar", "Right Sidebar CTR", value = "3.1, 2.9, 3.0, 3.2, 3.3, 2.8, 3.4, 3.1, 3.2, 3.5"),
            actionButton("analyze_button", "Analyze Data")
          )
        )
      ),
      tabItem(
        tabName = "ctr_analysis",
        fluidRow(
          box(
            title = "CTR Analysis Results",
            status = "primary",
            verbatimTextOutput("results_text"),
            plotOutput("ctr_plot"),
            actionButton("summary_button", "Show Summary and Conclusion")
          )
        )
      ),
      tabItem(
        tabName = "summary_and_conclusion",
        fluidRow(
          box(
            title = "Summary and Conclusion",
            status = "primary",
            verbatimTextOutput("left_sidebar_summary"),
            verbatimTextOutput("center_page_summary"),
            verbatimTextOutput("right_sidebar_summary"),
            uiOutput("left_sidebar_conclusion"),
            uiOutput("center_page_conclusion"),
            uiOutput("right_sidebar_conclusion")
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Render overview table
  output$overview_table <- renderDT({
    datatable(data_default, options = list(lengthMenu = c(5, 10, 15), pageLength = 5))
  })
  
  # Data input
  observeEvent(input$analyze_button, {
    
    # Extracting user input
    user_data <- data.frame(
      Day = 1:10,
      Left_Sidebar = as.numeric(unlist(strsplit(input$left_sidebar, ","))),
      Center_Page = as.numeric(unlist(strsplit(input$center_page, ","))),
      Right_Sidebar = as.numeric(unlist(strsplit(input$right_sidebar, ",")))
    )
    
    # Combine default and user data
    combined_data <- bind_rows(data_default, user_data)
    
    # Perform statistical analysis
    result_left <- t.test(combined_data$Left_Sidebar, mu = 0)
    result_center <- t.test(combined_data$Center_Page, mu = 0)
    result_right <- t.test(combined_data$Right_Sidebar, mu = 0)
    
    # Output results
    output$results_text <- renderText({
      paste("Paired t-test p-value (Left Sidebar):", signif(result_left$p.value, 4),
            "\nPaired t-test p-value (Center Page):", signif(result_center$p.value, 4),
            "\nPaired t-test p-value (Right Sidebar):", signif(result_right$p.value, 4))
    })
    
    # Plot CTR performance
    output$ctr_plot <- renderPlot({
      # Bar chart showing CTR by ad placement location
      bar_chart_data <- gather(combined_data, key = "Ad_Placement", value = "CTR", -Day)
      ggplot(bar_chart_data, aes(x = Ad_Placement, y = CTR, fill = Ad_Placement)) +
        geom_bar(stat = "identity") +
        labs(title = "CTR Performance by Ad Placement",
             y = "Click-Through Rate (CTR)")
    })
    
    # Switch to the CTR Analysis tab
    updateTabItems(session, "sidebarMenu", "ctr_analysis")
  })
  
  # Show Summary and Conclusion button event
  observeEvent(input$summary_button, {
    
    # Extracting user input
    user_data <- data.frame(
      Day = 1:10,
      Left_Sidebar = as.numeric(unlist(strsplit(input$left_sidebar, ","))),
      Center_Page = as.numeric(unlist(strsplit(input$center_page, ","))),
      Right_Sidebar = as.numeric(unlist(strsplit(input$right_sidebar, ",")))
    )
    
    # Combine default and user data
    combined_data <- bind_rows(data_default, user_data)
    
    # Perform statistical analysis
    result_left <- t.test(combined_data$Left_Sidebar, mu = 0)
    result_center <- t.test(combined_data$Center_Page, mu = 0)
    result_right <- t.test(combined_data$Right_Sidebar, mu = 0)
    
    # Output summary and conclusion
    output$left_sidebar_summary <- renderText({
      paste("Summary (Left Sidebar):",
            "\nMean:", signif(mean(combined_data$Left_Sidebar), 4),
            "\nSD:", signif(sd(combined_data$Left_Sidebar), 4),
            "\nP-Value (T-Test):", signif(result_left$p.value, 4))
    })
    
    output$center_page_summary <- renderText({
      paste("Summary (Center Page):",
            "\nMean:", signif(mean(combined_data$Center_Page), 4),
            "\nSD:", signif(sd(combined_data$Center_Page), 4),
            "\nP-Value (T-Test):", signif(result_center$p.value, 4))
    })
    
    output$right_sidebar_summary <- renderText({
      paste("Summary (Right Sidebar):",
            "\nMean:", signif(mean(combined_data$Right_Sidebar), 4),
            "\nSD:", signif(sd(combined_data$Right_Sidebar), 4),
            "\nP-Value (T-Test):", signif(result_right$p.value, 4))
    })
    
    output$left_sidebar_conclusion <- renderUI({
      if (result_left$p.value < 0.05) {
        div("Conclusion (Left Sidebar): Menolak Hipotesis Nol karena p-value < alpha = 0.05
            yang artinya bahwa performa CTR untuk kategori tersebut signifikan", class = "conclusion-box")
      } else {
        div("Conclusion (Left Sidebar): Gagal Menolak Hipotesis Nol karena p-value > alpha = 0.05")
      }
    })
    
    output$center_page_conclusion <- renderUI({
      if (result_center$p.value < 0.05) {
        div("Conclusion (Center Page): Menolak Hipotesis Nol karena p-value < alpha = 0.05
            yang artinya bahwa performa CTR untuk kategori tersebut signifikan", class = "conclusion-box")
      } else {
        div("Conclusion (Center Page): Gagal Menolak Hipotesis Nol karena p-value > alpha = 0.05")
      }
    })
    
    output$right_sidebar_conclusion <- renderUI({
      if (result_right$p.value < 0.05) {
        div("Conclusion (Right Sidebar): Menolak Hipotesis Nol karena p-value < alpha = 0.05
            yang artinya bahwa performa CTR untuk kategori tersebut signifikan", class = "conclusion-box")
      } else {
        div("Conclusion (Right Sidebar): Gagal Menolak Hipotesis Nol karena p-value > alpha = 0.05")
      }
    })
    
    # Switch to the Summary and Conclusion tab
    updateTabItems(session, "sidebarMenu", "summary_and_conclusion")
  })
}

# Run the Shiny app
shinyApp(ui, server)
