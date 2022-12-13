library(shinydashboard)
library(shiny)
library(shinyBS)
library(h2o)
library(tidyverse)
library(shinyjs)
library(highcharter)
library(plotly)
library(DT)

ui <- dashboardPage(
  dashboardHeader(title = "Loan dashboard 2022"),
  dashboardSidebar(fileInput("file", "Įkelti csv failą"),
                   sidebarMenu(
                   menuItem("Apžvalga", tabName = "dashboard", icon = icon("dashboard", lib = "glyphicon")),
                   menuItem("Duomenys", tabName = "data", icon = icon("table")),
                   menuItem("Grafikai", tabName = "charts", icon = icon("stats", lib = "glyphicon")),
                   menuItem("Spėjimai", tabName = "predictions", icon = icon("scale", lib = "glyphicon")),
                   menuItem("Apie", tabName = "about", icon = icon("info-circle")))),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      tabItem("dashboard",
              fluidRow(
                valueBoxOutput("loansBox"),
                valueBoxOutput("loansStatusTBox"),
                valueBoxOutput("loansStatusFBox")
              ),
              
              ),
      tabItem("data",
              tabPanel("Bar",
                       dataTableOutput("dataTable")
              )
              
      ),
      tabItem("charts",
              fluidRow(
              box(width=6,title = 'Paskolos trukmės stulpelinė diagrama',solidHeader = T,status = 'primary',
                  hr(),
                  plotOutput('termPlot'),
                  hr(),
              ),
              box(width=6,title = 'Kredito įvertinimo stulpelinė diagrama',solidHeader = T,status = 'primary',
                  hr(),
                  plotOutput('creditScorePlot'),
                  hr(),
              ),
              box(width=6,title = 'Kredito įvertinimo stulpelinė diagrama',solidHeader = T,status = 'primary',
                  hr(),
                  plotOutput('loanAmountPlot'),
                  hr(),
              ),
              )
      ),
      
      tabItem("predictions",
              dataTableOutput("predictions")
      ),
      
      tabItem("about",
              h1(tags$b("DVDA projektas")),
              h2( "Kūrė:",tags$b("Paulius Žilinskas")),
              h2( "Grupė:",tags$b("MGDVDAM-2")),
              )
      )
    )
)
server <- function(input, output) {
  h2o.init()
  model <- h2o.loadModel("XGBoost_1_AutoML_1_20221117_61447")
  
  output$table <- renderTable({
    req(input$file)
    table <- read_csv(input$file$datapath)
    
    tableCount <<- table %>% tally()
    tableData <<- table
    
    table
  })
  
  output$predictions <- renderDataTable({
    req(input$file)
    df_test <- h2o.importFile(input$file$datapath)
    p <- h2o.predict(model, df_test)
    
    loanStatusFCount <<- as.data.frame(p) %>% tally(predict == 0)
    loanStatusTCount <<- as.data.frame(p) %>% tally(predict == 1)
    
    p %>%
      as_tibble() %>%
      mutate(y = predict) %>%
      select(y) %>%
      rownames_to_column("id")
  })
  
  output$dataTable <- renderDataTable({
    req(input$file)
    #tableData
    datatable(
      tableData,
      options = list(
        scrollX = TRUE
      )        
    )
  })
  
  output$loansBox <- renderValueBox({
    req(input$file)
    valueBox(
      tableCount,
      "Paskolos prašymai", icon = icon("piggy-bank", lib = "glyphicon"),
      color = "yellow"
    )
  })
  
  output$loansStatusTBox <- renderValueBox({
    req(input$file)
    valueBox(
      loanStatusTCount , "Patvirtinti", icon = icon("ok", lib = "glyphicon"),
      color = "green"
    )
  })
  
  output$loansStatusFBox <- renderValueBox({
    req(input$file)
    valueBox(
      loanStatusFCount,
      "Atmesti",
      icon = icon("remove", lib = "glyphicon"),
      color = "red"
    )
  })
  
  output$termPlot <- renderPlot({
    req(input$file)
    ggplot(tableData, aes(x=as.factor(term), fill=as.factor(term) )) + 
      geom_bar(stat='count', fill="steelblue") +

      xlab('Paskolos trukmė')+
      ylab('Dažnis')+
      coord_flip()+
      theme_minimal()
  })
  
  output$creditScorePlot <- renderPlot({
    req(input$file)
    ggplot(tableData, aes(x=as.factor(credit_score), fill=as.factor(credit_score) )) + 
      geom_bar(stat='count', fill="steelblue") +
      scale_fill_brewer(palette = "Set1") +
      xlab('Kredito įvertinimas')+
      ylab('Dažnis')+
      coord_flip()+
      theme_minimal()
  })
  
  output$loanAmountPlot <- renderPlot({
    req(input$file)
    ggplot(tableData, aes(x=years_credit_history)) + 
      xlab('Kredito istorija(metai)')+
      ylab('Dažnis')+
      geom_histogram(fill="steelblue") +
      theme_minimal()
  })
  
}
shinyApp(ui, server)
