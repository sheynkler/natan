


library(shiny)
library(shinythemes)
library(DT)


source("modules_ui.R")

shinyUI(
  fluidPage(
    theme = shinytheme("cerulean"),
    includeCSS("css/mycss.css"),
    shinyjs::useShinyjs(),
    

    
    titlePanel("Регрессии Натана Абрамовича"),
    fluidRow(column(
      4,
      
      tags$div(
        tags$h3("Выберите данные: с компьютера, демонстрационные или создайте вектор"),
        tabsetPanel(
          id = "tabs",
          selected = "model_data",
          type = "tabs",
          tabPanel(
            "с компьютера",
            value = "from_computer",
            tags$div(class = "small", csvFileInput()),
            actionButton("submit", "collapse/expand")
          ),
          tabPanel(
            "демо",
            value = "demo",
            selectInput(
              "selectDemoData",
              label = h4("Demo data"),
              selectize = F,
              choices = as.list(names_dates_datasets
              ),
              selected = NULL
            ),
            tags$h3(textOutput("data_titel")),
            br()
            
          ),
          tabPanel(
            "моделирование",
            value = "model_data",
            numericInput(
              "model_data_lenght",
              label = ("Длина"),
              value = 100
            ),
            textInput(
              "model_data_mean",
              label = ("среднее"),
              value = "50"
            ),
            textInput(
              "model_data_sigma",
              label = ("Разброс"),
              value = "10"
            )
            
      
            
          )
          
          
          
          
        )
        #,
        #uiOutput("dependent_2")
      ),

      uiOutput("undependent")
      #,
      
      #'table_undependet_var()',
      #table_undependet_var()
      
      
      
    ),
    column(
      8,
      fluidRow(
        
        verbatimTextOutput("str")
        
      ),
      fluidRow(
        
        verbatimTextOutput("summary")
        
      ),
      fluidRow(
        

         #h4("Эмпирическая функция распределения вероятности"),

        #plotOutput("hist_ecdf"),
        br(), br(),
        h4("Эмпирическая функция распределения вероятности с доверительным интервалом и теоретической функцией распределения"),
        plotOutput("hist_ecdf_custom"),
        plotOutput("plot_approcsimation"),
        plotOutput("plot_plotnost")
        
        
      )
      
      
      
      
    )
    )
  )
)