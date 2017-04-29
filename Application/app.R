library(shinydashboard)
library(googlesheets)
fields <- c("store", "category", "date","amount","notes")
sheet.key <- "1o1DuXJUVu0szmEIj5AMeOpqP3lqIAS9ANEZF9Rkix8U"
library(data.table)
library(ggplot2)
library(dplyr)
source("helpers.R")


ui <- dashboardPage(
    dashboardHeader(title = "Budget Tracker"),
    dashboardSidebar(
      sidebarMenu(
        textInput("store", label = h4("Store Name"),value = "Store Name"),
        radioButtons("category", label = h4("Category"),
                     choices = list("Bills" = "Bills", "Fun" = "Fun",
                                    "Groceries" = "Groceries", "Clothes" = "Clothes",
                                    "Dining Out" = "Dining Out",
                                    "Transportation" = "Transportation","Other" = "Other"),selected = "Bills"),
        dateInput("date", label = h4("Date"),format = "mm/dd/yyyy",value = Sys.Date()),
        numericInput("amount",label = h4("Amount"),value = 1),
        textInput("notes", label = h4("Notes"),value = "Personal Notes"),
        actionButton("button", "Update"))
      ),
    dashboardBody(
                fluidRow(
                          valueBoxOutput("money", width = 3),
                          valueBoxOutput("funmoney", width = 3),
                          valueBoxOutput("food",width = 3)
                ),
                fluidRow(
                          box(checkboxGroupInput("line.input", "Expenses Included:",
                                        c("Bills" = "Bills",
                                         "Fun" = "Fun",
                                         "Groceries" = "Groceries",
                                         "Clothes" = "Clothes",
                                         "Dining Out" = "Dining Out",
                                         "Transportation" = "Transportation",
                                         "Other" = "Other"
                                          ),selected = c("Bills","Fun","Groceries","Clothes",
                                                         "Dining Out","Transportation","Other")),width = 3),
                          box(plotOutput("line.plot"), width = 6)
                ),
                fluidRow(
                    box(dateRangeInput("date.range","Time Period:", 
                                     start = as.Date(cut(Sys.Date(),"month")), 
                                                         end = Sys.Date()),
                         checkboxInput("include.bills","Include Bills?",
                                            value = F),
                        checkboxInput("percentage","Express as %?",value = T),width = 3),
                  box(plotOutput("bar.plot"),width = 6))
                )
)



server <- shinyServer(function(input, output){
  rewind <- get_date_range()[1] ; forward <- get_date_range()[2]
  
  formData <- reactive({
    data <- sapply(fields, function(x) input[[x]])
    data
  })
  
  #saveData(formData(),sheet.key)
  sheet <- gs_key(sheet.key)
  
  updated.data <- data.frame(gs_read_csv(sheet))
  updated.data <- format_data(updated.data)
  
  #browser()
  
  output$money <- renderValueBox({
    money <- sum(updated.data$Amount[(updated.data$Date >= rewind) & 
                                       (updated.data$Date <= forward)])
    money <- ifelse(is.na(money),0,money)
    valueBox(
      value = formatC(money, digits = 2, format = "f"),
      subtitle = "Total Money Spent this week",
      icon = icon("money"),
      color = if (money >= 300){
        "red"
      } else if (money >= 150){
        "yellow"
      }  else { "green"}
    )
  })
  
  output$funmoney <- renderValueBox({
    funmoney <- sum(updated.data$Amount[(updated.data$Date >= rewind) & 
                                          (updated.data$Date <= forward) & 
                                          (updated.data$Category == "Fun")])
    funmoney <- ifelse(is.na(funmoney),0,funmoney)
    valueBox(
      value = formatC(funmoney, digits = 2, format = "f"),
      subtitle = "Amount spent on Fun this week",
      icon = icon("glass"),
      color = if (funmoney >= 100){
        "red"
      } else if (funmoney >= 50){
        "yellow"
      }  else { "green"}
    )})
  
  output$food <- renderValueBox({
    foodmoney <- sum(updated.data$Amount[(updated.data$Date >= rewind) & 
                                          (updated.data$Date <= forward) & 
                                          (updated.data$Category == "Dining Out" | updated.data$Category == "Groceries")])
    foodmoney <- ifelse(is.na(foodmoney),0,foodmoney)
    valueBox(
      value = formatC(foodmoney, digits = 2, format = "f"),
      subtitle = "Amount spent on Food this week",
      icon = icon("cutlery"),
      color = if (foodmoney >= 100){
        "red"
      } else if (foodmoney >= 50){
        "yellow"
      }  else { "green"}
    )})
  
  line.plot <- reactive(make.line.plot(updated.data,input$line.input))
  
  output$line.plot <- renderPlot(print(line.plot()))
  
  bar.plot <- reactive(make.bar.plot(updated.data,input$date.range,input$include.bills,input$percentage))
  
  output$bar.plot <- renderPlot(print(bar.plot()))
  
  observeEvent(input$button, {
    browser()
    saveData(formData(),sheet.key)
    sheet <- gs_key(sheet.key)
    
    updated.data <- data.frame(gs_read_csv(sheet))
    updated.data <- format_data(updated.data)
    
    #browser()
    
    line.plot <- reactive(make.line.plot(updated.data,input$line.input))
    
    output$line.plot <- renderPlot(print(line.plot()))
    
    bar.plot <- reactive(make.bar.plot(updated.data,input$date.range,input$include.bills,
                                       input$percentage))
    
    output$bar.plot <- renderPlot(print(bar.plot()))
    
    
    output$money <- renderValueBox({
      money <- sum(updated.data$Amount[(updated.data$Date >= rewind) & 
                                         (updated.data$Date <= forward)])
      valueBox(
        value = formatC(money, digits = 2, format = "f"),
        subtitle = "Total Money Spent this week",
        icon = icon("money"),
        color = if (money >= 300){
          "red"
        } else if (money >= 150){
          "yellow"
        }  else { "green"}
      )
    })
    
    output$funmoney <- renderValueBox({
      funmoney <- sum(updated.data$Amount[(updated.data$Date >= rewind) & 
                                            (updated.data$Date <= forward) & 
                                            (updated.data$Category == "Fun")])
      funmoney <- ifelse(is.na(funmoney),0,funmoney)
      valueBox(
        value = formatC(funmoney, digits = 2, format = "f"),
        subtitle = "Amount spent on Fun this week",
        icon = icon("glass"),
        color = if (funmoney >= 100){
          "red"
        } else if (funmoney >= 50){
          "yellow"
        }  else { "green"})
      })
    
      output$food <- renderValueBox({
        foodmoney <- sum(updated.data$Amount[(updated.data$Date >= rewind) & 
                                               (updated.data$Date <= forward) & 
                                               (updated.data$Category == "Dining Out" | updated.data$Category == "Groceries")])
        foodmoney <- ifelse(is.na(foodmoney),0,foodmoney)
        valueBox(
          value = formatC(foodmoney, digits = 2, format = "f"),
          subtitle = "Amount spent on Fun this week",
          icon = icon("glass"),
          color = if (foodmoney >= 100){
            "red"
          } else if (foodmoney >= 50){
            "yellow"
          }  else { "green"}
        )})
    
    
    })
    
    
})
  

shinyApp(ui = ui, server = server)




      
