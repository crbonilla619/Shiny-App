library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)


df <- read_csv('Sales_Byte_ARS_Data_Management - Sales Transactions.csv')
df$Date <- as.Date(df$Date,"%m/%d/%Y")
summary(df)


daily_sales <- df %>% group_by(Date) %>% 
  summarise(total_sales_dollar = sum(Total), total_sales_qty = sum(Qty))

daily_sales_by_ftype <- df %>% group_by(Date, Fridge_Type) %>% 
  summarise(total_sales_dollar = sum(Total), total_sales_qty = sum(Qty))

daily_sales_by_location <- df %>% 
  group_by(Date,Kiosk,Fridge_Type) %>% 
  summarise(total_sales_dollar = sum(Total), total_sales_qty = sum(Qty))

daily_sales_by_product <- df %>%
  group_by(Date, Product) %>%
  summarise(total_sales_dollar = sum(Total), total_sales_qty = sum(Qty))

shinyServer(function(input,output){
  selectData <- reactive({
  #For the first tab  
    req(input$Date)
    validate(need(!is.na(input$Date[1]) & !is.na(input$Date[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$Date[1] < input$Date[2], "Error: Start date should be earlier than end date."))
    {if (input$Fridge_Type != ''){filter(daily_sales_by_ftype, Fridge_Type == req(input$Fridge_Type), Date > as.POSIXct(input$Date[1]) & Date < as.POSIXct(input$Date[2]))}
      else if (input$Kiosk != ''){filter(daily_sales_by_location, Kiosk == req(input$Kiosk), Date > as.POSIXct(input$Date[1]) & Date < as.POSIXct(input$Date[2]))}
      else if (input$Product != ''){filter(daily_sales_by_product, Product==req(input$Product), Date>as.POSIXct(input$Date[1]) & Date < as.POSIXct(input$Date[2]))}
      else{filter(daily_sales, Date > as.POSIXct(input$Date[1]) & Date < as.POSIXct(input$Date[2]))}}
  #For the second tab  
  })
  selectDataT <- reactive({
    req(input$DateT)
    validate(need(!is.na(input$DateT[1]) & !is.na(input$DateT[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$DateT[1] < input$DateT[2], "Error: Start date should be earlier than end date."))
    {if (input$Fridge_TypeT != ''){filter(daily_sales_by_ftype, Fridge_Type == req(input$Fridge_TypeT), Date > as.POSIXct(input$DateT[1]) & Date < as.POSIXct(input$DateT[2]))}
      else if (input$KioskT != ''){filter(daily_sales_by_location, Kiosk == req(input$KioskT), Date > as.POSIXct(input$DateT[1]) & Date < as.POSIXct(input$DateT[2]))}
      else if (input$ProductT != ''){filter(daily_sales_by_product, Product==req(input$ProductT), Date>as.POSIXct(input$DateT[1]) & Date < as.POSIXct(input$DateT[2]))}
      else{filter(daily_sales, Date > as.POSIXct(input$DateT[1]) & Date < as.POSIXct(input$DateT[2]))}}
  })
  output$timeseriesplot <- renderPlot({
    if(input$qty){ggplot(selectData(),aes(x=Date, y=total_sales_qty))+geom_point()+ylab('Sales volume')}
    else{ggplot(selectData(), aes(x=Date, y=total_sales_dollar))+geom_path()+ylab('Sales in $')}
  }
  )
  output$description <- renderText({
    if(input$Fridge_Type != '') {paste('You are now viewing sales breakdown by fridge type [',input$Fridge_Type, ']. No Kiosk has been selected')}
    else if(input$Kiosk !=''){paste('You have selected [',input$Kiosk, ']. You are now viewing sales at', input$Kiosk)}
    else if(input$Product !=''){paste('You have selected product[',input$Product,']. You are now viewing sales of',input$Product)}
    else{paste('You are now viewing aggregate daily sales')}
  })
  output$table <- DT::renderDataTable({selectDataT()})
  output$descriptionT <- renderText({
    if(input$Fridge_TypeT != '') {paste('You are now viewing sales breakdown by fridge type [',input$Fridge_TypeT, ']. No Kiosk has been selected')}
    else if(input$KioskT !=''){paste('You have selected [',input$KioskT, ']. You are now viewing sales at', input$KioskT)}
    else if(input$ProductT !=''){paste('You have selected product[',input$ProductT,']. You are now viewing sales of',input$ProductT)}
    else{paste('You are now viewing aggregate daily sales')}
  })
})
