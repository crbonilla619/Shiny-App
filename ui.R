library(shiny)
library(shinydashboard)

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

shinyUI(
  dashboardPage(skin = "black",
    dashboardHeader(title = tags$a(href='https://www.6amhealth.com/',tags$img(src="//cdn.shopify.com/s/files/1/2639/0012/files/6AM_BB_b555713e-b5ab-47ec-a4a3-881dfa1c0567_500x.png?v=1568994487",
                                                                              height = "30px"),
                                   style = "padding-top:10px; padding-bottom:10px;")),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Dashboard"),
        menuItem("Charts",tabName = 'visuals'),
        menuItem("Reports", tabName = 'reports'),
        menuItem("Analysis")
      )
    ),
    dashboardBody(
      tabItems(
      tabItem(tabName = "visuals",
              fluidPage(
                titlePanel('Daily Sales'),
                sidebarLayout(
                  sidebarPanel(
                    dateRangeInput(inputId = 'Date',
                                   label = 'Select a date range:',
                                   start = '2019-03-19',
                                   end = '2019-09-08',
                                   format = 'yyyy-mm-dd'),
                    selectizeInput(inputId = "Fridge_Type", label = strong("Fridge Type"),
                                   choices = unique(daily_sales_by_ftype$Fridge_Type)
                    ),
                    selectizeInput(inputId = 'Kiosk', label=strong('Kiosk'),
                                   choices = unique(daily_sales_by_location$Kiosk),selected = NULL),
                    
                    selectizeInput(inputId= 'Product', label=strong('Product'),
                                   choices = unique(daily_sales_by_product$Product))
                    
                  ),
                  mainPanel(
                    helpText('To view sales breakdown by one criterion, remove your selection for others. To view aggregate sales, remove all selections'),
                    verbatimTextOutput('description'),
                    plotOutput(outputId = 'timeseriesplot'),
                    checkboxInput('qty', 'Click to view sales volume', value=FALSE)
                  )
                )
              )
            ),
      tabItem(tabName = 'reports',
              fluidPage(
                titlePanel('Daily Sales Reports'),
                sidebarLayout(
                  sidebarPanel(
                    dateRangeInput(inputId = 'DateT',
                                   label = 'Select a date range:',
                                   start = '2019-03-19',
                                   end = '2019-09-08',
                                   format = 'yyyy-mm-dd'),
                    selectizeInput(inputId = "Fridge_TypeT", label = strong("Fridge Type"),
                                   choices = unique(daily_sales_by_ftype$Fridge_Type)
                    ),
                    selectizeInput(inputId = 'KioskT', label=strong('Kiosk'),
                                   choices = unique(daily_sales_by_location$Kiosk),selected = NULL),
                    
                    selectizeInput(inputId= 'ProductT', label=strong('Product'),
                                   choices = unique(daily_sales_by_product$Product))
                    
                  ),
                  mainPanel(
                    helpText('To view sales breakdown by one criterion, remove your selection for others. To view aggregate sales, remove all selections'),
                    verbatimTextOutput('descriptionT'),
                    DT::dataTableOutput("table")
                  )
                )
              ))
    ))
  )
)