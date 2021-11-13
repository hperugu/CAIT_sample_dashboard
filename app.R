###### A simple R Shiny Application
## Harikishan Perugu
## Use runApp(appDir="yourDir",port=8080)
############
#For data manipulation
library(dplyr) #mutate, filter and other functions
library(reshape2) #melt and dcast function
library(tidyverse)
library(forcats)

####For visualization
library(shiny)
library(ggplot2)
library(viridis) #for color blind friendly palettes
library(dplyr) #mutate, filter and other functions
library(reshape2) #melt and dcast function
library(tidyverse)
library(forcats)

####For visualization
library(ggplot2)
library(viridis) #for color blind friendly palettes

library(shinydashboard)
library(DT)
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("GHG Emissions", icon = icon("th"), tabName = "widgets",
             badgeLabel = "new", badgeColor = "green")
  )
)
ui <- dashboardPage(
  dashboardHeader(title = "CAIT Dashboard Sample"),
  sidebar,
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(plotOutput("plot1", height = 350)),
      box(plotOutput("plot2", height = 350)),
      box(plotOutput("plot3", height = 350)),
      box(DT::dataTableOutput("table1", height = "350"))
    )
  )
)

server <- function(input, output) {
    data_combined <- read.csv(file ="C:/Users/wb580236/OneDrive - WBG/Documents/R/myApp/CAIT_MASTERFILE_2000-2018.csv")
    data_incgrp <- filter(data_combined, is.na(WB_IncGrp)==FALSE) %>% group_by(Year, Sector2, WB_IncGrp) %>%
    summarise(n=sum(Value, na.rm=TRUE)) %>%
    mutate(percentage = (n/sum(n))*100)
    
    data_world <- filter(data_combined, Country=="WORLD")
    stack_data <- filter(data_incgrp, Sector2=="TransportLogistics")
    SelectCo <- "Philippines"
    SelectYr <- 2018
    data_value <- filter(data_combined, Country==SelectCo)
    data_pcnt <- filter(data_combined, Country==SelectCo, Year==SelectYr) %>% group_by(Sector2) %>%
    summarise(n = sum(Value, na.rm=TRUE)) %>% 
    mutate(percentage = (n / sum(n))*100)
   

    #r1 <- dcast(filter(data_value, Country==SelectCo && Year==SelectYr), Country ~ Sector2, value.var="Value")
    #r2 <- dcast(filter(data_pcnt, Country==SelectCo, Sector=="Transport"), Country ~ Year, value.var="percentage")
    #t.co <- rbind(r1, r2)
    #t.co$Units <- c("Value (Mt CO2eq)", "Percentage (%)")

    output$plot1 <- renderPlot({
    ggplot(stack_data, aes(x=Year, y=n,  fill=WB_IncGrp)) + 
      geom_area() +
      scale_fill_viridis(discrete=TRUE, option="plasma") +
      theme_bw()+
      ylab("Greenhouse gas emissions from transport \n and bunker fuels (Mt CO2eq)") 
})
   output$plot2 <- renderPlot({
   filter(data_value, Country==SelectCo & Year==SelectYr) %>%
      ggplot(aes(x="", y=Value, fill=Sector2)) + 
      geom_bar(stat="identity", width=1) +
      coord_polar("y", start=0) +
      scale_fill_viridis(discrete=TRUE, option="viridis") +
      theme_bw()+
      ggtitle(SelectCo)

  })
  output$plot3 <- renderPlot({
   filter(data_value, Country==SelectCo) %>%
  mutate(Sector=fct_reorder(Sector, Value, .desc=FALSE)) %>%   #Order the sectors in terms of increasing emissions
  ggplot(aes(x=Year, y=Value, fill=Sector)) + 
      geom_area() +
      scale_fill_viridis(discrete=TRUE, option="plasma") +
      theme_bw()+
      ylab("Greenhouse gas emissions (Mt CO2eq)") +
      ggtitle(SelectCo)

  })
 output$table1 <- renderDataTable(data_pcnt)
}

shinyApp(ui, server)
