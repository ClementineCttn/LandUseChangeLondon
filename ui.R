##########################################
# Project Land Use change in London
# Clementine Cottineau 
# UCL - CASA - UDL
# 1 November 2016
##########################################

library(shiny)
library(leaflet)



shinyUI(
  fluidPage(
    titlePanel(h2(
      "Land Use Change in London 2007-2010"
    )),
        selectInput(
        "year",
        "Year",
        choices = c(2007, 2010),
        selected = 2007
    ),
    selectInput(
      "variable",
      "Colour Map by...",
      choices = c("PREVIOUS_LAND_USE", "CURRENT_LAND_USE", "PROPOSED_USE"),
      selected = "PREVIOUS_LAND_USE"
    ),
    leafletOutput('map'),
    selectInput(
      "table",
      "Table to print",
      choices = c("Absolute number of conversion"="N", 
                  "Percentage in lines" = "LinePct",
                  "Percentage in columns" = "ColPct"),
      selected = "N"
    ),
    dataTableOutput('transitions')
  )
  )
    
    