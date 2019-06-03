#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(dplyr)
library(data.table)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(viridis)


ui <- navbarPage(title = "Effects of Air Pollution Around the World",
                 theme = shinytheme("cerulean"),
                 tabPanel("Introduction",
                          mainPanel(
                            uiOutput("introductionText")
                          )),
                 tabPanel("Male vs. Female Deaths",
                          #Application title
                          titlePanel("Comparing Male and Female Death Rates"),
                          
                          # Sidebar with a slider input for number of bins
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("radio", label = h3("Select a Cause of Death:"),
                                choices = list("Lower respiratory infections" = 1,
                                      "Trachea, bronchus, lung cancers" = 2,
                                      "Ischaemic heart disease" = 3,
                                      "Stroke" = 4,
                                      "Chronic obstructive pulmonary disease" = 5),
                                selected = 1)
                              
                            ),
                            
                            mainPanel(
                              plotOutput("graph"),
                              textOutput("plotText")
                            )
                          )
                 ),
                 tabPanel("Death Rate by Country",
                          #Application title
                          titlePanel("Air Pollution Attributable Death Rate by Country"),
                          
                          #Sidebar
                          sidebarLayout(
                            sidebarPanel(
                              p("Causes of Death to Include:"),
                              checkboxInput("LRI", "Lower Respiratory Infections",
                                            value = TRUE),
                              checkboxInput("TBLR", "Trachea, Bronchus, Lung Cancers"
                                            , value = TRUE),
                              checkboxInput("IHD", "Ischaemic Heart Disease",
                                            value = TRUE),
                              checkboxInput("Stroke", "Stroke", value = TRUE),
                              checkboxInput("COPD", "Chronic Obstructive Pulmonary Disease",
                                            value = TRUE)
                            ),
                            
                            mainPanel(
                              plotOutput("map"),
                              textOutput("mapText")
                            )
                          )
                 ),
                 tabPanel("Death Rates by Cause",
                          titlePanel("Top 10 Death Rates by Cause"),
                          
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("select", label = h3("Choose a cause"), 
                                choices = list("Lower respiratory infections" = 1,
                                             "Trachea, bronchus, lung cancers" = 2,
                                             "Ischaemic heart disease" = 3,
                                             "Stroke" = 4,
                                             "Chronic obstructive pulmonary disease" = 5), 
                                          selected = 1)
                            ),
                            mainPanel(
                              tableOutput("table"),
                              textOutput("tableText")
                            )
                          )
                 ),
                 tabPanel("Conclusion",
                          mainPanel(
                            uiOutput("conclusionText")
                          ))
)