#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# # Define UI for application that draws a histogram
# ui <- fluidPage(
#    
#    # Application title
#    titlePanel("Old Faithful Geyser Data"),
#    
#    # Sidebar with a slider input for number of bins 
#    sidebarLayout(
#       sidebarPanel(
#          sliderInput("bins",
#                      "Number of bins:",
#                      min = 1,
#                      max = 50,
#                      value = 30)
#       ),
#       
#       # Show a plot of the generated distribution
#       mainPanel(
#          plotOutput("distPlot")
#       )
#    )
# )

library(shiny)
library(shinythemes)
library(dplyr)
library(data.table)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(viridis)


ui <- navbarPage(title = "Pollution",
             theme = shinytheme("cerulean"),
             tabPanel("Overview"),
             tabPanel("Interactive Scatter Plot/Bar Graph",
                      #Application title
                      titlePanel("Old Faithful Geyser Data"),
                      
                      # Sidebar with a slider input for number of bins
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("bins",
                                      "Number of bins:",
                                      min = 1,
                                      max = 50,
                                      value = 30)
                        ),
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          plotOutput("distPlot")
                        )
                      )
             ),
             tabPanel("Map",
                      #Application title
                      titlePanel("Air Pollution Attributable Death Rate by Country"),
                      
                      #Sidebar
                      sidebarLayout(
                        sidebarPanel(
                          p("Causes of Death to Include:"),
                          checkboxInput("LRI", "Lower Respiratory Infections", value = TRUE),
                          checkboxInput("TBLR", "Trachea, Bronchus, Lung Cancers", value = TRUE),
                          checkboxInput("IHD", "Ischaemic Heart Disease", value = TRUE),
                          checkboxInput("Stroke", "Stroke", value = TRUE),
                          checkboxInput("COPD", "Chronic Obstructive Pulmonary Disease", value = TRUE)
                        ),
                        
                        mainPanel(
                          plotOutput("map")
                        )
                      )
             ),
             tabPanel("Summary")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
   
   output$map <- renderPlot({
     no_axes <- theme(
       axis.text = element_blank(),
       axis.line = element_blank(),
       axis.ticks = element_blank(),
       panel.border = element_blank(),
       panel.grid = element_blank(),
       axis.title = element_blank(),
       panel.background = element_blank()
     )
     
     # generate data table based on input from ui.R
     main_table <- fread("data_by_country.csv", stringsAsFactors = FALSE)
     main_table <- dplyr::select(main_table, V1, V2, V3)
     setnames(main_table, old=c("V1","V2","V3"), new=c("Country", "Cause", "Deaths"))
     main_table <- main_table[-c(1:3),]
     main_table$Deaths <- gsub("x.*", "", main_table$Deaths)

     cause_table <- filter(main_table, Cause != "Total")
     # generate map from data table
     if(input$LRI == FALSE) {
       cause_table <- filter(cause_table, Cause != "Lower respiratory infections")
     }
     if(input$TBLR == FALSE) {
       cause_table <- filter(cause_table, Cause != "Trachea, Bronchus, Lung Cancers")
     }
     if(input$IHD == FALSE) {
       cause_table <- filter(cause_table, Cause != "Ischaemic Heart Disease")
     }
     if(input$Stroke == FALSE) {
       cause_table <- filter(cause_table, Cause != "Stroke")
     }
     if(input$COPD == FALSE) {
       cause_table <- filter(cause_table, Cause != "Chronic Obstructive Pulmonary Disease")
     }
     
     final_table <- data.table(cause_table$Country, as.integer(cause_table$Deaths))
     final_table <- aggregate(final_table$V2, by=list(V1=final_table$V1), FUN=sum)
     final_table$region <- final_table$V1
     final_table <- dplyr::select(final_table, x, region)
     
     #Some countries are named differently in our data and in the maps package, so here we rename them.
     final_table$region[136] <- "Russia" 
     final_table$region[175] <- "USA" 
     final_table$region[78] <- "Iran" 
     final_table$region[20] <- "Bolivia" 
     final_table$region[179] <- "Venezuela" 
     final_table$region[173] <- "UK" 
     final_table$region[45] <- "North Korea" 
     final_table$region[132] <- "South Korea" 
     final_table$region[174] <- "Tanzania" 
     final_table$region[180] <- "Vietnam" 
     final_table$region[91] <- "Laos" 
     final_table$region[39] <- "Republic of Congo" 
     final_table$region[160] <- "Syria" 
     final_table$region[44] <- "Czech Republic" 
     final_table$region[134] <- "Macedonia" 
     final_table$region[133] <- "Moldova" 
     
     
     world_map <- map_data("world")
     final_table <- inner_join(world_map, final_table, by = "region")

     world_base <- ggplot(data = world_map, mapping = aes(x = long, y = lat, group = group)) + 
       coord_fixed(1.3) + 
       geom_polygon(color = NA, fill = "gray")
     
     data_map <- world_base + 
       geom_polygon(data = final_table, aes(fill = x), color = "white") +
       geom_polygon(color = "white", fill = NA) +
       scale_fill_gradientn(colors = rev(viridis(8)), limits = c(0, NA))  +
       labs(fill = "Deaths per 100,000") +
       theme_bw()  + 
       no_axes
     
     data_map
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

