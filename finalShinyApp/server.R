#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
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

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$graph <- renderPlot({
    myTable <- fread("data_by_country.csv", stringsAsFactors = FALSE) %>%
      select(V2, V4, V5) %>% setnames(old = c("V2", "V4", "V5"),
                                          new = c("Cause", "Male", "Female"))
    myTable <- myTable[-c(1:3),]
    myTable$Male <- gsub("x.*", "", myTable$Male)
    myTable$Female <- gsub("x.*", "", myTable$Female)
    
    if(input$radio == 1) {
      chosenTable <- filter(myTable, myTable$Cause %in% "Lower respiratory infections")
      plot(chosenTable$Male, chosenTable$Female, main = "Male vs. Female Deaths for Lower respiratory infections in each country", xlab = "Male deaths",
           ylab = "Female Deaths")
    } else if (input$radio == 2) {
      chosenTable <- filter(myTable, myTable$Cause %in% "Trachea, bronchus, lung cancers")
      plot(chosenTable$Male, chosenTable$Female, main = "Male vs. Female Deaths for Trachea, bronchus, and lung cancers in each country", xlab = "Male deaths",
           ylab = "Female Deaths")
    } else if (input$radio == 3) {
      chosenTable <- filter(myTable, myTable$Cause %in% "Ischaemic heart disease")
      plot(chosenTable$Male, chosenTable$Female, main = "Male vs. Female Deaths for Ischaemic heart disease in each country", xlab = "Male deaths",
           ylab = "Female Deaths")
    } else if (input$radio == 4) {
      chosenTable <- filter(myTable, myTable$Cause %in% "Stroke")
      plot(chosenTable$Male, chosenTable$Female, main = "Male vs. Female Deaths for Stroke in each country", xlab = "Male deaths",
           ylab = "Female Deaths")
    } else {
      chosenTable <- filter(myTable, myTable$Cause %in% "Chronic obstructive pulmonary disease")
      plot(chosenTable$Male, chosenTable$Female, main = "Male vs. Female Deaths for Chronic obstructive pukmonary disease in each country", xlab = "Male deaths",
           ylab = "Female Deaths")
      # chosenTable$Color = "cyan"
      # for(i in 1:length(chosenTable)){
      #   if(chosenTable[i, 2] > chosenTable[i, 3]) {
      #     chosenTable[i, 4] = "red"
      #   }
      # }
      # qplot(Male, Female, data = chosenTable, colour = Color)
    }
  })
  output$plotText <- renderText({
    paste("Each dot represents a country. If the plot is linear, that means roughly the same amount of males and females
          died from the specified cause in each country. If the dots are more spread out along the bottom, that means
          roughly more males than females died of the cause in each country. The same goes for females if the dots
          are more spread out along the left side of the plot. By creating this plot, we are able to determine if certain
          causes of death affect mostly men or women. If one cause affects mostly women, that could be a sign
          that we need to focus more research on that cuase in women because that's where it's most prominent.")
  })
  
  output$overviewText <- renderUI({
    url <- a("WHO Dataset", href="http://apps.who.int/gho/data/view.main.SDGAIRBOD392v?lang=en")
    tagList("The purpose of this project for us was to examine how air pollution affected
            causes of death around the world for 2016. We got all of our data from a", url,
            "Our dataset had five causes of death for each country, and every cause of death had
            data about the number of males, females, and people of both sexes that had
            died from this cause in 2016. This project was created by...")
  })
  
  output$text <- renderText({
    paste("This will be a table of the total number of deaths for each sex for each cause")
  })
  
  output$table <- renderText({
    newTable <- fread("data_by_country.csv", stringsAsFactors = FALSE) %>%
      select(V1, V2, V3, V4, V5) %>% setnames(old = c("V1", "V2", "V3", "V4", "V5"),
                                      new = c("Country", "Cause", "BothSexes", "Male", "Female"))
    newTable <- newTable[-c(1:3),]
    newTable$BothSexes <- gsub("x.*", "", newTable$BothSexes)
    newTable$Male <- gsub("x.*", "", newTable$Male)
    newTable$Female <- gsub("x.*", "", newTable$Female)
    
    if(input$select == 1) {
      chosenTable <- filter(newTable, newTable$Cause %in% "Lower respiratory infections")
    } else if (input$select2) {
      chosenTable <- filter(newTable, newTable$Cause %in% "Trachea, bronchus, lung cancers")
    } else if (input$select == 3) {
      chosenTable <- filter(newTable, newTable$Cause %in% "Ischaemic heart disease")
    } else if (input$select == 4) {
      chosenTable <- filter(newTable, newTable$Cause %in% "Stroke")
    } else {
      chosenTable <- filter(newTable, newTable$Cause %in% "Chronic obstructive pulmonary disease")
    }
    paste("", nrow(chosenTable))
    #chosenTable <- summarize(chosenTable, )
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
