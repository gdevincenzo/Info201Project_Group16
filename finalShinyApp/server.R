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
      chosenTable <- filter(myTable, myTable$Cause %in% 
                              "Lower respiratory infections")
      plot(chosenTable$Male, chosenTable$Female,
           main = "Male vs. Female Deaths for Lower respiratory infections in 
           each country", xlab = "Male deaths", ylab = "Female Deaths")
    } else if (input$radio == 2) {
      chosenTable <- filter(myTable, myTable$Cause %in% 
                              "Trachea, bronchus, lung cancers")
      plot(chosenTable$Male, chosenTable$Female, 
           main = "Male vs. Female Deaths for Trachea, bronchus, and lung cancers
           in each country", xlab = "Male deaths", ylab = "Female Deaths")
    } else if (input$radio == 3) {
      chosenTable <- filter(myTable, myTable$Cause %in% "Ischaemic heart disease")
      plot(chosenTable$Male, chosenTable$Female, 
           main = "Male vs. Female Deaths for Ischaemic heart disease in
           each country", xlab = "Male deaths", ylab = "Female Deaths")
    } else if (input$radio == 4) {
      chosenTable <- filter(myTable, myTable$Cause %in% "Stroke")
      plot(chosenTable$Male, chosenTable$Female, main = "Male vs. Female Deaths 
           for Stroke in each country", xlab = "Male deaths",
           ylab = "Female Deaths")
    } else {
      chosenTable <- filter(myTable, myTable$Cause %in% 
                              "Chronic obstructive pulmonary disease")
      plot(chosenTable$Male, chosenTable$Female, 
           main = "Male vs. Female Deaths for Chronic obstructive pulmonary 
           disease in each country", xlab = "Male deaths",ylab = "Female Deaths")
    }
  })
  output$plotText <- renderText({
    paste("Each dot represents a country. If the plot is linear, that means 
        roughly the same amount of males and females died from the specified 
        cause in each country. If the dots are more spread out along the bottom, 
        that means roughly more males than females died of the cause in each 
        country. The same goes for females if the dots are more spread out along
        the left side of the plot. By creating this plot, we are able to 
        determine if certain causes of death affect mostly men or women. If one 
        cause affects mostly women, that could be a sign that we need to focus 
        more research on that cause in women because that's where it's most
        prominent.")
  })
  
  output$introductionText <- renderUI({
    url <- a("WHO Dataset.", href="http://apps.who.int/gho/data/view.main.SDGAIRBOD392v?lang=en")
    
    tagList("With climate change on the rise, the state of our environment has 
            become a big area of interest recently. That's why we decided to focus
            our project on some area of environmental research. We chose a dataset
            that had information about how air pollution affected death rates around
            the world in 2016. There were 5 causes of deaths that were of interest
            in our dataset because they are all caused by pollution: Lower 
            respiratory infections; Trachea, bronchus, and lung cancers; 
            Ischaemic heart disease; Stroke; and Chronic obstructive pulmonary 
            disease. Along with these five causes were the number of males, females,
            and members of both sex that had died from each cause in each country.
            We got all of our data from a", url, tags$br(), tags$br(), tags$i("This project was created by 
            Allison Gibbons, Gabriela De Vincenzo, Josephine Millard, and Varun 
            Sathambakkam."))
  })
  
  output$conclusionText <- renderUI({
    url <- a("our GitHub Repo.", href="https://github.com/gdevincenzo/Info201Project_Group16")
    tagList("Through our data anaylsis, we were able to identify a few interesting
            points about air pollution. For trachea, bronchus, and lung cancers, a 
            noticeably large amount of men died compared to the number of women. 
            This could be a sign that these cancers affect more men and women, 
            which would be useful in future research of these cancers. We also 
            noticed that the lower respiratory infections had the highest rate
            of people dying per 100,00 compared to the other causes. It would be
            interesting to research why these are so common and if they're always
            fatal, so we could possibly lower this rate. The final point of interest
            we found is that the Democratic People's Republic of Korea was top on
            the list for 10 countries with the highest death rate for trachea, 
            bronchus, and lung cancers, stroke, and chronic obstructive pulmonary
            disease. This information is very useful to people who are interested
            in the geographical effects of air pollution, because many of the 
            negative effects are in the same area. We hope that our audience has
            learned a lot about air pollution, and can potentially use this
            information to guide future research or answer any questions they had
            about this topic. Here's a link to", url)
  })
  
  output$tableText <- renderText({
    paste("This is a table of the top 10 countries with the most deaths ordered 
        by both sexes for each cause. We decided to make this table to show which
          countries are struggling most with each of these issues. If a country 
          shows up on the top 10 list for multiple causes, that could be a sign 
          that more research and assistance needs to go to that country in order 
          to reduce the effects of air pollution in that area.")
  })
  
  output$table <- renderTable({
    newTable <- fread("data_by_country.csv", stringsAsFactors = FALSE) %>%
      select(V1, V2, V3, V4, V5) %>% setnames(old = c("V1", "V2", "V3", "V4", 
                                                      "V5"),
                                      new = c("Country", "Cause", "BothSexes", 
                                              "Male", "Female"))
    newTable <- newTable[-c(1:3),]
    newTable$BothSexes <- gsub("x.*", "", newTable$BothSexes)
    newTable$Male <- gsub("x.*", "", newTable$Male)
    newTable$Female <- gsub("x.*", "", newTable$Female)
    
    if(input$select == 1) {
      chosenTable <- filter(newTable, newTable$Cause %in% 
                              "Lower respiratory infections")
    } else if (input$select == 2) {
      chosenTable <- filter(newTable, newTable$Cause %in% 
                              "Trachea, bronchus, lung cancers")
    } else if (input$select == 3) {
      chosenTable <- filter(newTable, newTable$Cause %in% 
                              "Ischaemic heart disease")
    } else if (input$select == 4) {
      chosenTable <- filter(newTable, newTable$Cause %in% "Stroke")
    } else {
      chosenTable <- filter(newTable, newTable$Cause %in% 
                              "Chronic obstructive pulmonary disease")
    }
    chosenTable <- select(chosenTable, "Country", "BothSexes", "Male", "Female")
    chosenTable <- arrange(chosenTable, desc(as.integer(chosenTable$BothSexes)))
    chosenTable <- chosenTable[c(1:10),]
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
    
    cause_table <- main_table
    num_selected <- 5
    if(input$LRI == FALSE) {
      cause_table <- filter(cause_table, Cause != "Lower respiratory infections")
      num_selected = num_selected - 1
    }
    if(input$TBLR == FALSE) {
      cause_table <- filter(cause_table, Cause != "Trachea, bronchus, lung cancers")
      num_selected = num_selected - 1
    }
    if(input$IHD == FALSE) {
      cause_table <- filter(cause_table, Cause != "Ischaemic heart disease")
      num_selected = num_selected - 1
    }
    if(input$Stroke == FALSE) {
      cause_table <- filter(cause_table, Cause != "Stroke")
      num_selected = num_selected - 1
    }
    if(input$COPD == FALSE) {
      cause_table <- filter(cause_table, Cause != "Chronic obstructive pulmonary disease")
      num_selected = num_selected - 1
    }
    
    #in case of no data selected
    if(num_selected != 0){
      cause_table <- filter(cause_table, Cause != "Total")
    }else{
      cause_table$Deaths <- 0
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
    
    # generate map from data table
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
  
  output$mapText <- renderText({
    paste("The world map above shows us how many people per 100,000 died from the chosen causes of death in each country. So,
          you can examine where in the world people more often die from a specific cause, or set of causes. This tool can be 
          used to check overall air pollution attributable death rate by country by keeping all possible causes of death selected. 
          This information can also be used to extrapolate air pollution levels, as countries with higher air pollution are most 
          often associated with a higher air pollution attributable death rate.")
  })
}
