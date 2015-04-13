## app.R ##
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(ggmap)
library(leaflet)


# change the path to db to match your path, all other code should not 
# need to be changed
db <- src_sqlite("/home/jacob/documents/blog/arrests/arrests.db")

arrestData <- collect(tbl(db, sql("SELECT * FROM criminal")))
chargeData <- tbl(db, sql("SELECT * FROM charges"))
chargeType <- tbl(db, sql("SELECT * FROM chargeType"))

chargeData <- inner_join(chargeData, chargeType)

header <- dashboardHeader(title = "Visualizing Arrests")

sidebar <- dashboardSidebar(
  # tab: dashboard (summary stats) #############################################
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Mapping", tabName = "leaflet", icon = icon("map-marker")),
    menuItem("21-Ordinance", tabName = "under21", icon = icon("line-chart")),
    menuItem("Source code", icon = icon("github"), 
             href = "https://github.com/iacobus42/arrests",
             newtab = TRUE)
    )
)

body <- dashboardBody(
  tabItems(
    # tab: dashboard (summary stats) ###########################################
    tabItem(tabName = "dashboard",
            fluidRow(box("This dashboard summarizes the arrest data for 
                         Johnson County, Iowa as recorded by the 
                         Press-Citizen newspaper in mid-April 2015.
                         The data is incomplete for years up to 2007
                         but appears to be complete for years 2008 onward.
                         The data reflects arrests, not convictions or 
                         crime scene locations, and the inclusion of a data
                         point should not indicate the crime location or
                         the guilt of the named subject.", 
                         title = "Introduction", width = 12)),
              fluidRow(
              tabBox(
                tabPanel("All Crime", 
                         fluidRow(
                           box(plotOutput("ggmapAll"), 
                               width = 4),
                           box(plotOutput("allTs"), 
                               width = 8),
                           width = 12), 
                         width = 12),
                tabPanel("Violent Crime", 
                         fluidRow(
                           box(plotOutput("ggmapViolent"), 
                               width = 4),
                           box(plotOutput("violentTs"), 
                               width = 8),
                           width = 12), 
                         width = 12),
                tabPanel("Drug Crimes", 
                         fluidRow(
                           box(plotOutput("ggmapDrug"), 
                               width = 4),
                           box(plotOutput("drugTs"), 
                               width = 8),
                           width = 12), 
                         width = 12),
                tabPanel("Alcohol", 
                         fluidRow(
                           box(plotOutput("ggmapEtoh"), 
                               width = 4),
                           box(plotOutput("etohTs"), 
                               width = 8),
                           width = 12), 
                         width = 12),
                tabPanel("DUI", 
                         fluidRow(
                           box(plotOutput("ggmapDUI"), 
                               width = 4),
                           box(plotOutput("duiTs"), 
                               width = 8),
                           width = 12), 
                         width = 12),
                width = 12),
    width = 12)
    ),
    # tab: location (leaflet) ##################################################
    tabItem(tabName = "leaflet",
            fluidRow(
              box("This tab is focused on the mapping of arrest locations over
                  space. The sliders allow the user to adjust the years and
                  the months included in the map. The coding of a charge into
                  violent/drug/alcohol/DUI was done by reviewing a list of 
                  charges and an educated guess. It is not an official coding. 
                  Additionally, locations are questionable. Arrest location 
                  information was only minimially cleaned before being 
                  geocoded with locations that did not geocode on the first 
                  pass being discarded.", 
                  width = 12, title = "About this tab")),
            fluidRow(
              box(selectInput("crimeType",
                          label = "Charge Type",
                          choices = c("Violent", "Drug", 
                                      "Alcohol", "DUI"),
                          selected = "Violent"), width = 4),
              box(sliderInput("year",
                          label = "Years",
                          min = 2008,
                          max = 2015,
                          value = c(2008, 2009)), width = 4),
              
              box(sliderInput("month",
                          label = "Months",
                          min = 1,
                          max = 12, 
                          value = c(1, 12)), width = 4)              
            ),
            fluidRow(
              infoBoxOutput("nArrests", width = 4),
              infoBoxOutput("nPeople", width = 4),
              infoBoxOutput("allPercent", width = 4)
            ),
            fluidRow(
              box(leafletOutput("map"), width = 8),
              box(title = "Ten Most Common Charges in this Group", 
                  tableOutput("freqCharge"), width = 4)
            )
    ),
    # tab: time under 21
    tabItem(tabName = "under21",
            fluidRow(box(
              "In June 2010, Iowa City passed a city ordinance that forbid 
              patrons under the drinking age from bars after 10 PM. This 
              ordinance was passed after a significant problem with underage
              drinking in the downtown area. Below, I show counts, mapping and
              time series data on four possible charges that would be sensitive 
              to this law. Public intoxication was used as a measure of 
              excessive and distruptive behavior following drinking. Underage
              possession of alcohol was included as arrests for this charge,
              relative to citations (which some evidence says remains 
              unchanged), are likely to indicate excessive drunkness OR the 
              subject was not local. Posession of a fake ID was included to 
              see if the law induced demand for fake IDs. Finally, Iowa 
              includes a provision for a disorderly house that is generally
              applied to excessive parties, particularly with underage drinking.
              This should capture a shifting from 'bar' drinking to 
              'house-parties'.", 
              width = 12, title = "About this tab")),
            fluidRow(
              box(
                selectInput("under21Measure", "Charge Name", 
                          choices = c("public intoxication",
                                      "underage possession of alcohol",                                      
                                      "possess fictitious drivers license or id",
                                      "disorderly house"),
                          selected = "public intoxication")
                )
            ),
            fluidRow(
              infoBoxOutput("nBefore", width = 4),
              infoBoxOutput("nAfter", width = 4),
              infoBoxOutput("nDelta", width = 4)
            ),
            fluidRow(
              box(leafletOutput("under21map"), width = 4),
              box(plotOutput("under21ts"), width = 8)
            )
    )
  )
)

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {
  
  output$nBefore <- renderInfoBox({
    subset <- filter(chargeData, charge == input$under21Measure) %>%
      collect() %>%
      inner_join(arrestData) %>%
      filter(year < 2010, year >= 2008)
    n <- round(nrow(collect(subset)) / 24, 1)
    infoBox(
      "Arrests Before / Month", n)
  })
  
  output$nAfter <- renderInfoBox({
    subset <- filter(chargeData, charge == input$under21Measure) %>%
      collect() %>%
      inner_join(arrestData) %>%
      filter(year > 2010)
    n <- round(nrow(collect(subset)) / 51, 1)
    infoBox(
      "Arrests After / Month", n)
  })
  
  output$nDelta <- renderInfoBox({
    subset <- filter(chargeData, charge == input$under21Measure) %>%
      collect() %>%
      inner_join(arrestData) %>%
      filter(year < 2010, year >= 2008)
    nbefore <- nrow(collect(subset)) / 24
    subset <- filter(chargeData, charge == input$under21Measure) %>%
      collect() %>%
      inner_join(arrestData) %>%
      filter(year > 2010)
    nafter <- nrow(collect(subset)) / 51
    d <- round((nafter - nbefore) / nbefore * 100, 2)
    infoBox(
      "% Change", d)
  })
  
  output$under21map <- renderLeaflet({
    plotData <- filter(chargeData, charge == input$under21Measure) %>%
      collect() %>%
      inner_join(arrestData) %>%
      filter(year >= 2008, !is.na(lat))
    plotData$before <- ifelse(plotData$year <= 2010, "red", "blue")
    map <- leaflet() %>% 
      addTiles()  %>% 
      fitBounds(-91.593, 41.634, -91.483, 41.68) %>%
      addCircles(data = plotData, color = plotData$before, fill = FALSE) 
    map
  })
  
  output$under21ts <- renderPlot({
    data <- filter(chargeData, charge == input$under21Measure) %>%
      collect() %>%
      inner_join(arrestData) %>%
      filter(year >= 2008) %>% 
      select(one_of("month", "day", "year"))
    data$days <- as.numeric(
      as.Date(paste(data$year, data$month, data$day, sep = "-")) - 
        as.Date(paste(data$year, "01", "01", sep = "-")))
    data$week <- data$year + floor(data$days / 7) / 52
    data <- group_by(data, week) %>%
      summarize(n = n())
    data$ordinance <- ifelse(data$week <= 2010.57, 
                             "Not in Effect", 
                             "In Effect")
    ggplot(data, aes(x = week, y = n)) + 
      geom_point() + 
      geom_line(alpha = 0.5) + 
      scale_x_continuous("") + 
      scale_y_continuous(
        paste("Number Arrests for ", input$under21Measure, " / Week", sep = ""))
  })
  
  output$freqCharge <- renderTable({
    if (input$crimeType == "Alcohol") {
      subset <- select(chargeData, one_of("aid", "charge", 
                                          "etoh"))
      subset <- collect(subset)
      names(subset)[3] <- "type"
      subset <- filter(subset, type == 1)
      } else {
      subset <- select(chargeData, one_of("aid", "charge", 
                                          tolower(input$crimeType)))
      subset <- collect(subset)
      names(subset)[3] <- "type"
      subset <- filter(subset, type == 1)
    }
    plotData <- inner_join(subset, select(arrestData, 
                                          one_of("aid", "month", "day", 
                                                 "year")))
    plotData <- filter(plotData, 
                       year >= input$year[1],
                       year <= input$year[2], 
                       month >= input$month[1],
                       month <= input$month[2])
  
    nCharges <- group_by(plotData, charge) %>%
      summarize(n = n())
    nCharges$percent <- round(nCharges$n / sum(nCharges$n) * 100, 2)
    nCharges <- arrange(nCharges, desc(n)) %>% head(10)
    as.data.frame(nCharges)
  }, digits = 1)
  
  output$nArrests <- renderInfoBox({
    if (input$crimeType == "Alcohol") {
      subset <- select(chargeData, one_of("aid", "charge", 
                                          "etoh"))
      subset <- collect(subset)
      names(subset)[3] <- "type" 
    } else {
      subset <- select(chargeData, one_of("aid", "charge", 
                                          tolower(input$crimeType)))
      subset <- collect(subset)
      names(subset)[3] <- "type"
    }
    subset <- filter(subset, type == 1)
    plotData <- inner_join(subset, select(arrestData, 
                                          one_of("aid", "month", "day", "year")))
    plotData <- filter(plotData, 
                       year >= input$year[1],
                       year <= input$year[2], 
                       month >= input$month[1],
                       month <= input$month[2])
    n <- nrow(plotData)
    infoBox(
      "Arrests", n)
  })
  
  output$nPeople <- renderInfoBox({
    if (input$crimeType == "Alcohol") {
      subset <- select(chargeData, one_of("aid", "charge", 
                                          "etoh"))
      subset <- collect(subset)
      names(subset)[3] <- "type" 
    } else {
      subset <- select(chargeData, one_of("aid", "charge", 
                                          tolower(input$crimeType)))
      subset <- collect(subset)
      names(subset)[3] <- "type"
    }
    subset <- filter(subset, type == 1)
    plotData <- inner_join(subset, select(arrestData, 
                                          one_of("aid", "month", "year", 
                                                 "firstName", "lastName")))
    plotData <- filter(plotData, 
                       year >= input$year[1],
                       year <= input$year[2], 
                       month >= input$month[1],
                       month <= input$month[2])
    names <- unique(plotData[c("firstName", "lastName")])
    n <- nrow(names)
    infoBox(
      "People Arrested", n)
  })
  
  output$allPercent <- renderInfoBox({
    allN <- nrow(chargeData %>% 
                   collect() %>%
                   inner_join(collect(arrestData)) %>%
                   filter(year >= input$year[1],
                          year <= input$year[2], 
                          month >= input$month[1],
                          month <= input$month[2]))
    if (input$crimeType == "Alcohol") {
      subset <- select(chargeData, one_of("aid", "charge", 
                                          "etoh"))
      subset <- collect(subset)
      names(subset)[3] <- "type" 
    } else {
      subset <- select(chargeData, one_of("aid", "charge", 
                                          tolower(input$crimeType)))
      subset <- collect(subset)
      names(subset)[3] <- "type"
    }
    subset <- filter(subset, type == 1)
    plotData <- inner_join(subset, select(arrestData, 
                                          one_of("aid", "month", "day", "year")))
    plotData <- filter(plotData, 
                       year >= input$year[1],
                       year <= input$year[2], 
                       month >= input$month[1],
                       month <= input$month[2])
    nMonths <- nrow(unique(plotData[c("month", "year")]))
    n <- round(nrow(plotData) / allN * 100, 1)
    infoBox(
      "Percent of All Arrests", n)
  })
  
  output$map <- renderLeaflet({
    if (input$crimeType == "Alcohol") {
      subset <- select(chargeData, one_of("aid", "charge", 
                                          "etoh"))
      subset <- collect(subset)
      names(subset)[3] <- "type"
      subset <- filter(subset, type == 1)
      plotData <- inner_join(subset, select(arrestData, 
                                            one_of("aid", "lastName", "firstName",
                                                   "month", "day", "year", "lat", 
                                                   "lng")))
      plotData <- filter(plotData, !is.na(lat),
                         year >= input$year[1],
                         year <= input$year[2], 
                         month >= input$month[1],
                         month <= input$month[2])
      plotData$tag <- paste(plotData$firstName, " ", plotData$lastName, 
                            " arrested on ",
                            plotData$month, "-", plotData$day, "-",
                            plotData$year, " for ", plotData$charge, sep = "")  
    } else {
      subset <- select(chargeData, one_of("aid", "charge", 
                                          tolower(input$crimeType)))
      subset <- collect(subset)
      names(subset)[3] <- "type"
      subset <- filter(subset, type == 1)
      plotData <- inner_join(subset, select(arrestData, 
                                            one_of("aid", "lastName", "firstName",
                                                   "month", "day", "year", "lat", 
                                                   "lng")))
      plotData <- filter(plotData, !is.na(lat),
                         year >= input$year[1],
                         year <= input$year[2], 
                         month >= input$month[1],
                         month <= input$month[2])
      plotData$tag <- paste(plotData$firstName, " ", plotData$lastName, 
                            " arrested on ",
                            plotData$month, "-", plotData$day, "-", 
                            plotData$year, " for ", plotData$charge, sep = "")  
    }
    map <- leaflet() %>% 
      addTiles()  %>% 
      fitBounds(-91.593, 41.634, -91.483, 41.68) %>%
      addMarkers(data = plotData, popup = plotData$tag) 
    map
  })
  
  output$ggmapViolent <- renderPlot({
    data <- filter(chargeData, violent == 1) %>%
      collect() %>%
      inner_join(arrestData) %>% 
      filter(year >= 2008, !is.na(lat)) %>%
      select(one_of("lat", "lng"))
    
    icmap <- get_map(location = "iowa ciy", maptype = "roadmap", zoom = 12)
    icmap <- ggmap(icmap, extent = "device", legend = "none")
    icmap + stat_density2d(aes(x = lng, y = lat, fill = ..level..),
                           alpha = 0.5,
                           bins = 10, geom = "polygon",
                           data = data) +
      scale_fill_gradient(low = "black", high = "red") + 
      theme(legend.position="none") + 
      ggtitle("Violent Crime Arrests Distribution")
  })
  
  output$violentTs <- renderPlot({
    data <- filter(chargeData, violent == 1) %>%
      collect() %>%
      inner_join(arrestData) %>% 
      filter(year >= 2008, !is.na(lat)) %>%
      select(one_of("month", "day", "year"))
    data$days <- as.numeric(
      as.Date(paste(data$year, data$month, data$day, sep = "-")) - 
      as.Date(paste(data$year, "01", "01", sep = "-")))
    data$week <- data$year + floor(data$days / 7) / 52
    data <- group_by(data, week) %>%
      summarize(n = n())
    ggplot(data, aes(x = week, y = n)) + 
      geom_point() + 
      geom_line(alpha = 0.5) + 
      geom_smooth() + 
      ggtitle("Violent Crime Time Series") + 
      scale_x_continuous("") + 
      scale_y_continuous("Number Arrests for Violent Crimes / Week")
  })
  
  output$ggmapDrug <- renderPlot({
    data <- filter(chargeData, drug == 1) %>%
      collect() %>%
      inner_join(arrestData) %>% 
      filter(year >= 2008, !is.na(lat)) %>%
      select(one_of("lat", "lng"))
    
    icmap <- get_map(location = "iowa ciy", maptype = "roadmap", zoom = 12)
    icmap <- ggmap(icmap, extent = "device", legend = "none")
    icmap + stat_density2d(aes(x = lng, y = lat, fill = ..level..),
                           alpha = 0.5,
                           bins = 10, geom = "polygon",
                           data = data) +
      scale_fill_gradient(low = "black", high = "red") + 
      theme(legend.position="none") + 
      ggtitle("Drug Crime Arrests Distribution")
  })
  
  output$drugTs <- renderPlot({
    data <- filter(chargeData, drug == 1) %>%
      collect() %>%
      inner_join(arrestData) %>% 
      filter(year >= 2008, !is.na(lat)) %>%
      select(one_of("month", "day", "year"))
    data$days <- as.numeric(
      as.Date(paste(data$year, data$month, data$day, sep = "-")) - 
        as.Date(paste(data$year, "01", "01", sep = "-")))
    data$week <- data$year + floor(data$days / 7) / 52
    data <- group_by(data, week) %>%
      summarize(n = n())
    ggplot(data, aes(x = week, y = n)) + 
      geom_point() + 
      geom_line(alpha = 0.5) + 
      geom_smooth() + 
      ggtitle("Drug Crime Time Series") + 
      scale_x_continuous("") + 
      scale_y_continuous("Number Arrests for Drug Crimes / Week")
  })
  
  output$ggmapDUI <- renderPlot({
    data <- filter(chargeData, dui == 1) %>%
      collect() %>%
      inner_join(arrestData) %>% 
      filter(year >= 2008, !is.na(lat)) %>%
      select(one_of("lat", "lng"))
    
    icmap <- get_map(location = "iowa ciy", maptype = "roadmap", zoom = 12)
    icmap <- ggmap(icmap, extent = "device", legend = "none")
    icmap + stat_density2d(aes(x = lng, y = lat, fill = ..level..),
                           alpha = 0.5,
                           bins = 10, geom = "polygon",
                           data = data) +
      scale_fill_gradient(low = "black", high = "red") + 
      theme(legend.position="none") + 
      ggtitle("DUI Arrests Distribution")
  })
  
  output$duiTs <- renderPlot({
    data <- filter(chargeData, dui == 1) %>%
      collect() %>%
      inner_join(arrestData) %>% 
      filter(year >= 2008, !is.na(lat)) %>%
      select(one_of("month", "day", "year"))
    data$days <- as.numeric(
      as.Date(paste(data$year, data$month, data$day, sep = "-")) - 
        as.Date(paste(data$year, "01", "01", sep = "-")))
    data$week <- data$year + floor(data$days / 7) / 52
    data <- group_by(data, week) %>%
      summarize(n = n())
    ggplot(data, aes(x = week, y = n)) + 
      geom_point() + 
      geom_line(alpha = 0.5) + 
      geom_smooth() + 
      ggtitle("DUI Time Series") + 
      scale_x_continuous("") + 
      scale_y_continuous("Number Arrests for DUIs / Week")
  })
  
  output$ggmapEtoh <- renderPlot({
    data <- filter(chargeData, etoh == 1) %>%
      collect() %>%
      inner_join(arrestData) %>% 
      filter(year >= 2008, !is.na(lat)) %>%
      select(one_of("lat", "lng"))
    
    icmap <- get_map(location = "iowa ciy", maptype = "roadmap", zoom = 12)
    icmap <- ggmap(icmap, extent = "device", legend = "none")
    icmap + stat_density2d(aes(x = lng, y = lat, fill = ..level..),
                           alpha = 0.5,
                           bins = 10, geom = "polygon",
                           data = data) +
      scale_fill_gradient(low = "black", high = "red") + 
      theme(legend.position="none") + 
      ggtitle("Alcohol Arrests Distribution")
  })
  
  output$etohTs <- renderPlot({
    data <- filter(chargeData, etoh == 1) %>%
      collect() %>%
      inner_join(arrestData) %>% 
      filter(year >= 2008, !is.na(lat)) %>%
      select(one_of("month", "day", "year"))
    data$days <- as.numeric(
      as.Date(paste(data$year, data$month, data$day, sep = "-")) - 
        as.Date(paste(data$year, "01", "01", sep = "-")))
    data$week <- data$year + floor(data$days / 7) / 52
    data <- group_by(data, week) %>%
      summarize(n = n())
    ggplot(data, aes(x = week, y = n)) + 
      geom_point() + 
      geom_line(alpha = 0.5) + 
      geom_smooth() + 
      ggtitle("Alcohol Crime Time Series") + 
      scale_x_continuous("") + 
      scale_y_continuous("Number Arrests for Alcohol / Week")
  })
  
  output$ggmapAll <- renderPlot({
    data <- chargeData %>%
      collect() %>%
      inner_join(arrestData) %>% 
      filter(year >= 2008, !is.na(lat)) %>%
      select(one_of("lat", "lng"))
    
    icmap <- get_map(location = "iowa ciy", maptype = "roadmap", zoom = 12)
    icmap <- ggmap(icmap, extent = "device", legend = "none")
    icmap + stat_density2d(aes(x = lng, y = lat, fill = ..level..),
                           alpha = 0.5,
                           bins = 10, geom = "polygon",
                           data = data) +
      scale_fill_gradient(low = "black", high = "red") + 
      theme(legend.position="none") + 
      ggtitle("Arrests Distribution")
  })
  
  output$allTs <- renderPlot({
    data <- chargeData %>%
      collect() %>%
      inner_join(arrestData) %>% 
      filter(year >= 2008, !is.na(lat)) %>%
      select(one_of("month", "day", "year"))
    data$days <- as.numeric(
      as.Date(paste(data$year, data$month, data$day, sep = "-")) - 
        as.Date(paste(data$year, "01", "01", sep = "-")))
    data$week <- data$year + floor(data$days / 7) / 52
    data <- group_by(data, week) %>%
      summarize(n = n())
    ggplot(data, aes(x = week, y = n)) + 
      geom_point() + 
      geom_line(alpha = 0.5) + 
      geom_smooth() + 
      ggtitle("All Crime Time Series") + 
      scale_x_continuous("") + 
      scale_y_continuous("Number Arrests / Week")
  })
}


shinyApp(ui, server)
