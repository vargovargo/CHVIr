#devtools::install_github('hadley/ggplot2')
library(shiny)
library(tidyverse)
library(markdown)
library(leaflet)
library(shinythemes)
library(ggthemes)
library(sf)
# library(plotly)
# library(ggplot2)


links <-read.csv("CHPRlinks.csv", header=T)
CHVIdata <- read.csv("chviCountyTidy.csv", header=T)
counties <- st_read("counties.geojson", stringsAsFactors = F) %>% st_transform(crs = 4326) 

##### Define UI for application that draws a histogram #####
ui <- navbarPage(
  theme = shinytheme("flatly"),
  title = "CHVIz",
  tabPanel(
    "Single County",
    # Create a new Row in the UI for selectInputs

    
#####  Select an County  #####       
    
    fluidRow(
      column(3,
             selectInput("cnty1",
                         "Select a County",
                         c(sort(unique(as.character(CHVIdata$county)))
                         ))
      ),
      column(3,
             p(uiOutput("downloadCHPR1"))
      ),
    wellPanel(plotOutput("plotCounty")))),
    
    
#####  Select an Indicator Tool  #####    
   

tabPanel(
  "Single Indicator",
 
    fluidRow(
      column(3,
             selectInput("cnty",
                         "Highlight County",
                         c(sort(unique(as.character(CHVIdata$county)))
                         ))
             ),
      column(3,
             selectInput("ind",
                         "Select an Indicator",
                         c("Projected number of extreme heat days",
                           "Three-year ozone concentration exceedance",
                           "Annual Mean Ambient Concentration of Fine Particulate Matter (PM2.5)",
                           "Population living in sea level rise inundation areas",
                           "Percent of population currently living in very high wildfire risk areas",
                           "Percent of population aged 65 years or older",
                           "Percent of population age less than 5 years",
                           "Number of Violent Crimes per 1,000 Population",
                           "Percent of population with a disability",
                           "High School or Greater Educational Attainment in the Population Aged 25 Years and Older",
                           "Percent of adults aged 18 - 64 without health insurance",
                           "Percent of households with no one aged > 14 years speaking English",
                           "Percent of population employed and aged > 16 working outdoors",
                           "Overall, concentrated, and child (0 to 18 years of age) poverty rate",
                           "Percent of households with no vehicle ownership",
                           "Percent of households with air conditioning",
                           "Percent without tree canopy coverage",
                           "Percent impervious surface cover"
                         ))),
      column(3,
             uiOutput("chooseStrata")
           ),
      column(3,
             downloadLink(outputId = "downloadData", label = "Download Selected Data"),
             p(uiOutput("downloadCHPR"))
             )
    
  ),
    wellPanel(plotOutput("plot")),
    fluidRow(column(6,
                    wellPanel(DT::dataTableOutput("table"))
                    ), 
             column(6,
                    wellPanel(leafletOutput("map"))
                    ))
),
  tabPanel(title = "Cummulative Vulnerability", 
           fluidRow(
             column(3,
                    selectInput("exposure",
                                "Exposure Indicator",
                                c("Projected number of extreme heat days",
                                  "Three-year ozone concentration exceedance",
                                  "Annual Mean Ambient Concentration of Fine Particulate Matter (PM2.5)",
                                  "Population living in sea level rise inundation areas",
                                  "Percent of population currently living in very high wildfire risk areas"
                                  ))
                                ),
             column(3,
                    selectInput("sensitivity",
                                "Sensitivity Indicator",
                                c("Percent of population aged 65 years or older",
                                  "Percent of population age less than 5 years",
                                  "Number of Violent Crimes per 1,000 Population",
                                  "Percent of population with a disability",
                                  "High School or Greater Educational Attainment in the Population Aged 25 Years and Older",
                                  "Percent of adults aged 18 - 64 without health insurance",
                                  "Percent of households with no one aged > 14 years speaking English",
                                  "Percent of population employed and aged > 16 working outdoors",
                                  "Overall, concentrated, and child (0 to 18 years of age) poverty rate",
                                  "Percent of households with no vehicle ownership"
                                )))#,
             # column(3,
             #        selectInput("capacity",
             #                    "Adaptive Capacity Indicator",
             #                    c("Percent of households with air conditioning",
             #                      "Percent without tree canopy coverage",
             #                      "Percent impervious surface cover"
             #                      )))
             ),
           fluidRow(
             column(8,wellPanel(plotOutput("triplePlot"))
             ), 
             column(4,
                    includeMarkdown("vulnerability.md"))
                         
           )
          ),


#####  About Page  ####

  navbarMenu(
    "About",
    tabPanel("About",
             fluidRow(
               column(
                 6,
                 includeMarkdown("about.md"),
                 tags$br(),
                 img(
                   class = "img-polaroid",
                   src = "https://www.cdph.ca.gov/Programs/OHE/PublishingImages/Policy%20Unit/CDPH-Climate-change-and-health-impacts-diagram.png",
                   width = 700,
                   alt = "Impact of Climate Change on Human Health and Exacerbation of Existing Inquities `(`Adapted from CDC, J. Patz`)`."
                 )
               )
             ))
  )
#####  Finish About  #####

)

##### SERVER #####
server <- function(input, output, session) {

averages <- CHVIdata %>%
    filter(metric =="est") %>%
    group_by(def, ind, strata) %>%
    summarise(stateAverage = mean(value, na.rm=T))
  
##### create reactive table for single indicator #####
  
  data58 <- reactive({

    if (!is.null(input$strt))
    {
      CHVIdata %>%
      mutate(COUNTYFI_1 = as.character(paste0("0",COUNTYFI_1))) %>%
      filter(def == input$ind & strata == input$strt ) %>%
      spread(key = metric, value = value) %>%
      rename(
        County = county,
        Region = climReg,
        Definition = def,
        Mean = est, 
        Numerator = numratr,
        Denominator = denmntr) %>%
      mutate(selCounty = ifelse(County == input$cnty, "yes", "no"),
             selRegion = ifelse(Region == CHVIdata$climReg[CHVIdata$county == input$cnty][1], "yes","no"))
    }
    
    else({
      CHVIdata %>%
        mutate(COUNTYFI_1 = as.character(paste0("0",COUNTYFI_1))) %>%
        filter(def == input$ind) %>%
        spread(key = metric, value = value) %>%
        rename(
          County = county,
          Region = climReg,
          Definition = def,
          Mean = est, 
          Numerator = numratr,
          Denominator = denmntr) %>%
          mutate(selCounty = ifelse(County == input$cnty, "yes", "no"),
                 selRegion = ifelse(Region == CHVIdata$climReg[CHVIdata$county == input$cnty][1], "yes","no"))
    }) 
 })
  
  
##### generate strata selection dropdown #####
  
  output$chooseStrata <- renderUI({
    selectInput("strt",
                "Strata",{
                  
               if(unique(
                 as.character({CHVIdata %>% filter(def == input$ind)}$strata)
               ) == "none")
                 return()
               else (unique(
                 as.character({CHVIdata %>% filter(def == input$ind)}$strata)
               ))
               
                }
            )
  })
  

##### generate table of the data #####
  
  output$table <- DT::renderDataTable({DT::datatable(
    data58()  %>%
     select(
        County,
        Region,
        Definition,
        strata,
        Mean,
        LL95,
        UL95,
        Numerator,
        Denominator
      )) %>% DT::formatRound(c(5:9), 1)
  })
  

 
##### generate map #####
  
  output$map <- renderLeaflet({
    if (input$ind != "All") {
  
       mapTemp <- left_join(counties, data58()) 
      
      pal <- colorQuantile(
        palette = "RdYlBu",
        n = 5,
        reverse = TRUE,
        domain = mapTemp$Mean
      )
      
      mapTemp %>%
        leaflet()  %>% 
        addTiles() %>%
        addPolygons(
          color = "#444444",
          weight = 1,
          fillOpacity = 0.6,
          fillColor = ~ pal(Mean),
          popup = paste0("This is ",mapTemp$County," County. The ",mapTemp$Definition," in this county is ",
                         round(mapTemp$Mean[mapTemp$County == mapTemp$County]),". The state average is ", round(mean(mapTemp$Mean, na.rm=T),2))
        ) %>%
        addLegend("topright",
          pal = pal,
          values = ~ Mean,
          opacity = 1,
          labFormat = labelFormat(),
          title = input$ind
        )
      
    }
    
  })
  
  
##### generate County Plot #####
  
  output$plotCounty <- renderPlot({
    
    CHVIdata %>% filter(county == input$cnty1 & metric == "est") %>% 
    left_join(averages) %>% 
    mutate(label = paste0(def," - ", strata),
           ratio = value/stateAverage,
           category = ifelse(ratio < 0.9, "below CA average",
                             ifelse(ratio > 1.1, "above CA average","around CA average"))) %>%
    ggplot() + 
    geom_bar(aes(x=reorder(label, ratio), y=ratio, fill=category), stat="identity") +
    coord_flip() +
    geom_hline(yintercept = 1, linetype="dashed") + 
    xlab("Indicator and Strata") +
    ylab("Ratio to State Average") +
    scale_fill_discrete(name="")
  
  
  })
  
  
  
  
  
##### generate plot #####
  
  output$plot <- renderPlot({
    
    if(input$ind == "All") {
      data58() %>%
        ggplot() +
        geom_bar(
          aes(
            x = reorder(County, Mean),
            y = Mean,
            fill = selRegion,
            alpha = selCounty
          ),
          stat = "identity",
          position = "dodge"
        ) +
        xlab(label = "Counties") + ylab(input$ind) + guides(fill = FALSE, alpha = FALSE) +
        geom_hline(
          yintercept = mean(data58()$Mean, na.rm = T),
          color = "black",
          alpha = 0.5
        ) +
        scale_alpha_discrete(range = c(0.2, 0.9)) +
        theme_update(axis.text.x = element_text(angle = 60, hjust = 1))
    } 
    else ({
    data58() %>%
      ggplot() +
      geom_bar(
        aes(
          x = reorder(County, Mean),
          y = Mean,
          fill = selRegion,
          alpha = selCounty
        ),
        stat = "identity",
        position = "dodge"
      ) +
      xlab(label = "Counties") + ylab(input$ind) + guides(fill = FALSE, alpha = FALSE) +
      geom_hline(
        yintercept = mean(data58()$Mean, na.rm = T),
        color = "black",
        alpha = 0.5
      ) +
      scale_alpha_discrete(range = c(0.3, 0.8)) +
      theme_update(axis.text.x = element_text(angle = 60, hjust = 1))+ 
        ggtitle(paste0(input$cnty," county and Region (blue) compared to others in the state (red) - with the state average (black line) for the selcted indicator - ",input$ind))
    })
  })
  
  output$summary <-
    renderText(paste0("This section is still under development. You have selected ", input$cnty, " County"))
  
##### Download the csv of the selected data  ######  
  output$downloadData <- downloadHandler(
    filename = function () {
      paste0("selectedCHVIdata.csv")
    },
    
    content = function(file) {
      write.csv({
        data58() %>%
          select(
            County,
            Region,
            Definition,
            strata,
            Mean,
            LL95,
            UL95
          )
        }, file, row.names = F)
    }
    
  )
  
##### Download the County Health Profile Report  ######  
  
  
  output$downloadCHPR <- renderUI({ 
    HTML(paste0("<a href =", links$CHPR.Link[links$County %in% c(input$cnty, paste0(input$cnty," "))],">Download County Health Profile</a>"))
  })
  
  
  # output$downloadCHPR2 <- downloadHandler(
  #   filename = function () {
  #     paste0(input$cnty,"CountyHealthProfileReport.pdf")
  #   },
  #   
  #   content = function(file) {
  #     file.copy(links$CHPR.Link[links$County %in% c(input$cnty, paste0(input$cnty," "))], file)
  #   }
  #   
  # )
  
  
  
  
  
  ##### make triple plot #####
  
  output$triplePlot <- renderPlot({
  
  
    tri <- {CHVIdata %>% 
    filter(def  == input$exposure & strata %in% c("2085", 2085,"none") & metric =="est") %>%
    mutate(expTer = ntile(value, 3)) %>%
    select(county, climReg, COUNTYFI_1, def, value, expTer) %>% 
    spread(key = def, value = value)
           } %>% left_join({
    
    CHVIdata %>% 
      filter(def  == input$sensitivity & strata %in% c("Overall","ViolentCrime","total","2006-2010","2009-2013","All Non-English","none") & metric =="est") %>%
      mutate(sensTer = ntile(value, 3)) %>%
      select(county, climReg, COUNTYFI_1, def, value, sensTer) %>% 
      spread(key = def, value = value)
    }) %>%  
      mutate(vulnerability = factor(sensTer + expTer)) 

  # left_join({
  #   
  #   CHVIdata %>% 
  #     filter(def  == input$capacity & strata %in% c("population-weighted", "none") & metric =="est") %>%
  #     select(county, climReg, COUNTYFI_1, def, value) %>% 
  #     spread(key = def, value = value)
  #   
  # }) %>% 
      
      tri %>% ggplot(aes_q(x = as.name(names(tri)[5]), 
                           y = as.name(names(tri)[7]),
                           size = as.name(names(tri)[8]), 
                           color = as.name(names(tri)[8])
                     )) +
        geom_point(alpha = 0.9) +
        guides(alpha = FALSE, color = FALSE, size = FALSE) + 
        scale_color_brewer(palette = "Spectral", direction = -1) +
        scale_size_discrete(range = c(3,15)) + 
        geom_text(aes_q(x = as.name(names(tri)[5]), 
                        y = as.name(names(tri)[7]), 
                        label = as.name(names(tri)[1])), size= 3, color="black") + 
        ggtitle("This plot displays the vulnerability to two factors. Counties in the top right corner (red) are in the top third of all counties for each.")
    
  })
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
