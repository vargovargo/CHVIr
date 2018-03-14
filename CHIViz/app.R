library(shiny)
library(tidyverse)
library(markdown)
library(leaflet)
library(shinythemes)
library(ggthemes)
library(sf)


links <-read.csv(url("https://raw.githubusercontent.com/vargovargo/CHVIr/master/CHPRlinks.csv"), header=T)
CHVIdata <- read.csv(url("https://raw.githubusercontent.com/vargovargo/CHVIr/master/chviCountyTidy.csv"), header=T)

counties <- st_read("counties.geojson", stringsAsFactors = F) %>% st_transform(crs = 4326) 

# Define UI for application that draws a histogram
ui <- navbarPage(
  theme = shinytheme("flatly"),
  title = "CHVIz",
  tabPanel(
    "Select your data",
    # Create a new Row in the UI for selectInputs
    fluidRow(
      column(3,
             selectInput("cnty",
                         "County",
                         c(unique(as.character(CHVIdata$county)),"All"
                         ))
             ),
      column(3,
             selectInput("ind",
                         "Indicator",
                         c(unique(as.character(CHVIdata$def)),"All"
                         ))),
      column(3,
             uiOutput("chooseStrata")
           ),
      column(3,
             downloadButton(outputId = "downloadData", label = "Download"))
      
    ),
    wellPanel(plotOutput("plot")),
    fluidRow(column(6,
                    wellPanel(DT::dataTableOutput("table"))
                    ), 
             column(6,
                    leafletOutput("map")
                    ))
  ),
  tabPanel(title = "Explore", textOutput("summary")
          ),

  navbarMenu(
    "Documentation",
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
             )),
    tabPanel(tags$a(
      "CDPH CalBrace", "https://bit.ly/calbrace"
    )),
    tabPanel(title = "Summary", textOutput("path"))
  )
  
)

############ SERVER
server <- function(input, output, session) {
  path <- renderText(links$CHPR.Link[links$County == input$cnty])
  
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
        Mean = est) %>%
      mutate(selected = ifelse(County == input$cnty, "yes", "no"))
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
          Mean = est) %>%
        mutate(selected = ifelse(County == input$cnty, "yes", "no"))
    }) 
})
  
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
  

  # Filter data based on selections
  output$table <- DT::renderDataTable({DT::datatable(
    data58()  %>%
     select(
        County,
        Region,
        Definition,
        strata,
        Mean,
        LL95,
        UL95
        # Numerator,
        # Denominator
      )) %>% DT::formatRound(c(5:7), 2)
  })
  
  
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
        addLegend("bottomright",
          pal = pal,
          values = ~ Mean,
          opacity = 1,
          labFormat = labelFormat(),
          title = input$ind
        )
      
    }
    
  })
  
  output$plot <- renderPlot({
    
    if(input$ind == "All") {
      data58() %>%
        ggplot() +
        geom_bar(
          aes(
            x = reorder(County, Mean),
            y = Mean,
            fill = selected,
            alpha = selected
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
        theme_fivethirtyeight() +
        scale_color_fivethirtyeight()+ theme_update(axis.text.x = element_text(angle = 60, hjust = 1))
    } 
    else ({
    data58() %>%
      ggplot() +
      geom_bar(
        aes(
          x = reorder(County, Mean),
          y = Mean,
          fill = selected,
          alpha = selected
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
      theme_fivethirtyeight() +
      scale_color_fivethirtyeight()+ theme_update(axis.text.x = element_text(angle = 60, hjust = 1))
    })
  })
  
  output$summary <-
    renderText(paste0("You have selected ", input$cnty, " County"))
  
  
  output$downloadData <- downloadHandler(
    filename = function () {
      paste0("selectedCHVIdata_", input$cnty, ".csv")
    },
    
    content = function(file) {
      write.csv({data1()  %>%
          spread(key = metric, value = value) %>%
          rename(
            County = county,
            Region = climReg,
            Definition = def,
            Denominator = denmntr,
            Mean = est,
            Numerator = numratr
          ) %>%
          select(
            County,
            Region,
            Definition,
            strata,
            Mean,
            LL95,
            UL95
            # Numerator,
            # Denominator
          )}, file, row.names = F)
    }
    
  )
}

# Run the application
shinyApp(ui = ui, server = server)
