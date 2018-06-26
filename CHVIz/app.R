library(shiny)
library(tidyverse)
library(leaflet)
library(shinythemes)
library(ggthemes)
library(sf)
library(DT)
library(plotly)

links <- read.csv("CHPRlinks.csv", header = T)
CHVIdata <- readRDS("chviCountyTidyRace.RDS")
CHVItracts <- readRDS("chviTractTidy.RDS")
counties <-
  st_read("counties.geojson", stringsAsFactors = F)  %>% st_transform(crs = 4326)
tracts <-
  st_read("tracts.GeoJSON", stringsAsFactors = F) %>% st_transform(crs = 4326) %>%
  mutate(COUNTYFI_1 = as.character(paste0(STATE, COUNTY)))


CHVIdata$def <-
  ifelse(
    CHVIdata$def == "percent impervious surface cover",
    "Percent impervious surface cover",
    CHVIdata$def
  )
CHVIdata <- left_join(x = CHVIdata, y = {
  data.frame(
    def = c(
      "Percent of households without air conditioning",
      "Percent without tree canopy coverage",
      "Percent of population age less than 5 years",
      "Number of Violent Crimes per 1,000 Population",
      "Percent of population with a disability",
      "High School or Greater Educational Attainment in the Population Aged 25 Years and Older",
      "Percent of population aged 65 years or older",
      "Projected number of extreme heat days",
      "Percent impervious surface cover",
      "Percent of adults aged 18 - 64 without health insurance",
      "Percent of households with no one aged > 14 years speaking English",
      "Percent of population employed and aged > 16 working outdoors",
      "Three-year ozone concentration exceedance",
      "Annual Mean Ambient Concentration of Fine Particulate Matter (PM2.5)",
      "Overall, concentrated, and child (0 to 18 years of age) poverty rate",
      "Population living in sea level rise inundation areas",
      "Percent of households with no vehicle ownership",
      "Percent of population currently living in very high wildfire risk areas"
    ),
    defShort = c(
      "% HH w/o AC",
      "% w/o Tree Canopy",
      "% under 5",
      "Violent Crimes/1,000",
      "% with a Disability",
      "% w/ HS Education",
      "% over 65",
      "Extreme Heat Days",
      "% Impervious Surface",
      "% w/o Health Insurance",
      "% HH w/o English Speaker",
      "% outdoor workers",
      "O3 Concentration above Standard",
      "Annual Mean PM2.5 Concentration",
      "% in Poverty",
      "% in Sea Level Rise Risk Areas",
      "% HH w/o Vehicle",
      " % in Very High Wildfire Risk"
    )
  )
})


narratives <-
  data.frame(
    def = c(
      "Percent of households without air conditioning",
      "Percent without tree canopy coverage",
      "Percent of population age less than 5 years",
      "Number of Violent Crimes per 1,000 Population",
      "Percent of population with a disability",
      "High School or Greater Educational Attainment in the Population Aged 25 Years and Older",
      "Percent of population aged 65 years or older",
      "Projected number of extreme heat days",
      "Percent impervious surface cover",
      "Percent of adults aged 18 - 64 without health insurance",
      "Percent of households with no one aged > 14 years speaking English",
      "Percent of population employed and aged > 16 working outdoors",
      "Three-year ozone concentration exceedance",
      "Annual Mean Ambient Concentration of Fine Particulate Matter (PM2.5)",
      "Overall, concentrated, and child (0 to 18 years of age) poverty rate",
      "Population living in sea level rise inundation areas",
      "Percent of households with no vehicle ownership",
      "Percent of population currently living in very high wildfire risk areas"
    ),
    narrativeLink = c(
      "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/AirConditioning_797_Narrative_12-14-2016.pdf",
      "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/BRACE_TreeCanopy_458_Narrative_12-5-2016.pdf",
      "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/Children0to4_788_Narrative_11-8-2016.pdf",
      "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/HCI_Crime_752-Narrative_Examples-10-30-15.pdf",
      "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/BRACE_Disability_Narrative_795_11-16-2016.pdf",
      "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/Educ_attain_HS_Narrative_Examples4-28-13.pdf",
      "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/Elderly_789_Narrative_11-9-2016.pdf",
      "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/BRACE_ExtremeHeat_Narrative_03-29-2017.pdf",
      "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/ImperviousSurfaces_423_Narrative_12-2-2016.pdf",
      "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/BRACE_Insurance_187_Narrative_11-29-2016.pdf",
      "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/BRACE_LinguisticIsolation_Narrative_11-15-2016.pdf",
      "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/BRACE_OutdoorsWorkers_Narrative_790_12-5-2016.pdf",
      "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/BRACE_Ozone_801_Narrative_11-8-2016.pdf",
      "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/BRACE_PM25_776_Narrative_8-1-2017.pdf",
      "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/HCI_PovertyRate_754_Narrative_Examples11-5-13rev3-12-14.pdf",
      "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/Sealevelrise_Narrative_11-1-2016.pdf",
      "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/CarOwnership_37_Narrative_9-6-16.pdf",
      "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/WildfireZone_786_Narrative_11-8-2016.pdf"
    )
  )



##### Define UI for application that draws a histogram #####
ui <-  fluidPage(
  div(style="background-color:#FEFEFE;padding: 1px 0px;height: 0px",
      titlePanel(
        title="",
        windowTitle="CHVIz"
      )
  ),
  
  navbarPage(position = "fixed-top", 
             header = tags$style(type="text/css", "body {padding-top: 70px;}"), 
             theme = shinytheme("flatly"),
             title = div("CCHVIz",a(href="https://www.cdph.ca.gov/Programs/OHE/Pages/CCHEP.aspx" 
                                   ,img(src="https://raw.githubusercontent.com/vargovargo/CHVIr/master/CHVIz/images/CDPHLogo.gif", height= "45", style = "position: relative; top: -12px; right: 0 px;")
             )),
            
             
             tabPanel("About",
                      fluidRow(
                          includeMarkdown("about.md")
                      )
                      ),
             
             tabPanel(title = "Vulnerability", 
                      fluidRow(
                        column(8,
                               includeMarkdown("vulnerability.md"
                               )),  
                        column(2,
                               selectInput("exposure",
                                           "Exposure Indicator",
                                           c("Projected number of extreme heat days",
                                             "Three-year ozone concentration exceedance",
                                             "Annual Mean Ambient Concentration of Fine Particulate Matter (PM2.5)",
                                             "Population living in sea level rise inundation areas",
                                             "Percent of population currently living in very high wildfire risk areas"
                                           )),
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
                                             "Percent of households with no vehicle ownership",
                                             "Percent of households without air conditioning",
                                             "Percent without tree canopy coverage",
                                             "Percent impervious surface cover"
                                           ))
                                ),
                        column(2,
                               img(
                                 class = "img-polaroid",
                                 src = "https://raw.githubusercontent.com/vargovargo/CHVIr/master/CHVIz/images/vulnerabilityLegend.png",
                                 alt = "Vulnerability Bivariate Legend"
                               )
                        )
                        
                      ),
                      
                      fluidRow(
                        column(8,wellPanel(plotlyOutput("triplePlot",height = "600px"),
                                           downloadLink(outputId = "downloadVulnerabilityFigure",label = "Download the data in this figure")
                                           )
                        ), 
                        column(4,wellPanel(leafletOutput("vulnMap",height = "600px"),
                                           downloadLink(outputId = "downloadVulnerabilityMap",label = "Download the data in this Map")))
                      )),
             
             tabPanel("County Snapshot",
                      # Create a new Row in the UI for selectInputs
                      
                      
                      #####  Select an County  #####       
                      
                      fluidRow(
                        column(8, includeMarkdown("countyPlot.md")),
                        column(2,
                               selectInput("cnty1",
                                           "Select a County",
                                           c(sort(unique(as.character(CHVIdata$county)))
                                           )),
                               p(uiOutput("downloadCHPR1"))
                        )),
                      wellPanel(plotlyOutput("plotCounty",height = "600px"),
                                downloadLink(outputId = "downloadCountySnapshot",label = "Download the data in this figure")),
                      wellPanel(DT::dataTableOutput("countyTable"))
                      
             ),
         
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
                                      "Percent of households without air conditioning",
                                      "Percent without tree canopy coverage",
                                      "Percent impervious surface cover"
                                    ))),
                 column(3,
                        uiOutput("chooseStrata")
                 ),
                 column(3, 
                        p(uiOutput("downloadNarrative"))
                 )
                 
                 
               ),
               
               fluidRow(column(8,
                               wellPanel(leafletOutput("map", height = "600px"),
                                         downloadLink(outputId = "downloadSingleIndicatorMap",label = "Download the data in this Map")
                                         )
               ), 
               column(4,
                      wellPanel(plotlyOutput("plot", height = "600px"),
                                downloadLink(outputId = "downloadSingleIndicator",label = "Download the data in this figure")
                                )  
               )),
               wellPanel(DT::dataTableOutput("table"))
             ),

             tabPanel("Query the Data",
                      fluidRow(
                        column(3,
                               selectInput("cntyDNLD",
                                           "Select a County",
                                           c("All",sort(unique(as.character(CHVIdata$county)))
                                           ))
                        ),
                        column(3,
                               selectInput("indDNLD",
                                           "Select an Indicator",
                                           c("All",
                                             "Projected number of extreme heat days",
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
                                             "Percent of households without air conditioning",
                                             "Percent without tree canopy coverage",
                                             "Percent impervious surface cover"
                                           ))),
                        # column(2,
                        #         selectInput("raceDNLD",
                        #                     "Race",
                        #                     c("Total",
                        #                       "AIAN",
                        #                       "Asian",
                        #                       "AfricanAm",
                        #                       "Latino",
                        #                       "NHOPI",
                        #                       "White",
                        #                       "Multiple",
                        #                       "Other")
                        #  )),  
                        # column(2,
                        #              uiOutput("chooseStrataDNLD") # this criteria and race were creating problems in the queries
                        #  ), 
                        column(3,
                               p(),
                               downloadButton(outputId = "downloadData", label = "Download Selected Data")
                        )
                        
                        # ,
                        # 
                        # column(3,
                        #        p(),
                        #        downloadButton(outputId = "downloadSpatial", label = "Download Spatial Data")
                        # )
                      ),             
                      fluidRow(
                        wellPanel(DT::dataTableOutput("downloadTable"))
                      )),
             
             
             #####  Additional Page  ####
             
             navbarMenu(
               "How to Use",
              tabPanel("The Vulnerability Page",
                       fluidRow(
                         column(4,
                                includeMarkdown("howToVuln.md"))
                                
                         )
                         )
                       )
  #####  Finish Additional  #####
             
  )
)

##### SERVER #####
server <- function(input, output, session) {
  
  averages <- CHVIdata %>%
    group_by(def, ind, strata) %>%
    summarise(stateAverage = mean(est, na.rm=T))
  
  
  #####  reactive table (tab 1 - single county) #####
  
  data.tab1 <- eventReactive(input$cnty1,{
    
    CHVIdata %>% filter(county == input$cnty1 &  race == "Total") %>% 
      left_join(averages) %>% 
      rename(County = county, 
             Region = climReg, 
             Indicator = def, 
             Strata = strata,
             Value = est, 
             CA_avg = stateAverage
      ) %>%
      mutate(label = paste0(defShort, " - ", Strata),
             ratio = ifelse(is.na(Value), 0, Value/CA_avg),
             Category = ifelse(ratio < 0.9, "below CA average",
                               ifelse(ratio > 1.1, "above CA average","around CA average")),
             fillColor = ifelse(ratio < 0.9, "#F2F1E6",
                                ifelse(ratio > 1.1, "#685DA9","#9198AA")))
  })
  
  
  ##### Download the csv of (tab 1 - single county)  ######  
  output$downloadCountySnapshot <- downloadHandler(
    filename = function () {
      paste0("countySnapshotFigure.csv")
    },
    
    content = function(file) {
      write.csv(data.tab1(), file, row.names = F)
    }
    
  )
  
  
  
  
  ##### generate County (tab 1 - single county) Plot #####
  
  output$plotCounty <- renderPlotly({
    
    tab1.df <- data.tab1()
    
    plot_ly( 
      data = tab1.df,
      x =  ~ round(ratio,2),
      y =  ~ reorder(label, ratio),
      marker = list(color = tab1.df[["fillColor"]],
                    line = list(color = "#404040", width=.5)
      ),
      type = "bar",
      hoverinfo = 'text',
      text = ~paste('</br>', paste(tab1.df[["Indicator"]]," - ",tab1.df[["Strata"]]),
                    '</br> County Value:', round(tab1.df[["Value"]],2),
                    '</br> State Average:', round(tab1.df[["CA_avg"]],2)),
      
      showlegend = FALSE
      ) %>%
      layout(title = paste0('County Snapshot for ',tab1.df[["County"]], ' County -   \n (shows how the values in the county compare to the state average)' ),
             margin = list(l = 300,
                           t = 70),
             xaxis = list(
               title = "Ratio to State Average",
               size = 4,
               autotick = TRUE,
               ticks = "outside",
               tick0 = 0,
               dtick = 1,
               ticklen = 5,
               tickwidth = 2,
               tickcolor = toRGB("black")
             ),
             yaxis = list(title = "Indicator and Strata", 
                          type = "category", 
                          dtick=1, 
                          size=2), 
             shapes = list(
               list(
               type = "rect", 
               fillcolor = "#F2F1E6",
               line = list(color = "#F2F1E6"),
               opacity = .1,
               y0 = 0, 
               y1 = 1, 
               yref = "paper",
               x0 = 0, 
               x1 = 0.9 
             ), 
             list(
               type = "rect", 
               fillcolor = "#9198AA",
               line = list(color = "#9198AA"),
               opacity = .1,
               y0 = 0, 
               y1 = 1, 
               yref = "paper",
               x0 = 0.9, 
               x1 = 1.1 
             ),
             list(
               type = "rect", 
               fillcolor = "#685DA9",
               line = list(color = "#685DA9"),
               opacity = .1,
               y0 = 0, 
               y1 = 1, 
               yref = "paper",
               x0 = 1.1, 
               x1 = max(tab1.df[["ratio"]]) 
             ))
      )  %>%
      config(collaborate = FALSE,
             displaylogo = FALSE,
             modeBarButtonsToRemove = list(
               'toggleSpikelines',
               'sendDataToCloud',
               'hoverCompareCartesian',
               'zoom2d',
               'pan2d',
               'select2d',
               'lasso2d',
               'zoomIn2d',
               'zoomOut2d',
               'autoScale2d',
               'resetScale2d',
               'hoverClosestCartesian'
             )
      )
      
    # ggplot() + 
    # geom_bar(aes(x=reorder(label, ratio), y=ratio, fill=category), stat="identity") +
    # coord_flip() +
    # geom_hline(yintercept = 1, linetype="dashed") + 
    # xlab("Indicator and Strata") +
    # ylab("Ratio to State Average") +
    # scale_fill_discrete(name="")
  
  
  })
  
  
##### generate County (tab 1 - single county) Table ######  
  
  output$countyTable <- DT::renderDataTable({DT::datatable(
    
    data.tab1() %>%
      select(County, Region, Indicator, Strata, Value, CA_avg, Category), 
    options=list(pageLength = 25)
  )  %>% DT::formatRound(c(5,6), 1)
    
  })
  
  
  
##### generate strata selection dropdown #####
  
  output$chooseStrata <- renderUI({
    selectInput("strt",
                "Strata",{
                  
                  unique(
                    as.character({CHVIdata %>% filter(def == input$ind)}$strata)
                  )
                  
                }
    )
  })
  
  
  
##### create reactive table for single indicator #####
  
  data.tab2 <- reactive({
    
    CHVIdata %>%
      mutate(COUNTYFI_1 = as.character(paste0("0",COUNTYFI_1))) %>%
      filter(def == input$ind & strata == input$strt & race == "Total") %>%
      rename(
        County = county,
        Region = climReg,
        Definition = def,
        Mean = est, 
        Numerator = numratr,
        Denominator = denmntr) %>%
      mutate(selCounty = ifelse(County == input$cnty, "yes", "no"),
             selRegion = ifelse(Region == CHVIdata$climReg[CHVIdata$county == input$cnty][1], 
                                paste0("In ",CHVIdata$climReg[CHVIdata$county == input$cnty][1]," region"),
                                "Outside region"),
             countyColor = ifelse(County == input$cnty,"rgba(104,93,169, 1)", 
                                  ifelse(Region == CHVIdata$climReg[CHVIdata$county == input$cnty][1],
                                         "rgba(145,152,170, 0.5)",
                                         "rgba(242,241,230, 0.3)")))
    
  })
  
  
  ##### Download the csv of (tab 2 - single indicator)  ######  
  output$downloadSingleIndicator <- downloadHandler(
    filename = function () {
      paste0("singleIndicatorFigure.csv")
    },
    
    content = function(file) {
      write.csv(data.tab2(), file, row.names = F)
    }
    
  )
  
  
##### generate table of the data (tab 2 - single indicator) #####
  
  output$table <- DT::renderDataTable({DT::datatable(
    data.tab2()  %>%
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

  

 
##### Census tract data (tab 2 - single indicator) ######
  
 tractData <- reactive({
   
   CHVItracts %>% 
     filter(def == input$ind & strata == input$strt)  %>%
     mutate(ct10 = as.character(paste0('0',ct10))) 
  
 })
  
 
selectedFIPS <- eventReactive(input$cnty, {
   
   as.character(paste0(CHVIdata$COUNTYFI_1[CHVIdata$county == input$cnty][1]))
   
})


average <- eventReactive(c(input$ind, input$strt), {
  
  as.numeric({averages %>% filter(def == input$ind & strata == input$strt) %>%
      ungroup() %>% select(stateAverage)}[1])
})
  
##### generate map (tab 2 - single indicator) #####
  
  output$map <- renderLeaflet({
    
      mapTemp <- tracts %>% 
        filter(COUNTYFI_1 == selectedFIPS()) %>%
        left_join(tractData()) 
      
      # countyTemp <- left_join(counties, data.tab2())
      
      pal <- colorQuantile(
        palette = c("#685DA9",
                    "#CB6F6B", 
                    "#EFA96E",  
                    "#2A8CC5",
                    "#F2F1E6") ,
        n = 5,
        reverse = TRUE,
        domain =  mapTemp$est
      )
      
      pal2 <- colorQuantile(
        palette =  c("#685DA9",
                     "#CB6F6B", 
                     "#EFA96E",  
                     "#2A8CC5",
                     "#F2F1E6"),
        n = 5,
        reverse = TRUE,
        domain = tractData()$est
    
      )
      
      mapTemp %>%
        leaflet()  %>% 
        addProviderTiles(providers$CartoDB.Positron) %>% 
        addPolygons(
          color = "#444444",
          weight = 1,
          smoothFactor = 0.1,
          fillOpacity = 0.6,
          fillColor = ~ pal(est),
          highlightOptions = highlightOptions(color = "white", weight = 2,
                                              bringToFront = TRUE),
          popup = paste0("This is tract ", mapTemp$ct10, " in ",mapTemp$county," County. The ",mapTemp$def," in this tract is ",
                         round(mapTemp$est,1),". The county average is ", round(mean(mapTemp$est, na.rm=T),1),
                         ". The state average is ", round(average(),1)),
          group="Tract Qunitiles") %>%
        addPolygons(
          color = "#444444",
          weight = 1,
          smoothFactor = 0.1,
          fillOpacity = 0.6,
          fillColor = ~ pal2(est),
          highlightOptions = highlightOptions(color = "white", weight = 2,
                                              bringToFront = TRUE),
          popup = paste0("This is tract ", mapTemp$ct10, " in ",mapTemp$county," County. The ",mapTemp$def," in this tract is ",
                         round(mapTemp$est,1),". The county average is ", round(mean(mapTemp$est, na.rm=T),1),
                         ". The state average is ", round(average(),1)),
          group="State Quintiles") %>%
          addLayersControl(
            baseGroups = c("Tract Qunitiles", "State Quintiles"),
            options = layersControlOptions(collapsed = TRUE)
          ) %>% 
          
        addLegend("bottomleft",
                  pal = pal,
                  values = ~ na.exclude(mapTemp$est),
                  opacity = .4,
                  title = input$ind
        )

    
  })


##### Download the csv of (tab 2 - single indicator map)  ######  
output$downloadSingleIndicatorMap <- downloadHandler(
  filename = function () {
    paste0("singleIndicatorMapData.csv")
  },
  
  content = function(file) {
    write.csv({
      tractData() %>%
        filter(county == input$cnty)
      }, file, row.names = F)
  }
  
)


  

######   county map for (tab 2 - single indicator) ######
##### generate map (tab 2) #####

# output$map <- renderLeaflet({
#  
#     mapTemp <- left_join(counties, data.tab2()) 
#     
#     pal <- colorQuantile(
#       palette = "RdYlBu",
#       n = 5,
#       reverse = TRUE,
#       domain = mapTemp$Mean
#     )
#     
#     mapTemp %>%
#       leaflet()  %>% 
#       addTiles() %>%
#       addPolygons(
#         color = "#444444",
#         weight = 1,
#         smoothFactor = 0.1,
#         fillOpacity = 0.6,
#         fillColor = ~ pal(Mean),
#         highlightOptions = highlightOptions(color = "white", weight = 2,
#                                             bringToFront = TRUE),
#         label = ~ (mapTemp$County),
#         popup = paste0("This is ",mapTemp$County," County. The ",mapTemp$Definition," in this county is ",
#                        round(mapTemp$Mean[mapTemp$County == mapTemp$County]),". The state average is ", round(mean(mapTemp$Mean, na.rm=T),2))
#       ) %>%
#       addLegend("topright",
#                 pal = pal,
#                 values = ~ Mean,
#                 opacity = 1,
#                 labFormat = labelFormat(),
#                 title = input$ind 
#       ) %>%
#       clearControls()
#   
#   
# })






  
##### generate plot (tab 2 - single indicator) #####
  

  output$plot <- renderPlotly({
    
    tab2.df <- data.tab2()
    
   plot_ly(
      data = tab2.df,
      x =  ~ round(Mean, 2),
      y =  ~ reorder(County, Mean),
      marker = list(color = tab2.df[["countyColor"]],
                    line = list(color = "#404040", width=.5)
      ),
      type = "bar",
      hoverinfo = 'text',
      text = ~paste('</br> County:',tab2.df[["County"]],
                    '</br> Region:', tab2.df[["Region"]],
                    '</br> Indicator:', round(tab2.df[["Mean"]],2), tab2.df[["defShort"]],
                    '</br> State Average:', round(averages$stateAverage[averages$def == input$ind & averages$strata == input$strt],2)),
      showlegend = FALSE
    ) %>%
      layout(
        title = paste0(input$ind, "\n for California Counties \n (",input$cnty," county [dark], region [light], CA avg [dotted])"),
        margin = list(l = 130,
                      t = 105),
        xaxis = list(
          title = ifelse(input$strt == "none", input$ind,paste0(input$ind," - ", input$strt)),
          autotick = TRUE,
          ticks = "outside",
          tick0 = 0,
          dtick = 0.25,
          ticklen = 5,
          tickwidth = 2,
          tickcolor = toRGB("black")
        ),
        yaxis = list(title = "Counties",
                     type = "categorical",
                     dtick=1,
                     tickfont=list(
                       size=8
                     )
                     ), 
        shapes = list(
          type = "line", 
          y0 = 0, 
          y1 = 1, 
          yref = "paper",
          x0 = averages$stateAverage[averages$def == input$ind & averages$strata == input$strt], 
          x1 = averages$stateAverage[averages$def == input$ind & averages$strata == input$strt], 
          line = list(color = "black", dash = "dot"), opacity = .5
        )
      ) %>%
      config(collaborate = FALSE,
             cloud = FALSE,
             displaylogo = FALSE,
        modeBarButtonsToRemove = list(
          'toggleSpikelines',
          'sendDataToCloud',
          'hoverCompareCartesian',
          'zoom2d',
          'pan2d',
          'select2d',
          'lasso2d',
          'zoomIn2d',
          'zoomOut2d',
          'autoScale2d',
          'resetScale2d',
          'hoverClosestCartesian'
        )
      )

    
  })
  

##### make vulnerability table #####

triple <- reactive({
 
   foo <- {CHVIdata %>% 
      filter(def  == input$exposure & strata %in% c("2085", 2085, "none") & race == "Total") %>%
      mutate(expTer = ntile(est, 3)) %>%
      select(county, climReg, COUNTYFI_1, def, est, expTer) %>% 
      spread(key = def, value = est)
  } %>% left_join({
    
    CHVIdata %>% 
      filter(def  == input$sensitivity & strata %in% c("Overall","ViolentCrime","total","2006-2010","2009-2013","All Non-English","none", "population-weighted") & race =="Total") %>%
      mutate(sensTer = ntile(est, 3)) %>%
      select(county, climReg, COUNTYFI_1, def, est, sensTer) %>% 
      spread(key = def, value = est) %>% 
      left_join({
        CHVIdata %>%
          filter(def  == "Percent of population aged 65 years or older" & race == "Total") %>%
          select(county, denmntr)
      }) %>%
      rename(Population = denmntr)
  }) %>%  
     mutate(Population = as.numeric(as.character(Population)),
            vulnerability = factor(ifelse(expTer == 1 & sensTer == 1, "Low Exposure, Low Sensitivity",
                                          ifelse(expTer == 1 & sensTer == 2, "Low Exposure, Medium Sensitivity",
                                                 ifelse(expTer == 1 & sensTer == 3, "Low Exposure, High Sensitivity",
                                                        ifelse(expTer == 2 & sensTer == 1, "Medium Exposure, Low Sensitivity",
                                                               ifelse(expTer == 3 & sensTer == 1, "High Exposure, Low Sensitivity",
                                                                      ifelse(expTer == 2 & sensTer == 2, "Medium Exposure, Medium Sensitivity",
                                                                             ifelse(expTer == 2 & sensTer == 3, "Medium Exposure, High Sensitivity",
                                                                                    ifelse(expTer == 3 & sensTer == 2, "High Exposure, Medium Sensitivity","High Exposure, High Sensitivity"
                                                                                    )))))))), levels = c("Low Exposure, Low Sensitivity",
                                                                                                         "Low Exposure, Medium Sensitivity",
                                                                                                         "Medium Exposure, Low Sensitivity",
                                                                                                         "Medium Exposure, Medium Sensitivity",
                                                                                                         "High Exposure, Low Sensitivity",
                                                                                                         "Low Exposure, High Sensitivity",
                                                                                                         "High Exposure, Medium Sensitivity",
                                                                                                         "Medium Exposure, High Sensitivity",
                                                                                                         "High Exposure, High Sensitivity"
                                                                                    )),
            sign = ifelse(expTer == 1 & sensTer == 1, "rgba(242,241,230, 0.5)",
                          ifelse(expTer == 1 & sensTer == 2,"rgba(42,140,197, 0.7)",
                                 ifelse(expTer == 1 & sensTer == 3, "rgba(51,101,147, 0.8)",
                                        ifelse(expTer == 2 & sensTer == 1, "rgba(239,169,110,.7)",
                                               ifelse(expTer == 3 & sensTer == 1, "rgba(203,111,107,0.8)",
                                                      ifelse(expTer == 2 & sensTer == 2, "rgba(145,152,170,0.7)",
                                                             ifelse(expTer == 2 & sensTer == 3, "rgba(149,123,152,0.9)",
                                                                    ifelse(expTer == 3 & sensTer == 2, "rgba(137,136,164,0.9)","rgba(104,93,169,1)"
                                                                    )))))))),
            size = ntile(Population,29)
     )
   
  foo <- na.omit(foo)

})




##### Download the csv of (vulnerability tab)  ######  
output$downloadVulnerabilityFigure <- downloadHandler(
  filename = function () {
    paste0("vulnerabilityFigure.csv")
  },
  
  content = function(file) {
    write.csv(triple(), file, row.names = F)
  }
  
)


##### Download the csv of (vulnerability tab)  ######  
output$downloadVulnerabilityMap <- downloadHandler(
  filename = function () {
    paste0("vulnerabilityMap.csv")
  },
  
  content = function(file) {
    write.csv(triple(), file, row.names = F)
  }
  
)


##### make triple plot (tab 3) #####
  
  output$triplePlot <- renderPlotly({
    
    tri <- triple()
    
     plot_ly(
      data = tri,
      x =  ~ round(tri[[5]],2),
      y =  ~ round(tri[[7]],2),
      hoverinfo = 'text',
      text = ~paste('</br> County:',tri[["county"]],
                    '</br> Population:',format(tri[["Population"]], big.mark = ","),
                    '</br> Exposure:', round(tri[[5]],2), names(tri)[5],
                    '</br> Sensitivity:', round(tri[[7]],2),  names(tri)[7]),
      showlegend = FALSE
    ) %>%
      add_markers(type = 'scatter', 
                  mode = 'markers',
                  size = ~tri[["Population"]]+50,
                  marker = list(color = tri[["sign"]], 
                                size = tri[["size"]]*25,   
                                line = list(color = 'rgba(99,99,99, .8)',width = 0.5))) %>%
      add_text(type = 'scatter',mode = 'text', text = tri[["county"]], textposition = 'top right',
                textfont = list(
                  size = 10,
                  color = toRGB("grey40"))) %>%
      layout(title = paste0('Combined Vulnerability from Exposure (',names(tri)[5], ')    \n and Sensitivity (',names(tri)[7],")") ,
             margin = list(l = 50,
                           t = 70),
             xaxis = list(
               title = names(tri)[5],
               autotick = TRUE,
               ticks = "outside",
               tick0 = 0,
               dtick = 0.25,
               ticklen = 5,
               tickwidth = 2,
               tickcolor = toRGB("black")
             ),
             yaxis = list(title = names(tri)[7],
                          autotick = TRUE,
                          ticks = "outside",
                          tick0 = 0,
                          dtick = 0.25,
                          ticklen = 5,
                          tickwidth = 2,
                          tickcolor = toRGB("black"))
      ) %>%
      config(collaborate = FALSE,
             displaylogo = FALSE,
             modeBarButtonsToRemove = list(
               'toggleSpikelines',
               'sendDataToCloud',
               'hoverCompareCartesian',
              'hoverClosestCartesian'
             )
      )
    
  })
  
  
##### generate map (vulnerability Tab) #####
#### Attention

output$vulnMap <- renderLeaflet({
      
      mapTemp2 <- left_join(counties, triple()) %>% rename(County = county)
      
      pal <- colorFactor(c("#F2F1E6", 
                           "#2A8CC5", 
                           "#EFA96E",  
                           "#9198AA", 
                           "#CB6F6B",
                           "#336593",
                           "#8988A4",
                           "#957B98",
                           "#685DA9"), mapTemp2$vulnerability)
      
      
      mapTemp2 %>%
        leaflet()  %>% 
        addProviderTiles(providers$CartoDB.Positron) %>% 
        addPolygons(
          color = "#444444",
          weight = 1,
          smoothFactor = 0.1,
          fillOpacity = 0.6,
          fillColor =  ~pal(vulnerability),
          highlightOptions = highlightOptions(color = "white", weight = 2,
                                              bringToFront = TRUE),
          label = ~ (County),
          popup = paste0("This is ",mapTemp2$County," County. </br> Its vulnerability is defined by ", mapTemp2$vulnerability, 
                    '</br> Exposure: ', round(triple()[[5]],2), ' ', names(triple())[5],
                         '</br> Sensitivity: ', round(triple()[[7]],2), ' ',  names(triple())[7])) %>%
        addLegend("topright",
                  pal = pal,
                  values = ~ vulnerability,
                  opacity = 1,
                  labFormat = labelFormat(),
                  title = input$ind 
        ) %>%
        clearControls()
      
    })
  



  data.dnld <- eventReactive(c(input$cntyDNLD, input$indDNLD),{
    
    if(input$cntyDNLD  == "All" & input$indDNLD != "All") {
      
      CHVIdata %>%
        mutate(COUNTYFI_1 = as.character(paste0("0",COUNTYFI_1))) %>%
        filter(def == input$indDNLD) %>%
        rename(
          County = county,
          Region = climReg,
          Definition = def,
          Mean = est, 
          Numerator = numratr,
          Denominator = denmntr,
          Strata = strata, 
          Race = race)
    } else { if(input$cntyDNLD  != "All" & input$indDNLD != "All") {
      
      CHVIdata %>%
        mutate(COUNTYFI_1 = as.character(paste0("0",COUNTYFI_1))) %>%
        filter(def == input$indDNLD & county == input$cntyDNLD) %>%
        rename(
          County = county,
          Region = climReg,
          Definition = def,
          Mean = est, 
          Numerator = numratr,
          Denominator = denmntr,
          Strata = strata, 
          Race = race)
    } else { if(input$cntyDNLD  != "All" & input$indDNLD == "All") {
      
      CHVIdata %>%
        mutate(COUNTYFI_1 = as.character(paste0("0",COUNTYFI_1))) %>%
        filter(county == input$cntyDNLD) %>%
        rename(
          County = county,
          Region = climReg,
          Definition = def,
          Mean = est, 
          Numerator = numratr,
          Denominator = denmntr,
          Strata = strata, 
          Race = race)
    } else {   
      
      CHVIdata %>%
        mutate(COUNTYFI_1 = as.character(paste0("0",COUNTYFI_1))) %>%
        rename(
          County = county,
          Region = climReg,
          Definition = def,
          Mean = est, 
          Numerator = numratr,
          Denominator = denmntr,
          Strata = strata, 
          Race = race)
    } } }
    
  })
  
   
##### Download the csv of the selected data (download tab)  ######  
  output$downloadData <- downloadHandler(
    filename = function () {
      paste0("selectedCHVIdata.csv")
    },
    
    content = function(file) {
      write.csv({
        data.dnld() %>%
          select(
            County,
            Region,
            Definition,
            Strata,
            Race,
            Mean,
            LL95,
            UL95,
            Numerator,
            Denominator
          )
        }, file, row.names = F)
    }
    
  )
  
  
  # ##### Download the kml of the selected data (download tab)  ######  
  # output$downloadSpatial <- downloadHandler(
  #   filename = function () {
  #     paste0("selectedCHVIdata.kml")
  #   },
  #   
  #   content = function(file) {
  #    
  #     left_join(counties, data.dnld()) %>% st_transform(crs = 4326) %>% 
  #       st_write(dsn = file)
  #     
  #   }
  #   
  # )
  
  
  
  ##### generate (download tab) Table ######  
  
  output$downloadTable <- DT::renderDataTable({DT::datatable(
    
    data.dnld() %>%
      select(
        County,
        Region,
        Definition,
        Strata,
        Race,
        Mean,
        LL95,
        UL95,
        Numerator,
        Denominator
      ), 
    options=list(pageLength = 25)
  )  %>% DT::formatRound(c(6:8), 1) %>%
      DT::formatRound(c(9:10), 0)
    
  })
  
  
  
##### Download the County Health Profile Report  ######  
  
  
  output$downloadCHPR <- renderUI({ 
    HTML(paste0('<a href =', links$CHPR.Link[links$County %in% c(input$cntyCHPR, paste0(input$cntyCHPR," "))],' target="_blank">Download County Health Profile</a>'))
  })
  
  output$downloadCHPR1 <- renderUI({ 
    HTML(paste0('<a href =', links$CHPR.Link[links$County %in% c(input$cnty1, paste0(input$cnty1," "))],' target="_blank">Download County Health Profile</a>'))
  })
  
  output$downloadNarrative <- renderUI({ 
    HTML(paste0('<a href =', narratives$narrativeLink[narratives$def == input$ind],' target="_blank">Download the Narrative for this Indicator</a>'))
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
  
}

# Run the application
shinyApp(ui = ui, server = server)
