library(shiny)
library(tidyverse)
library(leaflet)
library(shinythemes)
library(ggthemes)
library(sf)
library(DT)
library(plotly)

links <-read.csv("CHPRlinks.csv", header=T)
CHVIdata <- readRDS("chviCountyTidyRace.RDS") 
CHVItracts <- readRDS("chviTractTidy.RDS")
counties <- st_read("counties.geojson", stringsAsFactors = F) %>% st_transform(crs = 4326) 
tracts <- st_read("tracts.GeoJSON", stringsAsFactors = F) %>% st_transform(crs = 4326) %>%
  mutate(COUNTYFI_1 = as.character(paste0(STATE, COUNTY)))

CHVIdata$def <- ifelse(CHVIdata$def == "percent impervious surface cover", "Percent impervious surface cover", CHVIdata$def)
CHVIdata <- left_join(x = CHVIdata, y = {
  data.frame(def= c("Percent of households with air conditioning", 
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
  defShort = c( "% HH w/ AC",
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
  )}
)


narratives <- data.frame(def = c("Percent of households with air conditioning", 
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
narrativeLink = c("https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/AirConditioning_797_Narrative_12-14-2016.pdf",
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
             title = div("CHVIz",a(href="https://www.cdph.ca.gov/Programs/OHE/Pages/CCHEP.aspx" 
                                   ,img(src="https://raw.githubusercontent.com/vargovargo/CHVIr/master/CHVIz/CDPHLogo.gif", height= "45", style = "position: relative; top: -12px; right: 0 px;")
             )),
             
             tabPanel("About",
                      fluidRow(
                        column(
                          6,
                          includeMarkdown("about.md"),
                          tags$br(),
                          img(
                            class = "img-polaroid",
                            src = "https://www.cdph.ca.gov/Programs/OHE/PublishingImages/Policy%20Unit/CDPH-Climate-change-and-health-impacts-diagram.png",
                            width = 900,
                            alt = "Impact of Climate Change on Human Health and Exacerbation of Existing Inquities `(`Adapted from CDC, J. Patz`)`."
                          )
                        )
                      )),
             
             tabPanel(title = "Vulnerability", 
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
                                             "Percent of households with no vehicle ownership",
                                             "Percent without tree canopy coverage",
                                             "Percent impervious surface cover"
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
                        column(9,wellPanel(plotlyOutput("triplePlot"))
                        ), 
                        column(3,
                               includeMarkdown("vulnerability.md"))
                        
                      )
             ),
             
             
             
             
             tabPanel("Single County",
                      # Create a new Row in the UI for selectInputs
                      
                      
                      #####  Select an County  #####       
                      
                      fluidRow(
                        column(3,
                               selectInput("cnty1",
                                           "Select a County",
                                           c(sort(unique(as.character(CHVIdata$county)))
                                           ))
                        ),
                        column(3,br(),
                               p(uiOutput("downloadCHPR1"))
                        )),
                      fluidRow(
                        column(9, wellPanel(plotlyOutput("plotCounty"))),
                        column(3, includeMarkdown("countyPlot.md"))),
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
                                      "Percent of households with air conditioning",
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
                               wellPanel(plotlyOutput("plot"))
               ), 
               column(4,
                      wellPanel(leafletOutput("map"))
               )),
               wellPanel(DT::dataTableOutput("table"))
             ),

             tabPanel("Download your Data",
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
                                             "Percent of households with air conditioning",
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
                      ))
             
             
             #####  Additional Page  ####
             
             # navbarMenu(
             #   "Additional Resources",
             #  tabPanel("County Profile Report",
             #           fluidRow(
             #             column(4, 
             #                    selectInput("cntyCHPR",
             #                                "Select Your County",
             #                                c(sort(unique(as.character(CHVIdata$county)))
             #                                ))
             #             )),
             #           fluidRow(
             #             column(
             #               4,
             #               p(uiOutput("downloadCHPR"))
             #             )
             #           ))
             #  
             #   
             #  
             #   
             #   
             #   
             # )
             #####  Finish Additional  #####
             
  )
)

##### SERVER #####
server <- function(input, output, session) {
  
  averages <- CHVIdata %>%
    group_by(def, ind, strata) %>%
    summarise(stateAverage = mean(est, na.rm=T))
  
  
  #####  reactive table tab 1 #####
  
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
             ratio = Value/CA_avg,
             Category = ifelse(ratio < 0.9, "below CA average",
                               ifelse(ratio > 1.1, "above CA average","around CA average")),
             fillColor = ifelse(ratio < 0.9, "#91bfdb",
                                ifelse(ratio > 1.1, "#fc8d59","#ffffbf")))
  })
  
  
  ##### generate County (tab 1) Plot #####
  
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
                          size=2)
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
  
  
##### generate County (tab 1) Table ######  
  
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
             countyColor = ifelse(County == input$cnty,"rgba(165,15,21, 1)", 
                                  ifelse(Region == CHVIdata$climReg[CHVIdata$county == input$cnty][1],
                                         "rgba(251,106,74, 0.5)",
                                         "rgba(247,247,247, 0.3)")))
    
  })
  
  
##### generate table of the data (tab 2) #####
  
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
  
 
##### Census tract data ######
  
 tractData <- reactive({
   
   CHVItracts %>% 
     filter(def == input$ind & strata == input$strt)  %>%
     mutate(ct10 = as.character(paste0('0',ct10))) 
  
 })
  
 
selectedFIPS <- eventReactive(input$cnty, {
   
   as.character(paste0("0",CHVIdata$COUNTYFI_1[CHVIdata$county == input$cnty][1]))
   
})


average <- eventReactive(c(input$ind, input$strt), {
  
  as.numeric({averages %>% filter(def == input$ind & strata == input$strt) %>%
      ungroup() %>% select(stateAverage)}[1])
})
  
##### generate map (tab 2) #####
  
  output$map <- renderLeaflet({
    
      mapTemp <- tracts %>% 
        filter(COUNTYFI_1 == selectedFIPS()) %>%
        left_join(tractData()) 
      
      pal <- colorBin(
        palette = "RdYlBu",
        bins = 8,
        reverse = TRUE,
        domain = NULL
      )
      
      pal2 <- colorBin(
        palette = "Blues",
        bins = 4,
        reverse = FALSE,
        domain = NULL
    
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
                         group="Diverging Colors") %>% 
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
              group="Single Color")  %>%
          addLayersControl(
            baseGroups = c("Diverging Colors", "Single Color"),
            options = layersControlOptions(collapsed = TRUE)
          ) 
          
        # addLegend("topright",
        #           pal = pal,
        #           values = ~ mapTemp$est,
        #           opacity = .4,
        #           title = input$ind 
        # ) 
      
    
  })
  
  
##### generate plot (tab 2) #####
  
  # output$plot <- renderPlot({
  # 
  #   data.tab2() %>%
  #     ggplot() +
  #     geom_bar(
  #       aes(
  #         x = reorder(County, Mean),
  #         y = Mean,
  #         fill = selRegion,
  #         alpha = selCounty
  #       ),
  #       stat = "identity",
  #       position = "dodge"
  #     ) +
  #     xlab(label = "Counties") + ylab(input$ind) + guides(fill = FALSE, alpha = FALSE) +
  #     geom_hline(
  #       yintercept = mean(data.tab2()$Mean, na.rm = T),
  #       color = "black",
  #       alpha = 0.5
  #     ) +
  #     scale_alpha_discrete(range = c(0.3, 0.8)) +
  #       scale_fill_manual(values = c("green","blue")) +
  #       coord_flip() +
  #       theme_update(axis.text.y = element_text(size=6), axis.text.x = element_text(size=10))+ 
  #       ggtitle(paste0(input$cnty," county (dark blue) and its region (light blue) compared to others in the state (green) -   \n shown for ",input$ind, " ."))
  #   
  #   
  #   #    
  # })
  
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
        title = paste0(input$cnty," county (red) and its region (pink) compared to others in the state -   \n shown for ",input$ind) ,
        margin = list(l = 130,
                      t = 70),
        xaxis = list(
          title = input$ind,
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
                     dtick=2
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
  
##### make triple plot (tab 3) #####
  
  output$triplePlot <- renderPlotly({
    
    
    tri <- {CHVIdata %>% 
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
             vulnerability = factor(sensTer + expTer))
    
    tri[["sign"]] <- ifelse(tri[["vulnerability"]] == 2, "rgba(26,152,80, 0.5)",
                            ifelse(tri[["vulnerability"]] == 3, "rgba(166,217,106, 0.6)",
                                   ifelse(tri[["vulnerability"]] == 4, "rgba(253,174,97, 0.7)",
                                          ifelse(tri[["vulnerability"]] == 5, "rgba(244,109,67, 0.9)", "rgba(215,48,39, 1)"))))
    
    tri[["size"]] <- ntile(tri[["Population"]],29)
    
    
    # left_join({
    #   
    #   CHVIdata %>% 
    #     filter(def  == input$capacity & strata %in% c("population-weighted", "none") & metric =="est") %>%
    #     select(county, climReg, COUNTYFI_1, def, value) %>% 
    #     spread(key = def, value = value)
    #   
    # }) %>% 
    
    # tri %>% ggplot(aes_q(x = as.name(names(tri)[5]), 
    #                      y = as.name(names(tri)[7]),
    #                      size = as.name(names(tri)[8]), 
    #                      color = as.name(names(tri)[8])
    #                )) +
    #   geom_point(alpha = 0.9) +
    #   guides(alpha = FALSE, color = FALSE, size = FALSE) + 
    #   scale_color_brewer(palette = "Spectral", direction = -1) +
    #   scale_size_continuous(range = c(3,15)) + 
    #   geom_text(aes_q(x = as.name(names(tri)[5]), 
    #                   y = as.name(names(tri)[7]), 
    #                   label = as.name(names(tri)[1])), size= 3, color="black") + 
    #   ggtitle("This plot displays the vulnerability to two factors. Counties in the top right corner (red) are in the top third of all counties for each.")
    # 
    
    plot_ly(
      data = tri,
      x =  ~ round(tri[[5]],2),
      y =  ~ round(tri[[7]],2),
      hoverinfo = 'text',
      text = ~paste('</br> County',tri[["county"]],
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
      layout(title = paste0('Combined Vulnerabiltity from Exposure (',names(tri)[5], ')    \n and Sensitivity (',names(tri)[7],")") ,
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
  
  
  
  
##### Strata Interface for Download tab #####
  
  # output$chooseStrataDNLD <- renderUI({
  #   
  #   if(input$indDNLD =="All"){
  #     return()
  #   } else {
  #     
  #   selectInput("strtDNLD",
  #               "Strata",{
  #                 unique(
  #                   as.character({CHVIdata %>% filter(def == input$indDNLD)}$strata)
  #                 )
  #                 
  #               }
  #   )}
  # })
  # 
  

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
    HTML(paste0('<a href =', narratives$narrativeLink[narratives$def == input$ind],' target="_blank">Learn more about this Indicator</a>'))
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
