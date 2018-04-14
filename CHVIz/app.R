#devtools::install_github('hadley/ggplot2')
library(shiny)
library(tidyverse)
library(markdown)
library(leaflet)
library(shinythemes)
library(ggthemes)
library(sf)
library(DT)
library(plotly)

links <-read.csv("CHPRlinks.csv", header=T)
CHVIdata <- read.csv("chviCountyTidy.csv", header=T, stringsAsFactors = FALSE) 
counties <- st_read("counties.geojson", stringsAsFactors = F) %>% st_transform(crs = 4326) 

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
                    "percent impervious surface cover", 
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
  defShort = c( "% HH with AC",
                "% Tree Canopy",                                                   
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
             title = div("CHVIz",a(href="https://www.cdph.ca.gov/Programs/OHE/Pages/CCHEP.aspx" #,
                                   # img(src="https://raw.githubusercontent.com/vargovargo/CHVIr/master/CHIViz/CCHEPbannerLong.gif", style = "position: relative; top: -3px; right: 0px;")
                                   )),
             
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
      column(5,
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
             column(9,wellPanel(plotlyOutput("triplePlot"))
             ), 
             column(3,
                    includeMarkdown("vulnerability.md"))
                         
           )
          ),


#####  Additional Page  ####

  navbarMenu(
    "Additional Resources",
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
               column(3,
                      p(),
               downloadButton(outputId = "downloadData", label = "Download Selected Data")
             )),             
            fluidRow(
               wellPanel(DT::dataTableOutput("downloadTable"))
    )),
   
   tabPanel("County Profile Report",
            fluidRow(
              column(4, 
                     selectInput("cntyCHPR",
                                 "Select Your County",
                                 c(sort(unique(as.character(CHVIdata$county)))
                                 ))
              )),
            fluidRow(
              column(
                4,
                p(uiOutput("downloadCHPR"))
              )
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
                   width = 700,
                   alt = "Impact of Climate Change on Human Health and Exacerbation of Existing Inquities `(`Adapted from CDC, J. Patz`)`."
                 )
               )
             ))
    
    
    
  )
#####  Finish Additional  #####

)
)

##### SERVER #####
server <- function(input, output, session) {

averages <- CHVIdata %>%
    filter(metric =="est") %>%
    group_by(def, ind, strata) %>%
    summarise(stateAverage = mean(value, na.rm=T))
  
##### create reactive table for single indicator #####
  
  data.tab2 <- reactive({

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
             selRegion = ifelse(Region == CHVIdata$climReg[CHVIdata$county == input$cnty][1], 
                                paste0("In ",CHVIdata$climReg[CHVIdata$county == input$cnty][1]," region"),
                               "Outside region"),
             countyColor = ifelse(County == input$cnty,"rgba(165,15,21, 1)", 
                                  ifelse(Region == CHVIdata$climReg[CHVIdata$county == input$cnty][1],
                                         "rgba(251,106,74, 0.5)",
                                         "rgba(247,247,247, 0.3)")))
   
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
  

  
#####  reactive table tab 1 #####
  
  data.tab1 <- eventReactive(input$cnty1,{
    
    CHVIdata %>% filter(county == input$cnty1 & metric == "est") %>% 
      left_join(averages) %>% 
      mutate(label = paste0(defShort," - ", strata),
             ratio = value/stateAverage,
             category = ifelse(ratio < 0.9, "below CA average",
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
        showlegend = FALSE
      ) %>%
      layout(margin = list(l = 300),
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
      select(county, climReg, def, strata, value, stateAverage, category) %>%
      rename(County = county, 
             Region = climReg, 
             Indicator = def, 
             Strata = strata,
             Valule = value, 
             CA_avg = stateAverage, 
             Category =category
             ), 
    options=list(pageLength = 25)
  )  %>% DT::formatRound(c(5,6), 1)
    
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
  
  
  
##### generate map (tab 2) #####
  
  output$map <- renderLeaflet({
    if (input$ind != "All") {
      
      mapTemp <- left_join(counties, data.tab2()) 
      
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
          smoothFactor = 0.1,
          fillOpacity = 0.6,
          fillColor = ~ pal(Mean),
          highlightOptions = highlightOptions(color = "white", weight = 2,
                                              bringToFront = TRUE),
          label = ~ (mapTemp$County),
          popup = paste0("This is ",mapTemp$County," County. The ",mapTemp$Definition," in this county is ",
                         round(mapTemp$Mean[mapTemp$County == mapTemp$County]),". The state average is ", round(mean(mapTemp$Mean, na.rm=T),2))
        ) %>%
        addLegend("topright",
                  pal = pal,
                  values = ~ Mean,
                  opacity = 1,
                  labFormat = labelFormat(),
                  title = input$ind 
        ) %>%
        clearControls()
      
    }
    
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
      showlegend = FALSE
    ) %>%
      layout(
        title = paste0(input$cnty," county (red) and its region (pink) compared to others in the state -   \n shown for ",input$ind, ".") ,
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
        filter(def  == input$exposure & strata %in% c("2085", 2085,"none") & metric =="est") %>%
        mutate(expTer = ntile(value, 3)) %>%
        select(county, climReg, COUNTYFI_1, def, value, expTer) %>% 
        spread(key = def, value = value)
    } %>% left_join({
      
      CHVIdata %>% 
        filter(def  == input$sensitivity & strata %in% c("Overall","ViolentCrime","total","2006-2010","2009-2013","All Non-English","none") & metric =="est") %>%
        mutate(sensTer = ntile(value, 3)) %>%
        select(county, climReg, COUNTYFI_1, def, value, sensTer) %>% 
        spread(key = def, value = value) %>% 
        left_join({
          CHVIdata %>%
            filter(def  == input$sensitivity & strata %in% c("Overall","ViolentCrime","total","2006-2010","2009-2013","All Non-English","none") & metric == "denmntr") %>%
            select(county, value)
        }) %>%
        rename(Population = value)
    }) %>%  
      mutate(Population = as.numeric(as.character(Population)),
             vulnerability = factor(sensTer + expTer))
    
    tri[["sign"]] <- ifelse(tri[["vulnerability"]] == 2, "rgba(26,152,80, 0.5)",
                            ifelse(tri[["vulnerability"]] == 3, "rgba(166,217,106, 0.6)",
                                   ifelse(tri[["vulnerability"]] == 4, "rgba(253,174,97, 0.7)",
                                          ifelse(tri[["vulnerability"]] == 5, "rgba(244,109,67, 0.9)", "rgba(215,48,39, 1)"))))
    
    tri[["size"]] <- ntile(tri[["Population"]],20)
    
    
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
      text = ~paste('</br> County ',tri[["county"]],
                    '</br> Population: ',format(tri[["Population"]], big.mark = ","),
                    '</br> Exposure: ', round(tri[[5]],2), " ", names(tri)[5],
                    '</br> Sensitivity: ', round(tri[[7]],2), " ", names(tri)[7]),
      showlegend = FALSE
    ) %>%
      add_markers(type = 'scatter', 
                  mode = 'markers',
                  size = ~tri[["Population"]]+35,
                  marker = list(color = tri[["sign"]], 
                                size = tri[["size"]]*25,   
                                line = list(color = 'rgba(99,99,99, .8)',width = 0.5))) %>%
      add_text(type = 'scatter',mode = 'text', text = tri[["county"]], textposition = 'top right',
                textfont = list(
                  family = "sans serif",
                  size = 8,
                  color = toRGB("grey20"))) %>%
      layout(margin = list(l = 50),
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
  
  output$chooseStrataDNLD <- renderUI({
    
    if(input$indDNLD =="All"){
      return()
    } else {
      
    selectInput("strtDNLD",
                "Strata",{
                  unique(
                    as.character({CHVIdata %>% filter(def == input$indDNLD)}$strata)
                  )
                  
                }
    )}
  })
  
  
  
  data.dnld <- eventReactive(c(input$cntyDNLD, input$indDNLD),{
    
    if(input$cntyDNLD  == "All" & input$indDNLD != "All") {
      
      CHVIdata %>%
        mutate(COUNTYFI_1 = as.character(paste0("0",COUNTYFI_1))) %>%
        filter(def == input$indDNLD) %>%
        spread(key = metric, value = value) %>%
        rename(
          County = county,
          Region = climReg,
          Definition = def,
          Mean = est, 
          Numerator = numratr,
          Denominator = denmntr)
    } else { if(input$cntyDNLD  != "All" & input$indDNLD != "All") {
      
      CHVIdata %>%
        mutate(COUNTYFI_1 = as.character(paste0("0",COUNTYFI_1))) %>%
        filter(def == input$indDNLD & county == input$cntyDNLD) %>%
        spread(key = metric, value = value) %>%
        rename(
          County = county,
          Region = climReg,
          Definition = def,
          Mean = est, 
          Numerator = numratr,
          Denominator = denmntr)
    } else { if(input$cntyDNLD  != "All" & input$indDNLD == "All") {
      
      CHVIdata %>%
        mutate(COUNTYFI_1 = as.character(paste0("0",COUNTYFI_1))) %>%
        filter(county == input$cntyDNLD) %>%
        spread(key = metric, value = value) %>%
        rename(
          County = county,
          Region = climReg,
          Definition = def,
          Mean = est, 
          Numerator = numratr,
          Denominator = denmntr)
    } else {   
      
      CHVIdata %>%
        mutate(COUNTYFI_1 = as.character(paste0("0",COUNTYFI_1))) %>%
        spread(key = metric, value = value) %>%
        rename(
          County = county,
          Region = climReg,
          Definition = def,
          Mean = est, 
          Numerator = numratr,
          Denominator = denmntr)
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
            strata,
            Mean,
            LL95,
            UL95,
            Numerator,
            Denominator
          )
        }, file, row.names = F)
    }
    
  )
  
  
  ##### generate (download tab) Table ######  
  
  output$downloadTable <- DT::renderDataTable({DT::datatable(
    
    data.dnld() %>%
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
      ), 
    options=list(pageLength = 25)
  )  %>% DT::formatRound(c(5:9), 1)
    
  })
  
  
  
##### Download the County Health Profile Report  ######  
  
  
  output$downloadCHPR <- renderUI({ 
    HTML(paste0("<a href =", links$CHPR.Link[links$County %in% c(input$cntyCHPR, paste0(input$cntyCHPR," "))],">Download County Health Profile</a>"))
  })
  
  output$downloadCHPR1 <- renderUI({ 
    HTML(paste0("<a href =", links$CHPR.Link[links$County %in% c(input$cnty1, paste0(input$cnty1," "))],">Download County Health Profile</a>"))
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
