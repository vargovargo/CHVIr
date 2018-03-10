library(shiny)
library(tidyverse)
library(markdown)
library(leaflet)
links <-read.csv(url("https://raw.githubusercontent.com/vargovargo/CHVIr/master/CHPRlinks.csv"), header=T)
CHVIdata <- read.csv(url("https://raw.githubusercontent.com/vargovargo/CHVIr/master/chviCountyTidy.csv"), header=T)


# Define UI for application that draws a histogram
ui <-navbarPage(theme = "bootstrap.css",
              title = "CHVIz",
              tabPanel("County Data",
                       # Create a new Row in the UI for selectInputs
                       fluidRow(
                         column(4,
                                selectInput("cnty",
                                            "County",
                                            c("All",
                                              unique(as.character(CHVIdata$county))))
                         ),
                         column(4,
                                selectInput("ind",
                                            "Indicator",
                                            c("All",
                                              unique(as.character(CHVIdata$def))))
                         ),
                         column(4,
                                downloadHandler("downloadData", "Download" )
                                )
                         
                       ),
                       # Create a new row for the table.
                       wellPanel(
                         DT::dataTableOutput("table")
                       ), 
                       
                       wellPanel(
                        plotOutput("plot")
                       ),
                       wellPanel(
                         leafletOutput("map")
                       )
              ),
              tabPanel("Summary",
                       verbatimTextOutput("summary")
              ),
              navbarMenu("Documnetation",
                         tabPanel("About",
                                  fluidRow(
                                    column(6,
                                           includeMarkdown("about.md")
                                    ),
                                    column(6,
                                           img(class="img-polaroid",
                                               src="https://www.cdph.ca.gov/Programs/OHE/PublishingImages/Policy%20Unit/CDPH-Climate-change-and-health-impacts-diagram.png",
                                               height = 400,
                                               alt="Impact of Climate Change on Human Health and Exacerbation of Existing Inquities `(`Adapted from CDC, J. Patz`)`.")
                                           )
                                    )
                                  )
                         )
              )
   



# Define server logic required to draw a histogram
server <- function(input, output, session) {
  path <- renderText(links$CHPR.Link[links$County == input$cnty])
  
  data <- reactive({
    tempData <- CHVIdata
    if (input$cnty != "All") {
      tempData <- tempData[tempData$county == input$cnty,]
    }
    if (input$ind != "All") {
      tempData <- tempData[tempData$def == input$ind,]
    }
    tempData
  })
  
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable(data()))
  
  output$downloadData <- downloadHandler(
    
       filename = function () {
         paste0("selectedCHVIdata_",input$cnty,".csv")
       },

       content = function(file) {
         write.csv(data(), file, row.names=FALSE)
       }
       
  )  
}

# Run the application 
shinyApp(ui = ui, server = server)

