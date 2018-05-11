
library(shiny)
library(tidyverse)
library(leaflet)
library(shinythemes)
library(ggthemes)
library(sf)
library(DT)
library(plotly)

CHVIdata <- readRDS("~/GitHub/CHVIr/CHVIz/chviCountyTidyRace.RDS")

tri <- {CHVIdata %>% 
    filter(def  == "Projected number of extreme heat days"  & strata %in% c("2085", 2085, "none") & race == "Total") %>%
    mutate(expTer = ntile(est, 3)) %>%
    select(county, climReg, COUNTYFI_1, def, est, expTer) %>% 
    spread(key = def, value = round(est,2))
} %>% left_join({
  
  CHVIdata %>% 
    filter(def  == "Percent of population aged 65 years or older"  & strata %in% c("Overall","ViolentCrime","total","2006-2010","2009-2013","All Non-English","none", "population-weighted") & race =="Total") %>%
    mutate(sensTer = ntile(est, 3)) %>%
    select(county, climReg, COUNTYFI_1, def, est, sensTer) %>% 
    spread(key = def, value = round(est,2)) %>% 
    left_join({
      CHVIdata %>%
        filter(def  == "Percent of population aged 65 years or older" & race == "Total") %>%
        select(county, denmntr)%>%
        rename(Population = denmntr)
    }) 
  
}) %>%  
  mutate(Population = as.numeric(as.character(Population)),
         vulnerability = factor(ifelse(sensTer + expTer == 2, "lowest",
                                       ifelse(sensTer + expTer == 3, "low",
                                              ifelse(sensTer + expTer == 4, "medium",
                                                     ifelse(sensTer + expTer == 5, "high","highest")))), 
                                levels = c("lowest","low","medium","high","highest")))
tri[["sign"]] <- ifelse(tri[["vulnerability"]] == "lowest", "rgba(26,152,80, 0.5)",
                        ifelse(tri[["vulnerability"]] == "low", "rgba(166,217,106, 0.6)",
                               ifelse(tri[["vulnerability"]] == "medium", "rgba(253,174,97, 0.7)",
                                      ifelse(tri[["vulnerability"]] == "high", "rgba(244,109,67, 0.9)", "rgba(215,48,39, 1)"))))

tri[["size"]] <- ntile(tri[["Population"]],5)

tri <- na.omit(tri)



#devtools::install_github("rCharts", "ramnathv")

library(rCharts)

# names(tri)[5] <- "Exposure"
# names(tri)[7] <- "Sensitivity"


p1 <- nPlot(Sensitivity ~ Exposure, group = 'vulnerability', data = tri, type = 'scatterChart')
p1$xAxis(axisLabel = "Projected number of extreme heat days")
p1$yAxis(axisLabel = "Percent of population aged 65 years or older")

p1


d8 <- dPlot(x = names(tri)[5],
  y = "Percent of population aged 65 years or older",
  z = "size",
  groups = "vulnerability",
  data = tri,
  type = "bubble"
)
d8$xAxis()
d8$zAxis(type = "addMeasureAxis")
d8






#devtools::install_github("mrjoh3/c3")
library(c3)


tri %>%
  c3(x = "Projected number of extreme heat days", 
     y = "Percent of population aged 65 years or older", 
     group = 'vulnerability') %>% 
  point_options(r = 4, 
                expand.r = 2) %>%
  c3_scatter()  %>% RColorBrewer(pal="YlOrRd", reverse =TRUE)

RColorBrewer::brewer.pal.info

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
           'hoverClosestCartesian'
         )
  )

})

