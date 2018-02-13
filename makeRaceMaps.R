rm(list=ls())

library(tidyverse)
library(sf)
library(leaflet)

setwd(dir = "~/CHVI_copy/CHVIcsvs/")

# work from the network
# setwd("//phitprlcsrvip04/OHEGroup/HCI/Data/CCHVI in one folder for web/CHVIcsvs/")

counties <- st_read("../shapes/Counties2010.shp")
tracts <- st_read("../shapes/census_tracts2010.shp")

#############################
######### Functions #########
#############################

readCHVI <- function(fileName){
   temp <- read.csv(fileName, header=T, stringsAsFactors = F) %>% select(1:26) %>% .[,order(names(.))]
   
   names(temp) <- c(
     "CA_decile",
     "CA_RR",
     "county_fips",
     "county_name",
     "denominator",
     "estimate",
     "geoname",
     "geotype",
     "geotypevalue",
     "ind_definition",
     "ind_id",
     "LL_95CI",
     "numerator",
     "race_eth_code",
     "race_eth_name",
     "region_code",
     "region_name",
     "reportyear",
     "RSE",
     "SE",
     "strata_one_code",
     "strata_one_name",
     "strata_two_code",
     "strata_two_name",
     "UL_95CI",
     "version"
   )
   
   temp$reportyear <- as.character(as.numeric(temp$reportyear))
   
   return(temp)
}



GISbyRace <- function(indicator = "indicator"){
  
  path <- ifelse(indicator %in% c("wildfire","WildFire", "Fire","fire","wild fire","WF"),
               "./BRACE_Wildfire_786_CT_PL_CO_RE_CA.csv",
               ifelse(indicator %in% c("sea level rise","SLR", "sea level","Sea Level Rise", "Sea Level", "sealevel"),
                      "./BRACE_SLR_784_CT_PL_CO_RE_CA_11-1-2016.csv",
                      ifelse(indicator %in% c("PM25","Particulate Matter", "particulate matter","PM2.5", "pm", "PM"),
                             "./BRACE_PM25levels_776_CT_PL_CO_RE_CA.csv",
                             ifelse(indicator %in% c("Ozone","ozone", "o3","O3"),
                                    "./BRACE_Ozone_801_CT_PL_CO_RE_CA.csv","no path"
                                    ))))
  
  data <- readCHVI(path)
  
# tracts
tractData <- data %>% 
  select(ind_definition, county_name,geotypevalue,geotype,race_eth_name, estimate) %>%
  filter(geotype == "CT")  %>% 
  spread(key = race_eth_name, value = estimate) %>% 
  mutate(ct10= as.character(paste0("0",geotypevalue))) 

tracts %>% left_join(tractData) %>% st_write(dsn = paste0("../shapes/",indicator,"_RaceTracts.shp"), delete_layer=TRUE)

# Counties
countyData <- data %>% 
  select(ind_definition, county_name,geotypevalue,geotype,race_eth_name, estimate) %>%
  filter(geotype == "CO")  %>% 
  spread(key = race_eth_name, value = estimate) %>% 
  mutate(COUNTYFI_1= as.character(paste0("0",geotypevalue))) 

counties %>% left_join(countyData) %>% st_write(dsn = paste0("../shapes/",indicator,"_RaceCounties.shp"),delete_layer=TRUE)

}

#####################################
GISbyRace(indicator = "wildfire")
GISbyRace(indicator = "sea level rise")
GISbyRace(indicator = "pm")
GISbyRace(indicator = "ozone")


wildfire <- readCHVI("./BRACE_Wildfire_786_CT_PL_CO_RE_CA.csv")
seaLevel <- readCHVI("./BRACE_SLR_784_CT_PL_CO_RE_CA_11-1-2016.csv")
ozone <- readCHVI("./BRACE_Ozone_801_CT_PL_CO_RE_CA.csv")

# WF tract by race
WFtract <- wildfire %>% 
  select(ind_definition, county_name,geotypevalue,geotype,race_eth_name, estimate) %>%
  filter(geotype == "CT")  %>% 
  spread(key = race_eth_name, value = estimate) %>% 
  mutate(ct10= as.character(paste0("0",geotypevalue)),
         min = apply(.[,c("AfricanAm","AIAN","Asian","Latino","Multiple", "NHOPI","Other", "White")],FUN = min, MARGIN = 1),
         max = apply(.[,c("AfricanAm","AIAN","Asian","Latino","Multiple", "NHOPI","Other", "White")],FUN = max, MARGIN = 1),
         diff = (max-min))

WFtractsMap <- tracts %>% left_join(WFtract) %>% st_transform(crs = 4326)
pal <- colorNumeric(
  palette = "Reds",
  domain = WFtractsMap$diff)


WFtractsMap %>% 
  leaflet() %>%
  addTiles() %>%
  addPolygons(color = "#444444", weight = 1, fillOpacity = 0.6,
              fillColor = ~pal(diff),
              popup = paste("In this tract (in ",WFtractsMap$county_name," County), ",
                            round(WFtractsMap$Total, 2),"% of people live in high Wildfire risk areas. This includes ",
                            round(WFtractsMap$White, 2),"% of the tract's White population, ",
                            round(WFtractsMap$AfricanAm, 2),"% of the tract's African American population, ",
                            round(WFtractsMap$Asian, 2),"% of the tract's Asian population, and ",
                            round(WFtractsMap$Latino, 2),"% of the tract's Latino population."
                            )) %>%
  addLegend("bottomright", pal = pal, values = ~diff,
            title = "Difference between most and least at risk",
            labFormat = labelFormat(suffix = " percentage points"),
            opacity = 1
  )


WFcounty <- wildfire %>% 
  select(ind_definition, county_name,geotypevalue,geotype,race_eth_name, estimate) %>%
  filter(geotype == "CO")  %>% 
  spread(key = race_eth_name, value = estimate) %>% 
  mutate(COUNTYFI_1= as.character(paste0("0",geotypevalue)))  


WFcountyMap <- counties %>% left_join(WFcounty) %>% st_transform(crs = 4326)
pal <- colorNumeric(
  palette = "Reds",
  domain = WFcountyMap$Total)



WFcountyMap %>% 
  leaflet() %>%
  addTiles() %>%
  addPolygons(color = "#444444", weight = 1, fillOpacity = 0.6,
              fillColor = ~pal(Total),
              popup = paste0("In ",WFcountyMap$county_name," County, ",
                            round(WFcountyMap$Total, 2),"% of people live in high Wildfire risk areas. This includes ",
                            round(WFcountyMap$White, 2),"% of the county's White population, ",
                            round(WFcountyMap$AfricanAm, 2),"% of the county's African American population, ",
                            round(WFcountyMap$Asian, 2),"% of the county's Asian population, and ",
                            round(WFcountyMap$Latino, 2),"% of the county's Latino population.")) %>%
  addLegend("bottomright", pal = pal, values = ~Total,
            title = "Percent of the County Living in High WIldfire Risk",
            labFormat = labelFormat(suffix = "%"),
            opacity = 1)




##################
##### Ozone
#################



O3county <- ozone %>% 
  select(ind_definition, county_name,geotypevalue,geotype,race_eth_name, estimate) %>%
  filter(geotype == "CO")  %>% 
  spread(key = race_eth_name, value = estimate) %>% 
  mutate(COUNTYFI_1= as.character(paste0("0",geotypevalue)))  


O3countyMap <- counties %>% left_join(O3county) %>% st_transform(crs = 4326)
pal <- colorNumeric(
  palette = "Reds",
  domain = O3countyMap$Total)



O3countyMap %>% 
  leaflet() %>%
  addTiles() %>%
  addPolygons(color = "#444444", weight = 1, fillOpacity = 0.6,
              fillColor = ~pal(Total),
              popup = paste0("In ",O3countyMap$county_name," County, the average ozone exceedance is about ",
                             round(O3countyMap$Total, 3),"ppm. For the White population it is about ",
                             round(O3countyMap$White, 3),"ppm, for the Black population it is about  ",
                             round(O3countyMap$AfricanAm, 3),"ppm, for the Asian population it is about ",
                             round(O3countyMap$Asian, 3),"ppm, and for the Latino population it is about ",
                             round(O3countyMap$Latino, 3),"ppm.")) %>%
  addLegend("bottomright", pal = pal, values = ~Total,
            title = "Ozone exceedances",
            labFormat = labelFormat(suffix = "ppm"),
            opacity = 1)


##################
##### Seal Level
#################



SLRcounty <- seaLevel %>% 
  select(ind_definition, county_name,geotypevalue,geotype,race_eth_name, estimate) %>%
  filter(geotype == "CO")  %>% 
  spread(key = race_eth_name, value = estimate) %>% 
  filter(Total > 0) %>%
  mutate(COUNTYFI_1= as.character(paste0("0",geotypevalue)))  


SLRcountyMap <- counties %>% left_join(SLRcounty) %>% st_transform(crs = 4326)
pal <- colorNumeric(
  palette = "Blues",
  domain = SLRcountyMap$Total)



SLRcountyMap %>% 
  leaflet() %>%
  addTiles() %>%
  addPolygons(color = "#444444", weight = 1, fillOpacity = 0.6,
              fillColor = ~pal(Total),
              popup = paste0("In ",SLRcountyMap$county_name," County, ",
                             round(SLRcountyMap$Total, 2),"% of people live in high sea level rise risk areas. This includes ",
                             round(SLRcountyMap$White, 2),"% of the county's White population, ",
                             round(SLRcountyMap$AfricanAm, 2),"% of the county's African American population, ",
                             round(SLRcountyMap$Asian, 2),"% of the county's Asian population, and ",
                             round(SLRcountyMap$Latino, 2),"% of the county's Latino population.")) %>%
  addLegend("bottomright", pal = pal, values = ~Total,
            title = "Percent of the County Living in High Sea Level Inundation Risk Areas",
            labFormat = labelFormat(suffix = "%"),
            opacity = 1)
