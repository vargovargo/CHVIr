rm(list=ls())

library(tidyverse)
library(sf)
library(spData)
library(spDataLarge)

#############################


# testFile <- "~/CHVI_copy/CHVIcsvs/BRACE_Disability_795_CT_PL_CO_RE_CA.csv"

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


# work from the network
# setwd("//phitprlcsrvip04/OHEGroup/HCI/Data/CCHVI in one folder for web/CHVIcsvs/")

# airConditioning <- readCHVI("./BRACE_AirConditioning_797_CO_RE_CA.csv")
# carOwnership <- readCHVI("./BRACE_CarOwnership_37_CT_PL_CO_RE_CA.csv")
# children <- readCHVI("./BRACE_children_788_CT_PL_CO_RE_CA.csv")
# disability <- readCHVI("./BRACE_Disability_795_CT_PL_CO_RE_CA.csv")
# elderly <- readCHVI("./BRACE_elderly65over_789_CT_PL_CO_RE_CA_11-8-2016.csv")
# extremeHeat <- readCHVI("./BRACE_ExtremeHeat_791_CO.csv")
# impervious <- readCHVI("./BRACE_ImperviousSurfaces_423_CT_PL_CO_RE_CA.csv")
# insurance <- readCHVI("./BRACE_Insurance_795_CT_PL_CO_RE_CA.csv")
# linguisticIso <- readCHVI("./BRACE_LinguisticIsolation_800_CT_PL_CO_RE_CA.csv")
# outdoorWorkers <- readCHVI("./BRACE_OutdoorWorkers_790_CT_PL_CO_RE_CA.csv")
ozone <- readCHVI("./BRACE_Ozone_801_CT_PL_CO_RE_CA.csv")
pm25 <- readCHVI("./BRACE_PM25levels_776_CT_PL_CO_RE_CA.csv")
# race <- readCHVI("./BRACE_race_795_CT_PL_CO_RE_CA.csv")
seaLevel <- readCHVI("./BRACE_SLR_784_CT_PL_CO_RE_CA_11-1-2016.csv")
# treeCanopy <- readCHVI("./BRACE_TreeCanopy_458_CT_PL_CO_RE_CA.csv")
wildfire <- readCHVI("./BRACE_Wildfire_786_CT_PL_CO_RE_CA.csv")




#####################################


counties <- st_read("../shapes/Counties2010.shp")
tracts <- st_read("../shapes/census_tracts2010.shp")



unique(wildfire$ind_definition)
unique(wildfire$race_eth_name)
length(unique(wildfire$geotypevalue))


# WF tract by race
WFtract <- wildfire %>% 
  select(ind_definition, county_name,geotypevalue,geotype,race_eth_name, estimate) %>%
  filter(geotype == "CT")  %>% 
  spread(key = race_eth_name, value = estimate) %>% 
  mutate(ct10= as.character(paste0("0",geotypevalue))) 
  
tracts %>% left_join(WFtract) %>% st_write(dsn = "../shapes/WF_RaceTracts.shp")


# WF County by race
WFcounty <- wildfire %>% 
  select(ind_definition, county_name,geotypevalue,geotype,race_eth_name, estimate) %>%
  filter(geotype == "CO")  %>% 
  spread(key = race_eth_name, value = estimate) %>% 
  mutate(COUNTYFI_1= as.character(paste0("0",geotypevalue)))  

counties %>% left_join(WFcounty) %>% st_write(dsn = "../shapes/WF_RaceCounties.shp")

# O3 tract by race
O3tract <- ozone %>% 
  select(ind_definition, county_name,geotypevalue,geotype,race_eth_name, estimate) %>%
  filter(geotype == "CT")  %>% 
  spread(key = race_eth_name, value = estimate) %>% 
  mutate(ct10= as.character(paste0("0",geotypevalue))) 

tracts %>% left_join(O3tract) %>% st_write(dsn = "../shapes/O3_RaceTracts.shp")


# O3 County by race
O3county <- ozone %>% 
  select(ind_definition, county_name,geotypevalue,geotype,race_eth_name, estimate) %>%
  filter(geotype == "CO")  %>% 
  spread(key = race_eth_name, value = estimate) %>% 
  mutate(COUNTYFI_1= as.character(paste0("0",geotypevalue)))  

counties %>% left_join(O3county) %>% st_write(dsn = "../shapes/O3_RaceCounties.shp")


# PM tract by race
PMtract <- pm25 %>% 
  select(ind_definition, county_name,geotypevalue,geotype,race_eth_name, estimate) %>%
  filter(geotype == "CT")  %>% 
  spread(key = race_eth_name, value = estimate) %>% 
  mutate(ct10= as.character(paste0("0",geotypevalue))) 

tracts %>% left_join(PMtract) %>% st_write(dsn = "../shapes/PM_RaceTracts.shp")


# PM County by race
PMcounty <- pm25 %>% 
  select(ind_definition, county_name,geotypevalue,geotype,race_eth_name, estimate) %>%
  filter(geotype == "CO")  %>% 
  spread(key = race_eth_name, value = estimate) %>% 
  mutate(COUNTYFI_1= as.character(paste0("0",geotypevalue)))  

counties %>% left_join(PMcounty) %>% st_write(dsn = "../shapes/PM_RaceCounties.shp")



# SLR tract by race
SLRtract <- seaLevel %>% 
  select(ind_definition, county_name,geotypevalue,geotype,race_eth_name, estimate) %>%
  filter(geotype == "CT")  %>% 
  spread(key = race_eth_name, value = estimate) %>% 
  mutate(ct10= as.character(paste0("0",geotypevalue))) 

tracts %>% left_join(SLRtract) %>% st_write(dsn = "../shapes/SLR_RaceTracts.shp")


# SLR County by race
SLRcounty <- seaLevel %>% 
  select(ind_definition, county_name,geotypevalue,geotype,race_eth_name, estimate) %>%
  filter(geotype == "CO")  %>% 
  spread(key = race_eth_name, value = estimate) %>% 
  mutate(COUNTYFI_1= as.character(paste0("0",geotypevalue)))  

counties %>% left_join(SLRcounty) %>% st_write(dsn = "../shapes/SLR_RaceCounties.shp")





















######################################
# simple features learning
######################################
world %>% 
  plot()

vector_filepath <- system.file("shapes/world.gpkg", package="spData")
vector_filepath
world <- st_read(vector_filepath)
sf::st_drivers()
world
class(world)

world %>% 
  ggplot(aes(x=continent, y=area_km2, fill=region_un)) + geom_bar(stat="identity", position="stack") 

world %>% 
  left_join(worldbank_df, by = "iso_a2") %>%
  select(name_long, pop, pop_growth, area_km2) %>%
  arrange(area_km2) %>% 
  mutate(pop_density = pop/area_km2) %>%
  rename(population = pop)

world_cont = world %>% 
  group_by(continent) %>% 
  summarize(pop_sum = sum(pop, na.rm = TRUE))
