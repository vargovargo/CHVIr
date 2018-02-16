rm(list=ls())

library(tidyverse)

#############################
######### Function ##########
#############################
library(sf)

tracts <- st_read("~/CHVI_copy/shapes/census_tracts2010.shp")
counties <- st_read("~/CHVI_copy/shapes/Counties2010.shp")



# this function reads in the excell file from our CCHEP/CalBRACE site based on the indicator name you provide 
# the function also take the geography you would like, mostly county "CO" or census tract "CT" 

simplifyCHVI <- function(indicator = "indicator"){
  
   # find url associated with this indicator
   # more variables can be added can be added here
   # this is where it translates the indicator name that was input into the url for the coresponding file on our website
   fileName <- ifelse(indicator %in% c("wildfire","WildFire", "Fire","fire","wild fire","WF"),
                 "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/BRACE_Wildfire_786_CT_PL_CO_RE_CA.xlsx",
                 ifelse(indicator %in% c("sea level rise","SLR", "sea level","Sea Level Rise", "Sea Level", "sealevel"),
                        "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/BRACE_SLR_784_CT_PL_CO_RE_CA_11-1-2016.xlsx",
                        ifelse(indicator %in% c("PM25","Particulate Matter", "particulate matter","PM2.5", "pm", "PM"),
                               "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/BRACE_PM25levels_776_CT_PL_CO_RE_CA.XLSX",
                               ifelse(indicator %in% c("Ozone","ozone", "o3","O3"),
                                      "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/BRACE_Ozone_801_CT_PL_CO_RE_CA.XLSX","no path"
                               ))))
  
  
   p1f <- tempfile()
   download.file(fileName, p1f, mode="wb")
   temp <- readxl::read_xlsx(p1f, sheet="Data") %>% select(1:26) %>% .[,order(names(.))] 
  
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
   
   
   # we will need to change these settings for some indicators that have different use of the strata or report years (heat)
   temp2 <- temp %>% 
      filter(geotype == "CT", race_eth_name == "Total")  %>% # here's where we can filter the data further
      select(ind_definition, county_name, geotypevalue, geotype, estimate, region_name, LL_95CI, UL_95CI, numerator, denominator) %>% # the file is parsed down to these columns only
      mutate(ct10= as.character(paste0("0",geotypevalue))) 
   
   temp2 %>% write.csv(paste0("~/CHVI_",indicator,"_tract.csv"),row.names=F) # it writes a file to your home directory with the name including the indicator and the geography
    
   tracts %>%  left_join(temp2) %>% st_transform(crs = 4326) %>% 
       st_write(dsn = normalizePath(paste0("~/CHVI_",indicator,"_tract.shp")), delete_layer=TRUE)
   
   
   temp2 <- temp %>% 
     filter(geotype == "CO", race_eth_name == "Total")  %>% # here's where we can filter the data further
     select(ind_definition, county_name, geotypevalue, geotype, estimate, region_name, LL_95CI, UL_95CI, numerator, denominator) %>% # the file is parsed down to these columns only
     mutate(COUNTYFI_1= as.character(paste0("0",geotypevalue))) 
   
   temp2 %>% write.csv(paste0("~/CHVI_",indicator,"_county.csv"),row.names=F) # it writes a file to your home directory with the name including the indicator and the geography
   
   counties %>%  left_join(temp2) %>% st_transform(crs = 4326) %>% 
     st_write(dsn = normalizePath(paste0("~/CHVI_",indicator,"_county.shp")), delete_layer=TRUE)
   
   return(temp)
}


# here we can loop the indicators and geographies to create all the tables we'll send to connect to shapefiles
indicators <- c("wildfire","ozone","pm","SLR")

for(indicator in indicators){
    simplifyCHVI(indicator = indicator) 
}


##########################################################
# direct application of the code if you know the CHVI url
##########################################################
children <- "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/BRACE_children_788_CT_PL_CO_RE_CA.XLSX"
elderly <- "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/BRACE_elderly65over_789_CT_PL_CO_RE_CA_11-8-2016.XLSX"
race <- "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/BRACE_race_795_CT_PL_CO_RE_CA.XLSX"
outdoor <- "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/BRACE_OutdoorWorkers_790_CT_PL_CO_RE_CA.XLSX"
vehicles <- "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/BRACE_CarOwnership_37_CT_PL_CO_RE_CA.XLSX"
linguistic <- "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/BRACE_LinguisticIsolation_800_CT_PL_CO_RE_CA.XLSX"
disability <- "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/BRACE_Disability_795_CT_PL_CO_RE_CA.XLSX"
insurance <- "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/BRACE_Insurance_795_CT_PL_CO_RE_CA.XLSX"
crime <- "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/HCI_Crime_752_PL_CO_RE_CA_2000-2013_21OCT15.xlsx"



indicator <- "crime"

fileName <- crime
p1f <- tempfile()
download.file(fileName, p1f, mode="wb")
temp <- readxl::read_xlsx(p1f, sheet="Data") %>% select(1:26) %>% .[,order(names(.))] 

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


# we will need to change these settings for some indicators that have different use of the strata or report years (heat)
temp2 <- temp %>% 
  filter(geotype == "CO", race_eth_name == "Total")  %>% # here's where we can filter the data further
  select(ind_definition, county_name, geotypevalue, geotype, estimate, region_name, LL_95CI, UL_95CI, numerator, denominator) %>% # the file is parsed down to these columns only
  mutate(ct10= as.character(paste0("0",geotypevalue))) 

temp2 %>% write.csv(paste0("~/CHVI_",indicator,"_tract.csv"),row.names=F) # it writes a file to your home directory with the name including the indicator and the geography

tracts %>%  left_join(temp2) %>% st_transform(crs = 4326) %>% 
  st_write(dsn = normalizePath(paste0("~/CHVI_",indicator,"_tract.shp")), delete_layer=TRUE)


temp2 <- temp %>% 
  filter(geotype == "CO", race_eth_name == "Total")  %>% # here's where we can filter the data further
  select(ind_definition, county_name, geotypevalue, geotype, estimate, region_name, LL_95CI, UL_95CI, numerator, denominator) %>% # the file is parsed down to these columns only
  mutate(COUNTYFI_1= as.character(paste0("0",geotypevalue))) 

temp2 %>% write.csv(paste0("~/CHVI_",indicator,"_county.csv"),row.names=F) # it writes a file to your home directory with the name including the indicator and the geography

counties %>%  left_join(temp2) %>% st_transform(crs = 4326) %>% 
  st_write(dsn = normalizePath(paste0("~/CHVI_",indicator,"_county.shp")), delete_layer=TRUE)

