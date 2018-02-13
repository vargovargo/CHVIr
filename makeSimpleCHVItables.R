rm(list=ls())

library(tidyverse)

# work locally
# setwd(dir = "~/CHVI_copy/CHVIcsvs/")

# work from the network
# setwd("//phitprlcsrvip04/OHEGroup/HCI/Data/CCHVI in one folder for web/CHVIcsvs/")


#############################
######### Functions #########
#############################

simplifyCHVI <- function(indicator = "indicator", geography = "CT"){
  
   # find url associated with this indicator
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
   
    temp <- temp %>% 
      filter(geotype == geography, race_eth_name == "Total")  %>% 
      select(ind_definition, county_name, geotypevalue,geotype, estimate,  ) %>%
      mutate(ct10= as.character(paste0("0",geotypevalue))) %>%
      write.csv(paste0("~/CHVI_",indicator,"_",geography,".csv"),row.names=F)
    
   return(temp)
}

indicators <- c("wildfire","ozone","pm","SLR")
geographies <- c("CO","CT")

for(indicator in indicators){
  for(geography in geographies){
    simplifyCHVI(indicator = indicator, geography = geography)
  }
}
  
