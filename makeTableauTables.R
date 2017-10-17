rm(list=ls())

library(tidyverse)

# work Local
setwd("~/CHVI_copy/CHVIcsvs/")

# work from the network
# setwd("//phitprlcsrvip04/OHEGroup/HCI/Data/CCHVI in one folder for web/CHVIcsvs/")

allCHVI <- read.csv("../allCHVI.csv", header=T)

chvisList <- data.frame(CHVI=unique(allCHVI$ind_definition), 
                        varName = c("airCond", 
                                    "vehOwn", 
                                    "young",
                                    "disability",
                                    "elderly",
                                    "extremeHeat",
                                    "impervious",
                                    "lackInsurance",
                                    "lingIso",
                                    "outdoorWorkers",
                                    "ozone",
                                    "finePM",
                                    "raceEthn",
                                    "seaLevel",
                                    "canopy",
                                    "wildfire"
                                    ))

turnTab <- function(indicator, name, geog){
  
 temp <- allCHVI %>% filter(ind_definition == indicator & geotype == geog) %>%
    mutate(state = "California", variable = name) %>%
    select(variable, county_name, state, estimate, region_name, race_eth_name, ind_definition) # %>% 
   # write.csv(paste0("./TableauCHVI/",name,"TAB.csv"), row.names=FALSE)
 return(temp)
}

CountyTab <- data.frame()

for(i in 1:nrow(chvisList)){
  
 abc <- turnTab(indicator = chvisList[i,"CHVI"], name = chvisList[i,"varName"], geog = "CO" )
  CountyTab <- bind_rows(CountyTab, abc)
  
}


