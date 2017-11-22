rm(list=ls())

library(tidyverse)

#############################


testFile <- "~/CHVI_copy/CHVIcsvs/BRACE_Disability_795_CT_PL_CO_RE_CA.csv"

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

airConditioning <- readCHVI("./BRACE_AirConditioning_797_CO_RE_CA.csv")
carOwnership <- readCHVI("./BRACE_CarOwnership_37_CT_PL_CO_RE_CA.csv")
children <- readCHVI("./BRACE_children_788_CT_PL_CO_RE_CA.csv")
disability <- readCHVI("./BRACE_Disability_795_CT_PL_CO_RE_CA.csv")
elderly <- readCHVI("./BRACE_elderly65over_789_CT_PL_CO_RE_CA_11-8-2016.csv")
extremeHeat <- readCHVI("./BRACE_ExtremeHeat_791_CO.csv")
impervious <- readCHVI("./BRACE_ImperviousSurfaces_423_CT_PL_CO_RE_CA.csv")
insurance <- readCHVI("./BRACE_Insurance_795_CT_PL_CO_RE_CA.csv")
linguisticIso <- readCHVI("./BRACE_LinguisticIsolation_800_CT_PL_CO_RE_CA.csv")
outdoorWorkers <- readCHVI("./BRACE_OutdoorWorkers_790_CT_PL_CO_RE_CA.csv")
ozone <- readCHVI("./BRACE_Ozone_801_CT_PL_CO_RE_CA.csv")
pm25 <- readCHVI("./BRACE_PM25levels_776_CT_PL_CO_RE_CA.csv")
race <- readCHVI("./BRACE_race_795_CT_PL_CO_RE_CA.csv")
seaLevel <- readCHVI("./BRACE_SLR_784_CT_PL_CO_RE_CA_11-1-2016.csv")
treeCanopy <- readCHVI("./BRACE_TreeCanopy_458_CT_PL_CO_RE_CA.csv")
wildfire <- readCHVI("./BRACE_Wildfire_786_CT_PL_CO_RE_CA.csv")


allCHVI <- bind_rows(airConditioning,
                     carOwnership, 
                     children, 
                     disability, 
                     elderly, 
                     extremeHeat, 
                     impervious, 
                     insurance, 
                     linguisticIso,
                     outdoorWorkers, 
                     ozone, 
                     pm25, 
                     race,
                     seaLevel,
                     treeCanopy, 
                     wildfire
                     ) %>%
  filter(ind_definition != "")

#write.csv(allCHVI, "~/CHVI_copy/allCHVI.csv", row.names=F)


#####################################
rm(list=ls())

library(tidyverse)


# work Local
setwd("~/CHVI_copy/CHVIcsvs/")

allCHVI <- read.csv("../allCHVI.csv", header=T)
unique(allCHVI$ind_definition)
unique(allCHVI$race_eth_name)

# racial disparity plot for tree canopy
allCHVI %>% 
  filter(geotype == "CT" & 
         ind_definition == "Percent of population aged 65 years or older", 
         race_eth_name %in% c("White","AfricanAm","Asian","Latino"))  %>% 
  ggplot(aes(x=numerator, colour=county_name)) + stat_ecdf() + facet_grid(region_name ~race_eth_name , scales = "free_x" ) + ggtitle("racial disparity plot for tree canopy")  

# racial disparity plot for vehicle ownership
allCHVI %>% 
  filter(geotype == "CT" & 
           ind_definition == "Percent without tree canopy coverage")  %>% 
  ggplot(aes(x=estimate, colour=race_eth_name)) + stat_ecdf() + facet_wrap(~ region_name) + ggtitle("racial disparity plot for vehicle ownership")  


# density plots
allCHVI %>% 
  filter(geotype == "CT" & 
           ind_definition %in% c("Percent of households with no vehicle ownership",
                                 "Percent of population currently living in very high wildfire risk areas",
                                 "Percent without tree canopy coverage",
                                 "Percent of population aged 65 years or older",
                                 "Percent of population age less than 5 years", 
                                 "Percent of households with no one aged > 14 years speaking English",
                                 "Percent of population with a disability",
                                 "Percent of adults aged 18 - 64 without health insurance", 
                                 "Population living in sea level rise inundation areas",
                                 "Percent of households with air conditioning"),
         race_eth_name %in% c("White","AfricanAm","Asian","Latino"))  %>% 
  ggplot(aes(x=estimate, fill=race_eth_name)) + geom_density(alpha= 0.3, position="stack") + facet_wrap(~ ind_definition + reportyear)

# box plots by race
allCHVI %>% 
  filter(geotype == "CT" & 
           ind_definition %in% c("Percent of households with no vehicle ownership",
                                 "Percent of population currently living in very high wildfire risk areas",
                                 "Percent without tree canopy coverage",
                                 "Percent of population aged 65 years or older",
                                 "Percent of population age less than 5 years", 
                                 "Percent of households with no one aged > 14 years speaking English",
                                 "Percent of population with a disability",
                                 "Percent of adults aged 18 - 64 without health insurance", 
                                 "Population living in sea level rise inundation areas",
                                 "Percent of households with air conditioning"),
         race_eth_name %in% c("White","AfricanAm","Asian","Latino"))  %>% 
  ggplot(aes(y=estimate, x=race_eth_name)) + geom_boxplot() + facet_wrap(~ ind_definition + reportyear)




#racial disparity plot for wildfire risk
allCHVI %>% 
  filter(geotype == "CT" & 
         ind_definition == "Percent of population currently living in very high wildfire risk areas", 
         race_eth_name %in% c("White","AfricanAm","Asian","Latino")) %>% 
  ggplot(aes(x=numerator, colour=race_eth_name)) + stat_ecdf() + facet_wrap(~ region_name ) + ggtitle("racial disparity plot for wildfire risk")  


allCHVI %>% 
  filter(geotype == "CT" & 
           ind_definition == "Percent of population currently living in very high wildfire risk areas", 
         race_eth_name %in% c("White","AfricanAm","Asian","Latino")) %>% 
  ggplot(aes(region_name, estimate, colour=race_eth_name)) + geom_boxplot() + ggtitle("% under 5") 


###############################
# scatter plot heat and elderly
heat <- allCHVI %>% 
  filter(geotype == "CO" & 
         ind_definition %in%  c("Projected number of extreme heat days")) %>% 
  mutate(indicator = "ExtremeHeatDays") %>%
  select(county_name, estimate, region_name, indicator, reportyear) %>%
  spread(key = indicator, value = estimate)


elderly <- allCHVI %>% 
  filter(geotype == "CO" & 
           ind_definition %in%  c("Percent of population aged 65 years or older")) %>% 
  mutate(indicator = "PercentElderly") %>%
  select(county_name, estimate, indicator, race_eth_name) %>%
  spread(key = indicator, value = estimate) %>% 
  full_join(heat)

write.csv(elderlyHeat, "~/CHVI_copy/CHVIcsvs/TableauCHVI/crosstabElderlyHeat.csv", row.names = F)

elderly  %>%
  ggplot(aes(x=PercentElderly, y=ExtremeHeatDays, colour=race_eth_name)) + geom_point() + facet_grid(region_name ~ reportyear) + ggtitle("scatter plot")  

###############################
# scatter plot heat and race
heat <- allCHVI %>% 
  filter(geotype == "CO" & 
           ind_definition %in%  c("Projected number of extreme heat days")) %>% 
  mutate(indicator = "ExtremeHeatDays") %>%
  select(county_name, estimate, region_name, indicator, reportyear) %>%
  spread(key = indicator, value = estimate)


raceHeat <- allCHVI %>% 
  filter(geotype == "CO" &
    ind_definition == "Percent of population by race and ethnicity" &
      race_eth_name %in% c("White","AfricanAm","Asian","Latino")) %>% 
  mutate(indicator = "PercentByRace") %>%
  select(county_name, estimate, indicator, race_eth_name) %>%
  spread(key = indicator, value = estimate) %>% 
  full_join(heat)

write.csv(raceHeat, "~/CHVI_copy/CHVIcsvs/TableauCHVI/crosstabRaceHeat.csv", row.names = F)

raceHeat  %>%
  ggplot(aes(x=PercentByRace, y=ExtremeHeatDays, fill=race_eth_name)) + geom_bar(stat="identity", position ="stack") + facet_grid(reportyear ~ county_name) + ggtitle("scatter plot")  

  
  
  
  
  
  
  
  
allCHVI %>% 
  filter(geotype == "CO" & 
           ind_definition == "Percent of population aged 65 years or older", 
         race_eth_name %in% c("White","AfricanAm","Asian","Latino")) %>% 
  ggplot(aes(x=county_name, y=estimate , colour=race_eth_name)) + geom_point() + ggtitle("wildfire risk by county/race")  +coord_flip()

allCHVI %>% 
  filter(geotype == "CO" & 
           ind_definition == "Percent without tree canopy coverage", 
         race_eth_name %in% c("White","AfricanAm","Asian","Latino")) %>% 
  ggplot(aes(x=ind_definition, y=estimate , fill=race_eth_name)) + geom_bar(stat="identity", position="dodge") + ggtitle("wildfire risk by county/race")  + facet_wrap(~county_name)



  



# add error bars

allCHVI %>% 
  filter(geotype == "CA" & estimate > 0 & ind_definition == "Percent of households with air conditioning" & race_eth_name !="Total") %>%
  ggplot(aes(x=county_name, y=estimate,ymax = UL_95CI, ymin=LL_95CI, fill=race_eth_name)) + geom_bar(stat = "identity", position="dodge") + ggtitle("CA % of households with air conditioning (by race)") + geom_errorbar(position="dodge")
# the result is concerning because the 'estimates' do no always fall within the confidence intervals...
# we can verify this by looking at the data that we send to the plot 
allCHVI %>% 
  filter(geotype == "CA" & estimate > 0 & ind_definition == "Percent of households with air conditioning" & race_eth_name !="Total") %>%
  select(LL_95CI, estimate, UL_95CI)
  
# recreate a figure from the county VA report

# get list of all available indicators
unique(allCHVI$ind_definition)

# then examine the records
allCHVI %>% 
  filter(geotype == "CO" & ind_definition == "Projected number of extreme heat days") %>% head()

allCHVI %>% 
  filter(geotype == "CO" & ind_definition == "Projected number of extreme heat days") %>% 
  ggplot(aes(x=county_name, y=estimate, fill=region_name, group=region_name)) + geom_bar(stat = "identity", position="dodge") + ggtitle("# of projected Extreme Heat Days")  + facet_grid(.~ reportyear) + theme(axis.text.x = element_text(angle = 90, hjust = 1,size=7))


