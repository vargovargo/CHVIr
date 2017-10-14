rm(list=ls())

library(tidyverse)



testFile <- "~/CHVI_copy/BRACE_AirConditioning_797_CO_RE_CA.xlsx"

readCHVI <- function(fileName){
   temp <- readxl::read_excel(fileName, sheet="Data") %>% select(1:26) %>% .[,order(names(.))]
   
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

# work Local
  setwd("~/CHVI_copy/")

# work from the network
# setwd("//phitprlcsrvip04/OHEGroup/HCI/Data/CCHVI in one folder for web/CHVIcsvs/")

airConditioning <- readCHVI("./BRACE_AirConditioning_797_CO_RE_CA.xlsx")
carOwnership <- readCHVI("./BRACE_CarOwnership_37_CT_PL_CO_RE_CA.xlsx")
children <- readCHVI("./BRACE_children_788_CT_PL_CO_RE_CA.xlsx")
disability <- readCHVI("./BRACE_Disability_795_CT_PL_CO_RE_CA.xlsx")
elderly <- readCHVI("./BRACE_elderly65over_789_CT_PL_CO_RE_CA_11-8-2016.xlsx")
extremeHeat <- readCHVI("./BRACE_ExtremeHeat_791_CO.xlsx")
impervious <- readCHVI("./BRACE_ImperviousSurfaces_423_CT_PL_CO_RE_CA.xlsx")
insurance <- readCHVI("./BRACE_Insurance_795_CT_PL_CO_RE_CA.xlsx")
linguisticIso <- readCHVI("./BRACE_LinguisticIsolation_800_CT_PL_CO_RE_CA.xlsx")
outdoorWorkers <- readCHVI("./BRACE_OutdoorWorkers_790_CT_PL_CO_RE_CA.xlsx")
ozone <- readCHVI("./BRACE_Ozone_801_CT_PL_CO_RE_CA.xlsx")
pm25 <- readCHVI("./BRACE_PM25levels_776_CT_PL_CO_RE_CA.xlsx")
race <- readCHVI("./BRACE_race_795_CT_PL_CO_RE_CA.xlsx")
seaLevel <- readCHVI("./BRACE_SLR_784_CT_PL_CO_RE_CA_11-1-2016.xlsx")
treeCanopy <- readCHVI("./BRACE_TreeCanopy_458_CT_PL_CO_RE_CA.xlsx")
wildfire <- readCHVI("./BRACE_Wildfire_786_CT_PL_CO_RE_CA.xlsx")


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

#unique(allCHVI$ind_definition)

allCHVI %>% 
  filter(geotype == "CA" & estimate > 0 & ind_definition == "Percent of households with air conditioning" & race_eth_name !="Total")  %>% 
  ggplot(aes(x=county_name, y=estimate, fill=race_eth_name)) + geom_bar(stat = "identity", position="dodge") + ggtitle("CA % of households with air conditioning (by race)")  


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


