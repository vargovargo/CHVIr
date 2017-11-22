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
  


#write.csv(allCHVI, "~/CHVI_copy/allCHVI.csv", row.names=F)

unique(ozone$ind_definition)

  
  
  
  
ozone %>% 
  filter(geotype == "CO"  & 
           race_eth_name !="Total" &
           denominator > 100)  %>%
  select(county_name, estimate, race_eth_name, ind_definition) %>%
  full_join((group_by(.,county_name,ind_definition) %>%
                summarize(privValue = min(estimate, na.rm=T))
  )) %>%
  mutate(absDiff = estimate - privValue,
         relDiff = (absDiff/privValue)) %>%
  ggplot(aes(x=county_name, y=absDiff, colour=race_eth_name)) + geom_point() + coord_flip() + ggtitle("county-specific Ozone absolute Differences")
  

pm25 %>% 
  filter(geotype == "CO"  & 
           race_eth_name !="Total" &
           denominator > 100)  %>%
  select(county_name, estimate, race_eth_name, ind_definition) %>%
  full_join((group_by(.,county_name,ind_definition) %>%
               summarize(privValue = min(estimate, na.rm=T))
  )) %>%
  mutate(absDiff = estimate - privValue,
         relDiff = (absDiff/privValue)) %>%
  ggplot(aes(x=county_name, y=100*relDiff, colour=race_eth_name)) + geom_point() + coord_flip() + ggtitle("county-specific PM2.5 Relative Differences")



disability %>% 
  filter(geotype == "CO" &
           strata_one_name !="total") %>%
  select(county_name, estimate, strata_one_name, ind_definition) %>%
  ggplot(aes(x= county_name, y=estimate, fill=strata_one_name)) + 
  geom_bar(stat="identity",position="stack") + coord_flip() + ggtitle("county-specific %Disability Relative Differences")

disability %>% 
  filter(geotype == "CO") %>%
  select(county_name, estimate, strata_one_name, ind_definition) %>%
  spread(key=strata_one_name, value=estimate)%>%
  mutate(mentalPct = mental/total, physicalPCT=physical/total) %>%
  select(county_name, mentalPct, physicalPCT, ind_definition) %>%
  gather(mentalPct, physicalPCT, key = type, value = PercentageOfDiasbility) %>%
  ggplot(aes(x=county_name, y=PercentageOfDiasbility, fill=type)) + geom_bar(stat="identity",position="stack") + coord_flip() + ggtitle("county-specific %Disability Relative Differences")

insurance %>% 
  filter(geotype == "CO"  & 
           race_eth_name !="Total" &
           denominator > 100)  %>%
  select(county_name, estimate, race_eth_name, ind_definition) %>%
  full_join((group_by(.,county_name,ind_definition) %>%
               summarize(privValue = min(estimate, na.rm=T))
  )) %>%
  mutate(absDiff = estimate - privValue,
         relDiff = (absDiff/privValue))

  


linguisticIso %>% 
  filter(geotype == "CO")  %>%
  select(county_name, estimate, region_name, strata_one_name, ind_definition, LL_95CI, UL_95CI, numerator, denominator) %>%
  filter(strata_one_name != "All Non-English") %>%
  ggplot(aes(x=ind_definition, y=numerator, fill=strata_one_name)) + 
  geom_bar(stat="identity", position="stack") +
  facet_wrap(~ region_name + county_name) + 
  ggtitle("South Central Valley Linguistic Isolation") +coord_flip()

