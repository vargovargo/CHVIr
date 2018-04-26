rm(list = ls())

library(tidyverse)

workingPath <- "~/CHVI_copy/data/" # work local
# workingPath <-
#   "//phdeorlcsrvip01/Crossbranch/CDC_BRACE/Data/CHVI-CHPR Data/data/" # work local

###  filenames ###
wildfire <-
  "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/BRACE_Wildfire_786_CT_PL_CO_RE_CA.xlsx"
slr <-
  "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/BRACE_SLR_784_CT_PL_CO_RE_CA_11-1-2016.xlsx"
ozone <-
  "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/BRACE_Ozone_801_CT_PL_CO_RE_CA.XLSX"
pm <-
  "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/BRACE_PM25levels_776_CT_PL_CO_RE_CA.XLSX"
heat <-
  "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/BRACE_ExtremeHeat_791_CO.xlsx"
children <-
  "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/BRACE_children_788_CT_PL_CO_RE_CA.XLSX"
elderly <-
  "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/BRACE_elderly65over_789_CT_PL_CO_RE_CA_11-8-2016.XLSX"
race <-
  "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/BRACE_race_795_CT_PL_CO_RE_CA.XLSX"
outdoor <-
  "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/BRACE_OutdoorWorkers_790_CT_PL_CO_RE_CA.XLSX"
vehicles <-
  "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/BRACE_CarOwnership_37_CT_PL_CO_RE_CA.XLSX"
linguistic <-
  "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/BRACE_LinguisticIsolation_800_CT_PL_CO_RE_CA.XLSX"
disability <-
  "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/BRACE_Disability_795_CT_PL_CO_RE_CA.XLSX"
insurance <-
  "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/BRACE_Insurance_795_CT_PL_CO_RE_CA.XLSX"
crime <-
  "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/HCI_Crime_752_PL_CO_RE_CA_2000-2013_21OCT15.xlsx"
poverty <-
  "~/CHVI_copy/HCI_PovertyRate_754_CT_PL_CO_RE_CA_1-22-14.xlsx"
education <- "~/CHVI_copy/ed_attain_ge_hs_output04-14-13.xlsx"
canopy <-
  "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/BRACE_TreeCanopy_458_CT_PL_CO_RE_CA.xlsx"
impervious <-
  "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/BRACE_ImperviousSurfaces_423_CT_PL_CO_RE_CA.xlsx"
ac <-
  "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/BRACE_AirConditioning_797_CO_RE_CA.xlsx"


namesList <- c(
  "def",
  "county",
  "geotypv",
  "geotype",
  "est",
  "region",
  "LL95",
  "UL95",
  "numratr",
  "denmntr",
  "race",
  "COUNTYFI_1",
  "strata",
  "ind"
)

#############################
######### Function ##########
#############################


# this function reads in the excell file from our CCHEP/CalBRACE site based on the indicator name you provide
# the function also take the geography you would like, mostly county "CO" or census tract "CT"

simplifyCHVI <- function(indicator = "indicator") {
  # find url associated with this indicator
  # more variables can be added can be added here
  # this is where it translates the indicator name that was input into the url for the coresponding file on our website
  fileName <-
    ifelse(
      indicator %in% c("wildfire", "WildFire", "Fire", "fire", "wild fire", "WF"),
      "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/BRACE_Wildfire_786_CT_PL_CO_RE_CA.xlsx",
      ifelse(
        indicator %in% c(
          "sea level rise",
          "SLR",
          "sea level",
          "Sea Level Rise",
          "Sea Level",
          "sealevel"
        ),
        "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/BRACE_SLR_784_CT_PL_CO_RE_CA_11-1-2016.xlsx",
        ifelse(
          indicator %in% c(
            "PM25",
            "Particulate Matter",
            "particulate matter",
            "PM2.5",
            "pm",
            "PM"
          ),
          "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/BRACE_PM25levels_776_CT_PL_CO_RE_CA.XLSX",
          ifelse(
            indicator %in% c("Ozone", "ozone", "o3", "O3"),
            "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/BRACE_Ozone_801_CT_PL_CO_RE_CA.XLSX",
            ifelse(
              indicator %in% c("elderly", "old", "Elderly", "over65", "Over65", "65+"),
              "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/BRACE_elderly65over_789_CT_PL_CO_RE_CA_11-8-2016.XLSX",
              ifelse(
                indicator %in% c("children", "young", "Children"),
                "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/BRACE_children_788_CT_PL_CO_RE_CA.XLSX",
                ifelse(
                  indicator %in% c("outdoor workers", "Outdoor", "outdoor", "workers"),
                  "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/BRACE_OutdoorWorkers_790_CT_PL_CO_RE_CA.XLSX",
                  ifelse(
                    indicator %in% c(
                      "crime",
                      "ViolentCrime",
                      "violent crime",
                      "Crime",
                      "violentCrime"
                    ),
                    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/HCI_Crime_752_PL_CO_RE_CA_2000-2013_21OCT15.xlsx",
                    ifelse(
                      indicator %in% c("race", "Race", "ethnicity", "Ethnicity"),
                      "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/BRACE_race_795_CT_PL_CO_RE_CA.XLSX",
                      ifelse(
                        indicator %in% c("vehicle", "Vehicle", "vehicles", "Vehicles"),
                        "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/BRACE_CarOwnership_37_CT_PL_CO_RE_CA.XLSX",
                        ifelse(
                          indicator %in% c(
                            "linguisticIsolation",
                            "linguistic",
                            "LinguisticIsolation",
                            "Linguistic"
                          ),
                          "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/BRACE_LinguisticIsolation_800_CT_PL_CO_RE_CA.XLSX",
                          ifelse(
                            indicator %in% c(
                              "insurance",
                              "Insurance",
                              "healthInsurance",
                              "HealthInsurance"
                            ),
                            "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/BRACE_Insurance_795_CT_PL_CO_RE_CA.XLSX",
                            "no path"
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  
  p1f <- tempfile()
  download.file(fileName, p1f, mode = "wb")
  temp <-
    readxl::read_xlsx(p1f, sheet = "Data") %>% select(1:26) %>% .[, order(names(.))]
  
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
    filter(geotype == "CT")  %>% # here's where we can filter the data further
    select(
      ind_definition,
      county_name,
      geotypevalue,
      geotype,
      estimate,
      region_name,
      LL_95CI,
      UL_95CI,
      numerator,
      denominator,
      race_eth_name
    ) %>% # the file is parsed down to these columns only
    mutate(ct10 = as.character(geotypevalue),
           strat = "none",
           ind = indicator)
  
  
  names(temp2) <- namesList
  
  temp2 %>% write.csv(paste0(workingPath, "tables/tracts/CHVI_", indicator, "_tract.csv"),
                      row.names = F) # it writes a file to your home directory with the name including the indicator and the geography
  
  # CHVItracts <- tracts %>%  left_join(temp2) %>% st_transform(crs = 4326)
  #
  #
  # CHVItracts %>%
  #   select(GEO_ID,STATE,COUNTY,TRACT,NAME,LSAD,ct10,ct9,indic,cnty,geotypv,geotype,est,
  #          region,LL95,UL95,numratr,denmntr,geometry) %>%
  #   st_write(dsn = normalizePath(paste0(workingPath,"tables/CHVI_",indicator,"_tract.shp")), delete_layer=TRUE)
  
  temp2 <- temp %>%
    filter(geotype == "CO")  %>% # here's where we can filter the data further
    select(
      ind_definition,
      county_name,
      geotypevalue,
      geotype,
      estimate,
      region_name,
      LL_95CI,
      UL_95CI,
      numerator,
      denominator,
      race_eth_name
    ) %>% # the file is parsed down to these columns only
    mutate(
      COUNTYFI_1 = as.character(geotypevalue),
      strat = "none",
      ind = indicator
    )
  
  names(temp2) <- namesList
  
  temp2 %>% write.csv(
    paste0(
      workingPath,
      "tables/counties/CHVI_",
      indicator,
      "_county.csv"
    ),
    row.names = F
  ) # it writes a file to your home directory with the name including the indicator and the geography
  
  # CHVIcounties <- counties %>%  left_join(temp2) %>% st_transform(crs = 4326)
  #
  # CHVIcounties %>%
  #   st_write(dsn = normalizePath(paste0(workingPath,"tables/counties/CHVI_",indicator,"_county.shp")), delete_layer=TRUE)
  
  
  return(temp)
}



# here we can loop the indicators and geographies to create all the tables we'll send to connect to shapefiles
indicators <-
  c("wildfire",
    "ozone",
    "pm",
    "SLR",
    "children",
    "elderly",
    "vehicles",
    "outdoor")

for (indicator in indicators) {
  simplifyCHVI(indicator = indicator)
}

simplifyCHVI("wildfire")


#### run for air conditioning ##############################

indicator <- "ac"

fileName <- ac
p1f <- tempfile()
download.file(fileName, p1f, mode = "wb")
temp <-
  readxl::read_xlsx(p1f, sheet = "Data") %>% select(1:26) %>% .[, order(names(.))]

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

# parse and write  COUNTY data
temp2 <- temp %>%
  filter(geotype == "CO")  %>% # here's where we can filter the data further
  select(
    ind_definition,
    county_name,
    geotypevalue,
    geotype,
    estimate,
    region_name,
    LL_95CI,
    UL_95CI,
    numerator,
    denominator,
    race_eth_name
  ) %>% # the file is parsed down to these columns only
  mutate(COUNTYFI_1 = as.character(geotypevalue),
         strata = "none",
         ind = indicator)

names(temp2) <- namesList

temp2 %>% write.csv(paste0(
  workingPath,
  "tables/counties/CHVI_",
  indicator,
  "_county.csv"
),
row.names = F) # it writes a file to your home directory with the name including the indicator and the geography



#### run for extreme heat ##############################

indicator <- "heat"

fileName <- heat
p1f <- tempfile()
download.file(fileName, p1f, mode = "wb")
temp <-
  readxl::read_xlsx(p1f, sheet = "Data") %>% select(1:26) %>% .[, order(names(.))]

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

# run it for pop weigthed imperv
strat <- '2050' # use 2050 or 2085

# parse and write 2050 COUNTY data
temp2 <- temp %>%
  filter(geotype == "CO", reportyear == strat)  %>% # here's where we can filter the data further
  select(
    ind_definition,
    county_name,
    geotypevalue,
    geotype,
    estimate,
    region_name,
    LL_95CI,
    UL_95CI,
    numerator,
    denominator,
    race_eth_name
  ) %>% # the file is parsed down to these columns only
  mutate(COUNTYFI_1 = as.character(geotypevalue),
         strata = strat,
         ind = indicator)

names(temp2) <- namesList

temp2 %>% write.csv(
  paste0(
    workingPath,
    "tables/counties/CHVI_",
    indicator,
    "_",
    strat,
    "_county.csv"
  ),
  row.names = F
) # it writes a file to your home directory with the name including the indicator and the geography

strat <- '2085' # use 2050 or 2085

# parse and write 2085 COUNTY data
temp2 <- temp %>%
  filter(geotype == "CO", reportyear == strat)  %>% # here's where we can filter the data further
  select(
    ind_definition,
    county_name,
    geotypevalue,
    geotype,
    estimate,
    region_name,
    LL_95CI,
    UL_95CI,
    numerator,
    denominator,
    race_eth_name
  ) %>% # the file is parsed down to these columns only
  mutate(COUNTYFI_1 = as.character(geotypevalue),
         strata = strat,
         ind = indicator)

names(temp2) <- namesList

temp2 %>% write.csv(
  paste0(
    workingPath,
    "tables/counties/CHVI_",
    indicator,
    "_",
    strat,
    "_county.csv"
  ),
  row.names = F
) # it writes a file to your home directory with the name including the indicator and the geography



#### run for impervious (twice) ##############################

indicator <- "impervious"

fileName <- impervious
p1f <- tempfile()
download.file(fileName, p1f, mode = "wb")
temp <-
  readxl::read_xlsx(p1f, sheet = "Data") %>% select(1:26) %>% .[, order(names(.))]

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

# run it for pop weigthed imperv
strat <-
  'population-weighted' # use population-weighted or area-weighted for imperv

# parse and write pop-weighted TRACT data
temp2 <- temp %>%
  filter(geotype == "CT",
         race_eth_name == "Total",
         strata_one_name == strat)  %>% # here's where we can filter the data further
  select(
    ind_definition,
    county_name,
    geotypevalue,
    geotype,
    estimate,
    region_name,
    LL_95CI,
    UL_95CI,
    numerator,
    denominator,
    race_eth_name
  ) %>% # the file is parsed down to these columns only
  mutate(ct10 = as.character(geotypevalue),
         strata = strat,
         ind = indicator)

names(temp2) <- namesList

temp2 %>% write.csv(
  paste0(
    workingPath,
    "tables/tracts/CHVI_",
    indicator,
    "_",
    strat,
    "_tract.csv"
  ),
  row.names = F
) # it writes a file to your home directory with the name including the indicator and the geography

# parse and write pop-weighted COUNTY data
temp2 <- temp %>%
  filter(geotype == "CO",
         race_eth_name == "Total",
         strata_one_name == strat)  %>% # here's where we can filter the data further
  select(
    ind_definition,
    county_name,
    geotypevalue,
    geotype,
    estimate,
    region_name,
    LL_95CI,
    UL_95CI,
    numerator,
    denominator,
    race_eth_name
  ) %>% # the file is parsed down to these columns only
  mutate(COUNTYFI_1 = as.character(geotypevalue),
         strata = strat,
         ind = indicator)

names(temp2) <- namesList

temp2 %>% write.csv(
  paste0(
    workingPath,
    "tables/counties/CHVI_",
    indicator,
    "_",
    strat,
    "_county.csv"
  ),
  row.names = F
) # it writes a file to your home directory with the name including the indicator and the geography

# run it for area weigthed imperv
strat <-
  'area-weighted' # use population-weighted or area-weighted for imperv

# parse and write area-weighted TRACT data
temp2 <- temp %>%
  filter(geotype == "CT",
         race_eth_name == "Total",
         strata_one_name == strat)  %>% # here's where we can filter the data further
  select(
    ind_definition,
    county_name,
    geotypevalue,
    geotype,
    estimate,
    region_name,
    LL_95CI,
    UL_95CI,
    numerator,
    denominator,
    race_eth_name
  ) %>% # the file is parsed down to these columns only
  mutate(ct10 = as.character(geotypevalue),
         strata = strat,
         ind = indicator)


names(temp2) <- namesList

temp2 %>% write.csv(
  paste0(
    workingPath,
    "tables/tracts/CHVI_",
    indicator,
    "_",
    strat,
    "_tract.csv"
  ),
  row.names = F
) # it writes a file to your home directory with the name including the indicator and the geography

# parse and write area-weighted COUNTY data
temp2 <- temp %>%
  filter(geotype == "CO",
         race_eth_name == "Total",
         strata_one_name == strat)  %>% # here's where we can filter the data further
  select(
    ind_definition,
    county_name,
    geotypevalue,
    geotype,
    estimate,
    region_name,
    LL_95CI,
    UL_95CI,
    numerator,
    denominator,
    race_eth_name
  ) %>% # the file is parsed down to these columns only
  mutate(COUNTYFI_1 = as.character(geotypevalue),
         strata = strat,
         ind = indicator)

names(temp2) <- namesList

temp2 %>% write.csv(
  paste0(
    workingPath,
    "tables/counties/CHVI_",
    indicator,
    "_",
    strat,
    "_county.csv"
  ),
  row.names = F
) # it writes a file to your home directory with the name including the indicator and the geography



#### run for canopy (twice) ##############################

indicator <- "canopy"

fileName <- canopy
p1f <- tempfile()
download.file(fileName, p1f, mode = "wb")
temp <-
  readxl::read_xlsx(p1f, sheet = "Data") %>% select(1:26) %>% .[, order(names(.))]

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

# run it for pop weigthed canopy
strat <-
  'population-weighted' # use population-weighted or area-weighted for canopy

# parse and write pop-weighted TRACT data
temp2 <- temp %>%
  filter(geotype == "CT",
         race_eth_name == "Total",
         strata_one_name == strat)  %>% # here's where we can filter the data further
  select(
    ind_definition,
    county_name,
    geotypevalue,
    geotype,
    estimate,
    region_name,
    LL_95CI,
    UL_95CI,
    numerator,
    denominator,
    race_eth_name
  ) %>% # the file is parsed down to these columns only
  mutate(ct10 = as.character(geotypevalue),
         strata = strat,
         ind = indicator)

names(temp2) <- namesList

temp2 %>% write.csv(
  paste0(
    workingPath,
    "tables/tracts/CHVI_",
    indicator,
    "_",
    strat,
    "_tract.csv"
  ),
  row.names = F
) # it writes a file to your home directory with the name including the indicator and the geography

# parse and write pop-weighted COUNTY data
temp2 <- temp %>%
  filter(geotype == "CO",
         race_eth_name == "Total",
         strata_one_name == strat)  %>% # here's where we can filter the data further
  select(
    ind_definition,
    county_name,
    geotypevalue,
    geotype,
    estimate,
    region_name,
    LL_95CI,
    UL_95CI,
    numerator,
    denominator,
    race_eth_name
  ) %>% # the file is parsed down to these columns only
  mutate(COUNTYFI_1 = as.character(geotypevalue),
         strata = strat,
         ind = indicator)

names(temp2) <- namesList

temp2 %>% write.csv(
  paste0(
    workingPath,
    "tables/counties/CHVI_",
    indicator,
    "_",
    strat,
    "_county.csv"
  ),
  row.names = F
) # it writes a file to your home directory with the name including the indicator and the geography

# run it for area weigthed canopy
strat <-
  'area-weighted' # use population-weighted or area-weighted for canopy

# parse and write area-weighted TRACT data
temp2 <- temp %>%
  filter(geotype == "CT",
         race_eth_name == "Total",
         strata_one_name == strat)  %>% # here's where we can filter the data further
  select(
    ind_definition,
    county_name,
    geotypevalue,
    geotype,
    estimate,
    region_name,
    LL_95CI,
    UL_95CI,
    numerator,
    denominator,
    race_eth_name
  ) %>% # the file is parsed down to these columns only
  mutate(ct10 = as.character(geotypevalue),
         strata = strat,
         ind = indicator)


names(temp2) <- namesList

temp2 %>% write.csv(
  paste0(
    workingPath,
    "tables/tracts/CHVI_",
    indicator,
    "_",
    strat,
    "_tract.csv"
  ),
  row.names = F
) # it writes a file to your home directory with the name including the indicator and the geography

# parse and write area-weighted COUNTY data
temp2 <- temp %>%
  filter(geotype == "CO",
         race_eth_name == "Total",
         strata_one_name == strat)  %>% # here's where we can filter the data further
  select(
    ind_definition,
    county_name,
    geotypevalue,
    geotype,
    estimate,
    region_name,
    LL_95CI,
    UL_95CI,
    numerator,
    denominator,
    race_eth_name
  ) %>% # the file is parsed down to these columns only
  mutate(COUNTYFI_1 = as.character(geotypevalue),
         strata = strat,
         ind = indicator)

names(temp2) <- namesList

temp2 %>% write.csv(
  paste0(
    workingPath,
    "tables/counties/CHVI_",
    indicator,
    "_",
    strat,
    "_county.csv"
  ),
  row.names = F
) # it writes a file to your home directory with the name including the indicator and the geography


#### run for disability (thrice) ##############################

indicator <- "disability"

fileName <- disability
p1f <- tempfile()
download.file(fileName, p1f, mode = "wb")
temp <-
  readxl::read_xlsx(p1f, sheet = "Data") %>% select(1:26) %>% .[, order(names(.))]

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

strat <- 'mental' # use metal, physical, total for disability

# we will need to change these settings for some indicators that have different use of the strata or report years (heat)
temp2 <- temp %>%
  filter(geotype == "CT",
         race_eth_name == "Total",
         strata_one_name == strat)  %>% # here's where we can filter the data further
  select(
    ind_definition,
    county_name,
    geotypevalue,
    geotype,
    estimate,
    region_name,
    LL_95CI,
    UL_95CI,
    numerator,
    denominator,
    race_eth_name
  ) %>% # the file is parsed down to these columns only
  mutate(ct10 = as.character(geotypevalue),
         strata = strat,
         ind = indicator)


names(temp2) <- namesList

temp2 %>% write.csv(
  paste0(
    workingPath,
    "tables/tracts/CHVI_",
    indicator,
    "_",
    strat,
    "_tract.csv"
  ),
  row.names = F
) # it writes a file to your home directory with the name including the indicator and the geography

temp2 <- temp %>%
  filter(geotype == "CO",
         race_eth_name == "Total",
         strata_one_name == strat)  %>% # here's where we can filter the data further
  select(
    ind_definition,
    county_name,
    geotypevalue,
    geotype,
    estimate,
    region_name,
    LL_95CI,
    UL_95CI,
    numerator,
    denominator,
    race_eth_name
  ) %>% # the file is parsed down to these columns only
  mutate(COUNTYFI_1 = as.character(geotypevalue),
         strata = strat,
         ind = indicator)

names(temp2) <- namesList

temp2 %>% write.csv(
  paste0(
    workingPath,
    "tables/counties/CHVI_",
    indicator,
    "_",
    strat,
    "_county.csv"
  ),
  row.names = F
) # it writes a file to your home directory with the name including the indicator and the geography


strat <- 'physical' # use metal, physical, total for disability

# we will need to change these settings for some indicators that have different use of the strata or report years (heat)
temp2 <- temp %>%
  filter(geotype == "CT",
         race_eth_name == "Total",
         strata_one_name == strat)  %>% # here's where we can filter the data further
  select(
    ind_definition,
    county_name,
    geotypevalue,
    geotype,
    estimate,
    region_name,
    LL_95CI,
    UL_95CI,
    numerator,
    denominator,
    race_eth_name
  ) %>% # the file is parsed down to these columns only
  mutate(ct10 = as.character(geotypevalue),
         strata = strat,
         ind = indicator)


names(temp2) <- namesList

temp2 %>% write.csv(
  paste0(
    workingPath,
    "tables/tracts/CHVI_",
    indicator,
    "_",
    strat,
    "_tract.csv"
  ),
  row.names = F
) # it writes a file to your home directory with the name including the indicator and the geography

temp2 <- temp %>%
  filter(geotype == "CO",
         race_eth_name == "Total",
         strata_one_name == strat)  %>% # here's where we can filter the data further
  select(
    ind_definition,
    county_name,
    geotypevalue,
    geotype,
    estimate,
    region_name,
    LL_95CI,
    UL_95CI,
    numerator,
    denominator,
    race_eth_name
  ) %>% # the file is parsed down to these columns only
  mutate(COUNTYFI_1 = as.character(geotypevalue),
         strata = strat,
         ind = indicator)

names(temp2) <- namesList

temp2 %>% write.csv(
  paste0(
    workingPath,
    "tables/counties/CHVI_",
    indicator,
    "_",
    strat,
    "_county.csv"
  ),
  row.names = F
) # it writes a file to your home directory with the name including the indicator and the geography





strat <- 'total' # use metal, physical, total for disability

# we will need to change these settings for some indicators that have different use of the strata or report years (heat)
temp2 <- temp %>%
  filter(geotype == "CT",
         race_eth_name == "Total",
         strata_one_name == strat)  %>% # here's where we can filter the data further
  select(
    ind_definition,
    county_name,
    geotypevalue,
    geotype,
    estimate,
    region_name,
    LL_95CI,
    UL_95CI,
    numerator,
    denominator,
    race_eth_name
  ) %>% # the file is parsed down to these columns only
  mutate(ct10 = as.character(geotypevalue),
         strata = strat,
         ind = indicator)


names(temp2) <- namesList

temp2 %>% write.csv(
  paste0(
    workingPath,
    "tables/tracts/CHVI_",
    indicator,
    "_",
    strat,
    "_tract.csv"
  ),
  row.names = F
) # it writes a file to your home directory with the name including the indicator and the geography

temp2 <- temp %>%
  filter(geotype == "CO",
         race_eth_name == "Total",
         strata_one_name == strat)  %>% # here's where we can filter the data further
  select(
    ind_definition,
    county_name,
    geotypevalue,
    geotype,
    estimate,
    region_name,
    LL_95CI,
    UL_95CI,
    numerator,
    denominator,
    race_eth_name
  ) %>% # the file is parsed down to these columns only
  mutate(COUNTYFI_1 = as.character(geotypevalue),
         strata = strat,
         ind = indicator)

names(temp2) <- namesList

temp2 %>% write.csv(
  paste0(
    workingPath,
    "tables/counties/CHVI_",
    indicator,
    "_",
    strat,
    "_county.csv"
  ),
  row.names = F
) # it writes a file to your home directory with the name including the indicator and the geography

#### run for insurance ##############################

indicator <- "insurance"

fileName <- insurance
p1f <- tempfile()
download.file(fileName, p1f, mode = "wb")
temp <-
  readxl::read_xlsx(p1f, sheet = "Data") %>% select(1:26) %>% .[, order(names(.))]

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

#temp$reportyear <- as.character(as.numeric(temp$reportyear))

# run it for pop weigthed imperv
strat <- '2009-2013'

# parse and write pop-weighted TRACT data
temp2 <- temp %>%
  filter(geotype == "CT", reportyear == strat)  %>% # here's where we can filter the data further
  select(
    ind_definition,
    county_name,
    geotypevalue,
    geotype,
    estimate,
    region_name,
    LL_95CI,
    UL_95CI,
    numerator,
    denominator,
    race_eth_name
  ) %>% # the file is parsed down to these columns only
  mutate(ct10 = as.character(geotypevalue),
         strata = strat,
         ind = indicator)

names(temp2) <- namesList

temp2 %>% write.csv(paste0(workingPath, "tables/tracts/CHVI_", indicator, "_tract.csv"),
                    row.names = F) # it writes a file to your home directory with the name including the indicator and the geography

# parse and write pop-weighted COUNTY data
temp2 <- temp %>%
  filter(geotype == "CO", reportyear == strat)  %>% # here's where we can filter the data further
  select(
    ind_definition,
    county_name,
    geotypevalue,
    geotype,
    estimate,
    region_name,
    LL_95CI,
    UL_95CI,
    numerator,
    denominator,
    race_eth_name
  ) %>% # the file is parsed down to these columns only
  mutate(COUNTYFI_1 = as.character(geotypevalue),
         strata = strat,
         ind = indicator)

names(temp2) <- namesList

temp2 %>% write.csv(paste0(
  workingPath,
  "tables/counties/CHVI_",
  indicator,
  "_county.csv"
),
row.names = F) # it writes a file to your home directory with the name including the indicator and the geography


#### run for linguistic isolation (twice) ##############################

indicator <- "linguistic"

fileName <- linguistic
p1f <- tempfile()
download.file(fileName, p1f, mode = "wb")
temp <-
  readxl::read_xlsx(p1f, sheet = "Data") %>% select(1:26) %>% .[, order(names(.))]

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

# run it for pop weigthed imperv
strat <-
  'All Non-English' # use population-weighted or area-weighted for imperv

# parse and write pop-weighted TRACT data
temp2 <- temp %>%
  filter(geotype == "CT",
         race_eth_name == "Total",
         strata_one_name == strat)  %>% # here's where we can filter the data further
  select(
    ind_definition,
    county_name,
    geotypevalue,
    geotype,
    estimate,
    region_name,
    LL_95CI,
    UL_95CI,
    numerator,
    denominator,
    race_eth_name
  ) %>% # the file is parsed down to these columns only
  mutate(ct10 = as.character(geotypevalue),
         strata = strat,
         ind = indicator)

names(temp2) <- namesList

temp2 %>% write.csv(paste0(workingPath, "tables/tracts/CHVI_", indicator, "_tract.csv"),
                    row.names = F) # it writes a file to your home directory with the name including the indicator and the geography

# parse and write pop-weighted COUNTY data
temp2 <- temp %>%
  filter(geotype == "CO",
         race_eth_name == "Total",
         strata_one_name == strat)  %>% # here's where we can filter the data further
  select(
    ind_definition,
    county_name,
    geotypevalue,
    geotype,
    estimate,
    region_name,
    LL_95CI,
    UL_95CI,
    numerator,
    denominator,
    race_eth_name
  ) %>% # the file is parsed down to these columns only
  mutate(COUNTYFI_1 = as.character(geotypevalue),
         strata = strat,
         ind = indicator)

names(temp2) <- namesList

temp2 %>% write.csv(paste0(
  workingPath,
  "tables/counties/CHVI_",
  indicator,
  "_county.csv"
),
row.names = F) # it writes a file to your home directory with the name including the indicator and the geography

#### run for crime ##############################

indicator <- "crime"

fileName <- crime
p1f <- tempfile()
download.file(fileName, p1f, mode = "wb")
temp <-
  readxl::read_xlsx(p1f, sheet = "ViolentCrime") %>% select(1:26) %>% .[, order(names(.))]

temp$reportyear <- as.character(as.numeric(temp$reportyear))

temp2 <- temp %>%
  filter(
    geotype == "CO",
    race_eth_name == "Total",
    strata_level_name_code == 5,
    reportyear == 2013
  )  %>% # here's where we can filter the data further
  select(
    ind_definition,
    county_name,
    geotypevalue,
    geotype,
    rate,
    region_name,
    ll_95ci,
    ul_95ci,
    numerator,
    denominator,
    race_eth_name
  ) %>% # the file is parsed down to these columns only
  mutate(COUNTYFI_1 = as.character(geotypevalue),
         strata = "ViolentCrime",
         ind = indicator)

names(temp2) <- namesList

temp2 %>% write.csv(paste0(
  workingPath,
  "tables/counties/CHVI_",
  indicator,
  "_county.csv"
),
row.names = F) # it writes a file to your home directory with the name including the indicator and the geography

#### run for poverty (thrice) ##############################

indicator <- "poverty"

fileName <- poverty
temp <- readxl::read_xlsx(fileName, sheet = "poverty_rate")

#temp$reportyear <- as.character(temp$reportyear)


# run for Children
strat <- 'Child' # use Child, Concentrated, Overall for poverty

# parse and write TRACT data
temp2 <- temp %>%
  filter(geotype == "CT",
         race_eth_name == "Total",
         Poverty == strat,
         reportyear == "2006-2010")  %>% # here's where we can filter the data further
  select(
    ind_definition,
    county_name,
    geotypevalue,
    geotype,
    percent,
    region_name,
    LL_95CI_percent,
    UL_95CI_percent,
    NumPov,
    TotalPop,
    race_eth_name
  ) %>% # the file is parsed down to these columns only
  mutate(ct10 = as.character(geotypevalue),
         strata = strat,
         ind = indicator)


names(temp2) <- namesList

temp2 %>% write.csv(
  paste0(
    workingPath,
    "tables/tracts/CHVI_",
    indicator,
    "_",
    strat,
    "_tract.csv"
  ),
  row.names = F
) # it writes a file to your home directory with the name including the indicator and the geography

# parse and write COUNTY data
temp2 <- temp %>%
  filter(geotype == "CO",
         race_eth_name == "Total",
         Poverty == strat,
         reportyear == "2006-2010")  %>% # here's where we can filter the data further
  select(
    ind_definition,
    county_name,
    geotypevalue,
    geotype,
    percent,
    region_name,
    LL_95CI_percent,
    UL_95CI_percent,
    NumPov,
    TotalPop,
    race_eth_name
  ) %>% # the file is parsed down to these columns only
  mutate(COUNTYFI_1 = as.character(geotypevalue),
         strata = strat,
         ind = indicator)

names(temp2) <- namesList

temp2 %>% write.csv(
  paste0(
    workingPath,
    "tables/counties/CHVI_",
    indicator,
    "_",
    strat,
    "_county.csv"
  ),
  row.names = F
) # it writes a file to your home directory with the name including the indicator and the geography

#
# # run for Concentrated Poverty
strat <- 'Concentrated' # use Child, Concentrated, Overall for poverty

# parse and write COUNTY data
temp2 <- temp %>%
  filter(geotype == "CO",
         race_eth_name == "Total",
         Poverty == strat,
         reportyear == "2006-2010")  %>% # here's where we can filter the data further
  select(
    ind_definition,
    county_name,
    geotypevalue,
    geotype,
    percent,
    region_name,
    LL_95CI_percent,
    UL_95CI_percent,
    NumPov,
    TotalPop,
    race_eth_name
  ) %>% # the file is parsed down to these columns only
  mutate(COUNTYFI_1 = as.character(geotypevalue),
         strata = strat,
         ind = indicator)

names(temp2) <- namesList

temp2 %>% write.csv(paste0(workingPath,"tables/counties/CHVI_",indicator,"_",strat,"_county.csv"),row.names=F) # it writes a file to your home directory with the name including the indicator and the geography



# run for Overall Poverty
strat <- 'Overall' # use Child, Concentrated, Overall for poverty

# parse and write TRACT data
temp2 <- temp %>%
  filter(geotype == "CT",
         race_eth_name == "Total",
         Poverty == strat,
         reportyear == "2006-2010")  %>% # here's where we can filter the data further
  select(
    ind_definition,
    county_name,
    geotypevalue,
    geotype,
    percent,
    region_name,
    LL_95CI_percent,
    UL_95CI_percent,
    NumPov,
    TotalPop,
    race_eth_name
  ) %>% # the file is parsed down to these columns only
  mutate(ct10 = as.character(geotypevalue),
         strata = strat,
         ind = indicator)


names(temp2) <- namesList

temp2 %>% write.csv(
  paste0(
    workingPath,
    "tables/tracts/CHVI_",
    indicator,
    "_",
    strat,
    "_tract.csv"
  ),
  row.names = F
) # it writes a file to your home directory with the name including the indicator and the geography

# parse and write COUNTY data
temp2 <- temp %>%
  filter(geotype == "CO",
         race_eth_name == "Total",
         Poverty == strat,
         reportyear == "2006-2010")  %>% # here's where we can filter the data further
  select(
    ind_definition,
    county_name,
    geotypevalue,
    geotype,
    percent,
    region_name,
    LL_95CI_percent,
    UL_95CI_percent,
    NumPov,
    TotalPop,
    race_eth_name
  ) %>% # the file is parsed down to these columns only
  mutate(COUNTYFI_1 = as.character(geotypevalue),
         strata = strat,
         ind = indicator)

names(temp2) <- namesList

temp2 %>% write.csv(
  paste0(
    workingPath,
    "tables/counties/CHVI_",
    indicator,
    "_",
    strat,
    "_county.csv"
  ),
  row.names = F
) # it writes a file to your home directory with the name including the indicator and the geography

#### run for education ##############################

indicator <- "education"

fileName <- education
temp <- readxl::read_xlsx(fileName, sheet = "Educ_attain")

#temp$reportyear <- as.character(as.numeric(temp$reportyear))

# parse and write TRACT data
temp2 <- temp %>%
  filter(geotype == "CT",
         race_eth_name == "Total",
         reportyear == "2006-2010")  %>% # here's where we can filter the data further
  select(
    ind_definition,
    county_name,
    geotypevalue,
    geotype,
    p_hs_edatt,
    region_name,
    LL_95CI,
    UL_95CI,
    pop25pl_hs,
    pop25pl,
    race_eth_name
  ) %>% # the file is parsed down to these columns only
  mutate(ct10 = as.character(geotypevalue),
         strata = "2006-2010",
         ind = indicator)


names(temp2) <- namesList

temp2 %>% write.csv(paste0(workingPath, "tables/tracts/CHVI_", indicator, "_tract.csv"),
                    row.names = F) # it writes a file to your home directory with the name including the indicator and the geography

# parse and write COUNTY data
temp2 <- temp %>%
  filter(geotype == "CO",
         race_eth_name == "Total",
         reportyear == "2006-2010")  %>% # here's where we can filter the data further
  select(
    ind_definition,
    county_name,
    geotypevalue,
    geotype,
    p_hs_edatt,
    region_name,
    LL_95CI,
    UL_95CI,
    pop25pl_hs,
    pop25pl,
    race_eth_name
  ) %>% # the file is parsed down to these columns only
  mutate(COUNTYFI_1 = as.character(geotypevalue),
         strata = "2006-2010",
         ind = indicator)

names(temp2) <- namesList

temp2 %>% write.csv(paste0(
  workingPath,
  "tables/counties/CHVI_",
  indicator,
  "_county.csv"
),
row.names = F) # it writes a file to your home directory with the name including the indicator and the geography


#############################
### combine all the files for COUNTY level

rm(list = ls())

library(tidyverse)
library(sf)

workingPath <- "~/CHVI_copy/data/" # work local
# workingPath <-
#   "//phdeorlcsrvip01/Crossbranch/CDC_BRACE/Data/CHVI-CHPR Data/data/" # work on network
setwd(paste0(workingPath, "tables/counties/"))


files <- list.files(getwd())



for (file in files) {
  temp <- read.csv(file, header = T, stringsAsFactors = FALSE) %>% 
    transform(numratr = as.numeric(numratr),
              denmntr = as.numeric(denmntr)) %>%
    mutate(
      numratr = ifelse(is.na(numratr), as.numeric(as.character(numratr)), numratr),
      denmntr = ifelse(is.na(denmntr), as.numeric(as.character(denmntr)), denmntr),
      strata = ifelse(is.integer(strata), as.character(as.numeric(strata)), strata)
    )
  
  if (exists("countyCHVIs")) {
    countyCHVIs <- bind_rows(countyCHVIs, temp)
  } else{
    countyCHVIs <- temp
  }
}

countyTab <- countyCHVIs %>%
  mutate(ind_strt = paste0(ind, "_", strata),
         COUNTYFI_1 = as.character(paste0("0", COUNTYFI_1))) %>%
  select(-region) %>%
  left_join(read.csv(
    url(
      "https://raw.githubusercontent.com/vargovargo/CHVIr/master/county_region_lookup.csv"
    ),
    header = TRUE
  ))

countyTab %>% write.csv(paste0(workingPath, "tables/CHVIcountiesTIDY.csv"),
                        row.names = FALSE)

countyShpTab <- countyTab %>%
  filter(metric == "est") %>%
  select(county, COUNTYFI_1, value, ind_strt) %>%
  spread(key = ind_strt, value = value) %>%
  left_join(read.csv(
    url(
      "https://raw.githubusercontent.com/vargovargo/CHVIr/master/county_region_lookup.csv"
    ),
    header = TRUE
  ))

names(countyShpTab) <-
  c(
    "county",
    "COUNTYFI_1",
    "AC",
    "arTREE",
    "popTREE",
    "CHILD",
    "CRIME",
    "mntlDSBL",
    "physDSBL",
    "totDSBL",
    "EDU",
    "ELDER",
    "HEAT50",
    "HEAT85",
    "arIMPRV",
    "popIMPRV",
    "INSUR",
    "LING",
    "OUTDR",
    "OZN",
    "PM",
    "chldPOV",
    "concPOV",
    "allPOV",
    "SLR",
    "VEH",
    "WF",
    "ClimReg"
  )

counties <-
  st_read(paste0(workingPath, "shapefiles/Counties2010.shp"))

CHVIcounties <-
  counties %>%  left_join(countyShpTab) %>% st_transform(crs = 4326)

plot(CHVIcounties)

CHVIcounties %>%
  st_write(dsn = normalizePath(
    paste0(workingPath, "shapefiles/CHVI_indicators_county.shp")
  ), delete_layer = TRUE)

### combine all the files for TRACT level

rm(list = ls())

library(tidyverse)
library(sf)

#workingPath <- "~/CHVI_copy/data/" # work local
workingPath <-"//phdeorlcsrvip01/Crossbranch/CDC_BRACE/Data/CHVI-CHPR Data/data/" # work on network

setwd(paste0(workingPath, "tables/tracts/"))

files <- list.files(getwd())

for (file in files) {
  temp <- read.csv(file, header = T, stringsAsFactors = FALSE) %>%
    transform(numratr = as.numeric(numratr),
              denmntr = as.numeric(denmntr)) %>%
    mutate(
      numratr = ifelse(is.na(numratr), as.numeric(as.character(numratr)), numratr),
      denmntr = ifelse(is.na(denmntr), as.numeric(as.character(denmntr)), denmntr),
      strata = ifelse(is.integer(strata), as.character(as.numeric(strata)), strata)
    ) %>%
    rename(county = cnty)
  
  if (exists("tractCHVIs")) {
    tractCHVIs <- bind_rows(tractCHVIs, temp)
  } else{
    tractCHVIs <- temp
  }
}

tractTab <- tractCHVIs %>%
  mutate(ind_strt = paste0(ind, "_", strata),
         ct10 = as.character(paste0("0", ct10))) %>%
  select(-region) %>%
  left_join(read.csv(
    url(
      "https://raw.githubusercontent.com/vargovargo/CHVIr/master/county_region_lookup.csv"
    ),
    header = TRUE
  ))

tractTab %>% write.csv(paste0(workingPath, "tables/CHVItractsTIDY.csv"), row.names = FALSE)

tractShpTab <- tractTab %>%
  filter(metric == "est") %>%
  select(county, ct10, value, ind_strt) %>%
  spread(key = ind_strt, value = value) %>%
  left_join(read.csv(
    url(
      "https://raw.githubusercontent.com/vargovargo/CHVIr/master/county_region_lookup.csv"
    ),
    header = TRUE
  ))

names(tractShpTab) <-
  c(
    "county",
    "ct10",
    "arTREE",
    "popTREE",
    "CHILD",
    "mntlDSBL",
    "physDSBL",
    "totDSBL",
    "EDU",
    "ELDER",
    "arIMPRV",
    "popIMPRV",
    "INSUR",
    "LING",
    "OUTDR",
    "OZN",
    "PM",
    "chldPOV",
    "allPOV",
    "SLR",
    "VEH",
    "WF",
    "ClimReg"
  )

tracts <-
  st_read(paste0(workingPath, "shapefiles/census_tracts2010.shp"))

CHVItracts <-
  tracts %>%  left_join(tractShpTab) %>% st_transform(crs = 4326)

plot(CHVItracts)

CHVItracts %>%
  st_write(dsn = normalizePath(paste0(
    workingPath, "shapefiles/CHVI_indicators_tract.shp"
  )), delete_layer = TRUE)
