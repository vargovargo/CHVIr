rm(list=ls())

library(tidyverse)

# work Local
setwd("~/CHVI_copy/CHVIcsvs/")

# work from the network
# setwd("//phitprlcsrvip04/OHEGroup/HCI/Data/CCHVI in one folder for web/CHVIcsvs/")


allCHVI <- read.csv("../allCHVI.csv", header=T)

unique(allCHVI$ind_definition)

geog = c("CO")

elderly <- allCHVI %>% filter(ind_definition == "Percent of population aged 65 years or older" & geotype %in% geog) %>%
  mutate(state = "California") %>%
  write.csv("./TableauCHVI/elderlyTAB.csv", row.names=FALSE)

carOwnership <- allCHVI %>% filter(ind_definition == "Percent of households with no vehicle ownership" & geotype %in% geog) %>%
  mutate(state = "California") %>%
  write.csv("./TableauCHVI/carOwnTAB.csv", row.names=FALSE)

SeaLevel <- allCHVI %>% filter(ind_definition == "Population living in sea level rise inundation areas" & geotype %in% geog) %>%
  mutate(state = "California") %>%
  write.csv("./TableauCHVI/carOwnTAB.csv", row.names=FALSE)
