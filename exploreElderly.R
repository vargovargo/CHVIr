rm(list=ls())

library(tidyverse)

# work Local
setwd("~/CHVI_copy/CHVIcsvs/")

# work from the network
# setwd("//phitprlcsrvip04/OHEGroup/HCI/Data/CCHVI in one folder for web/CHVIcsvs/")


allCHVI <- read.csv("../allCHVI.csv", header=T)

unique(allCHVI$ind_definition)

elderly <- allCHVI %>% filter(ind_definition == "Percent of population aged 65 years or older" & geotype %in% c("CO")) %>%
  mutate(state_name = "California") %>%
  write.csv("./TableauCHVI/elderly.csv", row.names=FALSE)
