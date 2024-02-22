####load package####
library(gesttools)
library(did)
library(haven)
library(CBPS)
library(rddtools)
library(rdrobust)
library(gtsummary)
library(mediation)
library(rddensity)
library(stringr.tools)
library(dplyr)
library(stringr)
library(readODS)
library(panelr)
library(DiagrammeR)
library(pastecs)
library(RColorBrewer)
library(curl)
library(lavaan)
library(panelr)
library(viridis)
library(lavaanPlot)
library(readxl)
library(gt)
library(modelsummary)
library(panelr)
library(pdftools)
library(rgeos)
library(sp)
library(rgdal)
library(ggmap)
library(readstata13)
library(maptools)
library(sf)
library(fuzzyjoin)
library(raster)
library(tidyverse)
library(stringr)
library(rebus)
library(plm)
library(texreg)
library(regclass)
library(stargazer)
library(sjPlot)
library(lme4)
library(reshape2)
library(clubSandwich)
library(lmerTest)
library(sf)
library(sp)
library(spdep)
library(rgdal)
library(rgeos)
library(tmap)
library(tmaptools)
library(spgwr)
library(grid)
library(gridExtra)
library(zoo)
library(lubridate)
library(jtools)
library(kableExtra)


rm(list=setdiff(ls(), c("")))


options(scipen=999)



setwd("C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/PhDing2020")


####NHS####


allocation13_14 <- read.csv("Data/allocation_13_14.csv")
allocation14_15 <- read.csv("Data/allocation_14_15.csv")
allocation15_16 <- read.csv("Data/allocation_15_16.csv")
allocation16_17 <- read.csv("Data/allocation_16_17.csv")
allocation17_18 <- read.csv("Data/allocation_17_18.csv")
allocation18_19 <- read.csv("Data/allocation_18_19.csv")
allocation19_20 <- read.csv("Data/allocation_19_20.csv")




names(allocation13_14)[names(allocation13_14)=="CCG.name"] <- "CCG_Name"
names(allocation14_15)[names(allocation14_15)=="CCG.code"] <- "CCG_Name"
names(allocation15_16)[names(allocation15_16)=="CCG.code"] <- "CCG_Name"
names(allocation16_17)[names(allocation16_17)=="ccg_name"] <- "CCG_Name"
names(allocation17_18)[names(allocation17_18)=="CCG"] <- "CCG_Name"
names(allocation17_18)[names(allocation17_18)=="CCG.Code"] <- "CCG_Code"
names(allocation18_19)[names(allocation18_19)=="ccg_name"] <- "CCG_Name"
names(allocation18_19)[names(allocation18_19)=="ccg_code"] <- "CCG_Code"
names(allocation19_20)[names(allocation19_20)=="Name"] <- "CCG_Name"
names(allocation19_20)[names(allocation19_20)=="Code"] <- "CCG_Code"



allocation13_14 <- dplyr::filter(allocation13_14, grepl("NHS",CCG_Name))
allocation14_15 <- dplyr::filter(allocation14_15, grepl("NHS",CCG_Name))
allocation15_16 <- dplyr::filter(allocation15_16, grepl("NHS",CCG_Name))
allocation16_17 <- dplyr::filter(allocation16_17, grepl("NHS",CCG_Name))
allocation17_18 <- dplyr::filter(allocation17_18, grepl("NHS",CCG_Name))
allocation18_19 <- dplyr::filter(allocation18_19, grepl("NHS",CCG_Name))
allocation19_20 <- dplyr::filter(allocation19_20, grepl("NHS",CCG_Name))




allocation13_14<- separate(allocation13_14, col = "CCG_Name" , into = c("CCG_Code", "CCG_Name"), sep = " NHS ")
allocation14_15<- separate(allocation14_15, col = "CCG_Name" , into = c("CCG_Code", "CCG_Name"), sep = " NHS ")
allocation15_16<- separate(allocation15_16, col = "CCG_Name" , into = c("CCG_Code", "CCG_Name"), sep = " NHS ")


allocation13_14$CCG_Name <-  gsub('&','and',allocation13_14$CCG_Name)
allocation14_15$CCG_Name <-  gsub('&','and',allocation14_15$CCG_Name)
allocation15_16$CCG_Name <-  gsub('&','and',allocation15_16$CCG_Name)
allocation16_17$CCG_Name <-  gsub('&','and',allocation16_17$CCG_Name)
allocation17_18$CCG_Name <-  gsub('&','and',allocation17_18$CCG_Name)
allocation18_19$CCG_Name <-  gsub('&','and',allocation18_19$CCG_Name)
allocation19_20$CCG_Name <-  gsub('&','and',allocation19_20$CCG_Name)


allocation13_14$CCG_Name <-  gsub('[[:punct:] ]+',' ',allocation13_14$CCG_Name)
allocation14_15$CCG_Name <-  gsub('[[:punct:] ]+',' ',allocation14_15$CCG_Name)
allocation15_16$CCG_Name <-  gsub('[[:punct:] ]+',' ',allocation15_16$CCG_Name)
allocation16_17$CCG_Name <-  gsub('[[:punct:] ]+',' ',allocation16_17$CCG_Name)
allocation17_18$CCG_Name <-  gsub('[[:punct:] ]+',' ',allocation17_18$CCG_Name)
allocation18_19$CCG_Name <-  gsub('[[:punct:] ]+',' ',allocation18_19$CCG_Name)
allocation19_20$CCG_Name <-  gsub('[[:punct:] ]+',' ',allocation19_20$CCG_Name)

allocation16_17$CCG_Name <-  gsub('NHS ','',allocation16_17$CCG_Name)
allocation17_18$CCG_Name <-  gsub('NHS ','',allocation17_18$CCG_Name)
allocation18_19$CCG_Name <-  gsub('NHS ','',allocation18_19$CCG_Name)
allocation19_20$CCG_Name <-  gsub('NHS ','',allocation19_20$CCG_Name)

allocation13_14$CCG_Name <-  toupper(allocation13_14$CCG_Name)
allocation14_15$CCG_Name <-  toupper(allocation14_15$CCG_Name)
allocation15_16$CCG_Name <-  toupper(allocation15_16$CCG_Name)
allocation16_17$CCG_Name <-  toupper(allocation16_17$CCG_Name)
allocation17_18$CCG_Name <-  toupper(allocation17_18$CCG_Name)
allocation18_19$CCG_Name <-  toupper(allocation18_19$CCG_Name)
allocation19_20$CCG_Name <-  toupper(allocation19_20$CCG_Name)

allocation13_14$CCG_Name <-  str_trim(allocation13_14$CCG_Name)
allocation14_15$CCG_Name <-  str_trim(allocation14_15$CCG_Name)
allocation15_16$CCG_Name <-  str_trim(allocation15_16$CCG_Name)
allocation16_17$CCG_Name <-  str_trim(allocation16_17$CCG_Name)
allocation17_18$CCG_Name <-  str_trim(allocation17_18$CCG_Name)
allocation18_19$CCG_Name <-  str_trim(allocation18_19$CCG_Name)
allocation19_20$CCG_Name <-  str_trim(allocation19_20$CCG_Name)


codes <- unique(rbind(allocation17_18[c("CCG_Name", "CCG_Code")],allocation15_16[c("CCG_Name", "CCG_Code")]))
codes_ccg <- codes

allocation16_17 <- merge(allocation16_17, codes, by="CCG_Name", all.x=T)
allocation16_17$CCG_Code[allocation16_17$CCG_Name=="SOUTH EAST STAFFS AND SEISDON PENINSULAR CCG"] <- "05Q"

allocation13_14$year <- 2014
allocation14_15$year <- 2015
allocation15_16$year <- 2016
allocation16_17$year <- 2017
allocation17_18$year <- 2018
allocation18_19$year <- 2019
allocation19_20$year <- 2020


names(allocation13_14)[names(allocation13_14)=="Allocation"] <- "Allocation_000s"
names(allocation14_15)[names(allocation14_15)=="ccg.programme.budget.allocation"] <- "Allocation_000s"
names(allocation15_16)[names(allocation15_16)=="Programme.allocation"] <- "Allocation_000s"
names(allocation16_17)[names(allocation16_17)=="Final.allocation.after.place.based.pace..of.change"] <- "Allocation_000s"
names(allocation17_18)[names(allocation17_18)=="Final.allocation.after.place.based.pace..of.change"] <- "Allocation_000s"
names(allocation18_19)[names(allocation18_19)=="Final.allocation"] <- "Allocation_000s"
names(allocation19_20)[names(allocation19_20)=="Final.allocation.after.place.based.pace.of.change"] <- "Allocation_000s"

names(allocation13_14)[names(allocation13_14)=="registered_pop"] <- "Est_registered_pop"
names(allocation14_15)[names(allocation14_15)=="Estimated.registered.population"] <- "Est_registered_pop"
#names(allocation15_16)[names(allocation15_16)=="Programme.allocation"] <- "Est_registered_pop"
names(allocation16_17)[names(allocation16_17)=="Estimated.registered.population"] <- "Est_registered_pop"
names(allocation17_18)[names(allocation17_18)=="Estimated.registered.population"] <- "Est_registered_pop"
names(allocation18_19)[names(allocation18_19)=="Estimated.registered.population"] <- "Est_registered_pop"
names(allocation19_20)[names(allocation19_20)=="Estimated.12.month.average.registered.population.1"] <- "Est_registered_pop"


names(allocation13_14)[names(allocation13_14)=="allocation_per_head"] <- "Allocation_per_head"
names(allocation14_15)[names(allocation14_15)=="Allocation.per.head"] <- "Allocation_per_head"
names(allocation15_16)[names(allocation15_16)=="per.capita.allocation"] <- "Allocation_per_head"
names(allocation16_17)[names(allocation16_17)=="Final.per.capita.allocation"] <- "Allocation_per_head"
names(allocation17_18)[names(allocation17_18)=="Final.per.capita.allocation"] <- "Allocation_per_head"
names(allocation18_19)[names(allocation18_19)=="Final.per.capita.allocation"] <- "Allocation_per_head"
#names(allocation19_20)[names(allocation19_20)=="Final.allocation.after.place.based.pace.of.change"] <- "Allocation_per_head"


#names(allocation13_14)[names(allocation13_14)=="Allocation"] <- "Distance_from_target"
names(allocation14_15)[names(allocation14_15)=="Distance.from.target"] <- "Distance_from_target"
names(allocation15_16)[names(allocation15_16)=="closing.distance.from.target"] <- "Distance_from_target"
names(allocation16_17)[names(allocation16_17)=="Final.closing.DfT"] <- "Distance_from_target"
names(allocation17_18)[names(allocation17_18)=="Final.closing.DfT"] <- "Distance_from_target"
names(allocation18_19)[names(allocation18_19)=="Final.closing.DfT"] <- "Distance_from_target"
names(allocation19_20)[names(allocation19_20)=="Final.closing.DfT"] <- "Distance_from_target"


allocation13_14$Allocation_000s <-  gsub(',','',allocation13_14$Allocation_000s)
allocation14_15$Allocation_000s <-  gsub(',','',allocation14_15$Allocation_000s)
allocation15_16$Allocation_000s <-  gsub(',','',allocation15_16$Allocation_000s)
allocation16_17$Allocation_000s <-  gsub(',','',allocation16_17$Allocation_000s)
allocation17_18$Allocation_000s <-  gsub(',','',allocation17_18$Allocation_000s)
allocation18_19$Allocation_000s <-  gsub(',','',allocation18_19$Allocation_000s)
allocation19_20$Allocation_000s <-  gsub(',','',allocation19_20$Allocation_000s)

allocation13_14$Est_registered_pop <-  gsub(',','',allocation13_14$Est_registered_pop)
allocation14_15$Est_registered_pop <-  gsub(',','',allocation14_15$Est_registered_pop)
#allocation15_16$Est_registered_pop <-  gsub(',','',allocation15_16$Est_registered_pop)
allocation16_17$Est_registered_pop <-  gsub(',','',allocation16_17$Est_registered_pop)
allocation17_18$Est_registered_pop <-  gsub(',','',allocation17_18$Est_registered_pop)
allocation18_19$Est_registered_pop <-  gsub(',','',allocation18_19$Est_registered_pop)
allocation19_20$Est_registered_pop <-  gsub(',','',allocation19_20$Est_registered_pop)

allocation13_14$Allocation_per_head <-  gsub(',','',allocation13_14$Allocation_per_head)
allocation14_15$Allocation_per_head <-  gsub(',','',allocation14_15$Allocation_per_head)
allocation15_16$Allocation_per_head <-  gsub(',','',allocation15_16$Allocation_per_head)
allocation16_17$Allocation_per_head <-  gsub(',','',allocation16_17$Allocation_per_head)
allocation17_18$Allocation_per_head <-  gsub(',','',allocation17_18$Allocation_per_head)
allocation18_19$Allocation_per_head <-  gsub(',','',allocation18_19$Allocation_per_head)
#allocation19_20$Allocation_per_head <-  gsub(',','',allocation19_20$Allocation_per_head)

#allocation13_14$Distance_from_target <-  gsub(',','',allocation13_14$Distance_from_target)
allocation14_15$Distance_from_target <-  gsub(',','',allocation14_15$Distance_from_target)
allocation15_16$Distance_from_target <-  gsub(',','',allocation15_16$Distance_from_target)
allocation16_17$Distance_from_target <-  gsub(',','',allocation16_17$Distance_from_target)
allocation17_18$Distance_from_target <-  gsub(',','',allocation17_18$Distance_from_target)
allocation18_19$Distance_from_target <-  gsub(',','',allocation18_19$Distance_from_target)
allocation19_20$Distance_from_target <-  gsub(',','',allocation19_20$Distance_from_target)

allocation14_15$Distance_from_target <-  gsub('%','',allocation14_15$Distance_from_target)
allocation15_16$Distance_from_target <-  gsub('%','',allocation15_16$Distance_from_target)
allocation16_17$Distance_from_target <-  gsub('%','',allocation16_17$Distance_from_target)
allocation17_18$Distance_from_target <-  gsub('%','',allocation17_18$Distance_from_target)
allocation18_19$Distance_from_target <-  gsub('%','',allocation18_19$Distance_from_target)
allocation19_20$Distance_from_target <-  gsub('%','',allocation19_20$Distance_from_target)


allocation13_14$Allocation_000s <-  as.numeric(allocation13_14$Allocation_000s)
allocation14_15$Allocation_000s <-  as.numeric(allocation14_15$Allocation_000s)
allocation15_16$Allocation_000s <-  as.numeric(allocation15_16$Allocation_000s)
allocation16_17$Allocation_000s <-  as.numeric(allocation16_17$Allocation_000s)
allocation17_18$Allocation_000s <-  as.numeric(allocation17_18$Allocation_000s)
allocation18_19$Allocation_000s <-  as.numeric(allocation18_19$Allocation_000s)
allocation19_20$Allocation_000s <-  as.numeric(allocation19_20$Allocation_000s)

allocation13_14$Est_registered_pop <-  as.numeric(allocation13_14$Est_registered_pop)
allocation14_15$Est_registered_pop <-  as.numeric(allocation14_15$Est_registered_pop)
#allocation15_16$Est_registered_pop <-  as.numeric(allocation15_16$Est_registered_pop)
allocation16_17$Est_registered_pop <-  as.numeric(allocation16_17$Est_registered_pop)
allocation17_18$Est_registered_pop <-  as.numeric(allocation17_18$Est_registered_pop)
allocation18_19$Est_registered_pop <-  as.numeric(allocation18_19$Est_registered_pop)
allocation19_20$Est_registered_pop <-  as.numeric(allocation19_20$Est_registered_pop)

allocation13_14$Allocation_per_head <-  as.numeric(allocation13_14$Allocation_per_head)
allocation14_15$Allocation_per_head <-  as.numeric(allocation14_15$Allocation_per_head)
allocation15_16$Allocation_per_head <-  as.numeric(allocation15_16$Allocation_per_head)
allocation16_17$Allocation_per_head <-  as.numeric(allocation16_17$Allocation_per_head)
allocation17_18$Allocation_per_head <-  as.numeric(allocation17_18$Allocation_per_head)
allocation18_19$Allocation_per_head <-  as.numeric(allocation18_19$Allocation_per_head)
#allocation19_20$Allocation_per_head <-  as.numeric(allocation19_20$Allocation_per_head)

#allocation13_14$Distance_from_target <-  as.numeric(allocation13_14$Distance_from_target)
allocation14_15$Distance_from_target <-  as.numeric(allocation14_15$Distance_from_target)
allocation15_16$Distance_from_target <-  as.numeric(allocation15_16$Distance_from_target)
allocation16_17$Distance_from_target <-  as.numeric(allocation16_17$Distance_from_target)
allocation17_18$Distance_from_target <-  as.numeric(allocation17_18$Distance_from_target)
allocation18_19$Distance_from_target <-  as.numeric(allocation18_19$Distance_from_target)
allocation19_20$Distance_from_target <-  as.numeric(allocation19_20$Distance_from_target)


allocation13_14$Distance_from_target <- NA
allocation19_20$Allocation_per_head <- allocation19_20$Allocation_000s/allocation19_20$Est_registered_pop
allocation15_16$Est_registered_pop <- NA

allocation_data_CCG <- rbind(allocation13_14[c("CCG_Code", "CCG_Name", "year", "Allocation_000s", "Est_registered_pop", "Allocation_per_head", "Distance_from_target")],
                             allocation14_15[c("CCG_Code", "CCG_Name", "year","Allocation_000s", "Est_registered_pop", "Allocation_per_head", "Distance_from_target")],
                             allocation15_16[c("CCG_Code", "CCG_Name", "year","Allocation_000s", "Est_registered_pop", "Allocation_per_head", "Distance_from_target")],
                             allocation16_17[c("CCG_Code", "CCG_Name", "year","Allocation_000s", "Est_registered_pop", "Allocation_per_head", "Distance_from_target")],
                             allocation17_18[c("CCG_Code", "CCG_Name", "year","Allocation_000s", "Est_registered_pop", "Allocation_per_head", "Distance_from_target")],
                             allocation18_19[c("CCG_Code", "CCG_Name", "year","Allocation_000s", "Est_registered_pop", "Allocation_per_head", "Distance_from_target")],
                             allocation19_20[c("CCG_Code", "CCG_Name", "year","Allocation_000s", "Est_registered_pop", "Allocation_per_head", "Distance_from_target")])



allocation12_13 <- read.csv("Data/allocation_12_13.csv")
allocation11_12 <- read.csv("Data/allocation_11_12.csv")
allocation09_11 <- read.csv("Data/allocation_09_11.csv")
allocation06_08 <- read.csv("Data/allocation_06_08.csv")
allocation03_06 <- read.csv("Data/allocation_03_06.csv")
allocation02_03 <- read.csv("Data/allocation_02_03.csv")
allocation01_02 <- read.csv("Data/allocation_01_02.csv")
allocation00_01 <- read.csv("Data/allocation_00_01.csv")


names(allocation12_13)[names(allocation12_13)=="PCT"] <- "PCT_Name"
names(allocation11_12)[names(allocation11_12)=="PCT"] <- "PCT_Name"
names(allocation09_11)[names(allocation09_11)=="PCT"] <- "PCT_Name"
names(allocation06_08)[names(allocation06_08)=="PCT"] <- "PCT_Name"
names(allocation03_06)[names(allocation03_06)=="PCT"] <- "PCT_Name"
names(allocation02_03)[names(allocation02_03)=="Health.Authority"] <- "PCT_Name"
names(allocation01_02)[names(allocation01_02)=="Health.authority"] <- "PCT_Name"
names(allocation00_01)[names(allocation00_01)=="Health.Authority"] <- "PCT_Name"

names(allocation12_13)[names(allocation12_13)=="Code"] <- "PCT_Code"
names(allocation11_12)[names(allocation11_12)=="Code"] <- "PCT_Code"
names(allocation09_11)[names(allocation09_11)=="Code"] <- "PCT_Code"
names(allocation06_08)[names(allocation06_08)=="Code"] <- "PCT_Code"
names(allocation03_06)[names(allocation03_06)=="Code"] <- "PCT_Code"
#names(allocation02_03)[names(allocation02_03)=="Health.Authority"] <- "PCT_Name"
#names(allocation01_02)[names(allocation01_02)=="Health.authority"] <- "PCT_Name"
#names(allocation00_01)[names(allocation00_01)=="Health.Authority"] <- "PCT_Name"

names(allocation12_13)[names(allocation12_13)=="Total.revenue.allocations"] <- "Allocation_000s"
names(allocation11_12)[names(allocation11_12)=="Total.revenue.allocations"] <- "Allocation_000s"
allocation10_11 <- allocation09_11[c("PCT_Name", "PCT_Code", "X2010.11.allocation")]
allocation09_10 <- allocation09_11[c("PCT_Name", "PCT_Code", "X2009.10.allocation")]
allocation08_09 <- allocation09_11[c("PCT_Name", "PCT_Code", "X2009.10.opening.baseline")]
names(allocation10_11)[names(allocation10_11)=="X2010.11.allocation"] <- "Allocation_000s"
names(allocation09_10)[names(allocation09_10)=="X2009.10.allocation"] <- "Allocation_000s"
names(allocation08_09)[names(allocation08_09)=="X2009.10.opening.baseline"] <- "Allocation_000s"
allocation07_08 <- allocation06_08[c("PCT_Name", "PCT_Code", "X2007.08.recurrent.allocation")]
allocation06_07 <- allocation06_08[c("PCT_Name", "PCT_Code", "X2006.07.recurrent.allocation")]

names(allocation07_08)[names(allocation07_08)=="X2007.08.recurrent.allocation"] <- "Allocation_000s"
names(allocation06_07)[names(allocation06_07)=="X2006.07.recurrent.allocation"] <- "Allocation_000s"

allocation05_06 <- allocation03_06[c("PCT_Name", "PCT_Code", "X2005.06.allocation")]
allocation04_05 <- allocation03_06[c("PCT_Name", "PCT_Code", "X2004.05.allocation")]
allocation03_04 <- allocation03_06[c("PCT_Name", "PCT_Code", "X2003.04.allocation")]


names(allocation05_06)[names(allocation05_06)=="X2005.06.allocation"] <- "Allocation_000s"
names(allocation04_05)[names(allocation04_05)=="X2004.05.allocation"] <- "Allocation_000s"
names(allocation03_04)[names(allocation03_04)=="X2003.04.allocation"] <- "Allocation_000s"

names(allocation02_03)[names(allocation02_03)=="Total.allocation"] <- "Allocation_000s"
names(allocation01_02)[names(allocation01_02)=="Total.allocation"] <- "Allocation_000s"
names(allocation00_01)[names(allocation00_01)=="Total.allocation"] <- "Allocation_000s"

allocation00_01$year <- 2001
allocation01_02$year <- 2002
allocation02_03$year <- 2003
allocation03_04$year <- 2004
allocation04_05$year <- 2005
allocation05_06$year <- 2006
allocation06_07$year <- 2007
allocation07_08$year <- 2008
allocation08_09$year <- 2009
allocation09_10$year <- 2010
allocation10_11$year <- 2011
allocation11_12$year <- 2012
allocation12_13$year <- 2013

allocation03_13 <- rbind(allocation12_13[c("PCT_Name", "PCT_Code", "Allocation_000s", "year")],
                         allocation11_12[c("PCT_Name", "PCT_Code", "Allocation_000s", "year")],
                         allocation10_11[c("PCT_Name", "PCT_Code", "Allocation_000s", "year")],
                         allocation09_10[c("PCT_Name", "PCT_Code", "Allocation_000s", "year")],
                         allocation08_09[c("PCT_Name", "PCT_Code", "Allocation_000s", "year")],
                         allocation07_08[c("PCT_Name", "PCT_Code", "Allocation_000s", "year")],
                         allocation06_07[c("PCT_Name", "PCT_Code", "Allocation_000s", "year")],
                         allocation05_06[c("PCT_Name", "PCT_Code", "Allocation_000s", "year")],
                         allocation04_05[c("PCT_Name", "PCT_Code", "Allocation_000s", "year")],
                         allocation03_04[c("PCT_Name", "PCT_Code", "Allocation_000s", "year")])

allocation00_03 <- rbind(allocation00_01[c("PCT_Name", "Allocation_000s", "year")],
                         allocation01_02[c("PCT_Name", "Allocation_000s", "year")],
                         allocation02_03[c("PCT_Name", "Allocation_000s", "year")])



pctlookup <- read.csv("Data/pct_lookup_oa.csv")
pctlookupcodes <- read.csv("Data/pct_lookup_oa_codes.csv")

names(pctlookup)[names(pctlookup)=="PCO11NM"] <- "PCT_Name"
names(pctlookupcodes)[names(pctlookupcodes)=="PCO11NM"] <- "PCT_Name"
names(pctlookupcodes)[names(pctlookupcodes)=="PCO11CDO"] <- "PCT_Code"



pctlookupcodes$PCT_Name <-  gsub('&','and',pctlookupcodes$PCT_Name)
pctlookup$PCT_Name <-  gsub('&','and',pctlookup$PCT_Name)
allocation03_13$PCT_Name <-  gsub('&','and',allocation03_13$PCT_Name)
allocation00_03$PCT_Name <-  gsub('&','and',allocation00_03$PCT_Name)

pctlookupcodes$PCT_Name <-  gsub('[[:punct:] ]+',' ',pctlookupcodes$PCT_Name)
pctlookup$PCT_Name <-  gsub('[[:punct:] ]+',' ',pctlookup$PCT_Name)
allocation03_13$PCT_Name <-  gsub('[[:punct:] ]+',' ',allocation03_13$PCT_Name)
allocation00_03$PCT_Name <-  gsub('[[:punct:] ]+',' ',allocation00_03$PCT_Name)

pctlookupcodes$PCT_Name <-  gsub('NHS ','',pctlookupcodes$PCT_Name)
pctlookup$PCT_Name <-  gsub('NHS ','',pctlookup$PCT_Name)
allocation00_03$PCT_Name <-  gsub('NHS ','',allocation00_03$PCT_Name)
allocation03_13$PCT_Name <-  gsub('NHS ','',allocation03_13$PCT_Name)

pctlookupcodes$PCT_Name <-  gsub('PCT','',pctlookupcodes$PCT_Name)
pctlookup$PCT_Name <-  gsub('PCT','',pctlookup$PCT_Name)
allocation00_03$PCT_Name <-  gsub('PCT','',allocation00_03$PCT_Name)
allocation03_13$PCT_Name <-  gsub('PCT','',allocation03_13$PCT_Name)

pctlookupcodes$PCT_Name <-  toupper(pctlookupcodes$PCT_Name)
pctlookup$PCT_Name <-  toupper(pctlookup$PCT_Name)
allocation00_03$PCT_Name <-  toupper(allocation00_03$PCT_Name)
allocation03_13$PCT_Name <-  toupper(allocation03_13$PCT_Name)

pctlookupcodes$PCT_Name <-  str_trim(pctlookupcodes$PCT_Name)
pctlookup$PCT_Name <-  str_trim(pctlookup$PCT_Name)
allocation00_03$PCT_Name <-  str_trim(allocation00_03$PCT_Name)
allocation03_13$PCT_Name <-  str_trim(allocation03_13$PCT_Name)

pctlookupcodes <- pctlookupcodes[which(pctlookupcodes$PCT_Name!=""),]
pctlookup <- pctlookup[which(pctlookup$PCT_Name!=""),]
allocation00_03 <- allocation00_03[which(allocation00_03$PCT_Name!=""),]
allocation03_13 <- allocation03_13[which(allocation03_13$PCT_Name!=""),]


codes <- allocation03_13[c("PCT_Name", "PCT_Code")]

allocation00_03 <- merge(allocation00_03, codes, by="PCT_Name", all.x=T)


#allocation_data_PCT <- rbind(allocation00_03, allocation03_13)

allocation_data_PCT <- allocation03_13

allocation_data_PCT <- allocation_data_PCT[!grepl(" SHA", allocation_data_PCT$PCT_Name),]
allocation_data_PCT <- allocation_data_PCT[which(allocation_data_PCT$PCT_Name!="ENGLAND"&
                                                   allocation_data_PCT$PCT_Name!="TOTAL ENGLAND"&
                                                   allocation_data_PCT$PCT_Name!="ENGLAND TOTAL"),]

allocation_data_PCT$Allocation_000s <- gsub('[[:punct:] ]+','',allocation_data_PCT$Allocation_000s)
allocation_data_PCT$Allocation_000s  <- as.numeric(allocation_data_PCT$Allocation_000s )

allocation_data_PCT <- allocation_data_PCT%>%
  dplyr::group_by(PCT_Code)%>%
  mutate(nobs = n())%>%
  dplyr::filter(nobs>9)%>%
  dplyr::ungroup()
#pctlookup <- merge(pctlookup[c("PCT_Name", "OA11CD")], pctlookupcodes[c("PCT_Name", "PCT_Code")], by="PCT_Name", all=T)

#pctlookup <- pctlookup[!grepl("W", pctlookup$OA11CD),]

# 
# allocation_data_PCT <- merge(allocation_data_PCT, pctlookup, by=c("PCT_Name"), all=T)
# 
# allocation_data_PCT <- allocation_data_PCT[!is.na(allocation_data_PCT$PCT_Name),]
# allocation_data_PCT <- allocation_data_PCT[!is.na(allocation_data_PCT$Allocation_000s),]
# 
matrix_lookup <- read.csv("Data/PCT_CCG_matrix.csv")

matrix_lookup[matrix_lookup=='0%'] <- NA

matrix_lookup <-matrix_lookup %>% 
  pivot_longer(
    cols = !PCT_Code, 
    names_to = "CCG_Code", 
    values_to = "lookup"
  )

matrix_lookup <- matrix_lookup[complete.cases(matrix_lookup),]

names(matrix_lookup)[names(matrix_lookup)=="PCT_Code"] <- "PCT_Code"

matrix_lookup$CCG_Code <-  sub('.', '', matrix_lookup$CCG_Code)



rm(list=setdiff(ls(), c("matrix_lookup", "allocation_data_CCG", "allocation_data_PCT", "codes_ccg")))



merged <- merge(allocation_data_PCT, matrix_lookup, by="PCT_Code", all=T)

merged$lookup <- gsub('[[:punct:] ]+','',merged$lookup)

merged$lookup <- as.numeric(merged$lookup)

merged$Allocation_000s <- merged$Allocation_000s*(merged$lookup/100)

merged <- aggregate(.~CCG_Code+year, data=merged[c("CCG_Code", "year", "Allocation_000s")], sum)

df <- rbind(merged, allocation_data_CCG[c("CCG_Code", "year", "Allocation_000s")])

df <- df %>% dplyr::group_by(CCG_Code) %>% mutate(nobs = n())

#

df <- merge(df, codes_ccg ,by="CCG_Code", all.x=T)

df$CCG_Name[df$CCG_Code=="14L"] <- "GREATER MANCHESTER"
df$CCG_Name[df$CCG_Code=="14Y"] <- "BUCKINGHAMSHIRE OXFORDSHIRE AND BERKSHIRE WEST"
df$CCG_Name[df$CCG_Code=="15A"] <- "BERKSHIRE WEST"
df$CCG_Name[df$CCG_Code=="15C"] <- "BRISTOL NORTH SOMERSET AND SOUTH GLOUCESTERSHIRE"
df$CCG_Name[df$CCG_Code=="15E"] <- "BIRMINGHAM AND SOLIHULL"
df$CCG_Name[df$CCG_Code=="15F"] <- "LEEDS"
df$CCG_Name[df$CCG_Code=="15M"] <- "DERBY AND DERBYSHIRE"
df$CCG_Name[df$CCG_Code=="15D"] <- "EAST BERKSHIRE"
df$CCG_Name[df$CCG_Code=="NHS Erewash CCG"] <- "EREWASH"
df$CCG_Code[df$CCG_Code=="NHS Erewash CCG"] <- "03X"
df$CCG_Name[df$CCG_Code=="NHS Hardwick CCG"] <- "HARDWICK"
df$CCG_Code[df$CCG_Code=="NHS Hardwick CCG"] <- "03Y"
df$CCG_Name[df$CCG_Code=="NHS Kernow CCG"] <- "KERNOW"
df$CCG_Code[df$CCG_Code=="NHS Kernow CCG"] <- "11N"




####Population####

ccgpop04 <- read_csv("Data/ccg_pop_2004.csv")[c(1:3)]
ccgpop05 <- read_csv("Data/ccg_pop_2005.csv")[c(1:3)]
ccgpop06 <- read_csv("Data/ccg_pop_2006.csv")[c(1:3)]
ccgpop07 <- read_csv("Data/ccg_pop_2007.csv")[c(1:3)]
ccgpop08 <- read_csv("Data/ccg_pop_2008.csv")[c(1:3)]
ccgpop09 <- read_csv("Data/ccg_pop_2009.csv")[c(1:3)]
ccgpop10 <- read_csv("Data/ccg_pop_2010.csv")[c(1:3)]
ccgpop11 <- read_csv("Data/ccg_pop_2011.csv")[c(1:3)]



ccgpop12 <- read_csv("Data/ccg_pop_2012.csv", skip = 6)[c(1:3)]
ccgpop13 <- read_csv("Data/ccg_pop_2013.csv", skip = 6)[c(1:3)]
ccgpop14 <- read_csv("Data/ccg_pop_2014.csv", skip = 6)[c(1:3)]
ccgpop15 <- read_csv("Data/ccg_pop_2015.csv", skip = 6)[c(1:3)]
ccgpop16 <- read_csv("Data/ccg_pop_2016.csv", skip = 6)[c(1:3)]
ccgpop17 <- read_csv("Data/ccg_pop_2017.csv", skip = 6)[c(1:3)]
ccgpop18 <- read_csv("Data/ccg_pop_2018.csv", skip = 6)[c(1:3)]
ccgpop19 <- read_csv("Data/ccg_pop_2019.csv", skip = 6)[c(1,2,7)]
ccgpop20 <- read_csv("Data/ccg_pop_2020.csv", skip = 6)[c(1,2,7)]

ccgpop04$year <- 2004
ccgpop05$year <- 2005
ccgpop06$year <- 2006
ccgpop07$year <- 2007
ccgpop08$year <- 2008
ccgpop09$year <- 2009
ccgpop10$year <- 2010
ccgpop11$year <- 2011

ccgpop12$year <- 2012
ccgpop13$year <- 2013
ccgpop14$year <- 2014
ccgpop15$year <- 2015
ccgpop16$year <- 2016
ccgpop17$year <- 2017
ccgpop18$year <- 2018
ccgpop19$year <- 2019
ccgpop20$year <- 2020

names(ccgpop12) <- names(ccgpop04)
names(ccgpop13) <- names(ccgpop04)
names(ccgpop14) <- names(ccgpop04)
names(ccgpop15) <- names(ccgpop04)
names(ccgpop16) <- names(ccgpop04)
names(ccgpop17) <- names(ccgpop04)
names(ccgpop18) <- names(ccgpop04)
names(ccgpop19) <- names(ccgpop04)
names(ccgpop20) <- names(ccgpop04)

ccgpop <- rbind(ccgpop04, ccgpop05, ccgpop06, ccgpop07, ccgpop08, ccgpop09, ccgpop10, ccgpop11,
                ccgpop12, ccgpop13, ccgpop14, ccgpop15, ccgpop16, ccgpop17, ccgpop18, ccgpop19,ccgpop20)


names(ccgpop)[names(ccgpop)=="Area_Name"] <- "CCG_Name"

ccgpop$CCG_Name <-  gsub('&','and',ccgpop$CCG_Name)
ccgpop$CCG_Name <-  gsub('[[:punct:] ]+',' ',ccgpop$CCG_Name)
ccgpop$CCG_Name <-  gsub('NHS ','',ccgpop$CCG_Name)
ccgpop$CCG_Name <-  gsub('CCG','',ccgpop$CCG_Name)
ccgpop$CCG_Name <-  toupper(ccgpop$CCG_Name)
ccgpop$CCG_Name <-  str_trim(ccgpop$CCG_Name)

df$CCG_Name <-  gsub('CCG','',df$CCG_Name)
df$CCG_Name <-  str_trim(df$CCG_Name)


df <- merge(df, ccgpop,by=c("CCG_Name", "year"),all.x=T)


####deprivation####


imd <- read.csv("Data/CCG_IMD_19.csv")

names(imd)[names(imd)=="ccg19nm"] <- "CCG_Name"


imd$CCG_Name <-  gsub('&','and',imd$CCG_Name)
imd$CCG_Name <-  gsub('[[:punct:] ]+',' ',imd$CCG_Name)
imd$CCG_Name <-  gsub('NHS ','',imd$CCG_Name)
imd$CCG_Name <-  gsub('CCG','',imd$CCG_Name)
imd$CCG_Name <-  str_trim(imd$CCG_Name)
imd$CCG_Name <-  toupper(imd$CCG_Name)

df <- merge(df, imd, by="CCG_Name", all.x=T)


####GDP Deflator####


gdp <- read_csv("Data/GDP_Deflators_Qtrly_National_Accounts_March_2021_update.csv", skip = 6)[c(2:4)]
gdp$`2019-20 = 100` <-as.numeric(gdp$`2019-20 = 100`)
gdp <- gdp[complete.cases(gdp$`2019-20 = 100`),]

gdp$year <-  gsub("^.{0,5}", "", gdp$`Financial year`)
gdp$year <-  str_prefix(gdp$year, "20")

gdp$year<- as.numeric(gdp$year)
gdp <- gdp[which(gdp$year<2030),]
gdp$gdp_deflation_multiplier <- 100/gdp$`2019-20 = 100`

df <- merge(df, gdp[c("year", "gdp_deflation_multiplier")], by="year", all.x=T)

df$deflated_per_person_allocation <- (df$Allocation_000s/df$all_ages)*df$gdp_deflation_multiplier


####NHS Spend#####


library(NHSOutsourcingTMortality)
library(zen4R)

#myDataCCG <- read.csv("crahal-NHSSpend-1846777/data/data_merge/")
#unzip("NHSSpend-v.1.0.2.zip")
#unzip("crahal-NHSSpend-1846777/data/data_final/payments_ccg_final.zip")
myDataCCG <- read.csv("payments_ccg_final.csv")

#myDataCCG <- NHSOutsourcingTMortality::Download_CCG_payments() 
MyAnnualDataCCG <- Create_Annual_CCG_dataset(myDataCCG = myDataCCG)
lookup <- read.csv("Data/ccg_list.csv")[c(1,3)]
lookup <- lookup[lookup$abrev!="",]

lookup <- lookup %>% dplyr::rename(dept = abrev,
                                   CCG_Name = ccg19nm)

MyAnnualDataCCG <- merge(MyAnnualDataCCG, lookup, by="dept", all.x=T)

MyAnnualDataCCG$CCG_Name <-  gsub('&','and',MyAnnualDataCCG$CCG_Name)
MyAnnualDataCCG$CCG_Name <-  gsub('[[:punct:] ]+',' ',MyAnnualDataCCG$CCG_Name)
MyAnnualDataCCG$CCG_Name <-  gsub('NHS ','',MyAnnualDataCCG$CCG_Name)
MyAnnualDataCCG$CCG_Name <-  gsub('CCG','',MyAnnualDataCCG$CCG_Name)
MyAnnualDataCCG$CCG_Name <-  str_trim(MyAnnualDataCCG$CCG_Name)
MyAnnualDataCCG$CCG_Name <-  toupper(MyAnnualDataCCG$CCG_Name)

df <- merge(MyAnnualDataCCG, df, by=c("CCG_Name", "year"),all=T)




####La Allocation####

LA_allocation_07_08 <- read.csv("Data/LA_allocation_2008.csv", skip=5)
LA_allocation_08_09 <- read.csv("Data/LA_allocation_2009.csv", skip=5)
LA_allocation_09_10 <- read.csv("Data/LA_allocation_2010.csv", skip=5)
LA_allocation_10_11 <- read.csv("Data/LA_allocation_2011.csv", skip=7)
LA_allocation_11_12 <- read.csv("Data/LA_allocation_2012.csv", skip=5)
LA_allocation_12_13 <- read.csv("Data/LA_allocation_2013.csv", skip=5)
LA_allocation_13_14 <- read.csv("Data/LA_allocation_2014.csv", skip=5)
LA_allocation_14_15 <- read.csv("Data/LA_allocation_2015.csv", skip=5)
LA_allocation_15_16 <- read.csv("Data/LA_allocation_2016.csv", skip=6)
LA_allocation_16_17 <- read.csv("Data/LA_allocation_2017.csv", skip=6)
LA_allocation_17_18 <- read.csv("Data/LA_allocation_2018.csv", skip=4)
LA_allocation_18_19 <- read.csv("Data/LA_allocation_2019.csv", skip=6)
LA_allocation_19_20 <- read.csv("Data/LA_allocation_2020.csv", skip=6)

LA_allocation_07_08 <- LA_allocation_07_08 %>% dplyr::select(E.code, Local.authority, Class, Revenue.Support.Grant, Redistributed.non.domestic.rates)
LA_allocation_08_09 <- LA_allocation_08_09 %>% dplyr::select(E.code, Local.authority, Class, Revenue.Support.Grant, Redistributed.non.domestic.rates)
LA_allocation_09_10 <- LA_allocation_09_10 %>% dplyr::select(E.code, Local.authority, Class, Revenue.Support.Grant, Redistributed.non.domestic.rates)
LA_allocation_10_11 <- LA_allocation_10_11 %>% dplyr::select(E.code, Local.authority, Class, Revenue.Support.Grant, Redistributed.non.domestic.rates)
LA_allocation_11_12 <- LA_allocation_11_12 %>% dplyr::select(E.code, Local.authority, Class, Revenue.Support.Grant, Redistributed.non.domestic.rates)
LA_allocation_12_13 <- LA_allocation_12_13 %>% dplyr::select(E.code, Local.authority, Class, Revenue.Support.Grant, Redistributed.non.domestic.rates)
LA_allocation_13_14 <- LA_allocation_13_14 %>% dplyr::select(E.code, Local.authority, Class, Revenue.Support.Grant, Retained.income.from.Rate.Retention.Scheme)
LA_allocation_14_15 <- LA_allocation_14_15 %>% dplyr::select(E.code, Local.authority, Class, Revenue.Support.Grant, Retained.income.from.Rate.Retention.Scheme)
LA_allocation_15_16 <- LA_allocation_15_16 %>% dplyr::select(E.code, Local.authority, Class, Revenue.Support.Grant, Retained.income.from.Rate.Retention.Scheme)
LA_allocation_16_17 <- LA_allocation_16_17 %>% dplyr::select(E.code, Local.authority, Class, Revenue.Support.Grant, Retained.income.from.Rate.Retention.Scheme)
LA_allocation_17_18 <- LA_allocation_17_18 %>% dplyr::select(E.code, Local.authority, Class, Revenue.Support.Grant, Retained.income.from.Rate.Retention.Scheme)
LA_allocation_18_19 <- LA_allocation_18_19 %>% dplyr::select(E.code, Local.authority, Class, Revenue.Support.Grant, Retained.income.from.Rate.Retention.Scheme)
LA_allocation_19_20 <- LA_allocation_19_20 %>% dplyr::select(E.code, Local.authority, Class, Revenue.Support.Grant, Retained.income.from.Rate.Retention.Scheme)

LA_allocation_07_08$year <- 2008
LA_allocation_08_09$year <- 2009
LA_allocation_09_10$year <- 2010
LA_allocation_10_11$year <- 2011
LA_allocation_11_12$year <- 2012
LA_allocation_12_13$year <- 2013
LA_allocation_13_14$year <- 2014
LA_allocation_14_15$year <- 2015
LA_allocation_15_16$year <- 2016
LA_allocation_16_17$year <- 2017
LA_allocation_17_18$year <- 2018
LA_allocation_18_19$year <- 2019
LA_allocation_19_20$year <- 2020



LA_allocation <- plyr::rbind.fill(LA_allocation_07_08, LA_allocation_08_09, LA_allocation_09_10,
                                  LA_allocation_10_11, LA_allocation_11_12, LA_allocation_12_13,
                                  LA_allocation_13_14, LA_allocation_14_15, LA_allocation_15_16,
                                  LA_allocation_16_17, LA_allocation_17_18, LA_allocation_18_19,
                                  LA_allocation_19_20)

LA_allocation <- LA_allocation[grepl("^E", LA_allocation$E.code, ignore.case = TRUE),]
LA_upper_lookup <- read.csv("Data/Lower_Tier_Local_Authority_to_Upper_Tier_Local_Authority_(December_2020)_Lookup_in_England_and_Wales.csv")

LA_allocation$Local.authority <-  gsub('&','and',LA_allocation$Local.authority)
LA_upper_lookup$LTLA20NM <-  gsub('&','and',LA_upper_lookup$LTLA20NM)
LA_upper_lookup$UTLA20NM <-  gsub('&','and',LA_upper_lookup$UTLA20NM)

LA_allocation$Local.authority <-  gsub('[[:punct:] ]+',' ',LA_allocation$Local.authority)
LA_upper_lookup$LTLA20NM <-  gsub('[[:punct:] ]+',' ',LA_upper_lookup$LTLA20NM)
LA_upper_lookup$UTLA20NM <-  gsub('[[:punct:] ]+',' ',LA_upper_lookup$UTLA20NM)

LA_allocation$Local.authority <-  gsub(' UA','',LA_allocation$Local.authority)
LA_allocation$Local.authority <-  gsub(' BC','',LA_allocation$Local.authority)
LA_allocation$Local.authority <-  gsub(' CC','',LA_allocation$Local.authority)
LA_allocation$Local.authority <-  gsub(' DC','',LA_allocation$Local.authority)
LA_allocation$Local.authority <-  gsub(' MBC','',LA_allocation$Local.authority)
#LA_upper_lookup$LTLA20NM <-  gsub(' UA','',LA_upper_lookup$LTLA20NM)

#LA_allocation$Local.authority <-  gsub('PCT','',LA_allocation$Local.authority)
#LA_upper_lookup$LTLA20NM <-  gsub('PCT','',LA_upper_lookup$LTLA20NM)

LA_allocation$Local.authority <-  toupper(LA_allocation$Local.authority)
LA_upper_lookup$LTLA20NM <-  toupper(LA_upper_lookup$LTLA20NM)
LA_upper_lookup$UTLA20NM <-  toupper(LA_upper_lookup$UTLA20NM)

LA_allocation$Local.authority <-  str_trim(LA_allocation$Local.authority)
LA_upper_lookup$LTLA20NM <-  str_trim(LA_upper_lookup$LTLA20NM)
LA_upper_lookup$UTLA20NM <-  str_trim(LA_upper_lookup$UTLA20NM)

LA_upper_lookup <- LA_upper_lookup %>%dplyr::rename(Local.authority = LTLA20NM)

#LA_allocation[which(LA_allocation$Local.authority=="MIDDLESBOROUGH"),]$Local.authority <- "MIDDLESBROUGH"

####MERGE BY POP HERE AND DO POP WEIGHTED AVERAGES down to lower tiers
####NAHHHH FUCK IT, DO UPPER LA LOOK TO CCG FOR LA FUNDS AND LOWER FOR BENS


LA_allocation <- merge(LA_allocation, LA_upper_lookup, by= "Local.authority", all=T)

LA_allocation[which(is.na(LA_allocation$UTLA20NM)&LA_allocation$Class=="SC"),]$UTLA20NM <- LA_allocation[which(is.na(LA_allocation$UTLA20NM)&LA_allocation$Class=="SC"),]$Local.authority
LA_allocation[which(LA_allocation$Local.authority=="AYLESBURY VALE"),]$UTLA20NM <- "BUCKINGHAMSHIRE"
LA_allocation[which(LA_allocation$Local.authority=="CHILTERN"),]$UTLA20NM <- "BUCKINGHAMSHIRE"
LA_allocation[which(LA_allocation$Local.authority=="SOUTH BUCKS"),]$UTLA20NM <- "BUCKINGHAMSHIRE"
LA_allocation[which(LA_allocation$Local.authority=="WYCOMBE"),]$UTLA20NM <- "BUCKINGHAMSHIRE"
LA_allocation[which(LA_allocation$Local.authority=="SOUTH BUCKINGHAMSHIRE"),]$UTLA20NM <- "BUCKINGHAMSHIRE"
LA_allocation[which(LA_allocation$Local.authority=="ALNWICK"),]$UTLA20NM <- "NORTHUMBERLAND"
LA_allocation[which(LA_allocation$Local.authority=="TYNEDALE"),]$UTLA20NM <- "NORTHUMBERLAND"
LA_allocation[which(LA_allocation$Local.authority=="CASTLE MORPETH"),]$UTLA20NM <- "NORTHUMBERLAND"
LA_allocation[which(LA_allocation$Local.authority=="WANSBECK"),]$UTLA20NM <- "NORTHUMBERLAND"
LA_allocation[which(LA_allocation$Local.authority=="BERWICK UPON TWEED"),]$UTLA20NM <- "NORTHUMBERLAND"
LA_allocation[which(LA_allocation$Local.authority=="BLYTH VALLEY"),]$UTLA20NM <- "NORTHUMBERLAND"
LA_allocation[which(LA_allocation$Local.authority=="BOURNEMOUTH"),]$UTLA20NM <- "BOURNEMOUTH CHRISTCHURCH AND POOLE"
LA_allocation[which(LA_allocation$Local.authority=="CHRISTCHURCH"),]$UTLA20NM <- "BOURNEMOUTH CHRISTCHURCH AND POOLE"
LA_allocation[which(LA_allocation$Local.authority=="POOLE"),]$UTLA20NM <- "BOURNEMOUTH CHRISTCHURCH AND POOLE"
LA_allocation[which(LA_allocation$Local.authority=="BRIDGNORTH"),]$UTLA20NM <- "SHROPSHIRE"
LA_allocation[which(LA_allocation$Local.authority=="NORTH SHROPSHIRE"),]$UTLA20NM <- "SHROPSHIRE"
LA_allocation[which(LA_allocation$Local.authority=="SOUTH SHROPSHIRE"),]$UTLA20NM <- "SHROPSHIRE"
LA_allocation[which(LA_allocation$Local.authority=="OSWESTRY"),]$UTLA20NM <- "SHROPSHIRE"
LA_allocation[which(LA_allocation$Local.authority=="SHREWSBURY AND ATCHAM"),]$UTLA20NM <- "SHROPSHIRE"
LA_allocation[which(LA_allocation$Local.authority=="BRISTOL"),]$UTLA20NM <- "BRISTOL CITY OF"
LA_allocation[which(LA_allocation$Local.authority=="CARADON"),]$UTLA20NM <- "CORNWALL"
LA_allocation[which(LA_allocation$Local.authority=="CARRICK"),]$UTLA20NM <- "CORNWALL"
LA_allocation[which(LA_allocation$Local.authority=="KERRIER"),]$UTLA20NM <- "CORNWALL"
LA_allocation[which(LA_allocation$Local.authority=="PENWITH"),]$UTLA20NM <- "CORNWALL"
LA_allocation[which(LA_allocation$Local.authority=="RESTORMEL"),]$UTLA20NM <- "CORNWALL"
LA_allocation[which(LA_allocation$Local.authority=="NORTH CORNWALL"),]$UTLA20NM <- "CORNWALL"
LA_allocation[which(LA_allocation$Local.authority=="CHESTER LE STREET"),]$UTLA20NM <- "COUNTY DURHAM"
LA_allocation[which(LA_allocation$Local.authority=="DERWENTSIDE"),]$UTLA20NM <- "COUNTY DURHAM"
LA_allocation[which(LA_allocation$Local.authority=="DURHAM"),]$UTLA20NM <- "COUNTY DURHAM"
LA_allocation[which(LA_allocation$Local.authority=="DURHAM CITY"),]$UTLA20NM <- "COUNTY DURHAM"
LA_allocation[which(LA_allocation$Local.authority=="EASINGTON"),]$UTLA20NM <- "COUNTY DURHAM"
LA_allocation[which(LA_allocation$Local.authority=="SEDGEFIELD"),]$UTLA20NM <- "COUNTY DURHAM"
LA_allocation[which(LA_allocation$Local.authority=="TEESDALE"),]$UTLA20NM <- "COUNTY DURHAM"
LA_allocation[which(LA_allocation$Local.authority=="WEAR VALLEY"),]$UTLA20NM <- "COUNTY DURHAM"
LA_allocation[which(LA_allocation$Local.authority=="CHESTER"),]$UTLA20NM <- "CHESHIRE WEST AND CHESTER"
LA_allocation[which(LA_allocation$Local.authority=="VALE ROYAL"),]$UTLA20NM <- "CHESHIRE WEST AND CHESTER"
LA_allocation[which(LA_allocation$Local.authority=="ELLESMERE PORT AND NESTON"),]$UTLA20NM <- "CHESHIRE WEST AND CHESTER"
LA_allocation[which(LA_allocation$Local.authority=="CITY OF NOTTINGHAM"),]$UTLA20NM <- "NOTTINGHAM"
LA_allocation[which(LA_allocation$Local.authority=="CONGLETON"),]$UTLA20NM <- "CHESHIRE EAST"
LA_allocation[which(LA_allocation$Local.authority=="MACCLESFIELD"),]$UTLA20NM <- "CHESHIRE EAST"
LA_allocation[which(LA_allocation$Local.authority=="CREWE AND NANTWICH"),]$UTLA20NM <- "CHESHIRE EAST"
LA_allocation[which(LA_allocation$Local.authority=="DERBY CITY"),]$UTLA20NM <- "DERBY"
LA_allocation[which(LA_allocation$Local.authority=="EAST DORSET"),]$UTLA20NM <- "DORSET"
LA_allocation[which(LA_allocation$Local.authority=="NORTH DORSET"),]$UTLA20NM <- "DORSET"
LA_allocation[which(LA_allocation$Local.authority=="WEST DORSET"),]$UTLA20NM <- "DORSET"
LA_allocation[which(LA_allocation$Local.authority=="PURBECK"),]$UTLA20NM <- "DORSET"
LA_allocation[which(LA_allocation$Local.authority=="WEYMOUTH AND PORTLAND"),]$UTLA20NM <- "DORSET"
LA_allocation[which(LA_allocation$Local.authority=="FOLKSTONE AND HYTHE"),]$UTLA20NM <- "KENT"
LA_allocation[which(LA_allocation$Local.authority=="SHEPWAY"),]$UTLA20NM <- "KENT"
LA_allocation[which(LA_allocation$Local.authority=="FOREST HEATH"),]$UTLA20NM <- "SUFFOLK"
LA_allocation[which(LA_allocation$Local.authority=="ST EDMUNDSBURY"),]$UTLA20NM <- "SUFFOLK"
LA_allocation[which(LA_allocation$Local.authority=="SUFFOLK COASTAL"),]$UTLA20NM <- "SUFFOLK"
LA_allocation[which(LA_allocation$Local.authority=="WAVENEY"),]$UTLA20NM <- "SUFFOLK"
LA_allocation[which(LA_allocation$Local.authority=="HEREFORDSHIRE"),]$UTLA20NM <- "HEREFORDSHIRE COUNTY OF"
LA_allocation[which(LA_allocation$Local.authority=="KENNET"),]$UTLA20NM <- "WILTSHIRE"
LA_allocation[which(LA_allocation$Local.authority=="NORTH WILTSHIRE"),]$UTLA20NM <- "WILTSHIRE"
LA_allocation[which(LA_allocation$Local.authority=="WEST WILTSHIRE"),]$UTLA20NM <- "WILTSHIRE"
LA_allocation[which(LA_allocation$Local.authority=="SALISBURY"),]$UTLA20NM <- "WILTSHIRE"
LA_allocation[which(LA_allocation$Local.authority=="KINGSTON UPON HULL"),]$UTLA20NM <- "KINGSTON UPON HULL CITY OF"
LA_allocation[which(LA_allocation$Local.authority=="LEICESTER CITY"),]$UTLA20NM <- "LEICESTER"
LA_allocation[which(LA_allocation$Local.authority=="LINCOLN CITY"),]$UTLA20NM <- "LINCOLNSHIRE"
LA_allocation[which(LA_allocation$Local.authority=="MAIDON"),]$UTLA20NM <- "ESSEX"
LA_allocation[which(LA_allocation$Local.authority=="MEDWAY TOWNS"),]$UTLA20NM <- "MEDWAY"
LA_allocation[which(LA_allocation$Local.authority=="MID BEDFORDSHIRE"),]$UTLA20NM <- "CENTRAL BEDFORDSHIRE"
LA_allocation[which(LA_allocation$Local.authority=="SOUTH BEDFORDSHIRE"),]$UTLA20NM <- "CENTRAL BEDFORDSHIRE"
#LA_allocation[which(LA_allocation$Local.authority=="MIDDLESBOROUGH"),]$UTLA20NM <- "MIDDLESBROUGH"
LA_allocation[which(LA_allocation$Local.authority=="NEWCASTLE"),]$UTLA20NM <- "NEWCASTLE UPON TYNE"
LA_allocation[which(LA_allocation$Local.authority=="NORWICH CITY"),]$UTLA20NM <- "NORFOLK"
LA_allocation[which(LA_allocation$Local.authority=="NOTTINGHAM CITY"),]$UTLA20NM <- "NOTTINGHAM"
LA_allocation[which(LA_allocation$Local.authority=="TAUNTON DEANE"),]$UTLA20NM <- "SOMERSET"
LA_allocation[which(LA_allocation$Local.authority=="WEST SOMERSET"),]$UTLA20NM <- "SOMERSET"
LA_allocation[which(LA_allocation$Local.authority=="TELFORD AND THE WREKIN"),]$UTLA20NM <- "TELFORD AND WREKIN"
LA_allocation[which(LA_allocation$Local.authority=="THE MEDWAY TOWNS"),]$UTLA20NM <- "MEDWAY"



# check <- unique(LA_allocation[is.na(LA_allocation$UTLA20NM),][c("Local.authority","UTLA20NM")])
# check2 <- check[which(check$Class=="SC"),]
# check3 <- check[!grepl("AUTHORITY", check$Local.authority),]


LA_allocation$Revenue.Support.Grant <-  gsub('[[:punct:] ]+','',LA_allocation$Revenue.Support.Grant)
LA_allocation$Redistributed.non.domestic.rates <-  gsub('[[:punct:] ]+','',LA_allocation$Redistributed.non.domestic.rates)
LA_allocation$Retained.income.from.Rate.Retention.Scheme <-  gsub('[[:punct:] ]+','',LA_allocation$Retained.income.from.Rate.Retention.Scheme)

LA_allocation$Revenue.Support.Grant <- as.numeric(LA_allocation$Revenue.Support.Grant)
LA_allocation$Redistributed.non.domestic.rates <- as.numeric(LA_allocation$Redistributed.non.domestic.rates)
LA_allocation$Retained.income.from.Rate.Retention.Scheme <- as.numeric(LA_allocation$Retained.income.from.Rate.Retention.Scheme)

LA_allocation$Revenue.Support.Grant[is.na(LA_allocation$Revenue.Support.Grant)] <- 0
LA_allocation$Redistributed.non.domestic.rates[is.na(LA_allocation$Redistributed.non.domestic.rates)] <- 0
LA_allocation$Retained.income.from.Rate.Retention.Scheme[is.na(LA_allocation$Retained.income.from.Rate.Retention.Scheme)] <- 0

LA_allocation$Central_gov_allocation <- LA_allocation$Revenue.Support.Grant+LA_allocation$Redistributed.non.domestic.rates+LA_allocation$Retained.income.from.Rate.Retention.Scheme

codes <- LA_allocation%>% dplyr::select(UTLA20CD,UTLA20NM) %>%
  dplyr::filter(!is.na(UTLA20CD),
                !is.na(UTLA20NM))%>%
  dplyr::distinct()

LA_allocation <- aggregate(.~UTLA20NM+year, data=LA_allocation[c("UTLA20NM","year","Revenue.Support.Grant","Redistributed.non.domestic.rates","Retained.income.from.Rate.Retention.Scheme", "Central_gov_allocation" )], sum)

LA_allocation <- merge(unique(LA_allocation), codes, by="UTLA20NM", all=T)

LA_pops <- read.csv("Data/MYEB2_detailed_components_of_change_series_EW_(2020_geog20).csv")[c(1:25)]

LA_pops <- merge(LA_pops, LA_upper_lookup, by.x="ladcode20", by.y="LTLA20CD", all=F)

LA_pops <- aggregate(.~UTLA20CD, data=LA_pops[-c(1, 2,3,4,5,26,27,29)], sum)

LA_pops <- LA_pops%>%pivot_longer(cols = !UTLA20CD, names_to = "year", values_to = "population")

LA_pops$year <- gsub("population_","", LA_pops$year)

LA_allocation <- merge(LA_allocation, LA_pops, by=c("UTLA20CD", "year"), all.x=T)
LA_allocation <- merge(LA_allocation, gdp, by= "year", all.x=T)

LA_allocation$deflated_per_person_allocation_la <- (LA_allocation$Central_gov_allocation/LA_allocation$population)*LA_allocation$gdp_deflation_multiplier
codes <- LA_allocation%>% dplyr::select(UTLA20CD,UTLA20NM) %>%
  dplyr::filter(!is.na(UTLA20CD),
                !is.na(UTLA20NM))%>%
  dplyr::distinct()
LA_allocation <- LA_allocation %>% dplyr::select(-UTLA20CD)%>% dplyr::distinct()

LA_allocation <- merge(LA_allocation, codes, by="UTLA20NM", all=T)

####LA Benefits Data####
#Housing aye, DLA to PIP aye, incapacity to ESA,  tax credits; UC

pip <- read.csv("Data/pip.csv", skip=6)[c(1:4)]
uc <- read.csv("Data/UC.csv", skip=9)[c(2:5)]
dla_pre2018 <- read.csv("Data/DLA_pre2018.csv", skip=6)[c(1:4)]
dla_post2018 <- read.csv("Data/DLA_post2018.csv", skip=6)[c(1:4)]
esa_pre2018 <- read.csv("Data/ESA_pre2018.csv", skip=6)[c(1:4)]
esa_post2018 <- read.csv("Data/ESA_post2018.csv", skip=6)[c(1:4)]
incapacity <- read.csv("Data/incapacity.csv", skip=6)[c(1:4)]
income_support <- read.csv("Data/income support.csv", skip=6)[c(1:4)]
jobseekers <- read.csv("Data/jobseekers.csv", skip=6)[c(1:4)]
housing_benefits_pre2018 <- read.csv("Data/Housing_benefits_pre2018.csv", skip=6)[c(1:4)]
housing_benefits_post2018 <- read.csv("Data/Housing_benefits_post2018.csv", skip=6)[c(1:4)]
carers_allowance <- read.csv("Data/carers_allowance.csv", skip=6)[c(1:4)]

pip <- pip[pip$Month!="",]
pip <- pip%>%pivot_wider(names_from = Counting, values_from = Count, names_prefix = "pip_")
pip$year <- stringr::str_extract(pip$Month, "^.{4}")
pip$month <- stringr::str_extract(pip$Month, "^.{6}")
pip$month <- gsub( "^.{4}","", pip$month)
pip <- pip%>%mutate(quarter = ifelse(pip$month=="01"|pip$month=="02"|pip$month=="03",1, 
                                     ifelse(pip$month=="04"|pip$month=="05"|pip$month=="06",2, 
                                            ifelse(pip$month=="07"|pip$month=="08"|pip$month=="09",3,
                                                   ifelse(pip$month=="10"|pip$month=="11"|pip$month=="12",4,NA))))) 

uc <- uc[uc$Month!="",]
uc <- uc[str_detect(uc$Month, "^.{6}$"),]
uc <- uc%>%pivot_wider(names_from = Counting, values_from = Count, names_prefix = "uc_")
uc$year <- str_sub(uc$Month, - 2, - 1)   
uc$year <-   str_prefix(uc$year, "20")
uc$month <- str_sub(uc$Month, 1, 3)   
uc <- uc%>%mutate(month = ifelse(uc$month=="Jan", "01", ifelse(uc$month=="Feb", "02", ifelse(uc$month=="Mar","03", 
                                                                                             ifelse(uc$month=="Apr", "04", ifelse(uc$month=="May", "05", ifelse(uc$month=="Jun","06", 
                                                                                                                                                                ifelse(uc$month=="Jul", "07", ifelse(uc$month=="Aug", "08", ifelse(uc$month=="Sep","09",
                                                                                                                                                                                                                                   ifelse(uc$month=="Oct","10",ifelse(uc$month=="Nov" ,"11", ifelse(uc$month=="Dec","12",NA)))))))))))))

uc <- uc%>%mutate(quarter = ifelse(uc$month=="01"|uc$month=="02"|uc$month=="03",1, 
                                   ifelse(uc$month=="04"|uc$month=="05"|uc$month=="06",2, 
                                          ifelse(uc$month=="07"|uc$month=="08"|uc$month=="09",3,
                                                 ifelse(uc$month=="10"|uc$month=="11"|uc$month=="12",4,NA))))) 

dla_pre2018 <- dla_pre2018[dla_pre2018$Quarter!="",]
dla_pre2018 <- dla_pre2018%>%pivot_wider(names_from = Counting, values_from = Count, names_prefix = "dla_pre2018_")
dla_pre2018$year <- str_sub(dla_pre2018$Quarter, - 2, - 1)   
dla_pre2018$year <-   str_prefix(dla_pre2018$year, "20")
dla_pre2018$Quarter <- stringr::str_extract(dla_pre2018$Quarter, "^.{3}")
dla_pre2018 <- dla_pre2018%>%mutate(quarter = ifelse(dla_pre2018$Quarter=="Feb",1, 
                                                     ifelse(dla_pre2018$Quarter=="May",2, 
                                                            ifelse(dla_pre2018$Quarter=="Aug",3,
                                                                   ifelse(dla_pre2018$Quarter=="Nov",4,NA))))) 


dla_post2018 <- dla_post2018[dla_post2018$Quarter!="",]
dla_post2018 <- dla_post2018%>%pivot_wider(names_from = Counting, values_from = Count, names_prefix = "dla_post2018_")
dla_post2018$year <- str_sub(dla_post2018$Quarter, - 2, - 1)   
dla_post2018$year <-   str_prefix(dla_post2018$year, "20")
dla_post2018$Quarter <- stringr::str_extract(dla_post2018$Quarter, "^.{3}")
dla_post2018 <- dla_post2018%>%mutate(quarter = ifelse(dla_post2018$Quarter=="Feb",1, 
                                                       ifelse(dla_post2018$Quarter=="May",2, 
                                                              ifelse(dla_post2018$Quarter=="Aug",3,
                                                                     ifelse(dla_post2018$Quarter=="Nov",4,NA))))) 

esa_pre2018 <- esa_pre2018[esa_pre2018$Quarter!="",]
esa_pre2018 <- esa_pre2018%>%pivot_wider(names_from = Counting, values_from = Count, names_prefix = "esa_pre2018_")
esa_pre2018$year <- str_sub(esa_pre2018$Quarter, - 2, - 1)   
esa_pre2018$year <-   str_prefix(esa_pre2018$year, "20")
esa_pre2018$Quarter <- stringr::str_extract(esa_pre2018$Quarter, "^.{3}")
esa_pre2018 <- esa_pre2018%>%mutate(quarter = ifelse(esa_pre2018$Quarter=="Feb",1, 
                                                     ifelse(esa_pre2018$Quarter=="May",2, 
                                                            ifelse(esa_pre2018$Quarter=="Aug",3,
                                                                   ifelse(esa_pre2018$Quarter=="Nov",4,NA))))) 


esa_post2018 <- esa_post2018[esa_post2018$Quarter!="",]
esa_post2018 <- esa_post2018%>%pivot_wider(names_from = Counting, values_from = Count, names_prefix = "esa_post2018_")
esa_post2018$year <- str_sub(esa_post2018$Quarter, - 2, - 1)   
esa_post2018$year <-   str_prefix(esa_post2018$year, "20")
esa_post2018$Quarter <- stringr::str_extract(esa_post2018$Quarter, "^.{3}")
esa_post2018 <- esa_post2018%>%mutate(quarter = ifelse(esa_post2018$Quarter=="Feb",1, 
                                                       ifelse(esa_post2018$Quarter=="May",2, 
                                                              ifelse(esa_post2018$Quarter=="Aug",3,
                                                                     ifelse(esa_post2018$Quarter=="Nov",4,NA))))) 

incapacity <- incapacity[incapacity$Quarter!="",]
incapacity <- incapacity%>%pivot_wider(names_from = Counting, values_from = Count, names_prefix = "incapacity_")
incapacity$year <- str_sub(incapacity$Quarter, - 2, - 1)   
incapacity$year <-   str_prefix(incapacity$year, "20")
incapacity$Quarter <- stringr::str_extract(incapacity$Quarter, "^.{3}")
incapacity <- incapacity%>%mutate(quarter = ifelse(incapacity$Quarter=="Feb",1, 
                                                   ifelse(incapacity$Quarter=="May",2, 
                                                          ifelse(incapacity$Quarter=="Aug",3,
                                                                 ifelse(incapacity$Quarter=="Nov",4,NA))))) 


income_support <- income_support[income_support$Quarter!="",]
income_support <- income_support%>%pivot_wider(names_from = Counting, values_from = Count, names_prefix = "income_support_")
income_support$year <- str_sub(income_support$Quarter, - 2, - 1)   
income_support$year <-   str_prefix(income_support$year, "20")
income_support$Quarter <- stringr::str_extract(income_support$Quarter, "^.{3}")
income_support <- income_support%>%mutate(quarter = ifelse(income_support$Quarter=="Feb",1, 
                                                           ifelse(income_support$Quarter=="May",2, 
                                                                  ifelse(income_support$Quarter=="Aug",3,
                                                                         ifelse(income_support$Quarter=="Nov",4,NA))))) 


jobseekers <- jobseekers[jobseekers$Quarter!="",]
jobseekers <- jobseekers%>%pivot_wider(names_from = Counting, values_from = Count, names_prefix = "jobseekers_")
jobseekers$year <- str_sub(jobseekers$Quarter, - 2, - 1)   
jobseekers$year <-   str_prefix(jobseekers$year, "20")
jobseekers$Quarter <- stringr::str_extract(jobseekers$Quarter, "^.{3}")
jobseekers <- jobseekers%>%mutate(quarter = ifelse(jobseekers$Quarter=="Feb",1, 
                                                   ifelse(jobseekers$Quarter=="May",2, 
                                                          ifelse(jobseekers$Quarter=="Aug",3,
                                                                 ifelse(jobseekers$Quarter=="Nov",4,NA))))) 


carers_allowance <- carers_allowance[carers_allowance$Quarter!="",]
carers_allowance <- carers_allowance%>%pivot_wider(names_from = Counting, values_from = Count, names_prefix = "carers_allowance_")
carers_allowance$year <- str_sub(carers_allowance$Quarter, - 2, - 1)   
carers_allowance$year <-   str_prefix(carers_allowance$year, "20")
carers_allowance$Quarter <- stringr::str_extract(carers_allowance$Quarter, "^.{3}")
carers_allowance <- carers_allowance%>%mutate(quarter = ifelse(carers_allowance$Quarter=="Feb",1, 
                                                               ifelse(carers_allowance$Quarter=="May",2, 
                                                                      ifelse(carers_allowance$Quarter=="Aug",3,
                                                                             ifelse(carers_allowance$Quarter=="Nov",4,NA))))) 


housing_benefits_pre2018 <- housing_benefits_pre2018[housing_benefits_pre2018$Month!="",]
housing_benefits_pre2018 <- housing_benefits_pre2018%>%pivot_wider(names_from = Counting, values_from = Count, names_prefix = "housing_benefits_pre2018_")
housing_benefits_pre2018$year <- stringr::str_extract(housing_benefits_pre2018$Month, "^.{4}")
housing_benefits_pre2018$month <- stringr::str_extract(housing_benefits_pre2018$Month, "^.{6}")
housing_benefits_pre2018$month <- gsub( "^.{4}","", housing_benefits_pre2018$month)
housing_benefits_pre2018 <- housing_benefits_pre2018%>%mutate(quarter = ifelse(housing_benefits_pre2018$month=="01"|housing_benefits_pre2018$month=="02"|housing_benefits_pre2018$month=="03",1, 
                                                                               ifelse(housing_benefits_pre2018$month=="04"|housing_benefits_pre2018$month=="05"|housing_benefits_pre2018$month=="06",2, 
                                                                                      ifelse(housing_benefits_pre2018$month=="07"|housing_benefits_pre2018$month=="08"|housing_benefits_pre2018$month=="09",3,
                                                                                             ifelse(housing_benefits_pre2018$month=="10"|housing_benefits_pre2018$month=="11"|housing_benefits_pre2018$month=="12",4,NA))))) 


housing_benefits_post2018 <- housing_benefits_post2018[housing_benefits_post2018$Month!="",]
housing_benefits_post2018 <- housing_benefits_post2018%>%pivot_wider(names_from = Counting, values_from = Count, names_prefix = "housing_benefits_post2018_")
housing_benefits_post2018$year <- stringr::str_extract(housing_benefits_post2018$Month, "^.{4}")
housing_benefits_post2018$month <- stringr::str_extract(housing_benefits_post2018$Month, "^.{6}")
housing_benefits_post2018$month <- gsub( "^.{4}","", housing_benefits_post2018$month)
housing_benefits_post2018 <- housing_benefits_post2018%>%mutate(quarter = ifelse(housing_benefits_post2018$month=="01"|housing_benefits_post2018$month=="02"|housing_benefits_post2018$month=="03",1, 
                                                                                 ifelse(housing_benefits_post2018$month=="04"|housing_benefits_post2018$month=="05"|housing_benefits_post2018$month=="06",2, 
                                                                                        ifelse(housing_benefits_post2018$month=="07"|housing_benefits_post2018$month=="08"|housing_benefits_post2018$month=="09",3,
                                                                                               ifelse(housing_benefits_post2018$month=="10"|housing_benefits_post2018$month=="11"|housing_benefits_post2018$month=="12",4,NA))))) 

uc <- uc %>%dplyr::select(-Month) %>% dplyr::rename(Local.authority=National...Regional...LA...OAs )
pip <- pip %>%dplyr::select(-Month)%>% dplyr::rename(Local.authority= National...Regional...LA...OAs..V.)
housing_benefits_post2018 <- housing_benefits_post2018 %>%dplyr::select(-Month)%>% dplyr::rename(Local.authority= National...Regional...LA...OAs..I..II..VII..XVIII.)
housing_benefits_pre2018 <- housing_benefits_pre2018 %>%dplyr::select(-Month)%>% dplyr::rename(Local.authority= National...Regional...Admin.LA..VI..XVIII.)
carers_allowance <- carers_allowance %>%dplyr::select(-Quarter)%>% dplyr::rename(Local.authority= National...Regional...LA...OAs..II.)
jobseekers <- jobseekers %>%dplyr::select(-Quarter)%>% dplyr::rename(Local.authority= National...Regional...LA...OAs)
income_support <- income_support %>%dplyr::select(-Quarter)%>% dplyr::rename(Local.authority= National...Regional...LA...OAs)
incapacity <- incapacity %>%dplyr::select(-Quarter)%>% dplyr::rename(Local.authority= National...Regional...LA...OAs)
esa_post2018 <- esa_post2018 %>%dplyr::select(-Quarter)%>% dplyr::rename(Local.authority= National...Regional...LA...OAs)
esa_pre2018 <- esa_pre2018 %>%dplyr::select(-Quarter)%>% dplyr::rename(Local.authority= National...Regional...LA...OAs)
dla_post2018 <- dla_post2018 %>%dplyr::select(-Quarter)%>% dplyr::rename(Local.authority= National...Regional...LA...OAs)
dla_pre2018 <- dla_pre2018 %>%dplyr::select(-Quarter)%>% dplyr::rename(Local.authority= National...Regional...LA...OAs..II.)


benefits_no_TC <- merge(uc, pip, by=c("Local.authority", "year", "month", "quarter" ),all=T)
benefits_no_TC <- merge(benefits_no_TC, housing_benefits_post2018, by=c("Local.authority", "year", "month", "quarter" ),all=T)
benefits_no_TC <- merge(benefits_no_TC, housing_benefits_pre2018, by=c("Local.authority", "year", "month", "quarter" ),all=T)
benefits_no_TC <- merge(benefits_no_TC, carers_allowance, by=c("Local.authority", "year", "quarter"),all=T)
benefits_no_TC <- merge(benefits_no_TC, jobseekers, by=c("Local.authority", "year", "quarter"),all=T)
benefits_no_TC <- merge(benefits_no_TC, income_support, by=c("Local.authority", "year", "quarter"),all=T)
benefits_no_TC <- merge(benefits_no_TC, incapacity, by=c("Local.authority", "year", "quarter"),all=T)
benefits_no_TC <- merge(benefits_no_TC, esa_post2018, by=c("Local.authority", "year", "quarter"),all=T)
benefits_no_TC <- merge(benefits_no_TC, esa_pre2018, by=c("Local.authority", "year", "quarter"),all=T)
benefits_no_TC <- merge(benefits_no_TC, dla_post2018, by=c("Local.authority", "year", "quarter"),all=T)
benefits_no_TC <- merge(benefits_no_TC, dla_pre2018, by=c("Local.authority", "year", "quarter"),all=T)

benefits_no_TC <- benefits_no_TC[which(benefits_no_TC$year!="2099"),]
benefits_no_TC <- benefits_no_TC[which(benefits_no_TC$year!="2000"),]
benefits_no_TC <- benefits_no_TC[which(benefits_no_TC$year!="2001"),]
benefits_no_TC <- benefits_no_TC[which(benefits_no_TC$year!="2002"),]
benefits_no_TC <- benefits_no_TC[which(benefits_no_TC$year!="2003"),]
benefits_no_TC <- benefits_no_TC[which(benefits_no_TC$year!="2004"),]
benefits_no_TC <- benefits_no_TC[which(benefits_no_TC$year!="2005"),]
benefits_no_TC <- benefits_no_TC[which(benefits_no_TC$year!="2006"),]
benefits_no_TC <- benefits_no_TC[which(benefits_no_TC$year!="2007"),]
benefits_no_TC <- benefits_no_TC[which(benefits_no_TC$year!="2021"),]
benefits_no_TC <- benefits_no_TC[which(benefits_no_TC$year!="2022"),]
benefits_no_TC <- benefits_no_TC[which(benefits_no_TC$year!="2023"),]

#TCs#

# tcs09val <- read.csv("Data/tax_credits_val_09.csv",skip=3)[c(1,8)]
# tcs09n <- read.csv("Data/tax_credits_n_09.csv",skip=2)[c(1,8)]

tcs09n <- rbind(read_excel("Data/full_tax_credits_09.xls", sheet = 13, skip = 2)[c(1,8)],
                read_excel("Data/full_tax_credits_09.xls", sheet = 15, skip = 2)[c(1,8)],
                read_excel("Data/full_tax_credits_09.xls", sheet = 17, skip = 2)[c(1,8)],
                read_excel("Data/full_tax_credits_09.xls", sheet = 19, skip = 2)[c(1,8)],
                read_excel("Data/full_tax_credits_09.xls", sheet = 21, skip = 2)[c(1,8)],
                read_excel("Data/full_tax_credits_09.xls", sheet = 23, skip = 2)[c(1,8)],
                read_excel("Data/full_tax_credits_09.xls", sheet = 25, skip = 2)[c(1,8)],
                read_excel("Data/full_tax_credits_09.xls", sheet = 27, skip = 2)[c(1,8)])

tcs09val <- rbind(read_excel("Data/full_tax_credits_09.xls", sheet = 14, skip = 2)[c(1,8)],
                  read_excel("Data/full_tax_credits_09.xls", sheet = 16, skip = 2)[c(1,8)],
                  read_excel("Data/full_tax_credits_09.xls", sheet = 18, skip = 2)[c(1,8)],
                  read_excel("Data/full_tax_credits_09.xls", sheet = 20, skip = 2)[c(1,8)],
                  read_excel("Data/full_tax_credits_09.xls", sheet = 22, skip = 2)[c(1,8)],
                  read_excel("Data/full_tax_credits_09.xls", sheet = 24, skip = 2)[c(1,8)],
                  read_excel("Data/full_tax_credits_09.xls", sheet = 26, skip = 2)[c(1,8)],
                  read_excel("Data/full_tax_credits_09.xls", sheet = 28, skip = 2)[c(1,8)])

tcs10 <- read.csv("Data/tax_credits_10.csv", skip=2)[c(1,3:5, 13,26)]
tcs11 <- read.csv("Data/tax_credits_11.csv", skip=2)[c(1,3:5, 13,26)]
tcs12 <- read.csv("Data/tax_credits_12.csv", skip=2)[c(1,3:5, 12,25)]
tcs13 <- read.csv("Data/tax_credits_13.csv", skip=3)[c(2,4:6, 12,24)]
tcs14 <- read.csv("Data/tax_credits_14.csv", skip=3)[c(2,4:6, 12,24)]
tcs15 <- read.csv("Data/tax_credits_15.csv", skip=2)[c(1,3:5, 12,24)]
tcs16 <- read.csv("Data/tax_credits_16.csv", skip=2)[c(1,3:5, 12,24)]
tcs17 <- read.csv("Data/tax_credits_17.csv", skip=2)[c(1:4, 13,28)]
tcs18 <- read.csv("Data/tax_credits_18.csv",skip=0)[c(1,2,8,18)]
names(tcs18) <- tcs18[1,]
tcs19 <- read.csv("Data/tax_credits_19.csv", skip=3)[c(1:4, 13,28)]
tcs20 <- read.csv("Data/tax_credits_20.csv",skip=1)[c(1,2,8,18)]


tcs09n <-tcs09n[tcs09n$`Total in receipt (out-of-work and in-work families)`!=7.9,]
tcs09val <-tcs09val[tcs09val$...8!="3643",]
names(tcs09n) <- c("Local.authority", "tc_n_recipients_000s")
names(tcs09val) <- c("Local.authority", "tc_average_val")
tcs09n <- tcs09n[complete.cases(tcs09n$Local.authority),]
tcs09val <- tcs09val[complete.cases(tcs09val$Local.authority),]
tcs09 <- merge(unique(tcs09val), unique(tcs09n), by="Local.authority", all=T)

tcs10[tcs10==""] <- NA
tcs10 <- tcs10 %>%unite(col='Local.authority', c('Area.names', 'X', 'X.1'), sep='',  na.rm = TRUE)
names(tcs10) <- c("UTLA20CD","Local.authority", "tc_n_recipients_000s","tc_average_val")
tcs10 <- tcs10[which(tcs10$Local.authority!=""),]
tcs10 <- tcs10[complete.cases(tcs10$Local.authority),]
tcs10 <- tcs10[startsWith(tcs10$UTLA20CD, "E"),]

tcs11[tcs11==""] <- NA
tcs11 <- tcs11 %>%unite(col='Local.authority', c('Area.names', 'X', 'X.1'), sep='',  na.rm = TRUE)
names(tcs11) <- c("UTLA20CD","Local.authority", "tc_n_recipients_000s","tc_average_val")
tcs11 <- tcs11[which(tcs11$Local.authority!=""),]
tcs11 <- tcs11[complete.cases(tcs11$Local.authority),]
tcs11 <- tcs11[startsWith(tcs11$UTLA20CD, "E"),]

tcs12[tcs12==""] <- NA
tcs12 <- tcs12 %>%unite(col='Local.authority', c('Area.names', 'X', 'X.1'), sep='',  na.rm = TRUE)
names(tcs12) <- c("UTLA20CD","Local.authority", "tc_n_recipients_000s","tc_average_val")
tcs12 <- tcs12[which(tcs12$Local.authority!=""),]
tcs12 <- tcs12[complete.cases(tcs12$Local.authority),]
tcs12 <- tcs12[startsWith(tcs12$UTLA20CD, "E"),]


tcs13[tcs13==""] <- NA
tcs13 <- tcs13 %>%unite(col='Local.authority', c('Area.names', 'X.1', 'X.2'), sep='',  na.rm = TRUE)
names(tcs13) <- c("UTLA20CD","Local.authority", "tc_n_recipients_000s","tc_average_val")
tcs13 <- tcs13[which(tcs13$Local.authority!=""),]
tcs13 <- tcs13[complete.cases(tcs13$Local.authority),]
tcs13 <- tcs13[startsWith(tcs13$UTLA20CD, "E"),]


tcs14[tcs14==""] <- NA
tcs14 <- tcs14 %>%unite(col='Local.authority', c('Area.names', 'X.1', 'X.2'), sep='',  na.rm = TRUE)
names(tcs14) <- c("UTLA20CD","Local.authority", "tc_n_recipients_000s","tc_average_val")
tcs14 <- tcs14[which(tcs14$Local.authority!=""),]
tcs14 <- tcs14[complete.cases(tcs14$Local.authority),]
tcs14 <- tcs14[startsWith(tcs14$UTLA20CD, "E"),]


tcs15[tcs15==""] <- NA
tcs15 <- tcs15 %>%unite(col='Local.authority', c('Area.names', 'X', 'X.1'), sep='',  na.rm = TRUE)
names(tcs15) <- c("UTLA20CD","Local.authority", "tc_n_recipients_000s","tc_average_val")
tcs15 <- tcs15[which(tcs15$Local.authority!=""),]
tcs15 <- tcs15[complete.cases(tcs15$Local.authority),]
tcs15 <- tcs15[startsWith(tcs15$UTLA20CD, "E"),]


tcs16[tcs16==""] <- NA
tcs16 <- tcs16 %>%unite(col='Local.authority', c('Area.names', 'X', 'X.1'), sep='',  na.rm = TRUE)
names(tcs16) <- c("UTLA20CD","Local.authority", "tc_n_recipients_000s","tc_average_val")
tcs16 <- tcs16[which(tcs16$Local.authority!=""),]
tcs16 <- tcs16[complete.cases(tcs16$Local.authority),]
tcs16 <- tcs16[startsWith(tcs16$UTLA20CD, "E"),]

tcs17[tcs17==""] <- NA
tcs17 <- tcs17 %>%unite(col='Local.authority', c('X', 'X.1', 'X.2'), sep='',  na.rm = TRUE)
names(tcs17) <- c("Local.authority","UTLA20CD", "tc_n_recipients_000s","tc_average_val")
tcs17 <- tcs17[which(tcs17$Local.authority!=""),]
tcs17 <- tcs17[complete.cases(tcs17$Local.authority),]
tcs17 <- tcs17[startsWith(tcs17$UTLA20CD, "E"),]

tcs18[tcs18==""] <- NA
names(tcs18) <- c("Local.authority","UTLA20CD", "tc_n_recipients_000s","tc_average_val")
tcs18 <- tcs18[which(tcs18$Local.authority!=""),]
tcs18 <- tcs18[complete.cases(tcs18$Local.authority),]
tcs18 <- tcs18[startsWith(tcs18$UTLA20CD, "E"),]


tcs19[tcs19==""] <- NA
tcs19 <- tcs19 %>%unite(col='Local.authority', c('X', 'X.1', 'X.2'), sep='',  na.rm = TRUE)
names(tcs19) <- c("Local.authority","UTLA20CD", "tc_n_recipients_000s","tc_average_val")
tcs19 <- tcs19[which(tcs19$Local.authority!=""),]
tcs19 <- tcs19[complete.cases(tcs19$Local.authority),]
tcs19 <- tcs19[startsWith(tcs19$UTLA20CD, "E"),]
tcs19 <- tcs19[which(tcs19$Local.authority!=""),]
tcs19 <- tcs19[complete.cases(tcs19$Local.authority),]

tcs20[tcs20==""] <- NA
names(tcs20) <- c("Local.authority","UTLA20CD", "tc_n_recipients_000s","tc_average_val")
tcs20 <- tcs20[which(tcs20$Local.authority!=""),]
tcs20 <- tcs20[complete.cases(tcs20$Local.authority),]
tcs20 <- tcs20[startsWith(tcs20$UTLA20CD, "E"),]

tcs09$year <- 2009
tcs10$year <- 2010
tcs11$year <- 2011
tcs12$year <- 2012
tcs13$year <- 2013
tcs14$year <- 2014
tcs15$year <- 2015
tcs16$year <- 2016
tcs17$year <- 2017
tcs18$year <- 2018
tcs19$year <- 2019
tcs20$year <- 2020

tcs <- plyr::rbind.fill(tcs10,tcs11,tcs12,tcs13,tcs14,tcs15,tcs16,tcs17,tcs18,tcs19,tcs20)


tcs$Local.authority <-  gsub('&','and',tcs$Local.authority)
tcs09$Local.authority <-  gsub('&','and',tcs09$Local.authority)
benefits_no_TC$Local.authority <-  gsub('&','and',benefits_no_TC$Local.authority)

tcs$Local.authority <-  gsub('[[:punct:] ]+',' ',tcs$Local.authority)
tcs09$Local.authority <-  gsub('[[:punct:] ]+',' ',tcs09$Local.authority)
benefits_no_TC$Local.authority <-  gsub('[[:punct:] ]+',' ',benefits_no_TC$Local.authority)



#tcs$Local.authority <-  gsub('PCT','',tcs$Local.authority)
#benefits_no_TC$LTLA20NM <-  gsub('PCT','',benefits_no_TC$LTLA20NM)

tcs$Local.authority <-  toupper(tcs$Local.authority)
tcs09$Local.authority <-  toupper(tcs09$Local.authority)
benefits_no_TC$Local.authority <-  toupper(benefits_no_TC$Local.authority)

tcs$Local.authority <-  str_trim(tcs$Local.authority)
tcs09$Local.authority <-  str_trim(tcs09$Local.authority)
benefits_no_TC$Local.authority <-  str_trim(benefits_no_TC$Local.authority)




codes <- unique(tcs[c("Local.authority", "UTLA20CD")])

tcs09 <- merge(tcs09,codes, by="Local.authority", all.x=T)

tcs09[which(tcs09$Local.authority=="YORKSHIRE UA"),]$UTLA20CD <- "E06000011"
tcs09[which(tcs09$Local.authority=="WILTSHIRE"),]$UTLA20CD <- "E06000054"
#tcs09[which(tcs09$Local.authority=="WINDSOR AND"),]$UTLA20CD <- "E06000040"
tcs09[which(tcs09$Local.authority=="MAIDENHEAD UA"),]$UTLA20CD <- "E06000040"
tcs09[which(tcs09$Local.authority=="SOMERSET UA"),]$UTLA20CD <- "E10000027"
tcs09[which(tcs09$Local.authority=="SHROPSHIRE"),]$UTLA20CD <- "E06000051"
tcs09[which(tcs09$Local.authority=="LINCOLNSHIRE UA"),]$UTLA20CD <- "E10000019"
tcs09[which(tcs09$Local.authority=="HEREFORDSHIRE UA"),]$UTLA20CD <- "E06000019"
tcs09[which(tcs09$Local.authority=="GLOUCESTERSHIRE UA"),]$UTLA20CD <- "E10000013"
tcs09[which(tcs09$Local.authority=="DURHAM"),]$UTLA20CD <- "E06000047"
tcs09[which(tcs09$Local.authority=="CITY OF UA"),]$UTLA20CD <- "E06000010"
tcs09[which(tcs09$Local.authority=="CLEVELAND UA"),]$UTLA20CD <- "E06000003"
tcs09[which(tcs09$Local.authority=="DARWEN UA"),]$UTLA20CD <- "E06000008"
tcs09[which(tcs09$Local.authority=="BEDFORD"),]$UTLA20CD <- "E06000055"



tcs <- rbind(tcs, tcs09)

tcs$Local.authority <-  gsub(' UA','',tcs$Local.authority)
tcs$Local.authority <-  gsub(' BC','',tcs$Local.authority)
tcs$Local.authority <-  gsub(' DC','',tcs$Local.authority)
tcs$Local.authority <-  gsub(' MBC','',tcs$Local.authority)
benefits_no_TC$Local.authority <-  gsub(' UA','',benefits_no_TC$Local.authority)
benefits_no_TC$Local.authority <-  gsub(' BC','',benefits_no_TC$Local.authority)
benefits_no_TC$Local.authority <-  gsub(' DC','',benefits_no_TC$Local.authority)
benefits_no_TC$Local.authority <-  gsub(' MBC','',benefits_no_TC$Local.authority)

benefits_no_TC$Local.authority <- gsub("[[:digit:]]", "", benefits_no_TC$Local.authority)

codes <-unique(tcs[c("Local.authority", "UTLA20CD")])
benefits_no_TC <- merge(benefits_no_TC, codes, by="Local.authority", all.x=T)
check <-unique(benefits_no_TC[c("Local.authority", "UTLA20CD")])

benefits_no_TC$financial_year <- as.numeric(benefits_no_TC$year)

benefits_no_TC <- benefits_no_TC %>%mutate(financial_year = ifelse(as.numeric(benefits_no_TC$month)<5,financial_year, ifelse(as.numeric(benefits_no_TC$month)>4,financial_year+1, NA)))

tcs <- tcs %>%dplyr::rename(financial_year=year)

benefits <- merge(benefits_no_TC[complete.cases(benefits_no_TC$UTLA20CD),], tcs, by=c("UTLA20CD", "financial_year"),all=T)

rm(list=setdiff(ls(), c("df", "LA_allocation", "benefits", "benefits_no_TC", "tcs", "gdp", "LA_pops")))

for(var in c(7:33,35,36)){
  benefits[[var]] <-  gsub("[^[:digit:]. ]", "",  benefits[[var]])
  benefits[[var]] <- as.double(benefits[[var]])
}

benefits[is.na(benefits$Local.authority.x),]$Local.authority.x <- benefits[is.na(benefits$Local.authority.x),]$Local.authority.y


benefits_sum <- benefits %>% dplyr::filter(!is.na(UTLA20CD)&!is.na(financial_year)) %>% replace(is.na(.), 0) %>% 
  dplyr::select(-month)%>% 
  aggregate(.~UTLA20CD+financial_year+year+quarter+Local.authority.x+Local.authority.y,data=., mean)




# benefits_sum <- benefits$ %>% replace(is.na(.), 0) %>% 
#   dplyr::select(-month)%>% 
#   dplyr::group_by(UTLA20CD+financial_year+year+quarter+Local.authority.x+Local.authority.y)%>%
#   dplyr::summarise(`uc_Households on Universal Credit` = sum(`uc_Households on Universal Credit`),
#                    )



#  aggregate(.~UTLA20CD+financial_year+year+quarter+Local.authority.x+Local.authority.y,data=., mean)

benefits_sum$total_welfare <- ((benefits_sum$`uc_Households on Universal Credit`*3)*(benefits_sum$`uc_Mean of Payment Amount`-benefits_sum$`uc_Mean of Payment Amount`))+
  ((benefits_sum$`pip_PIP Cases with Entitlement`*3)*benefits_sum$`pip_Mean of Financial Award`)+
  ((benefits_sum$`housing_benefits_post2018_Housing Benefit Claimants`*3)*((benefits_sum$`housing_benefits_post2018_Mean of Weekly Award Amount`*4)-(benefits_sum$`housing_benefits_post2018_Mean of Weekly Spare Room Reduction Amount`*4)))+
  ((benefits_sum$`housing_benefits_pre2018_Housing Benefit Claimants`*3)*((benefits_sum$`housing_benefits_pre2018_Mean of Weekly Award Amount`*4)-(benefits_sum$`housing_benefits_pre2018_Mean of Weekly Spare Room Reduction Amount`*4)))+
  (benefits_sum$`carers_allowance_CA (In Payment)`*(benefits_sum$`carers_allowance_Mean of Weekly Award Amount`*12))+
  (benefits_sum$`jobseekers_Jobseekers Allowance`*(benefits_sum$`jobseekers_Mean of Weekly Award Amount (III)`*12))+
  (benefits_sum$`income_support_Income Support`*(benefits_sum$`income_support_Mean of Weekly Award Amount`*12))+
  (benefits_sum$`incapacity_Incapacity Benefit and Severe Disablement Allowance`*(benefits_sum$`incapacity_Mean of Weekly Award Amount (III)`*12))+
  (benefits_sum$`esa_post2018_Employment and Support Allowance Caseload - 2011 Geographies`*(benefits_sum$`esa_post2018_Mean of Weekly Award Amount (IV)`*12))+
  (benefits_sum$`esa_pre2018_Employment and Support Allowance Caseload`*(benefits_sum$`esa_pre2018_Mean of Weekly Award Amount`*12))+
  (benefits_sum$`dla_post2018_DLA (in payment) - 2011 Geographies`*(benefits_sum$`dla_post2018_Mean of Weekly Award Amount`*12))+
  (benefits_sum$`dla_pre2018_DLA (in payment)`*(benefits_sum$`dla_pre2018_Mean of Weekly Award Amount`*12))+
  ((benefits_sum$tc_n_recipients_000s*1000)*(benefits_sum$tc_average_val/4))

benefits_sum$year <- benefits_sum$financial_year

benefits_plot <- merge(LA_allocation[c("UTLA20CD", "gdp_deflation_multiplier", "year", "population")], benefits_sum[c("UTLA20CD", "total_welfare", "year", "quarter")], by=c("UTLA20CD", "year"), all=T)

benefits_plot$deflated_per_person_benefits <- (benefits_plot$total_welfare*benefits_plot$gdp_deflation_multiplier)/benefits_plot$population

benefits_plot$time <- benefits_plot$quarter+(4*(benefits_plot$year-2009))

benefits_plot <- benefits_plot[which(benefits_plot$time<49),]
benefits_plot <- unique(benefits_plot)
benefits_plot <- benefits_plot[which(benefits_plot$quarter!=0),]


benefits_sum_year <- benefits %>% dplyr::filter(!is.na(UTLA20CD)&!is.na(financial_year)) %>% replace(is.na(.), 0) %>% 
  dplyr::select(-month, -quarter, -financial_year,  -Local.authority.y)%>% 
  aggregate(.~UTLA20CD+Local.authority.x+year,data=., mean)




# benefits_sum <- benefits$ %>% replace(is.na(.), 0) %>% 
#   dplyr::select(-month)%>% 
#   dplyr::group_by(UTLA20CD+financial_year+year+quarter+Local.authority.x+Local.authority.y)%>%
#   dplyr::summarise(`uc_Households on Universal Credit` = sum(`uc_Households on Universal Credit`),
#                    )



#  aggregate(.~UTLA20CD+financial_year+year+quarter+Local.authority.x+Local.authority.y,data=., mean)

benefits_sum_year$total_welfare <- ((benefits_sum_year$`uc_Households on Universal Credit`*12)*(benefits_sum_year$`uc_Mean of Payment Amount`-benefits_sum_year$`uc_Mean of Payment Amount`))+
  ((benefits_sum_year$`pip_PIP Cases with Entitlement`*12)*benefits_sum_year$`pip_Mean of Financial Award`)+
  ((benefits_sum_year$`housing_benefits_post2018_Housing Benefit Claimants`*12)*((benefits_sum_year$`housing_benefits_post2018_Mean of Weekly Award Amount`*4)-(benefits_sum_year$`housing_benefits_post2018_Mean of Weekly Spare Room Reduction Amount`*4)))+
  ((benefits_sum_year$`housing_benefits_pre2018_Housing Benefit Claimants`*12)*((benefits_sum_year$`housing_benefits_pre2018_Mean of Weekly Award Amount`*4)-(benefits_sum_year$`housing_benefits_pre2018_Mean of Weekly Spare Room Reduction Amount`*4)))+
  (benefits_sum_year$`carers_allowance_CA (In Payment)`*(benefits_sum_year$`carers_allowance_Mean of Weekly Award Amount`*52))+
  (benefits_sum_year$`jobseekers_Jobseekers Allowance`*(benefits_sum_year$`jobseekers_Mean of Weekly Award Amount (III)`*52))+
  (benefits_sum_year$`income_support_Income Support`*(benefits_sum_year$`income_support_Mean of Weekly Award Amount`*52))+
  (benefits_sum_year$`incapacity_Incapacity Benefit and Severe Disablement Allowance`*(benefits_sum_year$`incapacity_Mean of Weekly Award Amount (III)`*52))+
  (benefits_sum_year$`esa_post2018_Employment and Support Allowance Caseload - 2011 Geographies`*(benefits_sum_year$`esa_post2018_Mean of Weekly Award Amount (IV)`*52))+
  (benefits_sum_year$`esa_pre2018_Employment and Support Allowance Caseload`*(benefits_sum_year$`esa_pre2018_Mean of Weekly Award Amount`*52))+
  (benefits_sum_year$`dla_post2018_DLA (in payment) - 2011 Geographies`*(benefits_sum_year$`dla_post2018_Mean of Weekly Award Amount`*52))+
  (benefits_sum_year$`dla_pre2018_DLA (in payment)`*(benefits_sum_year$`dla_pre2018_Mean of Weekly Award Amount`*52))+
  ((benefits_sum_year$tc_n_recipients_000s*1000)*(benefits_sum_year$tc_average_val))



LA_pops <- read.csv("Data/MYEB2_detailed_components_of_change_series_EW_(2020_geog20).csv")[c(1:25)]
LA_pops2 <- read.csv("Data/MYEB1_detailed_population_estimates_series_UK_(2018).csv")
#LA_pops <- merge(LA_pops, LA_upper_lookup, by.x="ladcode20", by.y="LTLA20CD", all=F)

LA_pops <- aggregate(.~laname20, data=LA_pops[-c( 1,3,4,5)], sum)
LA_pops2 <- aggregate(.~lad2018_name, data=LA_pops2[-c( 1,3,4,5)], sum)

LA_pops <- LA_pops%>%pivot_longer(cols = !laname20, names_to = "year", values_to = "population")%>%
  dplyr::rename(UTLA20NM = laname20)

LA_pops2 <- LA_pops2%>%pivot_longer(cols = !lad2018_name, names_to = "year", values_to = "population")%>%
  dplyr::rename(UTLA20NM = lad2018_name)

LA_pops$year <- gsub("population_","", LA_pops$year)
LA_pops2$year <- gsub("population_","", LA_pops2$year)


LA_pops <- rbind(LA_pops2,LA_pops )


benefits_sum_year <- benefits_sum_year %>% dplyr::rename(UTLA20NM = Local.authority.x)

LA_pops$UTLA20NM <-  gsub('&','and',LA_pops$UTLA20NM)
benefits_sum_year$UTLA20NM <-  gsub('&','and',benefits_sum_year$UTLA20NM)

LA_pops$UTLA20NM <-  gsub('[[:punct:] ]+',' ',LA_pops$UTLA20NM)
benefits_sum_year$UTLA20NM <-  gsub('[[:punct:] ]+',' ',benefits_sum_year$UTLA20NM)

LA_pops$UTLA20NM <-  toupper(LA_pops$UTLA20NM)
benefits_sum_year$UTLA20NM <-  toupper(benefits_sum_year$UTLA20NM)

LA_pops$UTLA20NM <-  str_trim(LA_pops$UTLA20NM)
benefits_sum_year$UTLA20NM <-  str_trim(benefits_sum_year$UTLA20NM)


LA_pops <- LA_pops %>% dplyr::distinct(UTLA20NM,year, .keep_all = T)

benefits_sum_year[benefits_sum_year$UTLA20NM=="SHEPWAY",]$UTLA20NM <- "FOLKESTONE AND HYTHE"

benefits_sum_year <- merge(benefits_sum_year, LA_pops, by=c("UTLA20NM", "year"),all.x=T)

benefits_sum_year <- benefits_sum_year[benefits_sum_year$year!=0,]

benefits_sum_year <- merge(benefits_sum_year, gdp, by="year", all.x=T)

benefits_sum_year$deflated_per_person_benefits <- (benefits_sum_year$total_welfare*benefits_sum_year$gdp_deflation_multiplier)/benefits_sum_year$population


#LAdf <- merge(LA_allocation, benefits_sum_year[c("UTLA20CD", "total_welfare", "year")], by=c("UTLA20CD", "year"), all=T)


####Lookup#####

localauthority <-read_csv("Data/LSOAtoCCGtoLAD19.csv")

localauthority <- localauthority%>%dplyr::select(CCG19NM, LAD19CD)%>%
  dplyr::rename(CCG_Name = CCG19NM,
                UTLA20CD = LAD19CD)

localauthority <- unique(localauthority)

benefits_sum_year <- merge(benefits_sum_year[c("UTLA20CD", "deflated_per_person_benefits", "year")], localauthority, by= "UTLA20CD",all=T)

benefits_sum_year <- benefits_sum_year %>%dplyr::select(CCG_Name, year,  deflated_per_person_benefits)

benefits_sum_year$CCG_Name <-  gsub('&','and',benefits_sum_year$CCG_Name)
benefits_sum_year$CCG_Name <-  gsub('[[:punct:] ]+',' ',benefits_sum_year$CCG_Name)
benefits_sum_year$CCG_Name <-  gsub('NHS ','',benefits_sum_year$CCG_Name)
benefits_sum_year$CCG_Name <-  gsub('CCG','',benefits_sum_year$CCG_Name)
benefits_sum_year$CCG_Name <-  str_trim(benefits_sum_year$CCG_Name)
benefits_sum_year$CCG_Name <-  toupper(benefits_sum_year$CCG_Name)

benefits_sum_year <- aggregate(.~CCG_Name+year,data=benefits_sum_year, mean)

df <- merge(df, benefits_sum_year, by=c("CCG_Name", "year"),all=T)


localauthority <-read_csv("Data/LSOAtoCCGtoLAD19.csv")


localauthority <- localauthority%>%dplyr::select(CCG19NM, LAD19CD)%>%
  dplyr::rename(CCG_Name = CCG19NM,
                UTLA20CD = LAD19CD)

localauthority <- unique(localauthority)

LA_upper_lookup <- read.csv("Data/Lower_Tier_Local_Authority_to_Upper_Tier_Local_Authority_(December_2020)_Lookup_in_England_and_Wales.csv")

localauthority <- merge(localauthority,LA_upper_lookup , by.x="UTLA20CD", by.y="LTLA20CD", all.x=T )

localauthority$UTLA20NM <-  gsub('&','and',localauthority$UTLA20NM)

localauthority$UTLA20NM <-  gsub('[[:punct:] ]+',' ',localauthority$UTLA20NM)
localauthority$UTLA20NM <-  gsub(' UA','',localauthority$UTLA20NM)
localauthority$UTLA20NM <-  gsub(' BC','',localauthority$UTLA20NM)
localauthority$UTLA20NM <-  gsub(' CC','',localauthority$UTLA20NM)
localauthority$UTLA20NM <-  gsub(' DC','',localauthority$UTLA20NM)
localauthority$UTLA20NM <-  gsub(' MBC','',localauthority$UTLA20NM)

localauthority$UTLA20NM <-  toupper(localauthority$UTLA20NM)

localauthority$UTLA20NM <-  str_trim(localauthority$UTLA20NM)

LA_allocation <- merge(LA_allocation, localauthority, by= "UTLA20NM", all.x=T)

LA_allocation$CCG_Name[LA_allocation$UTLA20NM=="BUCKINGHAMSHIRE"] <- "BUCKINGHAMSHIRE"

LA_allocation <- LA_allocation %>%dplyr::select(CCG_Name, year,  deflated_per_person_allocation_la)

LA_allocation$CCG_Name <-  gsub('&','and',LA_allocation$CCG_Name)
LA_allocation$CCG_Name <-  gsub('[[:punct:] ]+',' ',LA_allocation$CCG_Name)
LA_allocation$CCG_Name <-  gsub('NHS ','',LA_allocation$CCG_Name)
LA_allocation$CCG_Name <-  gsub('CCG','',LA_allocation$CCG_Name)
LA_allocation$CCG_Name <-  str_trim(LA_allocation$CCG_Name)
LA_allocation$CCG_Name <-  toupper(LA_allocation$CCG_Name)

LA_allocation <- aggregate(.~CCG_Name+year,data=LA_allocation, mean)

df <- merge(df, LA_allocation, by=c("CCG_Name", "year"),all=T)

df$deflated_per_person_benefits <- df$deflated_per_person_benefits/1000

####lags####

lagged <- df[c("CCG_Name", "Preventatable_Mortality_Rate", "deflated_per_person_allocation", "deflated_per_person_allocation_la", "deflated_per_person_benefits", "Private_Sector_Procurement_Spend", "year")]
lagged$year <- as.character(as.double(as.character(lagged$year))+1)

names(lagged)[names(lagged)=="Private_Sector_Procurement_Spend"] <- "lagged_profit"
names(lagged)[names(lagged)=="deflated_per_person_allocation"] <- "lagged_ccg"
names(lagged)[names(lagged)=="deflated_per_person_allocation_la"] <- "lagged_la"
names(lagged)[names(lagged)=="deflated_per_person_benefits"] <- "lagged_bens"
names(lagged)[names(lagged)=="Preventatable_Mortality_Rate"] <- "lagged_prevent"

df <- merge(df, lagged, by=c("CCG_Name", "year"),all.x=T)
####Mortality full####

avoidable_mortality1 <- read_csv("Data/avoidablemortality2018.csv")
treatable_mortality1 <- read_csv("Data/treatablemortality2018.csv")
preventable_mortality1 <- read_csv("Data/preventablemortality2018.csv")

avoidable_mortality <- read_csv("Data/avoidable_full.csv") 
treatable_mortality <- read_csv("Data/treatable_full.csv") 
preventable_mortality <- read_csv("Data/preventable_full.csv")

avoidable_mortality <- avoidable_mortality[which(avoidable_mortality$Sex=="Persons"),]
treatable_mortality <- treatable_mortality[which(treatable_mortality$Sex=="Persons"),]
preventable_mortality <- preventable_mortality[which(preventable_mortality$Sex=="Persons"),]

avoidable_mortality1 <- avoidable_mortality1[which(avoidable_mortality1$Sex=="Persons"),]
treatable_mortality1 <- treatable_mortality1[which(treatable_mortality1$Sex=="Persons"),]
preventable_mortality1 <- preventable_mortality1[which(preventable_mortality1$Sex=="Persons"),]


avoidable_mortality <- avoidable_mortality %>% dplyr::select(-Sex, -`Area Code`)
treatable_mortality <- treatable_mortality %>% dplyr::select(-Sex, -`Area Code`)
preventable_mortality <- preventable_mortality %>% dplyr::select(-Sex, -`Area Code`)

avoidable_mortality1 <- avoidable_mortality1 %>% dplyr::select(-Sex, -`Area Code`)
treatable_mortality1 <- treatable_mortality1 %>% dplyr::select(-Sex, -`Area Code`)
preventable_mortality1 <- preventable_mortality1 %>% dplyr::select(-Sex, -`Area Code`)


avoidable_mortality <- avoidable_mortality %>% pivot_longer(cols=!`Area Name`, names_to = "year", values_to = "avoidable_mortality_rate")
treatable_mortality <- treatable_mortality %>% pivot_longer(cols=!`Area Name`, names_to = "year", values_to = "treatable_mortality_rate")
preventable_mortality <- preventable_mortality %>% pivot_longer(cols=!`Area Name`, names_to = "year", values_to = "preventable_mortality_rate")

avoidable_mortality1 <- avoidable_mortality1 %>% pivot_longer(cols=!`Area Name`, names_to = "year", values_to = "avoidable_mortality_rate")
treatable_mortality1 <- treatable_mortality1 %>% pivot_longer(cols=!`Area Name`, names_to = "year", values_to = "treatable_mortality_rate")
preventable_mortality1 <- preventable_mortality1 %>% pivot_longer(cols=!`Area Name`, names_to = "year", values_to = "preventable_mortality_rate")


names(avoidable_mortality)[names(avoidable_mortality)=="Area Name"] <- "CCG_Name"
names(treatable_mortality)[names(treatable_mortality)=="Area Name"] <- "CCG_Name"
names(preventable_mortality)[names(preventable_mortality)=="Area Name"] <- "CCG_Name"

names(avoidable_mortality1)[names(avoidable_mortality1)=="Area Name"] <- "CCG_Name"
names(treatable_mortality1)[names(treatable_mortality1)=="Area Name"] <- "CCG_Name"
names(preventable_mortality1)[names(preventable_mortality1)=="Area Name"] <- "CCG_Name"



avoidable_mortality <- dplyr::filter(avoidable_mortality, grepl("NHS",CCG_Name))
treatable_mortality <- dplyr::filter(treatable_mortality, grepl("NHS",CCG_Name))
preventable_mortality <- dplyr::filter(preventable_mortality, grepl("NHS",CCG_Name))

avoidable_mortality$CCG_Name <-  gsub('NHS ','',avoidable_mortality$CCG_Name)
treatable_mortality$CCG_Name <-  gsub('NHS ','',treatable_mortality$CCG_Name)
preventable_mortality$CCG_Name <-  gsub('NHS ','',preventable_mortality$CCG_Name)

avoidable_mortality$CCG_Name <-  gsub('CCG','',avoidable_mortality$CCG_Name)
treatable_mortality$CCG_Name <-  gsub('CCG','',treatable_mortality$CCG_Name)
preventable_mortality$CCG_Name <-  gsub('CCG','',preventable_mortality$CCG_Name)

avoidable_mortality$CCG_Name <-  gsub('&','and',avoidable_mortality$CCG_Name)
treatable_mortality$CCG_Name <-  gsub('&','and',treatable_mortality$CCG_Name)
preventable_mortality$CCG_Name <-  gsub('&','and',preventable_mortality$CCG_Name)

avoidable_mortality$CCG_Name <-  gsub('[[:punct:] ]+',' ',avoidable_mortality$CCG_Name)
treatable_mortality$CCG_Name <-  gsub('[[:punct:] ]+',' ',treatable_mortality$CCG_Name)
preventable_mortality$CCG_Name <-  gsub('[[:punct:] ]+',' ',preventable_mortality$CCG_Name)

avoidable_mortality$CCG_Name <-  toupper(avoidable_mortality$CCG_Name)
treatable_mortality$CCG_Name <-  toupper(treatable_mortality$CCG_Name)
preventable_mortality$CCG_Name <-  toupper(preventable_mortality$CCG_Name)

avoidable_mortality$CCG_Name <-  str_trim(avoidable_mortality$CCG_Name)
treatable_mortality$CCG_Name <-  str_trim(treatable_mortality$CCG_Name)
preventable_mortality$CCG_Name <-  str_trim(preventable_mortality$CCG_Name)

avoidable_mortality$year <- as.numeric(avoidable_mortality$year)
treatable_mortality$year <- as.numeric(treatable_mortality$year)
preventable_mortality$year <- as.numeric(preventable_mortality$year)

avoidable_mortality1 <- dplyr::filter(avoidable_mortality1, grepl("NHS",CCG_Name))
treatable_mortality1 <- dplyr::filter(treatable_mortality1, grepl("NHS",CCG_Name))
preventable_mortality1 <- dplyr::filter(preventable_mortality1, grepl("NHS",CCG_Name))

avoidable_mortality1$CCG_Name <-  gsub('NHS ','',avoidable_mortality1$CCG_Name)
treatable_mortality1$CCG_Name <-  gsub('NHS ','',treatable_mortality1$CCG_Name)
preventable_mortality1$CCG_Name <-  gsub('NHS ','',preventable_mortality1$CCG_Name)

avoidable_mortality1$CCG_Name <-  gsub('CCG','',avoidable_mortality1$CCG_Name)
treatable_mortality1$CCG_Name <-  gsub('CCG','',treatable_mortality1$CCG_Name)
preventable_mortality1$CCG_Name <-  gsub('CCG','',preventable_mortality1$CCG_Name)

avoidable_mortality1$CCG_Name <-  gsub('&','and',avoidable_mortality1$CCG_Name)
treatable_mortality1$CCG_Name <-  gsub('&','and',treatable_mortality1$CCG_Name)
preventable_mortality1$CCG_Name <-  gsub('&','and',preventable_mortality1$CCG_Name)

avoidable_mortality1$CCG_Name <-  gsub('[[:punct:] ]+',' ',avoidable_mortality1$CCG_Name)
treatable_mortality1$CCG_Name <-  gsub('[[:punct:] ]+',' ',treatable_mortality1$CCG_Name)
preventable_mortality1$CCG_Name <-  gsub('[[:punct:] ]+',' ',preventable_mortality1$CCG_Name)

avoidable_mortality1$CCG_Name <-  toupper(avoidable_mortality1$CCG_Name)
treatable_mortality1$CCG_Name <-  toupper(treatable_mortality1$CCG_Name)
preventable_mortality1$CCG_Name <-  toupper(preventable_mortality1$CCG_Name)

avoidable_mortality1$CCG_Name <-  str_trim(avoidable_mortality1$CCG_Name)
treatable_mortality1$CCG_Name <-  str_trim(treatable_mortality1$CCG_Name)
preventable_mortality1$CCG_Name <-  str_trim(preventable_mortality1$CCG_Name)

avoidable_mortality1$year <- as.numeric(avoidable_mortality1$year)
treatable_mortality1$year <- as.numeric(treatable_mortality1$year)
preventable_mortality1$year <- as.numeric(preventable_mortality1$year)

avoidable_mortality <- unique(rbind(avoidable_mortality1, avoidable_mortality))
preventable_mortality <- unique(rbind(preventable_mortality1, preventable_mortality))
treatable_mortality <- unique(rbind(treatable_mortality1, treatable_mortality))

avoidable_mortality <- avoidable_mortality %>% dplyr::distinct(CCG_Name, year, .keep_all=T)
treatable_mortality <- treatable_mortality %>% dplyr::distinct(CCG_Name, year, .keep_all=T)
preventable_mortality <- preventable_mortality %>% dplyr::distinct(CCG_Name, year, .keep_all=T)


df <- merge(df, avoidable_mortality, by=c("CCG_Name",  "year"), all.x=T)
df <- merge(df, treatable_mortality, by=c("CCG_Name", "year"), all.x=T)
df <- merge(df, preventable_mortality, by=c( "CCG_Name", "year"), all.x=T)

####Balance####

file_vector <- list.files(path = "Data/extra_accounts")
file_vector %>% head()
pdf_list <- file_vector[grepl(".pdf",file_vector)]


corpus_raw_balance <- data.frame("CCG_Code" = c(),"text" = c())
for (i in 1:46){
  print(i)
  er <-  pdf_text(paste("Data/extra_accounts/", pdf_list[i],sep = ""))
  er <-  er[grepl("taxpayer", ignore.case=TRUE, er)]
  readr::read_lines(er)-> document_text
  data.frame("CCG_Code" = gsub(x =pdf_list[i],pattern = ".pdf", replacement = ""), 
             "text" = document_text, stringsAsFactors = FALSE)%>% 
    filter(grepl("balance at",text, ignore.case=TRUE))  -> document_balance
  colnames(document_balance) <- c("CCG_Code", "text")
  corpus_raw_balance <- rbind(corpus_raw_balance,document_balance) 
}


corpus_raw_balance2 <- data.frame("CCG_Code" = c(),"text" = c())
for (i in 48:92){
  print(i)
  er <-  pdf_text(paste("Data/extra_accounts/", pdf_list[i],sep = ""))
  er <-  er[grepl("taxpayer", ignore.case=TRUE, er)]
  readr::read_lines(er)-> document_text
  data.frame("CCG_Code" = gsub(x =pdf_list[i],pattern = ".pdf", replacement = ""), 
             "text" = document_text, stringsAsFactors = FALSE)%>% 
    filter(grepl("balance at",text, ignore.case=TRUE))  -> document_balance
  colnames(document_balance) <- c("CCG_Code", "text")
  corpus_raw_balance2 <- rbind(corpus_raw_balance2,document_balance) 
}


corpus_raw_balance3 <- data.frame("CCG_Code" = c(),"text" = c())
for (i in 95:154){
  print(i)
  er <-  pdf_text(paste("Data/extra_accounts/", pdf_list[i],sep = ""))
  er <-  er[grepl("taxpayer", ignore.case=TRUE, er)]
  readr::read_lines(er)-> document_text
  data.frame("CCG_Code" = gsub(x =pdf_list[i],pattern = ".pdf", replacement = ""), 
             "text" = document_text, stringsAsFactors = FALSE)%>% 
    filter(grepl("balance at",text, ignore.case=TRUE))  -> document_balance
  colnames(document_balance) <- c("CCG_Code", "text")
  corpus_raw_balance3 <- rbind(corpus_raw_balance3,document_balance) 
}

corpus_raw_balance4 <- data.frame("CCG_Code" = c(),"text" = c())
for (i in 156:200){
  print(i)
  er <-  pdf_text(paste("Data/extra_accounts/", pdf_list[i],sep = ""))
  er <-  er[grepl("taxpayer", ignore.case=TRUE, er)]
  readr::read_lines(er)-> document_text
  data.frame("CCG_Code" = gsub(x =pdf_list[i],pattern = ".pdf", replacement = ""), 
             "text" = document_text, stringsAsFactors = FALSE)%>% 
    filter(grepl("balance at",text, ignore.case=TRUE))  -> document_balance
  colnames(document_balance) <- c("CCG_Code", "text")
  corpus_raw_balance4 <- rbind(corpus_raw_balance4,document_balance) 
}

corpus_raw_balance5 <- data.frame("CCG_Code" = c(),"text" = c())
for (i in 202:219){
  print(i)
  er <-  pdf_text(paste("Data/extra_accounts/", pdf_list[i],sep = ""))
  er <-  er[grepl("taxpayer", ignore.case=TRUE, er)]
  readr::read_lines(er)-> document_text
  data.frame("CCG_Code" = gsub(x =pdf_list[i],pattern = ".pdf", replacement = ""), 
             "text" = document_text, stringsAsFactors = FALSE)%>% 
    filter(grepl("balance at",text, ignore.case=TRUE))  -> document_balance
  colnames(document_balance) <- c("CCG_Code", "text")
  corpus_raw_balance5 <- rbind(corpus_raw_balance5,document_balance) 
}

corpus_raw_balance6 <- data.frame("CCG_Code" = c(),"text" = c())
for (i in 221:227){
  print(i)
  er <-  pdf_text(paste("Data/extra_accounts/", pdf_list[i],sep = ""))
  er <-  er[grepl("taxpayer", ignore.case=TRUE, er)]
  readr::read_lines(er)-> document_text
  data.frame("CCG_Code" = gsub(x =pdf_list[i],pattern = ".pdf", replacement = ""), 
             "text" = document_text, stringsAsFactors = FALSE)%>% 
    filter(grepl("balance at",text, ignore.case=TRUE))  -> document_balance
  colnames(document_balance) <- c("CCG_Code", "text")
  corpus_raw_balance6 <- rbind(corpus_raw_balance6,document_balance) 
}

corpus_raw_balance7 <- data.frame("CCG_Code" = c(),"text" = c())
for (i in 229:263){
  print(i)
  er <-  pdf_text(paste("Data/extra_accounts/", pdf_list[i],sep = ""))
  er <-  er[grepl("taxpayer", ignore.case=TRUE, er)]
  readr::read_lines(er)-> document_text
  data.frame("CCG_Code" = gsub(x =pdf_list[i],pattern = ".pdf", replacement = ""), 
             "text" = document_text, stringsAsFactors = FALSE)%>% 
    filter(grepl("balance at",text, ignore.case=TRUE))  -> document_balance
  colnames(document_balance) <- c("CCG_Code", "text")
  corpus_raw_balance7 <- rbind(corpus_raw_balance7,document_balance) 
}

corpus_raw_balance8 <- data.frame("CCG_Code" = c(),"text" = c())
for (i in 265:330){
  print(i)
  er <-  pdf_text(paste("Data/extra_accounts/", pdf_list[i],sep = ""))
  er <-  er[grepl("taxpayer", ignore.case=TRUE, er)]
  readr::read_lines(er)-> document_text
  data.frame("CCG_Code" = gsub(x =pdf_list[i],pattern = ".pdf", replacement = ""), 
             "text" = document_text, stringsAsFactors = FALSE)%>% 
    filter(grepl("balance at",text, ignore.case=TRUE))  -> document_balance
  colnames(document_balance) <- c("CCG_Code", "text")
  corpus_raw_balance8 <- rbind(corpus_raw_balance8,document_balance) 
}


corpus_raw_balance <- rbind(corpus_raw_balance, corpus_raw_balance2, corpus_raw_balance3, corpus_raw_balance4,
                            corpus_raw_balance5, corpus_raw_balance6, corpus_raw_balance7, corpus_raw_balance8)



balance<- corpus_raw_balance %>% tidyr::separate(., col = "text" , into = c("text", "month", "year"), sep = "(?<=[a-zA-Z*])\\s*(?=[0-9])")%>%
  tidyr::separate(., col="year", into = c("year", "balance"), sep="\\s+")%>%
  tidyr::separate(.,col="month", into = c("month","value2"), sep="\\s*\\(\\s*")%>%
  dplyr::mutate(value2 = paste("-", value2, sep=""))%>%
  dplyr::mutate(balance = ifelse(is.na(balance), value2, balance),
                balance = gsub("\\(", "-", balance),
                balance = gsub("\\)", "", balance),
                balance = gsub(",","", balance),
                balance = as.numeric(sub("\\s.*", "", balance)))

yearfix <- balance[is.na(balance$year),]

yearfix$year <- yearfix$CCG_Code
yearfix$year <- gsub("[^0-9.]", "", yearfix$year)
yearfix$year <- paste("20", yearfix$year, sep = "")

yearfix <- yearfix%>% dplyr::mutate(year = ifelse(grepl("april", month, ignore.case=T), as.numeric(year)-1, year))


balance <- rbind(balance[!is.na(balance$year),], yearfix)

balance <- balance %>% filter(!grepl(",", year))%>%
  dplyr::mutate(dept = paste("NHS_", gsub("\\d", "", CCG_Code), "_CCG", sep=""))%>%
  dplyr::select(dept,year, balance)%>%
  dplyr::filter(!(dept=="NHS_BNSGSG_CCG" & year==2018))

balance[balance$dept=="NHS_BNSGSG_CCG"&balance$year=="18",]$year <- "2018"

balance14 <- read.csv("Data/balance14.csv")
balance15 <- read.csv("Data/balance15.csv")
balance16 <- read.csv("Data/balance16.csv")
balance17 <- read.csv("Data/balance17.csv")

balancepre <- rbind(balance14, balance15, balance16, balance17)%>%
  dplyr::rename(CCG_Name = ccg19nm,
                balance = Balance)%>%
  dplyr::mutate(balance =gsub(",","", balance),
  )

balancepre$CCG_Name <-  gsub('NHS ','',balancepre$CCG_Name)
balancepre$CCG_Name <-  gsub('CCG','',balancepre$CCG_Name)
balancepre$CCG_Name <-  gsub('&','and',balancepre$CCG_Name)
balancepre$CCG_Name <-  gsub('[[:punct:] ]+',' ',balancepre$CCG_Name)
balancepre$CCG_Name <-  toupper(balancepre$CCG_Name)
balancepre$CCG_Name <-  str_trim(balancepre$CCG_Name)
balancepre$year <- as.numeric(balancepre$year)


balancepre[balancepre$CCG_Name=="SOUTH EAST STAFFS AND SEISDON AND PENINSULAR",]$CCG_Name <- "SOUTH EAST STAFFORDSHIRE AND SEISDON PENINSULAR"
balancepre[balancepre$CCG_Name=="GATESHEAD",]$CCG_Name <- "NEWCASTLE GATESHEAD"
balancepre[balancepre$CCG_Name=="NORTH EAST WEST DEVON",]$CCG_Name <- "NORTH EASTERN AND WESTERN DEVON"
balancepre[balancepre$CCG_Name=="CASTLE POINT RAYLEIGH AND ROCHFORD",]$CCG_Name <- "CASTLE POINT AND ROCHFORD"




#check <- balancepre %>%dplyr::distinct(CCG_Name, year)
#check <- merge(df[c("CCG_Name", "year", "avoidable_mortality_rate")], balancepre, by=c("CCG_Name", "year"), all=T)
df <- merge(df, balancepre, by=c("CCG_Name", "year"), all=T)
#check2 <- merge(df[c("dept", "year", "avoidable_mortality_rate")], balance, by=c("dept", "year"), all=T)
balance <- balance%>%filter(year!=2012)%>%dplyr::distinct(dept, year, .keep_all = T)
df <- merge(df, balance, by=c("dept", "year"), all=T)

df <- df%>% dplyr::mutate(balance = ifelse(!is.na(balance.x), balance.x, balance.y))%>%
  dplyr::select(-balance.x, -balance.y, -Social_Care_Procurement_Spend,- Hospital_Services_Procurement_Spend,
                -GP_Services_Procurement_Spend, -Other_Health_Services_Procurement_Spend,
                -Building_Maintenance_Procurement_Spend, -Specialist_Services_Procurement_Spend, -NotAssigned_Procurement_Spend,
                -Foundational_Services_Procurement_Spend, -Professional_Services_Spend, -Dental_Services_Procurement_Spend,
                -Medical_Nursing_Homes_Procurement_Spend, -nogpPrivate_Sector_Procurement_Spend, -NOGPTotal_Procurement_Spend,
                -nogpHealth_Services_Procurement_Spend,
                -Food_Procurement_Spend, -Transport_Procurement_Spend, -Management_Procurement_Spend, -Total_Management_Procurement_Spend,
                -over70, -over75, -over80)

####Treatments####

dfin <-  read.csv("Data/Commissioner_admitted.csv")
dfout <- read.csv("Data/Commissioner_non_admitted.csv")

#dfin <- dfin%>%dplyr::select(-X, -nhs)
#dfout <- dfout%>%dplyr::select(-X, -nhs)

names(dfout)[names(dfout)=="Total.All"] <- "Total.nonadmitted"
names(dfin)[names(dfin)=="Total.All"] <- "Total.admitted"

names(dfout)[names(dfout)=="within18"] <- "within18.nonadmitted"
names(dfin)[names(dfin)=="within18"] <- "within18.admitted"

commissioner_data <- merge(dfin, dfout, by=c("month", "year","Provider.Org.Code", "Provider.Org.Name","Commissioner.Org.Code", "Commissioner.Org.Name", "Sector", "Treatment.Function.Name", "Treatment.Function.Code"), all=T)
commissioner_data[is.na(commissioner_data)] <- 0

commissioner_data$Total.All <- as.numeric(commissioner_data$Total.nonadmitted)+as.numeric(commissioner_data$Total.admitted)

commissioner_data$within18_all <- (((as.numeric(commissioner_data$within18.admitted)*as.numeric(commissioner_data$Total.admitted))+
                                      (as.numeric(commissioner_data$within18.nonadmitted)*as.numeric(commissioner_data$Total.nonadmitted)))/
                                     (as.numeric(commissioner_data$Total.All)))*100





commissioner_data <- commissioner_data%>%mutate(monthno = ifelse(commissioner_data$month=="january", 1,
                                                                 ifelse(commissioner_data$month=="february", 2,
                                                                        ifelse(commissioner_data$month=="march", 3,
                                                                               ifelse(commissioner_data$month=="april", 4,
                                                                                      ifelse(commissioner_data$month=="may", 5,
                                                                                             ifelse(commissioner_data$month=="june", 6,
                                                                                                    ifelse(commissioner_data$month=="july", 7,
                                                                                                           ifelse(commissioner_data$month=="august", 8,
                                                                                                                  ifelse(commissioner_data$month=="september", 9,
                                                                                                                         ifelse(commissioner_data$month=="october", 10,
                                                                                                                                ifelse(commissioner_data$month=="november", 11,
                                                                                                                                       ifelse(commissioner_data$month=="december", 12,NA)))))))))))))

commissioner_data$yearno <- as.double(commissioner_data$year)-2011

commissioner_data$time <- commissioner_data$monthno+(12*commissioner_data$yearno)-3

commissioner_data$Sector[commissioner_data$Provider.Org.Name=="TETBURY HOSPITAL TRUST LTD"] <- "Independent"
commissioner_data$Sector[commissioner_data$Provider.Org.Name=="FOSCOTE COURT (BANBURY) TRUST LTD"] <- "Independent"

commissioner_data <- commissioner_data %>%   filter(str_detect(Commissioner.Org.Name, "CCG"))

commissioner_data <- aggregate(.~Commissioner.Org.Name+Sector+year, data=commissioner_data[c("Commissioner.Org.Name","Sector", "year", "Total.All")], sum)

commissioner_data <- commissioner_data %>%  pivot_wider(names_from = Sector, values_from = c(Total.All))%>%
  dplyr::mutate(outsourcing_treats = (Independent/(NHS+Independent))*100,
                total_treats = NHS+Independent)%>%
  dplyr::rename(CCG_Name = Commissioner.Org.Name)

commissioner_data$CCG_Name <-  gsub('NHS ','',commissioner_data$CCG_Name)
commissioner_data$CCG_Name <-  gsub('CCG','',commissioner_data$CCG_Name)
commissioner_data$CCG_Name <-  gsub('&','and',commissioner_data$CCG_Name)
commissioner_data$CCG_Name <-  gsub('[[:punct:] ]+',' ',commissioner_data$CCG_Name)
commissioner_data$CCG_Name <-  toupper(commissioner_data$CCG_Name)
commissioner_data$CCG_Name <-  str_trim(commissioner_data$CCG_Name)
commissioner_data$year <- as.numeric(commissioner_data$year)

commissioner_data <- commissioner_data%>%dplyr::distinct(CCG_Name, year, .keep_all = T)

check3 <- merge(df[c("CCG_Name", "year", "avoidable_mortality_rate")], commissioner_data, by=c("CCG_Name", "year"), all=T)
df <- merge(df, commissioner_data, by=c("CCG_Name", "year"), all=T)

rm(list=setdiff(ls(), c("df", "benefits_plot","LA_allocation")))

#write.csv(df, "C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/austerity_and_privatisation/Data/main_data.csv")
#write.csv(benefits_plot, "C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/austerity_and_privatisation/Data/socialsecurity_data.csv")
#write.csv(LA_allocation, "C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/austerity_and_privatisation/Data/LA_data.csv")
####ANALYSIS####
