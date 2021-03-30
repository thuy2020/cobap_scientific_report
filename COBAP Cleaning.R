## clear global environment: rm(list = ls())

library(readr)
library(dplyr)
library(readxl)
library(tidyr)
library(rio)

## personal - just setting my working directory 
getwd()
setwd("/Users/thuyn/Desktop/cobap_scientific_report/data")

## importing data
covidvar <- read_excel("Worldometer_2020.06.13_download_m.xlsx", 
                                              skip = 2)
wgidataset <- read_excel("wgidataset.xlsx", 
                         sheet = "GovernmentEffectiveness", col_names = TRUE, 
                         skip = 14)
WDIdata <- read_csv("WDIdata.csv")

## the piece measures COVID cases per 1,000 people and tests per 100 people
covidvar$`cases/1000` <- (covidvar$TotalCases / (covidvar$Population/1000)) #Mean = 1.69

covidvar$`tests/100` <- (covidvar$TotalTests / (covidvar$Population/100)) #Mean = 3.75
covidvar$Code <- as.character(covidvar$Code)

## critical case rate = critical cases / COVID-19 infections
### only 131 countries have data on critical cases...
covidvar$`critical case rate` <- ((covidvar$`Serious,Critical`)/(covidvar$TotalCases)) 

## morality - log(deaths/100 cases)
covidvar$`mortalityrate` <- log1p((covidvar$TotalDeaths)/(covidvar$TotalCases/100)) #mean = 3.70

## filtering out the government effectiveness variable
govteffect <- subset(wgidataset[,c(1,2,123)])
colnames(govteffect) <- c("Country","Code","Score")

covidvar <- tibble(covidvar[-1,]) ## don't need the WORLD row from the original data
corona <- covidvar %>% select(c(Code, Country,`cases/1000`,`tests/100`,`critical case rate`
                      , `mortalityrate`)) 

## this will end up being the final dataset
mydataset <- merge(corona,govteffect, by ="Code")
mydataset <- mydataset[-7] ## do not need two separate country columns


hosp <- filter(WDIdata, `Series Name` == 'Hospital beds (per 1,000 people)')
population <- filter(WDIdata, `Series Code` == 
                    "SP.POP.65UP.TO.ZS")
disease <- filter(WDIdata, `Series Code` == "SH.DTH.COMM.ZS")
logistics <- filter(WDIdata, `Series Code` == "LP.LPI.INFR.XQ")

## first, creating the hospital variable
hosp <- hosp[!(hosp$`2016 [YR2016]` == ".." & hosp$`2017 [YR2017]` == ".." 
               & hosp$`2018 [YR2018]` == ".." & hosp$`2019 [YR2019]` == ".."
               & hosp$`2020 [YR2020]` == ".."),] 
hosp <- hosp[-c(3,4)] 

hosp[hosp == ".."] <- NA

yr2016 <- as.numeric(hosp$`2016 [YR2016]`)
hosp$y2016 <- yr2016
yr2017 <- as.numeric(hosp$`2017 [YR2017]`)
hosp$y2017 <- yr2017
yr2018 <- as.numeric(hosp$`2018 [YR2018]`)
hosp$y2018 <- yr2018
yr2019 <- as.numeric(hosp$`2019 [YR2019]`)
hosp$y2019 <- yr2019
yr2020 <- as.numeric(hosp$`2020 [YR2020]`)
hosp$y2020 <- yr2020

hosp <- hosp[-c(3:7)]
hosp <- hosp[-7]

year <- c(hosp$y2016, hosp$y2017, hosp$y2018, hosp$y2019)

## want to select the most recent of each year for each country 
h <- pivot_longer(hosp, cols = starts_with("y"), names_to = "year", 
                    names_prefix = "YR", values_to = "beds", values_drop_na = TRUE)

beds <- h %>%
  group_by(`Country Code`) %>%
  summarise(beds = last(beds))

beds <- rename(beds, Code = `Country Code`)

mydataset <- merge(mydataset, beds, by = "Code")

## need population, communicable disease deaths, etc. 
pop65 <- population[-c(1,3:7,9)]
pop65 <- rename(pop65, population_65_percent = "2019 [YR2019]") 
pop65 <- rename(pop65, Code = "Country Code")

mydataset <- merge(mydataset, pop65, by = "Code")

## communicable disease is only available for 2016. 
cdisease <- disease[-c(1,3:4,6:9)]
cdisease <- rename(cdisease, death_communicable_disease = "2016 [YR2016]")
cdisease <- rename(cdisease, Code = "Country Code")

mydataset <- merge(mydataset, cdisease, by = "Code")

## logistics score most recently available for 2018
lscore <- logistics[c(2,7)]
lscore <- rename(lscore, logisics_score = "2018 [YR2018]")
lscore <- rename(lscore, Code = "Country Code")

mydataset <- merge(mydataset, lscore, by = "Code") %>% 
  rename("ISO3" = Code) 

colnames(mydataset)

saveRDS(mydataset, "coviddata.RDS")
