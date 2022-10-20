# This file cleans and produces the policy data used in one version of the 
# "Something in the Air" working paper (Spring 2022); the alternate version used
# data obtained by webscraping from the ECOLEX database

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(data.table)
library(readxl)
library(stargazer)
library(gridExtra)
setwd("/Users/tristanmisko/Documents/Berkeley/ECON/H195B")
FAO <- read.csv("FAOLEX_ALL.csv")
FAO <- data.table(FAO)
FAO$AirDomain <- !is.na(str_match(FAO$Domain, "[Aa]ir"))
FAO$EnergySubject <- !is.na(str_match(FAO$Primary.subjects, "[Ee]nergy"))
FAO
unique(FAO$Country.Territory)[!is.na(str_match(unique(FAO$Country.Territory), "United States of America"))]

kwords <- c("air quality/air pollution",
            "authorization/permit",
            "basic legislation",
            "bioenergy",
            "biofuel",
            "data collection/reporting",
            "emissions",
            "emissions trading",
            "energy conservation/energy production",
            "enforcement/compliance",
            "environmental standards",
            "hydropower.generation",
            "monitoring",
            "nuclear energy",
            "pollution control",
            "public health",
            "renewable energy",
            "standards",
            "subsidy/incentive")
FAO$YEAR <- as.numeric(str_extract(FAO$Date.of.original.text, "\\d{4}"))

code_table <- read.csv("aqi_countrylist.csv") %>% select(1:3)
code_table <- data.table(code_table)
FAO$Country.Territory <- str_replace(FAO$Country.Territory, "Korea, Republic of", "Korea")
FAO$Country.Territory <- str_replace(FAO$Country.Territory, "Korea, Dem. People's Rep.", "NKor")
FAO$Country.Territory <- str_replace(FAO$Country.Territory, "United States of America", "United States")
FAO$Country.Territory <- str_replace(FAO$Country.Territory, "Slovakia", "Slovak Republic")
OECD <- c("Australia", "Austria", "Belgium", "Canada", "Chile", "Colombia", 
          "Costa+Rica", "Czech+Republic", "Denmark", "Estonia", "Finland", 
          "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", 
          "Israel", "Italy", "Japan", "Korea%2C+Republic+of", "Latvia", "Lithuania", 
          "Luxembourg", "Mexico", "Netherlands", "New+Zealand", "Norway", "Poland", 
          "Portugal", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey",
          "United+Kingdom", "United+States+of+America")
setkey(code_table, "URL_Slug")
OECD_table <- code_table[OECD]
OECD_codes <- OECD_table$Code
setkey(code_table, "Location")
FAO$COU <- code_table[FAO$Country.Territory]$Code
FAO$OECD <- FAO$COU %in% OECD_codes
          
FAO <- FAO %>% filter(OECD, Territorial.subdivision == "")
FAO$Legislation <- FAO$Type.of.text == "Legislation"
FAO$Regulation <- FAO$Type.of.text == "Regulation"
FAO$Policy <- FAO$Type.of.text == "Policy"
FAO$Constitution <- FAO$Type.of.text == "Constitution"
setkey(code_table, "Code")

FAO <- filter(FAO, YEAR >= 1990 & YEAR <= 2019)
dummy_data <- data.table(FAO$Record.Id)
for (k in kwords){
    dummy_data <- cbind(dummy_data, as.numeric(!is.na(str_extract(FAO$Keywords, k))))
}
colnames(dummy_data) <- c("Record.Id", str_replace_all(kwords, "[\\s/]", "\\."))
FAO <- merge(FAO, dummy_data, by.x = "Record.Id", by.y = "Record.Id")
regex_string <- "(\\b[Rr]oad\\b)|(\\b[Cc]ar\\b)|([Tt]ruck)|([Vv]ehic)|(\\b[Aa]utomo)|([Hh]ighway)"
match_matrix <- str_match(FAO$Abstract, regex_string)
FAO$electric.vehicle <- as.numeric(!is.na(str_match(FAO$Abstract, "[Ee]lectric vehic")))
vehicle_relevance <- rowSums(!is.na(match_matrix))
FAO$vehicle_relevance <- vehicle_relevance
colnames(FAO)
FAO <- select(FAO, c(1:20, 48, 21:33, 47, 34:46))
index_set <- which(vehicle_relevance > 0)
vehicle_policy <- FAO[index_set] %>% filter(YEAR >= 2000)
domains <- unique(vehicle_policy$Domain)
subjects <- unique(vehicle_policy$Primary.subjects)
relevant_domains <- domains[rowSums(!is.na(str_match(domains, "([Ee]nvironment)|(Air)"))) > 0]
relevant_subject <- subjects[rowSums(!is.na(str_match(subjects, "(Environment)|(National)|(General)|(Energy)|(Health)"))) > 0]
vehicle_policy <- vehicle_policy %>% filter(Domain %in% relevant_domains, Primary.subjects %in% relevant_subject)
# vehicle policy for source behavior
vehicle_policy$match_string <- paste(vehicle_policy$COU, vehicle_policy$YEAR, sep = "")
vpsb <- vehicle_policy %>% group_by(match_string) %>% 
    summarize_at(colnames(vehicle_policy)[-c(1:24, 49)], sum)
vpid <- vehicle_policy %>% group_by(match_string) %>% 
            summarize_at(c("COU", "YEAR"), first)
vpsb <- merge(vpid, vpsb, by.x = "match_string", by.y = "match_string")

write.csv(vpsb, "vpsb.csv")

vpsb_keywords <- vpsb %>% group_by(COU) %>%
    summarize_at(-c(1:2), sum)
vpsb_keywords <- data.table(ungroup(vpsb_keywords))
vpsb_keywords <- melt(vpsb_keywords, id.vars = "COU", variable.name = "keyword", value.name = "count") %>%
    group_by(keyword) %>% summarize(sum(count))
View(vpsb_keywords)

#vehicle policy for generation intensity
vehicle_policy_gi <- vehicle_policy %>% filter(air.quality.air.pollution == 1 | electric.vehicle == 1)

vpgi <- vehicle_policy_gi %>% group_by(match_string) %>% 
    summarize_at(colnames(vehicle_policy_gi)[-c(1:24, 49)], sum)
vpid <- vehicle_policy_gi %>% group_by(match_string) %>% 
    summarize_at(c("COU", "YEAR"), first)
vpgi <- merge(vpid, vpgi, by.x = "match_string", by.y = "match_string")

write.csv(vpgi, "vpgi.csv")
vpgi_keywords <- vpgi %>% group_by(COU) %>%
    summarize_at(-c(1:2), sum)
vpgi_keywords
vpgi_keywords <- data.table(ungroup(vpgi_keywords))
vpgi_keywords <- melt(vpgi_keywords, id.vars = "COU", variable.name = "keyword", value.name = "count") %>%
    group_by(keyword) %>% summarize(sum(count))
View(vpgi_keywords)


# energy policy
energy <- FAO %>% filter(EnergySubject, YEAR >= 1990 & YEAR <= 2019) %>%
    filter(energy.conservation.energy.production == 1)

domains <- unique(energy$Domain)
subjects <- unique(energy$Primary.subjects)
relevant_domains <- domains[rowSums(!is.na(str_match(domains, "([Ee]nvironment)|(Air)|(Energy)"))) > 0]
relevant_subject <- subjects[rowSums(!is.na(str_match(subjects, "(Environment)|(National)|(General)|(Energy)"))) > 0]
energy <- energy %>% filter(Domain %in% relevant_domains,
                            Primary.subjects %in% relevant_subject)
energy$match_string <- paste(energy$COU, energy$YEAR, sep="")
colnames(energy)
pgsb <- energy %>% group_by(match_string) %>% 
    summarize_at(colnames(energy)[-c(1:24, 49)], sum)
pgid <- energy %>% group_by(match_string) %>% 
    summarize_at(c("COU", "YEAR"), first)
pgsb <- merge(pgid, pgsb, by.x = "match_string", by.y = "match_string")

write.csv(pgsb, "pgsb.csv")

pgsb_keywords <- ungroup(pgsb) %>% group_by(COU) %>%
    summarize_at(-c(1:2), sum)
pgsb_keywords <- data.table(ungroup(pgsb_keywords))
pgsb_keywords <- melt(pgsb_keywords, id.vars = "COU", variable.name = "keyword", value.name = "count") %>%
    group_by(keyword) %>% summarize(sum(count))
View(pgsb_keywords)
        
energy_gi <- energy %>% filter(air.quality.air.pollution == 1 | emissions == 1 | AirDomain)
pggi <- energy_gi %>% group_by(match_string) %>% 
    summarize_at(colnames(energy)[-c(1:24, 49)], sum)
pgid <- energy %>% group_by(match_string) %>% 
    summarize_at(c("COU", "YEAR"), first)
pggi <- merge(pgid, pggi, by.x = "match_string", by.y = "match_string")

write.csv(pggi, "pggi.csv")

pggi_keywords <- ungroup(pggi) %>% group_by(COU) %>%
    summarize_at(-c(1:2), sum)
pggi_keywords <- data.table(ungroup(pggi_keywords))
pggi_keywords <- melt(pggi_keywords, id.vars = "COU", variable.name = "keyword", value.name = "count") %>%
    group_by(keyword) %>% summarize(sum(count))
View(pggi_keywords)

keywords <- merge(vpsb_keywords, vpgi_keywords, by.x = "keyword", by.y = "keyword")
keywords <- merge(keywords, pgsb_keywords, by.x = "keyword", by.y = "keyword")
keywords <- merge(keywords, pggi_keywords, by.x = "keyword", by.y = "keyword")
colnames(keywords)[2:5] <- c("vpsb", "vpgi", "pgsb", "pggi")
View(keywords)