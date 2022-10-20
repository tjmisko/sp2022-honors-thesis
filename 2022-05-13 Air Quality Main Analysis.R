# this file produces the main data sets used in the "Something in the Air" working
# paper by Tristan Misko (Spring 2022)

# load packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(data.table)
library(readxl)
library(stargazer)
library(gridExtra)

# load in datasets
polcon <- read_excel("POLCON_2017.xlsx") %>% select(c("ctrynm", "polity_country", "year", "polconiii")) %>%
     filter(year > 1989)
pollution <- read.csv("oecd_aqi/emissions_by_source.csv")
pm25 <- read.csv("air_quality_data_oecd.csv")
policy <- read.csv("Policy\ Data/policy_data_w_dummies.csv")
code_table <- read.csv("aqi_countrylist.csv") %>% select(1:3)
OECD <- c("Australia", "Austria", "Belgium", "Canada", "Chile", "Colombia",
          "Costa+Rica", "Czech+Republic", "Denmark", "Estonia", "Finland",
          "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland",
          "Israel", "Italy", "Japan", "Korea%2C+Republic+of", "Latvia", "Lithuania",
          "Luxembourg", "Mexico", "Netherlands", "New+Zealand", "Norway", "Poland",
          "Portugal", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey",
          "United+Kingdom", "United+States+of+America")

policy$OECD <- policy$Country %in% OECD

# clean out excess columns, filter out non legislation, filter out laws with NA names
policy <- policy %>% select(c(-1, -2)) %>% filter(Type == "Legislation", Name != "NA")
length(policy)
keyword_list <- colnames(policy)[-c(1:5, 433)]
mat <- as.matrix(select(policy, -c(1:5, 433)))

# generate frequency table
frequency_table <- data.frame(keyword = keyword_list,
                              keyword_frequency = colSums(matrix(as.numeric(mat),
                                                                 ncol = ncol(mat)))) %>%
    arrange(-keyword_frequency)
frequency_table$keyword_prop <- frequency_table$keyword_frequency/nrow(policy)
year_bucket <- Vectorize(function(year){
    if (year < 2000){return("1990s")}
    if (year < 2010 & year >= 2000){return("2000s")}
    else {return("2010s")}
}, "year")
OECD_policy <- policy %>% filter(OECD)
mat <- as.matrix(select(OECD_policy, -c(1:5, 433)))
OECD_frequency_table <- data.frame(keyword = keyword_list,
                              keyword_frequency = colSums(matrix(as.numeric(mat),
                                                                 ncol = ncol(mat)))) %>%
    arrange(-keyword_frequency)
OECD_frequency_table$keyword_prop <- OECD_frequency_table$keyword_frequency/nrow(OECD_policy)

OECD_frequency_table
policy$keyword_count <- unlist(lapply(str_extract_all(policy$Keywords, ","), FUN = length)) + 1
policy$NameShort <- paste(str_extract(policy$Name, ".{40}"), "...", sep = "")

# create a lookup table to convert between country codes and country names
code_lookup <- data.table(code_table)
country_lookup <- data.table(code_table)
setkey(code_lookup, "Code")
setkey(country_lookup, "Location")
policy$year_bucket <- year_bucket(policy$Date)

policy_by_decade <- policy %>% select(-c("Type", "Name", "Keywords")) %>% group_by(Country, year_bucket)
policy_by_decade <- ungroup(policy_by_decade)
policy_by_decade <- policy_by_decade %>% summarize_at(colnames(policy_by_decade)[-c(1:5,430:435)], sum)

policy_by_decade_top25 <- policy_by_decade %>% select(c(1:2, top25))
policy_by_decade_top25$nlaws <- summarize(group_by(policy, Country, year_bucket), nlaws = n())$nlaws
select(policy_by_decade_top25, c(1:2, 28, 3:27))

polcon$year_bucket <- year_bucket(polcon$year)

polcon_decade_means <- filter(polcon, polity_country %in% OECD) %>% group_by(ctrynm, year_bucket) %>%
    summarize(mean(polconiii))

countrylist <- c()
for (code in polcon_decade_means$ctrynm){
    countrylist <- c(countrylist, code_lookup[code][[1]])
}
polcon_decade_means$country <- countrylist
polcon_decade_means
policy_by_decade
pbd_cross <- filter(policy_by_decade, Country %in% polcon_decade_means$country)
policy_passage <- select(data.frame(pbd_cross, polcon_decade_means), -32)
policy_passage
reg <- lm(data = policy_passage, nlaws ~ mean.polconiii.)
summary(reg)

# Regression Plot of nlaws on POLCON
ggplot(policy_passage, aes(x = mean.polconiii., y = nlaws)) + geom_point() +
    geom_abline(slope = summary(reg)$coef[2], intercept = summary(reg)$coef[1]) +
    xlab("Decadal Mean Political Constraints Index") +
    ylab("Number of Air Quality Related Laws Passed Per Decade") +
    labs(title = "No Relationship Between Political Constraint Index and the Number of Air Quality Laws Passed")

# histogram of keyword count
ggplot(policy, aes(x = keyword_count)) +
    geom_histogram(binwidth = 0.5, col = "black", fill = "darkblue") +
    xlab("Keyword Count")
mean(policy$keyword_count)
# histogram of log keyword count
ggplot(policy, aes(x = log(keyword_count))) +
    geom_histogram(binwidth = 0.05, col = "black", fill = "darkblue") +
    xlab("Log Keyword Count") +
    labs(title = "Distribution of Keyword Count",
         subtitle = "Keyword Count is Roughly Log Normal with a Long Right Tail (Mean = 8.30 Keywords)")

# is the number of laws a country passes consistent over time?
dat <- policy %>% group_by(Date, Country) %>% summarize(nlaws = n(),
                                                        mean_keyword_count = mean(keyword_count))
dat2 <- policy %>% group_by(Date) %>% summarize(nlaws = n())
dat3 <- policy %>% filter(as.logical(climate.change)) %>%
    group_by(Date) %>% summarize(nlaws = n())

qdat <- dat %>% group_by(Date) %>%
    summarize(q00 = quantile(nlaws, 0),
              q20 = quantile(nlaws, 0.2),
              q40 = quantile(nlaws, 0.4),
              q50 = quantile(nlaws, 0.5),
              q60 = quantile(nlaws, 0.6),
              q80 = quantile(nlaws, 0.8),
              q100 = quantile(nlaws, 1))
dat4 <- policy %>% group_by(Country) %>% summarize(nlaws = n()) %>%
    arrange(-nlaws)
dat4
# time series of the number of laws across all countries by year
ggplot(dat2, aes(x = Date, y = nlaws)) + geom_line() +
    geom_line(data = dat3, col = "red")

# time series of number of laws passed in each country by year
stripe_plot <- ggplot()
for (c in unique(dat$Country)){
    stripe_plot <- stripe_plot +
        geom_line(data = filter(dat, Country == c), aes(x = Date, y = nlaws))
}

# stripe plot with quantiles
stripe_plot2 <- ggplot(qdat, aes(x = Date)) +
    geom_area(aes(y = q100), fill = "darkred", alpha = 0.7) +
    geom_area(aes(y = q80), fill = "coral4", alpha = 0.7) +
    geom_area(aes(y = q60), fill = "coral3", alpha = 0.7) +
    geom_area(aes(y = q40), fill = "coral2", alpha = 0.7) +
    geom_area(aes(y = q20), fill = "coral1", alpha = 0.7) +
    geom_area(aes(y = q00), fill = "coral", alpha = 0.7) +
    geom_line(aes(y = q50), size = 1.25)
stripe_plot2 + xlab("Year") + ylab("Number of Laws Passed") +
    labs(title = "Number of Air Quality Related Laws Passed per Year",
         subtitle = "A few countries pass many laws per year, but most pass only a couple.")

# plot nlaws vs mean_keyword_count
ggplot(dat, aes(x = nlaws, y = mean_keyword_count)) + geom_point() +
    labs(title = "Average Keywords Per Law: Average Keyword Count is Decreasing in the Number of Laws",
         subtitle = "Mean Keywords and Mean Number of Laws Per Year Taken Over Decadal Buckets") +
    xlab("Number of Laws Passed (Decade Average)") + ylab("Mean Keyword Count Per Law (Decade Average)")

# import datasets
driving <- read.csv("transport_data.csv") %>% filter(VARIABLE == "T-PASS-RD-TOT") %>%
    arrange(Country)

road_pollution <- pollution %>% filter(VAR == "MOB_ROAD")
road_pollution <- spread(road_pollution %>% select(c("COU", "Country", "POL", "YEA", "Value")), POL, Value)
road_pollution <- filter(road_pollution, YEA >= 2000)
road_pollution$match_string <- paste(road_pollution$COU, road_pollution$YEA, sep ="")

vpsb <- read.csv("vpsb.csv")
vpgi <- read.csv("vpgi.csv")
vpsb <- data.table(vpsb) %>% select(-1)
vpgi <- data.table(vpgi) %>% select(-1)

road_sb <- merge(road_pollution, vpsb, by.x = "match_string", by.y = "match_string", all.x = TRUE, all.y = TRUE) %>% 
    select(-c("YEAR", "COU.y"))
colnames(road_sb)[2] <- "COU"
road_gi <- merge(road_pollution, vpgi, by.x = "match_string", by.y = "match_string", all.x = TRUE, all.y = TRUE) %>%
    select(-c("YEAR", "COU.y"))
colnames(road_gi)[2] <- "COU"
nas_sb <- as.matrix(select(road_sb, -c(1:10)))
nas_gi <- as.matrix(select(road_gi, -c(1:10)))
nas_sb[is.na(nas_sb)] <- 0
nas_gi[is.na(nas_gi)] <- 0
road_sb <- data.table(select(road_sb, 1:10), nas_sb)
road_gi <- data.table(select(road_gi, 1:10), nas_gi)

road_sb_cum <- road_sb %>% group_by(COU) %>%
    mutate(across(colnames(road_sb)[-c(1:10)], ~cumsum(.x)))
road_gi_cum <- road_gi %>% group_by(COU) %>%
    mutate(across(colnames(road_gi)[-c(1:10)], ~cumsum(.x)))

road_sb <- data.table(filter(road_sb_cum, !is.na(YEA)))
road_gi <- data.table(filter(road_gi_cum, !is.na(YEA)))
driving <- select(driving, c("COUNTRY", "Year", "Value"))
driving$match_string <- paste(driving$COUNTRY, driving$Year, sep="")
driving <- select(driving, -c("COUNTRY", "Year"))

colnames(driving)[1] <- "Passenger.Kilometers"
road_sb <- merge(driving, road_sb, by.x = "match_string", by.y = "match_string")
road_gi <- merge(driving, road_gi, by.x = "match_string", by.y = "match_string")

road_av_sb <- road_sb %>% group_by(Country) %>% summarize(nyears = n())
road_av_gi <- road_gi %>% group_by(Country) %>% summarize(nyears = n())

#pull in control variables
gdp <- read.csv("gdp_per_capita.csv")
infra <- read.csv("infrastructure_spending.csv")
gdp <- data.table(gdp)
infra <- data.table(infra) %>% select(c(1, 3, 4, 6, 7))
infra <- spread(infra, SUBJECT, Value)
infra_prop <- filter(infra, MEASURE == "EUR")
infra_prop$tot_inland <- rowSums(select(infra_prop, -c(1:3, 5, 9)), na.rm = T)
infra_prop$prop_road <- infra_prop$ROAD/infra_prop$tot_inland
infra_prop$match_string <- paste(infra_prop$LOCATION, infra_prop$TIME, sep = "")
infra_gdp <- filter(infra, MEASURE == "PC_GDP")
infra_gdp$match_string <- paste(infra_gdp$LOCATION, infra_gdp$TIME, sep = "")
infra_gdp <- select(infra_gdp, c("match_string", "INLAND"))
infra_prop <- select(infra_prop, c("match_string", "prop_road", "LOCATION", "TIME"))
infra_prop <- merge(infra_prop, infra_gdp, by.x = "match_string", by.y = "match_string")
infra_prop$InfraSpend <- infra_prop$prop_road*infra_prop$INLAND
lags <- spread(select(infra_prop, -c("match_string", "prop_road", "INLAND")), LOCATION, InfraSpend)
lags <- as.matrix(lags)
lags[,1] <- 1997:2022
lags <- data.table(lags)
lags <- filter(melt(lags, id.vars="TIME", variable.name = "COU", value.name = "Lagged2YInfraSpend"), !is.na(Lagged2YInfraSpend))
lags$match_string <- paste(lags$COU, lags$TIME, sep="")
infra_prop$match_string <- paste(infra_prop$LOCATION, infra_prop$TIME, sep = "")
infra_prop <- infra_prop %>% select(-c("LOCATION", "TIME"))
infra_prop <- merge(infra_prop, lags, by.x = "match_string", by.y = "match_string")
infra_prop <- select(infra_prop, c("match_string", "InfraSpend", "Lagged2YInfraSpend"))
infra_prop

gdp$match_string <- paste(gdp$LOCATION, gdp$TIME, sep = "")
pop <- gdp %>% filter(SUBJECT == "T_POPTOT", TIME >= 1990) %>% select("match_string", "Value")
gdp <- gdp %>% filter(SUBJECT == "T_GDPPOP", MEASURE == "VPVOB", TIME >= 1990) %>% select("match_string", "Value")
colnames(pop)[2] <- "TotPop"
colnames(gdp)[2] <- "GDPperCAP"

#append controls
road_sb <- merge(road_sb, gdp, by.x = "match_string", by.y = "match_string")
road_sb <- merge(road_sb, pop, by.x = "match_string", by.y = "match_string")
road_sb <- merge(road_sb, infra_prop, by.x = "match_string", by.y = "match_string")
road_sb <- data.table(road_sb)

road_gi <- merge(road_gi, gdp, by.x = "match_string", by.y = "match_string")
road_gi <- merge(road_gi, pop, by.x = "match_string", by.y = "match_string")
road_gi <- merge(road_gi, infra_prop, by.x = "match_string", by.y = "match_string")
road_gi <- data.table(road_gi)

road_sb$PKperCap <- road_sb$Passenger.Kilometers/road_sb$TotPop
road_sb$A.CO <- road_sb$CO/road_sb$PKperCap
road_sb$A.NOX <- road_sb$NOX/road_sb$PKperCap
road_sb$A.NMVOC <- road_sb$NMVOC/road_sb$PKperCap
road_sb$A.PM10 <- road_sb$PM10/road_sb$PKperCap
road_sb$A.PM2.5 <- road_sb$`PM2-5`/road_sb$PKperCap
road_sb$A.SOX <- road_sb$SOX/road_sb$PKperCap

road_gi$PKperCap <- road_gi$Passenger.Kilometers/road_gi$TotPop
road_gi$A.CO <- road_gi$CO/road_gi$PKperCap
road_gi$A.NOX <- road_gi$NOX/road_gi$PKperCap
road_gi$A.NMVOC <- road_gi$NMVOC/road_gi$PKperCap
road_gi$A.PM10 <- road_gi$PM10/road_gi$PKperCap
road_gi$A.PM2.5 <- road_gi$`PM2-5`/road_gi$PKperCap
road_gi$A.SOX <- road_gi$SOX/road_gi$PKperCap

gas_price <- read.csv("gas_price/gas_price_data.csv")
gas_price <- data.table(gas_price)
gas_price <- melt(data = gas_price, 
                  id.vars = c("Country.Name", "Country.Code"), 
                  measure.vars = c(5:22),
                  variable.name = "Year", 
                  value.name="gas.price.usd.per.liter")
gas_price$Year <- as.numeric(unlist(str_extract_all(gas_price$Year, "\\d{4}")))
gas_price$match_string <- paste(gas_price$Country.Code, gas_price$Year, sep = "")
gas_price <- gas_price %>% select(c("match_string", "gas.price.usd.per.liter"))
road_sb <- data.table(merge(road_sb, gas_price, by.x = "match_string", by.y = "match_string"))
road_gi <- data.table(merge(road_gi, gas_price, by.x = "match_string", by.y = "match_string"))

# append rail controls
rail <- read.csv("rail_transport.csv")
rail <- rail %>% filter(VARIABLE == "T-PASS-RL-TOT")
rail <- rail %>% select(c("COUNTRY", "YEAR", "Value"))
colnames(rail)[3] <- "Total.Rail.Transport"
rail$match_string <- paste(rail$COUNTRY, rail$YEAR, sep="")
rail <- rail %>% select("match_string", "Total.Rail.Transport")
road_sb <- data.table(merge(road_sb, rail, by.x = "match_string", by.y = "match_string"))
road_gi <- data.table(merge(road_gi, rail, by.x = "match_string", by.y = "match_string"))
road_sb$RAILperCAP <- road_sb$Total.Rail.Transport/road_sb$TotPop
road_gi$RAILperCAP <- road_gi$Total.Rail.Transport/road_gi$TotPop

road_sb <- road_sb %>% filter(COU != "RUS")
road_gi <- road_gi %>% filter(COU != "RUS")

write.csv(road_sb, "road_sb.csv")
write.csv(road_gi, "road_gi.csv")

# power generation policy data
pgsb <- data.table(read.csv("pgsb.csv"))
pggi <- data.table(read.csv("pggi.csv"))
pgsb <- pgsb %>% select(-1) %>% select(-c("COU", "YEAR"))
pggi <- pggi %>% select(-1) %>% select(-c("COU", "YEAR"))

# power consumption source behavior
energy <- data.table(read.csv("energy_data.csv"))
energy <- melt(energy, id.vars = c("Indicator.Code", "Country"), variable.name = "Year", value.name="Energy.Consumption")
energy$YEAR <- as.numeric(str_extract(energy$Year, "\\d{4}"))
energy$Country <- str_extract(energy$Country, "\\b.*")
energy$COU <- country_lookup[energy$Country]$Code
energy <- energy %>% filter(!is.na(COU))
energy$match_string <- paste(energy$COU, energy$YEAR, sep="")
energy <- energy %>% select(c("match_string", "Energy.Consumption"))

# power generation pollution data
pg_pollution <- pollution %>% filter(VAR == "STAT_POW")
pg_pollution <- spread(pg_pollution %>% select(c("COU", "Country", "POL", "YEA", "Value")), POL, Value)
pg_pollution$match_string <- paste(pg_pollution$COU, pg_pollution$YEA, sep ="")
pg_pollution <- data.table(select(pg_pollution, c("match_string", 4:9)))

# merge the datasets
pg <- merge(x = energy, y = pg_pollution, by.x = "match_string", by.y="match_string")
pg <- merge(x = pg, y = pop, by.x = "match_string", by.y="match_string")
pg <- merge(x = pg, y = gdp, by.x = "match_string", by.y="match_string")

pg$Energy.Consumption <- as.numeric(pg$Energy.Consumption)
pg <- pg %>% filter(!is.na(Energy.Consumption))
pg$s <- pg$Energy.Consumption/pg$TotPop * 1000
pg$A.CO <- pg$CO/pg$s
pg$A.NOX <- pg$NOX/pg$s
pg$A.NMVOC <- pg$NMVOC/pg$s
pg$A.PM10 <- pg$PM10/pg$s
pg$A.PM2.5 <- pg$`PM2-5`/pg$s
pg$A.SOX <- pg$SOX/pg$s

pg_sb <- merge(pg, pgsb, by.x = "match_string", by.y = "match_string", all.x = TRUE, all.y = TRUE)
pg_sb 
pg_gi <- merge(pg, pggi, by.x = "match_string", by.y = "match_string", all.x = TRUE, all.y = TRUE)
nas_sb <- as.matrix(select(pg_sb, -c(1:17)))
nas_gi <- as.matrix(select(pg_gi, -c(1:17)))
nas_sb[is.na(nas_sb)] <- 0
nas_gi[is.na(nas_gi)] <- 0
pg_sb <- data.table(select(pg_sb, 1:17), nas_sb)
pg_gi <- data.table(select(pg_gi, 1:17), nas_gi)
pg_sb$COU <- str_extract(pg_sb$match_string, "\\w{3}")
pg_gi$COU <- str_extract(pg_gi$match_string, "\\w{3}")
colnames(pg_sb)
pg_sb <- pg_sb %>% select(c(1, 42, 2:41))
pg_gi <- pg_gi %>% select(c(1, 42, 2:41))
colnames(pg_gi)
pg_sb_cum <- pg_sb %>% group_by(COU) %>%
    mutate(across(colnames(pg_sb)[-c(1:18)], ~cumsum(.x)))
pg_gi_cum <- pg_gi %>% group_by(COU) %>%
    mutate(across(colnames(pg_gi)[-c(1:18)], ~cumsum(.x)))

pg_sb <- data.table(filter(pg_sb_cum, !is.na(s)))
pg_gi <- data.table(filter(pg_gi_cum, !is.na(s)))

write.csv(pg_sb, "pg_sb.csv")
write.csv(pg_gi, "pg_gi.csv")

# overdetermined plot
BEL <- filter(road, COU == "BEL")
explot1 <- ggplot(BEL, aes(x = YEA, y = A.CO)) + 
    geom_vline(xintercept = c(2003, 2010, 2013), col ="blue", alpha = 0.5) + 
    geom_line() + xlab("Year") + labs(title = "Tractable System",
                                      subtitle = "A Plausbile Estimation of Effect of Policy Can Be Made")
explot2 <- ggplot(BEL, aes(x = YEA, y = A.CO)) + 
    geom_vline(xintercept = 2000:2016, col ="blue", alpha = 0.5) + 
    geom_line() + xlab("Year") + 
    labs(title = "Overdetermined System",
         subtitle = "Estimation of Effect of Policy is Difficult in this Setting")

grid.arrange(explot1, explot2, nrow = 1, top ="Determining the Effect of Policy on the Generation Intensity of CO (Blue Lines Indicate a Policy Passage Date)")

# determine proxy status
vk <- data.table(read.csv("vk.csv"))
vk <- melt(vk, id.vars = c("Country"), variable.name = "Year", value.name="VehicleKilometers")
vk <- vk[-which(is.na(vk$VehicleKilometers))]

vk$Year <- as.numeric(substr(vk$Year, 2, 5))
vk$code <- country_lookup[vk$Country]$Code
vk[which(is.na(vk$code))]$code <- rep("RUS", 7)
vk$match_string <- paste(vk$code, vk$Year, sep ="")
vk <- select(vk, c(5,3))

tk <- data.table(read.csv("goods_transport.csv"))
tk <- tk %>% filter(VARIABLE == "T-GOODS-RD-TOT")
tk$match_string <- paste(tk$COUNTRY, tk$YEAR, sep="")
tk <- select(tk, c("match_string", "Value"))
colnames(tk)[2] <- "TonneKilometers"
pk <- data.table(match_string = road$match_string,
                 PassengerKilometers = road$Passenger.Kilometers,
                 TotPop = road$TotPop)
vk_est <- merge(tk, pk, by.x = "match_string", by.y = "match_string")
vk_est <- merge(vk, vk_est, by.x = "match_string", by.y = "match_string")
vk_est$VehicleKilometers <- as.numeric(str_replace_all(vk_est$VehicleKilometers, ",", ""))

vk_est$VKperCap <- vk_est$VehicleKilometers/vk_est$TotPop
vk_est$TKperCap <- vk_est$TonneKilometers/vk_est$TotPop
vk_est$PKperCap <- vk_est$PassengerKilometers/vk_est$TotPop
vk_model1 <- lm(data = vk_est, 
               VehicleKilometers ~ TonneKilometers + PassengerKilometers - 1)
vk_model2 <- lm(data = vk_est, 
               VehicleKilometers ~ TonneKilometers + PassengerKilometers)
vk_model3 <- lm(data = vk_est, 
                VKperCap ~ TKperCap + PKperCap - 1)
vk_model4 <- lm(data = vk_est, 
                VKperCap ~ TKperCap + PKperCap)

stargazer(vk_model1, vk_model2, vk_model3, vk_model4)

ggplot(vk_est, aes(x = PKperCap, y = VKperCap)) + geom_point() +
    geom_abline(slope = vk_model3$coefficients[2], intercept = 0, col = "blue") +
    geom_abline(slope = vk_model4$coefficients[3], 
                intercept = vk_model4$coefficients[1], col = "red") + 
    xlab("Passenger Kilometers of Road Transport per Capita") + 
    ylab("Vehicle Kilometers of Road Transport per Capita")

ggplot(vk_est, aes(x = TKperCap, y = VKperCap)) + geom_point() +
    geom_abline(slope = vk_model3$coefficients[1], intercept = 0, col = "blue") +
    geom_abline(slope = vk_model4$coefficients[2], 
                intercept = vk_model4$coefficients[1], col = "red") +
    xlab("Passenger Kilometers of Road Transport per Capita") + 
    ylab("Vehicle Kilometers of Road Transport per Capita")

ggplot(vk_est, aes(x = PassengerKilometers, y = VehicleKilometers)) + geom_point() +
    geom_abline(slope = vk_model1$coefficients[2], intercept = 0, col = "blue") +
    geom_abline(slope = vk_model2$coefficients[3], 
                intercept = vk_model4$coefficients[1], col = "red")

total_pollution <- pollution %>% filter(VAR == "TOT")
total_pollution <- spread(total_pollution %>% select(c("COU", "Country", "POL", "YEA", "Value")), POL, Value)
total_pollution$match_string <- paste(total_pollution$COU, total_pollution$YEA, sep="")
colnames(total_pollution)[4:9] <- paste(rep("Tot", 6), colnames(total_pollution)[4:9], sep="")

road_pollution$match_string <- paste(road_pollution$COU, road_pollution$YEA,sep="")
electricity_pollution$match_string <- paste(electricity_pollution$COU, electricity_pollution$YEA,sep="")
ep <- data.table(select(electricity_pollution, c(10, 4:9)))
rp <- data.table(select(road_pollution, c(10, 4:9)))
tp <- data.table(select(total_pollution, c(10, 4:9)))
colnames(ep)[2:7] <- paste(rep("PG.", 6), colnames(ep)[2:7], sep="")
colnames(rp)[2:7] <- paste(rep("RT.", 6), colnames(rp)[2:7], sep="")

v <- merge(tp, merge(ep, rp, by.x = "match_string", by.y = "match_string"),
           by.x = "match_string", by.y = "match_string")
v$COU <- str_extract(v$match_string, ".{3}")
prop_analyzed <- v %>% group_by(COU) %>%
    summarize(p.CO = trunc(mean((RT.CO + PG.CO)/TotCO)*10^4)/10^4,
              p.NMVOC = trunc(mean((RT.NMVOC + PG.NMVOC)/TotNMVOC)*10^4)/10^4,
              p.NOX = trunc(mean((RT.NOX + PG.NOX)/TotNOX)*10^4)/10^4,
              p.PM10 = trunc(mean((RT.PM10 + PG.PM10)/TotPM10)*10^4)/10^4, 
              p.PM25 = trunc(mean((`RT.PM2-5` + `PG.PM2-5`)/`TotPM2-5`)*10^4)/10^4,
              p.SOX = trunc(mean((RT.SOX + PG.SOX)/TotSOX)*10^4)/10^4)
prop_analyzed <- ungroup(prop_analyzed)
prop_analyzed$Country <- code_lookup[prop_analyzed$COU]$Location
prop_analyzed$Country[8] <- "Czech Republic"
for (c in unique(road$Country.x)){
    df <- filter(road, c)

}
for (c in OECD){
    df <- filter(vehicle_pol, Country == c)
    new_df <- data.frame(Country = rep(c, 30), Year = 1990:2019)
    if (nrow(df) == 0){
        new_df
    }
    for (y in 1990:2019){

    }
}
graphlist <- list()
for (code in countries){
    series_data <- filter(pm25, COU == code)
    cname <- code_lookup[code][[1]]
    es <- emissions_standards %>% filter(Country == cname)
    t <- paste("PM2.5 Time Series for", cname)
    print(t)
    plt <- ggplot() +
        geom_line(data = series_data, aes(x = Year, y = Value)) +
        geom_vline(data = es, aes(xintercept = Date, col = NameShort)) +
        labs(title = t) + ylim(0,30)
    graphlist[[code]] <- plt
    ggsave(plot = plt, device = "png",
           filename = paste(code, ".png", sep = ""),
           width = 400, height = 200, unit = "mm")
}