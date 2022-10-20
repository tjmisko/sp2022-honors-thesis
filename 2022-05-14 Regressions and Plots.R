# This file runs the regressions and produces the regression tables and the 
# lineplots used in the "Something in the Air" working paper, Tristan Misko (Spring 2022)

# Load packages
library(dplyr)
library(lfe)
library(ggplot2)
library(gridExtra)
library(stargazer)


# import data
road_sb <- data.table(read.csv("road_sb.csv")) %>% select(-1)
road_gi <- data.table(read.csv("road_gi.csv")) %>% select(-1)
# regression for sb
regPK.1 <- felm(PKperCap ~ Legislation + Regulation + Policy + 
                    gas.price.usd.per.liter + RAILperCAP + Lagged2YInfraSpend | factor(YEA), data = road_sb)
regPK.2 <- felm(PKperCap ~ Legislation + Regulation + Policy + 
                    gas.price.usd.per.liter + RAILperCAP + Lagged2YInfraSpend + GDPperCAP| factor(YEA), data = road_sb)
regPK.3 <- felm(PKperCap ~ authorization.permit + basic.legislation + data.collection.reporting + 
                    electric.vehicle + emissions + emissions.trading + enforcement.compliance + environmental.standards + 
                    pollution.control + public.health + subsidy.incentive +
                    gas.price.usd.per.liter + RAILperCAP + Lagged2YInfraSpend | factor(YEA), data = road_sb)
regPK.4 <- felm(PKperCap ~ authorization.permit + basic.legislation + data.collection.reporting + 
                    electric.vehicle + emissions + emissions.trading + enforcement.compliance + environmental.standards + 
                    pollution.control + public.health + subsidy.incentive +
                    gas.price.usd.per.liter + RAILperCAP + Lagged2YInfraSpend + GDPperCAP| factor(YEA), data = road_sb)

stargazer(regPK.1, regPK.2, regPK.3, regPK.4, 
          se = list(summary(regPK.1, robust = T)$coefficients[,2],
                    summary(regPK.2, robust = T)$coefficients[,2],
                    summary(regPK.3, robust = T)$coefficients[,2],
                    summary(regPK.4, robust = T)$coefficients[,2]))
# Regressions for A.CO
regCO.1 <- felm(A.CO ~ Legislation + Regulation +
                    gas.price.usd.per.liter + RAILperCAP + Lagged2YInfraSpend | factor(YEA), data = road_gi)
regCO.2 <- felm(A.CO ~ Legislation + Regulation +
                    gas.price.usd.per.liter + RAILperCAP + Lagged2YInfraSpend + GDPperCAP| factor(YEA), data = road_gi)
regCO.3 <- felm(A.CO ~ authorization.permit + basic.legislation + data.collection.reporting + 
                    electric.vehicle + enforcement.compliance + environmental.standards + 
                    pollution.control + public.health + subsidy.incentive +
                    gas.price.usd.per.liter + RAILperCAP + Lagged2YInfraSpend | factor(YEA), data = road_gi)
regCO.4 <- felm(A.CO ~ authorization.permit + basic.legislation + data.collection.reporting + 
                    electric.vehicle + enforcement.compliance + environmental.standards + 
                    pollution.control + public.health + subsidy.incentive +
                    gas.price.usd.per.liter + RAILperCAP + Lagged2YInfraSpend + GDPperCAP | factor(YEA), data = road_gi)

stargazer(regCO.1, regCO.2, regCO.3, regCO.4, 
          se = list(summary(regCO.1, robust = T)$coefficients[,2],
                    summary(regCO.2, robust = T)$coefficients[,2],
                    summary(regCO.3, robust = T)$coefficients[,2],
                    summary(regCO.4, robust = T)$coefficients[,2]))

# Regressions for A.NMVOC
regNMVOC.1 <- felm(A.NMVOC ~ Legislation + Regulation +
                    gas.price.usd.per.liter + RAILperCAP + Lagged2YInfraSpend | factor(YEA), data = road_gi)
regNMVOC.2 <- felm(A.NMVOC ~ Legislation + Regulation +
                    gas.price.usd.per.liter + RAILperCAP + Lagged2YInfraSpend + GDPperCAP| factor(YEA), data = road_gi)
regNMVOC.3 <- felm(A.NMVOC ~ authorization.permit + basic.legislation + data.collection.reporting + 
                    electric.vehicle + enforcement.compliance + environmental.standards + 
                    pollution.control + public.health + subsidy.incentive +
                    gas.price.usd.per.liter + RAILperCAP + Lagged2YInfraSpend | factor(YEA), data = road_gi)
regNMVOC.4 <- felm(A.NMVOC ~ authorization.permit + basic.legislation + data.collection.reporting + 
                    electric.vehicle + enforcement.compliance + environmental.standards + 
                    pollution.control + public.health + subsidy.incentive +
                    gas.price.usd.per.liter + RAILperCAP + Lagged2YInfraSpend + GDPperCAP | factor(YEA), data = road_gi)

stargazer(regNMVOC.1, regNMVOC.2, regNMVOC.3, regNMVOC.4, 
          se = list(summary(regNMVOC.1, robust = T)$coefficients[,2],
                    summary(regNMVOC.2, robust = T)$coefficients[,2],
                    summary(regNMVOC.3, robust = T)$coefficients[,2],
                    summary(regNMVOC.4, robust = T)$coefficients[,2]))

# Regressions for A.NOX
regNOX.1 <- felm(A.NOX ~ Legislation + Regulation +
                       gas.price.usd.per.liter + RAILperCAP + Lagged2YInfraSpend | factor(YEA), data = road_gi)
regNOX.2 <- felm(A.NOX ~ Legislation + Regulation +
                       gas.price.usd.per.liter + RAILperCAP + Lagged2YInfraSpend + GDPperCAP| factor(YEA), data = road_gi)
regNOX.3 <- felm(A.NOX ~ authorization.permit + basic.legislation + data.collection.reporting + 
                       electric.vehicle + enforcement.compliance + environmental.standards + 
                       pollution.control + public.health + subsidy.incentive +
                       gas.price.usd.per.liter + RAILperCAP + Lagged2YInfraSpend | factor(YEA), data = road_gi)
regNOX.4 <- felm(A.NOX ~ authorization.permit + basic.legislation + data.collection.reporting + 
                       electric.vehicle + enforcement.compliance + environmental.standards + 
                       pollution.control + public.health + subsidy.incentive +
                       gas.price.usd.per.liter + RAILperCAP + Lagged2YInfraSpend + GDPperCAP | factor(YEA), data = road_gi)

stargazer(regNOX.1, regNOX.2, regNOX.3, regNOX.4, 
          se = list(summary(regNOX.1, robust = T)$coefficients[,2],
                    summary(regNOX.2, robust = T)$coefficients[,2],
                    summary(regNOX.3, robust = T)$coefficients[,2],
                    summary(regNOX.4, robust = T)$coefficients[,2]))

# Regressions for A.PM10
regPM10.1 <- felm(A.PM10 ~ Legislation + Regulation +
                     gas.price.usd.per.liter + RAILperCAP + Lagged2YInfraSpend | factor(YEA), data = road_gi)
regPM10.2 <- felm(A.PM10 ~ Legislation + Regulation +
                     gas.price.usd.per.liter + RAILperCAP + Lagged2YInfraSpend + GDPperCAP| factor(YEA), data = road_gi)
regPM10.3 <- felm(A.PM10 ~ authorization.permit + basic.legislation + data.collection.reporting + 
                     electric.vehicle + enforcement.compliance + environmental.standards + 
                     pollution.control + public.health + subsidy.incentive +
                     gas.price.usd.per.liter + RAILperCAP + Lagged2YInfraSpend | factor(YEA), data = road_gi)
regPM10.4 <- felm(A.PM10 ~ authorization.permit + basic.legislation + data.collection.reporting + 
                     electric.vehicle + enforcement.compliance + environmental.standards + 
                     pollution.control + public.health + subsidy.incentive +
                     gas.price.usd.per.liter + RAILperCAP + Lagged2YInfraSpend + GDPperCAP | factor(YEA), data = road_gi)

stargazer(regPM10.1, regPM10.2, regPM10.3, regPM10.4, 
          se = list(summary(regPM10.1, robust = T)$coefficients[,2],
                    summary(regPM10.2, robust = T)$coefficients[,2],
                    summary(regPM10.3, robust = T)$coefficients[,2],
                    summary(regPM10.4, robust = T)$coefficients[,2]))

# Regressions for A.PM2.5
regPM2.5.1 <- felm(A.PM2.5 ~ Legislation + Regulation +
                     gas.price.usd.per.liter + RAILperCAP + Lagged2YInfraSpend | factor(YEA), data = road_gi)
regPM2.5.2 <- felm(A.PM2.5 ~ Legislation + Regulation +
                     gas.price.usd.per.liter + RAILperCAP + Lagged2YInfraSpend + GDPperCAP| factor(YEA), data = road_gi)
regPM2.5.3 <- felm(A.PM2.5 ~ authorization.permit + basic.legislation + data.collection.reporting + 
                     electric.vehicle + enforcement.compliance + environmental.standards + 
                     pollution.control + public.health + subsidy.incentive +
                     gas.price.usd.per.liter + RAILperCAP + Lagged2YInfraSpend | factor(YEA), data = road_gi)
regPM2.5.4 <- felm(A.PM2.5 ~ authorization.permit + basic.legislation + data.collection.reporting + 
                     electric.vehicle + enforcement.compliance + environmental.standards + 
                     pollution.control + public.health + subsidy.incentive +
                     gas.price.usd.per.liter + RAILperCAP + Lagged2YInfraSpend + GDPperCAP | factor(YEA), data = road_gi)

stargazer(regPM2.5.1, regPM2.5.2, regPM2.5.3, regPM2.5.4, 
          se = list(summary(regPM2.5.1, robust = T)$coefficients[,2],
                    summary(regPM2.5.2, robust = T)$coefficients[,2],
                    summary(regPM2.5.3, robust = T)$coefficients[,2],
                    summary(regPM2.5.4, robust = T)$coefficients[,2]))

# Regressions for A.SOX
regSOX.1 <- felm(A.SOX ~ Legislation + Regulation +
                     gas.price.usd.per.liter + RAILperCAP + Lagged2YInfraSpend | factor(YEA), data = road_gi)
regSOX.2 <- felm(A.SOX ~ Legislation + Regulation +
                     gas.price.usd.per.liter + RAILperCAP + Lagged2YInfraSpend + GDPperCAP| factor(YEA), data = road_gi)
regSOX.3 <- felm(A.SOX ~ authorization.permit + basic.legislation + data.collection.reporting + 
                     electric.vehicle + enforcement.compliance + environmental.standards + 
                     pollution.control + public.health + subsidy.incentive +
                     gas.price.usd.per.liter + RAILperCAP + Lagged2YInfraSpend | factor(YEA), data = road_gi)
regSOX.4 <- felm(A.SOX ~ authorization.permit + basic.legislation + data.collection.reporting + 
                     electric.vehicle + enforcement.compliance + environmental.standards + 
                     pollution.control + public.health + subsidy.incentive +
                     gas.price.usd.per.liter + RAILperCAP + Lagged2YInfraSpend + GDPperCAP | factor(YEA), data = road_gi)

stargazer(regSOX.1, regSOX.2, regSOX.3, regSOX.4, 
          se = list(summary(regSOX.1, robust = T)$coefficients[,2],
                    summary(regSOX.2, robust = T)$coefficients[,2],
                    summary(regSOX.3, robust = T)$coefficients[,2],
                    summary(regSOX.4, robust = T)$coefficients[,2]))

# data
pg_sb <- data.table(read.csv("pg_sb.csv")) %>% select(-1)
pg_gi <- data.table(read.csv("pg_gi.csv")) %>% select(-1)
pg_sb$YEA <- as.numeric(str_extract(pg_sb$match_string, "\\d{4}"))
pg_gi$YEA <- as.numeric(str_extract(pg_gi$match_string, "\\d{4}"))
pg_sb$GDPperCAP <- pg_sb$GDPperCAP/1000
pg_gi$GDPperCAP <- pg_gi$GDPperCAP/1000
# regression for sb
regPK.1 <- felm(s ~ Legislation + Regulation + Policy + GDPperCAP | factor(YEA), data = pg_sb)
regPK.2 <- felm(s ~ bioenergy + biofuel + hydropower.generation + nuclear.energy + renewable.energy + GDPperCAP | factor(YEA), data=pg_sb)
regPK.3 <- felm(s ~ authorization.permit + basic.legislation + data.collection.reporting + 
                    emissions + emissions.trading + enforcement.compliance + environmental.standards + 
                    pollution.control + public.health + subsidy.incentive + GDPperCAP | factor(YEA), data = pg_sb)
regPK.4 <- felm(s ~ authorization.permit + basic.legislation + data.collection.reporting + 
                    emissions + emissions.trading + enforcement.compliance + environmental.standards + 
                    pollution.control + public.health + subsidy.incentive + GDPperCAP| factor(YEA), data = pg_gi)

stargazer(regPK.1, regPK.2, regPK.3, regPK.4, 
          se = list(summary(regPK.1, robust = T)$coefficients[,2],
                    summary(regPK.2, robust = T)$coefficients[,2],
                    summary(regPK.3, robust = T)$coefficients[,2],
                    summary(regPK.4, robust = T)$coefficients[,2]))
# Regressions for A.CO
regCO.1 <- felm(A.CO ~ Legislation + Regulation + Policy + GDPperCAP | factor(YEA), data = pg_sb)
regCO.2 <- felm(A.CO ~ bioenergy + biofuel + hydropower.generation + nuclear.energy + renewable.energy + GDPperCAP | factor(YEA), data=pg_sb)
regCO.3 <- felm(A.CO ~ authorization.permit + basic.legislation + data.collection.reporting + 
                    emissions + emissions.trading + enforcement.compliance + environmental.standards + 
                    pollution.control + public.health + subsidy.incentive + GDPperCAP | factor(YEA), data = pg_sb)
regCO.4 <- felm(A.CO ~ authorization.permit + basic.legislation + data.collection.reporting + 
                    emissions + emissions.trading + enforcement.compliance + environmental.standards + 
                    pollution.control + public.health + subsidy.incentive + GDPperCAP| factor(YEA), data = pg_gi)

stargazer(regCO.1, regCO.2, regCO.3, regCO.4, 
          se = list(summary(regCO.1, robust = T)$coefficients[,2],
                    summary(regCO.2, robust = T)$coefficients[,2],
                    summary(regCO.3, robust = T)$coefficients[,2],
                    summary(regCO.4, robust = T)$coefficients[,2]))

# Regressions for A.NMVOC
regNMVOC.1 <- felm(A.NMVOC ~ Legislation + Regulation + Policy + GDPperCAP | factor(YEA), data = pg_sb)
regNMVOC.2 <- felm(A.NMVOC ~ bioenergy + biofuel + hydropower.generation + nuclear.energy + renewable.energy  + GDPperCAP | factor(YEA), data=pg_sb)
regNMVOC.3 <- felm(A.NMVOC ~ authorization.permit + basic.legislation + data.collection.reporting + 
                    emissions + emissions.trading + enforcement.compliance + environmental.standards + 
                    pollution.control + public.health + subsidy.incentive + GDPperCAP | factor(YEA), data = pg_sb)
regNMVOC.4 <- felm(A.NMVOC ~ authorization.permit + basic.legislation + data.collection.reporting + 
                    emissions + emissions.trading + enforcement.compliance + environmental.standards + 
                    pollution.control + public.health + subsidy.incentive + GDPperCAP| factor(YEA), data = pg_gi)

stargazer(regNMVOC.1, regNMVOC.2, regNMVOC.3, regNMVOC.4, 
          se = list(summary(regNMVOC.1, robust = T)$coefficients[,2],
                    summary(regNMVOC.2, robust = T)$coefficients[,2],
                    summary(regNMVOC.3, robust = T)$coefficients[,2],
                    summary(regNMVOC.4, robust = T)$coefficients[,2]))

# Regressions for A.NOX
regNOX.1 <- felm(A.NOX ~ Legislation + Regulation + Policy + GDPperCAP | factor(YEA), data = pg_sb)
regNOX.2 <- felm(A.NOX ~ bioenergy + biofuel + hydropower.generation + nuclear.energy + renewable.energy  + GDPperCAP | factor(YEA), data=pg_sb)
regNOX.3 <- felm(A.NOX ~ authorization.permit + basic.legislation + data.collection.reporting + 
                       emissions + emissions.trading + enforcement.compliance + environmental.standards + 
                       pollution.control + public.health + subsidy.incentive + GDPperCAP | factor(YEA), data = pg_sb)
regNOX.4 <- felm(A.NOX ~ authorization.permit + basic.legislation + data.collection.reporting + 
                       emissions + emissions.trading + enforcement.compliance + environmental.standards + 
                       pollution.control + public.health + subsidy.incentive + GDPperCAP| factor(YEA), data = pg_gi)

stargazer(regNOX.1, regNOX.2, regNOX.3, regNOX.4, 
          se = list(summary(regNOX.1, robust = T)$coefficients[,2],
                    summary(regNOX.2, robust = T)$coefficients[,2],
                    summary(regNOX.3, robust = T)$coefficients[,2],
                    summary(regNOX.4, robust = T)$coefficients[,2]))


# Regressions for A.PM10
regPM10.1 <- felm(A.PM10 ~ Legislation + Regulation + Policy + GDPperCAP | factor(YEA), data = pg_sb)
regPM10.2 <- felm(A.PM10 ~ bioenergy + biofuel + hydropower.generation + nuclear.energy + renewable.energy  + GDPperCAP | factor(YEA), data=pg_sb)
regPM10.3 <- felm(A.PM10 ~ authorization.permit + basic.legislation + data.collection.reporting + 
                     emissions + emissions.trading + enforcement.compliance + environmental.standards + 
                     pollution.control + public.health + subsidy.incentive + GDPperCAP | factor(YEA), data = pg_sb)
regPM10.4 <- felm(A.PM10 ~ authorization.permit + basic.legislation + data.collection.reporting + 
                     emissions + emissions.trading + enforcement.compliance + environmental.standards + 
                     pollution.control + public.health + subsidy.incentive + GDPperCAP| factor(YEA), data = pg_gi)

stargazer(regPM10.1, regPM10.2, regPM10.3, regPM10.4, 
          se = list(summary(regPM10.1, robust = T)$coefficients[,2],
                    summary(regPM10.2, robust = T)$coefficients[,2],
                    summary(regPM10.3, robust = T)$coefficients[,2],
                    summary(regPM10.4, robust = T)$coefficients[,2]))

# Regressions for A.PM2.5
regPM2.5.1 <- felm(A.PM2.5 ~ Legislation + Regulation + Policy + GDPperCAP | factor(YEA), data = pg_sb)
regPM2.5.2 <- felm(A.PM2.5 ~ bioenergy + biofuel + hydropower.generation + nuclear.energy + renewable.energy  + GDPperCAP | factor(YEA), data=pg_sb)
regPM2.5.3 <- felm(A.PM2.5 ~ authorization.permit + basic.legislation + data.collection.reporting + 
                      emissions + emissions.trading + enforcement.compliance + environmental.standards + 
                      pollution.control + public.health + subsidy.incentive + GDPperCAP | factor(YEA), data = pg_sb)
regPM2.5.4 <- felm(A.PM2.5 ~ authorization.permit + basic.legislation + data.collection.reporting + 
                      emissions + emissions.trading + enforcement.compliance + environmental.standards + 
                      pollution.control + public.health + subsidy.incentive + GDPperCAP| factor(YEA), data = pg_gi)

stargazer(regPM2.5.1, regPM2.5.2, regPM2.5.3, regPM2.5.4, 
          se = list(summary(regPM2.5.1, robust = T)$coefficients[,2],
                    summary(regPM2.5.2, robust = T)$coefficients[,2],
                    summary(regPM2.5.3, robust = T)$coefficients[,2],
                    summary(regPM2.5.4, robust = T)$coefficients[,2]))

# Regressions for A.SOX
regSOX.1 <- felm(A.SOX ~ Legislation + Regulation + Policy + GDPperCAP | factor(YEA), data = pg_sb)
regSOX.2 <- felm(A.SOX ~ bioenergy + biofuel + hydropower.generation + nuclear.energy + renewable.energy + GDPperCAP| factor(YEA), data=pg_sb)
regSOX.3 <- felm(A.SOX ~ authorization.permit + basic.legislation + data.collection.reporting + 
                     emissions + emissions.trading + enforcement.compliance + environmental.standards + 
                     pollution.control + public.health + subsidy.incentive + GDPperCAP | factor(YEA), data = pg_sb)
regSOX.4 <- felm(A.SOX ~ authorization.permit + basic.legislation + data.collection.reporting + 
                     emissions + emissions.trading + enforcement.compliance + environmental.standards + 
                     pollution.control + public.health + subsidy.incentive + GDPperCAP| factor(YEA), data = pg_gi)

stargazer(regSOX.1, regSOX.2, regSOX.3, regSOX.4, 
          se = list(summary(regSOX.1, robust = T)$coefficients[,2],
                    summary(regSOX.2, robust = T)$coefficients[,2],
                    summary(regSOX.3, robust = T)$coefficients[,2],
                    summary(regSOX.4, robust = T)$coefficients[,2]))

PK.q <- road %>% group_by(YEAR) %>%
    summarize(q00 = quantile(PKperCap, 0),
              q20 = quantile(PKperCap, 0.2),
              q40 = quantile(PKperCap, 0.4),
              q50 = quantile(PKperCap, 0.5),
              q60 = quantile(PKperCap, 0.6),
              q80 = quantile(PKperCap, 0.8),
              q100 = quantile(PKperCap, 0.9))

PK.stripe_plot <- ggplot(PK.q, aes(x = YEAR)) +
    geom_area(aes(y = q100), fill = "darkred", alpha = 0.7) +
    geom_area(aes(y = q80), fill = "coral4", alpha = 0.7) +
    geom_area(aes(y = q60), fill = "coral3", alpha = 0.7) +
    geom_area(aes(y = q40), fill = "coral2", alpha = 0.7) +
    geom_area(aes(y = q20), fill = "coral1", alpha = 0.7) +
    geom_area(aes(y = q00), fill = "coral", alpha = 0.7) +
    geom_line(aes(y = q50), size = 1.25) +
    ylab("Passenger Kilometers") +
    labs(title = "Passenger Kilometers by Year",
         subtitle = "Source Behavior Proxy for Road Transport (US Values Surpressed for Visibility)")
PK.stripe_plot


A.CO.q <- road %>% group_by(YEAR) %>%
    summarize(q00 = quantile(A.CO, 0),
              q20 = quantile(A.CO, 0.2),
              q40 = quantile(A.CO, 0.4),
              q50 = quantile(A.CO, 0.5),
              q60 = quantile(A.CO, 0.6),
              q80 = quantile(A.CO, 0.8),
              q100 = quantile(A.CO, 1))

A.CO.stripe_plot <- ggplot(A.CO.q, aes(x = YEAR)) +
    geom_area(aes(y = q100), fill = "darkred", alpha = 0.7) +
    geom_area(aes(y = q80), fill = "coral4", alpha = 0.7) +
    geom_area(aes(y = q60), fill = "coral3", alpha = 0.7) +
    geom_area(aes(y = q40), fill = "coral2", alpha = 0.7) +
    geom_area(aes(y = q20), fill = "coral1", alpha = 0.7) +
    geom_area(aes(y = q00), fill = "coral", alpha = 0.7) +
    geom_line(aes(y = q50), size = 1.25) +
    ylab("A.CO") +
    labs(title = "Carbon Monoxide Generation Intensity by Year",
         subtitle = "Road Tansport Sources, 20% Quantiles with Median in Black")
A.CO.stripe_plot


A.NOX.q <- road %>% group_by(YEAR) %>%
    summarize(q00 = quantile(A.NOX, 0),
              q20 = quantile(A.NOX, 0.2),
              q40 = quantile(A.NOX, 0.4),
              q50 = quantile(A.NOX, 0.5),
              q60 = quantile(A.NOX, 0.6),
              q80 = quantile(A.NOX, 0.8),
              q100 = quantile(A.NOX, 1))

A.NOX.stripe_plot <- ggplot(A.NOX.q, aes(x = YEAR)) +
    geom_area(aes(y = q100), fill = "darkred", alpha = 0.7) +
    geom_area(aes(y = q80), fill = "coral4", alpha = 0.7) +
    geom_area(aes(y = q60), fill = "coral3", alpha = 0.7) +
    geom_area(aes(y = q40), fill = "coral2", alpha = 0.7) +
    geom_area(aes(y = q20), fill = "coral1", alpha = 0.7) +
    geom_area(aes(y = q00), fill = "coral", alpha = 0.7) +
    geom_line(aes(y = q50), size = 1.25) +
    ylab("A.NOX") +
    labs(title = "Nitrogen Oxide Generation Intensity by Year",
         subtitle = "Road Transport Sources, 20% Quantiles with Median in Black")
A.NOX.stripe_plot

A.NMVOC.q <- road %>% group_by(YEAR) %>%
    summarize(q00 = quantile(A.NMVOC, 0),
              q20 = quantile(A.NMVOC, 0.2),
              q40 = quantile(A.NMVOC, 0.4),
              q50 = quantile(A.NMVOC, 0.5),
              q60 = quantile(A.NMVOC, 0.6),
              q80 = quantile(A.NMVOC, 0.8),
              q100 = quantile(A.NMVOC, 1))

A.NMVOC.stripe_plot <- ggplot(A.NMVOC.q, aes(x = YEAR)) +
    geom_area(aes(y = q100), fill = "darkred", alpha = 0.7) +
    geom_area(aes(y = q80), fill = "coral4", alpha = 0.7) +
    geom_area(aes(y = q60), fill = "coral3", alpha = 0.7) +
    geom_area(aes(y = q40), fill = "coral2", alpha = 0.7) +
    geom_area(aes(y = q20), fill = "coral1", alpha = 0.7) +
    geom_area(aes(y = q00), fill = "coral", alpha = 0.7) +
    geom_line(aes(y = q50), size = 1.25) +
    ylab("A.NMVOC") +
    labs(title = "NMVOC Generation Intensity by Year",
         subtitle = "Road Transport Sources, 20% Quantiles with Median in Black")
A.NMVOC.stripe_plot

A.PM10.q <- road %>% group_by(YEAR) %>%
    summarize(q00 = quantile(A.PM10, 0, na.rm = T),
              q20 = quantile(A.PM10, 0.2, na.rm = T),
              q40 = quantile(A.PM10, 0.4, na.rm = T),
              q50 = quantile(A.PM10, 0.5, na.rm = T),
              q60 = quantile(A.PM10, 0.6, na.rm = T),
              q80 = quantile(A.PM10, 0.8, na.rm = T),
              q100 = quantile(A.PM10, 1, na.rm = T))

A.PM10.stripe_plot <- ggplot(A.PM10.q, aes(x = YEAR)) +
    geom_area(aes(y = q100), fill = "darkred", alpha = 0.7) +
    geom_area(aes(y = q80), fill = "coral4", alpha = 0.7) +
    geom_area(aes(y = q60), fill = "coral3", alpha = 0.7) +
    geom_area(aes(y = q40), fill = "coral2", alpha = 0.7) +
    geom_area(aes(y = q20), fill = "coral1", alpha = 0.7) +
    geom_area(aes(y = q00), fill = "coral", alpha = 0.7) +
    geom_line(aes(y = q50), size = 1.25) +
    ylab("A.PM10") +
    labs(title = "Particulate Matter (PM10) Generation Intensity by Year",
         subtitle = "Road Transport Sources, 20% Quantiles with Median in Black")
A.PM10.stripe_plot

A.PM25.q <- road %>% group_by(YEAR) %>%
    summarize(q00 = quantile(A.PM2.5, 0, na.rm = T),
              q20 = quantile(A.PM2.5, 0.2, na.rm = T),
              q40 = quantile(A.PM2.5, 0.4, na.rm = T),
              q50 = quantile(A.PM2.5, 0.5, na.rm = T),
              q60 = quantile(A.PM2.5, 0.6, na.rm = T),
              q80 = quantile(A.PM2.5, 0.8, na.rm = T),
              q100 = quantile(A.PM2.5, 1, na.rm = T))

A.PM25.stripe_plot <- ggplot(A.PM25.q, aes(x = YEAR)) +
    geom_area(aes(y = q100), fill = "darkred", alpha = 0.7) +
    geom_area(aes(y = q80), fill = "coral4", alpha = 0.7) +
    geom_area(aes(y = q60), fill = "coral3", alpha = 0.7) +
    geom_area(aes(y = q40), fill = "coral2", alpha = 0.7) +
    geom_area(aes(y = q20), fill = "coral1", alpha = 0.7) +
    geom_area(aes(y = q00), fill = "coral", alpha = 0.7) +
    geom_line(aes(y = q50), size = 1.25) +
    ylab("A.PM2.5") +
    labs(title = "Particulate Matter (PM2.5) Generation Intensity by Year",
         subtitle = "Road Transport Sources, 20% Quantiles with Median in Black")
A.PM25.stripe_plot


A.SOX.q <- road %>% group_by(YEAR) %>%
    summarize(q00 = quantile(A.SOX, 0),
              q20 = quantile(A.SOX, 0.2),
              q40 = quantile(A.SOX, 0.4),
              q50 = quantile(A.SOX, 0.5),
              q60 = quantile(A.SOX, 0.6),
              q80 = quantile(A.SOX, 0.8),
              q100 = quantile(A.SOX, 1))

A.SOX.stripe_plot <- ggplot(A.SOX.q, aes(x = YEAR)) +
    geom_area(aes(y = q100), fill = "darkred", alpha = 0.7) +
    geom_area(aes(y = q80), fill = "coral4", alpha = 0.7) +
    geom_area(aes(y = q60), fill = "coral3", alpha = 0.7) +
    geom_area(aes(y = q40), fill = "coral2", alpha = 0.7) +
    geom_area(aes(y = q20), fill = "coral1", alpha = 0.7) +
    geom_area(aes(y = q00), fill = "coral", alpha = 0.7) +
    geom_line(aes(y = q50), size = 1.25) +
    ylab("A.SOX") +
    labs(title = "Sulfur Oxide Generation Intensity by Year",
         subtitle = "Road Transport Sources, 20% Quantiles with Median in Black")
A.SOX.stripe_plot

A_plt_grid <- grid.arrange(A.CO.stripe_plot, A.NOX.stripe_plot, A.NMVOC.stripe_plot,
                             A.PM10.stripe_plot, A.PM25.stripe_plot, A.SOX.stripe_plot, nrow = 3)
A_plt_grid
stargazer(regPK.base, regPK.time, regPK.pc.base, regPK.pc.time, regPK.rich.base, regPK.rich.time)
stargazer(regCO.base, regCO.time, regCO.pc.base, regCO.pc.time, regCO.rich.base, regCO.rich.time)
stargazer(regNOX.base, regNOX.time, regNOX.pc.base, regNOX.pc.time, regNOX.rich.base, regNOX.rich.time)
stargazer(regNMVOC.base, regNMVOC.time, regNMVOC.pc.base, regNMVOC.pc.time, regNMVOC.rich.base, regNMVOC.rich.time)
stargazer(regPM25.base, regPM25.time, regPM25.pc.base, regPM25.pc.time, regPM25.rich.base, regPM25.rich.time)
stargazer(regPM10.base, regPM10.time, regPM10.pc.base, regPM10.pc.time, regPM10.rich.base, regPM10.rich.time)
stargazer(regSOX.base, regSOX.time, regSOX.pc.base, regSOX.pc.time, regSOX.rich.base, regSOX.rich.time)