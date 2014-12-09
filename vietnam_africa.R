rm(list=ls())
library(WDI)
library(ggplot2)
library(plyr)
library(cluster)

startyear <- 1975
endyear <- 2011
wanted_indicator <-c("NY.GDP.MKTP.KD", "NY.GDP.PCAP.KD", "SP.POP.TOTL",
  "NV.AGR.TOTL.ZS","NE.EXP.GNFS.ZS", "NY.GDP.TOTL.RT.ZS")
data_raw <- WDI(country = "all", indicator = wanted_indicator,
            start = startyear, end = endyear, extra = TRUE)


#Subset the data to include Vietnam and Sub-Saharan countries only
data <- subset(data_raw,
               region=="Sub-Saharan Africa (all income levels)" | country=="Vietnam",
               select = country:NY.GDP.TOTL.RT.ZS)
names(data) <- c("country", "year", "gdp", "gdp.pc", "pop", "agriculture.%gdp", "export.%gdp", "resource.%gdp")

# Plot

full_plot <- ggplot(data=data, aes(x=year, y=gdp.pc)) +
  geom_line(aes(group=country), alpha=0.4)
vietnam <- geom_line(data=subset(data, country=="Vietnam"), col="red")
outlier_names <- geom_text(data=subset(data, gdp.pc > quantile(gdp.pc, probs=0.8, na.rm=T) & year == 2010),
            aes(label=country))

full_plot + vietnam
full_plot + vietnam + outlier_names
full_plot + vietnam + outlier_names + coord_cartesian(ylim=c(0, 2000)) + geom_smooth(method="lm")
full_plot + vietnam + outlier_names + coord_cartesian(ylim=c(0, 1500)) + geom_smooth(method="lm")
# Similarity

data_avg <- ddply(data, .(country), colwise(mean, na.rm=TRUE))
data_sim <- data_avg[ , c("pop", "agriculture.%gdp", "export.%gdp", "resource.%gdp")]
rownames(data_sim) <- data_avg$country

vietnam_idx <- which(data_sim$country=="Vietnam")
distance <- as.matrix(daisy(data_sim, metric="gower"))[vietnam_idx, ]


data_group <- subset(data, country %in% names(distance)[distance < median(distance, na.rm=TRUE)])
ggplot(data=data_group, aes(x=year, y=gdp.pc)) +
  geom_line(aes(group=country), alpha=0.4) +
  geom_line(data=subset(data, country=="Vietnam"), col="red") +
  geom_point(data=subset(data, country=="Vietnam"), col="blue", shape=1)
