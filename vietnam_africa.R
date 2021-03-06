rm(list=ls())
library(WDI)
library(ggplot2)

startyear <- 1975
endyear <- 2011

# ---- Download data ----
wanted_indicator <-c("NY.GDP.MKTP.KD", "NY.GDP.PCAP.KD", "SP.POP.TOTL",
  "NV.AGR.TOTL.ZS","NE.EXP.GNFS.ZS", "NY.GDP.TOTL.RT.ZS")
data_raw <- WDI(country = "all", indicator = wanted_indicator,
            start = startyear, end = endyear, extra = TRUE)

# ---- Subset and rename data ----
data <- subset(data_raw,
               region=="Sub-Saharan Africa (all income levels)" | country=="Vietnam",
               select = country:NY.GDP.TOTL.RT.ZS)
names(data) <- c("country", "year", "gdp", "gdp.pc", "pop",
                 "agriculture.%gdp", "export.%gdp", "resource.%gdp")

# ---- Plotting ----

# With ggplot2, we can create individual elements that can be joined later
# base_plot: time series of all countries in the dataset
base_plot <- ggplot(data=data, aes(x=year, y=gdp.pc)) +
  geom_line(aes(group=country), alpha=0.2)
# vietnam_line: time series of Vietnam
vietnam_line <- geom_line(data=subset(data, country=="Vietnam"),
                          col="red", size=1.3)
# outlier_names: names of the countries with "outlier" gdp.pc (top 20% quantile)
outlier_names <- geom_text(data=subset(data, gdp.pc > quantile(gdp.pc, probs=0.8, na.rm=T) & year == 2010),
                           aes(label=country))
# median_line: the median line summarizing African countries
median_line <- stat_summary(data=subset(data, country!="Vietnam"),
                            fun.data="median_hilow",
                            geom="smooth", conf.int=0.5)
labels <- labs(title="Vietnam-African countries comparison",
       x="Year", y="GDP per capita (2005 USD)")

# Now we can add these individual elements together as we wish
base_plot
base_plot + vietnam_line

base_plot + vietnam_line + outlier_names +
  theme_minimal() + labels
ggsave("vietnam_africa_full.pdf")

base_plot + vietnam_line + outlier_names +
  median_line + coord_cartesian(ylim=c(0, 1500)) +
  theme_minimal() + labels
ggsave("vietnam_africa.pdf")
