rm(list=ls())
library(WDI)
library(ggplot2)
library(plyr)
library(cluster)
library(RColorBrewer)
library(dplyr)

startyear <- 1975
endyear <- 2011

# ---- Download data ----
wanted_indicator <-c("NY.GDP.MKTP.KD", "NY.GDP.PCAP.KD",
                     "SP.DYN.IMRT.IN", "SP.DYN.LE00.IN")
data_raw <- WDI(country = "all", indicator = wanted_indicator,
                start = startyear, end = endyear, extra = TRUE)
data_raw <- data_raw %>%
  filter(region != "Aggregates") %>%
  filter(!is.na(region)) %>%
  arrange(region)

# ---- Construct similarity ----
region <- c("Sub-Saharan Africa (all income levels)",
            "Latin America & Caribbean (all income levels)")
wanted_variables <- c("region", "country", "year",
                      "NY.GDP.PCAP.KD", "SP.DYN.IMRT.IN", "SP.DYN.LE00.IN")
data_region <- data_raw[data_raw[['region']] %in% region , wanted_variables]
data_avg <- ddply(data_region, .(region, country), colwise(mean, na.rm=TRUE))

data_sim <- data_avg[ , !names(data_avg) %in% c("region", "country", "year")]
rownames(data_sim) <- data_avg$country
data_sim <- na.omit(data_sim)

distance_matrix <- as.matrix(daisy(data_sim, metric="gower"))

pdf("gower_similarity.pdf", height=7, width=7)
sim_heatmap <- heatmap(distance_matrix, Rowv=NA, Colv=NA,
                       col=brewer.pal(7, "Oranges"), scale="column",
                       margins=c(5,10))
dev.off()