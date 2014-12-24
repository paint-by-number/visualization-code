rm(list=ls())
source("../functions.R")

packs <- c("ggplot2", "cshapes", "foreign", "animation", "gridExtra", "countrycode", "dplyr")
f_install_and_load(packs)

dd_raw <- read.dta("data/ddrevisited.dta")
dd <- dd_raw[ , c("cowcode", "year", "ctryname", "democracy")]
dd$democracy <- factor(dd$democracy)

blank <- theme(axis.line=element_blank(),axis.text.x=element_blank(),
               axis.text.y=element_blank(),axis.ticks=element_blank(),
               axis.title.x=element_blank(),
               axis.title.y=element_blank(),legend.position="none",
               panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
               panel.grid.minor=element_blank(),plot.background=element_blank())

gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}


saveHTML({
  for (i in 1946:2008) {
    # World map
    world <- cshp(date=as.Date(paste0(i , "-1-1")))
    world.points <- fortify(world, region='COWCODE')
    world.df <- merge(world.points, dd[dd$year==i, ], by.x="id", by.y="cowcode", all.x=TRUE)
    world.df <- world.df[order(world.df$order), ]
    p.map <- ggplot(world.df, aes(long, lat, group=group, fill=democracy)) +
      geom_polygon() +
      labs(title=paste("Democracy and Dictatorship, year", i))

    # Country list
    d <- unique(dd[dd$democracy==0, "cowcode"])
    d <- d[!is.na(d)]
    num_col <- 6
    num_row <- ceiling(length(d) / num_col)

    df_full <- data.frame(cowcode=d,
      country=countrycode(d, origin="cown", dest="country.name")) %>%
      arrange(country)
    df <- cbind.data.frame(df_full,
      x=floor(1:length(d) / num_row),
      y=(length(d)-1):0 %% num_row,
      democracy=as.factor(df_full$cowcode %in% dd[dd$democracy==0 & dd$year==i, "cowcode"]))

    p.name <- ggplot(data=df, aes(x, y, label=country, col=democracy)) +
      geom_text(hjust=0, size=4) + scale_color_manual(values=c("#EDF2F1", "black")) +
      expand_limits(x=c(0, num_col + 0.5)) + blank

    # Time series
    df_ts <- ddply(dd, .(year), summarize,
      mean_dem = mean(as.numeric(as.character(democracy)), na.rm=TRUE),
      count_dem = sum(as.numeric(as.character(democracy)), na.rm=TRUE),
      count_na = sum(is.na((democracy))),
      count_total = sum(!is.na((democracy)))
    )
    p.ts <- ggplot(data=df_ts[df_ts$year <= i, ], aes(x=year, y=mean_dem)) +
      geom_line(col=gg_color_hue(2)[2]) +
      expand_limits(x=c(1946, 2008), y=c(0, 1)) +
      scale_x_continuous(breaks=c(1946, seq(1950, 2005, by=5), 2008)) +
      ggtitle(paste("Proportion of Democracy =",
                     df_ts[df_ts$year==i, "count_dem"], "/",
                     df_ts[df_ts$year==i, "count_total"])) +
      ylab("proportion")

    # Plot the three plots
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(2, 2)))
    vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
    print(p.map, vp = vplayout(1, 1))
    print(p.ts, vp = vplayout(2, 1))
    print(p.name, vp = vplayout(1:2, 2))
  }
}, img.name="dd_map", interval=1,
ani.width=1200, ani.height=600, loop=FALSE,
title="Democracy and Dictatorship over Time",
outdir=getwd())


