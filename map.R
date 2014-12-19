rm(list=ls())
source("functions.R")
packs <- c("rgdal", "rgeos", "maptools", "spdep", "spatstat", "raster",
           "ggplot2", "reshape2", "scales")
f_install_and_load(packs)

ds <- readOGR(dsn="data/", layer="kenya")
summary(ds)
str(ds, max.level=2)
dsdat <- as(ds, "data.frame")

plot(ds)

d <- read.csv("data/kenpop89to99.csv")
summary(d)

d <- d[, c("ip89DId", "PopChg", "BrateChg", "Y89Pop","Y99Pop")]
summary(d)
nrow(d)
d <- unique(d)

# ---- Join data with shapefiles data ----

ds@data <- data.frame(as(ds, "data.frame"),
                      d[match(ds@data[, "ip89DId"], d[, "ip89DId"]), ])

# ---- Generate random points ----

win <- bbox(ds)
win <- t(win)
win <- as.vector(win)

dran <- runifpoint(100, win=as.vector(t(bbox(ds))))
plot(ds)
plot(dran, add=T)

# ---- Convert random points to data.frame ----
dp <- as.data.frame(dran)
dp$values <- rnorm(100, 5, 10)
head(dp)

# ---- Convert random points to spatial points data frame ----
dsp <- SpatialPointsDataFrame(coords=dp[ , c("x", "y")],
                              data=data.frame(values=dp$values))
dsp@proj4string <- ds@proj4string

# ---- Point in poly join ----
dsdat <- over(ds, dsp, fn=mean)
head(dsdat)
dsdat
dsp@data
ds@data

ds@data[rownames(dsdat), "pntvals"] <- dsdat
head(ds@data)

# ---- Read and crop a raster ----
g <- readGDAL(fname="data/anom.2000.03.tiff")
g <- raster(g)
plot(g)
plot(ds, add=T)

gc <- crop(g, ds)
plot(gc)
plot(ds, add=T)

ds@data$precip <- extract(gc, ds, fun=mean, weights=FALSE)
extract(gc, ds, weights=FALSE)

# ---- Make maps in ggplot2 ----
pds <- fortify(ds, region="ip89DId")
pds$ip89DId <- as.integer(pds$id)

p1 <- ggplot(data=d) + geom_map(aes(fill=PopChg, map_id=ip89DId), map=pds) +
  expand_limits(x = pds$long, y = pds$lat) +
  coord_equal() +
  xlab("Basic map with default elements")

p1 + scale_fill_gradient(name="Population \nChange", low="wheat", high="steelblue") +
  guides(fill="colorbar")

cens <- as.data.frame(coordinates(ds))
#extract the coordinates for centroid of each polygon
cens$Region <- ds$ip89DName
cens$ip89DId <- ds$ip89DId

p1 + geom_text(data = cens, aes(V1, V2, label = Region), size = 2.5, vjust = 1)
