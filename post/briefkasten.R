library(ggplot2)
library(dplyr)
library(raster)
library(geosphere)
library(tictoc)
library(stringr)

# Daten einlesen
dat <- read.csv("./data/zugangspunkte-post.csv", header=TRUE, sep=";")

# Zusammenfassung der Spalte "poityp_de" mit "Filiale",
# "Postfachanlage", "Briefeinwurf" etc.
summary(dat["poityp_de"])

# Anzahl der Filialen mit verschiedenen "geopoint"-Koordinaten
filialen = dat[dat["poityp_de"]=="Filiale",]
paste("Verschiedene Filialen: ", dim(unique(filialen["geopoint"]))[1])

# Anzahl Postfachanlagen mit verschiedenen "geopoint"-Koordinaten
postfachanlagen = dat[dat["poityp_de"]=="Postfachanlage",]
paste("Verschiedene Postfachanlagen: ",
            dim(unique(postfachanlagen["geopoint"]))[1])

bk = dat[dat["poityp_de"]=="Briefeinwurf",]
paste("Verschiedene Briefkästen: ", dim(unique(bk["geopoint"]))[1])

a = table(bk["geopoint"])
head(a[order(a, decreasing=T)])

bk[bk["geopoint"]=="46.23077572, 6.10762985", "POIName_de"]

kantbk = table(bk["address_kantoncode"])
kantbk = kantbk[order(-kantbk)]
kantbk

kantbk <- data.frame(kantbk[1:26])
colnames(kantbk) <- c("Kanton", "Anzahl")

p <- ggplot(data=kantbk, aes(x=Kanton, y=Anzahl)) +
   geom_bar(stat="identity", fill="steelblue") +
   geom_text(aes(label=Anzahl), vjust=0, hjust=0.5, size=2.8, angle=0, color="black") +
   xlab("\nKanton") +
   ylab("Anzahl Briefkästen\n") +
   theme_minimal()

p

bkcoord <- str_split_fixed(bk$geopoint, ",", 2)
coords <- data.frame(cbind(as.numeric(bkcoord[,1]), as.numeric(bkcoord[,2])))
colnames(coords) <- c("lat", "lon")
bk["lat"] = coords["lat"]
bk["lon"] = coords["lon"]
coordinates(coords) <- c("lon", "lat")
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
proj4string(coords) <- crs.geo
summary(coords)
ch <- getData("GADM", country = "CHE", level = 0)
png(file="figures/alle-briefkasten-map.png",width=800,height=600)
plot(coords, pch = 20, cex=0.8, col = "steelblue")
plot(ch, add = T)

bk[which.max(bk$lat), c("POIName_de", "geopoint")]
bk[which.min(bk$lat), c("POIName_de", "geopoint")]
bk[which.min(bk$lon), c("POIName_de", "geopoint")]
bk[which.max(bk$lon), c("POIName_de", "geopoint")]

maxN = which.max(bk$lat)
maxS = which.min(bk$lat)
maxW = which.min(bk$lon)
maxE = which.max(bk$lon)

NSWE <- distm(bk[c(maxN,maxW) ,c('lon','lat')], bk[c(maxS, maxE),c('lon','lat')], fun=distHaversine)
paste("Nord-Süd: ", round(NSWE[1,1]/1000,3), "km")
paste("West-Ost: ", round(NSWE[2,2]/1000,3), "km")

require("geosphere")
tic("Berechnung der Distanzmatrix")
mat <- distm(bk[,c('lon','lat')], bk[,c('lon','lat')], fun=distHaversine)
toc()

col_min <- function(colnr, matr){
    # Minimum und position des Minimums in jeder Spalte.
    m <- which.min(matr[,colnr])
    value <- matr[m,colnr]
    c(m, colnr, value)
}

get_remotest_n <- function(matr, n){
   # Berechnet das grösste Spaltenminimum und die Position in der Matrix.
   nr_cols = dim(matr)[2]
   colmins = sapply(1:nr_cols, function(j) col_min(j, matr))
   colmins = colmins[,order(-colmins[3,])]
   colmins[,1:n]
}

mat[mat==0] <- NA
remotest5 = get_remotest_n(mat, 5)

a <- data.frame(cbind(remotest5[3,], bk[remotest5[2,], c("POIName_de", "geopoint")]))
colnames(a) <- c("Distanz (m)", "Ort", "geopoint")
rownames(a) <- 1:5
a

library(raster)
ch <- getData("GADM", country = "CHE", level = 0)
bkcoords <- SpatialPoints(bk[,c("lon", "lat")])
ext <- extent(5.956063, 10.49511, 45.81706, 47.80848)
# grobes Raster
r <- raster(ext, nrow = 110, ncol = 175)
chraster <- rasterize(ch, r)

tic("Distance matrix")
D <- distanceFromPoints(object = chraster, xy = bkcoords)
toc()

D[which(is.na(chraster[]))] <- NA
remotest_n <- function(n, D, raster){
     # Extrahiert die n Punkte mit dem grössten Abstand zu einem
     # Briefkasten und deren Koordinaten.
     rem_n = head(order(values(D), decreasing=T), n)
     coords = xyFromCell(raster, rem_n)
     res = data.frame(cbind(values(D)[rem_n], coords[,2], coords[,1]))
     colnames(res) = c("Distanz (m)", "lat", "lon")
     rownames(res) = 1:n
     res
  }
  remotest_10 = remotest_n(10, D, chraster)
  remotest_10

png(file="figures/schweiz-distance-110-175.png",width=800,height=600)
plot(D)
