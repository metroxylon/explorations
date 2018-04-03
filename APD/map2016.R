library(rworldmap)

apd <- read.csv("data/apd.csv", check.names=FALSE)

apd["ISO3"] <- sub("XKX", "KOS", unlist(apd["ISO3"]))
head(apd)




apdMapData <- joinCountryData2Map(apd,
                               nameJoinColumn="ISO3",
                               joinCode="ISO3" )

# mapDevice('x11') #create a world shaped window
png(filename="figures/apd2016.png",  width = 12, height = 8, units = 'in', res = 300)
catMethod=seq(from=0,to=50,by=5)

apdmap <- mapCountryData(apdMapData,
         nameColumnToPlot='2016',
         catMethod=catMethod,
         numCats=50,
         addLegend=FALSE,
         colourPalette='topo',
         mapTitle = "Aufteilung der APD in 2016 (in Mio.)"
         )

# adding legend
do.call(addMapLegend,
        c(apdmap,
          legendLabels="all",
          legendWidth=0.5,
          legendIntervals="data",
          horizontal=TRUE))
dev.off()
