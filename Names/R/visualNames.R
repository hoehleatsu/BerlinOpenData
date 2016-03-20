######################################################################
## Illustrating names of newborns in Berlin 2015 per Bezirk using
## Berlin Open Data and #rstats.
##
## Author: Michael Höhle <http://www.math.su.se/~hoehle>
## Date:   2016-03-21
######################################################################

library("rgdal")
library("rgeos")
library("dplyr")
library("wordcloud")
source("mywordcloud.R")

##Make an SpatialPolygon object containing the Bezirke
##Data available from Berlin Open Data
##http://daten.berlin.de/datensaetze/rbs-lor-lebensweltlich-orientierte-r%C3%A4ume-dezember-2015
map <- readOGR(dsn="../../LOR/RBS_OD_LOR_2015_12/",layer="RBS_OD_LOR_2015_12")
bezmap <- gUnaryUnion(map, id=as(map,"data.frame")$BEZNAME)

##Read first names given in 2015. Data available from Berlin Open Data
##http://daten.berlin.de/datensaetze/liste-der-h%C3%A4ufigen-vornamen-2015
bezirke <- names(bezmap)
bezNames <- NULL
for (bez in tolower(bezirke)) {
  fileName <- paste0("../data/",bez,".csv")
  fileName <- gsub("ö","oe",fileName) #renamed
  theBez <- read.csv2(file=fileName)
  theBez <- theBez %>% mutate(total=sum(anzahl),weight=anzahl/sum(anzahl))
  bezNames <- rbind(bezNames, data.frame(theBez, bezirk=bez))
}

head(bezNames)

##Create palette - include alpha channel for better visability
pal = brewer.pal(9, "BuGn")
pal <- pal[-(1:4)]
pal <- paste0(pal,"1A")


######################################################################
##Create a map and add names (no wordcloud). No ggplot or other
##fancy #dataviz is used...just base graphics
######################################################################
loc <- coordinates(bezmap)
rownames(loc) <- tolower(rownames(loc))

png(file="firstname.png",width=480,height=480,bg="white",pointsize=14)
par(mar=c(0,0,4.1,0))
plot(bezmap,border=rgb(0.9,0.9,0.9))
points(loc,cex=0.01)
for (bez in tolower(bezirke)) {
  theBezNames <- bezNames %>% filter(bezirk==bez)
  topMale    <- theBezNames %>% filter(geschlecht == "m") %>% head(n=1)
  topFemale  <- theBezNames %>% filter(geschlecht == "w") %>% head(n=1)
  text(loc[bez,1],loc[bez,2],paste0(topMale$vorname,collapse=" "), col="blue",pos=1,offset=0.3)
  text(loc[bez,1],loc[bez,2],paste0(topFemale$vorname,collapse=" "), col="magenta",pos=3,offset=0.3)
}
title("Most common first name for newborns 2015\n in Berlin (female/male per Bezirk)")
dev.off()


######################################################################
##2nd try, now with wordclouds. No ggplot or other
##fancy #dataviz is used...just base graphics
######################################################################

png(file="firstname-wordcloud.png",width=640,height=640,bg="white",pointsize=16)
par(mar=c(0,0,4.1,0))
plot(bezmap,border=rgb(0.8,0.8,0.8))
for (bez in tolower(bezirke)) {
  theBez <- bezmap[tolower(names(bezmap)) == bez,]
  mywordcloud(words=theBezNames$vorname, freq=theBezNames$anzahl,scale=c(2,1),random.order=FALSE,colors = pal,offset=loc[bez,],use.r.layout=FALSE, bbox=bbox(theBez),min.freq=5)
}
plot(bezmap,border=rgb(0.8,0.8,0.8),add=TRUE)
for (bez in tolower(bezirke)) {
  theBezNames <- bezNames %>% filter(bezirk==bez)
  topMale    <- theBezNames %>% filter(geschlecht == "m") %>% head(n=1)
  topFemale  <- theBezNames %>% filter(geschlecht == "w") %>% head(n=1)
  text(loc[bez,1],loc[bez,2],paste0(topMale$vorname,collapse=" "), col="blue",pos=1,offset=0.3)
  text(loc[bez,1],loc[bez,2],paste0(topFemale$vorname,collapse=" "), col="magenta",pos=3,offset=0.3)
}
title("Most common first name for newborns 2015\n in Berlin (female/male per Bezirk)")
dev.off()

