######################################################################
## Illustrating names of newborns in Berlin 2015/2016 per Bezirk using
## Berlin Open Data and #rstats.
##
## Author: Michael Höhle <http://www.math.su.se/~hoehle>
## Date:   2016-03-21, modified 2017-01-27
######################################################################

library("rgdal")
library("rgeos")
library("dplyr")
library("wordcloud")
library("magrittr")
source("mywordcloud.R")

##If you are unhappy about the colours, change them here
palette <- c(f="darkred",m="blue")

##Make an SpatialPolygon object containing the Bezirke
##Data available from Berlin Open Data
##http://daten.berlin.de/datensaetze/rbs-lor-lebensweltlich-orientierte-r%C3%A4ume-dezember-2015
map <- readOGR(dsn="../../LOR/RBS_OD_LOR_2015_12/",layer="RBS_OD_LOR_2015_12")
bezmap <- gUnaryUnion(map, id=as(map,"data.frame")$BEZNAME)

##Read first names given in 2015. Data available from Berlin Open Data
##http://daten.berlin.de/datensaetze/liste-der-h%C3%A4ufigen-vornamen-2015
bezirke <- names(bezmap)
years <- 2012:2016

##Create palette - include alpha channel for better visability
pal = brewer.pal(9, "BuGn")
pal <- pal[-(1:4)]
pal <- paste0(pal,"1A")

## for (year in years) {
##   bezNames <- NULL
##   for (bez in tolower(bezirke)) {
##     fileName <- paste0("../data",year,"/",bez,".csv")
##     fileName <- gsub("ö","oe",fileName) #renamed
##     theBez <- read.csv2(file=fileName)
##     theBez <- theBez %>% mutate(year=year,total=sum(anzahl),weight=anzahl/sum(anzahl))
##     bezNames <- rbind(bezNames, data.frame(theBez, bezirk=bez))
##   }



##   ##======================================================================
##   ##Create a map and add names (no wordcloud). No ggplot or other
##   ##fancy #dataviz is used...just base graphics
##   ##======================================================================
##   loc <- coordinates(bezmap)
##   rownames(loc) <- tolower(rownames(loc))

##   png(file=paste0("firstname-",year,".png"),width=480,height=480,bg="white",pointsize=14)
##   par(mar=c(0,0,4.1,0))
##   plot(bezmap,border=rgb(0.9,0.9,0.9))
##   points(loc,cex=0.01)
##   for (bez in tolower(bezirke)) {
##     theBezNames <- bezNames %>% filter(bezirk==bez)
##     topMale    <- theBezNames %>% filter(geschlecht == "m") %>% head(n=1)
##     topFemale  <- theBezNames %>% filter(geschlecht == "w") %>% head(n=1)
##     text(loc[bez,1],loc[bez,2],paste0(topMale$vorname,collapse=" "), col=palette["m"],pos=1,offset=0.3)
##     text(loc[bez,1],loc[bez,2],paste0(topFemale$vorname,collapse=" "), col=palette["f"],pos=3,offset=0.3)
##   }
##   title(paste0("Most common first name for newborns ",year,"\n in Berlin (female/male per Bezirk)"))
##   dev.off()


##   ##======================================================================
##   ##2nd try, now with wordclouds. No ggplot or other
##   ##fancy #dataviz is used...just base graphics
##   ##======================================================================

##   png(file=paste0("firstname-wordcloud-",year,".png"),width=640,height=640,bg="white",pointsize=16)
##   par(mar=c(0,0,4.1,0))
##   plot(bezmap,border=rgb(0.8,0.8,0.8))
##   for (bez in tolower(bezirke)) {
##     theBez <- bezmap[tolower(names(bezmap)) == bez,]
##     mywordcloud(words=theBezNames$vorname, freq=theBezNames$anzahl,scale=c(2,1),random.order=FALSE,colors = pal,offset=loc[bez,],use.r.layout=FALSE, bbox=bbox(theBez),min.freq=5)
##   }
##   plot(bezmap,border=rgb(0.8,0.8,0.8),add=TRUE)
##   for (bez in tolower(bezirke)) {
##     theBezNames <- bezNames %>% filter(bezirk==bez)
##     topMale    <- theBezNames %>% filter(geschlecht == "m") %>% head(n=1)
##     topFemale  <- theBezNames %>% filter(geschlecht == "w") %>% head(n=1)
##     text(loc[bez,1],loc[bez,2],paste0(topMale$vorname,collapse=" "), col=palette["m"],pos=1,offset=0.3)
##     text(loc[bez,1],loc[bez,2],paste0(topFemale$vorname,collapse=" "), col=palette["f"],pos=3,offset=0.3)
##   }
##   title(paste0("Most common first name for newborns in\n Berlin ",year," (female/male for each Bezirk)"))
##   dev.off()
## }


######################################################################
## More structured for several years
######################################################################

##Load data
bezNames <- NULL
for (year in years) {
  for (bezirk in bezirke) {
    bez <- tolower(bezirk)
    fileName <- paste0("../data",year,"/",bez,".csv")
    fileName <- gsub("ö","oe",fileName) #renamed
    theBez <- read.csv2(file=fileName)
    theBez <- theBez %>% mutate(year=year,total=sum(anzahl),weight=anzahl/sum(anzahl))
    bezNames <- rbind(bezNames, data.frame(theBez, bezirk=bez, bezirkDE=bezirk))
  }
}
bezNames <- as.tbl(bezNames)
bezNames

plotIt <- function(years, cex=1, withWordCloud=TRUE) {
  for (theYear in years) {
    par(mar=c(0,0,4.1,0))
    plot(bezmap,border=rgb(0.8,0.8,0.8))

    ##word clouds
    if (withWordCloud) {
      for (bez in tolower(bezirke)) {
        theBez <- bezmap[tolower(names(bezmap)) == bez,]
        theBezNames <- bezNames %>% filter(bezirk==bez & year == theYear)
        mywordcloud(words=theBezNames$vorname, freq=theBezNames$anzahl,scale=c(2,1),random.order=FALSE,colors = pal,offset=loc[bez,],use.r.layout=FALSE, bbox=bbox(theBez),min.freq=5)
      }
      ##redraw borders
      plot(bezmap,border=rgb(0.8,0.8,0.8),add=TRUE)
    }

    ##top name drawn seperately (slow)
    for (bez in tolower(bezirke)) {
      theBezNames <- bezNames %>% filter(bezirk==bez & year == theYear)
      topMale    <- theBezNames %>% filter(geschlecht == "m") %>% head(n=1)
      topFemale  <- theBezNames %>% filter(geschlecht == "w") %>% head(n=1)
      text(loc[bez,1],loc[bez,2],paste0(topMale$vorname,collapse=" "), col=palette["m"],pos=1,offset=0.3,cex=cex)
      text(loc[bez,1],loc[bez,2],paste0(topFemale$vorname,collapse=" "), col=palette["f"],pos=3,offset=0.3,cex=cex)
    }
    title(paste0("Most common first name for newborns in\n Berlin ",theYear," (female/male for each Bezirk)"),cex.main=cex)
  }
  invisible()
}

loc <- coordinates(bezmap)
rownames(loc) <- tolower(rownames(loc))
##plot(bezmap["Tempelhof-Schöneberg",])
##plot(bezmap)
##plot(bezmap["Lichtenberg",],add=TRUE,lwd=3)
loc["tempelhof-schöneberg",] <- loc["tempelhof-schöneberg",] + c(0, 3000)
loc["lichtenberg",] <- loc["lichtenberg",] + c(0, 1500)



library("animation")
saveGIF(plotIt(years,cex=1.5,withWordCloud=TRUE), movie.name=paste0("firstnames_berlin_",paste(range(years),collapse="-"),".gif"),ani.height=600,ani.width=640,interval=2)
saveVideo(plotIt(years,cex=1.5,withWordCloud=TRUE), video.name=paste0("firstnames_berlin_",paste(range(years),collapse="-"),".mp4"),ani.height=600,ani.width=640,interval=2,other.opts = "-pix_fmt yuv420p -b 300k")

dim(bezNames)

#Most popular #babynames dynamics in #Berlin 2012-2016 @OpenDataBerlin #rstats

######################################################################
## Name collisions
######################################################################
bezNames %>% group_by(year, geschlecht,vorname) %>% summarise(n=sum(anzahl)) %>% arrange(year,desc(n)) %>% top_n(2)


##devtools::install_github("hoehleatsu/birthdayproblem")
library(birthdayproblem)
library(ggplot2)
library(forcats)
collision <- bezNames %>% group_by(year,bezirkDE) %>% do({
  n <- 26L #c(20L,26L,30L)
  foo <- mutate(., p=anzahl/sum(anzahl))
  p <- sapply(n, function(n) pbirthday_up(n=n, foo$p ,method="mase1992")$prob)
  data.frame(n=n, p=p, gini=ineq::Gini(foo$anzahl))
})

##Problems with extra naming
nNames <- data.frame(rbind(c("Charlottenburg-Wilmersdorf",2377,2758,505,57),
                 c("Friedrichshain-Kreuzberg",  2538,2187,478,43),
                 c("Lichtenberg",               2012,1343,190,15),
                 c("Marzahn-Hellersdorf",        725,472, 77, 4),
                 c("Mitte",                     2629,2160,398,52),
                 c("Neukölln",                  2052,1543,192,18),
                 c("Pankow",                    2513,2123,349,2),
                 c("Reinickendorf",             622,499,  68,4),
                 c("Spandau",                   1614,1641,302,26),
                 c("Steglitz-Zehlendorf",       506,575,97,6),
                 c("Tempelhof-Schöneberg",      2838,2817,462,46),
                 c("Treptow-Köpenick",          777,612,89,9)))
colnames(nNames) <- c("Bezirk","1","2","3",">3")
nNames

nNames <- nNames %>% mutate(`1`=as.numeric(as.character(`1`)),
                  `2`=as.numeric(as.character(`2`)),
                  `3`=as.numeric(as.character(`3`)),
                  `>3`=as.numeric(as.character(`>3`)),
                  total = `1` + `2` + `3` + `>3`,
                  propWithMoreThanOne = (total - `1`)/total,
                  propOne = (`1`)/total,
                  propWithMoreThanOne_str = sprintf("%.0f%%",propWithMoreThanOne*100),
                  propOne_str = sprintf("%.0f%%",propOne*100))

foo <- cbind(nNames %>% mutate(bezirk_with_prob=paste0(Bezirk, "\n(",as.character(propOne_str),")")) )
##foo <- cbind(nNames %>% mutate(bezirk_with_prob=paste0(Bezirk, "\n(",as.character(propOne_str)," has more than 1 name)")) )
##collision %>% inner_join(foo, by=c("bezirkDE"="Bezirk"))
collision <- collision %>% merge(foo, by.x="bezirkDE", by.y="Bezirk")


levels(collision$bezirkDE) <- levels(collision$bezirkDE)[c(9,8,7,3,1,5,2,4,10,11,6,12)]
collision$bezirk_with_prob <- as.factor(collision$bezirk_with_prob)
levels(collision$bezirk_with_prob) <- levels(collision$bezirk_with_prob)[c(9,8,7,3,1,5,2,4,10,11,6,12)]

##Correlation between the probability of
with(collision %>% filter(year==2016), cor(propOne,p))
with(collision %>% filter(year==2016), cor(propOne,p,method="spearman"))
with(collision %>% filter(year==2016), plot(propOne,p))

ggplot(collision, aes(x=year, y=p)) + geom_line(color="steelblue") + geom_point(color="indianred3") + facet_wrap(~ bezirk_with_prob) + ggtitle("Probability of a name collision in a class of 26 born in year YYYY") + ylab("Probability") + xlab("Year") + scale_y_continuous(labels=scales::percent) + theme(axis.text.x=element_text(angle = 45, vjust = 0.5),strip.background = element_rect(fill="lightblue"))
ggsave(filename="collisionprob-berlin.png",width=7, height=4.5, dpi=72*1.5)

ggplot(collision, aes(x=year, y=gini)) + geom_line(color="steelblue") + geom_point(color="indianred3") + facet_wrap(~ bezirk_with_prob) + ggtitle("Gini Coefficient for Number of Kids per Name") + ylab("Gini coefficient") + xlab("Year") + scale_y_continuous(labels=scales::percent) + theme(axis.text.x=element_text(angle = 45, vjust = 0.5),strip.background = element_rect(fill="lightblue"))


ggsave(filename="gini-berlin.png",width=7, height=4.5, dpi=72*1.5)




######################################################################
## Josef-Index
######################################################################

##theName <- "Josef" ; theColor <- "lightblue"
theName <- "Sabine" ; theColor <- "salmon2"

##Empty frame with zero entries to avoid the zero-bug
zeropad <- expand.grid(vorname=theName,geschlecht=NA,anzahl=0, year=bezNames %$% year %>% unique, bezirkDE=bezNames %$% bezirkDE %>% unique)

##Count
name <- rbind(zeropad, bezNames %>% select(-total,-weight,-bezirk) %>%
                       filter(vorname == theName)) %>%
  group_by(bezirkDE,year) %>% summarise(anzahl=sum(anzahl))


ggplot(name, aes(x=year, y=anzahl)) + geom_line(color="steelblue") + geom_point(color="indianred3") + facet_wrap(~ bezirkDE) + ggtitle(paste0("Kinder mit dem Namen '",theName,"' in Berlin ",paste0(range(name$year),collapse="-"))) + ylab("Anzahl") + xlab("Jahr") + theme(axis.text.x=element_text(angle = 45, vjust = 0.5),strip.background = element_rect(fill=theColor)) + scale_y_continuous(breaks=scales::pretty_breaks())

ggsave(filename=paste0(theName,"-berlin.png"),width=7, height=4.5, dpi=72*1.5)

#Josef- & Sabinetrends in den Berliner Bezirken: http://www.morgenpost.de/kolumne/Zwischenmenschlich/article210008739/Wenn-Hipstereltern-ihren-Kindern-absurde-Namen-geben.html … @nina_paulsen @morgenpost @OpenDataBerlin #ddj #rettetjosef
