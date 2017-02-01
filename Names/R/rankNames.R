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
library("magrittr")
library("pbapply")
library("tidyr")
##Make an SpatialPolygon object containing the Bezirke
##Data available from Berlin Open Data
##http://daten.berlin.de/datensaetze/rbs-lor-lebensweltlich-orientierte-r%C3%A4ume-dezember-2015
map <- readOGR(dsn="../../LOR/RBS_OD_LOR_2015_12/",layer="RBS_OD_LOR_2015_12")
bezmap <- gUnaryUnion(map, id=as(map,"data.frame")$BEZNAME)


##Read first names given in 2015. Data available from Berlin Open Data
##http://daten.berlin.de/datensaetze/liste-der-h%C3%A4ufigen-vornamen-2015
bezirke <- names(bezmap)
bezNames <- NULL
year <- 2016
for (bez in tolower(bezirke)) {
  fileName <- paste0("../data",year,"/",bez,".csv")
  fileName <- gsub("ö","oe",fileName) #renamed
  theBez <- read.csv2(file=fileName)
  theBez <- theBez %>% mutate(total=sum(anzahl),weight=anzahl/sum(anzahl))
  bezNames <- rbind(bezNames, data.frame(theBez, bezirk=bez))
}
bezNames <- bezNames %>% mutate(strata=paste0(geschlecht,"-",vorname))
allStrata <- bezNames %>% distinct(strata, .keep_all=TRUE)

##Aggregate over bezirke and sort according to rank
newborn <- bezNames %>% group_by(vorname, geschlecht) %>% summarise(anzahl=sum(anzahl)) %>% arrange(desc(anzahl))
##Expanded dataset
kids <- bezNames[rep(seq_len(nrow(bezNames)), times=bezNames$anzahl),]
kids$anzahl <- 1

kids %>% group_by(geschlecht) %>% summarise(n=n()) %>% mutate(frac=n/sum(n))

p_sex <- prop.table(table(kids$geschlecht))
kids$p_select <- p_sex[as.character(kids$geschlecht)]

girls <- newborn %>% filter(geschlecht == "w")
boys  < newborn %>% filter(geschlecht == "m")

######################################################################
## We assume
stat <- function(x, idx=seq_len(nrow(x))) {
  x_boot <- x[idx,] %>% group_by(vorname) %>% summarise(anzahl = sum(anzahl))
  x_names <- data.frame(vorname=unique(x$vorname))
  x_joined <- left_join(x_names, x_boot, by="vorname") %>%
    mutate(anzahl=ifelse(is.na(anzahl),0, anzahl),
           rank=rank(-anzahl, ties.method="min"))
  return(x_joined$rank)
}

boys_exp <- boys[rep(seq_len(nrow(boys)), boys$anzahl), ] %>% mutate(anzahl=1)

r_obs <- stat(boys_exp, sample(1:nrow(boys_exp), size=nrow(boys_exp), replace=TRUE))
r_boot <- replicate( 999, stat(boys_exp,  sample(1:nrow(boys_exp), size=nrow(boys_exp), replace=TRUE)))
r <- cbind(r_obs, r_boot)

foo <- data.frame(boys, rank=as.data.frame(t(apply(r, 1, quantile, prob=c(0.025, 0.975)))))
foo %>% filter(row_number() <= 10)


######################################################################
## Sample gender
######################################################################

p_sex <- prop.table(table(kids$geschlecht))

stat2 <- function(x) {
  x <- x %>% ungroup %>% mutate(rank=rank(-anzahl, ties.method="min"))
  return(x %$% rank)
}

stat2_sample <- function(x, n_total=sum(kids$anzahl), prob) {
  n <- rbinom(1, size=n_total,prob=prob)
  x_boot <- data.frame(vorname=sample(x$vorname, size=n, replace=TRUE, prob=x$anzahl),anzahl=1) %>% group_by(vorname) %>% summarise(anzahl = sum(anzahl))
  x_names <- data.frame(vorname=unique(x$vorname))
  x_joined <- left_join(x_names, x_boot, by="vorname") %>%
    mutate(anzahl=ifelse(is.na(anzahl),0, anzahl))
  stat2(x_joined)
}

ranks_sample <- function(x, R=999) {
  ##Which sex are we looking at? (implicit: all the same)
  geschlecht <- x[1,] %$% geschlecht %>% as.character

  ##Compute actual ranks
  r_obs <- x %>% stat2

  ##Bootstrap the ranks
  r_boot <- pbreplicate( R, stat2_sample(x, prob=p_sex[geschlecht]))

  ##Join
  res <- data.frame(vorname=x$vorname, q=t(apply(cbind(r_obs, r_boot),1, quantile,prob=c(0.025,0.975))))
  res
}


set.seed(123)
kids_wranks <- kids %>% group_by(geschlecht) %>% do( {
  ranks <- ranks_sample(.)
  return(ranks)
})


######################################################################
## Sample straified within each bezirk
######################################################################

stat <- function(x,  idx=seq_len(nrow(x))) {
  x_boot <- x[idx,] %>% group_by(geschlecht,vorname) %>% summarise(anzahl = sum(anzahl)) %>% mutate(strata=paste0(geschlecht,"-",vorname))

  x_joined <- left_join(allStrata, x_boot, by="strata") %>%
    mutate(anzahl=ifelse(is.na(anzahl.y),0, anzahl.y)) %>%
    group_by(geschlecht.x) %>%
    mutate(rank=rank(-anzahl, ties.method="min"))
  return(x_joined$rank)
}


                                        #theRanks <- stat(kids)
set.seed(123)
#debug("stat")
b <- boot::boot(kids, statistic=stat, R=999, strata=kids$bezirk, weights=kids$p_select)

rankCI <- cbind(allStrata[,c("vorname","geschlecht")], rankci=t(apply(cbind(b$t0, t(b$t)),1,quantile, prob=c(0.05,0.95),type=3))) %>% group_by(geschlecht)

rankCI$theRank <- stat(kids)
rankCI <- rankCI %>% select(vorname, geschlecht, theRank, `rankci.5%`, `rankci.95%`)

rankCI %>% arrange(theRank) %>% do({ head(.) })

rankCI %>% arrange(`rankci.5%`) %>% do({ head(.) })

