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
library("forcats")

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
##Rename all columns and factor levels to english
bezNames <- bezNames %>%
  rename(firstname = vorname, count = anzahl, district = bezirk) %>%
  mutate(sex = fct_recode(geschlecht, c("m"="m", "f"="w")))


##Introduce a short unique strata identifier (geschlecht x vorname)
bezNames <- bezNames %>% mutate(strata=paste0(sex,"-",firstname)) %>% select(-total,-weight,-geschlecht)
rownames(bezNames) <- NULL
write.csv(bezNames, file=paste0("berlin-firstnames-",year,".csv"),row.names=FALSE)

######################################################################
## Done with data wrangling
######################################################################

bezNames <- read.csv(file=paste0("berlin-firstnames-",year,".csv"))

bezNames %>% filter(firstname == "")

##Make a data.frame containing all possible strata of gender and firstname.
##This is how our return should look every time
allStrata <- bezNames %>% distinct(strata, .keep_all=TRUE) %>% mutate(count=0)

##Aggregate district stratified data over district and sort according to rank
newborn <- bezNames %>% group_by(firstname, sex) %>%
  summarise(count=sum(count)) %>% group_by(firstname, sex)

##Result ordered
newborn %>% arrange(desc(count)) %>% group_by(sex) %>% do({head(.,n=5)})


##Expanded dataset, containing one row for each kid (can this be done dplyr style?)
kids <- bezNames[rep(seq_len(nrow(bezNames)), times=bezNames$count),] %>%
  mutate(count=1)

##Check gender distribution
kids %>% group_by(sex) %>% summarise(n=n()) %>% mutate(fraction=n/sum(n))
##Old R style to get a named vector
p_sex <- prop.table(table(kids$sex))
##Sex ratio
p_sex["m"]/p_sex["f"]

##p_sex <- c("m"=0.01,"w"=0.99)
##Add a selection probability column based on sex
kids$p_select <- p_sex[as.character(kids$sex)]


######################################################################
## Sample straified within each district
######################################################################

rank_boot <- function(x,  idx=seq_len(nrow(x))) {
  ##Summarise how many times each first name appears in the
  ##bootstrapped data set.  Append a data.frame containing all
  ##sex-firstname strata (so they appears in the group_by but each
  ##containing zero counts.
  x_boot <- x %>% slice(idx) %>% bind_rows(allStrata)

  ##Summarise the number of occurences for each sex-firstname strata
  ##and compute the ranks. Important: the grouping has to be in the
  ##same order newborn, which groups by firstname and then sex.
  aggrx_wranks <- x_boot %>%  group_by(firstname,sex) %>%
    summarise(count = sum(count)) %>%
    group_by(sex) %>%
    mutate(rank=rank(-count, ties.method="min")) %>%
    group_by(firstname,sex)

  ##Done
  return(aggrx_wranks %$% rank)
}



set.seed(123)
##debug("stat") ; stat(kids)
##Bootstrap within strata districts.
b <- boot::boot(kids, statistic=rank_boot, R=999, strata=kids$district, weights=kids$p_select)

##Percentile based 95% CI for the ranks. Note: We ensured that the
##ranks returned by the stat function are in the same order as
##newborn.
newborn_ranks <- data.frame(newborn[,c("firstname","sex")], rank=stat(kids),rankci=t(apply(cbind(b$t0, t(b$t)),1,quantile, prob=c(0.05,0.95),type=3))) %>% tbl_df

##Show top-10 rank for each gender
newborn_ranks %>% group_by(sex) %>% arrange(rank) %>% do({ head(., n=10) })

##Use lower CI
newborn_ranks %>% group_by(sex) %>% arrange(`rankci.5.`) %>% select(firstname, sex, rankci.5.) %>% do({ head(.) })

nb_rankswunc <- newborn_ranks %>% group_by(sex, rankci.5.) %>% do( {
  data.frame(uc_rank=.$rankci.5.[1], names=paste(.$firstname, collapse=", "))
}) %>% group_by(sex) %>% select(uc_rank, names)


nb_rankswunc %>% filter(sex == "f")
nb_rankswunc %>% filter(sex == "m")

######################################################################
##
######################################################################

##Determine probabilty of being sampled in each sex strata.
bezNames_pro <- bezNames %>% group_by(sex, district) %>% mutate(prob=count/sum(count))


one_sample <- function(newborn) {
  ##How many boys-> girl and how many girls-> boys in each district
}
