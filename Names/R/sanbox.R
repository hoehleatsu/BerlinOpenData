girls <- newborn %>% filter(sex == "w")
boys  < newborn %>% filter(sex == "m")

######################################################################
## We assume
stat <- function(x, idx=seq_len(nrow(x))) {
  x_boot <- x[idx,] %>% group_by(firstname) %>% summarise(anzahl = sum(anzahl))
  x_names <- data.frame(firstname=unique(x$firstname))
  x_joined <- left_join(x_names, x_boot, by="firstname") %>%
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

p_sex <- prop.table(table(kids$sex))

stat2 <- function(x) {
  x <- x %>% ungroup %>% mutate(rank=rank(-anzahl, ties.method="min"))
  return(x %$% rank)
}

stat2_sample <- function(x, n_total=sum(kids$anzahl), prob) {
  n <- rbinom(1, size=n_total,prob=prob)
  x_boot <- data.frame(firstname=sample(x$firstname, size=n, replace=TRUE, prob=x$anzahl),anzahl=1) %>% group_by(firstname) %>% summarise(anzahl = sum(anzahl))
  x_names <- data.frame(firstname=unique(x$firstname))
  x_joined <- left_join(x_names, x_boot, by="firstname") %>%
    mutate(anzahl=ifelse(is.na(anzahl),0, anzahl))
  stat2(x_joined)
}

ranks_sample <- function(x, R=999) {
  ##Which sex are we looking at? (implicit: all the same)
  sex <- x[1,] %$% sex %>% as.character

  ##Compute actual ranks
  r_obs <- x %>% stat2

  ##Bootstrap the ranks
  r_boot <- pbreplicate( R, stat2_sample(x, prob=p_sex[sex]))

  ##Join
  res <- data.frame(firstname=x$firstname, q=t(apply(cbind(r_obs, r_boot),1, quantile,prob=c(0.025,0.975))))
  res
}


set.seed(123)
kids_wranks <- kids %>% group_by(sex) %>% do( {
  ranks <- ranks_sample(.)
  return(ranks)
})
