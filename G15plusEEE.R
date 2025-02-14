
### iNat
### eButterfly
### eBird
### Plantnet
### Sentinelle
### Carapace
### Christmas Bird Count
### Feeder Watch
### Great Backyard Bird Count
### Observations.org
### EPOQ
### Chauve-souris au abris
### Décompte des grenouilles
### Atlas des oiseaux nicheurs
### RON


library(jsonlite)
library(data.table)
library(mgcv)
library(sf)
library(gratia)

d <- fread("/home/frousseu/Downloads/0067051-241126133413365.csv")
d <- d[institutionCode == "iNaturalist",]
d <- d[species != "", ]

eee <- fread("/home/frousseu/Downloads/sentinelle_liste_sp.csv")[-1, ]$Nom_latin
#eee <- st_read("/home/frousseu/Downloads/especes_exo_envahissantes.gpkg") |>
#  _$Nom_espece_latin |>
#  strsplit(" ") |>
#  lapply("[", 1:2) |>
#  sapply(paste, collapse = " ") |>
#  unique()
eee <- sub("^(([^ ]+\\s+){1}[^ ]+).*", "\\1", eee)

years <- 2008:2024

###############################################################
### Taux d'observation espèces exotiques envahissantes ########
###############################################################

xtot <- fromJSON(paste0("https://api.inaturalist.org/v1/observations/histogram?interval=year&place_id=13336&year=", paste(years, collapse = ","),"&order=desc&order_by=created_at"))#$total_results
xtot <- unlist(xtot$results$year)
xtot0 <- cbind(years, xtot) |> as.data.frame()


#eee <- eee[1:25]
#eee <- d$species[!d$species %in% c(eee, "")]
#eee <- names(rev(sort(table(eee))))[1:50]
#eee <- sample(unique(eee), 250)
#tab <- rev(sort(table(d$species)))
#tab <- tab[tab>=100]
#eee <- sample(names(tab)[1:50],50)
#eee <- sample(names(tab),100)


res <- lapply(eee, function(i){
  sp <- i#"Heraclides cresphontes"
  sp <- gsub(" ", "%20", sp)
  json <- fromJSON(paste0("https://api.inaturalist.org/v1/search?q=", sp)) |>
    _$results$record

  if(is.null(json)){
    return(NULL)
  }

  taxon_id <- json$id[1]
  taxon_group_id <- json$iconic_taxon_id[1]

  if(TRUE){
    taxon_group_id <- paste0("&taxon_id=",taxon_group_id)
    xtot <- fromJSON(paste0("https://api.inaturalist.org/v1/observations/histogram?interval=year&place_id=13336&year=", paste(years, collapse = ","), taxon_group_id,"&order=desc&order_by=created_at"))#$total_results
    xtot <- unlist(xtot$results$year)
    xtot <- cbind(years, xtot) |> as.data.frame()
  }else{
    xtot <- xtot0
  }


  ### HISTO
  xs <- fromJSON(paste0("https://api.inaturalist.org/v1/observations/histogram?interval=year&taxon_id=", taxon_id, "&place_id=13336&year=", paste(years, collapse = ","),"&order=desc&order_by=created_at"))#$total_results
  xs <- unlist(xs$results$year)
  if(is.null(xs)){
    return(NULL)
  }
  xs <- data.frame(years = as.integer(substr(names(xs),1, 4)), xs)

  p <- merge(xs, xtot, all.y = TRUE)
  p$xs <- ifelse(is.na(p$xs), 0, p$xs)

  names(p) <- c("year", "nobs", "ntot")
  print(i)
  cbind(species = i, p)

})
names(res) <- eee
p <- do.call("rbind", res)

eeeninat <- p |>
  as.data.table() |>
  _[, .(n = sum(nobs)), by = .(species)] |>
  _[order(-n), ]

eeengbif <- d[species %in% eee, ] |>
  _[, .(n = .N), by = .(species)] |>
  _[order(-n), ]



#names(p) <- c("year", "nobs", "ntot")
par(mar = c(2, 2.5, 1, 3))
plot(0, 0, xlim = range(pretty(years)), ylim = c(0, min(c(max(p$nobs / p$ntot), 0.03))), type = "b", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
axis(1, at = pretty(years), cex.axis = 0.75, tcl = -0.1, mgp = c(1, 0.15, 0))
axis(2, at = pretty(p$nobs / p$ntot), las = 2, cex.axis = 0.75, tcl = -0.1, mgp = c(1, 0.15, 0))

l <- split(p, p$species)
lapply(l, function(i){
  lines(i$year, i$nobs / i$ntot, col = adjustcolor("black", 0.5))
  ii <- i[nrow(i), ]
  text(ii$year + 0.25, ii$nobs / ii$ntot, labels = ii$species, cex = 0.35, adj = c(0, 0.5), xpd = TRUE)
})


###########
### ratio #
###########

invisible(ll <- lapply(l, function(i){
  yr <- 2021:2024
  ii <- i[i$year %in% yr, ]
  i$ratio <- (i$nobs / i$ntot) / mean(ii$nobs/ii$ntot)
  #i$sratio <- ifelse(i$ratio < 0, -1/i$ratio - 1, i$ratio - 1)
  i
}))
pp <- do.call("rbind", ll)

par(mar = c(2, 2.5, 1, 3))
#xlim <- c(2018, 2024)
xlim <- range(pretty(years))
plot(0, 0, xlim = xlim, ylim = c(0, 4), type = "b", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
axis(1, at = pretty(xlim), cex.axis = 0.75, tcl = -0.1, mgp = c(1, 0.15, 0))
axis(2, at = pretty(p$ratio), las = 2, cex.axis = 0.75, tcl = -0.1, mgp = c(1, 0.15, 0))

th <- 5
ll <- split(pp, pp$species)
invisible(lapply(ll, function(i){
  if(sum(i$nobs) < th){
    return(NULL)
  }
  lines(i$year, i$ratio, col = adjustcolor("black", 0.2))
  ii <- i[nrow(i), ]
  text(ii$year + 0.25, ii$ratio, labels = ii$species, cex = 0.35, adj = c(0, 0.5), xpd = TRUE)
}))
abline(1, 0, lty = 3)
dat <- do.call("rbind", lapply(ll, function(i){  if(sum(i$nobs) < th){ return(NULL) }else{ i }}))
m <- loess(ratio ~ year, data = dat, span = 0.75)
preds <- predict(m, data.frame(year = years))
m <- gam(ratio ~ s(year), data = dat)
preds <- predict(m, data.frame(year = years), se.fit = TRUE)
polygon(c(years, rev(years), years[1]), c(preds$fit + preds$se.fit*1.96, rev(preds$fit - preds$se.fit*1.96), (preds$fit + preds$se.fit*1.96)[1]), border = NA, col = adjustcolor("red", 0.20))
lines(years, preds$fit, lwd = 3, col = adjustcolor("red", 0.75))
mtext(side = 1, text = "Années", line = 1, font = 2)
mtext(side = 2, text = "Taux d'observation standardisé moyen", line = 1.25, font = 2)
moy <- aggregate(ratio ~ year, data = dat, FUN = mean)
lines(moy$year, moy$ratio, type = "b", lwd = 2, pch = 16, col = adjustcolor("black", 0.75))

#observers <- fread("/home/frousseu/Downloads/observers.csv.gz")
#taxa <- fread("/home/frousseu/Downloads/taxa.csv.gz")
#test <- fread("/home/frousseu/Downloads/observations.csv.gz", nrows = 10000000, select = c(3, 4, 6, 7))
#test<-fread("https://inaturalist-open-data.s3.amazonaws.com/taxa.csv.gz", nrows= 10)
#test[grep("Elymus trachycaulus", name), ]
#par(new = TRUE)
#plot(p$year, p$ntot, type = "l", ylim = c(0, max(p$ntot)), lwd = 6, col = adjustcolor("black", 0.1), xlab = "", ylab = "", xaxt = "n", yaxt = "n")
#axis(4, at = pretty(c(0, p$ntot)), las = 2, cex.axis = 0.75, tcl = -0.1, mgp = c(1, 0.25, 0))

#r <- rast("/home/frousseu/Downloads/VersionCourante/Cotq_10m/Geotiff/Version202201/COTQ_2022_V1.tif")
#r <- crop(r, c(-75, -71.5, 45, 45.5)) |> trim()
#r <- crop(r, c(-73.9, -73.85, 45.08, 45.12)) |> trim()
#r <- crop(r, c(-72, -71.9, 45.45, 45.52)) |> trim()

###############################################################
###############################################################
###############################################################

library(jsonlite)

years <- 2008:2024

xtot <- fromJSON(paste0("https://api.inaturalist.org/v1/observations/histogram?interval=year&place_id=13336&year=", paste(years, collapse = ","),"&order=desc&order_by=created_at"))#$total_results
xtot <- unlist(xtot$results$year)
xtot0 <- cbind(years, xtot) |> as.data.frame()

sp <- "Phragmites australis"
sp <- gsub(" ", "%20", sp)
json <- fromJSON(paste0("https://api.inaturalist.org/v1/search?q=", sp)) |>
  _$results$record
taxon_id <- json$id[1]
taxon_group_id <- json$iconic_taxon_id[1]

if(TRUE){
  taxon_group_id <- paste0("&taxon_id=",taxon_group_id)
  xtot <- fromJSON(paste0("https://api.inaturalist.org/v1/observations/histogram?interval=year&place_id=13336&year=", paste(years, collapse = ","), taxon_group_id,"&order=desc&order_by=created_at"))#$total_results
}else{
  xtot <- xtot0
}

xtot <- unlist(xtot$results$year)
xtot <- cbind(years, xtot) |> as.data.frame()

### HISTO
xs <- fromJSON(paste0("https://api.inaturalist.org/v1/observations/histogram?interval=year&taxon_id=", taxon_id, "&place_id=13336&year=", paste(years, collapse = ","),"&order=desc&order_by=created_at"))#$total_results
xs <- unlist(xs$results$year)
xs <- data.frame(years = as.integer(substr(names(xs),1, 4)), xs)

p <- merge(xs, xtot, all.y = TRUE)
p$xs <- ifelse(is.na(p$xs), 0, p$xs)


par(mar = c(2, 2.5, 1, 3))
plot(p$year, p$xs / p$xtot, xlim = range(pretty(years)), ylim = c(0, max(p$xs / p$xtot)), type = "b", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
#plot(p$year, p$nobs / p$ntot, ylim = c(0, max(p$nobs / p$ntot)), type = "b", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
axis(1, at = pretty(years), cex.axis = 0.75, tcl = -0.1, mgp = c(1, 0.15, 0))
axis(2, at = pretty(p$xs / p$xtot), las = 2, cex.axis = 0.75, tcl = -0.1, mgp = c(1, 0.15, 0))
par(new = TRUE)
plot(p$year, p$xtot, type = "l", ylim = c(0, max(p$xtot)), lwd = 6, col = adjustcolor("black", 0.1), xlab = "", ylab = "", xaxt = "n", yaxt = "n")
axis(4, at = pretty(c(0, p$xtot)), las = 2, cex.axis = 0.75, tcl = -0.1, mgp = c(1, 0.25, 0))


### MULTIPLE API REQUEST
#obs <- sapply(years, function(i){
#  xs <- fromJSON(paste0("https://api.inaturalist.org/v1/observations?taxon_id=", taxon_id, "&place_id=13336&year=", i,"&order=desc&order_by=created_at"))$total_results
#  xtot <- fromJSON(paste0("https://api.inaturalist.org/v1/observations?place_id=13336&year=", i,"&order=desc&order_by=created_at"))$total_results
#  c(xs, xtot)
#})
#p <- cbind(years, t(obs)) |> as.data.frame()


###################################################################
### The following is not very useful I think ######################
###################################################################

library(data.table)
library(mgcv)
library(sf)

d <- fread("/home/frousseu/Documents/github/flore.quebec/data/gbif/0039190-240321170329656.csv")
d <- d[institutionCode == "iNaturalist", ]
d[, date := eventDate]
d[, jul := as.integer(format(as.Date(date),"%j"))]
d[, year := as.integer(substr(date,1,4))]
d <- d[species != "",]
years <- 2019:(max(d$year)-1)
d <- d[year %in% years, ]

x <- d[, .(n = .N), by = c("species", "year")]
a <- d[, .(N = .N), by = c("year")]
x <- merge(x, a)
x[, p := n / N]
#x[, p := p / max(p), by = "species"]
add <- expand.grid(species = unique(x$species), year = unique(x$year), n = 0, N = NA, p = 0) |>
  as.data.table()
x[add, on = c("species", "year")]
#x[, p := (p - mean(p)) / sd(x$p), by = "species"]
x <- x[order(species, year),]
l <- split(x, by = "species")
#l <- l[sapply(l, nrow) == (length(years)-1)]

#eee <- eee[eee %in% d$species]
eee <- eee[eee %in% names(l)]
noneee <- names(l)[!names(l) %in% eee]

plot(0, 0, xlim = range(years), ylim = range(x$p), type = "n")
#lapply(l, function(i){
#  lines(i$year, i$p, lwd = 2, col = adjustcolor("black", 0.025))
#})
v <- seq(min(years), max(years), length.out = 100)
ps <- lapply(names(l), function(species){
  dat <- l[[species]]
  #print(dat)
  if(nrow(dat) < 3){return(NULL)}
  #plot(0, 0, xlim = range(years), ylim = range(x$p), type = "n")
  #points(dat$year, dat$p, cex = 2, lwd = 2, col = "tomato")
  m <- gam(p ~ s(year, k = 1), data = dat)
  k <- v >= min(dat$year) & v <= max(dat$year)
  vv <- v[k]
  preds <- predict(m, data.frame(year = vv))
  lines(vv, preds, lwd = 3, col = adjustcolor("black", 0.05))
  res <- rep(NA, length(v))
  res[which(k)] <- preds
  #res[which(vv >= min(dat$years)] <-
  res# - mean(res)
})
lines(v, colMeans(do.call("rbind", ps), na.rm = TRUE))

q <- t(apply(do.call("rbind", ps), 2, function(i){quantile(i, prob = c(0.1, 0.9), na.rm = TRUE)}))
plot(0, 0, xlim = range(years), ylim = range(q), type = "n")
lines(v, q[ , 1])
lines(v, q[ , 2])

################################################################
### bootstrap observers ########################################
################################################################

library(data.table)
library(mgcv)
library(sf)

#d <- fread("/home/frousseu/Documents/github/flore.quebec/data/gbif/0039190-240321170329656.csv")
#d <- fread("/home/frousseu/Downloads/0014913-241024112534372.csv")
d <- fread("/home/frousseu/Downloads/0067051-241126133413365.csv")
d <- d[institutionCode == "iNaturalist", ]
#d <- d[phylum == "Tracheophyta", ]
d[, date := eventDate]
d[, jul := as.integer(format(as.Date(date),"%j"))]
d[, year := as.integer(substr(date,1,4))]
d <- d[species != "",]
years <- 2012:2024#(max(d$year)-1)
d <- d[year %in% years, ]


x <- d[, .(n = .N), by = c("species", "year")]
a <- d[, .(N = .N), by = c("year")]
x <- merge(x, a)
x[, p := n / N]
#x[, p := p / max(p), by = "species"]
add <- expand.grid(species = unique(x$species), year = unique(x$year), n = 0, N = NA, p = 0) |>
  as.data.table()
x[add, on = c("species", "year")]
#x[, p := (p - mean(p)) / sd(x$p), by = "species"]
x <- x[order(species, year),]
l <- split(x, by = "species")
#l <- l[sapply(l, nrow) == (length(years)-1)]

X <- d[ , .(species, year, date, recordedBy, kingdom)] |>
  _[order(recordedBy, date), ]# |>
#_[ , first := min(date), by = .(recordedBy)][first <= "2013-01-01", ]# |>
#_[ , rank := seq_len(.N), by = .(recordedBy) ][rank >= 150, ] |>


X[recordedBy %in% sample(unique(recordedBy), 3, replace = TRUE), ]

add <- expand.grid(species = unique(d$species), year = unique(d$year), n = 0, stringsAsFactors = FALSE) |>
  as.data.table() |>
  _[order(species, year), ]



xl <- lapply(1:200, function(i){
  print(i)
  obs <- table(X$recordedBy) |>
    (\(.) {.[. >= 1]})() |> # remove observers with less than x observations
    names() |>
    sample(replace = TRUE) |>
    table() |>
    as.data.table() |>
    setnames(c("recordedBy", "nreps"))
  x <- merge(X, obs, all.x = TRUE)
  x <- x[!is.na(nreps)]
  x <- x[rep(1:nrow(x), times = x$nreps), ]

  sy <- x[, .(n = .N), by = c("species", "year")]
  y <- x[, .(N = .N), by = c("year")]
  #y <- x[kingdom == "Plantae", .(N = .N), by = c("year")]
  x <- rbind(sy, add)
  x <- x[!duplicated(x, by = c("species", "year")), ][order(species, year),]
  x <- merge(x, y, all.x = TRUE)
  #x[, p := (p - mean(p)) / sd(x$p), by = "species"]
  #x[, p := p / max(p), by = "species"]
  x <- x[order(species, year),]
  x[, p := n / N]
  x <- x[, .(species, year, p)]
  x$p
})

p <- do.call("cbind", xl) |> as.data.table()
p <- cbind(add[ ,.(species, year)], p)
l <- split(p, p$species)



plotsp <- function(sp){
  x <- l[[sp]]
  mat <- as.matrix(x[, 3:ncol(x)])
  xlim <- range(x$year) + c(-0.5, 0.5)
  ylim <- c(0, max(mat, na.rm = TRUE))
  plot(x$year, x$V1, type = "n", xlim = xlim, ylim = ylim, xaxt = "n", yaxt = "n")
  invisible(lapply(2:ncol(x), function(i){
    lines(x$year, x[[i]], col = adjustcolor("black", 0.025))
  }))
  ci <- t(apply(mat, 1, quantile, prob = c(0.025, 0.975), na.rm = TRUE))
  polygon(c(x$year, rev(x$year), x$year[1]), c(ci[ , 1], rev(ci[ , 2]), ci[ 1, 1]), col = adjustcolor("red", 0.25), border = NA)
  lines(x$year, ci[, 1], col = adjustcolor("red", 0.5))
  lines(x$year, ci[, 2], col = adjustcolor("red", 0.5))
  mtext(side = 3, line = -3, adj = 0.5, text = sp, font = 3, cex = 1)
  axis(1, at = pretty(xlim), tcl = 0.1, mgp = c(1, 0.05, 0), cex.axis = 0.75, col = "grey80")
  axis(2, at = pretty(ylim), label = pretty(ylim) * 100, tcl = 0.1, mgp = c(1, 0.15, 0), las = 2, cex.axis = 0.75, col = "grey80")
  box(col = "grey80")
}

spe <- eeengbif$species[1:24]
spe <- c("Lymantria dispar", "Aglais io", "Vanessa atalanta", "Procyon lotor", "Odocoileus virginianus", "Trillium erectum", "Nymphalis antiopa", "Papilio canadensis", "Sciurus carolinensis")
par(mfrow = n2mfrow(length(spe)), mar = c(0, 3, 0, 0), oma = c(4, 4, 4, 4))
lapply(spe, function(i){
  plotsp(i)
})
mtext(side = 1, outer = TRUE, line = 2, text = "Années", font = 2)
mtext(side = 2, outer = TRUE, line = 2, text = "(Nb obs. / Nb obs. tot) * 100", font = 2)
par(mfrow = c(1, 1), mar = c(4, 4, 1, 1), oma = c(0, 0, 0, 0))


#observations <- d[, .(n = .N), by = c("year")][order(-year),]
#observers <- unique(d, by = c("year", "recordedBy")) |>
#  _[, .(n = .N), by = c("year")] |>
#  _[order(-year),]

#par(new = TRUE)
#plot(observations$year, observations$n, type = "l", yaxt = "n", xlab = "", ylab = "", lwd = 5, col = adjustcolor("black", 0.25))
#par(new = TRUE)
#plot(observers$year, observers$n, type = "l", yaxt = "n", xlab = "", ylab = "", lwd = 5, col = adjustcolor("black", 0.25))

##############
#hist(table(X$species), breaks = 100)
tab <- rev(sort(table(X$species)))
sp <- eee[eee %in% names(tab)]
sort(match(eee, names(tab)))
#sp <- names(rev(sort(tab)))[199:250]
plot(l[[1]]$year, l[[1]]$V1, type = "n", ylim = c(0, 0.015), xlab = "Year", ylab = "Nb obs / Total")
invisible(o <- lapply(sp, function(j){
  x <- l[[j]]
  mat <- as.matrix(x[, 3:ncol(x)])
  #invisible(lapply(2:ncol(x), function(i){
  #  lines(x$year, x[[i]], col = adjustcolor("black", 0.025))
  #}))
  ci <- t(apply(mat, 1, quantile, prob = c(0.025, 0.975), na.rm = TRUE))
  offset <- 0#tail(((ci[, 2] - ci[, 1])/2) + ci[, 1], 1)
  polygon(c(x$year, rev(x$year), x$year[1]), c(ci[ , 1], rev(ci[ , 2]), ci[ 1, 1]) - offset, col = adjustcolor("black", 0.05), border = NA)
  #lines(x$year, ci[, 1], col = adjustcolor("red", 0.5))
  #lines(x$year, ci[, 2], col = adjustcolor("red", 0.5))

}))


### mean/median nb of observations
X[ , .(n = .N), by = .(recordedBy, year)] |>
  _[ , .(moy = mean(n), med = median(n)), by = .(year)] |>
  _[order(year), ]

### mean/median cumulative nb of observations
X[ , .(n = .N), by = .(recordedBy, year)] |>
  _[order(recordedBy, year), ] |>
  _[ , n := cumsum(n), by = .(recordedBy)] |>
  _[ , .(moy = mean(n), med = median(n)), by = .(year)] |>
  _[order(year), ]

### proportions of user with fewer than
than <- 50
X[ , .(n = .N), by = .(recordedBy, year)] |>
  _[order(recordedBy, year), ] |>
  _[ , n := cumsum(n), by = .(recordedBy)] |>
  _[ , .(p = sum(n > than)/.N), by = .(year)] |>
  _[order(year), ] |>
  plot(type = "b")


p <- c(1, 1.05, 1.1, 1.25, 1.5, 2, 5)

y <- rev(unique(c(rev(1/p), 1*p)))
lab <- y
#lab <- unique(c((rev(p) - 1) * 100, - ((1 / (1/p)) - 1) * 100))
x <- seq_along(y)

plot(x, log(y), yaxt = "n")
axis(2, at = log(y), label = round(lab, 2), las = 2, tcl = -0.1, mgp = c(0.5, 0.25, 0), cex.axis = 0.5)


# when talking about group, mention target group à la Phillips et al. Maxent
# contrast inat data with ebird, mention ebird is not affected by the same bias as PO data
# the simpler the model, the more the uncertainty is underestimated
# Andy Dobson Hmm, shouldn't K be the # of locations where at A/2..?
# How to measure if the level of sampling is enough at different places within the convex hull? If we have a spatial effect, perhaps it is less of a problem?
# Oksanen experts focus on are species and inexperienced produce more reliable results
# at first experts use the platform, but after a while, less experienced gather and need a way to take this into account


prob <- 0.00015
ll<- lapply(1:1000, function(i){
  d[, .(n = .N), by = .(year)][order(year), .(year, n)] |>
    _[ , p := rbinom(1, n, prob = prob) / n, by = .(year)]
}) |> rbindlist() |>
  _[, .(low = quantile(p, prob = 0.025), high = quantile(p, prob = 0.975)), by = .(year)] |>
  _[order(year), ]

plot(ll$year, ll$low, type = "l", ylim = c(0, 5 * prob))
lines(ll$year, ll$high)





##############################################
### GAMs #####################################

x <- 2010:2024
y <- seq(2, 0.5, length.out = length(x))

#dat$nobs <- rbinom(nrow(dat), size = dat$ntot, prob = 0.001)
#dat$ratio <- dat$nobs/dat$ntot / tail(dat$nobs/dat$ntot, 1)

dats <- lapply(res[!sapply(res, is.null)], function(i){
  if(tail(i$nobs, 1) == 0){
    moy <- sum(i$nobs)/sum(i$ntot)
  } else {
    moy <- tail(i$nobs/i$ntot, 1)
  }
  i$ratio <- i$nobs / i$ntot / moy
  i
})
dats


plot(x, y, type = "n", log = "y", yaxt = "n", ylim = c(0.01, 100), cex = 0)

lapply(names(dats), function(sp){
  #sp <- sample(names(dats),1)
  dat <- dats[[sp]]
  x <- dat$year
  y <- dat$ratio
  #points(x, y, type = "b", log = "y", pch = 16, col = adjustcolor("black", 0.5), cex = 4 * dat$ntot/max(dat$ntot))
  abline(log(1), 0, lty = 3)
  v <- 2:10
  at <- c(rev(1/v), 1, v)
  axis(2, at = at, label = round(at, 2), las = 2)

  m <- gam(cbind(nobs, ntot) ~ s(year, k = 4, m = 1), family = binomial, data = dat)
  v <- seq(min(dat$year), max(dat$year), length.out = 100)
  #p <- predict(m, data.frame(year = v), se.fit = TRUE)
  #p <- data.frame(fit = p$fit, low = p$fit - p$se.fit*1.96, high = p$fit + p$se.fit*1.96)
  p <- confint(m, parm = "s(year)", type = "simultaneous", data = data.frame(year = v), shift = TRUE) |>
    as.data.frame() |>
    _[,c(".estimate", ".lower_ci", ".upper_ci")] |>
    setnames(c("fit", "low", "high"))
  p <- as.data.frame(lapply(p, boot::inv.logit))
  p <- p / tail(dat$nobs/dat$ntot/dat$ratio, 1)
  #lines(v, p$fit, col = "red")
  polygon(c(v, rev(v), v[1]), c(p$high, rev(p$low), (p$high)[1]), border = NA, col = adjustcolor("black", 0.025))
  #sp
  #sum(dat$nobs)/sum(dat$ntot)
})



#a <- rbindlist(res) |>
#  _[ , .(n = sum(nobs), tot = max(ntot)), by = .(year)]
#plot(a$year, a$n / a$tot, type = "b")



##########################################################################
### overall EEE rate #####################################################
##########################################################################

xl <- lapply(1:100, function(i){
  print(i)
  x <- d
  #x <- x[phylum != "Chordata", ]
  x <- x[year >= 2008, ]
  obs <- table(x$recordedBy) |>
    (\(.) {.[. >= 1]})() |> # remove observers with less than x observations
    names() |>
    sample(replace = TRUE) |>
    #sample(2000) |> # for fun, just keep x observers each time
    table() |>
    as.data.table() |>
    setnames(c("recordedBy", "nreps"))
  #x <- merge(x, obs, all.x = TRUE)
  #x <- x[!is.na(nreps)]
  x <- x[obs, on = "recordedBy"] # replaces the two previous lines
  print(nrow(x))
  x <- x[rep(1:nrow(x), times = x$nreps), ]
  x <- x[, ntot := .N, by = c("year")] |>
    _[, .(nobs = .N), by = c("species", "family", "order", "class", "year", "ntot")] |>
    _[species %in% eee, ] |>
    #_[order %in% "Lepidoptera", ] |>
    _[ , .(n = sum(nobs)), by = .(year, ntot)] |>
    _[order(year), ]
  x
})

par(mar = c(4, 4, 1, 1))
plot(xl[[1]]$year, xl[[1]]$n / xl[[1]]$ntot, type = "n", ylim = c(0.0005, 0.05), xlab = "Années", ylab = "Nb obs. / Nb obs. tot.")
lapply(xl, function(i){
  lines(i$year, i$n / i$ntot, col = adjustcolor("black", 0.15))
})




#########################################################################
### per observer bonus ##################################################

x <- d
x <- unique(x[ , .(recordedBy, species, year)]) |>
      _[ , .(n = .N), by = .(year)]




add <- expand.grid(species = unique(d$species), year = unique(d$year), recordedBy = unique(d$recordedBy), n = 0, stringsAsFactors = FALSE) |>
 as.data.table() |>
 _[order(species, year), ]


x <- d
x <- unique(x[ , .(year, recordedBy)]) |>
  split(x = _, by = "year")




 ########################################################################
 ### Spatial expansion ##################################################
 ########################################################################

 library(geodata)
 library(rmapshaper)

 can <- gadm("CAN", path = "/home/frousseu/Downloads") |> st_as_sf()
 qc <- can[can$NAME_1 == "Québec",] |> ms_simplify(0.01) |> st_transform(6624)

 d <- fread("/home/frousseu/Downloads/0067051-241126133413365.csv")
 d <- d[institutionCode == "iNaturalist", ]
 d[, date := eventDate]
 d[, jul := as.integer(format(as.Date(date),"%j"))]
 d[, year := as.integer(substr(date,1,4))]
 d <- d[year >= 2012, ]
 d <- d[species != "",]

 sp <- "Rhamnus cathartica"
 x <- d[species == sp, ] |>
   as.data.frame() |>
   st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) |>
   st_transform(6624)
 hull <- x |>
   st_union() |>
   st_convex_hull()

 lx <- split(x, x$year)
 lx <- lx[sort(names(lx))]
 nx <- sapply(lx, nrow)
 lxs <- sapply(seq_along(lx), function(i){
   do.call("rbind", lx[1:i]) |>
     st_union() |>
     st_convex_hull() |>
     st_area() |>
     as.numeric()
 })

 ds <- d |>
   as.data.frame() |>
   st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) |>
   st_transform(6624)

 ds <- ds[hull, ]

 ld <- split(ds, ds$year)
 ld <- ld[names(lx)]

 years <- as.integer(names(lx))

 plot(years, lxs, ylim = c(0, max(lxs) * 1.5), lwd = 4, col = "red", type = "l", ylab = "Area", main = sp)

 lapply(1:10, function(j){
   lds <- sapply(seq_along(nx), function(i){
     s <- do.call("rbind", ld[1:i])
     set.seed(j)
     no <- sapply(ld, nrow)
     seqs <- lapply(seq_along(no), function(i){
       seq_len(no[i]) + sum(c(0, no)[1:i])
     })
     samps <- unlist(lapply(seq_along(seqs), function(i){
       sample(seqs[[i]], nx[i])
     }))
     s <- s[samps[1:sum(nx[1:i])], ] |>
       st_union() |>
       st_convex_hull() |>
       st_area() |>
       as.numeric()
     s
   })
   lines(years, lds, type = "l", lwd = 2, col = adjustcolor("black", 0.1))
 })


 #################################
 #################################
 #################################

 sprange <- lapply(seq_along(lx), function(i){
   do.call("rbind", lx[1:i]) |>
     st_union()
 })


 lds <- sapply(seq_along(nx), function(i){
   s <- do.call("rbind", ld[1:i])
   set.seed(j)
   no <- sapply(ld, nrow)
   seqs <- lapply(seq_along(no), function(i){
     seq_len(no[i]) + sum(c(0, no)[1:i])
   })
   samps <- unlist(lapply(seq_along(seqs), function(i){
     sample(seqs[[i]], nx[i])
   }))
   s <- s[samps[1:sum(nx[1:i])], ] |>
     st_union()
   s
 })


 plot(st_geometry(st_crop(qc, hull)), mar = c(0, 0, 0, 0))
 plot(st_geometry(hull), add = TRUE)
 lapply(seq_along(sprange), function(i){
   plot(st_geometry(lds[[i]]), add = TRUE, pch = 16, col = "black")
   plot(st_geometry(sprange[[i]]), add = TRUE, pch = 16, col = "red")
   Sys.sleep(1)
 })


 tm_shape(qc) +
   #tm_bubbles(size = "pop2020") +
   tm_basemap("USGS.USImageryTopo", zoom = 1) +
   tm_borders()



#########################################################
### get observers from inat #############################

get_all_observers <- function(){
  i <- 1
  a <- list()
  url <- "https://api.inaturalist.org/v1/observations/observers?place_id=13336&page=1"
  x <- fromJSON(url)
  while(sum(i * 500) < x$total_results){
    print(i)
    url <- paste0("https://api.inaturalist.org/v1/observations/observers?place_id=13336&page=", i)
    x <- fromJSON(url)
    res <- data.frame(
      login =x$results$user$login,
      name =x$results$user$name,
      id =x$results$user$id,
      created_at =x$results$user$created_at,
      observations_count =x$results$user$observations_count
    )
    a[[i]] <- res
    i <- i + 1
  }
  a
}






