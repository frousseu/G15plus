

###############################################################
### Rangs de précarité ########################################
###############################################################

set.seed(123)
n <- 50
years <- seq(1990, 2024, by = 5)#1990:2024
change <- rpois(n * length(years), 0.025) * sample(c(1, -1), n * length(years), prob = c(0.5, 0.5), replace = TRUE)
m <-matrix(change, ncol = length(years))
m[, 1] <- 0

y <- t(apply(m, 1, cumsum))
y <- apply(y, 2, sum)


par(mar = c(3, 3, 1, 1))
plot(years, y, type = "b", lwd = 2, pch = 16, col = adjustcolor("black", 0.75), ylim = range(c(y, -y)), yaxt = "n", xaxt = "n", xlab = "", ylab = "")
axis(2, las = 2, mgp = c(2, 0.25, 0), tcl = -0.2)
axis(1, mgp = c(2, 0.25, 0), tcl = -0.2)
mtext(side = 1, line = 1.5, text = "Années", font = 2)
mtext(side = 2, line = 1.75, text = " ( précarité + )  Rang de précarité  ( précarité - )", font = 2)
abline(0, 0, lty = 3)


###############################################################
### Rangs CDPNQ ###############################################
###############################################################

library(readxl)
library(data.table)


#sp <- "Lampropeltis"
#(sp <- grep(sp, ra$SNAME, value = TRUE)[1])
#cat("\014")
#split(rp, rp$SNAME)[sp]
#split(ra, ra$SNAME)[sp]
#l[sp]


ra <- read_excel("/home/frousseu/Documents/uds/biodiversitéquébec/RangSActuel_v2024-10-07.xlsx") |> setDT()
#split(ra, ra$SNAME)
names(ra)[which(names(ra) == "DATE dernière évaluation Rang S")] <- "date"
ra[ , bd := "actuel"]

rp <- read_excel("/home/frousseu/Documents/uds/biodiversitéquébec/RangSPassé_v2024-10-07.xlsx") |> setDT()
#split(rp, rp$SNAME)
names(rp)[which(names(rp) == "Ancienne valeur")] <- "SRANK"
names(rp)[which(names(rp) == "Date ancienne valeur")] <- "previous_date"
names(rp)[which(names(rp) == "Date changement")] <- "date"
rp[ , bd := "passé"]

keepp <- c("SNAME", "SCOMNAME", "GRANDGROUPE", "GGROUPE", "SRANK", "bd", "date", "previous_date")
keepa <- keepp[-length(keepp)]

r <- rbind(ra[, ..keepa], rp[, ..keepp], fill = TRUE)
r <- r[order(SNAME, date, rev(bd)), ]
tab <- rev(sort(table(r$SRANK))); tab
grep("X", names(tab))
r[, SRANK := gsub("X", "0", SRANK)] # replace with 0 the worst case
r[, SRANK := gsub("H", "0", SRANK)]

text <- r$SRANK
n <- regmatches(text, gregexpr("\\d+", text)) |>
             sapply(function(i){mean(as.integer(i))})
n <- ifelse(is.nan(n), NA, n)
r[, n := n]

l <- split(r, r$SNAME) |>
      lapply(function(x){
        x[order(SNAME, date, -bd),]
        x[, date := as.Date(substr(date, 1, 10))]
        x[, previous_date := as.Date(substr(previous_date, 1, 10))]
        y <- tail(x, 1)
        y[ , date := Sys.Date()]
        x <- rbind(x, y) # add actual ranks and date
        y <- head(x, 1)
        y[ , date := previous_date]
        x <- rbind(y, x) # add first rank and date
        x[ , ndiff := n-n[!is.na(n)][1]]
        x
      })

l <- l[sapply(l, function(i){
  #!(all(is.na(i$n)) | is.na(tail(i$n, 1)))
  w <- which(!is.na(i$date) & !is.na(i$n))
  if(any(w)){
    if(nrow(unique(i[w, c("date", "n")])) < 2){
      FALSE
    }else{
      TRUE
    }
  }else{
    FALSE
  }
})] # remove all n = NA or n = NA now


mgp <- c(1, 0.25, 0)
tcl <- -0.2
cex.axis <- 0.75
vdate <- seq.Date(as.Date("1970-01-01"), as.Date("2040-01-01"), by = "5 year")
colgrid <- adjustcolor("black", 0.3)

#################################
### Show an example species #####
sp <- "Hemidactylium scutatum"
#(sp <- sample(r$SNAME, 1))
(name <- grep(sp, names(l), value = TRUE))
(x <- l[[name[1]]])
plot(x$date, x$n, ylim = c(0, 5), type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n")
abline(h = seq(0, 5, by = 0.5), lty = 3, col = colgrid)
abline(v = vdate, lty = 3, col = colgrid)
lines(x$date, x$n, type = "b", yaxt = "n", lwd = 2, pch = 16, cex = 1.5)
mtext(side = 3, line = 0, text = sp, font = 2)
mtext(side = 2, line = 2, text = "Rang de précarité", font = 2)
mtext(side = 1, line = 1.5, text = "Années", font = 2)
axis(2, at = 0:5, label = paste0(c("SH/SX", paste0("S", 1:5))), las = 2, mgp = mgp, tcl = tcl, cex.axis = cex.axis, col = "transparent")
axis.Date(1, mgp = mgp, tcl = tcl, cex.axis = cex.axis, col = "transparent")
a <- approx(x$date, x$n, ties = "ordered")
#lines(a$x, a$y, col = "red")


sp <- "Hemidactylium scutatum"
#(sp <- sample(r$SNAME, 1))
(name <- grep(sp, names(l), value = TRUE))
(x <- l[[name[1]]])
plot(x$date, x$ndiff, ylim = c(-4, 3.5), type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n")
abline(h = seq(-4, 3.5, by = 1), lty = 3, col = colgrid)
abline(v = vdate, lty = 3, col = colgrid)
axis(2, las = 2, mgp = mgp, tcl = tcl, cex.axis = cex.axis, col = "transparent")
axis.Date(1, mgp = mgp, tcl = tcl, cex.axis = cex.axis, col = "transparent")
lines(x$date, x$ndiff, type = "b", yaxt = "n", lwd = 2, pch = 16, cex = 1.5)
mtext(side = 3, line = 0, text = sp)
mtext(side = 1, line = 1.5, text = "Années", font = 2)
mtext(side = 2, line = 1.5, text = "Différence par rapport au rang initial", font = 2)


#################################
### Plot all species ############
xout <- seq(as.Date("1990-01-01"), Sys.Date(), by = 1)
plot(as.Date(0), 0, , xlim = range(xout) + c(0, 5 * 365), ylim = c(-4, 3.5), type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n")
abline(h = seq(-4, 3.5, by = 1), lty = 3, col = colgrid)
abline(v = vdate, lty = 3, col = colgrid)
axis(2, las = 2, mgp = mgp, tcl = tcl, cex.axis = cex.axis, col = "transparent")
axis.Date(1, mgp = mgp, tcl = tcl, cex.axis = cex.axis, col = "transparent")
lapply(l, function(i){
  offset <- rnorm(1, 0, 0.10)
  lines(i$date, offset + i$ndiff, ylim = c(0, 5), lwd = 1, col = adjustcolor("black", 0.10))
  text(tail(i$date, 1) + 265, tail(offset + i$ndiff, 1), label = i$SNAME[1], cex = 0.35, adj = c(0, 0.5), xpd = TRUE)
})
#abline(0, 0, lty = 3, col = "red")
mtext(side = 1, line = 1.5, text = "Années", font = 2)
mtext(side = 2, line = 1.5, text = "Différence par rapport au rang initial", font = 2)


### Add indicator
ll <- lapply(seq_along(l), function(i){
  x <- l[[i]]
  res <- as.data.frame(approx(x$date, x$ndiff, xout = xout, ties = "ordered"))
  names(res) <- c("date", names(l)[i])
  res[, 2]
})
I <- do.call("cbind", ll) |>
  apply(1, mean, na.rm = TRUE)

lines(xout, I, col = adjustcolor("red", 0.75), lwd = 4)


##################################
### Add a target species #########
#sp <- "Ambystoma laterale"
name <- grep(sp, names(l), value = TRUE); name
x <- l[[name[1]]]
lines(x$date, x$ndiff, type = "b", col = adjustcolor("orange",0.95), lwd = 2, pch = 16, cex = 1.5)
text(x$date[1] - 365, x$ndiff[1], x$SRANK[1], col = adjustcolor("orange",0.95), adj = c(1, 0.5), font = 2)
text(x$date[nrow(x)] + 265, x$ndiff[nrow(x)], x$SRANK[nrow(x)], col = adjustcolor("orange",0.95), adj = c(0, 0.5), xpd = TRUE, font = 2)
mtext(side = 3, line = -2, text = "Différence moyenne", col = adjustcolor("red", 0.75), adj = 0.05, font = 2, cex = 1.25)
mtext(side = 3, line = -4, text = paste0("", sp), col = adjustcolor("orange", 0.95), adj = 0.05, xpd = TRUE, font = 2, cex = 1.25)
#legend("topleft", legend = c("Différence moyenne", paste("Espèce:", sp)), lwd = c(4, 2), pch = c(NA, 16), col = adjustcolor(c("red", "orange"), 0.75), bty = "n", inset = c(0.025, 0), pt.cex = 1.5, cex = 1.25)



#########################################################
### check if the change increases with time #############
library(mgcv)
ll <- do.call("rbind", lapply(l, function(i){
  x <- i[!(is.na(i$date) | is.na(i$ndiff)), ]
  c(as.numeric(diff(range(x$date), units = "days")) / 365, x$ndiff[nrow(x)] - x$ndiff[1])
})) |> as.data.frame() |> setNames(c("years", "diff"))

years <- 0:40
plot(ll$years, ll$diff, xlim = c(0, 40))
m <- gam(diff ~ s(years), data = ll)
p <- predict(m, data.frame(years = years), se.fit = TRUE)
lines(years, p$fit, col = "red")
polygon(c(years, rev(years), years[1]), c(p$fit + p$se.fit*1.96, rev(p$fit - p$se.fit*1.96), (p$fit + p$se.fit*1.96)[1]), border = NA, col = adjustcolor("red", 0.20))


############################################################
### EMV ####################################################

library(data.table)
library(ebirdst)

eb <- ebirdst_runs |>
  as.data.table() |>
  _[ , .(scientific_name, breeding_start, breeding_end)] |>
  _[ , start := substr(breeding_start, 6, 10)] |>
  _[ , end := substr(breeding_end, 6, 10)] |>
  _[ , species := scientific_name] |>
  _[ , scientific_name := NULL]


classes <- c("Amphibia", "Reptilia", "Aves", "Chondrichthyes", "Petromyzontida", "Actinopterygii", "Mammalia")
replacement <- c("Reptiles", "Reptiles", "Birds", "Fish", "Fish", "Fish", "Mammals")


# https://www.donneesquebec.ca/recherche/dataset/liste-de-la-faune-vertebree-du-quebec
emv <- fread("/home/frousseu/Downloads/LFVQ_18_04_2024.csv") |>
  #_[ , .(Nom_francais, Nom_scientifique, GRAND_GROUPE, CLASSE, ORDRE, SOUS_ESPECE_POP, Rang_S, STATUT_LEMV)] |>
  _[STATUT_LEMV %in% c("Menacée", "Susceptible", "Vulnérable"), ] |>
  _[, species := sapply(strsplit(Nom_scientifique, " "), function(i){paste(i[1:2], collapse = " ")})] |>
  _[ -grep("Requin|Marsouin|Rorqual|Phoque|Baleine|Béluga", Nom_francais), ] |>
  _[ , group := replacement[match(CLASSE, classes)]] |>
  _[ , statut := STATUT_LEMV] |>
  _[ , .(species, statut, group)] |>
  unique()


emv <- eb[emv, on = .(species)] |>
  _[ , .(species, group, statut, start, end)]


#table(emv$STATUT_LEMV)
table(emv$GRAND_GROUPE)
table(emv$CLASSE)
table(emv$ORDRE)



