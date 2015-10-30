# 
# VERHUISDE PERSONEN, een analyse
# Tussen provincies, 2014
#

library(gtools)

# data klaarzetten
data <- read.csv2("Tussen_gemeenten_ver_281015194711.csv", 
                  skip = 3, nrows = 14,
                  colClasses = "character")[-c(1:2), -2]
prov14 <- data[,-1]
names(prov14) <- gsub("..PV.","", data[,1])
prov14 <- sapply(prov14, as.integer)
row.names(prov14) <- gsub("..PV.","", data[,1])

best14 <- prov14    # kolom = bestemming, rij = herkomst
herk14 <- t(prov14) # kolom = herkomst, rij = bestemming

# herbruikbare variabelen
coloring <- c(rainbow(12, alpha = 1)[odd(1:12)], 
              rainbow(12, alpha = 1)[even(1:12)])
bron <- "Bron: CBS, bewerking @martinvanelp"

# enkele barplots
par(mai = c(1,2,1,1), 
    oma = c(1.5,.5,.5,.5),
    mgp = c(0.75,2,0),
    mfcol = c(1,1))

# barplot bestemming op as
barplot(best14[,ncol(best14):1], 
        main = "Bestemming verhuisde personen, provincies, 2014",
        ylab = "Bestemming", horiz = TRUE, 
        las = 1, xlab = "Personen", xlim = c(0, 180000),
        legend.text = TRUE, args.legend = 
                list(title = "Herkomst", bty = "n", border = NA),
        border = NA, col = coloring)
mtext(bron, cex = .75, outer = TRUE, side = 1, adj = 1)

# barplot herkomst op as
barplot(herk14[,ncol(herk14):1], 
        main = "Herkomst verhuisde personen, provincies, 2014",
        ylab = "Herkomst", horiz = TRUE, 
        las = 1, xlab = "Personen", xlim = c(0, 180000), 
        legend.text = TRUE, args.legend = 
                list(title = "Bestemming", bty = "n", border = NA),
        border = NA, col = coloring)
mtext(bron, cex = .75, outer = TRUE, side = 1, adj = 1)

# multi barplots
par(mai = c(.2,.5,.5,.5), 
    oma = c(1.5,1.5,3.5,.5),
    mgp = c(3,1,0),
    mfrow = c(4,3))

# 12 plots met provincies als bestemming
for (i in 1:12) {
        barplot(best14[,i] / sum(best14[,i]), 
             main = colnames(best14)[i],
             ylab = "", ylim = c(0,1),
             xlab = "", names.arg = "",
             col = coloring[i], 
             border = 1, lwd = 3)
}

title(main = "Bestemming verhuisde personen, provincies, 2014",
      outer = TRUE, 
      cex.main = 1.5,
      cex.sub = 1)
mtext("(kolommen geven herkomst, volgorde als titels)",
      outer = TRUE,
      cex = 0.8)
mtext("Aandeel", cex = 1, outer = TRUE, side = 2, adj = .5)
mtext(bron, cex = .75, outer = TRUE, side = 1, adj = 1)

# 12 plots met provincies als herkomst
for (i in 1:12) {
        barplot(herk14[,i] / sum(herk14[,i]), 
                main = colnames(herk14)[i],
                ylab = "", ylim = c(0,1),
                xlab = "", names.arg = "",
                col = coloring[i], 
                border = 1, lwd = 3)
}

title(main = "Herkomst verhuisde personen, provincies, 2014",
      outer = TRUE, 
      cex.main = 1.5,
      cex.sub = 1)
mtext("(kolommen geven bestemming, volgorde als titels)",
      outer = TRUE,
      cex = 0.8)
mtext("Aandeel", cex = 1, outer = TRUE, side = 2, adj = .5)
mtext(bron, cex = .75, outer = TRUE, side = 1, adj = 1)
