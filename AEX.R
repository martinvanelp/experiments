## AEX data
#  http://www.skuzet.nl/download/historieken/category/2-historische-koersen-aex-fondsen.html

## SHARED FUNCTIONS
make.numeric <- function(x) { x <- sub(",", ".", x); as.numeric(x) }

## MAIN
#  load and init data
idata <- read.table("AEX.csv", header = FALSE, sep = ";", skip = 1,
                   colClasses = "character", 
                   col.names = c("Naam", "symbool", "datum", "open",
                                 "hoog", "laag", "slot", "volume", "NA"))

idata$datum <- as.Date(idata$datum, "%d-%m-%Y")
idata$jaar  <- as.factor(format(idata$datum, "%Y"))
idata$dag   <- as.Date(format(idata$datum, "%d-%m"), "%d-%m")

idata$open <- make.numeric(idata$open)
idata$hoog <- make.numeric(idata$hoog)
idata$laag <- make.numeric(idata$laag)
idata$slot <- make.numeric(idata$slot)

pdata <- subset(idata, idata$datum >= "1999-01-01")

#  base plot data
plot(pdata$datum, pdata$slot, type = "l")

# lattice plot data
library(lattice)
xyplot(slot ~ dag | jaar, data = pdata, panel = function(x, y, ...) {
        panel.xyplot(x, y, ...)     ## First call default panel function
        panel.lmline(x, y, col = 2, lwd = 2) ## Overlay a simple linear regression line
})
