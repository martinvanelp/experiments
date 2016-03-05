#
# ANALYSIS of STOCK TIME SERIES
#

# LIBRARIES and SETTINGS
library(lattice)

# FUNCTIONS
stockAnalysis <- function(symbol, from) {
        
        # set locale to make Date readable
        lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
        
        # gather data
        url <- paste0("https://www.google.com/finance/historical?q=",
                      symbol, "&startdate=", from, "&output=csv")
        idata <- read.csv(url, stringsAsFactors = FALSE)
        names(idata)[1] <- "Date"
        
        # organize data
        idata$Date <- as.Date(idata$Date,'%d-%B-%y')
        idata$Year <- as.factor(format(idata$Date, "%Y"))
        idata$Day  <- as.Date(format(idata$Date, "%d-%m"), "%d-%m")
        
        # select data
        pdata <- idata[idata$Date >= "1990-01-01" &
                               idata$Date < "2016-01-01",]
        
        # base plot data
        # plot(pdata$Date, pdata$Close, type = "l")
        
        # lattice plot data
        plot <- 
                xyplot(Close ~ Day | Year, data = pdata, main = symbol,
                       panel = function(x, y, ...) {
                               c <- lm(y ~ x)$coefficient[2]
                               if(c >  0) { panel.fill(col = "lightgreen") }
                               if(c <= 0) { panel.fill(col = "coral") }
                               ## First call default panel function
                               panel.xyplot(x, y, ...)
                               ## Overlay a simple linear regression line
                               panel.lmline(x, y, col = 2, lwd = 2)
                       })
        
        # reset locale
        Sys.setlocale("LC_TIME", lct)
        
        # output
        plot
}

# MAIN
## DJIA ETF
symbol <- 'NYSEARCA:DIA'
from   <- '1990-01-01'
png(file = "DJIA.png")
        stockAnalysis(symbol, from)
dev.off()

## S&P 500 ETF
symbol <- 'NYSEARCA:SPY'
from   <- '1990-01-01'
png(file = "SP500.png")
stockAnalysis(symbol, from)
dev.off()

## EURO STOXX 50 ETF
symbol <- 'NYSEARCA:FEZ'
from   <- '1990-01-01'
png(file = "ES50.png")
stockAnalysis(symbol, from)
dev.off()

## GOLD ETF
symbol <- 'NYSEARCA:GLD'
from   <- '1990-01-01'
png(file = "Gold.png")
stockAnalysis(symbol, from)
dev.off()
