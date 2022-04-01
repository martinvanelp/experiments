#
# ANALYSIS of STOCK TIME SERIES
#

# LIBRARIES and SETTINGS
library(quantmod)
library(lattice)

# FUNCTIONS
stockAnalysis <- function(symbol, from) {
        
        # set locale to make Date readable
        lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
        
        # gather data
        data <- getSymbols(symbol, 
                             from = from, 
                             to   = format(Sys.Date(), "%Y-%m-%d"),
                             env  = NULL)
        names(data) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
        
        # organize data
        Date <- as.Date(index(data),'%d-%B-%y')
        Year <- as.factor(format(index(data), "%Y"))
        Day  <- as.Date(format(index(data), "%d-%m"), "%d-%m")
        Close <- data$Close
        
        idata <- data.frame(Date, Year, Day, Close, row.names = NULL)
        
        # select data
        pdata <- idata[Date >= "1990-01-01" &
                               Date < "2022-01-01",]
        
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
symbol <- 'DIA'
from   <- '1990-01-01'
png(file = "DJIA.png")
        stockAnalysis(symbol, from)
dev.off()

## S&P 500 ETF
symbol <- 'SPY'
from   <- '1990-01-01'
png(file = "SP500.png")
stockAnalysis(symbol, from)
dev.off()

## EURO STOXX 50 ETF
symbol <- 'FEZ'
from   <- '1990-01-01'
png(file = "ES50.png")
stockAnalysis(symbol, from)
dev.off()

## GOLD ETF
symbol <- 'GLD'
from   <- '1990-01-01'
png(file = "Gold.png")
stockAnalysis(symbol, from)
dev.off()
