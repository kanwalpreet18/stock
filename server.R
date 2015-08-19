library(quantmod)
library(parallel)
library(shiny)

source("priceVolEarnings.R")

load("earningsData.rda")
load("priceData.rda")

list.stocks <- read.csv("ListOfScrips.csv", header = TRUE) # getting the bse stock list
stock.codes <- list.stocks$Security.Code
list.stocks$stock.ids <- paste0(list.stocks$Security.Id, ".BO")

CleanEarningsData <- function(x){
    tryCatch({
        rownames(x) <- x[ ,1]
        x <- x[-1, -1]
        x <- as.data.frame(t(x))
        colnames(x) <- c("sales", "operatingProfit", "otherIncome", "EBIT", "interest",
                         "depreciation", "tax", "netProfit", "EPS")
        rownames(x) <- paste0("30", gsub(" ", "", rownames(x)))
        x <- xts(x, as.Date(rownames(x), "%d%b%y"))},
             error = function(e) NULL)}

CleanPriceData <- function(x){
    x <- to.weekly(x)
    colnames(x) <- c("open", "high", "low", "close", "volume", "adjusted")
    x <- x["2013-03-01/2015-08-03"]
}

AdjustStockSplit <- function(stock.code, stock.id){
    stocksplit.data <- read.csv("stocksplits2010_15.csv", header = TRUE,
                                stringsAsFactors = FALSE)
    stocksplit.data <- stocksplit.data[ , c("Start.Date", "Event.Name")]
    stocksplit.data$Start.Date <- as.Date(stocksplit.data$Start.Date, "%d-%b-%y")

    stocksplit.data$stock <- sapply(stocksplit.data$Event.Name, function(x){
                                        tryCatch(strsplit(strsplit(x,
                                                                   "For ")[[1]][[2]],
                                                          " ")[[1]][1],
                                                 error =  function (e) NULL)})
    stocksplit.data <- stocksplit.data[!is.null(stocksplit.data$stock)]
    stocksplit.data <- stocksplit.data[grep(".BO", stocksplit.data$stock), ]

    stocksplit.data$stock <- gsub(".BO", "", stocksplit.data$stock)
    
    stocksplit.data$split <- sapply(stocksplit.data$Event.Name, function(x){
                                        strsplit(strsplit(x,
                                               "For ")[[1]][[2]], " ")[[1]][4]})
    stocksplit.data$split <- sapply(stocksplit.data$split, function(x){
                                        n <- as.numeric(strsplit(x, "-")[[1]][1])
                                        d <- as.numeric(strsplit(x, "-")[[1]][2])
                                        split <- n/d
                                    })
    split <- 1
    split.date <- NULL
    if(stock.code %in% stocksplit.data$stock){
        split <- stocksplit.data[which(stocksplit.data$stock == stock.code), "split"]
        split.date <- stocksplit.data[which(stocksplit.data$stock == stock.code), "Start.Date"]
    }
    if(stock.id %in% stocksplit.data$stock){
        split <- stocksplit.data[which(stocksplit.data$stock == stock.id), "split"]
        split.date <- stocksplit.data[which(stocksplit.data$stock == stock.id), "Start.Date"]
    }
    return(as.list(split, split.date))
}
               
    

MakeCharts <- function(){
    ## changing the names of earnings dataframe and cleaning it
    names(earnings.data) <- list.stocks$stock.ids
    earnings.data <- lapply(earnings.data, function (x) {
                                tryCatch({
                                    rownames(x) <- x[ ,1]
                                    x <- x[-1, -1]
                                    x <- as.data.frame(t(x))
                                    colnames(x) <- c("sales", "operatingProfit", "otherIncome", "EBIT", "interest",
                                                     "depreciation", "tax", "netProfit", "EPS")
                                    rownames(x) <- paste0("30", gsub(" ", "", rownames(x)))
                                    x <- xts(x, as.Date(rownames(x), "%d%b%y"))},
                                         error = function(e) NULL)         
                            })

    ## convert daily prices to weekly prices
    price.data <- lapply(price.data, function(x){
                             x <- to.weekly(x)
                             colnames(x) <- c("open", "high", "low", "close", "volume", "adjusted")
                             x <- x["2013-03-01/2015-08-03"]
                             return(x)
                         })

    getDates <- function(data){
        tryCatch({
            data$rateReturn <- ROC(as.numeric(data$close), 52, type = "continuous")
            dates.pattern <- index(data)[which(data$rateReturn > 1.5)]
            if(length(dates.pattern) > 0)
                return(dates.pattern)
            else
                return(NULL)},
                 error = function(e) NULL)}

    firstDayWeek <- function(dates)
        {
            dates <- sapply(dates, function(x){ 
                                x <- as.Date(as.character(x))
                                no.of.days <- (as.numeric(format(as.Date(x), format = "%V")) - 1) * 7
                                year <- format(x, format = "%Y")
                                start.of.year <- as.Date(paste0(year, "-01-01"))
                                week.start <- as.character(as.Date(start.of.year + no.of.days))
                                return(week.start)
                            })
            return(unlist(dates))
        }

    ## Getting dates by which return patterns were noticed
    date.examples <- lapply(price.data, getDates)
    date.examples <- date.examples[!sapply(date.examples, is.null)]
    date.examples <-  lapply(date.examples, firstDayWeek)

    ## subsetting those stocks which have patterns
    price.data <- price.data[names(price.data) %in% names(date.examples)]
    earnings.data <- earnings.data[names(earnings.data) %in% names(date.examples)]


    ## getting the Heikenashi OHLC
    price.data <- mclapply(price.data, function(x){
                               x$xopen <- x$open
                               x$xhigh <- x$high
                               x$xlow <- x$low
                               x$xclose <- x$close
                               for (i in 2:nrow(x)){
                                   x$xopen[i] <- (x$xopen[i-1] + x$close[i-1])/2
                                   x$xclose[i] <- (x$open[i] + x$high[i] + x$low[i] + x$close[i])/4
                                   x$xhigh[i] <- max(x$high[i], x$xopen[i], x$xclose[i])
                                   x$xlow[i] <- min(x$low[i], x$xopen[i], x$xclose[i])
                               }
                               return(x)
                           }, mc.cores = 3)

    for(i in 1:length(price.data)){
        cat("Printing for: ", names(price.data)[i], "\n")
        
        if(i == 1)
            data <- price.data[[i]][-121, ]
        else
            data <- price.data[[i]]
        
        price <- data[ , 1:4]
        volume <- data[ , 5]
        earnings <- earnings.data[[i]]$EPS
        a <- merge(earnings, price, all = TRUE)
        a <- a[paste0(min(index(price)), "/", max(index(price)))]
        
        ## Making the pdf for that company.
        pdf(paste0("PLOTS/PRICE_VOL_EPS/", gsub(".BO", "", names(price.data)[i]) ,".pdf"))

        par(mfrow = c(3,1), oma = c(0,0,2,0), mai = c(0.4, 0.5, 0.4, 0.5))
        candlecolors.normal <- ifelse(price[,'close'] > price[,'open'], 'GREEN', 'RED')
        plot.xts(price, type = 'candles', candle.col=candlecolors.normal,
                 bar.col='BLACK', width = 150000, main = "Price")
        lines(EMA(price$close, 10))
        candlecolors.normal <- ifelse(price[,'close'] > price[,'open'], 'GREEN', 'RED')
        plot(volume, type = 'h', main = "Volume")
        lines(EMA(volume$volume, 10), col = "red")
        plot(a$EPS, type = 'p', main = "EPS")
        title(main = names(price.data)[i], outer = T)
        dev.off()
    }

}

MakeCharts <- function(stock.code){
    stock.id <- list.stocks[list.stocks$Security.Code == stock.code, "Security.Id"]
    stock.id <- paste0(stock.id, ".BO")

    data <- GetPriceEarnings(stock.code)
    earnings.data <- data[[1]]
    price.data <- data[[2]]
    
    earnings.data <- CleanEarningsData(earnings.data)
    price.data <- CleanPriceData(price.data)

    split <- AdjustStockSplit(stock.code, stock.id)[[1]][1]
    split.date <- AdjustStockSplit(stock.code, stock.id)[[1]][2]

    if(is.na(split.date)){
           price <- price.data[ , 1:4]
           volume <- price.data[ , 5]
       }
       else {
           price <- price.data[ , 1:4]
           price1 <- price[paste0("/", split.date)]
           price2 <- price[paste0(split.date, "/")]
           price1 <- price1/split
           price <- rbind(price1, price2)
           
           volume <- price.data[ , 5]
           volume1 <- volume[paste0("/", split.date)]
           volume2 <- volume[paste0(split.date, "/")]
           volume1 <- volume1*split
           volume <- rbind(volume1, volume2)
       }

       
    earnings <- earnings.data$EPS
    a <- merge(earnings, price, all = TRUE)
    a <- a[paste0(min(index(price)), "/", max(index(price)))]
    
    ## Making the pdf for that company.
    p <- recordPlot()
    ## pdf(paste0("PLOTS/PRICE_VOL_EPS/HOLDINGS/", gsub(".BO", "", stock.id), ".pdf"))
    par(mfrow = c(2,1), oma = c(0,0,2,0), mai = c(0.6, 1, 0.6, 0.6))
    candlecolors.normal <- ifelse(price[,'close'] > price[,'open'], 'GREEN', 'RED')
    plot.xts(price, log = "y", type = 'candles', candle.col=candlecolors.normal,
             bar.col='BLACK', width = 180000, main = "Price", axes.Y = FALSE)
    lines(EMA(price$close, 10), main = "")
    axis(2)
    par(new = TRUE)
    plot.xts(a$EPS, yaxt = "n", pch = 21,
             type = "p", main = "", axes = F, col = "blue", bg = "blue")
    axis(4)
    candlecolors.normal <- ifelse(price[,'close'] > price[,'open'], 'DARKGREEN', 'RED')
    par(new = FALSE)
    plot.xts(volume/1000, type = 'h', main = "Volume", log = "y", ylab = "(in thousands)",
             col = candlecolors.normal, minor.ticks = FALSE)
    lines(EMA(volume$volume/1000, 10), col = "black")
    title(main = stock.id, outer = T)
#    dev.off()
}



                                        # Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {

                                        # Expression that generates a plot of the distribution. The expression
                                        # is wrapped in a call to renderPlot to indicate that:
                                        #
                                        #  1) It is "reactive" and therefore should be automatically
                                        #     re-executed when inputs change
                                        #  2) Its output type is a plot
                                        #
                output$plot <- renderPlot({

                                        # generate an rnorm distribution and plot it
                    plot <- MakeCharts(input$obs)
                })
            })
