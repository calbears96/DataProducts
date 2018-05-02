#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(scales)
library(quantmod)
library(dplyr)
library(zoo)
library(gridExtra)

#load in stock price data using quantmod
SymbolsToGrab = c('AAPL', 'GOOG', 'MSFT', 'AMZN', 'FB')
tickers = c('AAPL', 'GOOG','MSFT', 'AMZN', 'FB')

#grab the relevant tickers
getSymbols(Symbols = SymbolsToGrab)
#dailyReturn(getSymbols(SymbolsToGrab))

df1=NULL

Daily_Returns = NULL
for(i in SymbolsToGrab){
  df1 = dailyReturn(getSymbols(i, auto.assign = FALSE))
  returns = df1
  Daily_Returns = cbind(Daily_Returns, returns)
}

#add returns to stock data
AAPL = cbind(AAPL, Daily_Returns$daily.returns)
GOOG = cbind(GOOG, Daily_Returns$daily.returns.1)
MSFT = cbind(MSFT, Daily_Returns$daily.returns.2)
AMZN = cbind(AMZN, Daily_Returns$daily.returns.3)
FB = cbind(FB, Daily_Returns$daily.returns.4)
#GSPC = cbind(GSPC, Daily_Returns$daily.returns.5)


#create empty dataframe
StockData = data.frame(Company = character(),
                       Date = as.Date(character()),
                       Close = numeric(),
                       Volume = numeric(),
                       DailyReturn = numeric(),
                       stringsAsFactors = FALSE)

#for loop to go through tickers and grab the closing price, volume, date
for(i in 1:length(tickers)){
  StockData_df = cbind(Company=rep(tickers[i], nrow(get(tickers[i]))),fortify.zoo(get(tickers[i])[,c(4:5,7)]))
  names(StockData_df)[2:5] = c('Date', 'Close', 'Volume', 'DailyReturn' )
  StockData = rbind(StockData, StockData_df)
}


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  reactiveDates = reactive({
    StockData %>% filter(as.Date(Date)>=input$DateRange[1] & as.Date(Date)<input$DateRange[2])
  })
  
    output$StockPlot <- renderPlot({
        
      
        # select stocks based on user input
        stock = filter(reactiveDates(), grepl(input$Company, Company))
        
        # draw plot of stock price
        if (input$AddTrend==TRUE & input$Returns==FALSE & input$Volume==FALSE){

          stockplot = ggplot(data=stock, aes(x=Date, y=Close)) + 
            geom_line(aes(color=Company), alpha=.8) + 
            xlab('Date') + ylab('Closing price') + 
            scale_y_continuous(label=dollar_format()) + 
            theme_classic() + geom_smooth(method = 'lm') 
          
        stockplot
        }  else if (input$AddTrend==FALSE & input$Returns==FALSE & input$Volume==FALSE) {

          stockplot = ggplot(data=stock, aes(x=Date, y=Close)) + 
            geom_line(aes(color=Company), alpha=.8) + 
            xlab('Date') + ylab('Closing price') + 
            scale_y_continuous(label=dollar_format()) + 
            theme_classic() 
          
          stockplot          
        } else if (input$AddTrend==TRUE & input$Returns==TRUE & input$Volume==FALSE){

          
         stockplot = ggplot(data=stock, aes(x=Date, y=Close)) + 
            geom_line(aes(color=Company), alpha=.8) + 
            xlab('Date') + ylab('Closing price') + 
            scale_y_continuous(label=dollar_format()) + 
            theme_classic() + geom_smooth(method = 'lm')  
          
          returnplot = ggplot(data=stock, aes(x=Date, y=DailyReturn)) +
            geom_line(color='green', alpha=.9) +
            xlab('Date') + ylab('Daily return') +
            scale_y_continuous(label=percent_format())+
            theme_classic()
          
          grid.arrange(stockplot, returnplot, ncol=1)
          
        } else if (input$AddTrend==FALSE & input$Returns==TRUE & input$Volume==FALSE){

          stockplot = ggplot(data=stock, aes(x=Date, y=Close)) + 
            geom_line(aes(color=Company), alpha=.8) + 
            xlab('Date') + ylab('Closing price') + 
            scale_y_continuous(label=dollar_format()) + 
            theme_classic()   
          
          returnplot = ggplot(data=stock, aes(x=Date, y=DailyReturn)) +
            geom_line(color='green', alpha=.9) +
            xlab('Date') + ylab('Daily return') +
            scale_y_continuous(label=percent_format())+
            theme_classic()
          
          grid.arrange(stockplot, returnplot, ncol=1)     
          
        } else if (input$AddTrend==FALSE & input$Returns==TRUE & input$Volume==TRUE){
          stockplot = ggplot(data=stock, aes(x=Date, y=Close)) + 
            geom_line(aes(color=Company), alpha=.8) + 
            xlab('Date') + ylab('Closing price') + 
            scale_y_continuous(label=dollar_format()) + 
            theme_classic()   
          
          returnplot = ggplot(data=stock, aes(x=Date, y=DailyReturn)) +
            geom_line(color='green', alpha=.9) +
            xlab('Date') + ylab('Daily return') +
            scale_y_continuous(label=percent_format())+
            theme_classic()
          
          volumeplot = ggplot(data=stock, aes(x=Date, y=Volume/1000000)) +
            geom_line(color='black', alpha=.9) +
            xlab('Date') + ylab('Volume (millions)') +
            theme_classic()
          
          grid.arrange(stockplot, returnplot, volumeplot, ncol=1)   
          
        } else if (input$AddTrend==TRUE & input$Returns==FALSE & input$Volume==TRUE){
          stockplot = ggplot(data=stock, aes(x=Date, y=Close)) + 
            geom_line(aes(color=Company), alpha=.8) + 
            xlab('Date') + ylab('Closing price') + 
            scale_y_continuous(label=dollar_format()) + 
            theme_classic() + geom_smooth(method = 'lm') 
          
          volumeplot = ggplot(data=stock, aes(x=Date, y=Volume/1000000)) +
            geom_line(color='black', alpha=.9) +
            xlab('Date') + ylab('Volume (millions)') +
            theme_classic()
          
          grid.arrange(stockplot, volumeplot, ncol=1) 
          
        } else if (input$AddTrend==FALSE & input$Returns==FALSE & input$Volume==TRUE){
          
          stockplot = ggplot(data=stock, aes(x=Date, y=Close)) + 
            geom_line(aes(color=Company), alpha=.8) + 
            xlab('Date') + ylab('Closing price') + 
            scale_y_continuous(label=dollar_format()) + 
            theme_classic() 
          
          volumeplot = ggplot(data=stock, aes(x=Date, y=Volume/1000000)) +
            geom_line(color='black', alpha=.9) +
            xlab('Date') + ylab('Volume (millions)') +
            theme_classic()
          
          grid.arrange(stockplot, volumeplot, ncol=1) 
          
        } else {
          stockplot = ggplot(data=stock, aes(x=Date, y=Close)) + 
            geom_line(aes(color=Company), alpha=.8) + 
            xlab('Date') + ylab('Closing price') + 
            scale_y_continuous(label=dollar_format()) + 
            theme_classic() + geom_smooth(method = 'lm') 
          
          returnplot = ggplot(data=stock, aes(x=Date, y=DailyReturn)) +
            geom_line(color='green', alpha=.9) +
            xlab('Date') + ylab('Daily return') +
            scale_y_continuous(label=percent_format())+
            theme_classic()
          
          volumeplot = ggplot(data=stock, aes(x=Date, y=Volume/1000000)) +
            geom_line(color='black', alpha=.9) +
            xlab('Date') + ylab('Volume (millions)') +
            theme_classic()
          
          grid.arrange(stockplot, returnplot, volumeplot, ncol=1)           
        }
      
    })
    
    output$model = renderText({
      
      stock = filter(reactiveDates(), grepl(input$Company, Company))
      model = lm(Close ~ Date, stock)
      equation = paste('Close =', + round(coef(model)[1],2), '+', + round(coef(model)[2],2),
                       '*Date', ', r^2=', round(summary(model)$r.squared,3))
      if (input$AddTrend==TRUE){
        equation
      } else{
        equation = ''
        equation
      }
  
    })
    
})
