

WebScraping <- function(state){

url <- paste("https://covidtracking.com/data/state/",state,"#historical",sep='')
url_fl <- read_html(url)
fl.data <- html_nodes(url_fl, '.state-history-module--history--2YbCy td')
#rank.data
covid_data <- html_text(fl.data)
#head(covid_data)

#getting all data in a data frame
covid_data_df <- cbind.data.frame(split(covid_data, rep(1:9, times=length(covid_data)/9)), stringsAsFactors=F)[,-2]
colnames(covid_data_df) <- c("Date","NewT","Cases","Negative","Pending","Hospitalized","Deaths","Total")
covid_data_df$Date <- as.Date(strptime(covid_data_df$Date,"%a %B %d %Y", tz='EST'))

#View(covid_data_df)
#get the first order difference (the change day to day)
hospitalized <- covid_data_df[,c(1,6)]
hospitalized <- hospitalized[!(hospitalized$Hospitalized == "N/A"),]
#View(hospitalized)

hospitalized$Hospitalized <- as.numeric(gsub(",", "", hospitalized$Hospitalized))
hospitalizedinv <- hospitalized[order(nrow(hospitalized):1),]
#View(hospitalizedinv)
hospitalizedinv$daily <- c(hospitalizedinv$Hospitalized[1],
                           diff(hospitalizedinv$Hospitalized))
hospitalizedinv$ma <- SMA(hospitalizedinv$daily,n=7)

hospitalizedinv <- data.frame(hospitalizedinv)}


#WebScraping("https://covidtracking.com/data/state/florida#historical")
#

#plot(hospitalizedinv$Date,hospitalizedinv$daily,type='l',main='Hospitalizations in FL', ylab='New Hospitalizations',
#     xlab='Month',bty="l")
#lines(hospitalizedinv$Date,ma, col='red')
#abline(v=as.numeric(date("2020-06-01")),col="blue",lwd=2)
#legend("topleft", 
#       legend=c('New Hospitalizations','Moving Average (7)','Reopen Date'),
#       col=c('black','red','blue'), lwd=2)

#plot(forecast(auto.arima(hospitalizedinv$daily)))
