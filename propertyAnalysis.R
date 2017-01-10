#! /usr/bin/env Rscript

library("stringr")

load.csv <- function(filename) {
  csv <- read.csv(filename, header = TRUE, na.strings=c("", "NA"), stringsAsFactors=FALSE)
  return (csv)
}

write.mapping.data <- function(station.data, house.price.data) {
  summary <- aggregate(station.data$Name, list(station.data$LocalAuthority), length)
  colnames(summary) <- c("uk_council","value")
  write.csv(summary, 'stationsummary.csv', row.names=FALSE) 
  
  current.prices <- house.price.data[house.price.data$Date == "2016-10-01", c(2,4)]
  colnames(current.prices) <- c("uk_council", "value")
  write.csv(current.prices, 'currentprices.csv', row.names=FALSE) 
  
  delta.prices <- house.price.data[house.price.data$Date == "2016-10-01", c(2,7)]
  colnames(delta.prices) <- c("uk_council", "value")
  write.csv(delta.prices, 'deltaprices.csv', row.names=FALSE) 
}

average.prices <- function(price.data, start.date, col.names) {
  average.data <- aggregate(price.data[price.data$Date>start.date, 4], list(price.data[price.data$Date>start.date,1]), mean)
  colnames(average.data) <- col.names
  return (average.data)
}

clean.wikipedia.data <- function(station.data) {
  # Cleansing Wikipedia data
  # ========================
  # 1.City of Southampton => Southampton (and City of Brighton and Hove - excluding City of Westminster for Victoria and City of London)
  # 2.Borough of Tonbridge and Malling => Tonbridge and Malling
  # 3.London Borough of Lambeth => Lambeth
  # 4.Royal Borough of Kensington and Chelsea => Kensington and Chelsea
  # 5.District of Wealden => Wealden
  # 6.Lewes (District) => Lewes (Fareham too)
  # 7.Horsham, West Sussex => Horsham
  # 8.Brighton & Hove => Brighton and Hove
  # 9.Kensington and Chelsea => Kensington And Chelsea

  cleansed <- station.data
  cleansed[cleansed$LocalAuthority != "City of Westminster" & cleansed$LocalAuthority != "City of London",4] <- stringr::str_replace_all(cleansed[cleansed$LocalAuthority != "City of Westminster" & cleansed$LocalAuthority != "City of London",4],"^City of ", "")
  cleansed$LocalAuthority <- stringr::str_replace_all(cleansed$LocalAuthority,"^[A-Za-z ]*Borough of ", "")
  cleansed$LocalAuthority <- stringr::str_replace_all(cleansed$LocalAuthority,"^District of ", "")
  cleansed$LocalAuthority <- stringr::str_replace_all(cleansed$LocalAuthority," \\(district\\)$", "")
  cleansed$LocalAuthority <- stringr::str_replace_all(cleansed$LocalAuthority,", West Sussex$", "")
  cleansed$LocalAuthority <- stringr::str_replace_all(cleansed$LocalAuthority,", East Sussex$", "")
  cleansed$LocalAuthority <- stringr::str_replace_all(cleansed$LocalAuthority,"\\&", "and")
  cleansed[cleansed$LocalAuthority=="Kensington and Chelsea", 4] <- "Kensington And Chelsea"

  return (cleansed)
}

stations.raw <- load.csv("/Users/jbowman/go/src/github.com/james-bowman/wikipediascraper/stations.csv")
stations <- clean.wikipedia.data(stations.raw)

#prices <- load.csv("/Users/jbowman/R/southerntrains/UK-HPI-full-file-2016-10.csv")
prices <- load.csv("http://publicdata.landregistry.gov.uk/market-trend-data/house-price-index-data/UK-HPI-full-file-2016-10.csv")

# convert date fields from strings to dates so they format and sort correctly
prices$Date <- as.Date(prices$Date , "%d/%m/%Y")
prices[order(prices$Date ),]

# Select Land Registry data where associated local authority region has a train station served by Southern Rail
southern.prices <- prices[prices$RegionName %in% stations$LocalAuthority,]

start.date <- as.Date("31/12/2015","%d/%m/%Y")
col.names <- c("Month", "Average Property Price (£)")

average.national.data <- average.prices(prices, start.date, col.names)
average.southern.data <- average.prices(southern.prices, start.date, col.names)

data.range <- range(average.national.data[,2], average.southern.data[,2])
plot(average.national.data, type="o", col="blue", ylim=data.range)
lines(average.southern.data, type="o", pch=22, col="red")
title(main="Land Registry House Prices Jan-Oct 2016")
legend("right", legend=c("Nationwide","Southern Rail Area"), cex=0.8, col=c("blue","red"), pch=21:22, lty=1)

