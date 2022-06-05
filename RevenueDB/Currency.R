#install.packages("readr")
#install.packages("rvest")

#Make sure all required libraries are installed
library(readr)
library(dplyr)
library(lubridate)
library(rvest)
library(stringr)

#Set working directory (The final file produced will be written to this filepath location)
#setwd("")

#input yr and qtr_num here
yr <- 2021
qtr_num <- 4

if (qtr_num == 1) {
  url <- c(paste0('https://www.x-rates.com/historical/?from=USD&amount=1&date=',yr,'-02-15'))
} else if (qtr_num == 2) {
  url <- c(paste0('https://www.x-rates.com/historical/?from=USD&amount=1&date=',yr,'-05-15'))
} else if (qtr_num == 3) {
  url <- c(paste0('https://www.x-rates.com/historical/?from=USD&amount=1&date=',yr,'-08-15'))
} else (
  url <- c(paste0('https://www.x-rates.com/historical/?from=USD&amount=1&date=',yr,'-11-15'))
)

print(url)

#Extract raw table
raw_tbl <- url %>%
  read_html() %>%
  html_nodes(xpath ='//*[@id="content"]/div[1]/div/div[1]/div[1]/table[2]') %>%
  html_table()
raw_tbl <- raw_tbl[[1]]

#Add in date fields
raw_tbl$yr <- yr
raw_tbl$qtr_num <- qtr_num
raw_tbl$Calendarqtr_num <- paste0(raw_tbl$yr, '-Q', raw_tbl$qtr_num)

names(raw_tbl)[names(raw_tbl) == 'US Dollar'] <- 'CurrencyName'
names(raw_tbl)[names(raw_tbl) == '1.00 USD'] <- 'USD_Rate'
names(raw_tbl)[names(raw_tbl) == 'inv. 1.00 USD'] <- 'Foreign_Rate'

raw_tbl <- raw_tbl %>% filter(CurrencyName == 'British Pound' |
                              CurrencyName == 'Euro' |
                              CurrencyName == 'Japanese Yen' |
                              CurrencyName == 'Swiss Franc')

CurrencyName <- unique(raw_tbl$CurrencyName)
CurrencyCode <- c('EUR','JPY','CHF','GBP')
#CurrencyCode <- c('ARS','AUD','BHD','BWP','BRL','BND','BGN','CAD','CLF','CNY','COP','HRK','CZK','DKK','EUR','HKD','HUF','ISK','INR','IDR','IRR','ILS',
#                  'JPY','KZT','KRW','LYD','MYR','MRU','MXN','NPR','NZD','NOK','OMR','PKR','PHP','PLN','QAR','RON','RUB','SAR','SGD','ZAR','LKR','SEK',
#                  'CHF','TWD','THB','TTD','TRY','AED','GBP','VEF')

code_table <- as.data.frame(cbind(CurrencyName, CurrencyCode))

combined_table <- right_join(code_table, raw_tbl)
combined_table$USD_Rate <- round(combined_table$USD_Rate, digits = 3)
combined_table$Foreign_Rate <- round(combined_table$Foreign_Rate, digits = 6)

names(combined_table)[names(combined_table) == 'yr'] <- 'Year'
names(combined_table)[names(combined_table) == 'qtr_num'] <- 'Quarter'
names(combined_table)[names(combined_table) == 'Calendarqtr_num'] <- 'CalendarQuarter'

#Write out final currency table
write.csv(combined_table, paste0(yr, '-Q', qtr_num, 'CurrencyTable.csv'), row.names = FALSE)
