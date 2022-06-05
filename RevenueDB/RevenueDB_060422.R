##################################################################################################################
#Make sure all required libraries are installed####
#Uncomment install.packages lines to install packages if necessary
#install.packages("readr")
#install.packages("dplyr")
#install.packages("lubridate")
#install.packages("rvest")
#install.packages("stringr")
#install.packages("xml2")
#install.packages("rlist")
#install.packages("pdftools")
#install.packages("httr")
#install.packages("readxl")
#install.packages("stringi")

library(readr)
library(dplyr)
library(lubridate)
library(rvest)
library(stringr)
library(xml2)
library(rlist)
library(pdftools)
library(readxl)
library(httr)
library(stringi)

#Set working directory (filepath of drug_list.csv file and currencytable.csv file)
#setwd("")
drug_list <- read_csv("C:/Users/DC/Downloads/oncology_drug_list.csv", show_col_types = FALSE)
drug_list$brand <- tolower(drug_list$brand)

#Get date for most recent revenue quarter
revenue_date <- ym(paste0(year(Sys.Date() %m-% months(3)),'-',month(Sys.Date() %m-% months(3))))
yr <- year(revenue_date)
if (month(revenue_date) >= 10) {
  quarter <- "Fourth"
  qtr_num <- 4
} else if (month(revenue_date) <= 3) {
  quarter <- "First"
  qtr_num <- 1
} else if (month(revenue_date) > 3 && month(revenue_date) <=  6) {
  quarter <- "Second"
  qtr_num <- 2
} else {
  quarter <- "Third"
  qtr_num <- 3
}

if (month(revenue_date) == 3) {
  jpy_yr <- yr -1
} else {
  jpy_yr <- yr
}
if (qtr_num == 1) {
  jpy_qtr <- 4
} else {
  jpy_qtr <- qtr_num - 1
}

currency_tbl <- read_csv(paste0(yr, '-Q', qtr_num, 'CurrencyTable.csv'), show_col_types = FALSE)
page <- c()
tbl_list <- list()
#Date manual override (for specific quarter of data; don't use if just looking for most recent quarter)####
#This also will not work for some companies
#yr <- 2021
#quarter <- fourth #string value (first, second, third, fourth)
#qtr_num <- 4 #integer value (1,2,3,4)
#################################################################################################################

#AbbVie####
if (qtr_num == 1) {
  url <- paste0('https://investors.abbvie.com/news-releases/news-release-details/abbvie-reports-first-quarter-',yr,'-financial-results')
} else if (qtr_num == 2) {
  url <- paste0('https://investors.abbvie.com/news-releases/news-release-details/abbvie-reports-second-quarter-',yr,'-financial-results')
} else if (qtr_num == 3) {
  url <- paste0('https://investors.abbvie.com/news-releases/news-release-details/abbvie-reports-third-quarter-',yr,'-financial-results')
} else {
  url <- paste0('https://investors.abbvie.com/news-releases/news-release-details/abbvie-reports-full-year-and-fourth-quarter-',yr,'-financial')
}

#Extract raw table
raw_tbl <- url %>%
  read_html() %>%
  html_nodes(xpath ='//*[@id="block-nir-pid1431-content"]/article/div[3]/div[2]/div[3]/table/tbody') %>%
  html_table()
raw_tbl <- raw_tbl[[1]]
raw_tbl$X1 <- tolower(raw_tbl$X1)

abbv_tbl <- raw_tbl %>%
  select(X1, X2) %>%
  filter(X1 %in% drug_list$brand)
names(abbv_tbl)[names(abbv_tbl) == 'X1'] <- 'brand'
names(abbv_tbl)[names(abbv_tbl) == 'X2'] <- 'reported_revenue'
abbv_tbl <- abbv_tbl %>%
  inner_join(drug_list, by = 'brand') %>%
  select(molecule, reported_revenue)
abbv_tbl$reported_revenue <- gsub("[[:punct:]]","", abbv_tbl$reported_revenue)
abbv_tbl$reported_revenue <- as.numeric(abbv_tbl$reported_revenue)

#Amgen####
if (qtr_num == 1) {
  url <- paste0('https://investors.amgen.com/news-releases/news-release-details/amgen-reports-first-quarter-',yr,'-financial-results')
} else if (qtr_num == 2) {
  url <- paste0('https://investors.amgen.com/news-releases/news-release-details/amgen-reports-third-quarter-2021-financial-results')
} else if (qtr_num == 3) {
  url <- paste0('https://investors.amgen.com/news-releases/news-release-details/amgen-reports-third-quarter-2021-financial-results')
} else {
  url <- paste0('https://investors.amgen.com/news-releases/news-release-details/amgen-reports-fourth-quarter-and-full-year-2021-financial')
}

raw_tbl <- url %>%
  read_html() %>%
  html_nodes(xpath ='//table') %>%
  html_table()
raw_tbl <- raw_tbl[[2]]
raw_tbl$X1 <- tolower(raw_tbl$X1)

raw_tbl$X1 <- gsub("[[:punct:]]","", raw_tbl$X1)
raw_tbl$X3 <- gsub("[[:punct:]]","", raw_tbl$X3)
raw_tbl$X3 <- str_trim(raw_tbl$X3)

amgn_tbl <- raw_tbl %>%
  select(X1, X3) %>%
  filter(X1 %in% drug_list$brand)
names(amgn_tbl)[names(amgn_tbl) == 'X1'] <- 'brand'
names(amgn_tbl)[names(amgn_tbl) == 'X3'] <- 'reported_revenue'
amgn_tbl <- amgn_tbl %>%
  inner_join(drug_list, by = 'brand') %>%
  select(molecule, reported_revenue)
amgn_tbl$reported_revenue <- as.numeric(amgn_tbl$reported_revenue)

#Astellas####
url <- "https://www.astellas.com/en/investors/ir-library/business-results"
raw_list <- url %>%
  read_html() %>%
  html_nodes("object") %>%  
  html_attr("data") %>%  
  str_subset("\\.html") %>% unique()
url <- raw_list
url_unique_str <- sub("\\.net.*","",url)

raw_list <- url %>%
  read_html() %>%
  html_nodes("a") %>%  # find all links in the page
  html_attr("href") %>% # get the url for these links
  str_subset(paste0('q',jpy_yr,'_sup_en')) %>%
  str_c(paste0(url_unique_str,".net"), .) %>% unique()

for (i in 1:length(raw_list)) {
  raw_pdf <- pdf_text(raw_list[i])
  pdf.text<-unlist(raw_pdf)
  pdf.text<-tolower(raw_pdf)
  page_find <- data.frame(str_detect(pdf.text, "\\(2\\) united states")) #find unique string for pdf page
  colnames(page_find)<-"Result"
  page_find<-subset(page_find,page_find$Result==TRUE)

  if (qtr_num == 4) {
    if (i == 3) {
      page[i] <- as.numeric(rownames(page_find))[1] 
    } else {
      page[i] <- as.numeric(rownames(page_find))[2] 
    } 
  } else if (qtr_num == 3) {
    if (i == 2) {
      page[i] <- as.numeric(rownames(page_find))[1] 
    } else {
      page[i] <- as.numeric(rownames(page_find))[2] 
    } 
  } else if (qtr_num == 1) {
    if (i == 4) {
      page[i] <- as.numeric(rownames(page_find))[1] 
    } else {
      page[i] <- as.numeric(rownames(page_find))[2] 
    } 
  }
}

for (i in 1:length(raw_list)) {
  raw_pdf <- pdf_text(rev(raw_list)[i])
  profile <- raw_pdf[rev(page)[i]]
  profile <- strsplit(profile, "\n")
  profile <- profile[[1]]
  profile <- trimws(profile)
  raw_tbl <- data.frame(str_split_fixed(profile, " {2,}", 10))
  raw_tbl$X1 <- tolower(raw_tbl$X1)

  if (qtr_num == 4) {
    if (i == 1) {
        astl_tbl <- raw_tbl %>%
          select(X1, X6) %>%
          filter(X1 %in% drug_list$brand)
        names(astl_tbl)[names(astl_tbl) == 'X1'] <- 'brand'
        names(astl_tbl)[names(astl_tbl) == 'X6'] <- 'reported_revenue'
    } else if (i == 2) {
        astl_tbl <- raw_tbl %>%
          select(X1, X4) %>%
          filter(X1 %in% drug_list$brand)
        names(astl_tbl)[names(astl_tbl) == 'X1'] <- 'brand'
        names(astl_tbl)[names(astl_tbl) == 'X4'] <- 'reported_revenue'
    } else {
        astl_tbl <- raw_tbl %>%
          select(X1, X3) %>%
          filter(X1 %in% drug_list$brand)
        names(astl_tbl)[names(astl_tbl) == 'X1'] <- 'brand'
        names(astl_tbl)[names(astl_tbl) == 'X3'] <- 'reported_revenue'
    }
  } else if (qtr_num == 3) {
    if (i == 1) {
        astl_tbl <- raw_tbl %>%
          select(X1, X4) %>%
          filter(X1 %in% drug_list$brand)
        names(astl_tbl)[names(astl_tbl) == 'X1'] <- 'brand'
        names(astl_tbl)[names(astl_tbl) == 'X4'] <- 'reported_revenue'
    } else if (i == 2) {
        astl_tbl <- raw_tbl %>%
          select(X1, X3) %>%
          filter(X1 %in% drug_list$brand)
        names(astl_tbl)[names(astl_tbl) == 'X1'] <- 'brand'
        names(astl_tbl)[names(astl_tbl) == 'X3'] <- 'reported_revenue'
    }
  } else if (qtr_num == 1) {
    if (i == 1) {
        astl_tbl <- raw_tbl %>%
          select(X1, X8) %>%
          filter(X1 %in% drug_list$brand)
        names(astl_tbl)[names(astl_tbl) == 'X1'] <- 'brand'
        names(astl_tbl)[names(astl_tbl) == 'X8'] <- 'reported_revenue'
    } else if (i == 2) {
        astl_tbl <- raw_tbl %>%
          select(X1, X6) %>%
          filter(X1 %in% drug_list$brand)
        names(astl_tbl)[names(astl_tbl) == 'X1'] <- 'brand'
        names(astl_tbl)[names(astl_tbl) == 'X6'] <- 'reported_revenue'
    } else if (i == 3) {
        astl_tbl <- raw_tbl %>%
          select(X1, X4) %>%
          filter(X1 %in% drug_list$brand)
        names(astl_tbl)[names(astl_tbl) == 'X1'] <- 'brand'
        names(astl_tbl)[names(astl_tbl) == 'X4'] <- 'reported_revenue'
    } else {
        astl_tbl <- raw_tbl %>%
          select(X1, X2) %>%
          filter(X1 %in% drug_list$brand)
        names(astl_tbl)[names(astl_tbl) == 'X1'] <- 'brand'
        names(astl_tbl)[names(astl_tbl) == 'X2'] <- 'reported_revenue'
    }
  }
}
  astl_tbl <- astl_tbl %>%
    inner_join(drug_list, by = 'brand') %>%
    select(molecule, reported_revenue) %>% 
    unique() %>% distinct(molecule, .keep_all = TRUE)

  astl_tbl$reported_revenue <- as.numeric(astl_tbl$reported_revenue)
  #assign(paste0("astl_tbl_",i), astl_tbl, envir = .GlobalEnv)    


#AstraZeneca####
url <- paste0("https://www.astrazeneca.com/investor-relations/results-and-presentations.html#",year(ymd(Sys.Date())),"-0")
raw_list <- url %>%
  read_html() %>%
  html_nodes("a") %>%  # find all links in the page
  html_attr("href") %>% # get the url for these links
  str_subset("\\.pdf") %>% # find those that end in pdf only
  str_c("https://www.astrazeneca.com", .)# prepend the website to the url
raw_list <- list.take(raw_list, 20)
html_df <- data.frame(raw_list)
html_df <- filter(html_df, grepl("announcement",raw_list))

if (qtr_num == 4) {
  raw_pdf <- pdf_text(html_df[1,])
  profile <- raw_pdf[55] #The number in the [] represents the page of the pdf file, check the page if code isn't working.
  profile <- strsplit(profile, "\n")
  profile <- profile[[1]]
  profile <- trimws(profile)
  raw_tbl <- data.frame(str_split_fixed(profile, " {2,}", 15))
  raw_tbl$X1 <- tolower(raw_tbl$X1)
  
  azn_tbl <- raw_tbl %>%
    select(X1, X8) %>%
    filter(X1 %in% drug_list$brand)
  names(azn_tbl)[names(azn_tbl) == 'X1'] <- 'brand'
  names(azn_tbl)[names(azn_tbl) == 'X8'] <- 'reported_revenue'
  azn_tbl <- azn_tbl %>%
    inner_join(drug_list, by = 'brand') %>%
    select(molecule, reported_revenue)
} else {
  raw_pdf <- pdf_text(html_df[1,])
  profile <- raw_pdf[43] #The number in the [] represents the page of the pdf file, check the page if code isn't working.
  profile <- strsplit(profile, "\n")
  profile <- profile[[1]]
  profile <- trimws(profile)
  raw_tbl <- data.frame(str_split_fixed(profile, " {2,}", 15))
  raw_tbl$X1 <- tolower(raw_tbl$X1)
  
  azn_tbl <- raw_tbl %>%
    select(X1, X8) %>%
    filter(X1 %in% drug_list$brand)
  names(azn_tbl)[names(azn_tbl) == 'X1'] <- 'brand'
  names(azn_tbl)[names(azn_tbl) == 'X8'] <- 'reported_revenue'
  azn_tbl <- azn_tbl %>%
    inner_join(drug_list, by = 'brand') %>%
    select(molecule, reported_revenue)
}

azn_tbl$reported_revenue <- as.numeric(azn_tbl$reported_revenue)

#AVEO####
if (qtr_num == 1) {
  url <- paste0('http://investor.aveooncology.com/news-releases/news-release-details/aveo-oncology-reports-first-quarter-',yr,'-financial-results')
} else if (qtr_num == 2) {
  url <- paste0('http://investor.aveooncology.com/news-releases/news-release-details/aveo-oncology-reports-second-quarter-',yr,'-financial-results-and')
} else if (qtr_num == 3) {
  url <- paste0('http://investor.aveooncology.com/news-releases/news-release-details/aveo-oncology-reports-third-quarter-',yr,'-financial-results-and')
} else {
  url <- paste0('http://investor.aveooncology.com/news-releases/news-release-details/aveo-oncology-announce-full-year-',yr,'-financial-results-and-')
}

if (qtr_num == 4) {
  raw_list <- url %>%
    read_html() %>%
    html_nodes("a") %>%  # find all links in the page
    html_attr("href") %>% # get the url for these links
    str_subset("\\/pdf") %>% # find those that end in pdf only
    str_c("http://investor.aveooncology.com/", .)# prepend the website to the url
  html_df <- data.frame(raw_list)
  raw_pdf <- pdf_text(html_df[1,])
  profile <- raw_pdf[5]
  profile <- strsplit(profile, "\n")
  profile <- profile[[1]]
  profile <- trimws(profile)
  raw_tbl <- data.frame(str_split_fixed(profile, " {2,}", 5))
  raw_tbl$X1 <- tolower(raw_tbl$X1)
  
  aveo_tbl <- raw_tbl %>%
    select(X1, X3) %>%
    filter(grepl('fotivda', X1) & grepl('product revenue', X1))
  names(aveo_tbl)[names(aveo_tbl) == 'X1'] <- 'brand'
  names(aveo_tbl)[names(aveo_tbl) == 'X3'] <- 'reported_revenue'
  aveo_tbl[1,1] <- 'fotivda'
  aveo_tbl <- aveo_tbl %>%
    inner_join(drug_list, by = 'brand') %>%
    select(molecule, reported_revenue)
} else {
  raw_list <- url %>%
    read_html() %>%
    html_nodes("a") %>%  # find all links in the page
    html_attr("href") %>% # get the url for these links
    str_subset("\\/pdf") %>% # find those that end in pdf only
    str_c("http://investor.aveooncology.com/", .)# prepend the website to the url
  html_df <- data.frame(raw_list)
  raw_pdf <- pdf_text(html_df[1,])
  profile <- raw_pdf[5]
  profile <- strsplit(profile, "\n")
  profile <- profile[[1]]
  profile <- trimws(profile)
  raw_tbl <- data.frame(str_split_fixed(profile, " {2,}", 5))
  raw_tbl$X1 <- tolower(raw_tbl$X1)
  
  aveo_tbl <- raw_tbl %>%
    select(X1, X3) %>%
    filter(grepl('fotivda', X1) & grepl('product revenue', X1))
  names(aveo_tbl)[names(aveo_tbl) == 'X1'] <- 'brand'
  names(aveo_tbl)[names(aveo_tbl) == 'X3'] <- 'reported_revenue'
  aveo_tbl[1,1] <- 'fotivda'
  aveo_tbl <- aveo_tbl %>%
    inner_join(drug_list, by = 'brand') %>%
    select(molecule, reported_revenue)
}

aveo_tbl$reported_revenue <- as.numeric(gsub("[[:punct:]]", "", aveo_tbl$reported_revenue))
aveo_tbl$reported_revenue <- round(aveo_tbl$reported_revenue/1000,2)

#BeiGeneCHECK####
#Does not provide table of revenue at the product level
#Use url: https://ir.beigene.com/news-events/press-releases/ to find quarterly report

#url <- 'https://ir.beigene.com/news-events/press-releases/'
#raw_list <- url %>%
#  read_html() %>%
#  html_nodes("a") %>%  # find all links in the page
#  html_attr("href") %>% # get the url for these links
#  str_subset("\\?id") %>% # find relevant links

#Blueprint####
if (qtr_num == 1) {
  url <- paste0('https://ir.blueprintmedicines.com/news-releases/news-release-details/blueprint-medicines-reports-first-quarter-',yr,'-results')
} else if (qtr_num == 2) {
  url <- paste0('https://ir.blueprintmedicines.com/news-releases/news-release-details/blueprint-medicines-reports-second-quarter-',yr,'-financial')
} else if (qtr_num == 3) {
  url <- paste0('https://ir.blueprintmedicines.com/news-releases/news-release-details/blueprint-medicines-reports-third-quarter-',yr,'-financial-results')
} else {
  url <- paste0('https://ir.blueprintmedicines.com/news-releases/news-release-details/blueprint-medicines-reports-fourth-quarter-and-full-year-',yr)
}

raw_tbl <- url %>%
  read_html() %>%
  html_nodes(xpath ='//*[contains(concat( " ", @class, " " ), concat( " ", "xn-content", " " ))]') %>%
  html_table()
raw_tbl <- raw_tbl[[1]]
raw_tbl$X1 <- tolower(raw_tbl$X1)

if (qtr_num == 4) {
  blue_tbl <- raw_tbl %>%
    select(X1, X6) %>%
    filter(grepl('product revenue', X1))
  names(blue_tbl)[names(blue_tbl) == 'X1'] <- 'brand'
  names(blue_tbl)[names(blue_tbl) == 'X6'] <- 'reported_revenue'
  blue_tbl[1,1] <- 'ayvakit'
  blue_tbl <- blue_tbl %>%
    inner_join(drug_list, by = 'brand') %>%
    select(molecule, reported_revenue)
} else {
  blue_tbl <- raw_tbl %>%
    select(X1, X4) %>%
    filter(grepl('product revenue', X1))
  names(blue_tbl)[names(blue_tbl) == 'X1'] <- 'brand'
  names(blue_tbl)[names(blue_tbl) == 'X4'] <- 'reported_revenue'
  blue_tbl[1,1] <- 'ayvakit'
  blue_tbl <- blue_tbl %>%
    inner_join(drug_list, by = 'brand') %>%
    select(molecule, reported_revenue)
}

blue_tbl$reported_revenue <- as.numeric(gsub("[[:punct:]]", "", blue_tbl$reported_revenue))
blue_tbl$reported_revenue <- round(blue_tbl$reported_revenue/1000,2)

#BMS####
#Check new financial statements, may be changes in formatting
if (qtr_num == 1) {
  url <- paste0('https://news.bms.com/news/corporate-financial/',yr,'/Bristol-Myers-Squibb-Reports-First-Quarter-Financial-Results-for-',yr,'/default.aspx')
} else if (qtr_num == 2) {
  url <- paste0('https://news.bms.com/news/corporate-financial/',yr,'/Bristol-Myers-Squibb-Reports-Second-Quarter-Financial-Results-for-',yr,'/default.aspx')
} else if (qtr_num == 3) {
  url <- paste0('https://news.bms.com/news/corporate-financial/',yr,'/Bristol-Myers-Squibb-Reports-Third-Quarter-Financial-Results-for-',yr,'/default.aspx')
} else {
  url <- paste0('https://news.bms.com/news/corporate-financial/',year(Sys.Date())-1,'/Bristol-Myers-Squibb-Reports-Fourth-Quarter-and-Full-Year-Financial-Results-for-',2020,'/default.aspx')
}

if (qtr_num == 4) {
  raw_tbl <- url %>%
    read_html() %>%
    html_nodes(xpath ='//*[contains(concat( " ", @class, " " ), concat( " ", "q4default", " " ))]') %>%
    html_table()
  raw_tbl <- raw_tbl[[1]]
  raw_tbl$X1 <- tolower(raw_tbl$X1)
  raw_tbl<- raw_tbl[53:88,]
  
  bms_tbl <- raw_tbl %>%
    select(X1, X15) %>%
    filter(X1 %in% drug_list$brand)
  names(bms_tbl)[names(bms_tbl) == 'X1'] <- 'brand'
  names(bms_tbl)[names(bms_tbl) == 'X15'] <- 'reported_revenue'
  bms_tbl <- bms_tbl %>%
    inner_join(drug_list, by = 'brand') %>%
    select(molecule, reported_revenue)
} else {
  raw_tbl <- url %>%
    read_html() %>%
    html_nodes(xpath ='//*[contains(concat( " ", @class, " " ), concat( " ", "q4default", " " ))]') %>%
    html_table()
  raw_tbl <- raw_tbl[[1]]
  raw_tbl$X1 <- tolower(raw_tbl$X1) #Run up to here to check rownums
  raw_tbl<- raw_tbl[66:88,] #Only include rownums necessary
  
  bms_tbl <- raw_tbl %>%
    select(X1, X8) %>%
    filter(X1 %in% drug_list$brand)
  names(bms_tbl)[names(bms_tbl) == 'X1'] <- 'brand'
  names(bms_tbl)[names(bms_tbl) == 'X8'] <- 'reported_revenue'
  bms_tbl <- bms_tbl %>%
    inner_join(drug_list, by = 'brand') %>%
    select(molecule, reported_revenue)
}

bms_tbl$reported_revenue <- as.numeric(gsub("[[:punct:]]", "", bms_tbl$reported_revenue))
bms_tbl$reported_revenue <- as.numeric(bms_tbl$reported_revenue)

#Clovis####
if (qtr_num == 1) {
  url <- paste0('https://s22.q4cdn.com/778348918/files/doc_financials/2022/q1/Clovis-Oncology-Announces-First-Quarter-',yr,'-Operating-Results-and-Provides-Update-on-Clinical-Development-Programs-',yr,'.pdf')
} else if (qtr_num == 2) {
  url <- paste0('https://s22.q4cdn.com/778348918/files/doc_financials/2022/q2/Clovis-Oncology-Announces-Second-Quarter-',yr,'-Operating-Results-and-Provides-Update-on-Clinical-Development-Programs-',yr,'.pdf')
} else if (qtr_num == 3) {
  url <- paste0('https://s22.q4cdn.com/778348918/files/doc_financials/2022/q3/Clovis-Oncology-Announces-Third-Quarter-',yr,'-Operating-Results-and-Provides-Update-on-Clinical-Development-Programs-',yr,'.pdf')
} else {
  url <- paste0('https://s22.q4cdn.com/778348918/files/doc_financials/2021/q4/Clovis-Oncology-Announces-',yr,'-Operating-Results-and-Anticipated-',year(Sys.Date()),'-Development-Milestones.pdf')
}

if (qtr_num == 4) {
  raw_pdf <- pdf_text(url)
  profile <- raw_pdf[8]
  profile <- strsplit(profile, "\n")
  profile <- profile[[1]]
  profile <- trimws(profile)
  raw_tbl <- data.frame(str_split_fixed(profile, " {2,}", 5))
  raw_tbl$X1 <- tolower(raw_tbl$X1)
  
  clvs_tbl <- raw_tbl %>%
    select(X1, X3) %>%
    filter(grepl('product revenue', X1))
  names(clvs_tbl)[names(clvs_tbl) == 'X1'] <- 'brand'
  names(clvs_tbl)[names(clvs_tbl) == 'X3'] <- 'reported_revenue'
  clvs_tbl[1,1] <- 'rubraca'
  clvs_tbl <- clvs_tbl %>%
    inner_join(drug_list, by = 'brand') %>%
    select(molecule, reported_revenue)
} else {
  raw_pdf <- pdf_text(url)
  profile <- raw_pdf[9]
  profile <- strsplit(profile, "\n")
  profile <- profile[[1]]
  profile <- trimws(profile)
  raw_tbl <- data.frame(str_split_fixed(profile, " {2,}", 5))
  raw_tbl$X1 <- tolower(raw_tbl$X1)
  
  clvs_tbl <- raw_tbl %>%
    select(X1, X3) %>%
    filter(grepl('product revenue', X1))
  names(clvs_tbl)[names(clvs_tbl) == 'X1'] <- 'brand'
  names(clvs_tbl)[names(clvs_tbl) == 'X3'] <- 'reported_revenue'
  clvs_tbl[1,1] <- 'rubraca'
  clvs_tbl <- clvs_tbl %>%
    inner_join(drug_list, by = 'brand') %>%
    select(molecule, reported_revenue)
}

clvs_tbl$reported_revenue <- as.numeric(gsub("[[:punct:]]", "", clvs_tbl$reported_revenue))
clvs_tbl$reported_revenue <- round(as.numeric(clvs_tbl$reported_revenue/1000),2)

#EisaiCHECK####
#url <- "https://www.eisai.com/ir/library/index.html"
#
#raw_list <- url %>%
#read_html() %>%
#html_nodes("a") %>%  # find all links in the page
#html_attr("href") %>%
#str_subset("settlement")

#Eli Lilly####
url <- "https://investor.lilly.com/financial-information/quarterly-results"
  
raw_list <- url %>%
read_html() %>%
html_nodes("a") %>%  # find all links in the page
html_attr("href") %>% # get the url for these links
str_subset("static-files") %>% # find those that contain "static-files" only to extract the excel file
str_c("https://investor.lilly.com/", .) %>% unique()

GET(raw_list[3], write_disk(tf <- tempfile(fileext = ".xlsx")))
raw_tbl <- read_excel(tf, sheet = paste0(yr, " Revenue"))
x <- c('X1','X2','X3','X4','X5','X6','X7','X8','X9','X10','X11','X12','X13','X14','X15','X16','X17','X18','X19','X20','X21')
names(raw_tbl) <- x
raw_tbl$X2 <- tolower(raw_tbl$X2)

if (qtr_num == 1) {
  lly_tbl <- raw_tbl %>%
    select(X2, X3) %>%
    filter(X2 %in% drug_list$brand)
  names(lly_tbl)[names(lly_tbl) == 'X2'] <- 'brand'
  names(lly_tbl)[names(lly_tbl) == 'X3'] <- 'reported_revenue'
  lly_tbl <- lly_tbl %>%
    inner_join(drug_list, by = 'brand') %>%
    select(molecule, reported_revenue)
} else if (qtr_num == 2) {
  lly_tbl <- raw_tbl %>%
    select(X2, X7) %>%
    filter(X2 %in% drug_list$brand)
  names(lly_tbl)[names(lly_tbl) == 'X2'] <- 'brand'
  names(lly_tbl)[names(lly_tbl) == 'X7'] <- 'reported_revenue'
  lly_tbl <- lly_tbl %>%
    inner_join(drug_list, by = 'brand') %>%
    select(molecule, reported_revenue)
} else if (qtr_num == 3) {
  lly_tbl <- raw_tbl %>%
    select(X2, X11) %>%
    filter(X2 %in% drug_list$brand)
  names(lly_tbl)[names(lly_tbl) == 'X2'] <- 'brand'
  names(lly_tbl)[names(lly_tbl) == 'X11'] <- 'reported_revenue'
  lly_tbl <- lly_tbl %>%
    inner_join(drug_list, by = 'brand') %>%
    select(molecule, reported_revenue)
} else {
  lly_tbl <- raw_tbl %>%
    select(X2, X15) %>%
    filter(X2 %in% drug_list$brand)
  names(lly_tbl)[names(lly_tbl) == 'X2'] <- 'brand'
  names(lly_tbl)[names(lly_tbl) == 'X15'] <- 'reported_revenue'
  lly_tbl <- lly_tbl %>%
    inner_join(drug_list, by = 'brand') %>%
    select(molecule, reported_revenue)
}

lly_tbl$reported_revenue <- as.numeric(lly_tbl$reported_revenue)

#Epizyme####
url <- "https://epizyme.gcs-web.com/financial-information/quarterly-results"

raw_list <- url %>%
  read_html() %>%
  html_nodes("a") %>%  # find all links in the page
  html_attr("href") %>%
  str_subset(paste0(tolower(quarter),"-quarter")) %>%
  str_c("https://epizyme.gcs-web.com", .) %>% unique()

raw_tbl <- raw_list[1] %>%
  read_html() %>%
  html_nodes(xpath ='//*[contains(concat( " ", @class, " " ), concat( " ", "text-content", " " ))]') %>%
  html_table()
raw_tbl <- raw_tbl[[1]]
raw_tbl$X1 <- tolower(raw_tbl$X1)

epzm_tbl <- raw_tbl %>%
  select(X1, X3) %>%
  filter(X1 == 'product revenue, net')

epzm_tbl$X1 <- "tazverik"
epzm_tbl$X3 <- as.numeric(gsub(",", "", epzm_tbl$X3))
names(epzm_tbl)[names(epzm_tbl) == 'X1'] <- 'brand'
names(epzm_tbl)[names(epzm_tbl) == 'X3'] <- 'reported_revenue'
epzm_tbl <- epzm_tbl %>%
  inner_join(drug_list, by = 'brand') %>%
  select(molecule, reported_revenue)

epzm_tbl$reported_revenue <- as.numeric(epzm_tbl$reported_revenue)/1000

#Exelixis####
#Manual



#Gilead####
#Stopped reporting Zydelig as of Q1 2022
#url <- "https://investors.gilead.com/financial-information/quarterly-earnings"
#
#raw_list <- url %>%
#  read_html() %>%
#  html_nodes("a") %>%  # find all links in the page
#  html_attr("href") %>% # get the url for these links
#  str_subset("static-files") %>%
#  str_c("https://investors.gilead.com/", .) %>% unique()
#
#raw_pdf <- pdf_text(raw_list[3])
#pdf.text<-unlist(raw_pdf)
#pdf.text<-tolower(raw_pdf)
#page_find <- data.frame(str_detect(pdf.text, "zydelig [\U{2013}] u.s.")) #find unique string for pdf page
#colnames(page_find)<-"Result"
#page_find<-subset(page_find,page_find$Result==TRUE)
#page <- as.numeric(rownames(page_find)) 
#
#raw_pdf <- pdf_text(raw_list[4])
#profile <- raw_pdf[page]
#profile <- strsplit(profile, "\n")
#profile <- profile[[1]]
#profile <- trimws(profile)
#raw_tbl <- data.frame(str_split_fixed(profile, " {2,}", 5))
#raw_tbl$X1 <- tolower(raw_tbl$X1)
#
#gil_tbl <- raw_tbl %>%
#  select(X1, X2) %>%
#  filter(X1 %in% drug_list$brand | grepl("zydelig [\U{2013}] u.s.", X1))
#
#gil_tbl$X1 <- "zydelig"
#names(gil_tbl)[names(gil_tbl) == 'X1'] <- 'brand'
#names(gil_tbl)[names(gil_tbl) == 'X2'] <- 'reported_revenue'
#gil_tbl <- gil_tbl %>%
#  inner_join(drug_list, by = 'brand') %>%
#  select(molecule, reported_revenue)
#gil_tbl$reported_revenue <- as.numeric(gil_tbl$reported_revenue)

#GSK####
url <- "https://www.gsk.com/en-gb/investors/quarterly-results/"

raw_list <- url %>%
  read_html() %>%
  html_nodes("a") %>%  # find all links in the page
  html_attr("href") %>%
  str_subset(paste0(yr,"-announcement")) %>%
  str_c("https://www.gsk.com", .) %>% unique()

raw_pdf <- pdf_text(raw_list[1])
pdf.text<-unlist(raw_pdf)
pdf.text<-tolower(raw_pdf)
page_find <- data.frame(str_detect(pdf.text, "press release\n\n\nspecialty medicines turnover [\U{2013}] three months")) #find unique string for pdf page
colnames(page_find)<-"Result"
page_find<-subset(page_find,page_find$Result==TRUE)
page <- as.numeric(rownames(page_find)) 

raw_pdf <- pdf_text(raw_list[1])
profile <- raw_pdf[page]
profile <- strsplit(profile, "\n")
profile <- profile[[1]]
profile <- trimws(profile)
raw_tbl <- data.frame(str_split_fixed(profile, " {2,}", 15))
raw_tbl$X1 <- tolower(raw_tbl$X1)

gsk_tbl <- raw_tbl %>%
  select(X1, X5) %>%
  filter(X1 %in% drug_list$brand)
gsk_tbl$X5 <- as.numeric(gsk_tbl$X5)

names(gsk_tbl)[names(gsk_tbl) == 'X1'] <- 'brand'
names(gsk_tbl)[names(gsk_tbl) == 'X5'] <- 'reported_revenue'
gsk_tbl <- gsk_tbl %>%
  inner_join(drug_list, by = 'brand') %>%
  select(molecule, reported_revenue)

gsk_tbl$reported_revenue <- round((gsk_tbl$reported_revenue * currency_tbl$Foreign_Rate[currency_tbl$CurrencyCode == 'GBP']),2)  

#Incyte####
if (qtr_num != 4) {
  url <- paste0("https://investor.incyte.com/news-releases/news-release-details/incyte-reports-",yr,"-",quarter,"-quarter-financial-results-and-provides")
} else {
  url <- paste0("https://investor.incyte.com/press-releases/press-releases/",yr,"/Incyte-Reports-",yr,"-Fourth-Quarter-and-Year-End-Financial-Results-and-Provides-2022-Financial-Guidance-and-Updates-on-Key-Clinical-Programs/default.aspx")
}

raw_list <- url %>%
  read_html() %>%
  html_nodes("a") %>%  # find all links in the page
  html_attr("href") %>%
  str_subset("pdf") %>%
  str_c("https://investor.incyte.com/", . ) %>% unique()

#raw_list <- str_replace(raw_list, fixed("//"),"https://")

raw_pdf <- pdf_text(raw_list)
pdf.text<-unlist(raw_pdf)
pdf.text<-tolower(raw_pdf)
page_find <- data.frame(str_detect(pdf.text, "net product revenues:")) #find unique string for pdf page
colnames(page_find)<-"Result"
page_find<-subset(page_find,page_find$Result==TRUE)
page <- as.numeric(rownames(page_find)) 

raw_pdf <- pdf_text(raw_list)
profile <- raw_pdf[page]
profile <- strsplit(profile, "\n")
profile <- profile[[1]]
profile <- trimws(profile)
raw_tbl <- data.frame(str_split_fixed(profile, " {1,}", 7))
raw_tbl$X1 <- tolower(raw_tbl$X1)

incy_tbl <- raw_tbl %>%
  filter(X1 %in% drug_list$brand)
rev_values <- c()

for (i in 1:length(incy_tbl$X1)) {
  x <- paste(incy_tbl[i,], collapse = ' ')
  x <- gsub(",", "", x)
  rev_values[i] <- stri_extract_first_regex(x, "[0-9]+")
}

rev_values <- as.numeric(rev_values)
incy_tbl <- data.frame(brand = incy_tbl$X1, reported_revenue = round(rev_values/1000,2))
incy_tbl <- incy_tbl %>%
  inner_join(drug_list, by = 'brand') %>%
  select(molecule, reported_revenue)

incy_tbl <- subset(incy_tbl, molecule != "capmatinib")

#Jazz####
url <- "http://investor.jazzpharma.com/financial-information/quarterly-results"
raw_list <- url %>%
  read_html() %>%
  html_nodes("a") %>%  # find all links in the page
  html_attr("href") %>%
  str_subset("jazz-pharmaceuticals-announces") %>%
  str_c("http://investor.jazzpharma.com", .) %>% unique()
url <- raw_list[1]

raw_list <- url %>%
  read_html() %>%
  html_nodes("a") %>%  # find all links in the page
  html_attr("href") %>%
  str_subset("\\/pdf") %>%
  str_c("http://investor.jazzpharma.com", .) %>% unique()

raw_pdf <- pdf_text(raw_list)
pdf.text<-unlist(raw_pdf)
pdf.text<-tolower(raw_pdf)
page_find <- data.frame(str_detect(pdf.text, "pro forma net product")) #find unique string for pdf page
colnames(page_find)<-"Result"
page_find<-subset(page_find,page_find$Result==TRUE)
page <- as.numeric(rownames(page_find)) 

raw_pdf <- pdf_text(raw_list)
profile <- raw_pdf[page]
profile <- strsplit(profile, "\n")
profile <- profile[[1]]
profile <- trimws(profile)
raw_tbl <- data.frame(str_split_fixed(profile, " {2,}", 5))
raw_tbl$X1 <- tolower(raw_tbl$X1)

jazz_tbl <- raw_tbl %>%
  select(X1, X2) %>%
  filter(X1 %in% drug_list$brand)
jazz_tbl$X2 <- as.numeric(gsub(",", "", jazz_tbl$X2))
jazz_tbl$X2 <- as.numeric(jazz_tbl$X2)
jazz_tbl$X2 <- round(jazz_tbl$X2/1000, 2)

names(jazz_tbl)[names(jazz_tbl) == 'X1'] <- 'brand'
names(jazz_tbl)[names(jazz_tbl) == 'X2'] <- 'reported_revenue'
jazz_tbl <- jazz_tbl %>%
  inner_join(drug_list, by = 'brand') %>%
  select(molecule, reported_revenue)

#JNJ####
url <- "https://johnsonandjohnson.gcs-web.com/"

raw_list <- url %>%
  read_html() %>%
  html_nodes("a") %>%  # find all links in the page
  html_attr("href") %>% # get the url for these links
  str_subset("static-files") %>%
  str_c("https://johnsonandjohnson.gcs-web.com", .) %>% unique()

raw_pdf <- pdf_text(raw_list[4])
profile <- raw_pdf[3]
profile <- strsplit(profile, "\n")
profile <- profile[[1]]
profile <- trimws(profile)
raw_tbl <- data.frame(str_split_fixed(profile, " {2,}", 11))
raw_tbl$X1 <- tolower(raw_tbl$X1)

jnj_tbl <- raw_tbl %>%
  select(X1, X2) %>%
  filter(X1 %in% drug_list$brand | grepl('us', X1))
row_id <- as.numeric(rownames(jnj_tbl)[which(jnj_tbl$X1 %in% drug_list$brand)])

jnj_names <- c()
jnj_values <- c()

for (i in 1:length(row_id)) {
  jnj_names[i] <- jnj_tbl$X1[row_id[i]]
  jnj_values[i] <- jnj_tbl$X2[row_id[i]+1]
}

jnj_tbl <- data.frame(cbind(jnj_names,jnj_values))

names(jnj_tbl)[names(jnj_tbl) == 'jnj_names'] <- 'brand'
names(jnj_tbl)[names(jnj_tbl) == 'jnj_values'] <- 'reported_revenue'
jnj_tbl <- jnj_tbl %>%
  inner_join(drug_list, by = 'brand') %>%
  select(molecule, reported_revenue)

jnj_tbl$reported_revenue <- as.numeric(jnj_tbl$reported_revenue)

#Novartis####
if (qtr_num == 1) {
  url <- paste0('https://www.novartis.com/news/novartis-financial-results-q1-',yr)
} else if (qtr_num == 2) {
  url <- paste0('https://www.novartis.com/news/novartis-financial-results-q2-',yr)
} else if (qtr_num == 3) {
  url <- paste0('https://www.novartis.com/news/novartis-financial-results-q3-',yr)
} else {
  url <- paste0('https://www.novartis.com/news/novartis-',yr,'-financial-results')
}

raw_list <- url %>%
  read_html() %>%
  html_nodes("a") %>%  # find all links in the page
  html_attr("href") %>% # get the url for these links
  str_subset("interim-financial")

raw_pdf <- pdf_text(raw_list)
pdf.text<-unlist(raw_pdf)
pdf.text<-tolower(raw_pdf)
page_find <- data.frame(str_detect(pdf.text, "net sales of the top 20 innovative medicines division")) #find unique string for pdf page
colnames(page_find)<-"Result"
page_find<-subset(page_find,page_find$Result==TRUE)
page <- as.numeric(rownames(page_find))[1]

raw_pdf <- pdf_text(raw_list)
profile <- strsplit(raw_pdf[page], "\n")
profile <- profile[[1]]
profile <- trimws(profile)
raw_tbl <- data.frame(str_split_fixed(profile, " {1,}", 11))
raw_tbl$X1 <- tolower(raw_tbl$X1)

cols.num <- c("X2","X3","X4","X5","X6",
              "X7","X8","X9","X10","X11")
raw_tbl[cols.num] <- sapply(raw_tbl[cols.num], as.integer)
raw_tbl <- raw_tbl[c(-2,-3)]

nov_tbl <- raw_tbl %>%
  filter(X1 %in% drug_list$brand)
rev_values <- c()

for (i in 1:length(nov_tbl$X1)) {
  x <- paste(nov_tbl[i,], collapse = ' ')
  rev_values[i] <- stri_extract_first_regex(x, "[0-9]+")
}
rev_values <- as.numeric(rev_values)
nov_tbl <- data.frame(brand = nov_tbl$X1, reported_revenue = rev_values)

#Following drugs have no reported sales within the US
nov_tbl$reported_revenue[nov_tbl$brand == 'jakavi'] <- NA
nov_tbl$reported_revenue[nov_tbl$brand == 'xolair'] <- NA
nov_tbl$reported_revenue[nov_tbl$brand == 'lucentis'] <- NA

nov_tbl <- nov_tbl %>%
  inner_join(drug_list, by = 'brand') %>%
  select(molecule, reported_revenue)

#Pfizer####
if (qtr_num == 1) {
  url <- paste0('https://s28.q4cdn.com/781576035/files/doc_financials/',yr,'/q1/Q1-',yr,'-PFE-Earnings-Release.pdf')
} else if (qtr_num == 2) {
  url <- paste0('https://s28.q4cdn.com/781576035/files/doc_financials/',yr,'/q2/Q2-',yr,'-PFE-Earnings-Release.pdf')
} else if (qtr_num == 3) {
  url <- paste0('https://s28.q4cdn.com/781576035/files/doc_financials/',yr,'/q3/Q3-',yr,'-PFE-Earnings-Release.pdf')
} else {
  url <- paste0('https://s28.q4cdn.com/781576035/files/doc_financials/',yr,'/q4/Q4-',yr,'-PFE-Earnings-Release.pdf')
}

raw_pdf <- pdf_text(url)
pdf.text<-unlist(raw_pdf)
pdf.text<-tolower(raw_pdf)
page_find <- data.frame(str_detect(pdf.text, paste0(tolower(quarter),'-quarter ',yr,' and ',yr-1))) #find unique string for pdf page
colnames(page_find)<-"Result"
page_find<-subset(page_find,page_find$Result==TRUE)
page <- as.numeric(rownames(page_find))[1]

raw_pdf <- pdf_text(url)
profile <- strsplit(raw_pdf[page], "\n")
profile <- profile[[1]]
profile <- trimws(profile)
raw_tbl <- data.frame(str_split_fixed(profile, " {2,}", 12))
raw_tbl$X1 <- tolower(raw_tbl$X1)

pfe_tbl <- raw_tbl %>%
  select(X1, X6) %>%
  filter(X1 %in% drug_list$brand)

names(pfe_tbl)[names(pfe_tbl) == 'X1'] <- 'brand'
names(pfe_tbl)[names(pfe_tbl) == 'X6'] <- 'reported_revenue'

pfe_tbl <- pfe_tbl %>%
  inner_join(drug_list, by = 'brand') %>%
  select(molecule, reported_revenue)

pfe_tbl$reported_revenue <- as.numeric(pfe_tbl$reported_revenue)

#Roche####
if (qtr_num == 1) {
  url <- paste0('https://www.roche.com/investors/events/q1-results-',yr)
} else if (qtr_num == 2) {
  url <- paste0('https://www.roche.com/investors/events/half-year-results-',yr)
} else if (qtr_num == 3) {
  url <- paste0('https://www.roche.com/investors/events/q3-results-',yr)
} else {
  url <- paste0('https://www.roche.com/investors/events/annual-results-',yr)
}

raw_list <- url %>%
  read_html() %>%
  html_nodes("a") %>%  # find all links in the page
  html_attr("href") %>% # get the url for these links
  str_subset("_ir_")

raw_pdf <- pdf_text(raw_list)
pdf.text<-unlist(raw_pdf)
pdf.text<-tolower(raw_pdf)
page_find <- data.frame(str_detect(pdf.text, "sales growth united states")) #find unique string for pdf page
colnames(page_find)<-"Result"
page_find<-subset(page_find,page_find$Result==TRUE)
page <- as.numeric(rownames(page_find))[2]

raw_pdf <- pdf_text(raw_list)
profile <- strsplit(raw_pdf[page], "\n")
profile <- profile[[1]]
profile <- trimws(profile)
raw_tbl <- data.frame(str_split_fixed(profile, " {2,}", 11))
raw_tbl$X1 <- tolower(raw_tbl$X1)

roche_tbl <- raw_tbl %>%
  select(X1, X10) %>%
  filter(X1 %in% drug_list$brand)
roche_tbl$X10 <- as.numeric(gsub(",", "", roche_tbl$X10))
roche_tbl$X10 <- as.numeric(roche_tbl$X10)
roche_tbl$X10 <- round((roche_tbl$X10*currency_tbl$Foreign_Rate[currency_tbl$CurrencyCode == 'CHF']),2)

names(roche_tbl)[names(roche_tbl) == 'X1'] <- 'brand'
names(roche_tbl)[names(roche_tbl) == 'X10'] <- 'reported_revenue'
roche_tbl <- roche_tbl %>%
  inner_join(drug_list, by = 'brand') %>%
  select(molecule, reported_revenue)

#Sanofi####
url <- "https://www.sanofi.com/en/investors/financial-results-and-events/financial-calendar"

raw_list <- url %>%
  read_html() %>%
  html_nodes("a") %>%  # find all links in the page
  html_attr("href") %>%
  str_subset(paste0("Q",qtr_num,"-results-",yr)) %>%
  str_c("https://www.sanofi.com", .)

url <- raw_list

raw_list <- url %>%
  read_html() %>%
  html_nodes("a") %>%  # find all links in the page
  html_attr("href") %>%
  str_subset("\\.pdf") %>%
  str_c("https://www.sanofi.com", .) %>% unique()

raw_pdf <- pdf_text(raw_list[1])
pdf.text<-unlist(raw_pdf)
pdf.text<-tolower(raw_pdf)
page_find <- data.frame(str_detect(pdf.text, "net sales by gbu, franchise, geographic region and product")) #find unique string for pdf page
colnames(page_find)<-"Result"
page_find<-subset(page_find,page_find$Result==TRUE)
page <- as.numeric(rownames(page_find))[1]

raw_pdf <- pdf_text(raw_list[1])
profile <- strsplit(raw_pdf[page], "\n")
profile <- profile[[1]]
profile <- trimws(profile)
raw_tbl <- data.frame(str_split_fixed(profile, " {2,}", 11))
raw_tbl$X1 <- tolower(raw_tbl$X1)

sny_tbl <- raw_tbl %>%
  select(X1, X5) %>%
  filter(X1 %in% drug_list$brand)

sny_tbl$X5 <- as.numeric(sny_tbl$X5)
sny_tbl$X5 <- round(sny_tbl$X5*currency_tbl$Foreign_Rate[currency_tbl$CurrencyCode == 'EUR'],2)

names(sny_tbl)[names(sny_tbl) == 'X1'] <- 'brand'
names(sny_tbl)[names(sny_tbl) == 'X5'] <- 'reported_revenue'
sny_tbl <- sny_tbl %>%
  inner_join(drug_list, by = 'brand') %>%
  select(molecule, reported_revenue)

#Seagen####
if (qtr_num == 4) {
  url <- paste0("https://investor.seagen.com/press-releases/news-details/",year(Sys.Date()),"/Seagen-Reports-Fourth-Quarter-and-Full-Year-",yr,"-Financial-Results/")
} else {
  url <- paste0("https://investor.seagen.com/press-releases/news-details/",yr,"/Seagen-Reports-",quarter,"-Quarter-",yr,"-Financial-Results/")
}

raw_tbl <- url %>%
  read_html() %>%
  html_nodes(xpath ='//*[contains(concat( " ", @class, " " ), concat( " ", "module_container--outer", " " ))]') %>%
  html_table()
raw_tbl <- raw_tbl[[11]]
raw_tbl$X1 <- tolower(raw_tbl$X1)

sgen_tbl <- raw_tbl %>%
  select(X1, X3) %>%
  filter(X1 %in% drug_list$brand)
names(sgen_tbl)[names(sgen_tbl) == 'X1'] <- 'brand'
names(sgen_tbl)[names(sgen_tbl) == 'X3'] <- 'reported_revenue'
sgen_tbl <- sgen_tbl %>%
  inner_join(drug_list, by = 'brand') %>%
  select(molecule, reported_revenue) %>% 
  unique() %>% distinct(molecule, .keep_all = TRUE)

sgen_tbl$reported_revenue <- as.numeric(sgen_tbl$reported_revenue)

#Takeda####
url <- "https://www.takeda.com/investors/financial-results/"

raw_list <- url %>%
  read_html() %>%
  html_nodes("a") %>%  # find all links in the page
  html_attr("href") %>%
  str_subset("_qfr_en.pdf") %>%
  str_c("https://www.takeda.com", .)

raw_pdf <- pdf_text(raw_list[length(raw_list)])
pdf.text<-unlist(raw_pdf)
pdf.text<-tolower(raw_pdf)
page_find <- data.frame(str_detect(pdf.text, paste0("q",jpy_qtr,"\n"))) #find unique string for pdf page
colnames(page_find)<-"Result"
page_find<-subset(page_find,page_find$Result==TRUE)
page <- as.numeric(rownames(page_find))

raw_pdf <- pdf_text(raw_list[length(raw_list)])
profile <- strsplit(raw_pdf[page[-1]], "\n")
profile <- profile[[1]]
profile <- trimws(profile)
raw_tbl <- data.frame(str_split_fixed(profile, " {2,}", 15))
raw_tbl$X1 <- tolower(raw_tbl$X1)

tkda_tbl <- raw_tbl %>%
  select(X1, X5) %>%
  filter(X1 %in% drug_list$brand)
tkda_tbl$X1 <- str_replace_all(tkda_tbl$X1, regex("[[:punct:][:digit:][:cntrl:]]"), "")
tkda_tbl$X1 <- trimws(tkda_tbl$X1)

names(tkda_tbl)[names(tkda_tbl) == 'X1'] <- 'brand'
names(tkda_tbl)[names(tkda_tbl) == 'X5'] <- 'reported_revenue'
tkda_tbl$reported_revenue <- as.numeric(as.character(tkda_tbl$reported_revenue))

tkda_tbl$reported_revenue[tkda_tbl$brand == 'adcetris'] <- NA
tkda_tbl$reported_revenue[tkda_tbl$brand == 'vectibix'] <- NA

tkda_tbl$reported_revenue <- round(tkda_tbl$reported_revenue*1000/currency_tbl$USD_Rate[currency_tbl$CurrencyCode == 'JPY'],2)

tkda_tbl <- tkda_tbl %>%
  inner_join(drug_list, by = 'brand') %>%
  select(molecule, reported_revenue)

#CombineTbls####
combined_tbl <- rbind(abbv_tbl,amgn_tbl,astl_tbl,azn_tbl,aveo_tbl,
                      blue_tbl,bms_tbl,clvs_tbl,lly_tbl,epzm_tbl,
                      gsk_tbl,incy_tbl,jazz_tbl,jnj_tbl,
                      nov_tbl,pfe_tbl,roche_tbl,sny_tbl,sgen_tbl,
                      tkda_tbl)
names(combined_tbl)[names(combined_tbl) == 'molecule'] <- 'DrugMasterName'
names(combined_tbl)[names(combined_tbl) == 'reported_revenue'] <- 'Reported_Revenue'

final_tbl <- data.frame(DrugMasterName = combined_tbl$DrugMasterName, Year = yr, Quarter = qtr_num,
                        CalendarQuarter = paste0(yr,"-Q",qtr_num), Reported_Revenue = combined_tbl$Reported_Revenue)
#Functions
#page_finder <- function(string = "") {
#  raw_pdf <- pdf_text(raw_list)
#  pdf.text<-unlist(raw_pdf)
#  pdf.text<-tolower(raw_pdf)
#  page_find <- data.frame(str_detect(pdf.text, string)) #find unique string for pdf page`
#  colnames(page_find)<-"Result"
#  page_find<-subset(page_find,page_find$Result==TRUE)
#  page <- as.numeric(rownames(page_find)) 
#
#  print(page)
#}