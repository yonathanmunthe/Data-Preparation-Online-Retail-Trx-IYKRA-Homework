# Title       : Data Preparation_Cleansing and Formatting_Online Retail Trx_20190823.R
# Date Updated: 2019-08-23
# Author      : Yonathan Elia Munthe
# Description : This is an online retail script and its data prep process with RFM method.
# Objective   : To prep data to be ready to be used (after cleansing and formatting)
# Data source : https://archive.ics.uci.edu/ml/datasets/online+retail 

# This is a transnational data set which contains all the transactions occurring between 01/12/2010 and 09/12/2011 for a UK-based and registered non-store online retail.The company mainly sells unique all-occasion gifts. Many customers of the company are wholesalers.

# load library
library(tidyverse)
library(lubridate)
library(DataExplorer)

# set working directory and load the downloaded data
setwd("~/R/Project/Data Cleansing/formatting-master/homework")
rawdata <- read.csv("rawdata.csv",stringsAsFactors = F)

# preview first 3 rows of data
head(rawdata, n = 3)

# preview last 3 rows of data
tail(rawdata, n = 3)

# summary of the data 
summary(rawdata)

# We are going to clean the raw data :

# 1. Checking variable raw data
str(rawdata)
plot_str(rawdata)

# From the output, we identify the InvoiceDate is in chr type format and we should convert it to a date friendly format so we can use it as a date (to define recency). We are going to use lubridate package. with command : library(lubridate) that has been coded before.

# Convert the data type for InvoiceDate variable from chr to Date (POSIXct)
rawdata$InvoiceDate <- dmy_hm(rawdata$InvoiceDate)
str(rawdata)
plot_str(rawdata)

# 2. Checking missing data
plot_missing(rawdata)

# Customer ID have ~25% missing value. What are you going to do with that? Please ask your data owner! (in this case, we think that those empty data because the customer has no customer ID or bought as a guest)

# Because we want to identify the rfm from these customers, we should drop the empty data rows with this command : 
rawdata_drop <- rawdata[!is.na(rawdata$CustomerID),]

# check missing data again
plot_missing(rawdata_drop)

# Quiz      : Please make a tidy table from produk, transaksi, and profil_pelanggan table, thus contain the following variables using rawdata_drop data frame:
# CustomerID | Recency | Frequency | Amount 
# Recency   : jumlah hari ini (31 Des 2011) s.d. terakhir bertransaksi (dalam hari)
# Frequency : jumlah transaksi yang terjadi oleh Customer ID unik
# Monetary  : jumlah uang yang dibelanjakan oleh Customer ID unik

rawdata_drop

# Menghitung recency
recency <- rawdata_drop %>% group_by(CustomerID) %>% arrange(desc(InvoiceDate)) %>%   filter(row_number()==1) %>% mutate(recency = as.numeric(as.duration(interval(InvoiceDate,ymd("2011-12-31"))))/86400) %>% select(CustomerID, recency)  
recency

# Menghitung frequency
frequency <- rawdata_drop %>% group_by(CustomerID) %>% summarise(frequency = n_distinct(InvoiceNo)) 
frequency

# Menghitung monetary
monetary <- rawdata_drop %>% group_by(CustomerID) %>% summarise(monetary=sum(UnitPrice*Quantity))
monetary

# Join tabel recency + frequency + monetary dengan menggunakan left_join function :
df_rfm <- recency %>% left_join(frequency,by="CustomerID") %>% left_join(monetary,by="CustomerID") %>% arrange(CustomerID)

# Data prep done, view table :
df_rfm

# Check summary df_rfm, can you check the median of each recency, frequency, and monetary?
summary(df_rfm)

# Median of recency, frequency, dan monetary known, you can also use function: median(). 
mr <- median(df_rfm$recency)
mf <- median(df_rfm$frequency)
mm <- median(df_rfm$monetary)

# Encode with this condition: If more than median, then high(1), if less than median then low(0), and assign as variable: df_rfm_encod

df_rfm_encod_answer <- df_rfm %>% mutate (recency=ifelse(recency>=mr,1,0),
                                          frequency=ifelse(frequency>=mf,1,0),
                                          monetary=ifelse(monetary>=mm,1,0))
df_rfm_encod_answer