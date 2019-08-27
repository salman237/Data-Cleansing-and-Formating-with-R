# Data source : https://archive.ics.uci.edu/ml/datasets/online+retail 

# load data
onlineretail <- read.csv("Documents/Github/Data-Cleansing-and-Formatting-with-R/OnlineRetail.csv",stringsAsFactors = F)

# summary of the data 
summary(onlineretail)

# checking variable data
str(onlineretail)
plot_str(onlineretail)

# Convert the data type for InvoiceDate variable from chr to Date (POSIXct)
onlineretail$InvoiceDate <- dmy_hm(onlineretail$InvoiceDate)

# Are there any missing data? 
plot_missing(onlineretail)

# if you want to drop, simply use this command: 
onlineretail_drop <- onlineretail[!is.na(onlineretail$CustomerID),]

# check missing data again 
plot_missing(onlineretail_drop)

# Recency   : jumlah hari ini s.d. terakhir bertransaksi (dalam hari)
# Frequency : jumlah transaksi yang terjadi dalam 6 bulan terakhir 
# Monetary  : jumlah uang yang dibelanjakan oleh Customer ID unik

frequency <- onlineretail_drop %>% group_by(CustomerID) %>% summarise(frequency = n_distinct(InvoiceNo)) 
monetary <- onlineretail_drop %>% group_by(CustomerID) %>% summarise(monetary=sum(UnitPrice*Quantity))                                               
recency <- onlineretail_drop %>% group_by(CustomerID) %>% arrange(desc(InvoiceDate)) %>%   filter(row_number()==1) %>% mutate(recency = as.numeric(as.duration(interval(InvoiceDate,ymd("2011-12-31"))))/86400) %>% select(CustomerID, recency)  

onlineretail_rfm <- recency %>% left_join(frequency,by="CustomerID") %>% left_join(monetary,by="CustomerID")

write.csv(onlineretail_rfm,"online_retail_rfm.csv",quote=F,row.names = F)
