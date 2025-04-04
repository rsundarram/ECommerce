# high value customers identification 

ecom <- read.csv('Ecommerce.csv',stringsAsFactors = FALSE)


View(ecom)

str(ecom)

ecom <- ecom[,-9]


str(ecom)


summary(ecom)

# how to find missing values

sapply(ecom,function(x){sum(is.na(x))})

is.na(c('a',NA,'c'))

sum(is.na(c('a',NA,'c')))  



#if  customer id - is NA, delete it

ecom_1 <- na.omit(ecom)

dim(ecom_1)

dim(ecom)


summary(ecom_1)


# high value customers -


# high order value

# higher frequency of ordering

# high recent orders

str(ecom_1)

#invoice date - date format

as.Date('character',format ='%d-%m-%Y')



install.packages('lubridate')

library('lubridate')

date-month-year

dmy('28-Mar-20')


#yyyy-mm-dd output format for dates

ecom_1$InvoiceDate <-  dmy(ecom_1$InvoiceDate)


str(ecom_1)

max(ecom_1$InvoiceDate)

# days since last order

ecom_1$days_passed <-max(ecom_1$InvoiceDate)- ecom_1$InvoiceDate

View(ecom_1)


# summarise this info on the level of customer id

library('dplyr')

# remove the obs where the quantity < 0

ecom_1 <- ecom_1 %>% filter(Quantity >0)

summary(ecom_1)

# aggregate the dataset at the level of customer id
ecom_cust <- ecom_1 %>% group_by(CustomerID) %>%  summarise (total_quantity=as.numeric(sum(Quantity)),
                                                             total_value=as.numeric(sum(Quantity*UnitPrice)),
                                                             last_order=as.numeric(min(days_passed)),
                                                             
                                                             dist_orders=n_distinct(StockCode))



View(ecom_cust)

# clusters


# normalize the data

ecom_sc <- scale(ecom_cust[,-1])


# optimum no of clusters

# factoextra

fviz_nbclust(ecom_sc,kmeans,method='wss')

# optimum no of clusters-4

kmeans_model <- kmeans(ecom_sc,4,iter.max = 30)

# hier clustering

distance <- dist(ecom_sc,method='euclidean')


hier_clus <- hclust(distance,method='ward.D')




# which obs will go to which clus

ecom_cust$clus_num <-  kmeans_model$cluster



kmeans_model$size

