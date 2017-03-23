#code by Sunil Udhayakumar

#Paytm machine learning challenge

#read the transaction/sales data after converting it into csv file
sales <- read.csv(file = "D:/Paytm/part-00000.csv",sep = ",")

#read the order_return/ordercancel data after converting it into csv file
return <- read.csv(file = "D:/Paytm/return.csv",sep = ",")

#---------- group the merchant id with respect to total # of orders and total # of items sold ------------

table(sales$merchant_id)

merchantbyorder <- data.frame(table(sales$merchant_id))
no_of_items <- aggregate(sales$qty_ordered, by=list(merchant_id=sales$merchant_id), FUN=sum)
merchantbyorder[,3] <- no_of_items$x

colnames(merchantbyorder) <- c("merchant_id","total_no_orders","total_no_items")

#---------- group the merchant id with respect to total revenue ------------


merchantbyrevenue <- aggregate(sales$item_selling_price, by = list(Category = sales$merchant_id), FUN=sum)
colnames(merchantbyrevenue) <- c("merchant_id","total_revenue")

#------------------------------------------------------------------
#check if any product is listed item_price above MRP
#sales[sales$item_mrp < sales$item_price,]

#---------find Merchants who sold products with selling price more than listed price ----------
invalid_selling_price <- sales[sales$item_price < sales$item_selling_price,]

#---------find invalid orders which have been returned -------------
keys <- c("merchant_id", "T4")
tinvalid <- merge(invalid_selling_price,return,by= keys)

#------------seperate date from time stamp -----------------------------------------------------
# create a data frame for the date values
merchant_by_shippingdate <- data.frame(sales$merchant_id)

#items created at                              
merchant_by_shippingdate[,2] <- gsub("(\\d{2}:\\d{2}).*", "", sales$item_created_at, perl=TRUE)
merchant_by_shippingdate[,2] <- as.Date(merchant_by_shippingdate[,2],"%Y-%m-%d")

#item ship by date
merchant_by_shippingdate[,3]<- gsub("(\\d{2}:\\d{2}).*", "", sales$item_ship_by_date, perl=TRUE)
merchant_by_shippingdate[,3] <- as.Date(merchant_by_shippingdate[,3],"%Y-%m-%d")

#fulfillment shipped at
merchant_by_shippingdate[,4]<- gsub("(\\d{2}:\\d{2}).*", "", sales$fulfillment_shipped_at, perl=TRUE)
merchant_by_shippingdate[,4] <- as.Date(merchant_by_shippingdate[,4],"%Y-%m-%d")

#fulfillment created at
merchant_by_shippingdate[,5]<- gsub("(\\d{2}:\\d{2}).*", "", sales$fulfillment_created_at, perl=TRUE)
merchant_by_shippingdate[,5] <- as.Date(merchant_by_shippingdate[,5],"%Y-%m-%d")

#change the column names
colnames(merchant_by_shippingdate) <- c("merchant_id","item_created_at","item_ship_by_date","fulfillment_shipped_at","fulfillment_created_at")

merchant_by_shippingdate[,6] <- difftime(merchant_by_shippingdate$item_ship_by_date,merchant_by_shippingdate$fulfillment_shipped_at,units="days")

merchant_by_shippingdate <- merchant_by_shippingdate[complete.cases(merchant_by_shippingdate),]

merchant_shippingdate <- aggregate(merchant_by_shippingdate$V6, by=list(merchant_id=merchant_by_shippingdate$merchant_id), FUN=sum)

merchant_shippingdate <- read.csv(file = "D:/Paytm/shipping.csv",sep = ",")

#------------groupby of return and cancel orders-------------------

table(sales$merchant_id)

merchantbyreturn <- data.frame(table(sales$merchant_id))
#no_of_items <- aggregate(return$cancel_num, by=list(merchant_id=sales$merchant_id), FUN=sum)
merchantbyreturn <- aggregate(return$cancel_num, by=list(merchant_id=return$merchant_id), FUN=sum)

return_num <- aggregate(return$return_num, by=list(merchant_id=return$merchant_id), FUN=sum)

merchantbyreturn[,3] <- return_num[,2]
rm(return_num)
colnames(merchantbyreturn) <- c("merchant_id","cancel_num","return_num")

#------------------return and revenue tables are joined --------

unik <- merge(merchantbyrevenue,merchantbyreturn,by="merchant_id",all.y = TRUE)

revenue_return <- unik[complete.cases(unik),]


#merge revenue_return with merchant by order

merchant_final <- merge(revenue_return,merchantbyorder,by="merchant_id",all.y = TRUE)

merchant_final <- merge(merchant_final,merchant_shippingdate,by="merchant_id",all.y = TRUE)

#remove unwanted column

merchant_final$X <- NULL

#----------------normalize the values------------

final <- merchant_final

final$norm_total_revenue_ <- final$total_revenue/sum(final$total_revenue)
final$norm_cancel_num_ <- final$cancel_num/sum(final$cancel_num)
final$norm_return_num <- final$return_num/sum(final$return_num)
final$norm_total_orders <- final$total_no_orders/sum(final$total_no_orders)
final$norm_total_items <- final$total_no_items/sum(final$total_no_items)

#-------------finding the Z-score -----------
final$result <- rowSums(final[,c("norm_total_revenue_","norm_total_orders","x","norm_total_items")]) - rowSums(final[,c("norm_cancel_num_","norm_return_num")])
final$z_score <- scale(final$result,center = TRUE,scale = TRUE)

result <- final[,c(1,14)]
#result ranking in descending order
result <- result[order(-result$z_score),]






