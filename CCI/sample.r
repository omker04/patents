library(data.table)
library(dplyr)
library(gtools)
library(tidyr)
library(cclust)
library(cluster)
library(caret)
scan_data = fread("/Users/skarma1/Desktop/International/sample_uk.xls")
colnames(scan_data) = gsub("cust_scan_category_3122.","",colnames(scan_data))
scan_data$upc_nbr = as.numeric(scan_data$upc_nbr)
visit_data0 = scan_data %>% select(store_nbr,visit_date,visit_nbr,upc_nbr,subclass_desc,fineline_desc,brand_name,unit_qty,retail_price,household_id)
visit_data0$unique_visit = paste0(visit_data0$store_nbr,"_",visit_data0$visit_date,"_",visit_data0$visit_nbr)
visit_hh_sales_sum = visit_data0 %>% group_by(household_id,unique_visit) %>% summarise(sum(retail_price))
visit_hh_sales_var = visit_data0 %>% group_by(household_id,unique_visit) %>% summarise(var(retail_price))
visit_hh_sales_var$`var(retail_price)` = na.replace(visit_hh_sales_var$`var(retail_price)`,0)
hh_sales_sum = visit_hh_sales_sum %>% group_by(household_id) %>% summarise(mean(`sum(retail_price)`))
hh_sales_var = visit_hh_sales_var %>% group_by(household_id) %>% summarise(mean(`var(retail_price)`))

hh_brand0 = visit_data0 %>% select(brand_name,unit_qty,retail_price,household_id)
hh_brand1 = hh_brand0 %>% group_by(household_id,brand_name) %>% summarise_each(funs(sum))
hh_brand_sales = spread(hh_brand1 %>% select(household_id,brand_name,retail_price),brand_name,retail_price,fill = 0)
hh_brand_sales = as.data.frame(hh_brand_sales)
colnames(hh_brand_sales)[-1]= paste("bsales", colnames(hh_brand_sales)[-1], sep = "_")
hh_brand_sales1 = as.data.frame(cbind(hh_brand_sales[1],t(apply(hh_brand_sales[,2:ncol(hh_brand_sales)],1,function(x){na.replace(x/sum(x),0)}))))

hh_brand_units = spread(hh_brand1 %>% select(household_id,brand_name,unit_qty),brand_name,unit_qty,fill = 0)
hh_brand_units = as.data.frame(hh_brand_units)
colnames(hh_brand_units)[-1]= paste("bunits", colnames(hh_brand_units)[-1], sep = "_")
hh_brand_units1 = as.data.frame(cbind(hh_brand_units[1],t(apply(hh_brand_units[,2:ncol(hh_brand_units)],1,function(x){na.replace(x/sum(x),0)}))))

hh_fineline0 = visit_data0 %>% select(fineline_desc,unit_qty,retail_price,household_id)
hh_fineline1 = hh_fineline0 %>% group_by(household_id,fineline_desc) %>% summarise_each(funs(sum))
hh_fineline_sales = spread(hh_fineline1 %>% select(household_id,fineline_desc,retail_price),fineline_desc,retail_price,fill = 0)
hh_fineline_sales = as.data.frame(hh_fineline_sales)
colnames(hh_fineline_sales)[-1]= paste("fsales", colnames(hh_fineline_sales)[-1], sep = "_")
hh_fineline_sales1 = as.data.frame(cbind(hh_fineline_sales[1],t(apply(hh_fineline_sales[,2:ncol(hh_fineline_sales)],1,function(x){na.replace(x/sum(x),0)}))))
hh_fineline_units = spread(hh_fineline1 %>% select(household_id,fineline_desc,unit_qty),fineline_desc,unit_qty,fill = 0)
hh_fineline_units = as.data.frame(hh_fineline_units)
colnames(hh_fineline_units)[-1]= paste("fbunits", colnames(hh_fineline_units)[-1], sep = "_")
hh_fineline_units1 = as.data.frame(cbind(hh_fineline_units[1],t(apply(hh_fineline_units[,2:ncol(hh_fineline_units)],1,function(x){na.replace(x/sum(x),0)}))))


#hh_data = merge(merge(merge(merge(merge(hh_sales_sum,hh_sales_var),hh_brand_sales1),hh_brand_units1),hh_fineline_sales1),hh_fineline_units1)
hh_data = merge(merge(merge(hh_sales_sum,hh_sales_var),hh_brand_sales1),hh_fineline_sales1)
hh_data0 = scale(hh_data[,-1])

set.seed(201546)
nc = seq(10,50,5)
sil = c()
for(i in 1:length(nc))
{
obj1 = kmeans(hh_data0,nc[i])
s1 = silhouette(obj1$cluster,dist(hh_data0))
sil[i] = mean(s1[,3])
}
plot(nc,sil,type = "b")
opt_cl = 25

obj2 = kmeans(hh_data0,opt_cl)
hh_cluster = cbind(hh_data[,1],obj2$cluster)
colnames(hh_cluster) = c("household_id","cluster")

####Visit_data
visit_brand0 = visit_data0 %>% select(brand_name,unit_qty,retail_price,unique_visit)
visit_brand1 = visit_brand0 %>% group_by(unique_visit,brand_name) %>% summarise_each(funs(sum))
visit_brand_sales = spread(visit_brand1 %>% select(unique_visit,brand_name,retail_price),brand_name,retail_price,fill = 0)
visit_brand_sales = as.data.frame(visit_brand_sales)
colnames(visit_brand_sales)[-1]= paste("bsales", colnames(visit_brand_sales)[-1], sep = "_")
visit_brand_sales1 = as.data.frame(cbind(visit_brand_sales[1],t(apply(visit_brand_sales[,2:ncol(visit_brand_sales)],1,function(x){na.replace(x/sum(x),0)}))))

visit_brand_units = spread(visit_brand1 %>% select(unique_visit,brand_name,unit_qty),brand_name,unit_qty,fill = 0)
visit_brand_units = as.data.frame(visit_brand_units)
colnames(visit_brand_units)[-1]= paste("bunits", colnames(visit_brand_units)[-1], sep = "_")
visit_brand_units1 = as.data.frame(cbind(visit_brand_units[1],t(apply(visit_brand_units[,2:ncol(hh_brand_units)],1,function(x){na.replace(x/sum(x),0)}))))

visit_fineline0 = visit_data0 %>% select(fineline_desc,unit_qty,retail_price,unique_visit)
visit_fineline1 = visit_fineline0 %>% group_by(unique_visit,fineline_desc) %>% summarise_each(funs(sum))
visit_fineline_sales = spread(visit_fineline1 %>% select(unique_visit,fineline_desc,retail_price),fineline_desc,retail_price,fill = 0)
visit_fineline_sales = as.data.frame(visit_fineline_sales)
colnames(visit_fineline_sales)[-1]= paste("fsales", colnames(visit_fineline_sales)[-1], sep = "_")
visit_fineline_sales1 = as.data.frame(cbind(visit_fineline_sales[1],t(apply(visit_fineline_sales[,2:ncol(visit_fineline_sales)],1,function(x){na.replace(x/sum(x),0)}))))
visit_fineline_units = spread(visit_fineline1 %>% select(unique_visit,fineline_desc,unit_qty),fineline_desc,unit_qty,fill = 0)
visit_fineline_units = as.data.frame(visit_fineline_units)
colnames(visit_fineline_units)[-1]= paste("fbunits", colnames(visit_fineline_units)[-1], sep = "_")
visit_fineline_units1 = as.data.frame(cbind(visit_fineline_units[1],t(apply(visit_fineline_units[,2:ncol(visit_fineline_units)],1,function(x){na.replace(x/sum(x),0)}))))
visit_data_f = merge(merge(merge(visit_hh_sales_sum,visit_hh_sales_var),visit_brand_sales1),visit_fineline_sales1)
visit_data_f1 = merge(visit_data_f,hh_cluster)
visit_data_f2 = visit_data_f1 %>% select(-household_id)

####Classification
cttrain = createDataPartition(visit_data_f2$cluster,p = 0.7)
acttrain = visit_data_f2[cttrain$Resample1,]
valid = visit_data_f2[-cttrain$Resample1,]

acttrain$cluster = factor(acttrain$cluster)
valid$cluster = factor(valid$cluster)
library(randomForest)
train_result <- randomForest(x = acttrain[,-c(1,ncol(acttrain))],y = acttrain$cluster,
                             importance = TRUE, ntree = 500, mtry = sqrt(ncol(acttrain) ))
# trvotes = train_result$votes
# plot(train_result)
# varImpPlot(train_result)

vld_class = predict(train_result,newdata = valid[,-ncol(valid)],type ="class")
c1 = confusionMatrix(valid$cluster,vld_class)
rfacc = c1$overall[1]
#test_class = predict(train_result,newdata = test_data,type ="class")


