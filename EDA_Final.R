###################################################################################################################################
# Ecommerce Capstone Project
# EDA File
###################################################################################################################################

###################################################### Load required packages######################################################

requiredPackages = c('DataCombine','scales','glmnet','DAAG','caret','GGally','corrplot','readxl',
                     'lubridate','gdata','ggplot2','dplyr','reshape','tidyr','data.table','MASS','car')

#Installing needed packages
for(i in requiredPackages){
  if(!require(i,character.only = TRUE)){
    install.packages(i)
  }
  library(i, character.only = TRUE)
}

###################################################################################################################################

###################################################### Load Order data ############################################################

con_order <- read.csv ( "ConsumerElectronics.csv" , header = T , stringsAsFactors = F)
nrow(con_order)  ##1648824
str(con_order)

# Counting NA values in each column
sapply(con_order, function(x){sum(is.na(x))})   

nrow(filter(con_order , con_order$gmv <= 0 ))  # 349 rows 

table(con_order$deliverybdays)  # negative and very high deliverybdays 
table(con_order$deliverycdays)  # negative and very high deliverycdasy

table(con_order$s1_fact.order_payment_type)  ## COD is preferred over Prepaid 

table(con_order$sla)  #5979 rows with 0 SLA

length(unique(con_order$pincode)) # 7565 unique values 
length(unique(con_order$cust_id)) # 1201090 distinct customers 

unique(con_order$product_analytic_super_category) # Single value CE 
unique(con_order$product_analytic_sub_category)   # 14 distinct values 
unique(con_order$product_analytic_category)       # 5 distinct values 
unique(con_order$product_analytic_vertical)       # 74 distinct values 

## 51 product_analytic_vertical under CameraAccessory", "GamingAccessory", "HomeAudio"
filter ( con_order ,con_order$product_analytic_sub_category %in% c("CameraAccessory", "GamingAccessory", "HomeAudio")) %>% group_by(product_analytic_sub_category , product_analytic_vertical)%>% summarise(  count = n()) 

nrow(filter(con_order , con_order$product_mrp <= 0 )) ## 5308 row


min(con_order$order_date)   ## "2015-05-19 13:42:09"
max (con_order$order_date ) ## "2016-07-25 01:19:45"

# Create a year-month variable 
con_order$Year_Month <- paste(con_order$Year, con_order$Month , sep = '-')
con_order$order_date <- as.Date(con_order$order_date)
con_order$start_week_date <-   floor_date(as.Date(con_order$order_date), unit="week" , week_start = getOption("lubridate.week.start", 1))
con_order[which(con_order$start_week_date < '2015-07-01'),"start_week_date"] <- '2015-07-01'


# Find out the week number from a date
con_order$week_no <-   strftime( con_order$start_week_date ,format="%V")
# Create a year-month variable 
con_order$Year_Month <- paste(con_order$Year, con_order$Month , sep = '-')

# Filter for date July 2015 to June 2016
con_order <- subset(con_order, con_order$order_date >= '2015-07-01' & con_order$order_date < '2016-07-01')

# Remove the missing values 
row.has.na <- apply(con_order, 1, function(x){any(is.na(x))})

con_order <- con_order[!row.has.na,]

con_order$week_no <- as.numeric(con_order$week_no)

# Add a varaiable to specify month number as per week start date 
con_order$month_asper_week_startdate <- format(con_order$start_week_date , "%m")

con_order$month_asper_week_startdate <- as.numeric(con_order$month_asper_week_startdate)

# Number the week from 1 to 53
con_order$week_no[con_order$Year == 2015 ] <- (con_order$week_no[con_order$Year == 2015 ]) -26
con_order$week_no[con_order$Year == 2016 & con_order$week_no !=53 ] <- (con_order$week_no[con_order$Year == 2016 & con_order$week_no !=53 ]) +27
con_order[which(con_order$Year == 2016 & con_order$Month==1 &con_order$week_no == 53 ), "week_no"] <- con_order[which(con_order$Year == 2016 & con_order$Month==1 &con_order$week_no == 53 ), "week_no"] - 26



# Filter out the rows having mrp value 0 
con_order <- con_order[!con_order$product_mrp <= 0,]

con_order$deliverybdays[con_order$deliverybdays < 0] = 0
con_order$deliverycdays[con_order$deliverycdays < 0] = 0
con_order$product_procurement_sla [con_order$product_procurement_sla <0 ] =0

con_order$deliverybdays <- as.numeric(con_order$deliverybdays)
con_order$deliverycdays <- as.numeric(con_order$deliverycdays)
con_order$sla <- as.numeric(con_order$sla)
con_order$delivery_on_time <- con_order $sla - (con_order$deliverybdays+con_order$deliverycdays+con_order$product_procurement_sla)

con_order$delivery_status[con_order$delivery_on_time < 0] <- 'LateDelivery'
con_order$delivery_status[con_order$delivery_on_time == 0] <- 'TimeDelivery'
con_order$delivery_status[con_order$delivery_on_time > 0] <- 'EarlyDelivery'


######################## Load Product data### tags###########

##############################################################
## KPI FUNCTION FOR ALL THE 3 CATEGORIES ##
##############################################################

  
  # dataset=GamingAccessory
  #1. KPI - List price for all the products
con_order$list_price <- con_order$gmv/con_order$Units

#2. KP - Promotional Offer for all the products
con_order$promotional_offer <- (con_order$MRP-con_order$list_price)/con_order$MRP


#3. Clustering
## Creating a new KPI (though not used in the final model)
## It divides the products into three categories based on MRP and num units sold - 
## mass market, medium market and premium product

#con_order$P_analytic_vertical <- factor(con_order$P_analytic_vertical)

#cluster<- aggregate(cbind(Units,list_price,MRP)~P_analytic_vertical,con_order,mean)


#if(nrow(cluster)<=3){
  
 # cluster$p_tag <-NA
  
  
  # Assuming premium product:- 
  
  #cluster$P_tag[which(cluster$MRP>=mean(cluster$MRP))] <- "Middle_p"
  #cluster$P_tag[-which(cluster$MRP>=mean(cluster$MRP))] <- "Mass_p"
  
  #cluster <- cluster[,-c(2:4)]
  
  #dataset <-merge(dataset,cluster,by="P_analytic_vertical")
  
#} else {
  
  #str(cluster) 
  
 # cluster$list_price_1 <- scale(cluster$list_price)
  #cluster$MRP_1<- scale(cluster$MRP)
  #cluster$Units_1 <- scale(cluster$Units)
  
  #str(cluster)
  
  #k1 <- cluster[,-c(2:4)]
  
  
  #clust <- kmeans(k1[,-1], centers = 3,iter.max = 50,nstart = 50)
  
  #cluster$P_tag <- as.factor(clust$cluster)
  
  #cluster <- cluster[,c(1,8)]
  
  # Add extra column in dataset with 
  
  #dataset <-merge(dataset,cluster,by=c("P_analytic_vertical"),all.x=TRUE)
  
  
  #library("plyr")
  #library("dplyr")
  
  #k2 <- table(dataset$P_tag)
  
  #levels(dataset$P_tag)[which(k2==max(table(dataset$P_tag)))] <- "Mass_p"
  #levels(dataset$P_tag)[which(k2==min(table(dataset$P_tag)))] <- "Premium_p"
  #levels(dataset$P_tag)[which(k2!=max(table(dataset$P_tag))& k2!=min(table(dataset$P_tag)))] <- "Middle_p"
  
  
#}



###################################################################################################################################

###################################################### Load Media Investment data ################################################

Media_Investment <- read_excel("Media data and other information.xlsx",sheet="Media Investment",skip = 3, col_names = FALSE)

colnames(Media_Investment)<-c("Year","Month","Total_Investment","TV","Digital","Sponsorship","Content_Marketing","Online_Marketing",
                              "Affiliates","SEM","Radio","Other")

#rm(Media_Investment)



## computing Month, week, and no.of days per week (month, week)
days <- seq(as.Date("2015-07-01"),as.Date("2016-06-30"),'days')
weekdays <- data.frame('days'=days, Month = month(days), week = week(days),nweek = rep(1,length(days)))
weekdays <- data.frame(weekdays %>% group_by(Month,week) %>% summarise(nweeks = sum(nweek)))
weekdays$fracDays <- weekdays$nweeks/7

## Replacing NA values
Media_Investment[is.na(Media_Investment)] <- 0

## converting montly spend to weekly
Media_Investment <- cbind(Month=Media_Investment[,c(2)], Media_Investment[,-c(1,2)]/4.30)

## Add weekly information
media_investment_weekly <- merge(weekdays,Media_Investment, by='Month', all.x = TRUE)

colnames(media_investment_weekly)

## Converting media Investment at weekly granularity
# pro-rate weekly investment as per the ratio of its days span over adjacent months
media_investment_weekly_final <- data.frame(media_investment_weekly %>% group_by(week) %>% 
                                              summarise(Total_Investment = sum(Total_Investment*fracDays),
                                                        TV = sum(TV*fracDays), Digital=sum(Digital*fracDays),Sponsorship = sum(Sponsorship*fracDays), 
                                                        Content_Marketing = sum(Content_Marketing*fracDays),Online_Marketing = sum(Online_Marketing*fracDays), 
                                                        Affiliates = sum(Affiliates*fracDays), SEM = sum(SEM*fracDays), Radio = sum(Radio*fracDays), 
                                                        Other = sum(Other*fracDays)))

## Converting media investement into crores
media_investment_weekly_final[,2:11] <- media_investment_weekly_final[,2:11]*10000000

# Considering the adstock rate as 50%
adstock_rate = 0.50

# Creating the adstock for each media investment
df <- data.frame(week_no=1:53)

for(i in 3:ncol(media_investment_weekly_final)){
  
  df[[paste0(colnames(media_investment_weekly_final)[i],"_adstock")]] <- stats::filter(x=media_investment_weekly_final[i], 
                                                                                       filter=adstock_rate, method="recursive")
  
}

media_investment_weekly_final <- cbind(media_investment_weekly_final,df)


###################################################################################################################################

############################################# Loading Special Sale Calender Data ##################################################

#Special_Sale_calender <- read.xls("Media data and other information.xlsx", sheet = 3, header = TRUE ,stringsAsFactors = F   )
Special_Sale_calender <- read_excel("Media data and other information.xlsx",sheet="Special Sale Calendar")
Special_Sale_calender$Year[1:6] <- 2015
Special_Sale_calender$Year[7:12] <- 2016
Special_Sale_calender[,1] <- NULL

#Derived manually from holidays
Special_Sale_calender$s_week <- c(3,7,9,16,19,26,30,32,34,33,37,48)
Special_Sale_calender$e_week <- c(3,8,9,16,20,27,30,32,34,34,37,48)
Special_Sale_calender$promotion_type <- trim(sapply(Special_Sale_calender$`Sales Calendar`, function(x) substr(x , 1,  (regexpr("\\(", x[1])-1 ))))
Special_Sale_calender$'Sales Calendar' <- NULL
Special_Sale_calender<-as.data.frame(Special_Sale_calender)
Special_Sale_calender_long <- gather ( Special_Sale_calender , week_type , week_no , 2:3)
Special_Sale_calender_long$week_type <- NULL

###################################################################################################################################

############################################# Loading NPS (Net Promoter Score) Data ###############################################

nps <- read_excel("Media data and other information.xlsx",sheet="Monthly NPS Score",skip = 2, col_names = FALSE)

str(nps)
nps <- nps[2:13]
t_nps <- transpose(nps)
t_nps$Month <- c(seq(7,12,1),seq(1,6,1))
colnames(t_nps)[1] <- "NPS"

###################################################################################################################################


###############################Aggregate the multiple dataset to create a master dataset at weekly level ##########################

## Group the data at weekly level 
weekly_order_data <- con_order %>% group_by ( Year,
                                              month_asper_week_startdate,
                                              product_analytic_category,
                                              product_analytic_sub_category,
                                              product_analytic_vertical,
                                              Year_Month ,
                                              week_no)%>% summarise( prepaid_cnt =  sum(ifelse (s1_fact.order_payment_type =='Prepaid' , 1 , 0)) ,
                                                                     cod_cnt =  sum(ifelse (s1_fact.order_payment_type =='COD' , 1,0)) ,
                                                                     delayed_delivery_cnt =sum(ifelse (delivery_status =='LateDelivery' , 1 , 0)), 
                                                                     early_delivery_cnt =sum(ifelse (delivery_status =='EarlyDelivery' , 1 , 0)), 
                                                                     onetime_delivery_cnt =sum(ifelse (delivery_status =='TimeDelivery' , 1 , 0)), 
                                                                     tot_gmv = sum(gmv) , 
                                                                     tot_units = sum(units) , 
                                                                     tot_product_mrp = sum( as.numeric (product_mrp)), 
                                                                     avg_gmv = mean(gmv) , avg_mrp = mean(product_mrp) , 
                                                                     no_of_customer = length(unique(cust_id)), 
                                                                     no_of_orders = length(unique(order_id)) , 
                                                                     list_price = (tot_gmv/tot_units) , 
                                                                     avg_price = mean(list_price) )

colnames(weekly_order_data)[2] <- "Month"

# Merge the Media_Investment data with weekly data 
#weekly_order_med_inv_data <- merge(weekly_order_data ,Media_Investment , by=c("Year" , "Month"))
weekly_order_med_inv_data <- merge(weekly_order_data ,media_investment_weekly_final , by=c("week_no"))

# Remove Year_Month.y
weekly_order_med_inv_data$Year_Month.y <- NULL 

# Merge the NPS data  
weekly_order_med_inv_data <- merge(weekly_order_med_inv_data ,t_nps , by=c("Month"))

# Find out how many weekly data are there in a month  
week_in_a_month <- weekly_order_med_inv_data %>% group_by( Month ) %>% summarize (  tot_week = length(unique(week_no)) )

weekly_order_med_inv_data <- merge(weekly_order_med_inv_data ,week_in_a_month, by = c ( "Month") )

count_in_a_week <- weekly_order_med_inv_data %>% group_by( week_no ) %>% summarize ( total_row = n())

weekly_order_med_inv_data <- merge(weekly_order_med_inv_data ,count_in_a_week, by = c ( "week_no") )

#Convert monthly ad spend into weekly ad spend 
weekly_order_med_inv_data[,c(22:30)] <- weekly_order_med_inv_data[,c(22:30)]/(weekly_order_med_inv_data$tot_week*weekly_order_med_inv_data$total_row)

## Add the promotional sale name in dataset

weekly_order_med_inv_data$promotion_type <- NULL
for (row_no  in 1:nrow(Special_Sale_calender) ) {
  for (week in Special_Sale_calender[row_no,2] : Special_Sale_calender[row_no,3] ){
    print(paste("The week is", week))
    weekly_order_med_inv_data[which(weekly_order_med_inv_data$week_no==week),"promotion_type"]  <-   Special_Sale_calender[row_no,4]
  }
}




###################################################################################################################################



#############################################################################################
## Engineered variables 
###############################################################################################
weekly_order_med_inv_data$discount_over_mrp <- (weekly_order_med_inv_data$tot_product_mrp-weekly_order_med_inv_data$tot_gmv)/weekly_order_med_inv_data$tot_product_mrp
weekly_order_med_inv_data$Holiday_week <- ifelse (is.na(weekly_order_med_inv_data$promotion_type) , 'N','Y' )
weekly_order_med_inv_data[which(is.na(weekly_order_med_inv_data$promotion_type)), "promotion_type"] <- "No_promotion"
weekly_order_med_inv_data$value_per_visitor <- weekly_order_med_inv_data$tot_gmv/weekly_order_med_inv_data$no_of_customer



#############################################################################
### Perform EDA analysis on weekly_order_med_inv_data and order_rawdata
## EDA will show we need different ad spend for each product category 
#############################################################################


## Create week wise sale and ad spend details for various sub category level 
week_sale_details <- weekly_order_med_inv_data %>% group_by (product_analytic_sub_category, week_no)%>% 
  summarise(tot_sales = sum(tot_gmv) ,
            tot_tv_spend = sum (TV), tot_dig_spend = sum (Digital), 
            tot_spon_spend = sum(Sponsorship) , tot_content_spend = sum(Content_Marketing),
            tot_online_spend = sum(Online_Marketing) ,tot_aff_spend = sum(Affiliates),
            tot_sem_spend = sum(SEM) ,tot_radio_spend = sum(Radio), 
            tot_oter_spend = sum(Other))


## weekly sale details for different sub category 
p <- ggplot(week_sale_details , aes ( x = week_no , y = tot_sales))+ geom_line()
p + facet_wrap( ~ week_sale_details$product_analytic_sub_category, nrow =2, ncol = 7) + labs(x = "week", y = "Sales " ) + ggtitle ( " Sales  vs Total Ad spend")

## Total TV ad spend vs sales details
p <- ggplot(week_sale_details , aes ( x = tot_tv_spend , y = tot_sales))+ geom_line()
p + facet_wrap( ~ week_sale_details$product_analytic_sub_category, nrow =2, ncol = 7) + labs(x = "Ad spend", y = "Sales ") + ggtitle ( " Sale vs TV Ad")

p <- ggplot(week_sale_details , aes ( x = tot_dig_spend , y = tot_sales))+ geom_line()
p + facet_wrap( ~ week_sale_details$product_analytic_sub_category, nrow =2, ncol = 7) + labs(x = "Ad spend", y = "Sales ") + ggtitle ( " Sale vs Digital Ad")

p <- ggplot(week_sale_details , aes ( x = tot_spon_spend , y = tot_sales))+ geom_line()
p + facet_wrap( ~ week_sale_details$product_analytic_sub_category, nrow =2, ncol = 7) + labs(x = "Ad spend", y = "Sales ") + ggtitle ( " Sale vs Sponsor Ad")

p <- ggplot(week_sale_details , aes ( x = tot_content_spend , y = tot_sales))+ geom_line()
p + facet_wrap( ~ week_sale_details$product_analytic_sub_category, nrow =2, ncol = 7) + labs(x = "Ad spend", y = "Sales ") + ggtitle ( " Sale vs Content Ad")

p <- ggplot(week_sale_details , aes ( x = tot_online_spend , y = tot_sales))+ geom_line()
p + facet_wrap( ~ week_sale_details$product_analytic_sub_category, nrow =2, ncol = 7) + labs(x = "Ad spend", y = "Sales ") + ggtitle ( " Sale vs Online Ad")

p <- ggplot(week_sale_details , aes ( x = tot_aff_spend , y = tot_sales))+ geom_line()
p + facet_wrap( ~ week_sale_details$product_analytic_sub_category, nrow =2, ncol = 7) + labs(x = "Ad spend", y = "Sales ") + ggtitle ( " Sale vs Affiliate Ad")

p <- ggplot(week_sale_details , aes ( x = tot_sem_spend , y = tot_sales))+ geom_line()
p + facet_wrap( ~ week_sale_details$product_analytic_sub_category, nrow =2, ncol = 7) + labs(x = "Ad spend", y = "Sales ") + ggtitle ( " Sale vs SEM Ad")

p <- ggplot(week_sale_details , aes ( x = tot_radio_spend , y = tot_sales))+ geom_line()
p + facet_wrap( ~ week_sale_details$product_analytic_sub_category, nrow =2, ncol = 7) + labs(x = "Ad spend", y = "Sales ") + ggtitle ( " Sale vs Radio Ad")

p <- ggplot(week_sale_details , aes ( x = tot_oter_spend , y = tot_sales))+ geom_line()
p + facet_wrap( ~ week_sale_details$product_analytic_sub_category, nrow =2, ncol = 7) + labs(x = "Ad spend", y = "Sales ") + ggtitle ( " Sale vs Other Ad")

## Sale at different promotional and non promotional weeks
weekly_order_med_inv_data %>% group_by ( promotion_type) %>% summarise( Avg_sale = mean(tot_gmv)) %>% ggplot( aes ( x=promotion_type, y =Avg_sale  )) + geom_bar(stat = "identity")

## Sale at different promotional weeks for different sub categories 
weekly_order_med_inv_data %>% group_by ( product_analytic_sub_category ,promotion_type) %>% summarise( Avg_sale = mean(tot_gmv)) %>% ggplot( aes ( x=promotion_type, y =Avg_sale  )) + geom_bar(stat = "identity") +facet_wrap( ~ product_analytic_sub_category, nrow =2, ncol = 7)+theme(axis.text.x=element_text(angle = -90, hjust = 0))

## weekly ad spend vs sales 
sale_vs_week_ad <- weekly_order_med_inv_data %>% group_by ( week_no) %>% summarise(tot_sales = sum(tot_gmv) , ad_spend = sum(TV+Digital+Sponsorship+Content_Marketing+Online_Marketing+Affiliates+SEM+Radio+Other))
sale_vs_week_ad_long <- gather(sale_vs_week_ad, Type, Spend, 2:3)
ggplot(sale_vs_week_ad_long, aes ( x= week_no , y = Spend , color = Type))+geom_line()

### Disocunt percentage vs average sales

discount_vs_sales <- weekly_order_med_inv_data[,c("tot_gmv" , "discount_over_mrp")]
discount_vs_sales$discount_range <- ifelse (discount_vs_sales$discount_over_mrp <= .1 , 'up to 10', ifelse ( discount_vs_sales$discount_over_mrp > .1 & discount_vs_sales$discount_over_mrp <= .3 , 'up to 30', ifelse(discount_vs_sales$discount_over_mrp > .3 & discount_vs_sales$discount_over_mrp <= .5 , 'up to 50','>50') ))
discount_vs_sales %>% group_by(discount_range) %>% summarise( avg_sale = mean(tot_gmv)) %>% ggplot(aes ( x=discount_range , y =avg_sale  )) + geom_bar(stat = "identity")

## Avg discount at different promotional and non promotional week
weekly_order_med_inv_data %>% group_by(promotion_type) %>% summarise(avg_disc = mean(discount_over_mrp)) %>% ggplot(aes(x= promotion_type, y =avg_disc )) + geom_bar(stat = "identity")

## Nps vs week
weekly_order_med_inv_data %>% group_by(week_no) %>% summarise(nps = mean(NPS)) %>% ggplot(aes(x= week_no, y =nps )) + geom_bar(stat = "identity")

## payment type vs number of orders
con_order %>% group_by ( s1_fact.order_payment_type) %>% summarise(order_cnt = n()) %>% ggplot(aes(x= s1_fact.order_payment_type, y =order_cnt )) + geom_bar(stat = "identity")

## delivery_status  vs number of orders
con_order %>% group_by ( delivery_status) %>% summarise(order_cnt = n()) %>% ggplot(aes(x= delivery_status, y =order_cnt )) + geom_bar(stat = "identity")


############################################################################
###create 3 different data set & add engineered kpis
#############################################################################
unique(weekly_order_med_inv_data$product_analytic_category)
## Since model needs to be built at sub category level, this varaiable is needed 
weekly_order_med_inv_data$product_analytic_category <- NULL 
weekly_order_med_inv_data$Year_Month.x <- NULL 


weekly_order_med_inv_data$TV_adstock <- as.vector(weekly_order_med_inv_data$TV_adstock)
weekly_order_med_inv_data$Digital_adstock <- as.vector(weekly_order_med_inv_data$Digital_adstock)
weekly_order_med_inv_data$Sponsorship_adstock <- as.vector(weekly_order_med_inv_data$Sponsorship_adstock)
weekly_order_med_inv_data$Content_Marketing_adstock <- as.vector(weekly_order_med_inv_data$Content_Marketing_adstock)
weekly_order_med_inv_data$Online_Marketing_adstock <- as.vector(weekly_order_med_inv_data$Online_Marketing_adstock)
weekly_order_med_inv_data$Affiliates_adstock <- as.vector(weekly_order_med_inv_data$Affiliates_adstock)
weekly_order_med_inv_data$SEM_adstock <- as.vector(weekly_order_med_inv_data$SEM_adstock)
weekly_order_med_inv_data$Radio_adstock <- as.vector(weekly_order_med_inv_data$Radio_adstock)
weekly_order_med_inv_data$Other_adstock <- as.vector(weekly_order_med_inv_data$Other_adstock)

weekly_order_med_inv_data$Holiday_week <- NULL


## Create a dataset only for Home audio , camera accessory and gaming accessories 
weekly_order_med_inv_data <- filter ( weekly_order_med_inv_data ,weekly_order_med_inv_data$product_analytic_sub_category %in% c("CameraAccessory", "GamingAccessory", "HomeAudio")) 

## Dummy variable creation for character data types 
weekly_order_med_inv_data_chr <- weekly_order_med_inv_data[,c(5,44)]
colnames(weekly_order_med_inv_data_chr)


weekly_order_med_inv_data_fact <- data.frame(sapply(weekly_order_med_inv_data_chr, function(x) factor(x)))
str(weekly_order_med_inv_data_fact)



unique(weekly_order_med_inv_data_fact$promotion_type)

colnames(weekly_order_med_inv_data_fact)

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(weekly_order_med_inv_data_fact, 
                            function(x) data.frame(model.matrix(~x-1,data =weekly_order_med_inv_data_fact))[,-1]))

## Create master data set by appending dummies with main data set 
weekly_order_med_inv_data_overall <- cbind(weekly_order_med_inv_data[,c(1:4,6:45,46)],dummies) 
#View(weekly_order_med_inv_data_overall) 

colnames(weekly_order_med_inv_data)


#Plotting box plots to know outliers

boxplot(weekly_order_med_inv_data_overall$tot_gmv , main = "GMV boxplot")

boxplot(weekly_order_med_inv_data_overall$tot_units , main = "Total units boxplot")

boxplot(weekly_order_med_inv_data_overall$tot_product_mrp , main = "Product MRP boxplot")

boxplot(weekly_order_med_inv_data_overall$TV, main = "TV spends boxplot")

boxplot(weekly_order_med_inv_data_overall$Digital , main = "Digital spends boxplot")

boxplot(weekly_order_med_inv_data_overall$Sponsorship , main = "Sponsorship boxplot")

boxplot(weekly_order_med_inv_data_overall$Content_Marketing , main = "Content Marketing boxplot")

boxplot(weekly_order_med_inv_data_overall$Online_Marketing, main = "Online marketing boxplot")

boxplot(weekly_order_med_inv_data_overall$Affiliates , main = "Affiliates boxplot")

boxplot(weekly_order_med_inv_data_overall$SEM , main = "SEM boxplot")

boxplot(weekly_order_med_inv_data_overall$Radio , main = "Radio Marketing boxplot")

boxplot(weekly_order_med_inv_data_overall$Other , main = "Other Marketing boxplot")


#Reducing or capping outliers
overall_quantile <- sapply(weekly_order_med_inv_data_overall[,c("tot_gmv","tot_units", "tot_product_mrp" , "TV" ,"Digital",
                                                                "Sponsorship", "Content_Marketing", "Online_Marketing" ,"Affiliates", "SEM" ,"Radio" , "Other" )], 
                           function(x) quantile(x,seq(0,1,.01),na.rm = T)) 


#Removing outliers of some columns
outliers <- function(x , lower_quantile, upper_quantile) {
  qnt <- quantile(x, probs=c(lower_quantile, upper_quantile), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  y <- x
  y[x < qnt[1]] <- qnt[1]
  y[x > qnt[2]] <- qnt[2]
  y
}

#rm(weekly_order_med_inv_data_overall)

weekly_order_med_inv_data_overall$tot_gmv <- outliers (weekly_order_med_inv_data_overall$tot_gmv,0, .97 ) 
weekly_order_med_inv_data_overall$tot_units <- outliers (weekly_order_med_inv_data_overall$tot_units,0, .97 ) 
weekly_order_med_inv_data_overall$tot_product_mrp <- outliers (weekly_order_med_inv_data_overall$tot_product_mrp,0, .97 ) 
weekly_order_med_inv_data_overall$TV <- outliers (weekly_order_med_inv_data_overall$TV,0, .98 ) 
weekly_order_med_inv_data_overall$Digital <- outliers (weekly_order_med_inv_data_overall$Digital,0, .95 ) 
weekly_order_med_inv_data_overall$Sponsorship <- outliers (weekly_order_med_inv_data_overall$Sponsorship,0, .95 ) 
weekly_order_med_inv_data_overall$Content_Marketing <- outliers (weekly_order_med_inv_data_overall$Content_Marketing,0, .95 ) 
weekly_order_med_inv_data_overall$SEM <- outliers (weekly_order_med_inv_data_overall$SEM,0, .95 ) 
weekly_order_med_inv_data_overall$Radio <- outliers (weekly_order_med_inv_data_overall$Radio,0, .95 ) 
weekly_order_med_inv_data_overall$Other <- outliers (weekly_order_med_inv_data_overall$Other,0, .95 )

weekly_order_med_inv_data_overall$TV_adstock <- outliers (weekly_order_med_inv_data_overall$TV_adstock,0, .95 )
weekly_order_med_inv_data_overall$Digital_adstock <- outliers(weekly_order_med_inv_data_overall$Digital_adstock,0, .95 )
weekly_order_med_inv_data_overall$Sponsorship_adstock <- outliers(weekly_order_med_inv_data_overall$Sponsorship_adstock,0, .95 )
weekly_order_med_inv_data_overall$Content_Marketing_adstock <- outliers(weekly_order_med_inv_data_overall$Content_Marketing_adstock,0, .95 )
weekly_order_med_inv_data_overall$Online_Marketing_adstock <- outliers(weekly_order_med_inv_data_overall$Online_Marketing_adstock,0, .95 )
weekly_order_med_inv_data_overall$Affiliates_adstock <- outliers(weekly_order_med_inv_data_overall$Affiliates_adstock,0, .95 )
weekly_order_med_inv_data_overall$SEM_adstock <- outliers(weekly_order_med_inv_data_overall$SEM_adstock,0, .95 )
weekly_order_med_inv_data_overall$Radio_adstock <- outliers(weekly_order_med_inv_data_overall$Radio_adstock,0, .95 )
weekly_order_med_inv_data_overall$Other_adstock <- outliers(weekly_order_med_inv_data_overall$Other_adstock,0, .95 )




## Find out  how many distinct values are there are for different columns
sapply(weekly_order_med_inv_data_overall, function(x) length(unique(x)))

weekly_order_med_inv_data_overall$total_row <- NULL
weekly_order_med_inv_data_overall$tot_week <- NULL

weekly_order_med_inv_data_overall$promotion_type <- NULL
weekly_order_med_inv_data_overall$Year_Month <- NULL

## Check the correlation among multiple varaiables to decide which varaiables are highly corelated with each other
## Column 4 has been excluded as it contains sub category 

#corr <- cor(weekly_order_med_inv_data_overall2[,-c (4)])

##Depending on higher corelation or since there vars are direct proxy to sales , so taking them out
weekly_order_med_inv_data_overall$avg_mrp <- NULL
weekly_order_med_inv_data_overall$avg_price <- NULL
weekly_order_med_inv_data_overall$tot_units <- NULL
weekly_order_med_inv_data_overall$no_of_orders <- NULL
weekly_order_med_inv_data_overall$tot_product_mrp <- NULL
weekly_order_med_inv_data_overall$avg_gmv <- NULL
weekly_order_med_inv_data_overall$value_per_visitor <- NULL
weekly_order_med_inv_data_overall$Year <- NULL
weekly_order_med_inv_data_overall$no_of_customer <- NULL
weekly_order_med_inv_data_overall$delayed_delivery_cnt <- NULL
weekly_order_med_inv_data_overall$early_delivery_cnt <- NULL
weekly_order_med_inv_data_overall$onetime_delivery_cnt <- NULL
weekly_order_med_inv_data_overall$cod_cnt <- NULL
weekly_order_med_inv_data_overall$prepaid_cnt <- NULL

# Take back up of master dataset weekly_order_med_inv_data_overall
weekly_order_med_inv_data_overall2 <- weekly_order_med_inv_data_overall

unique(weekly_order_med_inv_data_overall$product_analytic_sub_category)

# Create CameraAccessory for modelling 
CameraAccessory <- filter(weekly_order_med_inv_data_overall, weekly_order_med_inv_data_overall$product_analytic_sub_category=="CameraAccessory")
CameraAccessory$product_analytic_sub_category <- NULL
str(CameraAccessory)
colnames(HomeAudio)

# Create GamingAccessory for modelling 
GamingAccessory <- filter(weekly_order_med_inv_data_overall, weekly_order_med_inv_data_overall$product_analytic_sub_category=="GamingAccessory")
GamingAccessory$product_analytic_sub_category <- NULL
str(GamingAccessory)

# Create HomeAudiofor modelling 
HomeAudio <- filter(weekly_order_med_inv_data_overall, weekly_order_med_inv_data_overall$product_analytic_sub_category=="HomeAudio")
HomeAudio$product_analytic_sub_category <- NULL
str(HomeAudio)

####################################################################################################################################################################################################################################################################################################################################

#This function creates lag variables which will be used in lag based models
#So we will execute this function when we need lag variables in our dataframe
#This function is called when making of Lag based models (Distributed Lag Model & Lag + Multiplicative Model)

lag_var_creation <- function(x,y){
  
  weekly_order_med_inv_data_lag <- x %>% 
    arrange ( week_no) %>% 
    group_by(product_analytic_sub_category ,product_analytic_vertical) %>%
    mutate(gmv_lag_1 = lag(tot_gmv, 1))  %>%
    mutate(gmv_lag_2 = lag(tot_gmv, 2)) %>% 
    mutate(gmv_lag_3 = lag(tot_gmv, 3)) 
  # add change in sales  last 3 weeks
  
  weekly_order_med_inv_data_lag <- weekly_order_med_inv_data_lag %>% 
    arrange ( week_no) %>%
    group_by(product_analytic_sub_category ,product_analytic_vertical ) %>% 
    mutate(gmv_change_from_w1 = (tot_gmv-lag(tot_gmv, 1))/tot_gmv) %>% 
    mutate(gmv_change_from_w2 = (tot_gmv-lag(tot_gmv, 2))/tot_gmv) %>%
    mutate(gmv_change_from_w3 = (tot_gmv-lag(tot_gmv, 3))/tot_gmv) 
  
  # add list price for   last 3 weeks
  weekly_order_med_inv_data_lag <- weekly_order_med_inv_data_lag %>% 
    arrange ( week_no) %>% 
    group_by(product_analytic_sub_category,product_analytic_vertical ) %>%
    mutate(list_price_lag_1 = lag(list_price, 1))  %>% 
    mutate(list_price_lag_2 = lag(list_price, 2)) %>% 
    mutate(list_price_lag_3 = lag(list_price, 3)) 
  
  #### add list price change  for   last 3 weeks
  weekly_order_med_inv_data_lag <- weekly_order_med_inv_data_lag %>%
    arrange ( week_no) %>%
    group_by(product_analytic_sub_category,product_analytic_vertical ) %>%
    mutate(price_change_from_w1 = (list_price-lag(list_price, 1))/list_price) %>% 
    mutate(list_price_change_from_w2 = (list_price-lag(list_price, 2))/list_price) %>%
    mutate(list_price_change_from_w3 = (list_price-lag(list_price, 3))/list_price)
  
 
  ## Filter out CameraAccessory", GamingAccessory and  HomeAudio sub category
  weekly_order_med_inv_data_lag <- weekly_order_med_inv_data_lag %>%
    filter ( product_analytic_sub_category %in% c("CameraAccessory", "GamingAccessory", "HomeAudio"))
  
  weekly_order_med_inv_data_lag <- as.data.frame(weekly_order_med_inv_data_lag)
  
  ## Since these dataframe has been created using weekly_order_ad_data, so we have character variables in  this dataset
  ## Convert char vars in factorial
  weekly_order_med_inv_data_lag_chr <- weekly_order_med_inv_data_lag[,c(5,44)]
  weekly_order_med_inv_data_lag_chr_fact <- data.frame(sapply(weekly_order_med_inv_data_lag_chr, function(x) factor(x)))
  str(weekly_order_med_inv_data_lag_chr_fact)
  
  # creating dummy variables for factor attributes
  dummies1<- data.frame(sapply(weekly_order_med_inv_data_lag_chr_fact, 
                               function(x) data.frame(model.matrix(~x-1,data =weekly_order_med_inv_data_lag_chr_fact))[,-1]))
  
  
  ## Combine dummies with other varaiables to create master data set 
  weekly_order_med_inv_data_lag_overall <- cbind ( weekly_order_med_inv_data_lag[, -c(5,34)], dummies1 ) 
  weekly_order_med_inv_data_lag_overall$promotion_type <- NULL #Removing promotion type since we have made its dummy variables
  
  ## Since these variables are highly corelated  or direct proxy to sales , so taking them out 
  weekly_order_med_inv_data_lag_overall$tot_week <- NULL 
  weekly_order_med_inv_data_lag_overall$total_row <- NULL
  weekly_order_med_inv_data_lag_overall$avg_mrp <- NULL
  weekly_order_med_inv_data_lag_overall$avg_price <- NULL
  weekly_order_med_inv_data_lag_overall$tot_units <- NULL
  weekly_order_med_inv_data_lag_overall$no_of_orders <- NULL
  weekly_order_med_inv_data_lag_overall$tot_product_mrp <- NULL
  weekly_order_med_inv_data_lag_overall$avg_gmv <- NULL
  weekly_order_med_inv_data_lag_overall$value_per_visitor <- NULL
  weekly_order_med_inv_data_lag_overall$Year <- NULL
  weekly_order_med_inv_data_lag_overall$no_of_customer <- NULL
  weekly_order_med_inv_data_lag_overall$delayed_delivery_cnt <- NULL
  weekly_order_med_inv_data_lag_overall$early_delivery_cnt <- NULL
  weekly_order_med_inv_data_lag_overall$onetime_delivery_cnt <- NULL
  weekly_order_med_inv_data_lag_overall$cod_cnt <- NULL
  weekly_order_med_inv_data_lag_overall$prepaid_cnt <- NULL
  weekly_order_med_inv_data_lag_overall$Year_Month <- NULL
  weekly_order_med_inv_data_lag_overall$week_no <- NULL
  weekly_order_med_inv_data_lag_overall$Month <- NULL
  
  ##weekly_order_med_inv_data_lag_overall [,c(1,2,4:36)] <- scale(weekly_order_med_inv_data_lag_overall [,c(1,2,4:36)])
  
  # Create sub data set for each sub category
  result <- filter ( weekly_order_med_inv_data_lag_overall ,weekly_order_med_inv_data_lag_overall$product_analytic_sub_category == y) 
  result$product_analytic_sub_category <- NULL
  
  #Removing NA rows
  result <- na.omit(result)
  
  #scaling numerical variables
  #result [,c(1:35)] <- scale(result [,c(1:35)])
  
  return(result)
  
}




#Check file Camera_Accesory.R for Camera Accesory Models
#Check file Game_Accesory.R for Gaming Accesory Models
#Check file Home Audio.R for Home Audio Models
