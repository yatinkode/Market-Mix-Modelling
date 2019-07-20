########Home accessory###########

linear_home <- HomeAudio
linear_home$week_no <- NULL
linear_home$Month <- NULL
linear_home$Year_Month <- NULL

colnames(linear_home)

#Scaling the dataset
linear_home[,1:24] <- data.frame(scale(linear_home[,1:24], center = TRUE))

set.seed(100)
trainindices= sample(1:nrow(linear_home), 0.7*nrow(linear_home))
#Generate the train data set
train = linear_home[trainindices,]
#Similarly store the rest of the observations into an object "test".
test = linear_home[-trainindices,]

home_model1 <- lm(tot_gmv~. , data = train)
summary(home_model1)

#Adjusted R-squared:  0.9343


step_home_model1 <- stepAIC(home_model1, direction = "both")
summary(step_home_model1)
vif(step_home_model1)

#Adjusted R-squared:  0.9423

# Remove Content marketing
step_home_model2 <- lm(formula = tot_gmv ~ Total_Investment+TV+Digital+Sponsorship
                       +Content_Marketing+Online_Marketing+Affiliates+Other+Sponsorship_adstock+
                         product_analytic_vertical.xFMRadio+product_analytic_vertical.xHiFiSystem
                       +product_analytic_vertical.xHomeAudioSpeaker+promotion_type.xDaussera.sale
                       +product_analytic_vertical.xVoiceRecorder+product_analytic_vertical.xBoomBox, data = train )
summary(step_home_model2) 
# Adjusted R-squared:  0.9415 
vif(step_home_model2)


# Remove sponsership promotion_type.xDaussera.sale

step_home_model3 <- lm (formula=tot_gmv ~ Total_Investment+TV+Digital+Sponsorship
                        +Content_Marketing+
                          Online_Marketing+Affiliates+
                          Other+
                          product_analytic_vertical.xFMRadio+product_analytic_vertical.xHiFiSystem
                        +product_analytic_vertical.xHomeAudioSpeaker+Sponsorship_adstock
                        +product_analytic_vertical.xVoiceRecorder+product_analytic_vertical.xBoomBox,
                       data = train )

summary(step_home_model3)

#Adjusted R-squared:  0.9407 

# step_home_model3_dummy <- lm (formula=tot_gmv ~ Total_Investment+TV+Digital+Sponsorship
#                         +Content_Marketing+
#                           Online_Marketing+Affiliates+
#                           Other+
#                           product_analytic_vertical.xFMRadio+product_analytic_vertical.xHiFiSystem
#                         +product_analytic_vertical.xHomeAudioSpeaker
#                         +product_analytic_vertical.xVoiceRecorder+product_analytic_vertical.xBoomBox,
#                         data = train )
# 
# summary(step_home_model3_dummy)

vif(step_home_model3)

#Remove product_analytic_vertical.xHiFiSystem, sponsership adstock

step_home_model4 <- lm (formula=tot_gmv ~ Total_Investment+TV+Digital+Sponsorship
                        +Content_Marketing+
                          Online_Marketing+Affiliates+
                          Other+Sponsorship_adstock+
                          product_analytic_vertical.xFMRadio
                        +product_analytic_vertical.xHomeAudioSpeaker
                        +product_analytic_vertical.xVoiceRecorder+product_analytic_vertical.xBoomBox,
                        data = train )
summary(step_home_model4)
# Adjusted R-squared:  0.9399 
vif(step_home_model4)

# step_home_model4 <- lm (formula=tot_gmv ~ Total_Investment+Sponsorship
#                         +Content_Marketing+Online_Marketing+Affiliates+
#                           product_analytic_vertical.xFMRadio+product_analytic_vertical.xHomeAudioSpeaker+promotion_type.xDaussera.sale
#                         +product_analytic_vertical.xVoiceRecorder+product_analytic_vertical.xBoomBox,data = train)
# summary(step_home_model4)
# vif(step_home_model4)



## remove  sponsership adstock  

step_home_model5 <- lm (formula=tot_gmv ~ Total_Investment+TV+Digital+Sponsorship
                        +Content_Marketing+
                          Online_Marketing+Affiliates+
                          Other+
                          product_analytic_vertical.xFMRadio
                        +product_analytic_vertical.xHomeAudioSpeaker
                        +product_analytic_vertical.xVoiceRecorder+product_analytic_vertical.xBoomBox,
                        data = train)
summary(step_home_model5)
vif(step_home_model5)

# Adjusted R-squared:  0.8814 

## remove Digital

step_home_model6 <- lm (formula=tot_gmv ~ Total_Investment+TV+Sponsorship
                        +Content_Marketing+
                          Online_Marketing+Affiliates+
                          Other+
                          product_analytic_vertical.xFMRadio
                        +product_analytic_vertical.xHomeAudioSpeaker
                        +product_analytic_vertical.xVoiceRecorder+product_analytic_vertical.xBoomBox,
                        data = train)
summary(step_home_model6)
vif(step_home_model6)
#Adjusted R-squared:  0.8818
#removed others
step_home_model7 <- lm (formula=tot_gmv ~ Total_Investment+TV+Sponsorship
                        +Content_Marketing+
                          Online_Marketing+Affiliates+
                          
                          product_analytic_vertical.xFMRadio
                        +product_analytic_vertical.xHomeAudioSpeaker
                        +product_analytic_vertical.xVoiceRecorder+product_analytic_vertical.xBoomBox,
                        data = train)
summary(step_home_model7)
vif(step_home_model7)

#Adjusted R-squared:  0.8813 

# remove TV
step_home_model8 <- lm (formula=tot_gmv ~ Total_Investment+Sponsorship
                        +Content_Marketing+
                          Online_Marketing+Affiliates+
                          
                          product_analytic_vertical.xFMRadio
                        +product_analytic_vertical.xHomeAudioSpeaker
                        +product_analytic_vertical.xVoiceRecorder+product_analytic_vertical.xBoomBox,
                        data = train)
summary(step_home_model8)
vif(step_home_model8)
 # Adjusted R-squared:  0.8817 

#Removed boombox
step_home_model9 <- lm (formula=tot_gmv ~ Total_Investment+Sponsorship
                        +Content_Marketing+
                          Online_Marketing+Affiliates+
                          
                          product_analytic_vertical.xFMRadio
                        +product_analytic_vertical.xHomeAudioSpeaker
                        +product_analytic_vertical.xVoiceRecorder,
                        data = train)
summary(step_home_model9)
vif(step_home_model9)
#Adjusted R-squared:  0.8798 

#removed Affiliates
step_home_model10 <- lm (formula=tot_gmv ~ Total_Investment+Sponsorship
                        +Content_Marketing+
                          Online_Marketing+
                          
                          product_analytic_vertical.xFMRadio
                        +product_analytic_vertical.xHomeAudioSpeaker
                        +product_analytic_vertical.xVoiceRecorder,
                        data = train)
summary(step_home_model10)
vif(step_home_model10)
#Adjusted R-squared:  0.8725

# Removed Total_Investment
step_home_model11 <- lm (formula=tot_gmv ~ Sponsorship
                         +Content_Marketing+
                           Online_Marketing+
                           
                           product_analytic_vertical.xFMRadio
                         +product_analytic_vertical.xHomeAudioSpeaker
                         +product_analytic_vertical.xVoiceRecorder,
                         data = train)
summary(step_home_model11)
vif(step_home_model11)
#Adjusted R-squared:  0.8567 
#removed Online_Marketing
step_home_model12 <- lm (formula=tot_gmv ~ Sponsorship
                         +Content_Marketing+
                           
                           product_analytic_vertical.xFMRadio
                         +product_analytic_vertical.xHomeAudioSpeaker
                         +product_analytic_vertical.xVoiceRecorder,
                         data = train)
summary(step_home_model12)
vif(step_home_model12)

summary(step_home_model12)
#Adjusted R-squared:  0.8662  

## R2 in test data
gmv_home_prediction <- predict(step_home_model12, test)
test$predicted_gmv <- gmv_home_prediction

home_r <- cor(test$tot_gmv, test$predicted_gmv)
home_rsquared <- home_r^2
home_rsquared
#R squared on test 0.767409

######################################################################
###############Elasticity Chart####################################
home_final_model <- step_home_model12

home_elasticity <- function(var) {
  x <- as.numeric(home_final_model$coefficients[var])
  return(x)
}

var_list_home <- list()

for(i in 2:length(home_final_model$coefficients)) {
  var_list_home[i-1] <- home_elasticity(names(home_final_model$coefficients)[i])
}

elasticity.outputs.home <- data.frame(names(home_final_model$coefficients[2:length(home_final_model$coefficients)]))
elasticity.outputs.home <- cbind(elasticity.outputs.home,do.call(rbind.data.frame, var_list_home))
colnames(elasticity.outputs.home) <- c("Variable","Elasticity")

elasticity.outputs.home$direction <- ifelse(elasticity.outputs.home$Elasticity > 0, "Positive", "Negative")

ggplot(data=elasticity.outputs.home, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Home Audio - Linear Model") +xlab("Variables")

########################################Cross validation####################

home_lm_cv <- cv.lm(data = train, form.lm = step_home_model6,m =10)
# ms = 0.145

##################################Multiplicative model############################
#
HomeAudio_Multi<- as.data.frame(log1p(HomeAudio))
HomeAudio_Multi$week_no <- NULL
HomeAudio_Multi$Month <- NULL
HomeAudio_Multi$Year_Month <- NULL

summary(HomeAudio_Multi)
set.seed(100)
indices= sample(1:nrow(HomeAudio_Multi), 0.7*nrow(HomeAudio_Multi))

train_homeAudio <- HomeAudio_Multi[indices,]
test_homeAudio <- HomeAudio_Multi[-indices,]

multi_home_model_1 <-lm(tot_gmv~.,data=train_homeAudio)

summary(multi_home_model_1)

##Stepwise variable deduction
step_home_multi_model_2 <- stepAIC(multi_home_model_1, direction="both")

summary(step_home_multi_model_2)
vif(step_home_multi_model_2)

# Adjusted R-squared:  0.8318 

#Removing variables Online_Marketing, Online_Marketing_adstock, Radio, Affiliates, Total_Investment, Content_Marketing_adstock, 
#Content_Marketing, SEM_adstock, promotion_type.xNo_promotion, TV, Digital, Sponsorship, product_analytic_vertical.xDock, Radio_adstock one by one

step_home_multi_model_3  <- lm(formula = tot_gmv ~ list_price + week + 
                                 SEM + Other + 
                                 Affiliates_adstock + 
                                 NPS + discount_over_mrp + product_analytic_vertical.xDJController + 
                                 product_analytic_vertical.xDockingStation + 
                                 product_analytic_vertical.xFMRadio + product_analytic_vertical.xHiFiSystem + 
                                 product_analytic_vertical.xHomeAudioSpeaker + product_analytic_vertical.xKaraokePlayer + 
                                 product_analytic_vertical.xSlingBox + product_analytic_vertical.xSoundMixer + 
                                 promotion_type.xBig.Diwali.Sale + promotion_type.xBSD.5 + 
                                 promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                                 promotion_type.xEid...Rathayatra.sale + 
                                 promotion_type.xPacman + promotion_type.xRepublic.Day, data = train_homeAudio)

summary(step_home_multi_model_3)
vif(step_home_multi_model_3)

#Removing Affiliates_adstock, SEM, NPS, promotion_type.xBig.Diwali.Sale, promotion_type.xChristmas...New.Year.Sale, promotion_type.xEid...Rathayatra.sale
#, promotion_type.xBSD.5, promotion_type.xDaussera.sale , promotion_type.xPacman, promotion_type.xRepublic.Day one by one
step_home_multi_model_4  <- lm(formula = tot_gmv ~ list_price + week + Other + 
                                 discount_over_mrp + product_analytic_vertical.xDJController + 
                                 product_analytic_vertical.xDockingStation + 
                                 product_analytic_vertical.xFMRadio + product_analytic_vertical.xHiFiSystem + 
                                 product_analytic_vertical.xHomeAudioSpeaker + product_analytic_vertical.xKaraokePlayer + 
                                 product_analytic_vertical.xSlingBox + product_analytic_vertical.xSoundMixer
                                  , data = train_homeAudio)

summary(step_home_multi_model_4)
vif(step_home_multi_model_4)

#Adjusted R-squared:  0.795 


#Testing

predict_home_mul <- predict(step_home_multi_model_4, test_homeAudio)
test_homeAudio$predicted_gmv <- predict_home_mul

# Now, we need to test the r square between actual and predicted sales.
home_multi_r <- cor(test_homeAudio$tot_gmv, test_homeAudio$predicted_gmv)
home_multi_r <- cor(test_homeAudio$tot_gmv, test_homeAudio$predicted_gmv)^2
home_multi_r
#Rsquare on Test Data : 0.640613 


##########################Cross Validation######################################
multi_home_lm_cv <- cv.lm(data = train_homeAudio, form.lm = step_home_multi_model_4,m =10)
#ms = 0.80

##############################Elasticity Chart######################################

multi_home_final_model <- step_home_multi_model_4

multi_home_elasticity <- function(var) {
  x <- as.numeric(multi_home_final_model$coefficients[var])
  return(x)
}

multi_var_list_home <- list()

for(i in 2:length(multi_home_final_model$coefficients)) {
  multi_var_list_home[i-1] <- multi_home_elasticity(names(multi_home_final_model$coefficients)[i])
}

multi_elasticity.outputs.home <- data.frame(names(multi_home_final_model$coefficients[2:length(multi_home_final_model$coefficients)]))
multi_elasticity.outputs.home <- cbind(multi_elasticity.outputs.home,do.call(rbind.data.frame, multi_var_list_home))
colnames(multi_elasticity.outputs.home) <- c("Variable","Elasticity")

multi_elasticity.outputs.home$direction <- ifelse(multi_elasticity.outputs.home$Elasticity > 0, "Positive", "Negative")

ggplot(data=multi_elasticity.outputs.home, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Home Audio - Multiplicative Model") +xlab("Variables")



########################################################################## Distributed Lag Model ##################################

#Create lag variables in the dataset needed for distributed lag model
weekly_order_med_inv_data_lag_home <- lag_var_creation(weekly_order_med_inv_data,"HomeAudio")
weekly_order_med_inv_data_lag_home [,c(1:35)] <- scale(weekly_order_med_inv_data_lag_home [,c(1:35)])

#Dividing into Train and Test data
set.seed(200)

trainindices= sample(1:nrow(weekly_order_med_inv_data_lag_home), 0.7*nrow(weekly_order_med_inv_data_lag_home))
#Generate the train data set
train_home = weekly_order_med_inv_data_lag_home[trainindices,]

#Similarly store the rest of the observations into an object "test".
test_home = weekly_order_med_inv_data_lag_home[-trainindices,]


home_model1 <- lm(tot_gmv~. , data = train_home)
summary(home_model1)


step_home_model1 <- stepAIC(home_model1, direction = "both")
summary(step_home_model1)
vif(step_home_model1)

step_home_model2 <- lm(formula = tot_gmv ~ Total_Investment + Digital + Sponsorship + 
                         Online_Marketing + SEM + Other + Digital_adstock + Content_Marketing_adstock + 
                         Online_Marketing_adstock + SEM_adstock + discount_over_mrp + 
                         gmv_lag_1 + gmv_lag_3 + gmv_change_from_w1 + product_analytic_vertical.xBoomBox + 
                         product_analytic_vertical.xDock + product_analytic_vertical.xFMRadio + 
                         product_analytic_vertical.xHomeAudioSpeaker + promotion_type.xChristmas...New.Year.Sale + 
                         promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                         promotion_type.xValentine.s.Day, data = train_home)


summary(step_home_model2)
vif(step_home_model2)


#Remove Total_Investment, SEM_adstock, SEM, Content_Marketing_adstock, Online_Marketing, Digital, discount_over_mrp, gmv_lag_1, 
#Online_Marketing_adstock, promotion_type.xValentine.s.Day, promotion_type.xNo_promotion, Sponsorship one by one
step_home_model3 <- lm(formula = tot_gmv ~ Other + Digital_adstock +  
                         gmv_lag_3 + gmv_change_from_w1 + product_analytic_vertical.xBoomBox + 
                         product_analytic_vertical.xDock + product_analytic_vertical.xFMRadio + 
                         product_analytic_vertical.xHomeAudioSpeaker + promotion_type.xChristmas...New.Year.Sale + 
                         promotion_type.xPacman + promotion_type.xRepublic.Day, data = train_home)


summary(step_home_model3)
vif(step_home_model3)

#Remove product_analytic_vertical.xBoomBox, Digital_adstock, product_analytic_vertical.xFMRadio, 
#product_analytic_vertical.xDock, promotion_type.xChristmas...New.Year.Sale, promotion_type.xPacman, promotion_type.xRepublic.Day , Other one by one
step_home_model4 <- lm(formula = tot_gmv ~ gmv_lag_3 + gmv_change_from_w1 +  
                         product_analytic_vertical.xHomeAudioSpeaker, data = train_home)


summary(step_home_model4)
vif(step_home_model4)

#Adjusted R-squared:  0.781


#Testing

gmv_home_prediction <- predict(step_home_model4, test_home)
test_home$predicted_gmv <- gmv_home_prediction

home_r <- cor(test_home$tot_gmv, test_home$predicted_gmv)
home_rsquared <- home_r^2
home_rsquared   
#Rsquare on test data = 0.725

############################################################################### Estimating elasticity ##################################

home_final_model <- step_home_model4

elasticity <- function(var) {
  x <- as.numeric(home_final_model$coefficients[var] )
  return(x)
}

var_list <- list()

for(i in 2:length(home_final_model$coefficients)) {
  var_list[i-1] <- elasticity(names(home_final_model$coefficients)[i])
}

elasticity.outputs <- data.frame(names(home_final_model$coefficients[2:length(home_final_model$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")
elasticity.outputs <- arrange( elasticity.outputs , -Elasticity)

ggplot(data=elasticity.outputs[, ], aes(x=reorder(Variable,Elasticity),y=Elasticity,label = round(Elasticity,2))) +
  geom_bar(position="dodge",stat="identity", fill="grey") + 
  coord_flip() +
  ggtitle("Home Audio -  Distributed Lag  Model") +xlab("Variables")+
  geom_text(size = 3, position = position_stack(vjust = 0.5))

########################################################################## Cross validation ######################################

home_lm_cv <- cv.lm(data = weekly_order_med_inv_data_lag_home , form.lm = step_home_model4, m =10)
#ms 1.17e+12

###################################################################################################################################


################################################################################# Koyck Model #####################################

str(HomeAudio)
sum(is.na(HomeAudio))
HomeAudioKoyck <- HomeAudio
library(DataCombine)
# Creating Lag variable 
HomeAudioKoyck <- slide(HomeAudioKoyck, Var = "tot_gmv",slideBy = -1)
str(HomeAudioKoyck)
colnames(HomeAudioKoyck)
HomeA_koyck <- na.omit(HomeAudioKoyck)
HomeA_koyck1 <- data.frame(scale(HomeA_koyck[,c(1:26)]))
HomeA_koyck2 <- data.frame(HomeA_koyck[,27:87])
HomeA_koyck3 <- data.frame(scale(HomeA_koyck[,88]))

HomeA_koyck <- cbind(HomeA_koyck1, HomeA_koyck2, HomeA_koyck3)
str(HomeA_koyck)
# removing month and year columns
HomeA_koyck <- HomeA_koyck[,-c(1,2)]
# splitting train & test sets
trainindices= sample(1:nrow(HomeA_koyck), 0.6*nrow(HomeA_koyck))
train_h = HomeA_koyck[trainindices,]
test_h = HomeA_koyck[-trainindices,]
str(train_h)
names(train_h)
## building first overall model
Koyck_model1_home <- lm(tot_gmv~.,train_h)
summary(Koyck_model1_home)

# Evaluating the first models for significant predictors
Koyck_model2_home <- stepAIC(Koyck_model1_home,direction = "both")
summary(Koyck_model2_home)
vif(Koyck_model2_home)

# removing Online.marketing  as vif is very high
Koyck_model3_home<- lm(formula = tot_gmv ~ Total_Investment + Digital + Sponsorship + 
                         Content_Marketing + Online_Marketing + Affiliates + SEM + 
                         Other + Online_Marketing_adstock + Affiliates_adstock + SEM_adstock + 
                         Radio_adstock + NPS + discount_over_mrp + product_analytic_vertical.xDJController + 
                         product_analytic_vertical.xDock + product_analytic_vertical.xDockingStation + 
                         product_analytic_vertical.xFMRadio + product_analytic_vertical.xHiFiSystem + 
                         product_analytic_vertical.xHomeAudioSpeaker + scale.HomeA_koyck...88.., 
                       data = train_h)

summary(Koyck_model3_home)
vif(Koyck_model3_home)

# removing variables Online_Marketing, Total_Investment, Online_Marketing_adstock, SEM, Digital,
#Radio_adstock, Content_Marketing, Affiliates, SEM_adstock, discount_over_mrp, Affiliates_adstock, Other,
#product_analytic_vertical.xDJController, product_analytic_vertical.xDock, product_analytic_vertical.xHiFiSystem,
#product_analytic_vertical.xDockingStation, NPS, scale.HomeA_koyck...88.., Sponsorship one by one
Koyck_model4_home <-  lm(formula = tot_gmv ~   
                           product_analytic_vertical.xFMRadio +
                           product_analytic_vertical.xHomeAudioSpeaker, 
                         data = train_h)

summary(Koyck_model4_home)
vif(Koyck_model4_home)

#Adjusted R-squared:  0.891

final_koyck_home <-  Koyck_model4_home

par(mfrow = c(2,2))
plot(final_koyck_home, main = "Final Home Audio - Koyck Model")



# Testing
pred_koyck_home <- predict(final_koyck_home, test_h)
home_r_koyck <- cor(test_h$tot_gmv, pred_koyck_home)
home_rsquared <- home_r_koyck^2
home_rsquared
#Rsquared on test: 0.708

############################################################################### Elasticity Measure ###################################

home_final_model <- final_koyck_home

elasticity <- function(var) {
  x <- as.numeric(home_final_model$coefficients[var] * mean(train_h[,var])/mean(train_h$tot_gmv))
  return(x)
}

var_list <- list()

for(i in 2:length(home_final_model$coefficients)) {
  var_list[i-1] <- elasticity(names(home_final_model$coefficients)[i])
}

elasticity.outputs <- data.frame(names(home_final_model$coefficients[2:length(home_final_model$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")

ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity,label = round(Elasticity,2))) +
  geom_bar(position="dodge",stat="identity", fill="grey") + 
  coord_flip() +
  ggtitle("Home Audio - Koyck Model") +xlab("Variables")+
  geom_text(size = 3, position = position_stack(vjust = 0.5))


############################################################################### Cross validation ###################################

crossval <- cv.lm(data = train_h, form.lm = formula(final_koyck_home),m = 10, 
                  main = "Cross Validation Home Audio Model")

#ms: 0.133

###################################################################################################################################

############################################################### Lag Plus Multiplicative model #####################################

#Create lag variables in the dataset needed for Lag Plus Multiplicative model
weekly_data_lag_plus_mul_home <- lag_var_creation(weekly_order_med_inv_data,"HomeAudio")



weekly_data_lag_plus_mul_home_tmp  <- lapply (weekly_data_lag_plus_mul_home[,1:35] ,function(x) ifelse ( x==0 , 0, log(abs(x)))*sign(x) )
weekly_data_lag_plus_mul_home <- data.frame( cbind( weekly_data_lag_plus_mul_home_tmp ,weekly_data_lag_plus_mul_home[,36:96] ))

colnames(weekly_data_lag_plus_mul_home)

weekly_data_lag_plus_mul_home [,c(1:35)] <- scale(weekly_data_lag_plus_mul_home [,c(1:35)])

set.seed(200)

trainindices= sample(1:nrow(weekly_data_lag_plus_mul_home), 0.7*nrow(weekly_data_lag_plus_mul_home))
#Generate the train data set
train_lagmul_home = weekly_data_lag_plus_mul_home[trainindices,]

#Similarly store the rest of the observations into an object "test".
test_lagmul_home = weekly_data_lag_plus_mul_home[-trainindices,]



mul_lag_home_model1 <- lm(tot_gmv~. , data = train_lagmul_home)
summary(mul_lag_home_model1)


step_lag_home_model1 <- stepAIC(mul_lag_home_model1, direction = "both")
summary(step_lag_home_model1)
vif (step_lag_home_model1 )

#Remove variables Total_Investment, list_price_lag_3, Digital, Digital_adstock, list_price_lag_2, SEM_adstock,gmv_lag_3, Other_adstock, 
#promotion_type.xValentine.s.Day,SEM,Content_Marketing, promotion_type.xNo_promotion, gmv_change_from_w1, promotion_type.xRepublic.Day, promotion_type.xBSD.5, NPS one by one
mul_lag_home_model2 <- lm(formula = tot_gmv ~ list_price +  TV + Radio + Affiliates_adstock + discount_over_mrp + 
                            gmv_change_from_w2 + gmv_change_from_w3 + product_analytic_vertical.xDJController + 
                            product_analytic_vertical.xDock + product_analytic_vertical.xDockingStation + 
                            product_analytic_vertical.xFMRadio + product_analytic_vertical.xHiFiSystem + 
                            product_analytic_vertical.xHomeAudioSpeaker + product_analytic_vertical.xSoundMixer , data = train_lagmul_home)

summary(mul_lag_home_model2)
vif(mul_lag_home_model2)



#Adjusted R-squared:  0.841


home_cam_prediction <- predict(mul_lag_home_model2, test_lagmul_home)
test_lagmul_home$predicted_gmv <- home_cam_prediction

home_r <- cor(test_lagmul_home$tot_gmv, test_lagmul_home$predicted_gmv  )
home_rsquared <- home_r^2
home_rsquared   
# Test data rsquare =  0.842


############################################################################### Estimating elasticity ##################################

home_final_model <- mul_lag_home_model2

elasticity <- function(var) {
  x <- as.numeric(home_final_model$coefficients[var] )
  return(x)
}

var_list <- list()

for(i in 2:length(home_final_model$coefficients)) {
  var_list[i-1] <- elasticity(names(home_final_model$coefficients)[i])
}

elasticity.outputs <- data.frame(names(home_final_model$coefficients[2:length(home_final_model$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")
elasticity.outputs <- arrange( elasticity.outputs , -Elasticity)

ggplot(data=elasticity.outputs[, ], aes(x=reorder(Variable,Elasticity),y=Elasticity,label = round(Elasticity,2))) +
  geom_bar(position="dodge",stat="identity", fill="grey") + 
  coord_flip() +
  ggtitle("Home  Audio -  Lag Distributed Multipicative  Model") +xlab("Parameters")+
  geom_text(size = 3, position = position_stack(vjust = 0.5))


########################################################################## Cross validation ######################################

game_lm_cv <- cv.lm(data = weekly_data_lag_plus_mul_home , form.lm = home_final_model, m =10)

##ms - 0.168

###################################################################################################################################

###################################################
#Home Audio     				        #	Train	#	Test    #
###################################################
#Linear Model					          #	86.6 	#	76.7    #
#Multiplicative model			      #	79.5  #	64.0    #
#Distributed Lag Model          # 78.1  # 72.1    #
#koyck Model					          #	89.1  #	70.8    #
#Lag plus Multiplicative model	#	84.1	#	84.2    #
#                               #       #         #
###################################################





