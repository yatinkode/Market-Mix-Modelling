###################################################################################################################################
# Game Accesory Models
###################################################################################################################################


###################################################################### Linear Model ###############################################
linear_game <- GamingAccessory
linear_game$week_no <- NULL
linear_game$Month <- NULL
linear_game$Year_Month <- NULL

colnames(linear_game)
linear_game[,1:24] <- data.frame(scale(linear_game[,1:24], center = TRUE))
set.seed(100)

trainindices = sample(1:nrow(linear_game), 0.7*nrow(linear_game))
#Generate the train data set
train_game = linear_game[trainindices,]
#Similarly store the rest of the observations into an object "test".
test_game = linear_game[-trainindices,]

game_model1 <- lm(tot_gmv~. , data = train_game)
summary(game_model1)

step_game_model1 <- stepAIC(game_model1, direction = "both")
summary(step_game_model1)
vif(step_game_model1)

#Removing memory card, product_analytic_vertical.xGamingMousePad, 
#SEM, discount_over_mrp
step_game_model2 <- lm(formula = tot_gmv ~ Total_Investment + 
                         TV + 
                         Sponsorship + 
                         Content_Marketing + 
                         Affiliates + 
                         Radio + 
                         Other + 
                         TV_adstock + 
                         Content_Marketing_adstock + 
                         Online_Marketing_adstock + 
                         Radio_adstock + 
                         Other_adstock + 
                         product_analytic_vertical.xGamePad + 
                         product_analytic_vertical.xGamingAccessoryKit + 
                         product_analytic_vertical.xGamingHeadset + 
                         product_analytic_vertical.xGamingKeyboard + 
                         product_analytic_vertical.xGamingMouse + 
                         product_analytic_vertical.xJoystickGamingWheel + 
                         product_analytic_vertical.xMotionController + 
                         promotion_type.xChristmas...New.Year.Sale + 
                         promotion_type.xDaussera.sale + 
                         promotion_type.xNo_promotion + 
                         promotion_type.xRakshabandhan.Sale, 
                       data = train_game)
summary(step_game_model2)
vif(step_game_model2)

#Removed: product_analytic_vertical.xMotionController 
step_game_model3 <- lm(formula = tot_gmv ~ Total_Investment + 
                         TV + 
                         Sponsorship + 
                         Content_Marketing + 
                         Affiliates + 
                         Radio + 
                         Other + 
                         TV_adstock + 
                         Content_Marketing_adstock + 
                         Online_Marketing_adstock + 
                         Radio_adstock + 
                         Other_adstock + 
                         product_analytic_vertical.xGamePad + 
                         product_analytic_vertical.xGamingAccessoryKit + 
                         product_analytic_vertical.xGamingHeadset + 
                         product_analytic_vertical.xGamingKeyboard + 
                         product_analytic_vertical.xGamingMouse + 
                         product_analytic_vertical.xJoystickGamingWheel + 
                         promotion_type.xChristmas...New.Year.Sale + 
                         promotion_type.xDaussera.sale + 
                         promotion_type.xNo_promotion + 
                         promotion_type.xRakshabandhan.Sale, 
                       data = train_game)
summary(step_game_model3)  #Adjusted R-squared:  0.7498
vif(step_game_model3)

#Removed: product_analytic_vertical.xGamingAccessoryKit
step_game_model4 <- lm(formula = tot_gmv ~ Total_Investment + 
                         TV + 
                         Sponsorship + 
                         Content_Marketing + 
                         Affiliates + 
                         Radio + 
                         Other + 
                         TV_adstock + 
                         Content_Marketing_adstock + 
                         Online_Marketing_adstock + 
                         Radio_adstock + 
                         Other_adstock + 
                         product_analytic_vertical.xGamePad + 
                         product_analytic_vertical.xGamingHeadset + 
                         product_analytic_vertical.xGamingKeyboard + 
                         product_analytic_vertical.xGamingMouse + 
                         product_analytic_vertical.xJoystickGamingWheel + 
                         promotion_type.xChristmas...New.Year.Sale + 
                         promotion_type.xDaussera.sale + 
                         promotion_type.xNo_promotion + 
                         promotion_type.xRakshabandhan.Sale, 
                       data = train_game)
summary(step_game_model4) #Adjusted R-squared:  0.7489
vif(step_game_model4)

#Removed: product_analytic_vertical.xJoystickGamingWheel
step_game_model5 <- lm(formula = tot_gmv ~ Total_Investment + 
                         TV + 
                         Sponsorship + 
                         Content_Marketing + 
                         Affiliates + 
                         Radio + 
                         Other + 
                         TV_adstock + 
                         Content_Marketing_adstock + 
                         Online_Marketing_adstock + 
                         Radio_adstock + 
                         Other_adstock + 
                         product_analytic_vertical.xGamePad + 
                         product_analytic_vertical.xGamingHeadset + 
                         product_analytic_vertical.xGamingKeyboard + 
                         product_analytic_vertical.xGamingMouse + 
                         promotion_type.xChristmas...New.Year.Sale + 
                         promotion_type.xDaussera.sale + 
                         promotion_type.xNo_promotion + 
                         promotion_type.xRakshabandhan.Sale, 
                       data = train_game)
summary(step_game_model5) #Adjusted R-squared:  0.7485
vif(step_game_model5)

#Removed: Content_Marketing_adstock
step_game_model6 <- lm(formula = tot_gmv ~ Total_Investment + 
                         TV + 
                         Sponsorship + 
                         Content_Marketing + 
                         Affiliates + 
                         Radio + 
                         Other + 
                         TV_adstock + 
                         Online_Marketing_adstock + 
                         Radio_adstock + 
                         Other_adstock + 
                         product_analytic_vertical.xGamePad + 
                         product_analytic_vertical.xGamingHeadset + 
                         product_analytic_vertical.xGamingKeyboard + 
                         product_analytic_vertical.xGamingMouse + 
                         promotion_type.xChristmas...New.Year.Sale + 
                         promotion_type.xDaussera.sale + 
                         promotion_type.xNo_promotion + 
                         promotion_type.xRakshabandhan.Sale, 
                       data = train_game)
summary(step_game_model6) #Adjusted R-squared: 0.746
vif(step_game_model6)

#Based on high VIF values
# Remove Total_Investment (highest VIF)
step_game_model7 <- lm(formula = tot_gmv ~ TV + 
                         Sponsorship + 
                         Content_Marketing + 
                         Affiliates + 
                         Radio + 
                         Other + 
                         TV_adstock + 
                         Online_Marketing_adstock + 
                         Radio_adstock + 
                         Other_adstock + 
                         product_analytic_vertical.xGamePad + 
                         product_analytic_vertical.xGamingHeadset + 
                         product_analytic_vertical.xGamingKeyboard + 
                         product_analytic_vertical.xGamingMouse + 
                         promotion_type.xChristmas...New.Year.Sale + 
                         promotion_type.xDaussera.sale + 
                         promotion_type.xNo_promotion + 
                         promotion_type.xRakshabandhan.Sale, 
                       data = train_game)
summary(step_game_model7) #Adjusted R-squared: 0.7325
vif(step_game_model7)

# Removed: Other_adstock because of very high VIF
step_game_model8 <- lm(formula = tot_gmv ~ TV + 
                         Sponsorship + 
                         Content_Marketing + 
                         Affiliates + 
                         Radio + 
                         Other + 
                         TV_adstock + 
                         Online_Marketing_adstock + 
                         Radio_adstock + 
                         product_analytic_vertical.xGamePad + 
                         product_analytic_vertical.xGamingHeadset + 
                         product_analytic_vertical.xGamingKeyboard + 
                         product_analytic_vertical.xGamingMouse + 
                         promotion_type.xChristmas...New.Year.Sale + 
                         promotion_type.xDaussera.sale + 
                         promotion_type.xNo_promotion + 
                         promotion_type.xRakshabandhan.Sale, 
                       data = train_game)
summary(step_game_model8) #Adjusted R-squared: 0.7281
vif(step_game_model8)

# Removed: promotion_type.xDaussera.sale
step_game_model9 <- lm(formula = tot_gmv ~ TV + 
                         Sponsorship + 
                         Content_Marketing + 
                         Affiliates + 
                         Radio + 
                         Other + 
                         TV_adstock + 
                         Online_Marketing_adstock + 
                         Radio_adstock + 
                         product_analytic_vertical.xGamePad + 
                         product_analytic_vertical.xGamingHeadset + 
                         product_analytic_vertical.xGamingKeyboard + 
                         product_analytic_vertical.xGamingMouse + 
                         promotion_type.xChristmas...New.Year.Sale + 
                         promotion_type.xNo_promotion + 
                         promotion_type.xRakshabandhan.Sale, 
                       data = train_game)
summary(step_game_model9) #Adjusted R-squared: 0.7282
vif(step_game_model9)

#Removed: Other
step_game_model10 <- lm(formula = tot_gmv ~ TV + 
                         Sponsorship + 
                         Content_Marketing + 
                         Affiliates + 
                         Radio + 
                         TV_adstock + 
                         Online_Marketing_adstock + 
                         Radio_adstock + 
                         product_analytic_vertical.xGamePad + 
                         product_analytic_vertical.xGamingHeadset + 
                         product_analytic_vertical.xGamingKeyboard + 
                         product_analytic_vertical.xGamingMouse + 
                         promotion_type.xChristmas...New.Year.Sale + 
                         promotion_type.xNo_promotion + 
                         promotion_type.xRakshabandhan.Sale, 
                       data = train_game)
summary(step_game_model10) #Adjusted R-squared: 0.7286
vif(step_game_model10)

#Removed: Radio_adstock
step_game_model11 <- lm(formula = tot_gmv ~ TV + 
                          Sponsorship + 
                          Content_Marketing + 
                          Affiliates + 
                          Radio + 
                          TV_adstock + 
                          Online_Marketing_adstock + 
                          product_analytic_vertical.xGamePad + 
                          product_analytic_vertical.xGamingHeadset + 
                          product_analytic_vertical.xGamingKeyboard + 
                          product_analytic_vertical.xGamingMouse + 
                          promotion_type.xChristmas...New.Year.Sale + 
                          promotion_type.xNo_promotion + 
                          promotion_type.xRakshabandhan.Sale, 
                        data = train_game)
summary(step_game_model11) #Adjusted R-squared: 0.7291
vif(step_game_model11)

#Removed: Sponsorship
step_game_model12 <- lm(formula = tot_gmv ~ TV + 
                          Content_Marketing + 
                          Affiliates + 
                          Radio + 
                          TV_adstock + 
                          Online_Marketing_adstock + 
                          product_analytic_vertical.xGamePad + 
                          product_analytic_vertical.xGamingHeadset + 
                          product_analytic_vertical.xGamingKeyboard + 
                          product_analytic_vertical.xGamingMouse + 
                          promotion_type.xChristmas...New.Year.Sale + 
                          promotion_type.xNo_promotion + 
                          promotion_type.xRakshabandhan.Sale, 
                        data = train_game)
summary(step_game_model12) #Adjusted R-squared: 0.7295
vif(step_game_model12)

# #Removed: TVAdstock (high p and VIF values)
step_game_model13 <- lm(formula = tot_gmv ~ TV +
                          Content_Marketing +
                          Affiliates +
                          Radio +
                          Online_Marketing_adstock +
                          product_analytic_vertical.xGamePad +
                          product_analytic_vertical.xGamingHeadset +
                          product_analytic_vertical.xGamingKeyboard +
                          product_analytic_vertical.xGamingMouse +
                          promotion_type.xChristmas...New.Year.Sale +
                          promotion_type.xNo_promotion +
                          promotion_type.xRakshabandhan.Sale,
                        data = train_game)
summary(step_game_model13) #Adjusted R-squared: 0.7266 [drop in value]
vif(step_game_model13)

#Removed: Online_Marketing_adstock
step_game_model14 <- lm(formula = tot_gmv ~ TV +
                          Content_Marketing +
                          Affiliates +
                          Radio +
                          product_analytic_vertical.xGamePad +
                          product_analytic_vertical.xGamingHeadset +
                          product_analytic_vertical.xGamingKeyboard +
                          product_analytic_vertical.xGamingMouse +
                          promotion_type.xChristmas...New.Year.Sale +
                          promotion_type.xNo_promotion +
                          promotion_type.xRakshabandhan.Sale,
                        data = train_game)
summary(step_game_model14) #Adjusted R-squared: 0.7263 [drop in value]
vif(step_game_model14)

#Removed: Content Marketing
step_game_model15 <- lm(formula = tot_gmv ~ TV +
                          Affiliates +
                          Radio +
                          product_analytic_vertical.xGamePad +
                          product_analytic_vertical.xGamingHeadset +
                          product_analytic_vertical.xGamingKeyboard +
                          product_analytic_vertical.xGamingMouse +
                          promotion_type.xChristmas...New.Year.Sale +
                          promotion_type.xNo_promotion +
                          promotion_type.xRakshabandhan.Sale,
                        data = train_game)
summary(step_game_model15) #Adjusted R-squared: 0.726 [drop in value]
vif(step_game_model15)

#Removed: Radio
step_game_model16 <- lm(formula = tot_gmv ~ TV +
                          Affiliates +
                          product_analytic_vertical.xGamePad +
                          product_analytic_vertical.xGamingHeadset +
                          product_analytic_vertical.xGamingKeyboard +
                          product_analytic_vertical.xGamingMouse +
                          promotion_type.xChristmas...New.Year.Sale +
                          promotion_type.xNo_promotion +
                          promotion_type.xRakshabandhan.Sale,
                        data = train_game)
summary(step_game_model16) #Adjusted R-squared: 0.7259 [drop in value]
vif(step_game_model16) #<-- Final model



########################################################
# Validation

gmv_game_prediction <- predict(step_game_model16, test_game)
test_game$predicted_gmv <- gmv_game_prediction

cam_r <- cor(test_game$tot_gmv, test_game$predicted_gmv)
cam_rsquared <- cam_r^2
cam_rsquared
#R squared on test: 0.62

############################################################################### Estimating elasticity ##################################


game_final_model <- step_game_model4

elasticity <- function(var) {
  x <- as.numeric(game_final_model$coefficients[var])
  return(x)
}

var_list_game <- list()

for(i in 2:length(game_final_model$coefficients)) {
  var_list_game[i-1] <- elasticity(names(game_final_model$coefficients)[i])
}

elasticity.outputs.game <- data.frame(names(game_final_model$coefficients[2:length(game_final_model$coefficients)]))
elasticity.outputs.game <- cbind(elasticity.outputs.game,do.call(rbind.data.frame, var_list_game))
colnames(elasticity.outputs.game) <- c("Variable","Elasticity")

elasticity.outputs.game$direction <- ifelse(elasticity.outputs.game$Elasticity > 0, "Positive", "Negative")

ggplot(data=elasticity.outputs.game, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Game Accessory - Linear Model") +xlab("Variables")

########################################################################## Cross validation ######################################

game_lm_cv <- cv.lm(data = train_game, form.lm = step_game_model4,m =10)

#ms 0.331


###################################################################################################################################

########################################################################## Multiplicative Linear Model ############################


multi_gaming <- GamingAccessory
multi_gaming$week_no <- NULL
multi_gaming$Month <- NULL

#Treatment of NULL values
summary(multi_gaming)
multi_gaming$TV[which(multi_gaming$TV == 0)] <- 0.01
multi_gaming$Radio[which(multi_gaming$Radio == 0)] <- 0.01
multi_gaming$Other[which(multi_gaming$Other == 0 )] <- 0.01
multi_gaming$Content_Marketing[which(multi_gaming$Content_Marketing == 0)] <- 0.01

#Taking log of the data for multiplicative behaviour
#Log of the numerical variables
multi_gaming[,1:24] <- data.frame(sign(multi_gaming[,1:24])*log(abs(multi_gaming[,1:24])))

trainindices.game.m= sample(1:nrow(multi_gaming), 0.6*nrow(multi_gaming))
#Generate the train data set
train.game.m = multi_gaming[trainindices.game.m,]
#Similarly store the rest of the observations into an object "test".
test.game.m = multi_gaming[-trainindices.game.m,]

model.game.mul1 <- lm(tot_gmv~., data = train.game.m)
summary(model.game.mul1)

model.game.mul2 <- stepAIC(model.game.mul1, direction = "both")
summary(model.game.mul2)
vif(model.game.mul2)


model.game.mul3 <- lm(formula = tot_gmv ~ list_price + week + Total_Investment + 
                        TV + Sponsorship + Content_Marketing + Online_Marketing + 
                        Affiliates + SEM + Radio + Other + Sponsorship_adstock + 
                        Affiliates_adstock + SEM_adstock + Radio_adstock + NPS + 
                        discount_over_mrp + product_analytic_vertical.xCoolingPad + 
                        product_analytic_vertical.xGameControlMount + product_analytic_vertical.xGamePad + 
                        product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamingAdapter + 
                        product_analytic_vertical.xGamingChargingStation + product_analytic_vertical.xGamingGun + 
                        product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                        product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                        product_analytic_vertical.xJoystickGamingWheel + product_analytic_vertical.xMotionController + 
                        promotion_type.xEid...Rathayatra.sale + promotion_type.xIndependence.Sale + 
                        promotion_type.xNo_promotion + promotion_type.xRakshabandhan.Sale + 
                        promotion_type.xRepublic.Day, data = train.game.m)

summary(model.game.mul3)
vif(model.game.mul3)

#removing Radio, Sponsorship, Radio_adstock, Affiliates_adstock, discount_over_mrp, product_analytic_vertical.xGamingKeyboard, 
#promotion_type.xEid...Rathayatra.sale, Online_Marketing, Content_Marketing, promotion_type.xRepublic.Day, product_analytic_vertical.xGamingAccessoryKit, 
model.game.mul4 <- lm(formula = tot_gmv ~ list_price + week + Total_Investment + 
                        TV + Affiliates + SEM + Other + Sponsorship_adstock + SEM_adstock + 
                        NPS + product_analytic_vertical.xCoolingPad + product_analytic_vertical.xGameControlMount + 
                        product_analytic_vertical.xGamePad + product_analytic_vertical.xGamingAdapter + 
                        product_analytic_vertical.xGamingChargingStation + product_analytic_vertical.xGamingGun + 
                        product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingMouse + 
                        product_analytic_vertical.xGamingSpeaker + product_analytic_vertical.xJoystickGamingWheel + 
                        product_analytic_vertical.xMotionController + promotion_type.xIndependence.Sale + 
                        promotion_type.xNo_promotion + promotion_type.xRakshabandhan.Sale, 
                      data = train.game.m)

summary(model.game.mul4)
vif(model.game.mul4)

#Adjusted R-squared:  0.768

#Testing

predict_game_mul <- predict(model.game.mul4, test.game.m)
test.game.m$predicted_gmv <- predict_game_mul

# Now, we need to test the r square between actual and predicted sales.
game_r <- cor(test.game.m$tot_gmv, test.game.m$predicted_gmv)
game_r <- cor(test.game.m$tot_gmv, test.game.m$predicted_gmv)^2
game_r
#Rsquare on Test Data : 0.766

############################################################################### Estimating elasticity ##################################

game.mul.model <- model.game.mul4

elasticity <- function(var) {
  x <- as.numeric(game.mul.model$coefficients[var] * mean(train.game.m[,var])/mean(train.game.m$tot_gmv))
  return(x)
}

varlist.mul.game <- list()

for(i in 2:length(game.mul.model$coefficients)) {
  varlist.mul.game[i-1] <- elasticity(names(game.mul.model$coefficients)[i])
}

elasticity.game.mul <- data.frame(names(game.mul.model$coefficients[2:length(game.mul.model$coefficients)]))
elasticity.game.mul <- cbind(elasticity.game.mul,do.call(rbind.data.frame, varlist.mul.game))
colnames(elasticity.game.mul) <- c("Variable","Elasticity")

elasticity.game.mul$direction <- ifelse(elasticity.game.mul$Elasticity > 0, "Positive", "Negative")

ggplot(data=elasticity.game.mul, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Game Accessory - Multiplicative Model") +xlab("Variables")

########################################################################## Cross validation ######################################

game.mul.cv <- cv.lm(data = train.game.m , form.lm = model.game.mul4, m =10)

#ms 1.13


###################################################################################################################################
########################################################################## Distributed Lag Model ##################################

#Create lag variables in the dataset needed for distributed lag model
weekly_order_med_inv_data_lag_game <- lag_var_creation(weekly_order_med_inv_data,"GamingAccessory")

colnames(weekly_order_med_inv_data_lag_game)

weekly_order_med_inv_data_lag_game [,c(1:35)] <- scale(weekly_order_med_inv_data_lag_game [,c(1:35)])

#Dividing into Train and Test data
set.seed(200)

trainindices= sample(1:nrow(weekly_order_med_inv_data_lag_game), 0.7*nrow(weekly_order_med_inv_data_lag_game))

#Generate the train data set
train_game = weekly_order_med_inv_data_lag_game[trainindices,]

#Similarly store the rest of the observations into an object "test".
test_game = weekly_order_med_inv_data_lag_game[-trainindices,]


game_model1 <- lm(tot_gmv~. , data = train_game)
summary(game_model1)


step_game_model1 <- stepAIC(game_model1, direction = "both")
summary(step_game_model1)
vif(step_game_model1)

#Removing SEM_adstock, Radio_adstock, Total_Investment, Radio, Affiliates, Other_adstock, Content_Marketing, gmv_lag_2, 
#gmv_lag_1, TV_adstock, discount_over_mrp, Online_Marketing_adstock, list_price_lag_3, product_analytic_vertical.xGamingMousePad one by one
step_game_model3 <- lm(formula = tot_gmv ~ week +Other + Digital_adstock + 
                         gmv_change_from_w1 + product_analytic_vertical.xGamePad + 
                         product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamingHeadset + 
                         product_analytic_vertical.xGamingKeyboard + product_analytic_vertical.xGamingMemoryCard + 
                         product_analytic_vertical.xGamingMouse + 
                         product_analytic_vertical.xJoystickGamingWheel + promotion_type.xBSD.5 + 
                         promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                         promotion_type.xIndependence.Sale + promotion_type.xPacman + 
                         promotion_type.xRakshabandhan.Sale + promotion_type.xValentine.s.Day, 
                       data = train_game)

summary(step_game_model3)
vif(step_game_model3)

#Removing Digital_adstock, product_analytic_vertical.xGamingKeyboard, promotion_type.xChristmas...New.Year.Sale, 
#product_analytic_vertical.xGamingAccessoryKit, product_analytic_vertical.xGamingMemoryCard, 
#product_analytic_vertical.xJoystickGamingWheel, promotion_type.xPacman, promotion_type.xBSD.5, promotion_type.xValentine.s.Day one by one 

step_game_model4 <- lm(formula = tot_gmv ~ week +Other + 
                         gmv_change_from_w1 + product_analytic_vertical.xGamePad + 
                         product_analytic_vertical.xGamingHeadset + 
                         product_analytic_vertical.xGamingMouse + 
                         promotion_type.xDaussera.sale + 
                         promotion_type.xIndependence.Sale + 
                         promotion_type.xRakshabandhan.Sale, 
                       data = train_game)

summary(step_game_model4)
vif(step_game_model4)

#Adjusted R-squared:  0.73

#Testing
gmv_game_prediction <- predict(step_game_model4, test_game[,-1])
test_game$predicted_gmv <- gmv_game_prediction

game_r <- cor(test_game$tot_gmv, test_game$predicted_gmv)
game_rsquared <- game_r^2
game_rsquared   
#Rsquare test = 0.688


############################################################################### Estimating elasticity ##################################

game_final_model <- step_game_model4

elasticity <- function(var) {
  x <- as.numeric(game_final_model$coefficients[var] )
  return(x)
}

var_list <- list()

for(i in 2:length(game_final_model$coefficients)) {
  var_list[i-1] <- elasticity(names(game_final_model$coefficients)[i])
}

elasticity.outputs <- data.frame(names(game_final_model$coefficients[2:length(game_final_model$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")
elasticity.outputs <- arrange( elasticity.outputs , -Elasticity)

ggplot(data=elasticity.outputs[, ], aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Gaming  Accessory -  Distributed Lag  Model") +xlab("Variables")

########################################################################## Cross validation ######################################

game_lm_cv <- cv.lm(data = weekly_order_med_inv_data_lag_game , form.lm = step_game_model4, m =10)
#ms   4.3e+10

###################################################################################################################################

################################################################################# Koyck Model #####################################

str(GamingAccessory)
sum(is.na(GamingAccessory))
GamingAccessoryKoyck <- GamingAccessory
library(DataCombine)
# Creating Lag variable 
GamingAccessoryKoyck <- slide(GamingAccessoryKoyck, Var = "tot_gmv",slideBy = -1)
str(GamingAccessoryKoyck)

colnames(Game_koyck)

Game_koyck <- na.omit(GamingAccessoryKoyck)
Game_koyck1 <- data.frame(scale(Game_koyck[,c(1:26)]))
Game_koyck2 <- data.frame(Game_koyck[,27:87])
Game_koyck3 <- data.frame(scale(Game_koyck[,88]))

Game_koyck <- cbind(Game_koyck1, Game_koyck2, Game_koyck3)
str(Game_koyck)
# removing month and year columns
Game_koyck <- Game_koyck[,-c(1,2)]
# splitting train & test sets
set.seed(100)
trainindices= sample(1:nrow(Game_koyck), 0.7*nrow(Game_koyck))
train_g = Game_koyck[trainindices,]
test_g = Game_koyck[-trainindices,]

## building first overall model
Koyck_model1_game <- lm(tot_gmv~.,train_g)
summary(Koyck_model1_game)

# Evaluating the first models for significant predictors
Koyck_model2_game <- stepAIC(Koyck_model1_game,direction = "both")
summary(Koyck_model2_game)
vif(Koyck_model2_game)

Koyck_model3_game<- lm(formula = tot_gmv ~ Total_Investment + TV + Sponsorship + 
                         Content_Marketing + Online_Marketing + Affiliates + SEM + 
                         Radio + Digital_adstock + Sponsorship_adstock + Content_Marketing_adstock + 
                         Online_Marketing_adstock + Affiliates_adstock + SEM_adstock + 
                         Radio_adstock + NPS + discount_over_mrp + product_analytic_vertical.xGamePad + 
                         product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamingHeadset + 
                         product_analytic_vertical.xGamingKeyboard + product_analytic_vertical.xGamingMemoryCard + 
                         product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingMousePad + 
                         product_analytic_vertical.xGamingSpeaker + product_analytic_vertical.xJoystickGamingWheel + 
                         product_analytic_vertical.xMotionController + promotion_type.xBig.Diwali.Sale + 
                         promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                         promotion_type.xEid...Rathayatra.sale + promotion_type.xIndependence.Sale + 
                         promotion_type.xNo_promotion + promotion_type.xRakshabandhan.Sale + 
                         scale.Game_koyck...88.., data = train_g)

summary(Koyck_model3_game)
vif(Koyck_model3_game)

# removing variables 

#Remove Affiliates, Total_Investment, SEM_adstock, Online_Marketing_adstock, Content_Marketing_adstock, SEM, Sponsorship, 
#Online_Marketing, Radio, Content_Marketing, Sponsorship_adstock, discount_over_mrp, Affiliates_adstock, Radio_adstock, promotion_type.xChristmas...New.Year.Sale one by one
Koyck_model4_game<- lm(formula = tot_gmv ~ TV +  
                         Digital_adstock + 
                         NPS  + product_analytic_vertical.xGamePad + 
                         product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamingHeadset + 
                         product_analytic_vertical.xGamingKeyboard + product_analytic_vertical.xGamingMemoryCard + 
                         product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingMousePad + 
                         product_analytic_vertical.xGamingSpeaker + product_analytic_vertical.xJoystickGamingWheel + 
                         product_analytic_vertical.xMotionController + promotion_type.xBig.Diwali.Sale + 
                         promotion_type.xDaussera.sale + 
                         promotion_type.xEid...Rathayatra.sale + promotion_type.xIndependence.Sale + 
                         promotion_type.xNo_promotion + promotion_type.xRakshabandhan.Sale + 
                         scale.Game_koyck...88.., data = train_g)

summary(Koyck_model4_game)
vif(Koyck_model4_game)
#Remove NPS, product_analytic_vertical.xGamingAccessoryKit, promotion_type.xDaussera.sale , product_analytic_vertical.xGamingMousePad, 
#product_analytic_vertical.xGamingMemoryCard, promotion_type.xBig.Diwali.Sale, Digital_adstock, product_analytic_vertical.xJoystickGamingWheel, product_analytic_vertical.xMotionController   
#promotion_type.xEid...Rathayatra.sale, product_analytic_vertical.xGamingSpeaker, product_analytic_vertical.xMotionController one by one  
Koyck_model5_game<- lm(formula = tot_gmv ~ TV +  product_analytic_vertical.xGamePad + 
                         product_analytic_vertical.xGamingHeadset + 
                         product_analytic_vertical.xGamingKeyboard + 
                         product_analytic_vertical.xGamingMouse +
                         promotion_type.xIndependence.Sale + 
                         promotion_type.xNo_promotion + promotion_type.xRakshabandhan.Sale, data = train_g)

summary(Koyck_model5_game)
vif(Koyck_model5_game)

#Adjusted R-squared:  0.697

# final model after AIC and VIF tuning
final_koyck_game <- Koyck_model5_game


par(mfrow = c(2,2))
plot(final_koyck_game, main = "Final Gaming Accessory - Koyck Model")

# Testing
pred_koyck_game <- predict(final_koyck_game, test_g)
RMSE(test_g$tot_gmv,pred_koyck_game)
game_r_koyck <- cor(test_g$tot_gmv, pred_koyck_game)
game_rsquared <- game_r_koyck^2
game_rsquared
#Rsquared on test: 0.657

############################################################################### Elasticity Measure ###################################

game_final_model <- final_koyck_game

elasticity <- function(var) {
  x <- as.numeric(game_final_model$coefficients[var] * mean(train_g[,var])/mean(train_g$tot_gmv))
  return(x)
}

var_list <- list()

for(i in 2:length(game_final_model$coefficients)) {
  var_list[i-1] <- elasticity(names(game_final_model$coefficients)[i])
}

elasticity.outputs <- data.frame(names(game_final_model$coefficients[2:length(game_final_model$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")

ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Gaming Accessory - Koyck Model") +xlab("Variables")

############################################################################### Cross validation ###################################

crossval <- cv.lm(data = train_g, form.lm = formula(final_koyck_game),m = 10, main = "Cross Validation Gaming Accessory")
#ms = 0.352

###################################################################################################################################

############################################################### Lag Plus Multiplicative model #####################################

#Create lag variables in the dataset needed for Lag Plus Multiplicative model
weekly_data_lag_plus_mul_game <- lag_var_creation(weekly_order_med_inv_data,"GamingAccessory")

weekly_data_lag_plus_mul_game <-na.omit(weekly_data_lag_plus_mul_game)

weekly_data_lag_plus_mul_game_tmp  <- lapply (weekly_data_lag_plus_mul_game[,1:35] ,function(x) ifelse ( x==0 , 0, log(abs(x)))*sign(x) )
weekly_data_lag_plus_mul_game <- data.frame( cbind( weekly_data_lag_plus_mul_game_tmp ,weekly_data_lag_plus_mul_game[,36:96] ))

weekly_data_lag_plus_mul_game [,c(1:35)] <- scale(weekly_data_lag_plus_mul_game [,c(1:35)])

set.seed(200)


trainindices= sample(1:nrow(weekly_data_lag_plus_mul_game), 0.7*nrow(weekly_data_lag_plus_mul_game))
#Generate the train data set
train_lagmul_game = weekly_data_lag_plus_mul_game[trainindices,]

#Similarly store the rest of the observations into an object "test".
test_lagmul_game = weekly_data_lag_plus_mul_game[-trainindices,]

lag_mul_gam_model_1 <- lm(tot_gmv~. , data = train_lagmul_game)
summary(lag_mul_gam_model_1)

step_lag_mul_gam_model_1 <- stepAIC(lag_mul_gam_model_1, direction = "both")
summary(step_lag_mul_gam_model_1)
vif (step_lag_mul_gam_model_1 )

step_lag_mul_gam_model_2 <- lm(formula = tot_gmv ~ list_price + Total_Investment + TV + Digital + 
                                 Sponsorship + Content_Marketing + Affiliates + Radio + Other + 
                                 TV_adstock + Digital_adstock + Online_Marketing_adstock + 
                                 Affiliates_adstock + Radio_adstock + Other_adstock + NPS + 
                                 gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + gmv_change_from_w2 + 
                                 gmv_change_from_w3 + list_price_lag_1 + product_analytic_vertical.xCoolingPad + 
                                 product_analytic_vertical.xGamePad + product_analytic_vertical.xGamingAccessoryKit + 
                                 product_analytic_vertical.xGamingChargingStation + product_analytic_vertical.xGamingHeadset + 
                                 product_analytic_vertical.xGamingKeyboard + product_analytic_vertical.xGamingMouse + 
                                 product_analytic_vertical.xGamingSpeaker + promotion_type.xBSD.5 + 
                                 promotion_type.xChristmas...New.Year.Sale + promotion_type.xIndependence.Sale + 
                                 promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRakshabandhan.Sale + 
                                 promotion_type.xRepublic.Day, data = train_lagmul_game)
summary(step_lag_mul_gam_model_2)
vif (step_lag_mul_gam_model_2 )

# Remove variables Radio, Other_adstock, Digital, Digital_adstock, promotion_type.xChristmas...New.Year.Sale, 
#promotion_type.xPacman, promotion_type.xBSD.5, gmv_change_from_w2 one by one
step_game_model3 <- lm(formula = tot_gmv ~ list_price + Total_Investment + TV + Sponsorship + Content_Marketing + Affiliates + Other + 
                         TV_adstock + Online_Marketing_adstock + Affiliates_adstock + Radio_adstock +  NPS + 
                         gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 +  gmv_change_from_w3 + list_price_lag_1 + 
                         product_analytic_vertical.xCoolingPad + product_analytic_vertical.xGamePad + product_analytic_vertical.xGamingAccessoryKit + 
                         product_analytic_vertical.xGamingChargingStation + product_analytic_vertical.xGamingHeadset + 
                         product_analytic_vertical.xGamingKeyboard + product_analytic_vertical.xGamingMouse + 
                         product_analytic_vertical.xGamingSpeaker + promotion_type.xIndependence.Sale + 
                         promotion_type.xNo_promotion + promotion_type.xRakshabandhan.Sale + 
                         promotion_type.xRepublic.Day, data = train_lagmul_game)

summary(step_game_model3)
vif (step_game_model3 )

#Remove variables Affiliates, Affiliates_adstock, Sponsorship, gmv_lag_3, promotion_type.xRepublic.Day one by one due to high VIF
step_game_model4 <- lm(formula = tot_gmv ~ list_price + Total_Investment + TV + 
                         Content_Marketing + Other + 
                         TV_adstock + 
                          Radio_adstock +  NPS + 
                         gmv_lag_2 + gmv_change_from_w1 +  
                         gmv_change_from_w3 + list_price_lag_1 + product_analytic_vertical.xCoolingPad + 
                         product_analytic_vertical.xGamePad + product_analytic_vertical.xGamingAccessoryKit + 
                         product_analytic_vertical.xGamingChargingStation + product_analytic_vertical.xGamingHeadset + 
                         product_analytic_vertical.xGamingKeyboard + product_analytic_vertical.xGamingMouse + 
                         product_analytic_vertical.xGamingSpeaker + 
                         promotion_type.xIndependence.Sale + 
                         promotion_type.xNo_promotion + promotion_type.xRakshabandhan.Sale, data = train_lagmul_game)

summary(step_game_model4)
vif (step_game_model4 )


#Adjusted R-squared:  0.813 


## Model evalution on test dataset
gmv_cam_prediction <- predict(step_game_model4, test_lagmul_game[,-1])
test_lagmul_game$predicted_gmv <- gmv_cam_prediction

game_r <- cor(test_lagmul_game$tot_gmv, test_lagmul_game$predicted_gmv  )
game_rsquared <- game_r^2
game_rsquared   
#Rsquare on test = 0.776

############################################################################### Estimating elasticity ##################################

game_final_model <- step_game_model4

elasticity <- function(var) {
  x <- as.numeric(game_final_model$coefficients[var] )
  return(x)
}

var_list <- list()

for(i in 2:length(game_final_model$coefficients)) {
  var_list[i-1] <- elasticity(names(game_final_model$coefficients)[i])
}

elasticity.outputs <- data.frame(names(game_final_model$coefficients[2:length(game_final_model$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")
elasticity.outputs <- arrange( elasticity.outputs , -Elasticity)

ggplot(data=elasticity.outputs[, ], aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Gaming  Accessory -  Lag Distributed Multipicative  Model") +xlab("Variables")

########################################################################## Cross validation ######################################

game_lm_cv <- cv.lm(data = weekly_data_lag_plus_mul_game , form.lm = step_game_model4, m =10)

#ms 0.212 

###################################################################################################################################

###################################################
#Game Accesory  				        #	Train	#	Test    #
###################################################
#Linear Model					          #	72.59 #	62      #
#Multiplicative model			      #	76.8  #	76.6    #
#Distributed Lag Model          # 73    # 68.8    #
#koyck Model					          #	89.1  #	70.8    #
#Lag plus Multiplicative model	#	81.3	#	77.6    #
#                               # 69.7  # 65.7    #
###################################################