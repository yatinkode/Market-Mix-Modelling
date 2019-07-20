###################################################################################################################################
# Camera Accesory Models
###################################################################################################################################


###################################################################### Linear Model ###############################################

linear_camera <- CameraAccessory
linear_camera$week_no <- NULL
linear_camera$Month <- NULL
linear_camera$Year_Month <- NULL

colnames(linear_camera)

#Scaling the dataset
linear_camera[,1:24] <- data.frame(scale(linear_camera[,1:24], center = TRUE))

set.seed(100)
trainindices= sample(1:nrow(linear_camera), 0.7*nrow(linear_camera))
#Generate the train data set
train = linear_camera[trainindices,]
#Similarly store the rest of the observations into an object "test".
test = linear_camera[-trainindices,]

cam_model1 <- lm(tot_gmv~. , data = train)
summary(cam_model1) #Adjusted R-squared:  0.825

step_cam_model1 <- stepAIC(cam_model1, direction = "both")
summary(step_cam_model1)             #Adjusted R-squared:  0.828 
vif(step_cam_model1)

#Attributes to be removed: Digital_adstock
step_cam_model2 <- lm(formula = tot_gmv ~ Total_Investment + 
                        Digital + 
                        Sponsorship + 
                        Content_Marketing + 
                        Online_Marketing + 
                        Affiliates + 
                        SEM + 
                        TV_adstock + 
                        Content_Marketing_adstock + 
                        Online_Marketing_adstock + 
                        SEM_adstock + 
                        Radio_adstock + 
                        Other_adstock + 
                        NPS + discount_over_mrp + 
                        product_analytic_vertical.xCameraAccessory + 
                        product_analytic_vertical.xCameraBag + 
                        product_analytic_vertical.xCameraBattery + 
                        product_analytic_vertical.xCameraBatteryCharger + 
                        product_analytic_vertical.xCameraBatteryGrip + 
                        product_analytic_vertical.xCameraEyeCup + 
                        product_analytic_vertical.xCameraFilmRolls + 
                        product_analytic_vertical.xCameraHousing + 
                        product_analytic_vertical.xCameraLEDLight + 
                        product_analytic_vertical.xCameraMicrophone + 
                        product_analytic_vertical.xCameraMount + 
                        product_analytic_vertical.xCameraRemoteControl + 
                        product_analytic_vertical.xCameraTripod + 
                        product_analytic_vertical.xExtensionTube + 
                        product_analytic_vertical.xFilter + 
                        product_analytic_vertical.xFlash + 
                        product_analytic_vertical.xFlashShoeAdapter + 
                        product_analytic_vertical.xLens + 
                        product_analytic_vertical.xReflectorUmbrella + 
                        product_analytic_vertical.xSoftbox + 
                        product_analytic_vertical.xStrap + 
                        product_analytic_vertical.xTeleconverter + 
                        product_analytic_vertical.xTelescope + 
                        promotion_type.xChristmas...New.Year.Sale + 
                        promotion_type.xDaussera.sale + 
                        promotion_type.xNo_promotion, data = train)

summary(step_cam_model2) #Adjusted R-squared:  0.828
vif(step_cam_model2)

#Removed: Online_Marketing_adstock
step_cam_model3 <- lm(formula = tot_gmv ~ Total_Investment + 
                        Digital + 
                        Sponsorship + 
                        Content_Marketing + 
                        Online_Marketing + 
                        Affiliates + 
                        SEM + 
                        TV_adstock + 
                        Content_Marketing_adstock + 
                        SEM_adstock + 
                        Radio_adstock + 
                        Other_adstock + 
                        NPS + discount_over_mrp + 
                        product_analytic_vertical.xCameraAccessory + 
                        product_analytic_vertical.xCameraBag + 
                        product_analytic_vertical.xCameraBattery + 
                        product_analytic_vertical.xCameraBatteryCharger + 
                        product_analytic_vertical.xCameraBatteryGrip + 
                        product_analytic_vertical.xCameraEyeCup + 
                        product_analytic_vertical.xCameraFilmRolls + 
                        product_analytic_vertical.xCameraHousing + 
                        product_analytic_vertical.xCameraLEDLight + 
                        product_analytic_vertical.xCameraMicrophone + 
                        product_analytic_vertical.xCameraMount + 
                        product_analytic_vertical.xCameraRemoteControl + 
                        product_analytic_vertical.xCameraTripod + 
                        product_analytic_vertical.xExtensionTube + 
                        product_analytic_vertical.xFilter + 
                        product_analytic_vertical.xFlash + 
                        product_analytic_vertical.xFlashShoeAdapter + 
                        product_analytic_vertical.xLens + 
                        product_analytic_vertical.xReflectorUmbrella + 
                        product_analytic_vertical.xSoftbox + 
                        product_analytic_vertical.xStrap + 
                        product_analytic_vertical.xTeleconverter + 
                        product_analytic_vertical.xTelescope + 
                        promotion_type.xChristmas...New.Year.Sale + 
                        promotion_type.xDaussera.sale + 
                        promotion_type.xNo_promotion, data = train)

summary(step_cam_model3) #Adjusted R-squared:  0.827
vif(step_cam_model3)

#Removed: TV_adstock
step_cam_model4 <- lm(formula = tot_gmv ~ Total_Investment + 
                        Digital + 
                        Sponsorship + 
                        Content_Marketing + 
                        Online_Marketing + 
                        Affiliates + 
                        SEM + 
                        Content_Marketing_adstock + 
                        SEM_adstock + 
                        Radio_adstock + 
                        Other_adstock + 
                        NPS + discount_over_mrp + 
                        product_analytic_vertical.xCameraAccessory + 
                        product_analytic_vertical.xCameraBag + 
                        product_analytic_vertical.xCameraBattery + 
                        product_analytic_vertical.xCameraBatteryCharger + 
                        product_analytic_vertical.xCameraBatteryGrip + 
                        product_analytic_vertical.xCameraEyeCup + 
                        product_analytic_vertical.xCameraFilmRolls + 
                        product_analytic_vertical.xCameraHousing + 
                        product_analytic_vertical.xCameraLEDLight + 
                        product_analytic_vertical.xCameraMicrophone + 
                        product_analytic_vertical.xCameraMount + 
                        product_analytic_vertical.xCameraRemoteControl + 
                        product_analytic_vertical.xCameraTripod + 
                        product_analytic_vertical.xExtensionTube + 
                        product_analytic_vertical.xFilter + 
                        product_analytic_vertical.xFlash + 
                        product_analytic_vertical.xFlashShoeAdapter + 
                        product_analytic_vertical.xLens + 
                        product_analytic_vertical.xReflectorUmbrella + 
                        product_analytic_vertical.xSoftbox + 
                        product_analytic_vertical.xStrap + 
                        product_analytic_vertical.xTeleconverter + 
                        product_analytic_vertical.xTelescope + 
                        promotion_type.xChristmas...New.Year.Sale + 
                        promotion_type.xDaussera.sale + 
                        promotion_type.xNo_promotion, data = train)

summary(step_cam_model4) #Adjusted R-squared:  0.827
vif(step_cam_model4)

#Removed: Other_Adstock
step_cam_model5 <- lm(formula = tot_gmv ~ Total_Investment + 
                        Digital + 
                        Sponsorship + 
                        Content_Marketing + 
                        Online_Marketing + 
                        Affiliates + 
                        SEM + 
                        Content_Marketing_adstock + 
                        SEM_adstock + 
                        Radio_adstock + 
                        NPS + discount_over_mrp + 
                        product_analytic_vertical.xCameraAccessory + 
                        product_analytic_vertical.xCameraBag + 
                        product_analytic_vertical.xCameraBattery + 
                        product_analytic_vertical.xCameraBatteryCharger + 
                        product_analytic_vertical.xCameraBatteryGrip + 
                        product_analytic_vertical.xCameraEyeCup + 
                        product_analytic_vertical.xCameraFilmRolls + 
                        product_analytic_vertical.xCameraHousing + 
                        product_analytic_vertical.xCameraLEDLight + 
                        product_analytic_vertical.xCameraMicrophone + 
                        product_analytic_vertical.xCameraMount + 
                        product_analytic_vertical.xCameraRemoteControl + 
                        product_analytic_vertical.xCameraTripod + 
                        product_analytic_vertical.xExtensionTube + 
                        product_analytic_vertical.xFilter + 
                        product_analytic_vertical.xFlash + 
                        product_analytic_vertical.xFlashShoeAdapter + 
                        product_analytic_vertical.xLens + 
                        product_analytic_vertical.xReflectorUmbrella + 
                        product_analytic_vertical.xSoftbox + 
                        product_analytic_vertical.xStrap + 
                        product_analytic_vertical.xTeleconverter + 
                        product_analytic_vertical.xTelescope + 
                        promotion_type.xChristmas...New.Year.Sale + 
                        promotion_type.xDaussera.sale + 
                        promotion_type.xNo_promotion, data = train)

summary(step_cam_model5) #Adjusted R-squared:  0.827
vif(step_cam_model5)

#Removed: Other_Adstock
step_cam_model6 <- lm(formula = tot_gmv ~ Total_Investment + 
                        Digital + 
                        Sponsorship + 
                        Content_Marketing + 
                        Online_Marketing + 
                        Affiliates + 
                        SEM + 
                        Content_Marketing_adstock + 
                        SEM_adstock + 
                        Radio_adstock + 
                        discount_over_mrp + 
                        product_analytic_vertical.xCameraAccessory + 
                        product_analytic_vertical.xCameraBag + 
                        product_analytic_vertical.xCameraBattery + 
                        product_analytic_vertical.xCameraBatteryCharger + 
                        product_analytic_vertical.xCameraBatteryGrip + 
                        product_analytic_vertical.xCameraEyeCup + 
                        product_analytic_vertical.xCameraFilmRolls + 
                        product_analytic_vertical.xCameraHousing + 
                        product_analytic_vertical.xCameraLEDLight + 
                        product_analytic_vertical.xCameraMicrophone + 
                        product_analytic_vertical.xCameraMount + 
                        product_analytic_vertical.xCameraRemoteControl + 
                        product_analytic_vertical.xCameraTripod + 
                        product_analytic_vertical.xExtensionTube + 
                        product_analytic_vertical.xFilter + 
                        product_analytic_vertical.xFlash + 
                        product_analytic_vertical.xFlashShoeAdapter + 
                        product_analytic_vertical.xLens + 
                        product_analytic_vertical.xReflectorUmbrella + 
                        product_analytic_vertical.xSoftbox + 
                        product_analytic_vertical.xStrap + 
                        product_analytic_vertical.xTeleconverter + 
                        product_analytic_vertical.xTelescope + 
                        promotion_type.xChristmas...New.Year.Sale + 
                        promotion_type.xDaussera.sale + 
                        promotion_type.xNo_promotion, data = train)

summary(step_cam_model6) #Adjusted R-squared:  0.827
vif(step_cam_model6)

#Removed: Other_Adstock
step_cam_model7 <- lm(formula = tot_gmv ~ Total_Investment + 
                        Digital + 
                        Sponsorship + 
                        Content_Marketing + 
                        Affiliates + 
                        SEM + 
                        Content_Marketing_adstock + 
                        SEM_adstock + 
                        Radio_adstock + 
                        discount_over_mrp + 
                        product_analytic_vertical.xCameraAccessory + 
                        product_analytic_vertical.xCameraBag + 
                        product_analytic_vertical.xCameraBattery + 
                        product_analytic_vertical.xCameraBatteryCharger + 
                        product_analytic_vertical.xCameraBatteryGrip + 
                        product_analytic_vertical.xCameraEyeCup + 
                        product_analytic_vertical.xCameraFilmRolls + 
                        product_analytic_vertical.xCameraHousing + 
                        product_analytic_vertical.xCameraLEDLight + 
                        product_analytic_vertical.xCameraMicrophone + 
                        product_analytic_vertical.xCameraMount + 
                        product_analytic_vertical.xCameraRemoteControl + 
                        product_analytic_vertical.xCameraTripod + 
                        product_analytic_vertical.xExtensionTube + 
                        product_analytic_vertical.xFilter + 
                        product_analytic_vertical.xFlash + 
                        product_analytic_vertical.xFlashShoeAdapter + 
                        product_analytic_vertical.xLens + 
                        product_analytic_vertical.xReflectorUmbrella + 
                        product_analytic_vertical.xSoftbox + 
                        product_analytic_vertical.xStrap + 
                        product_analytic_vertical.xTeleconverter + 
                        product_analytic_vertical.xTelescope + 
                        promotion_type.xChristmas...New.Year.Sale + 
                        promotion_type.xDaussera.sale + 
                        promotion_type.xNo_promotion, data = train)

summary(step_cam_model7) #Adjusted R-squared: 0.826
vif(step_cam_model7)

#Removed: Radio_adstock
step_cam_model8 <- lm(formula = tot_gmv ~ Total_Investment + 
                        Digital + 
                        Sponsorship + 
                        Content_Marketing + 
                        Affiliates + 
                        SEM + 
                        Content_Marketing_adstock + 
                        SEM_adstock + 
                        discount_over_mrp + 
                        product_analytic_vertical.xCameraAccessory + 
                        product_analytic_vertical.xCameraBag + 
                        product_analytic_vertical.xCameraBattery + 
                        product_analytic_vertical.xCameraBatteryCharger + 
                        product_analytic_vertical.xCameraBatteryGrip + 
                        product_analytic_vertical.xCameraEyeCup + 
                        product_analytic_vertical.xCameraFilmRolls + 
                        product_analytic_vertical.xCameraHousing + 
                        product_analytic_vertical.xCameraLEDLight + 
                        product_analytic_vertical.xCameraMicrophone + 
                        product_analytic_vertical.xCameraMount + 
                        product_analytic_vertical.xCameraRemoteControl + 
                        product_analytic_vertical.xCameraTripod + 
                        product_analytic_vertical.xExtensionTube + 
                        product_analytic_vertical.xFilter + 
                        product_analytic_vertical.xFlash + 
                        product_analytic_vertical.xFlashShoeAdapter + 
                        product_analytic_vertical.xLens + 
                        product_analytic_vertical.xReflectorUmbrella + 
                        product_analytic_vertical.xSoftbox + 
                        product_analytic_vertical.xStrap + 
                        product_analytic_vertical.xTeleconverter + 
                        product_analytic_vertical.xTelescope + 
                        promotion_type.xChristmas...New.Year.Sale + 
                        promotion_type.xDaussera.sale + 
                        promotion_type.xNo_promotion, data = train)

summary(step_cam_model8) #Adjusted R-squared: 0.825  
vif(step_cam_model8)

# Removed: SEM_Adstock and Content_Marketing_Adstock
step_cam_model9 <- lm(formula = tot_gmv ~ Total_Investment + 
                        Digital + 
                        Sponsorship + 
                        Content_Marketing + 
                        Affiliates + 
                        SEM + 
                        discount_over_mrp + 
                        product_analytic_vertical.xCameraAccessory + 
                        product_analytic_vertical.xCameraBag + 
                        product_analytic_vertical.xCameraBattery + 
                        product_analytic_vertical.xCameraBatteryCharger + 
                        product_analytic_vertical.xCameraBatteryGrip + 
                        product_analytic_vertical.xCameraEyeCup + 
                        product_analytic_vertical.xCameraFilmRolls + 
                        product_analytic_vertical.xCameraHousing + 
                        product_analytic_vertical.xCameraLEDLight + 
                        product_analytic_vertical.xCameraMicrophone + 
                        product_analytic_vertical.xCameraMount + 
                        product_analytic_vertical.xCameraRemoteControl + 
                        product_analytic_vertical.xCameraTripod + 
                        product_analytic_vertical.xExtensionTube + 
                        product_analytic_vertical.xFilter + 
                        product_analytic_vertical.xFlash + 
                        product_analytic_vertical.xFlashShoeAdapter + 
                        product_analytic_vertical.xLens + 
                        product_analytic_vertical.xReflectorUmbrella + 
                        product_analytic_vertical.xSoftbox + 
                        product_analytic_vertical.xStrap + 
                        product_analytic_vertical.xTeleconverter + 
                        product_analytic_vertical.xTelescope + 
                        promotion_type.xChristmas...New.Year.Sale + 
                        promotion_type.xDaussera.sale + 
                        promotion_type.xNo_promotion, data = train)

summary(step_cam_model9) #Adjusted R-squared: 0.825  
vif(step_cam_model9)

#Removed: Digital (high P VALUE)
step_cam_model10 <- lm(formula = tot_gmv ~ Total_Investment + 
                        Sponsorship + 
                        Content_Marketing + 
                        Affiliates + 
                        SEM + 
                        discount_over_mrp + 
                        product_analytic_vertical.xCameraAccessory + 
                        product_analytic_vertical.xCameraBag + 
                        product_analytic_vertical.xCameraBattery + 
                        product_analytic_vertical.xCameraBatteryCharger + 
                        product_analytic_vertical.xCameraBatteryGrip + 
                        product_analytic_vertical.xCameraEyeCup + 
                        product_analytic_vertical.xCameraFilmRolls + 
                        product_analytic_vertical.xCameraHousing + 
                        product_analytic_vertical.xCameraLEDLight + 
                        product_analytic_vertical.xCameraMicrophone + 
                        product_analytic_vertical.xCameraMount + 
                        product_analytic_vertical.xCameraRemoteControl + 
                        product_analytic_vertical.xCameraTripod + 
                        product_analytic_vertical.xExtensionTube + 
                        product_analytic_vertical.xFilter + 
                        product_analytic_vertical.xFlash + 
                        product_analytic_vertical.xFlashShoeAdapter + 
                        product_analytic_vertical.xLens + 
                        product_analytic_vertical.xReflectorUmbrella + 
                        product_analytic_vertical.xSoftbox + 
                        product_analytic_vertical.xStrap + 
                        product_analytic_vertical.xTeleconverter + 
                        product_analytic_vertical.xTelescope + 
                        promotion_type.xChristmas...New.Year.Sale + 
                        promotion_type.xDaussera.sale + 
                        promotion_type.xNo_promotion, data = train)

summary(step_cam_model10) #Adjusted R-squared: 0.824  
vif(step_cam_model10)

#Removed: product_analytic_vertical.xFlashShoeAdapter 
step_cam_model11 <- lm(formula = tot_gmv ~ Total_Investment + 
                         Sponsorship + 
                         Content_Marketing + 
                         Affiliates + 
                         SEM + 
                         discount_over_mrp + 
                         product_analytic_vertical.xCameraAccessory + 
                         product_analytic_vertical.xCameraBag + 
                         product_analytic_vertical.xCameraBattery + 
                         product_analytic_vertical.xCameraBatteryCharger + 
                         product_analytic_vertical.xCameraBatteryGrip + 
                         product_analytic_vertical.xCameraEyeCup + 
                         product_analytic_vertical.xCameraFilmRolls + 
                         product_analytic_vertical.xCameraHousing + 
                         product_analytic_vertical.xCameraLEDLight + 
                         product_analytic_vertical.xCameraMicrophone + 
                         product_analytic_vertical.xCameraMount + 
                         product_analytic_vertical.xCameraRemoteControl + 
                         product_analytic_vertical.xCameraTripod + 
                         product_analytic_vertical.xExtensionTube + 
                         product_analytic_vertical.xFilter + 
                         product_analytic_vertical.xFlash + 
                         product_analytic_vertical.xLens + 
                         product_analytic_vertical.xReflectorUmbrella + 
                         product_analytic_vertical.xSoftbox + 
                         product_analytic_vertical.xStrap + 
                         product_analytic_vertical.xTeleconverter + 
                         product_analytic_vertical.xTelescope + 
                         promotion_type.xChristmas...New.Year.Sale + 
                         promotion_type.xDaussera.sale + 
                         promotion_type.xNo_promotion, data = train)

summary(step_cam_model11) #Adjusted R-squared: 0.823  
vif(step_cam_model11)

#Removed: discount_over_MRP
step_cam_model12 <- lm(formula = tot_gmv ~ Total_Investment + 
                         Sponsorship + 
                         Content_Marketing + 
                         Affiliates + 
                         SEM + 
                         product_analytic_vertical.xCameraAccessory + 
                         product_analytic_vertical.xCameraBag + 
                         product_analytic_vertical.xCameraBattery + 
                         product_analytic_vertical.xCameraBatteryCharger + 
                         product_analytic_vertical.xCameraBatteryGrip + 
                         product_analytic_vertical.xCameraEyeCup + 
                         product_analytic_vertical.xCameraFilmRolls + 
                         product_analytic_vertical.xCameraHousing + 
                         product_analytic_vertical.xCameraLEDLight + 
                         product_analytic_vertical.xCameraMicrophone + 
                         product_analytic_vertical.xCameraMount + 
                         product_analytic_vertical.xCameraRemoteControl + 
                         product_analytic_vertical.xCameraTripod + 
                         product_analytic_vertical.xExtensionTube + 
                         product_analytic_vertical.xFilter + 
                         product_analytic_vertical.xFlash + 
                         product_analytic_vertical.xLens + 
                         product_analytic_vertical.xReflectorUmbrella + 
                         product_analytic_vertical.xSoftbox + 
                         product_analytic_vertical.xStrap + 
                         product_analytic_vertical.xTeleconverter + 
                         product_analytic_vertical.xTelescope + 
                         promotion_type.xChristmas...New.Year.Sale + 
                         promotion_type.xDaussera.sale + 
                         promotion_type.xNo_promotion, data = train)

summary(step_cam_model12) #Adjusted R-squared: 0.822  
vif(step_cam_model12)

#Removed product_analytic_vertical.xCameraBag
step_cam_model13 <- lm(formula = tot_gmv ~ Total_Investment + 
                         Sponsorship + 
                         Content_Marketing + 
                         Affiliates + 
                         SEM + 
                         product_analytic_vertical.xCameraAccessory + 
                         product_analytic_vertical.xCameraBattery + 
                         product_analytic_vertical.xCameraBatteryCharger + 
                         product_analytic_vertical.xCameraBatteryGrip + 
                         product_analytic_vertical.xCameraEyeCup + 
                         product_analytic_vertical.xCameraFilmRolls + 
                         product_analytic_vertical.xCameraHousing + 
                         product_analytic_vertical.xCameraLEDLight + 
                         product_analytic_vertical.xCameraMicrophone + 
                         product_analytic_vertical.xCameraMount + 
                         product_analytic_vertical.xCameraRemoteControl + 
                         product_analytic_vertical.xCameraTripod + 
                         product_analytic_vertical.xExtensionTube + 
                         product_analytic_vertical.xFilter + 
                         product_analytic_vertical.xFlash + 
                         product_analytic_vertical.xLens + 
                         product_analytic_vertical.xReflectorUmbrella + 
                         product_analytic_vertical.xSoftbox + 
                         product_analytic_vertical.xStrap + 
                         product_analytic_vertical.xTeleconverter + 
                         product_analytic_vertical.xTelescope + 
                         promotion_type.xChristmas...New.Year.Sale + 
                         promotion_type.xDaussera.sale + 
                         promotion_type.xNo_promotion, data = train)

summary(step_cam_model13) #Adjusted R-squared: 0.821  
vif(step_cam_model13)

#Removed: product_analytic_vertical.xCameraTripod
step_cam_model14 <- lm(formula = tot_gmv ~ Total_Investment + 
                         Sponsorship + 
                         Content_Marketing + 
                         Affiliates + 
                         SEM + 
                         product_analytic_vertical.xCameraAccessory + 
                         product_analytic_vertical.xCameraBattery + 
                         product_analytic_vertical.xCameraBatteryCharger + 
                         product_analytic_vertical.xCameraBatteryGrip + 
                         product_analytic_vertical.xCameraEyeCup + 
                         product_analytic_vertical.xCameraFilmRolls + 
                         product_analytic_vertical.xCameraHousing + 
                         product_analytic_vertical.xCameraLEDLight + 
                         product_analytic_vertical.xCameraMicrophone + 
                         product_analytic_vertical.xCameraMount + 
                         product_analytic_vertical.xCameraRemoteControl + 
                         product_analytic_vertical.xExtensionTube + 
                         product_analytic_vertical.xFilter + 
                         product_analytic_vertical.xFlash + 
                         product_analytic_vertical.xLens + 
                         product_analytic_vertical.xReflectorUmbrella + 
                         product_analytic_vertical.xSoftbox + 
                         product_analytic_vertical.xStrap + 
                         product_analytic_vertical.xTeleconverter + 
                         product_analytic_vertical.xTelescope + 
                         promotion_type.xChristmas...New.Year.Sale + 
                         promotion_type.xDaussera.sale + 
                         promotion_type.xNo_promotion, data = train)

summary(step_cam_model14) #Adjusted R-squared: 0.821  
vif(step_cam_model14)

#Removed: product_analytic_vertical.xCameraBattery
step_cam_model15 <- lm(formula = tot_gmv ~ Total_Investment + 
                         Sponsorship + 
                         Content_Marketing + 
                         Affiliates + 
                         SEM + 
                         product_analytic_vertical.xCameraAccessory + 
                         product_analytic_vertical.xCameraBatteryCharger + 
                         product_analytic_vertical.xCameraBatteryGrip + 
                         product_analytic_vertical.xCameraEyeCup + 
                         product_analytic_vertical.xCameraFilmRolls + 
                         product_analytic_vertical.xCameraHousing + 
                         product_analytic_vertical.xCameraLEDLight + 
                         product_analytic_vertical.xCameraMicrophone + 
                         product_analytic_vertical.xCameraMount + 
                         product_analytic_vertical.xCameraRemoteControl + 
                         product_analytic_vertical.xExtensionTube + 
                         product_analytic_vertical.xFilter + 
                         product_analytic_vertical.xFlash + 
                         product_analytic_vertical.xLens + 
                         product_analytic_vertical.xReflectorUmbrella + 
                         product_analytic_vertical.xSoftbox + 
                         product_analytic_vertical.xStrap + 
                         product_analytic_vertical.xTeleconverter + 
                         product_analytic_vertical.xTelescope + 
                         promotion_type.xChristmas...New.Year.Sale + 
                         promotion_type.xDaussera.sale + 
                         promotion_type.xNo_promotion, data = train)

summary(step_cam_model15) #Adjusted R-squared: 0.82  
vif(step_cam_model15)

#Removed: product_analytic_vertical.xFlash
step_cam_model16 <- lm(formula = tot_gmv ~ Total_Investment + 
                         Sponsorship + 
                         Content_Marketing + 
                         Affiliates + 
                         SEM + 
                         product_analytic_vertical.xCameraAccessory + 
                         product_analytic_vertical.xCameraBatteryCharger + 
                         product_analytic_vertical.xCameraBatteryGrip + 
                         product_analytic_vertical.xCameraEyeCup + 
                         product_analytic_vertical.xCameraFilmRolls + 
                         product_analytic_vertical.xCameraHousing + 
                         product_analytic_vertical.xCameraLEDLight + 
                         product_analytic_vertical.xCameraMicrophone + 
                         product_analytic_vertical.xCameraMount + 
                         product_analytic_vertical.xCameraRemoteControl + 
                         product_analytic_vertical.xExtensionTube + 
                         product_analytic_vertical.xFilter + 
                         product_analytic_vertical.xLens + 
                         product_analytic_vertical.xReflectorUmbrella + 
                         product_analytic_vertical.xSoftbox + 
                         product_analytic_vertical.xStrap + 
                         product_analytic_vertical.xTeleconverter + 
                         product_analytic_vertical.xTelescope + 
                         promotion_type.xChristmas...New.Year.Sale + 
                         promotion_type.xDaussera.sale + 
                         promotion_type.xNo_promotion, data = train)

summary(step_cam_model16) #Adjusted R-squared: 0.82  
vif(step_cam_model16)

#Removed: product_analytic_vertical.xTeleconverter
step_cam_model17 <- lm(formula = tot_gmv ~ Total_Investment + 
                         Sponsorship + 
                         Content_Marketing + 
                         Affiliates + 
                         SEM + 
                         product_analytic_vertical.xCameraAccessory + 
                         product_analytic_vertical.xCameraBatteryCharger + 
                         product_analytic_vertical.xCameraBatteryGrip + 
                         product_analytic_vertical.xCameraEyeCup + 
                         product_analytic_vertical.xCameraFilmRolls + 
                         product_analytic_vertical.xCameraHousing + 
                         product_analytic_vertical.xCameraLEDLight + 
                         product_analytic_vertical.xCameraMicrophone + 
                         product_analytic_vertical.xCameraMount + 
                         product_analytic_vertical.xCameraRemoteControl + 
                         product_analytic_vertical.xExtensionTube + 
                         product_analytic_vertical.xFilter + 
                         product_analytic_vertical.xLens + 
                         product_analytic_vertical.xReflectorUmbrella + 
                         product_analytic_vertical.xSoftbox + 
                         product_analytic_vertical.xStrap + 
                         product_analytic_vertical.xTelescope + 
                         promotion_type.xChristmas...New.Year.Sale + 
                         promotion_type.xDaussera.sale + 
                         promotion_type.xNo_promotion, data = train)

summary(step_cam_model17) #Adjusted R-squared: 0.818  
vif(step_cam_model17)

#Removed: Total_Investment (high VIF)
step_cam_model18 <- lm(formula = tot_gmv ~ Sponsorship + 
                         Content_Marketing + 
                         Affiliates + 
                         SEM + 
                         product_analytic_vertical.xCameraAccessory + 
                         product_analytic_vertical.xCameraBatteryCharger + 
                         product_analytic_vertical.xCameraBatteryGrip + 
                         product_analytic_vertical.xCameraEyeCup + 
                         product_analytic_vertical.xCameraFilmRolls + 
                         product_analytic_vertical.xCameraHousing + 
                         product_analytic_vertical.xCameraLEDLight + 
                         product_analytic_vertical.xCameraMicrophone + 
                         product_analytic_vertical.xCameraMount + 
                         product_analytic_vertical.xCameraRemoteControl + 
                         product_analytic_vertical.xExtensionTube + 
                         product_analytic_vertical.xFilter + 
                         product_analytic_vertical.xLens + 
                         product_analytic_vertical.xReflectorUmbrella + 
                         product_analytic_vertical.xSoftbox + 
                         product_analytic_vertical.xStrap + 
                         product_analytic_vertical.xTelescope + 
                         promotion_type.xChristmas...New.Year.Sale + 
                         promotion_type.xDaussera.sale + 
                         promotion_type.xNo_promotion, data = train)

summary(step_cam_model18) #Adjusted R-squared: 0.811  
vif(step_cam_model18)

#Removed Affiliate
step_cam_model19 <- lm(formula = tot_gmv ~ Sponsorship + 
                         Content_Marketing + 
                         SEM + 
                         product_analytic_vertical.xCameraAccessory + 
                         product_analytic_vertical.xCameraBatteryCharger + 
                         product_analytic_vertical.xCameraBatteryGrip + 
                         product_analytic_vertical.xCameraEyeCup + 
                         product_analytic_vertical.xCameraFilmRolls + 
                         product_analytic_vertical.xCameraHousing + 
                         product_analytic_vertical.xCameraLEDLight + 
                         product_analytic_vertical.xCameraMicrophone + 
                         product_analytic_vertical.xCameraMount + 
                         product_analytic_vertical.xCameraRemoteControl + 
                         product_analytic_vertical.xExtensionTube + 
                         product_analytic_vertical.xFilter + 
                         product_analytic_vertical.xLens + 
                         product_analytic_vertical.xReflectorUmbrella + 
                         product_analytic_vertical.xSoftbox + 
                         product_analytic_vertical.xStrap + 
                         product_analytic_vertical.xTelescope + 
                         promotion_type.xChristmas...New.Year.Sale + 
                         promotion_type.xDaussera.sale + 
                         promotion_type.xNo_promotion, data = train)

summary(step_cam_model19) #Adjusted R-squared: 0.811  
vif(step_cam_model19)

#Removed SEM
step_cam_model20 <- lm(formula = tot_gmv ~ Sponsorship + 
                         Content_Marketing + 
                         product_analytic_vertical.xCameraAccessory + 
                         product_analytic_vertical.xCameraBatteryCharger + 
                         product_analytic_vertical.xCameraBatteryGrip + 
                         product_analytic_vertical.xCameraEyeCup + 
                         product_analytic_vertical.xCameraFilmRolls + 
                         product_analytic_vertical.xCameraHousing + 
                         product_analytic_vertical.xCameraLEDLight + 
                         product_analytic_vertical.xCameraMicrophone + 
                         product_analytic_vertical.xCameraMount + 
                         product_analytic_vertical.xCameraRemoteControl + 
                         product_analytic_vertical.xExtensionTube + 
                         product_analytic_vertical.xFilter + 
                         product_analytic_vertical.xLens + 
                         product_analytic_vertical.xReflectorUmbrella + 
                         product_analytic_vertical.xSoftbox + 
                         product_analytic_vertical.xStrap + 
                         product_analytic_vertical.xTelescope + 
                         promotion_type.xChristmas...New.Year.Sale + 
                         promotion_type.xDaussera.sale + 
                         promotion_type.xNo_promotion, data = train)

summary(step_cam_model20) #Adjusted R-squared: 0.809
vif(step_cam_model20) #<-- Final Model

#Testing

gmv_prediction <- predict(step_cam_model20, test)
test$predicted_gmv <- gmv_prediction

cam_r <- cor(test$tot_gmv, test$predicted_gmv)
cam_rsquared <- cam_r^2
cam_rsquared
#R squared on test 0.699

############################################################################### Estimating elasticity ##################################


cam_final_model <- step_cam_model20

elasticity <- function(var) {
  x <- as.numeric(cam_final_model$coefficients[var])
  return(x)
}

var_list <- list()

for(i in 2:length(cam_final_model$coefficients)) {
  var_list[i-1] <- elasticity(names(cam_final_model$coefficients)[i])
}

elasticity.outputs <- data.frame(names(cam_final_model$coefficients[2:length(cam_final_model$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")

ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity,label = round(Elasticity,2))) +
  geom_bar(position="dodge",stat="identity", fill="grey") + 
  coord_flip() +
  ggtitle("Camera Accessory - Linear Model") +xlab("Variables")+
  geom_text(size = 3, position = position_stack(vjust = 0.5))



############################################################################### Cross validation ###################################



cam_lm_cv <- cv.lm(data = train, form.lm = step_cam_model20, m =10)

#ms = 0.192

###################################################################################################################################

########################################################################## Multiplicative Linear Model ############################



multi_camera <- CameraAccessory
multi_camera$week_no <- NULL
multi_camera$Month <- NULL


#Treatment of zero values
summary(multi_camera)
multi_camera$TV[which(multi_camera$TV == 0)] <- 0.01
multi_camera$Radio[which(multi_camera$Radio == 0)] <- 0.01
multi_camera$Other[which(multi_camera$Other == 0)] <- 0.01
multi_camera$Content_Marketing[which(multi_camera$Content_Marketing == 0)] <- 0.01

#Log of the numerical variables
multi_camera[,1:13] <- data.frame(sign(multi_camera[,1:13])*log(abs(multi_camera[,1:13])))

trainindices.m= sample(1:nrow(multi_camera), 0.6*nrow(multi_camera))
#Generate the train data set
train.m = multi_camera[trainindices.m,]
#Similarly store the rest of the observations into an object "test".
test.m = multi_camera[-trainindices.m,]

model.mul1 <- lm(tot_gmv~., data = train.m)
summary(model.mul1) #Adjusted R-squared:  0.858

model.mul2 <- stepAIC(model.mul1, direction = "both")
summary(model.mul2)  #Adjusted R-squared:  0.86
vif(model.mul2)

# Removed: promotion_type.xDaussera.sale
model.mul3<- lm(formula = tot_gmv ~ list_price + 
                  week + 
                  TV + 
                  Digital + 
                  Content_Marketing + 
                  Affiliates + 
                  SEM + 
                  TV_adstock + 
                  Digital_adstock + 
                  Sponsorship_adstock + 
                  Content_Marketing_adstock + 
                  Online_Marketing_adstock + 
                  Affiliates_adstock + 
                  Radio_adstock + 
                  discount_over_mrp + 
                  product_analytic_vertical.xCameraAccessory + 
                  product_analytic_vertical.xCameraBag + 
                  product_analytic_vertical.xCameraBattery + 
                  product_analytic_vertical.xCameraBatteryGrip + 
                  product_analytic_vertical.xCameraEyeCup + 
                  product_analytic_vertical.xCameraFilmRolls + 
                  product_analytic_vertical.xCameraHousing + 
                  product_analytic_vertical.xCameraLEDLight + 
                  product_analytic_vertical.xCameraMicrophone + 
                  product_analytic_vertical.xCameraMount + 
                  product_analytic_vertical.xCameraRemoteControl + 
                  product_analytic_vertical.xCameraTripod + 
                  product_analytic_vertical.xExtensionTube + 
                  product_analytic_vertical.xFlash + 
                  product_analytic_vertical.xFlashShoeAdapter + 
                  product_analytic_vertical.xLens + 
                  product_analytic_vertical.xReflectorUmbrella + 
                  product_analytic_vertical.xSoftbox + 
                  product_analytic_vertical.xStrap + 
                  product_analytic_vertical.xTeleconverter + 
                  product_analytic_vertical.xTelescope + 
                  promotion_type.xBig.Diwali.Sale + 
                  promotion_type.xBSD.5 + 
                  promotion_type.xChristmas...New.Year.Sale + 
                  promotion_type.xEid...Rathayatra.sale + 
                  promotion_type.xIndependence.Sale + 
                  promotion_type.xNo_promotion + 
                  promotion_type.xPacman + 
                  promotion_type.xRepublic.Day + 
                  promotion_type.xValentine.s.Day, data = train.m)

summary(model.mul3)      #Adjusted R-squared:  0.859
vif(model.mul3)

# Removed: promotion_type.xBig.Diwali.Sale
model.mul4<- lm(formula = tot_gmv ~ list_price + 
                  week + 
                  TV + 
                  Digital + 
                  Content_Marketing + 
                  Affiliates + 
                  SEM + 
                  TV_adstock + 
                  Digital_adstock + 
                  Sponsorship_adstock + 
                  Content_Marketing_adstock + 
                  Online_Marketing_adstock + 
                  Affiliates_adstock + 
                  Radio_adstock + 
                  discount_over_mrp + 
                  product_analytic_vertical.xCameraAccessory + 
                  product_analytic_vertical.xCameraBag + 
                  product_analytic_vertical.xCameraBattery + 
                  product_analytic_vertical.xCameraBatteryGrip + 
                  product_analytic_vertical.xCameraEyeCup + 
                  product_analytic_vertical.xCameraFilmRolls + 
                  product_analytic_vertical.xCameraHousing + 
                  product_analytic_vertical.xCameraLEDLight + 
                  product_analytic_vertical.xCameraMicrophone + 
                  product_analytic_vertical.xCameraMount + 
                  product_analytic_vertical.xCameraRemoteControl + 
                  product_analytic_vertical.xCameraTripod + 
                  product_analytic_vertical.xExtensionTube + 
                  product_analytic_vertical.xFlash + 
                  product_analytic_vertical.xFlashShoeAdapter + 
                  product_analytic_vertical.xLens + 
                  product_analytic_vertical.xReflectorUmbrella + 
                  product_analytic_vertical.xSoftbox + 
                  product_analytic_vertical.xStrap + 
                  product_analytic_vertical.xTeleconverter + 
                  product_analytic_vertical.xTelescope + 
                  promotion_type.xBSD.5 + 
                  promotion_type.xChristmas...New.Year.Sale + 
                  promotion_type.xEid...Rathayatra.sale + 
                  promotion_type.xIndependence.Sale + 
                  promotion_type.xNo_promotion + 
                  promotion_type.xPacman + 
                  promotion_type.xRepublic.Day + 
                  promotion_type.xValentine.s.Day, data = train.m)

summary(model.mul4)      #Adjusted R-squared:  0.859
vif(model.mul4)

#Removed: promotion_type.xBSD.5
#Removed: promotion_type.xEid...Rathayatra.sale 
model.mul5<- lm(formula = tot_gmv ~ list_price + 
                  week + 
                  TV + 
                  Digital + 
                  Content_Marketing + 
                  Affiliates + 
                  SEM + 
                  TV_adstock + 
                  Digital_adstock + 
                  Sponsorship_adstock + 
                  Content_Marketing_adstock + 
                  Online_Marketing_adstock + 
                  Affiliates_adstock + 
                  Radio_adstock + 
                  discount_over_mrp + 
                  product_analytic_vertical.xCameraAccessory + 
                  product_analytic_vertical.xCameraBag + 
                  product_analytic_vertical.xCameraBattery + 
                  product_analytic_vertical.xCameraBatteryGrip + 
                  product_analytic_vertical.xCameraEyeCup + 
                  product_analytic_vertical.xCameraFilmRolls + 
                  product_analytic_vertical.xCameraHousing + 
                  product_analytic_vertical.xCameraLEDLight + 
                  product_analytic_vertical.xCameraMicrophone + 
                  product_analytic_vertical.xCameraMount + 
                  product_analytic_vertical.xCameraRemoteControl + 
                  product_analytic_vertical.xCameraTripod + 
                  product_analytic_vertical.xExtensionTube + 
                  product_analytic_vertical.xFlash + 
                  product_analytic_vertical.xFlashShoeAdapter + 
                  product_analytic_vertical.xLens + 
                  product_analytic_vertical.xReflectorUmbrella + 
                  product_analytic_vertical.xSoftbox + 
                  product_analytic_vertical.xStrap + 
                  product_analytic_vertical.xTeleconverter + 
                  product_analytic_vertical.xTelescope + 
                  promotion_type.xChristmas...New.Year.Sale + 
                  promotion_type.xIndependence.Sale + 
                  promotion_type.xNo_promotion + 
                  promotion_type.xPacman + 
                  promotion_type.xRepublic.Day + 
                  promotion_type.xValentine.s.Day, data = train.m)

summary(model.mul5)      #Adjusted R-squared:  0.858
vif(model.mul5)

#Removed: promotion_type.xPacman
model.mul6<- lm(formula = tot_gmv ~ list_price + 
                  week + 
                  TV + 
                  Digital + 
                  Content_Marketing + 
                  Affiliates + 
                  SEM + 
                  TV_adstock + 
                  Digital_adstock + 
                  Sponsorship_adstock + 
                  Content_Marketing_adstock + 
                  Online_Marketing_adstock + 
                  Affiliates_adstock + 
                  Radio_adstock + 
                  discount_over_mrp + 
                  product_analytic_vertical.xCameraAccessory + 
                  product_analytic_vertical.xCameraBag + 
                  product_analytic_vertical.xCameraBattery + 
                  product_analytic_vertical.xCameraBatteryGrip + 
                  product_analytic_vertical.xCameraEyeCup + 
                  product_analytic_vertical.xCameraFilmRolls + 
                  product_analytic_vertical.xCameraHousing + 
                  product_analytic_vertical.xCameraLEDLight + 
                  product_analytic_vertical.xCameraMicrophone + 
                  product_analytic_vertical.xCameraMount + 
                  product_analytic_vertical.xCameraRemoteControl + 
                  product_analytic_vertical.xCameraTripod + 
                  product_analytic_vertical.xExtensionTube + 
                  product_analytic_vertical.xFlash + 
                  product_analytic_vertical.xFlashShoeAdapter + 
                  product_analytic_vertical.xLens + 
                  product_analytic_vertical.xReflectorUmbrella + 
                  product_analytic_vertical.xSoftbox + 
                  product_analytic_vertical.xStrap + 
                  product_analytic_vertical.xTeleconverter + 
                  product_analytic_vertical.xTelescope + 
                  promotion_type.xChristmas...New.Year.Sale + 
                  promotion_type.xIndependence.Sale + 
                  promotion_type.xNo_promotion + 
                  promotion_type.xRepublic.Day + 
                  promotion_type.xValentine.s.Day, data = train.m)

summary(model.mul6)      #Adjusted R-squared:  0.858
vif(model.mul6)

#Removed: promotion_type.xPacman
model.mul7<- lm(formula = tot_gmv ~ list_price + 
                  week + 
                  Digital + 
                  Content_Marketing + 
                  Affiliates + 
                  SEM + 
                  TV_adstock + 
                  Digital_adstock + 
                  Sponsorship_adstock + 
                  Content_Marketing_adstock + 
                  Online_Marketing_adstock + 
                  Affiliates_adstock + 
                  Radio_adstock + 
                  discount_over_mrp + 
                  product_analytic_vertical.xCameraAccessory + 
                  product_analytic_vertical.xCameraBag + 
                  product_analytic_vertical.xCameraBattery + 
                  product_analytic_vertical.xCameraBatteryGrip + 
                  product_analytic_vertical.xCameraEyeCup + 
                  product_analytic_vertical.xCameraFilmRolls + 
                  product_analytic_vertical.xCameraHousing + 
                  product_analytic_vertical.xCameraLEDLight + 
                  product_analytic_vertical.xCameraMicrophone + 
                  product_analytic_vertical.xCameraMount + 
                  product_analytic_vertical.xCameraRemoteControl + 
                  product_analytic_vertical.xCameraTripod + 
                  product_analytic_vertical.xExtensionTube + 
                  product_analytic_vertical.xFlash + 
                  product_analytic_vertical.xFlashShoeAdapter + 
                  product_analytic_vertical.xLens + 
                  product_analytic_vertical.xReflectorUmbrella + 
                  product_analytic_vertical.xSoftbox + 
                  product_analytic_vertical.xStrap + 
                  product_analytic_vertical.xTeleconverter + 
                  product_analytic_vertical.xTelescope + 
                  promotion_type.xChristmas...New.Year.Sale + 
                  promotion_type.xIndependence.Sale + 
                  promotion_type.xNo_promotion + 
                  promotion_type.xRepublic.Day + 
                  promotion_type.xValentine.s.Day, data = train.m)

summary(model.mul7)      #Adjusted R-squared:  0.857
vif(model.mul7)

#Removed: SEM
model.mul8<- lm(formula = tot_gmv ~ list_price + 
                  week + 
                  Digital + 
                  Content_Marketing + 
                  Affiliates + 
                  TV_adstock + 
                  Digital_adstock + 
                  Sponsorship_adstock + 
                  Content_Marketing_adstock + 
                  Online_Marketing_adstock + 
                  Affiliates_adstock + 
                  Radio_adstock + 
                  discount_over_mrp + 
                  product_analytic_vertical.xCameraAccessory + 
                  product_analytic_vertical.xCameraBag + 
                  product_analytic_vertical.xCameraBattery + 
                  product_analytic_vertical.xCameraBatteryGrip + 
                  product_analytic_vertical.xCameraEyeCup + 
                  product_analytic_vertical.xCameraFilmRolls + 
                  product_analytic_vertical.xCameraHousing + 
                  product_analytic_vertical.xCameraLEDLight + 
                  product_analytic_vertical.xCameraMicrophone + 
                  product_analytic_vertical.xCameraMount + 
                  product_analytic_vertical.xCameraRemoteControl + 
                  product_analytic_vertical.xCameraTripod + 
                  product_analytic_vertical.xExtensionTube + 
                  product_analytic_vertical.xFlash + 
                  product_analytic_vertical.xFlashShoeAdapter + 
                  product_analytic_vertical.xLens + 
                  product_analytic_vertical.xReflectorUmbrella + 
                  product_analytic_vertical.xSoftbox + 
                  product_analytic_vertical.xStrap + 
                  product_analytic_vertical.xTeleconverter + 
                  product_analytic_vertical.xTelescope + 
                  promotion_type.xChristmas...New.Year.Sale + 
                  promotion_type.xIndependence.Sale + 
                  promotion_type.xNo_promotion + 
                  promotion_type.xRepublic.Day + 
                  promotion_type.xValentine.s.Day, data = train.m)

summary(model.mul8)      #Adjusted R-squared:  0.857
vif(model.mul8)

# Remove: product_analytic_vertical.xCameraTripod, 
# promotion_type.xRepublic.Day, promotion_type.xValentine.s.Day 
model.mul9<- lm(formula = tot_gmv ~ list_price + 
                  week + 
                  Digital + 
                  Content_Marketing + 
                  Affiliates + 
                  TV_adstock + 
                  Digital_adstock + 
                  Sponsorship_adstock + 
                  Content_Marketing_adstock + 
                  Online_Marketing_adstock + 
                  Affiliates_adstock + 
                  Radio_adstock + 
                  discount_over_mrp + 
                  product_analytic_vertical.xCameraAccessory + 
                  product_analytic_vertical.xCameraBag + 
                  product_analytic_vertical.xCameraBattery + 
                  product_analytic_vertical.xCameraBatteryGrip + 
                  product_analytic_vertical.xCameraEyeCup + 
                  product_analytic_vertical.xCameraFilmRolls + 
                  product_analytic_vertical.xCameraHousing + 
                  product_analytic_vertical.xCameraLEDLight + 
                  product_analytic_vertical.xCameraMicrophone + 
                  product_analytic_vertical.xCameraMount + 
                  product_analytic_vertical.xCameraRemoteControl + 
                  product_analytic_vertical.xCameraTripod + 
                  product_analytic_vertical.xExtensionTube + 
                  product_analytic_vertical.xFlash + 
                  product_analytic_vertical.xFlashShoeAdapter + 
                  product_analytic_vertical.xLens + 
                  product_analytic_vertical.xReflectorUmbrella + 
                  product_analytic_vertical.xSoftbox + 
                  product_analytic_vertical.xStrap + 
                  product_analytic_vertical.xTeleconverter + 
                  product_analytic_vertical.xTelescope + 
                  promotion_type.xChristmas...New.Year.Sale + 
                  promotion_type.xIndependence.Sale + 
                  promotion_type.xNo_promotion
                  , data = train.m)

summary(model.mul9)      #Adjusted R-squared:  0.856
vif(model.mul9)

# Removed: Content_Marketing
model.mul10<- lm(formula = tot_gmv ~ list_price + 
                  week + 
                  Digital + 
                  Affiliates + 
                  TV_adstock + 
                  Digital_adstock + 
                  Sponsorship_adstock + 
                  Content_Marketing_adstock + 
                  Online_Marketing_adstock + 
                  Affiliates_adstock + 
                  Radio_adstock + 
                  discount_over_mrp + 
                  product_analytic_vertical.xCameraAccessory + 
                  product_analytic_vertical.xCameraBag + 
                  product_analytic_vertical.xCameraBattery + 
                  product_analytic_vertical.xCameraBatteryGrip + 
                  product_analytic_vertical.xCameraEyeCup + 
                  product_analytic_vertical.xCameraFilmRolls + 
                  product_analytic_vertical.xCameraHousing + 
                  product_analytic_vertical.xCameraLEDLight + 
                  product_analytic_vertical.xCameraMicrophone + 
                  product_analytic_vertical.xCameraMount + 
                  product_analytic_vertical.xCameraRemoteControl + 
                  product_analytic_vertical.xCameraTripod + 
                  product_analytic_vertical.xExtensionTube + 
                  product_analytic_vertical.xFlash + 
                  product_analytic_vertical.xFlashShoeAdapter + 
                  product_analytic_vertical.xLens + 
                  product_analytic_vertical.xReflectorUmbrella + 
                  product_analytic_vertical.xSoftbox + 
                  product_analytic_vertical.xStrap + 
                  product_analytic_vertical.xTeleconverter + 
                  product_analytic_vertical.xTelescope + 
                  promotion_type.xChristmas...New.Year.Sale + 
                  promotion_type.xIndependence.Sale + 
                  promotion_type.xNo_promotion
                , data = train.m)

summary(model.mul10)      #Adjusted R-squared:  0.855
vif(model.mul10)

#Removed: TV_adstock 
model.mul11<- lm(formula = tot_gmv ~ list_price + 
                   week + 
                   Digital + 
                   Affiliates + 
                   Digital_adstock + 
                   Sponsorship_adstock + 
                   Content_Marketing_adstock + 
                   Online_Marketing_adstock + 
                   Affiliates_adstock + 
                   Radio_adstock + 
                   discount_over_mrp + 
                   product_analytic_vertical.xCameraAccessory + 
                   product_analytic_vertical.xCameraBag + 
                   product_analytic_vertical.xCameraBattery + 
                   product_analytic_vertical.xCameraBatteryGrip + 
                   product_analytic_vertical.xCameraEyeCup + 
                   product_analytic_vertical.xCameraFilmRolls + 
                   product_analytic_vertical.xCameraHousing + 
                   product_analytic_vertical.xCameraLEDLight + 
                   product_analytic_vertical.xCameraMicrophone + 
                   product_analytic_vertical.xCameraMount + 
                   product_analytic_vertical.xCameraRemoteControl + 
                   product_analytic_vertical.xCameraTripod + 
                   product_analytic_vertical.xExtensionTube + 
                   product_analytic_vertical.xFlash + 
                   product_analytic_vertical.xFlashShoeAdapter + 
                   product_analytic_vertical.xLens + 
                   product_analytic_vertical.xReflectorUmbrella + 
                   product_analytic_vertical.xSoftbox + 
                   product_analytic_vertical.xStrap + 
                   product_analytic_vertical.xTeleconverter + 
                   product_analytic_vertical.xTelescope + 
                   promotion_type.xChristmas...New.Year.Sale + 
                   promotion_type.xIndependence.Sale + 
                   promotion_type.xNo_promotion
                 , data = train.m)

summary(model.mul11)      #Adjusted R-squared:  0.855
vif(model.mul11)

#Removed: product_analytic_vertical.xCameraTripod, 
#promotion_type.xChristmas...New.Year.Sale
model.mul12<- lm(formula = tot_gmv ~ list_price + 
                   week + 
                   Digital + 
                   Affiliates + 
                   Digital_adstock + 
                   Sponsorship_adstock + 
                   Content_Marketing_adstock + 
                   Online_Marketing_adstock + 
                   Affiliates_adstock + 
                   Radio_adstock + 
                   discount_over_mrp + 
                   product_analytic_vertical.xCameraAccessory + 
                   product_analytic_vertical.xCameraBag + 
                   product_analytic_vertical.xCameraBattery + 
                   product_analytic_vertical.xCameraBatteryGrip + 
                   product_analytic_vertical.xCameraEyeCup + 
                   product_analytic_vertical.xCameraFilmRolls + 
                   product_analytic_vertical.xCameraHousing + 
                   product_analytic_vertical.xCameraLEDLight + 
                   product_analytic_vertical.xCameraMicrophone + 
                   product_analytic_vertical.xCameraMount + 
                   product_analytic_vertical.xCameraRemoteControl + 
                   product_analytic_vertical.xExtensionTube + 
                   product_analytic_vertical.xFlash + 
                   product_analytic_vertical.xFlashShoeAdapter + 
                   product_analytic_vertical.xLens + 
                   product_analytic_vertical.xReflectorUmbrella + 
                   product_analytic_vertical.xSoftbox + 
                   product_analytic_vertical.xStrap + 
                   product_analytic_vertical.xTeleconverter + 
                   product_analytic_vertical.xTelescope + 
                   promotion_type.xIndependence.Sale + 
                   promotion_type.xNo_promotion
                 , data = train.m)

summary(model.mul12)      #Adjusted R-squared:  0.854
vif(model.mul12)


#Removed: product_analytic_vertical.xCameraBag
model.mul13<- lm(formula = tot_gmv ~ list_price + 
                   week + 
                   Digital + 
                   Affiliates + 
                   Digital_adstock + 
                   Sponsorship_adstock + 
                   Content_Marketing_adstock + 
                   Online_Marketing_adstock + 
                   Affiliates_adstock + 
                   Radio_adstock + 
                   discount_over_mrp + 
                   product_analytic_vertical.xCameraAccessory + 
                   product_analytic_vertical.xCameraBattery + 
                   product_analytic_vertical.xCameraBatteryGrip + 
                   product_analytic_vertical.xCameraEyeCup + 
                   product_analytic_vertical.xCameraFilmRolls + 
                   product_analytic_vertical.xCameraHousing + 
                   product_analytic_vertical.xCameraLEDLight + 
                   product_analytic_vertical.xCameraMicrophone + 
                   product_analytic_vertical.xCameraMount + 
                   product_analytic_vertical.xCameraRemoteControl + 
                   product_analytic_vertical.xExtensionTube + 
                   product_analytic_vertical.xFlash + 
                   product_analytic_vertical.xFlashShoeAdapter + 
                   product_analytic_vertical.xLens + 
                   product_analytic_vertical.xReflectorUmbrella + 
                   product_analytic_vertical.xSoftbox + 
                   product_analytic_vertical.xStrap + 
                   product_analytic_vertical.xTeleconverter + 
                   product_analytic_vertical.xTelescope + 
                   promotion_type.xIndependence.Sale + 
                   promotion_type.xNo_promotion
                 , data = train.m)

summary(model.mul13)      #Adjusted R-squared:  0.852
vif(model.mul13)

#Removed: Online_Marketing_adstock (high VIF value)
#Removed: product_analytic_vertical.xCameraBag
model.mul14<- lm(formula = tot_gmv ~ list_price + 
                   week + 
                   Digital + 
                   Affiliates + 
                   Digital_adstock + 
                   Sponsorship_adstock + 
                   Content_Marketing_adstock + 
                   Affiliates_adstock + 
                   Radio_adstock + 
                   discount_over_mrp + 
                   product_analytic_vertical.xCameraAccessory + 
                   product_analytic_vertical.xCameraBattery + 
                   product_analytic_vertical.xCameraBatteryGrip + 
                   product_analytic_vertical.xCameraEyeCup + 
                   product_analytic_vertical.xCameraFilmRolls + 
                   product_analytic_vertical.xCameraHousing + 
                   product_analytic_vertical.xCameraLEDLight + 
                   product_analytic_vertical.xCameraMicrophone + 
                   product_analytic_vertical.xCameraMount + 
                   product_analytic_vertical.xCameraRemoteControl + 
                   product_analytic_vertical.xExtensionTube + 
                   product_analytic_vertical.xFlash + 
                   product_analytic_vertical.xFlashShoeAdapter + 
                   product_analytic_vertical.xLens + 
                   product_analytic_vertical.xReflectorUmbrella + 
                   product_analytic_vertical.xSoftbox + 
                   product_analytic_vertical.xStrap + 
                   product_analytic_vertical.xTeleconverter + 
                   product_analytic_vertical.xTelescope + 
                   promotion_type.xIndependence.Sale + 
                   promotion_type.xNo_promotion
                 , data = train.m)

summary(model.mul14)      #Adjusted R-squared:  0.85
vif(model.mul14)

#Remove Affiliates
model.mul15<- lm(formula = tot_gmv ~ list_price + 
                   week + 
                   Digital + 
                   Digital_adstock + 
                   Sponsorship_adstock + 
                   Content_Marketing_adstock + 
                   Affiliates_adstock + 
                   Radio_adstock + 
                   discount_over_mrp + 
                   product_analytic_vertical.xCameraAccessory + 
                   product_analytic_vertical.xCameraBattery + 
                   product_analytic_vertical.xCameraBatteryGrip + 
                   product_analytic_vertical.xCameraEyeCup + 
                   product_analytic_vertical.xCameraFilmRolls + 
                   product_analytic_vertical.xCameraHousing + 
                   product_analytic_vertical.xCameraLEDLight + 
                   product_analytic_vertical.xCameraMicrophone + 
                   product_analytic_vertical.xCameraMount + 
                   product_analytic_vertical.xCameraRemoteControl + 
                   product_analytic_vertical.xExtensionTube + 
                   product_analytic_vertical.xFlash + 
                   product_analytic_vertical.xFlashShoeAdapter + 
                   product_analytic_vertical.xLens + 
                   product_analytic_vertical.xReflectorUmbrella + 
                   product_analytic_vertical.xSoftbox + 
                   product_analytic_vertical.xStrap + 
                   product_analytic_vertical.xTeleconverter + 
                   product_analytic_vertical.xTelescope + 
                   promotion_type.xIndependence.Sale + 
                   promotion_type.xNo_promotion
                 , data = train.m)

summary(model.mul15)      #Adjusted R-squared:   0.849
vif(model.mul15)

#Removed: Affiliates_adstock
model.mul16<- lm(formula = tot_gmv ~ list_price + 
                   week + 
                   Digital + 
                   Digital_adstock + 
                   Sponsorship_adstock + 
                   Content_Marketing_adstock + 
                   Radio_adstock + 
                   discount_over_mrp + 
                   product_analytic_vertical.xCameraAccessory + 
                   product_analytic_vertical.xCameraBattery + 
                   product_analytic_vertical.xCameraBatteryGrip + 
                   product_analytic_vertical.xCameraEyeCup + 
                   product_analytic_vertical.xCameraFilmRolls + 
                   product_analytic_vertical.xCameraHousing + 
                   product_analytic_vertical.xCameraLEDLight + 
                   product_analytic_vertical.xCameraMicrophone + 
                   product_analytic_vertical.xCameraMount + 
                   product_analytic_vertical.xCameraRemoteControl + 
                   product_analytic_vertical.xExtensionTube + 
                   product_analytic_vertical.xFlash + 
                   product_analytic_vertical.xFlashShoeAdapter + 
                   product_analytic_vertical.xLens + 
                   product_analytic_vertical.xReflectorUmbrella + 
                   product_analytic_vertical.xSoftbox + 
                   product_analytic_vertical.xStrap + 
                   product_analytic_vertical.xTeleconverter + 
                   product_analytic_vertical.xTelescope + 
                   promotion_type.xIndependence.Sale + 
                   promotion_type.xNo_promotion
                 , data = train.m)

summary(model.mul16)      #Adjusted R-squared:   0.848
vif(model.mul16)

#Removed: Content_Marketing_adstock
model.mul17<- lm(formula = tot_gmv ~ list_price + 
                   week + 
                   Digital + 
                   Digital_adstock + 
                   Sponsorship_adstock + 
                   Radio_adstock + 
                   discount_over_mrp + 
                   product_analytic_vertical.xCameraAccessory + 
                   product_analytic_vertical.xCameraBattery + 
                   product_analytic_vertical.xCameraBatteryGrip + 
                   product_analytic_vertical.xCameraEyeCup + 
                   product_analytic_vertical.xCameraFilmRolls + 
                   product_analytic_vertical.xCameraHousing + 
                   product_analytic_vertical.xCameraLEDLight + 
                   product_analytic_vertical.xCameraMicrophone + 
                   product_analytic_vertical.xCameraMount + 
                   product_analytic_vertical.xCameraRemoteControl + 
                   product_analytic_vertical.xExtensionTube + 
                   product_analytic_vertical.xFlash + 
                   product_analytic_vertical.xFlashShoeAdapter + 
                   product_analytic_vertical.xLens + 
                   product_analytic_vertical.xReflectorUmbrella + 
                   product_analytic_vertical.xSoftbox + 
                   product_analytic_vertical.xStrap + 
                   product_analytic_vertical.xTeleconverter + 
                   product_analytic_vertical.xTelescope + 
                   promotion_type.xIndependence.Sale + 
                   promotion_type.xNo_promotion
                 , data = train.m)

summary(model.mul17)      #Adjusted R-squared:   0.827
vif(model.mul17)

#Removed: Digital 
model.mul18<- lm(formula = tot_gmv ~ list_price + 
                   week + 
                   Digital_adstock + 
                   Sponsorship_adstock + 
                   Radio_adstock + 
                   discount_over_mrp + 
                   product_analytic_vertical.xCameraAccessory + 
                   product_analytic_vertical.xCameraBattery + 
                   product_analytic_vertical.xCameraBatteryGrip + 
                   product_analytic_vertical.xCameraEyeCup + 
                   product_analytic_vertical.xCameraFilmRolls + 
                   product_analytic_vertical.xCameraHousing + 
                   product_analytic_vertical.xCameraLEDLight + 
                   product_analytic_vertical.xCameraMicrophone + 
                   product_analytic_vertical.xCameraMount + 
                   product_analytic_vertical.xCameraRemoteControl + 
                   product_analytic_vertical.xExtensionTube + 
                   product_analytic_vertical.xFlash + 
                   product_analytic_vertical.xFlashShoeAdapter + 
                   product_analytic_vertical.xLens + 
                   product_analytic_vertical.xReflectorUmbrella + 
                   product_analytic_vertical.xSoftbox + 
                   product_analytic_vertical.xStrap + 
                   product_analytic_vertical.xTeleconverter + 
                   product_analytic_vertical.xTelescope + 
                   promotion_type.xIndependence.Sale + 
                   promotion_type.xNo_promotion
                 , data = train.m)

summary(model.mul18)      #Adjusted R-squared:   0.827
vif(model.mul18)

#Removed: Digital_adstock, sponsorship_adstock
model.mul19<- lm(formula = tot_gmv ~ list_price + 
                   week + 
                   Radio_adstock + 
                   discount_over_mrp + 
                   product_analytic_vertical.xCameraAccessory + 
                   product_analytic_vertical.xCameraBattery + 
                   product_analytic_vertical.xCameraBatteryGrip + 
                   product_analytic_vertical.xCameraEyeCup + 
                   product_analytic_vertical.xCameraFilmRolls + 
                   product_analytic_vertical.xCameraHousing + 
                   product_analytic_vertical.xCameraLEDLight + 
                   product_analytic_vertical.xCameraMicrophone + 
                   product_analytic_vertical.xCameraMount + 
                   product_analytic_vertical.xCameraRemoteControl + 
                   product_analytic_vertical.xExtensionTube + 
                   product_analytic_vertical.xFlash + 
                   product_analytic_vertical.xFlashShoeAdapter + 
                   product_analytic_vertical.xLens + 
                   product_analytic_vertical.xReflectorUmbrella + 
                   product_analytic_vertical.xSoftbox + 
                   product_analytic_vertical.xStrap + 
                   product_analytic_vertical.xTeleconverter + 
                   product_analytic_vertical.xTelescope + 
                   promotion_type.xIndependence.Sale + 
                   promotion_type.xNo_promotion
                 , data = train.m)

summary(model.mul19)      #Adjusted R-squared:   0.828
vif(model.mul19)

#Removed: product_analytic_vertical.xCameraFilmRolls
model.mul20<- lm(formula = tot_gmv ~ list_price + 
                   week + 
                   Radio_adstock + 
                   discount_over_mrp + 
                   product_analytic_vertical.xCameraAccessory + 
                   product_analytic_vertical.xCameraBattery + 
                   product_analytic_vertical.xCameraBatteryGrip + 
                   product_analytic_vertical.xCameraEyeCup + 
                   product_analytic_vertical.xCameraHousing + 
                   product_analytic_vertical.xCameraLEDLight + 
                   product_analytic_vertical.xCameraMicrophone + 
                   product_analytic_vertical.xCameraMount + 
                   product_analytic_vertical.xCameraRemoteControl + 
                   product_analytic_vertical.xExtensionTube + 
                   product_analytic_vertical.xFlash + 
                   product_analytic_vertical.xFlashShoeAdapter + 
                   product_analytic_vertical.xLens + 
                   product_analytic_vertical.xReflectorUmbrella + 
                   product_analytic_vertical.xSoftbox + 
                   product_analytic_vertical.xStrap + 
                   product_analytic_vertical.xTeleconverter + 
                   product_analytic_vertical.xTelescope + 
                   promotion_type.xIndependence.Sale + 
                   promotion_type.xNo_promotion
                 , data = train.m)

summary(model.mul20)      #Adjusted R-squared:   0.828
vif(model.mul20)



#Testing
predict_cam <- predict(model.mul20, test.m)
test.m$predicted_gmv <- predict_cam

# Now, we need to test the r square between actual and predicted sales.
cam_r <- cor(test.m$tot_gmv, test.m$predicted_gmv)
cam_rsquared <- cor(test.m$tot_gmv, test.m$predicted_gmv)^2
cam_rsquared
# Test Rsq 0.79

############################################################################### Estimating elasticity ##################################

cam.mul.model <- model.mul20

elasticity <- function(var) {
  x <- as.numeric(cam.mul.model$coefficients[var])
  return(x)
}

varlist.mul.cam <- list()

for(i in 2:length(cam.mul.model$coefficients)) {
  varlist.mul.cam[i-1] <- elasticity(names(cam.mul.model$coefficients)[i])
}

elasticity.cam.mul <- data.frame(names(cam.mul.model$coefficients[2:length(cam.mul.model$coefficients)]))
elasticity.cam.mul <- cbind(elasticity.cam.mul,do.call(rbind.data.frame, varlist.mul.cam))
colnames(elasticity.cam.mul) <- c("Variable","Elasticity")

elasticity.cam.mul$direction <- ifelse(elasticity.cam.mul$Elasticity > 0, "Positive", "Negative")

ggplot(data=elasticity.cam.mul, aes(x=reorder(Variable,Elasticity),y=Elasticity,label = round(Elasticity,2))) +
  geom_bar(position="dodge",stat="identity", fill="grey") + 
  coord_flip() +
  ggtitle("Camera Accessory - Multiplicative Model") +xlab("Variables")+
  geom_text(size = 3, position = position_stack(vjust = 0.5))


############################################################################### Cross validation ###################################

cam.mul.cv <- cv.lm(data = train.m , form.lm = model.mul20, m =10)
#ms 1.03 


########################################################################## Distributed Lag Model ##################################

#Create lag variables in the dataset needed for distributed lag model
weekly_order_med_inv_data_lag_cam <- lag_var_creation(weekly_order_med_inv_data,"CameraAccessory")

#Scaling numerical variables
weekly_order_med_inv_data_lag_cam [,c(1:35)] <- scale(weekly_order_med_inv_data_lag_cam [,c(1:35)])

#Dividing into Train and Test data
set.seed(200)


trainindices= sample(1:nrow(weekly_order_med_inv_data_lag_cam), 0.7*nrow(weekly_order_med_inv_data_lag_cam))
#Generate the train data set
train_cam = weekly_order_med_inv_data_lag_cam[trainindices,]

#Similarly store the rest of the observations into an object "test".
test_cam = weekly_order_med_inv_data_lag_cam[-trainindices,]



cam_model1 <- lm(tot_gmv~. , data = train_cam)
summary(cam_model1)


step_cam_model2 <- stepAIC(cam_model1, direction = "both")
summary(step_cam_model2)
vif(step_cam_model2)

step_cam_model3 <- lm(formula = tot_gmv ~ list_price + Total_Investment + TV + Sponsorship + 
                        Content_Marketing + Digital_adstock + SEM_adstock + discount_over_mrp + 
                        gmv_lag_1 + gmv_lag_3 + gmv_change_from_w2 + gmv_change_from_w3 + 
                        product_analytic_vertical.xCameraAccessory + product_analytic_vertical.xCameraBatteryCharger + 
                        product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                        product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                        product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                        product_analytic_vertical.xCameraRemoteControl + product_analytic_vertical.xCameraTripod + 
                        product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                        product_analytic_vertical.xFlash + product_analytic_vertical.xLens + 
                        product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                        product_analytic_vertical.xTelescope + promotion_type.xDaussera.sale + 
                        promotion_type.xPacman + promotion_type.xRakshabandhan.Sale + 
                        promotion_type.xValentine.s.Day, data = train_cam)


summary(step_cam_model3)
vif(step_cam_model3)



#Removing  Sponsorship, gmv_change_from_w3, SEM_adstock, Total_Investment, Content_Marketing, list_price, discount_over_mrp,TV, 
#gmv_change_from_w2, product_analytic_vertical.xCameraTripodone by one

step_cam_model4 <- lm(formula = tot_gmv ~ 
                        Digital_adstock + 
                        gmv_lag_1 + gmv_lag_3 +
                        product_analytic_vertical.xCameraAccessory + product_analytic_vertical.xCameraBatteryCharger + 
                        product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                        product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                        product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                        product_analytic_vertical.xCameraRemoteControl +  
                        product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                        product_analytic_vertical.xFlash + product_analytic_vertical.xLens + 
                        product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                        product_analytic_vertical.xTelescope + promotion_type.xDaussera.sale + 
                        promotion_type.xPacman + promotion_type.xRakshabandhan.Sale + 
                        promotion_type.xValentine.s.Day, data = train_cam)


summary(step_cam_model4)
vif(step_cam_model4)

# removed product_analytic_vertical.xFlash, product_analytic_vertical.xSoftbox ,gmv_lag_3, product_analytic_vertical.xCameraMicrophone,
#product_analytic_vertical.xCameraEyeCup, product_analytic_vertical.xCameraBatteryCharger, product_analytic_vertical.xFilter, product_analytic_vertical.xCameraFilmRolls,
#product_analytic_vertical.xStrap, product_analytic_vertical.xCameraBatteryGrip, product_analytic_vertical.xCameraMount one by one

step_cam_model5 <- lm(formula = tot_gmv ~ 
                        Digital_adstock + 
                        gmv_lag_1 + 
                        product_analytic_vertical.xCameraAccessory +
                        product_analytic_vertical.xCameraHousing + 
                        product_analytic_vertical.xCameraRemoteControl +  
                        product_analytic_vertical.xExtensionTube + 
                        product_analytic_vertical.xLens + 
                        promotion_type.xDaussera.sale + 
                        promotion_type.xPacman + promotion_type.xRakshabandhan.Sale + 
                        promotion_type.xValentine.s.Day, data = train_cam)


summary(step_cam_model5)
vif(step_cam_model5)


step_cam_model6 <- lm(formula = tot_gmv ~ gmv_lag_1 +product_analytic_vertical.xLens + 
                        promotion_type.xDaussera.sale + promotion_type.xRakshabandhan.Sale , data = train_cam)


summary(step_cam_model6)
vif(step_cam_model6)


#Adjusted R-squared:  0.69  

#Testing
gmv_cam_prediction <- predict(step_cam_model6, test_cam[,-3])
test_cam$predicted_gmv <- gmv_cam_prediction

cam_r <- cor(test_cam$tot_gmv, test_cam$predicted_gmv)
cam_rsquared <- cam_r^2
cam_rsquared
#Rsq = 0.618 on test data


############################################################################### Elasticity Measure ###################################


cam_final_model <- step_cam_model6

elasticity <- function(var) {
  x <- as.numeric(cam_final_model$coefficients[var] )
  return(x)
}

var_list <- list()

for(i in 2:length(cam_final_model$coefficients)) {
  var_list[i-1] <- elasticity(names(cam_final_model$coefficients)[i])
}

elasticity.outputs <- data.frame(names(cam_final_model$coefficients[2:length(cam_final_model$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")
elasticity.outputs <- arrange( elasticity.outputs , -Elasticity)

ggplot(data=elasticity.outputs[, ], aes(x=reorder(Variable,Elasticity),y=Elasticity,label = round(Elasticity,2))) +
  geom_bar(position="dodge",stat="identity", fill="grey") + 
  coord_flip() +
  ggtitle("Camera Accessory - Distributed Lag Model") +xlab("Variables")+
  geom_text(size = 3, position = position_stack(vjust = 0.5))


############################################################################### Cross validation ###################################

cam_lm_cv <- cv.lm(data = weekly_order_med_inv_data_lag_cam , form.lm = step_cam_model6, m =10)

#  ms  0.394

###################################################################################################################################

############################################################### Koyck Model #######################################################


str(CameraAccessory)
sum(is.na(CameraAccessory))
CameraAccessoryKoyck <- CameraAccessory

colnames(CameraAccessoryKoyck)

# Creating Lag variable 
CameraAccessoryKoyck <- slide(CameraAccessoryKoyck, Var = "tot_gmv",slideBy = -1)
str(CameraAccessoryKoyck)

Camera_koyck <- na.omit(CameraAccessoryKoyck)
Camera_koyck1 <- data.frame(scale(Camera_koyck[,c(1:26)]))
Camera_koyck2 <- data.frame(Camera_koyck[,27:87])
Camera_koyck3 <- data.frame(scale(Camera_koyck[,88]))

Camera_koyck <- cbind(Camera_koyck1, Camera_koyck2, Camera_koyck3)
str(Camera_koyck)

# removing month and year columns
Camera_koyck <- Camera_koyck[,-c(1,2)]
# splitting train & test sets
set.seed(200)
trainindices= sample(1:nrow(Camera_koyck), 0.7*nrow(Camera_koyck))
train_c = Camera_koyck[trainindices,]
test_c = Camera_koyck[-trainindices,]

## building first overall model
Koyck_model1_cam <- lm(tot_gmv~.,train_c)
summary(Koyck_model1)

# Evaluating the first models for significant predictors
Koyck_model2_cam <- stepAIC(Koyck_model1_cam,direction = "both")
summary(Koyck_model2_cam)
vif(Koyck_model2_cam)

Koyck_model3_cam <- lm(formula = tot_gmv ~ list_price + week + Total_Investment + 
                         Digital + Content_Marketing + Online_Marketing + Affiliates + 
                         Sponsorship_adstock + Content_Marketing_adstock + Online_Marketing_adstock + 
                         Affiliates_adstock + SEM_adstock + NPS + discount_over_mrp + 
                         product_analytic_vertical.xCameraAccessory + product_analytic_vertical.xCameraBag + 
                         product_analytic_vertical.xCameraBattery + product_analytic_vertical.xCameraBatteryCharger + 
                         product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                         product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                         product_analytic_vertical.xCameraLEDLight + product_analytic_vertical.xCameraMicrophone + 
                         product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                         product_analytic_vertical.xCameraTripod + product_analytic_vertical.xExtensionTube + 
                         product_analytic_vertical.xFilter + product_analytic_vertical.xFlash + 
                         product_analytic_vertical.xLens + product_analytic_vertical.xReflectorUmbrella + 
                         product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                         product_analytic_vertical.xTeleconverter + product_analytic_vertical.xTelescope + 
                         promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                         promotion_type.xNo_promotion + promotion_type.xRakshabandhan.Sale + 
                         promotion_type.xRepublic.Day, data = train_c)


summary(Koyck_model3_cam)
vif(Koyck_model3_cam)

#Removing variables list_price,  promotion_type.xNo_promotion, discount_over_mrp, product_analytic_vertical.xCameraBattery, product_analytic_vertical.xFlash,
#product_analytic_vertical.xCameraTripod, promotion_type.xRepublic.Day, promotion_type.xChristmas...New.Year.Sale, product_analytic_vertical.xCameraBag, 
#product_analytic_vertical.xReflectorUmbrella, SEM_adstock  one after another

Koyck_model4_cam<- lm(formula = tot_gmv ~ week + Total_Investment + 
                        Digital + Content_Marketing + Online_Marketing + Affiliates + 
                        Sponsorship_adstock + Content_Marketing_adstock + Online_Marketing_adstock + 
                        Affiliates_adstock + NPS +  
                        product_analytic_vertical.xCameraAccessory +  
                        product_analytic_vertical.xCameraBatteryCharger + 
                        product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                        product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                        product_analytic_vertical.xCameraLEDLight + product_analytic_vertical.xCameraMicrophone + 
                        product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                        product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter +  
                        product_analytic_vertical.xLens+
                        product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap +  product_analytic_vertical.xTelescope + 
                        promotion_type.xDaussera.sale + promotion_type.xRakshabandhan.Sale, data = train_c)

summary(Koyck_model4_cam)
vif(Koyck_model4_cam)

#Remove variables Online_Marketing_adstock, Online_Marketing, Digital, Affiliates_adstock one by one
Koyck_model5_cam<- lm(formula = tot_gmv ~ week + Total_Investment + Content_Marketing + Affiliates + 
                        Sponsorship_adstock + Content_Marketing_adstock + 
                        NPS +  product_analytic_vertical.xCameraAccessory +  
                        product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                        product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                        product_analytic_vertical.xCameraLEDLight + product_analytic_vertical.xCameraMicrophone + 
                        product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                        product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + product_analytic_vertical.xLens+
                        product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap +  product_analytic_vertical.xTelescope + 
                        promotion_type.xDaussera.sale + promotion_type.xRakshabandhan.Sale, data = train_c)
summary(Koyck_model5_cam)
vif(Koyck_model5_cam)

#Remove variables Total_Investment, Content_Marketing_adstock, Content_Marketing, Affiliates, Sponsorship_adstock, NPS one by one due to high vif
Koyck_model6_cam<- lm(formula = tot_gmv ~ week +product_analytic_vertical.xCameraAccessory +  
                        product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                        product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                        product_analytic_vertical.xCameraLEDLight + product_analytic_vertical.xCameraMicrophone + 
                        product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                        product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + product_analytic_vertical.xLens+
                        product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap +  product_analytic_vertical.xTelescope + 
                        promotion_type.xDaussera.sale + promotion_type.xRakshabandhan.Sale, data = train_c)
summary(Koyck_model6_cam)
vif(Koyck_model6_cam)

#	Adjusted R-squared:  0.794

final_koyck_camera <- Koyck_model6_cam

par(mfrow = c(2,2))
plot(final_koyck_camera, main = "Koyck Model- Camera Accesory")



# Testing
pred_koyck_cam <- predict(final_koyck_camera, test_c)
cam_r_koyck <- cor(test_c$tot_gmv, pred_koyck_cam)
cam_rsquared <- cam_r_koyck^2
cam_rsquared
#Rsquared on test: 0.734

############################################################################### Elasticity Measure ###################################

cam_final_model <- final_koyck_camera

elasticity <- function(var) {
  x <- as.numeric(cam_final_model$coefficients[var] * mean(train_c[,var])/mean(train_c$tot_gmv))
  return(x)
}

var_list <- list()

for(i in 2:length(cam_final_model$coefficients)) {
  var_list[i-1] <- elasticity(names(cam_final_model$coefficients)[i])
}

elasticity.outputs <- data.frame(names(cam_final_model$coefficients[2:length(cam_final_model$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")

ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity, label = round(Elasticity,2))) +
  geom_bar(position="dodge",stat="identity", fill="grey") + 
  coord_flip() +
  ggtitle("Camera Accessory - Koyck Model") +xlab("Variables")+
  geom_text(size = 3, position = position_stack(vjust = 0.5))

############################################################################### Cross validation ###################################

crossval <- cv.lm(data = train_c, form.lm = formula(final_koyck_camera),m = 10,main = "Cross Validation Camera Accessory Model")

# ms = 0.252 


###################################################################################################################################

############################################################### Lag Plus Multiplicative model #####################################

#Create lag variables in the dataset needed for Lag Plus Multiplicative model
weekly_data_lag_plus_mul_cam <- lag_var_creation(weekly_order_med_inv_data,"CameraAccessory")

weekly_data_lag_plus_mul_cam_tmp  <- as.data.frame(lapply (weekly_data_lag_plus_mul_cam[,1:35] ,function(x) ifelse ( x==0 , 0, log(abs(x)))*sign(x) ))
weekly_data_lag_plus_mul_cam <- data.frame( cbind( weekly_data_lag_plus_mul_cam_tmp ,weekly_data_lag_plus_mul_cam[,36:96] ))
weekly_data_lag_plus_mul_cam <- na.omit(weekly_data_lag_plus_mul_cam)

weekly_data_lag_plus_mul_cam [,c(1:35)] <- scale(weekly_data_lag_plus_mul_cam [,c(1:35)])

set.seed(200)

trainindices= sample(1:nrow(weekly_data_lag_plus_mul_cam), 0.7*nrow(weekly_data_lag_plus_mul_cam))
#Generate the train data set
train_lagmul_cam = weekly_data_lag_plus_mul_cam[trainindices,]

#Similarly store the rest of the observations into an object "test".
test_lagmul_cam = weekly_data_lag_plus_mul_cam[-trainindices,]

mul_lag_cam_mod1 <- lm(tot_gmv~. , data = train_lagmul_cam)
summary(cam_model1)


mul_lag_cam_mod1 <- stepAIC(cam_model1, direction = "both")
summary(mul_lag_cam_mod1)
vif (mul_lag_cam_mod1 )

mul_lag_cam_mod2 <- lm(formula = tot_gmv ~ list_price + Total_Investment + TV + Sponsorship + 
                         Content_Marketing + Digital_adstock + SEM_adstock + discount_over_mrp + 
                         gmv_lag_1 + gmv_lag_3 + gmv_change_from_w2 + gmv_change_from_w3 + 
                         product_analytic_vertical.xCameraAccessory + product_analytic_vertical.xCameraBatteryCharger + 
                         product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                         product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                         product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                         product_analytic_vertical.xCameraRemoteControl + product_analytic_vertical.xCameraTripod + 
                         product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                         product_analytic_vertical.xFlash + product_analytic_vertical.xLens + 
                         product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                         product_analytic_vertical.xTelescope + promotion_type.xDaussera.sale + 
                         promotion_type.xPacman + promotion_type.xRakshabandhan.Sale + 
                         promotion_type.xValentine.s.Day, data = train_cam)

summary(mul_lag_cam_mod2)
vif (mul_lag_cam_mod2 )




# Removed SEM_adstock, gmv_change_from_w3, Total_Investment, Content_Marketing, Sponsorship, product_analytic_vertical.xCameraFilmRolls, 
#list_price, product_analytic_vertical.xCameraTripod, gmv_lag_3, gmv_change_from_w2, TV, product_analytic_vertical.xFlash, product_analytic_vertical.xCameraBatteryCharger, product_analytic_vertical.xStrap one by one
mul_lag_cam_mod3 <- lm(formula = tot_gmv ~  
                         Digital_adstock + discount_over_mrp + 
                         gmv_lag_1 + 
                         product_analytic_vertical.xCameraAccessory +  
                         product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                         product_analytic_vertical.xCameraHousing + 
                         product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                         product_analytic_vertical.xCameraRemoteControl +  
                         product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                         product_analytic_vertical.xLens + 
                         product_analytic_vertical.xSoftbox + 
                         product_analytic_vertical.xTelescope + promotion_type.xDaussera.sale + 
                         promotion_type.xPacman + promotion_type.xRakshabandhan.Sale + 
                         promotion_type.xValentine.s.Day, data = train_cam)

summary(mul_lag_cam_mod3)
vif (mul_lag_cam_mod3 )



#Remove variables product_analytic_vertical.xCameraBatteryGrip, product_analytic_vertical.xTelescope, product_analytic_vertical.xCameraMount, 
#product_analytic_vertical.xCameraRemoteControl, Digital_adstock, product_analytic_vertical.xCameraMicrophone, product_analytic_vertical.xCameraAccessory, product_analytic_vertical.xSoftbox one by one
mul_lag_cam_mod4 <- lm(formula = tot_gmv ~  discount_over_mrp + gmv_lag_1 + 
                         product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraHousing + 
                         product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                         product_analytic_vertical.xLens + promotion_type.xDaussera.sale + 
                         promotion_type.xPacman + promotion_type.xRakshabandhan.Sale + promotion_type.xValentine.s.Day, data = train_cam)

summary(mul_lag_cam_mod4)
vif (mul_lag_cam_mod4 )

#Remove variables product_analytic_vertical.xExtensionTube, product_analytic_vertical.xCameraEyeCup, product_analytic_vertical.xCameraHousing,
#product_analytic_vertical.xFilter, promotion_type.xValentine.s.Day, promotion_type.xPacman one by one
mul_lag_cam_mod5 <- lm(formula = tot_gmv ~  discount_over_mrp + gmv_lag_1 + product_analytic_vertical.xLens + promotion_type.xDaussera.sale + 
                         promotion_type.xRakshabandhan.Sale, data = train_cam)

summary(mul_lag_cam_mod5)
vif (mul_lag_cam_mod5 )



#Adjusted R-squared:  0.697 

final_mul_lag_cam_model <- mul_lag_cam_mod5


#Testing

gmv_cam_prediction <- predict(final_mul_lag_cam_model, test_lagmul_cam[,-1])
test_lagmul_cam$predicted_gmv <- gmv_cam_prediction

cam_r <- cor(test_lagmul_cam$tot_gmv, test_lagmul_cam$predicted_gmv  )
cam_rsquared <- cam_r^2
cam_rsquared   
#Rsquareed on test = 0.534

############################################################################### Estimating elasticity ##################################

elasticity <- function(var) {
  x <- as.numeric(final_mul_lag_cam_model$coefficients[var] )
  return(x)
}

var_list <- list()

for(i in 2:length(final_mul_lag_cam_model$coefficients)) {
  var_list[i-1] <- elasticity(names(final_mul_lag_cam_model$coefficients)[i])
}

elasticity.outputs <- data.frame(names(final_mul_lag_cam_model$coefficients[2:length(final_mul_lag_cam_model$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")
elasticity.outputs <- arrange( elasticity.outputs , -Elasticity)

ggplot(data=elasticity.outputs[, ], aes(x=reorder(Variable,Elasticity),y=Elasticity,label = round(Elasticity,2))) +
  geom_bar(position="dodge",stat="identity", fill="grey") + 
  coord_flip() +
  ggtitle("Camera  Accessory -  Lag Distributed Multipicative  Model") +xlab("Variables")+
  geom_text(size = 3, position = position_stack(vjust = 0.5))


########################################################################## Cross validation ######################################

cam_lm_cv <- cv.lm(data = weekly_data_lag_plus_mul_cam , form.lm = final_mul_lag_cam_model, m =10)
#   ms  0.321

###################################################################################################################################

###################################################
#Camera Accesory				        #	Train	#	Test    #
###################################################
#Linear Model					          #	80.9	#	69.9    #
#Multiplicative model			      #	82.8	#	79.0    #
#Distrbuted Lag					        #	69.0  # 61.8    #
#koyck Model					          #	79.4	#	73.4    #
#Lag plus Multiplicative model	#	69.7  #	53.4    #
#                               #       #         #
# #################################################

