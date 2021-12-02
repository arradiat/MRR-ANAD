#uploading the different libraries
library('lubridate') # date and time
library('geosphere') # geospatial locations
library("glmnet")
library('alluvial') # visualisation
library('dplyr') # data manipulation
library('readr') # input/output
library('data.table') # data manipulation
library('tibble') # data wrangling
library('tidyr') # data wrangling
library('leaflet') # maps
library('leaflet.extras') # maps
library('maps') # maps
library('caret') # modelling
library('stringr') # string manipulation
library('forcats') # factor manipulation
library('ggplot2') # visualisation
library('scales') # visualisation
library('grid') # visualisation
library('RColorBrewer') # visualisation
library('corrplot') # visualisation
#uploading the data we use fread instead of read.table because its way faster
train <- as_tibble(fread("/Users/princessemame/APPRENTISSAGE AUTOMATIQUE /train.csv"))
#looking into the data with the summary function
as.table(summary(train))
#getting rid of the id column
train <- train[,-1]
#getting rid of extreme values for trip duration
train <- subset(train,train$trip_duration<21600)
train <-subset(train,train$trip_duration>=60)
#transforming the date and time into workable data and the vendor_id and passenger_count 
#into  factors ( for visualization purposes )
train <- train %>%  #this is a pipe meaning that you give the object train to the mutate function
  mutate(pickup_datetime = ymd_hms(pickup_datetime),
         dropoff_datetime = ymd_hms(dropoff_datetime),
         vendor_id = factor(vendor_id),
         passenger_count = factor(passenger_count))
#selecting randomly 8000 observation in our data set and placing them on the map
#of new york city to see the repartition of rides
set.seed(1234)
foo <- sample_n(train, 8e3)
leaflet(data = foo) %>% addProviderTiles("Esri.NatGeoWorldMap") %>%
  addCircleMarkers(~ pickup_longitude, ~pickup_latitude, radius = 1,
                   color = "deeppink", fillOpacity = 0.5)
#looking the distribution of the trip duration vector by plotting its histogram using ggplot 
train %>%
  ggplot(aes((trip_duration))) +
  geom_histogram(fill = "pink", bins = 150)
#looking the distribution of the trip duration vector by plotting the histogram of its logarithm using ggplot 
train %>%
  ggplot(aes(log(trip_duration))) +
  geom_histogram(fill = "cornflowerblue", bins = 150)
#visualisation of the number of trips for each of passenger_counts
p1 <- train %>%
  group_by(passenger_count) %>%
  count() %>%
  ggplot(aes(passenger_count, n, fill = passenger_count)) +# this give a color for each passenger count (which we already transformed into a factor)
  geom_col(fill=c("cornflowerblue","pink", "yellow", "orange","blue","deeppink","red","purple","brown")) +
  scale_y_sqrt() +# transforming the y_axis into the root square of counts of trips
  theme(legend.position = "none")+ labs(x = "number of passengers", y = "Total number of pickups") 
#visualisation of the number of trips with for each of vendors_id
p2 <- train %>%
  ggplot(aes(vendor_id, fill = vendor_id)) +
  geom_bar(fill=c("cornflowerblue","pink")) +
  theme(legend.position = "none")
#visualisation of the number of trips with for each flag status
p3 <- train %>%
  ggplot(aes(store_and_fwd_flag)) +
  geom_bar(fill=c("red","green")) +
  theme(legend.position = "none") +
  scale_y_log10()
#visualisation of the number of trips  for each  vendors_id according to the week day 
p4 <- train %>%
  mutate(wday = wday(pickup_datetime
  )) %>%
  group_by(wday, vendor_id) %>%# this mean for each day and each vendor id we sum the number of trips that hapened in that day 
  count() %>%
  ggplot(aes(wday, n, colour = vendor_id)) +
  geom_point(size = 3) +
  labs(x = "Day of the week", y = "Total number of pickups") +
  theme(legend.position = "none")
#visualisation of the number of trips  for each  vendors_id according to the hour of day 
p5 <- train %>%
  mutate(hpick = hour(pickup_datetime)) %>%
  group_by(hpick, vendor_id) %>%# this mean for each hour and each vendor id we sum the number of trips that hapened in that hour
  count() %>%
  ggplot(aes(hpick, n, color = vendor_id)) +
  geom_point(size = 3) +
  labs(x = "Hour of the day", y = "Total number of pickups") +
  theme(legend.position = "none")
p1;p2;p3;p4;p5
#visualisation of the number of trips  for each  vendors_id according to the hour of day for each month
p1 <- train %>%
  mutate(hpick = hour(pickup_datetime),
         Month = factor(month(pickup_datetime, label = TRUE))) %>%
  group_by(hpick, Month) %>%
  count() %>%
  ggplot(aes(hpick, n, color = Month)) +
  geom_line(size = 1.5) +
  labs(x = "Hour of the day", y = "count")
#visualisation of the number of trips  for each  vendors_id according to the hour of day for each week day
p2 <- train %>%
  mutate(hpick = hour(pickup_datetime),
         wday = factor(wday(pickup_datetime, label = TRUE, week_start = 1))) %>%
  group_by(hpick, wday) %>%
  count() %>%
  ggplot(aes(hpick, n, color = wday)) +
  geom_line(size = 1.5) +
  labs(x = "Hour of the day", y = "count")
p1;p2
#visualisation of the mean of trip duration accoridng to the week day
p1 <- train %>%
  mutate(wday = wday(pickup_datetime, label = TRUE, week_start = 1)) %>%
  group_by(wday, vendor_id) %>%
  summarise(mean_duration = mean(trip_duration)) %>%
  ggplot(aes(wday, mean_duration, color = vendor_id)) +
  geom_point(size = 4) +
  labs(x = "Day of the week", y = "Mean trip duration [s]")
#visualisation of the mean of trip duration accoridng to the hour of day
p2 <- train %>%
  mutate(hpick = hour(pickup_datetime)) %>%
  group_by(hpick, vendor_id) %>%
  summarise(mean_duration = mean(trip_duration)) %>%
  ggplot(aes(hpick, mean_duration, color = vendor_id)) +
  geom_point(size = 4) +
  labs(x = "Hour of the day", y = "Mean trip duration [s]") +
  theme(legend.position = "none")
p1;p2
#visualisation of the boxplot of duration accoridng for each vendor id 
train %>%
  ggplot(aes(passenger_count, trip_duration, color = passenger_count)) +
  geom_boxplot() +
  scale_y_log10() +
  theme(legend.position = "none") +
  facet_wrap(~ vendor_id) +
  labs(y = "Trip duration [s]", x = "Number of passengers")
#visualisation of the boxplot of duration accoridng for each passenger count
train %>%
  filter(vendor_id == 1) %>%
  ggplot(aes(passenger_count, trip_duration, color = passenger_count)) +
  geom_boxplot() +
  scale_y_log10() +
  facet_wrap(~ store_and_fwd_flag) +
  theme(legend.position = "none") +
  labs(y = "Trip duration [s]", x = "Number of passengers") 
#calculating the distance between the pick up and the drop off
library(geosphere)
n <- nrow(train)
#recupeation of the latitude and longitude columns
latA <-as.matrix(train[,6])
lonA <- as.matrix(train[,5])
latB <- as.matrix(train[,8])
lonB <- as.matrix(train[,7])
#putting the pick up geolocation into a matrix
p1 <- matrix(data = c(lonA,latA),nrow=n, ncol=2)
#putting the dropoff geolocation into a matrix
p2<- matrix(data = c(lonB,latB),nrow=n, ncol=2)
#computing the distance with the distgeo function which give the distance in meter
distance <- distGeo(p1,p2)
#taking the log of the distance +1 (to avid taking the log of 0 ) because we transformed our target variable into its log 
train$distance <- log(distance+1)
#adding the coloumn distance to the train data
train <- cbind(train,distance)
# switch des colonnes distance et duree
a <- c(colnames(train))
b <- a
b[11] <- a[12]
b[12 ] <- a[11]
#getting rid of extreme distance which are clearly outside of ne york
train <- subset(train,train$distance<=log(100000))
#plotting the relation between the log of the distance and the log of trip duration
set.seed(3)
train %>%
  sample_n(5e4) %>%#taking a random sample of size 5000
  ggplot(aes((distance), log(trip_duration)) )+
  geom_point()+
  labs(x = "Direct distance [m]", y = "Trip duration [s]")
#adding the columns corresponding to the day month ad hour of pick up into the data set and transforming the trip durantion into its log 
train <- train %>%
  mutate(month = month(pickup_datetime, label = TRUE),
         wday = wday(pickup_datetime, label = TRUE, week_start = 1),
         wday = fct_relevel(wday, c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
         hour=hour(pickup_datetime),
         trip_duration=log(trip_duration)
  )
#getting rid of obsevation with passenger_count greater than 7
subset(train, train$passenger_count<7)
#getting rid of the date time values since we already have the hour mont and day columns  
#getting rid of the store flag column 
train <- train[,-c(2, 3, 9)]
#reputting passenger count into a numeric values  we do minus 1 because the factor start at 1 instead of à
train$passenger_count <- as.numeric(train$passenger_count)-1
#same for wday and month
train$wday <- as.numeric(train$wday)
train$month <- as.numeric(train$month)
#correlation plot of our data
corrplot(cor(data.matrix(train)))
#linear regression using lm function. We take trip duration as a linear function of all the other variables
modreg_R1 <- lm(train$trip_duration~.,data=train)
#using summary to see the differnt coefficient obtained previously
summary(modreg_R1)
#adding the intercept column
intercept <- rep(1,nrow(train))
#making the X matrxi composed of the intercept and the explanatory variable 
X=as.matrix(cbind(intercept,train[,c(1:6,8:11)]))
#taking the beta vector which corresponds to the coefficient of the linear relation
beta=t(t(modreg_R1$coefficients))
#uplooading the test and the submit data set
test <- as_tibble(fread("/Users/princessemame/APPRENTISSAGE AUTOMATIQUE /test.csv"))
submit<- as_tibble(fread("/Users/princessemame/APPRENTISSAGE AUTOMATIQUE /sample_submission.csv"))
#Remaking the same trnsformation we did on the train data set on the test data set
test <- test[,-1]
test <- test %>%
  mutate(pickup_datetime = ymd_hms(pickup_datetime))
library(geosphere)
n <- nrow(test)
latA <-as.matrix(test$pickup_latitude)
lonA <- as.matrix(test$pickup_longitude)
latB <- as.matrix(test$dropoff_latitude)
lonB <- as.matrix(test$dropoff_longitude)
p1 <- matrix(data = c(lonA,latA),nrow=n, ncol=2)
p2<- matrix(data = c(lonB,latB),nrow=n, ncol=2)
distance <- distGeo(p1,p2)
distance <- log(distance+1)
test <- cbind(test,distance)
test <- test%>%
  mutate(store_and_fwd_flag = as.integer(factor(store_and_fwd_flag)),
         vendor_id = as.integer(vendor_id),
         month = (month(pickup_datetime)),
         hour = hour(pickup_datetime),
         wday = wday(pickup_datetime, label = TRUE, abbr = TRUE) , wday = as.integer(fct_relevel(wday, c("Sun", "Sat", "Mon", "Tue", "Wed", "Thu", "Fri"))),
  )
test$wday <- as.numeric(test$wday)
test$month <- as.numeric(test$month)
test <- test[,-c(2, 8)]
#creating the X for the test dataset
X=as.matrix(cbind(rep(1,(nrow(test))), test))
beta=t(t(modreg_R1$coefficients))
#computing the prediction by doing X*beta
Y_hat=X%*%beta
#writting the submit files
submit$trip_duration <- exp(Y_hat) # we take exponnetial cause we were using the log of trip duration
write.csv(x = submit, file = "sample_submission3.csv",row.names = FALSE)
#Feature Selection
#stepwise regression using the step function the starting model is that the target is constant and the last model is the one containing all the variables
regboth <- step(lm(train$trip_duration~1,data=train),list(upper=modreg_R1),direction = "both")
#backward regression using the step function the starting model is is the one containing all the variables
regbackward <- step(modreg_R1,direction = "backward")
#Stochastic gradient descent(SGD)
library(sgd)
model_train3<-train[,-c(2,3,13)]
#we run stochastic gradient descent on the underlying loss function using our model.
sgd.theta <- sgd(model_train3$trip_duration ~ ., data=model_train3, model="lm") 
#creating the X for the test dataset
X=as.matrix(cbind(rep(1,length(nrow(model_test))),model_test))
beta=t(t(sgd.theta$coefficients))
#computing the prediction by doing X*beta
Y_sgd=exp(X%*%beta)
#writting the submit files
sample_submission_data$trip_duration <- Y_sgd
write.csv(x = sample_submission_data, file = "submitsgd.csv",row.names = FALSE)
#Penalized regression methods
#Lasso regression
library(glmnet)
#Lasso regression find a parsimonious model which performs L1 regularization. The L1 regularization adds a penality equivalent to the absolute of the maginitude of regression coefficients and tries to minimize them 
#Lasso translates each coefficient by a constant factor $\lambda$, truncating at zero. This is called "soft thresholding".
modreg1=cv.glmnet(as.matrix(model_train3[,-7]),model_train3$trip_duration, alpha=1,nlambda=100,lambda.min.ratio=0.0001)
#This function does k-fold cross-validation for glmnet,  returns a value for the best lambda value.
best_lam <- modreg1$lambda.min
lasso_best <- glmnet(as.matrix(model_train3[,-7]),model_train3$trip_duration, alpha = 1, lambda = best_lam)
#computing the prediction using "predict"
Y_test<- exp(predict(lasso_best, s = best_lam, newx = as.matrix(model_test)))
#writting the submit files
sample_submission_data$trip_duration <- Y_test
write.csv(x = sample_submission_data, file = "submitlasso.csv",row.names = FALSE)
#Ridge regression
#Ridge regression is a parsimonious model which performs L2 regularization. The L2 regularization adds a penality equivalent to the square of the maginitude of regression coefficients and tries to minimize them.
#Ridge regression does a proportional shrinkage and handles collinear variables.
modreg2=cv.glmnet(as.matrix(model_train3[,-7]),
                  model_train3$trip_duration,alpha=0,
                  nlambda=100,lambda.min.ratio=0.0001)
#This function does k-fold cross-validation for glmnet,  returns a value for the best lambda value.
best_lambda=modreg2$lambda.min
best_ridge= glmnet(as.matrix(model_train3[,-7]),model_train3$trip_duration,alpha=0,lambda=best_lambda)
#computing the prediction using "predict"
Y_testridge=exp(predict(best_ridge, s =best_lambda, newx = as.matrix(model_test)))
#writting the submit files
sample_submission_data$trip_duration <-Y_testridge
write.csv(x = sample_submission_data, file = "submitridge.csv",row.names = FALSE)
#Elastic Net
#ElasticNet is a hybrid of both Lasso and Ridge regression. It is trained with both L1-norm and L2-norm prior as regularizer.
modreg3=cv.glmnet(as.matrix(model_train3[,-7]),
        model_train3$trip_duration,alpha=0.5,
        nlambda=100,lambda.min.ratio=0.0001)
best_lam <- modreg3$lambda.min
EN_best <- glmnet(as.matrix(model_train3[,-7]),
                  model_train3$trip_duration,alpha=0.5,
                  lambda = best_lam)
#computing the prediction using "predict"
Y_test_EN<- exp(predict(EN_best, s = best_lam,newx = as.matrix(model_test)))
#writting the submit files
sample_submission_data$trip_duration <- Y_test_EN
write.csv(x = sample_submission_data, file = "submitEN.csv",row.names = FALSE)
#k-Nearest Neighbors(KNN)
#K nearest neighbors is a simple algorithm that stores all available cases and predict the numerical target based on a similarity measure
prediction <- FNN::knn.reg(model_train3[,-7],model_test,
                           model_train3$trip_duration, k=25,
                           algorithm="kd_tree") 
#computing the prediction using "predict"
Y_KNN <- exp(prediction$pred)
#writting the submit files
sample_submission_data$trip_duration <- Y_KNN
write.csv(x = sample_submission_data, file = "submitKNN.csv",row.names = FALSE)
#Externe data
#Downloading the dataset from kaggle : Weather data in New York City - 2016
#Uploading the dataset
weather<-data.table::fread(file="C:/Users/nisri/Downloads/weather_data_nyc_centralpark_2016(1).csv",
                           sep=",",header=TRUE)
#Adding new columns that dispose of all the missing 'T' values by replacing them with 0.01.
#Adding an All precipitation column that sums the values of both the "Rain" and "Snow fall".
#Adding two more boolean columns that represent whether it has rained/snowed.
weather <- weather %>%
  mutate(date = dmy(date),
         rain = as.numeric(ifelse(precipitation == "T", "0.01", precipitation)),
         s_fall = as.numeric(ifelse(`snow fall` == "T", "0.01", `snow fall`)),
         s_depth = as.numeric(ifelse(`snow depth` == "T", "0.01", `snow depth`)),
         all_precip = s_fall + rain,
         has_snow = (s_fall > 0) | (s_depth > 0),
         has_rain = rain > 0,
         max_temp = `maximum temperature`,
         min_temp = `minimum temperature`)
#Keeping only the columns we added/modified.
foo <- weather %>%
  select(date, rain, s_fall, all_precip, has_snow, has_rain, s_depth, max_temp, min_temp)
#Adding the new columns/information to the initial dataset to create an expanded one.
model_train2 <- left_join(model_train2, foo, by = "date")
#Disposing of the pickuptime/dropofftime/date columns 
model_train4<-model_train2[,-c(2,3,13)]
#Converting the boolean values into integers.
model_train4$has_snow<-as.integer(as.factor(model_train4$has_snow))
model_train4$has_rain<-as.integer(as.factor(model_train4$has_rain))
#Disposiong of the all precipitation column
model_train4<-model_train4[,-14]
#Doing the same manipulations to the test dataset
model_test2 <- left_join(test_data1, foo, by = "date")
model_test2<-model_test2[,-c(1,3,9,14,17)]
model_test2$has_snow<-as.integer(as.factor(model_test2$has_snow))
model_test2$has_rain<-as.integer(as.factor(model_test2$has_rain))
##Using the same code for the OLS-RIDGE-LASSO-ELASTIC NET and KNN.