# Linear Regression Model
# Eugene Lucino
# Data Mining II

# ------------------------------
# Import Data
# ------------------------------
medals <- read.csv("Data Mining II/olympics.csv")
medals <- medals[,-c(1)]
# adjusted sports and events for Tokyo
adj2020 <- read.csv("Data Mining II/adjTokyo.csv")
adj2020 <- adj2020[,-c(1)]


# Training Set   - Games before 2016
# Validation Set - 2016 Games
# X - variables to make a model on
# Y - actual medal results
X <- medals[c(1:8)]
Y <- medals[c(1:3,9:12)]

X_train <- X[(X$Year <  2016),]
X_test  <- X[(X$Year == 2016),]
rownames(X_test) <- c()

Y_train <- Y[(Y$Year <  2016),]
Y_test  <- Y[(Y$Year == 2016),]
rownames(Y_test) <- c()



# ------------------------------
# Train Linear Regression Model
# ------------------------------
# create the model, separating by medal
golds.fit   <- lm(Y_train$Gold~.-Year-NOC-Country,   data=X_train)
silvers.fit <- lm(Y_train$Silver~.-Year-NOC-Country, data=X_train)
bronzes.fit <- lm(Y_train$Bronze~.-Year-NOC-Country, data=X_train)

# make predictions
Y_train$PredictedGold   <- as.integer(round(predict(golds.fit)))
Y_train$PredictedSilver <- as.integer(round(predict(silvers.fit)))
Y_train$PredictedBronze <- as.integer(round(predict(bronzes.fit)))
Y_train$PredictedMedals <- (Y_train$PredictedGold +
          Y_train$PredictedSilver + Y_train$PredictedBronze)


# root mean square error of models
sqrt(mean(golds.fit$fitted.values^2))
sqrt(mean(silvers.fit$fitted.values^2))
sqrt(mean(bronzes.fit$fitted.values^2))

# model coefficients
features = as.data.frame(coef(golds.fit))
features$SilverCoefficient <- coef(silvers.fit)
features$BronzeCoefficient <- coef(bronzes.fit)
names(features)[1] <- "GoldCoefficient"

# summary(golds.fit)
# summary(silvers.fit)
# summary(bronzes.fit)
# plot(golds.fit)
# plot(silvers.fit)
# plot(bronzes.fit)



# ------------------------------
# Validate Linear Regression Model
# ------------------------------

# five countries either won their very first medal or won
# their first medal since 1992; they are excluded due to
# lack of data and are predicted to win 0 medals in 2020
X_test <- X_test[!(X_test$NOC=='CIV' | X_test$NOC=='FIJ' |
                   X_test$NOC=='JOR' | X_test$NOC=='KOS' |
                   X_test$NOC=='NIG' ),]
Y_test <- Y_test[!(Y_test$NOC=='CIV' | Y_test$NOC=='FIJ' |
                   Y_test$NOC=='JOR' | Y_test$NOC=='KOS' |
                   Y_test$NOC=='NIG' ),]


Y_test$PredictedGold   <- as.integer(round(predict(golds.fit, newdata = X_test)))
Y_test$PredictedSilver <- as.integer(round(predict(silvers.fit, newdata = X_test)))
Y_test$PredictedBronze <- as.integer(round(predict(bronzes.fit, newdata = X_test)))
Y_test$PredictedMedals <- (Y_test$PredictedGold +
          Y_test$PredictedSilver + Y_test$PredictedBronze)


# residual plots of validation set
plot(Y_test$PredictedGold, (Y_test$Gold - Y_test$PredictedGold),
     ylab="Residuals", main="Validation Set Residuals - Gold")

plot(Y_test$PredictedSilver, (Y_test$Silver - Y_test$PredictedSilver),
     ylab="Residuals", main="Validation Set Residuals - Silver")

plot(Y_test$PredictedBronze, (Y_test$Bronze - Y_test$PredictedBronze),
     ylab="Residuals", main="Validation Set Residuals - Bronze")

# RMSE of validation set
sqrt(mean((Y_test$Gold-Y_test$PredictedGold)^2))
sqrt(mean((Y_test$Silver-Y_test$PredictedSilver)^2))
sqrt(mean((Y_test$Bronze-Y_test$PredictedBronze)^2))



# ------------------------------
# Predict Medals for Tokyo 2020
# ------------------------------

# incorporate adjusted sport and events counts
adj2020 <- cbind(X_test[1:6], Sports=adj2020$Sports,
                 Events=adj2020$Events)
# change year and host
tokyo <- adj2020
tokyo$Year <- 2020
tokyo$Home <- apply(tokyo, 1, FUN=function(x)
  if      (x[2] == "BRA") 0
  else if (x[2] == "JPN") 1 
  else    0)

# make the predictions
Y_2020 <- tokyo[,1:3]
Y_2020$PredictedGold   <- as.integer(round(predict(golds.fit, newdata = tokyo)))
Y_2020$PredictedSilver <- as.integer(round(predict(silvers.fit, newdata = tokyo)))
Y_2020$PredictedBronze <- as.integer(round(predict(bronzes.fit, newdata = tokyo)))
Y_2020$PredictedMedals <- (Y_2020$PredictedGold +
          Y_2020$PredictedSilver + Y_2020$PredictedBronze)

# find the top medaling countries
Y_2020 <- Y_2020[,-c(1)]
topTokyo <- Y_2020[order(-Y_2020$PredictedMedals,
                         -Y_2020$PredictedGold,
                         -Y_2020$PredictedSilver,
                         -Y_2020$PredictedBronze),]
rownames(topTokyo) <- c()

write.csv(topTokyo, "Data Mining II/finalstandings.csv",
          row.names = FALSE)
