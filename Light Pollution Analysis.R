Pollution <- read.csv(file.choose())
Vehicle <- read.csv(file.choose())
GDP <- read.csv(file.choose())
Electricity <- read.csv(file.choose())
Population <- read.csv(file.choose())

colnames(Pollution) <- c('Country','Rad2013','Rad2014','Rad2015')
colnames(Electricity) <- c('Country','Elec2013','Elec2014')
colnames(GDP) <- c('Country','GDP2013','GDP2014','GDP2015')
colnames(Population) <- c('Country','Pop2013','Pop2014','Pop2015')
colnames(Vehicle) <- c('Country','Veh2013','Veh2014','Veh2015')

#-------------------------------------------
# GDP wrt Pollution analysis

Pol_GDP <- merge(Pollution,GDP, by = 'Country')
head(Pol_GDP)


plot(Pol_GDP$GDP2013,Pol_GDP$Rad2013, col = "Blue", xlab = "GDP/Capita", ylab = "Radiance/1000", main = "2013 Radiance vs. GDP")
plot(log(Pol_GDP$GDP2013),log(Pol_GDP$Rad2013),col = "Red", xlab = "log(GDP/Capita)", ylab = "log(Radiance/1000)", main = "2013 log(Radiance) vs. log(GDP)")

lmregr <- lm(log(Rad2013)~log(GDP2013), data = Pol_GDP)
abline(lmregr, col = "Blue")



summary(lmregr)

#-------------------------------------------
# Vehicles wrt Pollution analysis
Pol_Veh <- merge(Pollution, Vehicle, by = 'Country')
head(Pol_Veh)

#png("Veh.png",width=700,height=450)
par(mfrow = c(1, 2))

plot(Pol_Veh$Veh2013, Pol_Veh$Rad2013, col = "Magenta", xlab = "Vehicles/1000", ylab = "Radiance/1000", main = "2013 Radiance vs. Vehicles Sold")
plot(log(Pol_Veh$Veh2013), log(Pol_Veh$Rad2013),col = "Green", xlab = "log(Vehicles/1000)", ylab = "log(Radiance/1000)", main = "2013 log(Radiance) vs. log(Vehicles Sold)")

lmregr1 <- lm(log(Rad2013)~log(Veh2013), data = Pol_Veh)
abline(lmregr1, col = "Purple")
#dev.off()
par(mfrow = c(1, 1))
summary(lmregr1)

#-------------------------------------------
# Electricity wrt Pollution analysis
Pol_Elec <- merge(Pollution, Electricity, by = 'Country')
head(Pol_Elec)

plot(Pol_Elec$Elec2013,Pol_Elec$Rad2013, col = "Purple", xlab = "Electricity/1000", ylab = "Radiance/1000", main = "2013 Radiance vs. Electricity Consumption")
plot(log(Pol_Elec$Elec2013),log(Pol_Elec$Rad2013),col = "Maroon", xlab = "log(Electricity/1000)", ylab = "log(Radiance/1000)", main = "2013 log(Radiance) vs. log(Electricity Consumption)")

lmregr2 <- lm(log(Rad2013)~log(Elec2013), data = Pol_Elec)
abline(lmregr2, col = "Green")

summary(lmregr2)
  
boxplot(Pol_Elec$Elec2013)
boxplot(Pol_Elec$Rad2013)
badrows <- which(Pol_Elec$Elec2013 >= 0.003)
testdf <- NULL
testdf <- Pol_Elec[-badrows,]
badrows <- which(testdf$Rad2013 >= 70)
testdf <- testdf[-badrows,]

badrows <- which(Pol_Elec$Elec2013 >= 0.01)
testdf <- NULL
testdf <- Pol_Elec[-badrows,]
badrows <- which(testdf$Rad2013 >= 250)
testdf <- testdf[-badrows,]

#png(file="Rad_Elec_Countries2.png",width=700,height=450,res=80)
plot(testdf$Elec2013,testdf$Rad2013,col = "Red", xlab = "Electricity/1000", ylab = "Radiance/1000", main = "2013 Radiance vs. Electricity Consumption")
text(testdf$Elec2013, testdf$Rad2013, labels=testdf$Country, cex= 0.6, pos = 3)
#dev.off()

lmregr2 <- lm(Rad2013~Elec2013, data = Pol_Elec)
summary(lmregr2)

#-------------------------------------------
# Merge all datasets
finaldf <- merge(Pol_GDP,Vehicle, by = 'Country')
finaldf <- merge(finaldf,Electricity, by = 'Country')

write.csv(finaldf, "FinalData.csv")

#-------------------------------------------
#Combined Regression

lmregr3 <- lm(log(Rad2013) ~ log(Veh2013)+log(GDP2013)+Elec2013 +I(log(GDP2013)*log(Elec2013)), data = finaldf)
summary(lmregr3)

lmregr4 <- lm(log(Rad2013) ~ log(Veh2013)+log(GDP2013)+Elec2013 +I(log(GDP2013)*log(Veh2013)), data = finaldf)
summary(lmregr4)

lmregr5 <- lm(log(Rad2013) ~ Veh2013 +log(GDP2013)+Elec2013, data = finaldf)
summary(lmregr5)

predictdf <- data.frame(Country = finaldf$Country, GDP2013 = finaldf$GDP2014, Elec2013 = finaldf$Elec2014, Veh2013 = finaldf$Veh2014)
predictdf$rad <- exp(predict(lmregr4, predictdf))
predictdf$rad2014 <- finaldf$Rad2014
head(predictdf,10)

attach(predictdf)
mydf <- NULL
mydf <- predictdf[order(-rad),]
mydf <- mydf[1:20,]
which(mydf$Country == "United States of America")
mydf$Country <- as.character(mydf$Country)
mydf$Country[8] = "USA"
mydf$Country[9] = "UAE"

Comparison <- NULL
Comparison <- matrix(mydf$rad2014,nrow = 1)
Comparison <- rbind(Comparison,mydf$rad)

#png(file="Predict_Rad.png",width=900,height=450,res=80)
barplot(Comparison, main = "Actual vs Predicted Radiance/1000 in 2014 - top 20 by predicted value", names.arg = mydf$Country,  xlab = "Country",ylab = "Rad/1000", col=c("green","red"), beside = TRUE, legend = c("Actual Radiance","Predicted Radiance"), cex.names = 0.5)
#dev.off()
#-------------------------------------------
# Lets try predicting the radiance change direction
head(finaldf)

pdf <- data.frame(Country = finaldf$Country)
pdf$GDP1314 <- (finaldf$GDP2014 - finaldf$GDP2013)/finaldf$GDP2013
pdf$GDP1415 <- (finaldf$GDP2015 - finaldf$GDP2014)/finaldf$GDP2014
pdf$Veh1314 <- (finaldf$Veh2014 - finaldf$Veh2013)/finaldf$Veh2013
pdf$Veh1415 <- (finaldf$Veh2015 - finaldf$Veh2014)/finaldf$Veh2014
radchange1314 <- (finaldf$Rad2014 - finaldf$Rad2013)/finaldf$Rad2013
radchange1415 <- (finaldf$Rad2015 - finaldf$Rad2014)/finaldf$Rad2014
pdf$raddir1314 <- as.factor(ifelse(radchange1314 >= 0, 1, 0))
pdf$raddir1415 <- as.factor(ifelse(radchange1415 >= 0, 1, 0))
head(pdf)

training <- data.frame(Country = pdf$Country, GDP = pdf$GDP1314, Veh = pdf$Veh1314, raddir = pdf$raddir1314)
testing <-  data.frame(Country = pdf$Country, GDP = pdf$GDP1415, Veh = pdf$Veh1415, raddir = pdf$raddir1415)
head(training)
head(testing)

# Try GLM to predict radiance direction for 14-15 using 13-14 as training data
model1 <- glm(raddir ~ GDP + Veh, data = training, family = binomial(link = 'logit'))
summary(model1)

predict <- predict(model1, testing, type = "response")
head(predict)
predict_dir <- ifelse(predict >= 0.3, 1, 0)

tbl <- table(predict_dir, testing$raddir)
tbl
Accuracy <- sum(diag(tbl))/sum(tbl)
Accuracy

#___________________________________________________
# Try Naive Bayes to predict radiance direction for 14-15 using 13-14 as training data
library ("klaR")
library ("caret")
library ("e1071")

model2 <- NaiveBayes(raddir ~ ., data=training)
predict_dir1 <- predict(model2, testing)
confusionMatrix(testing$raddir, predict_dir1$class)


#___________________________________________________
# Graphs
png("Radiance.png",width=750,height=450)
mydf1 <- subset(finaldf, Country == "United States of America" |Country == "Portugal" |Country == "Brunei" | Country == "Australia" | Country == "Russia" | Country == "Netherlands" )
Comp <- matrix(mydf1$Rad2013, nrow = 1)
Comp <- rbind(Comp,mydf1$Rad2014)
Comp <- rbind(Comp,mydf1$Rad2015)
barplot(Comp, main = "Radiance/1000 Growth", names.arg = mydf1$Country,  xlab = "Country",ylab = "Rad/1000", col=c("purple","magenta","green"), beside = TRUE, legend = c("2013","2014","2015"))
dev.off()
