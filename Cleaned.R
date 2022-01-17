#======== READING =========
data <- read.csv('big_removed.csv')
#add index
data$index <- 1:nrow(data)
str(head(data))
# Make it non-exponential ?

library(stringr)
suburbs <- read.csv('suburbs_data.csv')
suburbs$suburb <- str_to_title(suburbs$suburb)
str(head(suburbs))
crimeSum <- read.csv('sumSuburbs_columns.csv')
crimeSum$suburb <- str_to_title(crimeSum$suburb)
str(head(crimeSum))

data2 <- data[, c(2, 3, 5:8, 16, 17)]

data3 <- merge(x=data2, y=suburbs, by='suburb', all.x=T)
data3 <- data3[order(data3$index), ]

data4 <- merge(x=data3, y=crimeSum, by='suburb', all.x=T)

# No lat/lng for both
# No crime
data5 <- data4[, c(2:7, 11:14, 17:19)]
# Crime
#data5 <- data4[, c(2:7, 11:14, 17:29)]

#basic
#data5 <- data4[, c(2:5)]

# check for weird shapes
for(i in 2:length(data5)) {
  xLab <- names(data5)[i]
  print(paste('Displaying: ', xLab, ' vs. price', sep=''))
  plot(data5[, i], data5$price, xlab=xLab, ylab='price')
  a = readline('Press Enter to continue: ')
}

plot(data5[, 11]*data5[, 12], data5$price, ylab='price')

base <- 2011
for (i in 1:10)
{
  year <- paste('X', (base + i), sep='')
  # Estimate the crime rate per 100K people.
  #temp[, year] <- (temp[, year] / temp[, 'population']) * 100000
  data5[, year] <- (data5[, year] / data5[, 'population']) * 100000
}

#================ TRAIN TEST ================
samples <- sample(nrow(data5), nrow(data5)*0.6)
train <- data5[samples, ]
test <- data5[-samples, ]

# Testing set with sales that are within the last year, and have at least 2 historical sales. 3 max.
# Make sure all historical values are within 2012 to 2021 since the data doesn't exist.
library(stringr)
#str_count('Dec/2016/11500000.0/SOLD-Aug/2014/10000000.0/SOLD-May/2007/6600000.0/SOLD', '-') + 1
keepIndex <- c()
for(i in 1:nrow(data)) {
  # Old way. Just checks if the 
  #histKeep <- str_count(data[i, 'salesHistory'], '-') >= 1
  #yearKeep <- str_split(data[i, 'date'], '-')[[1]][3] == '2021'
  #if(histKeep && yearKeep) {
  #  keepIndex <- c(keepIndex, as.numeric(i))
  #}
  
  # Check if the sale year is in 2021 and check between June and September 
  yearKeep <- str_split(data[i, 'date'], '-')[[1]][3] == '2021'
  month <- match(str_split(data[i, 'date'], '-')[[1]][2], month.abb)
  monthKeep <- (month <= 9 && month >= 1)
  histCount <- str_count(data[i, 'salesHistory'], '-')
  if(histCount < 1 || yearKeep == F || monthKeep == F) {
    next
  }
  
  histAbove <- 0
  histPrices <- str_split(data[i, 'salesHistory'], '-')
  for(j in 1:length(histPrices[[1]])) {
    info <- str_split(histPrices[[1]][j], '/')[[1]]
    saleMonth <- info[1]
    monthNum <- match(saleMonth, month.abb)
    if(monthNum >= 10) { next }
    saleYear <- as.numeric(info[2])
    if(saleYear < 2012) { next }
    else { histAbove <- histAbove + 1}
  }
  if(histAbove >= histCount) {
    keepIndex <- c(keepIndex, i)
  }
}

newTrain <- data5[-keepIndex, ]
newTest <- data5[keepIndex, ]

#========= STANDARDISATION =============
library(dplyr)
#dataStd <- scale(data4[, c(2:7, 11:14, 17:29)]) 
dataStd <- data4[, c(2:7, 11:14, 17:29)] %>% mutate_all(~(scale(.) %>% as.vector))
head(dataStd)

dataStd2 <- dataStd[, c(1:13)]
# Check for 0s and 1s:
# https://stackoverflow.com/questions/15215457/standardize-data-columns-in-r
apply(dataStd2, 2, mean)
apply(dataStd2, 2, sd)

#samplesStd <- sample(nrow(dataStd), nrow(dataStd)*0.6)
#trainStd <- dataStd2[samples, ]
#testStd <- dataStd2[-samples, ]
# Use the training data with historical sales.
trainStd <- dataStd2[-keepIndex, ]
testStd <- dataStd2[keepIndex, ]

#========== MODEL WITH INTERACTION AND SQUARED TERMS ===========
# Standardised.
y    <- 'price'
formula <-  paste0(".*. + I(", names(data5)[names(data5)!=y], "^2)+", collapse="") %>%
  paste(y, "~", .) %>%
  substr(., 1, nchar(.)-1) %>%
  as.formula
modNormal <- lm(formula, data=newTrain)
modStd <- lm(formula, data=trainStd)

summary(modNormal)
summary(modStd)

#========= GENETIC ALG. =================
# Normal
x <- model.matrix(modNormal)[, -1]
str(x)
y <- model.response(model.frame(modNormal))

fitness <- function(s)
{
  inc <- which(s == 1)
  X <- cbind(1, x[, inc])
  mod <- lm.fit(X, y)
  class(mod) <- 'lm'
  -AIC(mod)
}

library(GA)
GA <- ga("binary", fitness=fitness, nBits=ncol(x),
         names=colnames(x), monitor=plot, popSize=100)

plot(GA)
summary(GA)
GA@solution[1,]

# Standardised
xStd <- model.matrix(modStd)[, -1]
str(xStd)
yStd <- model.response(model.frame(modStd))

fitnessStd <- function(s)
{
  inc <- which(s == 1)
  X <- cbind(1, xStd[, inc])
  mod <- lm.fit(X, yStd)
  class(mod) <- 'lm'
  -AIC(mod)
}

GAStd <- ga("binary", fitness=fitnessStd, nBits=ncol(xStd),
         names=colnames(xStd), monitor=plot, popSize=100)

plot(GAStd)
summary(GAStd)
GAStd@solution[1,]

# Compare without standardisation
selectedGA <- which(GA@solution[1, ] == 1)
selectedNamesGA <- names(selectedGA)
selectedStd <- which(GAStd@solution[1, ] == 1)
selectedNamesStd <- names(selectedStd)
selectedNamesGA
selectedNamesStd

diffGA <- setdiff(selectedNamesGA, selectedNamesStd)
diffStd <- setdiff(selectedNamesStd, selectedNamesGA)
diffGA
diffStd

predd <- predict(modStd, newdata=testStd[20,])
indexTest <- as.numeric(rownames(testStd[20, ]))
testStd[20, ]
dataStd2[indexTest, ]
dataStd[indexTest, ]
data[indexTest, ]
predd
#sd * z-score + mean
sd(data5[, 1])*predd + mean(data5[, 1])

#======= HISTORICAL ===========
historical2 <- read.csv('hist.csv', check.names=F)
testSample <- data[rownames(testStd[20, ]), ]
suburb <- historical2[historical2$suburb == tolower(testSample$suburb), ]
histSales <- testSample$salesHistory
library(stringr)
histSales2 <- str_split(histSales, '-')
histPrices <- data.frame()
for(i in 1:length(histSales2[[1]])) {
  info <- str_split(histSales2[[1]][i], '/')[[1]]
  # Skip over any sales less than $5000 in case a rental history snuck in.
  if(as.numeric(info[3]) < 5000) {
    next
  }
  #histPrices <- c(histPrices, c(info[1:3]))
  histPrices[i, 'month'] <- info[1]
  histPrices[i, 'year'] <- as.numeric(info[2])
  histPrices[i, 'price'] <- as.numeric(info[3])
}

histPrices
histPricesSimple <- c()
ratios <- c()
testColumn <- ''
for(i in 1:length(histPrices)) {
  year <- histPrices[i, 2]
  if(is.na(year)) { next }
  if(year == 2021) {
    month <- histPrices[i, 1]
    #https://stackoverflow.com/questions/6549239/convert-months-mmm-to-numeric
    monthNum <- match(month, month.abb)
    # Add a 0 to the left if the month isn't 10
    if(monthNum >= 10) { next }
    monthNum <- str_pad(monthNum, 2, 'left', '0')
    #testColumn <- c(testColumn, paste('2021-10/2021-', as.character(monthNum), sep=''))
    testColumn <- paste('2021-10/2021-', as.character(monthNum), sep='')
    ratios <- c(ratios, suburb[1, testColumn])
    histPricesSimple <- c(histPricesSimple, histPrices[i, 3])
  }
  else if(year >= 2012){
    #testColumn <- c(testColumn, paste('2021', year, sep='/'))
    testColumn <- paste('2021', year, sep='/')
    ratios <- c(ratios, suburb[1, testColumn])
    histPricesSimple <- c(histPricesSimple, histPrices[i, 3])
  }
}

if(length(ratios) == 0) {
  print('No ratios found. Sales history is too old for current data.')
}  else {
  weight <- 1/(length(ratios))
  weight
  predd <- 0
  for(i in 1:length(ratios)) {
    print(predd)
    predd <- predd + (histPricesSimple[i]* ratios[i] * weight)
    print(predd)
  }
  print(predd)
  print(ratios)
}

#====== FEATURE SELECTION =====
formulaStr <- paste('price ~ ', selectedNamesGA[1], sep='')
for(i in 2:length(selectedNamesGA)) {
  formulaStr <- paste(formulaStr, selectedNamesGA[i], sep='+')
}
formulaStr
newFormula <- as.formula(formulaStr)
modSelected <- lm(newFormula, data=newTrain)

formulaStrStd <- paste('price ~ ', selectedNamesStd[1], sep='')
for(i in 2:length(selectedNamesStd)) {
  formulaStrStd <- paste(formulaStrStd, selectedNamesStd[i], sep='+')
}
formulaStrStd
newFormulaStd <- as.formula(formulaStrStd)
modSelectedStd <- lm(newFormulaStd, data=trainStd)

#====== MSE AND RMSE =======
# All variables.
predAll <- data.frame(pred=predict(modNormal, newdata=newTest), actual=newTest$price)
predAll <- na.omit(predAll)
# MSE
allMSE <- mean((predAll$actual - predAll$pred)^2)
allRMSE <- sqrt(allMSE)

# Selected variables (from GA).
predSelected <- data.frame(pred=predict(modSelected, newdata=newTest), actual=newTest$price)
predSelected <- na.omit(predSelected)
# MSE
selectedMSE <- mean((predSelected$actual - predSelected$pred)^2)
selectedRMSE <- sqrt(selectedMSE)

allMSE
selectedMSE
allRMSE
selectedRMSE

diffMSE <- abs(allMSE - selectedMSE)
diffRMSE <- abs(allRMSE - selectedRMSE)
diffMSE
diffRMSE

# Selected variables standardised (from GA).
# This gives different selected variables.
predAllStd <- data.frame(pred=predict(modStd, newdata=testStd), actual=testStd$price)
predAllStd <- na.omit(predAllStd)
allStdMSE <- mean((predAllStd$actual - predAllStd$pred)^2)
allStdRMSE <- sqrt(allStdMSE)
allStdMSE
allStdRMSE


predSelectedStd <- data.frame(pred=predict(modSelectedStd, newdata=testStd), actual=testStd$price)
predSelectedStd <- na.omit(predSelectedStd)
# MSE
selectedStdMSE <- mean((predSelectedStd$actual - predSelectedStd$pred)^2)
selectedStdRMSE <- sqrt(selectedStdMSE)
selectedStdMSE
selectedStdRMSE

# Historical
predHistorical <- data.frame(actual=newTest$price)
historical2 <- read.csv('hist.csv', check.names=F)
for(i in 1:nrow(newTest)) {
  testSample <- data[rownames(newTest[i, ]), ]
  predHistorical[i, 'index'] <- testSample$index
  suburb <- historical2[historical2$suburb == tolower(testSample$suburb), ]
  histSales <- testSample$salesHistory
  library(stringr)
  histSales2 <- str_split(histSales, '-')
  histPrices <- data.frame()
  for(j in 1:length(histSales2[[1]])) {
    info <- str_split(histSales2[[1]][j], '/')[[1]]
    # Skip over any sales less than $5000 in case a rental history snuck in.
    if(as.numeric(info[3]) < 5000) {
      next
    }
    #histPrices <- c(histPrices, c(info[1:3]))
    histPrices[j, 'month'] <- info[1]
    histPrices[j, 'year'] <- as.numeric(info[2])
    histPrices[j, 'price'] <- as.numeric(info[3])
  }
  
  histPrices
  histPricesSimple <- c()
  ratios <- c()
  testColumn <- ''
  for(j in 1:length(histPrices)) {
    year <- histPrices[j, 2]
    if(is.na(year)) { next }
    if(year == 2021) {
      month <- histPrices[j, 1]
      #https://stackoverflow.com/questions/6549239/convert-months-mmm-to-numeric
      monthNum <- match(month, month.abb)
      # Add a 0 to the left if the month isn't 10
      if(monthNum >= 10) { next }
      monthNum <- str_pad(monthNum, 2, 'left', '0')
      #testColumn <- c(testColumn, paste('2021-10/2021-', as.character(monthNum), sep=''))
      testColumn <- paste('2021-10/2021-', as.character(monthNum), sep='')
      ratios <- c(ratios, suburb[1, testColumn])
      histPricesSimple <- c(histPricesSimple, histPrices[j, 3])
    }
    else if(year >= 2012){
      #testColumn <- c(testColumn, paste('2021', year, sep='/'))
      testColumn <- paste('2021', year, sep='/')
      ratios <- c(ratios, suburb[1, testColumn])
      histPricesSimple <- c(histPricesSimple, histPrices[j, 3])
    }
  }
  
  if(length(ratios) == 0) {
    print('No ratios found. Sales history is too old for current data or includes rental history.')
    print(testSample$index)
    print(testSample$salesHistory)
    predHistorical[i, 'pred'] <- NA
  }  else {
    weight <- 1/(length(ratios))
    weight
    predd <- 0
    for(j in 1:length(ratios)) {
      #print(predd)
      predd <- predd + (histPricesSimple[j]* ratios[j] * weight)
      #print(predd)
    }
    #print(predd)
    #print(ratios)
    predHistorical[i, 'pred'] <- predd
  }
}

predHistoricalNoNA <- na.omit(predHistorical)
historicalMSE <- mean((predHistoricalNoNA$actual - predHistoricalNoNA$pred)^2)
historicalRMSE <- sqrt(historicalMSE)
historicalMSE
historicalRMSE

sprintf('RMSE Difference: Selected Model - Predicted: %f', (selectedRMSE - historicalRMSE))

#================== ISOLATION FOREST ====================
library(isotree)
library(knitr)

isoTrain <- newTrain[, c(2:6)]
isoTest<- newTest[, c(2:6)]
iso <- isolation.forest(isoTrain, ntrees=1000, nthreads=3)
predTrain <- predict(iso, isoTrain)
pred <- predict(iso, isoTest)
data[which.max(predTrain), ]
data[which.max(pred), ]

plot(pred)
plot(predTrain)

# https://en.wikipedia.org/wiki/Isolation_forest#Anomaly_detection_with_isolation_forest
# (0.5, 1] for the score means it is an anomaly.
predDf <- data.frame(pred)
predDf[, 'index'] <- rownames(predDf)
predDfFilter <- predDf[predDf[, 'pred'] > 0.60, ]
print(predDfFilter)
ordered <- predDfFilter[order(predDfFilter[, 'pred'], decreasing=T),]
indices <- c(ordered$index)
indices
d <- data[unlist(indices), ]
d['pred'] <- ordered$pred
d <- d[, c(1, 3, 5:8, 15, 18)]
knitr::kable(d)

predDfTrain <- data.frame(predTrain)
predDfTrain[, 'index'] <- rownames(predDfTrain)
predDfTrainFilter <- predDfTrain[predDfTrain[, 'predTrain'] > 0.60, ]
print(predDfTrainFilter)
orderedTrain <- predDfTrainFilter[order(predDfTrainFilter$pred, decreasing=T),]
indicesTrain <- c(orderedTrain$index)
indicesTrain
dTrain <- data[unlist(indicesTrain), ]
dTrain['pred'] <- orderedTrain$pred
dTrain <- dTrain[, c(1, 3, 5:8, 15, 18)]
knitr::kable(dTrain)

#======= ANOMALY DETECTION =======
# Use the pre-trained tree instead. It detects anomalies fine.
bedroomsThresh <- median(newTrain$bedrooms) + 3*sd(newTrain$bedrooms)
bathroomsThresh <- median(newTrain$bathrooms) + 3*sd(newTrain$bathrooms)
parkingSpacesThresh <- median(newTrain$parking_spaces) + 3*sd(newTrain$parking_spaces)
landSizeThresh <- median(newTrain$land_size) + 3*sd(newTrain$land_size)

testAnomalyDf <- c()
#testAnomalyDf <- data.frame(=seq(1:nrow(newTest)))
testAnomalyDf$bedrooms <- as.numeric(newTest[, 'bedrooms'] > bedroomsThresh)
testAnomalyDf$bathrooms <- as.numeric(newTest[, 'bathrooms'] > bathroomsThresh)
testAnomalyDf$parking_spaces <- as.numeric(newTest[, 'parking_spaces'] > parkingSpacesThresh)
testAnomalyDf$land_size <- as.numeric(newTest[, 'land_size'] > landSizeThresh)
testAnomalyDf$tot <- testAnomalyDf$bedrooms + testAnomalyDf$bathrooms + testAnomalyDf$parking_spaces + testAnomalyDf$land_size

indicesBasic <- which(testAnomalyDf$tot >= 1)
dBasic <- data[unlist(indicesBasic), ]
dBasic <- dBasic[, c(1, 3, 5:8, 15)]
knitr::kable(dBasic)

#======= WEIRD HOUSES ========
anomalies <- indices
predHistoricalWeird <- data.frame(actual=newTest[anomalies, 'price'], row.names=anomalies)
historical2 <- read.csv('hist.csv', check.names=F)
for(a in anomalies) {
  # Check for houses
  testSample <- data[a, ]
  predHistoricalWeird[a, 'index'] <- testSample$index
  suburb <- historical2[historical2$suburb == tolower(testSample$suburb), ]
  histSales <- testSample$salesHistory
  library(stringr)
  histSales2 <- str_split(histSales, '-')
  histPrices <- data.frame()
  for(j in 1:length(histSales2[[1]])) {
    info <- str_split(histSales2[[1]][j], '/')[[1]]
    # Skip over any sales less than $5000 in case a rental history snuck in.
    if(as.numeric(info[3]) < 5000) {
      next
    }
    #histPrices <- c(histPrices, c(info[1:3]))
    histPrices[j, 'month'] <- info[1]
    histPrices[j, 'year'] <- as.numeric(info[2])
    histPrices[j, 'price'] <- as.numeric(info[3])
  }
  
  histPrices
  histPricesSimple <- c()
  ratios <- c()
  testColumn <- ''
  for(j in 1:length(histPrices)) {
    year <- histPrices[j, 2]
    if(is.na(year)) { next }
    if(year == 2021) {
      month <- histPrices[j, 1]
      #https://stackoverflow.com/questions/6549239/convert-months-mmm-to-numeric
      monthNum <- match(month, month.abb)
      # Add a 0 to the left if the month isn't 10
      if(monthNum >= 10) { next }
      monthNum <- str_pad(monthNum, 2, 'left', '0')
      #testColumn <- c(testColumn, paste('2021-10/2021-', as.character(monthNum), sep=''))
      testColumn <- paste('2021-10/2021-', as.character(monthNum), sep='')
      ratios <- c(ratios, suburb[1, testColumn])
      histPricesSimple <- c(histPricesSimple, histPrices[j, 3])
    }
    else if(year >= 2012){
      #testColumn <- c(testColumn, paste('2021', year, sep='/'))
      testColumn <- paste('2021', year, sep='/')
      ratios <- c(ratios, suburb[1, testColumn])
      histPricesSimple <- c(histPricesSimple, histPrices[j, 3])
    }
  }
  
  if(length(ratios) == 0) {
    print('No ratios found. Sales history is too old for current data or includes rental history.')
    print(testSample$index)
    print(testSample$salesHistory)
    predHistoricalWeird[a, 'pred'] <- NA
  }  else {
    weight <- 1/(length(ratios))
    weight
    predd <- 0
    for(j in 1:length(ratios)) {
      #print(predd)
      predd <- predd + (histPricesSimple[j]* ratios[j] * weight)
      #print(predd)
    }
    #print(predd)
    #print(ratios)
    predHistoricalWeird[a, 'pred'] <- predd
  }
}
predHistoricalWeird
histWeirdMSE <- mean((predHistoricalWeird$actual - predHistoricalWeird$pred)^2)
histWeirdRMSE <- sqrt(histWeirdMSE)
histWeirdMSE
histWeirdRMSE

# Model
predSelectedWeird <- data.frame(pred=predict(modSelected, newdata=newTest[anomalies, ]), actual=newTest[anomalies, 'price'])
predSelectedWeird <- na.omit(predSelectedWeird)
# MSE
selectedWeirdMSE <- mean((predSelectedWeird$actual - predSelectedWeird$pred)^2)
selectedWeirdRMSE <- sqrt(selectedWeirdMSE)
selectedWeirdMSE
selectedWeirdRMSE

histWeirdRMSE
selectedWeirdRMSE
sprintf('RMSE Difference: Selected Model - Predicted: %f', (selectedWeirdRMSE - histWeirdRMSE))


#======= Extra models (for testing) ======
basicMod <- lm(price ~ ., data=newTrain)
interactionsMod <- lm(price ~ .*., data=newTrain)
library(dplyr)
ySquared <- 'price'
formulaSquared <-  paste0("I(", names(data5)[names(data5)!=ySquared], "^2)+", collapse="") %>%
  paste(ySquared, "~", .) %>%
  substr(., 1, nchar(.)-1) %>%
  as.formula
squaredMod <- lm(formulaSquared, data=newTrain)
formulaSquared <-  paste0(". + I(", names(data5)[names(data5)!=ySquared], "^2)+", collapse="") %>%
  paste(ySquared, "~", .) %>%
  substr(., 1, nchar(.)-1) %>%
  as.formula
basicSquaredMod <- lm(formulaSquared, data=newTrain)

x <- model.matrix(basicMod)[, -1]
str(x)
y <- model.response(model.frame(basicMod))

fitness <- function(s)
{
  inc <- which(s == 1)
  X <- cbind(1, x[, inc])
  mod <- lm.fit(X, y)
  class(mod) <- 'lm'
  -AIC(mod)
}

library(GA)
GABasic <- ga("binary", fitness=fitness, nBits=ncol(x),
         names=colnames(x), monitor=plot, popSize=100)

x <- model.matrix(interactionsMod)[, -1]
str(x)
y <- model.response(model.frame(interactionsMod))
GAInteractions <- ga("binary", fitness=fitness, nBits=ncol(x),
         names=colnames(x), monitor=plot, popSize=100)

x <- model.matrix(squaredMod)[, -1]
str(x)
y <- model.response(model.frame(squaredMod))
GASquared <- ga("binary", fitness=fitness, nBits=ncol(x),
         names=colnames(x), monitor=plot, popSize=100)

x <- model.matrix(basicSquaredMod)[, -1]
str(x)
y <- model.response(model.frame(basicSquaredMod))
GABasicSquared <- ga("binary", fitness=fitness, nBits=ncol(x),
         names=colnames(x), monitor=plot, popSize=100)

selectedGABasic <- which(GABasic@solution[1, ] == 1)
selectedNamesGABasic <- names(selectedGABasic)
formulaStrBasic <- paste('price ~ ', selectedNamesGABasic[1], sep='')
for(i in 2:length(selectedNamesGABasic)) {
  formulaStrBasic <- paste(formulaStrBasic, selectedNamesGABasic[i], sep='+')
}
newFormulaBasic <- as.formula(formulaStrBasic)
modSelectedBasic <- lm(newFormulaBasic, data=newTrain)

selectedGAInteractions <- which(GAInteractions@solution[1, ] == 1)
selectedNamesGAInteractions <- names(selectedGAInteractions)
formulaStrInteractions <- paste('price ~ ', selectedNamesGAInteractions[1], sep='')
for(i in 2:length(selectedNamesGAInteractions)) {
  formulaStrInteractions <- paste(formulaStrInteractions, selectedNamesGAInteractions[i], sep='+')
}
newFormulaInteractions <- as.formula(formulaStrInteractions)
modSelectedInteractions <- lm(newFormulaInteractions, data=newTrain)

selectedGASquared <- which(GASquared@solution[1, ] == 1)
selectedNamesGASquared <- names(selectedGASquared)
formulaStrSquared <- paste('price ~ ', selectedNamesGASquared[1], sep='')
for(i in 2:length(selectedNamesGASquared)) {
  formulaStrSquared <- paste(formulaStrSquared, selectedNamesGASquared[i], sep='+')
}
newFormulaSquared <- as.formula(formulaStrSquared)
modSelectedSquared <- lm(newFormulaSquared, data=newTrain)

selectedGABasicSquared <- which(GABasicSquared@solution[1, ] == 1)
selectedNamesGABasicSquared <- names(selectedGABasicSquared)
formulaStrBasicSquared <- paste('price ~ ', selectedNamesGABasicSquared[1], sep='')
for(i in 2:length(selectedNamesGASquared)) {
  formulaStrBasicSquared <- paste(formulaStrBasicSquared, selectedNamesGABasicSquared[i], sep='+')
}
newFormulaBasicSquared <- as.formula(formulaStrBasicSquared)
modSelectedBasicSquared <- lm(newFormulaBasicSquared, data=newTrain)

# MSE, RMSE
predBasic <- data.frame(pred=predict(modSelectedBasic, newdata=newTest), actual=newTest$price)
predBasic <- na.omit(predBasic)
# MSE
basicMSE <- mean((predBasic$actual - predBasic$pred)^2)
basicRMSE <- sqrt(basicMSE)

predInteractions <- data.frame(pred=predict(modSelectedInteractions, newdata=newTest), actual=newTest$price)
predInteractions <- na.omit(predInteractions)
# MSE
interactionsMSE <- mean((predInteractions$actual - predInteractions$pred)^2)
interactionsRMSE <- sqrt(interactionsMSE)

predSquared <- data.frame(pred=predict(modSelectedSquared, newdata=newTest), actual=newTest$price)
predSquared <- na.omit(predSquared)
# MSE
squaredMSE <- mean((predSquared$actual - predSquared$pred)^2)
squaredRMSE <- sqrt(squaredMSE)

predBasicSquared <- data.frame(pred=predict(modSelectedBasicSquared, newdata=newTest), actual=newTest$price)
predBasicSquared <- na.omit(predBasicSquared)
# MSE
basicSquaredMSE <- mean((predBasicSquared$actual - predBasicSquared$pred)^2)
basicSquaredRMSE <- sqrt(basicSquaredMSE)

basicRMSE
interactionsRMSE
squaredRMSE
basicSquaredRMSE
selectedRMSE
historicalRMSE

# best to worst
# Historical, selected, interctions, ...
min(basicRMSE, interactionsRMSE, squaredRMSE, basicSquaredRMSE, historicalRMSE, selectedRMSE)

#======= Mean of historical and linear model =======
# Get new samples. This removes the assumption that some might houses might have no historical sales.
# This will never be the case as the data was cleaned.
library(stringr)
keepIndexNew <- c()
for(i in 1:nrow(data)) {
  # Check if the sale year is in 2021 and check between June and September 
  yearKeep <- str_split(data[i, 'date'], '-')[[1]][3] == '2021'
  month <- match(str_split(data[i, 'date'], '-')[[1]][2], month.abb)
  monthKeep <- (month <= 9 && month >= 1)
  histCount <- str_count(data[i, 'salesHistory'], '-')
  #if(histCount < 1 || yearKeep == F || monthKeep == F) {
  if(yearKeep == F || monthKeep == F) {
    next
  }
  
  histAbove <- 0
  histPrices <- str_split(data[i, 'salesHistory'], '-')
  for(j in 1:length(histPrices[[1]])) {
    info <- str_split(histPrices[[1]][j], '/')[[1]]
    saleMonth <- info[1]
    monthNum <- match(saleMonth, month.abb)
    saleYear <- as.numeric(info[2])
    salePrice <- as.numeric(info[3])
    if(monthNum >= 10 && saleYear == 2021) {
      histAbove <- histAbove - 1 
    }
    if(saleYear < 2012) {
      histAbove <- histAbove - 1
    }
    if(salePrice  < 5000) {
      histAbove <- histAbove - 1
    }
    #if(monthNum < 10 && saleYear >= 2012 && saleYear <= 2021 && salePrice > 5000) { histAbove <- histAbove + 1}
    else {
      histAbove <- histAbove + 1
    }
  }
  if(i == 1824 || i == 3763) {
    # Can't seem to get rid of these two points. They are troublesome.
    next
  }
  if(histAbove >= histCount) {
    keepIndexNew <- c(keepIndexNew, i)
  }
}

newTrainNew <- data5[-keepIndexNew, ]
newTestNew <- data5[keepIndexNew, ]

#samplesNew <- sample(nrow(data5), nrow(data5)*0.6)
#newTrainNew <- data5[samplesNew, ]
#newTestNew <- data5[-samplesNew, ]

y <- 'price'
formulaNew <-  paste0(".*. + I(", names(data5)[names(data5)!=y], "^2)+", collapse="") %>%
  paste(y, "~", .) %>%
  substr(., 1, nchar(.)-1) %>%
  as.formula

modNormalNew <- lm(formulaNew, data=newTrainNew)

# Feature selection
x <- model.matrix(modNormalNew)[, -1]
str(x)
y <- model.response(model.frame(modNormalNew))

fitness <- function(s)
{
  inc <- which(s == 1)
  X <- cbind(1, x[, inc])
  mod <- lm.fit(X, y)
  class(mod) <- 'lm'
  -AIC(mod)
}

library(GA)
GANew <- ga("binary", fitness=fitness, nBits=ncol(x),
         names=colnames(x), monitor=plot, popSize=100)

selectedGANew <- which(GANew@solution[1, ] == 1)
selectedNamesGANew <- names(selectedGANew)

formulaStrNew <- paste('price ~ ', selectedNamesGANew[1], sep='')
for(i in 2:length(selectedNamesGA)) {
  formulaStrNew <- paste(formulaStrNew, selectedNamesGANew[i], sep='+')
}
newFormulaNew <- as.formula(formulaStrNew)
modSelectedNew <- lm(newFormulaNew, data=newTrainNew)

predictSelected <- data.frame(pred=predict(modSelectedNew, newdata=newTestNew), actual=newTestNew$price, 
                              index=as.numeric(rownames(newTestNew)), row.names=rownames(newTestNew))
# MSE
predictSelectedMSE <- mean((predictSelected$actual - predictSelected$pred)^2)
predictSelectedRMSE <- sqrt(predictSelectedMSE)

# Get the historical prices
library(stringr)
historical2 <- read.csv('hist.csv', check.names=F)
predHistoricalNew <- data.frame(actual=newTestNew$price, index=as.numeric(rownames(newTestNew)), row.names=rownames(newTestNew))
for(i in 1:nrow(newTestNew)) {
  testSample <- data[rownames(newTestNew[i, ]), ]
  rowInt <- rownames(newTestNew[i, ])
  #predHistoricalNew[rowInt, 'index'] <- testSample$index
  suburb <- historical2[historical2$suburb == tolower(testSample$suburb), ]
  histSales <- testSample$salesHistory
  histSales2 <- str_split(histSales, '-')
  histPrices <- data.frame()
  for(j in 1:length(histSales2[[1]])) {
    info <- str_split(histSales2[[1]][j], '/')[[1]]
    # Skip over any sales less than $5000 in case a rental history snuck in.
    if(as.numeric(info[3]) < 5000) {
      next
    }
    #histPrices <- c(histPrices, c(info[1:3]))
    histPrices[j, 'month'] <- info[1]
    histPrices[j, 'year'] <- as.numeric(info[2])
    histPrices[j, 'price'] <- as.numeric(info[3])
  }
  
  histPrices
  histPricesSimple <- c()
  ratios <- c()
  testColumn <- ''
  if(length(histPrices) > 0) {
    for(j in 1:length(histPrices)) {
      year <- histPrices[j, 'year']
      if(is.na(year)) { next }
      if(year == 2021) {
        month <- histPrices[j, 'month']
        #https://stackoverflow.com/questions/6549239/convert-months-mmm-to-numeric
        monthNum <- match(month, month.abb)
        # Add a 0 to the left if the month isn't 10
        if(monthNum >= 10) { next }
        monthNum <- str_pad(monthNum, 2, 'left', '0')
        #testColumn <- c(testColumn, paste('2021-10/2021-', as.character(monthNum), sep=''))
        testColumn <- paste('2021-10/2021-', as.character(monthNum), sep='')
        ratios <- c(ratios, suburb[1, testColumn])
        ratio <- suburb[1, testColumn]
        #histPricesSimple <- c(histPricesSimple, histPrices[j, 3])
        histPricesSimple <- c(histPricesSimple, ratio*histPrices[j, 'price'])
      }
      else if(year >= 2012){
        #testColumn <- c(testColumn, paste('2021', year, sep='/'))
        testColumn <- paste('2021', year, sep='/')
        ratios <- c(ratios, suburb[1, testColumn])
        ratio <- suburb[1, testColumn]
        #histPricesSimple <- c(histPricesSimple, histPrices[j, 3])
        histPricesSimple <- c(histPricesSimple, ratio*histPrices[j, 'price'])
      }
    }
  }
  
  if(length(histPricesSimple) == 0) {
    predHistoricalNew[rowInt, 'histPrices'] <- paste('0')
  }
  else {
    predHistoricalNew[rowInt, 'histPrices'] <- paste(histPricesSimple, collapse='/')
  }
  
  weight <- 1/(length(ratios))
  weight
  predd <- 0
  for(j in 1:length(ratios)) {
   predd <- predd + (histPricesSimple[j]* weight)
  }
  if(length(predd) == 0) {
    predHistoricalNew[rowInt, 'predHist'] <- 0
  }
  else {
    predHistoricalNew[rowInt, 'predHist'] <- predd
  }
}

joinedPreds <- merge(predictSelected, predHistoricalNew, by='index')
joinedPreds <- subset(joinedPreds, select=c('index', 'pred', 'actual.x', 'histPrices', 'predHist'))

# https://www.statology.org/how-to-rename-data-frame-columns-in-r/
names(joinedPreds)[names(joinedPreds) == 'actual.x'] <- 'actual'

for(i in 1:nrow(joinedPreds)) {
  # Unpack historical prices
  histPrices <- str_split(joinedPreds[i, 'histPrices'], '/')[[1]]
  histPricesIndv <- c()
  for(j in 1:length(histPrices)) {
    histPricesIndv <- c(histPricesIndv, as.numeric(histPrices[j]))
  }
  predPrice <- joinedPreds[i, 'pred']
  joinedPreds[i, 'hybrid'] <- mean(c(histPricesIndv, predPrice))
}

row.names(joinedPreds) <- joinedPreds$index
joinedPreds <- joinedPreds[ordered(joinedPreds$index), ]

# Regression MSE, RMSE
regressMSE <- mean((joinedPreds$pred - joinedPreds$actual)^2)
regressRMSE <- sqrt(regressMSE)

# Historical MSE, RMSE
histMSE <- mean((joinedPreds$predHist - joinedPreds$actual)^2)
histRMSE <- sqrt(histMSE)

# Hybrid MSE, RMSE
hybridMSE <- mean((joinedPreds$hybrid - joinedPreds$actual)^2)
hybridRMSE <- sqrt(hybridMSE)

regressRMSE
hybridRMSE
histRMSE

sprintf('RMSE Difference: Regression Model - Hybrid: %f', (regressRMSE - hybridRMSE))
sprintf('RMSE Difference: Historical - Hybrid: %f', (histRMSE - hybridRMSE))

# Model with weights
transformedDataTrain <- newTrainNew
transformedDataTrain[, 'bin1'] <- 0
transformedDataTrain[, 'bin2'] <- 0
transformedDataTrain[, 'bin3'] <- 0
transformedDataTrain[, 'bin4'] <- 0
transformedDataTrain[, 'bin5'] <- 0


getRatio <- function(month, year, suburb) {
  ratio <- 1
  if(is.na(year)) { 
    return(ratio)
  }
  if(year == 2021) {
    #https://stackoverflow.com/questions/6549239/convert-months-mmm-to-numeric
    monthNum <- match(month, month.abb)
    # Add a 0 to the left if the month isn't 10
    if(monthNum >= 10) {
      return(ratio)
    }
    monthNum <- str_pad(monthNum, 2, 'left', '0')
    testColumn <- paste('2021-10/2021-', as.character(monthNum), sep='')
    ratio <- suburb[1, testColumn]
  }
  else if(year >= 2012){
    testColumn <- paste('2021', year, sep='/')
    ratio <- suburb[1, testColumn]
  }
  
  ratio
}

library(stringr)
for(i in 1:nrow(transformedDataTrain)) {
  testSample <- data[rownames(transformedDataTrain[i, ]), ]
  rowInt <- rownames(transformedDataTrain[i, ])
  suburb <- historical2[historical2$suburb == tolower(testSample$suburb), ]
  histSales <- testSample$salesHistory
  histSales2 <- str_split(histSales, '-')
  histPrices <- data.frame()
  tempSalesYear <- c()
  tempSalesPrice <- c()
  for(j in 1:length(histSales2[[1]])) {
    info <- str_split(histSales2[[1]][j], '/')[[1]]
    # Skip over any sales less than $5000 in case a rental history snuck in.
    if(as.numeric(info[3]) < 5000) {
      next
    }
    
    saleMonth <- info[1]
    saleYear <- as.numeric(info[2])
    salePrice <- as.numeric(info[3])
    
    if(saleYear < 2012) {
      next
    }
    
    tempSalesYear <- c(tempSalesYear, saleYear)
    tempSalesPrice <- c(tempSalesPrice, salePrice)
    
    if(j == 1) {
      ratio <- getRatio(saleMonth, saleYear, suburb)
      histPrices[j, 'year'] <- saleYear
      histPrices[j, 'price'] <- (salePrice*as.numeric(ratio))
    }
    
    bIndv <- F
    if(j > 1) {
      for(k in 1:length(tempSalesYear)) {
        if(k == j) {
          next
        }
        ratio <- getRatio(saleMonth, saleYear, suburb)
        if(tempSalesYear[k] %in% c(2020, 2021) && saleYear %in% c(2020, 2021)) {
          adjSalePrice <- (salePrice*as.numeric(ratio))
          salePrice <- mean(c(tempSalesPrice[k], adjSalePrice))
          histPrices[k, 'price'] <- salePrice
          histPrices[k, 'year'] <- tempSalesYear[k] 
        }
        else if(tempSalesYear[k] %in% c(2018, 2019) && saleYear %in% c(2018, 2019)) {
          adjSalePrice <- (salePrice*as.numeric(ratio))
          salePrice <- mean(c(tempSalesPrice[k], adjSalePrice))
          histPrices[k, 'price'] <- salePrice
          histPrices[k, 'year'] <- tempSalesYear[k] 
        }
        else if(tempSalesYear[k] %in% c(2016, 2017) && saleYear %in% c(2016, 2017)) {
          adjSalePrice <- (salePrice*as.numeric(ratio))
          salePrice <- mean(c(tempSalesPrice[k], adjSalePrice))
          histPrices[k, 'price'] <- salePrice
          histPrices[k, 'year'] <- tempSalesYear[k] 
        }
        else if(tempSalesYear[k] %in% c(2014, 2015) && saleYear %in% c(2014, 2015)) {
          adjSalePrice <- (salePrice*as.numeric(ratio))
          salePrice <- mean(c(tempSalesPrice[k], adjSalePrice))
          histPrices[k, 'price'] <- salePrice
          histPrices[k, 'year'] <- tempSalesYear[k] 
        }
        else if(tempSalesYear[k] %in% c(2012, 2013) && saleYear %in% c(2012, 2013)) {
          adjSalePrice <- (salePrice*as.numeric(ratio))
          salePrice <- mean(c(tempSalesPrice[k], adjSalePrice))
          histPrices[k, 'price'] <- salePrice
          histPrices[k, 'year'] <- tempSalesYear[k] 
        }
        else {
          bIndv <- T
        }
      }
    }
    if(bIndv) {
      ratio <- getRatio(saleMonth, saleYear, suburb)
      histPrices[j, 'year'] <- saleYear
      histPrices[j, 'price'] <- (salePrice*as.numeric(ratio))
    }
  }
  
  if(length(histPrices) > 0) {
    for(j in 1:length(histPrices)) {
      year <- histPrices[j, 'year']
      if(is.na(year)) { next }
      if(year %in% c(2020, 2021)) {
        transformedDataTrain[rowInt, 'bin5'] <- histPrices[j, 'price']
      }
      else if(year %in% c(2018, 2019)) {
        transformedDataTrain[rowInt, 'bin4'] <- histPrices[j, 'price']
      }
      else if(year %in% c(2016, 2017)) {
        transformedDataTrain[rowInt, 'bin3'] <- histPrices[j, 'price']
      }
      else if(year %in% c(2014, 2015)) {
        transformedDataTrain[rowInt, 'bin2'] <- histPrices[j, 'price']
      }
      else if(year %in% c(2012, 2013)) {
        transformedDataTrain[rowInt, 'bin1'] <- histPrices[j, 'price']
      }
    }
  }
}

transformedDataTest <- newTestNew
transformedDataTest[, 'bin1'] <- 0
transformedDataTest[, 'bin2'] <- 0
transformedDataTest[, 'bin3'] <- 0
transformedDataTest[, 'bin4'] <- 0
transformedDataTest[, 'bin5'] <- 0

for(i in 1:nrow(transformedDataTest)) {
  testSample <- data[rownames(transformedDataTest[i, ]), ]
  rowInt <- rownames(transformedDataTest[i, ])
  suburb <- historical2[historical2$suburb == tolower(testSample$suburb), ]
  histSales <- testSample$salesHistory
  histSales2 <- str_split(histSales, '-')
  histPrices <- data.frame()
  tempSalesYear <- c()
  tempSalesPrice <- c()
  for(j in 1:length(histSales2[[1]])) {
    info <- str_split(histSales2[[1]][j], '/')[[1]]
    # Skip over any sales less than $5000 in case a rental history snuck in.
    if(as.numeric(info[3]) < 5000) {
      next
    }
    
    saleMonth <- info[1]
    saleYear <- as.numeric(info[2])
    salePrice <- as.numeric(info[3])
    
    if(saleYear < 2012) {
      next
    }
    
    tempSalesYear <- c(tempSalesYear, saleYear)
    tempSalesPrice <- c(tempSalesPrice, salePrice)
    
    if(j == 1) {
      ratio <- getRatio(saleMonth, saleYear, suburb)
      histPrices[j, 'year'] <- saleYear
      histPrices[j, 'price'] <- salePrice      
    }
    
    bIndv <- F
    if(j > 1) {
      for(k in 1:length(tempSalesYear)) {
        if(k == j) {
          next
        }
        ratio <- getRatio(saleMonth, saleYear, suburb)
        if(tempSalesYear[k] %in% c(2020, 2021) && saleYear %in% c(2020, 2021)) {
          adjSalePrice <- (salePrice*as.numeric(ratio))
          salePrice <- mean(c(tempSalesPrice[k], adjSalePrice))
          histPrices[k, 'price'] <- salePrice
          histPrices[k, 'year'] <- tempSalesYear[k] 
        }
        else if(tempSalesYear[k] %in% c(2018, 2019) && saleYear %in% c(2018, 2019)) {
          adjSalePrice <- (salePrice*as.numeric(ratio))
          salePrice <- mean(c(tempSalesPrice[k], adjSalePrice))
          histPrices[k, 'price'] <- salePrice
          histPrices[k, 'year'] <- tempSalesYear[k] 
        }
        else if(tempSalesYear[k] %in% c(2016, 2017) && saleYear %in% c(2016, 2017)) {
          adjSalePrice <- (salePrice*as.numeric(ratio))
          salePrice <- mean(c(tempSalesPrice[k], adjSalePrice))
          histPrices[k, 'price'] <- salePrice
          histPrices[k, 'year'] <- tempSalesYear[k] 
        }
        else if(tempSalesYear[k] %in% c(2014, 2015) && saleYear %in% c(2014, 2015)) {
          adjSalePrice <- (salePrice*as.numeric(ratio))
          salePrice <- mean(c(tempSalesPrice[k], adjSalePrice))
          histPrices[k, 'price'] <- salePrice
          histPrices[k, 'year'] <- tempSalesYear[k] 
        }
        else if(tempSalesYear[k] %in% c(2012, 2013) && saleYear %in% c(2012, 2013)) {
          adjSalePrice <- (salePrice*as.numeric(ratio))
          salePrice <- mean(c(tempSalesPrice[k], ajdSalePrice))
          histPrices[k, 'price'] <- salePrice
          histPrices[k, 'year'] <- tempSalesYear[k] 
        }
        else {
          bIndv <- T
        }
      }
    }
    if(bIndv) {
      histPrices[j, 'year'] <- saleYear
      histPrices[j, 'price'] <- salePrice  
    }
  }
  
  if(length(histPrices) > 0) {
    for(j in 1:length(histPrices)) {
      year <- histPrices[j, 'year']
      if(is.na(year)) { next }
      if(year %in% c(2020, 2021)) {
        transformedDataTest[rowInt, 'bin5'] <- histPrices[j, 'price']
      }
      else if(year %in% c(2018, 2019)) {
        transformedDataTest[rowInt, 'bin4'] <- histPrices[j, 'price']
      }
      else if(year %in% c(2016, 2017)) {
        transformedDataTest[rowInt, 'bin3'] <- histPrices[j, 'price']
      }
      else if(year %in% c(2014, 2015)) {
        transformedDataTest[rowInt, 'bin2'] <- histPrices[j, 'price']
      }
      else if(year %in% c(2012, 2013)) {
        transformedDataTest[rowInt, 'bin1'] <- histPrices[j, 'price']
      }
    }
  }
}

# Add indicators
transformedDataTrain2 <- transformedDataTrain 
transformedDataTest2 <- transformedDataTest
for(i in 1:5) {
  bB <- paste0('bBin', i)
  b <- paste0('bin', i)
  transformedDataTrain2[, bB] <- apply(transformedDataTrain2, 1,
                                     FUN=function(x) if(x[b] == 0) 0 else 1)
  transformedDataTest2[, bB] <- apply(transformedDataTest2, 1,
                                     FUN=function(x) if(x[b] == 0) 0 else 1)
}

# Chosen arbitrarily
wtFn <- function(x) {
  (9/10) * exp(-x/5)
}

# pr = regression price
# phi = price historical for the bins
# pa = actual price of the house
# w = weights, to be estimated
#     1:   Regression weight
#     2:6: The weights per bin
# iphi: Indicator, if the historical price even exists.
# How to get w?
fancyFormula <- function(pr, phi, pa, iphi, w) {
  (w[1]*pr + w[2]*phi[1] + w[3]*phi[2] + w[4]*phi[3] + w[5]*phi[4] + w[6]*phi[5]) /  (w[1] + sum(w[2:6]*iphi))
}

fancyFormula(joinedPreds['9', 'pred'], as.numeric(transformedDataTest2['9', 14:18]), joinedPreds['9', 'actual'],
             as.numeric(transformedDataTest2['1', 19:23]), c(0.1, 0.2, 0.2, 0.2, 0.2, 0.3))

(0.1*joinedPreds['9', 'pred'] + 0.2*transformedDataTest2['9', 14] + 0.2*transformedDataTest2['9', 15] +
    0.2*transformedDataTest2['9', 16] + 0.2*transformedDataTest2['9', 17] + 0.3*transformedDataTest2['9', 18]) /
  (0.1 + (sum(c(0.2, 0.2, 0.2, 0.2, 0.3)*unlist(transformedDataTest2['9', 19:23]))))

