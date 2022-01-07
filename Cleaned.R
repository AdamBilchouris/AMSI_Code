#======== READING =========
data <- read.csv('big_removed.csv')
#add index
data$index <- 1:nrow(data)
str(head(data))
# Make it non-exponential ?

suburbs <- read.csv('suburbs_data.csv')
str(head(suburbs))
crimeSum <- read.csv('sumSuburbs_columns.csv')
str(head(crimeSum))

data2 <- data[, c(2, 3, 5:8, 16, 17)]

data3 <- merge(x=data2, y=suburbs, by='suburb', all.y=F)
data3 <- data3[order(data3$index), ]

data4 <- merge(x=data3, y=crimeSum, by='suburb', all.y=F)

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
