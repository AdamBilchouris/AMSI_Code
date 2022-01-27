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

#======== Models =========
# Get new samples. This removes the assumption that some might houses might have no historical sales.
# This will never be the case as the data was cleaned.

# Get avg  RMSE of all iterations.

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

train <- data5[-keepIndexNew, ]
test <- data5[keepIndexNew, ]

# 50% of current testing set gets put into a new dataset which is for testing. <- Validation
# The other 50% current test data + training data (3000 houses) <- New train.
# 10 , 20 , ... iterations.


#samplesNew <- sample(nrow(data5), nrow(data5)*0.6)
#train <- data5[samplesNew, ]
#test <- data5[-samplesNew, ]

library(dplyr)
y <- 'price'
formulaNew <-  paste0(".*. + I(", names(data5)[names(data5)!=y], "^2)+", collapse="") %>%
  paste(y, "~", .) %>%
  substr(., 1, nchar(.)-1) %>%
  as.formula

modNormalNew <- lm(formulaNew, data=train)

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
for(i in 2:length(selectedNamesGANew)) {
  formulaStrNew <- paste(formulaStrNew, selectedNamesGANew[i], sep='+')
}
newFormulaNew <- as.formula(formulaStrNew)
modSelectedNew <- lm(newFormulaNew, data=train)

predictSelected <- data.frame(pred=predict(modSelectedNew, newdata=test), actual=test$price, 
                              index=as.numeric(rownames(test)), row.names=rownames(test))


# MSE
predictSelectedMSE <- mean((predictSelected$actual - predictSelected$pred)^2)
predictSelectedRMSE <- sqrt(predictSelectedMSE)

# Get the historical prices
library(stringr)
historical2 <- read.csv('hist.csv', check.names=F)
predHistoricalNew <- data.frame(actual=test$price, index=as.numeric(rownames(test)), row.names=rownames(test))
for(i in 1:nrow(test)) {
  testSample <- data[rownames(test[i, ]), ]
  rowInt <- rownames(test[i, ])
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

# Training Data to predict weights
predictSelectedTrain <- data.frame(pred=predict(modSelectedNew, newdata=train), actual=train$price, 
                              index=as.numeric(rownames(train)), row.names=rownames(train))

predHistoricalNew <- data.frame(actual=train$price, index=as.numeric(rownames(train)), row.names=rownames(train))
for(i in 1:nrow(train)) {
  testSample <- data[rownames(train[i, ]), ]
  rowInt <- rownames(train[i, ])
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
        testColumn <- paste('2021-10/2021-', as.character(monthNum), sep='')
        ratios <- c(ratios, suburb[1, testColumn])
        ratio <- suburb[1, testColumn]
        histPricesSimple <- c(histPricesSimple, ratio*histPrices[j, 'price'])
      }
      else if(year >= 2012){
        #testColumn <- c(testColumn, paste('2021', year, sep='/'))
        testColumn <- paste('2021', year, sep='/')
        ratios <- c(ratios, suburb[1, testColumn])
        ratio <- suburb[1, testColumn]
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

joinedPredsTrain <- merge(predictSelectedTrain, predHistoricalNew, by='index')
joinedPredsTrain <- subset(joinedPredsTrain, select=c('index', 'pred', 'actual.x', 'histPrices', 'predHist'))

names(joinedPredsTrain)[names(joinedPredsTrain) == 'actual.x'] <- 'actual'

# Model with weights
transformedDataTrain <- train
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

transformedDataTest <- test
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

jp <- joinedPredsTrain
tdt2 <- transformedDataTrain2
fancyFormulaMSE <- function(w) {
  mean(
    (
      jp$actual - ((w[1]*jp$pred + w[2]*tdt2$bin1 + w[3]*tdt2$bin2 + w[4]*tdt2$bin3 + w[5]*tdt2$bin4 + w[6]*tdt2$bin5) / 
                 (w[1] + w[2]*tdt2$bBin1 + w[3]*tdt2$bBin2 + w[4]*tdt2$bBin3 + w[5]*tdt2$bBin4 + w[6]*tdt2$bBin5))
    )^2
  )
}

aaa <- optim(c(0.1, 0.1, 0.2, 0.2, 0.2, 0.2), fancyFormulaMSE, method='L-BFGS-B', lower=rep(0, 6))#, upper=rep(1, 6))
aaa$par
ww <- aaa$par/sum(aaa$par)
ww
sum(ww)

fancyFormula(joinedPreds['13', 'pred'], as.numeric(transformedDataTest2['13', 14:18]), joinedPreds['13', 'actual'],
             as.numeric(transformedDataTest2['13', 19:23]), ww)
joinedPreds['9', 'actual']

fancyFormula(joinedPreds['1', 'pred'], as.numeric(transformedDataTest2['1', 14:18]), joinedPreds['1', 'actual'],
             as.numeric(transformedDataTest2['1', 19:23]), ww)
joinedPreds['1', 'actual']

jpT <- joinedPreds
tdt2T <- transformedDataTest2
for(i in 1:nrow(jpT))
{
  pr <- jpT[i, 'pred']
  phi <- as.numeric(tdt2T[i, 14:18])
  pa <- jpT[i, 'actual']
  iphi <- as.numeric(tdt2T[i, 19:23])
  res <- fancyFormula(pr, phi, pa, iphi, ww)
  jpT[i, 'fancy'] <- res
}

fancyMSE <- mean((jpT$actual - jpT$fancy)^2)
fancyRMSE <- sqrt(fancyMSE)
fancyMSE
fancyRMSE

regressRMSE
histRMSE
hybridRMSE
fancyRMSE

sprintf('RMSE Difference: Regression Model - Historical: %f', (regressRMSE - histRMSE))
sprintf('RMSE Difference: Regression Model - Hybrid Mean: %f', (regressRMSE - hybridRMSE))
sprintf('RMSE Difference: Regression Model - Hybrid Fancy: %f', (regressRMSE - fancyRMSE))

sprintf('RMSE Difference: Historical - Hybrid Mean: %f', (histRMSE - hybridRMSE))
sprintf('RMSE Difference: Historical - Hybrid Fancy: %f', (histRMSE - fancyRMSE))

sprintf('RMSE Difference: Hybrid Mean - Hybrid Fancy: %f', (hybridRMSE - fancyRMSE))

#====== Isolation Forest ======
library(isotree)
isoForest <- isolation.forest(train[, c(2:13)], ntrees=1000)

isoPredTrain <- predict(isoForest, train[, c(2:13)])
predDfTrain <- data.frame(isoPredTrain)
names(predDfTrain)[names(predDfTrain) == 'isoPredTrain'] <- 'pred'
predDfTrain[, 'index'] <- rownames(predDfTrain)
predDfTrainFilter <- predDfTrain[predDfTrain['pred'] > 0.60, ]
orderedTrain <- predDfTrainFilter[order(predDfTrainFilter[, 'pred'], decreasing=T),]
indicesTrain <- c(orderedTrain$index)
dTrain <- data[unlist(indicesTrain), ]
dTrain['pred'] <- orderedTrain$pred
dTrain <- dTrain[, c(1, 3, 5:8, 15, 18)]
knitr::kable(dTrain)

isoPredTest <- predict(isoForest, test[, c(2:13)])
predDfTest <- data.frame(isoPredTest)
names(predDfTest)[names(predDfTest) == 'isoPredTest'] <- 'pred'
predDfTest[, 'index'] <- rownames(predDfTest)
predDfTestFilter <- predDfTest[predDfTest['pred'] > 0.6, ]
orderedTest <- predDfTestFilter[order(predDfTestFilter[, 'pred'], decreasing=T),]
indicesTest <- c(orderedTest$index)
dTest <- data[unlist(indicesTest), ]
dTest['pred'] <- orderedTest$pred
dTest <- dTest[, c(1, 3, 5:8, 15, 18)]
knitr::kable(dTest)

isoForestAll <- isolation.forest(data5[, c(2:13)], ntrees=1000)
isoPredAll <- predict(isoForestAll, data5[, c(2:13)])
predDfAll <- data.frame(isoPredAll)
names(predDfAll)[names(predDfAll) == 'isoPredAll'] <- 'pred'
predDfAll[, 'index'] <- rownames(predDfAll)
predDfAllFilter <- predDfAll[predDfAll['pred'] > 0.60, ]
orderedAll <- predDfAllFilter[order(predDfAllFilter[, 'pred'], decreasing=T),]
indicesAll <- c(orderedAll$index)
dAll <- data[unlist(indicesAll), ]
dAll['pred'] <- orderedAll$pred
dAll <- dAll[, c(1, 3, 5:8, 15, 18)]
knitr::kable(dAll)

# Predict prices through the three models.
dataIsoAll <- data5[indicesAll, ]
predRegressIsoAll <-  data.frame(pred=predict(modSelectedNew, newdata=dataIsoAll), actual=dataIsoAll$price, 
                              index=as.numeric(rownames(dataIsoAll)), row.names=rownames(dataIsoAll))

predHistoricalAll <- data.frame(actual=dataIsoAll$price, index=as.numeric(rownames(dataIsoAll)), row.names=rownames(dataIsoAll))
for(i in 1:nrow(dataIsoAll)) {
  testSample <- data[rownames(dataIsoAll[i, ]), ]
  rowInt <- rownames(dataIsoAll[i, ])
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
        testColumn <- paste('2021-10/2021-', as.character(monthNum), sep='')
        ratios <- c(ratios, suburb[1, testColumn])
        ratio <- suburb[1, testColumn]
        histPricesSimple <- c(histPricesSimple, ratio*histPrices[j, 'price'])
      }
      else if(year >= 2012){
        testColumn <- paste('2021', year, sep='/')
        ratios <- c(ratios, suburb[1, testColumn])
        ratio <- suburb[1, testColumn]
        histPricesSimple <- c(histPricesSimple, ratio*histPrices[j, 'price'])
      }
    }
  }
  
  if(length(histPricesSimple) == 0) {
    predHistoricalAll[rowInt, 'histPrices'] <- paste('0')
  }
  else {
    predHistoricalAll[rowInt, 'histPrices'] <- paste(histPricesSimple, collapse='/')
  }
  
  weight <- 1/(length(ratios))
  weight
  predd <- 0
  for(j in 1:length(ratios)) {
   predd <- predd + (histPricesSimple[j]* weight)
  }
  if(length(predd) == 0) {
    predHistoricalAll[rowInt, 'predHist'] <- 0
  }
  else {
    predHistoricalAll[rowInt, 'predHist'] <- predd
  }
}

joinedPredsAll <- merge(predRegressIsoAll, predHistoricalAll, by='index')
joinedPredsAll <- subset(joinedPredsAll, select=c('index', 'pred', 'actual.x', 'histPrices', 'predHist'))
names(joinedPredsAll)[names(joinedPredsAll) == 'actual.x'] <- 'actual'

for(i in 1:nrow(joinedPredsAll)) {
  # Unpack historical prices
  histPrices <- str_split(joinedPredsAll[i, 'histPrices'], '/')[[1]]
  histPricesIndv <- c()
  for(j in 1:length(histPrices)) {
    histPricesIndv <- c(histPricesIndv, as.numeric(histPrices[j]))
  }
  predPrice <- joinedPredsAll[i, 'pred']
  joinedPredsAll[i, 'hybrid'] <- mean(c(histPricesIndv, predPrice))
}

row.names(joinedPredsAll) <- joinedPredsAll$index
joinedPredsAll <- joinedPredsAll[ordered(joinedPredsAll$index), ]

transformedDataAll <- dataIsoAll
transformedDataAll[, 'bin1'] <- 0
transformedDataAll[, 'bin2'] <- 0
transformedDataAll[, 'bin3'] <- 0
transformedDataAll[, 'bin4'] <- 0
transformedDataAll[, 'bin5'] <- 0

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
for(i in 1:nrow(transformedDataAll)) {
  testSample <- data[rownames(transformedDataAll[i, ]), ]
  rowInt <- rownames(transformedDataAll[i, ])
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
        transformedDataAll[rowInt, 'bin5'] <- histPrices[j, 'price']
      }
      else if(year %in% c(2018, 2019)) {
        transformedDataAll[rowInt, 'bin4'] <- histPrices[j, 'price']
      }
      else if(year %in% c(2016, 2017)) {
        transformedDataAll[rowInt, 'bin3'] <- histPrices[j, 'price']
      }
      else if(year %in% c(2014, 2015)) {
        transformedDataAll[rowInt, 'bin2'] <- histPrices[j, 'price']
      }
      else if(year %in% c(2012, 2013)) {
        transformedDataAll[rowInt, 'bin1'] <- histPrices[j, 'price']
      }
    }
  }
}

transformedDataAll2 <- transformedDataAll
for(i in 1:5) {
  bB <- paste0('bBin', i)
  b <- paste0('bin', i)
  transformedDataAll2[, bB] <- apply(transformedDataAll2, 1,
                                     FUN=function(x) if(x[b] == 0) 0 else 1)
}

jpA <- joinedPredsAll
tdt2A <- transformedDataAll2
for(i in 1:nrow(jpA))
{
  pr <- jpA[i, 'pred']
  phi <- as.numeric(tdt2A[i, 14:18])
  pa <- jpA[i, 'actual']
  iphi <- as.numeric(tdt2A[i, 19:23])
  res <- fancyFormula(pr, phi, pa, iphi, ww)
  jpA[i, 'fancy'] <- res
}

fancyAllMSE <- mean((jpA$actual - jpA$fancy)^2)
fancyAllRMSE <- sqrt(fancyAllMSE)
fancyAllMSE
fancyAllRMSE

regressAllMSE <- mean((joinedPredsAll$pred - joinedPredsAll$actual)^2)
regressAllRMSE <- sqrt(regressAllMSE)

# Historical MSE, RMSE
histAllMSE <- mean((joinedPredsAll$predHist - joinedPredsAll$actual)^2)
histAllRMSE <- sqrt(histAllMSE)

# Hybrid MSE, RMSE
hybridAllMSE <- mean((joinedPredsAll$hybrid - joinedPredsAll$actual)^2)
hybridAllRMSE <- sqrt(hybridAllMSE)

regressAllRMSE
histAllRMSE
hybridAllRMSE
fancyAllRMSE

sprintf('RMSE Difference: Regression Model - Historical: %f', (regressAllRMSE - histAllRMSE))
sprintf('RMSE Difference: Regression Model - Hybrid Mean: %f', (regressAllRMSE - hybridAllRMSE))
sprintf('RMSE Difference: Regression Model - Hybrid Fancy: %f', (regressAllRMSE - fancyAllRMSE))

sprintf('RMSE Difference: Historical - Hybrid Mean: %f', (histAllRMSE - hybridAllRMSE))
sprintf('RMSE Difference: Historical - Hybrid Fancy: %f', (histAllRMSE - fancyAllRMSE))

sprintf('RMSE Difference: Hybrid Mean - Hybrid Fancy: %f', (hybridAllRMSE - fancyAllRMSE))


dataBundoora <- data[data$suburb == 'Bundoora', ]
bundoora <- dataBundoora
bundoora$dateObj <- as.Date(bundoora$date, '%d-%b-%Y')
bundoora$year <- as.numeric(format(bundoora$dateObj, '%Y'))

for(i in 1:nrow(bundoora)) {
  testSample <- data[rownames(bundoora[i, ]), ]
  rowInt <- rownames(bundoora[i, ])
  suburb <- historical2[historical2$suburb == 'bundoora', ]
  year <- as.numeric(format(bundoora[i, 'dateObj'], '%Y'))
    if(is.na(year)) { next }
    if(year == 2021) {
      monthNum <- as.numeric(format(bundoora[i, 'dateObj'], '%m'))
      #https://stackoverflow.com/questions/6549239/convert-months-mmm-to-numeric
      # Add a 0 to the left if the month isn't 10
      if(monthNum >= 10) { next }
      monthNum <- str_pad(monthNum, 2, 'left', '0')
      testColumn <- paste('2021-10/2021-', as.character(monthNum), sep='')
      ratio <- suburb[1, testColumn]
      bundoora[i, 'price'] <- ratio*bundoora[i, 'price']
    }
    else if(year >= 2012) {
      #testColumn <- c(testColumn, paste('2021', year, sep='/'))
      testColumn <- paste('2021', year, sep='/')
      ratio <- suburb[1, testColumn]
      bundoora[i, 'price'] <- ratio*bundoora[i, 'price']
  }
}

bundooraLM <- lm(price ~ dateObj, data=bundoora)
summary(bundooraLM)

mean2021 <- mean(bundoora[bundoora$year == 2021, ]$price)

bundoora2 <- bundoora$price - as.numeric(bundooraLM$coefficients[2])*as.numeric(bundoora$dateObj) + mean2021
plot(bundoora$dateObj, dataBundoora$price)
plot(bundoora$dateObj, bundoora$price)
plot(bundoora$dateObj, bundoora2)


dataAlbertPark <- data[data$suburb == 'Albert Park', ]
albertPark <- data[data$suburb == 'Albert Park', ]
albertPark$dateObj <- as.Date(albertPark$date, '%d-%b-%Y')
albertPark$year <- as.numeric(format(albertPark$dateObj, '%Y'))

for(i in 1:nrow(albertPark)) {
  testSample <- data[rownames(albertPark[i, ]), ]
  rowInt <- rownames(albertPark[i, ])
  suburb <- historical2[historical2$suburb == 'albert park', ]
  year <- as.numeric(format(albertPark[i, 'dateObj'], '%Y'))
    if(is.na(year)) { next }
    if(year == 2021) {
      monthNum <- as.numeric(format(albertPark[i, 'dateObj'], '%m'))
      #https://stackoverflow.com/questions/6549239/convert-months-mmm-to-numeric
      # Add a 0 to the left if the month isn't 10
      if(monthNum >= 10) { next }
      monthNum <- str_pad(monthNum, 2, 'left', '0')
      testColumn <- paste('2021-10/2021-', as.character(monthNum), sep='')
      ratio <- suburb[1, testColumn]
      albertPark[i, 'price'] <- ratio*albertPark[i, 'price']
    }
    else if(year >= 2012) {
      #testColumn <- c(testColumn, paste('2021', year, sep='/'))
      testColumn <- paste('2021', year, sep='/')
      ratio <- suburb[1, testColumn]
      albertPark[i, 'price'] <- ratio*albertPark[i, 'price']
  }
}

albertParkLM <- lm(price ~ dateObj, data=albertPark)
summary(albertParkLM)

mean2021 <- mean(albertPark[albertPark$year == 2021, ]$price)
mean2021

albertPark2 <- albertPark$price - as.numeric(albertParkLM$coefficients[2])*as.numeric(albertPark$dateObj) + mean2021
plot(albertPark$dateObj, dataAlbertPark$price)
plot(albertPark$dateObj, albertPark$price)
plot(albertPark$dateObj, albertPark2)

dataKew <- data[data$suburb == 'Kew', ]
kew <- data[data$suburb == 'Kew', ]
kew$dateObj <- as.Date(kew$date, '%d-%b-%Y')
kew$year <- as.numeric(format(kew$dateObj, '%Y'))

for(i in 1:nrow(kew)) {
  testSample <- data[rownames(kew[i, ]), ]
  rowInt <- rownames(kew[i, ])
  suburb <- historical2[historical2$suburb == 'kew', ]
  year <- as.numeric(format(kew[i, 'dateObj'], '%Y'))
    if(is.na(year)) { next }
    if(year == 2021) {
      monthNum <- as.numeric(format(kew[i, 'dateObj'], '%m'))
      #https://stackoverflow.com/questions/6549239/convert-months-mmm-to-numeric
      # Add a 0 to the left if the month isn't 10
      if(monthNum >= 10) { next }
      monthNum <- str_pad(monthNum, 2, 'left', '0')
      testColumn <- paste('2021-10/2021-', as.character(monthNum), sep='')
      ratio <- suburb[1, testColumn]
      kew[i, 'price'] <- ratio*kew[i, 'price']
    }
    else if(year >= 2012) {
      #testColumn <- c(testColumn, paste('2021', year, sep='/'))
      testColumn <- paste('2021', year, sep='/')
      ratio <- suburb[1, testColumn]
      kew[i, 'price'] <- ratio*kew[i, 'price']
  }
}

kewLM <- lm(price ~ dateObj, data=kew)
summary(kewLM)

mean2021 <- mean(kew[kew$year == 2021, ]$price)
mean2021

kew2 <- kew$price - as.numeric(kewLM$coefficients[2])*as.numeric(kew$dateObj) + mean2021
plot(kew$dateObj, dataKew$price)
plot(kew$dateObj, kew$price)
plot(kew$dateObj, kew2)

