data <- read.csv('big_removed.csv')
#add index
data$index <- 1:nrow(data)
str(head(data))

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
#data5[, 'price'] <- data5[, 'price'] / 100000
#data5[, 'median_income'] <- data5[, 'median_income'] / 100000

samples <- sample(nrow(data5), nrow(data5)*0.6)
train <- data5[samples, ]
test <- data5[-samples, ]

#=================   SQUARED + INTERACTIONS   ======================
# https://stackoverflow.com/questions/29032284/r-quadratic-regression-all-factors-how-to-specify-a-formula
library(dplyr)
y    <- 'price'
formula <-  paste0(".*. + I(", names(data5)[names(data5)!=y], "^2)+", collapse="") %>%
  paste(y, "~", .) %>%
  substr(., 1, nchar(.)-1) %>%
  as.formula
mod <- lm(formula, data=train)

summary(mod)

x <- model.matrix(mod)[, -1]
str(x)
y <- model.response(model.frame(mod))

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


predd <- predict(mod, newdata=test[20,])
aaaaaa <- test[20, ]
indexTest <- rownames(test[20, ])
test[20, ]
data4[indexTest, ]
data[indexTest, ]
predd

#========== HISTORICAL ATTEMPT =============
historical2 <- read.csv('hist.csv', check.names=F)
doreen <- historical2[historical2$suburb == 'doreen', ]
testSample <- data[indexTest, ]
testDate <- testSample$date
testYear <- strsplit(testDate, '-')[[1]][3]
testColumn <- paste('2021', testYear, sep='/')
testColumn

testYearInt <- as.numeric(testYear)

2021 - testYearInt

# don't include 2021
testColumns <- data.frame()
for(i in testYearInt:2020) {
  c <- paste('2021', as.character(i), sep='/')
  testColumns[1, c] <- doreen[, c]
}

weightUnif <- 1 / length(testColumns)
predd2 <- predd
for(i in length(testColumns)) {
  predd2 <- predd2 + (weightUnif*predd2*testColumns[, i])
}
data[indexTest, ]
predd
predd2

#predd * (1/4 * dromana[, testColumn] + 1/4 
testSample$salesHistory

#==============   FEATURES SELECTION   ==========================
# https://dzone.com/articles/feature-selection-using-genetic-algorithms-in-r
# 
selected <- which(GA@solution[1, ] == 1)
selectedNames <- names(selected)
selectedNames

formulaStr <- paste('price ~ ', selectedNames[1], sep='')
for(i in 2:length(selectedNames)) {
  formulaStr <- paste(formulaStr, selectedNames[i], sep='+')
}
formulaStr
newFormula <- as.formula(formulaStr)
modSelected <- lm(newFormula, data=train)
predd <- predict(modSelected, newdata=test[20,])
aaaaaa <- test[20, ]
indexTest <- rownames(test[20, ])
data4[indexTest, ]
data[indexTest, ]
predd


#=======  ???????? ABOVE WORKS BETTER (I think) ======================
modRemoved <- mod
aaa <- which(GA@solution[1, ] == 1)
modRemoved$coefficients <- modRemoved$coefficients[aaa]
modRemoved$assign <- modRemoved$assign[aaa]
modRemoved$rank <- length(aaa)
summary(mod)
summary(modRemoved)

predd <- predict(modRemoved, newdata=test[20,])
aaaaaa <- test[20, ]
indexTest <- rownames(test[20, ])
data4[indexTest, ]
data[indexTest, ]
predd


#================== ISOLATION FOREST ====================
library(isotree)
library(knitr)

isoData <- data5[, c(2:13)]
iso <- isolation.forest(isoData, ntrees=1000, nthreads=3)
pred <- predict(iso, isoData)
data[which.max(pred), ]

plot(pred)

predDf <- data.frame(pred)
predDf['index'] <- seq.int(nrow(predDf))
predDfFilter <- predDf[predDf['pred'] > 0.60, ]
print(predDfFilter)
ordered <- predDfFilter[order(predDfFilter$pred, decreasing=T),]
#print(ordered)

indices <- c(ordered$index)
indices
d <- data[unlist(indices), ]
d['pred'] <- ordered$pred
d <- d[, c(1, 3, 5:8, 15, 18)]
knitr::kable(d)

predd <- predict(modSelected, newdata=data5[rownames(d), ])

d
data[rownames(d), ]$url
predd


# Look at ways to find weird houses.

#==================   HISTORICAL   =========================== 
historical2 <- read.csv('hist.csv', check.names=F)
testSample <- data[rownames(test[30, ]), ]
suburb <- historical2[historical2$suburb == tolower(testSample$suburb), ]
histSales <- testSample$salesHistory
library(stringr)
histSales2 <- str_split(histSales, '-')
histPrices <- data.frame()
for(i in 1:length(histSales2[[1]])) {
  info <- str_split(histSales2[[1]][i], '/')[[1]]
  # Skip over any sales less than $5000 in case a rental history snuck in.
  if(info[3] < 5000) {
    next
  }
  #histPrices <- c(histPrices, c(info[1:3]))
  histPrices[i, 'month'] <- info[1]
  histPrices[i, 'year'] <- info[2]
  histPrices[i, 'price'] <- info[3]
}

histPrices
histPricesSimple <- c()
ratios <- c()
testColumn <- c()
for(i in 1:length(histPrices)) {
  year <- histPrices[i, 2]
  if(is.na(year)) { next }
  if(year == '2021') {
    month <- histPrices[i, 1]
    #https://stackoverflow.com/questions/6549239/convert-months-mmm-to-numeric
    monthNum <- match(month, month.abb)
    #testColumn <- c(testColumn, paste('2021-10/2021-', as.character(monthNum), sep=''))
    testColumn <- paste('2021-10/2021-', as.character(monthNum), sep='')
  }
  else {
    #testColumn <- c(testColumn, paste('2021', year, sep='/'))
    testColumn <- paste('2021', year, sep='/')
  }
  ratios <- c(ratios, suburb[1, testColumn])
  histPricesSimple <- c(histPricesSimple, as.numeric(histPrices[i, 3]))
}


## Year only
#for(i in 1:(floor(length(histPrices)/3))) {
#  year <- histPrices[i+1]
#  if(year == '2021') {
#    month <- histPrices[i*base]
#    #https://stackoverflow.com/questions/6549239/convert-months-mmm-to-numeric
#    monthNum <- match(month, month.abb)
#    testColumn <- c(testColumn, paste('2021-10/2021-', as.character(monthNum), sep=''))
#  }
#  else {
#    testColumn <- c(testColumn, paste('2021', year, sep='/'))
#  }
#  #ratios <- c(ratios, suburb[1, testColumn])
#  histPricesSimple <- c(histPricesSimple, as.numeric(histPrices[(i*base)+2]))
#  #print(paste(histPrices[i], histPrices[i+1], histPrices[i+2]))
#}

weight <- 1/(length(ratios))
weight
predd <- 0
for(i in 1:length(ratios)) {
  print(predd)
  predd <- predd + (histPricesSimple[i]) * (ratios[i] * weight^i)
  print(predd)
}
predd
ratios

#========= STANDARDISE ===========
library(dplyr)
dataStd <- data4[, c(2:7, 11:14, 17:29)] %>% mutate_all(~(scale(.) %>% as.vector))
head(dataStd)

dataStd2 <- dataStd[, c(1:13)]
# Check for 0s and 1s:
# https://stackoverflow.com/questions/15215457/standardize-data-columns-in-r
apply(dataStd2, 2, mean)
apply(dataStd2, 2, sd)

samplesStd <- sample(nrow(dataStd), nrow(dataStd)*0.6)
trainStd <- dataStd2[samples, ]
testStd <- dataStd2[-samples, ]

y    <- 'price'
formula <-  paste0(".*. + I(", names(dataStd2)[names(dataStd2)!=y], "^2)+", collapse="") %>%
  paste(y, "~", .) %>%
  substr(., 1, nchar(.)-1) %>%
  as.formula
mod <- lm(formula, data=trainStd)

summary(mod)

x <- model.matrix(mod)[, -1]
str(x)
y <- model.response(model.frame(mod))

fitness <- function(s)
{
  inc <- which(s == 1)
  X <- cbind(1, x[, inc])
  mod <- lm.fit(X, y)
  class(mod) <- 'lm'
  -AIC(mod)
}

library(GA)
GAStd <- ga("binary", fitness=fitness, nBits=ncol(x),
         names=colnames(x), monitor=plot, popSize=100)

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

predd <- predict(mod, newdata=testStd[20,])
aaaaaa <- test[20, ]
indexTest <- rownames(testStd[20, ])
testStd[20, ]
dataStd2[indexTest, ]
dataStd[indexTest, ]
data[indexTest, ]
predd
#sd * z-score + mean
sd(data5[, 1])*predd + mean(data5[, 1])

#===== ISOLATION FOREST WITH VARIABLES =====
toAddNames <- names(mod$coefficients[14:length(mod$coefficients)])

data6 <- data5
library(stringr)
for(t in toAddNames) {
  if(grepl('^2', t, fixed=T)) {
    #varName <- str_extract(t, '\\(([^)]+)\\)')
    #noBracket <- str_replace_all(varName, '[\\(\\)]', '')
    #print(noSq)
    #data6[, noBracket] <- as.numeric(as.numeric(data6[, noSq]) * as.numeric(data6[, noSq]))#noSq <- substr(noBracket, 1, str_length(noBracket) - 2)
    
    noSq <- substr(t, 3, str_length(t) - 3)
    data6[, t] <- as.numeric(as.numeric(data6[, noSq]) * as.numeric(data6[, noSq]))
  }
  if(grepl(':', t, fixed=T)) {
    varNames <- str_extract_all(t, '([\\w]+)([\\w]+)')
    first <- varNames[[1]][1]
    second <- varNames[[1]][2]
    data6[, paste(first, second, sep=':')] <- as.numeric(as.numeric(data6[, first]) * as.numeric(data6[, second]))
    
  }
}

# Isolation forest using all vars
library(isotree)
library(knitr)

isoDataAll <- data6[, c(2:length(data6[1, ]))]
isoAll <- isolation.forest(isoDataAll, ntrees=1000, nthreads=3)
pred <- predict(isoAll, isoDataAll)
data[which.max(pred), ]

plot(pred)

predDf <- data.frame(pred)
predDf['index'] <- seq.int(nrow(predDf))
predDfFilter <- predDf[predDf['pred'] > 0.60, ]
print(predDfFilter)
ordered <- predDfFilter[order(predDfFilter$pred, decreasing=T),]
#print(ordered)

indices <- c(ordered$index)
indices
d <- data[unlist(indices), ]
d['pred'] <- ordered$pred
d <- d[, c(1, 3, 5:8, 15, 18)]
knitr::kable(d)

allSet <- indices

samplesAll <- sample(nrow(data6), nrow(data6)*0.6)
trainAll <- data6[, samplesAll]
testAll <- data6[, -samplesAll]

# Use GA to get features, use them only.
modAll <- lm(price ~ ., data=trainAll)
x <- model.matrix(modAll)[, -1]
str(x)
y <- model.response(model.frame(modAll))

fitness <- function(s)
{
  inc <- which(s == 1)
  X <- cbind(1, x[, inc])
  mod <- lm.fit(X, y)
  class(mod) <- 'lm'
  -AIC(mod)
}

library(GA)
GAIso<- ga("binary", fitness=fitness, nBits=ncol(x),
         names=colnames(x), monitor=plot, popSize=100)

selectedIso <- which(GAIso@solution[1, ] == 1)
selectedIsoNames <- names(selectedIso)
selectedIsoNames

selectedIso

data7 <- data6[, c(1, selectedIso)]
isoDataFiltered <- data7[, c(2:length(data7[1, ]))]
isoFiltered <- isolation.forest(isoDataFiltered, ntrees=1000, nthreads=3)
pred <- predict(isoFiltered, isoDataFiltered)
data[which.max(pred), ]

plot(pred)

predDf <- data.frame(pred)
predDf['index'] <- seq.int(nrow(predDf))
predDfFilter <- predDf[predDf['pred'] > 0.60, ]
print(predDfFilter)
ordered <- predDfFilter[order(predDfFilter$pred, decreasing=T),]
#print(ordered)

indices <- c(ordered$index)
indices
d <- data[unlist(indices), ]
d['pred'] <- ordered$pred
d <- d[, c(1, 3, 5:8, 15, 18)]
knitr::kable(d)
filteredSet <- indices

allFiltered <- setdiff(allSet, filteredSet)
filteredAll <- setdiff(filteredSet, allSet)
allFiltered
filteredAll

allSet
filteredSet

#===== GET 2 SALES IN 2021 ========
library(stringr)
#str_count('Dec/2016/11500000.0/SOLD-Aug/2014/10000000.0/SOLD-May/2007/6600000.0/SOLD', '-') + 1
keepIndex <- c()
for(i in 1:nrow(data)) {
  histKeep <- str_count(data[i, 'salesHistory'], '-') >= 1
  yearKeep <- str_split(data[i, 'date'], '-')[[1]][3] == '2021'
  if(histKeep && yearKeep) {
    keepIndex <- c(keepIndex, as.numeric(i))
  }
}

length(keepIndex)
data[keepIndex, c(4, 5:8, 15, 17)]
#data5[keepIndex, c(2:5)]
newTrain <- data5[-keepIndex, ]
newTest <- data5[keepIndex, ]
