library(dplyr)
library(ggplot2)
library(ggthemes)
library(stringr)
library(gganimate)
library(png)
library(gifski)
library(GGally)
library(broom.helpers)
library(scales)
library(zoo)

data <- read.csv('big_removed.csv')
#add index
data$index <- 1:nrow(data)
data$dateObj <- as.Date(data$date, '%d-%b-%Y')
data$year <- as.numeric(format(data$dateObj, '%Y'))

suburbs <- read.csv('suburbs_data.csv')
suburbs$suburb <- str_to_title(suburbs$suburb)
crimeSum <- read.csv('sumSuburbs_columns.csv')
crimeSum$suburb <- str_to_title(crimeSum$suburb)
data2 <- data[, c(2, 3, 5:8, 16, 17)]
data3 <- merge(x=data2, y=suburbs, by='suburb', all.x=T)
data3 <- data3[order(data3$index), ]
data4 <- merge(x=data3, y=crimeSum, by='suburb', all.x=T)
data5 <- data4[, c(2:7, 11:14, 17:19)]

# Assuming dataAdj2 has been created in Looped.R
jj <- (merge(data, dataAdj2, by='row.names'))

graphDatePrice <- data[, c('dateObj', 'price', 'suburb', 'land_size', 'bedrooms', 'year')] %>%
  ggplot(aes(x=dateObj, y=price)) + #, colour=suburb)) +
  geom_point() +
  #geom_vline(xintercept=as.Date('2012', '%Y'), color='green') +
  labs(title='Date vs. Price',
       x='Date',
       y='Sale Price ($AUD)',
       color='Suburb') +
  ylim(0, 2500000) +
  theme_bw() +
  theme(axis.title=element_text())

graphDatePrice


graphDatePrice.animate <- graphDatePrice +
  transition_reveal(year) +
  view_follow(fixed_y=T)
  

anim <- animate(graphDatePrice.animate, height=700, width=1000, fps=30, duration=10, end_pause=180, res=100,
                renderer=gifski_renderer())

anim_save(filename='graphtest.gif', animation=anim)

historical2 <- read.csv('hist.csv', check.names=F)
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

graphBedroomsPrice <- bundoora[, c('dateObj', 'price', 'suburb', 'land_size', 'bedrooms', 'bathrooms')] %>%
  ggplot(aes(x=bedrooms, y=price)) + #, color=bathrooms)) + 
  geom_point() +
  labs(title='Bedrooms vs. Price Adj.',
       x='Bedrooms',
       y='Price Adjusted ($AUD)') + 
       #color='Bathrooms') +
  ylim(0, 2500000) +
  scale_x_continuous(breaks = pretty_breaks()) +
  theme_few() +
  theme(axis.title=element_text())
  #facet_wrap(~bathrooms, nrow=2, ncol=4, scale='free_y')

graphBedroomsPrice

graphBathroomsPrice <- bundoora[, c('dateObj', 'price', 'suburb', 'land_size', 'bedrooms', 'bathrooms')] %>%
  ggplot(aes(x=bathrooms, y=price)) + #, color=bathrooms)) + 
  geom_point() +
  labs(title='Bathrooms vs. Price Adj.',
       x='Bathooms',
       y='Sale Adjusted ($AUD)',
       color='Bathrooms') +
  ylim(0, 2500000) +
  scale_x_continuous(breaks = pretty_breaks()) +
  theme_few() +
  theme(axis.title=element_text())
  #facet_wrap(~bathrooms, nrow=2, ncol=4, scale='free_y')

graphBathroomsPrice

graphLandSizePrice <- data[, c('dateObj', 'price', 'suburb', 'land_size', 'bedrooms', 'bathrooms')] %>%
  ggplot(aes(x=land_size, y=price)) + #, color=bathrooms)) + 
  geom_point() +
  labs(title='Land Size vs. Price',
       x='Land Size',
       y='Sale Price ($AUD)',
       color='Bathrooms') +
  ylim(0, 2500000) +
  #scale_x_continuous(breaks = pretty_breaks()) +
  theme_few() +
  theme(axis.title=element_text())
  #facet_wrap(~bathrooms, nrow=2, ncol=4, scale='free_y')

graphLandSizePrice

graphBedroomsBathrooms <- data[, c('dateObj', 'price', 'suburb', 'land_size', 'bedrooms', 'bathrooms')] %>%
  ggplot(aes(x=bedrooms, y=bathrooms)) + #, color=bathrooms)) + 
  geom_point() +
  labs(title='Bedrooms vs. Bathrooms',
       x='Bedrooms',
       y='Bathooms') + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(breaks = pretty_breaks()) +
  theme_few() +
  theme(axis.title=element_text())
  #facet_wrap(~bathrooms, nrow=2, ncol=4, scale='free_y')

graphBedroomsBathrooms


graphBedroomsBathroomsBundoora <- bundoora[, c('dateObj', 'price', 'suburb', 'land_size', 'bedrooms', 'bathrooms')] %>%
  ggplot(aes(x=bedrooms, y=bathrooms)) + #, color=bathrooms)) + 
  geom_point() +
  labs(title='Bedrooms vs. Bathrooms',
       x='Bedrooms',
       y='Bathooms') + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(breaks = pretty_breaks()) +
  theme_few() +
  theme(axis.title=element_text())
  #facet_wrap(~bathrooms, nrow=2, ncol=4, scale='free_y')

graphBedroomsBathroomsBundoora

#toAddNames <- names(modNormalNew$coefficients[14:length(modNormalNew$coefficients)])
#data6 <- data5
#library(stringr)
#for(t in toAddNames) {
#  if(grepl('^2', t, fixed=T)) {
#    noSq <- substr(t, 3, str_length(t) - 3)
#    data6[, t] <- as.numeric(as.numeric(data6[, noSq]) * as.numeric(data6[, noSq]))
#  }
#  if(grepl(':', t, fixed=T)) {
#    varNames <- str_extract_all(t, '([\\w]+)([\\w]+)')
#    first <- varNames[[1]][1]
#    second <- varNames[[1]][2]
#    data6[, paste(first, second, sep=':')] <- as.numeric(as.numeric(data6[, first]) * as.numeric(data6[, second]))
#  }
#}

ggcorr(data5[, -1], label=T, label_alpha=T)

graphBoxBedrooms <- data[, c('dateObj', 'price', 'parking_spaces', 'land_size', 'bedrooms', 'bathrooms')] %>%
  ggplot(aes(x=bedrooms, y=bathrooms, group=bathrooms)) + 
  geom_point() +
  #geom_boxplot() +
  labs(x='Bedrooms',
       y='Bathrooms') +
  theme_bw() +
  theme(axis.title=element_text()) +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(breaks = pretty_breaks())
  #facet_wrap(~bathrooms, nrow=2, ncol=4, scale='free_y')

graphBoxBedrooms


graphDatePriceDiff <- jj[, c('dateObj', 'price.x', 'price.y', 'suburb')] %>%
  ggplot(aes(x=dateObj, y=price.y-price.x)) + #, colour=suburb)) +
  geom_point() +
  #geom_vline(xintercept=as.Date('2012', '%Y'), color='green') +
  labs(title='Date vs. Price Difference',
       x='Date',
       y='Price Difference($AUD)',
       color='Suburb') +
  ylim(0, 2500000) +
  theme_bw() +
  theme(axis.title=element_text())

graphDatePriceDiff

graphDatePriceAdj <- jj[, c('dateObj', 'price.x', 'price.y', 'suburb')] %>%
  ggplot(aes(x=dateObj, y=price.y)) + #, colour=suburb)) +
  geom_point() +
  geom_vline(xintercept=as.Date('01-Jan-2012', '%d-%b-%Y'), color='green') +
  labs(title='Date vs. Price Adj.',
       x='Date',
       y='Price Adjusted ($AUD)',
       color='Suburb') +
  ylim(0, 2500000) +
  theme_bw() +
  theme(axis.title=element_text())

graphDatePriceAdj

graphDistBallaratGeelong <- data5[, c('Dist_Ballarat', 'Dist_Geelong')] %>%
  ggplot(aes(x=Dist_Ballarat, y=Dist_Geelong)) +
  geom_point() + 
  labs(title='Dist. to Ballarat vs. Dist. to Geelong',
       x='Dist. to Ballarat',
       y='Dist. to Geelong') +
  theme_bw() +
  theme(axis.title=element_text())

graphDistBallaratGeelong 

graphElevGeelong <- data5[, c('elevation', 'Dist_Geelong')] %>%
  ggplot(aes(x=elevation, y=Dist_Geelong)) +
  geom_point() + 
  labs(title='Elevation vs. Dist. to Geelong',
       x='Elevation',
       y='Dist. to Geelong') +
  theme_bw() +
  theme(axis.title=element_text())

graphElevGeelong 

# https://stackoverflow.com/questions/13840761/add-moving-average-plot-to-time-series-plot-in-r
dataTsDf <- data[, c('dateObj', 'price')]
dataTsDf <- dataTsDf %>%
            group_by(dateObj) %>%  
            summarize(avg_price=median(price))
dataTsDf <- dataTsDf[order(dataTsDf$dateObj), ]
data.zoo <- zoo(dataTsDf$avg_price, dataTsDf$dateObj)
data.zoo2 <- merge(data.zoo, zoo(, seq(start(data.zoo), end(data.zoo), by='day')))
data.zoo3 <- na.spline(data.zoo2) 

#https://stackoverflow.com/questions/55719571/how-to-convert-a-list-of-zoo-objects-to-a-dataframe
ttt <- Reduce(function(x, y) merge(x, y, all = T), lapply(data.zoo3, function(x)
              cbind(`dateObj` = index(x), setNames(data.frame(x), attr(x, "avg_price")))))
names(ttt) <- c('dateObj', 'price')

merged <- merge(dataTsDf, ttt, by='dateObj', all.y=T)

#151 is 1970-06-01
as.Date(151)
#m.av <- rollmedian(data.zoo, 151, fill=c(NA, NULL, NA))
as.Date(366, origin='2012-01-07')
# 3 months
#m.av <- rollmedian(data.zoo3, 183, fill=c(NA, NULL, NA))
# 6 months
#m.av <- rollmedian(data.zoo3, 366, fill=c(NA, NULL, NA))
# 1 year
m.av <- rollmedian(data.zoo3, 732, fill=c(NA, NULL, NA))
merged$av <- coredata(m.av)

graphDatePriceRolling <-  ggplot(data=data, aes(x=dateObj, y=price)) +
  geom_point() + 
  geom_line(data=merged, aes(dateObj, av), lwd=0.5, col='green') + 
  labs(title='Date vs. Price',
       x='Date',
       y='Sale Price ($AUD)',
       color='Suburb') +
  ylim(0, 2500000) +
  theme_bw() +
  theme(axis.title=element_text())

graphDatePriceRolling

#dataAdjTsDf <- dataAdj[, c('dateObj', 'price')]
#dataAdjTsDf <- dataAdjTsDf[order(dataAdjTsDf$dateObj), ]
## https://community.rstudio.com/t/calculate-a-rolling-90-day-average-of-a-daily-time-series-with-duplicate-dates/17306/3
#dataAdjTsDf <- dataAdjTsDf %>%
#            group_by(dateObj) %>%  
#            summarize(avg_price=median(price))
#dataAdj.zoo <- zoo(dataAdjTsDf$avg_price, dataAdjTsDf$dateObj)
##151 is 1970-06-01
#as.Date(151)
#mAdj.av <- rollmedian(dataAdj.zoo, 151, fill=c(NA, NULL, NA))
#dataAdjTsDf$av = coredata(mAdj.av)
#
#graphDatePriceAdjRolling <-  ggplot(data=dataAdj, aes(x=dateObj, y=price)) +
#  geom_point() + 
#  geom_line(data=dataAdjTsDf, aes(dateObj, av), lwd=0.5, col='green') + 
#  labs(title='Date vs. Adj. Price',
#       x='Date',
#       y='Adjusted Price ($AUD)',
#       color='Suburb') +
#  ylim(0, 2500000) +
#  theme_bw() +
#  theme(axis.title=element_text())
#
#graphDatePriceAdjRolling
#

dataAdjTsDf <- dataAdj[, c('dateObj', 'price')]
dataAdjTsDf <- dataAdjTsDf %>%
  group_by(dateObj) %>%  
  summarize(avg_price=median(price))
dataAdjTsDf <- dataAdjTsDf[order(dataAdjTsDf$dateObj), ]
dataAdj.zoo <- zoo(dataAdjTsDf$avg_price, dataAdjTsDf$dateObj)
dataAdj.zoo2 <- merge(dataAdj.zoo, zoo(, seq(start(dataAdj.zoo), end(dataAdj.zoo), by='day')))
dataAdj.zoo3 <- na.spline(dataAdj.zoo2) 

#https://stackoverflow.com/questions/55719571/how-to-convert-a-list-of-zoo-objects-to-a-dataframe
ttt <- Reduce(function(x, y) merge(x, y, all = T), lapply(dataAdj.zoo3, function(x)
  cbind(`dateObj` = index(x), setNames(data.frame(x), attr(x, "avg_price")))))
names(ttt) <- c('dateObj', 'price')

mergedAdj <- merge(dataAdjTsDf, ttt, by='dateObj', all.y=T)

#151 is 1970-06-01
as.Date(151)
#m.av <- rollmedian(data.zoo, 151, fill=c(NA, NULL, NA))
as.Date(366, origin='2012-01-07')
# 3 months
#m.av <- rollmedian(dataAdj.zoo3, 183, fill=c(NA, NULL, NA))
# 6 months
#m.av <- rollmedian(dataAdj.zoo3, 366, fill=c(NA, NULL, NA))
# 1 year
m.av <- rollmedian(dataAdj.zoo3, 732, fill=c(NA, NULL, NA))
mergedAdj$av <- coredata(m.av)

graphDatePriceAdjRolling <-  ggplot(data=dataAdj, aes(x=dateObj, y=price)) +
  geom_point() + 
  geom_line(data=mergedAdj, aes(dateObj, av), lwd=0.5, col='green') + 
  labs(title='Date vs. Adj. Price',
       x='Date',
       y='Adjusted Price ($AUD)',
       color='Suburb') +
  ylim(0, 2500000) +
  theme_bw() +
  theme(axis.title=element_text())

graphDatePriceAdjRolling


# Andriy's suggestion
# Looks better
graphDatePriceSmooth <-  ggplot(data=data, aes(x=dateObj, y=price)) +
  geom_point() + 
  stat_smooth(method = "loess", geom='line', col='green', lwd=1) +
  labs(title='Date vs. Price',
       x='Date',
       y='Price ($AUD)',
       color='Suburb') +
  ylim(0, 2500000) +
  theme_bw() +
  theme(axis.title=element_text())

graphDatePriceSmooth


graphDatePriceAdjSmooth <-  ggplot(data=dataAdj, aes(x=dateObj, y=price)) +
  geom_point() + 
  stat_smooth(method = "loess", geom='line', col='green', lwd=1) +
  labs(title='Date vs. Adj. Price',
       x='Date',
       y='Adjusted Price ($AUD)',
       color='Suburb') +
  ylim(0, 2500000) +
  theme_bw() +
  theme(axis.title=element_text())

graphDatePriceAdjSmooth
