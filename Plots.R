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

graphBedroomsPrice <- bundoora[, c('dateObj', 'price', 'suburb', 'land_size', 'bedrooms', 'bathrooms')] %>%
  ggplot(aes(x=bedrooms, y=price)) + #, color=bathrooms)) + 
  geom_point() +
  labs(title='Bedrooms vs. Price',
       x='Bedrooms',
       y='Sale Price ($AUD)') + 
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
  labs(title='Bathrooms vs. Price',
       x='Bathooms',
       y='Sale Price ($AUD)',
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
  ggplot(aes(x=dateObj, y=price.y, colour=suburb)) +
  geom_point() +
  geom_vline(xintercept=as.Date('2012', '%Y'), color='green') +
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
