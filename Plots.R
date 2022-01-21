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

graphDatePrice <- data[, c('dateObj', 'price', 'suburb', 'land_size', 'bedrooms', 'year')] %>%
  ggplot(aes(x=dateObj, y=price, colour=suburb)) +
  geom_point() +
  geom_vline(xintercept=as.Date('2012', '%Y')) +
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

graphBedroomsPrice <- data[, c('dateObj', 'price', 'suburb', 'land_size', 'bedrooms', 'bathrooms')] %>%
  ggplot(aes(x=bedrooms, y=price, color=bathrooms)) + 
  geom_point() +
  labs(title='Date vs. Bedrooms',
       x='Bedrooms',
       y='Sale Price ($AUD)',
       color='Bathrooms') +
  theme_few() +
  theme(axis.title=element_text())
  #facet_wrap(~bathrooms, nrow=2, ncol=4, scale='free_y')

graphBedroomsPrice


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
  geom_boxplot() +
  labs(x='Bedrooms',
       y='Bathrooms') +
  theme_bw() +
  theme(axis.title=element_text()) +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(breaks = pretty_breaks())
  #facet_wrap(~bathrooms, nrow=2, ncol=4, scale='free_y')

graphBoxBedrooms

