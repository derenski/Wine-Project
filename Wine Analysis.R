winemag.data_first150k <- read.csv("~/Desktop/USC/USC_spring_2018_classes/DSO 545/wine data/wine-reviews/winemag-data_first150k.csv", comment.char="#")
winemag.data.130k.v2 <- read.csv("~/Desktop/USC/USC_spring_2018_classes/DSO 545/wine data/wine-reviews/winemag-data-130k-v2.csv",
                                 stringsAsFactors = F)


winemag.data.130k.v2$country[which(winemag.data.130k.v2$country == 'US')] <- 'USA'
winemag.data.130k.v2$country[which(winemag.data.130k.v2$country == 'England')] <- 'UK'
library(MASS)
library(dplyr)
library(stringr)
library(magrittr)
library(ggmap)




### Data cleaning 

missing_name <- which(winemag.data.130k.v2$taster_name == '')


winemag.data.130k.v2$taster_name[missing_name] <- 'Unknown'

table(winemag.data.130k.v2$taster_name)
country_by_taster <- winemag.data.130k.v2 %>% group_by(taster_name) %>%  count(country)


## Begninning exploratory analysis 

boxplot(log(winemag.data.130k.v2$price) ~ winemag.data.130k.v2$country)



table(winemag.data.130k.v2$region_1)



boxplot(log(winemag.data.130k.v2$price) ~ winemag.data.130k.v2$variety)




varieties <- table(winemag.data.130k.v2$variety)


varieties[order(varieties, decreasing = T)][1:5]

length(unique(winemag.data.130k.v2$title))


### Getting the year with regular expressions 


years <- str_extract_all(winemag.data.130k.v2$title, pattern = '[0-9]{4}')

### Some titles had to years in them. 
terrible_twos <- which(lengths(years) == 2)

pairing <- matrix(NA, ncol = 2)


for (i in terrible_twos){
  which_to_pick <- which(str_detect(years[[i]], pattern = '^2[0]') == T)
  
  if (!(TRUE %in% str_detect(years[[i]], pattern = '^2[0]') )){
    
    pairing <- rbind(pairing, c(i,NA))
  
   } else{
  
  
  pairing <- rbind(pairing, c(i,years[[i]][which_to_pick]))
}
  
  }



good_ones <- which(lengths(years) == 1)

pairing <- rbind(pairing, cbind(good_ones,unlist(years[good_ones])))


truehist(as.numeric(pairing[,2]))

summary(as.numeric(pairing[,2]))

table(pairing[,2])

### Some had no year listed
unknown_year <- which(lengths(years) == 0)


pairing <- rbind(pairing, cbind(unknown_year, rep(NA, length(unknown_year))  ))


pairing <- data.frame(pairing)
names(pairing) <- c('id','year')
pairing$year <- as.numeric(as.character(pairing$year))

pairing <- pairing[-1,]

row.names(pairing) <- pairing$id

m <- merge(winemag.data.130k.v2, pairing, by = 'row.names')


truehist(m$year)


m$year[which(m$year < 1995)] <- NA


table(m$province)

#### Looking at California 

### 'California', 'Central Valley','San Jose','Santa Cruz','San Clemente',

california_wines <- m %>% filter( province %in% c('California','Central Valley','San Jose','Santa Cruz','San Clemente'))

cal_mod_1 <- lm(log(california_wines$price) ~ california_wines$points)

cal_mod_2 <- lm(log(good_cleaned_wine$Price) ~ good_cleaned_wine$Score)

summary(cal_mod_1)
summary(cal_mod_2)

plot(log(california_wines$price) ~ california_wines$points)




table(california_wines$region_2)


summary(good_cleaned_wine$Score)

summary(california_wines$points)

spain_wines <- m %>% filter(region_2 == 'Sonoma')




spain_mod_1 <- lm(log(spain_wines$price) ~ spain_wines$points)
summary(spain_mod_1)

tab <- table(m$country)[order(table(m$country), decreasing = T)]


### Beginning of Formal Analysis 


the_countries_pulled <- names(tab)[tab >= 73]

regions_we_want <- m %>% filter(country %in% the_countries_pulled)



##### Price vs selected countries

(ggplot(regions_we_want, aes(x = country, y= log(price),col = country))+geom_boxplot()
  +theme(axis.text=element_text(size=6)) + ylab('Natural log of Price')
  +ggtitle("Distribution of log of Wine Prices, by Country"))


ggplot(regions_we_want, aes(x = country, y= price))+geom_boxplot()


(ggplot(regions_we_want, aes(x = country, y= points,col = country))+geom_boxplot()
  +theme(axis.text=element_text(size=6)) )
ggplot(regions_we_want, aes(x = country, y= price))+geom_boxplot()




(ggplot(regions_we_want, aes(x = points, y= log(price), col = country))
+geom_point()+geom_smooth(method = 'lm', aes(col = country)))





### Price vs selected varieties 


t_var <- table(m$variety)[order(table(m$variety), decreasing  = T)]

t_var[1:15]



the_varieties_pulled <- names(t_var)[t_var >= 2466]

varieties_we_want <- m %>% filter(variety %in% the_varieties_pulled)





(ggplot(varieties_we_want, aes(x = variety, y= log(price),col = variety))+geom_boxplot()
  +theme(axis.text=element_text(size=6)) )
ggplot(varieties_we_want, aes(x = variety, y= price))+geom_boxplot()


(ggplot(varieties_we_want, aes(x = variety, y= points,col = variety))+geom_boxplot()
  +theme(axis.text=element_text(size=6)) )
ggplot(regions_we_want, aes(x = variety, y= price))+geom_boxplot()


price_table <- m %>% group_by(variety) %>% summarise('Average Price' = mean(price,na.rm = T))

price_table %>% arrange(desc(`Average Price`))


the_varieties_we_can_look_at <- names(t_var)[t_var > 30]

final_price_table <- price_table %>% filter(variety %in% the_varieties_we_can_look_at)


final_price_table %>% arrange(desc(`Average Price`))



### Looking at average prices 



price_table <- m %>% group_by(country) %>% summarise('Average Price' 
                                                     = sd(price,na.rm = T))

price_table %>% arrange(desc(`Average Price`))


the_countries_we_can_look_at <- names(tab)[tab > 30]

final_price_table <- price_table %>% filter(country %in% the_countries_we_can_look_at)


final_price_table %>% arrange(desc(`Average Price`))


### Metric: Number of outliers in each country (in terms of price) 

outlier <- function(x){
  
  return(x >=   quantile(x,.75,na.rm = T)+1.5*IQR(x,na.rm = T) )
  
}

prop <- (regions_we_want %>% group_by(country) 
%>% summarise(premium_product =sum(outlier(price),na.rm = T) )
%>% arrange(desc(premium_product)))

prop$country <-factor(prop$country, levels = prop$country, ordered = T)

(ggplot(prop[1:10,], aes(x = country, y = premium_product, group = 1)) + geom_line() + 
    geom_point(aes(col = country)) + ylab('Number of Premium Wines') 
  +ggtitle('Number of Premium Wines, for Various Countries')
)



pre_price <- (regions_we_want %>% group_by(country) 
         %>% filter(outlier(price )))

ggplot(pre_price, aes(x = factor(points), y = log(price))) + geom_boxplot()


ggplot(pre_price, aes(x = country, y = log(price))) + geom_boxplot()

premium_prices <- (pre_price   %>% summarise('Average_Premium' = median(price))
 %>% arrange(desc(Average_Premium))
)


median_price_per_point <- (pre_price %>% group_by(points) %>% 
  summarize(med_price = median(price, na.rm = T)))

(ggplot(median_price_per_point, aes(x = points, y = med_price)) + geom_point()
+geom_smooth(method = 'loess'))



(ggplot(pre_price, aes(x = points, y =price)) + geom_point()
  +geom_smooth(method = 'loess'))


l <-loess(pre_price$price ~ pre_price$points)




ggplot(data = NULL, aes(y = l$fitted, x = pre_price$points)) + geom_line()


View(premium_prices)



premium_above_93 <- pre_price %>% filter(points >= 93)



t_super_prem <- table(premium_above_93$country)[order(table(premium_above_93$country), decreasing = T)]


summary(lm(log(premium_above_93$price) ~ premium_above_93$points))

plot(log(premium_above_93$price) ~ premium_above_93$points)


t_super_prem[1:10]


cvars <- (pre_price %>% group_by(country) %>% summarise(
  coeff_of_var = 100*sd(price, na.rm = T)/mean(price, na.rm = T))
%>%arrange(desc(coeff_of_var)))

View(cvars)




##### Present initial maps 

### World Level


world_map <- map_data(map = 'world')

# world_map <- map_data(map = 'world', region = c(the_countries_pulled))

unique(regions_we_want$country) %in% unique(world_map$region)

#,'UK'




names(world_map)[5] <- 'country'


#for_working_with_spat_data <- (regions_we_want %>% group_by(country) %>% 
#                  summarize(max_price = mean(price, na.rm = T))
#)

m$ones <- rep(1, dim(m)[1])

for_working_with_spat_data <- (regions_we_want %>% group_by(country) %>% 
                  summarize(max_price = mean(points, na.rm = T))
)



merged_spatial_data <- right_join(for_working_with_spat_data,world_map, by = 'country')

unique(merged_spatial_data$country)


merged_spatial_data <- merged_spatial_data[order(merged_spatial_data$country),]


merged_spatial_data <- arrange(merged_spatial_data, group, order)

ggplot(world_map, aes(x = long, y = lat, group = group
)) + geom_polygon( color = 'black', fill = 'white') 


(ggplot(merged_spatial_data, aes(x = long, y = lat, group = group,fill = max_price
)) + geom_polygon( color = 'black') 
  + scale_fill_continuous(name = 'Average Rating',low = 'white', high = 'dark red')
  +ggtitle('Average Wine Rating for Selected Countries'))


###### For United States Only 

us_data <- regions_we_want[which(regions_we_want$country == 'USA'), ]

world_map <- map_data(map = 'state')


us_data$province <-tolower(us_data$province)

unique(world_map$region) %in% unique(us_data$province) 

#,'UK'




names(world_map)[5] <- 'province'


for_working_with_spat_data <- (us_data %>% group_by(province) %>% 
                                 summarize(max_price = max(price, na.rm = T))
)

merged_spatial_data <- right_join(for_working_with_spat_data,world_map, by = 'province')

unique(merged_spatial_data$province)


merged_spatial_data <- merged_spatial_data[order(merged_spatial_data$province),]


merged_spatial_data <- arrange(merged_spatial_data, group, order)

ggplot(world_map, aes(x = long, y = lat, group = group
)) + geom_polygon( color = 'black', fill = 'white') 


(ggplot(merged_spatial_data, aes(x = long, y = lat, group = group,fill = max_price
)) + geom_polygon( color = 'black') + scale_fill_gradient2(low = 'white', high = 'dark red'))


##### Present maps 

### FOR EUROPE ONLY

europeanUnion <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                   "Czech Rep.","Denmark","Estonia","Finland","France",
                   "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                   "Lithuania","Luxembourg","Malta","Netherlands","Poland",
                   "Portugal","Romania","Slovakia","Slovenia","Spain",
                   "Sweden","UK")

world_map <- map_data(map = 'world', region = europeanUnion)

# world_map <- map_data(map = 'world', region = c(the_countries_pulled))

unique(regions_we_want$country) %in% unique(world_map$region)

#,'UK'




names(world_map)[5] <- 'country'


#for_working_with_spat_data <- (regions_we_want %>% group_by(country) %>% 
#                  summarize(max_price = mean(price, na.rm = T))
#)

m$ones <- rep(1, dim(m)[1])

for_working_with_spat_data <- (m %>% group_by(country) %>% 
                                 summarize(max_Price = mean(points, na.rm = T))
)



merged_spatial_data <- right_join(for_working_with_spat_data,world_map, by = 'country')

unique(merged_spatial_data$country)


merged_spatial_data <- merged_spatial_data[order(merged_spatial_data$country),]


merged_spatial_data <- arrange(merged_spatial_data, group, order)

ggplot(world_map, aes(x = long, y = lat, group = group
)) + geom_polygon( color = 'black', fill = 'white') 


(ggplot(merged_spatial_data, aes(x = long, y = lat, group = group,fill = max_Price
)) + geom_polygon( color = 'black') + 
    scale_fill_continuous(name ='Average Rating',low = 'white', high = 'dark red')
  + ggtitle('Average Wine Rating for Several European Countries'))




### Looking at reviewers 

table(m$taster_name)


#m$taster_name == 'Kerin O’Keefe' m$country == 'USA'

anne <- !(m$taster_name %in% unique(m$taster_name)[-11])

### How to reviewers' scores relate to price? 
famous_dude <- m[which(m$taster_name == 'Virginie Boone'), ]

### Roger Voss, Virginie Boone,Michael Schachner,Kerin O’Keefe

mod <- loess(famous_dude$price ~ famous_dude$points)


preds <- predict(mod, seq(80,100,.01))


(ggplot(data = NULL, aes(y = preds, x = seq(80,100,.01))) + geom_line()
+ylab('Price')+xlab('Points') + ggtitle('Price Aginst Points, for Roger Voss')
)



(ggplot(data = famous_dude, aes(y = price, x = points)) + geom_smooth(method = 'loess')
  +ylab('Price')+xlab('Points') + ggtitle('Price Aginst Points, for Roger Voss')
)

summary(lm(log(famous_dude$price) ~ famous_dude$points))

plot(log(famous_dude$price) ~ famous_dude$points)

cat(famous_dude$description[2])


#### Prices in countries vs points 

tab[order(tab, decreasing = T)]

countries <- m[which(m$country %in% c('France','Italy','Germany','Chile',
                                    'Argentina','Spain','Portugal','Austria',
                                    'Australia')), ]


us_samp <- sample(which(m$country == 'USA'), size = 10000, replace = F)


us_samp <- m[us_samp,]


countries <- rbind(countries, us_samp)


## mod <- loess(country$price ~ country$points)


preds <- predict(mod, seq(80,100,.01))


#(ggplot(data = NULL, aes(y = preds, x = seq(80,100,.01))) + geom_line()
#  +ylab('Price')+xlab('Points') + ggtitle('Price Aginst Points, for Roger Voss')
#)



(ggplot(data = countries, aes(y = price, x = points, col= country)) + geom_smooth(method = 'loess')
  +ylab('Price')+xlab('Points') + ggtitle('Average Price Aginst Points, for Various Countries')
 + facet_wrap(c('country'), nrow = 2, ncol = 5, scales = 'free')
  +scale_y_continuous(breaks = seq(50,600,50))+scale_x_continuous(breaks = seq(80,100,4)))


summary(lm(log(country$price) ~ country$points))


summary(lm(log(m$price) ~ m$points))



### Do reviewers dominate a market? (how many reviewers per country?)

regions_we_want$count <- rep(1,dim(regions_we_want)[1])


nums <- (regions_we_want %>% group_by(country, taster_name) %>% 
    summarize(num = sum(count)) %>% group_by(country) 
)

props <- rep(NA, dim(nums)[1])

how_many <- c()
for (i in unique(nums$country)){
  
  props[nums$country == i] <- nums$num[nums$country == i]/sum(nums$num[nums$country == i], na.rm = T)
  how_many <-c(how_many, 1:length(which(nums$country == i)))
}

nums$prop <- props
nums$rank <- how_many


final_market <- nums %>% arrange(country, desc(prop))


how_many <- c()
for (i in unique(final_market$country)){
how_many <-c(how_many, 1:length(which(final_market$country == i)))

}


final_market$rank <- how_many 

(ggplot(final_market, aes(x = rank, y = prop, col = country, group = country)) + geom_line() 
  + theme_gray() + facet_wrap('country', nrow = 2, ncol = 12)
)



first_level <- table(final_market$country)


dist_of_nums <-c()

for (j in 1:16){

  dist_of_nums[j] <- length(which(first_level <= j))/length(first_level)


}

(ggplot(data = NULL, aes(x = seq(1,16,1), y = dist_of_nums)) + geom_line() + geom_point()
+scale_x_continuous(breaks = 1:16) + xlab('Number of Reviewers')
  +ylab('Proportion of Countries With that Number of Reviewers or Less')
  +ggtitle('Cumulative Proportion of Number of Reviewers in a Country'))



##### "Bang For Points" In different countries (the "Coefficient of Value)

percentage_coefficients <- matrix(ncol=  2)

for (i in unique(regions_we_want$country)){
  mod <- lm(log(price) ~ points, data = regions_we_want[which(regions_we_want$country == i),])
  
  percentage_coefficients <- rbind(percentage_coefficients,c(i ,mod$coefficients[2]))
  
}


percentage_coefficients <- percentage_coefficients[-1,]

coefficiect_data <- data.frame(percentage_coefficients, stringsAsFactors = F)

names(coefficiect_data) <- c('country','coefficient_of_value')

coefficiect_data$coefficient_of_value <- as.numeric(coefficiect_data$coefficient_of_value)

### World Level


world_map <- map_data(map = 'world')

# world_map <- map_data(map = 'world', region = c(the_countries_pulled))

unique(coefficiect_data$country) %in% unique(world_map$region)

#,'UK'




names(world_map)[5] <- 'country'


#for_working_with_spat_data <- (coefficiect_data %>% group_by(country) %>% 
#                  summarize(max_price = mean(price, na.rm = T))
#)

# m$ones <- rep(1, dim(m)[1])

for_working_with_spat_data <- coefficiect_data



merged_spatial_data <- right_join(for_working_with_spat_data,world_map, by = 'country')

unique(merged_spatial_data$country)


merged_spatial_data <- merged_spatial_data[order(merged_spatial_data$country),]


merged_spatial_data <- arrange(merged_spatial_data, group, order)

ggplot(world_map, aes(x = long, y = lat, group = group
)) + geom_polygon( color = 'black', fill = 'white') 


(ggplot(merged_spatial_data, aes(x = long, y = lat, group = group,fill = 
                      (exp(coefficient_of_value)-1)*100
)) + geom_polygon( color = 'black') 
  + scale_fill_gradient2( name = 'Coefficient of Value',low = 'white', high = 'dark red')
+ggtitle('Coefficient of Value for Various Countries')  )




### FOR EUROPE ONLY

europeanUnion <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                   "Czech Rep.","Denmark","Estonia","Finland","France",
                   "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                   "Lithuania","Luxembourg","Malta","Netherlands","Poland",
                   "Portugal","Romania","Slovakia","Slovenia","Spain",
                   "Sweden","UK")

world_map <- map_data(map = 'world', region = europeanUnion)

# world_map <- map_data(map = 'world', region = c(the_countries_pulled))

unique(coefficiect_data$country) %in% unique(world_map$region)

#,'UK'




names(world_map)[5] <- 'country'


#for_working_with_spat_data <- (regions_we_want %>% group_by(country) %>% 
#                  summarize(max_price = mean(price, na.rm = T))
#)

m$ones <- rep(1, dim(m)[1])

for_working_with_spat_data <- coefficiect_data



merged_spatial_data <- right_join(for_working_with_spat_data,world_map, by = 'country')

unique(merged_spatial_data$country)


merged_spatial_data <- merged_spatial_data[order(merged_spatial_data$country),]


merged_spatial_data <- arrange(merged_spatial_data, group, order)

ggplot(world_map, aes(x = long, y = lat, group = group
)) + geom_polygon( color = 'black', fill = 'white') 


(ggplot(merged_spatial_data, aes(x = long, y = lat, group = group,
                                 fill =(exp(coefficient_of_value)-1)*100
)) + geom_polygon( color = 'black') + 
    scale_fill_gradient2(name = 'Coefficient of Value',low = 'white', high = 'dark red')
  +ggtitle('Coefficient of Value for Europe') )


############################### Propensity of reviews over different years 


time_period <- m[ which(m$year >= 1997 & m$year <= 2016) ,c('points','year','ones')]

new_time <- time_period %>% group_by(year,points) %>% summarise(Propensity = sum(ones))



(ggplot(new_time, aes(points, year))+ 
    geom_tile(aes(fill = Propensity)) + ggtitle('Propensity of Ratings in Different Years')
  +scale_x_continuous(breaks = seq(80,100,2)) + scale_y_continuous(breaks = seq(1997,2016,3)))

ggplot(faithfuld, aes(waiting, eruptions)) +
  geom_(aes(fill = density), interpolate = TRUE)












###### Looking at Price against points for reviewers in several countries 

specific_country=regions_we_want %>% filter(country=='Italy'|country=='France'|country=='USA'|country=='Chile',
                                            country=='Portugal',country=='Germany', country=='New Zealand', country=='South Africa', 
                                            country=='Canada', country=='Argentina', country=='Spain', country=='Austria',
                                            country=='Greece',country=='Australia', country=='Buglaria',country=='Israel',country=='UK',
                                            country=='Hungary',country=='Georgia',country=='Uruguay',country=='Slovenia',country=='Turkey',
                                            country=='Croatia',country=='Romania')
Italy=regions_we_want %>% filter(country=='Italy'&taster_name!='Paul Gregutt'&taster_name!='Unknown')
France=regions_we_want %>% filter(country=='France' & taster_name!='Unknown'&taster_name!='Michael Schachner')
USA=regions_we_want %>% filter(country=='USA'& taster_name!='Unknown' & taster_name!='Fiona Adams' & taster_name!='Roger Voss' & taster_name!='Christina Pickard', taster_name!='Lauren Buzzeo')
Portugal=regions_we_want %>% filter(country=='Portugal'& taster_name!='Unknown'&taster_name=='Roger Voss')
Spain=regions_we_want %>% filter(country=='Spain'& taster_name!='Unknown'&taster_name=='Michael Schachner')

ggplot(Italy, aes(x=points, y=price))+geom_point()+facet_wrap(taster_name~., scale='free')+
  geom_smooth(method = 'loess', aes(col = taster_name), se=FALSE)+
  labs(color='Taster Name', x='Points', y='Price')+
  ggtitle('Relationship between Price and Points by Different Tasters in Italy')+
  theme(axis.text=element_text(size=6))
ggplot(France, aes(x=points, y=price))+geom_point()+facet_wrap(taster_name~., scale='free')+
  geom_smooth(method = 'loess', aes(col = taster_name), se=FALSE)+
  labs(color='Taster Name', x='Points', y='Price')+
  ggtitle('Relationship between Price and Points by Different Tasters in France')
theme(axis.text=element_text(size=6))
table(m$taster_name)
ggplot(USA, aes(x=points, y=price))+geom_point()+facet_wrap(taster_name~., scale='free')+
  geom_smooth(method = 'loess', aes(col = taster_name), se=FALSE)+
  labs(color='Taster Name', x='Points', y='Price')+
  ggtitle('Relationship between Price and Points by Different Tasters in USA')+
  theme(axis.text=element_text(size=6))
ggplot(Portugal, aes(x=points, y=price))+geom_point()+facet_wrap(taster_name~., scale='free')+
  geom_smooth(method = 'loess', aes(col = taster_name), se=FALSE)+
  labs(color='Taster Name', x='Points', y='Price')+
  ggtitle('Relationship between Price and Points by Different Tasters in Portugal')
theme(axis.text=element_text(size=6))
ggplot(Spain, aes(x=points, y=price))+geom_point()+facet_wrap(taster_name~., scale='free')+
  geom_smooth(method = 'loess', aes(col = taster_name), se=FALSE)+
  labs(color='Taster Name', x='Points', y='Price')+
  ggtitle('Relationship between Price and Points by Different Tasters in Spain')
theme(axis.text=element_text(size=6))
table(m$taster_name)


reviewers_1=m%>%filter(taster_name=='Anna Lee C. Iijima' | taster_name=='Anne Krebiehl MW' | 
                         taster_name=='Jim Gordon' | taster_name=='Joe Czerwinski' | 
                         taster_name=='Kerin O’Keefe' | taster_name=='Lauren Buzzeo' | 
                         taster_name=='Matt Kettmann')
ggplot(reviewers_1, aes(x=points, y=price))+geom_point()+facet_wrap(taster_name~., scale='free')+
  geom_smooth(method = 'loess', aes(col = taster_name))+
  labs(color='Taster Name', x='Points', y='Price')+
  ggtitle('Relationship between Price and Points by Different Tasters over the World')+
  theme(axis.text=element_text(size=6))
reviewers_2=m%>%filter(taster_name=='Michael Schachner' | taster_name=='Paul Gregutt' | 
                         taster_name=='Roger Voss' | taster_name=='Sean P. Sullivan' | 
                         taster_name=='Susan Kostrzewa' | taster_name=='Lauren Buzzeo' | 
                         taster_name=='Virginie Boone')
ggplot(reviewers_2, aes(x=points, y=price))+geom_point()+facet_wrap(taster_name~., scale='free', nrow=2)+
  geom_smooth(method = 'loess', aes(col = taster_name))+
  labs(color='Taster Name', x='Points', y='Price')+
  ggtitle('Relationship between Price and Points by Different Tasters over the World')+
  theme(axis.text=element_text(size=6))




