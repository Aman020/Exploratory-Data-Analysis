install.packages("twitteR")
install.packages("rtweet")
install.packages("ggmap")
library(rtweet)
library(twitteR)
library(maps)
library(sp)    
library(maptools)
library(ggplot2)
library(maps)
library(RColorBrewer)

apiKey<- "XcHOAVRMJa0PnGMhBKsbZOF3W"
apiSecretKey <- "06gmf6S12GbulHThDQA3AlWB5el2e8HbVrYdlqIHKp6vTf2ucf"
accessToken<-"897511905970987008-WdYbvfk4wKWlUceed1EFLvXpaNhw4gJ"
accessTokenSecret<-"MXp4vCe3eHKPQREFkNI4BlBoeNqgqi4QGNpzZd79At8fV"
register_google(key = 'AIzaSyBChIhz8FzwkvhgiJEC4fFqgMuXpMEofAM')
twitter_token <- create_token(app = "Flu-Analysis-EDA",
                              consumer_key = "XcHOAVRMJa0PnGMhBKsbZOF3W",
                              consumer_secret = "06gmf6S12GbulHThDQA3AlWB5el2e8HbVrYdlqIHKp6vTf2ucf")
#Reference- https://stackoverflow.com/questions/8751497/latitude-longitude-coordinates-to-state-code-in-r
latlong2state <- function(pointsDF) {
  
  states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_sp <- map2SpatialPolygons(states, IDs=IDs,
                                   proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  indices <- over(pointsSP, states_sp)
  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
  stateNames[indices]
}


#-----------------Extracing Tweets with search string as flu-------------------------------------------



tweets_flu <- search_tweets(q = "flu", n = 15000, token = twitter_token, geocode = lookup_coords("usa"))
save_as_csv(tweets_flu,"Twitter_data/raw_rtweet_10k_flu.csv")
df_raw_tweet_flu_csv <- data.frame((tweets_flu))
geocode_ <- geocode(as.character(df_raw_tweet_flu_csv$location),output="more", override_limit=FALSE, source = c("google","dsk"))
df_geocode = data.frame(geocode_)
write.csv(df_geocode, file = "Twitter_data/cleandf_tweet_flu.csv")
df_geocode <-read.csv(file.choose())
df_clean_tweet_flu <-subset(df_geocode, (lon != "NA"))
df_clean_tweet_flu<-subset(df_geocode, (lat != "NA"))
df_clean_tweet_flu<-subset(df_geocode, (address != "NA"))
write.csv(df_clean_tweet_flu, file = "/Users/aman/R/Lab1EDA/Twitter_Data/cleandf_tweet_flu.csv")
df_clean_tweet_flu <- read.csv("/Users/aman/R/Lab1EDA/Twitter_Data/cleandf_tweet_flu.csv")



#-----------------Extracing Tweets with search string as #flu-------------------------------------------


df_raw_tweet_hashtag_flu_csv <- data.frame((tweets_hashtag_flu))
geocode_hashtag_flu <- geocode(as.character(df_raw_tweet_hashtag_flu_csv$location),output="more", override_limit=FALSE, source = c("google","dsk"))

df_clean_hashtag_tweet <-subset(geocode_hashtag_flu, (lon != "NA"))
df_clean_hashtag_tweet<-subset(geocode_hashtag_flu, (lat != "NA"))
df_clean_hashtag_tweet<-subset(geocode_hashtag_flu, (address != "NA"))
write.csv(df_clean_hashtag_tweet, file = "/Users/aman/R/Lab1EDA/Twitter_data/clean_df_rtweet_10k_hashtag_flu.csv")
df_clean_tweet_hashtagflu <- read.csv("/Users/aman/R/Lab1EDA/Twitter_data/clean_df_rtweet_10k_hashtag_flu.csv")

#------------- Merging all the longitude and latitude of all the tweets collected through various search strings-------

geodata_flue <- data.frame( df_clean_tweet_flu$lon, df_clean_tweet_flu$lat)
geodata_hashtagflu<- data.frame( df_clean_tweet_hashtagflu$lon, df_clean_tweet_hashtagflu$lat)
head(geodata_flue)
head(geodata_hashtagflu)
names(geodata_flue)<-names(geodata_hashtagflu)
geodata <-rbind (geodata_flue,geodata_hashtagflu )



usStatelocation <- latlong2state(geodata)
US_stateFreq <- table(usStatelocation)
df_US_state_frequency <- data.frame(US_stateFreq)
df_US_state_frequency
write.csv(df_US_state_frequency, file = "/Users/aman/R/Lab1EDA/Twitter_data/US_State_Frequency.csv",row.names=FALSE, na="")
us <- map_data("state")

# Reference -https://stackoverflow.com/questions/29614972/ggplot-us-state-map-colors-are-fine-polygons-jagged-r

gg <- ggplot()
gg <- gg + geom_map(data=us, map=us,aes(x = long, y = lat, map_id=region),
                    fill="#ffffff", color="#ffffff", size=1)
gg <- gg + geom_map(data=df_US_state_frequency, map=us,
                    aes(fill=df_US_state_frequency$Freq, map_id=df_US_state_frequency$usStatelocation),
                    color="#fddfff", size=0.15)
gg <- gg + labs(x="", y="", fill ="Frequency", title = "2019 Infulenza and Flu Tweets Frequency")
#gg <- gg + coord_map("albers", lat0 = 39, lat1 = 45) 
gg<-gg + scale_fill_gradient(name="Flu",low='white', high='red') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position=c("0.9", "0.2"))
gg <- gg + coord_map("albers", lat0 = 39, lat1 = 45) 
gg <- gg + theme(axis.ticks = element_blank())
gg



