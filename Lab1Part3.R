install.packages("twitteR")
install.packages("rtweet")
library(rtweet)
library(twitteR)

apiKey<- "api key"
apiSecretKey <- "api secret key"
accessToken<-"your access token"
accessTokenSecret<-"secret key"
setup_twitter_oauth(apiKey,apiSecretKey,accessToken,accessTokenSecret)
fluTweets <- searchTwitter("flu", n=10000,lang="en")
head(fluTweets)

df_fluTweets <- twListToDF(fluTweets)
write.csv(df_fluTweets, file = "twitter_data/raw_twitteR_flu_10k.csv")

twitter_token <- create_token(app = "Flu-Analysis-EDA",
                              consumer_key = "XcHOAVRMJa0PnGMhBKsbZOF3W",
                              consumer_secret = "06gmf6S12GbulHThDQA3AlWB5el2e8HbVrYdlqIHKp6vTf2ucf")

tweets <- search_tweets(q = "flu", n = 15000, token = twitter_token, geocode = lookup_coords("usa"))
save_as_csv(tweets,"twitter_data/raw_rtweet_flu_10k.csv")

register_google(key = 'AIzaSyBChIhz8FzwkvhgiJEC4fFqgMuXpMEofAM') 
raw_tweet_csv<-read.csv("twitter_data/user_lat_long_10kusers_march_2.csv")
df_raw_tweet_csv <- data.frame((raw_tweet_csv))
df_clean_df_raw_tweet_csv <-subset(df_raw_tweet_csv, (lon != "NA"))

df_clean_df_raw_tweet_csv<-subset(df_raw_tweet_csv, (lat != "NA"))
df_clean_df_raw_tweet_csv<-subset(df_raw_tweet_csv, (address != "NA"))
write.csv(df_clean_df_raw_tweet_csv, file = "twitter_data/clean_user_lat_long_10kusers_march_2.csv")



library(maps)
library(sp)    
library(maptools)
#This function is refer from https://stackoverflow.com/questions/8751497/latitude-longitude-coordinates-to-state-code-in-r
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

geodata <- data.frame( df_clean_df_raw_tweet_csv$lon, df_clean_df_raw_tweet_csv$lat)
usStatelocation <- latlong2state(geodata)
stateFreq <- table(usStatelocation)
df_state <- data.frame(stateFreq)
write.csv(df_state, file = "stateFreqDF.csv",row.names=FALSE, na="")


library(ggplot2)
library(maptools)
library(maps)
pFreqData <- read.csv(file="stateFreqDF.csv")
head(pFreqData)
library(RColorBrewer)

myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
us <- map_data("state")
head(us)

gg <- ggplot()
gg <- gg + geom_map(data=us, map=us,aes(x = long, y = lat, map_id=region),
                    fill="#ffffff", color="#ffffff", size=1)
gg <- gg + geom_map(data=pFreqData, map=us,
                    aes(fill=Freq, map_id=pFreqData$usStatelocation),
                    color="#ffffff", size=0.15)
gg <- gg + labs(x="", y="", fill ="Frequency", title = "2017-18 Infulenza and Flu Tweets Frequency Indicator")
gg <- gg + coord_map("albers", lat0 = 39, lat1 = 45) 

gg <- gg + theme(panel.border = element_blank(), panel.background = element_blank(), axis.ticks = element_blank(),
                 axis.text = element_blank(), plot.title = element_text(size = 15, face = "bold"))
gg <- gg + scale_fill_gradientn(colours = myPalette(300), limits=c(0,350))
gg


map <- ggplot(df_state, aes(x= df_state$lon, y=df_state$lat,fill=Freq,group=group)) + geom_polygon(color="black")

map + scale_fill_gradient(name="Flu",low='white', high='red') +
  labs(title = "Twitter Flu Analysis") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position=c("0.9", "0.2"))

