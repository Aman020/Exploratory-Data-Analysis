install.packages("twitteR")
install.packages("rtweet")
install.packages("ggmap")
library(rtweet)
library(ggmap)
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
getAndSaveTweets<- function(searchQuery,tweetsCount, oauthToken, outputFileName){
  tweets_flu <- search_tweets(q = searchQuery, n = tweetsCount, token = oauthToken, geocode = lookup_coords("usa"))
  tweets_flu <- subset(tweets_flu, tweets_flu$is_retweet == FALSE)
  save_as_csv(tweets_flu,outputFileName)
  return(tweets_flu)
}
cleanAndSaveGeocodedTweets<- function (rawTweets, outputFileName ){
  
  
  df_raw_tweet_flu_csv <- data.frame((rawTweets))
  geocode <- geocode(as.character(df_raw_tweet_flu_csv$location),output="more", override_limit=FALSE, source = c("google","dsk"))
  df_geocode = data.frame(geocode)
  df_clean_tweet_flu <-subset(df_geocode, (lon != "NA"))
  df_clean_tweet_flu<-subset(df_geocode, (lat != "NA"))
  df_clean_tweet_flu<-subset(df_geocode, (address != "NA"))
  write.csv(df_clean_tweet_flu, file = outputFileName)
  return(df_clean_tweet_flu)

  }
mergeGeocode<-function(...){
  argsList <- list(...)
  geocode <-data.frame()
  for(geocodeData in argsList)
  {
    geocode <- rbind(geocode,geocodeData)
  }
   return( geocode)
}
plotHeatMap <- function(data,location, frequency){
  # Reference -https://stackoverflow.com/questions/29614972/ggplot-us-state-map-colors-are-fine-polygons-jagged-r
  
  library(ggmap)
 
  library(maps)
  library(sp)    
  library(maptools)
  library(ggplot2)
  library(maps)
  library(RColorBrewer)
  us <- map_data("state")
  
  gg <- ggplot()
  gg <- gg + geom_map(data=us, map=us,aes(x = long, y = lat, map_id=region),
                      fill="#ffffff", color="#ffffff", size=1)
  gg <- gg + geom_map(data=data, map=us,
                      aes(fill=frequency, map_id=location),
                      color="#fddfff", size=0.15)
  gg <- gg + labs(x="", y="", fill ="Frequency", title = "2019 Infulenza and Flu Tweets Frequency")
  #gg <- gg + coord_map("albers", lat0 = 39, lat1 = 45) 
  gg<-gg + scale_fill_gradient(name="Flu",low='white', high='red') +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(legend.position=c("0.9", "0.2"))
  gg <- gg + coord_map("albers", lat0 = 39, lat1 = 45) 
  gg <- gg + theme(axis.ticks = element_blank())
  gg
}
getStateFrequency<-function(geocodes){
  dfGeodata<-data.frame( geocodes$lon, geocodes$lat)

  usStatelocation <- latlong2state(dfGeodata)
  US_stateFreq <- table(usStatelocation)
  df_US_state_frequency <- data.frame(US_stateFreq)
  return(df_US_state_frequency)
}


tweetsFlu <- getAndSaveTweets("flu",15000,twitter_token,"Twitter_data/raw_15k_flu_2mar.csv" )
tweetsHashFlu <- getAndSaveTweets("#flu",15000,twitter_token,"Twitter_data/raw_15k_HashFlu_2mar.csv" )
tweetsInfluenza <- getAndSaveTweets("influenza",15000,twitter_token,"Twitter_data/raw_15k_influenza_2mar.csv" )

#------------------ Cleaning the data and getting the geocodes. Saving it in a file------------------
cleanTweetsFlu <- cleanAndSaveGeocodedTweets(tweetsFlu,"Twitter_data/clean_15k_flu_2mar.csv")
cleanTweetsHashFlu <- cleanAndSaveGeocodedTweets(tweetsHashFlu,"Twitter_data/clean_15k_hash_flu_2mar.csv")
cleanTweetsInfluenza <- cleanAndSaveGeocodedTweets(tweetsInfluenza,"Twitter_data/clean_15k_influenza_2mar.csv")

#-----------------Plotting geocodes of  all the tweets collected using search query as flu----------
dfGeodataTweetsFluStateFreq<-getStateFrequency(cleanTweetsFlu)
plotHeatMap(dfGeodataTweetsFluStateFreq,dfGeodataTweetsFluStateFreq$usStatelocation,dfGeodataTweetsFluStateFreq$Freq)


#-----------------Plotting geocodes of  all the tweets collected using search query as #flu----------

dfGeodataTweetsHashFluStateFreq<-getStateFrequency(cleanTweetsHashFlu)
plotHeatMap(dfGeodataTweetsHashFluStateFreq,dfGeodataTweetsHashFluStateFreq$usStatelocation,dfGeodataTweetsHashFluStateFreq$Freq)

#-------------------Plotting geocodes of  all the tweets collected using search query as influenza----------
dfGeodataTweetsInfluenzaStateFreq<-getStateFrequency(cleanTweetsInfluenza)
plotHeatMap(dfGeodataTweetsInfluenzaStateFreq,dfGeodataTweetsInfluenzaStateFreq$usStatelocation,dfGeodataTweetsInfluenzaStateFreq$Freq)



#-------------------- Plotting all the geocodes of all the tweets collected-------------------
allGeodata <- mergeGeocode(cleanTweetsFlu,cleanTweetsInfluenza,cleanTweetsHashFlu)
allGeodataStateFreq<-getStateFrequency(allGeodata)
write.csv(allGeodataStateFreq, file = "/Users/aman/R/Lab1EDA/Twitter_data/US_State_Frequency_2mar.csv",row.names=FALSE, na="")
plotHeatMap(allGeodataStateFreq,allGeodataStateFreq$usStatelocation,allGeodataStateFreq$Freq)
