                                DATA COLLECTION AND EXPLORATORY DATA ANALYSIS
                                
## Goal:
1. Work on data analysis related to socially relevant current topic. (What better than “flu”?)
2. Learn by analysis of existing data analysis examples or vignettes. (TwitteR vignette)
3. Learn from data analysis and reporting examples available in public sources. (Flu.gov)
4. Apply methods for collecting data from publicly available data sources: flu.gov, fluview.
5. Install a work environment for carrying out various activities of the data science process: Jupyter, R,
    RStudio.
6. Extract data using APIs and OAuth keys (for collecting tweets).
7. Process the data collected for simple data analysis and charting (reproducible research).


## Preperation:

Here are the preliminary requirements for this project. 
1. Work environment: We will working on Jupyter with R kernel. Install Jupyter and R kernel.
2. Create an account on twitter as a user as well as a developer. In the developer site, the tab Apps is of particular interest. (After you create a twitter developer account,) you will click on Apps to create a new app called Lab1. Fill in the required fields as per the instructions given there. Once you submit you should be able to get the OAuth credentials 
***[9]*** that had four parts: Customer API key, Customer API secret, Access Token Key and Access Token Secret. All are needed for programmatically working with Twitter. (***Yes, you can auto-tweet, if you know what I mean ;-)***
3. R community has created a package for working with Twitter data called “twitteR”. Read the vignette by Jeff Gentry ***[12]*** about the package he contributed. Work on the vignette.


## Part 1:
Get familier with R and R studio. Run all the sample code.

## Part 2: Creation of charts


1. Navigate to the CDC site of flu data and analysis, flu.gov ***[11]*** and fluview ***[10]*** . Take a few minutes to review the contents and understand the context.
2. The fluview site discusses many variables or features that affect the reality that is playing out in front of our eyes.
3. Browse through the page, identify and review the charts and data (plain tables, .csv) under each chart; here are the ones with data.
  1) Influenza national summary (green and yellow chart)
  2) Positive tested
  3) Influenza sub-type pie-charts
  4) Mortality
  5) Pediatric deaths
  6) Influenza-like illness
  7) Flu heat map of USA (Required)
  You can down all these data and more from this link ***[13]*** .
4. Now repeat at least five of the seven flu charts discussed in the flu report for the week of Jan 28th, 2019. Beware, by the time we review it, it might have moved on to the next week and the data and the charts may be different. The last one, heat map is required for part 3 of your lab.
5.The chart 1) and 2) are provided only for the partial year. Aren’t we curious about the flu pattern for the entire year or 52 weeks? You can get this data appropriately selecting data from ***[13]*** and repeating the charting process using R (on Jupyter or R studio).
6. Now repeat this chart in 5) for just New York state. You can get this data also from the same source. Just explore around.

## Part 3: Twitter Application Development 

We will develop applications that are “data clients” for twitter data. Twitter supports many APIs, I have 
used Search API that is a part of the REST API.
1. We are NOT interested in sentiment analysis. We are interested in sheer number of tweets on a topic that is associated with “flu” or a related term that you uniquely determine that will be important influencer. You have to choose a good topic. Understand the Search API that we are using for can give you only limited number of tweets per day and also only a sampling of the all the tweets. You will collect at least 20000 tweets (How could we categorize them?). Group them by geo-location as in Google maps API (one more API) and plot them on the map of USA. Map the geolocations to states, and color the states according to the number of tweets or mentions per state. Also understand that geo-locations are approximate. Person tweeting may not be in the state at which the actual incident of the flu is happening. We don’t have to worry about this. We assume the location of the tweet and the occurrence of the tweet are in the same place.

###  Steps:

#### Input: 

1. Search word or hash tag related to flu. Data client processing: Obtain and group tweets by location into categories mentioned in “fluview”. Output: plot them on the states/ color the states by the strength.
2. Issue 1: Of course, there is an issue with location meta-data. This is not available (N/A) if the user does hide his/her location. Only 1% of the tweets have geolocations. Then how can we get “set of locations”?
3. Here is a verified approach using function of twitteR
  a) Convert search result tweets into dataframe
  b) Lookup screen names from this dataframe
  c) Convert these screen names into another dataframe
  d) Keep only users (user names) with location info
  e) Get the geocode of the locations from this dataframe
  f) Hints on TwitteR functions you may need: twListToDF, lookupUsers, geocode; Look up these
     functions in the twitteR manual.
4. Compare: Now compare your map and with map from CDC for the same date ranges. You can
   do that side by side in a Jupyter notebook by running the respective R commands.
5. Iterate: Try the comparison with different query words related to flu. Keep your words secret.
6. Run R server and make your analysis available for the world to see and interact. (Shiny App) 

## REFERENCES:
1. Jupyter.(http://jupyter.org/), last viewed 2019.
2. The R Language. https://cran.r-project.org/, last viewed 2019.
3. R-Studio. https://www.rstudio.com/, last viewed 2019.
4. Twitter API. Twitter Developer https://dev.twitter.com/, https://developer.twitter.com/en/docs last
viewed 2019.
5. TwitteR package. https://cran.r-project.org/web/packages/twitteR/twitteR.pdf, last viewed 2019.
6. D. Kahle and H. Wickham. ggmap: Spatial Visualization with ggplot2. The R Journal Vol. 5/1, June 2013, https://journal.r-project.org/archive/2013-1/kahle-wickham.pdf.
7. OAuth2.0. OAuth2.0: https://oauth.net/2/, last viewed 2019.
8. https://www.cdc.gov/flu/weekly,interactivedataanalysisofvariousfluparameters,lastviewed2019.
9. https://www.cdc.gov/flu/, CDC Weekly report on Flu Activity, last viewed 2019.
10. J. Gentry. TwitteR Vignette: A Twitter Client for R. http://geoffjentry.hexdump.org/twitteR.pdf, last
viewed 2017.
11. Fluviewdatadownloadhttps://gis.cdc.gov/grasp/fluview/fluportaldashboard.html.Lastviewed2019.
12. Twittergeolocations.https://developer.twitter.com/en/docs/geo/places-near-location/api-reference/get-
geo-search.html, last viewed 2019.


## Credits

This project contains scripts and other related material that is developed at University of Buffalo, The State University of New York.


## Acknowledgement

I would like to thank my Professor, [Bina Ramamurthy](http://www.buffalo.edu/news/experts/bina-ramamurthy-faculty-expert-blockchain.html), for the  guidance, encouragement and advice she has provided. I am also thankful to all the TA's who have clarified all my queries.
