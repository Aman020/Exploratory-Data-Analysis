

#---------------------------------------------------TASK 4------------------------------------------------------

influenza_national_summary <- function( filedata) {
  library(plotly)
  national_summary <- filedata
  week<- sprintf("%d %02d",national_summary$YEAR,national_summary$WEEK)
  week<- gsub(" ", "", week, fixed = TRUE)
  totalA <- national_summary$TOTAL.A
  totalB <- national_summary$TOTAL.B
  percentA <- national_summary$PERCENT.A
  percentB <- national_summary$PERCENT.B
  percentPositive <- national_summary$PERCENT.POSITIVE
  
    second_yaxis <- list(
    tickfont = list(color = "black"),
    overlaying = "y",
    side = "right",
    title = "Percent Positive"
  )
  
  t <- list(
    size = 8)
  

  data <- data.frame(week,totalA, totalB, percentA,percentB,percentPositive)
  
  p <- plot_ly(data) %>%
    add_trace(x=~week, y = ~totalA, type="bar", name = "Total A", color = I("yellow")) %>%
    add_trace(x=~week, y=~totalB, type = 'bar', name = 'Total B', color = I("dark green")) %>%
    add_trace(x=~week,y = ~percentA, type="scatter", mode="lines", name="Percentage A", color = I("orange"), yaxis='y2',line=list(dash="dash")) %>%
    add_trace(x=~week,y = ~percentB, type="scatter", mode="lines",name="Percentage B", color = I("green"), yaxis='y2', line=list(dash="dash")) %>%
    add_trace(x=~week,y = ~percentPositive, type="scatter", mode="lines", name="Total Percentage", color = I("black"), yaxis='y2') %>%
    layout( title = 'Influenza Positive Tests Reported to CDC by US-Cln-Lab, National Summary 2017-2018',yaxis = list(title = 'Total Positive Specimens '),  yaxis2=second_yaxis, barmode = "stack",font=t)
  p
}

influenza_positive_tested <-function(publicHealthfiledata, isNY = FALSE){
  if (isNY)
  {
    library(plotly)
    positive_test <- publicHealthfiledata
    noOfPositiveSpecimen <- positive_test$TOTAL.SPECIMENS
    aH1N1<-positive_test$A..2009.H1N1.
    aH3<-positive_test$A..H3.
    aSubtypyingNotPerformed<-positive_test$A..Subtyping.not.Performed.
    b<-positive_test$B
    bvc<-positive_test$BVic
    byam<-positive_test$BYam
    h3n2v<-positive_test$H3N2v
    season <-positive_test$SEASON_DESCRIPTION
    data<- data.frame(aSubtypyingNotPerformed,aH1N1,aH3,h3n2v,b,bvc,byam)
    pNY <- plot_ly(data)%>%
      add_trace(x =~season, y=~aSubtypyingNotPerformed,type="bar",name="A(subtyping not performed)", color =I("yellow"))%>% 
      add_trace(x =~season,y=~aH1N1,type="bar",name="A(H1N1)pdm09", color =I("orange"))%>%
      add_trace(x =~season,y=~aH3,type="bar",name="A(H3N2)", color =I("red"))%>%
      add_trace(x =~season,y=~h3n2v,type="bar",name="H3N2V", color =I("purple"))%>%
      add_trace(x =~season,y=~b,type="bar",name="B(lineage not performend)", color =I("blue"))%>%
      add_trace(x =~season,y=~bvc,type="bar",name="B(Victoria Lineage)", color =I("green"))%>%
      add_trace(x =~season,y=~byam,type="bar",name="B(Yamagata Lineage)", color =I("dark green"))%>%
      layout( title = 'Influenza Positive Tests Reported to CDC by US-Public Health Lab, National Summary 2017-2018-NY',xaxis= list(title = 'Season'),yaxis = list(title = 'Number of Positive Specimens '), barmode = "stack",font=t)
    pNY
  }
  else
  {
  t <- list(size =7)
  library(plotly)
  positive_test <- publicHealthfiledata
  week<- sprintf("%d %02d",positive_test$YEAR,positive_test$WEEK)
  week<- gsub(" ", "", week, fixed = TRUE)
  noOfPositiveSpecimen <- positive_test$TOTAL.SPECIMENS
  aH1N1<-positive_test$A..2009.H1N1.
  aH3<-positive_test$A..H3.
  aSubtypyingNotPerformed<-positive_test$A..Subtyping.not.Performed.
  b<-positive_test$B
  bvc<-positive_test$BVic
  byam<-positive_test$BYam
  h3n2v<-positive_test$H3N2v
  
  data<- data.frame(aSubtypyingNotPerformed,aH1N1,aH3,h3n2v,b,bvc,byam)
  p2 <- plot_ly(data)%>%
    add_trace(x=~week,y=~aSubtypyingNotPerformed,type="bar",name="A(subtyping not performed)", color =I("yellow"))%>% 
    add_trace(x=~week,y=~aH1N1,type="bar",name="A(H1N1)pdm09", color =I("orange"))%>%
    add_trace(x=~week,y=~aH3,type="bar",name="A(H3N2)", color =I("red"))%>%
    add_trace(x=~week,y=~h3n2v,type="bar",name="H3N2V", color =I("purple"))%>%
    add_trace(x=~week,y=~b,type="bar",name="B(lineage not performend)", color =I("blue"))%>%
    add_trace(x=~week,y=~bvc,type="bar",name="B(Victoria Lineage)", color =I("green"))%>%
    add_trace(x=~week,y=~byam,type="bar",name="B(Yamagata Lineage)", color =I("dark green"))%>%
    layout( title = 'Influenza Positive Tests Reported to CDC by US-Public Health Lab, National Summary 2017-2018',xaxis= list(title = 'Weeks'),yaxis = list(title = 'Number of Positive Specimens '), barmode = "stack",font=t)
 p2
  }
}

#-1) Influenza national summary (green and yellow chart)
influenza_national_summary(read.csv(file.choose(),skip =1))


#-2) Positive tested
influenza_positive_tested(read.csv(file.choose(),skip =1))

#-3) Mortality
library(plotly)
mortality_csv <- read.csv(file.choose())
percentPI <-  mortality_csv$PERCENT.P.I 
threshold <- mortality_csv$THRESHOLD
baseline <- mortality_csv$BASELINE
week <- sprintf("%02d",mortality_csv$WEEK)
displayWeek <- week
displayWeek
week <- paste(mortality_csv$SEASON, week)
f <- list( font = 4)

dfMortality <- data.frame(displayWeek,week, percentPI, threshold,baseline)

p <- plot_ly(dfMortality, x = ~week, y = ~percentPI, name = 'Percent Death due to Pneumonia and Influenza', type = 'scatter', mode = 'lines',
             line = list(color = 'red')) %>%
  add_trace(y = ~baseline, name = 'Seasonal Baseline', line = list(color = 'black')) %>%
  add_trace(y = ~threshold, name = 'Epidemic Threshold', line = list(color = 'black')) %>%
  layout(title = "Pneumonia and Influenza Mortality",
         xaxis = list(title = "MMWR Week",categoryorder = "array",categoryarray = week ), # https://stackoverflow.com/questions/40701491/plot-ly-in-r-unwanted-alphabetical-sorting-of-x-axis   - TO avoid unwanted sorting of a column using plotly 
         yaxis = list (title = "% of All Deaths due to P & I") ,font = f
         )
p



#-4) Pediatric Death 

library(plotly)
pdeath <- read.csv(file.choose(), skip =1)
week<- pdeath$WEEK.NUMBER
noOfDeath <- pdeath$NO..OF.DEATHS
previousDeath <- pdeath$PREVIOUS.WEEK.DEATHS
currentDeath<- pdeath$CURRENT.WEEK.DEATHS

data <- data.frame(week, noOfDeath, previousDeath, currentDeath)

p1<- plot_ly(data, x= ~week , y = ~previousDeath,name ='Previous Deaths', type = 'bar',color = I('dark green') )%>%
    add_trace( y =~currentDeath, color = I( 'pink') , name ="Current Deaths")%>%
  layout(title = "Number of Influenza-Associated Pediatric Deaths",
         xaxis = list(title = "Week" ), 
         yaxis = list (title = "Number of deaths"))
p1













#-------------------------------------------------TASK 5--------------------------------------------------------

#-1) Influenza national summary (green and yellow chart)
influenza_national_summary(read.csv(file.choose(),skip =1))


#-2) Positive tested
influenza_positive_tested(read.csv(file.choose(),skip =1))



#-------------------------------------------------TASK 6--------------------------------------------------------

#-NY State Influenza Positive Tested
influenza_positive_tested(read.csv(file.choose(),skip=1),TRUE)


#-NY State Summary
influenza_national_summary(read.csv(file.choose(),skip=1))
