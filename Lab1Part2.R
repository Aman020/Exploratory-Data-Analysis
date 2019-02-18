

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
    add_trace(x=~week, y=~totalB, type = 'bar', name = 'Total B', color = I("green")) %>%
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

#----------------------------National Summary----------------------------------------------------------
influenza_national_summary(read.csv('/Users/aman/R/Lab1EDA/FluViewPhase2Data/WHO_NREVSS_Clinical_Labs.csv'))

#----------------------------NY State Summary-------------------------------------------------------------
influenza_national_summary(read.csv('/Users/aman/R/Lab1EDA/FluViewPhase2Data-NY/WHO_NREVSS_Clinical_Labs.csv',skip=1))

#--------------------------- Influenza Positive Tested-----------------------------------------------------
influenza_positive_tested(read.csv('/Users/aman/R/Lab1EDA/FluViewPhase2Data/WHO_NREVSS_Public_Health_Labs.csv'))

#--------------------------NY State Influenza Positive Tested----------------------------------------------
influenza_positive_tested(read.csv('/Users/aman/R/Lab1EDA/FluViewPhase2Data-NY/WHO_NREVSS_Public_Health_Labs.csv',skip=1),TRUE)
