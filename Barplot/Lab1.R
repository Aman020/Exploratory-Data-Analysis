#---------------------------------------------------------

test1 <- read.csv(file.choose(), skip = 1)
test1
#barplot(test1, col="blue", type="o", ylim=c(0,90000), xlab="Week", ylab="Number of Positive Specimens")
par(bg = "white")
xy <- paste(test1$YEAR, test1$WEEK)
ab <-
  barplot(test1$TOTAL.A,
          col = "yellow",
          ylab = "Total Specimens",
          ylim = c(0, max(test1$TOTAL.A)))
par(new = T)
bc <-
  barplot(
    test1$TOTAL.B,
    col = "green",
    ylab = "Total Specimens",
    axes = FALSE,
    ylim = c(0, max(test1$TOTAL.B))
  )
par(new = T)
linePlot <-
  plot(
    test1$PERCENT.POSITIVE,
    col = "black",
    type = "l",
    ylim = c(0, max(test1$PERCENT.POSITIVE)),
    xlab = "",
    ylab = "" ,
    axes = FALSE
  )
#lines(influenzaPositive$PERCENT.A, col="blue", lty="dashed", ylim = c(0, 100), type = "l")
lines(
  test1$PERCENT.A,
  col = "yellow",
  lty = "dashed",
  ylim = c(0, max(test1$PERCENT.A)),
  type = "l"
)
lines(
  test1$PERCENT.B,
  col = "purple",
  lty = "dashed",
  ylim = c(0, max(test1$PERCENT.B)),
  type = "l"
)
#axis(4, at=ab, ylim = test1$PERCENT.POSITIVE)
axis(1,
     at = ab,
     labels = xy,
     cex.axis = 0.5)
axis(4,
     ylim = c(0, 30),
     col = "black",
     las = 0)

#############################################################

##Bihani

influenza <- read.csv(file.choose(), skip = 1)
par(bg = "cornsilk")
complete_week <- paste(influenza$YEAR, influenza$WEEK)
complete_week <- sprintf("%d%2d", influenza$YEAR, influenza$WEEK)
total_specimens <- influenza$TOTAL.SPECIMENS

barplot(
  influenza$TOTAL.A ,
  xlab = "weeks" ,
  ylab = "total specimens",
  names.arg = complete_week ,
  col = "yellow"
)
par(new = T)
barplot(
  influenza$TOTAL.B ,
  xlab = "weeks" ,
  ylab = "total specimens" ,
  names.arg = complete_week ,
  col = "green"
)
axis(
  4,
  ylim = c(influenza$PERCENT.POSITIVE),
  col = "black",
  las = 0
)
par(new = T)

plot(
  influenza$PERCENT.POSITIVE,
  col = "black",
  type = "l" ,
  ylab = "",
  xlab = "",
  axes = FALSE
)
lines(influenza$PERCENT.A,
      col = "red",
      type = "l" ,
      lty = "dotted")
lines(influenza$PERCENT.B,
      col = "blue",
      type = "l",
      lty = "dashed")

#############################################################
test2 <- read.csv(file.choose(), skip = 1)
par(bg = "white")
week1 <- paste(test2$YEAR, test2$WEEK)
if (identical(week1, character(0))) {
  print("Test")
  week1 <- test2$SEASON_DESCRIPTION
  print(week1)
}
as <-
  barplot(
    as.matrix(test2$A..Subtyping.not.Performed.),
    beside = TRUE,
    col = "yellow",
    ylab = "Total Specimens",
    ylim = c(0, max(test2$TOTAL.SPECIMENS))
  )
par(new = T)
sd <-
  barplot(
    as.matrix(test2$A..2009.H1N1.),
    beside = TRUE,
    col = "orange",
    ylab = "Total Specimens",
    axes = FALSE,
    ylim = c(0, max(test2$TOTAL.SPECIMENS))
  )
par(new = T)
df <-
  barplot(
    as.matrix(test2$A..H3.),
    beside = TRUE,
    col = "red",
    ylab = "Total Specimens",
    axes = FALSE,
    ylim = c(0, max(test2$TOTAL.SPECIMENS))
  )
par(new = T)
fg <-
  barplot(
    as.matrix(test2$H3N2v),
    beside = TRUE,
    col = "purple",
    ylab = "Total Specimens",
    axes = FALSE,
    ylim = c(0, max(test2$TOTAL.SPECIMENS))
  )
par(new = T)
gh <-
  barplot(
    as.matrix(test2$B),
    beside = TRUE,
    col = "dark green",
    ylab = "Total Specimens",
    axes = FALSE,
    ylim = c(0, max(test2$TOTAL.SPECIMENS))
  )
par(new = T)
hj <-
  barplot(
    as.matrix(test2$BVic),
    beside = TRUE,
    col = "light green",
    ylab = "Total Specimens",
    axes = FALSE,
    ylim = c(0, max(test2$TOTAL.SPECIMENS))
  )
par(new = T)
jk <-
  barplot(
    as.matrix(test2$BYam),
    beside = TRUE,
    col = "green",
    ylab = "Total Specimens",
    axes = FALSE,
    ylim = c(0, max(test2$TOTAL.SPECIMENS))
  )
par(new = T)
axis(
  1,
  at = jk,
  labels = week1,
  cex.axis = 0.5,
  las = 0
)

###################

influenzaPositive <- read.csv(file.choose(), skip = 1)
par(bg = "white")
barPlotVar <-
  barplot(
    height = influenzaPositive$TOTAL.A,
    ylab = "Number of Positive Specimens",
    xlab = "Weeks",
    col = "yellow",
    ylim = c(0, max(influenzaPositive$TOTAL.A))
  )
par(new = T)
barPlotVar2 <-
  barplot(
    height = influenzaPositive$TOTAL.B,
    ylab = "Number of Positive Specimens",
    xlab = "Weeks",
    col = "green",
    ylim = c(0, max(influenzaPositive$TOTAL.B)),
    axes = FALSE
  )
xaxisLabel = paste(influenzaPositive$YEAR, influenzaPositive$WEEK)
par(new = T)
linePlot <-
  plot(
    influenzaPositive$PERCENT.POSITIVE,
    col = "black",
    type = "l",
    ylim = c(0, max(influenzaPositive$PERCENT.POSITIVE)),
    xlab = "",
    ylab = "" ,
    axes = FALSE
  )
lines(
  influenzaPositive$PERCENT.A,
  col = "yellow",
  lty = "dashed",
  ylim = c(0, max(influenzaPositive$PERCENT.A)),
  type = "l"
)
lines(
  influenzaPositive$PERCENT.B,
  col = "dark green",
  lty = "dashed",
  ylim = c(0, max(influenzaPositive$PERCENT.B)),
  type = "l"
)
#lines(influenzaPositive$PERCENT.POSITIVE, type = "o", col = "blue")
axis(1, at = barPlotVar, labels = xaxisLabel)
axis(4,
     at = linePlot,
     ylab = "Percent Positive",
     ylim = c(0, max(influenzaPositive$PERCENT.POSITIVE)))