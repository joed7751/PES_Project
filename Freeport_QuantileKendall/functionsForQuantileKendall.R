# Functions for QuantileKendall script
# Author: Bob Hirsch
#
makeSortQ <- function(eList){
# creates a matrix called Qsort
# Qsort[dimDays,dimYears]
# no missing values, all values discharge values 
# sorted from smallest to largest over dimDays (if working with full year dimDays=365)
#   also creates other vectors that contain information about this array
#   
	localINFO <- getInfo(eList)
	localDaily <- getDaily(eList)
	paStart <- localINFO$paStart
	paLong <- localINFO$paLong
# determine the maximum number of days to put in the array
	numDays <- length(localDaily$DecYear)
	monthSeqFirst <- localDaily$MonthSeq[1]
	monthSeqLast <- localDaily$MonthSeq[numDays]
# creating a data frame (called startEndSeq) of the MonthSeq values that go into each year
	Starts <- seq(paStart, monthSeqLast, 12)
	Ends <- Starts + paLong - 1
	startEndSeq <- data.frame(Starts, Ends)
# trim this list of Starts and Ends to fit the period of record
	startEndSeq <- subset(startEndSeq, Ends >= monthSeqFirst & Starts <= monthSeqLast)
#	print(startEndSeq)

	numYearsRaw <- length(startEndSeq$Ends)
# set up some vectors to keep track of years
	good <- rep(0, numYearsRaw)
	numDays <- rep(0, numYearsRaw)
	midDecYear <- rep(0, numYearsRaw)
	Qraw <- matrix(nrow = 366, ncol = numYearsRaw)
for(i in 1: numYearsRaw) {
	startSeq <- startEndSeq$Starts[i]
	endSeq <- startEndSeq$Ends[i]
	startJulian <- getFirstJulian(startSeq)
# startJulian is the first julian day of the first month in the year being processed
# endJulian is the first julian day of the month right after the last month in the year being processed
	endJulian <- getFirstJulian(endSeq + 1)
	fullDuration <- endJulian - startJulian
#	cat("\n", i, startJulian, endJulian, fullDuration)
	yearDaily <- localDaily[localDaily$MonthSeq >= startSeq & (localDaily$MonthSeq <= endSeq), ]
	nDays <- length(yearDaily$Q)
	if(nDays == fullDuration) {
		good[i] <- 1
		numDays[i] <- nDays
		midDecYear[i] <- (yearDaily$DecYear[1] + yearDaily$DecYear[nDays]) / 2
		Qraw[1:nDays,i] <- yearDaily$Q
		}
		else {numDays[i] <- NA
			midDecYear[i] <- NA
			}
			}
# now we compress the matrix down to equal number of values in each column
	j <- 0
	numGoodYears <- sum(good)
#	print(good)
#	print(numGoodYears)
	dayCounts <- ifelse(good==1, numDays, NA)
	print(dayCounts)
	lowDays <- min(dayCounts, na.rm = TRUE)
	highDays <- max(dayCounts, na.rm = TRUE)
#	cat("\nminimum and maximum number of days in year", lowDays, highDays)
	dimYears <- numGoodYears
	dimDays <- lowDays
	sortQ <- matrix(nrow = dimDays, ncol = dimYears)
	time <- rep(0,dimYears)
	for (i in 1:numYearsRaw){
		if(good[i]==1) {
			j <- j + 1
			numD <- numDays[i]
#			cat("\n good year i, j, numD",i,j,numD)
			x <- sort(Qraw[1:numD, i])
			# separate odd numbers from even numbers of days
			if(numD == lowDays) {sortQ[1:dimDays,j] <- x} else {
		sortQ[1:dimDays,j] <- if(odd(numD)) leapOdd(x) else leapEven(x)
		}
		time[j] <- midDecYear[i]
#		cat("\n time in good year", time[j])
		}
		else {j <- j}
			}
	sortQList = list(sortQ,time)
	return(sortQList)			
			}
#
trendSortQ <- function(Qsort, time){
# note requires packages zyp, rkt, and Kendall
	nFreq <- dim(Qsort)[1]
#	cat("\n nFreq", nFreq)
	nYears <- length(time)
#	cat("\n nYears",nYears)
	results <- as.data.frame(matrix(ncol=9,nrow=nFreq))
	colnames(results) <- c("slopeLog","slopePct","pValue","pValueAdj","tau","rho1","rho2","freq","z")
	for(iRank in 1:nFreq){
		mkOut <- rkt(time,log(Qsort[iRank,]))
		results$slopeLog[iRank] <- mkOut$B
		results$slopePct[iRank] <- 100 * (exp(mkOut$B) - 1)
		results$pValue[iRank] <- mkOut$sl
		outZYP <- zyp.zhang(log(Qsort[iRank,]),time)
		results$pValueAdj[iRank] <- outZYP[6]
		results$tau[iRank] <- mkOut$tau
# I don't actually use this information in the current outputs, but the code is there 
# if one wanted to look at the serial correlation structure of the flow series 		
		serial <- acf(log(Qsort[iRank,]), lag.max = 2, plot = FALSE)
		results$rho1[iRank] <- serial$acf[2]
		results$rho2[iRank] <- serial$acf[3]
		frequency <- iRank / (nFreq + 1)
		results$freq[iRank] <- frequency
		results$z[iRank] <- qnorm(frequency)	
	}
	return(results)
}
#
#
getFirstJulian <- function(monthSeq){
	year <- 1850 + trunc((monthSeq - 1) / 12)
	month <- monthSeq - 12 * (trunc((monthSeq-1)/12))
#	cat("\nyear, month",year, month)	
	charMonth <- ifelse(month<10,paste("0",as.character(month),sep=""),as.character(month))
	theDate <- paste(year,"-",charMonth,"-01",sep="")
	Julian1 <- as.numeric(julian(as.Date(theDate),origin=as.Date("1850-01-01")))
	return(Julian1)
}
#
#
leapOdd <- function(x){
	n <- length(x)
	m <- n - 1
	mid <- (n + 1) / 2
	mid1 <- mid + 1
	midMinus <- mid - 1
	y <- rep(NA, m)
	y[1:midMinus] <- x[1:midMinus]
	y[mid:m] <- x[mid1:n]
	return(y)}
#
#
leapEven <- function(x){
	n <- length(x)
	m <- n - 1
	mid <- n / 2
	y <- rep(NA, m)
	mid1 <- mid + 1
	mid2 <- mid + 2
	midMinus <- mid - 1
	y[1:midMinus] <- x[1:midMinus]
	y[mid] <- (x[mid] + x[mid1]) / 2
	y[mid1:m] <- x[mid2 : n]
	return(y)
}
#
#
odd <- function(x) {if ((x %% 2) == 0) FALSE else TRUE}
#
#
calcWY <- function (df) 
{
    df$WaterYear <- as.integer(df$DecYear)
    df$WaterYear[df$Month >= 10] <- df$WaterYear[df$Month >= 
        10] + 1
    return(df)
}
#
#
calcCY <- function (df)
# computes climate year and adds it to the Daily data frame
{
  df$ClimateYear <- as.integer(df$DecYear)
  df$ClimateYear[df$Month >= 4] <- df$ClimateYear[df$Month >= 
                                                 4] + 1
  return(df)
}
#
#
#
plotFlowSingleKendall <- function (eList, istat, yearStart = NA, yearEnd = NA, qMax = NA, 
                                   printTitle = TRUE, tinyPlot = FALSE, customPar = FALSE, runoff = FALSE, 
                                   qUnit = 1, printStaName = TRUE, printPA = TRUE, printIstat = TRUE, 
                                   cex = 0.8, cex.axis = 1.1, cex.main = 1.1, lwd = 2, col = "black", 
                                   ...) 
{
  localAnnualSeries <- makeAnnualSeries(eList)
  localINFO <- getInfo(eList)
  qActual <- localAnnualSeries[2, istat, ]
  qSmooth <- localAnnualSeries[3, istat, ]
  years <- localAnnualSeries[1, istat, ]
  Q <- qActual
  time <- years
  LogQ <- log(Q)
  mktFrame <- data.frame(time,LogQ)
  mktFrame <- na.omit(mktFrame)
  mktOut <- rkt(mktFrame$time,mktFrame$LogQ)
  slope <- mktOut$B
  slopePct <- 100 * (exp(slope)) - 100
  slopePct <- format(slopePct,digits=2)
  pValue <- mktOut$sl
  pValue <- format(pValue,digits = 3)
  ### MK test goes here###
  if (is.numeric(qUnit)) {
    qUnit <- qConst[shortCode = qUnit][[1]]
  }
  else if (is.character(qUnit)) {
    qUnit <- qConst[qUnit][[1]]
  }
  if (sum(c("paStart", "paLong", "window") %in% names(localINFO)) == 
      3) {
    paLong <- localINFO$paLong
    paStart <- localINFO$paStart
    window <- localINFO$window
  }
  else {
    paLong <- 12
    paStart <- 10
    window <- 20
  }
  qFactor <- qUnit@qUnitFactor
  qActual <- if (runoff) 
    qActual * 86.4/localINFO$drainSqKm
  else qActual * qFactor
  qSmooth <- if (runoff) 
    qSmooth * 86.4/localINFO$drainSqKm
  else qSmooth * qFactor
  localSeries <- data.frame(years, qActual, qSmooth)
  localSeries <- if (is.na(yearStart)) 
    localSeries
  else subset(localSeries, years >= yearStart)
  localSeries <- if (is.na(yearEnd)) 
    localSeries
  else subset(localSeries, years <= yearEnd)
  yInfo <- generalAxis(x = qActual, maxVal = qMax, minVal = 0, 
                       tinyPlot = tinyPlot)
  xInfo <- generalAxis(x = localSeries$years, maxVal = yearEnd, 
                       minVal = yearStart, padPercent = 0, tinyPlot = tinyPlot)
  line1 <- if (printStaName) 
    localINFO$shortName
  else ""
  #  line2 <- if (printPA) 
  #    paste("\n", setSeasonLabelByUser(paStartInput = paStart, 
  #                                    paLongInput = paLong))
  #  else ""
  nameIstat <- c("minimum day", "7-day minimum", "30-day minimum", 
                 "median daily", "mean daily", "30-day maximum", "7-day maximum", 
                 "maximum day")
  line2 <- if (printIstat) 
    paste("\n", nameIstat[istat])
  else ""
  line3 <- if(printPA)
    paste("\nSlope estimate is ",slopePct,"% per year, Mann-Kendall p-value is ",pValue,sep="")
  else ""
  title <- if (printTitle) 
    paste(line1, line2, line3)
  else ""
  if (tinyPlot) {
    yLab <- qUnit@qUnitTiny
    title <- if (printTitle) 
      paste(nameIstat[istat])
    else ""
  }
  else {
    yLab <- qUnit@qUnitExpress
  }
  yLab <- if (runoff) 
    "Runoff in mm/day"
  else yLab
  genericEGRETDotPlot(x = localSeries$years, y = localSeries$qActual, 
                      xlim = c(xInfo$bottom, xInfo$top), ylim = c(yInfo$bottom, 
                                                                  yInfo$top), xlab = "", ylab = yLab, customPar = customPar, 
                      xTicks = xInfo$ticks, yTicks = yInfo$ticks, cex = cex, 
                      plotTitle = title, cex.axis = cex.axis, cex.main = cex.main, 
                      tinyPlot = tinyPlot, lwd = lwd, col = col, ...)
  lines(localSeries$years, localSeries$qSmooth, lwd = lwd, 
        col = col)
}
