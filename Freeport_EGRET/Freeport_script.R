#Load Libraries
library(EGRET)
library(dataRetrieval)
library(rloadest)
library(EGRETci)
library(foreach)
library(doParallel)
library(iterators)
library(zoo)
library(plotrix)
library(lubridate)
library(changepoint)
library(corrplot)
library(RColorBrewer)
library(plotly)
library(ggplot2)
library(viridis)
library(fields)
library(extrafont)
loadfonts()

# Sacramento River at Freeport
siteNumber<-  11447650

# By not setting a fixed start and end date, readNWIS will retrieve all available data
# Will adjust to desired dates below.
StartDate <- ""       
EndDate <- ""

# Get pure Q time series using the rloadest function (EGRET adds 0.1% of the period's mean discharge to 0 flow days
Q <- readNWISDaily("11447650",startDate=StartDate, endDate=EndDate,convert=FALSE)

# Look at flow record start and end dates
#range(Q$Date)
# [1] "1948-10-01" "2015-09-30"

#length(Q$Q[Q$Q==0])
# 35

# In write-up, make a comment about the number of 0-flow adjusted days, 
# or else include it in a table

# Now get Q data using EGRET function
siteNumber <- "11447650"  
QParameterCd <- "00060"
#Daily <- readNWISDaily(siteNumber, QParameterCd, StartDate, EndDate,convert=FALSE)

# There are 24471 data points, and 24471 days.
#


StartDate <- "1948-10-01"
EndDate <- "2019-04-30"
#Daily <- readNWISDaily(siteNumber, QParameterCd, StartDate, EndDate)
#write.csv(Daily,"FreeportDaily.csv")
# Metadata retrieval
INFO <- readNWISInfo(siteNumber = "11447650", parameterCd ="00060",interactive=FALSE)
INFO$staAbbrev <- paste(strsplit(INFO$station_nm," ")[[1]][1],strsplit(INFO$station_nm," ")[[1]][2])

##Bring in user Q, need to do this for Freeport because there are two different parameter codes for Q
#relevant data has already been compiled and is in the csv file shown below
fileName <- "FreeportDaily_cfs.csv"
filePath <- "/users/joed/Documents/Documents19/Biogeochemistry_2019/Freeport_EGRET/"
Daily <-readUserDaily(filePath, fileName, hasHeader = TRUE, separator = ",", qUnit = 1)

printFluxUnitCheatSheet()
# The following codes apply to the fluxUnit list:
# 1 =  poundsDay  ( pounds/day )
# 2 =  tonsDay  ( tons/day )
# 3 =  kgDay  ( kg/day )
# 4 =  thousandKgDay  ( thousands of kg/day )
# 5 =  tonsYear  ( tons/year )
# 6 =  thousandTonsYear  ( thousands of tons/year )
# 7 =  millionTonsYear  ( millions of tons/year )
# 8 =  thousandKgYear  ( thousands of kg/year )
# 9 =  millionKgYear  ( millions of kg/year )
# 10 =  billionKgYear  ( billions of kg/year )
# 11 =  thousandTonsDay  ( thousands of tons/day )
# 12 =  millionKgDay  ( millions of kg/day )

eList <- as.egret(INFO, Daily, NA, NA)

# store the annual series of discharge statistics
annualSeries <- makeAnnualSeries(eList)

# Uncomment the following for Joe
# -------------------------------
#setwd("/Users/joed/LTIMP_TA/LTIMP_TA2/EGRET/UT1/") # not entirely sure if this will work on a Mac or not?
#filePath <- '/Users/joed/LTIMP_TA/LTIMP_TA2/EGRET/UT1/'



# Set working directory so plots are written to appropriate location
subDir <- './Q'
if (file.exists(subDir)){
  setwd(file.path(getwd(),subDir))
} else {
  dir.create(file.path(getwd(),subDir))
  setwd(file.path(getwd(),subDir))
}

# Plotting the results for a single discharge statistic
# istat can range between 1-8; qUnit = 2: cfs
tiff("Annual_Seven_Day_Min_Flow_SacFreeport.tif", height = 600, width = 800, res=120)
plotFlowSingle(eList, istat = 2, qUnit = 2) #, showYLabels=FALSE)
#mtext(side=2, expression(paste("Flow, in  ",ft^3," ",s^-1,sep="")),line=3)
dev.off()

tiff("Annual_Median_Daily_SacFreeport.tif", height = 600, width = 800, res=120)
plotFlowSingle(eList, istat = 4, qUnit = 2) #, showYLabels=FALSE)
#mtext(side=2, expression(paste("Flow, in  ",ft^3," ",s^-1,sep="")),line=3)
#be sure to revisit this and add the data point for 1983/84 (somewhere in there)
dev.off()

tiff("Annual_Mean_Daily_SacFreeport.tif", height = 600, width = 800, res=120)
plotFlowSingle(eList, istat = 5, qUnit = 2) #, showYLabels=FALSE)
#mtext(side=2, expression(paste("Flow, in  ",ft^3," ",s^-1,sep="")),line=3)
#be sure to revisit this and add the data point for 1983/84 (somewhere in there)
dev.off()

tiff("Annual_Mean_Daily_SacFreeport.tif", height = 600, width = 800, res=120)
plotFlowSingle(eList, istat = 5, qUnit = 2) #, showYLabels=FALSE)
#mtext(side=2, expression(paste("Flow, in  ",ft^3," ",s^-1,sep="")),line=3)
#be sure to revisit this and add the data point for 1983/84 (somewhere in there)
dev.off()

tiff("Annual_30Day_Maximum_Q_SacFreeport.tif", height = 600, width = 800, res=120)
plotFlowSingle(eList, istat = 6, qUnit = 2) #, showYLabels=FALSE)
#mtext(side=2, expression(paste("Flow, in  ",ft^3," ",s^-1,sep="")),line=3)
#be sure to revisit this and add the data point for 1983/84 (somewhere in there)
dev.off()

tiff("Annual_7Day_Maximum_Q_SacFreeport.tif", height = 600, width = 800, res=120)
plotFlowSingle(eList, istat = 7, qUnit = 2) #, showYLabels=FALSE)
#mtext(side=2, expression(paste("Flow, in  ",ft^3," ",s^-1,sep="")),line=3)
#be sure to revisit this and add the data point for 1983/84 (somewhere in there)
dev.off()

tiff("Annual_Maximum_Q_SacFreeport.tif", height = 600, width = 800, res=120)
plotFlowSingle(eList, istat = 8, qUnit = 2) #, showYLabels=FALSE)
#mtext(side=2, expression(paste("Flow, in  ",ft^3," ",s^-1,sep="")),line=3)
#be sure to revisit this and add the data point for 1983/84 (somewhere in there)
dev.off()

#Plot changes in variability
tiff("SD_Q_window3_SacFreeport.tif", height = 600, width = 800, res=120)
plotSDLogQ(eList, qUnit = 2,window=3)
dev.off()

#plot daily flow
tiff("Period_Of_Record_Hydrograph_SacFreeport.tif", height = 600, width = 800, res=120)
plotQTimeDaily(eList, lwd = 1,qUnit = 2) #, showYLabels=FALSE)
#mtext(side=2, expression(paste("Flow, in  ",ft^3," ",s^-1,sep="")),line=3)
dev.off()

#plot daily flow
postscript("Period_Of_Record_Hydrograph_SacFreeport.ps",family="Courier", height=3.25, width=3.25)
par(mar=c(3,5,1,1))
plotQTimeDaily(eList,cex.axis =0.65,cex.main = 0.65, cex = 0.9,lwd = 0.25,col = "blue", qUnit = 2,cex.lab=0.75) #, showYLabels=FALSE)
#mtext(side=2, expression(paste("Flow, in  ",ft^3," ",s^-1,sep="")),line=3)
dev.off()


tiff("Period_Of_Record_Hydrograph_LogScale_SacFreeport.tif", height = 600, width = 800, res=120)
plotQTimeDaily(eList, lwd = 1,qUnit = 2,logScale=TRUE) #, showYLabels=FALSE)
#mtext(side=2, expression(paste("Flow, in  ",ft^3," ",s^-1,sep="")),line=3)
dev.off()

#plot several graphics at once
tiff("Plot4_window3_SacFreeport.tif", height = 600, width = 800, res=120)
plotFour(eList,qUnit = 2, window=3)
dev.off()


#############################################
# Look at trends at specific flow rates
#############################################
eListNext1 <- setPA(eList, paStart = 1, paLong = 2)
annualSeries <- makeAnnualSeries(eListNext1)
tiff("Jan-Feb_Flow_Analysis.tif", height = 600, width = 800, res=120)
plotFour(eListNext1, qUnit=1, window=3)
dev.off()

# Try March/April
eListNext2 <- setPA(eList, paStart = 3, paLong = 2)
annualSeries <- makeAnnualSeries(eListNext2)
tiff("Mar_Apr_Flow_Analysis.tif", height = 600, width = 800, res=120)
plotFour(eListNext2, qUnit=1,window=3)
dev.off()

eListNext3 <- setPA(eList, paStart = 5, paLong = 2)
annualSeries <- makeAnnualSeries(eListNext3)
tiff("May-Jun_Flow_Analysis.tif", height = 600, width = 800, res=120)
plotFour(eListNext3, qUnit=1,window=3)
dev.off()

eListNext4 <- setPA(eList, paStart = 1, paLong = 4)
annualSeries <- makeAnnualSeries(eListNext4)
tiff("Jan-Apr_Flow_Analysis.tif", height = 600, width = 800, res=120)
plotFour(eListNext4, qUnit=1,window=3)
dev.off()

eListNext5 <- setPA(eList, paStart = 4, paLong = 2)
annualSeries <- makeAnnualSeries(eListNext5)
tiff("Apr-May_Flow_Analysis.tif", height = 600, width = 800, res=120)
plotFour(eListNext5, qUnit=1,window=3)
dev.off()

#Try summer months
eListNext6 <- setPA(eList, paStart = 4, paLong = 5)
annualSeries <- makeAnnualSeries(eListNext6)
tiff("Apr-Sep_Flow_Analysis.tif", height = 600, width = 800, res=120)
plotFour(eListNext6, qUnit=1,window=3)
dev.off()

#Try fall, when flows may show a decrease with time as ag returns would be diminished under Ag
# dry up.
eListNext7 <- setPA(eList, paStart = 9, paLong = 3)
annualSeries <- makeAnnualSeries(eListNext7)
tiff("Sep-Nov_Flow_Analysis.tif", height = 600, width = 800, res=120)
plotFour(eListNext7, qUnit=1,window=3)
dev.off()

# Note that this approach of using alternatively named 
# eList's leaves the original intact so that we can use
# it below without fear of altering it from it's paStart=10
# and paLong = 12 values.  However, we need to set this up:

eList <- setPA(eList, paStart = 10, paLong = 12)
annualSeries <- makeAnnualSeries(eList)



#set file path to pull nitrate data from excel
#

########Start the analysis for Nitrate
#############
startDate    <- "1974-10-01"
endDate      <- "2019-04-30"
siteNumber<-  11447650
QParameterCd <- "00060"
parameterCd  <- "00631"  # "NO3"
##For the Freeport site, the truncated Q csv file is used to correspond to the available data
#filePath <- "/users/joed/Documents/Documents19/Biogeochemistry_2019/Freeport_EGRET/"
filePath <- "C:/Users/dsaleh/Documents/GitHub/PES_Project/Freeport_EGRET/"
fileName <- "FreeportDaily_truncated_cfs.csv"
Daily <-readUserDaily(filePath, fileName, hasHeader = TRUE, separator = ",", qUnit = 1)
#Sample <- readNWISSample(siteNumber, parameterCd, startDate, endDate)
#write.csv(Sample,"Sample_NO3_NWIS.csv")
####NItrate data had to be merged from NWIS and from Kratzer report, available is in the no3.csv file below
fileName <- "FreeportNO3.csv"
Sample <- readUserSample(filePath,fileName)
Sample <- removeDuplicates(Sample)
write.csv(Sample,'Sample_NO3.csv')
INFO <- readNWISInfo(siteNumber = siteNumber, parameterCd = parameterCd, interactive=FALSE)
INFO$staAbbrev <- paste(strsplit(INFO$station_nm," ")[[1]][1],strsplit(INFO$station_nm," ")[[1]][2])

# Have a look at the available range of NO3 data
range(Sample$Date)
# "1974-10-09" "2019-04-03"
#redo the Daily to match the available nitrate

eList <- mergeReport(INFO, Daily, Sample)

#Change the working directory; redirect plot output to NO3 folder, this keeps all of the analysis for each type of nutrient separate
setwd("C:/Users/dsaleh/Documents/GitHub/PES_Project/Freeport_EGRET")
subDir <- 'NO3/EGRET_plots'
if (file.exists(subDir)){
  setwd(file.path(getwd(),subDir))
} else {
  dir.create(file.path(getwd(),subDir), recursive = TRUE)
  setwd(file.path(getwd(),subDir))
}

# Plot water quality data
tiff("Conc_vs_Time_Sacramento_Freeport.tif", height = 600, width = 800, res=120)
plotConcTime(eList)
dev.off()

plotConcTime(eList)

# Now, a classic Q-C plot
tiff("Conc-Q_Inorg_N_Sacramento_Freeport.tif", height = 600, width = 800, res=120)
plotConcQ(eList, logScale=TRUE)
dev.off()

# The data set as flux values rather than as concentrations
tiff("Flux-Q_Inorg_N_Sacramento_Freeport.tif", height = 600, width = 800, res=120)
plotFluxQ(eList, fluxUnit=4)
dev.off()

# Monthly boxplots
tiff("Monthly-Conc_BoxPlots_Inorg_N_Sacramento_Freeport.tif", height = 600, width = 800, res=120)
boxConcMonth(eList, logScale=TRUE)
dev.off()

# Flow on days sampled vs. all other days
tiff("Flow_on_days_sampled_vs_all_other_days_Inorg_N_Sacramento_Freeport.tif", height = 600, width = 800, res=120)
boxQTwice(eList, qUnit=2)
dev.off()

#########################################
# Now start the Flow-Normalized Analysis
#########################################

# Build the regression model
eList <- modelEstimation(eList, windowY = 7, windowQ = 2, windowS = 0.5, minNumObs = 100, minNumUncen =50)
eList_NO3 <- eList



MonthlyResults <- calculateMonthlyResults(eList_NO3)
write.csv(MonthlyResults,'MonthlyResults_NO3.csv')
# Dump NO3-related flow-normalized data to text file for bringing together with other monitoring sites
#LocalDaily puts all of the modeling results into a single data frame, called localDaily
fluxunit=3
paLong <- 12
paStart <- 10
localDaily <- getDaily(eList_NO3)
write.csv(localDaily,'localDailyNO3.csv')
localAnnualResults <- setupYears(paStart = paStart, paLong = paLong, localDaily = localDaily)
write.table(localAnnualResults, file = 'SacFreeport_NO3_RawVals.txt', quote=FALSE, row.names=FALSE)
write.table(localAnnualResults, file = 'SacFreeport_NO3_RawVals.csv', quote=FALSE, row.names=FALSE)
# Plot the annual average concentration and annual flow-normalized concentration
tiff("Ann_Avg_Conc_&_Ann_Flow_Normalized_Conc_InorganicN_Sacramento_Freeport.tif", height = 600, width = 800, res=120)
plotConcHist(eList_NO3, plotFlowNorm=TRUE)
dev.off()

# Plot the annual average concentration and annual flow-normalized concentration
pdf("Ann_Avg_Conc_&_Ann_Flow_Normalized_Conc_InorganicN_Sacramento_Freeport.pdf")
plotConcHist(eList_NO3, plotFlowNorm=TRUE,tinyPlot = TRUE)
dev.off()

# Plot the annual flux and annual flow-normalized flux
tiff("Ann_Flux_&_Ann_Flow_Normalized_Flux_InorganicN_Sacramento_Freeport.tif" )
plotFluxHist(eList_NO3, plotFlowNorm = TRUE) 
dev.off()

# Look for a trend change:
tableChange(eList_NO3, fluxUnit=4, yearPoints=c(1974,1992,2000,2006,2015))



#Generate out-of-the-box diagnostic plots
tiff("fluxBiasMulti_Inorg_N_Sacramento_Freeport.tif", height = 1200, width = 1200, res=120)
fluxBiasMulti(eList_NO3, moreTitle = "WRTDS")
dev.off()

tiff("Modeled_Daily_Conc_wObservations_Inorg_N_Sacramento_Freeport.tif", height = 800, width = 1000, res=120)
plotConcTimeDaily(eList_NO3)
dev.off()




##############################

####The following code does some customized analyses
# Determine which flow rates to use for discharge-specific trends

# Baseflow: mean of the annual 30-day low flows
baseQ <- mean(aggregate(Q30 ~ waterYear, data = localDaily, min)[,2])
baseQ_txt <- format(baseQ, digits=2)
baseQ_txt_cfs <- format(baseQ * 35.315, digits=2)

# mid-range: median flow rate across all years
medQ <- median(localDaily$Q)
medQ_txt <- format(medQ, digits=2)
medQ_txt_cfs <- format(medQ * 35.315, digits=2)

# high flow: get the 25% quantile of each year's maximum Q7
# This will help ensure (but not guarantee) that every year is well represented in the high-end flows
highQ7 <- as.numeric(quantile(aggregate(Q7 ~ waterYear, data = localDaily, max)[,2])[2])
highQ7_txt <- format(highQ7, digits=2)
highQ7_txt_cfs <- format(highQ7 * 35.315, digits=2)

# The following bit of script generates a figure discussed by Joe in an email on 6/2/17
# -------------------------------------------------------------------------------------

tiff("Discharge_specific_trends_NO3_centered_on_06-01.tif", height = 600, width = 1200, res=120)
par(mar=c(4,6,4.1,8))
plotConcTimeSmooth(eList, q1 = baseQ, q2 = medQ, q3 = highQ7, centerDate='06-01', 
                   yearStart=localDaily$waterYear[1], yearEnd=localDaily$waterYear[nrow(localDaily)], 
                   logScale=FALSE, printLegend=FALSE)

# Determine y position of the legend
# ----------------------------------
y_l <- par('usr')[3]
y_u <- par('usr')[4]
y_m <- mean(y_l, y_u)

# Use the top version of the legend to add cfs

# legend('bottomleft', c(eval(substitute(expression(paste('Baseflow [',baseQ_txt,' ', m^3~s^-1,'(',baseQ_txt_cfs,' ',ft^3~s^-1,')]',sep=' ')), list(baseQ_txt=baseQ_txt, baseQ_txt_cfs=baseQ_txt_cfs))),
#                        eval(substitute(expression(paste('Median Flow [',medQ_txt,' ', m^3~s^-1,'(',medQ_txt_cfs,' ',ft^3~s^-1,')]',sep=' ')), list(medQ_txt=medQ_txt, medQ_txt_cfs=medQ_txt_cfs))),
#                        eval(substitute(expression(paste('High Flow [',highQ7_txt,' ', m^3~s^-1,'(',highQ7_txt_cfs,' ',ft^3~s^-1,')]',sep=' ')), list(highQ7_txt=highQ7_txt, highQ7_txt_cfs=highQ7_txt_cfs)))), 
#                        col=c('black','red','green'), lwd=2, bg='white', bty='n')

legend('bottomleft', c(eval(substitute(expression(paste('Baseflow (',baseQ_txt,' ', m^3~s^-1,')',sep=' ')), list(baseQ_txt=baseQ_txt))),
                       eval(substitute(expression(paste('Median Flow (',medQ_txt,' ', m^3~s^-1,')',sep=' ')), list(medQ_txt=medQ_txt))),
                       eval(substitute(expression(paste('High Flow (',highQ7_txt,' ', m^3~s^-1,')',sep=' ')), list(highQ7_txt=highQ7_txt)))), 
       col=c('black','red','green'), lwd=2, bg='white', bty='n')

dev.off()

# Restore original plotting margins
par(mar=c(5.1,6.1,4.1,2.1))

# The following bit of script generates a figure discussed by Michael in an email on 6/2/17
# -----------------------------------------------------------------------------------------
# Get the row number corresponding to the maximum concentration for each year (bearing in mind that these are 'within group' row numbers)
out <- aggregate(ConcDay ~ waterYear, data = localDaily, which.max)

# Ensure data is ordered by water year
out <- out[ order(out$waterYear), ]

# Get a count of the number of days within each of the water years
tbl <- table(localDaily$waterYear)

# Return the absolute row positions for each water year's max concentration
out$AbsConcDay <- out$ConcDay + cumsum(c(0,tbl[-length(tbl)]))

# Make a data.frame containing only the rows with each water year's max conc
out2 <- localDaily[out$AbsConcDay,]

# Gather only the needed data
out2 <- data.frame(Date=out2$Date, Q=out2$Q, Conc=out2$ConcDay, wyr=out2$waterYear, Julian=yday(as.Date(out2$Date)))

# Need to readjust the Julian day to start on Oct 1 (This function doesn't yet account for leap years)
out2$JulianWYR <- ifelse(out2$Julian > 273, out2$Julian - 273, 92 + out2$Julian)

# Plot it
tiff("JulianDay_of_Max_NO3_Conc.tif", height = 600, width = 800, res=120)
plot(out2$wyr, out2$JulianWYR, pch=16, xlab='Water Year', ylab='Julian Day', yaxs='i', ylim=c(0,370), las=1)
dev.off()

# In a follow-up email from Michael on 6/7/17, Michael suggested two alterations:
#  1) Apply a 30-day moving average
#  2) Use the flow-normalized concentration

# To start with, I'll attempt to apply a 30-day window to the simulated daily concentrations

localDaily$ConcDay_30day <- c(rep(rollapply(localDaily$ConcDay, width=30, mean)[1],times=14) , rollapply(localDaily$ConcDay, width=30, mean), rep(rollapply(localDaily$ConcDay, width=30, mean)[length(rollapply(localDaily$ConcDay, width=30, mean))], times=15))
out_m <- aggregate(ConcDay_30day ~ waterYear, data = localDaily, which.max)
out_m <- out_m[ order(out_m$waterYear), ]
out_m$AbsConcDay <- out_m$ConcDay + cumsum(c(0,tbl[-length(tbl)]))
out_m2 <- localDaily[out_m$AbsConcDay,]
out_m2 <- data.frame(Date=out_m2$Date, Q=out_m2$Q, Conc30=out_m2$ConcDay_30day, wyr=out_m2$waterYear, Julian=yday(as.Date(out_m2$Date)))
out_m2$JulianWYR <- ifelse(out_m2$Julian > 273, out_m2$Julian - 273, 92 + out_m2$Julian)

tiff("JulianDay_of_Max_NO3_Conc_Using_30_rollingAvg.tif", height = 600, width = 800, res=120)
plot(out_m2$wyr, out_m2$JulianWYR, pch=16, xlab='Water Year', ylab='Julian Day', yaxs='i', ylim=c(0,370), las=1)
dev.off()


# Next, I'll try using the flow-normalized concentration (same general code flow as above)
# First, try plotting flow-normalized concentration:
# Plot it
tiff("Flow_Normalized_Conc_BC1_NO3.tif", height = 600, width = 800, res=120)
plot(as.Date(localDaily$Date), localDaily$FNConc, typ='l', las=1, xlab='Time', ylab='Flow-normalized Concentration')
dev.off()

out_FN <- aggregate(FNConc ~ waterYear, data = localDaily, which.max)
out_FN <- out_FN[ order(out_FN$waterYear), ]
out_FN$AbsConcDay <- out_FN$FNConc + cumsum(c(0,tbl[-length(tbl)]))
out2_FN <- localDaily[out_FN$AbsConcDay,]
out2_FN <- data.frame(Date=out2_FN$Date, Q=out2_FN$Q, Conc=out2_FN$FNConc, wyr=out2_FN$waterYear, Julian=yday(as.Date(out2_FN$Date)))
out2_FN$JulianWYR <- ifelse(out2_FN$Julian > 273, out2_FN$Julian - 273, 92 + out2_FN$Julian)

# Plot it
tiff("JulianDay_of_Max_NO3_Flow_Normalized_Conc.tif", height = 600, width = 800, res=120)
plot(out2_FN$wyr, out2_FN$JulianWYR, pch=16, xlab='Water Year', ylab='Julian Day', yaxs='i', ylim=c(0,370), las=1)
dev.off()



# --------------------------------------------------------------------------------------------------------
# The following script is for a non-standard EGRET plot and instead help generate a plot Michael requested

localDaily <- getDaily(eList)

# Will need to adjust the date range below based on each gages unique start/stop dates
early_decade <- subset(localDaily, localDaily$Date > as.Date('1975-09-30') & localDaily$Date < as.Date('1985-10-01'))
recent_decade <- subset(localDaily, localDaily$Date > as.Date('2009-05-25'))


early_decade_monthly_mn <- aggregate(ConcDay ~ MonthSeq, data = early_decade, 'mean')
recent_decade_monthly_mn <- aggregate(ConcDay ~ MonthSeq, data = recent_decade, 'mean')

# early_decade_monthly_mn$month <- format(seq(as.Date('1972-10-01'), as.Date('1982-09-30'), by='month'), '%b')
early_decade_monthly_mn$month <- rep(c(10:12,1:9), times=10)
early_decade_mon_mn <- aggregate(ConcDay ~ month, data = early_decade_monthly_mn, 'mean')
early_decade_mon_sd <- aggregate(ConcDay ~ month, data = early_decade_monthly_mn, 'sd')
early_decade_mon_mn <- early_decade_mon_mn[c(10:12,1:9),]
early_decade_mon_sd <- early_decade_mon_sd[c(10:12,1:9),]

recent_decade_monthly_mn$month <- rep(c(10:12,1:9), times=10)
recent_decade_mon_mn <- aggregate(ConcDay ~ month, data = recent_decade_monthly_mn, 'mean')
recent_decade_mon_sd <- aggregate(ConcDay ~ month, data = recent_decade_monthly_mn, 'sd')
recent_decade_mon_mn <- recent_decade_mon_mn[c(10:12,1:9),]
recent_decade_mon_sd <- recent_decade_mon_sd[c(10:12,1:9),]


mdat2 <- matrix(c(early_decade_mon_mn$ConcDay, recent_decade_mon_mn$ConcDay),
                nrow=2,ncol = 12, byrow=TRUE,
                dimnames = list(c("1975-1985", "2009-2019"),
                                c(format(seq(as.Date('1973-10-01'), as.Date('1974-09-01'), by='month'), '%b'))))

# Be sure to adjust the legend's first decade start and stop year correctly
mx <- max(c((early_decade_mon_mn$ConcDay + early_decade_mon_sd$ConcDay), (recent_decade_mon_mn$ConcDay + recent_decade_mon_sd$ConcDay)))

tiff("timing_shift_in_NO3_conc_monthly_means.tif", height=800, width=900, res=130)
par(mar=c(3,5,2,1))
x <- barplot(mdat2, beside=TRUE, las=1, ylim=c(0,mx), col = c("lightblue", "mistyrose"))
abline(h=0)
arrows(x0=x[1,], y0=early_decade_mon_mn$ConcDay - early_decade_mon_sd$ConcDay, x1=x[1,], y1=early_decade_mon_mn$ConcDay + early_decade_mon_sd$ConcDay, angle=90, length=0.04, code=3)
arrows(x0=x[2,], y0=recent_decade_mon_mn$ConcDay - recent_decade_mon_sd$ConcDay, x1=x[2,], y1=recent_decade_mon_mn$ConcDay + recent_decade_mon_sd$ConcDay, angle=90, length=0.04, code=3)
mtext(side=2, expression(paste(NO[3],', mg ',L^-1,sep='')), line=3)
legend(x=25, y=0.9 * mx, c("1975-1985", "2009-2019"), pch=c(22,22), pt.cex=2, pt.bg=c("lightblue", "mistyrose"), bty='n', xpd=TRUE)
dev.off()

pdf("timing_shift_in_NO3_conc_monthly_means.pdf")
x <- barplot(mdat2, beside=TRUE, las=1, ylim=c(0,mx), col = c("lightblue", "mistyrose"))
abline(h=0)
arrows(x0=x[1,], y0=early_decade_mon_mn$ConcDay - early_decade_mon_sd$ConcDay, x1=x[1,], y1=early_decade_mon_mn$ConcDay + early_decade_mon_sd$ConcDay, angle=90, length=0.04, code=3)
arrows(x0=x[2,], y0=recent_decade_mon_mn$ConcDay - recent_decade_mon_sd$ConcDay, x1=x[2,], y1=recent_decade_mon_mn$ConcDay + recent_decade_mon_sd$ConcDay, angle=90, length=0.04, code=3)
mtext(side=2, expression(paste(NO[3],', mg ',L^-1,sep='')), line=3)
legend(x=25, y=0.9 * mx, c("1975-1985", "2009-2019"), pch=c(22,22), pt.cex=2, pt.bg=c("lightblue", "mistyrose"), bty='n', xpd=TRUE)
dev.off()

# Now attempting a Wilcox Test (aka Mann-Whitney-Wilcoxon Rank Sum test)
# ----------------------------------------------------------------------
early_jan <- subset(early_decade_monthly_mn, month==1)
recent_jan <- subset(recent_decade_monthly_mn, month==1)
Sacramento_Freeport_NO3_conc_jan_wilcox <- wilcox.test(recent_jan$ConcDay, early_jan$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_feb <- subset(early_decade_monthly_mn, month==2)
recent_feb <- subset(recent_decade_monthly_mn, month==2)
Sacramento_Freeport_NO3_conc_feb_wilcox <- wilcox.test(recent_feb$ConcDay, early_feb$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_mar <- subset(early_decade_monthly_mn, month==3)
recent_mar <- subset(recent_decade_monthly_mn, month==3)
Sacramento_Freeport_NO3_conc_mar_wilcox <- wilcox.test(recent_mar$ConcDay, early_mar$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_apr <- subset(early_decade_monthly_mn, month==4)
recent_apr <- subset(recent_decade_monthly_mn, month==4)
Sacramento_Freeport_NO3_conc_apr_wilcox <- wilcox.test(recent_apr$ConcDay, early_apr$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_may <- subset(early_decade_monthly_mn, month==5)
recent_may <- subset(recent_decade_monthly_mn, month==5)
Sacramento_Freeport_NO3_conc_may_wilcox <- wilcox.test(recent_may$ConcDay, early_may$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_jun <- subset(early_decade_monthly_mn, month==6)
recent_jun <- subset(recent_decade_monthly_mn, month==6)
Sacramento_Freeport_NO3_conc_jun_wilcox <- wilcox.test(recent_jun$ConcDay, early_jun$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_jul <- subset(early_decade_monthly_mn, month==7)
recent_jul <- subset(recent_decade_monthly_mn, month==7)
Sacramento_Freeport_NO3_conc_jul_wilcox <- wilcox.test(recent_jul$ConcDay, early_jul$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_aug <- subset(early_decade_monthly_mn, month==8)
recent_aug <- subset(recent_decade_monthly_mn, month==8)
Sacramento_Freeport_NO3_conc_aug_wilcox <- wilcox.test(recent_aug$ConcDay, early_aug$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_sep <- subset(early_decade_monthly_mn, month==9)
recent_sep <- subset(recent_decade_monthly_mn, month==9)
Sacramento_Freeport_NO3_conc_sep_wilcox <- wilcox.test(recent_sep$ConcDay, early_sep$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_oct <- subset(early_decade_monthly_mn, month==10)
recent_oct <- subset(recent_decade_monthly_mn, month==10)
Sacramento_Freeport_NO3_conc_oct_wilcox <- wilcox.test(recent_oct$ConcDay, early_oct$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_nov <- subset(early_decade_monthly_mn, month==11)
recent_nov <- subset(recent_decade_monthly_mn, month==11)
Sacramento_Freeport_NO3_conc_nov_wilcox <- wilcox.test(recent_nov$ConcDay, early_nov$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_dec <- subset(early_decade_monthly_mn, month==12)
recent_dec <- subset(recent_decade_monthly_mn, month==12)
Sacramento_Freeport_NO3_conc_dec_wilcox <- wilcox.test(recent_dec$ConcDay, early_dec$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

Conc_compare <- data.frame(chng_est=c(Sacramento_Freeport_NO3_conc_oct_wilcox$est,
                                      Sacramento_Freeport_NO3_conc_nov_wilcox$est,
                                      Sacramento_Freeport_NO3_conc_dec_wilcox$est,
                                      Sacramento_Freeport_NO3_conc_jan_wilcox$est,
                                      Sacramento_Freeport_NO3_conc_feb_wilcox$est,
                                      Sacramento_Freeport_NO3_conc_mar_wilcox$est,
                                      Sacramento_Freeport_NO3_conc_apr_wilcox$est,
                                      Sacramento_Freeport_NO3_conc_may_wilcox$est,
                                      Sacramento_Freeport_NO3_conc_jun_wilcox$est,
                                      Sacramento_Freeport_NO3_conc_jul_wilcox$est,
                                      Sacramento_Freeport_NO3_conc_aug_wilcox$est,
                                      Sacramento_Freeport_NO3_conc_sep_wilcox$est),
                           low_conf=c(Sacramento_Freeport_NO3_conc_oct_wilcox$conf.int[1],
                                      Sacramento_Freeport_NO3_conc_nov_wilcox$conf.int[1],
                                      Sacramento_Freeport_NO3_conc_dec_wilcox$conf.int[1],
                                      Sacramento_Freeport_NO3_conc_jan_wilcox$conf.int[1],
                                      Sacramento_Freeport_NO3_conc_feb_wilcox$conf.int[1],
                                      Sacramento_Freeport_NO3_conc_mar_wilcox$conf.int[1],
                                      Sacramento_Freeport_NO3_conc_apr_wilcox$conf.int[1],
                                      Sacramento_Freeport_NO3_conc_may_wilcox$conf.int[1],
                                      Sacramento_Freeport_NO3_conc_jun_wilcox$conf.int[1],
                                      Sacramento_Freeport_NO3_conc_jul_wilcox$conf.int[1],
                                      Sacramento_Freeport_NO3_conc_aug_wilcox$conf.int[1],
                                      Sacramento_Freeport_NO3_conc_sep_wilcox$conf.int[1]),
                           up_conf=c(Sacramento_Freeport_NO3_conc_oct_wilcox$conf.int[2],
                                     Sacramento_Freeport_NO3_conc_nov_wilcox$conf.int[2],
                                     Sacramento_Freeport_NO3_conc_dec_wilcox$conf.int[2],
                                     Sacramento_Freeport_NO3_conc_jan_wilcox$conf.int[2],
                                     Sacramento_Freeport_NO3_conc_feb_wilcox$conf.int[2],
                                     Sacramento_Freeport_NO3_conc_mar_wilcox$conf.int[2],
                                     Sacramento_Freeport_NO3_conc_apr_wilcox$conf.int[2],
                                     Sacramento_Freeport_NO3_conc_may_wilcox$conf.int[2],
                                     Sacramento_Freeport_NO3_conc_jun_wilcox$conf.int[2],
                                     Sacramento_Freeport_NO3_conc_jul_wilcox$conf.int[2],
                                     Sacramento_Freeport_NO3_conc_aug_wilcox$conf.int[2],
                                     Sacramento_Freeport_NO3_conc_sep_wilcox$conf.int[2]))

write.table(Conc_compare, "Sacramento_Freeport_NO3_conc_wilcox.txt", quote=FALSE, row.names=FALSE)

rng <- max(abs(c(Conc_compare$up_conf, Conc_compare$low_conf)))
tiff("Sacramento_Freeport_NO3_conc_shift_wilcox_Vert_Bars.tif", height=600, width=800, res=130)
par(mar=c(4,5,0.5,0.5))
plot(seq(1:12), Conc_compare$chng_est, typ='h', lend=1, lwd=15, col='white', xaxt='n', xlim=c(1,13), ylim=c(-rng, rng), xlab="Month", ylab=expression(paste("Median Concentration Change, mg  ",L^-1,sep='')), las=1)
plotCI(seq(1:12), Conc_compare$chng_est, ui=Conc_compare$up_conf, li=Conc_compare$low_conf, pch=16, add=TRUE)
abline(h=0)
axis(side=1,at=seq(1,12,by=1), labels=format(c(seq(as.Date("2000-10-01"), as.Date("2000-12-01"), by="month"), seq(as.Date("2000-01-01"), as.Date("2000-09-01"), by="month")),'%b'), las=2)
legend('topright', c("Median difference", "90% Confidence Interval for the Median"), pch=c(16,NA), lwd=c(NA,1), pt.cex=c(1,NA), pt.bg=c('black',NA), bty='n', bg='white')
dev.off()

pdf("Sacramento_Freeport_NO3_conc_shift_wilcox_Vert_Bars.pdf")
plot(seq(1:12), Conc_compare$chng_est, typ='h', lend=1, lwd=15, col='white', xaxt='n', xlim=c(1,13), ylim=c(-rng, rng), xlab="Month", ylab=expression(paste("Median Concentration Change, mg  ",L^-1,sep='')), las=1)
plotCI(seq(1:12), Conc_compare$chng_est, ui=Conc_compare$up_conf, li=Conc_compare$low_conf, pch=16, add=TRUE)
abline(h=0)
axis(side=1,at=seq(1,12,by=1), labels=format(c(seq(as.Date("2000-10-01"), as.Date("2000-12-01"), by="month"), seq(as.Date("2000-01-01"), as.Date("2000-09-01"), by="month")),'%b'), las=2)
legend('topright', c("Median difference", "90% Confidence Interval for the Median"), pch=c(16,NA), lwd=c(NA,1), pt.cex=c(1,NA), pt.bg=c('black',NA), bty='n', bg='white')
dev.off()


# Now do the load
# ---------------
early_decade_monthly_flx <- aggregate(FluxDay ~ MonthSeq, data = early_decade, 'sum')
recent_decade_monthly_flx <- aggregate(FluxDay ~ MonthSeq, data = recent_decade, 'sum')

# early_decade_monthly_mn$month <- format(seq(as.Date('1972-10-01'), as.Date('1982-09-30'), by='month'), '%b')
early_decade_monthly_flx$month <- rep(c(10:12,1:9), times=10)
early_decade_mon_mn_flx <- aggregate(FluxDay ~ month, data = early_decade_monthly_flx, 'mean')
early_decade_mon_sd_flx <- aggregate(FluxDay ~ month, data = early_decade_monthly_flx, 'sd')
early_decade_mon_mn_flx <- early_decade_mon_mn_flx[c(10:12,1:9),]
early_decade_mon_sd_flx <- early_decade_mon_sd_flx[c(10:12,1:9),]

recent_decade_monthly_flx$month <- rep(c(10:12,1:9), times=10)
recent_decade_mon_mn_flx <- aggregate(FluxDay ~ month, data = recent_decade_monthly_flx, 'mean')
recent_decade_mon_sd_flx <- aggregate(FluxDay ~ month, data = recent_decade_monthly_flx, 'sd')
recent_decade_mon_mn_flx <- recent_decade_mon_mn_flx[c(10:12,1:9),]
recent_decade_mon_sd_flx <- recent_decade_mon_sd_flx[c(10:12,1:9),]

mdat3 <- matrix(c(early_decade_mon_mn_flx$FluxDay, recent_decade_mon_mn_flx$FluxDay),
                nrow=2,ncol = 12, byrow=TRUE,
                dimnames = list(c("1975-1985", "2009-2019"),
                                c(format(seq(as.Date('1973-10-01'), as.Date('1974-09-01'), by='month'), '%b'))))

mx <- max(c((early_decade_mon_mn_flx$FluxDay + early_decade_mon_sd_flx$FluxDay), (recent_decade_mon_mn_flx$FluxDay + recent_decade_mon_sd_flx$FluxDay)))
tiff("timing_shift_in_NO3_load_monthly_means.tif", height=800, width=900, res=130)
x <- barplot(mdat3, beside=TRUE, las=1, ylim=c(0,mx), col = c("lightblue", "mistyrose"))
abline(h=0)
arrows(x0=x[1,], y0=early_decade_mon_mn_flx$FluxDay - early_decade_mon_sd_flx$FluxDay, x1=x[1,], y1=early_decade_mon_mn_flx$FluxDay + early_decade_mon_sd_flx$FluxDay, angle=90, length=0.04, code=3)
arrows(x0=x[2,], y0=recent_decade_mon_mn_flx$FluxDay - recent_decade_mon_sd_flx$FluxDay, x1=x[2,], y1=recent_decade_mon_mn_flx$FluxDay + recent_decade_mon_sd_flx$FluxDay, angle=90, length=0.04, code=3)
mtext(side=2, expression(paste(NO[3],', kg ',month^-1,sep='')), line=2.5)
legend(x=30, y=0.9 * mx, c("1975-1985", "2009-2019"), pch=c(22,22), pt.cex=2, pt.bg=c("lightblue", "mistyrose"), bty='n', xpd=TRUE)
dev.off()

# Apply Wilcox.text to the monthly loads here...
early_jan_flx <- subset(early_decade_monthly_flx, month==1)
recent_jan_flx <- subset(recent_decade_monthly_flx, month==1)
Sacramento_Freeport_NO3_flux_jan_wilcox <- wilcox.test(recent_jan_flx$FluxDay, early_jan_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_feb_flx <- subset(early_decade_monthly_flx, month==2)
recent_feb_flx <- subset(recent_decade_monthly_flx, month==2)
Sacramento_Freeport_NO3_flux_feb_wilcox <- wilcox.test(recent_feb_flx$FluxDay, early_feb_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_mar_flx <- subset(early_decade_monthly_flx, month==3)
recent_mar_flx <- subset(recent_decade_monthly_flx, month==3)
Sacramento_Freeport_NO3_flux_mar_wilcox <- wilcox.test(recent_mar_flx$FluxDay, early_mar_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_apr_flx <- subset(early_decade_monthly_flx, month==4)
recent_apr_flx <- subset(recent_decade_monthly_flx, month==4)
Sacramento_Freeport_NO3_flux_apr_wilcox <- wilcox.test(recent_apr_flx$FluxDay, early_apr_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_may_flx <- subset(early_decade_monthly_flx, month==5)
recent_may_flx <- subset(recent_decade_monthly_flx, month==5)
Sacramento_Freeport_NO3_flux_may_wilcox <- wilcox.test(recent_may_flx$FluxDay, early_may_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_jun_flx <- subset(early_decade_monthly_flx, month==6)
recent_jun_flx <- subset(recent_decade_monthly_flx, month==6)
Sacramento_Freeport_NO3_flux_jun_wilcox <- wilcox.test(recent_jun_flx$FluxDay, early_jun_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_jul_flx <- subset(early_decade_monthly_flx, month==7)
recent_jul_flx <- subset(recent_decade_monthly_flx, month==7)
Sacramento_Freeport_NO3_flux_jul_wilcox <- wilcox.test(recent_jul_flx$FluxDay, early_jul_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_aug_flx <- subset(early_decade_monthly_flx, month==8)
recent_aug_flx <- subset(recent_decade_monthly_flx, month==8)
Sacramento_Freeport_NO3_flux_aug_wilcox <- wilcox.test(recent_aug_flx$FluxDay, early_aug_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_sep_flx <- subset(early_decade_monthly_flx, month==9)
recent_sep_flx <- subset(recent_decade_monthly_flx, month==9)
Sacramento_Freeport_NO3_flux_sep_wilcox <- wilcox.test(recent_sep_flx$FluxDay, early_sep_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_oct_flx <- subset(early_decade_monthly_flx, month==10)
recent_oct_flx <- subset(recent_decade_monthly_flx, month==10)
Sacramento_Freeport_NO3_flux_oct_wilcox <- wilcox.test(recent_oct_flx$FluxDay, early_oct_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_nov_flx <- subset(early_decade_monthly_flx, month==11)
recent_nov_flx <- subset(recent_decade_monthly_flx, month==11)
Sacramento_Freeport_NO3_flux_nov_wilcox <- wilcox.test(recent_nov_flx$FluxDay, early_nov_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_dec_flx <- subset(early_decade_monthly_flx, month==12)
recent_dec_flx <- subset(recent_decade_monthly_flx, month==12)
Sacramento_Freeport_NO3_flux_dec_wilcox <- wilcox.test(recent_dec_flx$FluxDay, early_dec_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)


Flux_compare <- data.frame(chng_est=c(Sacramento_Freeport_NO3_flux_oct_wilcox$est,
                                      Sacramento_Freeport_NO3_flux_nov_wilcox$est,
                                      Sacramento_Freeport_NO3_flux_dec_wilcox$est,
                                      Sacramento_Freeport_NO3_flux_jan_wilcox$est,
                                      Sacramento_Freeport_NO3_flux_feb_wilcox$est,
                                      Sacramento_Freeport_NO3_flux_mar_wilcox$est,
                                      Sacramento_Freeport_NO3_flux_apr_wilcox$est,
                                      Sacramento_Freeport_NO3_flux_may_wilcox$est,
                                      Sacramento_Freeport_NO3_flux_jun_wilcox$est,
                                      Sacramento_Freeport_NO3_flux_jul_wilcox$est,
                                      Sacramento_Freeport_NO3_flux_aug_wilcox$est,
                                      Sacramento_Freeport_NO3_flux_sep_wilcox$est),
                           low_conf=c(Sacramento_Freeport_NO3_flux_oct_wilcox$conf.int[1],
                                      Sacramento_Freeport_NO3_flux_nov_wilcox$conf.int[1],
                                      Sacramento_Freeport_NO3_flux_dec_wilcox$conf.int[1],
                                      Sacramento_Freeport_NO3_flux_jan_wilcox$conf.int[1],
                                      Sacramento_Freeport_NO3_flux_feb_wilcox$conf.int[1],
                                      Sacramento_Freeport_NO3_flux_mar_wilcox$conf.int[1],
                                      Sacramento_Freeport_NO3_flux_apr_wilcox$conf.int[1],
                                      Sacramento_Freeport_NO3_flux_may_wilcox$conf.int[1],
                                      Sacramento_Freeport_NO3_flux_jun_wilcox$conf.int[1],
                                      Sacramento_Freeport_NO3_flux_jul_wilcox$conf.int[1],
                                      Sacramento_Freeport_NO3_flux_aug_wilcox$conf.int[1],
                                      Sacramento_Freeport_NO3_flux_sep_wilcox$conf.int[1]),
                           up_conf=c(Sacramento_Freeport_NO3_flux_oct_wilcox$conf.int[2],
                                     Sacramento_Freeport_NO3_flux_nov_wilcox$conf.int[2],
                                     Sacramento_Freeport_NO3_flux_dec_wilcox$conf.int[2],
                                     Sacramento_Freeport_NO3_flux_jan_wilcox$conf.int[2],
                                     Sacramento_Freeport_NO3_flux_feb_wilcox$conf.int[2],
                                     Sacramento_Freeport_NO3_flux_mar_wilcox$conf.int[2],
                                     Sacramento_Freeport_NO3_flux_apr_wilcox$conf.int[2],
                                     Sacramento_Freeport_NO3_flux_may_wilcox$conf.int[2],
                                     Sacramento_Freeport_NO3_flux_jun_wilcox$conf.int[2],
                                     Sacramento_Freeport_NO3_flux_jul_wilcox$conf.int[2],
                                     Sacramento_Freeport_NO3_flux_aug_wilcox$conf.int[2],
                                     Sacramento_Freeport_NO3_flux_sep_wilcox$conf.int[2]))

write.table(Flux_compare, "Sacramento_Freeport_NO3_flux_wilcox.txt", quote=FALSE, row.names=FALSE)

rng_flx <- max(abs(c(Flux_compare$up_conf, Flux_compare$low_conf)))
tiff("Sacramento_Freeport_NO3_flux_shift_wilcox_Vert_Bars.tif", height=600, width=800, res=130)
par(mar=c(4,5,0.5,0.5))
plot(seq(1:12), Flux_compare$chng_est, typ='h', lend=1, lwd=15, col='white', xaxt='n', xlim=c(1,13), ylim=c(-rng_flx, rng_flx), xlab="Month", ylab=expression(paste("Median Flux Change, kg",sep='')), las=1)
plotCI(seq(1:12), Flux_compare$chng_est, ui=Flux_compare$up_conf, li=Flux_compare$low_conf, pch=16, add=TRUE)
abline(h=0)
axis(side=1,at=seq(1,12,by=1), labels=format(c(seq(as.Date("2000-10-01"), as.Date("2000-12-01"), by="month"), seq(as.Date("2000-01-01"), as.Date("2000-09-01"), by="month")),'%b'), las=2)
legend('topright', c("Median difference", "90% Confidence Interval for the Median"), pch=c(16,NA), lwd=c(NA,1), pt.cex=c(1,NA), pt.bg=c('black',NA), bty='n', bg='white')
dev.off()


# End of non-standard EGRET plot section requested by Michael
# --------------------------------------------------------------------------------------------------------

#################  Using the plotConcQSmooth function  ################

#First do flow duration analysis
flowDuration(eList, centerDate = "06-01", qUnit = 2, span = 30)
date1 <- "1975-06-01"
date2 <- "2000-06-01"
date3 <- "2018-06-01"
qLow= baseQ
qHigh=highQ7

tiff("SacFreeport_Date_Discharge_NO3_conc_no_log.tif",height = 700, width = 1000, res=120)
plotConcQSmooth(eList,date1, date2, date3,qLow, qHigh, logScale=FALSE,printLegend =TRUE,legendLeft=0,legendTop=0,printTitle=TRUE)
dev.off()

flowDuration(eList, centerDate = "06-01", qUnit = 2, span = 30)
date1 <- "1975-12-01"
date2 <- "2000-12-01"
date3 <- "2018-12-01"
qLow= baseQ
qHigh=highQ7

tiff("SacFreeport_Date_Discharge_NO3_conc_no_log_December.tif",height = 700, width = 1000, res=120)
plotConcQSmooth(eList,date1, date2, date3,qLow, qHigh, logScale=FALSE,printLegend =TRUE,legendLeft=0,legendTop=0,printTitle=TRUE)
dev.off()

flowDuration(eList, centerDate = "12-01", qUnit = 2, span = 30)
date1 <- "1975-12-01"
date2 <- "2000-12-01"
date3 <- "2018-12-01"
qLow= baseQ
qHigh=highQ7

tiff("SacFreeport_Date_Discharge_NO3_conc_no_log_January.tif",height = 700, width = 1000, res=120)
plotConcQSmooth(eList,date1, date2, date3,qLow, qHigh, logScale=FALSE,printLegend =TRUE,legendLeft=0,legendTop=0,printTitle=TRUE)
dev.off()

# Exploring model behavior and adjusting model parameters
tiff("Contours_Inorg_N_Sacramento_Freeport.tif", height = 700, width = 1000, res=120)
plotContours(eList, qBottom=100,qTop=2500,yearStart=1975,yearEnd=2018, contourLevels=c(0.005,0.0075,0.01,0.0125,0.015,0.0175,0.02,0.0225,0.025,0.0275,0.03,0.0325,0.035,0.0375,0.04,0.0425,0.045,0.0475,0.05,0.0525,0.055,0.0575,0.06,0.0625,0.065), color.palette = colorRampPalette(c("violet", "purple", "blue", "cyan", "green", "yellow", "orange", "red"))) 
dev.off()

tiff("Log_Contours_Inorg_N_Sacramento_Freeport.tif", height = 700, width = 1000, res=120)
plotContours(eList, qBottom=100, qTop=2500, ylim=qTop,yearStart=1975, yearEnd=2018, contourLevels=c(-5.5,-5.4,-5.3,-5.2,-5.1,-5,-4.9,-4.8,-4.7,-4.6,-4.5,-4.4,-4.3,-4.2,-4.1,-4,-3.9,-3.8,-3.7,-3.6,-3.5,-3.4,-3.3,-3.2,-3.1,-3), color.palette = colorRampPalette(c("violet", "purple", "blue", "cyan", "green", "yellow", "orange", "red")), whatSurface=1) 
dev.off()

tiff("StdErr_of_Log_ContoursInorg_N_Sacramento_Freeport.tif", height = 700, width = 1000, res=120)
plotContours(eList, qBottom=100, qTop=2500, yearStart=1975, yearEnd=2018, contourLevels=c(0.43,0.44,0.45,0.46,0.47,0.48,0.49,0.5,0.51,0.52,0.53,0.54,0.55,0.56,0.57,0.58,0.59,0.6,0.61,0.62,0.63,0.64,0.65,0.66,0.67,0.68,0.69,0.7), color.palette = colorRampPalette(c("violet", "purple", "blue", "cyan", "green", "yellow", "orange", "red")), whatSurface=2) 
dev.off()

tiff("Contours_Difference_Inorg_N_Sacramento_Freeport.tif", height = 700, width = 1000, res=120)
plotDiffContours(eList, 1975,2018,NA,NA,maxDiff=0.08,color.palette = colorRampPalette(c("blue", "grey", "red")))
dev.off()

tiff("Contours_PercentDifference_Inorg_N_Sacramento_Freeport.tif", height = 700, width = 1000, res=120)
plotDiffContours(eList, 1975,2018,100,2500, maxDiff=200, plotPercent=TRUE)
dev.off()

#plotDiffContours2 <- function (eList, year0, year1, qBottom, qTop, maxDiff, whatSurface = 3, 
#                              tcl = 0.1, qUnit = 2, span = 60, pval = 0.05, printTitle = TRUE, 
#                              plotPercent = FALSE, vert1 = NA, vert2 = NA, horiz = NA, 
#                               flowDuration = TRUE, yTicks = NA, tick.lwd = 2, lwd = 1, 
#                               cex.main = 0.95, cex.axis = 1, customPar = FALSE, color.palette = colorRampPalette(c("blue", 
#                                                                                                                    "white", "red")), ...) 
#{
#  localINFO <- getInfo(eList)
#  localDaily <- getDaily(eList)
#  localsurfaces <- getSurfaces(eList)
#  if (is.numeric(qUnit)) {
#    qUnit <- qConst[shortCode = qUnit][[1]]
#  }
#  else if (is.character(qUnit)) {
#    qUnit <- qConst[qUnit][[1]]
#  }
#  if (!customPar) {
#    par(oma = c(6, 1, 6, 0))
#    par(mar = c(5, 5, 4, 2) + 0.1)
#  }
#  surfaceName <- c("log of Concentration", "Standard Error of log(C)", 
#                   "Concentration")
#  j <- 3
#  j <- if (whatSurface == 1) 
#    1
#  else j
#  j <- if (whatSurface == 2) 
#    2
#  else j
#  surf <- localsurfaces
#  bottomLogQ <- localINFO$bottomLogQ
#  stepLogQ <- localINFO$stepLogQ
#  nVectorLogQ <- localINFO$nVectorLogQ
#  bottomYear <- localINFO$bottomYear
#  stepYear <- localINFO$stepYear
#  nVectorYear <- localINFO$nVectorYear
#  start0 <- ((year0 - bottomYear) * 16) + 1
#  end0 <- start0 + 16
#  start1 <- ((year1 - bottomYear) * 16) + 1
#  end1 <- start1 + 16
#  if (plotPercent) {
#    diff <- (surf[, start1:end1, j] - surf[, start0:end0, 
#                                           j]) * 100/surf[, start0:end0, j]
#  }
#  else {
#    diff <- surf[, start1:end1, j] - surf[, start0:end0, 
#                                          j]
#  }
#  difft <- t(diff)
#  if (length(maxDiff) == 1) {
#    surfaceSpan <- c(-maxDiff, maxDiff)
#  }
#  else {
#    surfaceSpan <- range(maxDiff)
#  }
#  contourLevels <- pretty(surfaceSpan, n = 50)
#  x <- seq(0, 1, stepYear)
#  y <- ((1:nVectorLogQ) * stepLogQ) + (bottomLogQ - stepLogQ)
#  yLQ <- y
#  qFactor <- qUnit@qUnitFactor
#  y <- exp(y) * qFactor
#  numX <- length(x)
#  numY <- length(y)
#  if (is.na(yTicks[1])) {
#    qBottom <- max(0.9 * y[1], qBottom)
#    qTop <- min(1.1 * y[numY], qTop)
#    yTicks <- logPretty3(qBottom, qTop)
#    yTicks2 <- c(0.028,0.056,0.141,0.283,0.566,1.415,2.831,5.663,14.15,28.31,56.63,141.5) #cfs
#  }
#  xTicks <- c(0, 0.0848, 0.1642, 0.249, 0.331, 0.416, 0.498, 
#              0.583, 0.668, 0.75, 0.835, 0.917, 1)
#  xLabels <- c("Jan1", "Feb1", "Mar1", "Apr1", "May1", "Jun1", 
#               "Jul1", "Aug1", "Sep1", "Oct1", "Nov1", "Dec1", "Jan1")
#  nxTicks <- length(xTicks)
#  nYTicks <- length(yTicks)
#  numDays <- length(localDaily$Day)
#  freq <- rep(0, nVectorLogQ)
#  plotTitle <- if (printTitle) 
#    paste(localINFO$shortName, " ", localINFO$paramShortName, 
#          "\nEstimated", surfaceName[j], "change from", year0, 
#          "to", year1)
#  else ""
#  if (flowDuration) {
#    durSurf <- rep(0, 17 * nVectorLogQ)
#    dim(durSurf) <- c(17, nVectorLogQ)
#    centerDays <- seq(1, 388, 22.9)
#    centerDays <- floor(centerDays)
#    for (ix in 1:17) {
#      startDay <- centerDays[ix] - span
#      endDay <- centerDays[ix] + span
#      goodDays <- seq(startDay, endDay, 1)
#      goodDays <- ifelse(goodDays > 0, goodDays, goodDays + 
#                           365)
#      goodDays <- ifelse(goodDays < 366, goodDays, goodDays - 
#                           365)
#      numDays <- length(localDaily$Day)
#      isGood <- localDaily$Day %in% goodDays
#      spanDaily <- data.frame(localDaily, isGood)
#      spanDaily <- subset(spanDaily, isGood)
#      n <- length(spanDaily$Day)
#      LogQ <- spanDaily$LogQ
#      for (jQ in 1:nVectorLogQ) {
#        ind <- ifelse(LogQ < yLQ[jQ], 1, 0)
#        freq[jQ] <- sum(ind)/n
#      }
#      durSurf[ix, ] <- freq
#    }
#    plevels <- c(pval, 1 - pval)
#    pct1 <- format(plevels[1] * 100, digits = 2)
#    pct2 <- format(plevels[2] * 100, digits = 2)
#    firstLine <- paste(localINFO$shortName, "  ", localINFO$paramShortName, 
#                       sep = "")
#    secondLine <- if (plotPercent) {
#      paste("\nEstimated", surfaceName[j], "percent change from", 
#            year0, "to", year1)
#    }
#    else {
#      paste("\nEstimated", surfaceName[j], "change from", 
#            year0, "to", year1)
#    }
#    thirdLine <- paste("\nBlack lines are", pct1, "and", 
#                       pct2, "flow percentiles")
#    plotTitle <- paste(firstLine, secondLine, thirdLine)
#  }
#  vectorNone <- c(year0, log(yTicks[1], 10) - 1, year1, log(yTicks[1], 
#                                                            10) - 1)
#  v1 <- if (is.na(vert1)) 
#    vectorNone
#  else c(vert1, log(yTicks[1], 10), vert1, log(yTicks[nYTicks], 
#                                               10))
#  v2 <- if (is.na(vert2)) 
#    vectorNone
#  else c(vert2, log(yTicks[1], 10), vert2, log(yTicks[nYTicks], 
#                                               10))
#  h1 <- if (is.na(horiz)) 
#    vectorNone
#  else c(year0, log(horiz, 10), year1, log(horiz, 10))
#  deltaY <- (log(yTicks[length(yTicks)], 10) - log(yTicks[1], 
#                                                   10))/25
#  deltaX <- (1)/25
#  yLab <- qUnit@qUnitExpress
#  filled.contour(x, log(y, 10), difft, levels = contourLevels, 
#                 xlim = c(0, 1), ylim = c(log(yTicks[1], 10), log(yTicks[nYTicks], 
#                                                                  10)), xlab = "", ylab = yLab, xaxs = "i", yaxs = "i", 
#                 cex.main = cex.main, plot.axes = {
#                   axis(1, tcl = 0, at = xTicks, labels = xLabels, cex.axis = 0.9 * 
#                          cex.axis)
#                   axis(2, tcl = 0, las = 1, at = log(yTicks, 10), labels = yTicks, 
#                        cex.axis = cex.axis)
#                   axis(3, tcl = 0, at = xTicks, labels = FALSE)
#                   axis(4, tcl = 0, at = log(yTicks2, 10), labels = c("1","2","5","10","20","50","100","200","500","1000","2000","5000"))    #edm: yTicks -> yTicks2, added labels
#                   if (flowDuration) 
#                     contour(x, log(y, 10), durSurf, add = TRUE, drawlabels = FALSE, 
#                             levels = plevels, lwd = lwd,lty=2)  #adjust the 95% confidence intervals here
#                   segments(v1[1], v1[2], v1[3], v1[4])
#                   segments(v2[1], v2[2], v2[3], v2[4])
#                   segments(h1[1], h1[2], h1[3], h1[4])
#                   segments(xTicks, rep(log(yTicks[1], 10), length(xTicks)), 
#                            xTicks, rep(grconvertY(grconvertY(par("usr")[3], 
#                                                              from = "user", to = "inches") + tcl, from = "inches", 
#                                                   to = "user"), length(xTicks)), lwd = tick.lwd)
#                   segments(xTicks, rep(log(yTicks[nYTicks], 10), length(xTicks)), 
#                            xTicks, rep(grconvertY(grconvertY(par("usr")[4], 
#                                                              from = "user", to = "inches") - tcl, from = "inches", 
#                                                   to = "user"), length(xTicks)), lwd = tick.lwd)
#                   segments(rep(0, length(yTicks)), log(yTicks, 10), 
#                            rep(grconvertX(grconvertX(par("usr")[1], from = "user", 
#                                                      to = "inches") + tcl, from = "inches", to = "user"), 
#                                length(yTicks)), log(yTicks, 10), lwd = tick.lwd)
#                   segments(rep(grconvertX(grconvertX(par("usr")[2], 
#                                                      from = "user", to = "inches") - tcl, from = "inches", 
#                                           to = "user"), length(yTicks)), log(yTicks, 10), 
#                            rep(1, length(yTicks)), log(yTicks, 10), lwd = tick.lwd)
#                 }, color.palette = color.palette, ...)
#  
#  #try adding contours
#  par(new=T, mar=c(5.1,5.1,4.1,9.1))
#  contour(x, log(y, 10), difft, levels = seq(-100,200,by=50),xlim=c(0,1), ylim = c(log(yTicks[1], 10), log(yTicks[nYTicks],10)),
#          xaxs = "i",yaxs="i",xaxt="n",yaxt="n",labcex=1,method=c("flattest","edge")) #add=TRUE)
#  if (printTitle) 
#    title(plotTitle, outer = TRUE, cex.main = cex.main, line = -3)
#}
#
#
tiff("Contours_PercentDifference2_Inorg_N_Sacramento_Freeport.tif", height = 700, width = 1000, res=120)
plotDiffContours(eList, 1975,2018,qBottom = 100, qTop = 2500, maxDiff=c(-50,100), plotPercent=TRUE, lwd=3, color.palette=colorRampPalette(c("lightblue","white","beige","yellow", "orange", "red","brown")),tick.lwd = 1)
dev.off()

Sample$WY <- trunc(Sample$DecYear+0.25) 
tiff("Monthly_Boxplot_Inorg_N_Sacramento_Freeport.tif", height = 700, width = 1000, res=120)
par(mar=c(4,6,0.5,0.5))
boxplot(Sample$ConcAve~Sample$WY,log="y",varwidth=TRUE,ylim=c(0.001,0.4),yaxs="i",xlab="Water Year",las=1) 
mtext(side=2, expression(paste("Concentration, Inorganic Nitrogen, in mg  ",L^-1,sep="")),line=4)
dev.off()

localDaily_NO3_Plotting <-read.csv('localDailyNO3.csv')
attach(localDaily_NO3_Plotting)
Rdate <- strptime(as.character(Date),("%Y-%m-%d"))
localDaily_NO3_Plotting <- data.frame(localDaily_NO3_Plotting,Rdate)
setSweave("NitrateDailyModeled",5,5)
timePlot(Rdate,ConcDay,Plot=list(name="Freeport NO3 Sensor",what='lines',width='hairline',color='blue'))
graphics.off()


# ---------------------------
# Now run the EGRETci package
# ---------------------------

#Change working directory
setwd("..")
subDir <- 'EGRETci_plots'
if (file.exists(subDir)){
  setwd(file.path(getwd(),subDir))
} else {
  dir.create(file.path(getwd(),subDir), recursive = TRUE)
  setwd(file.path(getwd(),subDir))
}
#
#
#Interactive function to set up trend analysis:
#EGRETCI analysis
#caseSetUp <- trendSetUp(eList, nBoot=100, blockLength=200)
#eBoot <- wBT(eList, caseSetUp, fileName ="Sacramento_Freeport_SS_EGRETCIoutputText.txt")


caseSetUp <- trendSetUp(eList, 
                        year1=1975, 
                        year2=2018, 
                        nBoot = 200, 
                        bootBreak = 50, 
                        blockLength = 200)
eBoot <- wBT(eList, caseSetUp, fileName ="outputText.txt")





saveEGRETci(eList, eBoot, caseSetUp, fileName = "Sacramento_Freeport_NO3_EGRETci_output")

plotHistogramTrend2 <-
  function (eBoot, caseSetUp, eList, xSeq = seq(-100, 100, 10), 
            flux = TRUE, printTitle = TRUE, cex.main = 1.1, col.fill = "grey", xlim = c(-100,100),
            ...) 
  {
    bootOut <- eBoot$bootOut
    INFO <- eList$INFO
    if (flux) {
      xFlux <- eBoot$xFlux
      change <- 100 * bootOut$estF/bootOut$baseFlux
      reps <- 100 * xFlux/bootOut$baseFlux
      xlabel <- "Flux trend, in %"
      titleWord <- "Flux"
    }
    else {
      xConc <- eBoot$xConc
      change <- 100 * bootOut$estC/bootOut$baseConc
      reps <- 100 * xConc/bootOut$baseConc
      xlabel <- "Concentration trend, in %"
      titleWord <- "Concentration"
    }
    titleToPrint <- ifelse(printTitle, paste("Histogram of trend in", 
                                             INFO$paramShortName, "\n", titleWord, "Normalized Concentration:", 
                                             caseSetUp$year1, "to", caseSetUp$year2, "\n", INFO$shortName), 
                           "")
    hist(reps, breaks = xSeq, yaxs = "i", xaxs = "i", tcl = 0.5, 
         main = titleToPrint, freq = FALSE, xlab = xlabel, col = col.fill, 
         cex.main = cex.main, xlim=xlim, ...)
    abline(v = change, lwd = 3, lty = 2)
    abline(v = 0, lwd = 3)
    box()
    axis(3, tcl = 0.5, labels = FALSE)
    axis(4, tcl = 0.5, labels = FALSE)
  }

tiff("histo_Sacramento_Freeport_Trend_NO3_conc_flux.tif", height = 700, width = 1200, res=120)
par(mfrow=c(1,2))
plotHistogramTrend2(eBoot, caseSetUp, eList, flux=FALSE, xSeq = seq(-80000,80000,5),las=1,xlim=c(-100,100))
abline(h=0)

plotHistogramTrend2(eBoot, caseSetUp, eList, flux=TRUE, xSeq = seq(-50000,50000,5),las=1,xlim=c(-250,250))
abline(h=0)
dev.off()

nBoot <- 200
blockLength <- 200
coreOut <- 4 #Number of cores to leave out of processing tasks

widthCI <- 95
ciLower <- (50-(widthCI/2))/100
ciUpper <- (50+(widthCI/2))/100
probs <- c(ciLower,ciUpper)

nCores <- detectCores() - coreOut
cl <- makeCluster(nCores)
registerDoParallel(cl)
repAnnual <- foreach(n = 1:nBoot,.packages=c('EGRETci')) %dopar% {
  annualResults <- bootAnnual(eList, blockLength,startSeed = n)  
}
stopCluster(cl)

CIAnnualResults <- ciBands(eList, repAnnual, probs)
conc.poly.x <- c(CIAnnualResults$Year,rev(CIAnnualResults$Year))
conc.poly.y <- c(CIAnnualResults$FNConcLow,rev(CIAnnualResults$FNConcHigh))
flux.poly.x <- c(CIAnnualResults$Year,rev(CIAnnualResults$Year))
flux.poly.y <- c(CIAnnualResults$FNFluxLow*365,rev(CIAnnualResults$FNFluxHigh*365))

postscript("Sacramento_Freeport_Ann_Avg_Conc_&_Ann_Flow_Normalized_Conc_NO3_Boot.ps", family="Courier", height=6.25, width=7.25)
plotConcHistBoot(eList, CIAnnualResults, plotFlowNorm=TRUE, showYLabels=TRUE, showYAxis=TRUE,col=4)
polygon(x=conc.poly.x, y=conc.poly.y, col=rgb(24,116,205,40,max=255),border=NA)
dev.off()
#setSweave("Sacramento_Freeport_NO3_Conc_CI",7,7)
#plotConcHistBoot(eList, CIAnnualResults, plotFlowNorm=TRUE, showYLabels=TRUE, showYAxis=TRUE,col=4)
#graphics.off()
#postscript("Sacramento_Freeport_nitrate_flux_CI",7,7)
#plotFluxHistBoot(eList, fluxUnit=4, CIAnnualResults, plotFluxNorm = TRUE, showYLabels=TRUE, showYAxis=TRUE, col=4)
#graphics.off()
postscript("Sacramento_Freeport_Ann_Flux_&_Ann_Flow_Normalized_NO3_Flux_Boot.ps", family="Courier", height=6.25, width=7.25)
plotFluxHistBoot(eList, fluxUnit=4, CIAnnualResults, plotFluxNorm = TRUE, showYLabels=TRUE, showYAxis=TRUE, col=4)
polygon(x=flux.poly.x, y=flux.poly.y, col=rgb(24,116,205,40,max=255),border=NA)
dev.off()

saveEGRETci(eList, eBoot, fileName="N_Boot")
#save(repAnnual,file="Sacramento_Freeport_SS_RepAnnual.txt")
write.csv(repAnnual, "Sacramento_Freeport_SS_RepAnnual.csv")

#
## load(file="N_Boot.RData")
## load(file="RepAnnual")



#############################################################################
# Now do Orthophosphate, water, filtered, milligrams per liter as phosphorus
#############################################################################

# ***Note: Also referred to as SRP

parameterCd <- "00671"
startDate <- "1977-11-01"
endDate <- "2019-04-30"

#Sample <- readNWISSample(siteNumber, parameterCd, startDate, endDate)
#write.csv(Sample,"Sample_OP_NWIS.csv")

##need to have a user defined discharge file

fileName <- "FreeportDaily_OP_cfs.csv"
filePath <- "/users/joed/Documents/Documents19/Biogeochemistry_2019/Freeport_EGRET/"
Daily <-readUserDaily(filePath, fileName, hasHeader = TRUE, separator = ",", qUnit = 1)


#Daily <- readNWISDaily(siteNumber, QParameterCd, startDate, endDate)
fileName<- "SacFreeportOP.csv"
Sample <- readUserSample(filePath, fileName)
Sample <- removeDuplicates(Sample)
#Sample <- readNWISSample(siteNumber, parameterCd, startDate=startDate, endDate=endDate)
INFO <- readNWISInfo(siteNumber = siteNumber, parameterCd = parameterCd, interactive=FALSE)
INFO$staAbbrev <- paste(strsplit(INFO$station_nm," ")[[1]][1],strsplit(INFO$station_nm," ")[[1]][2])
range(Sample$Date)
#Sample <- removeDuplicates(Sample)
# "1977-11-15" "2019-04-03"
#No Breaks in OP data
eList <- NULL
eList <- mergeReport(INFO, Daily, Sample)

# Change the working directory; redirect plot output to OP folder
setwd("../..")
subDir <- 'OP/EGRET_plots'
if (file.exists(subDir)){
  setwd(file.path(getwd(),subDir))
} else {
  dir.create(file.path(getwd(),subDir), recursive = TRUE)
  setwd(file.path(getwd(),subDir))
}

#
# Plot water quality data
tiff("Conc_vs_Time_Sacramento_Freeport_OP.tif", height = 600, width = 800, res=120)
plotConcTime(eList)
dev.off()

plotConcTime(eList)
# Now, a classic Q-C plot
tiff("Conc-Q_Ortho_P_Sacramento_Freeport.tif", height = 600, width = 800, res=120)
plotConcQ(eList, logScale=TRUE)
dev.off()

# The data set as flux values rather than as concentrations
tiff("Flux-Q_Ortho_P_Sacramento_Freeport.tif", height = 600, width = 800, res=120)
plotFluxQ(eList, fluxUnit=4)
dev.off()

# Monthly boxplots
tiff("Monthly-Conc_BoxPlots_Sacramento_Freeport_Ortho_P.tif", height = 600, width = 800, res=120)
boxConcMonth(eList, logScale=TRUE)
dev.off()

# Flow on days sampled vs. all other days
tiff("Flow_on_days_sampled_vs_all_other_days_Sacramento_Freeport_Ortho_P.tif", height = 600, width = 800, res=120)
boxQTwice(eList, qUnit=1)
dev.off()

#####################################################
# Now start the Flow-Normalized Analysis for Ortho P
#####################################################

# Build the regression model
eList <- modelEstimation(eList, windowY = 7, windowQ = 2, windowS = 0.5, minNumObs = 100, minNumUncen =50)
MonthlyResults <- calculateMonthlyResults(eList)

# So as not confuse eList with NO3 above, save with an appropriate name
eList_OP <- eList

# Dump OP-related flow-normalized data to text file for bringing together with other monitoring sites
paLong <- 12
paStart <- 10
localDaily <- getDaily(eList_OP)
localAnnualResults <- setupYears(paStart = paStart, paLong = paLong, localDaily = localDaily)
write.table(localAnnualResults, file = 'SacFreeport_OP_RawVals.txt', quote=FALSE, row.names=FALSE)
write.csv(localDaily,'localDailyOP.csv')



# Plot the annual average concentration and annual flow-normalized concentration
tiff("Ann_Avg_Conc_&_Ann_Flow_Normalized_Conc_OP_Sacramento_Freeport.tif", height = 600, width = 800, res=120)
plotConcHist(eList_OP, plotFlowNorm=TRUE)
dev.off()

# Plot the annual average concentration and annual flow-normalized concentration
pdf("Ann_Avg_Conc_&_Ann_Flow_Normalized_Conc_OP_Sacramento_Freeport.pdf")
plotConcHist(eList_OP, plotFlowNorm=TRUE,tinyPlot = TRUE)
dev.off()

# Plot the annual flux and annual flow-normalized flux
tiff("Ann_Flux_&_Ann_Flow_Normalized_Flux_OPN_Sacramento_Freeport.tif" )
plotFluxHist(eList_OP, plotFlowNorm = TRUE) 
dev.off()

# Look for a trend change:
tableChange(eList_OP, fluxUnit=6, yearPoints=c(1974,1992,2000,2006,2015))



#Generate out-of-the-box diagnostic plots
tiff("fluxBiasMulti_OP_Sacramento_Freeport.tif", height = 1200, width = 1200, res=120)
fluxBiasMulti(eList_OP, moreTitle = "WRTDS")
dev.off()

tiff("Modeled_Daily_Conc_wObservations_OP_Sacramento_Freeport.tif", height = 800, width = 1000, res=120)
plotConcTimeDaily(eList_OP)
dev.off()


###########  Determine which flow rates to use for discharge-specific trends  #########

# Baseflow: mean of the annual 30-day low flows
baseQ <- mean(aggregate(Q30 ~ waterYear, data = localDaily, min)[,2])
baseQ_txt <- format(baseQ, digits=2)
baseQ_txt_cfs <- format(baseQ * 35.315, digits=2)

# mid-range: median flow rate across all years
medQ <- median(localDaily$Q)
medQ_txt <- format(medQ, digits=2)
medQ_txt_cfs <- format(medQ * 35.315, digits=2)

# high flow: get the 25% quantile of each year's maximum Q7
# This will help ensure (but not guarantee) that every year is well represented in the high-end flows
highQ7 <- as.numeric(quantile(aggregate(Q7 ~ waterYear, data = localDaily, max)[,2])[2])
highQ7_txt <- format(highQ7, digits=2)
highQ7_txt_cfs <- format(highQ7 * 35.315, digits=2)

# The following bit of script generates a figure discussed by Joe in an email on 6/2/17
# -------------------------------------------------------------------------------------

tiff("Discharge_specific_trends_OP_centered_on_06-01.tif", height = 600, width = 1200, res=120)
par(mar=c(4,6,4.1,8))
plotConcTimeSmooth(eList, q1 = baseQ, q2 = medQ, q3 = highQ7, centerDate='06-01', 
                   yearStart=localDaily$waterYear[1], yearEnd=localDaily$waterYear[nrow(localDaily)], 
                   logScale=TRUE, printLegend=FALSE)

# Determine y position of the legend
# ----------------------------------
y_l <- par('usr')[3]
y_u <- par('usr')[4]
y_m <- mean(y_l, y_u)

# Use the top version of the legend to add cfs

# legend('bottomleft', c(eval(substitute(expression(paste('Baseflow [',baseQ_txt,' ', m^3~s^-1,'(',baseQ_txt_cfs,' ',ft^3~s^-1,')]',sep=' ')), list(baseQ_txt=baseQ_txt, baseQ_txt_cfs=baseQ_txt_cfs))),
#                        eval(substitute(expression(paste('Median Flow [',medQ_txt,' ', m^3~s^-1,'(',medQ_txt_cfs,' ',ft^3~s^-1,')]',sep=' ')), list(medQ_txt=medQ_txt, medQ_txt_cfs=medQ_txt_cfs))),
#                        eval(substitute(expression(paste('High Flow [',highQ7_txt,' ', m^3~s^-1,'(',highQ7_txt_cfs,' ',ft^3~s^-1,')]',sep=' ')), list(highQ7_txt=highQ7_txt, highQ7_txt_cfs=highQ7_txt_cfs)))), 
#                        col=c('black','red','green'), lwd=2, bg='white', bty='n')

legend('bottomleft', c(eval(substitute(expression(paste('Baseflow (',baseQ_txt,' ', m^3~s^-1,')',sep=' ')), list(baseQ_txt=baseQ_txt))),
                       eval(substitute(expression(paste('Median Flow (',medQ_txt,' ', m^3~s^-1,')',sep=' ')), list(medQ_txt=medQ_txt))),
                       eval(substitute(expression(paste('High Flow (',highQ7_txt,' ', m^3~s^-1,')',sep=' ')), list(highQ7_txt=highQ7_txt)))), 
       col=c('black','red','green'), lwd=2, bg='white', bty='n')

dev.off()

# Restore original plotting margins
par(mar=c(5.1,6.1,4.1,2.1))

# The following bit of script generates a figure discussed by Michael in an email on 6/2/17
# -----------------------------------------------------------------------------------------
# Get the row number corresponding to the maximum concentration for each year (bearing in mind that these are 'within group' row numbers)
out <- aggregate(ConcDay ~ waterYear, data = localDaily, which.max)

# Ensure data is ordered by water year
out <- out[ order(out$waterYear), ]

# Get a count of the number of days within each of the water years
tbl <- table(localDaily$waterYear)

# Return the absolute row positions for each water year's max concentration
out$AbsConcDay <- out$ConcDay + cumsum(c(0,tbl[-length(tbl)]))

# Make a data.frame containing only the rows with each water year's max conc
out2 <- localDaily[out$AbsConcDay,]

# Gather only the needed data
out2 <- data.frame(Date=out2$Date, Q=out2$Q, Conc=out2$ConcDay, wyr=out2$waterYear, Julian=yday(as.Date(out2$Date)))

# Need to readjust the Julian day to start on Oct 1 (This function doesn't yet account for leap years)
out2$JulianWYR <- ifelse(out2$Julian > 273, out2$Julian - 273, 92 + out2$Julian)

# Plot it
tiff("JulianDay_of_Max_OP_Conc.tif", height = 600, width = 800, res=120)
plot(out2$wyr, out2$JulianWYR, pch=16, xlab='Water Year', ylab='Julian Day', yaxs='i', ylim=c(0,370), las=1)
dev.off()

# In a follow-up email from Michael on 6/7/17, Michael suggested two alterations:
#  1) Apply a 30-day moving average
#  2) Use the flow-normalized concentration

# To start with, I'll attempt to apply a 30-day window to the simulated daily concentrations

localDaily$ConcDay_30day <- c(rep(rollapply(localDaily$ConcDay, width=30, mean)[1],times=14) , rollapply(localDaily$ConcDay, width=30, mean), rep(rollapply(localDaily$ConcDay, width=30, mean)[length(rollapply(localDaily$ConcDay, width=30, mean))], times=15))
out_m <- aggregate(ConcDay_30day ~ waterYear, data = localDaily, which.max)
out_m <- out_m[ order(out_m$waterYear), ]
out_m$AbsConcDay <- out_m$ConcDay + cumsum(c(0,tbl[-length(tbl)]))
out_m2 <- localDaily[out_m$AbsConcDay,]
out_m2 <- data.frame(Date=out_m2$Date, Q=out_m2$Q, Conc30=out_m2$ConcDay_30day, wyr=out_m2$waterYear, Julian=yday(as.Date(out_m2$Date)))
out_m2$JulianWYR <- ifelse(out_m2$Julian > 273, out_m2$Julian - 273, 92 + out_m2$Julian)

tiff("JulianDay_of_Max_OP_Conc_Using_30_rollingAvg.tif", height = 600, width = 800, res=120)
plot(out_m2$wyr, out_m2$JulianWYR, pch=16, xlab='Water Year', ylab='Julian Day', yaxs='i', ylim=c(0,370), las=1)
dev.off()


# Next, I'll try using the flow-normalized concentration (same general code flow as above)
# First, try plotting flow-normalized concentration:
# Plot it
tiff("Flow_Normalized_Conc_SacFreeport_OP.tif", height = 600, width = 800, res=120)
plot(as.Date(localDaily$Date), localDaily$FNConc, typ='l', las=1, xlab='Time', ylab='Flow-normalized Concentration')
dev.off()

out_FN <- aggregate(FNConc ~ waterYear, data = localDaily, which.max)
out_FN <- out_FN[ order(out_FN$waterYear), ]
out_FN$AbsConcDay <- out_FN$FNConc + cumsum(c(0,tbl[-length(tbl)]))
out2_FN <- localDaily[out_FN$AbsConcDay,]
out2_FN <- data.frame(Date=out2_FN$Date, Q=out2_FN$Q, Conc=out2_FN$FNConc, wyr=out2_FN$waterYear, Julian=yday(as.Date(out2_FN$Date)))
out2_FN$JulianWYR <- ifelse(out2_FN$Julian > 273, out2_FN$Julian - 273, 92 + out2_FN$Julian)

# Plot it
tiff("JulianDay_of_Max_OP_Flow_Normalized_Conc.tif", height = 600, width = 800, res=120)
plot(out2_FN$wyr, out2_FN$JulianWYR, pch=16, xlab='Water Year', ylab='Julian Day', yaxs='i', ylim=c(0,370), las=1)
dev.off()

# The following bit of script generates a figure discussed by Michael in an email on 6/2/17
# -----------------------------------------------------------------------------------------
# Get the row number corresponding to the maximum concentration for each year (bearing in mind that these are 'within group' row numbers)
out <- aggregate(ConcDay ~ waterYear, data = localDaily, which.max)

# Ensure data is ordered by water year
out <- out[ order(out$waterYear), ]

# Get a count of the number of days within each of the water years
tbl <- table(localDaily$waterYear)

# Return the absolute row positions for each water year's max concentration
out$AbsConcDay <- out$ConcDay + cumsum(c(0,tbl[-length(tbl)]))

# Make a data.frame containing only the rows with each water year's max conc
out2 <- localDaily[out$AbsConcDay,]

# Gather only the needed data
out2 <- data.frame(Date=out2$Date, Q=out2$Q, Conc=out2$ConcDay, wyr=out2$waterYear, Julian=yday(as.Date(out2$Date)))

# Need to readjust the Julian day to start on Oct 1 (This function doesn't yet account for leap years)
out2$JulianWYR <- ifelse(out2$Julian > 273, out2$Julian - 273, 92 + out2$Julian)

# Plot it
tiff("JulianDay_of_Max_OP_Conc.tif", height = 600, width = 800, res=120)
plot(out2$wyr, out2$JulianWYR, pch=16, xlab='Water Year', ylab='Julian Day', yaxs='i', ylim=c(0,370), las=1)
dev.off()

# In a follow-up email from Michael on 6/7/17, Michael suggested two alterations:
#  1) Apply a 30-day moving average
#  2) Use the flow-normalized concentration

# To start with, I'll attempt to apply a 30-day window to the simulated daily concentrations

localDaily$ConcDay_30day <- c(rep(rollapply(localDaily$ConcDay, width=30, mean)[1],times=14) , rollapply(localDaily$ConcDay, width=30, mean), rep(rollapply(localDaily$ConcDay, width=30, mean)[length(rollapply(localDaily$ConcDay, width=30, mean))], times=15))
out_m <- aggregate(ConcDay_30day ~ waterYear, data = localDaily, which.max)
out_m <- out_m[ order(out_m$waterYear), ]
out_m$AbsConcDay <- out_m$ConcDay + cumsum(c(0,tbl[-length(tbl)]))
out_m2 <- localDaily[out_m$AbsConcDay,]
out_m2 <- data.frame(Date=out_m2$Date, Q=out_m2$Q, Conc30=out_m2$ConcDay_30day, wyr=out_m2$waterYear, Julian=yday(as.Date(out_m2$Date)))
out_m2$JulianWYR <- ifelse(out_m2$Julian > 273, out_m2$Julian - 273, 92 + out_m2$Julian)

tiff("JulianDay_of_Max_OP_Conc_Using_30_rollingAvg.tif", height = 600, width = 800, res=120)
plot(out_m2$wyr, out_m2$JulianWYR, pch=16, xlab='Water Year', ylab='Julian Day', yaxs='i', ylim=c(0,370), las=1)
dev.off()


# Next, I'll try using the flow-normalized concentration (same general code flow as above)
# First, try plotting flow-normalized concentration:
# Plot it
tiff("Flow_Normalized_Conc_SacFreeport_OP.tif", height = 600, width = 800, res=120)
plot(as.Date(localDaily$Date), localDaily$FNConc, typ='l', las=1, xlab='Time', ylab='Flow-normalized Concentration')
dev.off()

out_FN <- aggregate(FNConc ~ waterYear, data = localDaily, which.max)
out_FN <- out_FN[ order(out_FN$waterYear), ]
out_FN$AbsConcDay <- out_FN$FNConc + cumsum(c(0,tbl[-length(tbl)]))
out2_FN <- localDaily[out_FN$AbsConcDay,]
out2_FN <- data.frame(Date=out2_FN$Date, Q=out2_FN$Q, Conc=out2_FN$FNConc, wyr=out2_FN$waterYear, Julian=yday(as.Date(out2_FN$Date)))
out2_FN$JulianWYR <- ifelse(out2_FN$Julian > 273, out2_FN$Julian - 273, 92 + out2_FN$Julian)

# Plot it
tiff("JulianDay_of_Max_OP_Flow_Normalized_Conc.tif", height = 600, width = 800, res=120)
plot(out2_FN$wyr, out2_FN$JulianWYR, pch=16, xlab='Water Year', ylab='Julian Day', yaxs='i', ylim=c(0,370), las=1)
dev.off()


localDaily_OP <- getDaily(eList_OP)

early_decade_OP <- subset(localDaily_OP, localDaily_OP$Date < as.Date('1988-10-01') & localDaily_OP$Date > as.Date('1978-09-30'))
recent_decade_OP <- subset(localDaily_OP, localDaily_OP$Date > as.Date('2009-05-30'))


# Try calculating each month's mean and then take the mean/std. dev. of those and plot
# ------------------------------------------------------------------------------------

early_decade_monthly_mn_OP <- aggregate(ConcDay ~ MonthSeq, data = early_decade_OP, 'mean')
recent_decade_monthly_mn_OP <- aggregate(ConcDay ~ MonthSeq, data = recent_decade_OP, 'mean')

early_decade_monthly_mn_OP$month <- rep(c(10:12,1:9), times=10)
early_decade_mon_mn_OP <- aggregate(ConcDay ~ month, data = early_decade_monthly_mn_OP, 'mean')
early_decade_mon_sd_OP <- aggregate(ConcDay ~ month, data = early_decade_monthly_mn_OP, 'sd')
early_decade_mon_mn_OP <- early_decade_mon_mn_OP[c(10:12,1:9),]
early_decade_mon_sd_OP <- early_decade_mon_sd_OP[c(10:12,1:9),]

recent_decade_monthly_mn_OP$month <- rep(c(10:12,1:9), times=10)
recent_decade_mon_mn_OP <- aggregate(ConcDay ~ month, data = recent_decade_monthly_mn_OP, 'mean')
recent_decade_mon_sd_OP <- aggregate(ConcDay ~ month, data = recent_decade_monthly_mn_OP, 'sd')
recent_decade_mon_mn_OP <- recent_decade_mon_mn_OP[c(10:12,1:9),]
recent_decade_mon_sd_OP <- recent_decade_mon_sd_OP[c(10:12,1:9),]

mdat2_OP <- matrix(c(early_decade_mon_mn_OP$ConcDay, recent_decade_mon_mn_OP$ConcDay),
                   nrow=2,ncol = 12, byrow=TRUE,
                   dimnames = list(c("1978-1988", "2009-2019"),
                                   c(format(seq(as.Date('1973-10-01'), as.Date('1974-09-01'), by='month'), '%b'))))

mx <- max(c((early_decade_mon_mn_OP$ConcDay + early_decade_mon_sd_OP$ConcDay), (recent_decade_mon_mn_OP$ConcDay + recent_decade_mon_sd_OP$ConcDay)))
tiff("timing_shift_in_OP_conc_monthly_means.tif", height=800, width=900, res=130)
par(mar=c(3,5,2,1))
x <- barplot(mdat2_OP, beside=TRUE, las=1, ylim=c(0,mx), col = c("lightblue", "mistyrose"))
abline(h=0)
arrows(x0=x[1,], y0=early_decade_mon_mn_OP$ConcDay - early_decade_mon_sd_OP$ConcDay, x1=x[1,], y1=early_decade_mon_mn_OP$ConcDay + early_decade_mon_sd_OP$ConcDay, angle=90, length=0.04, code=3)
arrows(x0=x[2,], y0=recent_decade_mon_mn_OP$ConcDay - recent_decade_mon_sd_OP$ConcDay, x1=x[2,], y1=recent_decade_mon_mn_OP$ConcDay + recent_decade_mon_sd_OP$ConcDay, angle=90, length=0.04, code=3)
mtext(side=2, expression(paste('OP, mg ',L^-1,sep='')), line=3)
legend(x=12.5, y=0.9 * mx, c("1978-1988", "2009-2019"), pch=c(22,22), pt.cex=2, pt.bg=c("lightblue", "mistyrose"), bty='n', xpd=TRUE)
dev.off()


# Now attempting a Wilcox Test (aka Mann-Whitney-Wilcoxon Rank Sum test)
# ----------------------------------------------------------------------
early_jan_OP <- subset(early_decade_monthly_mn_OP, month==1)
recent_jan_OP <- subset(recent_decade_monthly_mn_OP, month==1)
Sacramento_Freeport_OP_conc_jan_wilcox <- wilcox.test(recent_jan_OP$ConcDay, early_jan_OP$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_feb_OP <- subset(early_decade_monthly_mn_OP, month==2)
recent_feb_OP <- subset(recent_decade_monthly_mn_OP, month==2)
Sacramento_Freeport_OP_conc_feb_wilcox <- wilcox.test(recent_feb_OP$ConcDay, early_feb_OP$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_mar_OP <- subset(early_decade_monthly_mn_OP, month==3)
recent_mar_OP <- subset(recent_decade_monthly_mn_OP, month==3)
Sacramento_Freeport_OP_conc_mar_wilcox <- wilcox.test(recent_mar_OP$ConcDay, early_mar_OP$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_apr_OP <- subset(early_decade_monthly_mn_OP, month==4)
recent_apr_OP <- subset(recent_decade_monthly_mn_OP, month==4)
Sacramento_Freeport_OP_conc_apr_wilcox <- wilcox.test(recent_apr_OP$ConcDay, early_apr_OP$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_may_OP <- subset(early_decade_monthly_mn_OP, month==5)
recent_may_OP <- subset(recent_decade_monthly_mn_OP, month==5)
Sacramento_Freeport_OP_conc_may_wilcox <- wilcox.test(recent_may_OP$ConcDay, early_may_OP$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_jun_OP <- subset(early_decade_monthly_mn_OP, month==6)
recent_jun_OP <- subset(recent_decade_monthly_mn_OP, month==6)
Sacramento_Freeport_OP_conc_jun_wilcox <- wilcox.test(recent_jun_OP$ConcDay, early_jun_OP$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_jul_OP <- subset(early_decade_monthly_mn_OP, month==7)
recent_jul_OP <- subset(recent_decade_monthly_mn_OP, month==7)
Sacramento_Freeport_OP_conc_jul_wilcox <- wilcox.test(recent_jul_OP$ConcDay, early_jul_OP$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_aug_OP <- subset(early_decade_monthly_mn_OP, month==8)
recent_aug_OP <- subset(recent_decade_monthly_mn_OP, month==8)
Sacramento_Freeport_OP_conc_aug_wilcox <- wilcox.test(recent_aug_OP$ConcDay, early_aug_OP$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_sep_OP <- subset(early_decade_monthly_mn_OP, month==9)
recent_sep_OP <- subset(recent_decade_monthly_mn_OP, month==9)
Sacramento_Freeport_OP_conc_sep_wilcox <- wilcox.test(recent_sep_OP$ConcDay, early_sep_OP$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_oct_OP <- subset(early_decade_monthly_mn_OP, month==10)
recent_oct_OP <- subset(recent_decade_monthly_mn_OP, month==10)
Sacramento_Freeport_OP_conc_oct_wilcox <- wilcox.test(recent_oct_OP$ConcDay, early_oct_OP$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_nov_OP <- subset(early_decade_monthly_mn_OP, month==11)
recent_nov_OP <- subset(recent_decade_monthly_mn_OP, month==11)
Sacramento_Freeport_OP_conc_nov_wilcox <- wilcox.test(recent_nov_OP$ConcDay, early_nov_OP$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_dec_OP <- subset(early_decade_monthly_mn_OP, month==12)
recent_dec_OP <- subset(recent_decade_monthly_mn_OP, month==12)
Sacramento_Freeport_OP_conc_dec_wilcox <- wilcox.test(recent_dec_OP$ConcDay, early_dec_OP$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

Conc_compare_OP <- data.frame(chng_est=c(Sacramento_Freeport_OP_conc_oct_wilcox$est,
                                         Sacramento_Freeport_OP_conc_nov_wilcox$est,
                                         Sacramento_Freeport_OP_conc_dec_wilcox$est,
                                         Sacramento_Freeport_OP_conc_jan_wilcox$est,
                                         Sacramento_Freeport_OP_conc_feb_wilcox$est,
                                         Sacramento_Freeport_OP_conc_mar_wilcox$est,
                                         Sacramento_Freeport_OP_conc_apr_wilcox$est,
                                         Sacramento_Freeport_OP_conc_may_wilcox$est,
                                         Sacramento_Freeport_OP_conc_jun_wilcox$est,
                                         Sacramento_Freeport_OP_conc_jul_wilcox$est,
                                         Sacramento_Freeport_OP_conc_aug_wilcox$est,
                                         Sacramento_Freeport_OP_conc_sep_wilcox$est),
                              low_conf=c(Sacramento_Freeport_OP_conc_oct_wilcox$conf.int[1],
                                         Sacramento_Freeport_OP_conc_nov_wilcox$conf.int[1],
                                         Sacramento_Freeport_OP_conc_dec_wilcox$conf.int[1],
                                         Sacramento_Freeport_OP_conc_jan_wilcox$conf.int[1],
                                         Sacramento_Freeport_OP_conc_feb_wilcox$conf.int[1],
                                         Sacramento_Freeport_OP_conc_mar_wilcox$conf.int[1],
                                         Sacramento_Freeport_OP_conc_apr_wilcox$conf.int[1],
                                         Sacramento_Freeport_OP_conc_may_wilcox$conf.int[1],
                                         Sacramento_Freeport_OP_conc_jun_wilcox$conf.int[1],
                                         Sacramento_Freeport_OP_conc_jul_wilcox$conf.int[1],
                                         Sacramento_Freeport_OP_conc_aug_wilcox$conf.int[1],
                                         Sacramento_Freeport_OP_conc_sep_wilcox$conf.int[1]),
                              up_conf=c(Sacramento_Freeport_OP_conc_oct_wilcox$conf.int[2],
                                        Sacramento_Freeport_OP_conc_nov_wilcox$conf.int[2],
                                        Sacramento_Freeport_OP_conc_dec_wilcox$conf.int[2],
                                        Sacramento_Freeport_OP_conc_jan_wilcox$conf.int[2],
                                        Sacramento_Freeport_OP_conc_feb_wilcox$conf.int[2],
                                        Sacramento_Freeport_OP_conc_mar_wilcox$conf.int[2],
                                        Sacramento_Freeport_OP_conc_apr_wilcox$conf.int[2],
                                        Sacramento_Freeport_OP_conc_may_wilcox$conf.int[2],
                                        Sacramento_Freeport_OP_conc_jun_wilcox$conf.int[2],
                                        Sacramento_Freeport_OP_conc_jul_wilcox$conf.int[2],
                                        Sacramento_Freeport_OP_conc_aug_wilcox$conf.int[2],
                                        Sacramento_Freeport_OP_conc_sep_wilcox$conf.int[2]))

write.table(Conc_compare_OP, "Sacramento_Freeport_OP_conc_wilcox.txt", quote=FALSE, row.names=FALSE)

rng_OP <- max(abs(c(Conc_compare_OP$up_conf, Conc_compare_OP$low_conf)))
tiff("Sacramento_Freeport_OP_conc_shift_wilcox_Vert_Bars.tif", height=600, width=800, res=130)
par(mar=c(4,5,0.5,0.5))
plot(seq(1:12), Conc_compare_OP$chng_est, typ='h', lend=1, lwd=15, col='white', xaxt='n', xlim=c(1,13), ylim=c(-rng_OP, rng_OP), xlab="Month", ylab=expression(paste("Median Concentration Change, mg  ",L^-1,sep='')), las=1)
plotCI(seq(1:12), Conc_compare_OP$chng_est, ui=Conc_compare_OP$up_conf, li=Conc_compare_OP$low_conf, pch=16, add=TRUE)
abline(h=0)
axis(side=1,at=seq(1,12,by=1), labels=format(c(seq(as.Date("2000-10-01"), as.Date("2000-12-01"), by="month"), seq(as.Date("2000-01-01"), as.Date("2000-09-01"), by="month")),'%b'), las=2)
legend('topright', c("Median difference", "90% Confidence Interval for the Median"), pch=c(16,NA), lwd=c(NA,1), pt.cex=c(1,NA), pt.bg=c('black',NA), bty='n', bg='white')
dev.off()

# Now do the load
# ---------------
early_decade_monthly_flx_OP <- aggregate(FluxDay ~ MonthSeq, data = early_decade_OP, 'sum')
recent_decade_monthly_flx_OP <- aggregate(FluxDay ~ MonthSeq, data = recent_decade_OP, 'sum')

# early_decade_monthly_mn$month <- format(seq(as.Date('1972-10-01'), as.Date('1982-09-30'), by='month'), '%b')
early_decade_monthly_flx_OP$month <- rep(c(10:12,1:9), times=10)
early_decade_mon_mn_flx_OP <- aggregate(FluxDay ~ month, data = early_decade_monthly_flx_OP, 'mean')
early_decade_mon_sd_flx_OP <- aggregate(FluxDay ~ month, data = early_decade_monthly_flx_OP, 'sd')
early_decade_mon_mn_flx_OP <- early_decade_mon_mn_flx_OP[c(10:12,1:9),]
early_decade_mon_sd_flx_OP <- early_decade_mon_sd_flx_OP[c(10:12,1:9),]

recent_decade_monthly_flx_OP$month <- rep(c(10:12,1:9), times=10)
recent_decade_mon_mn_flx_OP <- aggregate(FluxDay ~ month, data = recent_decade_monthly_flx_OP, 'mean')
recent_decade_mon_sd_flx_OP <- aggregate(FluxDay ~ month, data = recent_decade_monthly_flx_OP, 'sd')
recent_decade_mon_mn_flx_OP <- recent_decade_mon_mn_flx_OP[c(10:12,1:9),]
recent_decade_mon_sd_flx_OP <- recent_decade_mon_sd_flx_OP[c(10:12,1:9),]

mdat3_OP <- matrix(c(early_decade_mon_mn_flx_OP$FluxDay, recent_decade_mon_mn_flx_OP$FluxDay),
                   nrow=2,ncol = 12, byrow=TRUE,
                   dimnames = list(c("1978-1988", "2009-2019"),
                                   c(format(seq(as.Date('1973-10-01'), as.Date('1974-09-01'), by='month'), '%b'))))

mx <- max(c((early_decade_mon_mn_flx_OP$FluxDay + early_decade_mon_sd_flx_OP$FluxDay), (recent_decade_mon_mn_flx_OP$FluxDay + recent_decade_mon_sd_flx_OP$FluxDay)))
tiff("timing_shift_in_OP_load_monthly_means.tif", height=800, width=900, res=130)
x <- barplot(mdat3_OP, beside=TRUE, las=1, ylim=c(0,mx), col = c("lightblue", "mistyrose"))
abline(h=0)
arrows(x0=x[1,], y0=early_decade_mon_mn_flx_OP$FluxDay - early_decade_mon_sd_flx_OP$FluxDay, x1=x[1,], y1=early_decade_mon_mn_flx_OP$FluxDay + early_decade_mon_sd_flx_OP$FluxDay, angle=90, length=0.04, code=3)
arrows(x0=x[2,], y0=recent_decade_mon_mn_flx_OP$FluxDay - recent_decade_mon_sd_flx_OP$FluxDay, x1=x[2,], y1=recent_decade_mon_mn_flx_OP$FluxDay + recent_decade_mon_sd_flx_OP$FluxDay, angle=90, length=0.04, code=3)
mtext(side=2, expression(paste('OP, kg ',month^-1,sep='')), line=2.5)
legend(x=27, y=0.95 * mx, c("1978-1988", "2009-2019"), pch=c(22,22), pt.cex=2, pt.bg=c("lightblue", "mistyrose"), bty='n', xpd=TRUE)
dev.off()


early_jan_flx_OP <- subset(early_decade_monthly_flx_OP, month==1)
recent_jan_flx_OP <- subset(recent_decade_monthly_flx_OP, month==1)
Sacramento_Freeport_OP_flux_jan_wilcox <- wilcox.test(recent_jan_flx_OP$FluxDay, early_jan_flx_OP$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_feb_flx_OP <- subset(early_decade_monthly_flx_OP, month==2)
recent_feb_flx_OP <- subset(recent_decade_monthly_flx_OP, month==2)
Sacramento_Freeport_OP_flux_feb_wilcox <- wilcox.test(recent_feb_flx_OP$FluxDay, early_feb_flx_OP$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_mar_flx_OP <- subset(early_decade_monthly_flx_OP, month==3)
recent_mar_flx_OP <- subset(recent_decade_monthly_flx_OP, month==3)
Sacramento_Freeport_OP_flux_mar_wilcox <- wilcox.test(recent_mar_flx_OP$FluxDay, early_mar_flx_OP$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_apr_flx_OP <- subset(early_decade_monthly_flx_OP, month==4)
recent_apr_flx_OP <- subset(recent_decade_monthly_flx_OP, month==4)
Sacramento_Freeport_OP_flux_apr_wilcox <- wilcox.test(recent_apr_flx_OP$FluxDay, early_apr_flx_OP$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_may_flx_OP <- subset(early_decade_monthly_flx_OP, month==5)
recent_may_flx_OP <- subset(recent_decade_monthly_flx_OP, month==5)
Sacramento_Freeport_OP_flux_may_wilcox <- wilcox.test(recent_may_flx_OP$FluxDay, early_may_flx_OP$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_jun_flx_OP <- subset(early_decade_monthly_flx_OP, month==6)
recent_jun_flx_OP <- subset(recent_decade_monthly_flx_OP, month==6)
Sacramento_Freeport_OP_flux_jun_wilcox <- wilcox.test(recent_jun_flx_OP$FluxDay, early_jun_flx_OP$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_jul_flx_OP <- subset(early_decade_monthly_flx_OP, month==7)
recent_jul_flx_OP <- subset(recent_decade_monthly_flx_OP, month==7)
Sacramento_Freeport_OP_flux_jul_wilcox <- wilcox.test(recent_jul_flx_OP$FluxDay, early_jul_flx_OP$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_aug_flx_OP <- subset(early_decade_monthly_flx_OP, month==8)
recent_aug_flx_OP <- subset(recent_decade_monthly_flx_OP, month==8)
Sacramento_Freeport_OP_flux_aug_wilcox <- wilcox.test(recent_aug_flx_OP$FluxDay, early_aug_flx_OP$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_sep_flx_OP <- subset(early_decade_monthly_flx_OP, month==9)
recent_sep_flx_OP <- subset(recent_decade_monthly_flx_OP, month==9)
Sacramento_Freeport_OP_flux_sep_wilcox <- wilcox.test(recent_sep_flx_OP$FluxDay, early_sep_flx_OP$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_oct_flx_OP <- subset(early_decade_monthly_flx_OP, month==10)
recent_oct_flx_OP <- subset(recent_decade_monthly_flx_OP, month==10)
Sacramento_Freeport_OP_flux_oct_wilcox <- wilcox.test(recent_oct_flx_OP$FluxDay, early_oct_flx_OP$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_nov_flx_OP <- subset(early_decade_monthly_flx_OP, month==11)
recent_nov_flx_OP <- subset(recent_decade_monthly_flx_OP, month==11)
Sacramento_Freeport_OP_flux_nov_wilcox <- wilcox.test(recent_nov_flx_OP$FluxDay, early_nov_flx_OP$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_dec_flx_OP <- subset(early_decade_monthly_flx_OP, month==12)
recent_dec_flx_OP <- subset(recent_decade_monthly_flx_OP, month==12)
Sacramento_Freeport_OP_flux_dec_wilcox <- wilcox.test(recent_dec_flx_OP$FluxDay, early_dec_flx_OP$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)


Flux_compare_OP <- data.frame(chng_est=c(Sacramento_Freeport_OP_flux_oct_wilcox$est,
                                         Sacramento_Freeport_OP_flux_nov_wilcox$est,
                                         Sacramento_Freeport_OP_flux_dec_wilcox$est,
                                         Sacramento_Freeport_OP_flux_jan_wilcox$est,
                                         Sacramento_Freeport_OP_flux_feb_wilcox$est,
                                         Sacramento_Freeport_OP_flux_mar_wilcox$est,
                                         Sacramento_Freeport_OP_flux_apr_wilcox$est,
                                         Sacramento_Freeport_OP_flux_may_wilcox$est,
                                         Sacramento_Freeport_OP_flux_jun_wilcox$est,
                                         Sacramento_Freeport_OP_flux_jul_wilcox$est,
                                         Sacramento_Freeport_OP_flux_aug_wilcox$est,
                                         Sacramento_Freeport_OP_flux_sep_wilcox$est),
                              low_conf=c(Sacramento_Freeport_OP_flux_oct_wilcox$conf.int[1],
                                         Sacramento_Freeport_OP_flux_nov_wilcox$conf.int[1],
                                         Sacramento_Freeport_OP_flux_dec_wilcox$conf.int[1],
                                         Sacramento_Freeport_OP_flux_jan_wilcox$conf.int[1],
                                         Sacramento_Freeport_OP_flux_feb_wilcox$conf.int[1],
                                         Sacramento_Freeport_OP_flux_mar_wilcox$conf.int[1],
                                         Sacramento_Freeport_OP_flux_apr_wilcox$conf.int[1],
                                         Sacramento_Freeport_OP_flux_may_wilcox$conf.int[1],
                                         Sacramento_Freeport_OP_flux_jun_wilcox$conf.int[1],
                                         Sacramento_Freeport_OP_flux_jul_wilcox$conf.int[1],
                                         Sacramento_Freeport_OP_flux_aug_wilcox$conf.int[1],
                                         Sacramento_Freeport_OP_flux_sep_wilcox$conf.int[1]),
                              up_conf=c(Sacramento_Freeport_OP_flux_oct_wilcox$conf.int[2],
                                        Sacramento_Freeport_OP_flux_nov_wilcox$conf.int[2],
                                        Sacramento_Freeport_OP_flux_dec_wilcox$conf.int[2],
                                        Sacramento_Freeport_OP_flux_jan_wilcox$conf.int[2],
                                        Sacramento_Freeport_OP_flux_feb_wilcox$conf.int[2],
                                        Sacramento_Freeport_OP_flux_mar_wilcox$conf.int[2],
                                        Sacramento_Freeport_OP_flux_apr_wilcox$conf.int[2],
                                        Sacramento_Freeport_OP_flux_may_wilcox$conf.int[2],
                                        Sacramento_Freeport_OP_flux_jun_wilcox$conf.int[2],
                                        Sacramento_Freeport_OP_flux_jul_wilcox$conf.int[2],
                                        Sacramento_Freeport_OP_flux_aug_wilcox$conf.int[2],
                                        Sacramento_Freeport_OP_flux_sep_wilcox$conf.int[2]))

write.table(Flux_compare_OP, "Sacramento_Freeport_OP_flux_wilcox.txt", quote=FALSE, row.names=FALSE)

rng_flx_OP <- max(abs(c(Flux_compare_OP$up_conf, Flux_compare_OP$low_conf)))
tiff("Sacramento_Freeport_OP_flux_shift_wilcox_Vert_Bars.tif", height=600, width=800, res=130)
par(mar=c(4,5,0.5,0.5))
plot(seq(1:12), Flux_compare_OP$chng_est, typ='h', lend=1, lwd=15, col='white', xaxt='n', xlim=c(1,13), ylim=c(-rng_flx_OP, rng_flx_OP), xlab="Month", ylab=expression(paste("Median Flux Change, kg",sep='')), las=1)
plotCI(seq(1:12), Flux_compare_OP$chng_est, ui=Flux_compare_OP$up_conf, li=Flux_compare_OP$low_conf, pch=16, add=TRUE)
abline(h=0)
axis(side=1,at=seq(1,12,by=1), labels=format(c(seq(as.Date("2000-10-01"), as.Date("2000-12-01"), by="month"), seq(as.Date("2000-01-01"), as.Date("2000-09-01"), by="month")),'%b'), las=2)
legend('topright', c("Median difference", "90% Confidence Interval for the Median"), pch=c(16,NA), lwd=c(NA,1), pt.cex=c(1,NA), pt.bg=c('black',NA), bty='n', bg='white')
dev.off()



# Plot the annual average concentration and annual flow-normalized concentration
tiff("Ann_Avg_Conc_&_Ann_Flow_Normalized_Conc_orthoP_Sacramento_Freeport.tif", height = 600, width = 800, res=120)
plotConcHist(eList, plotFlowNorm=TRUE)
dev.off()

# Plot the annual flux and annual flow-normalized flux
tiff("Ann_Flux_&_Ann_Flow_Normalized_Flux_orthoP_Sacramento_Freeport.tif", height = 600, width = 800, res=120)
plotFluxHist(eList, plotFlowNorm = TRUE) # fluxMax) # fluxMax
dev.off()

#################  Using the plotConcQSmooth function  ###########
# First do flow duration analysis

flowDuration(eList, centerDate = "06-01", qUnit = 2, span = 30)
date1 <- "1980-06-01"
date2 <- "2000-06-01"
date3 <- "2018-06-01"
qLow= baseQ
qHigh=highQ7

tiff("BC1_Date_Discharge_OP_conc_no_log.tif",height = 700, width = 1000, res=120)
plotConcQSmooth(eList,date1, date2, date3,qLow, qHigh, logScale=FALSE,printLegend =TRUE,legendLeft=0,legendTop=0,printTitle=TRUE)
dev.off()

flowDuration(eList, centerDate = "12-01", qUnit = 2, span = 30)
date1 <- "1980-12-01"
date2 <- "2000-12-01"
date3 <- "2018-12-01"
qLow= baseQ
qHigh=highQ7

tiff("BC1_Date_Discharge_OP_conc_no_log_December.tif",height = 700, width = 1000, res=120)
plotConcQSmooth(eList,date1, date2, date3,qLow, qHigh, logScale=FALSE,printLegend =TRUE,legendLeft=0,legendTop=0,printTitle=TRUE)
dev.off()
# Look for a trend change:
tableChange(eList, fluxUnit=6, yearPoints=c(1976,2005,2017))



#Generate out-of-the-box diagnostic plots
tiff("fluxBiasMulti_Sacramento_Freeport_Ortho_P.tif", height = 1200, width = 1200, res=120)
fluxBiasMulti(eList, moreTitle = "WRTDS")
dev.off()

tiff("Modeled_Daily_Conc_wObservations_Sacramento_Freeport_Ortho_P.tif", height = 800, width = 1000, res=120)
plotConcTimeDaily(eList)
dev.off()

# Exploring model behavior and adjusting model parameters
tiff("Contours_Sacramento_Freeport_Ortho_P.tif", height = 700, width = 1000, res=120)
plotContours(eList,qBottom=100,qTop=2500,yearStart=1978,yearEnd=2019, contourLevels=c(0.0025,0.0028,0.0031,0.0034,0.0037,0.004,0.0043,0.0046,0.0049,0.0052,0.0055,0.0058,0.0061,0.0064,0.0067,0.007,0.0073,0.0076,0.0079,0.0082,0.0085,0.0088,0.0091,0.0094,0.0097,0.01,0.0103,0.0106,0.0109,0.0112,0.0115,0.0118), color.palette = colorRampPalette(c("violet", "purple", "blue", "cyan", "green", "yellow", "orange", "red"))) 
dev.off()

tiff("Log_Contours_Sacramento_Freeport_Ortho_P.tif", height = 700, width = 1000, res=120)
plotContours(eList,qBottom=100, qTop=2500, yearStart=1978, yearEnd=2019, contourLevels=c(-5.8,-5.75,-5.7,-5.65,-5.6,-5.55,-5.5,-5.45,-5.4,-5.35,-5.3,-5.25,-5.2,-5.15,-5.1,-5.05,-5,-4.95,-4.9,-4.85,-4.8,-4.75,-4.7,-4.65,-4.6,-4.55,-4.5), color.palette = colorRampPalette(c("violet", "purple", "blue", "cyan", "green", "yellow", "orange", "red")), whatSurface=1) 
dev.off()

tiff("StdErr_of_Log_Contours_Sacramento_Freeport_Ortho_P.tif", height = 700, width = 1000, res=120)
plotContours(eList, qBottom=100, qTop=2500, yearStart=1978, yearEnd=2019, contourLevels=c(0.35,0.36,0.37,0.38,0.39,0.4,0.41,0.42,0.43,0.44,0.45,0.46,0.47,0.48,0.49,0.5,0.51,0.52,0.53,0.54,0.55,0.56,0.57), color.palette = colorRampPalette(c("violet", "purple", "blue", "cyan", "green", "yellow", "orange", "red")), whatSurface=2) 
dev.off()

tiff("Contours_Difference_Ortho_P_Sacramento_Freeport.tif", height = 700, width = 1000, res=120)
plotDiffContours(eList,1978,2019,100,2500,maxDiff=0.15)
dev.off()

tiff("Contours_PercentDifference_Ortho_P_Sacramento_Freeport.tif", height = 700, width = 1000, res=120)
plotDiffContours(eList,1978,2019,100,2500,maxDiff=200, plotPercent=TRUE)
dev.off()

tiff("Contours_PercentDifference2_Ortho_P_Sacramento_Freeport.tif", height = 700, width = 1000, res=120)
plotDiffContours2(eList,1978, 2018,100,2500, maxDiff=c(-50,200), plotPercent=TRUE, lwd=3, color.palette=colorRampPalette(c("lightblue","white","beige","yellow", "orange", "red","brown")),tick.lwd = 1)
dev.off()

#####changepoint analysis
OP_changepoint <- read.table ("/Users/joed/LTIMP_TA/LTIMP_TA2/EGRET/BC1/OP/EGRET_plots/BC1_OP_RawVals.txt",header = TRUE)
OP_changepoint2<-as.numeric(OP_changepoint$FNConc) 
OPchangepoint2.binseg=cpt.meanvar(OP_changepoint2,test.stat='Normal',method='PELT',param.estimates=TRUE,Q=5,penalty="SIC")
cpts(OPchangepoint2.binseg)
tiff("/Users/joed/LTIMP_TA/LTIMP_TA2/EGRET/BC1/OP/EGRET_plots/BC1_OP_Changepoint",height = 700, width = 1200, res=120)
plot(OPchangepoint2.binseg,type='line',col="blue",ylim = c(0.0005,0.01))
dev.off()
OPchangepoint2.binseg
plot(OPchangepoint2.binseg,type='line',col="blue",ylim=c(0.0005,0.01))

# ---------------------------
# Now run the EGRETci package
# ---------------------------

#Change working directory
setwd("..")
subDir <- 'EGRETci_plots'
if (file.exists(subDir)){
  setwd(file.path(getwd(),subDir))
} else {
  dir.create(file.path(getwd(),subDir), recursive=TRUE)
  setwd(file.path(getwd(),subDir))
}

#Interactive function to set up trend analysis:
#caseSetUp <- trendSetUp(eList, nBoot=100, blockLength=200)
#eBoot <- wBT(eList, caseSetUp, fileName ="Sacramento_Freeport_orthoP_EGRETCIoutputText.txt")

caseSetUp <- trendSetUp(eList, 
                        year1=1979, 
                        year2=2018, 
                        nBoot = 200, 
                        bootBreak = 50, 
                        blockLength = 200)
eBoot <- wBT(eList, caseSetUp, fileName ="Sacramento_Freeport_orthoP_EGRETCIoutputText.txt")







saveEGRETci(eList, eBoot, caseSetUp, fileName = "EGRETci_output")

plotHistogramTrend2 <-
  function (eBoot, caseSetUp, eList, xSeq = seq(-100, 100, 10), 
            flux = TRUE, printTitle = TRUE, cex.main = 1.1, col.fill = "grey", xlim = c(-100,100),
            ...) 
  {
    bootOut <- eBoot$bootOut
    INFO <- eList$INFO
    if (flux) {
      xFlux <- eBoot$xFlux
      change <- 100 * bootOut$estF/bootOut$baseFlux
      reps <- 100 * xFlux/bootOut$baseFlux
      xlabel <- "Flux trend, in %"
      titleWord <- "Flux"
    }
    else {
      xConc <- eBoot$xConc
      change <- 100 * bootOut$estC/bootOut$baseConc
      reps <- 100 * xConc/bootOut$baseConc
      xlabel <- "Concentration trend, in %"
      titleWord <- "Concentration"
    }
    titleToPrint <- ifelse(printTitle, paste("Histogram of trend in", 
                                             INFO$paramShortName, "\n", titleWord, "Normalized Concentration:", 
                                             caseSetUp$year1, "to", caseSetUp$year2, "\n", INFO$shortName), 
                           "")
    hist(reps, breaks = xSeq, yaxs = "i", xaxs = "i", tcl = 0.5, 
         main = titleToPrint, freq = FALSE, xlab = xlabel, col = col.fill, 
         cex.main = cex.main, xlim=xlim, ...)
    abline(v = change, lwd = 3, lty = 2)
    abline(v = 0, lwd = 3)
    box()
    axis(3, tcl = 0.5, labels = FALSE)
    axis(4, tcl = 0.5, labels = FALSE)
  }

tiff("histo_OP_Sacramento_Freeport_Trend_conc_flux.tif", height = 700, width = 1200, res=120)
par(mfrow=c(1,2))
plotHistogramTrend2(eBoot, caseSetUp, eList, flux=FALSE, xSeq = seq(-8000,8000,5),las=1,xlim=c(-100,100))
abline(h=0)

plotHistogramTrend2(eBoot, caseSetUp, eList, flux=TRUE, xSeq = seq(-50000,50000,5),las=1)
abline(h=0)
dev.off()

nBoot <- 200
blockLength <- 200
coreOut <- 4 #Number of cores to leave out of processing tasks

widthCI <- 95
ciLower <- (50-(widthCI/2))/100
ciUpper <- (50+(widthCI/2))/100
probs <- c(ciLower,ciUpper)

nCores <- detectCores() - coreOut
cl <- makeCluster(nCores)
registerDoParallel(cl)
repAnnual <- foreach(n = 1:nBoot,.packages=c('EGRETci')) %dopar% {
  annualResults <- bootAnnual(eList, blockLength,startSeed = n)  
}
stopCluster(cl)

CIAnnualResults <- ciBands(eList, repAnnual, probs)
conc.poly.x <- c(CIAnnualResults$Year,rev(CIAnnualResults$Year))
conc.poly.y <- c(CIAnnualResults$FNConcLow,rev(CIAnnualResults$FNConcHigh))
flux.poly.x <- c(CIAnnualResults$Year,rev(CIAnnualResults$Year))
flux.poly.y <- c(CIAnnualResults$FNFluxLow*365,rev(CIAnnualResults$FNFluxHigh*365))

tiff("Ann_Avg_Conc_&_Ann_Flow_Normalized_Conc_Boot_Sacramento_Freeport_OP.tif", height = 500, width = 600, res=110)
plotConcHistBoot(eList, CIAnnualResults, plotFlowNorm=TRUE, showYLabels=TRUE, showYAxis=TRUE,col=4)
polygon(x=conc.poly.x, y=conc.poly.y, col=rgb(24,116,205,40,max=255),border=NA)
dev.off()

tiff("Ann_Flux_&_Ann_Flow_Normalized_Flux_Boot_Sacramento_Freeport_OP.tif", height = 500, width = 600, res=110)
plotFluxHistBoot(eList, fluxUnit=4, CIAnnualResults, plotFluxNorm = TRUE, showYLabels=TRUE, showYAxis=TRUE, col=4)
polygon(x=flux.poly.x, y=flux.poly.y, col=rgb(24,116,205,40,max=255),border=NA)
dev.off()
setSweave("Sacramento_Freeport_SRP_Conc_CI",7,7)
plotConcHistBoot(eList, CIAnnualResults, plotFlowNorm=TRUE, showYLabels=TRUE, showYAxis=TRUE,col=4)
graphics.off()
setSweave("Sacramento_Freeport_SRP_flux_CI",7,7)
plotFluxHistBoot(eList, fluxUnit=4, CIAnnualResults, plotFluxNorm = TRUE, showYLabels=TRUE, showYAxis=TRUE, col=4)
graphics.off()
saveEGRETci(eList, eBoot, fileName="N_Boot")
#save(repAnnual,file="RepAnnual")
write.csv(repAnnual,"repAnnual.csv")

# load(file="N_Boot.RData")
# load(file="RepAnnual")


#############################################################
# Working on TKN
#############################################################
startDate    <- "1973-02-01"
endDate      <- "2019-03-01"
siteNumber   <- "11447650"
QParameterCd <- "00060"
parameterCd  <- "00625"  # "TN"
##First get data from NWIS
##NWIS data has full record, no need to add anything from Kratzer
Sample <- readNWISSample(siteNumber, parameterCd, startDate, endDate)
#write.csv(Sample,"Sample_TKN_NWIS.csv")

##pull discharge from csv file
fileName     <- "SacFreeport_TKN_Discharge.csv"
Daily        <- Daily <-readUserDaily(filePath, fileName, hasHeader = TRUE, separator = ",", qUnit = 1)

#Sample       <- readUserSample(filePath, fileName)
Sample       <- removeDuplicates(Sample)
INFO         <- readNWISInfo(siteNumber = siteNumber, parameterCd = parameterCd, interactive=FALSE)
INFO$staAbbrev <- paste(strsplit(INFO$station_nm," ")[[1]][1],strsplit(INFO$station_nm," ")[[1]][2])

# Have a look at the available range of NO3 data
range(Sample$Date)
# "1973-02-01" "2019-03-13"
eList <- mergeReport(INFO, Daily, Sample)

# Change the working directory; redirect plot output to TN folder
setwd("../..")
subDir <- 'TKN/EGRET_plots'
if (file.exists(subDir)){
  setwd(file.path(getwd(),subDir))
} else {
  dir.create(file.path(getwd(),subDir), recursive=TRUE)
  setwd(file.path(getwd(),subDir))
}

# Plot water quality data
tiff("Conc_vs_Time_TKN_Sacramento_Freeport.tif", height = 600, width = 800, res=120)
plotConcTime(eList)
dev.off()

# Now, a classic Q-C plot
tiff("Conc-Q_Sacramento_Freeport_TN.tif", height = 600, width = 800, res=120)
plotConcQ(eList, logScale=TRUE)
dev.off()

# The data set as flux values rather than as concentrations
tiff("Flux-Q_Sacramento_Freeport_TN.tif", height = 600, width = 800, res=120)
plotFluxQ(eList, fluxUnit=4)
dev.off()

# Monthly boxplots
tiff("Monthly-Conc_BoxPlots_Sacramento_Freeport_TN.tif", height = 600, width = 800, res=120)
boxConcMonth(eList, logScale=TRUE)
dev.off()

# Flow on days sampled vs. all other days
tiff("Flow_on_days_sampled_vs_all_other_days_Sacramento_Freeport_TN.tif", height = 600, width = 800, res=120)
boxQTwice(eList, qUnit=1)
dev.off()

#########################################
# Now start the Flow-Normalized Analysis
#########################################

# Build the regression model
eList <- modelEstimation(eList, windowY = 7, windowQ = 2, windowS = 0.5, minNumObs = 100, minNumUncen =50)
eList_TN <- eList

MonthlyResults <- calculateMonthlyResults(eList_TN)

# Dump TN-related flow-normalized data to text file for bringing together with other monitoring sites
paLong <- 12
paStart <- 10
localDaily <- getDaily(eList_TN)
localAnnualResults <- setupYears(paStart = paStart, paLong = paLong, localDaily = localDaily)
write.table(localAnnualResults, file = 'SacFreeport_TKN_RawVals.txt', quote=FALSE, row.names=FALSE)
write.csv(localDaily,'localDailyTKN.csv')

# Plot the annual average concentration and annual flow-normalized concentration
tiff("Ann_Avg_Conc_&_Ann_Flow_Normalized_Conc_Sacramento_Freeport_TN.tif", height = 600, width = 800, res=120)
plotConcHist(eList_TN, plotFlowNorm=TRUE)
dev.off()

# Plot the annual flux and annual flow-normalized flux
tiff("Ann_Flux_&_Ann_Flow_Normalized_Flux_Sacramento_Freeport_TN.tif", height = 600, width = 800, res=120)
plotFluxHist(eList_TN, plotFlowNorm = TRUE) # fluxMax) # fluxMax
dev.off()

# Look for a trend change:
tableChange(eList_TN, fluxUnit=6, yearPoints=c(1989,2006,2017))



#Generate out-of-the-box diagnostic plots
tiff("fluxBiasMulti_Sacramento_Freeport_TN.tif", height = 1200, width = 1200, res=120)
fluxBiasMulti(eList, moreTitle = "WRTDS")
dev.off()

tiff("Modeled_Daily_Conc_wObservations_Sacramento_Freeport_TN.tif", height = 800, width = 1000, res=120)
plotConcTimeDaily(eList)
dev.off()

# Exploring model behavior and adjusting model parameters
tiff("Contours_Sacramento_Freeport_TN.tif", height = 700, width = 1000, res=120)
plotContours(eList, qBottom=100,qTop=2500,yearStart=1974,yearEnd=2019, contourLevels=c(0.050,0.075,0.100,0.125,0.150,0.175,0.200,0.225,0.250,0.275,0.300,0.325,0.350,0.375,0.400,0.425,0.450,0.475,0.500,0.525,0.55), color.palette = colorRampPalette(c("violet", "purple", "blue", "cyan", "green", "yellow", "orange", "red"))) 
dev.off()

tiff("Log_Contours_Sacramento_Freeport_TN.tif", height = 700, width = 1000, res=120)
plotContours(eList, qBottom=100, qTop=2500, yearStart=1974, yearEnd=2019, contourLevels=c(-2.85,-2.75,-2.65,-2.55,-2.45,-2.35,-2.25,-2.15,-2.05,-1.95,-1.85,-1.75,-1.65,-1.55,-1.45,-1.35,-1.25,-1.15,-1.05,-0.95,-0.85,-0.75), color.palette = colorRampPalette(c("violet", "purple", "blue", "cyan", "green", "yellow", "orange", "red")), whatSurface=1) 
dev.off()

tiff("StdErr_of_Log_Contours_Sacramento_Freeport_TN.tif", height = 700, width = 1000, res=120)
plotContours(eList, qBottom=100, qTop=2500, yearStart=1974, yearEnd=2019, contourLevels=c(0.12,0.14,0.16,0.18,0.20,0.22,0.24,0.26,0.28,0.30,0.32,0.34,0.36,0.38,0.40,0.42,0.44,0.46,0.48,0.50,0.52,0.54,0.56,0.58,0.60), color.palette = colorRampPalette(c("violet", "purple", "blue", "cyan", "green", "yellow", "orange", "red")), whatSurface=2) 
dev.off()

tiff("Contours_Difference_TKN_Sacramento_Freeport.tif", height = 700, width = 1000, res=120)
plotDiffContours(eList, 1974,2019,100,2500,maxDiff=1.0)
dev.off()

tiff("Contours_PercentDifference_TKN_Sacramento_Freeport.tif", height = 700, width = 1000, res=120)
plotDiffContours(eList, 1974,2019,100,2500, maxDiff=100, plotPercent=TRUE)
dev.off()

tiff("Contours_PercentDifference2_TKN_Sacramento_Freeport.tif", height = 700, width = 1000, res=120)
plotDiffContours(eList, 1974,2019,100,2500, maxDiff=c(-50,100), plotPercent=TRUE, lwd=3, color.palette=colorRampPalette(c("lightblue","white","beige","yellow", "orange", "red","brown")),tick.lwd = 1)
dev.off()

Sample$WY <- trunc(Sample$DecYear+0.25) 
tiff("Monthly_Boxplot_TKN_Sacramento_Freeport.tif", height = 700, width = 1000, res=120)
par(mar=c(4,6,0.5,0.5))
boxplot(Sample$ConcAve~Sample$WY,log="y",varwidth=TRUE,ylim=c(0.01,2),yaxs="i",xlab="Water Year",las=1) 
mtext(side=2, expression(paste("Concentration, Inorganic Nitrogen, in mg  ",L^-1,sep="")),line=4)
dev.off()


##############################
# Determine which flow rates to use for discharge-specific trends

# Baseflow: mean of the annual 30-day low flows
baseQ <- mean(aggregate(Q30 ~ waterYear, data = localDaily, min)[,2])
baseQ_txt <- format(baseQ, digits=2)
baseQ_txt_cfs <- format(baseQ * 35.315, digits=2)

# mid-range: median flow rate across all years
medQ <- median(localDaily$Q)
medQ_txt <- format(medQ, digits=2)
medQ_txt_cfs <- format(medQ * 35.315, digits=2)

# high flow: get the 25% quantile of each year's maximum Q7
# This will help ensure (but not guarantee) that every year is well represented in the high-end flows
highQ7 <- as.numeric(quantile(aggregate(Q7 ~ waterYear, data = localDaily, max)[,2])[2])
highQ7_txt <- format(highQ7, digits=2)
highQ7_txt_cfs <- format(highQ7 * 35.315, digits=2)

# The following bit of script generates a figure discussed by Joe in an email on 6/2/17
# -------------------------------------------------------------------------------------

tiff("Discharge_specific_trends_NO3_centered_on_06-01.tif", height = 600, width = 1200, res=120)
par(mar=c(4,6,4.1,8))
plotConcTimeSmooth(eList, q1 = baseQ, q2 = medQ, q3 = highQ7, centerDate='06-01', 
                   yearStart=localDaily$waterYear[1], yearEnd=localDaily$waterYear[nrow(localDaily)], 
                   logScale=FALSE, printLegend=FALSE)

# Determine y position of the legend
# ----------------------------------
y_l <- par('usr')[3]
y_u <- par('usr')[4]
y_m <- mean(y_l, y_u)

# Use the top version of the legend to add cfs

# legend('bottomleft', c(eval(substitute(expression(paste('Baseflow [',baseQ_txt,' ', m^3~s^-1,'(',baseQ_txt_cfs,' ',ft^3~s^-1,')]',sep=' ')), list(baseQ_txt=baseQ_txt, baseQ_txt_cfs=baseQ_txt_cfs))),
#                        eval(substitute(expression(paste('Median Flow [',medQ_txt,' ', m^3~s^-1,'(',medQ_txt_cfs,' ',ft^3~s^-1,')]',sep=' ')), list(medQ_txt=medQ_txt, medQ_txt_cfs=medQ_txt_cfs))),
#                        eval(substitute(expression(paste('High Flow [',highQ7_txt,' ', m^3~s^-1,'(',highQ7_txt_cfs,' ',ft^3~s^-1,')]',sep=' ')), list(highQ7_txt=highQ7_txt, highQ7_txt_cfs=highQ7_txt_cfs)))), 
#                        col=c('black','red','green'), lwd=2, bg='white', bty='n')

legend('bottomleft', c(eval(substitute(expression(paste('Baseflow (',baseQ_txt,' ', m^3~s^-1,')',sep=' ')), list(baseQ_txt=baseQ_txt))),
                       eval(substitute(expression(paste('Median Flow (',medQ_txt,' ', m^3~s^-1,')',sep=' ')), list(medQ_txt=medQ_txt))),
                       eval(substitute(expression(paste('High Flow (',highQ7_txt,' ', m^3~s^-1,')',sep=' ')), list(highQ7_txt=highQ7_txt)))), 
       col=c('black','red','green'), lwd=2, bg='white', bty='n')

dev.off()

# Restore original plotting margins
par(mar=c(5.1,6.1,4.1,2.1))

# The following bit of script generates a figure discussed by Michael in an email on 6/2/17
# -----------------------------------------------------------------------------------------
# Get the row number corresponding to the maximum concentration for each year (bearing in mind that these are 'within group' row numbers)
out <- aggregate(ConcDay ~ waterYear, data = localDaily, which.max)

# Ensure data is ordered by water year
out <- out[ order(out$waterYear), ]

# Get a count of the number of days within each of the water years
tbl <- table(localDaily$waterYear)

# Return the absolute row positions for each water year's max concentration
out$AbsConcDay <- out$ConcDay + cumsum(c(0,tbl[-length(tbl)]))

# Make a data.frame containing only the rows with each water year's max conc
out2 <- localDaily[out$AbsConcDay,]

# Gather only the needed data
out2 <- data.frame(Date=out2$Date, Q=out2$Q, Conc=out2$ConcDay, wyr=out2$waterYear, Julian=yday(as.Date(out2$Date)))

# Need to readjust the Julian day to start on Oct 1 (This function doesn't yet account for leap years)
out2$JulianWYR <- ifelse(out2$Julian > 273, out2$Julian - 273, 92 + out2$Julian)

# Plot it
tiff("JulianDay_of_Max_TKN_Conc.tif", height = 600, width = 800, res=120)
plot(out2$wyr, out2$JulianWYR, pch=16, xlab='Water Year', ylab='Julian Day', yaxs='i', ylim=c(0,370), las=1)
dev.off()

# In a follow-up email from Michael on 6/7/17, Michael suggested two alterations:
#  1) Apply a 30-day moving average
#  2) Use the flow-normalized concentration

# To start with, I'll attempt to apply a 30-day window to the simulated daily concentrations

localDaily$ConcDay_30day <- c(rep(rollapply(localDaily$ConcDay, width=30, mean)[1],times=14) , rollapply(localDaily$ConcDay, width=30, mean), rep(rollapply(localDaily$ConcDay, width=30, mean)[length(rollapply(localDaily$ConcDay, width=30, mean))], times=15))
out_m <- aggregate(ConcDay_30day ~ waterYear, data = localDaily, which.max)
out_m <- out_m[ order(out_m$waterYear), ]
out_m$AbsConcDay <- out_m$ConcDay + cumsum(c(0,tbl[-length(tbl)]))
out_m2 <- localDaily[out_m$AbsConcDay,]
out_m2 <- data.frame(Date=out_m2$Date, Q=out_m2$Q, Conc30=out_m2$ConcDay_30day, wyr=out_m2$waterYear, Julian=yday(as.Date(out_m2$Date)))
out_m2$JulianWYR <- ifelse(out_m2$Julian > 273, out_m2$Julian - 273, 92 + out_m2$Julian)

tiff("JulianDay_of_Max_TKN_Conc_Using_30_rollingAvg.tif", height = 600, width = 800, res=120)
plot(out_m2$wyr, out_m2$JulianWYR, pch=16, xlab='Water Year', ylab='Julian Day', yaxs='i', ylim=c(0,370), las=1)
dev.off()


# Next, I'll try using the flow-normalized concentration (same general code flow as above)
# First, try plotting flow-normalized concentration:
# Plot it
tiff("Flow_Normalized_Conc_BC1_TKN.tif", height = 600, width = 800, res=120)
plot(as.Date(localDaily$Date), localDaily$FNConc, typ='l', las=1, xlab='Time', ylab='Flow-normalized Concentration')
dev.off()

out_FN <- aggregate(FNConc ~ waterYear, data = localDaily, which.max)
out_FN <- out_FN[ order(out_FN$waterYear), ]
out_FN$AbsConcDay <- out_FN$FNConc + cumsum(c(0,tbl[-length(tbl)]))
out2_FN <- localDaily[out_FN$AbsConcDay,]
out2_FN <- data.frame(Date=out2_FN$Date, Q=out2_FN$Q, Conc=out2_FN$FNConc, wyr=out2_FN$waterYear, Julian=yday(as.Date(out2_FN$Date)))
out2_FN$JulianWYR <- ifelse(out2_FN$Julian > 273, out2_FN$Julian - 273, 92 + out2_FN$Julian)

# Plot it
tiff("JulianDay_of_Max_TKN_Flow_Normalized_Conc.tif", height = 600, width = 800, res=120)
plot(out2_FN$wyr, out2_FN$JulianWYR, pch=16, xlab='Water Year', ylab='Julian Day', yaxs='i', ylim=c(0,370), las=1)
dev.off()



# --------------------------------------------------------------------------------------------------------
# The following script is for a non-standard EGRET plot and instead help generate a plot Michael requested

localDaily <- getDaily(eList)

# Will need to adjust the date range below based on each gages unique start/stop dates
early_decade <- subset(localDaily, localDaily$Date > as.Date('1974-09-30') & localDaily$Date < as.Date('1984-10-01'))
recent_decade <- subset(localDaily, localDaily$Date > as.Date('2009-05-30'))


early_decade_monthly_mn <- aggregate(ConcDay ~ MonthSeq, data = early_decade, 'mean')
recent_decade_monthly_mn <- aggregate(ConcDay ~ MonthSeq, data = recent_decade, 'mean')

# early_decade_monthly_mn$month <- format(seq(as.Date('1972-10-01'), as.Date('1982-09-30'), by='month'), '%b')
early_decade_monthly_mn$month <- rep(c(10:12,1:9), times=10)
early_decade_mon_mn <- aggregate(ConcDay ~ month, data = early_decade_monthly_mn, 'mean')
early_decade_mon_sd <- aggregate(ConcDay ~ month, data = early_decade_monthly_mn, 'sd')
early_decade_mon_mn <- early_decade_mon_mn[c(10:12,1:9),]
early_decade_mon_sd <- early_decade_mon_sd[c(10:12,1:9),]

recent_decade_monthly_mn$month <- rep(c(10:12,1:9), times=10)
recent_decade_mon_mn <- aggregate(ConcDay ~ month, data = recent_decade_monthly_mn, 'mean')
recent_decade_mon_sd <- aggregate(ConcDay ~ month, data = recent_decade_monthly_mn, 'sd')
recent_decade_mon_mn <- recent_decade_mon_mn[c(10:12,1:9),]
recent_decade_mon_sd <- recent_decade_mon_sd[c(10:12,1:9),]


mdat2 <- matrix(c(early_decade_mon_mn$ConcDay, recent_decade_mon_mn$ConcDay),
                nrow=2,ncol = 12, byrow=TRUE,
                dimnames = list(c("1974-1984", "2009-2019"),
                                c(format(seq(as.Date('1973-10-01'), as.Date('1974-09-01'), by='month'), '%b'))))

# Be sure to adjust the legend's first decade start and stop year correctly
mx <- max(c((early_decade_mon_mn$ConcDay + early_decade_mon_sd$ConcDay), (recent_decade_mon_mn$ConcDay + recent_decade_mon_sd$ConcDay)))

tiff("timing_shift_in_TKN_conc_monthly_means.tif", height=800, width=900, res=130)
par(mar=c(3,5,2,1))
x <- barplot(mdat2, beside=TRUE, las=1, ylim=c(0,mx), col = c("lightblue", "mistyrose"))
abline(h=0)
arrows(x0=x[1,], y0=early_decade_mon_mn$ConcDay - early_decade_mon_sd$ConcDay, x1=x[1,], y1=early_decade_mon_mn$ConcDay + early_decade_mon_sd$ConcDay, angle=90, length=0.04, code=3)
arrows(x0=x[2,], y0=recent_decade_mon_mn$ConcDay - recent_decade_mon_sd$ConcDay, x1=x[2,], y1=recent_decade_mon_mn$ConcDay + recent_decade_mon_sd$ConcDay, angle=90, length=0.04, code=3)
mtext(side=2, expression(paste(TKN,', mg ',L^-1,sep='')), line=3)
legend(x=25, y=0.9 * mx, c("1974-1984", "2009-2019"), pch=c(22,22), pt.cex=2, pt.bg=c("lightblue", "mistyrose"), bty='n', xpd=TRUE)
dev.off()


# Now attempting a Wilcox Test (aka Mann-Whitney-Wilcoxon Rank Sum test)
# ----------------------------------------------------------------------
early_jan <- subset(early_decade_monthly_mn, month==1)
recent_jan <- subset(recent_decade_monthly_mn, month==1)
Sacramento_Freeport_TKN_conc_jan_wilcox <- wilcox.test(recent_jan$ConcDay, early_jan$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_feb <- subset(early_decade_monthly_mn, month==2)
recent_feb <- subset(recent_decade_monthly_mn, month==2)
Sacramento_Freeport_TKN_conc_feb_wilcox <- wilcox.test(recent_feb$ConcDay, early_feb$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_mar <- subset(early_decade_monthly_mn, month==3)
recent_mar <- subset(recent_decade_monthly_mn, month==3)
Sacramento_Freeport_TKN_conc_mar_wilcox <- wilcox.test(recent_mar$ConcDay, early_mar$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_apr <- subset(early_decade_monthly_mn, month==4)
recent_apr <- subset(recent_decade_monthly_mn, month==4)
Sacramento_Freeport_TKN_conc_apr_wilcox <- wilcox.test(recent_apr$ConcDay, early_apr$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_may <- subset(early_decade_monthly_mn, month==5)
recent_may <- subset(recent_decade_monthly_mn, month==5)
Sacramento_Freeport_TKN_conc_may_wilcox <- wilcox.test(recent_may$ConcDay, early_may$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_jun <- subset(early_decade_monthly_mn, month==6)
recent_jun <- subset(recent_decade_monthly_mn, month==6)
Sacramento_Freeport_TKN_conc_jun_wilcox <- wilcox.test(recent_jun$ConcDay, early_jun$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_jul <- subset(early_decade_monthly_mn, month==7)
recent_jul <- subset(recent_decade_monthly_mn, month==7)
Sacramento_Freeport_TKN_conc_jul_wilcox <- wilcox.test(recent_jul$ConcDay, early_jul$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_aug <- subset(early_decade_monthly_mn, month==8)
recent_aug <- subset(recent_decade_monthly_mn, month==8)
Sacramento_Freeport_TKN_conc_aug_wilcox <- wilcox.test(recent_aug$ConcDay, early_aug$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_sep <- subset(early_decade_monthly_mn, month==9)
recent_sep <- subset(recent_decade_monthly_mn, month==9)
Sacramento_Freeport_TKN_conc_sep_wilcox <- wilcox.test(recent_sep$ConcDay, early_sep$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_oct <- subset(early_decade_monthly_mn, month==10)
recent_oct <- subset(recent_decade_monthly_mn, month==10)
Sacramento_Freeport_TKN_conc_oct_wilcox <- wilcox.test(recent_oct$ConcDay, early_oct$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_nov <- subset(early_decade_monthly_mn, month==11)
recent_nov <- subset(recent_decade_monthly_mn, month==11)
Sacramento_Freeport_TKN_conc_nov_wilcox <- wilcox.test(recent_nov$ConcDay, early_nov$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_dec <- subset(early_decade_monthly_mn, month==12)
recent_dec <- subset(recent_decade_monthly_mn, month==12)
Sacramento_Freeport_TKN_conc_dec_wilcox <- wilcox.test(recent_dec$ConcDay, early_dec$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

Conc_compare <- data.frame(chng_est=c(Sacramento_Freeport_TKN_conc_oct_wilcox$est,
                                      Sacramento_Freeport_TKN_conc_nov_wilcox$est,
                                      Sacramento_Freeport_TKN_conc_dec_wilcox$est,
                                      Sacramento_Freeport_TKN_conc_jan_wilcox$est,
                                      Sacramento_Freeport_TKN_conc_feb_wilcox$est,
                                      Sacramento_Freeport_TKN_conc_mar_wilcox$est,
                                      Sacramento_Freeport_TKN_conc_apr_wilcox$est,
                                      Sacramento_Freeport_TKN_conc_may_wilcox$est,
                                      Sacramento_Freeport_TKN_conc_jun_wilcox$est,
                                      Sacramento_Freeport_TKN_conc_jul_wilcox$est,
                                      Sacramento_Freeport_TKN_conc_aug_wilcox$est,
                                      Sacramento_Freeport_TKN_conc_sep_wilcox$est),
                           low_conf=c(Sacramento_Freeport_TKN_conc_oct_wilcox$conf.int[1],
                                      Sacramento_Freeport_TKN_conc_nov_wilcox$conf.int[1],
                                      Sacramento_Freeport_TKN_conc_dec_wilcox$conf.int[1],
                                      Sacramento_Freeport_TKN_conc_jan_wilcox$conf.int[1],
                                      Sacramento_Freeport_TKN_conc_feb_wilcox$conf.int[1],
                                      Sacramento_Freeport_TKN_conc_mar_wilcox$conf.int[1],
                                      Sacramento_Freeport_TKN_conc_apr_wilcox$conf.int[1],
                                      Sacramento_Freeport_TKN_conc_may_wilcox$conf.int[1],
                                      Sacramento_Freeport_TKN_conc_jun_wilcox$conf.int[1],
                                      Sacramento_Freeport_TKN_conc_jul_wilcox$conf.int[1],
                                      Sacramento_Freeport_TKN_conc_aug_wilcox$conf.int[1],
                                      Sacramento_Freeport_TKN_conc_sep_wilcox$conf.int[1]),
                           up_conf=c(Sacramento_Freeport_TKN_conc_oct_wilcox$conf.int[2],
                                     Sacramento_Freeport_TKN_conc_nov_wilcox$conf.int[2],
                                     Sacramento_Freeport_TKN_conc_dec_wilcox$conf.int[2],
                                     Sacramento_Freeport_TKN_conc_jan_wilcox$conf.int[2],
                                     Sacramento_Freeport_TKN_conc_feb_wilcox$conf.int[2],
                                     Sacramento_Freeport_TKN_conc_mar_wilcox$conf.int[2],
                                     Sacramento_Freeport_TKN_conc_apr_wilcox$conf.int[2],
                                     Sacramento_Freeport_TKN_conc_may_wilcox$conf.int[2],
                                     Sacramento_Freeport_TKN_conc_jun_wilcox$conf.int[2],
                                     Sacramento_Freeport_TKN_conc_jul_wilcox$conf.int[2],
                                     Sacramento_Freeport_TKN_conc_aug_wilcox$conf.int[2],
                                     Sacramento_Freeport_TKN_conc_sep_wilcox$conf.int[2]))

write.table(Conc_compare, "Sacramento_Freeport_TKN_conc_wilcox.txt", quote=FALSE, row.names=FALSE)

rng <- max(abs(c(Conc_compare$up_conf, Conc_compare$low_conf)))
tiff("Sacramento_Freeport_TKN_conc_shift_wilcox_Vert_Bars.tif", height=600, width=800, res=130)
par(mar=c(4,5,0.5,0.5))
plot(seq(1:12), Conc_compare$chng_est, typ='h', lend=1, lwd=15, col='white', xaxt='n', xlim=c(1,13), ylim=c(-rng, rng), xlab="Month", ylab=expression(paste("Median Concentration Change, mg  ",L^-1,sep='')), las=1)
plotCI(seq(1:12), Conc_compare$chng_est, ui=Conc_compare$up_conf, li=Conc_compare$low_conf, pch=16, add=TRUE)
abline(h=0)
axis(side=1,at=seq(1,12,by=1), labels=format(c(seq(as.Date("2000-10-01"), as.Date("2000-12-01"), by="month"), seq(as.Date("2000-01-01"), as.Date("2000-09-01"), by="month")),'%b'), las=2)
legend('topright', c("Median difference", "90% Confidence Interval for the Median"), pch=c(16,NA), lwd=c(NA,1), pt.cex=c(1,NA), pt.bg=c('black',NA), bty='n', bg='white')
dev.off()


# Now do the load
# ---------------
early_decade_monthly_flx <- aggregate(FluxDay ~ MonthSeq, data = early_decade, 'sum')
recent_decade_monthly_flx <- aggregate(FluxDay ~ MonthSeq, data = recent_decade, 'sum')

# early_decade_monthly_mn$month <- format(seq(as.Date('1972-10-01'), as.Date('1982-09-30'), by='month'), '%b')
early_decade_monthly_flx$month <- rep(c(10:12,1:9), times=10)
early_decade_mon_mn_flx <- aggregate(FluxDay ~ month, data = early_decade_monthly_flx, 'mean')
early_decade_mon_sd_flx <- aggregate(FluxDay ~ month, data = early_decade_monthly_flx, 'sd')
early_decade_mon_mn_flx <- early_decade_mon_mn_flx[c(10:12,1:9),]
early_decade_mon_sd_flx <- early_decade_mon_sd_flx[c(10:12,1:9),]

recent_decade_monthly_flx$month <- rep(c(10:12,1:9), times=10)
recent_decade_mon_mn_flx <- aggregate(FluxDay ~ month, data = recent_decade_monthly_flx, 'mean')
recent_decade_mon_sd_flx <- aggregate(FluxDay ~ month, data = recent_decade_monthly_flx, 'sd')
recent_decade_mon_mn_flx <- recent_decade_mon_mn_flx[c(10:12,1:9),]
recent_decade_mon_sd_flx <- recent_decade_mon_sd_flx[c(10:12,1:9),]

mdat3 <- matrix(c(early_decade_mon_mn_flx$FluxDay, recent_decade_mon_mn_flx$FluxDay),
                nrow=2,ncol = 12, byrow=TRUE,
                dimnames = list(c("1974-1984", "2009-2019"),
                                c(format(seq(as.Date('1973-10-01'), as.Date('1974-09-01'), by='month'), '%b'))))

mx <- max(c((early_decade_mon_mn_flx$FluxDay + early_decade_mon_sd_flx$FluxDay), (recent_decade_mon_mn_flx$FluxDay + recent_decade_mon_sd_flx$FluxDay)))
tiff("timing_shift_in_TKN_load_monthly_means.tif", height=800, width=900, res=130)
x <- barplot(mdat3, beside=TRUE, las=1, ylim=c(0,mx), col = c("lightblue", "mistyrose"))
abline(h=0)
arrows(x0=x[1,], y0=early_decade_mon_mn_flx$FluxDay - early_decade_mon_sd_flx$FluxDay, x1=x[1,], y1=early_decade_mon_mn_flx$FluxDay + early_decade_mon_sd_flx$FluxDay, angle=90, length=0.04, code=3)
arrows(x0=x[2,], y0=recent_decade_mon_mn_flx$FluxDay - recent_decade_mon_sd_flx$FluxDay, x1=x[2,], y1=recent_decade_mon_mn_flx$FluxDay + recent_decade_mon_sd_flx$FluxDay, angle=90, length=0.04, code=3)
mtext(side=2, expression(paste(TKN,', kg ',month^-1,sep='')), line=2.5)
legend(x=30, y=0.9 * mx, c("1974-1984", "2009-2019"), pch=c(22,22), pt.cex=2, pt.bg=c("lightblue", "mistyrose"), bty='n', xpd=TRUE)
dev.off()

# Apply Wilcox.text to the monthly loads here...
early_jan_flx <- subset(early_decade_monthly_flx, month==1)
recent_jan_flx <- subset(recent_decade_monthly_flx, month==1)
Sacramento_Freeport_TKN_flux_jan_wilcox <- wilcox.test(recent_jan_flx$FluxDay, early_jan_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_feb_flx <- subset(early_decade_monthly_flx, month==2)
recent_feb_flx <- subset(recent_decade_monthly_flx, month==2)
Sacramento_Freeport_TKN_flux_feb_wilcox <- wilcox.test(recent_feb_flx$FluxDay, early_feb_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_mar_flx <- subset(early_decade_monthly_flx, month==3)
recent_mar_flx <- subset(recent_decade_monthly_flx, month==3)
Sacramento_Freeport_TKN_flux_mar_wilcox <- wilcox.test(recent_mar_flx$FluxDay, early_mar_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_apr_flx <- subset(early_decade_monthly_flx, month==4)
recent_apr_flx <- subset(recent_decade_monthly_flx, month==4)
Sacramento_Freeport_TKN_flux_apr_wilcox <- wilcox.test(recent_apr_flx$FluxDay, early_apr_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_may_flx <- subset(early_decade_monthly_flx, month==5)
recent_may_flx <- subset(recent_decade_monthly_flx, month==5)
Sacramento_Freeport_TKN_flux_may_wilcox <- wilcox.test(recent_may_flx$FluxDay, early_may_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_jun_flx <- subset(early_decade_monthly_flx, month==6)
recent_jun_flx <- subset(recent_decade_monthly_flx, month==6)
Sacramento_Freeport_TKN_flux_jun_wilcox <- wilcox.test(recent_jun_flx$FluxDay, early_jun_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_jul_flx <- subset(early_decade_monthly_flx, month==7)
recent_jul_flx <- subset(recent_decade_monthly_flx, month==7)
Sacramento_Freeport_TKN_flux_jul_wilcox <- wilcox.test(recent_jul_flx$FluxDay, early_jul_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_aug_flx <- subset(early_decade_monthly_flx, month==8)
recent_aug_flx <- subset(recent_decade_monthly_flx, month==8)
Sacramento_Freeport_TKN_flux_aug_wilcox <- wilcox.test(recent_aug_flx$FluxDay, early_aug_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_sep_flx <- subset(early_decade_monthly_flx, month==9)
recent_sep_flx <- subset(recent_decade_monthly_flx, month==9)
Sacramento_Freeport_TKN_flux_sep_wilcox <- wilcox.test(recent_sep_flx$FluxDay, early_sep_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_oct_flx <- subset(early_decade_monthly_flx, month==10)
recent_oct_flx <- subset(recent_decade_monthly_flx, month==10)
Sacramento_Freeport_TKN_flux_oct_wilcox <- wilcox.test(recent_oct_flx$FluxDay, early_oct_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_nov_flx <- subset(early_decade_monthly_flx, month==11)
recent_nov_flx <- subset(recent_decade_monthly_flx, month==11)
Sacramento_Freeport_TKN_flux_nov_wilcox <- wilcox.test(recent_nov_flx$FluxDay, early_nov_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_dec_flx <- subset(early_decade_monthly_flx, month==12)
recent_dec_flx <- subset(recent_decade_monthly_flx, month==12)
Sacramento_Freeport_TKN_flux_dec_wilcox <- wilcox.test(recent_dec_flx$FluxDay, early_dec_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)


Flux_compare <- data.frame(chng_est=c(Sacramento_Freeport_TKN_flux_oct_wilcox$est,
                                      Sacramento_Freeport_TKN_flux_nov_wilcox$est,
                                      Sacramento_Freeport_TKN_flux_dec_wilcox$est,
                                      Sacramento_Freeport_TKN_flux_jan_wilcox$est,
                                      Sacramento_Freeport_TKN_flux_feb_wilcox$est,
                                      Sacramento_Freeport_TKN_flux_mar_wilcox$est,
                                      Sacramento_Freeport_TKN_flux_apr_wilcox$est,
                                      Sacramento_Freeport_TKN_flux_may_wilcox$est,
                                      Sacramento_Freeport_TKN_flux_jun_wilcox$est,
                                      Sacramento_Freeport_TKN_flux_jul_wilcox$est,
                                      Sacramento_Freeport_TKN_flux_aug_wilcox$est,
                                      Sacramento_Freeport_TKN_flux_sep_wilcox$est),
                           low_conf=c(Sacramento_Freeport_TKN_flux_oct_wilcox$conf.int[1],
                                      Sacramento_Freeport_TKN_flux_nov_wilcox$conf.int[1],
                                      Sacramento_Freeport_TKN_flux_dec_wilcox$conf.int[1],
                                      Sacramento_Freeport_TKN_flux_jan_wilcox$conf.int[1],
                                      Sacramento_Freeport_TKN_flux_feb_wilcox$conf.int[1],
                                      Sacramento_Freeport_TKN_flux_mar_wilcox$conf.int[1],
                                      Sacramento_Freeport_TKN_flux_apr_wilcox$conf.int[1],
                                      Sacramento_Freeport_TKN_flux_may_wilcox$conf.int[1],
                                      Sacramento_Freeport_TKN_flux_jun_wilcox$conf.int[1],
                                      Sacramento_Freeport_TKN_flux_jul_wilcox$conf.int[1],
                                      Sacramento_Freeport_TKN_flux_aug_wilcox$conf.int[1],
                                      Sacramento_Freeport_TKN_flux_sep_wilcox$conf.int[1]),
                           up_conf=c(Sacramento_Freeport_TKN_flux_oct_wilcox$conf.int[2],
                                     Sacramento_Freeport_TKN_flux_nov_wilcox$conf.int[2],
                                     Sacramento_Freeport_TKN_flux_dec_wilcox$conf.int[2],
                                     Sacramento_Freeport_TKN_flux_jan_wilcox$conf.int[2],
                                     Sacramento_Freeport_TKN_flux_feb_wilcox$conf.int[2],
                                     Sacramento_Freeport_TKN_flux_mar_wilcox$conf.int[2],
                                     Sacramento_Freeport_TKN_flux_apr_wilcox$conf.int[2],
                                     Sacramento_Freeport_TKN_flux_may_wilcox$conf.int[2],
                                     Sacramento_Freeport_TKN_flux_jun_wilcox$conf.int[2],
                                     Sacramento_Freeport_TKN_flux_jul_wilcox$conf.int[2],
                                     Sacramento_Freeport_TKN_flux_aug_wilcox$conf.int[2],
                                     Sacramento_Freeport_TKN_flux_sep_wilcox$conf.int[2]))

write.table(Flux_compare, "Sacramento_Freeport_TKN_flux_wilcox.txt", quote=FALSE, row.names=FALSE)

rng_flx <- max(abs(c(Flux_compare$up_conf, Flux_compare$low_conf)))
tiff("Sacramento_Freeport_TKN_flux_shift_wilcox_Vert_Bars.tif", height=600, width=800, res=130)
par(mar=c(4,5,0.5,0.5))
plot(seq(1:12), Flux_compare$chng_est, typ='h', lend=1, lwd=15, col='white', xaxt='n', xlim=c(1,13), ylim=c(-rng_flx, rng_flx), xlab="Month", ylab=expression(paste("Median Flux Change, kg",sep='')), las=1)
plotCI(seq(1:12), Flux_compare$chng_est, ui=Flux_compare$up_conf, li=Flux_compare$low_conf, pch=16, add=TRUE)
abline(h=0)
axis(side=1,at=seq(1,12,by=1), labels=format(c(seq(as.Date("2000-10-01"), as.Date("2000-12-01"), by="month"), seq(as.Date("2000-01-01"), as.Date("2000-09-01"), by="month")),'%b'), las=2)
legend('topright', c("Median difference", "90% Confidence Interval for the Median"), pch=c(16,NA), lwd=c(NA,1), pt.cex=c(1,NA), pt.bg=c('black',NA), bty='n', bg='white')
dev.off()


# End of non-standard EGRET plot section requested by Michael
# --------------------------------------------------------------------------------------------------------

#################  Using the plotConcQSmooth function  ###########
#  First do flow duration analysis
flowDuration(eList, centerDate = "06-01", qUnit = 2, span = 30)
date1 <- "1975-06-01"
date2 <- "2000-06-01"
date3 <- "2018-06-01"
qLow= baseQ
qHigh=highQ7

tiff("BC1_Date_Discharge_TKN_conc_no_log.tif",height = 700, width = 1000, res=120)
plotConcQSmooth(eList,date1, date2, date3,qLow, qHigh, logScale=FALSE,printLegend =TRUE,legendLeft=0,legendTop=0,printTitle=TRUE)
dev.off()

flowDuration(eList, centerDate = "12-01", qUnit = 2, span = 30)
date1 <- "1975-12-01"
date2 <- "2000-12-01"
date3 <- "2018-12-01"
qLow= baseQ
qHigh=highQ7

tiff("BC1_Date_Discharge_TKN_conc_no_log_December.tif",height = 700, width = 1000, res=120)
plotConcQSmooth(eList,date1, date2, date3,qLow, qHigh, logScale=FALSE,printLegend =TRUE,legendLeft=0,legendTop=0,printTitle=TRUE)
dev.off()






# ---------------------------
# Now run the EGRETci package
# ---------------------------

# Change working directory
setwd("..")
subDir <- 'EGRETci_plots'
if (file.exists(subDir)){
  setwd(file.path(getwd(),subDir))
} else {
  dir.create(file.path(getwd(),subDir), recursive=TRUE)
  setwd(file.path(getwd(),subDir))
}

#Interactive function to set up trend analysis:
caseSetUp <- trendSetUp(eList, 
                        year1=1974, 
                        year2=2018, 
                        nBoot = 200, 
                        bootBreak = 50, 
                        blockLength = 200)
eBoot <- wBT(eList, caseSetUp, fileName ="Sacramento_Freeport_TKN_EGRETCIoutputText.txt")                                              


saveEGRETci(eList, eBoot, caseSetUp, fileName = "EGRETci_output")

plotHistogramTrend2 <-
  function (eBoot, caseSetUp, eList, xSeq = seq(-100, 100, 10), 
            flux = TRUE, printTitle = TRUE, cex.main = 1.1, col.fill = "grey", xlim = c(-100,100),
            ...) 
  {
    bootOut <- eBoot$bootOut
    INFO <- eList$INFO
    if (flux) {
      xFlux <- eBoot$xFlux
      change <- 100 * bootOut$estF/bootOut$baseFlux
      reps <- 100 * xFlux/bootOut$baseFlux
      xlabel <- "Flux trend, in %"
      titleWord <- "Flux"
    }
    else {
      xConc <- eBoot$xConc
      change <- 100 * bootOut$estC/bootOut$baseConc
      reps <- 100 * xConc/bootOut$baseConc
      xlabel <- "Concentration trend, in %"
      titleWord <- "Concentration"
    }
    titleToPrint <- ifelse(printTitle, paste("Histogram of trend in", 
                                             INFO$paramShortName, "\n", titleWord, "Normalized Concentration:", 
                                             caseSetUp$year1, "to", caseSetUp$year2, "\n", INFO$shortName), 
                           "")
    hist(reps, breaks = xSeq, yaxs = "i", xaxs = "i", tcl = 0.5, 
         main = titleToPrint, freq = FALSE, xlab = xlabel, col = col.fill, 
         cex.main = cex.main, xlim=xlim, ...)
    abline(v = change, lwd = 3, lty = 2)
    abline(v = 0, lwd = 3)
    box()
    axis(3, tcl = 0.5, labels = FALSE)
    axis(4, tcl = 0.5, labels = FALSE)
  }

tiff("histo_NO3_UpTruck_Trend_conc_flux.tif", height = 700, width = 1200, res=120)
par(mfrow=c(1,2))
plotHistogramTrend2(eBoot, caseSetUp, eList, flux=FALSE, xSeq = seq(-8000,8000,5),las=1,xlim=c(-100,100))
abline(h=0)

plotHistogramTrend2(eBoot, caseSetUp, eList, flux=TRUE, xSeq = seq(-50000,50000,5),las=1)
abline(h=0)
dev.off()

nBoot <- 200
blockLength <- 200
coreOut <- 4 #Number of cores to leave out of processing tasks

widthCI <- 95
ciLower <- (50-(widthCI/2))/100
ciUpper <- (50+(widthCI/2))/100
probs <- c(ciLower,ciUpper)

nCores <- detectCores() - coreOut
cl <- makeCluster(nCores)
registerDoParallel(cl)
repAnnual <- foreach(n = 1:nBoot,.packages=c('EGRETci')) %dopar% {
  annualResults <- bootAnnual(eList, blockLength,startSeed = n)  
}
stopCluster(cl)

CIAnnualResults <- ciBands(eList, repAnnual, probs)
conc.poly.x <- c(CIAnnualResults$Year,rev(CIAnnualResults$Year))
conc.poly.y <- c(CIAnnualResults$FNConcLow,rev(CIAnnualResults$FNConcHigh))
flux.poly.x <- c(CIAnnualResults$Year,rev(CIAnnualResults$Year))
flux.poly.y <- c(CIAnnualResults$FNFluxLow*365,rev(CIAnnualResults$FNFluxHigh*365))

tiff("Ann_Avg_Conc_&_Ann_Flow_Normalized_TKN_Conc_Boot.tif", height = 500, width = 600, res=110)
plotConcHistBoot(eList, CIAnnualResults, plotFlowNorm=TRUE, showYLabels=FALSE, showYAxis=FALSE,col=4)
polygon(x=conc.poly.x, y=conc.poly.y, col=rgb(24,116,205,40,max=255),border=NA)
dev.off()

tiff("Ann_Flux_&_Ann_Flow_Normalized_TKN_Flux_Boot.tif", height = 500, width = 600, res=110)
plotFluxHistBoot(eList, fluxUnit=4, CIAnnualResults, plotFluxNorm = TRUE, showYLabels=FALSE, showYAxis=TRUE, col=4)
polygon(x=flux.poly.x, y=flux.poly.y, col=rgb(24,116,205,40,max=255),border=NA)
dev.off()
setSweave("Sacramento_Freeport_TKN_Conc_CI",7,7)
plotConcHistBoot(eList, CIAnnualResults, plotFlowNorm=TRUE, showYLabels=TRUE, showYAxis=TRUE,col=4)
graphics.off()
setSweave("Sacramento_Freeport_TKN_flux_CI",7,7)
plotFluxHistBoot(eList, fluxUnit=4, CIAnnualResults, plotFluxNorm = TRUE, showYLabels=TRUE, showYAxis=TRUE, col=4)
graphics.off()
saveEGRETci(eList, eBoot, fileName="N_Boot")
#save(repAnnual,file="RepAnnual")
write.csv(repAnnual,'repAnnual.csv')
# load(file="N_Boot.RData")
# load(file="RepAnnual")


#############################################################
# Working on Total Phosphorus
#############################################################

startDate    <- "1970-10-01"
endDate      <- "2019-03-30"
siteNumber   <- "11447650"
QParameterCd <- "00060"
parameterCd  <- "00665"  # "TP"
##Get TP data from NWIS
Sample <- readNWISSample(siteNumber, parameterCd, startDate, endDate)
#Get Discharge from CSV file

fileName     <- "SacFreeportDischarge_TP.csv"
Daily <-readUserDaily(filePath, fileName, hasHeader = TRUE, separator = ",", qUnit = 1)



Sample       <- removeDuplicates(Sample)
INFO         <- readNWISInfo(siteNumber = siteNumber, parameterCd = parameterCd, interactive=FALSE)
INFO$staAbbrev <- paste(strsplit(INFO$station_nm," ")[[1]][1],strsplit(INFO$station_nm," ")[[1]][2])


# Have a look at the available range of TP data
range(Sample$Date)
# "1970-10-07" "2019-03-13"
eList <- mergeReport(INFO, Daily, Sample)

# Change the working directory; redirect plot output to NO3 folder
setwd("../..")
subDir <- 'TP/EGRET_plots'
if (file.exists(subDir)){
  setwd(file.path(getwd(),subDir))
} else {
  dir.create(file.path(getwd(),subDir), recursive=TRUE)
  setwd(file.path(getwd(),subDir))
}
write.csv(Sample,"Sample_TP.csv")
# Plot water quality data
tiff("Conc_vs_Time_Sacramento_Freeport_TP.tif", height = 600, width = 800, res=120)
plotConcTime(eList)
dev.off()

# Now, a classic Q-C plot
tiff("Conc-Q_Sacramento_Freeport_TP.tif", height = 600, width = 800, res=120)
plotConcQ(eList, logScale=TRUE)
dev.off()

# The data set as flux values rather than as concentrations
tiff("Flux-Q_Sacramento_Freeport_TP.tif", height = 600, width = 800, res=120)
plotFluxQ(eList, fluxUnit=4)
dev.off()

# Monthly boxplots
tiff("Monthly-Conc_BoxPlots_Sacramento_Freeport_TP.tif", height = 600, width = 800, res=120)
boxConcMonth(eList, logScale=TRUE)
dev.off()

# Flow on days sampled vs. all other days
tiff("Flow_on_days_sampled_vs_all_other_days_Sacramento_Freeport_TP.tif", height = 600, width = 800, res=120)
boxQTwice(eList, qUnit=1)
dev.off()

#########################################
# Now start the Flow-Normalized Analysis
#########################################

# Build the regression model
eList <- modelEstimation(eList, windowY = 7, windowQ = 2, windowS = 0.5, minNumObs = 100, minNumUncen =50)
eList_TP <- eList

MonthlyResults <- calculateMonthlyResults(eList_TP)

# Dump TP-related flow-normalized data to text file for bringing together with other monitoring sites
paLong <- 12
paStart <- 10
localDaily <- getDaily(eList_TP)
localAnnualResults <- setupYears(paStart = paStart, paLong = paLong, localDaily = localDaily)
write.table(localAnnualResults, file = 'SacFreeportTP_RawVals.txt', quote=FALSE, row.names=FALSE)
write.csv(localDaily,'localDailyTP.csv')


# Plot the annual average concentration and annual flow-normalized concentration
tiff("Ann_Avg_Conc_&_Ann_Flow_Normalized_Conc_TP.tif", height = 600, width = 800, res=120)
plotConcHist(eList, plotFlowNorm=TRUE)
dev.off()

# Plot the annual flux and annual flow-normalized flux
tiff("Ann_Flux_&_Ann_Flow_Normalized_Flux_Sacramento_Freeport_TP.tif", height = 600, width = 800, res=120)
plotFluxHist(eList, plotFlowNorm = TRUE) # fluxMax) # fluxMax
dev.off()

# Look for a trend change:
tableChange(eList, fluxUnit=6, yearPoints=c(1980, 1990,2017))



#Generate out-of-the-box diagnostic plots
tiff("fluxBiasMulti_Sacramento_FreeportTP.tif", height = 1200, width = 1000, res=120)
fluxBiasMulti(eList, moreTitle = "WRTDS")
dev.off()

tiff("Modeled_Daily_Conc_wObservations_Sacramento_FreeportTP.tif", height = 800, width = 1000, res=120)
plotConcTimeDaily(eList)
dev.off()

# Exploring model behavior and adjusting model parameters
tiff("Contours_Sacramento_Freeport.tif", height = 700, width = 1000, res=120)
plotContours(eList, qBottom=100,qTop=2500,yearStart=1970,yearEnd=2019, contourLevels=c(0.00,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.10,0.11,0.12,0.13,0.14,0.15,0.16,0.17,0.18,0.19,0.20,0.21), color.palette = colorRampPalette(c("violet", "purple", "blue", "cyan", "green", "yellow", "orange", "red"))) 
dev.off()

tiff("Log_Contours_Sacramento_Freeport_TP.tif", height = 700, width = 1000, res=120)
plotContours(eList, qBottom=100, qTop=2500, yearStart=1970, yearEnd=2019, contourLevels=c(-4.750,-4.625,-4.500,-4.375,-4.250,-4.125,-4.000,-3.875,-3.750,-3.625,-3.500,-3.375,-3.250,-3.125,-3.000,-2.875,-2.750,-2.625,-2.500,-2.375,-2.250,-2.125,-2.000,-1.875,-1.750,-1.625,-1.500), color.palette = colorRampPalette(c("violet", "purple", "blue", "cyan", "green", "yellow", "orange", "red")), whatSurface=1) 
dev.off()

tiff("StdErr_of_Log_Contours_Sacramento_Freeport_TP.tif", height = 700, width = 1000, res=120)
plotContours(eList, qBottom=100, qTop=2500, yearStart=1970, yearEnd=2019, contourLevels=c(0.250,0.275,0.300,0.325,0.350,0.375,0.400,0.425,0.450,0.475,0.500,0.525,0.550,0.575,0.600,0.625,0.650,0.675,0.700,0.725,0.750,0.775,0.800,0.825,0.850,0.875,0.900,0.925,0.950), color.palette = colorRampPalette(c("violet", "purple", "blue", "cyan", "green", "yellow", "orange", "red")), whatSurface=2) 
dev.off()

tiff("Contours_Difference_TP_Sacramento_Freeport.tif", height = 700, width = 1000, res=120)
plotDiffContours(eList,1970, 2019,100, 2500,maxDiff=0.1)
dev.off()

tiff("Contours_PercentDifference_TP_Sacramento_Freeport.tif", height = 700, width = 1000, res=120)
plotDiffContours(eList,1970, 2019,100,2500, maxDiff=100, plotPercent=TRUE)
dev.off()

tiff("Contours_PercentDifference2_TP.tif", height = 700, width = 1000, res=120)
plotDiffContours2(eList,1970,2019,100,2500, maxDiff=c(-100,100), plotPercent=TRUE, lwd=3, color.palette=colorRampPalette(c("purple","blue","lightblue","white","beige","yellow", "orange", "red","brown")),tick.lwd = 1)
dev.off()

Sample$WY <- trunc(Sample$DecYear+0.25) 
tiff("Monthly_Boxplot_Inorg_TP.tif", height = 700, width = 1000, res=120)
par(mar=c(4,6,0.5,0.5))
boxplot(Sample$ConcAve~Sample$WY,log="y",varwidth=TRUE,ylim=c(0.01,2),yaxs="i",xlab="Water Year",las=1) 
mtext(side=2, expression(paste("Concentration, Inorganic Nitrogen, in mg  ",L^-1,sep="")),line=4)
dev.off()




##############################
# Determine which flow rates to use for discharge-specific trends

# Baseflow: mean of the annual 30-day low flows
baseQ <- mean(aggregate(Q30 ~ waterYear, data = localDaily, min)[,2])
baseQ_txt <- format(baseQ, digits=2)
baseQ_txt_cfs <- format(baseQ * 35.315, digits=2)

# mid-range: median flow rate across all years
medQ <- median(localDaily$Q)
medQ_txt <- format(medQ, digits=2)
medQ_txt_cfs <- format(medQ * 35.315, digits=2)

# high flow: get the 25% quantile of each year's maximum Q7
# This will help ensure (but not guarantee) that every year is well represented in the high-end flows
highQ7 <- as.numeric(quantile(aggregate(Q7 ~ waterYear, data = localDaily, max)[,2])[2])
highQ7_txt <- format(highQ7, digits=2)
highQ7_txt_cfs <- format(highQ7 * 35.315, digits=2)

# The following bit of script generates a figure discussed by Joe in an email on 6/2/17
# -------------------------------------------------------------------------------------

tiff("Discharge_specific_trends_TP_centered_on_06-01.tif", height = 600, width = 1200, res=120)
par(mar=c(4,6,4.1,8))
plotConcTimeSmooth(eList, q1 = baseQ, q2 = medQ, q3 = highQ7, centerDate='06-01', 
                   yearStart=localDaily$waterYear[1], yearEnd=localDaily$waterYear[nrow(localDaily)], 
                   logScale=FALSE, printLegend=FALSE)

# Determine y position of the legend
# ----------------------------------
y_l <- par('usr')[3]
y_u <- par('usr')[4]
y_m <- mean(y_l, y_u)

# Use the top version of the legend to add cfs

# legend('bottomleft', c(eval(substitute(expression(paste('Baseflow [',baseQ_txt,' ', m^3~s^-1,'(',baseQ_txt_cfs,' ',ft^3~s^-1,')]',sep=' ')), list(baseQ_txt=baseQ_txt, baseQ_txt_cfs=baseQ_txt_cfs))),
#                        eval(substitute(expression(paste('Median Flow [',medQ_txt,' ', m^3~s^-1,'(',medQ_txt_cfs,' ',ft^3~s^-1,')]',sep=' ')), list(medQ_txt=medQ_txt, medQ_txt_cfs=medQ_txt_cfs))),
#                        eval(substitute(expression(paste('High Flow [',highQ7_txt,' ', m^3~s^-1,'(',highQ7_txt_cfs,' ',ft^3~s^-1,')]',sep=' ')), list(highQ7_txt=highQ7_txt, highQ7_txt_cfs=highQ7_txt_cfs)))), 
#                        col=c('black','red','green'), lwd=2, bg='white', bty='n')

legend('bottomleft', c(eval(substitute(expression(paste('Baseflow (',baseQ_txt,' ', m^3~s^-1,')',sep=' ')), list(baseQ_txt=baseQ_txt))),
                       eval(substitute(expression(paste('Median Flow (',medQ_txt,' ', m^3~s^-1,')',sep=' ')), list(medQ_txt=medQ_txt))),
                       eval(substitute(expression(paste('High Flow (',highQ7_txt,' ', m^3~s^-1,')',sep=' ')), list(highQ7_txt=highQ7_txt)))), 
       col=c('black','red','green'), lwd=2, bg='white', bty='n')

dev.off()

# Restore original plotting margins
par(mar=c(5.1,6.1,4.1,2.1))

# The following bit of script generates a figure discussed by Michael in an email on 6/2/17
# -----------------------------------------------------------------------------------------
# Get the row number corresponding to the maximum concentration for each year (bearing in mind that these are 'within group' row numbers)
out <- aggregate(ConcDay ~ waterYear, data = localDaily, which.max)

# Ensure data is ordered by water year
out <- out[ order(out$waterYear), ]

# Get a count of the number of days within each of the water years
tbl <- table(localDaily$waterYear)

# Return the absolute row positions for each water year's max concentration
out$AbsConcDay <- out$ConcDay + cumsum(c(0,tbl[-length(tbl)]))

# Make a data.frame containing only the rows with each water year's max conc
out2 <- localDaily[out$AbsConcDay,]

# Gather only the needed data
out2 <- data.frame(Date=out2$Date, Q=out2$Q, Conc=out2$ConcDay, wyr=out2$waterYear, Julian=yday(as.Date(out2$Date)))

# Need to readjust the Julian day to start on Oct 1 (This function doesn't yet account for leap years)
out2$JulianWYR <- ifelse(out2$Julian > 273, out2$Julian - 273, 92 + out2$Julian)

# Plot it
tiff("JulianDay_of_Max_TP_Conc.tif", height = 600, width = 800, res=120)
plot(out2$wyr, out2$JulianWYR, pch=16, xlab='Water Year', ylab='Julian Day', yaxs='i', ylim=c(0,370), las=1)
dev.off()

# In a follow-up email from Michael on 6/7/17, Michael suggested two alterations:
#  1) Apply a 30-day moving average
#  2) Use the flow-normalized concentration

# To start with, I'll attempt to apply a 30-day window to the simulated daily concentrations

localDaily$ConcDay_30day <- c(rep(rollapply(localDaily$ConcDay, width=30, mean)[1],times=14) , rollapply(localDaily$ConcDay, width=30, mean), rep(rollapply(localDaily$ConcDay, width=30, mean)[length(rollapply(localDaily$ConcDay, width=30, mean))], times=15))
out_m <- aggregate(ConcDay_30day ~ waterYear, data = localDaily, which.max)
out_m <- out_m[ order(out_m$waterYear), ]
out_m$AbsConcDay <- out_m$ConcDay + cumsum(c(0,tbl[-length(tbl)]))
out_m2 <- localDaily[out_m$AbsConcDay,]
out_m2 <- data.frame(Date=out_m2$Date, Q=out_m2$Q, Conc30=out_m2$ConcDay_30day, wyr=out_m2$waterYear, Julian=yday(as.Date(out_m2$Date)))
out_m2$JulianWYR <- ifelse(out_m2$Julian > 273, out_m2$Julian - 273, 92 + out_m2$Julian)

tiff("JulianDay_of_Max_TP_Conc_Using_30_rollingAvg.tif", height = 600, width = 800, res=120)
plot(out_m2$wyr, out_m2$JulianWYR, pch=16, xlab='Water Year', ylab='Julian Day', yaxs='i', ylim=c(0,370), las=1)
dev.off()


# Next, I'll try using the flow-normalized concentration (same general code flow as above)
# First, try plotting flow-normalized concentration:
# Plot it
tiff("Flow_Normalized_Conc_BC1_TP.tif", height = 600, width = 800, res=120)
plot(as.Date(localDaily$Date), localDaily$FNConc, typ='l', las=1, xlab='Time', ylab='Flow-normalized Concentration')
dev.off()

out_FN <- aggregate(FNConc ~ waterYear, data = localDaily, which.max)
out_FN <- out_FN[ order(out_FN$waterYear), ]
out_FN$AbsConcDay <- out_FN$FNConc + cumsum(c(0,tbl[-length(tbl)]))
out2_FN <- localDaily[out_FN$AbsConcDay,]
out2_FN <- data.frame(Date=out2_FN$Date, Q=out2_FN$Q, Conc=out2_FN$FNConc, wyr=out2_FN$waterYear, Julian=yday(as.Date(out2_FN$Date)))
out2_FN$JulianWYR <- ifelse(out2_FN$Julian > 273, out2_FN$Julian - 273, 92 + out2_FN$Julian)

# Plot it
tiff("JulianDay_of_Max_TP_Flow_Normalized_Conc.tif", height = 600, width = 800, res=120)
plot(out2_FN$wyr, out2_FN$JulianWYR, pch=16, xlab='Water Year', ylab='Julian Day', yaxs='i', ylim=c(0,370), las=1)
dev.off()



# --------------------------------------------------------------------------------------------------------
# The following script is for a non-standard EGRET plot and instead help generate a plot Michael requested

localDaily <- getDaily(eList)

# Will need to adjust the date range below based on each gages unique start/stop dates
early_decade <- subset(localDaily, localDaily$Date > as.Date('1970-10-01') & localDaily$Date < as.Date('1980-09-30'))
recent_decade <- subset(localDaily, localDaily$Date > as.Date('2009-04-30'))


early_decade_monthly_mn <- aggregate(ConcDay ~ MonthSeq, data = early_decade, 'mean')
recent_decade_monthly_mn <- aggregate(ConcDay ~ MonthSeq, data = recent_decade, 'mean')

# early_decade_monthly_mn$month <- format(seq(as.Date('1972-10-01'), as.Date('1982-09-30'), by='month'), '%b')
early_decade_monthly_mn$month <- rep(c(10:12,1:9), times=10)
early_decade_mon_mn <- aggregate(ConcDay ~ month, data = early_decade_monthly_mn, 'mean')
early_decade_mon_sd <- aggregate(ConcDay ~ month, data = early_decade_monthly_mn, 'sd')
early_decade_mon_mn <- early_decade_mon_mn[c(10:12,1:9),]
early_decade_mon_sd <- early_decade_mon_sd[c(10:12,1:9),]

recent_decade_monthly_mn$month <- rep(c(10:12,1:9), times=10)
recent_decade_mon_mn <- aggregate(ConcDay ~ month, data = recent_decade_monthly_mn, 'mean')
recent_decade_mon_sd <- aggregate(ConcDay ~ month, data = recent_decade_monthly_mn, 'sd')
recent_decade_mon_mn <- recent_decade_mon_mn[c(10:12,1:9),]
recent_decade_mon_sd <- recent_decade_mon_sd[c(10:12,1:9),]


mdat2 <- matrix(c(early_decade_mon_mn$ConcDay, recent_decade_mon_mn$ConcDay),
                nrow=2,ncol = 12, byrow=TRUE,
                dimnames = list(c("1970-1980", "2009-2019"),
                                c(format(seq(as.Date('1973-10-01'), as.Date('1974-09-01'), by='month'), '%b'))))

# Be sure to adjust the legend's first decade start and stop year correctly
mx <- max(c((early_decade_mon_mn$ConcDay + early_decade_mon_sd$ConcDay), (recent_decade_mon_mn$ConcDay + recent_decade_mon_sd$ConcDay)))

tiff("timing_shift_in_TP_conc_monthly_means.tif", height=800, width=900, res=130)
par(mar=c(3,5,2,1))
x <- barplot(mdat2, beside=TRUE, las=1, ylim=c(0,mx), col = c("lightblue", "mistyrose"))
abline(h=0)
arrows(x0=x[1,], y0=early_decade_mon_mn$ConcDay - early_decade_mon_sd$ConcDay, x1=x[1,], y1=early_decade_mon_mn$ConcDay + early_decade_mon_sd$ConcDay, angle=90, length=0.04, code=3)
arrows(x0=x[2,], y0=recent_decade_mon_mn$ConcDay - recent_decade_mon_sd$ConcDay, x1=x[2,], y1=recent_decade_mon_mn$ConcDay + recent_decade_mon_sd$ConcDay, angle=90, length=0.04, code=3)
mtext(side=2, expression(paste(TP,', mg ',L^-1,sep='')), line=3)
legend(x=25, y=0.9 * mx, c("1970-1980", "2009-2019"), pch=c(22,22), pt.cex=2, pt.bg=c("lightblue", "mistyrose"), bty='n', xpd=TRUE)
dev.off()


# Now attempting a Wilcox Test (aka Mann-Whitney-Wilcoxon Rank Sum test)
# ----------------------------------------------------------------------
early_jan <- subset(early_decade_monthly_mn, month==1)
recent_jan <- subset(recent_decade_monthly_mn, month==1)
Sacramento_Freeport_TP_conc_jan_wilcox <- wilcox.test(recent_jan$ConcDay, early_jan$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_feb <- subset(early_decade_monthly_mn, month==2)
recent_feb <- subset(recent_decade_monthly_mn, month==2)
Sacramento_Freeport_TP_conc_feb_wilcox <- wilcox.test(recent_feb$ConcDay, early_feb$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_mar <- subset(early_decade_monthly_mn, month==3)
recent_mar <- subset(recent_decade_monthly_mn, month==3)
Sacramento_Freeport_TP_conc_mar_wilcox <- wilcox.test(recent_mar$ConcDay, early_mar$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_apr <- subset(early_decade_monthly_mn, month==4)
recent_apr <- subset(recent_decade_monthly_mn, month==4)
Sacramento_Freeport_TP_conc_apr_wilcox <- wilcox.test(recent_apr$ConcDay, early_apr$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_may <- subset(early_decade_monthly_mn, month==5)
recent_may <- subset(recent_decade_monthly_mn, month==5)
Sacramento_Freeport_TP_conc_may_wilcox <- wilcox.test(recent_may$ConcDay, early_may$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_jun <- subset(early_decade_monthly_mn, month==6)
recent_jun <- subset(recent_decade_monthly_mn, month==6)
Sacramento_Freeport_TP_conc_jun_wilcox <- wilcox.test(recent_jun$ConcDay, early_jun$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_jul <- subset(early_decade_monthly_mn, month==7)
recent_jul <- subset(recent_decade_monthly_mn, month==7)
Sacramento_Freeport_TP_conc_jul_wilcox <- wilcox.test(recent_jul$ConcDay, early_jul$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_aug <- subset(early_decade_monthly_mn, month==8)
recent_aug <- subset(recent_decade_monthly_mn, month==8)
Sacramento_Freeport_TP_conc_aug_wilcox <- wilcox.test(recent_aug$ConcDay, early_aug$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_sep <- subset(early_decade_monthly_mn, month==9)
recent_sep <- subset(recent_decade_monthly_mn, month==9)
Sacramento_Freeport_TP_conc_sep_wilcox <- wilcox.test(recent_sep$ConcDay, early_sep$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_oct <- subset(early_decade_monthly_mn, month==10)
recent_oct <- subset(recent_decade_monthly_mn, month==10)
Sacramento_Freeport_TP_conc_oct_wilcox <- wilcox.test(recent_oct$ConcDay, early_oct$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_nov <- subset(early_decade_monthly_mn, month==11)
recent_nov <- subset(recent_decade_monthly_mn, month==11)
Sacramento_Freeport_TP_conc_nov_wilcox <- wilcox.test(recent_nov$ConcDay, early_nov$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_dec <- subset(early_decade_monthly_mn, month==12)
recent_dec <- subset(recent_decade_monthly_mn, month==12)
Sacramento_Freeport_TP_conc_dec_wilcox <- wilcox.test(recent_dec$ConcDay, early_dec$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

Conc_compare <- data.frame(chng_est=c(Sacramento_Freeport_TP_conc_oct_wilcox$est,
                                      Sacramento_Freeport_TP_conc_nov_wilcox$est,
                                      Sacramento_Freeport_TP_conc_dec_wilcox$est,
                                      Sacramento_Freeport_TP_conc_jan_wilcox$est,
                                      Sacramento_Freeport_TP_conc_feb_wilcox$est,
                                      Sacramento_Freeport_TP_conc_mar_wilcox$est,
                                      Sacramento_Freeport_TP_conc_apr_wilcox$est,
                                      Sacramento_Freeport_TP_conc_may_wilcox$est,
                                      Sacramento_Freeport_TP_conc_jun_wilcox$est,
                                      Sacramento_Freeport_TP_conc_jul_wilcox$est,
                                      Sacramento_Freeport_TP_conc_aug_wilcox$est,
                                      Sacramento_Freeport_TP_conc_sep_wilcox$est),
                           low_conf=c(Sacramento_Freeport_TP_conc_oct_wilcox$conf.int[1],
                                      Sacramento_Freeport_TP_conc_nov_wilcox$conf.int[1],
                                      Sacramento_Freeport_TP_conc_dec_wilcox$conf.int[1],
                                      Sacramento_Freeport_TP_conc_jan_wilcox$conf.int[1],
                                      Sacramento_Freeport_TP_conc_feb_wilcox$conf.int[1],
                                      Sacramento_Freeport_TP_conc_mar_wilcox$conf.int[1],
                                      Sacramento_Freeport_TP_conc_apr_wilcox$conf.int[1],
                                      Sacramento_Freeport_TP_conc_may_wilcox$conf.int[1],
                                      Sacramento_Freeport_TP_conc_jun_wilcox$conf.int[1],
                                      Sacramento_Freeport_TP_conc_jul_wilcox$conf.int[1],
                                      Sacramento_Freeport_TP_conc_aug_wilcox$conf.int[1],
                                      Sacramento_Freeport_TP_conc_sep_wilcox$conf.int[1]),
                           up_conf=c(Sacramento_Freeport_TP_conc_oct_wilcox$conf.int[2],
                                     Sacramento_Freeport_TP_conc_nov_wilcox$conf.int[2],
                                     Sacramento_Freeport_TP_conc_dec_wilcox$conf.int[2],
                                     Sacramento_Freeport_TP_conc_jan_wilcox$conf.int[2],
                                     Sacramento_Freeport_TP_conc_feb_wilcox$conf.int[2],
                                     Sacramento_Freeport_TP_conc_mar_wilcox$conf.int[2],
                                     Sacramento_Freeport_TP_conc_apr_wilcox$conf.int[2],
                                     Sacramento_Freeport_TP_conc_may_wilcox$conf.int[2],
                                     Sacramento_Freeport_TP_conc_jun_wilcox$conf.int[2],
                                     Sacramento_Freeport_TP_conc_jul_wilcox$conf.int[2],
                                     Sacramento_Freeport_TP_conc_aug_wilcox$conf.int[2],
                                     Sacramento_Freeport_TP_conc_sep_wilcox$conf.int[2]))

write.table(Conc_compare, "Sacramento_Freeport_TP_conc_wilcox.txt", quote=FALSE, row.names=FALSE)

rng <- max(abs(c(Conc_compare$up_conf, Conc_compare$low_conf)))
tiff("Sacramento_Freeport_TP_conc_shift_wilcox_Vert_Bars.tif", height=600, width=800, res=130)
par(mar=c(4,5,0.5,0.5))
plot(seq(1:12), Conc_compare$chng_est, typ='h', lend=1, lwd=15, col='white', xaxt='n', xlim=c(1,13), ylim=c(-rng, rng), xlab="Month", ylab=expression(paste("Median Concentration Change, mg  ",L^-1,sep='')), las=1)
plotCI(seq(1:12), Conc_compare$chng_est, ui=Conc_compare$up_conf, li=Conc_compare$low_conf, pch=16, add=TRUE)
abline(h=0)
axis(side=1,at=seq(1,12,by=1), labels=format(c(seq(as.Date("2000-10-01"), as.Date("2000-12-01"), by="month"), seq(as.Date("2000-01-01"), as.Date("2000-09-01"), by="month")),'%b'), las=2)
legend('topright', c("Median difference", "90% Confidence Interval for the Median"), pch=c(16,NA), lwd=c(NA,1), pt.cex=c(1,NA), pt.bg=c('black',NA), bty='n', bg='white')
dev.off()


# Now do the load
# ---------------
early_decade_monthly_flx <- aggregate(FluxDay ~ MonthSeq, data = early_decade, 'sum')
recent_decade_monthly_flx <- aggregate(FluxDay ~ MonthSeq, data = recent_decade, 'sum')

# early_decade_monthly_mn$month <- format(seq(as.Date('1972-10-01'), as.Date('1982-09-30'), by='month'), '%b')
early_decade_monthly_flx$month <- rep(c(10:12,1:9), times=10)
early_decade_mon_mn_flx <- aggregate(FluxDay ~ month, data = early_decade_monthly_flx, 'mean')
early_decade_mon_sd_flx <- aggregate(FluxDay ~ month, data = early_decade_monthly_flx, 'sd')
early_decade_mon_mn_flx <- early_decade_mon_mn_flx[c(10:12,1:9),]
early_decade_mon_sd_flx <- early_decade_mon_sd_flx[c(10:12,1:9),]

recent_decade_monthly_flx$month <- rep(c(10:12,1:9), times=10)
recent_decade_mon_mn_flx <- aggregate(FluxDay ~ month, data = recent_decade_monthly_flx, 'mean')
recent_decade_mon_sd_flx <- aggregate(FluxDay ~ month, data = recent_decade_monthly_flx, 'sd')
recent_decade_mon_mn_flx <- recent_decade_mon_mn_flx[c(10:12,1:9),]
recent_decade_mon_sd_flx <- recent_decade_mon_sd_flx[c(10:12,1:9),]

mdat3 <- matrix(c(early_decade_mon_mn_flx$FluxDay, recent_decade_mon_mn_flx$FluxDay),
                nrow=2,ncol = 12, byrow=TRUE,
                dimnames = list(c("1970-1980", "2009-2019"),
                                c(format(seq(as.Date('1973-10-01'), as.Date('1974-09-01'), by='month'), '%b'))))

mx <- max(c((early_decade_mon_mn_flx$FluxDay + early_decade_mon_sd_flx$FluxDay), (recent_decade_mon_mn_flx$FluxDay + recent_decade_mon_sd_flx$FluxDay)))
tiff("timing_shift_in_TP_load_monthly_means.tif", height=800, width=900, res=130)
x <- barplot(mdat3, beside=TRUE, las=1, ylim=c(0,mx), col = c("lightblue", "mistyrose"))
abline(h=0)
arrows(x0=x[1,], y0=early_decade_mon_mn_flx$FluxDay - early_decade_mon_sd_flx$FluxDay, x1=x[1,], y1=early_decade_mon_mn_flx$FluxDay + early_decade_mon_sd_flx$FluxDay, angle=90, length=0.04, code=3)
arrows(x0=x[2,], y0=recent_decade_mon_mn_flx$FluxDay - recent_decade_mon_sd_flx$FluxDay, x1=x[2,], y1=recent_decade_mon_mn_flx$FluxDay + recent_decade_mon_sd_flx$FluxDay, angle=90, length=0.04, code=3)
mtext(side=2, expression(paste(TP,', kg ',month^-1,sep='')), line=2.5)
legend(x=30, y=0.9 * mx, c("1970-1980", "2009-2019"), pch=c(22,22), pt.cex=2, pt.bg=c("lightblue", "mistyrose"), bty='n', xpd=TRUE)
dev.off()

# Apply Wilcox.text to the monthly loads here...
early_jan_flx <- subset(early_decade_monthly_flx, month==1)
recent_jan_flx <- subset(recent_decade_monthly_flx, month==1)
Sacramento_Freeport_TP_flux_jan_wilcox <- wilcox.test(recent_jan_flx$FluxDay, early_jan_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_feb_flx <- subset(early_decade_monthly_flx, month==2)
recent_feb_flx <- subset(recent_decade_monthly_flx, month==2)
Sacramento_Freeport_TP_flux_feb_wilcox <- wilcox.test(recent_feb_flx$FluxDay, early_feb_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_mar_flx <- subset(early_decade_monthly_flx, month==3)
recent_mar_flx <- subset(recent_decade_monthly_flx, month==3)
Sacramento_Freeport_TP_flux_mar_wilcox <- wilcox.test(recent_mar_flx$FluxDay, early_mar_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_apr_flx <- subset(early_decade_monthly_flx, month==4)
recent_apr_flx <- subset(recent_decade_monthly_flx, month==4)
Sacramento_Freeport_TP_flux_apr_wilcox <- wilcox.test(recent_apr_flx$FluxDay, early_apr_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_may_flx <- subset(early_decade_monthly_flx, month==5)
recent_may_flx <- subset(recent_decade_monthly_flx, month==5)
Sacramento_Freeport_TP_flux_may_wilcox <- wilcox.test(recent_may_flx$FluxDay, early_may_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_jun_flx <- subset(early_decade_monthly_flx, month==6)
recent_jun_flx <- subset(recent_decade_monthly_flx, month==6)
Sacramento_Freeport_TP_flux_jun_wilcox <- wilcox.test(recent_jun_flx$FluxDay, early_jun_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_jul_flx <- subset(early_decade_monthly_flx, month==7)
recent_jul_flx <- subset(recent_decade_monthly_flx, month==7)
Sacramento_Freeport_TP_flux_jul_wilcox <- wilcox.test(recent_jul_flx$FluxDay, early_jul_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_aug_flx <- subset(early_decade_monthly_flx, month==8)
recent_aug_flx <- subset(recent_decade_monthly_flx, month==8)
Sacramento_Freeport_TP_flux_aug_wilcox <- wilcox.test(recent_aug_flx$FluxDay, early_aug_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_sep_flx <- subset(early_decade_monthly_flx, month==9)
recent_sep_flx <- subset(recent_decade_monthly_flx, month==9)
Sacramento_Freeport_TP_flux_sep_wilcox <- wilcox.test(recent_sep_flx$FluxDay, early_sep_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_oct_flx <- subset(early_decade_monthly_flx, month==10)
recent_oct_flx <- subset(recent_decade_monthly_flx, month==10)
Sacramento_Freeport_TP_flux_oct_wilcox <- wilcox.test(recent_oct_flx$FluxDay, early_oct_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_nov_flx <- subset(early_decade_monthly_flx, month==11)
recent_nov_flx <- subset(recent_decade_monthly_flx, month==11)
Sacramento_Freeport_TP_flux_nov_wilcox <- wilcox.test(recent_nov_flx$FluxDay, early_nov_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_dec_flx <- subset(early_decade_monthly_flx, month==12)
recent_dec_flx <- subset(recent_decade_monthly_flx, month==12)
Sacramento_Freeport_TP_flux_dec_wilcox <- wilcox.test(recent_dec_flx$FluxDay, early_dec_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)


Flux_compare <- data.frame(chng_est=c(Sacramento_Freeport_TP_flux_oct_wilcox$est,
                                      Sacramento_Freeport_TP_flux_nov_wilcox$est,
                                      Sacramento_Freeport_TP_flux_dec_wilcox$est,
                                      Sacramento_Freeport_TP_flux_jan_wilcox$est,
                                      Sacramento_Freeport_TP_flux_feb_wilcox$est,
                                      Sacramento_Freeport_TP_flux_mar_wilcox$est,
                                      Sacramento_Freeport_TP_flux_apr_wilcox$est,
                                      Sacramento_Freeport_TP_flux_may_wilcox$est,
                                      Sacramento_Freeport_TP_flux_jun_wilcox$est,
                                      Sacramento_Freeport_TP_flux_jul_wilcox$est,
                                      Sacramento_Freeport_TP_flux_aug_wilcox$est,
                                      Sacramento_Freeport_TP_flux_sep_wilcox$est),
                           low_conf=c(Sacramento_Freeport_TP_flux_oct_wilcox$conf.int[1],
                                      Sacramento_Freeport_TP_flux_nov_wilcox$conf.int[1],
                                      Sacramento_Freeport_TP_flux_dec_wilcox$conf.int[1],
                                      Sacramento_Freeport_TP_flux_jan_wilcox$conf.int[1],
                                      Sacramento_Freeport_TP_flux_feb_wilcox$conf.int[1],
                                      Sacramento_Freeport_TP_flux_mar_wilcox$conf.int[1],
                                      Sacramento_Freeport_TP_flux_apr_wilcox$conf.int[1],
                                      Sacramento_Freeport_TP_flux_may_wilcox$conf.int[1],
                                      Sacramento_Freeport_TP_flux_jun_wilcox$conf.int[1],
                                      Sacramento_Freeport_TP_flux_jul_wilcox$conf.int[1],
                                      Sacramento_Freeport_TP_flux_aug_wilcox$conf.int[1],
                                      Sacramento_Freeport_TP_flux_sep_wilcox$conf.int[1]),
                           up_conf=c(Sacramento_Freeport_TP_flux_oct_wilcox$conf.int[2],
                                     Sacramento_Freeport_TP_flux_nov_wilcox$conf.int[2],
                                     Sacramento_Freeport_TP_flux_dec_wilcox$conf.int[2],
                                     Sacramento_Freeport_TP_flux_jan_wilcox$conf.int[2],
                                     Sacramento_Freeport_TP_flux_feb_wilcox$conf.int[2],
                                     Sacramento_Freeport_TP_flux_mar_wilcox$conf.int[2],
                                     Sacramento_Freeport_TP_flux_apr_wilcox$conf.int[2],
                                     Sacramento_Freeport_TP_flux_may_wilcox$conf.int[2],
                                     Sacramento_Freeport_TP_flux_jun_wilcox$conf.int[2],
                                     Sacramento_Freeport_TP_flux_jul_wilcox$conf.int[2],
                                     Sacramento_Freeport_TP_flux_aug_wilcox$conf.int[2],
                                     Sacramento_Freeport_TP_flux_sep_wilcox$conf.int[2]))

write.table(Flux_compare, "Sacramento_Freeport_TP_flux_wilcox.txt", quote=FALSE, row.names=FALSE)

rng_flx <- max(abs(c(Flux_compare$up_conf, Flux_compare$low_conf)))
tiff("Sacramento_Freeport_TP_flux_shift_wilcox_Vert_Bars.tif", height=600, width=800, res=130)
par(mar=c(4,5,0.5,0.5))
plot(seq(1:12), Flux_compare$chng_est, typ='h', lend=1, lwd=15, col='white', xaxt='n', xlim=c(1,13), ylim=c(-rng_flx, rng_flx), xlab="Month", ylab=expression(paste("Median Flux Change, kg",sep='')), las=1)
plotCI(seq(1:12), Flux_compare$chng_est, ui=Flux_compare$up_conf, li=Flux_compare$low_conf, pch=16, add=TRUE)
abline(h=0)
axis(side=1,at=seq(1,12,by=1), labels=format(c(seq(as.Date("2000-10-01"), as.Date("2000-12-01"), by="month"), seq(as.Date("2000-01-01"), as.Date("2000-09-01"), by="month")),'%b'), las=2)
legend('topright', c("Median difference", "90% Confidence Interval for the Median"), pch=c(16,NA), lwd=c(NA,1), pt.cex=c(1,NA), pt.bg=c('black',NA), bty='n', bg='white')
dev.off()


# End of non-standard EGRET plot section requested by Michael
# --------------------------------------------------------------------------------------------------------

#################  Using the plotConcQSmooth function  ###########
# First do flow duration analysis
flowDuration(eList, centerDate = "06-01", qUnit = 2, span = 30)
date1 <- "1971-06-01"
date2 <- "2000-06-01"
date3 <- "2018-06-01"
qLow= baseQ
qHigh=highQ7

tiff("BC1_Date_Discharge_TP_conc_no_log.tif",height = 700, width = 1000, res=120)
plotConcQSmooth(eList,date1, date2, date3,qLow, qHigh, logScale=FALSE,printLegend =TRUE,legendLeft=0,legendTop=0,printTitle=TRUE)
dev.off()

flowDuration(eList, centerDate = "12-01", qUnit = 2, span = 30)
date1 <- "1971-12-01"
date2 <- "2000-12-01"
date3 <- "2018-12-01"
qLow= baseQ
qHigh=highQ7

tiff("BC1_Date_Discharge_TP_conc_no_log_December.tif",height = 700, width = 1000, res=120)
plotConcQSmooth(eList,date1, date2, date3,qLow, qHigh, logScale=FALSE,printLegend =TRUE,legendLeft=0,legendTop=0,printTitle=TRUE)
dev.off()


# ---------------------------
# Now run the EGRETci package
# ---------------------------

# Change working directory
setwd("..")
subDir <- 'EGRETci_plots'
if (file.exists(subDir)){
  setwd(file.path(getwd(),subDir))
} else {
  dir.create(file.path(getwd(),subDir), recursive=TRUE)
  setwd(file.path(getwd(),subDir))
}

#Interactive function to set up trend analysis:

caseSetUp <- trendSetUp(eList, 
                        year1=1971, 
                        year2=2018, 
                        nBoot = 200, 
                        bootBreak = 50, 
                        blockLength = 200)
eBoot <- wBT(eList, caseSetUp, fileName ="Sacramento_Freeport_TP_EGRETCIoutputText.txt")

saveEGRETci(eList, eBoot, caseSetUp, fileName = "EGRETci_output")

plotHistogramTrend2 <-
  function (eBoot, caseSetUp, eList, xSeq = seq(-100, 100, 10), 
            flux = TRUE, printTitle = TRUE, cex.main = 1.1, col.fill = "grey", xlim = c(-100,100),
            ...) 
  {
    bootOut <- eBoot$bootOut
    INFO <- eList$INFO
    if (flux) {
      xFlux <- eBoot$xFlux
      change <- 100 * bootOut$estF/bootOut$baseFlux
      reps <- 100 * xFlux/bootOut$baseFlux
      xlabel <- "Flux trend, in %"
      titleWord <- "Flux"
    }
    else {
      xConc <- eBoot$xConc
      change <- 100 * bootOut$estC/bootOut$baseConc
      reps <- 100 * xConc/bootOut$baseConc
      xlabel <- "Concentration trend, in %"
      titleWord <- "Concentration"
    }
    titleToPrint <- ifelse(printTitle, paste("Histogram of trend in", 
                                             INFO$paramShortName, "\n", titleWord, "Normalized Concentration:", 
                                             caseSetUp$year1, "to", caseSetUp$year2, "\n", INFO$shortName), 
                           "")
    hist(reps, breaks = xSeq, yaxs = "i", xaxs = "i", tcl = 0.5, 
         main = titleToPrint, freq = FALSE, xlab = xlabel, col = col.fill, 
         cex.main = cex.main, xlim=xlim, ...)
    abline(v = change, lwd = 3, lty = 2)
    abline(v = 0, lwd = 3)
    box()
    axis(3, tcl = 0.5, labels = FALSE)
    axis(4, tcl = 0.5, labels = FALSE)
  }

tiff("histo_TP_SacFreeport_Trend_conc_flux.tif", height = 700, width = 1200, res=120)
par(mfrow=c(1,2))
plotHistogramTrend2(eBoot, caseSetUp, eList, flux=FALSE, xSeq = seq(-8000,8000,5),las=1,xlim=c(-100,100))
abline(h=0)

plotHistogramTrend2(eBoot, caseSetUp, eList, flux=TRUE, xSeq = seq(-50000,50000,5),las=1)
abline(h=0)
dev.off()

nBoot <- 200
blockLength <- 200
coreOut <- 4 #Number of cores to leave out of processing tasks

widthCI <- 95
ciLower <- (50-(widthCI/2))/100
ciUpper <- (50+(widthCI/2))/100
probs <- c(ciLower,ciUpper)

nCores <- detectCores() - coreOut
cl <- makeCluster(nCores)
registerDoParallel(cl)
repAnnual <- foreach(n = 1:nBoot,.packages=c('EGRETci')) %dopar% {
  annualResults <- bootAnnual(eList, blockLength,startSeed = n)  
}
stopCluster(cl)

CIAnnualResults <- ciBands(eList, repAnnual, probs)
conc.poly.x <- c(CIAnnualResults$Year,rev(CIAnnualResults$Year))
conc.poly.y <- c(CIAnnualResults$FNConcLow,rev(CIAnnualResults$FNConcHigh))
flux.poly.x <- c(CIAnnualResults$Year,rev(CIAnnualResults$Year))
flux.poly.y <- c(CIAnnualResults$FNFluxLow*365,rev(CIAnnualResults$FNFluxHigh*365))

tiff("Ann_Avg_Conc_&_Ann_Flow_Normalized_TP_Conc_Boot.tif", height = 500, width = 600, res=110)
plotConcHistBoot(eList, CIAnnualResults, plotFlowNorm=TRUE, showYLabels=FALSE, showYAxis=FALSE,col=4)
polygon(x=conc.poly.x, y=conc.poly.y, col=rgb(24,116,205,40,max=255),border=NA)
dev.off()

tiff("Ann_Flux_&_Ann_Flow_Normalized_TP_Flux_Boot.tif", height = 500, width = 600, res=110)
plotFluxHistBoot(eList, fluxUnit=4, CIAnnualResults, plotFluxNorm = TRUE, showYLabels=FALSE, showYAxis=TRUE, col=4)
polygon(x=flux.poly.x, y=flux.poly.y, col=rgb(24,116,205,40,max=255),border=NA)
dev.off()
setSweave("Sacramento_Freeport_TP_Conc_CI",7,7)
plotConcHistBoot(eList, CIAnnualResults, plotFlowNorm=TRUE, showYLabels=TRUE, showYAxis=TRUE,col=4)
graphics.off()
setSweave("Sacramento_Freeport_TP_flux_CI",7,7)
plotFluxHistBoot(eList, fluxUnit=4, CIAnnualResults, plotFluxNorm = TRUE, showYLabels=TRUE, showYAxis=TRUE, col=4)
graphics.off()
saveEGRETci(eList, eBoot, fileName="N_Boot")
#save(repAnnual,file="RepAnnual")
write.csv(repAnnual,'repAnnual.csv')

# load(file="N_Boot.RData")
# load(file="RepAnnual")


#############################################################
# Working on SSC
#############################################################

startDate    <- "1973-11-01"
endDate      <- "2018-09-30"
siteNumber   <- "11447650"
QParameterCd <- "00060"
parameterCd  <- "80154"  # "SSC"
Sample <- readNWISSample(siteNumber, parameterCd, startDate, endDate)
fileName <- "FreeportDaily_cfs_ssc.csv"
#Daily        <- readNWISDaily(siteNumber, QParameterCd, startDate, endDate)
Daily <-readUserDaily(filePath, fileName, hasHeader = TRUE, separator = ",", qUnit = 1)
removeDuplicates(Sample)
#fileName     <- "BC1_SSC.csv"
#Sample       <- readUserSample(filePath, fileName)
#Sample       <- removeDuplicates(Sample)

INFO <- readNWISInfo(siteNumber = siteNumber, parameterCd = parameterCd, interactive=FALSE)
INFO$staAbbrev <- paste(strsplit(INFO$station_nm," ")[[1]][1],strsplit(INFO$station_nm," ")[[1]][2])

# Have a look at the available range of WWC data
range(Sample$Date)
#"1973-11-28" "2018-09-28"
#
eList <- mergeReport(INFO, Daily, Sample)

# Change the working directory; redirect plot output to NO3 folder
setwd("../..")
subDir <- 'SSC/EGRET_plots'
if (file.exists(subDir)){
  setwd(file.path(getwd(),subDir))
} else {
  dir.create(file.path(getwd(),subDir), recursive=TRUE)
  setwd(file.path(getwd(),subDir))
}
write.csv(Sample,"Sample_SSC_Sacramento_Freeport.csv")

# Plot water quality data
tiff("Conc_vs_Time_Sacramento_Freeport_SSC.tif", height = 600, width = 800, res=120)
plotConcTime(eList)
dev.off()

# Now, a classic Q-C plot
tiff("Conc-Q_Sacramento_Freeport_SSC.tif", height = 600, width = 800, res=120)
plotConcQ(eList, logScale=TRUE)
dev.off()

# The data set as flux values rather than as concentrations
tiff("Flux-Q_Sacramento_Freeport_SSC.tif", height = 600, width = 800, res=120)
plotFluxQ(eList, fluxUnit=4)
dev.off()

# Monthly boxplots
tiff("Monthly-Conc_BoxPlots_Sacramento_Freeport_SSC.tif", height = 600, width = 800, res=120)
boxConcMonth(eList, logScale=TRUE)
dev.off()

# Flow on days sampled vs. all other days
tiff("Flow_on_days_sampled_vs_all_other_days_Sacramento_Freeport_SSC.tif", height = 600, width = 800, res=120)
boxQTwice(eList, qUnit=2)
dev.off()

#########################################
# Now start the Flow-Normalized Analysis
#########################################

# Build the regression model
eList <- modelEstimation(eList, windowY = 7, windowQ = 2, windowS = 0.5, minNumObs = 100, minNumUncen =50)
eList_SSC <- eList

MonthlyResults <- calculateMonthlyResults(eList)

# Plot the annual average concentration and annual flow-normalized concentration
tiff("Ann_Avg_Conc_&_Ann_Flow_Normalized_Conc_Sacramento_Freeport_SSC.tif", height = 600, width = 800, res=120)
plotConcHist(eList, plotFlowNorm=TRUE)
dev.off()

# Plot the annual flux and annual flow-normalized flux
tiff("Ann_Flux_&_Ann_Flow_Normalized_Flux_Sacramento_Freeport_SSC.tif", height = 600, width = 800, res=120)
plotFluxHist(eList, plotFlowNorm = TRUE) # fluxMax) # fluxMax
dev.off()

# Look for a trend change:
tableChange(eList, fluxUnit=6, yearPoints=c(1980, 1993,2017))

#  
# Dump SSC-related flow-normalized data to text file for bringing together with other monitoring sites
paLong <- 12
paStart <- 10
localDaily <- getDaily(eList_SSC)
localAnnualResults <- setupYears(paStart = paStart, paLong = paLong, localDaily = localDaily)
write.table(localAnnualResults, file = 'BC_SSC_RawVals.txt', quote=FALSE, row.names=FALSE)

write.csv(localDaily,'localDaily.csv')

#Generate out-of-the-box diagnostic plots
tiff("fluxBiasMulti_Sacramento_Freeport_SSC.tif", height = 1200, width = 1000, res=120)
fluxBiasMulti(eList, moreTitle = "WRTDS")
dev.off()

tiff("Modeled_Daily_Conc_wObservations_Sacramento_Freeport_SSC.tif", height = 800, width = 1000, res=120)
plotConcTimeDaily(eList)
dev.off()

# Exploring model behavior and adjusting model parameters
tiff("Contours_Sacramento_Freeport_SSC.tif", height = 700, width = 1000, res=120)
plotContours(eList, qBottom=100,qTop=2500,yearStart=1974,yearEnd=2019, contourLevels=seq(0,250,by=10), color.palette = colorRampPalette(c("violet", "purple", "blue", "cyan", "green", "yellow", "orange", "red"))) 
dev.off()

tiff("Log_Contours_Sacramento_Freeport_SSC.tif", height = 700, width = 1000, res=120)
plotContours(eList, qBottom=100, qTop=2500, yearStart=1974, yearEnd=2019, contourLevels=seq(0.5,5.8,by=0.1), color.palette = colorRampPalette(c("violet", "purple", "blue", "cyan", "green", "yellow", "orange", "red")), whatSurface=1) 
dev.off()

tiff("StdErr_of_Log_Contours_Sacramento_Freeport_SSC.tif", height = 700, width = 1000, res=120)
plotContours(eList, qBottom=100, qTop=2500, yearStart=1974, yearEnd=2019, contourLevels=seq(0.38,1.289,by=0.02), color.palette = colorRampPalette(c("violet", "purple", "blue", "cyan", "green", "yellow", "orange", "red")), whatSurface=2) 
dev.off()

tiff("Contours_Difference_SSC_Sacramento_Freeport.tif", height = 700, width = 1000, res=120)
plotDiffContours(eList, 1974,2018,100,2500,maxDiff=100)
dev.off()

tiff("Contours_PercentDifference_SSC_Sacramento_Freeport.tif", height = 700, width = 1000, res=120)
plotDiffContours(eList, 1974,2018,100,2500, maxDiff=100, plotPercent=TRUE)
dev.off()

tiff("Contours_PercentDifference2_SSC.tif", height = 700, width = 1000, res=120)
plotDiffContours(eList, 1974,2018,100,2500, maxDiff=c(-100,100), plotPercent=TRUE, lwd=3, color.palette=colorRampPalette(c("purple","blue","cadetblue","lightblue","white","beige","yellow", "orange", "red","brown")),tick.lwd = 1)
dev.off()

Sample$WY <- trunc(Sample$DecYear+0.25) 
tiff("Monthly_Boxplot_Inorg_SSC_Sacramento_Freeport.tif", height = 700, width = 1000, res=120)
par(mar=c(4,6,0.5,0.5))
boxplot(Sample$ConcAve~Sample$WY,log="y",varwidth=TRUE,ylim=c(0.1,1000),yaxs="i",xlab="Water Year",las=1) 
mtext(side=2, expression(paste("Concentration, Inorganic Nitrogen, in mg  ",L^-1,sep="")),line=4)
dev.off()



##############################
# Determine which flow rates to use for discharge-specific trends

# Baseflow: mean of the annual 30-day low flows
baseQ <- mean(aggregate(Q30 ~ waterYear, data = localDaily, min)[,2])
baseQ_txt <- format(baseQ, digits=2)
baseQ_txt_cfs <- format(baseQ * 35.315, digits=2)

# mid-range: median flow rate across all years
medQ <- median(localDaily$Q)
medQ_txt <- format(medQ, digits=2)
medQ_txt_cfs <- format(medQ * 35.315, digits=2)

# high flow: get the 25% quantile of each year's maximum Q7
# This will help ensure (but not guarantee) that every year is well represented in the high-end flows
highQ7 <- as.numeric(quantile(aggregate(Q7 ~ waterYear, data = localDaily, max)[,2])[2])
highQ7_txt <- format(highQ7, digits=2)
highQ7_txt_cfs <- format(highQ7 * 35.315, digits=2)

# The following bit of script generates a figure discussed by Joe in an email on 6/2/17
# -------------------------------------------------------------------------------------

tiff("Discharge_specific_trends_SSC_centered_on_06-01.tif", height = 600, width = 1200, res=120)
par(mar=c(4,6,4.1,8))
plotConcTimeSmooth(eList, q1 = baseQ, q2 = medQ, q3 = highQ7, centerDate='06-01', 
                   yearStart=localDaily$waterYear[1], yearEnd=localDaily$waterYear[nrow(localDaily)], 
                   logScale=FALSE, printLegend=FALSE)

# Determine y position of the legend
# ----------------------------------
y_l <- par('usr')[3]
y_u <- par('usr')[4]
y_m <- mean(y_l, y_u)

# Use the top version of the legend to add cfs

# legend('bottomleft', c(eval(substitute(expression(paste('Baseflow [',baseQ_txt,' ', m^3~s^-1,'(',baseQ_txt_cfs,' ',ft^3~s^-1,')]',sep=' ')), list(baseQ_txt=baseQ_txt, baseQ_txt_cfs=baseQ_txt_cfs))),
#                        eval(substitute(expression(paste('Median Flow [',medQ_txt,' ', m^3~s^-1,'(',medQ_txt_cfs,' ',ft^3~s^-1,')]',sep=' ')), list(medQ_txt=medQ_txt, medQ_txt_cfs=medQ_txt_cfs))),
#                        eval(substitute(expression(paste('High Flow [',highQ7_txt,' ', m^3~s^-1,'(',highQ7_txt_cfs,' ',ft^3~s^-1,')]',sep=' ')), list(highQ7_txt=highQ7_txt, highQ7_txt_cfs=highQ7_txt_cfs)))), 
#                        col=c('black','red','green'), lwd=2, bg='white', bty='n')

legend('bottomleft', c(eval(substitute(expression(paste('Baseflow (',baseQ_txt,' ', m^3~s^-1,')',sep=' ')), list(baseQ_txt=baseQ_txt))),
                       eval(substitute(expression(paste('Median Flow (',medQ_txt,' ', m^3~s^-1,')',sep=' ')), list(medQ_txt=medQ_txt))),
                       eval(substitute(expression(paste('High Flow (',highQ7_txt,' ', m^3~s^-1,')',sep=' ')), list(highQ7_txt=highQ7_txt)))), 
       col=c('black','red','green'), lwd=2, bg='white', bty='n')

dev.off()

# Restore original plotting margins
par(mar=c(5.1,6.1,4.1,2.1))

# The following bit of script generates a figure discussed by Michael in an email on 6/2/17
# -----------------------------------------------------------------------------------------
# Get the row number corresponding to the maximum concentration for each year (bearing in mind that these are 'within group' row numbers)
out <- aggregate(ConcDay ~ waterYear, data = localDaily, which.max)

# Ensure data is ordered by water year
out <- out[ order(out$waterYear), ]

# Get a count of the number of days within each of the water years
tbl <- table(localDaily$waterYear)

# Return the absolute row positions for each water year's max concentration
out$AbsConcDay <- out$ConcDay + cumsum(c(0,tbl[-length(tbl)]))

# Make a data.frame containing only the rows with each water year's max conc
out2 <- localDaily[out$AbsConcDay,]

# Gather only the needed data
out2 <- data.frame(Date=out2$Date, Q=out2$Q, Conc=out2$ConcDay, wyr=out2$waterYear, Julian=yday(as.Date(out2$Date)))

# Need to readjust the Julian day to start on Oct 1 (This function doesn't yet account for leap years)
out2$JulianWYR <- ifelse(out2$Julian > 273, out2$Julian - 273, 92 + out2$Julian)

# Plot it
tiff("JulianDay_of_Max_SSC_Conc.tif", height = 600, width = 800, res=120)
plot(out2$wyr, out2$JulianWYR, pch=16, xlab='Water Year', ylab='Julian Day', yaxs='i', ylim=c(0,370), las=1)
dev.off()

# In a follow-up email from Michael on 6/7/17, Michael suggested two alterations:
#  1) Apply a 30-day moving average
#  2) Use the flow-normalized concentration

# To start with, I'll attempt to apply a 30-day window to the simulated daily concentrations

localDaily$ConcDay_30day <- c(rep(rollapply(localDaily$ConcDay, width=30, mean)[1],times=14) , rollapply(localDaily$ConcDay, width=30, mean), rep(rollapply(localDaily$ConcDay, width=30, mean)[length(rollapply(localDaily$ConcDay, width=30, mean))], times=15))
out_m <- aggregate(ConcDay_30day ~ waterYear, data = localDaily, which.max)
out_m <- out_m[ order(out_m$waterYear), ]
out_m$AbsConcDay <- out_m$ConcDay + cumsum(c(0,tbl[-length(tbl)]))
out_m2 <- localDaily[out_m$AbsConcDay,]
out_m2 <- data.frame(Date=out_m2$Date, Q=out_m2$Q, Conc30=out_m2$ConcDay_30day, wyr=out_m2$waterYear, Julian=yday(as.Date(out_m2$Date)))
out_m2$JulianWYR <- ifelse(out_m2$Julian > 273, out_m2$Julian - 273, 92 + out_m2$Julian)

tiff("JulianDay_of_Max_SSC_Conc_Using_30_rollingAvg.tif", height = 600, width = 800, res=120)
plot(out_m2$wyr, out_m2$JulianWYR, pch=16, xlab='Water Year', ylab='Julian Day', yaxs='i', ylim=c(0,370), las=1)
dev.off()


# Next, I'll try using the flow-normalized concentration (same general code flow as above)
# First, try plotting flow-normalized concentration:
# Plot it
tiff("Flow_Normalized_Conc_BC1_SSC.tif", height = 600, width = 800, res=120)
plot(as.Date(localDaily$Date), localDaily$FNConc, typ='l', las=1, xlab='Time', ylab='Flow-normalized Concentration')
dev.off()

out_FN <- aggregate(FNConc ~ waterYear, data = localDaily, which.max)
out_FN <- out_FN[ order(out_FN$waterYear), ]
out_FN$AbsConcDay <- out_FN$FNConc + cumsum(c(0,tbl[-length(tbl)]))
out2_FN <- localDaily[out_FN$AbsConcDay,]
out2_FN <- data.frame(Date=out2_FN$Date, Q=out2_FN$Q, Conc=out2_FN$FNConc, wyr=out2_FN$waterYear, Julian=yday(as.Date(out2_FN$Date)))
out2_FN$JulianWYR <- ifelse(out2_FN$Julian > 273, out2_FN$Julian - 273, 92 + out2_FN$Julian)

# Plot it
tiff("JulianDay_of_Max_SSC_Flow_Normalized_Conc.tif", height = 600, width = 800, res=120)
plot(out2_FN$wyr, out2_FN$JulianWYR, pch=16, xlab='Water Year', ylab='Julian Day', yaxs='i', ylim=c(0,370), las=1)
dev.off()



# --------------------------------------------------------------------------------------------------------
# The following script is for a non-standard EGRET plot and instead help generate a plot Michael requested

localDaily <- getDaily(eList)

# Will need to adjust the date range below based on each gages unique start/stop dates
early_decade <- subset(localDaily, localDaily$Date > as.Date('1973-09-30') & localDaily$Date < as.Date('1983-11-01'))
recent_decade <- subset(localDaily, localDaily$Date > as.Date('2008-09-30'))


early_decade_monthly_mn <- aggregate(ConcDay ~ MonthSeq, data = early_decade, 'mean')
recent_decade_monthly_mn <- aggregate(ConcDay ~ MonthSeq, data = recent_decade, 'mean')

# early_decade_monthly_mn$month <- format(seq(as.Date('1972-10-01'), as.Date('1982-09-30'), by='month'), '%b')
early_decade_monthly_mn$month <- rep(c(10:12,1:9), times=10)
early_decade_mon_mn <- aggregate(ConcDay ~ month, data = early_decade_monthly_mn, 'mean')
early_decade_mon_sd <- aggregate(ConcDay ~ month, data = early_decade_monthly_mn, 'sd')
early_decade_mon_mn <- early_decade_mon_mn[c(10:12,1:9),]
early_decade_mon_sd <- early_decade_mon_sd[c(10:12,1:9),]

recent_decade_monthly_mn$month <- rep(c(10:12,1:9), times=10)
recent_decade_mon_mn <- aggregate(ConcDay ~ month, data = recent_decade_monthly_mn, 'mean')
recent_decade_mon_sd <- aggregate(ConcDay ~ month, data = recent_decade_monthly_mn, 'sd')
recent_decade_mon_mn <- recent_decade_mon_mn[c(10:12,1:9),]
recent_decade_mon_sd <- recent_decade_mon_sd[c(10:12,1:9),]


mdat2 <- matrix(c(early_decade_mon_mn$ConcDay, recent_decade_mon_mn$ConcDay),
                nrow=2,ncol = 12, byrow=TRUE,
                dimnames = list(c("1980-1990", "2007-2017"),
                                c(format(seq(as.Date('1973-10-01'), as.Date('1974-09-01'), by='month'), '%b'))))

# Be sure to adjust the legend's first decade start and stop year correctly
mx <- max(c((early_decade_mon_mn$ConcDay + early_decade_mon_sd$ConcDay), (recent_decade_mon_mn$ConcDay + recent_decade_mon_sd$ConcDay)))

tiff("timing_shift_in_SSC_conc_monthly_means.tif", height=800, width=900, res=130)
par(mar=c(3,5,2,1))
x <- barplot(mdat2, beside=TRUE, las=1, ylim=c(0,mx), col = c("lightblue", "mistyrose"))
abline(h=0)
arrows(x0=x[1,], y0=early_decade_mon_mn$ConcDay - early_decade_mon_sd$ConcDay, x1=x[1,], y1=early_decade_mon_mn$ConcDay + early_decade_mon_sd$ConcDay, angle=90, length=0.04, code=3)
arrows(x0=x[2,], y0=recent_decade_mon_mn$ConcDay - recent_decade_mon_sd$ConcDay, x1=x[2,], y1=recent_decade_mon_mn$ConcDay + recent_decade_mon_sd$ConcDay, angle=90, length=0.04, code=3)
mtext(side=2, expression(paste(SSC,', mg ',L^-1,sep='')), line=3)
legend(x=25, y=0.9 * mx, c("1973-1983", "2008-2018"), pch=c(22,22), pt.cex=2, pt.bg=c("lightblue", "mistyrose"), bty='n', xpd=TRUE)
dev.off()


# Now attempting a Wilcox Test (aka Mann-Whitney-Wilcoxon Rank Sum test)
# ----------------------------------------------------------------------
early_jan <- subset(early_decade_monthly_mn, month==1)
recent_jan <- subset(recent_decade_monthly_mn, month==1)
Sacramento_Freeport_SSC_conc_jan_wilcox <- wilcox.test(recent_jan$ConcDay, early_jan$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_feb <- subset(early_decade_monthly_mn, month==2)
recent_feb <- subset(recent_decade_monthly_mn, month==2)
Sacramento_Freeport_SSC_conc_feb_wilcox <- wilcox.test(recent_feb$ConcDay, early_feb$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_mar <- subset(early_decade_monthly_mn, month==3)
recent_mar <- subset(recent_decade_monthly_mn, month==3)
Sacramento_Freeport_SSC_conc_mar_wilcox <- wilcox.test(recent_mar$ConcDay, early_mar$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_apr <- subset(early_decade_monthly_mn, month==4)
recent_apr <- subset(recent_decade_monthly_mn, month==4)
Sacramento_Freeport_SSC_conc_apr_wilcox <- wilcox.test(recent_apr$ConcDay, early_apr$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_may <- subset(early_decade_monthly_mn, month==5)
recent_may <- subset(recent_decade_monthly_mn, month==5)
Sacramento_Freeport_SSC_conc_may_wilcox <- wilcox.test(recent_may$ConcDay, early_may$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_jun <- subset(early_decade_monthly_mn, month==6)
recent_jun <- subset(recent_decade_monthly_mn, month==6)
Sacramento_Freeport_SSC_conc_jun_wilcox <- wilcox.test(recent_jun$ConcDay, early_jun$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_jul <- subset(early_decade_monthly_mn, month==7)
recent_jul <- subset(recent_decade_monthly_mn, month==7)
Sacramento_Freeport_SSC_conc_jul_wilcox <- wilcox.test(recent_jul$ConcDay, early_jul$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_aug <- subset(early_decade_monthly_mn, month==8)
recent_aug <- subset(recent_decade_monthly_mn, month==8)
Sacramento_Freeport_SSC_conc_aug_wilcox <- wilcox.test(recent_aug$ConcDay, early_aug$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_sep <- subset(early_decade_monthly_mn, month==9)
recent_sep <- subset(recent_decade_monthly_mn, month==9)
Sacramento_Freeport_SSC_conc_sep_wilcox <- wilcox.test(recent_sep$ConcDay, early_sep$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_oct <- subset(early_decade_monthly_mn, month==10)
recent_oct <- subset(recent_decade_monthly_mn, month==10)
Sacramento_Freeport_SSC_conc_oct_wilcox <- wilcox.test(recent_oct$ConcDay, early_oct$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_nov <- subset(early_decade_monthly_mn, month==11)
recent_nov <- subset(recent_decade_monthly_mn, month==11)
Sacramento_Freeport_SSC_conc_nov_wilcox <- wilcox.test(recent_nov$ConcDay, early_nov$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_dec <- subset(early_decade_monthly_mn, month==12)
recent_dec <- subset(recent_decade_monthly_mn, month==12)
Sacramento_Freeport_SSC_conc_dec_wilcox <- wilcox.test(recent_dec$ConcDay, early_dec$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

Conc_compare <- data.frame(chng_est=c(Sacramento_Freeport_SSC_conc_oct_wilcox$est,
                                      Sacramento_Freeport_SSC_conc_nov_wilcox$est,
                                      Sacramento_Freeport_SSC_conc_dec_wilcox$est,
                                      Sacramento_Freeport_SSC_conc_jan_wilcox$est,
                                      Sacramento_Freeport_SSC_conc_feb_wilcox$est,
                                      Sacramento_Freeport_SSC_conc_mar_wilcox$est,
                                      Sacramento_Freeport_SSC_conc_apr_wilcox$est,
                                      Sacramento_Freeport_SSC_conc_may_wilcox$est,
                                      Sacramento_Freeport_SSC_conc_jun_wilcox$est,
                                      Sacramento_Freeport_SSC_conc_jul_wilcox$est,
                                      Sacramento_Freeport_SSC_conc_aug_wilcox$est,
                                      Sacramento_Freeport_SSC_conc_sep_wilcox$est),
                           low_conf=c(Sacramento_Freeport_SSC_conc_oct_wilcox$conf.int[1],
                                      Sacramento_Freeport_SSC_conc_nov_wilcox$conf.int[1],
                                      Sacramento_Freeport_SSC_conc_dec_wilcox$conf.int[1],
                                      Sacramento_Freeport_SSC_conc_jan_wilcox$conf.int[1],
                                      Sacramento_Freeport_SSC_conc_feb_wilcox$conf.int[1],
                                      Sacramento_Freeport_SSC_conc_mar_wilcox$conf.int[1],
                                      Sacramento_Freeport_SSC_conc_apr_wilcox$conf.int[1],
                                      Sacramento_Freeport_SSC_conc_may_wilcox$conf.int[1],
                                      Sacramento_Freeport_SSC_conc_jun_wilcox$conf.int[1],
                                      Sacramento_Freeport_SSC_conc_jul_wilcox$conf.int[1],
                                      Sacramento_Freeport_SSC_conc_aug_wilcox$conf.int[1],
                                      Sacramento_Freeport_SSC_conc_sep_wilcox$conf.int[1]),
                           up_conf=c(Sacramento_Freeport_SSC_conc_oct_wilcox$conf.int[2],
                                     Sacramento_Freeport_SSC_conc_nov_wilcox$conf.int[2],
                                     Sacramento_Freeport_SSC_conc_dec_wilcox$conf.int[2],
                                     Sacramento_Freeport_SSC_conc_jan_wilcox$conf.int[2],
                                     Sacramento_Freeport_SSC_conc_feb_wilcox$conf.int[2],
                                     Sacramento_Freeport_SSC_conc_mar_wilcox$conf.int[2],
                                     Sacramento_Freeport_SSC_conc_apr_wilcox$conf.int[2],
                                     Sacramento_Freeport_SSC_conc_may_wilcox$conf.int[2],
                                     Sacramento_Freeport_SSC_conc_jun_wilcox$conf.int[2],
                                     Sacramento_Freeport_SSC_conc_jul_wilcox$conf.int[2],
                                     Sacramento_Freeport_SSC_conc_aug_wilcox$conf.int[2],
                                     Sacramento_Freeport_SSC_conc_sep_wilcox$conf.int[2]))

write.table(Conc_compare, "Sacramento_Freeport_SSC_conc_wilcox.txt", quote=FALSE, row.names=FALSE)

rng <- max(abs(c(Conc_compare$up_conf, Conc_compare$low_conf)))
tiff("Sacramento_Freeport_SSC_conc_shift_wilcox_Vert_Bars.tif", height=600, width=800, res=130)
par(mar=c(4,5,0.5,0.5))
plot(seq(1:12), Conc_compare$chng_est, typ='h', lend=1, lwd=15, col='white', xaxt='n', xlim=c(1,13), ylim=c(-rng, rng), xlab="Month", ylab=expression(paste("Median Concentration Change, mg  ",L^-1,sep='')), las=1)
plotCI(seq(1:12), Conc_compare$chng_est, ui=Conc_compare$up_conf, li=Conc_compare$low_conf, pch=16, add=TRUE)
abline(h=0)
axis(side=1,at=seq(1,12,by=1), labels=format(c(seq(as.Date("2000-10-01"), as.Date("2000-12-01"), by="month"), seq(as.Date("2000-01-01"), as.Date("2000-09-01"), by="month")),'%b'), las=2)
legend('topright', c("Median difference", "90% Confidence Interval for the Median"), pch=c(16,NA), lwd=c(NA,1), pt.cex=c(1,NA), pt.bg=c('black',NA), bty='n', bg='white')
dev.off()


# Now do the load
# ---------------
early_decade_monthly_flx <- aggregate(FluxDay ~ MonthSeq, data = early_decade, 'sum')
recent_decade_monthly_flx <- aggregate(FluxDay ~ MonthSeq, data = recent_decade, 'sum')

# early_decade_monthly_mn$month <- format(seq(as.Date('1972-10-01'), as.Date('1982-09-30'), by='month'), '%b')
early_decade_monthly_flx$month <- rep(c(10:12,1:9), times=10)
early_decade_mon_mn_flx <- aggregate(FluxDay ~ month, data = early_decade_monthly_flx, 'mean')
early_decade_mon_sd_flx <- aggregate(FluxDay ~ month, data = early_decade_monthly_flx, 'sd')
early_decade_mon_mn_flx <- early_decade_mon_mn_flx[c(10:12,1:9),]
early_decade_mon_sd_flx <- early_decade_mon_sd_flx[c(10:12,1:9),]

recent_decade_monthly_flx$month <- rep(c(10:12,1:9), times=10)
recent_decade_mon_mn_flx <- aggregate(FluxDay ~ month, data = recent_decade_monthly_flx, 'mean')
recent_decade_mon_sd_flx <- aggregate(FluxDay ~ month, data = recent_decade_monthly_flx, 'sd')
recent_decade_mon_mn_flx <- recent_decade_mon_mn_flx[c(10:12,1:9),]
recent_decade_mon_sd_flx <- recent_decade_mon_sd_flx[c(10:12,1:9),]

mdat3 <- matrix(c(early_decade_mon_mn_flx$FluxDay, recent_decade_mon_mn_flx$FluxDay),
                nrow=2,ncol = 12, byrow=TRUE,
                dimnames = list(c("1980-1990", "2007-2017"),
                                c(format(seq(as.Date('1973-10-01'), as.Date('1974-09-01'), by='month'), '%b'))))

mx <- max(c((early_decade_mon_mn_flx$FluxDay + early_decade_mon_sd_flx$FluxDay), (recent_decade_mon_mn_flx$FluxDay + recent_decade_mon_sd_flx$FluxDay)))
tiff("timing_shift_in_SSC_load_monthly_means.tif", height=800, width=900, res=130)
x <- barplot(mdat3, beside=TRUE, las=1, ylim=c(0,mx), col = c("lightblue", "mistyrose"))
abline(h=0)
arrows(x0=x[1,], y0=early_decade_mon_mn_flx$FluxDay - early_decade_mon_sd_flx$FluxDay, x1=x[1,], y1=early_decade_mon_mn_flx$FluxDay + early_decade_mon_sd_flx$FluxDay, angle=90, length=0.04, code=3)
arrows(x0=x[2,], y0=recent_decade_mon_mn_flx$FluxDay - recent_decade_mon_sd_flx$FluxDay, x1=x[2,], y1=recent_decade_mon_mn_flx$FluxDay + recent_decade_mon_sd_flx$FluxDay, angle=90, length=0.04, code=3)
mtext(side=2, expression(paste(SSC,', kg ',month^-1,sep='')), line=2.5)
legend(x=30, y=0.9 * mx, c("1973-1983", "2008-2018"), pch=c(22,22), pt.cex=2, pt.bg=c("lightblue", "mistyrose"), bty='n', xpd=TRUE)
dev.off()

# Apply Wilcox.text to the monthly loads here...
early_jan_flx <- subset(early_decade_monthly_flx, month==1)
recent_jan_flx <- subset(recent_decade_monthly_flx, month==1)
Sacramento_Freeport_SSC_flux_jan_wilcox <- wilcox.test(recent_jan_flx$FluxDay, early_jan_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_feb_flx <- subset(early_decade_monthly_flx, month==2)
recent_feb_flx <- subset(recent_decade_monthly_flx, month==2)
Sacramento_Freeport_SSC_flux_feb_wilcox <- wilcox.test(recent_feb_flx$FluxDay, early_feb_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_mar_flx <- subset(early_decade_monthly_flx, month==3)
recent_mar_flx <- subset(recent_decade_monthly_flx, month==3)
Sacramento_Freeport_SSC_flux_mar_wilcox <- wilcox.test(recent_mar_flx$FluxDay, early_mar_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_apr_flx <- subset(early_decade_monthly_flx, month==4)
recent_apr_flx <- subset(recent_decade_monthly_flx, month==4)
Sacramento_Freeport_SSC_flux_apr_wilcox <- wilcox.test(recent_apr_flx$FluxDay, early_apr_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_may_flx <- subset(early_decade_monthly_flx, month==5)
recent_may_flx <- subset(recent_decade_monthly_flx, month==5)
Sacramento_Freeport_SSC_flux_may_wilcox <- wilcox.test(recent_may_flx$FluxDay, early_may_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_jun_flx <- subset(early_decade_monthly_flx, month==6)
recent_jun_flx <- subset(recent_decade_monthly_flx, month==6)
Sacramento_Freeport_SSC_flux_jun_wilcox <- wilcox.test(recent_jun_flx$FluxDay, early_jun_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_jul_flx <- subset(early_decade_monthly_flx, month==7)
recent_jul_flx <- subset(recent_decade_monthly_flx, month==7)
Sacramento_Freeport_SSC_flux_jul_wilcox <- wilcox.test(recent_jul_flx$FluxDay, early_jul_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_aug_flx <- subset(early_decade_monthly_flx, month==8)
recent_aug_flx <- subset(recent_decade_monthly_flx, month==8)
Sacramento_Freeport_SSC_flux_aug_wilcox <- wilcox.test(recent_aug_flx$FluxDay, early_aug_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_sep_flx <- subset(early_decade_monthly_flx, month==9)
recent_sep_flx <- subset(recent_decade_monthly_flx, month==9)
Sacramento_Freeport_SSC_flux_sep_wilcox <- wilcox.test(recent_sep_flx$FluxDay, early_sep_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_oct_flx <- subset(early_decade_monthly_flx, month==10)
recent_oct_flx <- subset(recent_decade_monthly_flx, month==10)
Sacramento_Freeport_SSC_flux_oct_wilcox <- wilcox.test(recent_oct_flx$FluxDay, early_oct_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_nov_flx <- subset(early_decade_monthly_flx, month==11)
recent_nov_flx <- subset(recent_decade_monthly_flx, month==11)
Sacramento_Freeport_SSC_flux_nov_wilcox <- wilcox.test(recent_nov_flx$FluxDay, early_nov_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_dec_flx <- subset(early_decade_monthly_flx, month==12)
recent_dec_flx <- subset(recent_decade_monthly_flx, month==12)
Sacramento_Freeport_SSC_flux_dec_wilcox <- wilcox.test(recent_dec_flx$FluxDay, early_dec_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)


Flux_compare <- data.frame(chng_est=c(Sacramento_Freeport_SSC_flux_oct_wilcox$est,
                                      Sacramento_Freeport_SSC_flux_nov_wilcox$est,
                                      Sacramento_Freeport_SSC_flux_dec_wilcox$est,
                                      Sacramento_Freeport_SSC_flux_jan_wilcox$est,
                                      Sacramento_Freeport_SSC_flux_feb_wilcox$est,
                                      Sacramento_Freeport_SSC_flux_mar_wilcox$est,
                                      Sacramento_Freeport_SSC_flux_apr_wilcox$est,
                                      Sacramento_Freeport_SSC_flux_may_wilcox$est,
                                      Sacramento_Freeport_SSC_flux_jun_wilcox$est,
                                      Sacramento_Freeport_SSC_flux_jul_wilcox$est,
                                      Sacramento_Freeport_SSC_flux_aug_wilcox$est,
                                      Sacramento_Freeport_SSC_flux_sep_wilcox$est),
                           low_conf=c(Sacramento_Freeport_SSC_flux_oct_wilcox$conf.int[1],
                                      Sacramento_Freeport_SSC_flux_nov_wilcox$conf.int[1],
                                      Sacramento_Freeport_SSC_flux_dec_wilcox$conf.int[1],
                                      Sacramento_Freeport_SSC_flux_jan_wilcox$conf.int[1],
                                      Sacramento_Freeport_SSC_flux_feb_wilcox$conf.int[1],
                                      Sacramento_Freeport_SSC_flux_mar_wilcox$conf.int[1],
                                      Sacramento_Freeport_SSC_flux_apr_wilcox$conf.int[1],
                                      Sacramento_Freeport_SSC_flux_may_wilcox$conf.int[1],
                                      Sacramento_Freeport_SSC_flux_jun_wilcox$conf.int[1],
                                      Sacramento_Freeport_SSC_flux_jul_wilcox$conf.int[1],
                                      Sacramento_Freeport_SSC_flux_aug_wilcox$conf.int[1],
                                      Sacramento_Freeport_SSC_flux_sep_wilcox$conf.int[1]),
                           up_conf=c(Sacramento_Freeport_SSC_flux_oct_wilcox$conf.int[2],
                                     Sacramento_Freeport_SSC_flux_nov_wilcox$conf.int[2],
                                     Sacramento_Freeport_SSC_flux_dec_wilcox$conf.int[2],
                                     Sacramento_Freeport_SSC_flux_jan_wilcox$conf.int[2],
                                     Sacramento_Freeport_SSC_flux_feb_wilcox$conf.int[2],
                                     Sacramento_Freeport_SSC_flux_mar_wilcox$conf.int[2],
                                     Sacramento_Freeport_SSC_flux_apr_wilcox$conf.int[2],
                                     Sacramento_Freeport_SSC_flux_may_wilcox$conf.int[2],
                                     Sacramento_Freeport_SSC_flux_jun_wilcox$conf.int[2],
                                     Sacramento_Freeport_SSC_flux_jul_wilcox$conf.int[2],
                                     Sacramento_Freeport_SSC_flux_aug_wilcox$conf.int[2],
                                     Sacramento_Freeport_SSC_flux_sep_wilcox$conf.int[2]))

write.table(Flux_compare, "Sacramento_Freeport_SSC_flux_wilcox.txt", quote=FALSE, row.names=FALSE)

rng_flx <- max(abs(c(Flux_compare$up_conf, Flux_compare$low_conf)))
tiff("Sacramento_Freeport_SSC_flux_shift_wilcox_Vert_Bars.tif", height=600, width=800, res=130)
par(mar=c(4,5,0.5,0.5))
plot(seq(1:12), Flux_compare$chng_est, typ='h', lend=1, lwd=15, col='white', xaxt='n', xlim=c(1,13), ylim=c(-rng_flx, rng_flx), xlab="Month", ylab=expression(paste("Median Flux Change, kg",sep='')), las=1)
plotCI(seq(1:12), Flux_compare$chng_est, ui=Flux_compare$up_conf, li=Flux_compare$low_conf, pch=16, add=TRUE)
abline(h=0)
axis(side=1,at=seq(1,12,by=1), labels=format(c(seq(as.Date("2000-10-01"), as.Date("2000-12-01"), by="month"), seq(as.Date("2000-01-01"), as.Date("2000-09-01"), by="month")),'%b'), las=2)
legend('topright', c("Median difference", "90% Confidence Interval for the Median"), pch=c(16,NA), lwd=c(NA,1), pt.cex=c(1,NA), pt.bg=c('black',NA), bty='n', bg='white')
dev.off()


# End of non-standard EGRET plot section requested by Michael
# --------------------------------------------------------------------------------------------------------

#################  Using the plotConcQSmooth function  ###########

# First do flow duration analysis

flowDuration(eList, centerDate = "06-01", qUnit = 2, span = 30)
date1 <- "1974-06-01"
date2 <- "2000-06-01"
date3 <- "2018-06-01"
qLow= baseQ
qHigh=highQ7



tiff("BC1_Date_Discharge_SSC_conc_no_log.tif",height = 700, width = 1000, res=120)
plotConcQSmooth(eList,date1, date2, date3,qLow, qHigh, logScale=FALSE,printLegend =TRUE,legendLeft=0,legendTop=0,printTitle=TRUE)
dev.off()

flowDuration(eList, centerDate = "12-01", qUnit = 2, span = 30)
date1 <- "1974-12-01"
date2 <- "2000-12-01"
date3 <- "2018-12-01"
qLow= baseQ
qHigh=highQ7



tiff("BC1_Date_Discharge_SSC_conc_no_log_December.tif",height = 700, width = 1000, res=120)
plotConcQSmooth(eList,date1, date2, date3,qLow, qHigh, logScale=FALSE,printLegend =TRUE,legendLeft=0,legendTop=0,printTitle=TRUE)
dev.off()





# ---------------------------
# Now run the EGRETci package
# ---------------------------

# Change working directory
setwd("..")
subDir <- 'EGRETci_plots'
if (file.exists(subDir)){
  setwd(file.path(getwd(),subDir))
} else {
  dir.create(file.path(getwd(),subDir), recursive=TRUE)
  setwd(file.path(getwd(),subDir))
}

#Interactive function to set up trend analysis:
caseSetUp <- trendSetUp(eList, 
                        year1=1975, 
                        year2=2018, 
                        nBoot = 200, 
                        bootBreak = 50, 
                        blockLength = 200)
eBoot <- wBT(eList, caseSetUp, fileName ="Sacramento_Freeport_SSC_EGRETCIoutputText.txt")
saveEGRETci(eList, eBoot, caseSetUp, fileName = "Sacramento_Freeport_SSC_EGRETci_output")

plotHistogramTrend2 <-
  function (eBoot, caseSetUp, eList, xSeq = seq(-100, 100, 10), 
            flux = TRUE, printTitle = TRUE, cex.main = 1.1, col.fill = "grey", xlim = c(-100,100),
            ...) 
  {
    bootOut <- eBoot$bootOut
    INFO <- eList$INFO
    if (flux) {
      xFlux <- eBoot$xFlux
      change <- 100 * bootOut$estF/bootOut$baseFlux
      reps <- 100 * xFlux/bootOut$baseFlux
      xlabel <- "Flux trend, in %"
      titleWord <- "Flux"
    }
    else {
      xConc <- eBoot$xConc
      change <- 100 * bootOut$estC/bootOut$baseConc
      reps <- 100 * xConc/bootOut$baseConc
      xlabel <- "Concentration trend, in %"
      titleWord <- "Concentration"
    }
    titleToPrint <- ifelse(printTitle, paste("Histogram of trend in", 
                                             INFO$paramShortName, "\n", titleWord, "Normalized Concentration:", 
                                             caseSetUp$year1, "to", caseSetUp$year2, "\n", INFO$shortName), 
                           "")
    hist(reps, breaks = xSeq, yaxs = "i", xaxs = "i", tcl = 0.5, 
         main = titleToPrint, freq = FALSE, xlab = xlabel, col = col.fill, 
         cex.main = cex.main, xlim=xlim, ...)
    abline(v = change, lwd = 3, lty = 2)
    abline(v = 0, lwd = 3)
    box()
    axis(3, tcl = 0.5, labels = FALSE)
    axis(4, tcl = 0.5, labels = FALSE)
  }

tiff("histo_Sacramento_Freeport_Trend_SS_conc_flux.tif", height = 700, width = 1200, res=120)
par(mfrow=c(1,2))
plotHistogramTrend2(eBoot, caseSetUp, eList, flux=FALSE, xSeq = seq(-80000,80000,5),las=1,xlim=c(-100,100))
abline(h=0)

plotHistogramTrend2(eBoot, caseSetUp, eList, flux=TRUE, xSeq = seq(-50000,50000,5),las=1,xlim=c(-250,250))
abline(h=0)
dev.off()

nBoot <- 200
blockLength <- 200
coreOut <- 4 #Number of cores to leave out of processing tasks

widthCI <- 95
ciLower <- (50-(widthCI/2))/100
ciUpper <- (50+(widthCI/2))/100
probs <- c(ciLower,ciUpper)

nCores <- detectCores() - coreOut
cl <- makeCluster(nCores)
registerDoParallel(cl)
repAnnual <- foreach(n = 1:nBoot,.packages=c('EGRETci')) %dopar% {
  annualResults <- bootAnnual(eList, blockLength,startSeed = n)  
}
stopCluster(cl)

CIAnnualResults <- ciBands(eList, repAnnual, probs)
conc.poly.x <- c(CIAnnualResults$Year,rev(CIAnnualResults$Year))
conc.poly.y <- c(CIAnnualResults$FNConcLow,rev(CIAnnualResults$FNConcHigh))
flux.poly.x <- c(CIAnnualResults$Year,rev(CIAnnualResults$Year))
flux.poly.y <- c(CIAnnualResults$FNFluxLow*365,rev(CIAnnualResults$FNFluxHigh*365))

tiff("Sacramento_Freeport_Ann_Avg_Conc_&_Ann_Flow_Normalized_Conc_SS_Boot.tif", height = 500, width = 600, res=110)
plotConcHistBoot(eList, CIAnnualResults, plotFlowNorm=TRUE, showYLabels=TRUE, showYAxis=TRUE,col=4)
polygon(x=conc.poly.x, y=conc.poly.y, col=rgb(24,116,205,40,max=255),border=NA)
dev.off()

tiff("Sacramento_Freeport_Ann_Flux_&_Ann_Flow_Normalized_SS_Flux_Boot.tif", height = 500, width = 600, res=110)
plotFluxHistBoot(eList, fluxUnit=4, CIAnnualResults, plotFluxNorm = TRUE, showYLabels=TRUE, showYAxis=TRUE, col=4)
polygon(x=flux.poly.x, y=flux.poly.y, col=rgb(24,116,205,40,max=255),border=NA)
dev.off()
setSweave("Sacramento_Freeport_SSC_Conc_CI",7,7)
plotConcHistBoot(eList, CIAnnualResults, plotFlowNorm=TRUE, showYLabels=TRUE, showYAxis=TRUE,col=4)
graphics.off()
setSweave("Sacramento_Freeport_SSC_flux_CI",7,7)
plotFluxHistBoot(eList, fluxUnit=4, CIAnnualResults, plotFluxNorm = TRUE, showYLabels=TRUE, showYAxis=TRUE, col=4)
graphics.off()
saveEGRETci(eList, eBoot, fileName="N_Boot")
#save(repAnnual,file="RepAnnual")
write.csv(repAnnual,'repAnnual.csv')

#####################################
#Complete the EGRET analysis for NH3
#####################################

startDate    <- "1979-10-01"
endDate      <- "2019-04-30"
siteNumber   <- "11447650"
QParameterCd <- "00060"
parameterCd  <- "00608"  # "NH3"

Sample <- readNWISSample(siteNumber, parameterCd, startDate, endDate,verbose=TRUE)
#NWIS matches Kratzer, but has a bad data point from the BGC group.
#Download the retrieved data, delete the bad point, and re-enter using readUserSample
write.csv(Sample,"SacFreeportNH3.csv")
fileName <- "SacFreeportNH3.csv"
Sample <- readUserSample(filePath,fileName)
Sample       <- removeDuplicates(Sample)
fileName     <- "FreeportDaily_NH3_cfs.csv"
Daily        <- Daily <-readUserDaily(filePath, fileName, hasHeader = TRUE, separator = ",", qUnit = 1)
#Sample       <- readUserSample(filePath, fileName)


INFO <- readNWISInfo(siteNumber = siteNumber, parameterCd = parameterCd, interactive=FALSE)
INFO$staAbbrev <- paste(strsplit(INFO$station_nm," ")[[1]][1],strsplit(INFO$station_nm," ")[[1]][2])

# Have a look at the available range of WWC data
range(Sample$Date)
# "1979-10-17" "2019-04-03"
#No missing years
eList <- mergeReport(INFO, Daily, Sample)

# Change the working directory; redirect plot output to NH3 folder
setwd("C:/Users/dsaleh/Documents/GitHub/PES_Project/Freeport_EGRET/")
subDir <- 'NH3/EGRET_plots'
if (file.exists(subDir)){
  setwd(file.path(getwd(),subDir))
} else {
  dir.create(file.path(getwd(),subDir), recursive=TRUE)
  setwd(file.path(getwd(),subDir))
}

# write.csv(Sample,"Sample_NH3_Sacramento_Freeport.csv")
# Plot water quality data
tiff("Conc_vs_Time_Sacramento_Freeport_NH3.tif", height = 600, width = 800, res=120)
plotConcTime(eList)
dev.off()

# Now, a classic Q-C plot
tiff("Conc-Q_Sacramento_Freeport_NH3.tif", height = 600, width = 800, res=120)
plotConcQ(eList, logScale=TRUE)
dev.off()

# The data set as flux values rather than as concentrations
tiff("Flux-Q_Sacramento_Freeport_NH3.tif", height = 600, width = 800, res=120)
plotFluxQ(eList, fluxUnit=4)
dev.off()

# Monthly boxplots
tiff("Monthly-Conc_BoxPlots_Sacramento_Freeport_NH3.tif", height = 600, width = 800, res=120)
boxConcMonth(eList, logScale=TRUE)
dev.off()

# Flow on days sampled vs. all other days
tiff("Flow_on_days_sampled_vs_all_other_days_Sacramento_Freeport_NH3.tif", height = 600, width = 800, res=120)
boxQTwice(eList, qUnit=2)
dev.off()

#########################################
# Now start the Flow-Normalized Analysis
#########################################

# Build the regression model
eList <- modelEstimation(eList, windowY = 7, windowQ = 2, windowS = 0.5, minNumObs = 100, minNumUncen =50)
eList_NH3 <- eList

MonthlyResults <- calculateMonthlyResults(eList)

# Dump NH3-related flow-normalized data to text file for bringing together with other monitoring sites
paLong <- 12
paStart <- 10
localDaily <- getDaily(eList_NH3)
localAnnualResults <- setupYears(paStart = paStart, paLong = paLong, localDaily = localDaily)
write.table(localAnnualResults, file = 'SacFreeport_NH3_RawVals.txt', quote=FALSE, row.names=FALSE)
write.csv(localDaily,'localDailyNH3.csv')
# Plot the annual average concentration and annual flow-normalized concentration
tiff("Ann_Avg_Conc_&_Ann_Flow_Normalized_Conc_Sacramento_Freeport_NH3.tif", height = 600, width = 800, res=120)
plotConcHist(eList, plotFlowNorm=TRUE)
dev.off()

plotConcHist(eList, plotFlowNorm=TRUE,printTitle = F,cex.axis = 0.8)

# Plot the annual flux and annual flow-normalized flux
tiff("Ann_Flux_&_Ann_Flow_Normalized_Flux_Sacramento_Freeport_NH3.tif", height = 600, width = 800, res=120)
plotFluxHist(eList, plotFlowNorm = TRUE) # fluxMax) # fluxMax
dev.off()




#################################################################
# Determine which flow rates to use for discharge-specific trends

# Baseflow: mean of the annual 30-day low flows
baseQ <- mean(aggregate(Q30 ~ waterYear, data = localDaily, min)[,2])
baseQ_txt <- format(baseQ, digits=2)
baseQ_txt_cfs <- format(baseQ * 35.315, digits=2)

# mid-range: median flow rate across all years
medQ <- median(localDaily$Q)
medQ_txt <- format(medQ, digits=2)
medQ_txt_cfs <- format(medQ * 35.315, digits=2)

# high flow: get the 25% quantile of each year's maximum Q7
# This will help ensure (but not guarantee) that every year is well represented in the high-end flows
highQ7 <- as.numeric(quantile(aggregate(Q7 ~ waterYear, data = localDaily, max)[,2])[2])
highQ7_txt <- format(highQ7, digits=2)
highQ7_txt_cfs <- format(highQ7 * 35.315, digits=2)

# The following bit of script generates a figure discussed by Joe in an email on 6/2/17
# -------------------------------------------------------------------------------------

tiff("Discharge_specific_trends_SSC_centered_on_06-01.tif", height = 600, width = 1200, res=120)
par(mar=c(4,6,4.1,8))
plotConcTimeSmooth(eList, q1 = baseQ, q2 = medQ, q3 = highQ7, centerDate='06-01', 
                   yearStart=localDaily$waterYear[1], yearEnd=localDaily$waterYear[nrow(localDaily)], 
                   logScale=FALSE, printLegend=FALSE)

# Determine y position of the legend
# ----------------------------------
y_l <- par('usr')[3]
y_u <- par('usr')[4]
y_m <- mean(y_l, y_u)

# Use the top version of the legend to add cfs

# legend('bottomleft', c(eval(substitute(expression(paste('Baseflow [',baseQ_txt,' ', m^3~s^-1,'(',baseQ_txt_cfs,' ',ft^3~s^-1,')]',sep=' ')), list(baseQ_txt=baseQ_txt, baseQ_txt_cfs=baseQ_txt_cfs))),
#                        eval(substitute(expression(paste('Median Flow [',medQ_txt,' ', m^3~s^-1,'(',medQ_txt_cfs,' ',ft^3~s^-1,')]',sep=' ')), list(medQ_txt=medQ_txt, medQ_txt_cfs=medQ_txt_cfs))),
#                        eval(substitute(expression(paste('High Flow [',highQ7_txt,' ', m^3~s^-1,'(',highQ7_txt_cfs,' ',ft^3~s^-1,')]',sep=' ')), list(highQ7_txt=highQ7_txt, highQ7_txt_cfs=highQ7_txt_cfs)))), 
#                        col=c('black','red','green'), lwd=2, bg='white', bty='n')

legend('bottomleft', c(eval(substitute(expression(paste('Baseflow (',baseQ_txt,' ', m^3~s^-1,')',sep=' ')), list(baseQ_txt=baseQ_txt))),
                       eval(substitute(expression(paste('Median Flow (',medQ_txt,' ', m^3~s^-1,')',sep=' ')), list(medQ_txt=medQ_txt))),
                       eval(substitute(expression(paste('High Flow (',highQ7_txt,' ', m^3~s^-1,')',sep=' ')), list(highQ7_txt=highQ7_txt)))), 
       col=c('black','red','green'), lwd=2, bg='white', bty='n')

dev.off()

# Restore original plotting margins
par(mar=c(5.1,6.1,4.1,2.1))

# The following bit of script generates a figure discussed by Michael in an email on 6/2/17
# -----------------------------------------------------------------------------------------
# Get the row number corresponding to the maximum concentration for each year (bearing in mind that these are 'within group' row numbers)
out <- aggregate(ConcDay ~ waterYear, data = localDaily, which.max)

# Ensure data is ordered by water year
out <- out[ order(out$waterYear), ]

# Get a count of the number of days within each of the water years
tbl <- table(localDaily$waterYear)

# Return the absolute row positions for each water year's max concentration
out$AbsConcDay <- out$ConcDay + cumsum(c(0,tbl[-length(tbl)]))

# Make a data.frame containing only the rows with each water year's max conc
out2 <- localDaily[out$AbsConcDay,]

# Gather only the needed data
out2 <- data.frame(Date=out2$Date, Q=out2$Q, Conc=out2$ConcDay, wyr=out2$waterYear, Julian=yday(as.Date(out2$Date)))

# Need to readjust the Julian day to start on Oct 1 (This function doesn't yet account for leap years)
out2$JulianWYR <- ifelse(out2$Julian > 273, out2$Julian - 273, 92 + out2$Julian)

# Plot it
tiff("JulianDay_of_Max_SSC_Conc.tif", height = 600, width = 800, res=120)
plot(out2$wyr, out2$JulianWYR, pch=16, xlab='Water Year', ylab='Julian Day', yaxs='i', ylim=c(0,370), las=1)
dev.off()

# In a follow-up email from Michael on 6/7/17, Michael suggested two alterations:
#  1) Apply a 30-day moving average
#  2) Use the flow-normalized concentration

# To start with, I'll attempt to apply a 30-day window to the simulated daily concentrations

localDaily$ConcDay_30day <- c(rep(rollapply(localDaily$ConcDay, width=30, mean)[1],times=14) , rollapply(localDaily$ConcDay, width=30, mean), rep(rollapply(localDaily$ConcDay, width=30, mean)[length(rollapply(localDaily$ConcDay, width=30, mean))], times=15))
out_m <- aggregate(ConcDay_30day ~ waterYear, data = localDaily, which.max)
out_m <- out_m[ order(out_m$waterYear), ]
out_m$AbsConcDay <- out_m$ConcDay + cumsum(c(0,tbl[-length(tbl)]))
out_m2 <- localDaily[out_m$AbsConcDay,]
out_m2 <- data.frame(Date=out_m2$Date, Q=out_m2$Q, Conc30=out_m2$ConcDay_30day, wyr=out_m2$waterYear, Julian=yday(as.Date(out_m2$Date)))
out_m2$JulianWYR <- ifelse(out_m2$Julian > 273, out_m2$Julian - 273, 92 + out_m2$Julian)

tiff("JulianDay_of_Max_SSC_Conc_Using_30_rollingAvg.tif", height = 600, width = 800, res=120)
plot(out_m2$wyr, out_m2$JulianWYR, pch=16, xlab='Water Year', ylab='Julian Day', yaxs='i', ylim=c(0,370), las=1)
dev.off()


# Next, I'll try using the flow-normalized concentration (same general code flow as above)
# First, try plotting flow-normalized concentration:
# Plot it
tiff("Flow_Normalized_Conc_BC1_SSC.tif", height = 600, width = 800, res=120)
plot(as.Date(localDaily$Date), localDaily$FNConc, typ='l', las=1, xlab='Time', ylab='Flow-normalized Concentration')
dev.off()

out_FN <- aggregate(FNConc ~ waterYear, data = localDaily, which.max)
out_FN <- out_FN[ order(out_FN$waterYear), ]
out_FN$AbsConcDay <- out_FN$FNConc + cumsum(c(0,tbl[-length(tbl)]))
out2_FN <- localDaily[out_FN$AbsConcDay,]
out2_FN <- data.frame(Date=out2_FN$Date, Q=out2_FN$Q, Conc=out2_FN$FNConc, wyr=out2_FN$waterYear, Julian=yday(as.Date(out2_FN$Date)))
out2_FN$JulianWYR <- ifelse(out2_FN$Julian > 273, out2_FN$Julian - 273, 92 + out2_FN$Julian)

# Plot it
tiff("JulianDay_of_Max_SSC_Flow_Normalized_Conc.tif", height = 600, width = 800, res=120)
plot(out2_FN$wyr, out2_FN$JulianWYR, pch=16, xlab='Water Year', ylab='Julian Day', yaxs='i', ylim=c(0,370), las=1)
dev.off()



# --------------------------------------------------------------------------------------------------------
# The following script is for a non-standard EGRET plot and instead help generate a plot Michael requested

localDaily <- getDaily(eList)

# Will need to adjust the date range below based on each gages unique start/stop dates
early_decade <- subset(localDaily, localDaily$Date > as.Date('1980-09-30') & localDaily$Date < as.Date('1990-10-01'))
recent_decade <- subset(localDaily, localDaily$Date > as.Date('2009-05-02'))


early_decade_monthly_mn <- aggregate(ConcDay ~ MonthSeq, data = early_decade, 'mean')
recent_decade_monthly_mn <- aggregate(ConcDay ~ MonthSeq, data = recent_decade, 'mean')

# early_decade_monthly_mn$month <- format(seq(as.Date('1972-10-01'), as.Date('1982-09-30'), by='month'), '%b')
early_decade_monthly_mn$month <- rep(c(10:12,1:9), times=10)
early_decade_mon_mn <- aggregate(ConcDay ~ month, data = early_decade_monthly_mn, 'mean')
early_decade_mon_sd <- aggregate(ConcDay ~ month, data = early_decade_monthly_mn, 'sd')
early_decade_mon_mn <- early_decade_mon_mn[c(10:12,1:9),]
early_decade_mon_sd <- early_decade_mon_sd[c(10:12,1:9),]

recent_decade_monthly_mn$month <- rep(c(10:12,1:9), times=10)
recent_decade_mon_mn <- aggregate(ConcDay ~ month, data = recent_decade_monthly_mn, 'mean')
recent_decade_mon_sd <- aggregate(ConcDay ~ month, data = recent_decade_monthly_mn, 'sd')
recent_decade_mon_mn <- recent_decade_mon_mn[c(10:12,1:9),]
recent_decade_mon_sd <- recent_decade_mon_sd[c(10:12,1:9),]


mdat2 <- matrix(c(early_decade_mon_mn$ConcDay, recent_decade_mon_mn$ConcDay),
                nrow=2,ncol = 12, byrow=TRUE,
                dimnames = list(c("1980-1990", "2009-2018"),
                                c(format(seq(as.Date('1973-10-01'), as.Date('1974-09-01'), by='month'), '%b'))))

# Be sure to adjust the legend's first decade start and stop year correctly
mx <- max(c((early_decade_mon_mn$ConcDay + early_decade_mon_sd$ConcDay), (recent_decade_mon_mn$ConcDay + recent_decade_mon_sd$ConcDay)))

tiff("timing_shift_in_NH4_conc_monthly_means.tif", height=800, width=900, res=130)
par(mar=c(3,5,2,1))
x <- barplot(mdat2, beside=TRUE, las=1, ylim=c(0,mx), col = c("lightblue", "mistyrose"))
abline(h=0)
arrows(x0=x[1,], y0=early_decade_mon_mn$ConcDay - early_decade_mon_sd$ConcDay, x1=x[1,], y1=early_decade_mon_mn$ConcDay + early_decade_mon_sd$ConcDay, angle=90, length=0.04, code=3)
arrows(x0=x[2,], y0=recent_decade_mon_mn$ConcDay - recent_decade_mon_sd$ConcDay, x1=x[2,], y1=recent_decade_mon_mn$ConcDay + recent_decade_mon_sd$ConcDay, angle=90, length=0.04, code=3)
mtext(side=2, expression(paste(NH[3],', mg ',L^-1,sep='')), line=3)
legend(x=25, y=0.9 * mx, c("1980-1990", "2009-2019"), pch=c(22,22), pt.cex=2, pt.bg=c("lightblue", "mistyrose"), bty='n', xpd=TRUE)
dev.off()

pdf("timing_shift_in_NH4_conc_monthly_means.pdf")
x <- barplot(mdat2, beside=TRUE, las=1, ylim=c(0,mx), col = c("lightblue", "mistyrose"))
abline(h=0)
arrows(x0=x[1,], y0=early_decade_mon_mn$ConcDay - early_decade_mon_sd$ConcDay, x1=x[1,], y1=early_decade_mon_mn$ConcDay + early_decade_mon_sd$ConcDay, angle=90, length=0.04, code=3)
arrows(x0=x[2,], y0=recent_decade_mon_mn$ConcDay - recent_decade_mon_sd$ConcDay, x1=x[2,], y1=recent_decade_mon_mn$ConcDay + recent_decade_mon_sd$ConcDay, angle=90, length=0.04, code=3)
mtext(side=2, expression(paste(NH[3],', mg ',L^-1,sep='')), line=3)
legend(x=25, y=0.9 * mx, c("1980-1990", "2009-2019"), pch=c(22,22), pt.cex=2, pt.bg=c("lightblue", "mistyrose"), bty='n', xpd=TRUE)
dev.off()

# Now attempting a Wilcox Test (aka Mann-Whitney-Wilcoxon Rank Sum test)
# ----------------------------------------------------------------------
early_jan <- subset(early_decade_monthly_mn, month==1)
recent_jan <- subset(recent_decade_monthly_mn, month==1)
Sacramento_Freeport_NH4_conc_jan_wilcox <- wilcox.test(recent_jan$ConcDay, early_jan$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_feb <- subset(early_decade_monthly_mn, month==2)
recent_feb <- subset(recent_decade_monthly_mn, month==2)
Sacramento_Freeport_NH4_conc_feb_wilcox <- wilcox.test(recent_feb$ConcDay, early_feb$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_mar <- subset(early_decade_monthly_mn, month==3)
recent_mar <- subset(recent_decade_monthly_mn, month==3)
Sacramento_Freeport_NH4_conc_mar_wilcox <- wilcox.test(recent_mar$ConcDay, early_mar$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_apr <- subset(early_decade_monthly_mn, month==4)
recent_apr <- subset(recent_decade_monthly_mn, month==4)
Sacramento_Freeport_NH4_conc_apr_wilcox <- wilcox.test(recent_apr$ConcDay, early_apr$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_may <- subset(early_decade_monthly_mn, month==5)
recent_may <- subset(recent_decade_monthly_mn, month==5)
Sacramento_Freeport_NH4_conc_may_wilcox <- wilcox.test(recent_may$ConcDay, early_may$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_jun <- subset(early_decade_monthly_mn, month==6)
recent_jun <- subset(recent_decade_monthly_mn, month==6)
Sacramento_Freeport_NH4_conc_jun_wilcox <- wilcox.test(recent_jun$ConcDay, early_jun$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_jul <- subset(early_decade_monthly_mn, month==7)
recent_jul <- subset(recent_decade_monthly_mn, month==7)
Sacramento_Freeport_NH4_conc_jul_wilcox <- wilcox.test(recent_jul$ConcDay, early_jul$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_aug <- subset(early_decade_monthly_mn, month==8)
recent_aug <- subset(recent_decade_monthly_mn, month==8)
Sacramento_Freeport_NH4_conc_aug_wilcox <- wilcox.test(recent_aug$ConcDay, early_aug$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_sep <- subset(early_decade_monthly_mn, month==9)
recent_sep <- subset(recent_decade_monthly_mn, month==9)
Sacramento_Freeport_NH4_conc_sep_wilcox <- wilcox.test(recent_sep$ConcDay, early_sep$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_oct <- subset(early_decade_monthly_mn, month==10)
recent_oct <- subset(recent_decade_monthly_mn, month==10)
Sacramento_Freeport_NH4_conc_oct_wilcox <- wilcox.test(recent_oct$ConcDay, early_oct$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_nov <- subset(early_decade_monthly_mn, month==11)
recent_nov <- subset(recent_decade_monthly_mn, month==11)
Sacramento_Freeport_NH4_conc_nov_wilcox <- wilcox.test(recent_nov$ConcDay, early_nov$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_dec <- subset(early_decade_monthly_mn, month==12)
recent_dec <- subset(recent_decade_monthly_mn, month==12)
Sacramento_Freeport_NH4_conc_dec_wilcox <- wilcox.test(recent_dec$ConcDay, early_dec$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

Conc_compare <- data.frame(chng_est=c(Sacramento_Freeport_NH4_conc_oct_wilcox$est,
                                      Sacramento_Freeport_NH4_conc_nov_wilcox$est,
                                      Sacramento_Freeport_NH4_conc_dec_wilcox$est,
                                      Sacramento_Freeport_NH4_conc_jan_wilcox$est,
                                      Sacramento_Freeport_NH4_conc_feb_wilcox$est,
                                      Sacramento_Freeport_NH4_conc_mar_wilcox$est,
                                      Sacramento_Freeport_NH4_conc_apr_wilcox$est,
                                      Sacramento_Freeport_NH4_conc_may_wilcox$est,
                                      Sacramento_Freeport_NH4_conc_jun_wilcox$est,
                                      Sacramento_Freeport_NH4_conc_jul_wilcox$est,
                                      Sacramento_Freeport_NH4_conc_aug_wilcox$est,
                                      Sacramento_Freeport_NH4_conc_sep_wilcox$est),
                           low_conf=c(Sacramento_Freeport_NH4_conc_oct_wilcox$conf.int[1],
                                      Sacramento_Freeport_NH4_conc_nov_wilcox$conf.int[1],
                                      Sacramento_Freeport_NH4_conc_dec_wilcox$conf.int[1],
                                      Sacramento_Freeport_NH4_conc_jan_wilcox$conf.int[1],
                                      Sacramento_Freeport_NH4_conc_feb_wilcox$conf.int[1],
                                      Sacramento_Freeport_NH4_conc_mar_wilcox$conf.int[1],
                                      Sacramento_Freeport_NH4_conc_apr_wilcox$conf.int[1],
                                      Sacramento_Freeport_NH4_conc_may_wilcox$conf.int[1],
                                      Sacramento_Freeport_NH4_conc_jun_wilcox$conf.int[1],
                                      Sacramento_Freeport_NH4_conc_jul_wilcox$conf.int[1],
                                      Sacramento_Freeport_NH4_conc_aug_wilcox$conf.int[1],
                                      Sacramento_Freeport_NH4_conc_sep_wilcox$conf.int[1]),
                           up_conf=c(Sacramento_Freeport_NH4_conc_oct_wilcox$conf.int[2],
                                     Sacramento_Freeport_NH4_conc_nov_wilcox$conf.int[2],
                                     Sacramento_Freeport_NH4_conc_dec_wilcox$conf.int[2],
                                     Sacramento_Freeport_NH4_conc_jan_wilcox$conf.int[2],
                                     Sacramento_Freeport_NH4_conc_feb_wilcox$conf.int[2],
                                     Sacramento_Freeport_NH4_conc_mar_wilcox$conf.int[2],
                                     Sacramento_Freeport_NH4_conc_apr_wilcox$conf.int[2],
                                     Sacramento_Freeport_NH4_conc_may_wilcox$conf.int[2],
                                     Sacramento_Freeport_NH4_conc_jun_wilcox$conf.int[2],
                                     Sacramento_Freeport_NH4_conc_jul_wilcox$conf.int[2],
                                     Sacramento_Freeport_NH4_conc_aug_wilcox$conf.int[2],
                                     Sacramento_Freeport_NH4_conc_sep_wilcox$conf.int[2]))

write.table(Conc_compare, "Sacramento_Freeport_NH4_conc_wilcox.txt", quote=FALSE, row.names=FALSE)

rng <- max(abs(c(Conc_compare$up_conf, Conc_compare$low_conf)))
tiff("Sacramento_Freeport_NH4_conc_shift_wilcox_Vert_Bars.tif", height=600, width=800, res=130)
par(mar=c(4,5,0.5,0.5))
plot(seq(1:12), Conc_compare$chng_est, typ='h', lend=1, lwd=15, col='white', xaxt='n', xlim=c(1,13), ylim=c(-rng, rng), xlab="Month", ylab=expression(paste("Median Concentration Change, mg  ",L^-1,sep='')), las=1)
plotCI(seq(1:12), Conc_compare$chng_est, ui=Conc_compare$up_conf, li=Conc_compare$low_conf, pch=16, add=TRUE)
abline(h=0)
axis(side=1,at=seq(1,12,by=1), labels=format(c(seq(as.Date("2000-10-01"), as.Date("2000-12-01"), by="month"), seq(as.Date("2000-01-01"), as.Date("2000-09-01"), by="month")),'%b'), las=2)
legend('topright', c("Median difference", "90% Confidence Interval for the Median"), pch=c(16,NA), lwd=c(NA,1), pt.cex=c(1,NA), pt.bg=c('black',NA), bty='n', bg='white')
dev.off()

pdf("Sacramento_Freeport_NH4_conc_shift_wilcox_Vert_Bars.pdf")
plot(seq(1:12), Conc_compare$chng_est, typ='h', lend=1, lwd=15, col='white', xaxt='n', xlim=c(1,13), ylim=c(-rng, rng), xlab="Month", ylab=expression(paste("Median Concentration Change, mg  ",L^-1,sep='')), las=1)
plotCI(seq(1:12), Conc_compare$chng_est, ui=Conc_compare$up_conf, li=Conc_compare$low_conf, pch=16, add=TRUE)
abline(h=0)
axis(side=1,at=seq(1,12,by=1), labels=format(c(seq(as.Date("2000-10-01"), as.Date("2000-12-01"), by="month"), seq(as.Date("2000-01-01"), as.Date("2000-09-01"), by="month")),'%b'), las=2)
legend('topright', c("Median difference", "90% Confidence Interval for the Median"), pch=c(16,NA), lwd=c(NA,1), pt.cex=c(1,NA), pt.bg=c('black',NA), bty='n', bg='white')
dev.off()

# Now do the load
# ---------------
early_decade_monthly_flx <- aggregate(FluxDay ~ MonthSeq, data = early_decade, 'sum')
recent_decade_monthly_flx <- aggregate(FluxDay ~ MonthSeq, data = recent_decade, 'sum')

# early_decade_monthly_mn$month <- format(seq(as.Date('1972-10-01'), as.Date('1982-09-30'), by='month'), '%b')
early_decade_monthly_flx$month <- rep(c(10:12,1:9), times=10)
early_decade_mon_mn_flx <- aggregate(FluxDay ~ month, data = early_decade_monthly_flx, 'mean')
early_decade_mon_sd_flx <- aggregate(FluxDay ~ month, data = early_decade_monthly_flx, 'sd')
early_decade_mon_mn_flx <- early_decade_mon_mn_flx[c(10:12,1:9),]
early_decade_mon_sd_flx <- early_decade_mon_sd_flx[c(10:12,1:9),]

recent_decade_monthly_flx$month <- rep(c(10:12,1:9), times=10)
recent_decade_mon_mn_flx <- aggregate(FluxDay ~ month, data = recent_decade_monthly_flx, 'mean')
recent_decade_mon_sd_flx <- aggregate(FluxDay ~ month, data = recent_decade_monthly_flx, 'sd')
recent_decade_mon_mn_flx <- recent_decade_mon_mn_flx[c(10:12,1:9),]
recent_decade_mon_sd_flx <- recent_decade_mon_sd_flx[c(10:12,1:9),]

mdat3 <- matrix(c(early_decade_mon_mn_flx$FluxDay, recent_decade_mon_mn_flx$FluxDay),
                nrow=2,ncol = 12, byrow=TRUE,
                dimnames = list(c("1980-1990", "2009-2019"),
                                c(format(seq(as.Date('1973-10-01'), as.Date('1974-09-01'), by='month'), '%b'))))

mx <- max(c((early_decade_mon_mn_flx$FluxDay + early_decade_mon_sd_flx$FluxDay), (recent_decade_mon_mn_flx$FluxDay + recent_decade_mon_sd_flx$FluxDay)))
tiff("timing_shift_in_NH4_load_monthly_means.tif", height=800, width=900, res=130)
x <- barplot(mdat3, beside=TRUE, las=1, ylim=c(0,mx), col = c("lightblue", "mistyrose"))
abline(h=0)
arrows(x0=x[1,], y0=early_decade_mon_mn_flx$FluxDay - early_decade_mon_sd_flx$FluxDay, x1=x[1,], y1=early_decade_mon_mn_flx$FluxDay + early_decade_mon_sd_flx$FluxDay, angle=90, length=0.04, code=3)
arrows(x0=x[2,], y0=recent_decade_mon_mn_flx$FluxDay - recent_decade_mon_sd_flx$FluxDay, x1=x[2,], y1=recent_decade_mon_mn_flx$FluxDay + recent_decade_mon_sd_flx$FluxDay, angle=90, length=0.04, code=3)
mtext(side=2, expression(paste(NH[3],', kg ',month^-1,sep='')), line=2.5)
legend(x=30, y=0.9 * mx, c("1980-1990", "2009-2019"), pch=c(22,22), pt.cex=2, pt.bg=c("lightblue", "mistyrose"), bty='n', xpd=TRUE)
dev.off()

# Apply Wilcox.text to the monthly loads here...
early_jan_flx <- subset(early_decade_monthly_flx, month==1)
recent_jan_flx <- subset(recent_decade_monthly_flx, month==1)
Sacramento_Freeport_NH4_flux_jan_wilcox <- wilcox.test(recent_jan_flx$FluxDay, early_jan_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_feb_flx <- subset(early_decade_monthly_flx, month==2)
recent_feb_flx <- subset(recent_decade_monthly_flx, month==2)
Sacramento_Freeport_NH4_flux_feb_wilcox <- wilcox.test(recent_feb_flx$FluxDay, early_feb_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_mar_flx <- subset(early_decade_monthly_flx, month==3)
recent_mar_flx <- subset(recent_decade_monthly_flx, month==3)
Sacramento_Freeport_NH4_flux_mar_wilcox <- wilcox.test(recent_mar_flx$FluxDay, early_mar_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_apr_flx <- subset(early_decade_monthly_flx, month==4)
recent_apr_flx <- subset(recent_decade_monthly_flx, month==4)
Sacramento_Freeport_NH4_flux_apr_wilcox <- wilcox.test(recent_apr_flx$FluxDay, early_apr_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_may_flx <- subset(early_decade_monthly_flx, month==5)
recent_may_flx <- subset(recent_decade_monthly_flx, month==5)
Sacramento_Freeport_NH4_flux_may_wilcox <- wilcox.test(recent_may_flx$FluxDay, early_may_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_jun_flx <- subset(early_decade_monthly_flx, month==6)
recent_jun_flx <- subset(recent_decade_monthly_flx, month==6)
Sacramento_Freeport_NH4_flux_jun_wilcox <- wilcox.test(recent_jun_flx$FluxDay, early_jun_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_jul_flx <- subset(early_decade_monthly_flx, month==7)
recent_jul_flx <- subset(recent_decade_monthly_flx, month==7)
Sacramento_Freeport_NH4_flux_jul_wilcox <- wilcox.test(recent_jul_flx$FluxDay, early_jul_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_aug_flx <- subset(early_decade_monthly_flx, month==8)
recent_aug_flx <- subset(recent_decade_monthly_flx, month==8)
Sacramento_Freeport_NH4_flux_aug_wilcox <- wilcox.test(recent_aug_flx$FluxDay, early_aug_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_sep_flx <- subset(early_decade_monthly_flx, month==9)
recent_sep_flx <- subset(recent_decade_monthly_flx, month==9)
Sacramento_Freeport_NH4_flux_sep_wilcox <- wilcox.test(recent_sep_flx$FluxDay, early_sep_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_oct_flx <- subset(early_decade_monthly_flx, month==10)
recent_oct_flx <- subset(recent_decade_monthly_flx, month==10)
Sacramento_Freeport_NH4_flux_oct_wilcox <- wilcox.test(recent_oct_flx$FluxDay, early_oct_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_nov_flx <- subset(early_decade_monthly_flx, month==11)
recent_nov_flx <- subset(recent_decade_monthly_flx, month==11)
Sacramento_Freeport_NH4_flux_nov_wilcox <- wilcox.test(recent_nov_flx$FluxDay, early_nov_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_dec_flx <- subset(early_decade_monthly_flx, month==12)
recent_dec_flx <- subset(recent_decade_monthly_flx, month==12)
Sacramento_Freeport_NH4_flux_dec_wilcox <- wilcox.test(recent_dec_flx$FluxDay, early_dec_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)


Flux_compare <- data.frame(chng_est=c(Sacramento_Freeport_NH4_flux_oct_wilcox$est,
                                      Sacramento_Freeport_NH4_flux_nov_wilcox$est,
                                      Sacramento_Freeport_NH4_flux_dec_wilcox$est,
                                      Sacramento_Freeport_NH4_flux_jan_wilcox$est,
                                      Sacramento_Freeport_NH4_flux_feb_wilcox$est,
                                      Sacramento_Freeport_NH4_flux_mar_wilcox$est,
                                      Sacramento_Freeport_NH4_flux_apr_wilcox$est,
                                      Sacramento_Freeport_NH4_flux_may_wilcox$est,
                                      Sacramento_Freeport_NH4_flux_jun_wilcox$est,
                                      Sacramento_Freeport_NH4_flux_jul_wilcox$est,
                                      Sacramento_Freeport_NH4_flux_aug_wilcox$est,
                                      Sacramento_Freeport_NH4_flux_sep_wilcox$est),
                           low_conf=c(Sacramento_Freeport_NH4_flux_oct_wilcox$conf.int[1],
                                      Sacramento_Freeport_NH4_flux_nov_wilcox$conf.int[1],
                                      Sacramento_Freeport_NH4_flux_dec_wilcox$conf.int[1],
                                      Sacramento_Freeport_NH4_flux_jan_wilcox$conf.int[1],
                                      Sacramento_Freeport_NH4_flux_feb_wilcox$conf.int[1],
                                      Sacramento_Freeport_NH4_flux_mar_wilcox$conf.int[1],
                                      Sacramento_Freeport_NH4_flux_apr_wilcox$conf.int[1],
                                      Sacramento_Freeport_NH4_flux_may_wilcox$conf.int[1],
                                      Sacramento_Freeport_NH4_flux_jun_wilcox$conf.int[1],
                                      Sacramento_Freeport_NH4_flux_jul_wilcox$conf.int[1],
                                      Sacramento_Freeport_NH4_flux_aug_wilcox$conf.int[1],
                                      Sacramento_Freeport_NH4_flux_sep_wilcox$conf.int[1]),
                           up_conf=c(Sacramento_Freeport_NH4_flux_oct_wilcox$conf.int[2],
                                     Sacramento_Freeport_NH4_flux_nov_wilcox$conf.int[2],
                                     Sacramento_Freeport_NH4_flux_dec_wilcox$conf.int[2],
                                     Sacramento_Freeport_NH4_flux_jan_wilcox$conf.int[2],
                                     Sacramento_Freeport_NH4_flux_feb_wilcox$conf.int[2],
                                     Sacramento_Freeport_NH4_flux_mar_wilcox$conf.int[2],
                                     Sacramento_Freeport_NH4_flux_apr_wilcox$conf.int[2],
                                     Sacramento_Freeport_NH4_flux_may_wilcox$conf.int[2],
                                     Sacramento_Freeport_NH4_flux_jun_wilcox$conf.int[2],
                                     Sacramento_Freeport_NH4_flux_jul_wilcox$conf.int[2],
                                     Sacramento_Freeport_NH4_flux_aug_wilcox$conf.int[2],
                                     Sacramento_Freeport_NH4_flux_sep_wilcox$conf.int[2]))

write.table(Flux_compare, "Sacramento_Freeport_NH4_flux_wilcox.txt", quote=FALSE, row.names=FALSE)

rng_flx <- max(abs(c(Flux_compare$up_conf, Flux_compare$low_conf)))
tiff("Sacramento_Freeport_NH4_flux_shift_wilcox_Vert_Bars.tif", height=600, width=800, res=130)
par(mar=c(4,5,0.5,0.5))
plot(seq(1:12), Flux_compare$chng_est, typ='h', lend=1, lwd=15, col='white', xaxt='n', xlim=c(1,13), ylim=c(-rng_flx, rng_flx), xlab="Month", ylab=expression(paste("Median Flux Change, kg",sep='')), las=1)
plotCI(seq(1:12), Flux_compare$chng_est, ui=Flux_compare$up_conf, li=Flux_compare$low_conf, pch=16, add=TRUE)
abline(h=0)
axis(side=1,at=seq(1,12,by=1), labels=format(c(seq(as.Date("2000-10-01"), as.Date("2000-12-01"), by="month"), seq(as.Date("2000-01-01"), as.Date("2000-09-01"), by="month")),'%b'), las=2)
legend('topright', c("Median difference", "90% Confidence Interval for the Median"), pch=c(16,NA), lwd=c(NA,1), pt.cex=c(1,NA), pt.bg=c('black',NA), bty='n', bg='white')
dev.off()


# End of non-standard EGRET plot section requested by Michael
# --------------------------------------------------------------------------------------------------------

#################  Using the plotConcQSmooth function  ###########
#First do flow duration analysis
flowDuration(eList, centerDate = "06-01", qUnit = 2, span = 30)
date1 <- "1980-06-01"
date2 <- "2000-06-01"
date3 <- "2019-06-01"
qLow= baseQ
qHigh=highQ7

tiff("BC1_Date_Discharge_NH3_conc_no_log.tif",height = 700, width = 1000, res=120)
plotConcQSmooth(eList,date1, date2, date3,qLow, qHigh, logScale=FALSE,printLegend =TRUE,legendLeft=0,legendTop=0,printTitle=TRUE)
dev.off()



# Look for a trend change:
tableChange(eList_NH3, fluxUnit=6, yearPoints=c(1989, 1993, 2014))

#

#Generate out-of-the-box diagnostic plots
tiff("fluxBiasMulti_Sacramento_Freeport_NH3.tif", height = 1200, width = 1000, res=120)
fluxBiasMulti(eList, moreTitle = "WRTDS")
dev.off()

tiff("Modeled_Daily_Conc_wObservations_Sacramento_Freeport_NH3.tif", height = 800, width = 1000, res=120)
plotConcTimeDaily(eList)
dev.off()

# Exploring model behavior and adjusting model parameters
tiff("Contours_Sacramento_Freeport_NH3.tif", height = 700, width = 1000, res=120)
plotContours(eList, qBottom=100,qTop=2500,yearStart=1980,yearEnd=2019, contourLevels=seq(0,250,by=10), color.palette = colorRampPalette(c("violet", "purple", "blue", "cyan", "green", "yellow", "orange", "red"))) 
dev.off()

tiff("Log_Contours_Sacramento_Freeport_NH3.tif", height = 700, width = 1000, res=120)
plotContours(eList, qBottom=100, qTop=2500, yearStart=1980, yearEnd=2019, contourLevels=seq(0.5,5.8,by=0.1), color.palette = colorRampPalette(c("violet", "purple", "blue", "cyan", "green", "yellow", "orange", "red")), whatSurface=1) 
dev.off()

tiff("StdErr_of_Log_Contours_Sacramento_Freeport_SSC.tif", height = 700, width = 1000, res=120)
plotContours(eList, qBottom=100, qTop=2500, yearStart=1980, yearEnd=2019, contourLevels=seq(0.38,1.289,by=0.02), color.palette = colorRampPalette(c("violet", "purple", "blue", "cyan", "green", "yellow", "orange", "red")), whatSurface=2) 
dev.off()

tiff("Contours_Difference_NH3_Sacramento_Freeport.tif", height = 700, width = 1000, res=120)
plotDiffContours(eList, 1980,2019,100,2500,maxDiff=0.25)
dev.off()

tiff("Contours_PercentDifference_NH3_Sacramento_Freeport.tif", height = 700, width = 1000, res=120)
plotDiffContours(eList, 1980,2019,100,2500, maxDiff=100, plotPercent=TRUE)
dev.off()

tiff("Contours_PercentDifference2_NH3.tif", height = 700, width = 1000, res=120)
plotDiffContours2(eList, 1980,2019,100,2500, maxDiff=c(-100,100), plotPercent=TRUE, lwd=3, color.palette=colorRampPalette(c("purple","blue","cadetblue","lightblue","white","beige","yellow", "orange", "red","brown")),tick.lwd = 1)
dev.off()

Sample$WY <- trunc(Sample$DecYear+0.25) 
tiff("Monthly_Boxplot_Inorg_NH3_Sacramento_Freeport.tif", height = 700, width = 1000, res=120)
par(mar=c(4,6,0.5,0.5))
boxplot(Sample$ConcAve~Sample$WY,log="y",varwidth=TRUE,yaxs="i",xlab="Water Year",las=1) 
mtext(side=2, expression(paste("Concentration, Inorganic Nitrogen, in mg  ",L^-1,sep="")),line=4)
dev.off()

#####changepoint analysis
#NH3_changepoint <- read.table ("/Users/joed/LTIMP_TA/LTIMP_TA2/EGRET/BC1/NH3/EGRET_plots/BC1_NH3_RawVals.txt",header = TRUE)
#NH3_changepoint2<-as.numeric(NH3_changepoint$FNConc) 
#NH3changepoint2.binseg=cpt.meanvar(NH3_changepoint2,test.stat='Normal',method='PELT',param.estimates=TRUE,Q=5,penalty="SIC")
#cpts(NH3changepoint2.binseg)
#tiff("/Users/joed/LTIMP_TA/LTIMP_TA2/EGRET/BC1/NH3/EGRET_plots/BC1_NH3_Changepoint",height = 700, width = 1200, res=120)
#plot(NH3changepoint2.binseg,type='line',col="blue",ylim = c(0.0005,0.008))
#dev.off()
#NH3changepoint2.binseg
#plot(NH3changepoint2.binseg,type='line',col="blue",ylim=c(0.0005,0.008))

# ---------------------------
# Now run the EGRETci package
# ---------------------------

# Change working directory
setwd("..")
subDir <- 'EGRETci_plots'
if (file.exists(subDir)){
  setwd(file.path(getwd(),subDir))
} else {
  dir.create(file.path(getwd(),subDir), recursive=TRUE)
  setwd(file.path(getwd(),subDir))
}
caseSetUp <- trendSetUp(eList, 
                        year1=1980, 
                        year2=2018, 
                        nBoot = 200, 
                        bootBreak = 50, 
                        blockLength = 200)
eBoot <- wBT(eList, caseSetUp, fileName ="outputText.txt")

#Interactive function to set up trend analysis:


#Should we reject Ho that Flow Normalized Concentration Trend = 0 ? Reject Ho
#best estimate is 0.00247 mg/L
#Lower and Upper 90% CIs 0.001106 0.003191
#also 95% CIs0.000908 0.003378
#and 50% CIs 0.001970 0.002801
#* Note p-value should be considered to be < stated value
#approximate two-sided p-value for Conc      0.02
#Likelihood that Flow Normalized Concentration is trending up =      0.995 is trending down =    0.00495
#
#Should we reject Ho that Flow Normalized Flux Trend = 0 ? Reject Ho
#best estimate is 7.077e-05 10^6 kg/year
#Lower and Upper 90% CIs 3.66e-05 1.06e-04
#also 95% CIs 3.23e-05 1.31e-04
#and 50% CIs 5.96e-05 8.86e-05
#approximate two-sided p-value for Flux     0.028
#Likelihood that Flow Normalized Flux is trending up = 0.985 is trending down= 0.0149
#
#Upward trend in concentration is highly likely
#Upward trend in flux is highly likely
#Downward trend in concentration is highly unlikely
#Downward trend in flux is highly unlikely                                                    


saveEGRETci(eList, eBoot, caseSetUp, fileName = "Sacramento_Freeport_NH3_EGRETci_output")

plotHistogramTrend2 <-
  function (eBoot, caseSetUp, eList, xSeq = seq(-100, 100, 10), 
            flux = TRUE, printTitle = TRUE, cex.main = 1.1, col.fill = "grey", xlim = c(-100,100),
            ...) 
  {
    bootOut <- eBoot$bootOut
    INFO <- eList$INFO
    if (flux) {
      xFlux <- eBoot$xFlux
      change <- 100 * bootOut$estF/bootOut$baseFlux
      reps <- 100 * xFlux/bootOut$baseFlux
      xlabel <- "Flux trend, in %"
      titleWord <- "Flux"
    }
    else {
      xConc <- eBoot$xConc
      change <- 100 * bootOut$estC/bootOut$baseConc
      reps <- 100 * xConc/bootOut$baseConc
      xlabel <- "Concentration trend, in %"
      titleWord <- "Concentration"
    }
    titleToPrint <- ifelse(printTitle, paste("Histogram of trend in", 
                                             INFO$paramShortName, "\n", titleWord, "Normalized Concentration:", 
                                             caseSetUp$year1, "to", caseSetUp$year2, "\n", INFO$shortName), 
                           "")
    hist(reps, breaks = xSeq, yaxs = "i", xaxs = "i", tcl = 0.5, 
         main = titleToPrint, freq = FALSE, xlab = xlabel, col = col.fill, 
         cex.main = cex.main, xlim=xlim, ...)
    abline(v = change, lwd = 3, lty = 2)
    abline(v = 0, lwd = 3)
    box()
    axis(3, tcl = 0.5, labels = FALSE)
    axis(4, tcl = 0.5, labels = FALSE)
  }

tiff("histo_NH4_Sacramento_Freeport_Trend_NH3_conc_flux.tif", height = 700, width = 1200, res=120)
par(mfrow=c(1,2))
plotHistogramTrend2(eBoot, caseSetUp, eList, flux=FALSE, xSeq = seq(-80000,80000,5),las=1,xlim=c(-100,100))
abline(h=0)

plotHistogramTrend2(eBoot, caseSetUp, eList, flux=TRUE, xSeq = seq(-50000,50000,5),las=1,xlim=c(-250,250))
abline(h=0)
dev.off()

nBoot <- 200
blockLength <- 200
coreOut <- 4 #Number of cores to leave out of processing tasks

widthCI <- 95
ciLower <- (50-(widthCI/2))/100
ciUpper <- (50+(widthCI/2))/100
probs <- c(ciLower,ciUpper)

nCores <- detectCores() - coreOut
cl <- makeCluster(nCores)
registerDoParallel(cl)
repAnnual <- foreach(n = 1:nBoot,.packages=c('EGRETci')) %dopar% {
  annualResults <- bootAnnual(eList, blockLength,startSeed = n)  
}
stopCluster(cl)

CIAnnualResults <- ciBands(eList, repAnnual, probs)
conc.poly.x <- c(CIAnnualResults$Year,rev(CIAnnualResults$Year))
conc.poly.y <- c(CIAnnualResults$FNConcLow,rev(CIAnnualResults$FNConcHigh))
flux.poly.x <- c(CIAnnualResults$Year,rev(CIAnnualResults$Year))
flux.poly.y <- c(CIAnnualResults$FNFluxLow*365,rev(CIAnnualResults$FNFluxHigh*365))

tiff("Sacramento_Freeport_Ann_Avg_Conc_&_Ann_Flow_Normalized_Conc_NH3_Boot.tif", height = 500, width = 600, res=110)
plotConcHistBoot(eList, CIAnnualResults, plotFlowNorm=TRUE, showYLabels=TRUE, showYAxis=TRUE,col=4)
polygon(x=conc.poly.x, y=conc.poly.y, col=rgb(24,116,205,40,max=255),border=NA)
dev.off()

tiff("Sacramento_Freeport_Ann_Flux_&_Ann_Flow_Normalized_NH3_Flux_Boot.tif", height = 500, width = 600, res=110)
plotFluxHistBoot(eList, fluxUnit=4, CIAnnualResults, plotFluxNorm = TRUE, showYLabels=TRUE, showYAxis=TRUE, col=4)
polygon(x=flux.poly.x, y=flux.poly.y, col=rgb(24,116,205,40,max=255),border=NA)
dev.off()
setSweave("Sacramento_Freeport_NH3_Conc_CI",7,7)
plotConcHistBoot(eList, CIAnnualResults, plotFlowNorm=TRUE, showYLabels=TRUE, showYAxis=TRUE,col=4)
graphics.off()
setSweave("Sacramento_Freeport_NH3_flux_CI",7,7)
plotFluxHistBoot(eList, fluxUnit=4, CIAnnualResults, plotFluxNorm = TRUE, showYLabels=TRUE, showYAxis=TRUE, col=4)
graphics.off()
saveEGRETci(eList, eBoot, fileName="N_Boot")
#save(repAnnual,file="RepAnnual")
write.csv(repAnnual,'repAnnual.csv')


###
Sensordatetime_shifted <- read.csv('sensordatetime_Shifted.csv')
attach(Sensordatetime_shifted)
Rdate <- strptime(as.character(datetime),("%Y-%m-%d%H:%M"))
Sensordatetime_shifted <- data.frame(Sensordatetime_shifted,Rdate)

attach(Sensordatetime_shifted)
setSweave("SensorDateTime_Shifted",5,5)
timePlot(Rdate,NO3_Shifted,Plot=list(name="Freeport NO3 Sensor",what='lines',width='hairline',color='blue'))
graphics.off()

Nitrate_EGRET_Short <- read.csv('localDailyNO3_EGRET_Short.csv')
attach(Nitrate_EGRET_Short)
Rdate <- strptime(as.character(Date),("%Y-%m-%d"))
Nitrate_EGRET_Short <- data.frame(Nitrate_EGRET_Short,Rdate)

#####plot sensor nitrate

Nitrate_Sensor_Freeport <- read.csv('NO3_Sensor_2013_Present.csv')
attach(Nitrate_Sensor_Freeport)
#Rdate <- strptime(as.character(datetime),("%Y-%m-%d%H:%M"))
#Nitrate_Sensor_Freeport <-data.frame(Nitrate_Sensor_Freeport,Rdate)
#Nitrate_FullRecord_Freeport <- data.frame(Nitrate_FullRecord_Freeport,Rdate)

attach(Nitrate_Sensor_Freeport)
setSweave("Nitrate_full_Record_shifted",5,5)
timePlot(Rdate,Nitrate_Shifted,Plot=list(name="Freeport NO3 Sensor Shifted",what='lines',width='hairline',color='blue'))
attach(Nitrate_EGRET_Short)
points(Date,NO3_Daily_Conc,type='line')

graphics.off()

attach(Nitrate_Sensor_Freeport)
setSweave("Discharge_fullRecord",5,5)
timePlot(Rdate,Discharge,Plot=list(name="Freeport NO3 Sensor Shifted",what='points',symbol='circle',size=0.0005,color='blue'))
graphics.off()

###plot nitrate sensor and EGRET output
attach(Nitrate_Sensor_Freeport)
setSweave("Nitrate_full_Record_shifted_test",5,5)
plot(Rdate,Nitrate_Shifted,type="line",col="blue",ylim=c(0,1))
attach(Nitrate_EGRET_Short)
points(Rdate,NO3_Daily_Conc,type='line',col='red')
graphics.off()

attach(Nitrate_EGRET_Short)
setSweave("EGRET_NO3_test",5,5)
plot(Rdate,NO3_Daily_Conc,type='line',col='red')
graphics.off()

#########plot sensor Nitrate and Freeport discharge
attach(Nitrate_Sensor_Freeport)
setSweave("Nitrate_shifted_Discharge",5,5)
plot(Rdate,Discharge*0.0283169,type="line",lwd=0.15,xaxt = "n", yaxt = "n", ylab = "", xlab = "",ylim=c(-50,3000),col="gray74")
par(new = TRUE)
plot(Rdate,Nitrate_Shifted,type="line",col="blue",ylim=c(0,1))
axis(side=4)
mtext("Discharge, cubic meters per second",side=4,line=3)
legend("topleft",c("NO3 Concentration","Discharge"),col=c("blue","gray74"),lwd=c(0.5,0.5))
graphics.off()
########################
#####Now add load
#######################
Nitrate_Sensor_Freeport_Load <- read.csv('NO3_Sensor_2013_Present_withLoad.csv')
attach(Nitrate_Sensor_Freeport_Load)
#Rdate <- strptime(as.character(datetime),("%Y-%m-%d%H:%M"))
#Nitrate_Sensor_Freeport_Load <-data.frame(Nitrate_Sensor_Freeport_Load,Rdate)
setSweave("Nitrate_Sensor_Load",5,5)
plot(Rdate,Load_kg,type='line',col='aquamarine',ylim=c(-100,700))
par(new = TRUE)
plot(Rdate,Discharge_cms,type="line",lwd=0.15,xaxt = "n", yaxt = "n", ylab = "", xlab = "",ylim=c(-50,3000),col="black")
axis(side=4)
mtext("Discharge, cubic meters per second",side=4,line=3)
legend("topleft",c("NO3 Load, kg","Discharge, cubic meters per second"),col=c("aquamarine","black"),lwd=c(0.5,0.5))
graphics.off()





#####plot nitrate to OP ratio
Freeport_Nitrate_OP_ratio <- read.csv('Freeport_Nitrate_OP.csv')
attach(Freeport_Nitrate_OP_ratio)
#Rdate <- strptime(as.character(Date),("%Y-%m-%d"))
#Freeport_Nitrate_OP_ratio <- data.frame(Freeport_Nitrate_OP_ratio,Rdate)
setSweave ('Freeport_N_OP_Ratio',6,6)
plot(Rdate,NPRatio,type="line",col='forestgreen',lwd=0.75)
par(new = TRUE)

plot(Rdate,OPConcmm,type="line",lwd=0.75,xaxt = "n", yaxt = "n", ylab = "", xlab = "",ylim=c(0,0.03),col="orange")
points(Rdate,NO3concmm,type='line',col='blue',lwd=0.75,lty=2)
axis(side=4)
mtext("nutrient concentration",side=4,line=3)
legend("topleft",c("NP Ratio","Nitrate","OrthoP"),col=c("forestgreen","blue","orange"),lty=c(1,2,1),lwd=c(0.75,0.5,0.5))
graphics.off()

attach(Freeport_Nitrate_OP_ratio)
setSweave("NitratePRatioBox",6,6)
boxPlot(NPRatio,group=Month,Box=list(type='extended',fill="turquoise",ylabels = "Auto",xtitle="month",xtitle='month',ytitle = "NP Ratio",caption = "N P Ratio"))
addTitle("NPRatio")
graphics.off()

#####plot ammonium to nitrate ratio ratio

Freeport_Am_Nitrate_ratio <- read.csv("Freeport_Am_NO3Ratio.csv")
attach(Freeport_Am_Nitrate_ratio)
#Rdate <- strptime(as.character(Date),("%Y-%m-%d"))
#Freeport_Am_Nitrate_ratio <- data.frame(Freeport_Am_Nitrate_ratio,Rdate)
setSweave ('Freeport_NH3_NO3_Ratio',5,5)
plot(Rdate,AmNitrateRatio,type="line",col='forestgreen',ylim=c(0,4))
par(new = TRUE)

plot(Rdate,NH3ConcMM,type="line",lwd=0.75,xaxt = "n", yaxt = "n", ylab = "", xlab = "",ylim=c(0,0.025),col="red")
points(Rdate,NO3Concmm,type='line',col='blue',lwd=0.75)
axis(side=4)
mtext("nutrient concentration",side=4,line=3)
legend("topleft",c("NH3:NO3 Ratio","Ammonium","Nitrate"),col=c("forestgreen","red","blue"),lwd=c(0.75,0.5,0.5))
graphics.off()


attach(Freeport_Am_Nitrate_ratio)

qplot(Month, AmNitrateRatio, data = Freeport_Am_Nitrate_ratio,group=Month,ylim=c(0,5),xlim=c(0,13), geom= "boxplot")


############plot nitrate to ammonium ratio


#Freeport_Am_Nitrate_ratio <- read.csv("Freeport_Am_NO3Ratio.csv")
attach(Freeport_Am_Nitrate_ratio)
#Rdate <- strptime(as.character(Date),("%Y-%m-%d"))
#Freeport_Am_Nitrate_ratio <- data.frame(Freeport_Am_Nitrate_ratio,Rdate)
setSweave ('Freeport_N03_Nh4_Ratio',5,5)
plot(Rdate,(NO3Concmm/NH3ConcMM),type="line",lwd=0.75,col='forestgreen',ylim=c(0,30))
par(new = TRUE)

plot(Rdate,NH3ConcMM,type="line",lwd=0.5,xaxt = "n", yaxt = "n", ylab = "", xlab = "",ylim=c(0,0.025),col="red")
points(Rdate,NO3Concmm,type='line',col='blue',lwd=0.5)
axis(side=4)
mtext("nutrient concentration",side=4,line=3)
legend("topleft",c("NO3:NH4 Ratio","Ammonium","Nitrate"),col=c("forestgreen","red","blue"),lwd=c(0.75,0.5,0.5))
graphics.off()

#####plot TN TP ratio

Freeport_TN_TP_ratio <- read.csv("TNTNRatio.csv")
attach(Freeport_TN_TP_ratio)
#Rdate <- strptime(as.character(Date),("%Y-%m-%d"))
#Freeport_TN_TP_ratio <- data.frame(Freeport_TN_TP_ratio,Rdate)
setSweave ('Freeport_TN_TP_Ratio',5,5)
plot(Rdate,TNTPRatio,type="line",col='forestgreen',ylim=c(0,30))
par(new = TRUE)
plot(Rdate,(TKNConcmm+NO3Concmm),type="line",lwd=0.75,xaxt = "n", yaxt = "n", ylab = "", xlab = "",ylim=c(0,0.1),col="red")

points(Rdate,TPConcmm,type='line',col='blue',lwd=0.75)
axis(side=4)
mtext("nutrient concentration",side=4,line=3)
legend("topleft",c("TN:TP Ratio","Total Kjeldahl N","Total Phosphorus"),col=c("forestgreen","red","blue"),lwd=c(0.75,0.5,0.5))
graphics.off()

qplot(Month, TNTPRatio, data = Freeport_TN_TP_ratio,group=Month,ylim=c(0,30),xlim=c(0,13), geom= "boxplot")

########plot DIN OP Ratio
Freeport_DIN_Nitrate_ratio <- read.csv("Freeport_DIN_OP.csv")
attach(Freeport_DIN_Nitrate_ratio)
#Rdate <- strptime(as.character(Date),("%Y-%m-%d"))
#Freeport_DIN_Nitrate_ratio <- data.frame(Freeport_DIN_Nitrate_ratio,Rdate)
setSweave ('Freeport_DIN_OP_Ratio',5,5)
plot(Rdate,DINOPRatio,type="line",col='forestgreen',lwd=0.5,ylim=c(0,25))
par(new = TRUE)

plot(Rdate,NH3Concmm,type="line",lwd=0.5,xaxt = "n", yaxt = "n", ylab = "", xlab = "",ylim=c(0,0.025),col="red")
points(Rdate,NO3concmm,type='line',col='blue',lwd=0.5)
points(Rdate,OPConcmm,type='line',col='orange',lwd=0.5)
axis(side=4)
mtext("nutrient concentration",side=4,line=3)
legend("topleft",c("DIN:OP Ratio","Ammonium","Nitrate","orthophosphate"),col=c("forestgreen","red","blue","orange"),lwd=c(0.5,0.5,0.5,0.5))
graphics.off()
