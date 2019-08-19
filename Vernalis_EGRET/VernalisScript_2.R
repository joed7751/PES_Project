###This is a script taken from another study, but we will do the same thing for the PES project
##So, the script needs to be modifed for the Vernalis site

# Load required libraries
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


#####This script was set up for the Upper Truckee River, 
#so will need to change everything to be relevant to the 
#San Joaquin River at Vernalis site

##Make sure that siteid is changed everywhere , the one on line 32 is correct

# siteid: 11303500

# By not setting a fixed start and end date, readNWIS will retrieve all available data
# Will adjust to desired dates below.
StartDate <- "1971-10-01"       # 1980-03-01
EndDate <- "2019-06-01"

#site id is correct for Vernalis
# Get pure Q time series using the rloadest function (EGRET adds 0.1% of the period's mean discharge to 0 flow days
Q <- readNWISDaily("11303500",startDate=StartDate, endDate=EndDate)

# Look at flow record start and end dates
range(Q$Date)
#  "1971-10-01" "2019-06-01"

length(Q$Q[Q$Q==0])
# 0

# In write-up, make a comment about the number of 0-flow adjusted days, 
# or else include it in a table

# There are 17411 data points, and 17411 days.
# Now get Q data using EGRET function, site id is correct for Vernalis
siteNumber <- "11303500"
QParameterCd <- "00060"
Daily <- readNWISDaily(siteNumber, QParameterCd, StartDate, EndDate)



###### With the information above, restrict the start and end dates to avoid gaps in the data
#StartDate <- "1971-10-01"
#EndDate <- "2019-06-01"
#Daily <- readNWISDaily(siteNumber, QParameterCd, StartDate, EndDate)
############################################################################################

# Metadata retrieval, site id is correct for Vernalis, INFO file will have 1 obs (for one site) and 52 veriables for that site
INFO <- readNWISInfo(siteNumber = "11303500", parameterCd ="00060",interactive=FALSE)
INFO$staAbbrev <- paste(strsplit(INFO$station_nm," ")[[1]][1],strsplit(INFO$station_nm," ")[[1]][2])

eList <- as.egret(INFO, Daily, NA, NA)

# store the annual series of discharge statistics
annualSeries <- makeAnnualSeries(eList)


# -------------------------------
#setwd("/Users/joed/PES_Project/Vernalis_EGRET/") 


# Uncomment the following for Eric, and Dina will need to set this up for her path
# --------------------------------
setwd("C:/Users/dsaleh/Documents/GitHub/PES_Project/Vernalis_EGRET")

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
tiff("Annual_Seven_Day_Min_Flow_SJVernalis.tif", height = 600, width = 800, res=120)
plotFlowSingle(eList, istat = 2, qUnit = 2) #, showYLabels=FALSE)
dev.off()

tiff("Annual_Median_Daily_SJVernalis.tif", height = 600, width = 800, res=120)
plotFlowSingle(eList, istat = 4, qUnit = 2) #, showYLabels=FALSE)
dev.off()

tiff("Annual_Mean_Daily_SJVernalis.tif", height = 600, width = 800, res=120)
plotFlowSingle(eList, istat = 5, qUnit = 2) #, showYLabels=FALSE)
dev.off()

tiff("Annual_Mean_Daily_SJVernaliscms.tif", height = 600, width = 800, res=120)
plotFlowSingle(eList, istat = 5, qUnit = 2) #, showYLabels=FALSE)
dev.off()

tiff("Annual_30Day_Maximum_Q_SJVernalis.tif", height = 600, width = 800, res=120)
plotFlowSingle(eList, istat = 6, qUnit = 2) #, showYLabels=FALSE)
dev.off()

tiff("Annual_7Day_Maximum_Q_SJVernalis.tif", height = 600, width = 800, res=120)
plotFlowSingle(eList, istat = 7, qUnit = 2) #, showYLabels=FALSE)
dev.off()

tiff("Annual_Maximum_Q_SJVernalis.tif", height = 600, width = 800, res=120)
plotFlowSingle(eList, istat = 8, qUnit = 2) #, showYLabels=FALSE)
dev.off()

#Plot changes in variability
tiff("SD_Q_window3_SJVernalis.tif", height = 600, width = 800, res=120)
plotSDLogQ(eList, window=3)
dev.off()

#plot daily flow
tiff("Period_Of_Record_Hydrograph_SJVernalis.tif", height = 600, width = 800, res=120)
plotQTimeDaily(eList, lwd = 1,qUnit = 2) #, showYLabels=FALSE)
dev.off()

#plot daily flow
postscript("Period_Of_Record_Hydrograph_SJVernalis.ps", family="Courier", height=3.25, width=3.25)
par(mar=c(3,5,1,1))
plotQTimeDaily(eList,cex.axis =0.65,cex.main = 0.65, cex = 0.9,lwd = 0.25,col = "blue", qUnit = 2,cex.lab=0.75) #, showYLabels=FALSE)
#mtext(side=2, expression(paste("Flow, in  ",ft^3," ",s^-1,sep="")),line=3)
dev.off()


tiff("Period_Of_Record_Hydrograph_LogScale_SJVernalis.tif", height = 600, width = 800, res=120)
plotQTimeDaily(eList, lwd = 1,qUnit = 2,logScale=TRUE) #, showYLabels=FALSE)
dev.off()

#plot several graphics at once
tiff("Plot4_window3_SJVernalis.tif", height = 600, width = 800, res=120)
plotFour(eList, window=3,qUnit = 2)
dev.off()


#############################################
# Look at trends at specific flow rates
#############################################

eListNext1 <- setPA(eList, paStart = 1, paLong = 2)
annualSeries <- makeAnnualSeries(eListNext1)
tiff("Jan-Feb_Flow_Analysis.tif", height = 600, width = 800, res=120)
plotFour(eListNext1, qUnit=2, window=3)
dev.off()

# Try March/April
eListNext2 <- setPA(eList, paStart = 3, paLong = 2)
annualSeries <- makeAnnualSeries(eListNext2)
tiff("Mar_Apr_Flow_Analysis.tif", height = 600, width = 800, res=120)
plotFour(eListNext2, qUnit=2,window=3)
dev.off()

eListNext3 <- setPA(eList, paStart = 5, paLong = 2)
annualSeries <- makeAnnualSeries(eListNext3)
tiff("May-Jun_Flow_Analysis.tif", height = 600, width = 800, res=120)
plotFour(eListNext3, qUnit=2,window=3)
dev.off()

eListNext4 <- setPA(eList, paStart = 1, paLong = 4)
annualSeries <- makeAnnualSeries(eListNext4)
tiff("Jan-Apr_Flow_Analysis.tif", height = 600, width = 800, res=120)
plotFour(eListNext4, qUnit=2,window=3)
dev.off()

eListNext5 <- setPA(eList, paStart = 4, paLong = 2)
annualSeries <- makeAnnualSeries(eListNext5)
tiff("Apr-May_Flow_Analysis.tif", height = 600, width = 800, res=120)
plotFour(eListNext5, qUnit=2,window=3)
dev.off()

#Try summer months
eListNext6 <- setPA(eList, paStart = 4, paLong = 5)
annualSeries <- makeAnnualSeries(eListNext6)
tiff("Apr-Sep_Flow_Analysis.tif", height = 600, width = 800, res=120)
plotFour(eListNext6, qUnit=2,window=3)
dev.off()

#Try fall, when flows may show a decrease with time as ag returns would be diminished under Ag
# dry up.
eListNext7 <- setPA(eList, paStart = 9, paLong = 3)
annualSeries <- makeAnnualSeries(eListNext7)
tiff("Sep-Nov_Flow_Analysis.tif", height = 600, width = 800, res=120)
plotFour(eListNext7, qUnit=2,window=3)
dev.off()

# Note that this approach of using alternatively named 
# eList's leaves the original intact so that we can use
# it below without fear of altering it from it's paStart=10
# and paLong = 12 values.  However, we need to set this up:

eList <- setPA(eList, paStart = 10, paLong = 12)
annualSeries <- makeAnnualSeries(eList)

#############################################################
# Now do the analysis for DIN: NO3 plus NO2
#############################################################

###site id is correct for Vernalis

startDate <- "1971-03-01"
endDate <- "2019-06-01"
siteNumber <- "11303500"
QParameterCd <- "00060"
parameterCd <- "00631"  # "NO3 and NO2"
#filePath <- "/Users/joed/PES_Project/Vernalis_EGRET/"

##Will need to change filePath for Dina's computer
filePath <- "C:/Users/dsaleh/Documents/GitHub/PES_Project/Vernalis_EGRET/"
setwd("C:/Users/dsaleh/Documents/GitHub/PES_Project/Vernalis_EGRET")
#setwd("/Users/joed/PES_Project/Vernalis_EGRET/")
Daily <- readNWISDaily(siteNumber, QParameterCd, startDate, endDate)
#Sample <- readNWISSample(siteNumber, parameterCd, startDate, endDate)
##NWIS DIN data has a gap between 1974 and 1979.  We will need to supplement
##the missing time using Charlie Kratzer's data
#write.csv(Sample,"NWIS_nitrate.csv")

##Add Kratzer's data to NWIS_nitrate2.csv Use this for all model runs
fileName <- "NWIS_nitrate2.csv"
Sample <- readUserSample(filePath, fileName)
Sample <- removeDuplicates(Sample)
write.csv(Sample,'Sample_NO3.csv')
#Sample <- readNWISSample(siteNumber, parameterCd, startDate, endDate)
INFO <- readNWISInfo(siteNumber = siteNumber, parameterCd = parameterCd, interactive=FALSE)
INFO$staAbbrev <- paste(strsplit(INFO$station_nm," ")[[1]][1],strsplit(INFO$station_nm," ")[[1]][2])

# Have a look at the available range of NO3 data
range(Sample$Date)
#  "1971-03-02" "2019-05-07"

eList <- mergeReport(INFO, Daily, Sample)

# Change the working directory; redirect plot output to NO3 folder
setwd("C:/Users/dsaleh/Documents/GitHub/PES_Project/Vernalis_EGRET")
setwd("/Users/joed/PES_Project/Vernalis_EGRET/")

subDir <- 'NO3/EGRET_plots'
if (file.exists(subDir)){
  setwd(file.path(getwd(),subDir))
} else {
  dir.create(file.path(getwd(),subDir), recursive = TRUE)
  setwd(file.path(getwd(),subDir))
}
plotConcTimeDaily(eList)

# Plot water quality data
tiff("Conc_vs_Time_SanJVernalis.tif", height = 600, width = 800, res=120)
plotConcTime(eList)
dev.off()

# Now, a classic Q-C plot
tiff("Conc-Q_SanJVernalis_Inorg_N.tif", height = 600, width = 800, res=120)
plotConcQ(eList, logScale=TRUE)
dev.off()

# The data set as flux values rather than as concentrations
tiff("Flux-Q_SanJVernalis_Inorg_N.tif", height = 600, width = 800, res=120)
plotFluxQ(eList, fluxUnit=4)
dev.off()

# Monthly boxplots
tiff("Monthly-Conc_BoxPlots_SanJVernalis_Inorg_N.tif", height = 600, width = 800, res=120)
boxConcMonth(eList, logScale=TRUE)
dev.off()

# Flow on days sampled vs. all other days
tiff("Flow_on_days_sampled_vs_all_other_days_SanJVernalis_Inorg_N.tif", height = 600, width = 800, res=120)
boxQTwice(eList, qUnit=2)
dev.off()
###########################################################################################################
#########################################
# Now start the Flow-Normalized Analysis
#########################################

# Build the regression model
eList <- modelEstimation(eList, windowY = 7, windowQ = 2, windowS = 0.5, minNumObs = 100, minNumUncen =50)
MonthlyResults <- calculateMonthlyResults(eList)


# Dump NO3-related flow-normalized data to text file for bringing together with other monitoring sites
paLong <- 12
paStart <- 10
localDaily <- getDaily(eList)
localAnnualResults <- setupYears(paStart = paStart, paLong = paLong, localDaily = localDaily)
write.table(localAnnualResults, file = 'SanJ_Vernalis_NO3_RawVals.txt', quote=FALSE, row.names=FALSE)
write.csv(Daily,'localDailyNO3.csv')

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
                   logScale=TRUE, printLegend=TRUE)

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
tiff("Flow_Normalized_Conc_TC1_NO3.tif", height = 600, width = 800, res=120)
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

#Will need to adjust the date range below based on each gages unique start/stop dates
#early_decade <- subset(localDaily, localDaily$Date > as.Date('1972-09-30') & localDaily$Date < as.Date('1982-10-01'))
#recent_decade <- subset(localDaily, localDaily$Date > as.Date('2009-06-01'))
early_decade <- subset(localDaily, localDaily$Date > as.Date('1971-10-01') & localDaily$Date < as.Date('1981-10-01'))
recent_decade <- subset(localDaily, localDaily$Date > as.Date('2009-07-01'))

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
                dimnames = list(c("1972-1982", "2009-2019"),
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
legend(x=25, y=0.9 * mx, c("1971-1981", "2009-2019"), pch=c(22,22), pt.cex=2, pt.bg=c("lightblue", "mistyrose"), bty='n', xpd=TRUE)
dev.off()


# Now attempting a Wilcox Test (aka Mann-Whitney-Wilcoxon Rank Sum test)
# ----------------------------------------------------------------------
early_jan <- subset(early_decade_monthly_mn, month==1)
recent_jan <- subset(recent_decade_monthly_mn, month==1)
SanJ_Vernalis_NO3_conc_jan_wilcox <- wilcox.test(recent_jan$ConcDay, early_jan$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_feb <- subset(early_decade_monthly_mn, month==2)
recent_feb <- subset(recent_decade_monthly_mn, month==2)
SanJ_Vernalis_NO3_conc_feb_wilcox <- wilcox.test(recent_feb$ConcDay, early_feb$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_mar <- subset(early_decade_monthly_mn, month==3)
recent_mar <- subset(recent_decade_monthly_mn, month==3)
SanJ_Vernalis_NO3_conc_mar_wilcox <- wilcox.test(recent_mar$ConcDay, early_mar$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_apr <- subset(early_decade_monthly_mn, month==4)
recent_apr <- subset(recent_decade_monthly_mn, month==4)
SanJ_Vernalis_NO3_conc_apr_wilcox <- wilcox.test(recent_apr$ConcDay, early_apr$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_may <- subset(early_decade_monthly_mn, month==5)
recent_may <- subset(recent_decade_monthly_mn, month==5)
SanJ_Vernalis_NO3_conc_may_wilcox <- wilcox.test(recent_may$ConcDay, early_may$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_jun <- subset(early_decade_monthly_mn, month==6)
recent_jun <- subset(recent_decade_monthly_mn, month==6)
SanJ_Vernalis_NO3_conc_jun_wilcox <- wilcox.test(recent_jun$ConcDay, early_jun$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_jul <- subset(early_decade_monthly_mn, month==7)
recent_jul <- subset(recent_decade_monthly_mn, month==7)
SanJ_Vernalis_NO3_conc_jul_wilcox <- wilcox.test(recent_jul$ConcDay, early_jul$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_aug <- subset(early_decade_monthly_mn, month==8)
recent_aug <- subset(recent_decade_monthly_mn, month==8)
SanJ_Vernalis_NO3_conc_aug_wilcox <- wilcox.test(recent_aug$ConcDay, early_aug$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_sep <- subset(early_decade_monthly_mn, month==9)
recent_sep <- subset(recent_decade_monthly_mn, month==9)
SanJ_Vernalis_NO3_conc_sep_wilcox <- wilcox.test(recent_sep$ConcDay, early_sep$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_oct <- subset(early_decade_monthly_mn, month==10)
recent_oct <- subset(recent_decade_monthly_mn, month==10)
SanJ_Vernalis_NO3_conc_oct_wilcox <- wilcox.test(recent_oct$ConcDay, early_oct$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_nov <- subset(early_decade_monthly_mn, month==11)
recent_nov <- subset(recent_decade_monthly_mn, month==11)
SanJ_Vernalis_NO3_conc_nov_wilcox <- wilcox.test(recent_nov$ConcDay, early_nov$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_dec <- subset(early_decade_monthly_mn, month==12)
recent_dec <- subset(recent_decade_monthly_mn, month==12)
SanJ_Vernalis_NO3_conc_dec_wilcox <- wilcox.test(recent_dec$ConcDay, early_dec$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

Conc_compare <- data.frame(chng_est=c(SanJ_Vernalis_NO3_conc_oct_wilcox$est,
                                      SanJ_Vernalis_NO3_conc_nov_wilcox$est,
                                      SanJ_Vernalis_NO3_conc_dec_wilcox$est,
                                      SanJ_Vernalis_NO3_conc_jan_wilcox$est,
                                      SanJ_Vernalis_NO3_conc_feb_wilcox$est,
                                      SanJ_Vernalis_NO3_conc_mar_wilcox$est,
                                      SanJ_Vernalis_NO3_conc_apr_wilcox$est,
                                      SanJ_Vernalis_NO3_conc_may_wilcox$est,
                                      SanJ_Vernalis_NO3_conc_jun_wilcox$est,
                                      SanJ_Vernalis_NO3_conc_jul_wilcox$est,
                                      SanJ_Vernalis_NO3_conc_aug_wilcox$est,
                                      SanJ_Vernalis_NO3_conc_sep_wilcox$est),
                           low_conf=c(SanJ_Vernalis_NO3_conc_oct_wilcox$conf.int[1],
                                      SanJ_Vernalis_NO3_conc_nov_wilcox$conf.int[1],
                                      SanJ_Vernalis_NO3_conc_dec_wilcox$conf.int[1],
                                      SanJ_Vernalis_NO3_conc_jan_wilcox$conf.int[1],
                                      SanJ_Vernalis_NO3_conc_feb_wilcox$conf.int[1],
                                      SanJ_Vernalis_NO3_conc_mar_wilcox$conf.int[1],
                                      SanJ_Vernalis_NO3_conc_apr_wilcox$conf.int[1],
                                      SanJ_Vernalis_NO3_conc_may_wilcox$conf.int[1],
                                      SanJ_Vernalis_NO3_conc_jun_wilcox$conf.int[1],
                                      SanJ_Vernalis_NO3_conc_jul_wilcox$conf.int[1],
                                      SanJ_Vernalis_NO3_conc_aug_wilcox$conf.int[1],
                                      SanJ_Vernalis_NO3_conc_sep_wilcox$conf.int[1]),
                           up_conf=c(SanJ_Vernalis_NO3_conc_oct_wilcox$conf.int[2],
                                     SanJ_Vernalis_NO3_conc_nov_wilcox$conf.int[2],
                                     SanJ_Vernalis_NO3_conc_dec_wilcox$conf.int[2],
                                     SanJ_Vernalis_NO3_conc_jan_wilcox$conf.int[2],
                                     SanJ_Vernalis_NO3_conc_feb_wilcox$conf.int[2],
                                     SanJ_Vernalis_NO3_conc_mar_wilcox$conf.int[2],
                                     SanJ_Vernalis_NO3_conc_apr_wilcox$conf.int[2],
                                     SanJ_Vernalis_NO3_conc_may_wilcox$conf.int[2],
                                     SanJ_Vernalis_NO3_conc_jun_wilcox$conf.int[2],
                                     SanJ_Vernalis_NO3_conc_jul_wilcox$conf.int[2],
                                     SanJ_Vernalis_NO3_conc_aug_wilcox$conf.int[2],
                                     SanJ_Vernalis_NO3_conc_sep_wilcox$conf.int[2]))

write.table(Conc_compare, "SanJ_Vernalis_NO3_conc_wilcox.txt", quote=FALSE, row.names=FALSE)

rng <- max(abs(c(Conc_compare$up_conf, Conc_compare$low_conf)))
tiff("SanJ_Vernalis_NO3_conc_shift_wilcox_Vert_Bars.tif", height=600, width=800, res=130)
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
                dimnames = list(c("1972-1982", "2009-2019"),
                                c(format(seq(as.Date('1973-10-01'), as.Date('1974-09-01'), by='month'), '%b'))))

mx <- max(c((early_decade_mon_mn_flx$FluxDay + early_decade_mon_sd_flx$FluxDay), (recent_decade_mon_mn_flx$FluxDay + recent_decade_mon_sd_flx$FluxDay)))
tiff("timing_shift_in_NO3_load_monthly_means.tif", height=800, width=900, res=130)
x <- barplot(mdat3, beside=TRUE, las=1, ylim=c(0,mx), col = c("lightblue", "mistyrose"))
abline(h=0)
arrows(x0=x[1,], y0=early_decade_mon_mn_flx$FluxDay - early_decade_mon_sd_flx$FluxDay, x1=x[1,], y1=early_decade_mon_mn_flx$FluxDay + early_decade_mon_sd_flx$FluxDay, angle=90, length=0.04, code=3)
arrows(x0=x[2,], y0=recent_decade_mon_mn_flx$FluxDay - recent_decade_mon_sd_flx$FluxDay, x1=x[2,], y1=recent_decade_mon_mn_flx$FluxDay + recent_decade_mon_sd_flx$FluxDay, angle=90, length=0.04, code=3)
mtext(side=2, expression(paste(NO[3],', kg ',month^-1,sep='')), line=2.5)
legend(x=30, y=0.9 * mx, c("1971-1981", "2009-2019"), pch=c(22,22), pt.cex=2, pt.bg=c("lightblue", "mistyrose"), bty='n', xpd=TRUE)
dev.off()

# Apply Wilcox.text to the monthly loads here...
early_jan_flx <- subset(early_decade_monthly_flx, month==1)
recent_jan_flx <- subset(recent_decade_monthly_flx, month==1)
SanJ_Vernalis_NO3_flux_jan_wilcox <- wilcox.test(recent_jan_flx$FluxDay, early_jan_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_feb_flx <- subset(early_decade_monthly_flx, month==2)
recent_feb_flx <- subset(recent_decade_monthly_flx, month==2)
SanJ_Vernalis_NO3_flux_feb_wilcox <- wilcox.test(recent_feb_flx$FluxDay, early_feb_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_mar_flx <- subset(early_decade_monthly_flx, month==3)
recent_mar_flx <- subset(recent_decade_monthly_flx, month==3)
SanJ_Vernalis_NO3_flux_mar_wilcox <- wilcox.test(recent_mar_flx$FluxDay, early_mar_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_apr_flx <- subset(early_decade_monthly_flx, month==4)
recent_apr_flx <- subset(recent_decade_monthly_flx, month==4)
SanJ_Vernalis_NO3_flux_apr_wilcox <- wilcox.test(recent_apr_flx$FluxDay, early_apr_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_may_flx <- subset(early_decade_monthly_flx, month==5)
recent_may_flx <- subset(recent_decade_monthly_flx, month==5)
SanJ_Vernalis_NO3_flux_may_wilcox <- wilcox.test(recent_may_flx$FluxDay, early_may_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_jun_flx <- subset(early_decade_monthly_flx, month==6)
recent_jun_flx <- subset(recent_decade_monthly_flx, month==6)
SanJ_Vernalis_NO3_flux_jun_wilcox <- wilcox.test(recent_jun_flx$FluxDay, early_jun_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_jul_flx <- subset(early_decade_monthly_flx, month==7)
recent_jul_flx <- subset(recent_decade_monthly_flx, month==7)
SanJ_Vernalis_NO3_flux_jul_wilcox <- wilcox.test(recent_jul_flx$FluxDay, early_jul_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_aug_flx <- subset(early_decade_monthly_flx, month==8)
recent_aug_flx <- subset(recent_decade_monthly_flx, month==8)
SanJ_Vernalis_NO3_flux_aug_wilcox <- wilcox.test(recent_aug_flx$FluxDay, early_aug_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_sep_flx <- subset(early_decade_monthly_flx, month==9)
recent_sep_flx <- subset(recent_decade_monthly_flx, month==9)
SanJ_Vernalis_NO3_flux_sep_wilcox <- wilcox.test(recent_sep_flx$FluxDay, early_sep_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_oct_flx <- subset(early_decade_monthly_flx, month==10)
recent_oct_flx <- subset(recent_decade_monthly_flx, month==10)
SanJ_Vernalis_NO3_flux_oct_wilcox <- wilcox.test(recent_oct_flx$FluxDay, early_oct_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_nov_flx <- subset(early_decade_monthly_flx, month==11)
recent_nov_flx <- subset(recent_decade_monthly_flx, month==11)
SanJ_Vernalis_NO3_flux_nov_wilcox <- wilcox.test(recent_nov_flx$FluxDay, early_nov_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_dec_flx <- subset(early_decade_monthly_flx, month==12)
recent_dec_flx <- subset(recent_decade_monthly_flx, month==12)
SanJ_Vernalis_NO3_flux_dec_wilcox <- wilcox.test(recent_dec_flx$FluxDay, early_dec_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)


Flux_compare <- data.frame(chng_est=c(SanJ_Vernalis_NO3_flux_oct_wilcox$est,
                                      SanJ_Vernalis_NO3_flux_nov_wilcox$est,
                                      SanJ_Vernalis_NO3_flux_dec_wilcox$est,
                                      SanJ_Vernalis_NO3_flux_jan_wilcox$est,
                                      SanJ_Vernalis_NO3_flux_feb_wilcox$est,
                                      SanJ_Vernalis_NO3_flux_mar_wilcox$est,
                                      SanJ_Vernalis_NO3_flux_apr_wilcox$est,
                                      SanJ_Vernalis_NO3_flux_may_wilcox$est,
                                      SanJ_Vernalis_NO3_flux_jun_wilcox$est,
                                      SanJ_Vernalis_NO3_flux_jul_wilcox$est,
                                      SanJ_Vernalis_NO3_flux_aug_wilcox$est,
                                      SanJ_Vernalis_NO3_flux_sep_wilcox$est),
                           low_conf=c(SanJ_Vernalis_NO3_flux_oct_wilcox$conf.int[1],
                                      SanJ_Vernalis_NO3_flux_nov_wilcox$conf.int[1],
                                      SanJ_Vernalis_NO3_flux_dec_wilcox$conf.int[1],
                                      SanJ_Vernalis_NO3_flux_jan_wilcox$conf.int[1],
                                      SanJ_Vernalis_NO3_flux_feb_wilcox$conf.int[1],
                                      SanJ_Vernalis_NO3_flux_mar_wilcox$conf.int[1],
                                      SanJ_Vernalis_NO3_flux_apr_wilcox$conf.int[1],
                                      SanJ_Vernalis_NO3_flux_may_wilcox$conf.int[1],
                                      SanJ_Vernalis_NO3_flux_jun_wilcox$conf.int[1],
                                      SanJ_Vernalis_NO3_flux_jul_wilcox$conf.int[1],
                                      SanJ_Vernalis_NO3_flux_aug_wilcox$conf.int[1],
                                      SanJ_Vernalis_NO3_flux_sep_wilcox$conf.int[1]),
                           up_conf=c(SanJ_Vernalis_NO3_flux_oct_wilcox$conf.int[2],
                                     SanJ_Vernalis_NO3_flux_nov_wilcox$conf.int[2],
                                     SanJ_Vernalis_NO3_flux_dec_wilcox$conf.int[2],
                                     SanJ_Vernalis_NO3_flux_jan_wilcox$conf.int[2],
                                     SanJ_Vernalis_NO3_flux_feb_wilcox$conf.int[2],
                                     SanJ_Vernalis_NO3_flux_mar_wilcox$conf.int[2],
                                     SanJ_Vernalis_NO3_flux_apr_wilcox$conf.int[2],
                                     SanJ_Vernalis_NO3_flux_may_wilcox$conf.int[2],
                                     SanJ_Vernalis_NO3_flux_jun_wilcox$conf.int[2],
                                     SanJ_Vernalis_NO3_flux_jul_wilcox$conf.int[2],
                                     SanJ_Vernalis_NO3_flux_aug_wilcox$conf.int[2],
                                     SanJ_Vernalis_NO3_flux_sep_wilcox$conf.int[2]))

write.table(Flux_compare, "SanJ_Vernalis_NO3_flux_wilcox.txt", quote=FALSE, row.names=FALSE)

rng_flx <- max(abs(c(Flux_compare$up_conf, Flux_compare$low_conf)))
tiff("SanJ_Vernalis_NO3_flux_shift_wilcox_Vert_Bars.tif", height=600, width=800, res=130)
par(mar=c(4,5,0.5,0.5))
plot(seq(1:12), Flux_compare$chng_est, typ='h', lend=1, lwd=15, col='white', xaxt='n', xlim=c(1,13), ylim=c(-rng_flx, rng_flx), xlab="Month", ylab=expression(paste("Median Flux Change, kg",sep='')), las=1)
plotCI(seq(1:12), Flux_compare$chng_est, ui=Flux_compare$up_conf, li=Flux_compare$low_conf, pch=16, add=TRUE)
abline(h=0)
axis(side=1,at=seq(1,12,by=1), labels=format(c(seq(as.Date("2000-10-01"), as.Date("2000-12-01"), by="month"), seq(as.Date("2000-01-01"), as.Date("2000-09-01"), by="month")),'%b'), las=2)
legend('topright', c("Median difference", "90% Confidence Interval for the Median"), pch=c(16,NA), lwd=c(NA,1), pt.cex=c(1,NA), pt.bg=c('black',NA), bty='n', bg='white')
dev.off()


# End of non-standard EGRET plot section requested by Michael
# --------------------------------------------------------------------------------------------------------



# Plot the annual average concentration and annual flow-normalized concentration
tiff("Ann_Avg_Conc_&_Ann_Flow_Normalized_Conc_SanJVernalis.tif", height = 600, width = 800, res=120)
plotConcHist(eList, plotFlowNorm=TRUE)
dev.off()

# Plot the annual average concentration and annual flow-normalized concentration
pdf("Ann_Avg_Conc_&_Ann_Flow_Normalized_Conc_SanJVernalis.pdf")
plotConcHist(eList, plotFlowNorm=TRUE, tinyPlot=TRUE,yearEnd = 2015)
dev.off()


plotConcHist(eList, plotFlowNorm=TRUE, printTitle=FALSE)

# Plot the annual flux and annual flow-normalized flux
tiff("Ann_Flux_&_Ann_Flow_Normalized_Flux_SanJVernalis.tif", height = 600, width = 800, res=120)
plotFluxHist(eList, plotFlowNorm = TRUE) # fluxMax) # fluxMax
dev.off()

# Look for a trend change:
tableChange(eList, fluxUnit=6, yearPoints=c(1971, 1981, 1991,2011,2019))


#
#Generate out-of-the-box diagnostic plots
tiff("fluxBiasMulti_SanJVernalis_Inorg_N.tif", height = 1200, width = 1200, res=120)
fluxBiasMulti(eList, moreTitle = "WRTDS")
dev.off()

tiff("Modeled_Daily_Conc_wObservations_SanJVernalis_Inorg_N.tif", height = 800, width = 1000, res=120)
plotConcTimeDaily(eList)
dev.off()

# Exploring model behavior and adjusting model parameters
tiff("Contours_SanJVernalis_Inorg_N.tif", height = 700, width = 1000, res=120)
plotContours(eList, qBottom=5,qTop=1000,yearStart=1971,yearEnd=2019, contourLevels=seq(0.0,0.28,by=0.005), color.palette = colorRampPalette(c("violet", "purple", "blue", "cyan", "green", "yellow", "orange", "red"))) 
dev.off()

tiff("Log_Contours_SanJVernalis_Inorg_N.tif", height = 700, width = 1000, res=120)
plotContours(eList, qBottom=5, qTop=1000, yearStart=1971, yearEnd=2019, contourLevels=seq(-6.4,-1.5,by=0.1), color.palette = colorRampPalette(c("violet", "purple", "blue", "cyan", "green", "yellow", "orange", "red")), whatSurface=1) 
dev.off()

tiff("StdErr_of_Log_Contours_SanJVernalis_Inorg_N.tif", height = 700, width = 1000, res=120)
plotContours(eList, qBottom=5, qTop=1000, yearStart=1971, yearEnd=2019, contourLevels=seq(0.35,0.88,by=0.01), color.palette = colorRampPalette(c("violet", "purple", "blue", "cyan", "green", "yellow", "orange", "red")), whatSurface=2) 
dev.off()

tiff("Contours_Difference_Inorg_N.tif", height = 700, width = 1000, res=120)
plotDiffContours(eList, 1971,2019,5,1000,maxDiff=0.5)
dev.off()

tiff("Contours_PercentDifference_UT_Inorg_N.tif", height = 700, width = 1000, res=120)
plotDiffContours(eList, 1971,2019,5,1000, maxDiff=100, plotPercent=TRUE)
dev.off()

plotDiffContours2 <- function (eList, year0, year1, qBottom, qTop, maxDiff, whatSurface = 3, 
                               tcl = 0.1, qUnit = 2, span = 60, pval = 0.05, printTitle = TRUE, 
                               plotPercent = FALSE, vert1 = NA, vert2 = NA, horiz = NA, 
                               flowDuration = TRUE, yTicks = NA, tick.lwd = 2, lwd = 1, 
                               cex.main = 0.95, cex.axis = 1, customPar = FALSE, color.palette = colorRampPalette(c("blue", 
                                                                                                                    "white", "red")), ...) 
{
  localINFO <- getInfo(eList)
  localDaily <- getDaily(eList)
  localsurfaces <- getSurfaces(eList)
  if (is.numeric(qUnit)) {
    qUnit <- qConst[shortCode = qUnit][[1]]
  }
  else if (is.character(qUnit)) {
    qUnit <- qConst[qUnit][[1]]
  }
  if (!customPar) {
    par(oma = c(6, 1, 6, 0))
    par(mar = c(5, 5, 4, 2) + 0.1)
  }
  surfaceName <- c("log of Concentration", "Standard Error of log(C)", 
                   "Concentration")
  j <- 3
  j <- if (whatSurface == 1) 
    1
  else j
  j <- if (whatSurface == 2) 
    2
  else j
  surf <- localsurfaces
  bottomLogQ <- localINFO$bottomLogQ
  stepLogQ <- localINFO$stepLogQ
  nVectorLogQ <- localINFO$nVectorLogQ
  bottomYear <- localINFO$bottomYear
  stepYear <- localINFO$stepYear
  nVectorYear <- localINFO$nVectorYear
  start0 <- ((year0 - bottomYear) * 16) + 1
  end0 <- start0 + 16
  start1 <- ((year1 - bottomYear) * 16) + 1
  end1 <- start1 + 16
  if (plotPercent) {
    diff <- (surf[, start1:end1, j] - surf[, start0:end0, 
                                           j]) * 100/surf[, start0:end0, j]
  }
  else {
    diff <- surf[, start1:end1, j] - surf[, start0:end0, 
                                          j]
  }
  difft <- t(diff)
  if (length(maxDiff) == 1) {
    surfaceSpan <- c(-maxDiff, maxDiff)
  }
  else {
    surfaceSpan <- range(maxDiff)
  }
  contourLevels <- pretty(surfaceSpan, n = 50)
  x <- seq(0, 1, stepYear)
  y <- ((1:nVectorLogQ) * stepLogQ) + (bottomLogQ - stepLogQ)
  yLQ <- y
  qFactor <- qUnit@qUnitFactor
  y <- exp(y) * qFactor
  numX <- length(x)
  numY <- length(y)
  if (is.na(yTicks[1])) {
    qBottom <- max(0.9 * y[1], qBottom)
    qTop <- min(1.1 * y[numY], qTop)
    yTicks <- logPretty3(qBottom, qTop)
    yTicks2 <- c(0.028,0.056,0.141,0.283,0.566,1.415,2.831,5.663,14.15,28.31,56.63,141.5) #cfs
  }
  xTicks <- c(0, 0.0848, 0.1642, 0.249, 0.331, 0.416, 0.498, 
              0.583, 0.668, 0.75, 0.835, 0.917, 1)
  xLabels <- c("Jan1", "Feb1", "Mar1", "Apr1", "May1", "Jun1", 
               "Jul1", "Aug1", "Sep1", "Oct1", "Nov1", "Dec1", "Jan1")
  nxTicks <- length(xTicks)
  nYTicks <- length(yTicks)
  numDays <- length(localDaily$Day)
  freq <- rep(0, nVectorLogQ)
  plotTitle <- if (printTitle) 
    paste(localINFO$shortName, " ", localINFO$paramShortName, 
          "\nEstimated", surfaceName[j], "change from", year0, 
          "to", year1)
  else ""
  if (flowDuration) {
    durSurf <- rep(0, 17 * nVectorLogQ)
    dim(durSurf) <- c(17, nVectorLogQ)
    centerDays <- seq(1, 388, 22.9)
    centerDays <- floor(centerDays)
    for (ix in 1:17) {
      startDay <- centerDays[ix] - span
      endDay <- centerDays[ix] + span
      goodDays <- seq(startDay, endDay, 1)
      goodDays <- ifelse(goodDays > 0, goodDays, goodDays + 
                           365)
      goodDays <- ifelse(goodDays < 366, goodDays, goodDays - 
                           365)
      numDays <- length(localDaily$Day)
      isGood <- localDaily$Day %in% goodDays
      spanDaily <- data.frame(localDaily, isGood)
      spanDaily <- subset(spanDaily, isGood)
      n <- length(spanDaily$Day)
      LogQ <- spanDaily$LogQ
      for (jQ in 1:nVectorLogQ) {
        ind <- ifelse(LogQ < yLQ[jQ], 1, 0)
        freq[jQ] <- sum(ind)/n
      }
      durSurf[ix, ] <- freq
    }
    plevels <- c(pval, 1 - pval)
    pct1 <- format(plevels[1] * 100, digits = 2)
    pct2 <- format(plevels[2] * 100, digits = 2)
    firstLine <- paste(localINFO$shortName, "  ", localINFO$paramShortName, 
                       sep = "")
    secondLine <- if (plotPercent) {
      paste("\nEstimated", surfaceName[j], "percent change from", 
            year0, "to", year1)
    }
    else {
      paste("\nEstimated", surfaceName[j], "change from", 
            year0, "to", year1)
    }
    thirdLine <- paste("\nBlack lines are", pct1, "and", 
                       pct2, "flow percentiles")
    plotTitle <- paste(firstLine, secondLine, thirdLine)
  }
  vectorNone <- c(year0, log(yTicks[1], 10) - 1, year1, log(yTicks[1], 
                                                            10) - 1)
  v1 <- if (is.na(vert1)) 
    vectorNone
  else c(vert1, log(yTicks[1], 10), vert1, log(yTicks[nYTicks], 
                                               10))
  v2 <- if (is.na(vert2)) 
    vectorNone
  else c(vert2, log(yTicks[1], 10), vert2, log(yTicks[nYTicks], 
                                               10))
  h1 <- if (is.na(horiz)) 
    vectorNone
  else c(year0, log(horiz, 10), year1, log(horiz, 10))
  deltaY <- (log(yTicks[length(yTicks)], 10) - log(yTicks[1], 
                                                   10))/25
  deltaX <- (1)/25
  yLab <- qUnit@qUnitExpress
  filled.contour(x, log(y, 10), difft, levels = contourLevels, 
                 xlim = c(0, 1), ylim = c(log(yTicks[1], 10), log(yTicks[nYTicks], 
                                                                  10)), xlab = "", ylab = yLab, xaxs = "i", yaxs = "i", 
                 cex.main = cex.main, plot.axes = {
                   axis(1, tcl = 0, at = xTicks, labels = xLabels, cex.axis = 0.9 * 
                          cex.axis)
                   axis(2, tcl = 0, las = 1, at = log(yTicks, 10), labels = yTicks, 
                        cex.axis = cex.axis)
                   axis(3, tcl = 0, at = xTicks, labels = FALSE)
                   axis(4, tcl = 0, at = log(yTicks2, 10), labels = c("1","2","5","10","20","50","100","200","500","1000","2000","5000"))    #edm: yTicks -> yTicks2, added labels
                   if (flowDuration) 
                     contour(x, log(y, 10), durSurf, add = TRUE, drawlabels = FALSE, 
                             levels = plevels, lwd = lwd,lty=2)  #adjust the 95% confidence intervals here
                   segments(v1[1], v1[2], v1[3], v1[4])
                   segments(v2[1], v2[2], v2[3], v2[4])
                   segments(h1[1], h1[2], h1[3], h1[4])
                   segments(xTicks, rep(log(yTicks[1], 10), length(xTicks)), 
                            xTicks, rep(grconvertY(grconvertY(par("usr")[3], 
                                                              from = "user", to = "inches") + tcl, from = "inches", 
                                                   to = "user"), length(xTicks)), lwd = tick.lwd)
                   segments(xTicks, rep(log(yTicks[nYTicks], 10), length(xTicks)), 
                            xTicks, rep(grconvertY(grconvertY(par("usr")[4], 
                                                              from = "user", to = "inches") - tcl, from = "inches", 
                                                   to = "user"), length(xTicks)), lwd = tick.lwd)
                   segments(rep(0, length(yTicks)), log(yTicks, 10), 
                            rep(grconvertX(grconvertX(par("usr")[1], from = "user", 
                                                      to = "inches") + tcl, from = "inches", to = "user"), 
                                length(yTicks)), log(yTicks, 10), lwd = tick.lwd)
                   segments(rep(grconvertX(grconvertX(par("usr")[2], 
                                                      from = "user", to = "inches") - tcl, from = "inches", 
                                           to = "user"), length(yTicks)), log(yTicks, 10), 
                            rep(1, length(yTicks)), log(yTicks, 10), lwd = tick.lwd)
                 }, color.palette = color.palette, ...)
  
  #try adding contours
  par(new=T, mar=c(5.1,5.1,4.1,9.1))
  contour(x, log(y, 10), difft, levels = seq(-100,200,by=50),xlim=c(0,1), ylim = c(log(yTicks[1], 10), log(yTicks[nYTicks],10)),
          xaxs = "i",yaxs="i",xaxt="n",yaxt="n",labcex=1,method=c("flattest","edge")) #add=TRUE)
  if (printTitle) 
    title(plotTitle, outer = TRUE, cex.main = cex.main, line = -3)
}


tiff("Contours_PercentDifference2_Inorg_N_SanJVernalis_DIN.tif", height = 700, width = 1000, res=120)
plotDiffContours2(eList, 1971,2019,5,1000, maxDiff=c(-100,100), plotPercent=TRUE, lwd=3, color.palette=colorRampPalette(c("blue","lightblue","white", "orange", "red")),tick.lwd = 1)
dev.off()

Sample$WY <- trunc(Sample$DecYear+0.25) 
tiff("Monthly_Boxplot_SanJVern_Inorg_N.tif", height = 700, width = 1000, res=120)
par(mar=c(4,6,0.5,0.5))
boxplot(Sample$ConcAve~Sample$WY,log="y",varwidth=TRUE,ylim=c(0.05,10),yaxs="i",xlab="Water Year",las=1) 
mtext(side=2, expression(paste("Concentration, Inorganic Nitrogen, in mg  ",L^-1,sep="")),line=4)
dev.off()

#################  Using the plotConcQSmooth function
###########
#First do flow duration analysis
flowDuration(eList, centerDate = "06-01", qUnit = 2, span = 30)
date1 <- "1972-06-01"
date2 <- "1982-06-01"
date3 <- "1992-06-01"
date4 <- "2002-06-01"
date5 <- "2012-06-01"
date6 <- "2019-06-01"
qLow= baseQ
qHigh=highQ7

#tiff("SanJVern_Date1_Discharge_NO3_conc_no_log.tif",height = 700, width = 1000, res=120)
plotConcQSmooth(eList,date1, date2, date3, qLow, qHigh, logScale=FALSE,printLegend =TRUE,legendLeft=0,legendTop=0,printTitle=TRUE)
dev.off()
tiff("SanJVern_Date2_Discharge_NO3_conc_no_log.tif",height = 700, width = 1000, res=120)
plotConcQSmooth(eList,date4, date5, date6, qLow, qHigh, logScale=FALSE,printLegend =TRUE,legendLeft=0,legendTop=0, printTitle=TRUE)
dev.off()

####################################################################################################################################
####################################################################################################################################
# ---------------------------
# Now run the EGRETci package
# ---------------------------
####################################################################################################################################
# Change working directory
setwd("C:/Users/dsaleh/Documents/GitHub/PES_Project/Vernalis_EGRET/NO3/")
#setwd("/Users/joed/PES_Project/Vernalis_EGRET/")

##Make sure that the working directory is set properly

#setwd("/Users/joed/PES_Project/Vernalis_EGRET/NO3/")
subDir <- 'EGRETci_plots'
if (file.exists(subDir)){
  setwd(file.path(getwd(),subDir))
} else {
  dir.create(file.path(getwd(),subDir), recursive = TRUE)
  setwd(file.path(getwd(),subDir))
}

#Interactive function to set up trend analysis:
caseSetUp <- trendSetUp(eList, 
                        year1=1972, 
                        year2=2018, 
                        nBoot = 200, 
                        bootBreak = 100, 
                        blockLength = 200)
eBoot <- wBT(eList, caseSetUp, fileName ="outputText.txt")


#

saveEGRETci(eList, eBoot, caseSetUp, fileName = "EGRETci_output_NO3")
#
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
#
nBoot <- 200
blockLength <- 200
coreOut <- 4 #Number of cores to leave out of processing tasks

widthCI <- 95
ciLower <- (50-(widthCI/2))/100
ciUpper <- (50+(widthCI/2))/100
probs <- c(ciLower,ciUpper)
#
nCores <- detectCores() - coreOut
cl <- makeCluster(nCores)
registerDoParallel(cl)
repAnnual <- foreach(n = 1:nBoot,.packages=c('EGRETci')) %dopar% {
  annualResults <- bootAnnual(eList, blockLength,startSeed = n)  
}
stopCluster(cl)
#
CIAnnualResults <- ciBands(eList, repAnnual, probs)
conc.poly.x <- c(CIAnnualResults$Year,rev(CIAnnualResults$Year))
conc.poly.y <- c(CIAnnualResults$FNConcLow,rev(CIAnnualResults$FNConcHigh))
flux.poly.x <- c(CIAnnualResults$Year,rev(CIAnnualResults$Year))
flux.poly.y <- c(CIAnnualResults$FNFluxLow*365,rev(CIAnnualResults$FNFluxHigh*365))
#
tiff("Ann_Avg_Conc_&_Ann_Flow_Normalized_Conc_Boot_SanJVernalisInorgN.tif", height = 500, width = 600, res=110)
plotConcHistBoot(eList, CIAnnualResults, plotFlowNorm=TRUE, showYLabels=TRUE, showYAxis=TRUE,col=4)
polygon(x=conc.poly.x, y=conc.poly.y, col=rgb(24,116,205,40,max=255),border=NA)
dev.off()
#
tiff("Ann_Flux_&_Ann_Flow_Normalized_Flux_Boot_SanJVernalisInorgN.tif", height = 500, width = 600, res=110)
plotFluxHistBoot(eList, fluxUnit=13, CIAnnualResults, showYLabels=TRUE, showYAxis=TRUE, col=4)
polygon(x=flux.poly.x, y=flux.poly.y, col=rgb(24,116,205,40,max=255),border=NA)
dev.off()
#
setSweave("SanJ_Vernalis_NO3_Conc_EGRETCI",7,7)
plotConcHistBoot(eList, CIAnnualResults, plotFlowNorm=TRUE, showYLabels=TRUE, showYAxis=TRUE,col=4)
polygon(x=conc.poly.x, y=conc.poly.y, col=rgb(24,116,205,40,max=255),border=NA)
dev.off()
graphics.off()

setSweave("SanJ_Vernalis_NO3_Flux_EGRETCI",7,7)
plotFluxHistBoot(eList, fluxUnit=13, CIAnnualResults, showYLabels=TRUE, showYAxis=TRUE, col=4)
polygon(x=flux.poly.x, y=flux.poly.y, col=rgb(24,116,205,40,max=255),border=NA)
graphics.off()




saveEGRETci(eList, eBoot, fileName="N_Boot_NO3")
save(repAnnual,file="RepAnnual")
write.csv(repAnnual,'reAnnual.csv')


##########################################################################################################################################
#############################################################################
# Now do Orthophosphate, water, filtered, milligrams per liter as phosphorus
#############################################################################

startDate <- "1974-10-01"
endDate <- "2019-05-30"
siteNumber <- "11303500"
QParameterCd <- "00060"
parameterCd <- "00671"

filePath <- "C:/Users/dsaleh/Documents/GitHub/PES_Project/Vernalis_EGRET/"
#filePath <- "/Users/joed/PES_Project/Vernalis_EGRET/"

Daily <- readNWISDaily(siteNumber, QParameterCd, startDate, endDate)
#Sample <- readNWISSample(siteNumber, parameterCd, startDate, endDate)

#write.csv(Sample,"C:/Users/dsaleh/Documents/GitHub/PES_Project/Vernalis_EGRET/NWIS_OP.csv")

##Add Kratzer's data to NWIS data
fileName <- "Vern_NWIS_and_KratzerOP.csv"
Sample <- readUserSample(filePath, fileName)
Sample <- removeDuplicates(Sample)

#Sample <- readNWISSample(siteNumber, parameterCd, startDate, endDate)
INFO <- readNWISInfo(siteNumber = siteNumber, parameterCd = parameterCd, interactive=FALSE)
INFO$staAbbrev <- paste(strsplit(INFO$station_nm," ")[[1]][1],strsplit(INFO$station_nm," ")[[1]][2])
# Have a look at the available range of NO3 data
range(Sample$Date)
#  "1974-10-02" "2019-05-07"

eList <- NULL
eList <- mergeReport(INFO, Daily, Sample)

######
# Change the working directory; redirect plot output to OP folder
#setwd("/Users/joed/PES_Project/Vernalis_EGRET/")

setwd("C:/Users/dsaleh/Documents/GitHub/PES_Project/Vernalis_EGRET")
subDir <- 'OP/EGRET_plots'
if (file.exists(subDir)){
  setwd(file.path(getwd(),subDir))
} else {
  dir.create(file.path(getwd(),subDir), recursive = TRUE)
  setwd(file.path(getwd(),subDir))
}
plotConcTimeDaily(eList)

# Plot water quality data
tiff("Conc_vs_Time_Ortho_P.tif", height = 600, width = 800, res=120)
plotConcTime(eList)
dev.off()

# Now, a classic Q-C plot
tiff("Conc-Q_SanJVernalis-_Ortho_P.tif", height = 600, width = 800, res=120)
plotConcQ(eList, logScale=TRUE)
dev.off()

# The data set as flux values rather than as concentrations
tiff("Flux-Q_SanJVernalis_Ortho_P.tif", height = 600, width = 800, res=120)
plotFluxQ(eList, fluxUnit=4)
dev.off()
plotConcTime(eList)
# Monthly boxplots
tiff("Monthly-Conc_BoxPlots_SanJVernalis_Ortho_P.tif", height = 600, width = 800, res=120)
boxConcMonth(eList, logScale=TRUE)
dev.off()

# Flow on days sampled vs. all other days
tiff("Flow_on_days_sampled_vs_all_other_days_SanJVernalis_Ortho_P.tif", height = 600, width = 800, res=120)
boxQTwice(eList, qUnit=1)
dev.off()

#####################################################
# Now start the Flow-Normalized Analysis for Ortho P
#####################################################

# Build the regression model
eList <- modelEstimation(eList, windowY = 7, windowQ = 2, windowS = 0.5, minNumObs = 100, minNumUncen = 50)
eList_OP <- eList

MonthlyResults <- calculateMonthlyResults(eList)

# Dump OP-related flow-normalized data to text file for bringing together with other monitoring sites
paLong <- 12
paStart <- 10
localDaily <- getDaily(eList_OP)
localAnnualResults <- setupYears(paStart = paStart, paLong = paLong, localDaily = localDaily)
write.table(localAnnualResults, file = 'Vern_OP_RawVals.txt', quote=FALSE, row.names=FALSE)
write.csv(Daily,'localDailyOP.csv')


# Plot the annual average concentration and annual flow-normalized concentration
tiff("Ann_Avg_Conc_&_Ann_Flow_Normalized_Conc_SanJVernalis_OP.tif", height = 600, width = 800, res=120)
plotConcHist(eList, plotFlowNorm=TRUE,cex.axis = 0.8)
dev.off()

# Plot the annual flux and annual flow-normalized flux
tiff("Ann_Flux_&_Ann_Flow_Normalized_Flux_SanJVernalis_OP.tif", height = 600, width = 800, res=120)
plotFluxHist(eList, plotFlowNorm = TRUE) # fluxMax) # fluxMax
dev.off()

###################
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
tiff("Flow_Normalized_Conc_OP.tif", height = 600, width = 800, res=120)
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



# --------------------------------------------------------------------------------------------------------
# The following script is for a non-standard EGRET plot and instead help generate a plot Michael requested

localDaily <- getDaily(eList)

# Will need to adjust the date range below based on each gages unique start/stop dates

early_decade <- subset(localDaily, localDaily$Date > as.Date('1974-10-01') & localDaily$Date < as.Date('1984-10-01'))
recent_decade <- subset(localDaily, localDaily$Date > as.Date('2009-06-01'))

early_decade_monthly_mn <- aggregate(ConcDay ~ MonthSeq, data = early_decade, 'mean')
recent_decade_monthly_mn <- aggregate(ConcDay ~ MonthSeq, data = recent_decade, 'mean')

# early_decade_monthly_mn$month <- format(seq(as.Date('1971-10-01'), as.Date('1981-10-01'), by='month'), '%b')
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

tiff("timing_shift_in_OP_conc_monthly_means.tif", height=800, width=900, res=130)
par(mar=c(3,5,2,1))
x <- barplot(mdat2, beside=TRUE, las=1, ylim=c(0,mx), col = c("lightblue", "mistyrose"))
abline(h=0)
arrows(x0=x[1,], y0=early_decade_mon_mn$ConcDay - early_decade_mon_sd$ConcDay, x1=x[1,], y1=early_decade_mon_mn$ConcDay + early_decade_mon_sd$ConcDay, angle=90, length=0.04, code=3)
arrows(x0=x[2,], y0=recent_decade_mon_mn$ConcDay - recent_decade_mon_sd$ConcDay, x1=x[2,], y1=recent_decade_mon_mn$ConcDay + recent_decade_mon_sd$ConcDay, angle=90, length=0.04, code=3)
mtext(side=2, expression(paste(OP,', mg ',L^-1,sep='')), line=3)
legend(x=25, y=0.9 * mx, c("1974-1984", "2009-2019"), pch=c(22,22), pt.cex=2, pt.bg=c("lightblue", "mistyrose"), bty='n', xpd=TRUE)
dev.off()


# Now attempting a Wilcox Test (aka Mann-Whitney-Wilcoxon Rank Sum test)
# ----------------------------------------------------------------------
early_jan <- subset(early_decade_monthly_mn, month==1)
recent_jan <- subset(recent_decade_monthly_mn, month==1)
SanJVernalis_OP_conc_jan_wilcox <- wilcox.test(recent_jan$ConcDay, early_jan$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_feb <- subset(early_decade_monthly_mn, month==2)
recent_feb <- subset(recent_decade_monthly_mn, month==2)
SanJVernalis_OP_conc_feb_wilcox <- wilcox.test(recent_feb$ConcDay, early_feb$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_mar <- subset(early_decade_monthly_mn, month==3)
recent_mar <- subset(recent_decade_monthly_mn, month==3)
SanJVernalis_OP_conc_mar_wilcox <- wilcox.test(recent_mar$ConcDay, early_mar$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_apr <- subset(early_decade_monthly_mn, month==4)
recent_apr <- subset(recent_decade_monthly_mn, month==4)
SanJVernalis_OP_conc_apr_wilcox <- wilcox.test(recent_apr$ConcDay, early_apr$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_may <- subset(early_decade_monthly_mn, month==5)
recent_may <- subset(recent_decade_monthly_mn, month==5)
SanJVernalis_OP_conc_may_wilcox <- wilcox.test(recent_may$ConcDay, early_may$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_jun <- subset(early_decade_monthly_mn, month==6)
recent_jun <- subset(recent_decade_monthly_mn, month==6)
SanJVernalis_OP_conc_jun_wilcox <- wilcox.test(recent_jun$ConcDay, early_jun$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_jul <- subset(early_decade_monthly_mn, month==7)
recent_jul <- subset(recent_decade_monthly_mn, month==7)
SanJVernalis_OP_conc_jul_wilcox <- wilcox.test(recent_jul$ConcDay, early_jul$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_aug <- subset(early_decade_monthly_mn, month==8)
recent_aug <- subset(recent_decade_monthly_mn, month==8)
SanJVernalis_OP_conc_aug_wilcox <- wilcox.test(recent_aug$ConcDay, early_aug$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_sep <- subset(early_decade_monthly_mn, month==9)
recent_sep <- subset(recent_decade_monthly_mn, month==9)
SanJVernalis_OP_conc_sep_wilcox <- wilcox.test(recent_sep$ConcDay, early_sep$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_oct <- subset(early_decade_monthly_mn, month==10)
recent_oct <- subset(recent_decade_monthly_mn, month==10)
SanJVernalis_OP_conc_oct_wilcox <- wilcox.test(recent_oct$ConcDay, early_oct$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_nov <- subset(early_decade_monthly_mn, month==11)
recent_nov <- subset(recent_decade_monthly_mn, month==11)
SanJVernalis_OP_conc_nov_wilcox <- wilcox.test(recent_nov$ConcDay, early_nov$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_dec <- subset(early_decade_monthly_mn, month==12)
recent_dec <- subset(recent_decade_monthly_mn, month==12)
SanJVernalis_OP_conc_dec_wilcox <- wilcox.test(recent_dec$ConcDay, early_dec$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

Conc_compare <- data.frame(chng_est=c(SanJVernalis_OP_conc_oct_wilcox$est,
                                      SanJVernalis_OP_conc_nov_wilcox$est,
                                      SanJVernalis_OP_conc_dec_wilcox$est,
                                      SanJVernalis_OP_conc_jan_wilcox$est,
                                      SanJVernalis_OP_conc_feb_wilcox$est,
                                      SanJVernalis_OP_conc_mar_wilcox$est,
                                      SanJVernalis_OP_conc_apr_wilcox$est,
                                      SanJVernalis_OP_conc_may_wilcox$est,
                                      SanJVernalis_OP_conc_jun_wilcox$est,
                                      SanJVernalis_OP_conc_jul_wilcox$est,
                                      SanJVernalis_OP_conc_aug_wilcox$est,
                                      SanJVernalis_OP_conc_sep_wilcox$est),
                           low_conf=c(SanJVernalis_OP_conc_oct_wilcox$conf.int[1],
                                      SanJVernalis_OP_conc_nov_wilcox$conf.int[1],
                                      SanJVernalis_OP_conc_dec_wilcox$conf.int[1],
                                      SanJVernalis_OP_conc_jan_wilcox$conf.int[1],
                                      SanJVernalis_OP_conc_feb_wilcox$conf.int[1],
                                      SanJVernalis_OP_conc_mar_wilcox$conf.int[1],
                                      SanJVernalis_OP_conc_apr_wilcox$conf.int[1],
                                      SanJVernalis_OP_conc_may_wilcox$conf.int[1],
                                      SanJVernalis_OP_conc_jun_wilcox$conf.int[1],
                                      SanJVernalis_OP_conc_jul_wilcox$conf.int[1],
                                      SanJVernalis_OP_conc_aug_wilcox$conf.int[1],
                                      SanJVernalis_OP_conc_sep_wilcox$conf.int[1]),
                           up_conf=c(SanJVernalis_OP_conc_oct_wilcox$conf.int[2],
                                     SanJVernalis_OP_conc_nov_wilcox$conf.int[2],
                                     SanJVernalis_OP_conc_dec_wilcox$conf.int[2],
                                     SanJVernalis_OP_conc_jan_wilcox$conf.int[2],
                                     SanJVernalis_OP_conc_feb_wilcox$conf.int[2],
                                     SanJVernalis_OP_conc_mar_wilcox$conf.int[2],
                                     SanJVernalis_OP_conc_apr_wilcox$conf.int[2],
                                     SanJVernalis_OP_conc_may_wilcox$conf.int[2],
                                     SanJVernalis_OP_conc_jun_wilcox$conf.int[2],
                                     SanJVernalis_OP_conc_jul_wilcox$conf.int[2],
                                     SanJVernalis_OP_conc_aug_wilcox$conf.int[2],
                                     SanJVernalis_OP_conc_sep_wilcox$conf.int[2]))

write.table(Conc_compare, "SanJVernalis_OP_conc_wilcox.txt", quote=FALSE, row.names=FALSE)

rng <- max(abs(c(Conc_compare$up_conf, Conc_compare$low_conf)))
tiff("SanJVernalis_OP_conc_shift_wilcox_Vert_Bars.tif", height=600, width=800, res=130)
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

# early_decade_monthly_mn$month <- format(seq(as.Date('1980-10-01'), as.Date('1990-09-30'), by='month'), '%b')
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
                                c(format(seq(as.Date('1980-10-01'), as.Date('1981-09-01'), by='month'), '%b'))))

mx <- max(c((early_decade_mon_mn_flx$FluxDay + early_decade_mon_sd_flx$FluxDay), (recent_decade_mon_mn_flx$FluxDay + recent_decade_mon_sd_flx$FluxDay)))
tiff("timing_shift_in_OP_load_monthly_means.tif", height=800, width=900, res=130)
x <- barplot(mdat3, beside=TRUE, las=1, ylim=c(0,mx), col = c("lightblue", "mistyrose"))
abline(h=0)
arrows(x0=x[1,], y0=early_decade_mon_mn_flx$FluxDay - early_decade_mon_sd_flx$FluxDay, x1=x[1,], y1=early_decade_mon_mn_flx$FluxDay + early_decade_mon_sd_flx$FluxDay, angle=90, length=0.04, code=3)
arrows(x0=x[2,], y0=recent_decade_mon_mn_flx$FluxDay - recent_decade_mon_sd_flx$FluxDay, x1=x[2,], y1=recent_decade_mon_mn_flx$FluxDay + recent_decade_mon_sd_flx$FluxDay, angle=90, length=0.04, code=3)
mtext(side=2, expression(paste(OP,', kg ',month^-1,sep='')), line=2.5)
legend(x=30, y=0.9 * mx, c("1974-1984", "2009-2019"), pch=c(22,22), pt.cex=2, pt.bg=c("lightblue", "mistyrose"), bty='n', xpd=TRUE)
dev.off()

# Apply Wilcox.text to the monthly loads here...
early_jan_flx <- subset(early_decade_monthly_flx, month==1)
recent_jan_flx <- subset(recent_decade_monthly_flx, month==1)
SanJVernalis_OP_flux_jan_wilcox <- wilcox.test(recent_jan_flx$FluxDay, early_jan_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_feb_flx <- subset(early_decade_monthly_flx, month==2)
recent_feb_flx <- subset(recent_decade_monthly_flx, month==2)
SanJVernalis_OP_flux_feb_wilcox <- wilcox.test(recent_feb_flx$FluxDay, early_feb_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_mar_flx <- subset(early_decade_monthly_flx, month==3)
recent_mar_flx <- subset(recent_decade_monthly_flx, month==3)
SanJVernalis_OP_flux_mar_wilcox <- wilcox.test(recent_mar_flx$FluxDay, early_mar_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_apr_flx <- subset(early_decade_monthly_flx, month==4)
recent_apr_flx <- subset(recent_decade_monthly_flx, month==4)
SanJVernalis_OP_flux_apr_wilcox <- wilcox.test(recent_apr_flx$FluxDay, early_apr_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_may_flx <- subset(early_decade_monthly_flx, month==5)
recent_may_flx <- subset(recent_decade_monthly_flx, month==5)
SanJVernalis_OP_flux_may_wilcox <- wilcox.test(recent_may_flx$FluxDay, early_may_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_jun_flx <- subset(early_decade_monthly_flx, month==6)
recent_jun_flx <- subset(recent_decade_monthly_flx, month==6)
SanJVernalis_OP_flux_jun_wilcox <- wilcox.test(recent_jun_flx$FluxDay, early_jun_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_jul_flx <- subset(early_decade_monthly_flx, month==7)
recent_jul_flx <- subset(recent_decade_monthly_flx, month==7)
SanJVernalis_OP_flux_jul_wilcox <- wilcox.test(recent_jul_flx$FluxDay, early_jul_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_aug_flx <- subset(early_decade_monthly_flx, month==8)
recent_aug_flx <- subset(recent_decade_monthly_flx, month==8)
SanJVernalis_OP_flux_aug_wilcox <- wilcox.test(recent_aug_flx$FluxDay, early_aug_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_sep_flx <- subset(early_decade_monthly_flx, month==9)
recent_sep_flx <- subset(recent_decade_monthly_flx, month==9)
SanJVernalis_OP_flux_sep_wilcox <- wilcox.test(recent_sep_flx$FluxDay, early_sep_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_oct_flx <- subset(early_decade_monthly_flx, month==10)
recent_oct_flx <- subset(recent_decade_monthly_flx, month==10)
SanJVernalis_OP_flux_oct_wilcox <- wilcox.test(recent_oct_flx$FluxDay, early_oct_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_nov_flx <- subset(early_decade_monthly_flx, month==11)
recent_nov_flx <- subset(recent_decade_monthly_flx, month==11)
SanJVernalis_OP_flux_nov_wilcox <- wilcox.test(recent_nov_flx$FluxDay, early_nov_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_dec_flx <- subset(early_decade_monthly_flx, month==12)
recent_dec_flx <- subset(recent_decade_monthly_flx, month==12)
SanJVernalis_OP_flux_dec_wilcox <- wilcox.test(recent_dec_flx$FluxDay, early_dec_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)


Flux_compare <- data.frame(chng_est=c(SanJVernalis_OP_flux_oct_wilcox$est,
                                      SanJVernalis_OP_flux_nov_wilcox$est,
                                      SanJVernalis_OP_flux_dec_wilcox$est,
                                      SanJVernalis_OP_flux_jan_wilcox$est,
                                      SanJVernalis_OP_flux_feb_wilcox$est,
                                      SanJVernalis_OP_flux_mar_wilcox$est,
                                      SanJVernalis_OP_flux_apr_wilcox$est,
                                      SanJVernalis_OP_flux_may_wilcox$est,
                                      SanJVernalis_OP_flux_jun_wilcox$est,
                                      SanJVernalis_OP_flux_jul_wilcox$est,
                                      SanJVernalis_OP_flux_aug_wilcox$est,
                                      SanJVernalis_OP_flux_sep_wilcox$est),
                           low_conf=c(SanJVernalis_OP_flux_oct_wilcox$conf.int[1],
                                      SanJVernalis_OP_flux_nov_wilcox$conf.int[1],
                                      SanJVernalis_OP_flux_dec_wilcox$conf.int[1],
                                      SanJVernalis_OP_flux_jan_wilcox$conf.int[1],
                                      SanJVernalis_OP_flux_feb_wilcox$conf.int[1],
                                      SanJVernalis_OP_flux_mar_wilcox$conf.int[1],
                                      SanJVernalis_OP_flux_apr_wilcox$conf.int[1],
                                      SanJVernalis_OP_flux_may_wilcox$conf.int[1],
                                      SanJVernalis_OP_flux_jun_wilcox$conf.int[1],
                                      SanJVernalis_OP_flux_jul_wilcox$conf.int[1],
                                      SanJVernalis_OP_flux_aug_wilcox$conf.int[1],
                                      SanJVernalis_OP_flux_sep_wilcox$conf.int[1]),
                           up_conf=c(SanJVernalis_OP_flux_oct_wilcox$conf.int[2],
                                     SanJVernalis_OP_flux_nov_wilcox$conf.int[2],
                                     SanJVernalis_OP_flux_dec_wilcox$conf.int[2],
                                     SanJVernalis_OP_flux_jan_wilcox$conf.int[2],
                                     SanJVernalis_OP_flux_feb_wilcox$conf.int[2],
                                     SanJVernalis_OP_flux_mar_wilcox$conf.int[2],
                                     SanJVernalis_OP_flux_apr_wilcox$conf.int[2],
                                     SanJVernalis_OP_flux_may_wilcox$conf.int[2],
                                     SanJVernalis_OP_flux_jun_wilcox$conf.int[2],
                                     SanJVernalis_OP_flux_jul_wilcox$conf.int[2],
                                     SanJVernalis_OP_flux_aug_wilcox$conf.int[2],
                                     SanJVernalis_OP_flux_sep_wilcox$conf.int[2]))

write.table(Flux_compare, "Vern_OP_flux_wilcox.txt", quote=FALSE, row.names=FALSE)

rng_flx <- max(abs(c(Flux_compare$up_conf, Flux_compare$low_conf)))
tiff("Vern_OP_flux_shift_wilcox_Vert_Bars.tif", height=600, width=800, res=130)
par(mar=c(4,5,0.5,0.5))
plot(seq(1:12), Flux_compare$chng_est, typ='h', lend=1, lwd=15, col='white', xaxt='n', xlim=c(1,13), ylim=c(-rng_flx, rng_flx), xlab="Month", ylab=expression(paste("Median Flux Change, kg",sep='')), las=1)
plotCI(seq(1:12), Flux_compare$chng_est, ui=Flux_compare$up_conf, li=Flux_compare$low_conf, pch=16, add=TRUE)
abline(h=0)
axis(side=1,at=seq(1,12,by=1), labels=format(c(seq(as.Date("2000-10-01"), as.Date("2000-12-01"), by="month"), seq(as.Date("2000-01-01"), as.Date("2000-09-01"), by="month")),'%b'), las=2)
legend('topright', c("Median difference", "90% Confidence Interval for the Median"), pch=c(16,NA), lwd=c(NA,1), pt.cex=c(1,NA), pt.bg=c('black',NA), bty='n', bg='white')
dev.off()

#################  Using the plotConcQSmooth function
###########
#First PO flow duration analysis
flowDuration(eList, centerDate = "06-01", qUnit = 2, span = 30)
date1 <- "1974-06-01"
date2 <- "1990-06-01"
date3 <- "2000-06-01"
qLow= baseQ
qHigh=highQ7

tiff("Vernalis_Date1_Discharge_OP_conc_no_log.tif",height = 700, width = 1000, res=120)
plotConcQSmooth(eList,date1, date2, date3,qLow, qHigh, logScale=FALSE,printLegend =TRUE,legendLeft=0,legendTop=0,printTitle=TRUE)
dev.off()

#Second PO flow duration analysis
flowDuration(eList, centerDate = "06-01", qUnit = 2, span = 30)
date4 <- "2010-06-01"
date5 <- "2019-06-01"

qLow= baseQ
qHigh=highQ7

tiff("Vernalis_Date2_Discharge_OP_conc_no_log.tif",height = 700, width = 1000, res=120)
plotConcQSmooth(eList,date3, date4, date5,qLow, qHigh, logScale=FALSE,printLegend =TRUE,legendLeft=0,legendTop=0,printTitle=TRUE)
dev.off()

# End of non-standard EGRET plot section requested by Michael
# --------------------------------------------------------------------------------------------------------



#
#Generate out-of-the-box diagnostic plots
tiff("fluxBiasMulti_SanJVernalis_Ortho_P.tif", height = 1200, width = 1200, res=120)
fluxBiasMulti(eList, moreTitle = "WRTDS")
dev.off()

tiff("Modeled_Daily_Conc_wObservations_SanJVernalis_Ortho_P.tif", height = 800, width = 1000, res=120)
plotConcTimeDaily(eList)
dev.off()

# Exploring model behavior and adjusting model parameters
tiff("Contours_SanJVernalis_Ortho_P.tif", height = 700, width = 1000, res=120)
plotContours(eList,qBottom=5,qTop=1000,yearStart=1974,yearEnd=2019, contourLevels=seq(0.003,0.03,by=0.0005), color.palette = colorRampPalette(c("violet", "purple", "blue", "cyan", "green", "yellow", "orange", "red"))) 
dev.off()

tiff("Log_Contours_SanJVernalis_Ortho_P.tif", height = 700, width = 1000, res=120)
plotContours(eList,qBottom=5, qTop=1000, yearStart=1974, yearEnd=2019, contourLevels=seq(-5.75,-3.4,by=0.1), color.palette = colorRampPalette(c("violet", "purple", "blue", "cyan", "green", "yellow", "orange", "red")), whatSurface=1) 
dev.off()

tiff("StdErr_of_Log_Contours_SanJVernalis_Ortho_P.tif", height = 700, width = 1000, res=120)
plotContours(eList, qBottom=5, qTop=1000, yearStart=1974, yearEnd=2019, contourLevels=seq(0.13,0.5,by=0.01), color.palette = colorRampPalette(c("violet", "purple", "blue", "cyan", "green", "yellow", "orange", "red")), whatSurface=2) 
dev.off()

tiff("Contours_Difference_Ortho_P_SanJVernalis.tif", height = 700, width = 1000, res=120)
plotDiffContours(eList, 1974,2019,5,1000,maxDiff=0.05)
dev.off()

tiff("Contours_PercentDifference_Ortho_P_SanJVernalis.tif", height = 700, width = 1000, res=120)
plotDiffContours(eList, 1979,2011,5,1000,maxDiff=100, plotPercent=TRUE)
dev.off()

tiff("Contours_PercentDifference2_Ortho_P_SanJVernalis.tif", height = 700, width = 1000, res=120)
plotDiffContours2(eList, 1974,2019,5,1000, maxDiff=c(-100,100), plotPercent=TRUE, lwd=3, color.palette=colorRampPalette(c("blue","lightblue","white","orange","red")),tick.lwd = 1)
dev.off()


#####changepoint analysis
#OP_changepoint <- read.table ("C:/Users/dsaleh/Documents/GitHub/PES_Project/Vernalis_EGRET/OP/EGRET_plots/_OP_RawVals.txt",header = TRUE)
#OP_changepoint2<-as.numeric(OP_changepoint$FNConc) 
#OPchangepoint2.binseg=cpt.meanvar(OP_changepoint2,test.stat='Normal',method='PELT',param.estimates=TRUE,Q=5,penalty="SIC")
#cpts(OPchangepoint2.binseg)
#tiff("C:/Users/dsaleh/Documents/GitHub/PES_Project/Vernalis_EGRET/OP/EGRET_plots/_OP_Changepoint.tif",height = 700, width = 1200, res=120)
#plot(OPchangepoint2.binseg,type='line',col="blue",ylim = c(0.009,0.014))
#dev.off()
#OPchangepoint2.binseg
#plot(OPchangepoint2.binseg,type='line',col="blue",ylim=c(0.009,0.014))

# ---------------------------
# Now run the EGRETci package
# ---------------------------
# Change working directory
##Make sure that it is set properly to your system
#setwd("/Users/joed/PES_Project/Vernalis_EGRET/OP/")
setwd("C:/Users/dsaleh/Documents/GitHub/PES_Project/Vernalis_EGRET/OP/")


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
                        bootBreak = 100, 
                        blockLength = 200)
eBoot <- wBT(eList, caseSetUp, fileName ="outputText.txt")

#Should we reject Ho that Flow Normalized Concentration Trend = 0 ? Do Not Reject Ho
#best estimate is 0.000817 mg/L
#Lower and Upper 90% CIs -0.000765  0.002487
#also 95% CIs-0.001026  0.002984
#and 50% CIs  0.000278  0.001452
#approximate two-sided p-value for Conc      0.35
#Likelihood that Flow Normalized Concentration is trending up =      0.827 is trending down =      0.173
#
#Should we reject Ho that Flow Normalized Flux Trend = 0 ? Do Not Reject Ho
#best estimate is -2.708e-06 10^6 kg/year
#Lower and Upper 90% CIs -3.98e-05  3.22e-05
#also 95% CIs -6.82e-05  3.35e-05
#and 50% CIs -1.70e-05  1.69e-05
#approximate two-sided p-value for Flux      0.96
#Likelihood that Flow Normalized Flux is trending up = 0.52 is trending down= 0.48
#
#Upward trend in concentration is likely
#Upward trend in flux is about as likely as not
#Downward trend in concentration is unlikely
#Downward trend in flux is about as likely as not
#

saveEGRETci(eList, eBoot, caseSetUp, fileName = "EGRETci_output_Ortho_P")

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

tiff("histo_NH3_SanJVernalis_Trend_conc_flux.tif", height = 700, width = 1200, res=120)
par(mfrow=c(1,2))
plotHistogramTrend2(eBoot, caseSetUp, eList, flux=FALSE, xSeq = seq(-8000,8000,5),las=1,xlim=c(-100,100))
abline(h=0)

plotHistogramTrend2(eBoot, caseSetUp, eList, flux=TRUE, xSeq = seq(-50000,50000,5),las=1)
abline(h=0)
dev.off()

nBoot <- 200
blockLength <- 200
coreOut <- 5 #Number of cores to leave out of processing tasks

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

tiff("Ann_Avg_Conc_&_Ann_Flow_Normalized_Conc_Boot_SanJVernalis_OP.tif", height = 500, width = 600, res=110)
plotConcHistBoot(eList, CIAnnualResults, plotFlowNorm=TRUE, showYLabels=TRUE, showYAxis=TRUE,col=4)
polygon(x=conc.poly.x, y=conc.poly.y, col=rgb(24,116,205,40,max=255),border=NA)
dev.off()

tiff("Ann_Flux_&_Ann_Flow_Normalized_Flux_Boot_SanJVernalis_OP.tif", height = 500, width = 600, res=110)
plotFluxHistBoot(eList, fluxUnit=13, CIAnnualResults, showYLabels=TRUE, showYAxis=TRUE, col=4)
polygon(x=flux.poly.x, y=flux.poly.y, col=rgb(24,116,205,40,max=255),border=NA)
dev.off()

setSweave("_OP_Conc_SanJVernalis_EGRETCI",7,7)
plotConcHistBoot(eList, CIAnnualResults, plotFlowNorm=TRUE, showYLabels=TRUE, showYAxis=TRUE,col=4)
polygon(x=conc.poly.x, y=conc.poly.y, col=rgb(24,116,205,40,max=255),border=NA)
graphics.off()

setSweave("_OP_Flux_SanJVernalis_EGRETCI",7,7)
plotFluxHistBoot(eList, fluxUnit=13, CIAnnualResults, showYLabels=TRUE, showYAxis=TRUE, col=4)
polygon(x=flux.poly.x, y=flux.poly.y, col=rgb(24,116,205,40,max=255),border=NA)
graphics.off()

saveEGRETci(eList, eBoot, fileName="N_Boot_Ortho_P")
save(repAnnual,file="RepAnnual")

# load(file="N_Boot.RData")
# load(file="RepAnnual")


#############################################################
# Working on TKN
#############################################################
startDate <- "1973-02-01"
endDate <- "2019-06-1"
siteNumber <- "11303500"
QParameterCd <- "00060"
parameterCd <- "00625"  # "TKN"

#filePath <- "/Users/joed/PES_Project/Vernalis_EGRET/"
filePath <- "C:/Users/dsaleh/Documents/GitHub/PES_Project/Vernalis_EGRET/"

Daily <- readNWISDaily(siteNumber, QParameterCd, startDate, endDate)
Sample <- readNWISSample(siteNumber, parameterCd, startDate, endDate)
#write.csv(Sample,"C:/Users/dsaleh/Documents/GitHub/PES_Project/Vernalis_EGRET/NWIS_TKN.csv")

##Add Kratzer's data Note: do not use Kratzer because Charlie's is TN and you want TKN
#fileName <- "CK_TN_data.csv"
#Sample <- readUserSample(filePath, fileName)
Sample <- removeDuplicates(Sample)



INFO <- readNWISInfo(siteNumber = siteNumber, parameterCd = parameterCd, interactive=FALSE)
INFO$staAbbrev <- paste(strsplit(INFO$station_nm," ")[[1]][1],strsplit(INFO$station_nm," ")[[1]][2])


# Have a look at the available range of TKN data
range(Sample$Date)
#  "1973-02-02" "2019-05-07"

eList <- mergeReport(INFO, Daily, Sample)
plotConcTime(eList)


# Change the working directory; redirect plot output to TKN folder
setwd ("C:/Users/dsaleh/Documents/GitHub/PES_Project/Vernalis_EGRET/")
#setwd("/Users/joed/PES_Project/Vernalis_EGRET/")
subDir <- 'TKN/EGRET_plots'
if (file.exists(subDir)){
  setwd(file.path(getwd(),subDir))
} else {
  dir.create(file.path(getwd(),subDir), recursive=TRUE)
  setwd(file.path(getwd(),subDir))
}

# Plot water quality data
tiff("Conc_vs_Time_SanJVernalis_TN.tif", height = 600, width = 800, res=120)
plotConcTime(eList)
dev.off()

# Now, a classic Q-C plot
tiff("Conc-Q_SanJVernalisSupTruck_TN.tif", height = 600, width = 800, res=120)
plotConcQ(eList, logScale=TRUE)
dev.off()

# The data set as flux values rather than as concentrations
tiff("Flux-Q_SanJVernalis_TN.tif", height = 600, width = 800, res=120)
plotFluxQ(eList, fluxUnit=4)
dev.off()

# Monthly boxplots
tiff("Monthly-Conc_BoxPlots_SanJVernalis_TN.tif", height = 600, width = 800, res=120)
boxConcMonth(eList, logScale=TRUE)
dev.off()

# Flow on days sampled vs. all other days
tiff("Flow_on_days_sampled_vs_all_other_days_SanJVernalis_TN.tif", height = 600, width = 800, res=120)
boxQTwice(eList, qUnit=1)
dev.off()

#########################################
# Now start the Flow-Normalized Analysis
#########################################

# Build the regression model
eList <- modelEstimation(eList, windowY = 7, windowQ = 2, windowS = 0.5, minNumObs = 100, minNumUncen =50)
eList_TN <- eList

MonthlyResults <- calculateMonthlyResults(eList)

# Dump TN-related flow-normalized data to text file for bringing together with other monitoring sites
paLong <- 12
paStart <- 10
localDaily <- getDaily(eList_TN)
localAnnualResults <- setupYears(paStart = paStart, paLong = paLong, localDaily = localDaily)
write.table(localAnnualResults, file = 'SanJVernalis_TKN_RawVals.txt', quote=FALSE, row.names=FALSE)

write.csv(Daily,'localDaily_TKN.csv')

# Plot the annual average concentration and annual flow-normalized concentration
tiff("Ann_Avg_Conc_&_Ann_Flow_Normalized_Conc_SanJVernalis_TN.tif", height = 600, width = 800, res=120)
plotConcHist(eList, plotFlowNorm=TRUE)
dev.off()

# Plot the annual flux and annual flow-normalized flux
tiff("Ann_Flux_&_Ann_Flow_Normalized_FluxSanJVernalis_TN.tif", height = 600, width = 800, res=120)
plotFluxHist(eList, plotFlowNorm = TRUE) # fluxMax) # fluxMax
dev.off()

# Look for a trend change:
tableChange(eList, fluxUnit=6, yearPoints=c(1990, 1997, 2005,2011))



#Generate out-of-the-box diagnostic plots
tiff("fluxBiasMulti_SanJVernalis_TN.tif", height = 1200, width = 1200, res=120)
fluxBiasMulti(eList, moreTitle = "WRTDS")
dev.off()

tiff("Modeled_Daily_Conc_wObservations_SanJVernalis_TN.tif", height = 800, width = 1000, res=120)
plotConcTimeDaily(eList)
dev.off()

# Exploring model behavior and adjusting model parameters
tiff("Contours_SanJVernalis_TN.tif", height = 700, width = 1000, res=120)
plotContours(eList, qBottom=5,qTop=1000,yearStart=1973,yearEnd=2019, contourLevels=seq(0.01,0.5,by=0.01), color.palette = colorRampPalette(c("violet", "purple", "blue", "cyan", "green", "yellow", "orange", "red"))) 
dev.off()

tiff("Log_Contours_SanJVernalis_TN.tif", height = 700, width = 1000, res=120)
plotContours(eList, qBottom=5, qTop=1000, yearStart=1974, yearEnd=2019, contourLevels=seq(-4.1,-0.45,by=0.1), color.palette = colorRampPalette(c("violet", "purple", "blue", "cyan", "green", "yellow", "orange", "red")), whatSurface=1) 
dev.off()

tiff("StdErr_of_Log_Contours_SanJVernalis_TN.tif", height = 700, width = 1000, res=120)
plotContours(eList, qBottom=5, qTop=1000, yearStart=1974, yearEnd=2019, contourLevels=seq(0.32,0.61,by=0.005), color.palette = colorRampPalette(c("violet", "purple", "blue", "cyan", "green", "yellow", "orange", "red")), whatSurface=2) 
dev.off()

tiff("Contours_Difference__TN.tif", height = 700, width = 1000, res=120)
plotDiffContours(eList, 1974,2019,5,1000,maxDiff=1.0)
dev.off()

tiff("Contours_PercentDifference__TN.tif", height = 700, width = 1000, res=120)
plotDiffContours(eList, 1974,2019,5,1000, maxDiff=100, plotPercent=TRUE)
dev.off()

tiff("Contours_PercentDifference2__TN.tif", height = 700, width = 1000, res=120)
plotDiffContours2(eList, 1974,2019,5,1000, maxDiff=c(-100,100), plotPercent=TRUE, lwd=3, color.palette=colorRampPalette(c("blue","lightblue","white","orange","red")),tick.lwd = 1)
dev.off()

Sample$WY <- trunc(Sample$DecYear+0.25) 
tiff("Monthly_Boxplot__TN.tif", height = 700, width = 1000, res=120)
par(mar=c(4,6,0.5,0.5))
boxplot(Sample$ConcAve~Sample$WY,log="y",varwidth=TRUE,ylim=c(0.01,2),yaxs="i",xlab="Water Year",las=1) 
mtext(side=2, expression(paste("Concentration, Inorganic Nitrogen, in mg  ",L^-1,sep="")),line=4)
dev.off()

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

tiff("Discharge_specific_trends_TKN_centered_on_06-01.tif", height = 600, width = 1200, res=120)
par(mar=c(4,6,4.1,8))
plotConcTimeSmooth(eList, q1 = baseQ, q2 = medQ, q3 = highQ7, centerDate='06-01', 
                   yearStart=localDaily$waterYear[1], yearEnd=localDaily$waterYear[nrow(localDaily)], 
                   logScale=TRUE, printLegend=TRUE)

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
tiff("Flow_Normalized_Conc_TC1_TKN.tif", height = 600, width = 800, res=120)
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
early_decade <- subset(localDaily, localDaily$Date > as.Date('1973-09-30') & localDaily$Date < as.Date('1983-10-01'))
recent_decade <- subset(localDaily, localDaily$Date > as.Date('2009-07-30'))


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
                dimnames = list(c("1973-1983", "2009-2019"),
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
legend(x=25, y=0.9 * mx, c("1973-1983", "2009-2019"), pch=c(22,22), pt.cex=2, pt.bg=c("lightblue", "mistyrose"), bty='n', xpd=TRUE)
dev.off()


# Now attempting a Wilcox Test (aka Mann-Whitney-Wilcoxon Rank Sum test)
# ----------------------------------------------------------------------
early_jan <- subset(early_decade_monthly_mn, month==1)
recent_jan <- subset(recent_decade_monthly_mn, month==1)
SanJVernalis_TKN_conc_jan_wilcox <- wilcox.test(recent_jan$ConcDay, early_jan$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_feb <- subset(early_decade_monthly_mn, month==2)
recent_feb <- subset(recent_decade_monthly_mn, month==2)
SanJVernalis_TKN_conc_feb_wilcox <- wilcox.test(recent_feb$ConcDay, early_feb$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_mar <- subset(early_decade_monthly_mn, month==3)
recent_mar <- subset(recent_decade_monthly_mn, month==3)
SanJVernalis_TKN_conc_mar_wilcox <- wilcox.test(recent_mar$ConcDay, early_mar$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_apr <- subset(early_decade_monthly_mn, month==4)
recent_apr <- subset(recent_decade_monthly_mn, month==4)
SanJVernalis_TKN_conc_apr_wilcox <- wilcox.test(recent_apr$ConcDay, early_apr$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_may <- subset(early_decade_monthly_mn, month==5)
recent_may <- subset(recent_decade_monthly_mn, month==5)
SanJVernalis_TKN_conc_may_wilcox <- wilcox.test(recent_may$ConcDay, early_may$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_jun <- subset(early_decade_monthly_mn, month==6)
recent_jun <- subset(recent_decade_monthly_mn, month==6)
SanJVernalis_TKN_conc_jun_wilcox <- wilcox.test(recent_jun$ConcDay, early_jun$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_jul <- subset(early_decade_monthly_mn, month==7)
recent_jul <- subset(recent_decade_monthly_mn, month==7)
SanJVernalis_TKN_conc_jul_wilcox <- wilcox.test(recent_jul$ConcDay, early_jul$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_aug <- subset(early_decade_monthly_mn, month==8)
recent_aug <- subset(recent_decade_monthly_mn, month==8)
SanJVernalis_TKN_conc_aug_wilcox <- wilcox.test(recent_aug$ConcDay, early_aug$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_sep <- subset(early_decade_monthly_mn, month==9)
recent_sep <- subset(recent_decade_monthly_mn, month==9)
SanJVernalis_TKN_conc_sep_wilcox <- wilcox.test(recent_sep$ConcDay, early_sep$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_oct <- subset(early_decade_monthly_mn, month==10)
recent_oct <- subset(recent_decade_monthly_mn, month==10)
SanJVernalis_TKN_conc_oct_wilcox <- wilcox.test(recent_oct$ConcDay, early_oct$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_nov <- subset(early_decade_monthly_mn, month==11)
recent_nov <- subset(recent_decade_monthly_mn, month==11)
SanJVernalis_TKN_conc_nov_wilcox <- wilcox.test(recent_nov$ConcDay, early_nov$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_dec <- subset(early_decade_monthly_mn, month==12)
recent_dec <- subset(recent_decade_monthly_mn, month==12)
SanJVernalis_TKN_conc_dec_wilcox <- wilcox.test(recent_dec$ConcDay, early_dec$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

Conc_compare <- data.frame(chng_est=c(SanJVernalis_TKN_conc_oct_wilcox$est,
                                      SanJVernalis_TKN_conc_nov_wilcox$est,
                                      SanJVernalis_TKN_conc_dec_wilcox$est,
                                      SanJVernalis_TKN_conc_jan_wilcox$est,
                                      SanJVernalis_TKN_conc_feb_wilcox$est,
                                      SanJVernalis_TKN_conc_mar_wilcox$est,
                                      SanJVernalis_TKN_conc_apr_wilcox$est,
                                      SanJVernalis_TKN_conc_may_wilcox$est,
                                      SanJVernalis_TKN_conc_jun_wilcox$est,
                                      SanJVernalis_TKN_conc_jul_wilcox$est,
                                      SanJVernalis_TKN_conc_aug_wilcox$est,
                                      SanJVernalis_TKN_conc_sep_wilcox$est),
                           low_conf=c(SanJVernalis_TKN_conc_oct_wilcox$conf.int[1],
                                      SanJVernalis_TKN_conc_nov_wilcox$conf.int[1],
                                      SanJVernalis_TKN_conc_dec_wilcox$conf.int[1],
                                      SanJVernalis_TKN_conc_jan_wilcox$conf.int[1],
                                      SanJVernalis_TKN_conc_feb_wilcox$conf.int[1],
                                      SanJVernalis_TKN_conc_mar_wilcox$conf.int[1],
                                      SanJVernalis_TKN_conc_apr_wilcox$conf.int[1],
                                      SanJVernalis_TKN_conc_may_wilcox$conf.int[1],
                                      SanJVernalis_TKN_conc_jun_wilcox$conf.int[1],
                                      SanJVernalis_TKN_conc_jul_wilcox$conf.int[1],
                                      SanJVernalis_TKN_conc_aug_wilcox$conf.int[1],
                                      SanJVernalis_TKN_conc_sep_wilcox$conf.int[1]),
                           up_conf=c(SanJVernalis_TKN_conc_oct_wilcox$conf.int[2],
                                     SanJVernalis_TKN_conc_nov_wilcox$conf.int[2],
                                     SanJVernalis_TKN_conc_dec_wilcox$conf.int[2],
                                     SanJVernalis_TKN_conc_jan_wilcox$conf.int[2],
                                     SanJVernalis_TKN_conc_feb_wilcox$conf.int[2],
                                     SanJVernalis_TKN_conc_mar_wilcox$conf.int[2],
                                     SanJVernalis_TKN_conc_apr_wilcox$conf.int[2],
                                     SanJVernalis_TKN_conc_may_wilcox$conf.int[2],
                                     SanJVernalis_TKN_conc_jun_wilcox$conf.int[2],
                                     SanJVernalis_TKN_conc_jul_wilcox$conf.int[2],
                                     SanJVernalis_TKN_conc_aug_wilcox$conf.int[2],
                                     SanJVernalis_TKN_conc_sep_wilcox$conf.int[2]))

write.table(Conc_compare, "SanJVernalis_TKN_conc_wilcox.txt", quote=FALSE, row.names=FALSE)

rng <- max(abs(c(Conc_compare$up_conf, Conc_compare$low_conf)))
tiff("SanJVernalis_TKN_conc_shift_wilcox_Vert_Bars.tif", height=600, width=800, res=130)
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
                dimnames = list(c("1973-1983", "2009-2019"),
                                c(format(seq(as.Date('1973-10-01'), as.Date('1974-09-01'), by='month'), '%b'))))

mx <- max(c((early_decade_mon_mn_flx$FluxDay + early_decade_mon_sd_flx$FluxDay), (recent_decade_mon_mn_flx$FluxDay + recent_decade_mon_sd_flx$FluxDay)))
tiff("timing_shift_inSanJVernalis_TKN_load_monthly_means.tif", height=800, width=900, res=130)
x <- barplot(mdat3, beside=TRUE, las=1, ylim=c(0,mx), col = c("lightblue", "mistyrose"))
abline(h=0)
arrows(x0=x[1,], y0=early_decade_mon_mn_flx$FluxDay - early_decade_mon_sd_flx$FluxDay, x1=x[1,], y1=early_decade_mon_mn_flx$FluxDay + early_decade_mon_sd_flx$FluxDay, angle=90, length=0.04, code=3)
arrows(x0=x[2,], y0=recent_decade_mon_mn_flx$FluxDay - recent_decade_mon_sd_flx$FluxDay, x1=x[2,], y1=recent_decade_mon_mn_flx$FluxDay + recent_decade_mon_sd_flx$FluxDay, angle=90, length=0.04, code=3)
mtext(side=2, expression(paste(TKN,', kg ',month^-1,sep='')), line=2.5)
legend(x=30, y=0.9 * mx, c("1973-1983", "2009-2019"), pch=c(22,22), pt.cex=2, pt.bg=c("lightblue", "mistyrose"), bty='n', xpd=TRUE)
dev.off()

# Apply Wilcox.text to the monthly loads here...
early_jan_flx <- subset(early_decade_monthly_flx, month==1)
recent_jan_flx <- subset(recent_decade_monthly_flx, month==1)
SanJVernalis_TKN_flux_jan_wilcox <- wilcox.test(recent_jan_flx$FluxDay, early_jan_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_feb_flx <- subset(early_decade_monthly_flx, month==2)
recent_feb_flx <- subset(recent_decade_monthly_flx, month==2)
SanJVernalis_TKN_flux_feb_wilcox <- wilcox.test(recent_feb_flx$FluxDay, early_feb_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_mar_flx <- subset(early_decade_monthly_flx, month==3)
recent_mar_flx <- subset(recent_decade_monthly_flx, month==3)
SanJVernalis_TKN_flux_mar_wilcox <- wilcox.test(recent_mar_flx$FluxDay, early_mar_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_apr_flx <- subset(early_decade_monthly_flx, month==4)
recent_apr_flx <- subset(recent_decade_monthly_flx, month==4)
SanJVernalis_TKN_flux_apr_wilcox <- wilcox.test(recent_apr_flx$FluxDay, early_apr_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_may_flx <- subset(early_decade_monthly_flx, month==5)
recent_may_flx <- subset(recent_decade_monthly_flx, month==5)
SanJVernalis_TKN_flux_may_wilcox <- wilcox.test(recent_may_flx$FluxDay, early_may_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_jun_flx <- subset(early_decade_monthly_flx, month==6)
recent_jun_flx <- subset(recent_decade_monthly_flx, month==6)
SanJVernalis_TKN_flux_jun_wilcox <- wilcox.test(recent_jun_flx$FluxDay, early_jun_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_jul_flx <- subset(early_decade_monthly_flx, month==7)
recent_jul_flx <- subset(recent_decade_monthly_flx, month==7)
SanJVernalis_TKN_flux_jul_wilcox <- wilcox.test(recent_jul_flx$FluxDay, early_jul_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_aug_flx <- subset(early_decade_monthly_flx, month==8)
recent_aug_flx <- subset(recent_decade_monthly_flx, month==8)
SanJVernalis_TKN_flux_aug_wilcox <- wilcox.test(recent_aug_flx$FluxDay, early_aug_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_sep_flx <- subset(early_decade_monthly_flx, month==9)
recent_sep_flx <- subset(recent_decade_monthly_flx, month==9)
SanJVernalis_TKN_flux_sep_wilcox <- wilcox.test(recent_sep_flx$FluxDay, early_sep_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_oct_flx <- subset(early_decade_monthly_flx, month==10)
recent_oct_flx <- subset(recent_decade_monthly_flx, month==10)
SanJVernalis_TKN_flux_oct_wilcox <- wilcox.test(recent_oct_flx$FluxDay, early_oct_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_nov_flx <- subset(early_decade_monthly_flx, month==11)
recent_nov_flx <- subset(recent_decade_monthly_flx, month==11)
SanJVernalis_TKN_flux_nov_wilcox <- wilcox.test(recent_nov_flx$FluxDay, early_nov_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_dec_flx <- subset(early_decade_monthly_flx, month==12)
recent_dec_flx <- subset(recent_decade_monthly_flx, month==12)
SanJVernalis_TKN_flux_dec_wilcox <- wilcox.test(recent_dec_flx$FluxDay, early_dec_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)


Flux_compare <- data.frame(chng_est=c(SanJVernalis_TKN_flux_oct_wilcox$est,
                                      SanJVernalis_TKN_flux_nov_wilcox$est,
                                      SanJVernalis_TKN_flux_dec_wilcox$est,
                                      SanJVernalis_TKN_flux_jan_wilcox$est,
                                      SanJVernalis_TKN_flux_feb_wilcox$est,
                                      SanJVernalis_TKN_flux_mar_wilcox$est,
                                      SanJVernalis_TKN_flux_apr_wilcox$est,
                                      SanJVernalis_TKN_flux_may_wilcox$est,
                                      SanJVernalis_TKN_flux_jun_wilcox$est,
                                      SanJVernalis_TKN_flux_jul_wilcox$est,
                                      SanJVernalis_TKN_flux_aug_wilcox$est,
                                      SanJVernalis_TKN_flux_sep_wilcox$est),
                           low_conf=c(SanJVernalis_TKN_flux_oct_wilcox$conf.int[1],
                                      SanJVernalis_TKN_flux_nov_wilcox$conf.int[1],
                                      SanJVernalis_TKN_flux_dec_wilcox$conf.int[1],
                                      SanJVernalis_TKN_flux_jan_wilcox$conf.int[1],
                                      SanJVernalis_TKN_flux_feb_wilcox$conf.int[1],
                                      SanJVernalis_TKN_flux_mar_wilcox$conf.int[1],
                                      SanJVernalis_TKN_flux_apr_wilcox$conf.int[1],
                                      SanJVernalis_TKN_flux_may_wilcox$conf.int[1],
                                      SanJVernalis_TKN_flux_jun_wilcox$conf.int[1],
                                      SanJVernalis_TKN_flux_jul_wilcox$conf.int[1],
                                      SanJVernalis_TKN_flux_aug_wilcox$conf.int[1],
                                      SanJVernalis_TKN_flux_sep_wilcox$conf.int[1]),
                           up_conf=c(SanJVernalis_TKN_flux_oct_wilcox$conf.int[2],
                                     SanJVernalis_TKN_flux_nov_wilcox$conf.int[2],
                                     SanJVernalis_TKN_flux_dec_wilcox$conf.int[2],
                                     SanJVernalis_TKN_flux_jan_wilcox$conf.int[2],
                                     SanJVernalis_TKN_flux_feb_wilcox$conf.int[2],
                                     SanJVernalis_TKN_flux_mar_wilcox$conf.int[2],
                                     SanJVernalis_TKN_flux_apr_wilcox$conf.int[2],
                                     SanJVernalis_TKN_flux_may_wilcox$conf.int[2],
                                     SanJVernalis_TKN_flux_jun_wilcox$conf.int[2],
                                     SanJVernalis_TKN_flux_jul_wilcox$conf.int[2],
                                     SanJVernalis_TKN_flux_aug_wilcox$conf.int[2],
                                     SanJVernalis_TKN_flux_sep_wilcox$conf.int[2]))

write.table(Flux_compare, "SanJVernalis_TKN_flux_wilcox.txt", quote=FALSE, row.names=FALSE)

rng_flx <- max(abs(c(Flux_compare$up_conf, Flux_compare$low_conf)))
tiff("SanJVernalis_TKN_flux_shift_wilcox_Vert_Bars.tif", height=600, width=800, res=130)
par(mar=c(4,5,0.5,0.5))
plot(seq(1:12), Flux_compare$chng_est, typ='h', lend=1, lwd=15, col='white', xaxt='n', xlim=c(1,13), ylim=c(-rng_flx, rng_flx), xlab="Month", ylab=expression(paste("Median Flux Change, kg",sep='')), las=1)
plotCI(seq(1:12), Flux_compare$chng_est, ui=Flux_compare$up_conf, li=Flux_compare$low_conf, pch=16, add=TRUE)
abline(h=0)
axis(side=1,at=seq(1,12,by=1), labels=format(c(seq(as.Date("2000-10-01"), as.Date("2000-12-01"), by="month"), seq(as.Date("2000-01-01"), as.Date("2000-09-01"), by="month")),'%b'), las=2)
legend('topright', c("Median difference", "90% Confidence Interval for the Median"), pch=c(16,NA), lwd=c(NA,1), pt.cex=c(1,NA), pt.bg=c('black',NA), bty='n', bg='white')
dev.off()

#################  Using the plotConcQSmooth function
###########
#First do flow duration analysis
flowDuration(eList, centerDate = "06-01", qUnit = 2, span = 30)
date1 <- "1973-06-01"
date2 <- "2000-06-01"
date3 <- "2019-06-01"
qLow= baseQ
qHigh=highQ7

tiff("Vernalis_Date_DischargeSanJVernalis_TKN_conc_no_log.tif",height = 700, width = 1000, res=120)
plotConcQSmooth(eList,date1, date2, date3,qLow, qHigh, logScale=FALSE,printLegend =TRUE,legendLeft=0,legendTop=0,printTitle=TRUE)
dev.off()

# End of non-standard EGRET plot section requested by Michael
# --------------------------------------------------------------------------------------------------------




# ---------------------------
# Now run the EGRETci package
# ---------------------------

# Change working directory
#setwd(""/Users/joed/PES_Project/Vernalis_EGRET/TKN/"")
setwd("C:/Users/dsaleh/Documents/GitHub/PES_Project/Vernalis_EGRET/TKN/")
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
                        bootBreak = 100, 
                        blockLength = 200)
eBoot <- wBT(eList, caseSetUp, fileName ="outputText.txt")


#

# 
saveEGRETci(eList, eBoot, caseSetUp, fileName = "EGRETci_output_TN")

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

tiff("histo_NH3_UpTruck_S_UpTruck_Trend_conc_flux.tif", height = 700, width = 1200, res=120)
par(mfrow=c(1,2))
plotHistogramTrend2(eBoot, caseSetUp, eList, flux=FALSE, xSeq = seq(-8000,8000,5),las=1,xlim=c(-100,100))
abline(h=0)

plotHistogramTrend2(eBoot, caseSetUp, eList, flux=TRUE, xSeq = seq(-50000,50000,5),las=1)
abline(h=0)
dev.off()

nBoot <- 200
blockLength <- 200
coreOut <- 5 #Number of cores to leave out of processing tasks

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

tiff("Ann_Avg_Conc_&_Ann_Flow_Normalized_Conc_Boot_SanJVernalis_TN.tif", height = 500, width = 600, res=110)
plotConcHistBoot(eList, CIAnnualResults, plotFlowNorm=TRUE, showYLabels=TRUE, showYAxis=TRUE,col=4)
polygon(x=conc.poly.x, y=conc.poly.y, col=rgb(24,116,205,40,max=255),border=NA)
dev.off()

tiff("Ann_Flux_&_Ann_Flow_Normalized_Flux_Boot_SanJVernalis_TKN.tif", height = 500, width = 600, res=110)
plotFluxHistBoot(eList, fluxUnit=13, CIAnnualResults, showYLabels=TRUE, showYAxis=TRUE, col=4)
polygon(x=flux.poly.x, y=flux.poly.y, col=rgb(24,116,205,40,max=255),border=NA)
dev.off()

setSweave("SanJVernalis_TKN_Conc_EGRETCI",7,7)
plotConcHistBoot(eList, CIAnnualResults, plotFlowNorm=TRUE, showYLabels=TRUE, showYAxis=TRUE,col=4)
polygon(x=conc.poly.x, y=conc.poly.y, col=rgb(24,116,205,40,max=255),border=NA)
graphics.off()

setSweave("SanJVernalis_TKN_Flux_EGRETCI",7,7)
plotFluxHistBoot(eList, fluxUnit=13, CIAnnualResults, showYLabels=TRUE, showYAxis=TRUE, col=4)
polygon(x=flux.poly.x, y=flux.poly.y, col=rgb(24,116,205,40,max=255),border=NA)
dev.off()
graphics.off()


saveEGRETci(eList, eBoot, fileName="TN_Boot_SanJVernalis.RData")
save(repAnnual,file="RepAnnual")

# load(file="TN_Boot_SanJVernalis.RData")
# load(file="RepAnnual")


#############################################################
# Working on TP
#############################################################

startDate <- "1971-10-01"
endDate <- "2019-06-01"
siteNumber <- "11303500"
QParameterCd <- "00060"
parameterCd <- "00665"  # "TP"
filePath <- "C:/Users/dsaleh/Documents/GitHub/PES_Project/Vernalis_EGRET/"
#filePath <- ("/Users/joed/PES_Project/Vernalis_EGRET/")
setwd("C:/Users/dsaleh/Documents/GitHub/PES_Project/Vernalis_EGRET")
#setwd("/Users/joed/PES_Project/Vernalis_EGRET/")
Daily <- readNWISDaily(siteNumber, QParameterCd, startDate, endDate)
#Sample <- readNWISSample(siteNumber, parameterCd, startDate, endDate)

##the missing time using Charlie Kratzer's data
#write.csv(Sample,"C:/Users/dsaleh/Documents/GitHub/PES_Project/Vernalis_EGRET/NWIS_TP.csv")

##Set fileName to combined Kratzer and NWIS data set
fileName <- "NWIS_and_Kratzer_TP.csv"
Sample <- readUserSample(filePath, fileName)
Sample <- removeDuplicates(Sample)

#Sample <- readNWISSample(siteNumber, parameterCd, startDate, endDate)
INFO <- readNWISInfo(siteNumber = siteNumber, parameterCd = parameterCd, interactive=FALSE)
INFO$staAbbrev <- paste(strsplit(INFO$station_nm," ")[[1]][1],strsplit(INFO$station_nm," ")[[1]][2])

# Have a look at the available range of TP data
range(Sample$Date)
#"1971-10-14" "2019-05-07"
eList <- mergeReport(INFO, Daily, Sample)

# Change the working directory; redirect plot output to TP folder
setwd("/Users/joed/PES_Project/Vernalis_EGRET/")
setwd("C:/Users/dsaleh/Documents/GitHub/PES_Project/Vernalis_EGRET/")
subDir <- 'TP/EGRET_plots'
if (file.exists(subDir)){
  setwd(file.path(getwd(),subDir))
} else {
  dir.create(file.path(getwd(),subDir), recursive=TRUE)
  setwd(file.path(getwd(),subDir))
}

# Plot water quality data
tiff("Conc_vs_Time_SanJVernalis_TP.tif", height = 600, width = 800, res=120)
plotConcTime(eList)
dev.off()

# Now, a classic Q-C plot
tiff("Conc-Q_SanJVernalis_TP.tif", height = 600, width = 800, res=120)
plotConcQ(eList, logScale=TRUE)
dev.off()

# The data set as flux values rather than as concentrations
tiff("Flux-Q_SanJVernalis_TP.tif", height = 600, width = 800, res=120)
plotFluxQ(eList, fluxUnit=4)
dev.off()

# Monthly boxplots
tiff("Monthly-Conc_BoxPlots_SanJVernalis_TP.tif", height = 600, width = 800, res=120)
boxConcMonth(eList, logScale=TRUE)
dev.off()

# Flow on days sampled vs. all other days
tiff("Flow_on_days_sampled_vs_all_other_days_SanJVernalis_TP.tif", height = 600, width = 800, res=120)
boxQTwice(eList, qUnit=1)
dev.off()

#########################################
# Now start the Flow-Normalized Analysis
#########################################

# Build the regression model
eList <- modelEstimation(eList, windowY = 7, windowQ = 2, windowS = 0.5, minNumObs = 100, minNumUncen =50)
eList_TP <- eList

MonthlyResults <- calculateMonthlyResults(eList)

# Dump TP-related flow-normalized data to text file for bringing together with other monitoring sites
paLong <- 12
paStart <- 10
localDaily <- getDaily(eList_TP)
localAnnualResults <- setupYears(paStart = paStart, paLong = paLong, localDaily = localDaily)
write.table(localAnnualResults, file = 'Vern_TP_RawVals.txt', quote=FALSE, row.names=FALSE)
write.csv(Daily,"localDailyTP.csv")

# Plot the annual average concentration and annual flow-normalized concentration
tiff("Ann_Avg_Conc_&_Ann_Flow_Normalized_Conc_SanJVernalis_TP.tif", height = 600, width = 800, res=120)
plotConcHist(eList, plotFlowNorm=TRUE)
dev.off()

# Plot the annual flux and annual flow-normalized flux
tiff("Ann_Flux_&_Ann_Flow_Normalized_Flux_SanJVernalis_TP.tif", height = 600, width = 800, res=120)
plotFluxHist(eList, plotFlowNorm = TRUE) # fluxMax) # fluxMax
dev.off()

# Look for a trend change:
tableChange(eList, fluxUnit=6, yearPoints=c(1971,1981,1991,2001,2011))


#
#Generate out-of-the-box diagnostic plots
tiff("fluxBiasMulti_SanJVernalis_TP.tif", height = 1200, width = 1000, res=120)
fluxBiasMulti(eList, moreTitle = "WRTDS")
dev.off()

tiff("Modeled_Daily_Conc_wObservations_SanJVernalis_TP.tif", height = 800, width = 1000, res=120)
plotConcTimeDaily(eList)
dev.off()

# Exploring model behavior and adjusting model parameters
tiff("Contours_SanJVernalis_TP.tif", height = 700, width = 1000, res=120)
plotContours(eList, qBottom=5,qTop=1000,yearStart=1971,yearEnd=2019, contourLevels=seq(0.01,0.08,by=0.001), color.palette = colorRampPalette(c("violet", "purple", "blue", "cyan", "green", "yellow", "orange", "red"))) 
dev.off()

tiff("Log_Contours_SanJVernalis_TP.tif", height = 700, width = 1000, res=120)
plotContours(eList, qBottom=5, qTop=1000, yearStart=1971, yearEnd=2019, contourLevels=seq(-4.380,-2.7,by=0.1), color.palette = colorRampPalette(c("violet", "purple", "blue", "cyan", "green", "yellow", "orange", "red")), whatSurface=1) 
dev.off()

tiff("StdErr_of_Log_Contours_SanJVernalis_TP.tif", height = 700, width = 1000, res=120)
plotContours(eList, qBottom=5, qTop=1000, yearStart=1971, yearEnd=2019, contourLevels=seq(0.16,0.45,by=0.01), color.palette = colorRampPalette(c("violet", "purple", "blue", "cyan", "green", "yellow", "orange", "red")), whatSurface=2) 
dev.off()

tiff("Contours_Difference_TP_SanJVernalis.tif", height = 700, width = 1000, res=120)
plotDiffContours(eList, 1971,2019,5,1000,maxDiff=0.25)
dev.off()

tiff("Contours_PercentDifference_TP_SanJVernalis.tif", height = 700, width = 1000, res=120)
plotDiffContours(eList, 1971,2019,5,1000, maxDiff=100, plotPercent=TRUE)
dev.off()

tiff("Contours_PercentDifference2_TP_SanJVernalis.tif", height = 700, width = 1000, res=120)
plotDiffContours2(eList, 1971,2019,5,1000, maxDiff=c(-100,100), plotPercent=TRUE, lwd=3, color.palette=colorRampPalette(c("purple","blue","lightblue","white","yellow", "orange", "red")),tick.lwd = 1)
dev.off()

Sample$WY <- trunc(Sample$DecYear+0.25) 
tiff("Monthly_Boxplot_Inorg__TP.tif", height = 700, width = 1000, res=120)
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

tiff("Discharge_specific_trends_centered_on_06-01.tif", height = 600, width = 1200, res=120)
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
tiff("Flow_Normalized_Conc_TC1_TP.tif", height = 600, width = 800, res=120)
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

localDaily <- getDaily(eList_TP)

# Will need to adjust the date range below based on each gages unique start/stop dates
early_decade <- subset(localDaily, localDaily$Date > as.Date('1971-10-01') & localDaily$Date < as.Date('1981-10-01'))
recent_decade <- subset(localDaily, localDaily$Date > as.Date('2009-07-01'))


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
                dimnames = list(c("1971-1981", "2009-2019"),
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
legend(x=25, y=0.9 * mx, c("1972-1982", "2009-2019"), pch=c(22,22), pt.cex=2, pt.bg=c("lightblue", "mistyrose"), bty='n', xpd=TRUE)
dev.off()


# Now attempting a Wilcox Test (aka Mann-Whitney-Wilcoxon Rank Sum test)
# ----------------------------------------------------------------------
early_jan <- subset(early_decade_monthly_mn, month==1)
recent_jan <- subset(recent_decade_monthly_mn, month==1)
SanJVernalis_TP_conc_jan_wilcox <- wilcox.test(recent_jan$ConcDay, early_jan$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_feb <- subset(early_decade_monthly_mn, month==2)
recent_feb <- subset(recent_decade_monthly_mn, month==2)
SanJVernalis_TP_conc_feb_wilcox <- wilcox.test(recent_feb$ConcDay, early_feb$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_mar <- subset(early_decade_monthly_mn, month==3)
recent_mar <- subset(recent_decade_monthly_mn, month==3)
SanJVernalis_TP_conc_mar_wilcox <- wilcox.test(recent_mar$ConcDay, early_mar$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_apr <- subset(early_decade_monthly_mn, month==4)
recent_apr <- subset(recent_decade_monthly_mn, month==4)
SanJVernalis_TP_conc_apr_wilcox <- wilcox.test(recent_apr$ConcDay, early_apr$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_may <- subset(early_decade_monthly_mn, month==5)
recent_may <- subset(recent_decade_monthly_mn, month==5)
SanJVernalis_TP_conc_may_wilcox <- wilcox.test(recent_may$ConcDay, early_may$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_jun <- subset(early_decade_monthly_mn, month==6)
recent_jun <- subset(recent_decade_monthly_mn, month==6)
SanJVernalis_TP_conc_jun_wilcox <- wilcox.test(recent_jun$ConcDay, early_jun$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_jul <- subset(early_decade_monthly_mn, month==7)
recent_jul <- subset(recent_decade_monthly_mn, month==7)
SanJVernalis_TP_conc_jul_wilcox <- wilcox.test(recent_jul$ConcDay, early_jul$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_aug <- subset(early_decade_monthly_mn, month==8)
recent_aug <- subset(recent_decade_monthly_mn, month==8)
SanJVernalis_TP_conc_aug_wilcox <- wilcox.test(recent_aug$ConcDay, early_aug$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_sep <- subset(early_decade_monthly_mn, month==9)
recent_sep <- subset(recent_decade_monthly_mn, month==9)
SanJVernalis_TP_conc_sep_wilcox <- wilcox.test(recent_sep$ConcDay, early_sep$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_oct <- subset(early_decade_monthly_mn, month==10)
recent_oct <- subset(recent_decade_monthly_mn, month==10)
SanJVernalis_TP_conc_oct_wilcox <- wilcox.test(recent_oct$ConcDay, early_oct$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_nov <- subset(early_decade_monthly_mn, month==11)
recent_nov <- subset(recent_decade_monthly_mn, month==11)
SanJVernalis_TP_conc_nov_wilcox <- wilcox.test(recent_nov$ConcDay, early_nov$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_dec <- subset(early_decade_monthly_mn, month==12)
recent_dec <- subset(recent_decade_monthly_mn, month==12)
SanJVernalis_TP_conc_dec_wilcox <- wilcox.test(recent_dec$ConcDay, early_dec$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

Conc_compare <- data.frame(chng_est=c(SanJVernalis_TP_conc_oct_wilcox$est,
                                      SanJVernalis_TP_conc_nov_wilcox$est,
                                      SanJVernalis_TP_conc_dec_wilcox$est,
                                      SanJVernalis_TP_conc_jan_wilcox$est,
                                      SanJVernalis_TP_conc_feb_wilcox$est,
                                      SanJVernalis_TP_conc_mar_wilcox$est,
                                      SanJVernalis_TP_conc_apr_wilcox$est,
                                      SanJVernalis_TP_conc_may_wilcox$est,
                                      SanJVernalis_TP_conc_jun_wilcox$est,
                                      SanJVernalis_TP_conc_jul_wilcox$est,
                                      SanJVernalis_TP_conc_aug_wilcox$est,
                                      SanJVernalis_TP_conc_sep_wilcox$est),
                           low_conf=c(SanJVernalis_TP_conc_oct_wilcox$conf.int[1],
                                      SanJVernalis_TP_conc_nov_wilcox$conf.int[1],
                                      SanJVernalis_TP_conc_dec_wilcox$conf.int[1],
                                      SanJVernalis_TP_conc_jan_wilcox$conf.int[1],
                                      SanJVernalis_TP_conc_feb_wilcox$conf.int[1],
                                      SanJVernalis_TP_conc_mar_wilcox$conf.int[1],
                                      SanJVernalis_TP_conc_apr_wilcox$conf.int[1],
                                      SanJVernalis_TP_conc_may_wilcox$conf.int[1],
                                      SanJVernalis_TP_conc_jun_wilcox$conf.int[1],
                                      SanJVernalis_TP_conc_jul_wilcox$conf.int[1],
                                      SanJVernalis_TP_conc_aug_wilcox$conf.int[1],
                                      SanJVernalis_TP_conc_sep_wilcox$conf.int[1]),
                           up_conf=c(SanJVernalis_TP_conc_oct_wilcox$conf.int[2],
                                     SanJVernalis_TP_conc_nov_wilcox$conf.int[2],
                                     SanJVernalis_TP_conc_dec_wilcox$conf.int[2],
                                     SanJVernalis_TP_conc_jan_wilcox$conf.int[2],
                                     SanJVernalis_TP_conc_feb_wilcox$conf.int[2],
                                     SanJVernalis_TP_conc_mar_wilcox$conf.int[2],
                                     SanJVernalis_TP_conc_apr_wilcox$conf.int[2],
                                     SanJVernalis_TP_conc_may_wilcox$conf.int[2],
                                     SanJVernalis_TP_conc_jun_wilcox$conf.int[2],
                                     SanJVernalis_TP_conc_jul_wilcox$conf.int[2],
                                     SanJVernalis_TP_conc_aug_wilcox$conf.int[2],
                                     SanJVernalis_TP_conc_sep_wilcox$conf.int[2]))

write.table(Conc_compare, "SanJVernalis_TP_conc_wilcox.txt", quote=FALSE, row.names=FALSE)

rng <- max(abs(c(Conc_compare$up_conf, Conc_compare$low_conf)))
tiff("SanJVernalis_TP_conc_shift_wilcox_Vert_Bars.tif", height=600, width=800, res=130)
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
                dimnames = list(c("1971-1981", "2009-2019"),
                                c(format(seq(as.Date('1973-10-01'), as.Date('1974-09-01'), by='month'), '%b'))))

mx <- max(c((early_decade_mon_mn_flx$FluxDay + early_decade_mon_sd_flx$FluxDay), (recent_decade_mon_mn_flx$FluxDay + recent_decade_mon_sd_flx$FluxDay)))
tiff("timing_shift_in_TP_load_monthly_means.tif", height=800, width=900, res=130)
x <- barplot(mdat3, beside=TRUE, las=1, ylim=c(0,mx), col = c("lightblue", "mistyrose"))
abline(h=0)
arrows(x0=x[1,], y0=early_decade_mon_mn_flx$FluxDay - early_decade_mon_sd_flx$FluxDay, x1=x[1,], y1=early_decade_mon_mn_flx$FluxDay + early_decade_mon_sd_flx$FluxDay, angle=90, length=0.04, code=3)
arrows(x0=x[2,], y0=recent_decade_mon_mn_flx$FluxDay - recent_decade_mon_sd_flx$FluxDay, x1=x[2,], y1=recent_decade_mon_mn_flx$FluxDay + recent_decade_mon_sd_flx$FluxDay, angle=90, length=0.04, code=3)
mtext(side=2, expression(paste(TP,', kg ',month^-1,sep='')), line=2.5)
legend(x=30, y=0.9 * mx, c("1972-1982", "2009-2019"), pch=c(22,22), pt.cex=2, pt.bg=c("lightblue", "mistyrose"), bty='n', xpd=TRUE)
dev.off()

# Apply Wilcox.text to the monthly loads here...
early_jan_flx <- subset(early_decade_monthly_flx, month==1)
recent_jan_flx <- subset(recent_decade_monthly_flx, month==1)
SanJVernalis_TP_flux_jan_wilcox <- wilcox.test(recent_jan_flx$FluxDay, early_jan_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_feb_flx <- subset(early_decade_monthly_flx, month==2)
recent_feb_flx <- subset(recent_decade_monthly_flx, month==2)
SanJVernalis_TP_flux_feb_wilcox <- wilcox.test(recent_feb_flx$FluxDay, early_feb_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_mar_flx <- subset(early_decade_monthly_flx, month==3)
recent_mar_flx <- subset(recent_decade_monthly_flx, month==3)
SanJVernalis_TP_flux_mar_wilcox <- wilcox.test(recent_mar_flx$FluxDay, early_mar_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_apr_flx <- subset(early_decade_monthly_flx, month==4)
recent_apr_flx <- subset(recent_decade_monthly_flx, month==4)
SanJVernalis_TP_flux_apr_wilcox <- wilcox.test(recent_apr_flx$FluxDay, early_apr_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_may_flx <- subset(early_decade_monthly_flx, month==5)
recent_may_flx <- subset(recent_decade_monthly_flx, month==5)
SanJVernalis_TP_flux_may_wilcox <- wilcox.test(recent_may_flx$FluxDay, early_may_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_jun_flx <- subset(early_decade_monthly_flx, month==6)
recent_jun_flx <- subset(recent_decade_monthly_flx, month==6)
SanJVernalis_TP_flux_jun_wilcox <- wilcox.test(recent_jun_flx$FluxDay, early_jun_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_jul_flx <- subset(early_decade_monthly_flx, month==7)
recent_jul_flx <- subset(recent_decade_monthly_flx, month==7)
SanJVernalis_TP_flux_jul_wilcox <- wilcox.test(recent_jul_flx$FluxDay, early_jul_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_aug_flx <- subset(early_decade_monthly_flx, month==8)
recent_aug_flx <- subset(recent_decade_monthly_flx, month==8)
SanJVernalis_TP_flux_aug_wilcox <- wilcox.test(recent_aug_flx$FluxDay, early_aug_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_sep_flx <- subset(early_decade_monthly_flx, month==9)
recent_sep_flx <- subset(recent_decade_monthly_flx, month==9)
SanJVernalis_TP_flux_sep_wilcox <- wilcox.test(recent_sep_flx$FluxDay, early_sep_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_oct_flx <- subset(early_decade_monthly_flx, month==10)
recent_oct_flx <- subset(recent_decade_monthly_flx, month==10)
SanJVernalis_TP_flux_oct_wilcox <- wilcox.test(recent_oct_flx$FluxDay, early_oct_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_nov_flx <- subset(early_decade_monthly_flx, month==11)
recent_nov_flx <- subset(recent_decade_monthly_flx, month==11)
SanJVernalis_TP_flux_nov_wilcox <- wilcox.test(recent_nov_flx$FluxDay, early_nov_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_dec_flx <- subset(early_decade_monthly_flx, month==12)
recent_dec_flx <- subset(recent_decade_monthly_flx, month==12)
SanJVernalis_TP_flux_dec_wilcox <- wilcox.test(recent_dec_flx$FluxDay, early_dec_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)


Flux_compare <- data.frame(chng_est=c(SanJVernalis_TP_flux_oct_wilcox$est,
                                      SanJVernalis_TP_flux_nov_wilcox$est,
                                      SanJVernalis_TP_flux_dec_wilcox$est,
                                      SanJVernalis_TP_flux_jan_wilcox$est,
                                      SanJVernalis_TP_flux_feb_wilcox$est,
                                      SanJVernalis_TP_flux_mar_wilcox$est,
                                      SanJVernalis_TP_flux_apr_wilcox$est,
                                      SanJVernalis_TP_flux_may_wilcox$est,
                                      SanJVernalis_TP_flux_jun_wilcox$est,
                                      SanJVernalis_TP_flux_jul_wilcox$est,
                                      SanJVernalis_TP_flux_aug_wilcox$est,
                                      SanJVernalis_TP_flux_sep_wilcox$est),
                           low_conf=c(SanJVernalis_TP_flux_oct_wilcox$conf.int[1],
                                      SanJVernalis_TP_flux_nov_wilcox$conf.int[1],
                                      SanJVernalis_TP_flux_dec_wilcox$conf.int[1],
                                      SanJVernalis_TP_flux_jan_wilcox$conf.int[1],
                                      SanJVernalis_TP_flux_feb_wilcox$conf.int[1],
                                      SanJVernalis_TP_flux_mar_wilcox$conf.int[1],
                                      SanJVernalis_TP_flux_apr_wilcox$conf.int[1],
                                      SanJVernalis_TP_flux_may_wilcox$conf.int[1],
                                      SanJVernalis_TP_flux_jun_wilcox$conf.int[1],
                                      SanJVernalis_TP_flux_jul_wilcox$conf.int[1],
                                      SanJVernalis_TP_flux_aug_wilcox$conf.int[1],
                                      SanJVernalis_TP_flux_sep_wilcox$conf.int[1]),
                           up_conf=c(SanJVernalis_TP_flux_oct_wilcox$conf.int[2],
                                     SanJVernalis_TP_flux_nov_wilcox$conf.int[2],
                                     SanJVernalis_TP_flux_dec_wilcox$conf.int[2],
                                     SanJVernalis_TP_flux_jan_wilcox$conf.int[2],
                                     SanJVernalis_TP_flux_feb_wilcox$conf.int[2],
                                     SanJVernalis_TP_flux_mar_wilcox$conf.int[2],
                                     SanJVernalis_TP_flux_apr_wilcox$conf.int[2],
                                     SanJVernalis_TP_flux_may_wilcox$conf.int[2],
                                     SanJVernalis_TP_flux_jun_wilcox$conf.int[2],
                                     SanJVernalis_TP_flux_jul_wilcox$conf.int[2],
                                     SanJVernalis_TP_flux_aug_wilcox$conf.int[2],
                                     SanJVernalis_TP_flux_sep_wilcox$conf.int[2]))

write.table(Flux_compare, "SanJVernalis_TP_fluxwilcox.txt", quote=FALSE, row.names=FALSE)

rng_flx <- max(abs(c(Flux_compare$up_conf, Flux_compare$low_conf)))
tiff("SanJVernalis_TP_fluxshift_wilcox_Vert_Bars.tif", height=600, width=800, res=130)
par(mar=c(4,5,0.5,0.5))
plot(seq(1:12), Flux_compare$chng_est, typ='h', lend=1, lwd=15, col='white', xaxt='n', xlim=c(1,13), ylim=c(-rng_flx, rng_flx), xlab="Month", ylab=expression(paste("Median Flux Change, kg",sep='')), las=1)
plotCI(seq(1:12), Flux_compare$chng_est, ui=Flux_compare$up_conf, li=Flux_compare$low_conf, pch=16, add=TRUE)
abline(h=0)
axis(side=1,at=seq(1,12,by=1), labels=format(c(seq(as.Date("2000-10-01"), as.Date("2000-12-01"), by="month"), seq(as.Date("2000-01-01"), as.Date("2000-09-01"), by="month")),'%b'), las=2)
legend('topright', c("Median difference", "90% Confidence Interval for the Median"), pch=c(16,NA), lwd=c(NA,1), pt.cex=c(1,NA), pt.bg=c('black',NA), bty='n', bg='white')
dev.off()


# End of non-standard EGRET plot section requested by Michael
# --------------------------------------------------------------------------------------------------------
#################  Using the plotConcQSmooth function
###########
#First do flow duration analysis
flowDuration(eList, centerDate = "06-01", qUnit = 2, span = 30)
date1 <- "1972-06-01"
date2 <- "1981-06-01"
date3 <- "1991-06-01"
date4 <- "2001-06-01"
date5 <- "2019-06-01"

qLow= baseQ
qHigh=highQ7

tiff("Vernalis_Date1_Discharge_TP_conc_no_log.tif",height = 700, width = 1000, res=120)
plotConcQSmooth(eList,date1, date2, date3,qLow, qHigh, logScale=FALSE,printLegend =TRUE,legendLeft=0,legendTop=0,printTitle=TRUE)
dev.off()

tiff("Vernalis_Date2_Discharge_TP_conc_no_log.tif",height = 700, width = 1000, res=120)
plotConcQSmooth(eList,date3, date4, date5,qLow, qHigh, logScale=FALSE,printLegend =TRUE,legendLeft=0,legendTop=0,printTitle=TRUE)
dev.off()


# ---------------------------
# Now run the EGRETci package for TP
# ---------------------------
setwd("C:/Users/dsaleh/Documents/GitHub/PES_Project/Vernalis_EGRET/TP/")

# Change working directory
#setwd("/Users/joed/PES_Project/Vernalis_EGRET/TP/")
subDir <- 'EGRETci_plots'
if (file.exists(subDir)){
  setwd(file.path(getwd(),subDir))
} else {
  dir.create(file.path(getwd(),subDir), recursive=TRUE)
  setwd(file.path(getwd(),subDir))
}

#Interactive function to set up trend analysis:
caseSetUp <- trendSetUp(eList, 
                        year1=1972, 
                        year2=2018, 
                        nBoot = 200, 
                        bootBreak = 100, 
                        blockLength = 200)
eBoot <- wBT(eList, caseSetUp, fileName ="outputText_TP.txt")


# 
saveEGRETci(eList, eBoot, caseSetUp, fileName = "EGRETci_output_TP")

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

tiff("histo_TP_UpTruck_Trend_conc_flux.tif", height = 700, width = 1200, res=120)
par(mfrow=c(1,2))
plotHistogramTrend2(eBoot, caseSetUp, eList, flux=FALSE, xSeq = seq(-8000,8000,5),las=1,xlim=c(-100,100))
abline(h=0)

plotHistogramTrend2(eBoot, caseSetUp, eList, flux=TRUE, xSeq = seq(-50000,50000,5),las=1)
abline(h=0)
dev.off()

nBoot <- 200
blockLength <- 200
coreOut <- 5 #Number of cores to leave out of processing tasks

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

tiff("Ann_Avg_Conc_&_Ann_Flow_Normalized_Conc_Boot_SanJVernalis_TP.tif", height = 500, width = 600, res=110)
plotConcHistBoot(eList, CIAnnualResults, plotFlowNorm=TRUE, showYLabels=TRUE, showYAxis=TRUE,col=4)
polygon(x=conc.poly.x, y=conc.poly.y, col=rgb(24,116,205,40,max=255),border=NA)
dev.off()

tiff("Ann_Flux_&_Ann_Flow_Normalized_Flux_Boot_SanJVernalis_TP.tif", height = 500, width = 600, res=110)
plotFluxHistBoot(eList, fluxUnit=13, CIAnnualResults, showYLabels=TRUE, showYAxis=TRUE, col=4)
polygon(x=flux.poly.x, y=flux.poly.y, col=rgb(24,116,205,40,max=255),border=NA)
dev.off()

setSweave("_TP_Conc_EGRETCI",7,7)
plotConcHistBoot(eList, CIAnnualResults, plotFlowNorm=TRUE, showYLabels=TRUE, showYAxis=TRUE,col=4)
polygon(x=conc.poly.x, y=conc.poly.y, col=rgb(24,116,205,40,max=255),border=NA)
graphics.off()

setSweave("SanJVernalis_TP_fluxEGRETCI",7,7)
plotFluxHistBoot(eList, fluxUnit=13, CIAnnualResults, showYLabels=TRUE, showYAxis=TRUE, col=4)
polygon(x=flux.poly.x, y=flux.poly.y, col=rgb(24,116,205,40,max=255),border=NA)
graphics.off()

saveEGRETci(eList, eBoot, fileName="TP_Boot_.RData")
save(repAnnual,file="RepAnnual")

# load(file="N_Boot.RData")
# load(file="RepAnnual")


#############################################################
# Working on SSC
#############################################################
startDate <- "1985-10-01"
endDate <- "2019-06-01"
siteNumber <- "11303500"
QParameterCd <- "00060"
parameterCd <- "80154"  # "SSC"

filePath <- "C:/Users/dsaleh/Documents/GitHub/PES_Project/Vernalis_EGRET/"
setwd("C:/Users/dsaleh/Documents/GitHub/PES_Project/Vernalis_EGRET")
Daily <- readNWISDaily(siteNumber, QParameterCd, startDate, endDate)


##the missing time using Charlie Kratzer's data

##Set fileName to combined Kratzer and NWIS data set
fileName <- "SSC_Vern_edited2.csv"
Sample <- readUserSample(filePath, fileName)
Sample <- removeDuplicates(Sample)
write.csv(Sample,'Sample_SSC.csv')
INFO <- readNWISInfo(siteNumber = siteNumber, parameterCd = parameterCd, interactive=FALSE)
INFO$staAbbrev <- paste(strsplit(INFO$station_nm," ")[[1]][1],strsplit(INFO$station_nm," ")[[1]][2])

# Have a look at the available range of TP data
range(Sample$Date)
eList <- mergeReport(INFO, Daily, Sample)


# Change the working directory; redirect plot output to SSC folder
#setwd("/Users/joed/PES_Project/Vernalis_EGRET/")
setwd("C:/Users/dsaleh/Documents/GitHub/PES_Project/Vernalis_EGRET")
subDir <- 'SSC/EGRET_plots'
if (file.exists(subDir)){
  setwd(file.path(getwd(),subDir))
} else {
  dir.create(file.path(getwd(),subDir), recursive=TRUE)
  setwd(file.path(getwd(),subDir))
}
plotConcTime(eList)
# Plot water quality data
tiff("Conc_vs_Time_SanJVernalisVern_SSC.tif", height = 600, width = 800, res=120)
plotConcTime(eList)
dev.off()

# Now, a classic Q-C plot
tiff("Conc-Q_SanJVernalisVern_SSC.tif", height = 600, width = 800, res=120)
plotConcQ(eList, logScale=TRUE)
dev.off()

# The data set as flux values rather than as concentrations
tiff("Flux-Q_SanJVernalisVern_SSC.tif", height = 600, width = 800, res=120)
plotFluxQ(eList, fluxUnit=4)
dev.off()

# Monthly boxplots
tiff("Monthly-Conc_BoxPlots_SanJVernalisVern_SSC.tif", height = 600, width = 800, res=120)
boxConcMonth(eList, logScale=TRUE)
dev.off()

# Flow on days sampled vs. all other days
tiff("Flow_on_days_sampled_vs_all_other_days_SanJVernalisVern_SSC.tif", height = 600, width = 800, res=120)
boxQTwice(eList, qUnit=1)
dev.off()

#########################################
# Now start the Flow-Normalized Analysis
#########################################

# Build the regression model
eList <- modelEstimation(eList, windowY = 7, windowQ = 2, windowS = 0.5, minNumObs = 100, minNumUncen =50)

MonthlyResults <- calculateMonthlyResults(eList)

# Dump SSC-related flow-normalized data to text file for bringing together with other monitoring sites
paLong <- 12
paStart <- 10
localDaily <- getDaily(eList)
localAnnualResults <- setupYears(paStart = paStart, paLong = paLong, localDaily = localDaily)
write.table(localAnnualResults, file = 'VernalisVern_SSC_RawVals.txt', quote=FALSE, row.names=FALSE)

write.csv(Daily,'localDaily_VernVern_SSC.csv')

# Plot the annual average concentration and annual flow-normalized concentration
tiff("Ann_Avg_Conc_&_Ann_Flow_Normalized_Conc_SanJVernalisVern_SSC.tif", height = 600, width = 800, res=120)
plotConcHist(eList, plotFlowNorm=TRUE)
dev.off()

# Plot the annual flux and annual flow-normalized flux
tiff("Ann_Flux_&_Ann_Flow_Normalized_Flux_SanJVernalisVern_SSC.tif", height = 600, width = 800, res=120)
plotFluxHist(eList, plotFlowNorm = TRUE) # fluxMax) # fluxMax
dev.off()

# Look for a trend change:
tableChange(eList, fluxUnit=6, yearPoints=c(1986,1996,2006,2016))


#Generate out-of-the-box diagnostic plots
tiff("fluxBiasMulti_SanJVernalisVern_SSC.tif", height = 1200, width = 1000, res=120)
fluxBiasMulti(eList, moreTitle = "WRTDS")
dev.off()

tiff("Modeled_Daily_Conc_wObservations_SanJVernalisVern_SSC.tif", height = 800, width = 1000, res=120)
plotConcTimeDaily(eList)
dev.off()

# Exploring model behavior and adjusting model parameters
tiff("Contours_SanJVernalisVern_SSC.tif", height = 700, width = 1000, res=120)
plotContours(eList, qBottom=5,qTop=1000,yearStart=1986,yearEnd=2019, contourLevels=seq(0,50,by=1), color.palette = colorRampPalette(c("violet", "purple", "blue", "cyan", "green", "yellow", "orange", "red"))) 
dev.off()

tiff("Log_Contours_SanJVernalisVern_SSC.tif", height = 700, width = 1000, res=120)
plotContours(eList, qBottom=5, qTop=1000, yearStart=1986, yearEnd=2019, contourLevels=seq(-2.4,4,by=0.1), color.palette = colorRampPalette(c("violet", "purple", "blue", "cyan", "green", "yellow", "orange", "red")), whatSurface=1) 
dev.off()

tiff("StdErr_of_Log_Contours_SanJVernalisVern_SSC.tif", height = 700, width = 1000, res=120)
plotContours(eList, qBottom=5, qTop=1000, yearStart=1986, yearEnd=2019, contourLevels=seq(0.38,0.98,by=0.01), color.palette = colorRampPalette(c("violet", "purple", "blue", "cyan", "green", "yellow", "orange", "red")), whatSurface=2) 
dev.off()

tiff("Contours_Difference_Vern_SSC.tif", height = 700, width = 1000, res=120)
plotDiffContours(eList, 1986,2019,5,1000,maxDiff=50)
dev.off()

tiff("Contours_PercentDifference_Vern_SSC.tif", height = 700, width = 1000, res=120)
plotDiffContours(eList, 1986,2019,5,1000, maxDiff=100, plotPercent=TRUE)
dev.off()

tiff("Contours_PercentDifference2Vern_SSC_SanJVernalis.tif", height = 700, width = 1000, res=120)
plotDiffContours2(eList, 1986,2019,5,1000, maxDiff=c(-100,100), plotPercent=TRUE, lwd=3, color.palette=colorRampPalette(c("blue","lightblue","white","yellow", "orange", "red")),tick.lwd = 1)
dev.off()

Sample$WY <- trunc(Sample$DecYear+0.25) 
tiff("Monthly_Boxplot_InorgVern_SSC_SanJVernalis.tif", height = 700, width = 1000, res=120)
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

tiff("Discharge_specific_trendsVern_SSC_centered_on_06-01.tif", height = 600, width = 1200, res=120)
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
tiff("JulianDay_of_MaxVern_SSC_Conc.tif", height = 600, width = 800, res=120)
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

tiff("JulianDay_of_MaxVern_SSC_Conc_Using_30_rollingAvg.tif", height = 600, width = 800, res=120)
plot(out_m2$wyr, out_m2$JulianWYR, pch=16, xlab='Water Year', ylab='Julian Day', yaxs='i', ylim=c(0,370), las=1)
dev.off()


# Next, I'll try using the flow-normalized concentration (same general code flow as above)
# First, try plotting flow-normalized concentration:
# Plot it
tiff("Flow_Normalized_Conc_TC1Vern_SSC.tif", height = 600, width = 800, res=120)
plot(as.Date(localDaily$Date), localDaily$FNConc, typ='l', las=1, xlab='Time', ylab='Flow-normalized Concentration')
dev.off()

out_FN <- aggregate(FNConc ~ waterYear, data = localDaily, which.max)
out_FN <- out_FN[ order(out_FN$waterYear), ]
out_FN$AbsConcDay <- out_FN$FNConc + cumsum(c(0,tbl[-length(tbl)]))
out2_FN <- localDaily[out_FN$AbsConcDay,]
out2_FN <- data.frame(Date=out2_FN$Date, Q=out2_FN$Q, Conc=out2_FN$FNConc, wyr=out2_FN$waterYear, Julian=yday(as.Date(out2_FN$Date)))
out2_FN$JulianWYR <- ifelse(out2_FN$Julian > 273, out2_FN$Julian - 273, 92 + out2_FN$Julian)

# Plot it
tiff("JulianDay_of_MaxVern_SSC_Flow_Normalized_Conc.tif", height = 600, width = 800, res=120)
plot(out2_FN$wyr, out2_FN$JulianWYR, pch=16, xlab='Water Year', ylab='Julian Day', yaxs='i', ylim=c(0,370), las=1)
dev.off()


# --------------------------------------------------------------------------------------------------------
# The following script is for a non-standard EGRET plot and instead help generate a plot Michael requested

localDaily <- getDaily(eList)

# Will need to adjust the date range below based on each gages unique start/stop dates
early_decade <- subset(localDaily, localDaily$Date > as.Date('1985-10-01') & localDaily$Date < as.Date('1995-10-01'))
recent_decade <- subset(localDaily, localDaily$Date > as.Date('2009-07-01'))


early_decade_monthly_mn <- aggregate(ConcDay ~ MonthSeq, data = early_decade, 'mean')
recent_decade_monthly_mn <- aggregate(ConcDay ~ MonthSeq, data = recent_decade, 'mean')

#early_decade_monthly_mn$month <- format(seq(as.Date('1972-10-01'), as.Date('1982-09-30'), by='month'), '%b')
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
                dimnames = list(c("1985-1995", "2009-2019"),
                                c(format(seq(as.Date('1985-10-01'), as.Date('1986-09-30'), by='month'), '%b'))))

# Be sure to adjust the legend's first decade start and stop year correctly
mx <- max(c((early_decade_mon_mn$ConcDay + early_decade_mon_sd$ConcDay), (recent_decade_mon_mn$ConcDay + recent_decade_mon_sd$ConcDay)))

tiff("timing_shift_inVern_SSC_conc_monthly_means.tif", height=800, width=900, res=130)
par(mar=c(3,5,2,1))
x <- barplot(mdat2, beside=TRUE, las=1, ylim=c(0,mx), col = c("lightblue", "mistyrose"))
abline(h=0)
arrows(x0=x[1,], y0=early_decade_mon_mn$ConcDay - early_decade_mon_sd$ConcDay, x1=x[1,], y1=early_decade_mon_mn$ConcDay + early_decade_mon_sd$ConcDay, angle=90, length=0.04, code=3)
arrows(x0=x[2,], y0=recent_decade_mon_mn$ConcDay - recent_decade_mon_sd$ConcDay, x1=x[2,], y1=recent_decade_mon_mn$ConcDay + recent_decade_mon_sd$ConcDay, angle=90, length=0.04, code=3)
mtext(side=2, expression(paste(SSC,', mg ',L^-1,sep='')), line=3)
legend(x=25, y=0.9 * mx, c("1985-1995", "2009-2019"), pch=c(22,22), pt.cex=2, pt.bg=c("lightblue", "mistyrose"), bty='n', xpd=TRUE)
dev.off()


# Now attempting a Wilcox Test (aka Mann-Whitney-Wilcoxon Rank Sum test)
# ----------------------------------------------------------------------
early_jan <- subset(early_decade_monthly_mn, month==1)
recent_jan <- subset(recent_decade_monthly_mn, month==1)
Vern_SSC_conc_jan_wilcox <- wilcox.test(recent_jan$ConcDay, early_jan$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_feb <- subset(early_decade_monthly_mn, month==2)
recent_feb <- subset(recent_decade_monthly_mn, month==2)
Vern_SSC_conc_feb_wilcox <- wilcox.test(recent_feb$ConcDay, early_feb$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_mar <- subset(early_decade_monthly_mn, month==3)
recent_mar <- subset(recent_decade_monthly_mn, month==3)
Vern_SSC_conc_mar_wilcox <- wilcox.test(recent_mar$ConcDay, early_mar$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_apr <- subset(early_decade_monthly_mn, month==4)
recent_apr <- subset(recent_decade_monthly_mn, month==4)
Vern_SSC_conc_apr_wilcox <- wilcox.test(recent_apr$ConcDay, early_apr$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_may <- subset(early_decade_monthly_mn, month==5)
recent_may <- subset(recent_decade_monthly_mn, month==5)
Vern_SSC_conc_may_wilcox <- wilcox.test(recent_may$ConcDay, early_may$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_jun <- subset(early_decade_monthly_mn, month==6)
recent_jun <- subset(recent_decade_monthly_mn, month==6)
Vern_SSC_conc_jun_wilcox <- wilcox.test(recent_jun$ConcDay, early_jun$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_jul <- subset(early_decade_monthly_mn, month==7)
recent_jul <- subset(recent_decade_monthly_mn, month==7)
Vern_SSC_conc_jul_wilcox <- wilcox.test(recent_jul$ConcDay, early_jul$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_aug <- subset(early_decade_monthly_mn, month==8)
recent_aug <- subset(recent_decade_monthly_mn, month==8)
Vern_SSC_conc_aug_wilcox <- wilcox.test(recent_aug$ConcDay, early_aug$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_sep <- subset(early_decade_monthly_mn, month==9)
recent_sep <- subset(recent_decade_monthly_mn, month==9)
Vern_SSC_conc_sep_wilcox <- wilcox.test(recent_sep$ConcDay, early_sep$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_oct <- subset(early_decade_monthly_mn, month==10)
recent_oct <- subset(recent_decade_monthly_mn, month==10)
Vern_SSC_conc_oct_wilcox <- wilcox.test(recent_oct$ConcDay, early_oct$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_nov <- subset(early_decade_monthly_mn, month==11)
recent_nov <- subset(recent_decade_monthly_mn, month==11)
Vern_SSC_conc_nov_wilcox <- wilcox.test(recent_nov$ConcDay, early_nov$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_dec <- subset(early_decade_monthly_mn, month==12)
recent_dec <- subset(recent_decade_monthly_mn, month==12)
Vern_SSC_conc_dec_wilcox <- wilcox.test(recent_dec$ConcDay, early_dec$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

Conc_compare <- data.frame(chng_est=c(Vern_SSC_conc_oct_wilcox$est,
                                      Vern_SSC_conc_nov_wilcox$est,
                                      Vern_SSC_conc_dec_wilcox$est,
                                      Vern_SSC_conc_jan_wilcox$est,
                                      Vern_SSC_conc_feb_wilcox$est,
                                      Vern_SSC_conc_mar_wilcox$est,
                                      Vern_SSC_conc_apr_wilcox$est,
                                      Vern_SSC_conc_may_wilcox$est,
                                      Vern_SSC_conc_jun_wilcox$est,
                                      Vern_SSC_conc_jul_wilcox$est,
                                      Vern_SSC_conc_aug_wilcox$est,
                                      Vern_SSC_conc_sep_wilcox$est),
                           low_conf=c(Vern_SSC_conc_oct_wilcox$conf.int[1],
                                      Vern_SSC_conc_nov_wilcox$conf.int[1],
                                      Vern_SSC_conc_dec_wilcox$conf.int[1],
                                      Vern_SSC_conc_jan_wilcox$conf.int[1],
                                      Vern_SSC_conc_feb_wilcox$conf.int[1],
                                      Vern_SSC_conc_mar_wilcox$conf.int[1],
                                      Vern_SSC_conc_apr_wilcox$conf.int[1],
                                      Vern_SSC_conc_may_wilcox$conf.int[1],
                                      Vern_SSC_conc_jun_wilcox$conf.int[1],
                                      Vern_SSC_conc_jul_wilcox$conf.int[1],
                                      Vern_SSC_conc_aug_wilcox$conf.int[1],
                                      Vern_SSC_conc_sep_wilcox$conf.int[1]),
                           up_conf=c(Vern_SSC_conc_oct_wilcox$conf.int[2],
                                     Vern_SSC_conc_nov_wilcox$conf.int[2],
                                     Vern_SSC_conc_dec_wilcox$conf.int[2],
                                     Vern_SSC_conc_jan_wilcox$conf.int[2],
                                     Vern_SSC_conc_feb_wilcox$conf.int[2],
                                     Vern_SSC_conc_mar_wilcox$conf.int[2],
                                     Vern_SSC_conc_apr_wilcox$conf.int[2],
                                     Vern_SSC_conc_may_wilcox$conf.int[2],
                                     Vern_SSC_conc_jun_wilcox$conf.int[2],
                                     Vern_SSC_conc_jul_wilcox$conf.int[2],
                                     Vern_SSC_conc_aug_wilcox$conf.int[2],
                                     Vern_SSC_conc_sep_wilcox$conf.int[2]))

write.table(Conc_compare, "Vern_SSC_conc_wilcox.txt", quote=FALSE, row.names=FALSE)

rng <- max(abs(c(Conc_compare$up_conf, Conc_compare$low_conf)))
tiff("Vern_SSC_conc_shift_wilcox_Vert_Bars.tif", height=600, width=800, res=130)
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
                dimnames = list(c("1990-2000", "2001-2011"),
                                c(format(seq(as.Date('1973-10-01'), as.Date('1974-09-01'), by='month'), '%b'))))

mx <- max(c((early_decade_mon_mn_flx$FluxDay + early_decade_mon_sd_flx$FluxDay), (recent_decade_mon_mn_flx$FluxDay + recent_decade_mon_sd_flx$FluxDay)))
tiff("timing_shift_inVern_SSC_load_monthly_means.tif", height=800, width=900, res=130)
x <- barplot(mdat3, beside=TRUE, las=1, ylim=c(0,mx), col = c("lightblue", "mistyrose"))
abline(h=0)
arrows(x0=x[1,], y0=early_decade_mon_mn_flx$FluxDay - early_decade_mon_sd_flx$FluxDay, x1=x[1,], y1=early_decade_mon_mn_flx$FluxDay + early_decade_mon_sd_flx$FluxDay, angle=90, length=0.04, code=3)
arrows(x0=x[2,], y0=recent_decade_mon_mn_flx$FluxDay - recent_decade_mon_sd_flx$FluxDay, x1=x[2,], y1=recent_decade_mon_mn_flx$FluxDay + recent_decade_mon_sd_flx$FluxDay, angle=90, length=0.04, code=3)
mtext(side=2, expression(paste(SSC,', kg ',month^-1,sep='')), line=2.5)
legend(x=30, y=0.9 * mx, c("1990-2000", "2001-2011"), pch=c(22,22), pt.cex=2, pt.bg=c("lightblue", "mistyrose"), bty='n', xpd=TRUE)
dev.off()

# Apply Wilcox.text to the monthly loads here...
early_jan_flx <- subset(early_decade_monthly_flx, month==1)
recent_jan_flx <- subset(recent_decade_monthly_flx, month==1)
Vern_SSC_flux_jan_wilcox <- wilcox.test(recent_jan_flx$FluxDay, early_jan_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_feb_flx <- subset(early_decade_monthly_flx, month==2)
recent_feb_flx <- subset(recent_decade_monthly_flx, month==2)
Vern_SSC_flux_feb_wilcox <- wilcox.test(recent_feb_flx$FluxDay, early_feb_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_mar_flx <- subset(early_decade_monthly_flx, month==3)
recent_mar_flx <- subset(recent_decade_monthly_flx, month==3)
Vern_SSC_flux_mar_wilcox <- wilcox.test(recent_mar_flx$FluxDay, early_mar_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_apr_flx <- subset(early_decade_monthly_flx, month==4)
recent_apr_flx <- subset(recent_decade_monthly_flx, month==4)
Vern_SSC_flux_apr_wilcox <- wilcox.test(recent_apr_flx$FluxDay, early_apr_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_may_flx <- subset(early_decade_monthly_flx, month==5)
recent_may_flx <- subset(recent_decade_monthly_flx, month==5)
Vern_SSC_flux_may_wilcox <- wilcox.test(recent_may_flx$FluxDay, early_may_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_jun_flx <- subset(early_decade_monthly_flx, month==6)
recent_jun_flx <- subset(recent_decade_monthly_flx, month==6)
Vern_SSC_flux_jun_wilcox <- wilcox.test(recent_jun_flx$FluxDay, early_jun_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_jul_flx <- subset(early_decade_monthly_flx, month==7)
recent_jul_flx <- subset(recent_decade_monthly_flx, month==7)
Vern_SSC_flux_jul_wilcox <- wilcox.test(recent_jul_flx$FluxDay, early_jul_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_aug_flx <- subset(early_decade_monthly_flx, month==8)
recent_aug_flx <- subset(recent_decade_monthly_flx, month==8)
Vern_SSC_flux_aug_wilcox <- wilcox.test(recent_aug_flx$FluxDay, early_aug_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_sep_flx <- subset(early_decade_monthly_flx, month==9)
recent_sep_flx <- subset(recent_decade_monthly_flx, month==9)
Vern_SSC_flux_sep_wilcox <- wilcox.test(recent_sep_flx$FluxDay, early_sep_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_oct_flx <- subset(early_decade_monthly_flx, month==10)
recent_oct_flx <- subset(recent_decade_monthly_flx, month==10)
Vern_SSC_flux_oct_wilcox <- wilcox.test(recent_oct_flx$FluxDay, early_oct_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_nov_flx <- subset(early_decade_monthly_flx, month==11)
recent_nov_flx <- subset(recent_decade_monthly_flx, month==11)
Vern_SSC_flux_nov_wilcox <- wilcox.test(recent_nov_flx$FluxDay, early_nov_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_dec_flx <- subset(early_decade_monthly_flx, month==12)
recent_dec_flx <- subset(recent_decade_monthly_flx, month==12)
Vern_SSC_flux_dec_wilcox <- wilcox.test(recent_dec_flx$FluxDay, early_dec_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)


Flux_compare <- data.frame(chng_est=c(Vern_SSC_flux_oct_wilcox$est,
                                      Vern_SSC_flux_nov_wilcox$est,
                                      Vern_SSC_flux_dec_wilcox$est,
                                      Vern_SSC_flux_jan_wilcox$est,
                                      Vern_SSC_flux_feb_wilcox$est,
                                      Vern_SSC_flux_mar_wilcox$est,
                                      Vern_SSC_flux_apr_wilcox$est,
                                      Vern_SSC_flux_may_wilcox$est,
                                      Vern_SSC_flux_jun_wilcox$est,
                                      Vern_SSC_flux_jul_wilcox$est,
                                      Vern_SSC_flux_aug_wilcox$est,
                                      Vern_SSC_flux_sep_wilcox$est),
                           low_conf=c(Vern_SSC_flux_oct_wilcox$conf.int[1],
                                      Vern_SSC_flux_nov_wilcox$conf.int[1],
                                      Vern_SSC_flux_dec_wilcox$conf.int[1],
                                      Vern_SSC_flux_jan_wilcox$conf.int[1],
                                      Vern_SSC_flux_feb_wilcox$conf.int[1],
                                      Vern_SSC_flux_mar_wilcox$conf.int[1],
                                      Vern_SSC_flux_apr_wilcox$conf.int[1],
                                      Vern_SSC_flux_may_wilcox$conf.int[1],
                                      Vern_SSC_flux_jun_wilcox$conf.int[1],
                                      Vern_SSC_flux_jul_wilcox$conf.int[1],
                                      Vern_SSC_flux_aug_wilcox$conf.int[1],
                                      Vern_SSC_flux_sep_wilcox$conf.int[1]),
                           up_conf=c(Vern_SSC_flux_oct_wilcox$conf.int[2],
                                     Vern_SSC_flux_nov_wilcox$conf.int[2],
                                     Vern_SSC_flux_dec_wilcox$conf.int[2],
                                     Vern_SSC_flux_jan_wilcox$conf.int[2],
                                     Vern_SSC_flux_feb_wilcox$conf.int[2],
                                     Vern_SSC_flux_mar_wilcox$conf.int[2],
                                     Vern_SSC_flux_apr_wilcox$conf.int[2],
                                     Vern_SSC_flux_may_wilcox$conf.int[2],
                                     Vern_SSC_flux_jun_wilcox$conf.int[2],
                                     Vern_SSC_flux_jul_wilcox$conf.int[2],
                                     Vern_SSC_flux_aug_wilcox$conf.int[2],
                                     Vern_SSC_flux_sep_wilcox$conf.int[2]))

write.table(Flux_compare, "Vern_SSC_flux_wilcox.txt", quote=FALSE, row.names=FALSE)

rng_flx <- max(abs(c(Flux_compare$up_conf, Flux_compare$low_conf)))
tiff("Vern_SSC_flux_shift_wilcox_Vert_Bars.tif", height=600, width=800, res=130)
par(mar=c(4,5,0.5,0.5))
plot(seq(1:12), Flux_compare$chng_est, typ='h', lend=1, lwd=15, col='white', xaxt='n', xlim=c(1,13), ylim=c(-rng_flx, rng_flx), xlab="Month", ylab=expression(paste("Median Flux Change, kg",sep='')), las=1)
plotCI(seq(1:12), Flux_compare$chng_est, ui=Flux_compare$up_conf, li=Flux_compare$low_conf, pch=16, add=TRUE)
abline(h=0)
axis(side=1,at=seq(1,12,by=1), labels=format(c(seq(as.Date("2000-10-01"), as.Date("2000-12-01"), by="month"), seq(as.Date("2000-01-01"), as.Date("2000-09-01"), by="month")),'%b'), las=2)
legend('topright', c("Median difference", "90% Confidence Interval for the Median"), pch=c(16,NA), lwd=c(NA,1), pt.cex=c(1,NA), pt.bg=c('black',NA), bty='n', bg='white')
dev.off()


# End of non-standard EGRET plot section requested by Michael
# --------------------------------------------------------------------------------------------------------
#################  Using the plotConcQSmooth function
###########
#First do flow duration analysis
flowDuration(eList, centerDate = "06-01", qUnit = 2, span = 30)

date1 <- "1985-06-01"
date2 <- "1995-06-01"
date3 <- "2005-06-01"
date4 <- "2015-06-01"

qLow= baseQ
qHigh=highQ7

tiff("Vernalis_Date_DischargeVern_SSC_conc_no_log.tif",height = 700, width = 1000, res=120)
plotConcQSmooth(eList,date1, date2, date3, qLow, qHigh, logScale=FALSE,printLegend =TRUE,legendLeft=0,legendTop=0,printTitle=TRUE)
dev.off()






# ---------------------------
# Now run the EGRETci package this was ran  on 8/19/2019
# ---------------------------
setwd("/Users/dsaleh/Documents/GitHub/PES_Project/Vernalis_EGRET/SSC/")
#setwd("/Users/joed/PES_Project/Vernalis_EGRET/")
# Change working directory
subDir <- 'EGRETci_plots'
if (file.exists(subDir)){
  setwd(file.path(getwd(),subDir))
} else {
  dir.create(file.path(getwd(),subDir), recursive=TRUE)
  setwd(file.path(getwd(),subDir))
}

#Interactive function to set up trend analysis:
caseSetUp <- trendSetUp(eList, 
                        year1=1985, 
                        year2=2018, 
                        nBoot = 200, 
                        bootBreak = 100, 
                        blockLength = 200)
eBoot <- wBT(eList, caseSetUp, fileName ="outputText.txt")
#
                
#

saveEGRETci(eList, eBoot, caseSetUp, fileName = "EGRETci_outputVern_SSC_.RData")

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

tiff("histoVern_SSC__Trend_conc_flux.tif", height = 700, width = 1200, res=120)
par(mfrow=c(1,2))
plotHistogramTrend2(eBoot, caseSetUp, eList, flux=FALSE, xSeq = seq(-800000,8000000,5),las=1,xlim=c(-150,50))
abline(h=0)

plotHistogramTrend2(eBoot, caseSetUp, eList, flux=TRUE, xSeq = seq(-5000000,50000000,5),las=1,xlim=c(-200,200))
abline(h=0)
dev.off()

nBoot <- 200
blockLength <- 200
coreOut <- 5 #Number of cores to leave out of processing tasks

widthCI <- 95
ciLower <- (50-(widthCI/2))/100
ciUpper <- (50+(widthCI/2))/100
probs <- c(ciLower,ciUpper)

nCores <- detectCores() - coreOut
cl <- makeCluster(nCores)
registerDoParallel(cl)
repAnnual <- foreach(n = 1:nBoot,.packages=c('EGRETci')) %dopar% {
  annualResults <- bootAnnual(eList, blockLength, startSeed = n)  
}
stopCluster(cl)

CIAnnualResults <- ciBands(eList, repAnnual, probs)
conc.poly.x <- c(CIAnnualResults$Year,rev(CIAnnualResults$Year))
conc.poly.y <- c(CIAnnualResults$FNConcLow,rev(CIAnnualResults$FNConcHigh))
flux.poly.x <- c(CIAnnualResults$Year,rev(CIAnnualResults$Year))
flux.poly.y <- c(CIAnnualResults$FNFluxLow*365,rev(CIAnnualResults$FNFluxHigh*365))

tiff("Ann_Avg_Conc_&_Ann_Flow_Normalized_Conc_Boot_SanJVernalisVern_SSC.tif", height = 500, width = 600, res=110)
plotConcHistBoot(eList, CIAnnualResults, plotFlowNorm=TRUE, showYLabels=TRUE, showYAxis=TRUE,col=4)
polygon(x=conc.poly.x, y=conc.poly.y, col=rgb(24,116,205,40,max=255),border=NA)
dev.off()

tiff("Ann_Flux_&_Ann_Flow_Normalized_Flux_Boot_SanJVernalisVern_SSC.tif", height = 500, width = 600, res=110)
plotFluxHistBoot(eList, fluxUnit=13, CIAnnualResults, showYLabels=TRUE, showYAxis=TRUE, col=4)
polygon(x=flux.poly.x, y=flux.poly.y, col=rgb(24,116,205,40,max=255),border=NA)
dev.off()

setSweave("Vernalis_ConcVern_SSC_EGRETCI",7,7)
plotConcHistBoot(eList, CIAnnualResults, plotFlowNorm=TRUE, showYLabels=TRUE, showYAxis=TRUE,col=4)
polygon(x=conc.poly.x, y=conc.poly.y, col=rgb(24,116,205,40,max=255),border=NA)
graphics.off()

setSweave("Vernalis_FluxVern_SSC_EGRETCI",7,7)
plotFluxHistBoot(eList, fluxUnit=13, CIAnnualResults, showYLabels=TRUE, showYAxis=TRUE, col=4)
polygon(x=flux.poly.x, y=flux.poly.y, col=rgb(24,116,205,40,max=255),border=NA)
graphics.off()


saveEGRETci(eList, eBoot, fileName="SSC_Boot_.RData")
save(repAnnual,file="RepAnnual")

# load(file="N_Boot.RData")
# load(file="RepAnnual")


#############################################################
# Working on NH4
#############################################################

startDate <- "1974-10-01"
endDate <- "2019-05-30"
siteNumber <- "11303500"
QParameterCd <- "00060"
parameterCd <- "00608"  # "NH4"

filePath <- "C:/Users/dsaleh/Documents/GitHub/PES_Project/Vernalis_EGRET/"
#filePath <- "/Users/joed/PES_Project/Vernalis_EGRET/"
#setwd("/Users/joed/PES_Project/Vernalis_EGRET/")
setwd("C:/Users/dsaleh/Documents/GitHub/PES_Project/Vernalis_EGRET/")

Daily <- readNWISDaily(siteNumber, QParameterCd, startDate, endDate)
##The file below contains the combined NWIS and Kratzer data sets
fileName <- "NWIS_and_Kratzer_NH3.csv"
Sample <- readUserSample(filePath, fileName)
#Sample <- readNWISSample(siteNumber, parameterCd, startDate, endDate)
Sample <- removeDuplicates(Sample)
INFO <- readNWISInfo(siteNumber = siteNumber, parameterCd = parameterCd, interactive=FALSE)
INFO$staAbbrev <- paste(strsplit(INFO$station_nm," ")[[1]][1],strsplit(INFO$station_nm," ")[[1]][2])

# Have a look at the available range of NH3 data
range(Sample$Date)
#"1974-10-02" "2019-05-07"
eList <- mergeReport(INFO, Daily, Sample)

# Change the working directory; redirect plot output to NH4 folder
#setwd("/Users/joed/PES_Project/Vernalis_EGRET/")
setwd("C:/Users/dsaleh/Documents/GitHub/PES_Project/Vernalis_EGRET/")
subDir <- 'NH4/EGRET_plots'
if (file.exists(subDir)){
  setwd(file.path(getwd(),subDir))
} else {
  dir.create(file.path(getwd(),subDir), recursive=TRUE)
  setwd(file.path(getwd(),subDir))
}
plotConcTime(eList)
# Plot water quality data
tiff("Conc_vs_Time_SanJVernalis_NH4.tif", height = 600, width = 800, res=120)
plotConcTime(eList)
dev.off()

# Now, a classic Q-C plot
tiff("Conc-Q_SanJVernalis_NH4.tif", height = 600, width = 800, res=120)
plotConcQ(eList, logScale=TRUE)
dev.off()

# The data set as flux values rather than as concentrations
tiff("FluxQ_SanJVernalis_NH4.tif", height = 600, width = 800, res=120)
plotFluxQ(eList, fluxUnit=4)
dev.off()

# Monthly boxplots
tiff("Monthly-Conc_BoxPlots_SanJVernalis_NH4.tif", height = 600, width = 800, res=120)
boxConcMonth(eList, logScale=TRUE)
dev.off()

# Flow on days sampled vs. all other days
tiff("Flow_on_days_sampled_vs_all_other_days_SanJVernalis_NH4.tif", height = 600, width = 800, res=120)
boxQTwice(eList, qUnit=1)
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
write.table(localAnnualResults, file = 'Vernalis_NH3_RawVals.txt', quote=FALSE, row.names=FALSE)

write.csv(Daily,'localDailyNH3.csv')

# Plot the annual average concentration and annual flow-normalized concentration
tiff("Ann_Avg_Conc_&_Ann_Flow_Normalized_Conc_SanJVernalis_NH4.tif", height = 600, width = 800, res=120)
plotConcHist(eList, plotFlowNorm=TRUE)
dev.off()

# Plot the annual flux and annual flow-normalized flux
tiff("Ann_Flux_&_Ann_Flow_Normalized_Flux_SanJVernalis_NH4.tif", height = 600, width = 800, res=120)
plotFluxHist(eList, plotFlowNorm = TRUE) # fluxMax) # fluxMax
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

tiff("Discharge_specific_trends_NH3_centered_on_06-01.tif", height = 600, width = 1200, res=120)
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
tiff("JulianDay_of_Max_NH3_Conc.tif", height = 600, width = 800, res=120)
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

tiff("JulianDay_of_Max_NH3_Conc_Using_30_rollingAvg.tif", height = 600, width = 800, res=120)
plot(out_m2$wyr, out_m2$JulianWYR, pch=16, xlab='Water Year', ylab='Julian Day', yaxs='i', ylim=c(0,370), las=1)
dev.off()


# Next, I'll try using the flow-normalized concentration (same general code flow as above)
# First, try plotting flow-normalized concentration:
# Plot it
tiff("Flow_Normalized_Conc_TC1_NH3.tif", height = 600, width = 800, res=120)
plot(as.Date(localDaily$Date), localDaily$FNConc, typ='l', las=1, xlab='Time', ylab='Flow-normalized Concentration')
dev.off()

out_FN <- aggregate(FNConc ~ waterYear, data = localDaily, which.max)
out_FN <- out_FN[ order(out_FN$waterYear), ]
out_FN$AbsConcDay <- out_FN$FNConc + cumsum(c(0,tbl[-length(tbl)]))
out2_FN <- localDaily[out_FN$AbsConcDay,]
out2_FN <- data.frame(Date=out2_FN$Date, Q=out2_FN$Q, Conc=out2_FN$FNConc, wyr=out2_FN$waterYear, Julian=yday(as.Date(out2_FN$Date)))
out2_FN$JulianWYR <- ifelse(out2_FN$Julian > 273, out2_FN$Julian - 273, 92 + out2_FN$Julian)

# Plot it
tiff("JulianDay_of_Max_NH3_Flow_Normalized_Conc.tif", height = 600, width = 800, res=120)
plot(out2_FN$wyr, out2_FN$JulianWYR, pch=16, xlab='Water Year', ylab='Julian Day', yaxs='i', ylim=c(0,370), las=1)
dev.off()


# --------------------------------------------------------------------------------------------------------
# The following script is for a non-standard EGRET plot and instead help generate a plot Michael requested

localDaily <- getDaily(eList)

# Will need to adjust the date range below based on each gages unique start/stop dates
early_decade <- subset(localDaily, localDaily$Date > as.Date('1974-09-30') & localDaily$Date < as.Date('1984-10-01'))
recent_decade <- subset(localDaily, localDaily$Date > as.Date('2009-06-01'))


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

tiff("timing_shift_in_NH3_conc_monthly_means.tif", height=800, width=900, res=130)
par(mar=c(3,5,2,1))
x <- barplot(mdat2, beside=TRUE, las=1, ylim=c(0,mx), col = c("lightblue", "mistyrose"))
abline(h=0)
arrows(x0=x[1,], y0=early_decade_mon_mn$ConcDay - early_decade_mon_sd$ConcDay, x1=x[1,], y1=early_decade_mon_mn$ConcDay + early_decade_mon_sd$ConcDay, angle=90, length=0.04, code=3)
arrows(x0=x[2,], y0=recent_decade_mon_mn$ConcDay - recent_decade_mon_sd$ConcDay, x1=x[2,], y1=recent_decade_mon_mn$ConcDay + recent_decade_mon_sd$ConcDay, angle=90, length=0.04, code=3)
mtext(side=2, expression(paste(NH[3],', mg ',L^-1,sep='')), line=3)
legend(x=25, y=0.9 * mx, c("1974-1984", "2009-2019"), pch=c(22,22), pt.cex=2, pt.bg=c("lightblue", "mistyrose"), bty='n', xpd=TRUE)
dev.off()


# Now attempting a Wilcox Test (aka Mann-Whitney-Wilcoxon Rank Sum test)
# ----------------------------------------------------------------------
early_jan <- subset(early_decade_monthly_mn, month==1)
recent_jan <- subset(recent_decade_monthly_mn, month==1)
Vern_NH3_conc_jan_wilcox <- wilcox.test(recent_jan$ConcDay, early_jan$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_feb <- subset(early_decade_monthly_mn, month==2)
recent_feb <- subset(recent_decade_monthly_mn, month==2)
Vern_NH3_conc_feb_wilcox <- wilcox.test(recent_feb$ConcDay, early_feb$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_mar <- subset(early_decade_monthly_mn, month==3)
recent_mar <- subset(recent_decade_monthly_mn, month==3)
Vern_NH3_conc_mar_wilcox <- wilcox.test(recent_mar$ConcDay, early_mar$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_apr <- subset(early_decade_monthly_mn, month==4)
recent_apr <- subset(recent_decade_monthly_mn, month==4)
Vern_NH3_conc_apr_wilcox <- wilcox.test(recent_apr$ConcDay, early_apr$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_may <- subset(early_decade_monthly_mn, month==5)
recent_may <- subset(recent_decade_monthly_mn, month==5)
Vern_NH3_conc_may_wilcox <- wilcox.test(recent_may$ConcDay, early_may$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_jun <- subset(early_decade_monthly_mn, month==6)
recent_jun <- subset(recent_decade_monthly_mn, month==6)
Vern_NH3_conc_jun_wilcox <- wilcox.test(recent_jun$ConcDay, early_jun$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_jul <- subset(early_decade_monthly_mn, month==7)
recent_jul <- subset(recent_decade_monthly_mn, month==7)
Vern_NH3_conc_jul_wilcox <- wilcox.test(recent_jul$ConcDay, early_jul$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_aug <- subset(early_decade_monthly_mn, month==8)
recent_aug <- subset(recent_decade_monthly_mn, month==8)
Vern_NH3_conc_aug_wilcox <- wilcox.test(recent_aug$ConcDay, early_aug$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_sep <- subset(early_decade_monthly_mn, month==9)
recent_sep <- subset(recent_decade_monthly_mn, month==9)
Vern_NH3_conc_sep_wilcox <- wilcox.test(recent_sep$ConcDay, early_sep$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_oct <- subset(early_decade_monthly_mn, month==10)
recent_oct <- subset(recent_decade_monthly_mn, month==10)
Vern_NH3_conc_oct_wilcox <- wilcox.test(recent_oct$ConcDay, early_oct$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_nov <- subset(early_decade_monthly_mn, month==11)
recent_nov <- subset(recent_decade_monthly_mn, month==11)
Vern_NH3_conc_nov_wilcox <- wilcox.test(recent_nov$ConcDay, early_nov$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_dec <- subset(early_decade_monthly_mn, month==12)
recent_dec <- subset(recent_decade_monthly_mn, month==12)
Vern_NH3_conc_dec_wilcox <- wilcox.test(recent_dec$ConcDay, early_dec$ConcDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

Conc_compare <- data.frame(chng_est=c(Vern_NH3_conc_oct_wilcox$est,
                                      Vern_NH3_conc_nov_wilcox$est,
                                      Vern_NH3_conc_dec_wilcox$est,
                                      Vern_NH3_conc_jan_wilcox$est,
                                      Vern_NH3_conc_feb_wilcox$est,
                                      Vern_NH3_conc_mar_wilcox$est,
                                      Vern_NH3_conc_apr_wilcox$est,
                                      Vern_NH3_conc_may_wilcox$est,
                                      Vern_NH3_conc_jun_wilcox$est,
                                      Vern_NH3_conc_jul_wilcox$est,
                                      Vern_NH3_conc_aug_wilcox$est,
                                      Vern_NH3_conc_sep_wilcox$est),
                           low_conf=c(Vern_NH3_conc_oct_wilcox$conf.int[1],
                                      Vern_NH3_conc_nov_wilcox$conf.int[1],
                                      Vern_NH3_conc_dec_wilcox$conf.int[1],
                                      Vern_NH3_conc_jan_wilcox$conf.int[1],
                                      Vern_NH3_conc_feb_wilcox$conf.int[1],
                                      Vern_NH3_conc_mar_wilcox$conf.int[1],
                                      Vern_NH3_conc_apr_wilcox$conf.int[1],
                                      Vern_NH3_conc_may_wilcox$conf.int[1],
                                      Vern_NH3_conc_jun_wilcox$conf.int[1],
                                      Vern_NH3_conc_jul_wilcox$conf.int[1],
                                      Vern_NH3_conc_aug_wilcox$conf.int[1],
                                      Vern_NH3_conc_sep_wilcox$conf.int[1]),
                           up_conf=c(Vern_NH3_conc_oct_wilcox$conf.int[2],
                                     Vern_NH3_conc_nov_wilcox$conf.int[2],
                                     Vern_NH3_conc_dec_wilcox$conf.int[2],
                                     Vern_NH3_conc_jan_wilcox$conf.int[2],
                                     Vern_NH3_conc_feb_wilcox$conf.int[2],
                                     Vern_NH3_conc_mar_wilcox$conf.int[2],
                                     Vern_NH3_conc_apr_wilcox$conf.int[2],
                                     Vern_NH3_conc_may_wilcox$conf.int[2],
                                     Vern_NH3_conc_jun_wilcox$conf.int[2],
                                     Vern_NH3_conc_jul_wilcox$conf.int[2],
                                     Vern_NH3_conc_aug_wilcox$conf.int[2],
                                     Vern_NH3_conc_sep_wilcox$conf.int[2]))

write.table(Conc_compare, "Vern_NH3_conc_wilcox.txt", quote=FALSE, row.names=FALSE)

rng <- max(abs(c(Conc_compare$up_conf, Conc_compare$low_conf)))
tiff("Vern_NH3_conc_shift_wilcox_Vert_Bars.tif", height=600, width=800, res=130)
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
tiff("timing_shift_inVern_NH3_load_monthly_means.tif", height=800, width=900, res=130)
x <- barplot(mdat3, beside=TRUE, las=1, ylim=c(0,mx), col = c("lightblue", "mistyrose"))
abline(h=0)
arrows(x0=x[1,], y0=early_decade_mon_mn_flx$FluxDay - early_decade_mon_sd_flx$FluxDay, x1=x[1,], y1=early_decade_mon_mn_flx$FluxDay + early_decade_mon_sd_flx$FluxDay, angle=90, length=0.04, code=3)
arrows(x0=x[2,], y0=recent_decade_mon_mn_flx$FluxDay - recent_decade_mon_sd_flx$FluxDay, x1=x[2,], y1=recent_decade_mon_mn_flx$FluxDay + recent_decade_mon_sd_flx$FluxDay, angle=90, length=0.04, code=3)
mtext(side=2, expression(paste(NH[3],', kg ',month^-1,sep='')), line=2.5)
legend(x=30, y=0.9 * mx, c("1974-1984", "2009-2019"), pch=c(22,22), pt.cex=2, pt.bg=c("lightblue", "mistyrose"), bty='n', xpd=TRUE)
dev.off()

# Apply Wilcox.text to the monthly loads here...
early_jan_flx <- subset(early_decade_monthly_flx, month==1)
recent_jan_flx <- subset(recent_decade_monthly_flx, month==1)
Vern_NH3_flux_jan_wilcox <- wilcox.test(recent_jan_flx$FluxDay, early_jan_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_feb_flx <- subset(early_decade_monthly_flx, month==2)
recent_feb_flx <- subset(recent_decade_monthly_flx, month==2)
Vern_NH3_flux_feb_wilcox <- wilcox.test(recent_feb_flx$FluxDay, early_feb_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_mar_flx <- subset(early_decade_monthly_flx, month==3)
recent_mar_flx <- subset(recent_decade_monthly_flx, month==3)
Vern_NH3_flux_mar_wilcox <- wilcox.test(recent_mar_flx$FluxDay, early_mar_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_apr_flx <- subset(early_decade_monthly_flx, month==4)
recent_apr_flx <- subset(recent_decade_monthly_flx, month==4)
Vern_NH3_flux_apr_wilcox <- wilcox.test(recent_apr_flx$FluxDay, early_apr_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_may_flx <- subset(early_decade_monthly_flx, month==5)
recent_may_flx <- subset(recent_decade_monthly_flx, month==5)
Vern_NH3_flux_may_wilcox <- wilcox.test(recent_may_flx$FluxDay, early_may_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_jun_flx <- subset(early_decade_monthly_flx, month==6)
recent_jun_flx <- subset(recent_decade_monthly_flx, month==6)
Vern_NH3_flux_jun_wilcox <- wilcox.test(recent_jun_flx$FluxDay, early_jun_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_jul_flx <- subset(early_decade_monthly_flx, month==7)
recent_jul_flx <- subset(recent_decade_monthly_flx, month==7)
Vern_NH3_flux_jul_wilcox <- wilcox.test(recent_jul_flx$FluxDay, early_jul_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_aug_flx <- subset(early_decade_monthly_flx, month==8)
recent_aug_flx <- subset(recent_decade_monthly_flx, month==8)
Vern_NH3_flux_aug_wilcox <- wilcox.test(recent_aug_flx$FluxDay, early_aug_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_sep_flx <- subset(early_decade_monthly_flx, month==9)
recent_sep_flx <- subset(recent_decade_monthly_flx, month==9)
Vern_NH3_flux_sep_wilcox <- wilcox.test(recent_sep_flx$FluxDay, early_sep_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_oct_flx <- subset(early_decade_monthly_flx, month==10)
recent_oct_flx <- subset(recent_decade_monthly_flx, month==10)
Vern_NH3_flux_oct_wilcox <- wilcox.test(recent_oct_flx$FluxDay, early_oct_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_nov_flx <- subset(early_decade_monthly_flx, month==11)
recent_nov_flx <- subset(recent_decade_monthly_flx, month==11)
Vern_NH3_flux_nov_wilcox <- wilcox.test(recent_nov_flx$FluxDay, early_nov_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)

early_dec_flx <- subset(early_decade_monthly_flx, month==12)
recent_dec_flx <- subset(recent_decade_monthly_flx, month==12)
Vern_NH3_flux_dec_wilcox <- wilcox.test(recent_dec_flx$FluxDay, early_dec_flx$FluxDay, exact=TRUE, conf.int = TRUE, conf.level = 0.9)


Flux_compare <- data.frame(chng_est=c(Vern_NH3_flux_oct_wilcox$est,
                                      Vern_NH3_flux_nov_wilcox$est,
                                      Vern_NH3_flux_dec_wilcox$est,
                                      Vern_NH3_flux_jan_wilcox$est,
                                      Vern_NH3_flux_feb_wilcox$est,
                                      Vern_NH3_flux_mar_wilcox$est,
                                      Vern_NH3_flux_apr_wilcox$est,
                                      Vern_NH3_flux_may_wilcox$est,
                                      Vern_NH3_flux_jun_wilcox$est,
                                      Vern_NH3_flux_jul_wilcox$est,
                                      Vern_NH3_flux_aug_wilcox$est,
                                      Vern_NH3_flux_sep_wilcox$est),
                           low_conf=c(Vern_NH3_flux_oct_wilcox$conf.int[1],
                                      Vern_NH3_flux_nov_wilcox$conf.int[1],
                                      Vern_NH3_flux_dec_wilcox$conf.int[1],
                                      Vern_NH3_flux_jan_wilcox$conf.int[1],
                                      Vern_NH3_flux_feb_wilcox$conf.int[1],
                                      Vern_NH3_flux_mar_wilcox$conf.int[1],
                                      Vern_NH3_flux_apr_wilcox$conf.int[1],
                                      Vern_NH3_flux_may_wilcox$conf.int[1],
                                      Vern_NH3_flux_jun_wilcox$conf.int[1],
                                      Vern_NH3_flux_jul_wilcox$conf.int[1],
                                      Vern_NH3_flux_aug_wilcox$conf.int[1],
                                      Vern_NH3_flux_sep_wilcox$conf.int[1]),
                           up_conf=c(Vern_NH3_flux_oct_wilcox$conf.int[2],
                                     Vern_NH3_flux_nov_wilcox$conf.int[2],
                                     Vern_NH3_flux_dec_wilcox$conf.int[2],
                                     Vern_NH3_flux_jan_wilcox$conf.int[2],
                                     Vern_NH3_flux_feb_wilcox$conf.int[2],
                                     Vern_NH3_flux_mar_wilcox$conf.int[2],
                                     Vern_NH3_flux_apr_wilcox$conf.int[2],
                                     Vern_NH3_flux_may_wilcox$conf.int[2],
                                     Vern_NH3_flux_jun_wilcox$conf.int[2],
                                     Vern_NH3_flux_jul_wilcox$conf.int[2],
                                     Vern_NH3_flux_aug_wilcox$conf.int[2],
                                     Vern_NH3_flux_sep_wilcox$conf.int[2]))

write.table(Flux_compare, "Vern_NH3_flux_wilcox.txt", quote=FALSE, row.names=FALSE)

rng_flx <- max(abs(c(Flux_compare$up_conf, Flux_compare$low_conf)))
tiff("Vern_NH3_flux_shift_wilcox_Vert_Bars.tif", height=600, width=800, res=130)
par(mar=c(4,5,0.5,0.5))
plot(seq(1:12), Flux_compare$chng_est, typ='h', lend=1, lwd=15, col='white', xaxt='n', xlim=c(1,13), ylim=c(-rng_flx, rng_flx), xlab="Month", ylab=expression(paste("Median Flux Change, kg",sep='')), las=1)
plotCI(seq(1:12), Flux_compare$chng_est, ui=Flux_compare$up_conf, li=Flux_compare$low_conf, pch=16, add=TRUE)
abline(h=0)
axis(side=1,at=seq(1,12,by=1), labels=format(c(seq(as.Date("2000-10-01"), as.Date("2000-12-01"), by="month"), seq(as.Date("2000-01-01"), as.Date("2000-09-01"), by="month")),'%b'), las=2)
legend('topright', c("Median difference", "90% Confidence Interval for the Median"), pch=c(16,NA), lwd=c(NA,1), pt.cex=c(1,NA), pt.bg=c('black',NA), bty='n', bg='white')
dev.off()


# End of non-standard EGRET plot section requested by Michael
# --------------------------------------------------------------------------------------------------------
#################  Using the plotConcQSmooth function
###########
#First do flow duration analysis
flowDuration(eList, centerDate = "06-01", qUnit = 2, span = 30)
date1 <- "1974-06-01"
date2 <- "2000-06-01"
date3 <- "2019-06-01"
qLow= baseQ
qHigh=highQ7

tiff("Vern_Date_Discharge_NH3_conc_no_log.tif",height = 700, width = 1000, res=120)
plotConcQSmooth(eList,date1, date2, date3,qLow, qHigh, logScale=FALSE,printLegend =TRUE,legendLeft=0,legendTop=0,printTitle=TRUE)
dev.off()




# Look for a trend change:
tableChange(eList, fluxUnit=6, yearPoints=c(1990,1998,2011))


#
#Generate out-of-the-box diagnostic plots
tiff("fluxBiasMulti_SanJVernalis_NH4.tif", height = 1200, width = 1000, res=120)
fluxBiasMulti(eList, moreTitle = "WRTDS")
dev.off()

tiff("Modeled_Daily_Conc_wObservations_SanJVernalis_NH4.tif", height = 800, width = 1000, res=120)
plotConcTimeDaily(eList)
dev.off()

# Exploring model behavior and adjusting model parameters
tiff("Contours_SanJVernalis_NH4.tif", height = 700, width = 1000, res=120)
plotContours(eList, qBottom=5,qTop=1000,yearStart=1974,yearEnd=2019, contourLevels=seq(0,50,by=1), color.palette = colorRampPalette(c("violet", "purple", "blue", "cyan", "green", "yellow", "orange", "red"))) 
dev.off()

tiff("Log_Contours_SanJVernalis_NH4.tif", height = 700, width = 1000, res=120)
plotContours(eList, qBottom=5, qTop=1000, yearStart=1974, yearEnd=2019, contourLevels=seq(-2.4,4,by=0.1), color.palette = colorRampPalette(c("violet", "purple", "blue", "cyan", "green", "yellow", "orange", "red")), whatSurface=1) 
dev.off()

tiff("StdErr_of_Log_Contours_SanJVernalis_NH4.tif", height = 700, width = 1000, res=120)
plotContours(eList, qBottom=5, qTop=1000, yearStart=1974, yearEnd=2019, contourLevels=seq(0.38,0.98,by=0.01), color.palette = colorRampPalette(c("violet", "purple", "blue", "cyan", "green", "yellow", "orange", "red")), whatSurface=2) 
dev.off()

tiff("Contours_Difference__NH4.tif", height = 700, width = 1000, res=120)
plotDiffContours(eList, 1974,2019,5,1000,maxDiff=0.3)
dev.off()

tiff("Contours_PercentDifference__NH4.tif", height = 700, width = 1000, res=120)
plotDiffContours(eList, 1974,2019,5,1000, maxDiff=100, plotPercent=TRUE)
dev.off()

tiff("Contours_PercentDifference2_NH4_SanJVernalis.tif", height = 700, width = 1000, res=120)
plotDiffContours2(eList, 1974,2019,5,1000, maxDiff=c(-100,100), plotPercent=TRUE, lwd=3, color.palette=colorRampPalette(c("blue","lightblue","white","yellow", "orange", "red")),tick.lwd = 1)
dev.off()

Sample$WY <- trunc(Sample$DecYear+0.25) 
tiff("Monthly_Boxplot_Inorg_NH4_SanJVernalis.tif", height = 700, width = 1000, res=120)
par(mar=c(4,6,0.5,0.5))
boxplot(Sample$ConcAve~Sample$WY,log="y",varwidth=TRUE,ylim=c(0.1,1000),yaxs="i",xlab="Water Year",las=1) 
mtext(side=2, expression(paste("Concentration, Inorganic Nitrogen, in mg  ",L^-1,sep="")),line=4)
dev.off()


#NH3_changepoint <- read.table ("/Users/joed/LTIMP_TA/LTIMP_TA2/EGRET//NH4/EGRET_plots/_NH3_RawVals.txt",header = TRUE)
#NH3_changepoint2<-as.numeric(NH3_changepoint$FNConc) 
#NH3changepoint2.binseg=cpt.meanvar(NH3_changepoint2,test.stat='Normal',method='PELT',param.estimates=TRUE,Q=5,penalty="SIC")
#cpts(NH3changepoint2.binseg)
#tiff("/Users/joed/LTIMP_TA/LTIMP_TA2/EGRET//NH4/EGRET_plots/_NH3_Changepoint",height = 700, width = 1200, res=120)
#plot(NH3changepoint2.binseg,type='line',col="blue",ylim = c(0.002,0.005))
#dev.off()
#NH3changepoint2.binseg
#plot(NH3changepoint2.binseg,type='line',col="blue",ylim=c(0.002,0.005))


# ---------------------------
# Now run the EGRETci package
# ---------------------------
#setwd("C:/Users/dsaleh/Documents/GitHub/PES_Project/Vernalis_EGRET")
#setwd("/Users/joed/PES_Project/Vernalis_EGRET/NH4/")
# Change working directory
setwd("C:/Users/dsaleh/Documents/GitHub/PES_Project/Vernalis_EGRET/NH4/")
#setwd("/Users/joed/PES_Project/Vernalis_EGRET/NH4/")
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
                        bootBreak = 100, 
                        blockLength = 200)
eBoot <- wBT(eList, caseSetUp, fileName ="outputText.txt")
#

#
saveEGRETci(eList, eBoot, caseSetUp, fileName = "EGRETci_outputVern_SSC_.RData")

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

tiff("histo_NH4__Trend_conc_flux.tif", height = 700, width = 1200, res=120)
par(mfrow=c(1,2))
plotHistogramTrend2(eBoot, caseSetUp, eList, flux=FALSE, xSeq = seq(-800000,8000000,5),las=1,xlim=c(-150,50))
abline(h=0)

plotHistogramTrend2(eBoot, caseSetUp, eList, flux=TRUE, xSeq = seq(-5000000,50000000,5),las=1,xlim=c(-200,200))
abline(h=0)
dev.off()

nBoot <- 200
blockLength <- 200
coreOut <- 5 #Number of cores to leave out of processing tasks

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

tiff("Ann_Avg_Conc_&_Ann_Flow_Normalized_Conc_Boot_SanJVernalis_NH4.tif", height = 500, width = 600, res=110)
plotConcHistBoot(eList, CIAnnualResults, plotFlowNorm=TRUE, showYLabels=TRUE, showYAxis=TRUE,col=4)
polygon(x=conc.poly.x, y=conc.poly.y, col=rgb(24,116,205,40,max=255),border=NA)
dev.off()

tiff("Ann_Flux_&_Ann_Flow_Normalized_Flux_Boot_SanJVernalis_NH4.tif", height = 500, width = 600, res=110)
plotFluxHistBoot(eList, fluxUnit=13, CIAnnualResults, showYLabels=TRUE, showYAxis=TRUE, col=4)
polygon(x=flux.poly.x, y=flux.poly.y, col=rgb(24,116,205,40,max=255),border=NA)
dev.off()

setSweave("Vern_Conc_NH4_EGRETCI",7,7)
plotConcHistBoot(eList, CIAnnualResults, plotFlowNorm=TRUE, showYLabels=TRUE, showYAxis=TRUE,col=4)
polygon(x=conc.poly.x, y=conc.poly.y, col=rgb(24,116,205,40,max=255),border=NA)
graphics.off()

setSweave("Vern_Flux_NH4_EGRETCI",7,7)
plotFluxHistBoot(eList, fluxUnit=13, CIAnnualResults, showYLabels=TRUE, showYAxis=TRUE, col=4)
polygon(x=flux.poly.x, y=flux.poly.y, col=rgb(24,116,205,40,max=255),border=NA)
graphics.off()


saveEGRETci(eList, eBoot, fileName="NH4_Boot_.RData")
save(repAnnual,file="RepAnnual")

# load(file="N_Boot.RData")
# load(file="RepAnnual")