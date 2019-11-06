library(EGRET)
library(rkt)
library(zyp)
library(Kendall)
library(zoo)

library(dataRetrieval)
library(rloadest)
library(EGRETci)
library(foreach)
library(doParallel)
library(iterators)
library(zoo)
library(plotrix)

#dat <- read.table(textConnection("staid  start  end  name
#10336698  1979-10-01  2017-02-18   Third_Crk_nr_Crystal_Bay_NV




setwd('/users/joed/Documents/Documents19/Biogeochemistry_2019/Freeport_QuantileKendall')

#for (i in (1:nrow(dat))){
    
 #   sta   <- dat[i,'staid']
#    param <- "00060"
#    start <- dat[i,'start']
#    end   <- dat[i,'end']
#    fileName <- paste0(dat[i,'name'],".pdf")

    sta <- "11447650"
    param <- "00060"
    start <- "1970-10-01"
      end   <- "2019-04-27"
       fileName <- "SacFreeport.pdf"
    
    paStart <- 10
    paLong <- 12
    
    # Bob Hirsch: note, for annual analyses it is best to use paStart = 4, paLong = 12
    # but for seasonal analyses pick the combination of paStart and paLong that
    # is appropriate for your work
    # for this next command I had to figure out the best start and end dates to get full water years
    filePath <- '/users/joed/Documents/Documents19/Biogeochemistry_2019/Freeport_QuantileKendall/'
    fileName <- "FreeportDaily_cfs.csv"
    #Daily <- readNWISDaily(sta,param,startDate = start, endDate = end)
    Daily <-readUserDaily(filePath, fileName, hasHeader = TRUE, separator = ",", qUnit = 1)
    
    # If there are NA's in Daily$Q, functions won't work, replace with interpolated values
    if(any(is.na(Daily$Q))){
        x <- zoo(Daily$Q,Daily$Date)
        x <- as.ts(x)
        x <- na.interp(x)
        Daily$Q <- x
        
        # Alternative for replace with 0.1% of the mean
        # Daily[is.na(Daily$Q),'Q'] <- mean(Daily$Q, na.rm=TRUE) * 0.001
    }
    INFO  <- readNWISInfo(sta,param,interactive=FALSE)
    eList <- as.egret(INFO,Daily)
    eList <- setPA(eList,paStart = paStart, paLong = paLong, window = 30)
    #old width and height were 12 and 10
    pdf(file = "Freeportquantile.pdf", width =6, height = 5)
      par(mfrow=c(2,2))
      plotFlowSingleKendall(eList, istat = 1, qUnit = 2,cex=0.5,cex.axis = 0.65, cex.main = 0.65,cex.lab=0.75,col='blue')
      plotFlowSingleKendall(eList, istat = 4, qUnit = 2,cex=0.5,cex.axis = 0.65, cex.main = 0.65, cex.lab=0.75)
      plotFlowSingleKendall(eList, istat = 8, qUnit = 2,cex=0.5,cex.axis = 0.65, cex.main = 0.65, cex.lab=0.75)
      plotFlowSingleKendall(eList, istat = 5, qUnit = 2,cex=0.5,cex.axis = 0.65, cex.main = 0.65, cex.lab=0.75)
    dev.off()
    
    
    # Be sure to paste the functions stored in "...\LTIMP_TA\LTIMP_TA2\Quantile_Kendall_AnalysisfunctionsForQuantileKendall.R"
    # before attempting to run the following commands
    # --------------------------------------------------------------------------------------------------------------------------
    eList   <- setPA(eList, paStart = paStart, paLong = paLong, window = 30)
    v       <- makeSortQ(eList)
    sortQ   <- v[[1]]
    time    <- v[[2]]
    results <- trendSortQ(sortQ, time)
    pvals   <- c(0.001,0.01,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99,0.999)
    zvals   <- qnorm(pvals)
    name    <- eList$INFO$shortName
    ymax    <- trunc(max(results$slopePct)*10)
    ymax    <- max(ymax + 2, 5)
    ymin    <- floor(min(results$slopePct)*10)
    ymin    <- min(ymin - 2, -5)
    yrange  <- c(ymin/10, ymax/10)
    yticks  <- axisTicks(yrange, log = FALSE)
    p       <- results$pValueAdj
    color   <- ifelse(p <= 0.1,"black","snow3")
    color   <- ifelse(p < 0.05, "red", color)
    pvals   <- c(0.001,0.01,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99,0.999)
    zvals   <- qnorm(pvals)
    name    <- paste(eList$INFO$shortName,
                     "\nAdjusted Mann Kendall significance",
                     "  ", "1970-2019")
    # old width and height were 8 and 6
    #filename2 <- paste0('Mann_Kendall_',dat[i,4],'.pdf')
    filename2 <- 'SacFreeport.pdf'
    pdf(file = filename2, width = 6, height = 4)
      plot(results$z,results$slopePct,
           col = color, 
           pch = 20, 
           cex = 1.0, 
           xlab = "Daily non-exceedance probability", 
           ylab = "Trend slope in percent per year", 
           xlim = c(-3.2, 3.2), 
           ylim = yrange, 
           las = 1, 
           tck = 0.02, 
       
            cex.lab = 1.2, 
           cex.axis = 1.2, 
           axes = FALSE, 
           frame.plot=TRUE)
      mtext(name, side =3, line = 1.5, cex = 1.2)
      axis(1,at=zvals,labels=pvals, las = 1, tck = 0.02)
      axis(2,at=yticks,labels = TRUE, las = 1, tck = 0.02)
      axis(3,at=zvals,labels=FALSE, las = 1, tck=0.02)
      axis(4,at=yticks,labels = FALSE, tick = TRUE, tck = 0.02)
      abline(h=0,col="red")
      legend("bottomright",c("> 0.1","0.05 - 0.1","< 0.05"),
             col = c("snow3","black","red"),pch = 20,
             pt.cex=1.0, cex = 1.0, bg='white', bty='n')
    dev.off()
#}

