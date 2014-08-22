#**********************************************************************************
# iscam-gui-figures-catch.r
# This file contains the code for catch values iscam inputs using the
# infrastructure provided with iscam-gui.
#
# Author            : Chris Grandin
# Development Date  : October 2013 - Present
#**********************************************************************************

plotCatch <- function(scenario   = 1,         # Scenario number
                      plotNum    = 1,         # Plot code number
                      png        = .PNG,      # TRUE/FALSE for PNG image output
                      fileText   = "Default", # Name of the file if png==TRUE
                      plotMCMC   = FALSE,     # TRUE/FALSE to plot MCMC output
                      ci         = NULL,      # confidence interval in % (0-100)
                      multiple   = FALSE,     # TRUE/FALSE to plot sensitivity cases
                      sensGroup  = 1,         # Sensitivity group to plot if multiple==TRUE
                      index      = 1,         # Gear index to plot
                      # PlotSpecs: Width, height, and resolution of screen and file
                      ps         = list(pngres = .RESOLUTION,
                                        pngw   = .WIDTH,
                                        pngh   = .HEIGHT,
                                        res    = .RESOLUTION,
                                        w      = .WIDTH,
                                        h      = .HEIGHT),
                      leg        = "topright",# Legend location. If NULL, none will be drawn
                      units    = .UNITS,
                      silent   = .SILENT){

  # Assumes that 'op' list exists and has been populated correctly.
  # plotNum must be one of:
  # 1  Landings
  # 2  SPR status, or the spawning potential ratio:
  #    (1-spr)/(1-spr.at.msy)
  currFuncName <- getCurrFunc()
  scenarioName <- op[[scenario]]$names$scenario
  inp          <- op[[scenario]]$inputs$data
  inputs       <- op[[scenario]]$inputs
  figDir       <- op[[scenario]]$names$figDir
  color        <- op[[scenario]]$inputs$color
  res          <- ps$pngres
  width        <- ps$pngw
  height       <- ps$pngh
  resScreen    <- ps$res
  widthScreen  <- ps$w
  heightScreen <- ps$h

  oldPar <- par(no.readonly=TRUE)
  on.exit(par(oldPar))

  if(plotNum < 1 || plotNum > 16){		  #RF changed 15 to 16
    return(FALSE)
  }
  isMCMC   <- op[[scenario]]$inputs$log$isMCMC
  figDir   <- op[[scenario]]$names$figDir
  out      <- op[[scenario]]$outputs$mpd

  filenameRaw  <- paste0(op[[scenario]]$names$scenario,"_",fileText,".png")
  filename     <- file.path(figDir,filenameRaw)
  if(png){
    graphics.off()
    png(filename,res=res,width=width,height=height,units=units)
  }
  if(isMCMC){
   # plot mcmc model runs
  }else{
    # plot mpd model runs
  }
  if(plotNum == 1){
    plotCatchesByGear(inp = inputs, scenarioName, leg = leg, col = color)
  }
  if(plotNum == 2){
    plotCatches(inp = inputs, scenarioName, leg = leg, col = color)
  }
  if(plotNum == 3){
    plotCatchFit(inp = inputs, out=out, scenarioName, leg = leg, col = color)
  }
  if(plotNum == 4){
    plotCatchFitMulti(inp = inputs, out=out, scenarioName, leg = leg, col = color)
  }
  if(plotNum == 5){
    plotCatchFitByGear(inp = inputs, out=out, scenarioName, gearInd = index, leg = leg, col = color)
  }
  if(plotNum == 16){
      plotExpVsObsAnnualMeanWt(inp = inputs, out=out, scenarioName, leg = leg, col = color)
 }

  if(png){
    cat(.PROJECT_NAME,"->",currFuncName,"Wrote figure to disk: ",filename,"\n\n",sep="")
    dev.off()
  }
  return(TRUE)
}

plotCatches <- function(inp,
                        scenarioName,
                        verbose = FALSE,
                        leg = "topright",
                        col = 1){
  # Catch plot for iscam model, plots by gear
  oldPar <- par(no.readonly=TRUE)
  on.exit(par(oldPar))

  # Check number of catch types (e.g., biomass, numbers, spawn)
  catchTypeList<-unique(inp$data$catch[,6])
  nCatchTypes<-length(catchTypeList)

  if (nCatchTypes == 1) {

     catch <- as.data.frame(inp$data$catch)
     p <- ggplot(catch,aes(x=factor(year),value))
	   p <- p + geom_bar(width=0.75,position="dodge",stat="identity")
     p <- p + labs(x="Year",y="Catch")
     p <- p + .PLOT_THEME
     p <- p + theme(axis.text.x = element_text(angle = -90, hjust = 0))
	   print(p)
  }

  if (nCatchTypes == 2) {

     minYear<-min(inp$data$catch[,1])
     maxYear<-max(inp$data$catch[,1])

     catch1 <- subset(as.data.frame(inp$data$catch), type==catchTypeList[1])
     catch2 <- subset(as.data.frame(inp$data$catch), type==catchTypeList[2])

     catchLab<-rep(NA, 2)
     for (i in 1:2) {
         if (catchTypeList[i] == 1) catchLab[i]<-"Biomass"
         if (catchTypeList[i] == 2) catchLab[i]<-"Numbers"
         if (catchTypeList[i] == 3) catchLab[i]<-"Spawn (Roe)"
      }

      p1 <- ggplot(catch1,aes(x=year,value)) + scale_x_continuous(limits = c(minYear, maxYear))
	    p1 <- p1 + geom_bar(width=0.75,position="dodge",stat="identity")
      p1 <- p1 + labs(x="Year",y=catchLab[1])
      p1 <- p1 + .PLOT_THEME
      p1 <- p1 + theme(axis.text.x = element_text(angle = -90, hjust = 0))

      p2 <- ggplot(catch2,aes(x=year,value))
      p2 <- p2 + geom_bar(width=0.75,position="dodge",stat="identity") + scale_x_continuous(limits = c(minYear, maxYear))
      p2 <- p2 + labs(x="Year",y=catchLab[2])
      p2 <- p2 + .PLOT_THEME
      p2 <- p2 + theme(axis.text.x = element_text(angle = -90, hjust = 0))

      multiplot(p1, p2, cols = 1)

  }

  if (nCatchTypes > 2) {
     print("Plotting of more than two catch types not yet possible")

  }



}


plotCatchesByGear <- function(inp,
                        scenarioName,
                        gearInd=1,
                        verbose = FALSE,
                        leg = "topright",
                        col = 1){
  # Catch plot for iscam model, plots by gear
  oldPar <- par(no.readonly=TRUE)
  on.exit(par(oldPar))

  # Check number of catch types (e.g., biomass, numbers, spawn)
  catchTypeList<-unique(inp$data$catch[,6])
  nCatchTypes<-length(catchTypeList)

  if (nCatchTypes == 1) {

     catch <- as.data.frame(inp$data$catch)
     p <- ggplot(catch,aes(x=factor(year),value,fill=factor(gear)))
	   p <- p + geom_bar(width=0.75,position="dodge",stat="identity")
     p <- p + labs(x="Year",y="Catch",fill="Gear")
     p <- p + .PLOT_THEME
     p <- p + theme(axis.text.x = element_text(angle = -90, hjust = 0))
	   print(p)
  }

  if (nCatchTypes == 2) {

     minYear<-min(inp$data$catch[,1])
     maxYear<-max(inp$data$catch[,1])

     catch1 <- subset(as.data.frame(inp$data$catch), type==catchTypeList[1])
     catch2 <- subset(as.data.frame(inp$data$catch), type==catchTypeList[2])

     catchLab<-rep(NA, 2)
     for (i in 1:2) {
         if (catchTypeList[i] == 1) catchLab[i]<-"Biomass"
         if (catchTypeList[i] == 2) catchLab[i]<-"Numbers"
         if (catchTypeList[i] == 3) catchLab[i]<-"Spawn (Roe)"
      }

      p1 <- ggplot(catch1,aes(x=year,value,fill=factor(gear))) + scale_x_continuous(limits = c(minYear, maxYear))
	    p1 <- p1 + geom_bar(width=0.75,position="dodge",stat="identity")
      p1 <- p1 + labs(x="Year",y=catchLab[1],fill="Gear")
      p1 <- p1 + .PLOT_THEME
      p1 <- p1 + theme(axis.text.x = element_text(angle = -90, hjust = 0))

      p2 <- ggplot(catch2,aes(x=year,value,fill=factor(gear)))
      p2 <- p2 + geom_bar(width=0.75,position="dodge",stat="identity") + scale_x_continuous(limits = c(minYear, maxYear))
      p2 <- p2 + labs(x="Year",y=catchLab[2],fill="Gear")
      p2 <- p2 + .PLOT_THEME
      p2 <- p2 + theme(axis.text.x = element_text(angle = -90, hjust = 0))

     multiplot(p1, p2, cols = 1)

  }

  if (nCatchTypes > 2) {
     print("Plotting of more than two catch types not yet possible")

  }



}



plotSPR <-  function(inp,
                     scenarioName,
                     verbose = FALSE,
                     leg = "topright",
                     col = 1){

}


plotCatchFit<-function(inp,
                                out,
                                scenarioName,
                                verbose = FALSE,
                                leg = "topright",
                                col = 1){

  ngear<-inp$data$ngear
  catchData <- inp$data$catch
  years <- catchData[,"year"]
  obsCt <- catchData[,"value"]
  gear <-catchData[,"gear"]
  gearList<-unique(gear)
  predCt <-out$ct

  par(mfrow=c(1,1),mar=c(5,4,2,2))


  pchList<-c(19,17,4,15)
  colList<-c("black", "red", "blue", "steelblue2")

  # Set-up plot area
  xLim <- range(years)
  yLim <- c(0,(max(obsCt,predCt)*1.1))

  plot(xLim, yLim, type="n", axes=TRUE, xlab="Year", ylab="Catch")
  box()

  for (i in 1:ngear) {
    points(years[gear==gearList[i]], obsCt[gear==gearList[i]], pch=pchList[i], col=colList[i])
    lines(years[gear==gearList[i]], predCt[gear==gearList[i]], col=colList[i])
  }

}


plotCatchFitMulti<-function(inp,
                                out,
                                scenarioName,
                                verbose = FALSE,
                                leg = "topright",
                                col = 1){

  catchData <- inp$data$catch
  years <- catchData[,"year"]
  obsCt <- catchData[,"value"]
  gear <-catchData[,"gear"]
  catchType<-catchData[,"type"]
  gearList<-unique(gear)
  ngear<-length(gearList)
  predCt <-out$ct

  if (ngear==1) par(mfrow=c(1,1),mar=c(5,4,2,2))
  if (ngear == 2) par(mfrow=c(2,1),mar=c(4,4,2,2))
  if (ngear == 3 | ngear == 4) par(mfrow=c(2,2),mar=c(4,4,2,2))
  if (ngear == 5 | ngear == 6) par(mfrow=c(3,2),mar=c(4,4,2,2))

  for (i in 1:ngear) {
      # Set-up plot area
      xLim <- range(years)
      yLim <- c(0,(max(obsCt[gear==gearList[i]],predCt[gear==gearList[i]])*1.1))

      if (unique(catchType[gear==gearList[i]]) == 1) catchLab<-"Catch Biomass"
      if (unique(catchType[gear==gearList[i]]) == 2) catchLab<-"Catch Numbers"
      if (unique(catchType[gear==gearList[i]]) == 3) catchLab<-"Spawn (Roe)"


      plot(xLim, yLim, type="n", axes=TRUE, xlab="Year", ylab=catchLab)

      points(years[gear==gearList[i]], obsCt[gear==gearList[i]], pch=19)
      lines(years[gear==gearList[i]], predCt[gear==gearList[i]], col="grey50")
      box()

  }

  par(mfrow=c(1,1),mar=c(5,4,2,2))

}


plotCatchFitByGear<-function(inp,
                                out,
                                scenarioName,
                                verbose = FALSE,
                                gearInd = 1,
                                leg = "topright",
                                col = 1){

  catchData <- inp$data$catch
  years <- catchData[,"year"]
  obsCt <- catchData[,"value"]
  gear <-catchData[,"gear"]
  catchType<-catchData[,"type"]
  predCt <-out$ct

  par(mfrow=c(1,1),mar=c(5,4,2,2))

      # Set-up plot area
      xLim <- range(years)
      yLim <- c(0,(max(obsCt[gear==gearInd],predCt[gear==gearInd])*1.1))

      if (unique(catchType[gear==gearInd]) == 1) catchLab<-"Catch Biomass"
      if (unique(catchType[gear==gearInd]) == 2) catchLab<-"Catch Numbers"
      if (unique(catchType[gear==gearInd]) == 3) catchLab<-"Spawn (Roe)"

      plot(xLim, yLim, type="n", axes=TRUE, xlab="Year", ylab=catchLab)

      points(years[gear==gearInd], obsCt[gear==gearInd], pch=19)
      lines(years[gear==gearInd], predCt[gear==gearInd], col="grey50")
      box()



}


plotExpVsObsAnnualMeanWt<-function(inp,
                                out,
                                scenarioName,
                                verbose = FALSE,
                                leg = "topright",
                                col = 1){
  nmeanwtObs <- inp$data$nmeanwtobs
  if( nmeanwtObs > 0){
		  meanwtData <- inp$data$meanwtdata
		  years <- meanwtData[,"year"]
		  obsMeanWt <- meanwtData[,"meanwt"]
		  gear <-meanwtData[,"gear"]
		  gearList<-unique(gear)
		  ngear<-length(gearList)	 #only plot for gears with data
		  predMeanWt <-out$annual_mean_weight

		  if (ngear==1) par(mfrow=c(1,1),mar=c(5,4,2,2))
		  if (ngear == 2) par(mfrow=c(2,1),mar=c(4,4,2,2))
		  if (ngear == 3 | ngear == 4) par(mfrow=c(2,2),mar=c(3,3,2,2))
		  if (ngear == 5 | ngear == 6) par(mfrow=c(3,2),mar=c(2,2,2,2))

		  for (i in 1:ngear) {
		      # Set-up plot area
		      xLim <- range(years)
		      yLim <- c(0,(max(obsMeanWt[gear==gearList[i]],predMeanWt[gear==gearList[i]])*1.1))

		      plot(xLim, yLim, type="n", axes=TRUE, xlab="Year", ylab="Mean Weight in Catch")

		      points(years[gear==gearList[i]], obsMeanWt[gear==gearList[i]], pch=19)
		      lines(years[gear==gearList[i]], predMeanWt[gear==gearList[i]], col="red")
		      box()
		  }
		  par(mfrow=c(1,1),mar=c(5,4,2,2))
	}else cat("WARNING: No Annual Mean Weight Data")
}



# Function for plotting mult-panel plots using ggplot
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
