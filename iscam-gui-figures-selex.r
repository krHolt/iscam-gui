#**********************************************************************************
# ss-explore-figures-selex.r
# This file contains the code for plotting selectivity values SS outputs using the
# infrastructure provided with ss-explore.
#
# Author            : Chris Grandin
# Development Date  : October 2013
# Current version   : 1.0
#**********************************************************************************

plotSelex <- function(plotNum    = 1,         # Plot code number
                   png        = .PNG,      # TRUE/FALSE for PNG image output
                   fileText   = "Default", # Name of the file if png==TRUE
                   plotMCMC   = FALSE,     # TRUE/FALSE to plot MCMC output
                   ci         = NULL,      # confidence interval in % (0-100)
                   multiple   = FALSE,     # TRUE/FALSE to plot sensitivity cases
                   sensGroup  = 1,         # Sensitivity group to plot if multiple==TRUE
                   index      = 1         # Gear index to plot 
                   ){

  # plotNum must be one of:
  # 1 Selectivity at age in end year by fleet (currently, only if logistic selectivity (by age or length) used)
  # 2 Selectivity at length in end year by fleet (only if length-based selectivity used)
  # 3 Selectivity at age relationship in end year by fleet (currently, only if logistic selectivity (by age or length) used)
  # 4 Selectivity at length relationship in end year by fleet (only if length-based selectivity used)

  # Other selectivities to be added later ....

  # 3  Selectivity at length time-varying surface
  # 4  Selectivity at length time-varying contour
  # 5  Retention at length time-varying surface
  # 6  Retention at length time-varying surface
  # 7  Discard mortality time-varying surface
  # 8  Discard mortality time-varying contour
  # 9  Selectivity, retention, and discard mortality at length in ending year
  # 10 NOT USED
  # 11 Selectivity at age time-varying surface
  # 12 Selectivity at age time-varying contour
  # 13 Selectivity at age in ending year if time-varying
  # 14 Selectivity at age in ending year if NOT time-varying
  # 15 NOT USED
  # 16 NOT USED
  # 17 NOT USED
  # 18 NOT USED
  # 19 NOT USED
  # 20 NOT USED
  # 21 Selectivity at age and length contour with overlaid growth curve
  # 22 Selectivity with uncertainty if requested at end of control file

  if(plotNum < 1   ||
     plotNum > 22  ||
     plotNum == 10 ||
     plotNum == 15 ||
     plotNum == 16 ||
     plotNum == 17 ||
     plotNum == 18 ||
     plotNum == 19 ||
     plotNum == 20
     ){
    return(FALSE)
  }
  val      <- getWinVal()
  scenario <- val$entryScenario
  currFuncName <- getCurrFunc()
  isMCMC   <- op[[scenario]]$inputs$log$isMCMC
  figDir   <- op[[scenario]]$names$figDir
  out      <- op[[scenario]]$outputs$mpd
  res          <- val$entryResolution
  width        <- val$entryWidth
  height       <- val$entryHeight
  resScreen    <- val$entryResolutionScreen
  widthScreen  <- val$entryWidthScreen
  heightScreen <- val$entryHeightScreen

  filenameRaw  <- paste0(op[[scenario]]$names$scenario,"_",fileText,".png")
  filename     <- file.path(figDir,filenameRaw)
  if(png){
    graphics.off()
    png(filename,res=res,width=width,height=height,units=units)
  }else{
    windows(width=widthScreen,height=heightScreen)
  }
  if(isMCMC){
   # plot mcmc model runs
  }else{
    # plot mpd model runs
  }

  if(plotNum==1)  plotLogisticSelAtAge(scenario, index)
  if(plotNum==2)  plotLogisticSelAtLength(scenario, index)
  if(plotNum==3)  plotLogisticSelAtAgeRelationship(scenario, index)
  if(plotNum==4)  plotLogisticSelAtLengthRelationship(scenario, index)

   if(plotNum>=5)  cat("No Plot Yet -- Coming Soon!!\n")

  #SSplotSelex(out,
 #             plot     = TRUE,
#              print    = FALSE,
#              subplot  = plotNum,
#              pheight  = height,
#              pwidth   = width,
 #             punits   = units,
#              res      = res,
#              verbose  =!silent)
  if(png){
    cat(.PROJECT_NAME,"->",currFuncName,"Wrote figure to disk: ",filename,"\n\n",sep="")
    dev.off()
  }
  return(TRUE)
}


# Function:  plotLogisticSelAtAge
# Written by: K. Holt, Sept 2, 2014
# Notes: Plots proportion selected at age for 3 selectivity types (logistic age-based, logistic age-based fixed, & logistic length-based)
# When selectivity is length-based, the plotted relationship is based on the specified legth-at-age key, which
# back-calculates from selectivity at length to selectivity at age.  Seperate lines are used for males and females when model is 2-sex.
# Function is adapted from plotLogisticSel()
 #Currently only implemented for seltypes 1,6 and 11 (estimated logistic age-based, fixed logistic age-based, or estimated logistic length-based)
 plotLogisticSelAtAge	<-	function(scenario, index){
 	aflag <- 0 #flag to set age or length
 	ngear <-  op[[scenario]]$inputs$data$ngear
  nsex <-  op[[scenario]]$inputs$data$nsex
	if(index <= ngear)
	{

		selType <-   op[[scenario]]$inputs$control$sel[1,index]
		selBlocks <- op[[scenario]]$inputs$control$sel[10,index] #selectivity time blocks

    if(selType==1 || selType ==6)  {aflag <-1}
		else if(selType==11) {aflag <- 2 }

		Age <- op[[scenario]]$output$mpd$age

		#no plot if sel is not one of the types listed above
		if(aflag > 0){

    if(selBlocks == 1)  {

			logselData <-  op[[scenario]]$output$mpd$log_sel
			logselData <- logselData[which(logselData[,1]==index),]
      xx <- logselData[,4:ncol(logselData)]
      selData <- exp(xx)
      selData <- selData[1,] #selectivity in first block
      selData <- as.matrix(selData)

      # Create above matrices for 2-sex case
      if (nsex == 2) {
         logselDataSex1 <- logselData[which(logselData[,2]==1),]    # sex 1 only
         xx1 <- logselDataSex1[,4:ncol(logselDataSex1)]
         selDataSex1 <- exp(xx1)
         selDataSex1 <- selDataSex1[1,] #selectivity in first block
         selDataSex1 <- as.matrix(selDataSex1)

         logselDataSex2  <- logselData[which(logselData[,2]==2),]       # sex 2 only
         xx2 <- logselDataSex2[,4:ncol(logselDataSex2)]
         selDataSex2 <- exp(xx2)
         selDataSex2 <- selDataSex2[1,] #selectivity in first block
         selDataSex2 <- as.matrix(selDataSex2)

      }


     if (nsex == 1) {
		    plot(Age, selData[,1], type="l", xlab="Age", ylab="Proportion", lwd=2, col=1, las=1, main=paste("Gear",index), ylim=c(0,1.1))	     #
	      legend("topleft", legend=legtext, lty=1:selBlocks, col=1:selBlocks, lwd=2, bty="n")
      }

     if (nsex == 2) {
		    plot(Age, selDataSex1[,1], type="l", xlab="Age", ylab="Proportion", lwd=2, col=1, las=1, main=paste("Gear",index), ylim=c(0,1.1))	     #
        lines (Age, selDataSex2[,1], lty=2, col=2, lwd=2 )
        legend("topleft", legend=c("Sex 1", "Sex 2"), lty=1:2, col=1:2, lwd=2, bty="n")
      }


			 }else cat("Plot not currently implemented for > one selectivity block\n")
       }else cat("Plot only works for gears with length-based selectivity\n")
   	     }else cat(paste("WARNING: Gear index exceeds the number of gears. Choose gear between 1 and", ngear,"\n"))
}


# Function:  plotLogisticSelAtAgeRelationship
# Written by: K. Holt, Sept 2, 2014
# Notes: Plots estimated (or fixed) relationship for age-based logistic selectivity
 #Currently only implemented for seltypes 1 and 6 (estimated logistic age-based, fixed logistic age-based)
 plotLogisticSelAtAgeRelationship	<-	function(scenario, index){
 	aflag <- 0 #flag to set age or length
 	ngear <-  op[[scenario]]$inputs$data$ngear
  nsex <-  op[[scenario]]$inputs$data$nsex
	if(index <= ngear)
	{

		selType <-   op[[scenario]]$inputs$control$sel[1,index]
		selBlocks <- op[[scenario]]$inputs$control$sel[10,index] #selectivity time blocks

    if(selType==1 || selType ==6)  {aflag <-1}
		else if(selType==11) {aflag <- 2 }

		#no plot if sel is not one of the types listed above
		if(aflag == 1){

     if(selBlocks == 1)  {

       a<-seq(op[[scenario]]$output$mpd$sage,op[[scenario]]$output$mpd$nage,length=100)

       selPars <-  op[[scenario]]$output$mpd$sel_par
			 selPars <- selPars[which(selPars[,1]==index),]

       if (selPars[2]==1) {  #Plot only works for gears with one time block at present
         ahat<-op[[scenario]]$output$mpd$sel_par[index,][3]
         ghat<-op[[scenario]]$output$mpd$sel_par[index,][4]
       }
       sel<-plogis(a,ahat,ghat)

		   plot(a, sel, type="l", xlab="Age", ylab="Proportion", lwd=2, col=1, las=1, main=paste("Gear",index), ylim=c(0,1.1))	     #

			 }else cat("Plot not currently implemented for > one selectivity block\n")
       }else cat("Plot only works for gears with age-based selectivity\n")
   	     }else cat(paste("WARNING: Gear index exceeds the number of gears. Choose gear between 1 and", ngear,"\n"))
}


 # Function:  plotLogisticSelAtLength
# Written by: K. Holt, Sept 2, 2014
# Notes: Plots proportion selected at length for length-based logistic selectivity.
# Function is adapted from plotLogisticSel()
 #Currently only implemented for seltype 11 (estimated logistic age-based, fixed logistic age-based, or estimated logistic length-based)
  plotLogisticSelAtLength	<-	function(scenario, index){
 	aflag <- 0 #flag to set age or length
 	ngear <-  op[[scenario]]$inputs$data$ngear
	if(index <= ngear)
	{

		selType <-   op[[scenario]]$inputs$control$sel[1,index]
		selBlocks <- op[[scenario]]$inputs$control$sel[10,index] #selectivity time blocks
    nsex <-  op[[scenario]]$inputs$data$nsex

    if(selType==1 || selType ==6)  {aflag <-1}
		else if(selType==11) {aflag <- 2 }

    Len <- op[[scenario]]$output$mpd$la

		#no plot if sel is not l  ength-based logistic
		if(aflag == 2){

    if(selBlocks == 1)  {

			logselData <-  op[[scenario]]$output$mpd$log_sel
			logselData <- logselData[which(logselData[,1]==index),]

      # If two-sex model, hardwired to use female relationship (although, will be the same for both sexes)
      if (nsex == 2) {
        logselData <- logselData[which(logselData[,2]==2),]
        Len<-Len[2,]
      }

      xx <- logselData[,4:ncol(logselData)]
      selData <- exp(xx)
      selData <- selData[1,] #selectivity in first block
      selData <- as.matrix(selData)

		  plot(Len, selData[,1], type="l", xlab="Length", ylab="Proportion", lwd=2, col=1, las=1, main=paste("Gear",index), ylim=c(0,1.1))	     #

			 }else cat("Plot not currently implemented for > one selectivity block\n")
       }else cat("Plot not currently implemented for the type of selectivity used for this gear\n")
   	     }else cat(paste("WARNING: Gear index exceeds the number of gears. Choose gear between 1 and", ngear,"\n"))
}


# Function:  plotLogisticSelAtLengthRelationship
# Written by: K. Holt, Sept 2, 2014
# Notes: Plots estimated (or fixed) relationship for length-based logistic selectivity
 #Currently only implemented for seltype 11
 plotLogisticSelAtLengthRelationship	<-	function(scenario, index){
 	aflag <- 0 #flag to set age or length
 	ngear <-  op[[scenario]]$inputs$data$ngear
  nsex <-  op[[scenario]]$inputs$data$nsex
	if(index <= ngear)
	{

		selType <-   op[[scenario]]$inputs$control$sel[1,index]
		selBlocks <- op[[scenario]]$inputs$control$sel[10,index] #selectivity time blocks

		if(selType==11) {aflag <- 1 }

		#no plot if sel is not one of the types listed above
		if(aflag == 1){

     if(selBlocks == 1)  {

       l<-seq(min(op[[scenario]]$output$mpd$la),max(op[[scenario]]$output$mpd$la),length=100)

       selPars <-  op[[scenario]]$output$mpd$sel_par
			 selPars <- selPars[which(selPars[,1]==index),]

       if (selPars[2]==1) {  #Plot only works for gears with one time block at present
         ahat<-op[[scenario]]$output$mpd$sel_par[index,][3]
         ghat<-op[[scenario]]$output$mpd$sel_par[index,][4]
       }
       sel<-plogis(l,ahat,ghat)

		   plot(l, sel, type="l", xlab="Length", ylab="Proportion", lwd=2, col=1, las=1, main=paste("Gear",index), ylim=c(0,1.1))	     #

			 }else cat("Plot not currently implemented for > one selectivity block\n")
       }else cat("Plot only works for gears with length-based selectivity\n")
   	     }else cat(paste("WARNING: Gear index exceeds the number of gears. Choose gear between 1 and", ngear,"\n"))
}






 #Currently only implemented for seltypes 1,6 and 11 (estimated logistic age-based, fixed logistic age-based, or estimated logistic length-based)
 plotLogisticSel	<-	function(scenario, index){
 	aflag <- 0 #flag to set age or length
 	ngear <-  op[[scenario]]$inputs$data$ngear
	if(index <= ngear)
	{
	
		selType <-   op[[scenario]]$inputs$control$sel[1,index]
		selBlocks <- op[[scenario]]$inputs$control$sel[10,index] #selectivity time blocks

    if(selType==1 || selType ==6)  {aflag <-1} 
		else if(selType==11) {aflag <- 2 }

		Age <- op[[scenario]]$output$mpd$age
		Len <- op[[scenario]]$output$mpd$la

		#no plot if sel is not one of the types listed above
		if(aflag > 0){
			logselData <-  op[[scenario]]$output$mpd$log_sel
			logselData <- logselData[which(logselData[,1]==index),]
			xx <- logselData[,4:ncol(logselData)]
			
			selData <- exp(xx)
			selData <- selData[1,] #selectivity in first block

      # K.Holt: problem right here because not accounting for different selectivity at age by sex ....
        # "length at age" can differ for males and females in 2-sex models

			startBlocks <- op[[scenario]]$inputs$control$syrtimeblock[index,]
			legtext <- paste("Selectivity Block 1 :", startBlocks[1])
			


			selData <- as.matrix(selData)
			
			if(selBlocks>1)  {
				for(i in 2:selBlocks) {
					logselBlockData <- xx[which(logselData[,3]==startBlocks[i]),]
					selData <- cbind(selData, exp(logselBlockData))
				}	
			}
							
      if(aflag==2) Xlab="Length-at-Age"
			if(aflag==1) Xlab="Age"
                       
		       if(aflag==1) {
		       		plot(Age, selData[,1], type="l", xlab=Xlab, ylab="Proportion", lwd=2, col=1, las=1, main=paste("Gear",index), ylim=c(0,1.1))	     #
	               		if(selBlocks>1){
	               			for(i in 2:selBlocks) {
	               				lines(Age, selData[,i], lty=i, col=i, lwd=2)
	               				legtext <- c(legtext, paste("Selectivity Block",i,":", startBlocks[i]))               				
	               			} #end for	
	               		}#end if
	               		legend("topleft", legend=legtext, lty=1:selBlocks, col=1:selBlocks, lwd=2, bty="n")
	               	} #end if
	               
	               if(aflag==2) {

                                plot(Len, selData[,1], type="l", xlab=Xlab, ylab="Proportion", lwd=2, col=1, las=1, main=paste("Gear",index),ylim=c(0,1.1))   #
	               	               if(selBlocks>1){
					for(i in 2:selBlocks) {
						lines(Len, selData[,i], lty=i, col=i, lwd=2)
						legtext <- c(legtext, paste("Selectivity Block",i,":", startBlocks[i]))
					}#end for		
				}#end if
	               		legend("topleft", legend=legtext, lty=1:selBlocks, col=1:selBlocks, lwd=2, bty="n")
	               	               
			}#end if
	
			 }else cat("Plot not currently implemented for the type of selectivity used for this gear\n") 
   	     }else cat(paste("WARNING: Gear index exceeds the number of gears. Choose gear between 1 and", ngear,"\n"))
   	}
