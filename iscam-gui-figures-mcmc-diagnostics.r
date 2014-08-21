#**********************************************************************************
# iscam-gui-figures-mcmc-convergence.r
# This file contains the code to plot mcmc convergence information
#
# Author            : Chris Grandin
# Development Date  : April 2014 - Present
#**********************************************************************************

plotConvergence <- function(scenario   = 1,         # Scenario number
                            plotNum    = 1,         # Plot code number
                            png        = .PNG,      # TRUE/FALSE for PNG image output
                            fileText   = "Default", # Name of the file if png==TRUE
                            # PlotSpecs: Width, height, and resolution of screen and file
                            ps         = list(pngres = .RESOLUTION,
                                              pngw   = .WIDTH,
                                              pngh   = .HEIGHT,
                                              res    = .RESOLUTION,
                                              w      = .WIDTH,
                                              h      = .HEIGHT),
                            burnthin   = list(0,1), # List of two elements, burnin and thinning
                            exFactor        = 1.5,
                            showEntirePrior = TRUE,
                            units           = .UNITS,
                            silent          = .SILENT){

  # Plot a convergence plot for an MCMC model run
  # plotNum must be one of:
  # 1 Trace plots
  # 2 Autocorrelation plots
  # 3 Density plots
  # 4 Pairs plots with histograms
  # 5 Priors vs. Posteriors plots
  # 6 Variance partitions

  currFuncName <- getCurrFunc()
  scenarioName <- op[[scenario]]$names$scenario
  mcmcOut      <- op[[scenario]]$outputs$mcmc
  inputs       <- op[[scenario]]$inputs # For the priors information
  if(is.null(mcmcOut)){
    cat0(.PROJECT_NAME,"->",currFuncName,"The model ",scenarioName," has no mcmc output data associated with it.\n")
    return(NULL)
  }

  figDir       <- op[[scenario]]$names$figDir
  res          <- ps$pngres
  width        <- ps$pngw
  height       <- ps$pngh
  resScreen    <- ps$res
  widthScreen  <- ps$w
  heightScreen <- ps$h

  filenameRaw  <- paste0(scenarioName,"_",fileText,".png")
  filename     <- file.path(figDir,filenameRaw)
  if(png){
    graphics.off()
    png(filename,res=res,width=width,height=height,units=units)
  }else{
    windows(width=widthScreen,height=heightScreen)
  }
  mcmcData <- mcmcOut$params
  # If you are trying to show more than one group or area's parameters,
  # comment the next line below out
  mcmcData <- stripAreasGroups(mcmcData)

  if(plotNum == 1){
    plotTraces(mcmcData, burnthin=burnthin)
  }
  if(plotNum == 2){
    plotAutocor(mcmcData, burnthin=burnthin)
  }
  if(plotNum == 3){
    plotDensity(mcmcData, burnthin=burnthin)
  }
  if(plotNum == 4){
    plotPairs(mcmcData, burnthin=burnthin)
  }
  if(plotNum == 5){
    plotPriorsPosts(mcmcData, inputs = inputs, burnthin=burnthin)
  }
  if(plotNum == 6){
    plotVariancePartitions(mcmcData, burnthin=burnthin)
  }
  if(png){
    cat(.PROJECT_NAME,"->",currFuncName,"Wrote figure to disk: ",filename,"\n\n",sep="")
    dev.off()
  }
  return(TRUE)
}

stripAreasGroups <- function(dat){
  # This is a hack function to remove the area and group prefixes for the mcmc data 'dat'
  # The reason is that for now we are just working with a single group and area, and
  # the extra text in the parameter names are confusing, eg. 'ro_gr1' will become just 'ro'
  # If you make a model with more than one group or area this will need to be revisited.
  # ALSO this removes 'f' which is assumed to be the objective function value
  # Note than q1, q2, q3... will stay the same and m1 and m2 will remain if the model was two-sex.

  pnames <- names(dat)
  pnames <- gsub("m_gs1","m1",pnames)
  pnames <- gsub("m_gs2","m2",pnames)
  pnames <- gsub("msy1","msy",pnames)
  pnames <- gsub("fmsy1","fmsy",pnames)
  pnames <- gsub("SSB1","ssb",pnames)
  names(dat) <- gsub("_+.*","",pnames)
  dat <- dat[,names(dat) != "f"]
  return(dat)
}


plotTraces <- function(mcmcData = NULL, burnthin = c(0,1), axis.lab.freq=200){
  # Traceplots for an mcmc matrix, mcmcData
  # axis.lab.freq is the frequency of x-axis labelling

  oldPar <- par(no.readonly=TRUE)
  on.exit(par(oldPar))

  burnin       <- burnthin[1]
  thinning     <- burnthin[2]
  currFuncName <- getCurrFunc()
  if(is.null(mcmcData)){
    cat0(.PROJECT_NAME,"->",currFuncName,"mcmcData must be supplied.\n")
    return(NULL)
  }
  if(burnin > nrow(mcmcData)){
    cat0(.PROJECT_NAME,"->",currFuncName,"Burnin value exceeds mcmc chain length.\n")
    return(NULL)
  }
  numParams <- ncol(mcmcData)
  # The next line is a simple algorithm, just generate the smallest square grid
  # that will hold all of the parameter trace plots.
  nrows <- ncols <- ceiling(sqrt(numParams))
	par(mfrow=c(nrows, ncols), las=1)
  mcmcData <- window(as.matrix(mcmcData), start=burnin, thin=thinning)
  for(param in 1:numParams){
    par(mar=.MCMC_MARGINS)
    mcmcTrace <- as.matrix(mcmcData[,param])
    plot(mcmcTrace, main=colnames(mcmcData)[param], type="l",ylab="",xlab="",axes=F)
    box()
    at <- labels <- seq(0,nrow(mcmcData), axis.lab.freq)
    axis(1, at=at, labels=labels)
    axis(2)
  }
}

plotAutocor <- function(mcmcData = NULL,
                        burnthin = c(0,1),
                        lag = c(0, 1, 5, 10, 15, 20, 30, 40, 50)){
  # Plot autocorrelations for all mcmc parameters
  oldPar <- par(no.readonly=TRUE)
  on.exit(par(oldPar))

  burnin       <- burnthin[1]
  thinning     <- burnthin[2]
  currFuncName <- getCurrFunc()
  if(is.null(mcmcData)){
    cat0(.PROJECT_NAME,"->",currFuncName,"mcmcData must be supplied.\n")
    return(NULL)
  }
  if(burnin > nrow(mcmcData)){
    cat0(.PROJECT_NAME,"->",currFuncName,"Burnin value exceeds mcmc chain length.\n")
    return(NULL)
  }
  numParams <- ncol(mcmcData)
  # The next line is a simple algorithm, just generate the smallest square grid
  # that will hold all of the parameter trace plots.
  nrows <- ncols <- ceiling(sqrt(numParams))
	par(mfrow=c(nrows, ncols), las=1)

  for(param in 1:numParams){
    par(mar=.MCMC_MARGINS)
    mcmcAutocor <- window(mcmc(as.ts(mcmcData[,param])), start = burnin, thin = thinning)
    autocorr.plot(mcmcAutocor, lag.max = 100, main = colnames(mcmcData)[param], auto.layout = FALSE)
  }
}

plotDensity <- function(mcmcData = NULL, burnthin = c(0,1), color = 1, opacity = 30){
  # Plot densities for the mcmc parameters in the matrix mcmcData
	oldPar	<- par(no.readonly=T)
  on.exit(par(oldPar))

  burnin       <- burnthin[1]
  thinning     <- burnthin[2]
  currFuncName <- getCurrFunc()
  if(is.null(mcmcData)){
    cat0(.PROJECT_NAME,"->",currFuncName,"mcmcData must be supplied.\n")
    return(NULL)
  }
  if(burnin > nrow(mcmcData)){
    cat0(.PROJECT_NAME,"->",currFuncName,"Burnin value exceeds mcmc chain length.\n")
    return(NULL)
  }
  numParams <- ncol(mcmcData)
  # The next line is a simple algorithm, just generate the smallest square grid
  # that will hold all of the parameter trace plots.
  nrows <- ncols <- ceiling(sqrt(numParams))
	par(mfrow=c(nrows, ncols), las=1)

  for(param in 1:numParams){
    par(mar=.MCMC_MARGINS)
    dat <- window(mcmc(as.ts(mcmcData[,param])), start = burnin, thin = thinning)
    dens <- density(dat)
    plot(dens, main = colnames(mcmcData)[param], ylab="")
    xx <- c(dens$x,rev(dens$x))
    yy <- c(rep(min(dens$y), length(dens$y)), rev(dens$y))
    shade <- .getShade(color, opacity)
    polygon(xx, yy, density = NA, col = shade)
  }
}

plotPairs <- function(mcmcData = NULL, burnthin = c(0,1)){
  # Pairs plots for an mcmc matrix, mcmcData

  oldPar <- par(no.readonly=TRUE)
  on.exit(par(oldPar))

  burnin       <- burnthin[1]
  thinning     <- burnthin[2]
  currFuncName <- getCurrFunc()
  if(is.null(mcmcData)){
    cat0(.PROJECT_NAME,"->",currFuncName,"mcmcData must be supplied.\n")
    return(NULL)
  }
  if(burnin > nrow(mcmcData)){
    cat0(.PROJECT_NAME,"->",currFuncName,"Burnin value exceeds mcmc chain length.\n")
    return(NULL)
  }

  panel.hist <- function(x, ...){
    usr    <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h      <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y      <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
  }

  numParams <- ncol(mcmcData)
  mcmcData <- window(as.matrix(mcmcData), start=burnin, thin=thinning)
  pairs(mcmcData, pch=".", upper.panel = panel.smooth, diag.panel = panel.hist, lower.panel = panel.smooth)
}

plotPriorsPosts <- function(mcmcData, inputs = NULL, burnthin = c(0,1), color = 1, opacity = 30){
  # Produce a grid of the parameters' posteriors with their priors overlaid.
	oldPar	<- par(no.readonly=T)
  on.exit(par(oldPar))

  burnin       <- burnthin[1]
  thinning     <- burnthin[2]
  currFuncName <- getCurrFunc()
  if(is.null(mcmcData)){
    cat0(.PROJECT_NAME,"->",currFuncName,"mcmcData must be supplied.\n")
    return(NULL)
  }
  if(is.null(inputs)){
    cat0(.PROJECT_NAME,"->",currFuncName,"inputs must be supplied so that the priors can be plotted over the posteriors.\n")
    return(NULL)
  }
  if(burnin > nrow(mcmcData)){
    cat0(.PROJECT_NAME,"->",currFuncName,"Burnin value exceeds mcmc chain length.\n")
    return(NULL)
  }
  # The values in the control file (inputs$control$param) for each of priorN are:
  # 1. ival  = initial value
  # 2. lb    = lower bound
  # 3. ub    = upper bound
  # 4. phz   = ADMB phase
  # 5. prior = prior distribution funnction
  #             0 = Uniform
  #             1 = normal    (p1=mu,p2=sig)
  #             2 = lognormal (p1=log(mu),p2=sig)
  #             3 = beta      (p1=alpha,p2=beta)
  #             4 = gamma     (p1=alpha,p2=beta)
  # 6. p1 (defined by 5 above)
  # 7. p2 (defined by 5 above)
  fNames <- c(dunif,dnorm,dlnorm,dbeta,dgamma)
  fNamesR <- c(runif,rnorm,rlnorm,rbeta,rgamma)

  numParams <- ncol(mcmcData)
  numQParams <- ncol(inputs$control$survq)
  qParams <- inputs$control$survq
  outputParamNames <- names(mcmcData)

  # Convert the priors in logspace into standard space
  paramSpecs <- convertLogParams(inputs$control$param)
  # Add in the q parameters
  for(q in 1:numQParams){
    # Add a row for each q to the paramSpecs matrix
    paramSpecs <- rbind(paramSpecs, c(NA, NA, NA, NA, qParams[1,q], exp(qParams[2,q]), exp(qParams[3,q])))
    rownames(paramSpecs)[nrow(paramSpecs)] <- paste0("q",q)
  }
  # First, figure out how many output parameters have associated priors and make the grid that size
  priorParamNames <- rownames(paramSpecs)
  # Add in the q parameters
  priorParamNames <- c(priorParamNames, paste0("q",1:numQParams))
  numWithPriors <- 0
  for(priorParam in 1:length(priorParamNames)){
    # For each prior that exists, match up the output paramater estimates
    pattern <- paste0("^",priorParamNames[priorParam],"_[[:alnum:]]+$")
    priorLoc <- grep(pattern, outputParamNames)
    if(length(priorLoc) == 0){
      pattern <- paste0("^",priorParamNames[priorParam],"$")
      priorLoc <- grep(pattern, outputParamNames)
    }
    if(length(priorLoc) > 0){
      numWithPriors <- numWithPriors + length(priorLoc)
    }
  }
  # Make a square grid of plots
  nrows <- ncols <- ceiling(sqrt(numWithPriors))
	par(mfrow=c(nrows, ncols), las=1)

  # Plot posteriors, then match up the input prior and plot
  for(param in 1:numParams){
    # Plot posterior density first
    par(mar=.MCMC_MARGINS)
    # Get posterior data
    dat <- window(mcmc(as.ts(mcmcData[,param])), start = burnin, thin = thinning)
    # Get rid of the trailing _ and group/area/sex numbers
    name <- sub("_.*","",outputParamNames[param])
    paramNames <- rownames(paramSpecs)
    # Match the name of the output posterior with its input parameter specifications
    row <- grep(name, paramNames)
    if(length(row) > 0){
      specs <- paramSpecs[row,]
      priorfn <- fNames[[specs[5]+1]] # +1 because the control prior starts at zero
      priorfnr <- fNamesR[[specs[5]+1]] # +1 because the control prior starts at zero
      xx <- list(p = dat, p1 = specs[6], p2 = specs[7], fn = priorfn, fnr = priorfnr, nm = outputParamNames[param])
      plot.marg(xx, breaks = "sturges", col = "wheat")
    }
  }
}

convertLogParams <- function(paramSpecs = NULL){
  # Take data frame 'paramSpecs' and change all the parameters within it that are labelled as "log_XXX" to
  # be in standard space, i.e. use the exp() command on all appropriate values.
  # Returns a data frame of the same size as 'dat', with all 'log_' removed from the parameter names

  inpParamNames <- rownames(paramSpecs)
  # Ugliness ensues here as the parameter names in the control file may not match those in the output.
  # Must use a series of special checks to match up estimated parameters with names that contain
  #  parameter names that had priors and ignore those that don't.
  pattern <- "^log_([[:alnum:]]+)$"
  logp <- grep(pattern, inpParamNames)
  logInpNames <- inpParamNames[logp]
  logInpNames <- sub(pattern, "\\1", logInpNames) # Now the log param names have no "log_" in front of them
  logParamSpecs <- paramSpecs[logp,]
  # Special cases.... yuck
  logInpNames <- sub("avgrec","rbar",logInpNames)
  logInpNames <- sub("recinit","rinit",logInpNames)

  nonLogInpNames <- inpParamNames[-logp]
  # Special case... yuck
  nonLogInpNames <- sub("steepness","h",nonLogInpNames)
  nonLogParamSpecs <- paramSpecs[-logp,]

  for(param in 1:length(logInpNames)){
    # exp the log functions before sending them off for plotting
    logParamSpecs[param, 1] <- exp(logParamSpecs[param,1])
    logParamSpecs[param, 2] <- exp(logParamSpecs[param,2])
    logParamSpecs[param, 3] <- exp(logParamSpecs[param,3])
    logParamSpecs[param, 6] <- exp(logParamSpecs[param,6])
    logParamSpecs[param, 7] <- exp(logParamSpecs[param,7])
  }
  rownames(logParamSpecs) <- logInpNames
  paramSpecs <- rbind(nonLogParamSpecs, logParamSpecs)
  # paramSpecs now holds the input values for all parameters in regular (non-log) space with
  # names to reflect this (log_) has been removed from the names of the log parameters
  return(paramSpecs)
}

plot.marg <- function(xx, breaks = "sturges", exFactor = 1.0, ...){
  # xx is a list(p=samples, p1=prior param 1, p2=prior param 2, fn=prior distribution)
  #  and ignore posterior distribution limits

  posteriorNoPlot <- hist(xx$p, breaks = breaks, plot=FALSE)
  xvals <- seq(min(posteriorNoPlot$breaks)/exFactor, max(posteriorNoPlot$breaks)*exFactor, length=250)

  pd <- xx$fn(xvals, xx$p1, xx$p2)
  z <- cbind(xvals, pd)
  xlim <- c(min(xvals), max(xvals))
  ss <- hist(xx$p, prob=TRUE, breaks = breaks, main = xx$nm, xlab="", cex.axis = 1.2, xlim = xlim, ylab = "", ...)
  lines(xvals, pd, col="green", lwd=2)
  #abline(v = xx$mle, lwd=2, lty=2, col=2)
}

plotVariancePartitions <- function(mcmcData, burnthin = c(0,1)){
  # Produce a grid of pairs plots for the variance parameters' posteriors
  # Assumes that there are an equal number of each rho and vartheta
  #  parameters, i.e. each group must have both rho and vartheta and
  #  they must be in the same column order in the matrix mcmcData.

	oldPar	<- par(no.readonly=T)
  on.exit(par(oldPar))

  val          <- getWinVal()
  burnin       <- val$burn
  thinning     <- val$thin
  currFuncName <- getCurrFunc()
  if(is.null(mcmcData)){
    cat0(.PROJECT_NAME,"->",currFuncName,"mcmcData must be supplied.\n")
    return(NULL)
  }
  # Create an empty parameter list and data frame for mcmc data and add each parameter to it
  rhoparams <- NULL
  vparams <- NULL

  # Get the names of all the output parameters for parsing
  names <- names(mcmcData)

  # Get rho parameters
  pattern <- "rho"
  rholoc <- grep(pattern, names)
  rhoparams <- c(rhoparams, names[rholoc])
  # Get vartheta parameters
  pattern <- "vartheta"
  varthetaloc <- grep(pattern, names)
  vparams <- c(vparams, names[varthetaloc])


  if(length(rhoparams) < 1 || length(vparams) < 1){
    cat0(.PROJECT_NAME,"->",currFuncName,"mcmcData must contain at least one of each variance parameter (rho and vartheta).\n")
    return(NULL)
  }
  rhodat <- window(mcmc(as.ts(mcmcData[rhoparams[1]])), start = burnin, thin = thinning)
  vdat   <- window(mcmc(as.ts(mcmcData[vparams[1]])), start = burnin, thin = thinning)
  sig    <- rhodat * vdat
  tau    <- (1 - rhodat) * vdat
  # Make names for sig and tau for this group
  # Get the group name from the first element of the rho names
  rhoname <- names[rholoc][1]
  gname <- sub("rho_([[:alnum:]]+)","\\1",rhoname)
  namevec <- c(paste0("sig_",gname), paste0("tau_",gname))
  d <- data.frame(rhodat, vdat)
  d[,namevec] <- c(sig, tau)
  # Now the first group's variance parameters are correctly loaded into the data.frame
  if(length(rhoparams) > 1){
    for(param in 2:length(rhoparams)){
      rhodat <- window(mcmc(as.ts(mcmcData[rhoparams[param]])), start = burnin, thin = thinning)
      vdat   <- window(mcmc(as.ts(mcmcData[vparams[param]])), start = burnin, thin = thinning)
      sig    <- rhodat * vdat
      tau    <- (1 - rhodat) * vdat
      # Make names for sig and tau for this group
      # Get the group name from the first element of the rho names
      rhoname <- names[rholoc][param]
      gname   <- sub("rho_([[:alnum:]]+)","\\1",rhoname)
      namevec <- c(rhoparams[param], vparams[parma], paste0("sig_",gname), paste0("tau_",gname))
      d[,namevec] <- c(rhodat, vdat, sig, tau)
    }
  }
  pairs(d, pch=".", upper.panel=NULL, gap=0)

}
