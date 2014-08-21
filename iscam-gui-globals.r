#*******************************************************************************
# iscam-gui-globals.r
# This file holds all global variable for the iscam-gui project.
# All the veriables declared here are hidden in the environment.
#*******************************************************************************

# Which operating system are you currently running?
.OS                              <- Sys.info()["sysname"]

# Project name
.PROJECT_NAME                    <- "iscam-gui"
.MAIN_FUNCTION_CALL              <- "iscam"

# Editor
.EDITOR                          <- file.path("C:","Progra~1","emacs-22.1","bin","runemacs.exe")
if(.OS == "Linux"){
  .EDITOR <- file.path("","usr","bin","emacs")
}
if(.OS == "Darwin"){
  # Set editor path for MAC
  .EDITOR <- file.path("","usr","bin","emacs")
}

# Directory names
.DATAFILE_DIR_NAME               <- paste0(.PROJECT_NAME,"-datafile-gui")
.SCENARIOS_DIR_NAME              <- file.path("Scenarios")
.SENS_FIGURES_DIR_NAME           <- file.path("SensitivityFigures")
.FIGURES_DIR_NAME                <- "Figures"
.TABLES_DIR_NAME                 <- "Tables"
# Retrospective directories
.RETRO_DIR_BASE                  <- "Retrospective"

# R source files
.UTILITIES_SOURCE                <- paste0(.PROJECT_NAME,"-utilities.r")
.LOAD_SCENARIOS_SOURCE           <- paste0(.PROJECT_NAME,"-load-scenarios.r")
.LOAD_BIODATA_SOURCE             <- paste0(.PROJECT_NAME,"-load-biodata.r")
.FILE_CONTROL_SOURCE             <- paste0(.PROJECT_NAME,"-file-control.r")
.REP_PARSER_SOURCE               <- paste0(.PROJECT_NAME,"-reptorlist.r")
.FIGURES_SOURCE                  <- paste0(.PROJECT_NAME,"-figures.r")
.FIGURES_BIOLOGY_SOURCE          <- paste0(.PROJECT_NAME,"-figures-biology.r")
.FIGURES_SELEX_SOURCE            <- paste0(.PROJECT_NAME,"-figures-selex.r")
.FIGURES_TIMESERIES_SOURCE       <- paste0(.PROJECT_NAME,"-figures-timeseries.r")
.FIGURES_CATCH_SOURCE            <- paste0(.PROJECT_NAME,"-figures-catch.r")
.FIGURES_MCMC_SOURCE             <- paste0(.PROJECT_NAME,"-figures-mcmc-diagnostics.r")
.FIGURES_RETROSPECTIVES_SOURCE   <- paste0(.PROJECT_NAME,"-figures-retrospectives.r")

# Plotting theme (ggplot). Only used in observed catch plot at this point.
.PLOT_THEME                      <- theme_bw(11)

# GUI definition files (see PBSModelling package)
.MAIN_GUI_DEF_FILE               <- paste0(.PROJECT_NAME,"-gui-specs.txt")

# iScam, lengthweight, vonB, and maturityage model executables location
.EXE_BASE_NAME                   <- "iscam"
.LW_EXE_BASE_NAME                <- "lengthweight"
.VONB_EXE_BASE_NAME              <- "vonb"
.MA_EXE_BASE_NAME                <- "maturityage"
if(.OS == "Linux" || .OS == "Darwin"){
  .EXE_FILE_NAME                 <- .EXE_BASE_NAME
  .LW_EXE_FILE_NAME              <- .LW_EXE_BASE_NAME
  .VONB_EXE_FILE_NAME            <- .VONB_EXE_BASE_NAME
  .MA_EXE_FILE_NAME              <- .MA_EXE_BASE_NAME
}else{
  .EXE_FILE_NAME                 <- paste0(.EXE_BASE_NAME,".exe")
  .LW_EXE_FILE_NAME              <- paste0(.LW_EXE_BASE_NAME,".exe")
  .VONB_EXE_FILE_NAME            <- paste0(.VONB_EXE_BASE_NAME,".exe")
  .MA_EXE_FILE_NAME              <- paste0(.MA_EXE_BASE_NAME,".exe")
}
.EXE_FILE_NAME_FULL_PATH         <- file.path("..","iSCAM","src","admb-code",.EXE_FILE_NAME)

.LAST_COMMAND_RUN_FILE_NAME      <- "lastCommandRun.rdata"  # Contains a list of the command line switches used in a run.
.STARTER_FILE_NAME               <- paste0(.EXE_BASE_NAME,".dat")
.REPORT_FILE_NAME                <- paste0(.EXE_BASE_NAME,".rep")
.PAR_FILE_NAME                   <- paste0(.EXE_BASE_NAME,".par")

# Biological data directory and file name(s)
.BIODATA_DIR_NAME                <- "Biodata"
.BIODATA_FILE_TYPES              <- list(c("rda"),c("rdata"))
.LW_DAT_FILE_NAME                <- paste0(.LW_EXE_BASE_NAME,".dat")
.LW_TPL_FILE_NAME                <- paste0(.LW_EXE_BASE_NAME,".tpl")
.LW_REP_FILE_NAME                <- paste0(.LW_EXE_BASE_NAME,".rep")
.VONB_DAT_FILE_NAME              <- paste0(.VONB_EXE_BASE_NAME,".dat")
.VONB_TPL_FILE_NAME              <- paste0(.VONB_EXE_BASE_NAME,".tpl")
.VONB_REP_FILE_NAME              <- paste0(.VONB_EXE_BASE_NAME,".rep")
.MA_DAT_FILE_NAME                <- paste0(.MA_EXE_BASE_NAME,".dat")
.MA_TPL_FILE_NAME                <- paste0(.MA_EXE_BASE_NAME,".tpl")
.MA_REP_FILE_NAME                <- paste0(.MA_EXE_BASE_NAME,".rep")

# Sensitivity file name (for grouping sensitivities together on plots)
.SCENARIO_INFO_FILE_NAME         <- "ScenarioInfo.txt"

# MCMC file names
.MCMC_FILE_NAME                  <- "iscam_mcmc.csv"
.MCMC_BIOMASS_FILE_NAME          <- "iscam_sbt_mcmc.csv"
.MCMC_RECRUITMENT_FILE_NAME      <- "iscam_rt_mcmc.csv"
.MCMC_FISHING_MORT_FILE_NAME     <- "iscam_ft_mcmc.csv"

# MCMC convergence plot margins
.MCMC_MARGINS                    <- c(2,4,2,2)

# GUI Header labels
.SCENARIO_LIST_LABEL             <- "Scenario List"
.SENSITIVITY_GROUP_LABEL         <- "Group"
.PLOT_COLOR_LABEL                <- "Color"
.PLOT_ORDER_LABEL                <- "Order"

# Messages
.TELL_USER_HOW_TO_START_GUI      <- "Type iscam() to start iscam-gui\n"
.TELL_USER_ABOUT_GUI_ARGUMENTS   <- paste0("Optional ",.PROJECT_NAME," arguments: ",.MAIN_FUNCTION_CALL,
                                           "(reloadScenarios = FALSE, silent = FALSE, copyModelExecutables = FALSE)\n\n")

# Model run command line outputs
.LOG_FILE_NAME                   <- "runoutput.log"  # This is the name of the logfile which holds all command line output.
.DOS_PIPE_STDOUT                 <- "1>"
.DOS_PIPE_STDERR                 <- "2>&1"
.DOS_APPEND_STDOUT               <- "1>>"
.DOS_APPEND_STDERR               <- "2>>&1"
.LINUX_APPEND_STDERR             <- "2>&1" # Linux/Mac requires this slightly different version for appending stderr
if(.OS == "Linux" || .OS == "Darwin"){
  .DOS_PIPE_TO_LOG                 <- paste(.DOS_PIPE_STDOUT, .LOG_FILE_NAME, .DOS_PIPE_STDERR)
  .DOS_APPEND_TO_LOG               <- paste(.DOS_APPEND_STDOUT, .LOG_FILE_NAME, .LINUX_APPEND_STDERR)
}else{
  .DOS_PIPE_TO_LOG                 <- paste(.DOS_PIPE_STDOUT, .LOG_FILE_NAME, .DOS_PIPE_STDERR)
  .DOS_APPEND_TO_LOG               <- paste(.DOS_APPEND_STDOUT, .LOG_FILE_NAME, .DOS_APPEND_STDERR)
}

# Output file list, used for cleaning of the directories
.OUTPUT_FILES                <- c(.LAST_COMMAND_RUN_FILE_NAME,
                                  .REPORT_FILE_NAME,
                                  .PAR_FILE_NAME,
                                  .LOG_FILE_NAME,
                                  paste0(.EXE_BASE_NAME,".b*"),
                                  paste0(.EXE_BASE_NAME,".p*"),    # matches .par file
                                  paste0(.EXE_BASE_NAME,".r*"),    # matches .rep file
                                  paste0(.EXE_BASE_NAME,".log"),
                                  "*.cov",
                                  "*.cor",
                                  "*.dep",
                                  "*.eva",
                                  "*.hes",
                                  "*.par",
                                  "*.re*",
                                  "*.std",
                                  "*.psv",
                                  "variance",
                                  "eigv.rpt",
                                  "fmin.log",
                                  "sims",
                                  # MCMC ouputs
                                  paste0(.EXE_BASE_NAME,".ecm"),
                                  paste0(.EXE_BASE_NAME,".hst"),
                                  paste0(.EXE_BASE_NAME,".mc*"),
                                  .MCMC_FILE_NAME,
                                  .MCMC_BIOMASS_FILE_NAME,
                                  .MCMC_RECRUITMENT_FILE_NAME,
                                  .MCMC_FISHING_MORT_FILE_NAME
                                  # Executable
                                  #.EXE_FILE_NAME  -- RF don't delete the executable!
                                  )
# Figure types
.PNG                             <- FALSE
.DEPLETION_FIGURE                <- 1

# Plotting
.RESOLUTION                      <- 300
.WIDTH                           <- 7
.HEIGHT                          <- 7
.UNITS                           <- "in"
.VERBOSE                         <- FALSE

# Retrospective colors, There are only 15 of them
.RETRO_COLORS                    <- c("black","blue","red","green","orange","salmon",
                                      "grey","aliceblue","chocolate","darkgreen",
                                      "cyan","darkorchid","gold1","goldenrod","deeppink")

# For the ScenarioInfo.txt file only:
.SENS_TEXT                       <- "# Sensitivity Group"
.DEFAULT_PLOT_COLOR              <- "1"
.PLOT_COLOR_TEXT                 <- "# Plotting color"
.DEFAULT_SENS_GROUP              <- "1"
.ORDER_TEXT                      <- "# Plotting order"
.DEFAULT_PLOT_ORDER              <- "1"

# Other globals
.FUNEVALS                        <- 1000
.PCHCODE                         <- 16
.BANNER                          <- "-----------------------------------------------------------------------------------\n"
.BANNER2                         <- "===================================================================================\n"

# Sneaky little variable, keep a global for if the plotting is currently on or not
.PLOT_IS_LIVE                    <- TRUE
