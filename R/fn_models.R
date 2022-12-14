# Collection of functions related to crop models


#' @title Launch Crop Model simulation
#' @description Launch a simulation of one of the supported crop models using
#' input from dataframes/files for plant/soil parameters, and weather data
#' @param model One of the supported crop models (Samara or STICS)
#' @param data Either a path to the STICS workspace containing USM files
#'  or a list of dataframes with input data (parameters, weather) for Samara
#' @param params Additional parameters to tune the model (see examples)
#' @return A dataframe with the result of the simulation
#' @examples
#' param <- read.table("samara_parameters.csv")
#' weather <- read.table("samara_weather.csv")
#' resSamara <- launch_sim("Samara", param, weather)
#'
#' simOptions <- stics_wrapper_option(javastics_path = "path_to_javastics",
#' workspace_path = "data/stics/", target_path = "examples_stics/results/",
#' verbose = false)
#' resStics <- launch_sim("STICS", "data/stics/", simOptions)
#' @export
#' @import rsamara
launch_sim <- function(model="Samara", data, params) {
 #TODO
}

#' @title Construct Samara dataframes
#' @description Construct dataframes of parameters and weather data from files
#' of individual variety and itk parameters to run a Samara simulation
#' @param variety Name of the studied variety
#' @param genotype Corresponding genotype code of the variety
#' @param itk Name of the studied itk
#' @param simlist Table containing information about itk name, starting and
#' ending simulation date
#' @param workspace Path to the parent folder containing the subfolders obs,
#' params, meteos
#' @param mean TMP FIX If observations are means or blocks
#' @return A list with parameter, weather and observation data
#' @examples
#' simlist <- read.table("data/samara/simulation_list.csv")
#' dfList <- construct_data("621B","G7","BAMA2014S1",simlist,"examples_samara/")
#' param <- dfList[["parameters"]]
#' weather <- dfList[["weathers"]]
#' obs <- dfList[["observations"]]
#' @export
#' @import rsamara
construct_data <- function(variety,genotype,itk,simlist,workspace,mean=F) {
  varietyData <- list()
  startingDates <- simlist$startingdate
  endingDates <- simlist$endingdate
  ib <- which(simlist$itkcode == itk)

  # variety and itk parameters
  varietyParameters <- read.csv(paste0(workspace,"varieties/",variety,".csv"))
  varietyParameters <- as.data.frame(varietyParameters[1,])
  itkParameters <- read.csv(paste0(workspace,"params/", itk,".csv"))
  viParams <- merge(varietyParameters,itkParameters)
  viParams$stemporosity = 0.67
  viParams$startingdate = rsamara::toJulianDayCalcC(startingDates[ib],"DMY",'/')
  viParams$endingdate = rsamara::toJulianDayCalcC(endingDates[ib],"DMY",'/')
  varietyData[["parameters"]] <- viParams

  # weather data
  itkWeather <- read.csv(paste0(workspace,"meteos/", itk, ".csv"))
  itkWeather <- itkWeather[,-c(1)]
  varietyData[["weathers"]] <- itkWeather

  # observation data
  viObservations <- list()
  viSd <- list()
  if(!mean) {
    for(j in 1:length(blocks)) {
      block <- blocks[[j]]
      obs <- read.csv(paste0(workspace,"obs/", itk, genotype, block, ".csv"))
      #tmp error fix, in original script also error fix for missing grainpop
      #not sure it is useful at this point
      names(obs)[names(obs) == "grainyieldpopfin"] <- "grainyieldpop"
      obs$plantheight = 10*obs$plantheight
      viObservations[[j]] <- obs
      varietyData[["observations"]] <- viObservations
    }
  } else {
    obs <- read.csv(paste0(workspace,"obs_mean/", itk, genotype, ".csv"))
    names(obs)[names(obs) == "grainyieldpopfin"] <- "grainyieldpop"
    obs$plantheight = 10*obs$plantheight
    viObservations[[1]] <- obs

    sd <- read.csv(paste0(workspace,"obs_sd/", itk, genotype, ".csv"))
    names(sd)[names(sd) == "grainyieldpopfin"] <- "grainyieldpop"
    sd$plantheight = 10*sd$plantheight
    viSd[[1]] <- sd

    varietyData[["observations"]] <- viObservations
    varietyData[["sd"]] <- viSd
  }

  return(varietyData)
}
