# Sample script to perform parameter estimation with Samara

#' @title Sample parameter estimation for Samara
#' @description Simple estimation sample of one variety in one location using
#' a custom score function. Eight parameters are estimated considering
#' phenotypic data of 4 replicates. The estimation is performed using DEoptim,
#' see more details in \insertRef{deoptim}{CGMTPE}
#' For testing purpose the number of iteration is set to 20, but should be
#' higher in real situations. The size of the population is set by default
#' to be 10 times the number of estimated parameters.
#' See more info about convergence in \insertRef{hu2013}{CGMTPE}.
#' This sample script does not use parallelisation
#' @param maxiter Maximal number of iterations, default to 20 (see above)
#' @param seed Random seed value
#' @examples sample_estimation(20,1337, get_score)
#' @import DEoptim
#' @import rsamara
#' @import Rcpp
#' @import hash
#' @export
sample_estimation <- function(maxiter=200, seed=2204, score_fn=get_score) {
  set.seed(seed)
  blocks <<- c("I","II","III","IV") #name of blocks used in obs file names
  estimInfo <- read.csv("data/samara/simulation_list.csv")
  paramInfo <- read.csv("data/samara/estimation_params.csv")
  lowerBounds <- as.numeric(paramInfo[which(paramInfo$Value == "MinValue"),
                                       -c(1,2)])
  upperBounds <- as.numeric(paramInfo[which(paramInfo$Value == "MaxValue"),
                                       -c(1,2)])
  paramBounds <- matrix(c(lowerBounds,upperBounds),ncol=2)
  paramOfInterest <- tolower(colnames(paramInfo)[-c(1,2)])

  # Step 1: construct necessary dataframes (see ?construct_data)
  varietyData <- construct_data(estimInfo$variety,estimInfo$genotype,
                                estimInfo$itkcode, estimInfo, "data/samara/")
  weather <- varietyData[["weathers"]]
  param <- varietyData[["parameters"]]
  obs <- varietyData[["observations"]]


  # Step 2: run estimation algorithm
  DEParams <- DEoptim.control(itermax=maxiter,strategy=2,trace=1,
                              NP=10*length(paramOfInterest))
  result <- DEoptim(fitness_score, lower=paramBounds[,1], upper=paramBounds[,2],
                    control=DEParams,
                    param, paramOfInterest, weather, obs, get_score, 1)
  return(result)
}

#' @title Run and plot simulation with estimated parameters
#' @description TODO
#' @param param Dataframe of all parameters used in simulation
#' (variety and environment)
#' @param weather Dataframe with weather data
#' @param estim Optionnal, vector with value of estimated parameters
#' @param paramnames Optionnal, vector with names of estimated parameters
#' @examples check_results(param, weather, estim ,paramnames)
#' @import rsamara
#' @export
check_results <- function(param, weather, estim=NA, paramnames=NA) {
  rsamara::init_sim_idx_simple(1, param, weather)
  if(!is.na(estim)) {
    if(is.na(paramnames)) {
      print("Please provide the list of estimated parameter names")
    } else {
      rsamara::update_sim_idx(1, estim, paramnames)
    }
  }
  sim <- rsamara::run_sim_idx(1)
  #TODO : add plots
}
