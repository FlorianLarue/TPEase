# Collection of functions to perform model calibration (parameter estimation)

#' @title Generic function for parameter estimation over 1 to n environments
#' @description TODO
#' @import rsamara
#' @export
estim_param <- function(p, param, paramnames, weathers,
                        obs, score_fn, metric="RMSE", weigh_fn=NA, varID=1) {
  if(depth(obs) > 2) {
    obser <- obs
  } else {
    obser <- list(obs)
  }
  totalScore <- 0
  for(e in 1:length(weathers)) {
    envScore <- fitness_score(p, param, paramnames, weathers[[e]], obser[[e]],
                              score_fn, as.numeric(paste0(varID,e)),
                              metric, weigh_fn)
    totalScore <- totalScore + envScore
  }
  return(totalScore)
}


#' @title Fitness function for parameter estimation of Samara
#' @description Computes the fitness score of a given parameter set candidate
#' and according to a user-defined function (see examples)
#' @param p A vector of a candidate parameter set
#' @param param A dataframe with all Samara parameters
#' @param paramnames A vector containing estimated parameter names
#' @param weather A dataframe with all weather data
#' @param obs A dataframe or list of dataframes with all observation data
#' @param score_fn A user-defined function to compute the error between
#' @param idx An integer used as the simulation identifier
#' @return The fitness score for the candidate parameter set
#'
#' @export
#' @import rsamara
fitness_score <- function(p, param, paramnames, weather, obser, score_fn, idx,
                          metric="RMSE", weigh_fn=NA) {
  # Step 1 : initiate simulation
  if(rsamara::sim_exist_idx(idx) == 0) {
    print(paste("Simulation", idx, "Created"))
    rsamara::init_sim_idx_simple(idx, param, weather)
  }
  # Step 2 : update simulation with estimated parameters values
  rsamara::update_sim_idx(idx, p, paramnames)
  # Step 3 : run simulation
  sim <- rsamara::run_sim_idx(idx)
  # Step 4 : compute fitness
  fitScore <- 0
  if(class(obser) == "list") {
    for(i in length(obser)) {
      obs <- obser[[i]]
      if("leavesnumber" %in% colnames(obs)) {
        colnames(obs)[colnames(obs) == "leavesnumber"] <- "haunindex" #tmp fix
      }
      obs <- rsamara::rcpp_reduceVobs(obs, sim)
      sim <- rsamara::rcpp_reduceResults(sim, obs)
      viScore <- score_fn(obs,sim,metric,weigh_fn)
      fitScore <- fitScore + viScore
    }
  } else if(class(obs) == "data.frame") {
    if("leavesnumber" %in% colnames(obs)) {
      colnames(obs)[colnames(obs) == "leavesnumber"] <- "haunindex" #tmp fix
    }
    obs <- rsamara::rcpp_reduceVobs(obs, sim)
    sim <- rsamara::rcpp_reduceResults(sim, obs)
    fitScore <- score_fn(obs,sim,metric,weigh_fn)
  }
  return(fitScore)
}

#' @title Compute fitness score between observations and simulations
#' @description TODO
#' @param obs Dataframe of observations
#' @param sim Dataframe of simulations
#' @param metric Either a single value of the chosen metric to compute fitness
#' (RMSE, MAE) or a vector with a metric for each column of \code{obs}
#' @param weight_fn Optionnal. TODO
#' @examples get_score(obs, sim, metric, weight_fn)
#' @import rsamara
#' @export
get_score <- function(obs, sim, metric, weight_fn) {
  if(class(obs) != "data.frame" || class(sim) != "data.frame") {
    print("Please provide a dataframe for observations and simulations")
  } else {
    score <- MAE(obs,sim)
    return(score)
  }
}
