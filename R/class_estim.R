#' R6 Class representing a VarEstim Estimation process
#'
#' @description
#' Estimation object for a given variety containing all information about
#' crop model parameter estimation
#'
#' @usage
#' res <- TPEase$new(...)
#' res$varieties[[1]]$createEstim(name, self, environments, eparam, weathers,
#'                                observations)
#'
#' @details
#' The \code{VarEstim} object contains all information to perform parameter
#' estimation for a given \code{TPEaseVar} variety including calibration
#' environments
#'
#' @import R6
#' @export
VarEstim <- R6::R6Class("VarEstim",
  public = list(
    #' @field name The \code{VarEstim} identifier
    name = NULL,
    #' @field parent The \code{TPEaseVar} object parent of VarEstim
    parent = NULL,
    #' @field environments A \code{list} of \code{EstimEnv} objects to use for
    #' estimation
    environments = list(),
    #' @field parameters All parameters (variety x environment) of the crop
    #' model
    parameters = data.frame(),
    #' @field DEparams A \code{DEoptim.control} object with hyperparameters
    #' used for \code{DEoptim} parameter estimation
    DEparams = NULL,
    #' @field DEresult The \code{DEoptim} result
    DEresult = NULL,
    #' @field paramNames The names of parameters to be estimated
    paramNames = NULL,

    #' @description Create a new \code{VarEstim} object
    #' @param name A \code{character} string identifier of \code{VarEstim}
    #' @param environments A \code{vector} of environments names
    #' @param eparam A \code{data.frame} of \code{environments} parameters
    #' @param weathers A \code{data.frame} of \code{environments} weathers
    #' @param observations A \code{data.frame} of \code{environments}
    #' observations for the parent \code{TPEaseVar}
    #' @param parent A \code{TPEaseVar} parent object of \code{VarEstim}
    #' @return A new \code{VarEstim} object.
    initialize = function(name="estim1", environments, eparam, weathers,
                         observations, parent) {
     self$name <- as.character(name)
     self$parent <- parent
     if(length(environments) > 1) {
       for(i in 1:length(environments)) {
         if(!is.na(environments[i])) {
           self$createEnv(environments[i], eparam[i,], weathers[[i]],
                          observations[[i]])
           param <- merge(self$parent$parameters, eparam[i,])
           param$X <- environments[i]
           self$parameters <- rbind(self$parameters, param)
         }
       }
     }
    },

    #' @description Create a \code{EstimEnv} object
    #' @param name A \code{character} string identifier of the \code{EstimEnv}
    #' @param eparam A \code{data.frame} of environment parameters
    #' @param weather A \code{data.frame} of environment's weather data
    #' @param observations A \code{data.frame} of observations of the parent
    #' \code{TPEaseVar} in the \code{EstimEnv}
    createEnv = function(name, eparam, weather, observations) {
     self$environments <- append(self$environments,
                                 EstimEnv$new(name, self, eparam,
                                              weather, observations))
    },

    #' @description Run parameter estimation
    #' @param maxiter A numeric value of the maximum number of iteration
    #' for \code{DEoptim}
    #' @param paramnames A \code{vector} of parameter names to be estimated
    #' @param metric A \code{character} string with the name of the metric to
    #' use for fitness computation. Options are c("RMSE","MAE","MSE")
    #' @param score_fn A \code{function} to compute fitness,
    #' see \code{get_score}
    #' @param weigh_fn Not used for the moment
    #' TODO: implement weigh_fn
    #' @param bounds A \code{matrix} or \code{data.frame} with lower (col1)
    #' and upper (col2) bounds for each of the parameters in \code{paramnames}
    #' (rows)
    #' @param args Additional parameters to be passed to \code{DEoptim.control}
    #' @import DEoptim
    #' @import rsamara
    runEstimation = function(maxiter=2000, paramnames=NA,
                            metric="RMSE", score_fn=get_score, weigh_fn=NA,
                            bounds=NA, args) {
     if(length(self$environments) < 1) {
       stop(paste0("No environments were found for estimation ", self$name,
                   " of variety ", self$parent$name), call.=F)
     } else {
       idv <- self$parent$parent$get_varid(self$parent$name)
       self$paramNames <- tolower(paramnames)
       self$DEparams <- do.call(DEoptim.control, c(args,itermax=maxiter))
       estimResult <- DEoptim(private$fitness, lower=bounds[1,],
                              upper=bounds[2,], control=self$DEparams,
                              score_fn=score_fn, idv=idv, metric=metric)
       self$DEresult <- estimResult
       self$parent$updateParam(estimResult$optim$bestmem, tolower(paramnames))
       cat(paste0("Estimation ", self$name, " is done. ",
                  "The final fitness score is ",
                  estimResult$optim$bestval,
                  " and parameters have been updated on variety ",
                  self$parent$name))
     }
    }
  ),

  private = list(
    score_fn = NULL,

    fitness = function(p, score_fn, idv, metric) {
      self$test <- self$test + 1
      if(length(self$environments) > 0) {
        sum_score <- 0
        for(i in 1:length(self$environments)) {
          rsamara::init_sim_idx_simple(i, self$parameters[i,],
                                       self$environments[[i]]$weather)
          rsamara::update_sim_idx(i, p, self$paramNames)
          sim <- rsamara::run_sim_idx(i)
          obs <- rcpp_reduceVobs(self$environments[[i]]$observations, sim)
          res <- rcpp_reduceResults(sim, obs)
          col_score <- 0
          for(j in 1:ncol(obs)) {
            score <- score_fn(obs[,j], res[,j], metric)
            if(!is.nan(score)) {
              col_score <- col_score + score
            } else {
              col_score <- col_score + 999999
            }
          }
          sum_score <- sum_score + col_score
          self$test <- c(self$test, col_score)
        }
        return(sum_score/length(self$environments))
      } else {
        stop(paste0("There are no environments attached to estimation ",
                    self$name,
                    " you can add environments by running createEnv() ",
                    "on this estimation object"))
      }
    }
  )
)
