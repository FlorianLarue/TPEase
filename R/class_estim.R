#'#' R6 Class Representing a parameter estimation process
#'
#' @description
#' TODO
#'
#' @details
#' TODO
#' @import R6
#' @export
estimP <- R6::R6Class("estimP",
  public = list(
   #' @field name A character string identifier of estimation
   name = NULL,
   #' @field parent Parent variety
   parent = NULL,
   #' @field environments List of environments to use for estimation
   environments = list(),
   #' @field parameters A dataframe with all parameters (variety x environment)
   #' used for simulation, each line is one environment
   parameters = data.frame(),
   #' @field DEparams A DEoptim.control object with parameters used for DEoptim
   #' parameter estimation
   DEparams = NULL,
   #' @field DEresult A DEoptim result object
   DEresult = NULL,
   #' @field paramNames A vector of estimated parameter names
   paramNames = NULL,
   #' @field test Debug
   test = NA,

   #' @description Create a new TPE soil object
   #' @param name A character string identifier of estimation
   #' @param parent Parent variety
   #' @param environments List of environment names
   #' @param eparam Parameters of environment
   #' @param weathers Weather of environment
   #' @param observations Observations of environment
   #' @return A new `TPEsoil` object.
   initialize = function(name="estim1", parent, environments, eparam, weathers,
                         observations) {
     self$name <- as.character(name)
     self$parent <- parent
     if(length(environments) > 1 || !is.na(environments)) {
       for(i in 1:length(environments)) {

         self$createEnv(environments[i], paramE, weathers[[i]],
                        observations[[i]])
         param <- merge(self$parent$parameters, eparam[i,])
         param$X <- environments[i]
         self$parameters <- rbind(self$parameters, param)
       }
     }
   },

   #' @description Create environment
   #' @param name Name of environment
   #' @param eparam Parameters of environment
   #' @param weather Weather of environment
   #' @param observations Observations of environment
   createEnv = function(name, eparam, weather, observations) {
     self$environments <- append(self$environments, TPEenv$new(name, self,
                                                               eparam, weather,
                                                               observations))
   },

   #' @description Run parameter estimation
   #' @import DEoptim
   #' @description Run parameter estimation
   #' @param maxiter A numeric value of the maximum number of iteration
   #' for DEoptim
   #' @param paramnames A vector of parameter names to be estimated
   #' @param metric A character string with the name of the metric to use
   #' for fitness computation. Options are c("RMSE","MAE","MSE")
   #' @param score_fn A function to compute fitness, see \code{get_score}
   #' @param weigh_fn Not used for the moment
   #' @param bounds A matrix with lower (col1) and upper (col2) bounds for
   #' each of the parameters in `paramnames` (rows)
   #' @param args Additional parameters to be passed to DEoptim.control
   runEstimation = function(maxiter=2000, paramnames=NA,
                            metric="RMSE", score_fn=get_score, weigh_fn=NA,
                            bounds=NA, args) {
     if(length(self$environments) < 1) {
       stop(paste0("No environments were found for estimation ", self$name,
                   " of variety ", self$parent$name), call.=F)
     } else {
       #TODO: need to find a better way to be sure the idx of the simulaiton
       # is not already in use by another variety X environment
       idv <- self$parent$parent$get_varid(self$parent$name)
       self$paramNames <- tolower(paramnames)
       self$DEparams <- do.call(DEoptim.control, c(args,itermax=maxiter))
       estimResult <- DEoptim(private$fitness, lower=bounds[1,],
                              upper=bounds[2,], control=self$DEparams,
                              score_fn=score_fn, idv=idv, metric=metric)
       self$DEresult <- estimResult
       self$parent$update_param(estimResult$optim$bestmem, tolower(paramnames))
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

    #' @import DEoptim
    #' @import rsamara
    fitness = function(p, score_fn, idv, metric) {
      self$test <- self$test + 1
      if(length(self$environments) > 0) {
        sum_score <- 0
        for(i in 1:length(self$environments)) {
          #TODO: need to reactivate and find a solution to remove initiated
          # simulation to prevent problems with not initiating new variety on
          # same environments
          #if(rsamara::sim_exist_idx(i) == 0) {
            rsamara::init_sim_idx_simple(i, self$parameters[i,],
                                         self$environments[[i]]$weather)
            #print(paste0("Simulation ", i, " created"))
          #}

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
