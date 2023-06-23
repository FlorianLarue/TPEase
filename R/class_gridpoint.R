#'#' R6 Class representing a simulation grid point
#'
#' @description
#' Environment object of a \code{TPEGrid} containing all information about a
#' given environment used in the TPE analysis process
#'
#' @usage
#' \code{GridPoint} are automatically created when creating a \code{TPEGrid}
#'
#' @details
#' The \code{GridPoint} object is a subclass of \code{TPEaseEnv} specific for a
#' TPE analysis and containing all information of the Environment,
#' defined as a combination of : soil, weather and crop management data as well
#' as geographical location inside the \code{TPEGrid}
#'
#' @import R6
#' @export
GridPoint <- R6::R6Class("GridPoint",
  inherit = TPEaseEnv,
  public = list(
    #' @field lon The longitude of the grid point
    lon = NULL,
    #' @field lat The latitude of the grid point
    lat = NULL,
    #' @field result Mean simulation results
    result = NULL,
    #' @field simulations Raw simulation results
    simulations = list(),

    #' @description Create a new \code{GridPoint} object
    #' @param parent A \code{TPEGrid} parent object of the \code{GridPoint}
    #' @param name A \code{character} string identifier of the \code{GridPoint}
    #' @param lat A \code{numeric} value of the latitude of grid point
    #' (in decimal degrees)
    #' @param lon A \code{numeric} value of the longitude of grid point
    #' (in decimal degrees)
    #' @return A new \code{GridPoint} object.
    initialize = function(parent=NA, name="11", lon=-4.3, lat=11.18) {
      super$initialize(name, parent, NA, NULL)
      self$lon <- lon
      self$lat <- lat
      self$cm$set_latlon(c(self$lat,self$lon))
    },

    #' @description Set soil parameters for grid point location
    #' @param soil User input of soil parameters, either :
    #'
    #' * A \code{data.frame} with latitude (first column), longitude
    #' (second column) and corresponding soil parameters
    #' (starting from third column, column name should be the parameter name)
    #' for each grid point
    #' * A \code{function} taking latitude and longitude as arguments and
    #' returning a named \code{vector} of parameter values
    #'
    #' @md
    set_soilParam = function(soil) {
      if(class(soil) == "data.frame") {
        self$soil$set_soilParam(colnames(soil)[3:ncol(soil)],
                                soil[which(soil[,1] == self$lat &
                                             soil[,2] == self$lon),
                                     3:ncol(soil)])
      } else if(class(soil) == "function") {
        self$soil$set_soilParam(names(soil(self$lat, self$lon)),
                                soil(self$lat, self$lon))
      } else {
        cat("Soil data.frame or function was not recognized")
      }
    },

    #TODO: generalize
    #' @description Set soil parameters for grid point location
    #' @param year Year of simulation
    #' @param cumulP Tmp for Adam et al.
    #' @param run Tmp for Adam et al.
    set_dateParam = function(year, cumulP, run=NA) {
      self$weather$set_dateParam(year, cumulP, run)
    },

    #' @description Set variety
    #' @param val \code{TPEaseVar} object
    set_variety = function(val) {
      self$variety <- val
    },

    #' @description Set weather data
    #' @param val A \code{data.frame} of weather data
    set_weather = function(val) {
      self$weather$set_weather(val)
    },

    #' @description Get simulation results
    #' @param val A \code{character} string of the variable name to retrieve
    get_sim = function(val) {
      if(!is.null(self$result)) {
        return(mean(self$result[,val],na.rm=T))
      } else {
        return(NA)
      }
    },

    #' @description Get mean and standard deviation of simulation result
    #' @param traitList A \code{vector} of trait names
    #' @importFrom matrixStats colSds
    get_meansd = function(traitList) {
      resDf <- as.data.frame(self$result)
      resDf[nrow(resDf)+1,] <- colMeans(resDf)
      resDf[nrow(resDf)+1,] <- colSds(as.matrix(resDf[-c(nrow(resDf)),]))
      colnames(resDf) <- traitList
      rownames(resDf) <- NULL
      #TODO: tmp fix for dev, find another way to save sd
      #self$test <- resDf[nrow(resDf),]
      return(resDf[nrow(resDf)-1,])
    },

    #' @description Generate climate
    #' @param rcp A \code{character} string with the name of the Representative
    #' Concentration Pathway to use. One of the following options
    #' c("rcp26","rcp45","rcp60","rcp85")
    #' @param year A \code{numeric} value of the year to simulate climate
    #' (this can include years from 2013 to 2099)
    #' @param yearNb A \code{numeric} value of the number of years to simulate
    #' @param modelNb A \code{character} string of the  general circulation
    #' model identifier to use, see \code{generateClimate}
    #' @param path A \code{character} string with the path to the marksim
    #' standalone. This path can not contain spaces
    #' @param pathCLI Optional. A \code{character} string with the path to
    #' CLI folder. If no value is provided, the CLI folder will be considered
    #' in the same folder as the marksim standalone. This path can not
    #' contain spaces
    #' @param seed An \code{integer} number to use as seed for marksim weather
    #' generator
    #' @param bs A \code{boolean} indicating if should weathers be stored for
    #' bootstrap
    genClimate = function(rcp, year, yearNb, modelNb, path, pathCLI, seed,
                          bs=F) {
      self$weather$genClimate(rcp, year, yearNb, modelNb, path, pathCLI, seed,
                              bs)
    },

    #' @description Run simulation on \code{GridPoint}
    #' @param param A \code{data.frame} of all crop model parameter values
    #' @param i An \code{integer} value of the row position of \code{GridPoint}
    #' in the \code{TPEGrid}
    #' @param j An \code{integer} value of the col position of \code{GridPoint}
    #' in the \code{TPEGrid}
    #' @param soilData User input of soil parameters, either :
    #'
    #' * A \code{data.frame} with latitude (first column), longitude
    #' (second column) and corresponding soil parameters
    #' (starting from third column, column name should be the parameter name)
    #' for each grid point
    #' * A \code{function} taking latitude and longitude as arguments and
    #' returning a named \code{vector} of parameter values
    #'
    #' @param latlonData Tmp for Adam et al.
    #' @param cumulP Tmp for Adam et al.
    #' @param traitList Optionnal. A \code{vector} of trait names to extract
    #' from simulations. This will delete the simulations and only keep the max
    #' for each year
    #' @param savePath Optional. A \code{character} string of the path where to
    #' save simulation files. If \code{NULL} (default),
    #' will not save simulations
    #' @importFrom stringr str_split_fixed
    #' @importFrom matrixStats colMaxs
    #' @importFrom data.table fwrite
    #' @md
    runSimulation = function(param, i, j, soilData, latlonData, cumulP,
                             traitList, savePath) {

      self$set_soilParam(soilData)

      if(!is.null(self$weather$yearLevels)) {
        for(year in self$weather$yearLevels) {
          self$weather$dateParam <- FALSE
          self$set_dateParam(year, cumulP)
          if(self$weather$dateParam) {
            self$parameters <- merge(param, merge(self$soil$parameters,
                                                  self$cm$parameters))

            rsamara::init_sim_idx_simple(as.numeric(paste0(self$name, year)),
                                         self$parameters,
                                         self$weather$simuWeather)
            sim <- rsamara::run_sim_idx(as.numeric(paste0(self$name, year)))

            if(!is.null(savePath) && !is.na(sim)) {
              fwrite(sim, paste0(savePath,"/sim_",self$name,"_",
                                 self$variety$name, year,".csv"))
            } else if(is.null(traitList) && !is.na(sim)) {
              self$simulations <- append(self$simulations, list(sim))
            }

            if(!is.null(traitList) && !is.na(sim)) {
              self$result <- rbind(self$result, matrixStats::colMaxs(
                as.matrix(sim[,traitList]),na.rm=T))
            }

          } else {
            warning(paste("Sowing date was not extracted from weatherdata for",
                          "grid point", self$name, "for year", year,
                          "simulation will not be run for this environment."),
                    call.=F)
          }
        }

        if(!is.null(traitList)) {
          self$result <- self$get_meansd(traitList)
          self$parent$add_gridpointResult(cbind(data.frame(x=self$lon,
                                                           y=self$lat),
                                                self$result))
        }

        self$weather$simuWeather <- NULL
      } else if(!self$soil$soilParam) {
        warning(paste("Soil parameters were not extracted for",
                      "grid point", self$name,
                      "Simulation for this grid point will not be run."),
                call.=F)
      } else {
        warning(paste0("No weather data on grid point ", self$name, ".",
                      "Simulation for this grid point will not be run."),
                call.=F)
      }
    },

    #' @description Run simulation on \code{GridPoint} for bootstrap
    #' @param param A \code{data.frame} of all crop model parameter values
    #' @param soilData User input of soil parameters, either :
    #'
    #' * A \code{data.frame} with latitude (first column), longitude
    #' (second column) and corresponding soil parameters
    #' (starting from third column, column name should be the parameter name)
    #' for each grid point
    #' * A \code{function} taking latitude and longitude as arguments and
    #' returning a named \code{vector} of parameter values
    #'
    #' @param latlonData Tmp for Adam et al.
    #' @param cumulP Tmp for Adam et al.
    #' @param traitList Optionnal. A \code{vector} of trait names to extract
    #' from simulations. This will delete the simulations and only keep the max
    #' for each year
    #' @param startingYear An \code{integer} value of the starting year of
    #' bootstrap climate generation
    #' @param savePath Optional. A \code{character} string of the path where to
    #' save simulation files. If \code{NULL} (default),
    #' will not save simulations
    #' @importFrom stringr str_split_fixed
    #' @importFrom matrixStats colMaxs
    #' @importFrom data.table fwrite
    #' @md
    runBSSimulation = function(param, soilData, latlonData, cumulP, traitList,
                               startingYear, savePath) {

      self$set_soilParam(soilData)

      if(!is.null(self$weather$yearLevels)) {
        for(i in 1:self$weather$yearLevels) {
          run <- ((i - 1) %/% 99) + 1
          index <- i - (99 * (run-1))
          years <- startingYear:(startingYear+98)
          year <- years[index]

          self$weather$dateParam <- FALSE
          self$weather$wData <- self$weather$wList[[run]]
          self$set_dateParam(year, cumulP, run)

          if(self$weather$dateParam) {
            self$parameters <- merge(param, merge(self$soil$parameters,
                                                  self$cm$parameters))
            rsamara::init_sim_idx_simple(as.numeric(paste0(run, year)),
                                         self$parameters,
                                         self$weather$simuWeather)
            sim <- rsamara::run_sim_idx(as.numeric(paste0(run, year)))

            if(!is.null(savePath) && !is.na(sim)) {
              fwrite(sim, paste0(savePath,"/sim_",self$name,"_", param$variety,
                                 "_", run,"_",year,".csv"))
            } else if(is.null(traitList) && !is.na(sim)) {
              self$simulations <- append(self$simulations, list(sim))
            }

            if(!is.null(traitList) && !is.na(sim)) {
              tmpRes <- matrixStats::colMaxs(as.matrix(sim[,traitList]),na.rm=T)
              tmpRes <- as.data.frame(as.list(tmpRes))
              colnames(tmpRes) <- traitList
              tmpRes$year <- year
              tmpRes$run <- run
              self$result <- rbind(self$result, tmpRes)
            }

            self$weather$simuWeather <- NULL
          } else {
            warning(paste("Sowing date was not extracted from weatherdata for",
                          "grid point", self$name, "for year", year),
                    call.=F)
          }
        }
      } else if(!self$soil$soilParam) {
        warning(paste("Soil parameters were not extracted for",
                      "grid point", self$name,
                      "Simulation for this grid point will not be run."),
                call.=F)
      } else {
        warning(paste0("No weather data on grid point ", self$name, ".",
                       "Simulation for this grid point will not be run."),
                call.=F)
      }
    }
  )
)
