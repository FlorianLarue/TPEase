#'#' R6 Class Representing a simulation grid point
#'
#' @description
#' TODO
#'
#' @details
#' TODO
#' @import R6
#' @export
gridPoint <- R6::R6Class("gridPoint",
  public = list(
    #' @field name Identifier of the grid point
    name = NULL,
    #' @field lon Longitude of the grid point
    lon = NULL,
    #' @field lat Latitude of the grid point
    lat = NULL,
    #' @field weather A weather object
    weather = NULL,
    #' @field soil A soil object
    soil = NULL,
    #' @field result Mean results
    result = NULL,
    #' @field simulations Model simulations
    simulations = list(),
    #' @field parent Grid parent of grid point
    parent = NULL,
    #' @field variety Optional. Specific variety for given gridPoint
    variety = NULL,
    #' @field test Test attribute
    test = NULL,
    #' @field param Test attribute
    param = NULL,

    #' @description Create a new TPE grid point object
    #' @param parent Grid parent of grid point
    #' @param name Identifier of the TPE analysis
    #' @param lat Latitude of grid point
    #' @param lon Longitude of grid point
    #' @return A new `TPEgrid` object.
    initialize = function(parent=NA, name="11", lon=-4.3, lat=11.18) {
      self$parent <- parent
      self$name <- name
      self$lon <- lon
      self$lat <- lat
      self$weather <- TPEweather$new(self$name, self)
      self$soil <- TPEsoil$new(self$name, self)
    },

    #' @description Set soil parameters for grid point location
    #' @param soil Dataframe with soil characteristics
    #' @param lat_lon Dataframe with correspondance of lat/lon with soil df
    set_soilParam = function(soil, lat_lon) {  #TODO: generalize
      self$soil$set_soilParam(soil, lat_lon)
    },

    #' @description Set soil parameters for grid point location
    #' @param year Year of simulation
    #' @param cumulP Tmp for Adam et al.
    #' @param run Tmp for Adam et al.
    set_dateParam = function(year, cumulP, run) {  #TODO: generalize tmp fix for Adam et al
      self$weather$set_dateParam(year, cumulP, run)
    },

    #' @description Set variety to this gridPoint
    #' @param val Object of type variety
    set_variety = function(val) {
      self$variety <- val
    },

    #' @description Set weather data
    #' @param val A dataframe of weather data
    set_weather = function(val) {
      self$weather$set_weather(val)
    },

    #' @description Get simulation data
    #' @param val Variable name to retrieve
    get_sim = function(val) {
      if(!is.null(self$result)) {
        return(mean(self$result[,val],na.rm=T))
      } else {
        return(NA)
      }
    },

    #' @description Get mean and sd of simulation result
    #' @param traitList Vector of trait names
    #' @importFrom matrixStats colSds
    get_meansd = function(traitList) {
      resDf <- as.data.frame(self$result)
      resDf[nrow(resDf)+1,] <- colMeans(resDf)
      resDf[nrow(resDf)+1,] <- colSds(as.matrix(resDf[-c(nrow(resDf)),]))
      colnames(resDf) <- traitList
      rownames(resDf) <- NULL
      #TODO: tmp fix for dev, find another way to save sd
      self$test <- resDf[nrow(resDf),]
      return(resDf[nrow(resDf)-1,])
    },

    #' @description Generate climate
    #' @param rcp Rcp scenario to use
    #' @param year Year to simulate climate
    #' @param yearNb Number of years to simulate
    #' @param modelNb Identifier of model to use, see \code{generateClimate}
    #' @param path Path to marksim standalone
    #' @param pathCLI Optional. Path to CLI folder for marksim standalone
    #' @param seed Integer number to use as seed for marksim weather generator
    #' @param bs Boolean. Should weathers be stored for bootstrap
    genClimate = function(rcp, year, yearNb, modelNb, path, pathCLI, seed,
                          bs=F) {
      self$weather$genClimate(rcp, year, yearNb, modelNb, path, pathCLI, seed,
                              bs)
    },

    #' @description Run simulation on grid point
    #' @param param Parameter values
    #' @param i row position of grid point
    #' @param j col position of grid point
    #' @param soilData Tmp for Adam et al.
    #' @param latlonData Tmp for Adam et al.
    #' @param cumulP Tmp for Adam et al.
    #' @param traitList Optionnal. Vector of trait names to extract from
    #' simulations. This will delete the simulations and only keep the max for
    #' each year
    #' @param savePath Optional. A character string of the path where to save
    #' simulation files. If NULL (default), will not save simulations
    #' @importFrom stringr str_split_fixed
    #' @importFrom matrixStats colMaxs
    #' @importFrom data.table fwrite
    runSimulation = function(param, i, j, soilData, latlonData, cumulP,
                             traitList, savePath) {
      self$set_soilParam(soilData, latlonData)
      if(!is.null(self$weather$yearLevels)) {
        for(year in self$weather$yearLevels) {
          if(!is.null(self$soil$soilParam)) {
            param$stockinisurf <- self$soil$soilParam[1]
            param$stockiniprof <- self$soil$soilParam[2]
            param$epaisseursurf <- self$soil$soilParam[3]
            param$epaisseurprof <- self$soil$soilParam[4]
            param$humpf <- self$soil$soilParam[5]
            param$humsat <- self$soil$soilParam[6]
            param$humfc <- self$soil$soilParam[7]
          } else {
            warning(paste("Soil parameters were not extracted from HC27 for",
                          "grid point", self$name,
                          "The current soil parameters from TPE analysis",
                          self$parent$parent$name, "will be used."),
                    call.=F)
          }
          param$wslong <- self$lon
          param$wslat <- self$lat

          self$weather$dateParam <- NULL
          self$set_dateParam(year, cumulP)
          if(!is.null(self$weather$dateParam)) {
            param$startingdate <- self$weather$dateParam[1]
            param$endingdate <- self$weather$dateParam[2]
            param$sowing <- self$weather$dateParam[3]
            self$param <- param
            rsamara::init_sim_idx_simple(as.numeric(paste0(self$name, year)),
                                         self$param, self$weather$simuWeather)
            sim <- rsamara::run_sim_idx(as.numeric(paste0(self$name, year)))

            if(!is.null(savePath)) {
              fwrite(sim, paste0(savePath,"/sim_",self$name,"_",self$variety$name,
                                 year,".csv"))
            } else if(is.null(traitList)) {
              self$simulations <- append(self$simulations, list(sim))
            }

            if(!is.null(traitList)) {
              self$result <- rbind(self$result, matrixStats::colMaxs(
                as.matrix(sim[,traitList])))
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
        }
        self$weather$simuWeather <- NULL
      } else {
        warning(paste0("No weather data on grid point ", self$name, ".",
                      "Simulation for this grid point will not be run."),
                call.=F)
      }
    },

    #' @description Run simulation on grid point
    #' @param param Parameter values
    #' @param soilData Tmp for Adam et al.
    #' @param latlonData Tmp for Adam et al.
    #' @param cumulP Tmp for Adam et al.
    #' @param traitList Optionnal. Vector of trait names to extract from
    #' simulations. This will delete the simulations and only keep the max for
    #' each year
    #' @param startingYear Starting year of bootstrap climate gen
    #' @param savePath Optional. A character string of the path where to save
    #' simulation files. If NULL (default), will not save simulations
    #' @importFrom stringr str_split_fixed
    #' @importFrom matrixStats colMaxs
    #' @importFrom data.table fwrite
    runBSSimulation = function(param, soilData, latlonData, cumulP, traitList,
                               startingYear, savePath) {
      self$set_soilParam(soilData, latlonData)
      for(i in 1:self$weather$yearLevels) {
        run <- ((i - 1) %/% 99) + 1
        index <- i - (99 * (run-1))
        years <- startingYear:(startingYear+98)
        year <- years[index]
        if(!is.null(self$soil$soilParam)) {
          param$stockinisurf <- self$soil$soilParam[1]
          param$stockiniprof <- self$soil$soilParam[2]
          param$epaisseursurf <- self$soil$soilParam[3]
          param$epaisseurprof <- self$soil$soilParam[4]
          param$humpf <- self$soil$soilParam[5]
          param$humsat <- self$soil$soilParam[6]
          param$humfc <- self$soil$soilParam[7]
        }
        param$wslong <- self$lon
        param$wslat <- self$lat

        self$weather$dateParam <- NULL
        self$weather$wData <- self$weather$wList[[run]]
        self$set_dateParam(year, cumulP, run)
        if(!is.null(self$weather$dateParam)) {
          param$startingdate <- self$weather$dateParam[1]
          param$endingdate <- self$weather$dateParam[2]
          param$sowing <- self$weather$dateParam[3]

          self$param <- param
          rsamara::init_sim_idx_simple(as.numeric(paste0(run, year)),
                                       self$param, self$weather$simuWeather)
          sim <- rsamara::run_sim_idx(as.numeric(paste0(run, year)))

          if(!is.null(savePath) && !is.na(sim)) {
            fwrite(sim, paste0(savePath,"/sim_",self$name,"_", param$variety, "_",
                               run,"_",year,".csv"))
          }

          if(!is.null(traitList) && !is.na(sim)) {
            tmpRes <- matrixStats::colMaxs(as.matrix(sim[,traitList]))
            tmpRes <- as.data.frame(as.list(tmpRes))
            colnames(tmpRes) <- traitList
            tmpRes$year <- year
            tmpRes$run <- run
            self$result <- rbind(self$result, tmpRes)
          }
        } else {
          warning(paste("Sowing date was not extracted from weatherdata for",
                        "grid point", self$name, "for year", year),
                  call.=F)
        }
      }
    }
  )
)
