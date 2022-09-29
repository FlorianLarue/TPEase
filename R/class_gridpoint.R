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
    set_dateParam = function(year) {  #TODO: generalize tmp fix for Adam et al
      self$weather$set_dateParam(year)
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

    #TODO: fix
    #' #' @description Get mean simulation result over
    #' #' @param traitList Vector of variable names to extract from simulation
    #' #' results
    #' get_result = function(traitList) {
    #'   resDf <- data.frame()
    #'   for(simu in 1:length(self$simulations)) {
    #'     tmpDf <- data.frame(year = self$weather$yearLevels[simu])
    #'     for(trait in traitList) {
    #'       tmpDf <- cbind(tmpDf, max(as.numeric(as.character(
    #'         self$simulations[[simu]][,trait])), na.rm=T))
    #'     }
    #'     resDf <- rbind(resDf, tmpDf)
    #'   }
    #'   resDf <- colMeans(resDf[,-c(1)])
    #'   colnames(resDf) <- traitList
    #'   return(resDf)
    #' },

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
    #' @param traitList Optionnal. Vector of trait names to extract from
    #' simulations. This will delete the simulations and only keep the max for
    #' each year
    #' @param savePath Optional. A character string of the path where to save
    #' simulation files. If NULL (default), will not save simulations
    #' @importFrom stringr str_split_fixed
    #' @importFrom matrixStats colMaxs
    #' @importFrom data.table fwrite
    runSimulation = function(param, i, j, soilData, latlonData, traitList,
                             savePath) {
      self$set_soilParam(soilData, latlonData)
      if(!is.null(self$weather$yearLevels)) {
        for(year in self$weather$yearLevels) {
          self$set_dateParam(year)
          if(!is.null(self$weather$dateParam)) {
            param$startingdate <- self$weather$dateParam[1]
            param$endingdate <- self$weather$dateParam[2]
            param$sowing <- self$weather$dateParam[3]
          } else {
            sy <- strsplit(rsamara::toStringDateCalcc(param$startingdate),
                           split="/")[[1]][[3]]
            ey <- strsplit(rsamara::toStringDateCalcc(param$endingdate),
                           split="/")[[1]][[3]]
            sm <- strsplit(rsamara::toStringDateCalcc(param$startingdate),
                           split="/")[[1]][[2]]
            em <- strsplit(rsamara::toStringDateCalcc(param$endingdate),
                           split="/")[[1]][[2]]
            simWeather <- self$weather$wData[which(stringr::str_split_fixed(
              self$weather$wData$weatherdate, "/",3)[,3] %in% c(sy,ey)),]
            simWeather <- simWeather[which(stringr::str_split_fixed(
              simWeather$weatherdate, "/",3)[,2] >= sm),]
            self$weather$simuWeather <- simWeather[which(stringr::str_split_fixed(
              simWeather$weatherdate, "/",3)[,2] <= em),]
            warning(paste("Sowing date was not extracted from weatherdata for",
                          "grid point", self$name, "for year", year,
                          "The default sowing date will be used"),
                    call.=F)
          }

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
          rsamara::init_sim_idx_simple(as.numeric(paste0(self$name, year)),
                                       param, self$weather$simuWeather)
          sim <- rsamara::run_sim_idx(as.numeric(paste0(self$name, year)))

          if(!is.null(savePath)) {
            fwrite(sim, paste0(savePath,"/sim_",self$name,"_",self$variety$name,
                               year,".csv"))
          }

          if(!is.null(traitList)) {
            self$result <- rbind(self$result, matrixStats::colMaxs(
              as.matrix(sim[,traitList])))
          } else {
            if(is.null(savePath)) {
              self$simulations <- append(self$simulations, list(sim))
            }
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
    }
  )
)
