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
    #' @field result Model simulation
    result = NULL,
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
        return(max(self$result[,val],na.rm=T))
      } else {
        return(NA)
      }
    },

    #' @description Generate climate
    #' @param rcp Rcp scenario to use
    #' @param year Year to simulate climate
    #' @param yearNb Number of years to simulate
    #' @param modelNb Identifier of model to use, see \code{generateClimate}
    #' @param path Path to marksim standalone
    #' @param pathCLI Optional. Path to CLI folder for marksim standalone
    genClimate = function(rcp, year, yearNb, modelNb, path, pathCLI) {
      self$weather$genClimate(rcp, year, yearNb, modelNb, path, pathCLI)
    },

    #' @description Run simulation on grid point
    #' @param param Parameter values
    #' @param i row position of grid point
    #' @param j col position of grid point
    #' @import stringr
    runSimulation = function(param, i, j) {
      if(!is.null(self$weather$wData)) {
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
                        "grid point", self$name,
                        "The current sowingdate from TPE analysis",
                        self$parent$parent$name, "will be used."),
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
        rsamara::init_sim_idx_simple(as.numeric(self$name), param,
                                     self$weather$simuWeather)
        res <- rsamara::run_sim_idx(as.numeric(self$name))
        self$result <- res
      } else {
        warning(paste("No weather data on grid point", self$name, ".",
                      "Simulation for this grid point will not be run."),
                call.=F)
      }
    }
  )
)
