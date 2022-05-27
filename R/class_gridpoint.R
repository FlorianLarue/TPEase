#'#' R6 Class Representing a simulation grid point
#'
#' @description
#' TODO
#'
#' @details
#' TODO
#' @import R6
gridPoint <- R6::R6Class("gridPoint",
  public = list(
    #' @field name Identifier of the grid point
    name = NULL,
    #' @field lon Longitude of the grid point
    lon = NULL,
    #' @field lat Latitude of the grid point
    lat = NULL,
    #' @field weather Samara climate data generated from marksim
    weather = NULL,
    #' @field simuweather Samara climate data for simulation of given year
    simuweather = NULL,
    #' @field result Model simulation
    result = NULL,
    #' @field soilParam soil parameters of grid point
    soilParam = NULL,
    #' @field dateParam sowing date of grid point
    dateParam = NULL,
    #' @field parent Grid parent of grid point
    parent = NULL,
    #' @field test Test attribute TODO: REMOVE
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
    },

    #' @description Change weather of grid point
    #' @param weather Weather dataframe
    set_weather = function(weather) {
      self$weather <- weather
    },

    #' @description Set soil parameters for grid point location
    set_soilParam = function() {  #TODO: generalize

    },

    #' @description Set soil parameters for grid point location
    #' @param year Year of simulation
    set_dateParam = function(year) {  #TODO: generalize
      if(!is.null(self$weather)) {
        self$simuweather <- self$weather[which(stringr::str_split_fixed(
          self$weather$weatherdate, "/",3)[,3] == year),]
        rainfall <- self$simuweather[149:nrow(self$simuweather),
                                     c("weatherdate", "rainfall")]
        # sowing date  using  criteria from Balme et al.
        for(i in 4:nrow(rainfall)) {
          stop <- FALSE
          if(sum(rainfall[seq(i-3,i-1),"rainfall"],na.rm=T) >= 20) {
            for(j in 2:23) {
              if(sum(rainfall[seq(i+j,i+j+7),"rainfall"],na.rm=T) == 0) {
                stop <- TRUE
                break
              }
            }
            if(stop == FALSE) {
              self$dateParam <- c(rsamara::toJulianDayCalcC(paste0(
                "01/01/", year), format="DMY",sep="/"),
                rsamara::toJulianDayCalcC(paste0("31/12/", year),
                                          format="DMY",sep="/"),
                rsamara::toJulianDayCalcC(self$simuweather[i,"weatherdate"],
                                          format="DMY",sep="/"))
              break
            }
          }
        }
        if(is.null(self$dateParam)) {
          print("No sowing date could be found.")
        }
      } else {
        stop(paste("Weather data for grid point", self$name, "is not set.
                   Please use genClimate() on grid", self$parent$name))
      }
    },

    #' @description Generate climate at grid point
    #' @param rcp Rcp scenario to use
    #' @param year Year to simulate climate
    #' @param yearNb Number of years to simulate
    #' @param modelNb Identifier of model to use, see \code{generateClimate}
    #' @param path Path to marksim standalone
    #' @param pathCLI Optional. Path to CLI folder for marksim standalone
    genClimate = function(rcp, year, yearNb, modelNb, path, pathCLI) {
      climate <- generateClimate(self$lon, self$lat, rcp, year, yearNb, modelNb,
                                 path,pathCLI)
      self$set_weather(climate)
    },

    #' @description Run simulation on grid point
    #' @param param Parameter values
    #' @import stringr
    runSimulation = function(param) {
      if(!is.null(self$dateParam)) {
        param$startingdate <- self$dateParam[1]
        param$endingdate <- self$dateParam[2]
        param$sowing <- self$dateParam[3]
      } else {
        sy <- strsplit(rsamara::toStringDateCalcc(param$startingdate),
                       split="/")[[1]][[3]]
        ey <- strsplit(rsamara::toStringDateCalcc(param$endingdate),
                       split="/")[[1]][[3]]
        sm <- strsplit(rsamara::toStringDateCalcc(param$startingdate),
                       split="/")[[1]][[2]]
        em <- strsplit(rsamara::toStringDateCalcc(param$endingdate),
                       split="/")[[1]][[2]]
        simWeather <- self$weather[which(stringr::str_split_fixed(
          self$weather$weatherdate, "/",3)[,3] %in% c(sy,ey)),]
        simWeather <- simWeather[which(stringr::str_split_fixed(
          simWeather$weatherdate, "/",3)[,2] >= sm),]
        self$simuweather <- simWeather[which(stringr::str_split_fixed(
          simWeather$weatherdate, "/",3)[,2] <= em),]
        warning("Sowing date was not extracted from weatherdate.
                The current sowingdate from TPEa will be used.")
      }
      rsamara::init_sim_idx_simple(as.numeric(self$name), param,
                                   self$simuweather)
      res <- rsamara::run_sim_idx(as.numeric(self$name))
      self$result <- res
    }
  )
)
