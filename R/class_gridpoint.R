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
    #' @param soil Dataframe with soil characteristics
    #' @param lat_lon Dataframe with correspondance of lat/lon with soil df
    set_soilParam = function(soil, lat_lon) {  #TODO: generalize

      gCode <- lat_lon[which(lat_lon$X == round(self$lon,digits=2) &
                                lat_lon$Y == round(self$lat,digits=2)),
                                "GRIDCODE"]

      self$soilParam <- c(soil[which(soil$GRIDCODE == gCode),"stockinisurf"],
                          soil[which(soil$GRIDCODE == gCode),"stockiniprof"],
                          soil[which(soil$GRIDCODE == gCode),"epaisseursurf"],
                          soil[which(soil$GRIDCODE == gCode),"epaisseurprof"],
                          soil[which(soil$GRIDCODE == gCode),"humpf"],
                          soil[which(soil$GRIDCODE == gCode),"humsat"],
                          soil[which(soil$GRIDCODE == gCode),"humfc"])
    },

    #' @description Set soil parameters for grid point location
    #' @param year Year of simulation
    set_dateParam = function(year) {  #TODO: generalize tmp fix for Adam et al
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
                rsamara::toJulianDayCalcC(rainfall[i,"weatherdate"],
                                          format="DMY",sep="/"))
              break
            }
          }
        }
        if(is.null(self$dateParam)) {
          warning(paste("No sowing date could be found for gridpoint",
                      self$name, "default sowing date will be used."),
                  call.=F)
        } else {
          self$simuweather$weatherdate <- as.Date(as.character(
            self$simuweather$weatherdate), format="%d/%m/%Y")
        }
      } else {
        warning(paste("Weather data for grid point", self$name,
        "is not set. Please use genClimate() on grid", self$parent$name,
        "if this was forgotten. Otherwise you can ignore this message."),
        call.=F)
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
                                 path, pathCLI)
      self$set_weather(climate)
    },

    #' @description Run simulation on grid point
    #' @param param Parameter values
    #' @param trait Trait name for matrix grid res
    #' @param i row position of grid point
    #' @param j col position of grid point
    #' @import stringr
    runSimulation = function(param, trait, i, j) {
      if(!is.null(self$weather)) {
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
          warning(paste("Sowing date was not extracted from weatherdata for",
                        "grid point", self$name,
                        "The current sowingdate from TPE analysis",
                        self$parent$parent$name, "will be used."),
                  call.=F)
        }

        if(!is.null(self$soilParam)) {
          param$stockinisurf <- self$soilParam[1]
          param$stockiniprof <- self$soilParam[2]
          param$epaisseursurf <- self$soilParam[3]
          param$epaisseurprof <- self$soilParam[4]
          param$humpf <- self$soilParam[5]
          param$humsat <- self$soilParam[6]
          param$humfc <- self$soilParam[7]
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
                                     self$simuweather)
        res <- rsamara::run_sim_idx(as.numeric(self$name))
        self$result <- res
        val <- max(self$result[,trait],na.rm=T)
        self$parent$set_resGrid(i,j,val)
      } else {
        self$parent$set_resGrid(i,j,NA)
        warning(paste("No weather data on grid point", self$name, ".",
                      "Simulation for this grid point will not be run."),
                call.=F)
      }
    }

  )
)
