#'#' R6 Class Representing a TPE weather
#'
#' @description
#' TODO
#'
#' @details
#' TODO
#' @import R6
#' @export
TPEweather <- R6::R6Class("TPEweather",
  public = list(
    #' @field name A character string identifier of the weather
    name = NULL,
    #' @field wData A dataframe with weather data
    wData = NULL,
    #' @field simuWeather A dataframe with weather data for simulation
    simuWeather = NULL,
    #' @field dateParam sowing date of grid point
    dateParam = NULL,
    #' @field parent Parent environment
    parent = NULL,
    #' @field test Debug
    test = NA,

    #' @description Create a new TPE weather object
    #' @param name A character string identifier of the TPE weather
    #' @param parent Parent environment
    #' @return A new `TPEweather` object.
    initialize = function(name="0101", parent=NULL) {
      self$name <- as.character(name)
      self$parent <- parent
    },

    #' @description Set weather data
    #' @param val A dataframe of weather data
    set_weather = function(val) {
      self$wData <- val
    },

    #' @description Generate climate
    #' @param rcp Rcp scenario to use
    #' @param year Year to simulate climate
    #' @param yearNb Number of years to simulate
    #' @param modelNb Identifier of model to use, see \code{generateClimate}
    #' @param path Path to marksim standalone
    #' @param pathCLI Optional. Path to CLI folder for marksim standalone
    genClimate = function(rcp, year, yearNb, modelNb, path, pathCLI) {
      climate <- generateClimate(self$parent$lon, self$parent$lat, rcp, year,
                                 yearNb, modelNb, path, pathCLI)
      self$set_weather(climate)
    },

    #' @description Set sowing date
    #' @param year Year of simulation
    set_dateParam = function(year) {  #TODO: generalize tmp fix for Adam et al
      if(!is.null(self$wData)) {
        self$simuWeather <- self$wData[which(stringr::str_split_fixed(
          self$wData$weatherdate, "/",3)[,3] == year),]
        rainfall <- self$simuWeather[149:nrow(self$simuWeather),
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
          self$simuWeather$weatherdate <- as.Date(as.character(
            self$simuWeather$weatherdate), format="%d/%m/%Y")
        }
      } else {
        warning(paste("Weather data for grid point", self$name,
                      "is not set. Please use genClimate() on grid",
                      self$parent$parent$name, "if this was forgotten.",
                      "Otherwise you can ignore this message."), call.=F)
      }
    }
  ),

  private = list(
  )
)
