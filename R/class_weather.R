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
    #' @field wList A list of weather dataframes to use when multiple weathers
    #' are generated
    wList = list(),
    #' @field simuWeather A dataframe with weather data for simulation
    simuWeather = NULL,
    #' @field dateParam Booleaon indicating if date parameters were extracted
    #' from weather data
    dateParam = FALSE,
    #' @field yearLevels Years of generated weather data
    yearLevels = NULL,
    #' @field parent Parent environment
    parent = NULL,
    #' @field dateParamDF sowing date of grid point
    dateParamDF = data.frame(),
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
      if(!is.null(self$wData)) {
        yl <- unique(stringr::str_split_fixed(self$wData$weatherdate,
                                              "/",3)[,3])
        self$yearLevels <- yl
      }
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
      climate <- generateClimate(self$parent$lon, self$parent$lat, rcp, year,
                                 yearNb, modelNb, path, pathCLI, seed)
      self$set_weather(climate)
      if(bs) {
        self$wList <- append(self$wList, list(self$wData))
        self$yearLevels <- 99*length(self$wList)
      }
    },

    #' @description Set sowing date
    #' @param year Year of simulation
    #' @param cumulP Tmp fix for Adam et al.
    #' @param run Tmp fix for Adam et al.
    set_dateParam = function(year, cumulP, run) {
      #TODO: generalize tmp fix for Adam et al
      if(!is.null(self$wData)) {
        self$simuWeather <- self$wData[which(stringr::str_split_fixed(
          self$wData$weatherdate, "/",3)[,3] == year),]
        rainfall <- self$simuWeather[149:245,
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
              cP <- cumul[which(cumul$X1 == round(self$parent$lat,digits=2) &
                                   cumul$X2 == round(self$parent$lon,digits=2)),
                          "cumul_pluvio"]
              sowing <- rsamara::toJulianDayCalcC(rainfall[i,"weatherdate"],
                                                  format="DMY",sep="/")
              if(length(cP) > 0 && cP >= 800) {
                sowing <- sowing + 15
              }

              self$parent$cm$set_dateparam(c(
                             rsamara::toJulianDayCalcC(paste0("01/01/", year),
                                                       format="DMY", sep="/"),
                             rsamara::toJulianDayCalcC(paste0("31/12/", year),
                                                       format="DMY", sep="/"),
                             sowing))

              self$dateParam <- TRUE
              if(!is.na(run)) {
                tmpdf <- data.frame(year = year, run = run, sowing = sowing)
                self$dateParamDF <- rbind(self$dateParamDF, tmpdf)
              }
              break
            }
          }
        }

        if(!is.null(self$dateParam)) {
          self$simuWeather$weatherdate <- as.Date(as.character(
            self$simuWeather$weatherdate), format="%d/%m/%Y")
        }
      } else {
        self$simuWeather <- NULL
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
