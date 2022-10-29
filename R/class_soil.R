#'#' R6 Class Representing a TPE soil
#'
#' @description
#' TODO
#'
#' @details
#' TODO
#' @import R6
#' @export
TPEsoil <- R6::R6Class("TPEsoil",
  public = list(
    #' @field name A character string identifier of the weather
    name = NULL,
    #' @field parent Parent environment
    parent = NULL,
    #' @field parameters Soil parameters
    parameters = NULL,
    #' @field soilParam Boolean indicating if soil parameters were extracted
    #' from HC27
    soilParam = FALSE,
    #' @field test Debug
    test = NA,

    #' @description Create a new TPE soil object
    #' @param name A character string identifier of the TPE soil
    #' @param parent Parent environment
    #' @return A new `TPEsoil` object.
    initialize = function(name="soil1", parent=NULL) {
      self$name <- as.character(name)
      parampath <- system.file("extdata", "soil.csv", package="CGMTPE")
      self$parameters <- data.frame(fread(parampath))
      self$parent <- parent
    },

    #' @description Set soil parameters for grid point location
    #' @param soil Dataframe with soil characteristics
    #' @param lat_lon Dataframe with correspondance of lat/lon with soil df
    set_soilParam = function(soil, lat_lon) {  #TODO: generalize
      gCode <- lat_lon[which(lat_lon$X == round(self$parent$lon,digits=2) &
                               lat_lon$Y == round(self$parent$lat,digits=2)),
                       "GRIDCODE"]
      if(length(gCode) > 0) {
        self$parameters[,c("stockinisurf","stockiniprof","epaisseursurf",
                           "epaisseurprof","humpf", "humsat","humfc")] <-
          c(soil[which(soil$GRIDCODE == gCode),
                 "stockinisurf"],
            soil[which(soil$GRIDCODE == gCode),
                 "stockiniprof"],
            soil[which(soil$GRIDCODE == gCode),
                 "epaisseursurf"],
            soil[which(soil$GRIDCODE == gCode),
                 "epaisseurprof"],
            soil[which(soil$GRIDCODE == gCode),
                 "humpf"],
            soil[which(soil$GRIDCODE == gCode),
                 "humsat"],
            soil[which(soil$GRIDCODE == gCode),
                 "humfc"])
        self$soilParam <- TRUE
      }
    }
  ),
  private = list()
)
