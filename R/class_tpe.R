#'#' R6 Class Representing a TPE analysis
#'
#' @description
#' TODO
#'
#' @details
#' TODO
#' @import R6
TPEa <- R6::R6Class("TPEa",
  public = list(
    #' @field name Identifier of the TPE analysis
    name = NULL,
    #' @field model Name of the crop model used for simulations
    model = NULL,
    #' @field varieties List of varieties names used for TPE analysis
    varieties = c(),
    #' @field environments List of environment names used for TPE analysis
    environments = c(),
    #' @field genotypes List of alternate varieties names used for TPE analysis
    genotypes = c(),
    #' @field latStart Starting latitude for the grid (upper left corner)
    latStart = NULL,
    #' @field lonStart Starting longitude for the grid (upper left corner)
    lonStart = NULL,
    #' @field grid Simulation grid
    grid = NULL,

    #' @description Create a new TPE analysis object.
    #' @param name Identifier of the TPE analysis
    #' @param model Name of the crop model used for simulations
    #' @param varieties List of varieties
    #' @param environments List of environments
    #' @param latStart Starting latitude for grid
    #' @param lonStart Starting longitude for grid
    #' @param genotypes Optional. List of alternate variety names.
    #' @return A new `TPEa` object.
    initialize = function(name="TPEa_1", model="Samara", varieties=NA,
                          environments=NA, latStart=NA, lonStart=NA,
                          genotypes=NA) {
      self$name <- as.character(name)
      self$model <- as.character(model)
      if(length(varieties) > 1 || !is.na(varieties)) {
        self$varieties <- as.character(varieties)
      } else {
        stop("Please provide at least one variety name.")
      }
      if(length(environments) > 1 || !is.na(environments)) {
        self$environments <- environments
      } else {
        stop("Please provide at least one environment name.")
      }
      if(!is.na(genotypes)) {
        if(length(genotypes != length(varieties))) {
          stop("Length of genotypes does not match length of varieties.\n
               Either provide an alternate name for each variety or use
               genotypes=NA.")
        } else {
          self$genotypes <- genotypes
        }
      } else {
        self$genotypes <- self$varieties
      }
      self$initMessage()
    },

    #' @description Change crop model
    #' @param val New model name
    #' @examples
    #' TPE_analysis <- TPEa("TPEa1", "STICS")
    #' TPE_analysis$model
    #' TPE_analysis$set_model("Samara")
    #' TPE_analysis$model
    set_model = function(val) {
      self$model <- val
    },

    #' @description Confirm creation of TPE analysis object
    initMessage = function() {
      plV <- length(self$varieties) > 1
      plE <- length(self$environmets) > 1
      cat(paste0("TPE analysis ", self$name, " created containing ",
                 length(self$varieties), ifelse(plV," varieties "," variety "),
                 "and ", length(self$environments), ifelse(plE," environments",
                                                           " environment. \n")))
    },

    #' @description Create a simulation grid
    #' @param cols Number of columns in the grid
    #' @param rows Number of rows in the grid
    createGrid = function(cols=NA,rows=NA) {
      self$grid <- vector("list",cols)
      for(i in 1:length(self$grid)) {
        self$grid[[i]] <- vector("list",rows)
        for(j in 1:length(self$grid[[i]])) {
          self$grid[[i]][[j]] <- TPEgrid$new(name=paste0("c",i,j))
        }
      }
    },

    #' @description Generate climate data for each point of the grid
    #' @param res Resolution of grid (in km)
    #' @param rcp Rcp scenario to use
    #' @param year Year to simulate climate
    #' @param yearNb Number of years to simulate
    #' @param modelNb Identifier of model to use, see \code{generateClimate}
    #' @param path Path to marksim standalone
    #' @param pathCLI Optional. Path to CLI folder for marksim standalone
    genClim = function(res=5, rcp="rcp26", year=2015, yearNb=99,
                       modelNb="00000000000000000", path=NA, pathCLI=NA) {
      for(i in 1:length(self$grid)) {
        lat <- self$latStart + (i*(res/111))
        for(j in 1:length(self$grid[[i]])) {
          lon <- self$lonStart + ((j*(res/111)) * cos(lat))
          climate <- generateClimate(lon,lat,rcp,year,yearNb,
                                     modelNb,path,pathCLI)
          self$grid[[i]][[j]]$set_weather(climate)
        }
      }
    }
  ),
  private = list()
)
