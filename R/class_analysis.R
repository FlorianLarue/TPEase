#'#' R6 Class Representing a CGMTPE analysis
#'
#' @description
#' TODO: change name
#' The CGMTPE Analysis object "analysis" is an environment containing all
#' objects used for estimation and TPE analysis
#' (varieties, environments, grids, maps, etc.)
#'
#' @details
#' TODO
#' @import R6
#' @export
CGMTPEa <- R6::R6Class("CGMTPEa",
  public = list(
    ## Attributes

    #' @field name A character string identifier of the CGMTPE analysis
    name = NULL,
    #' @field model A character string with the name of the crop model
    #' used for simulations (only "Samara" is supported for now)
    model = NULL,
    #' @field varieties A list of varieties objects
    varieties = list(),
    #' @field TPEanalysis A list of TPE analysis objects
    TPEanalysis = list(),
    #' @field test Debug
    test = NULL,


    ## Methods

    #' @description Create a new TPE analysis object
    #' @param name A character string identifier of the TPE analysis
    #' @param model A character string with the name of the crop model used
    #' for simulations
    #' @param varieties A list of varieties names
    #' @param genotypes Optional. A list of alternate variety names
    #' @param vparameters A vector or dataframe with variety specific parameters
    #' @return A new `CGMTPEa` object.
    initialize = function(name="CGMTPE_analysis", model="Samara", varieties=NA,
                          genotypes=NA, vparameters=NA) {

      self$name <- as.character(name)
      self$model <- as.character(model)

      vParamMissings <- c()

      if(length(varieties) > 1 || !is.na(varieties)) {
        private$varnames <- as.character(varieties)

        if(length(genotypes) != length(varieties) || is.na(genotypes)) {
          private$genotypes <- as.character(varieties)
          warning(paste("Length of genotypes does not match",
                        "length of varieties, name of varieties will be used."),
                  call.=F)
        } else {
          private$genotypes <- genotypes
        }

        for(i in 1:length(varieties)) {
          #TODO: change name of TPEvar class
          self$varieties <- append(self$varieties, TPEvar$new(
            name = as.character(private$varnames[i]),
            alt = as.character(private$genotypes[i]),
            parent = self))

          if(class(vparameters) == "data.frame" && nrow(vparameters) >= i) {
            self$varieties[[i]]$set_param(vparameters[i,])
          } else {
            vParamMissings <- c(vParamMissings, private$varnames[i])
          }
        }
      } else {
        stop("Please provide at least one variety name.")
      }

      self$initMessage(vParamMissings)
    },

    #' @description Confirm creation of TPE analysis object
    #' @param vParamMissings Name of varieties without parameters
    initMessage = function(vParamMissings) {
      if(length(vParamMissings) > 0) {
        warning(paste0("No parameters were provided for ",
                       ifelse(length(vParamMissings > 1), "varieties [", "variety ["),
                       paste0(vParamMissings, collapse=" "),
                       "]. Simulations will not be possible. ",
                       "You can set parameters by running set_param() on the ",
                       "variety object."), call.=F)
      }

      cat(paste0("CGMTPE analysis ", self$name, " created containing ",
                 length(private$varnames), ifelse(length(private$varnames) > 1,
                                                  " varieties "," variety "),
                 "\n"))
    },

    #' @description Set TPE analysis name
    #' @param val New TPE analysis name
    set_name = function(val) {
      self$name <- as.character(val)
    },

    #' @description Set model name
    #' @param val New model name
    set_model = function(val) {
      self$model <- as.character(val)
    },

    #' @description Set varieties, this is a list of TPEvar objects,
    #' use with caution
    #' @param val New varieties
    set_var = function(val) {
      if(depth(val) == 0) {
        if(class(val)[1] == "TPEvar") {
          self$varieties <- list(val)
        }
      } else if(depth(val) == 1) {
        if(sum(!(unlist(sapply(val, class)) %in% c("R6","TPEvar"))) == 0) {
          self$varieties <- val
        } else {
          stop(paste0("Error. Trying to set a non variety object into ",
                      self$name))
        }
      } else {
        stop("Depth of varieties can not exceed 1")
      }
    },

    #' @description Get names of all varieties
    get_varNames = function() {
      return(private$varnames)
    },

    #' @description Get names of all genotypes
    get_genotypes = function() {
      return(private$genotypes)
    },

    #' @description Get var id
    #' @param val Either var name or a var id (will return the var id)
    get_varid = function(val) {
      if(class(val) == "numeric") {
        if(val > length(self$get_varNames())) {
          stop(paste0("Variety identifier ", val, " not found."), call.=F)
        } else {
          id <- val
        }
      } else {
        if(!(val %in% self$get_varNames())) {
          stop(paste0("Variety identifier ", val, " not found."), call.=F)
        } else {
          id <- match(val, self$get_varNames())
        }
      }
      return(id)
    },

    #' @description Create a TPE analysis object
    #' @param name A character string of TPE analysis name
    createTPE = function(name = "TPEa_1") {
      self$TPEanalysis <- append(self$TPEanalysis,
                                 TPEa$new(name, self$model, self))
    },

    #' @description Create a TPE analysis object
    #' @param tpeID A TPE analysis identifier on which to create grid
    #' @param name A character string identifier of the grid
    #' @param varID A variety identifier to use for simulation on the grid
    #' @param latres A numeric value of the latitude resolution of the grid
    #' @param lonres A numeric value of the longitude resolution of the grid
    #' @param cols A numeric value of the number of columns in the grid
    #' @param rows A numeric value of the number of rows in the grid
    #' @param lon A numeric value of the starting longitude of the grid in
    #' decimal degrees
    #' @param lat A numeric value of the starting latitude of the grid in
    #' decimal degrees
    #' @param multigrid A boolean indicating if one grid should be created
    #' for each genotype with the same cols, rows, lon and lat. If true, varID
    #' will be ignored
    createGrid = function(tpeID = 1, name="g1", varID=NA, latres=0.35,
                          lonres=0.5, cols=5, rows=5, lon=NA, lat=NA,
                          multigrid=F) {
      self$TPEanalysis[[tpeID]]$createGrid(name, varID, latres, lonres, cols,
                                           rows, lon, lat, multigrid)
    },

    #' @description Generate climate data for one or several grids
    #' @param tpeID A TPE analysis identifier on which to create grid
    #' @param gridID Optional. A vector of grid identifiers
    #' (either index or name). By default will run all grids
    #' @param rcp A character string with the name of the Representative
    #' Concentration Pathway to use. One of the following options
    #' c("rcp26","rcp45","rcp60","rcp85")
    #' @param year A numeric value of the year to simulate climate
    #' (this can include years from 2013 to 2099)
    #' @param yearNb A numeric value of the number of years to simulate
    #' @param modelNb A character string of the  general circulation model
    #' identifier to use, see \code{generateClimate}
    #' @param path A character string with the path to the marksim standalone.
    #' For the moment, this path can not contain spaces
    #' @param pathCLI Optional. A character string with the path to CLI folder.
    #' If no value is provided, the CLI folder will be considered in the same
    #' folder as the marksim standalone. For the moment, this path can not
    #' contain spaces
    #' @param filesE Boolean. If weather files already exist
    #' @param verbose Boolean. If messages about starting climate generation
    #' should be shown
    genClimate = function(tpeID=1, gridID=NA, rcp="rcp26", year=2014, yearNb=1,
                          modelNb="00000000000000000", path=NA, pathCLI=NA,
                          filesE=F, verbose=T) {
      self$TPEanalysis[[tpeID]]$genClimate(gridID, rcp, year, yearNb, modelNb,
                                           path, pathCLI, filesE, verbose)
    },

    #' @description Run simulation on one or several grids
    #' @param tpeID A TPE analysis identifier on which to create grid
    #' @param gridID Optional. A vector of grid identifiers
    #' (either index or name). By default will run all grids
    #' @param varID A variety identifier to use for
    #' simulation on the grid. If NA, will use the variety attached to grid
    #' @param soilData Tmp for Adam et al.
    #' @param latlonData Tmp for Adam et al.
    #' @param traitList Optionnal. Vector of trait names to extract from
    #' simulations. This will delete the simulations and only keep the max for
    #' each year
    #' @param savePath Optional. A character string of the path where to save
    #' simulation files. If NULL (default), will not save simulations
    runGridSim = function(tpeID=1, gridID=NA, varID=NA, soilData=soil,
                          latlonData=lat_lon, traitList=NULL, savePath=NULL) {

      self$TPEanalysis[[tpeID]]$runGridSim(gridID, varID, soilData, latlonData,
                                           traitList, savePath)
    },

    #' @description Create raster map
    #' @param tpeID A TPE analysis identifier on which to create grid
    #' @param name A character string defining the name of the map
    #' @param bounds Optional. A vector of four numeric values as decimal degree
    #' of north, east, south, west bounds to crop map
    #' @param res Not used at the moment. A numeric value in sec of the
    #' resolution of world map to use. Options are c(1, 150, 900)
    #' for respectively 30sec, 2.5min, 15min
    createMap = function(tpeID=1, name="map1", bounds=NA, res=150) {
      self$TPEanalysis[[tpeID]]$createMap(name, bounds, res)
    }
  ),

  private = list(
    gridnames = NULL,
    varnames = NULL,
    genotypes = NULL,
    envnames = NULL,
    mapnames = NULL
  )
)
