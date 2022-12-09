#'#' R6 Class Representing a TPE analysis
#'
#' @description
#' The TPE Analysis object "TPEa" is an environment containing all objects
#' used for a TPE analysis (grid, maps, etc.)
#'
#' @details
#' TODO
#' @import R6
#' @export
TPEa <- R6::R6Class("TPEa",
  public = list(
    #' @field name A character string identifier of the TPE analysis
    name = NULL,
    #' @field model A character string with the name of the crop model
    #' used for simulations (only "Samara" is supported for now)
    model = NULL,
    #' @field grids A vector of simulation grids
    grids = list(),
    #' @field results A dataframe with grid simulation results
    results = NULL,
    #' @field maps A list of raster maps for each of the `grids`
    maps = list(),
    #' @field bootstrapGP A list of gridpoints to perform bootstrap
    bootstrapGP = list(),
    #' @field test Debug
    test = NULL,

    #' @description Create a new TPE analysis object
    #' @param name A character string identifier of the TPE analysis
    #' @param model A character string with the name of the crop model used
    #' for simulations
    #' @param parent Parent CGMTPE analysis object
    #' @return A new `TPEa` object.
    initialize = function(name="TPEa_1", model="Samara", parent=NULL) {

      self$name <- as.character(name)
      self$model <- as.character(model)

      private$parent <- parent

      self$initMessage()
    },

    #' @description Confirm creation of TPE analysis object
    initMessage = function() {
      cat(paste0("TPE analysis ", self$name, " created \n"))
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

    #' @description Set grids, this is a list of grid objects,
    #' use with caution this will overwrite existing grids
    #' @param val New grids
    set_grid = function(val) {
      if(depth(val) == 0) {
        if(class(val)[1] == "TPEgrid") {
          self$grids <- list(val)
        } else {
          stop(paste0("Error. Trying to set a non grid object into ",
                      self$name))
        }
      } else if(depth(val) == 1) {
        if(sum(!(unlist(sapply(val, class)) %in% c("R6","TPEgrid"))) == 0) {
          self$grids <- val
        } else {
          stop(paste0("Error. Trying to set a non grid object into ",
                      self$name))
        }
      } else {
        stop("Depth of grids can not exceed 1")
      }
    },

    #' @description Set maps, this is a list of map objects,
    #' use with caution, this will overwrite existing maps
    #' @param val New maps
    set_map = function(val) {
      if(depth(val) == 0) {
        if(class(val)[1] == "TPEmap") {
          self$maps <- list(val)
        } else {
          stop(paste0("Error. Trying to set a non map object into ",
                      self$name))
        }
      } else if(depth(val) == 1) {
        if(sum(!(unlist(sapply(val, class)) %in% c("R6","TPEmap"))) == 0) {
          self$maps <- val
        } else {
          stop(paste0("Error. Trying to set a non map object into ",
                      self$name))
        }
      } else {
        stop("Depth of maps can not exceed 1")
      }
    },

    #' @description Get names of all grids
    get_gridNames = function() {
      return(private$gridnames)
    },

    #' @description Get names of all maps
    get_mapNames = function() {
      return(private$mapnames)
    },

    #' @description Get grid id
    #' @param val Either grid name or a grid id (will return the grid id)
    get_gridid = function(val) {
      if(class(val) == "numeric" || class(val) == "integer") {
        if(val > length(self$get_gridNames())) {
          stop(paste0("Grid identifier ", val, " not found."), call.=F)
        } else {
          id <- val
        }
      } else {
        if(!(val %in% self$get_gridNames())) {
          print(val)
          print(class(val))
          print(self$get_gridNames())
          stop(paste0("Grid identifier ", val, " not found."), call.=F)
        } else {
          id <- match(val, self$get_gridNames())
        }
      }
      return(id)
    },

    #' @description Get map id
    #' @param val Either map name or a map id (will return the map id)
    get_mapid = function(val) {
      if(class(val) == "numeric") {
        if(val > length(self$get_mapNames())) {
          stop(paste0("Map identifier ", val, " not found."), call.=F)
        } else {
          id <- val
        }
      } else {
        if(!(val %in% self$get_mapNames())) {
          stop(paste0("Map identifier ", val, " not found."), call.=F)
        } else {
          id <- match(val, self$get_mapNames())
        }
      }
      return(id)
    },

    #' @description Get simulation results of all grids
    #' @param traitList List of variables to extract as results
    get_results = function(traitList = NA) {
      resDf <- data.frame()
      for(i in 1:length(self$get_gridNames())) {
        tmpDf <- self$grids[[i]]$get_results(traitList)
        tmpDf$variety <- private$parent$get_varNames()[i]
        resDf <- rbind(resDf, tmpDf)
      }
      self$results <- resDf
    },

    #' @description Create a simulation grid
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
    createGrid = function(name="g1", varID=NA, latres=0.35, lonres=0.5, cols=5,
                          rows=5, lon=NA, lat=NA, multigrid=F) {
      if(multigrid) {
        for(i in 1:length(private$parent$get_varNames())) {
          gname <- paste0(name, "_", private$parent$get_varNames()[i])
          if(gname %in% self$get_gridNames()) {
            stop(paste("Grid with name", gname, "already exist.",
                       "Please provide a unique name to be attached to",
                       "variety name."))
          } else {
            self$grids <- append(self$grids, TPEgrid$new(gname,
                                             private$parent$varieties[[i]],
                                             latres, lonres, cols, rows, lon,
                                             lat, self))
            private$gridnames <- c(private$gridnames, gname)
          }
        }
      } else {
        if(name %in% self$get_gridNames()) {
          stop(paste("Grid with name", name,"already exists.",
                     "Please provide a unique identifier."))
        } else {
          id <- private$parent$get_varid(varID)
          self$grids <- append(self$grids, TPEgrid$new(name,
                                           private$parent$varieties[[id]],
                                           latres, lonres, cols, rows, lon,
                                           lat, self))
          private$gridnames <- c(private$gridnames, name)
        }
      }
    },

    #' @description Generate climate data for one or several grids
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
    #' @param seed Integer number to use as seed for marksim weather generator
    genClimate = function(gridID=NA, rcp="rcp26", year=2014, yearNb=1,
                          modelNb="00000000000000000", path=NA, pathCLI=NA,
                          filesE=F, verbose=T, seed=1337) {
      if(sum(!is.na(gridID)) == 0) {
        idg <- self$get_gridNames()
      } else {
        idg <- gridID[!is.na(gridID)]
      }

      for(i in 1:length(idg)) {
        id <- self$get_gridid(idg[i])
        if(verbose) {
          cat(paste("Generating climate for grid", self$get_gridNames()[id],
                    "this may take some time \n"))
        }
        self$grids[[id]]$genClimate(rcp, year, yearNb, modelNb, path,
                                    pathCLI, seed, filesE, verbose, seed)
      }
    },

    #' @description Run simulation on one or several grids
    #' @param gridID Optional. A vector of grid identifiers
    #' (either index or name). By default will run all grids
    #' @param varID A variety identifier to use for
    #' simulation on the grid. If NA, will use the variety attached to grid
    #' @param soilData Tmp for Adam et al.
    #' @param latlonData Tmp for Adam et al.
    #' @param cumulP Tmp for Adam et al.
    #' @param traitList Optionnal. Vector of trait names to extract from
    #' simulations. This will delete the simulations and only keep the max for
    #' each year
    #' @param savePath Optional. A character string of the path where to save
    #' simulation files. If NULL (default), will not save simulations
    runGridSim = function(gridID=NA,varID=NA,soilData=soil,latlonData=lat_lon,
                          cumulP=cumul, traitList=NULL, savePath=NULL) {
      if(sum(!is.na(gridID)) == 0) {
        idg <- self$get_gridNames()
      } else {
        idg <- gridID[!is.na(gridID)]
      }

      for(i in 1:length(idg)) {
        id <- self$get_gridid(idg[i])
        if(!is.na(varID)) {
          idv <- self$get_varid(varID)
          self$grids[[id]]$set_var(self$varieties[[idv]])
        }
        self$grids[[id]]$runGridSim(soilData, latlonData, cumulP, traitList,
                                    savePath)
      }
    },

    #' @description Create raster map
    #' @param name A character string defining the name of the map
    #' @param bounds Optional. A vector of four numeric values as decimal degree
    #' of north, east, south, west bounds to crop map
    #' @param res Not used at the moment. A numeric value in sec of the
    #' resolution of world map to use. Options are c(1, 150, 900)
    #' for respectively 30sec, 2.5min, 15min
    createMap = function(name="map1", bounds=NA, res=150) {
      if(name %in% self$get_mapNames()) {
        stop(paste("Map with name ", name, " already exist"))
      } else {
        private$mapnames <- c(private$mapnames, name)
        self$maps <- append(self$maps, TPEmap$new(name, bounds, self, res))
      }
    },

    #' @description Run TPE clustering by performing Principle Component
    #' Analysis (PCA) and Hierarchical Clustering on Principle Component (HCPC)
    #' @param mapID A value or list of values of map identifiers
    #' (either index or names)on which to perform clustering
    #' @param traitList Vector of variable names to be used for PCA
    #' @param nbDim Integer of number of dimensions to use for PCA
    #' @param nbClust Integer of number of clusters to use for HCPC
    runClustering = function(mapID=1, traitList=c("GrainYieldPop"), nbDim=5,
                             nbClust=3) {
      self$get_results(traitList)
      self$maps[[mapID]]$runPCA(self$results, nbDim, traitList)
      self$maps[[mapID]]$runHCPC(nbClust)
    },

    #' @description Create plot on map based on grid simulation
    #' @param mapID Id of map to plot
    #' @import ggplot2
    plotMap = function(mapID=1) {
      self$maps[[mapID]]$plotMap()
    },

    #' @description Bootstrap to test if gridded simulations are close to
    #' simulations on observed sites
    #' @param nameList List of names of sites to test
    #' @param latList List of latitudes of sites to test
    #' @param lonList List of longitudes of sites to test
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
    #' @param seed Integer number to use as seed for marksim weather generator
    #' @param soilData Tmp fix for Adam et al.
    #' @param latlonData Tmp fix for Adam et al.
    #' @param cumulP Tmp fix for Adam et al.
    #' @param traitList Vector of variable names
    #' @param savePath Path to save simu
    bootstrap = function(nameList, latList, lonList, rcp="rcp26", year=2014,
                         yearNb=1, modelNb="00000000000000000", path=NA,
                         pathCLI=NA, seed=1337, soilData, latlonData, cumulP,
                         traitList, savePath) {
      currentPath <- getwd()
      setwd(path)

      nbVar <- length(private$parent$get_varNames())
      self$bootstrapGP <- vector(mode="list", length=nbVar)

      for(v in 1:nbVar) {
        for(i in 1:length(latList)) {
          self$bootstrapGP[[v]] <- append(self$bootstrapGP[[v]],
                                     gridPoint$new(parent=self,
                                                   name=nameList[[i]],
                                                   lon=lonList[[i]],
                                                   lat=latList[[i]]))

          if(yearNb > 99) {
            nbRun <- yearNb %/% 99
            for(j in 1:nbRun) {
              lSeed <- seed[j] #TODO: Tmp fix
              cat(paste0("Generating run ", j, " of point ", i,
                         " for variety ", private$parent$get_varNames()[[v]],
                         "\n"))
              self$bootstrapGP[[v]][[i]]$genClimate(rcp, year, 99, modelNb,
                                                    path, pathCLI, lSeed, bs=T)
            }

            param <- private$parent$varieties[[v]]$parameters
            self$bootstrapGP[[v]][[i]]$runBSSimulation(param, soilData,
                                                       latlonData, cumulP,
                                                       traitList, year, savePath)
          } else {
            lSeed <- seed[1]
            cat(paste0("Generating point ", i,
                       " for variety ", private$parent$get_varNames()[[v]],
                       "\n"))
            self$bootstrapGP[[v]][[i]]$genClimate(rcp, year, 99, modelNb,
                                                  path, pathCLI, lSeed, bs=T)
            param <- private$parent$varieties[[v]]$parameters
            self$bootstrapGP[[v]][[i]]$runBSSimulation(param, soilData,
                                                       latlonData, cumulP,
                                                       traitList, year, savePath)
          }
        }
      }
      setwd(currentPath)
    }
  ),

  private = list(
    gridnames = NULL,
    mapnames = NULL,
    parent = NULL
  )
)
