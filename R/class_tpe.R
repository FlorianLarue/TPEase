#' R6 Class representing a TPEaseTPE
#'
#' @description
#' The `TPEaseTPE` object is an environment containing all objects used for
#' TPE analysis (grids and maps)
#'
#' @import R6
#' @export
TPEaseTPE <- R6::R6Class("TPEaseTPE",
  public = list(
    #' @field name A character string identifier of the TPEaseTPE
    name = NULL,
    #' @field model A character string identifier of the used crop model
    #' (only "Samara" is supported for now)
    model = NULL,
    #' @field grids A list of TPEGrid objects
    grids = list(),
    #' @field results A dataframe with grid simulation result summary
    results = NULL,
    #' @field maps A list of TPEMap objects for each of the TPEGrid
    maps = list(),
    #' @field bootstrapGP A list of GridPoints to perform bootstrap
    bootstrapGP = list(),
    #' @field test Debug
    test = NULL,

    #' @description Create a new TPEaseTPE object
    #' @param name A character string identifier of the TPEaseTPE
    #' @param model A character string identifier of the used crop model
    #' @param parent A TPEase object parent of the TPEaseTPE
    #' @return A new `TPEaseTPE` object.
    initialize = function(name="TPE_1", model="Samara", parent=NULL) {
      self$name <- as.character(name)
      self$model <- as.character(model)
      private$parent <- parent
      self$initMessage()
    },

    #' @description Confirm creation of TPEaseTPE object
    initMessage = function() {
      cat(paste0("TPE ", self$name, " created \n"))
    },

    #' @description Set TPEaseTPE name
    #' @param val New TPEaseTPE name
    set_name = function(val) {
      self$name <- as.character(val)
    },

    #' @description Set model name
    #' @param val New model name
    set_model = function(val) {
      self$model <- as.character(val)
    },

    #' @description Set grids
    #' @param val New TPEGrid objects
    set_grid = function(val) {
      if(depth(val) == 0) {
        if(class(val)[1] == "TPEGrid") {
          self$grids <- list(val)
        } else {
          stop(paste0("Error. Trying to set a non grid object into ",
                      self$name))
        }
      } else if(depth(val) == 1) {
        if(sum(!(unlist(sapply(val, class)) %in% c("R6","TPEGrid"))) == 0) {
          self$grids <- val
        } else {
          stop(paste0("Error. Trying to set a non grid object into ",
                      self$name))
        }
      } else {
        stop("Depth of grids can not exceed 1")
      }
    },

    #' @description Set maps
    #' @param val New TPEMap objects
    set_map = function(val) {
      if(depth(val) == 0) {
        if(class(val)[1] == "TPEMap") {
          self$maps <- list(val)
        } else {
          stop(paste0("Error. Trying to set a non map object into ",
                      self$name))
        }
      } else if(depth(val) == 1) {
        if(sum(!(unlist(sapply(val, class)) %in% c("R6","TPEMap"))) == 0) {
          self$maps <- val
        } else {
          stop(paste0("Error. Trying to set a non map object into ",
                      self$name))
        }
      } else {
        stop("Depth of maps can not exceed 1")
      }
    },

    #' @description Get names of all TPEGrid
    get_gridNames = function() {
      return(private$gridnames)
    },

    #' @description Get names of all TPEMap
    get_mapNames = function() {
      return(private$mapnames)
    },

    #' @description Get TPEGrid id
    #' @param val A grid identifier
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

    #' @description Get TPEMap id
    #' @param val A map identifier
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
    #' @param traitList A list of variables to extract as results
    get_results = function(traitList = NA) {
      resDf <- data.frame()
      for(i in 1:length(self$get_gridNames())) {
        tmpDf <- self$grids[[i]]$get_results(traitList)
        tmpDf$variety <- private$parent$get_varNames()[i]
        resDf <- rbind(resDf, tmpDf)
      }
      self$results <- resDf
    },

    #' @description Create a TPEGrid object
    #' @param name A character string identifier of the grid
    #' @param varID A TPEaseVar identifier to use for simulation on the grid
    #' @param latres A numeric value of the latitude resolution of the grid
    #' @param lonres A numeric value of the longitude resolution of the grid
    #' @param cols A numeric value of the number of columns in the grid
    #' @param rows A numeric value of the number of rows in the grid
    #' @param lon A numeric value of the starting longitude of the grid in
    #' decimal degrees
    #' @param lat A numeric value of the starting latitude of the grid in
    #' decimal degrees
    #' @param multigrid A boolean indicating if one grid should be created
    #' for each TPEaseVar with the same cols, rows, lon and lat. If true, varID
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

    #' @description Generate climate data for one or several TPEGrid
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
    #' @param filesE A boolean indicating if weather files already exist
    #' @param verbose A boolean indicating ff messages about starting
    #' climate generation should be shown
    #' @param seed An integer number to use as seed for marksim weather
    #' generator
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
                                    pathCLI, filesE, verbose, seed)
      }
    },

    #' @description Run simulation on one or several TPEGrid
    #' @param gridID Optional. A vector of grid identifiers
    #' (either index or name). By default will run all grids
    #' @param varID A TPeaseVar identifier to use for
    #' simulation on the grid. If NA, will use the variety attached to grid
    #' @param soilData Tmp for Adam et al.
    #' @param latlonData Tmp for Adam et al.
    #' @param cumulP Tmp for Adam et al.
    #' @param traitList Optionnal. A vector of trait names to extract from
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

    #' @description Create TPEMap object
    #' @param name A character string identifier of the map
    #' @param bounds Optional. A vector of four numeric values as decimal degree
    #' of north, east, south, west bounds to crop map
    #' @param res Not used at the moment.
    #' TODO: remove map res
    createMap = function(name="map1", bounds=NA) {
      if(name %in% self$get_mapNames()) {
        stop(paste("Map with name ", name, " already exist"))
      } else {
        private$mapnames <- c(private$mapnames, name)
        self$maps <- append(self$maps, TPEmap$new(name, bounds, self))
      }
    },

    #' @description Run TPE clustering by performing Principle Component
    #' Analysis (PCA) and Hierarchical Clustering on Principle Component (HCPC)
    #' on a TPEMap
    #' @param mapID A value or list of values of map identifiers
    #' (either index or names) on which to perform clustering
    #' @param traitList A vector of variable names to be used for PCA
    #' @param nbDim An integer of number of dimensions to use for PCA
    #' @param nbClust An integer of number of clusters to use for HCPC
    runClustering = function(mapID=1, traitList=c("GrainYieldPop"), nbDim=5,
                             nbClust=3) {
      self$get_results(traitList)
      self$maps[[mapID]]$runPCA(self$results, nbDim, traitList)
      self$maps[[mapID]]$runHCPC(nbClust)
    },

    #' @description Create a plot on a TPEMap based on TPEGrid simulations
    #' @param mapID A value of map identifier
    #' @param trait A character string identifier of the data to plot,
    #' by default will plot the cluster computed by the runClustering function
    #' of the TPEeaseTPE object
    #' @param isFactor A boolean indicating if the trait should be considered
    #' as a factor for plotting (cluster is a factor)
    #' @import ggplot2
    plotMap = function(mapID=1, trait, isFactor) {
      self$maps[[mapID]]$plotMap(trait, isFactor)
    },

    #' @description Add ggplot2 objects to existing plots
    #' @param mapID A numeric value identifier of the TPEMap
    #' @param plotID A numeric value identifier of the plot
    #' @param plotAdd A list of ggplot2 objects to pass to the plot
    #' @import ggplot2
    addToPlot = function(mapID=1, plotID=1, plotAdd) {
      self$maps[[mapID]]$addToPlot(plotID, plotAdd)
    },

    #' @description Perform Bootstrap Analysis to test if gridded simulations
    #' are close to simulations on observed sites
    #' @param nameList A list of names of sites to test
    #' @param latList A list of latitudes of sites to test
    #' @param lonList A list of longitudes of sites to test
    #' @param rcp A character string with the name of the Representative
    #' Concentration Pathway to use. One of the following options
    #' c("rcp26","rcp45","rcp60","rcp85")
    #' @param year A numeric value of the year to simulate climate
    #' (this can include years from 2013 to 2099)
    #' @param yearNb A numeric value of the number of replicates to simulate
    #' @param modelNb A character string of the general circulation model
    #' identifier to use, see \code{generateClimate}
    #' @param path A character string with the path to the marksim standalone.
    #' For the moment, this path can not contain spaces
    #' @param pathCLI Optional. A character string with the path to CLI folder.
    #' If no value is provided, the CLI folder will be considered in the same
    #' folder as the marksim standalone. For the moment, this path can not
    #' contain spaces
    #' @param seed An integer number to use as seed for marksim weather
    #' generator
    #' @param soilData Tmp fix for Adam et al.
    #' @param latlonData Tmp fix for Adam et al.
    #' @param cumulP Tmp fix for Adam et al.
    #' @param traitList A vector of variable names
    #' @param savePath A character string of the path to save bootstrap
    #' simulations
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
