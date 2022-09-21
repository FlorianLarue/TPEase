#'#' R6 Class Representing a TPE map
#'
#' @description
#' TODO
#'
#' @details
#' TODO
#' @import R6
#' @export
TPEmap <- R6::R6Class("TPEmap",
  public = list(
    #' @field name A character string identifier of the map
    name = NULL,
    #' @field data A SpatialPolygondataframe of the desired map
    data = NULL,
    #' @field PCAres Principle Component Analysis result for `self$grid$gridRes`
    PCAres = NULL,
    #' @field HCPCres Hierarchical Clustering on Principle Components result for
    #' `PCAres`
    HCPCres = NULL,
    #' @field parent TPE analysis parent
    parent = NULL,
    #' @field grid A grid object attached to the map
    grid = NULL,
    #' @field plots A list of ggplot2 plots from the map
    plots = list(),
    #' @field test Debug
    test = NULL,

    #' @description Create a new TPE map object
    #' @param name A character string identifier of the TPE map
    #' @param grid A grid identifier attached to the map
    #' @param bounds Optional. A vector of four numeric values as decimal degree
    #' of north, east, south, west bounds to crop map
    #' @param parent TPE parent
    #' @param res Not active for now. A numeric value in sec of the resolution
    #' of world map to use. Options are c(1, 150, 900) for respectively
    #' 30sec, 2.5min, 15min
    #' @return A new `TPEmap` object.
    #' @importFrom raster raster
    #' @importFrom raster crop
    #' @importFrom raster extent
    #' @importFrom raster crs
    initialize = function(name="map1", grid=NA, bounds=NA,
                          parent=NA, res=150) {
      self$name <- as.character(name)
      self$parent <- parent

      self$grid <- self$parent$grids[[self$parent$get_gridid(grid)]]

      cat(paste("\nCreating map", name,"\n"))

      pathMap <- system.file("extdata", "World.shp", package="CGMTPE")
      tmpmap <- raster::shapefile(pathMap)
      if(length(bounds) == 4) {
        e <- as(raster::extent(bounds[4],bounds[2],bounds[3],bounds[1]),
                "SpatialPolygons")
        raster::crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
        tmpmap <- crop(tmpmap, e)
      } else if(!is.na(bounds)) {
        stop(paste("Length of bounds is not 4.",
                   "Please provide a value for each cardinal point,",
                   "or use bounds=NA"))
      }
      self$data <- tmpmap
    },

    #' @description Run Principle Component Analysis
    #' @import FactoMineR
    #' @import factoextra
    #' @param traitList Optional. Vector of variables to use to perform PCA.
    #' By default will run on all variables of the map data
    #' @param nbDim Number of dimensions
    runPCA = function(traitList = NULL, nbDim = 5) {
      gridRes <- self$grid$get_results(traitList)
      self$grid$set_gridres(gridRes)
      dfPCA <- gridRes[,traitList]
      res <- PCA(dfPCA, ncp = nbDim, graph = F)
      self$PCAres <- res
    },

    #' @description Run Hierarchical Clustering on Principle Components
    #' @import FactoMineR
    #' @import factoextra
    runHCPC = function() {
      if(!is.null(self$resPCA)) {
        stop(paste0("No PCA found for map ", self$name,
                    ". Please run runPCA()"))
      } else {
        res <- HCPC(self$PCAres, nb.clust = -1, graph = F)
        self$HCPCres <- res
        self$grid$gridRes$cluster <- res$data.clust$clust
      }
    },

    #' @description Create plot on map based on grid simulation
    #' @import ggplot2
    plotMap = function() {
      #TODO: might need to change as fortify may be deprecated in the future
      AG <- fortify(self$data)
      p <- ggplot() + geom_raster(data=self$grid$gridRes,
                                  aes(x=x, y=y, fill=as.factor(cluster)),
                                  interpolate=FALSE) +
      geom_polygon(data=AG, aes(x=long, y=lat, group=group),
                   size=0.3, colour="black", fill=NA) +
        coord_cartesian() +
        theme_bw() +
        xlab("Longitude") + ylab("Latitude")
      self$plots <- append(self$plots, list(p))
    }
  ),

  private = list()
)
