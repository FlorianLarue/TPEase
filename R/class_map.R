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
    #' @field plots A list of ggplot2 plots from the map
    plots = list(),
    #' @field test Debug
    test = NULL,

    #' @description Create a new TPE map object
    #' @param name A character string identifier of the TPE map
    #' @param bounds Optional. A vector of four numeric values as decimal degree
    #' of north, east, south, west bounds to crop map
    #' @param parent TPE parent
    #' @return A new `TPEmap` object.
    #' @importFrom raster crop
    #' @importFrom raster extent
    #' @importFrom raster crs
    #' @importFrom raster shapefile
    initialize = function(name="map1", bounds=NA, parent=NA) {
      self$name <- as.character(name)
      private$parent <- parent

      cat(paste("\nCreating map", name,"\n"))

      pathMap <- system.file("extdata", "World.shp", package="TPEase")
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
    #' @param dfPCA Data to perform PCA
    #' @param nbDim Number of dimensions
    #' @param traitList Vector of trait names to use for PCA
    runPCA = function(dfPCA = NULL, nbDim = 5, traitList) {
      cat(paste0("Running PCA on map ", self$name, "\n"))
      dfPC <- dfPCA[,traitList]
      res <- PCA(dfPC, ncp = nbDim, graph = F)
      self$PCAres <- res
    },

    #' @description Run Hierarchical Clustering on Principle Components
    #' @param nbClust Number of clusters
    #' @import FactoMineR
    runHCPC = function(nbClust) {
      if(!is.null(self$resPCA)) {
        stop(paste0("No PCA found for map ", self$name,
                    ". Please run runPCA()"))
      } else {
        cat(paste0("Running HCPC on map ", self$name, "\n"))
        res <- HCPC(self$PCAres, nb.clust = nbClust, graph = F)
        self$HCPCres <- res
        #TODO: save at correct spot
        private$parent$results$cluster <- res$data.clust$clust
      }
    },

    #' @description Create plot on map based on grid simulation
    #' @param trait A character string identifier of the data to plot,
    #' by default will plot the cluster computed by the runClustering function
    #' of the TPE analysis object
    #' @param isFactor A boolean indicating if the trait should be considered
    #' as a factor for plotting (cluster is a factor)
    #' @import ggplot2
    plotMap = function(trait="cluster", isFactor=T) {
      cat(paste0("Plotting map ", self$name,
                   " see print_maps() to show the plots\n"))
      #TODO: might need to change as fortify may be deprecated in the future
      AG <- fortify(self$data)

      #TODO: tmp fix, find better solution
      mapData <- private$parent$results[!is.na(
        private$parent$results[,3]),]

      p <- ggplot()
      if(isFactor) {
        p <- p + geom_raster(
          data=mapData,
          aes(x=x, y=y, fill=as.factor(mapData[,trait])),
          interpolate=FALSE
          )
      } else {
        p <- p + geom_raster(
          data=mapData,
          aes(x=x, y=y, fill=mapData[,trait]),
          interpolate=FALSE
        )
      }
      p <- p + geom_polygon(data=AG, aes(x=long, y=lat, group=group),
                   size=0.3, colour="black", fill=NA) +
        coord_cartesian() +
        facet_wrap(~ variety) +
        theme_bw() +
        xlab("Longitude") + ylab("Latitude") +
        labs(fill=trait)
      self$plots <- append(self$plots, list(p))
    },

    #' @description Create plot on map based on grid simulation
    #' @param plotID A numeric value identifier of the plot
    #' @param plotAdd A list of ggplot2 objects to pass to the plot
    #' @import ggplot2
    addToPlot = function(plotID=1, plotAdd) {
      for(i in 1:length(plotAdd)) {
        self$plots[[plotID]] <- self$plots[[plotID]] + plotAdd[[i]]
      }
    }

  ),

  private = list(
    parent = NULL
  )
)
