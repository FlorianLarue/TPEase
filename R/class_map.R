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
    #' @field data A dataframe with map coordinates and values
    data = NULL,
    #' @field PCAres Principle Component Analysis result for `data`
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
    #' @param res A numeric value in sec of the resolution of world map to use
    #' Options are c(1, 150, 900) for respectively 30sec, 2.5min, 15min
    #' @param bounds Optional. A vector of four numeric values as decimal degree
    #' of north, east, south, west bounds to crop map
    #' @param parent TPE parent
    #' @return A new `TPEmap` object.
    #' @importFrom raster raster
    #' @importFrom raster crop
    #' @importFrom raster extent
    #' @importFrom raster crs
    initialize = function(name="map1", grid=NA, res=150, bounds=NA,
                          parent=NA) {
      self$name <- as.character(name)
      self$parent <- parent

      self$grid <- self$parent$grids[[self$parent$get_gridid(grid)]]

      cat(paste("\nCreating map", name,"\n"))

      pathMap <- system.file("extdata", paste0("world_",as.character(res),
                                               ".tif"), package="CGMTPE")
      tmpmap <- raster(pathMap)
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
      map <- as(tmpmap, "SpatialPixelsDataFrame")
      map <- as.data.frame(map)
      colnames(map) <- c("value", "x", "y")
      map <- map[, names(map) != "value"]
      self$data <- map
    },

    #' @description Run Principle Component Analysis
    #' @import FactoMineR
    #' @import factoextra
    #' @param varList Optional. Vector of variables to use to perform PCA.
    #' By default will run on all variables of the map data
    #' @param nbDim Number of dimensions
    runPCA = function(varList = NULL, nbDim = 5) {
      gridRes <- self$grid$get_results(varList)
      dfPCA <- gridRes[,varList]
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
      }
    },

    #' @description Create plot on map based on grid simulation
    #' @import interp
    #' @import ggplot2
    plotMap = function() {
      cat(paste("Plotting map", self$name, "\n"))
      for(i in 1:nrow(self$grid$gridPoints)) {
        for(j in 1:ncol(self$grid$gridPoints)) {
          pointLat <- round(self$grid$gridPoints[i,j][[1]]$lat,digits=1)
          pointLon <- round(self$grid$gridPoints[i,j][[1]]$lon,digits=1)
          self$data[which(round(self$data$x,digits=1) == pointLon &
                     round(self$data$y,digits=1) == pointLat),][1,"value"] <-
            ifelse(self$grid$resGrid[i,j] == 0, NA, self$grid$resGrid[i,j])
        }
      }

      self$data$x <- round(self$data$x, digits=2)
      self$data$y <- round(self$data$y, digits=2)
      df2 <- self$data[!is.na(self$data$value),]
      bbox <- c(
        "xmin" = min(df2$x),
        "ymin" = min(df2$y),
        "xmax" = max(df2$x),
        "ymax" = max(df2$y)
      )
      grd_template <- expand.grid(
        x = seq(from = bbox["xmin"], to = bbox["xmax"], by = 0.1),
        y = seq(from = bbox["ymin"], to = bbox["ymax"], by = 0.1)
      )

      fit_TIN <- interp::interp(
        x = df2$x,
        y = df2$y,
        z = df2$value,
        xo = grd_template$x,
        yo = grd_template$y,
        output = "points"
      )
      fit <- data.frame(do.call(cbind, fit_TIN))

      missings <- c()
      for(k in 1:nrow(fit)) {
        if(nrow(self$data[which(round(self$data$x, digits=1) ==
                         round(fit[k,"x"], digits=1) &
                         round(self$data$y, digits=1) ==
                         round(fit[k,"y"], digits=1)),]) == 0) {
          missings <- c(missings,k)
        }
      }
      fit2 <- fit[-missings,]

      grid_plot <- ggplot() +
        geom_point(data = self$data,
                   mapping = aes(x = x, y = y, color = value)) +
        geom_point(data = fit2, aes(x = x, y = y, color = z)) +
        scale_color_gradientn(colors = c("blue", "yellow", "red"))

      self$plots <- append(self$plots, list(grid_plot))
    }
  ),

  private = list()
)
