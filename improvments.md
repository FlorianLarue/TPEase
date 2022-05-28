# CGMTPE

## Grid creation

```r
self$gridPoints <- matrix(list(), nrow=length, ncol=width)
if(!is.na(lon) & !is.na(lat)) {
	for(i in 1:nrow(self$gridPoints)) {
		latPoint <- self$latStart + (i*(self$res/111))
		for(j in 1:ncol(self$gridPoints)) {
			lonPoint <- self$lonStart + ((j*(self$res/111)) * cos(latPoint))
            self$gridPoints[i,j] <- list(gridPoint$new(parent=self,
                                                       name=paste0(i,j),
                                                       lon=lonPoint,
                                                       lat=latPoint))
        }
    }
} else {
	cat(paste("Missing longitude or latitude, grid will not be populated.",
	"If needed, please use populateGrid() on grid",self$name))
}
```

