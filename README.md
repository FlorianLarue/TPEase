# CGMTPE

R package to perform Target Population of Environments (TPE) analysis using Crop Growth Models (CGM). 

This package aims to facilitate the analysis of TPE (in particular the characterization of the environment according to the performance of cultivated plants) by providing a number of useful functions for: (1) model calibration by parameter estimation, (2) model validation, (3) generation of future climates on a grid and (4) simulation of plant performance on this grid. These results can then be used to characterize the environments. 

CGMTPE supports the Samara crop model (ref). Work is ongoing to also support STICS (ref) and other models (DSSAT, APSIM, ...).

## Installation

Use the provided install_github function from the [devtools](https://www.r-project.org/nosvn/pandoc/devtools.html) package to install CGMTPE:

```r
install_github("KawahFL/CGMTPE")
```

## Usage

```r
library(CGMTPE)

# load example data
loadData()

# create a TPE analysis object
TPE_analysis <- createTpe(varieties=c("Fadda"), 
                          environments=c("BAMA2014S1"),
                          latStart=11.8, lonStart=-4.3, 
                          parameters=param)

# create a simulation grid
TPE_analysis$createGrid(5,3,3) #size 3x3 with 5km grid points

# generate climate on each point of the grid
TPE_analysis$genClim(path="D:\\Marksim\\")

# run model simulation on each point of the grid
TPE_analysis$runSim()

# plot simulations
TPE_analysis$plotSim("GrainYieldPopFin")

```

## Contributing
Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

Please make sure to update tests as appropriate.
