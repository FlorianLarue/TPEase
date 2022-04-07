#' @import parallel
#' @import DEoptim
#' @import rsamara
#' @import Rcpp
#' @import hash

### Sample script to perform parameter estimation of Samara ###

## Set global options
options(stringsAsFactors = FALSE)
maxIter <- 2000
workDir <- "D:/Workspace/Postdoc/Samara/TPE-WA/"
nbCores <- detectCores()-2 # number of CPU cores used during estimation process
set.seed(1337)

## Declare functions
import_data <- function() {
  simulations <<- read.csv(paste(workDir, "simulation_list.csv", sep=""), sep=";")
  itks <<- simulations$itkcode[simulations$itkcode != ""] #remove empty rows
  varieties <<- simulations$variety[simulations$variety != ""]
  genotypes <<- simulations$genotype[simulations$genotype != ""]
  blocks <<- c("I","II","III","IV") #name of blocks used in obs file names
  estimParam <<- read.csv(paste0(workDir,"estimation_params.csv"),
                         header=T, sep=",") # info of estimated parameters
  paramOfInterest <<- tolower(colnames(estimParam)[-c(1,2)]) # name of est params
  startingDates <<- simulations$startingdate[simulations$startingdate != ""]
  endingDates <<- simulations$endingdate[simulations$endingdate != ""]
}
construct_data <- function(variety,genotype,itk) { # built data for var/itk
  ib <- match(itk,itks)
  varietyData <- list()

  # variety and itk parameters
  varietyParameters <- read.csv(paste0(workDir,"varieties/",variety,".csv"))
  varietyParameters <- as.data.frame(varietyParameters[1,])
  itkParameters <- read.csv(paste0(workDir,"params/", itk,".csv"))
  viParams <- merge(varietyParameters,itkParameters)
  viParams$stemporosity = 0.67
  viParams$startingdate = rsamara::toJulianDayCalcC(startingDates[ib],"DMY",'/')
  viParams$endingdate = rsamara::toJulianDayCalcC(endingDates[ib],"DMY",'/')
  varietyData[["parameters"]] <- viParams

  # weather data
  itkWeather <- read.csv(paste0(workDir,"meteos/", itk, ".csv"))
  itkWeather <- itkWeather[,-c(1)]
  varietyData[["weathers"]] <- itkWeather

  # observation data
  viObservations <- list()
  for(j in 1:length(blocks)) {
    block <- blocks[[j]]
    obs <- read.csv(paste0(workDir,"obs/", itk, genotype, block, ".csv"))
    #tmp error fix, in original script also error fix for missing grainpop
    #not sure it is useful at this point
    names(obs)[names(obs) == "grainyieldpopfin"] <- "grainyieldpop"
    obs$plantheight = 10*obs$plantheight
    viObservations[[j]] <- obs
  }
  varietyData[["observations"]] <- viObservations

  return(varietyData)
}
get_score <- function(obs,sim) { # defines how fitness is computed
  # Metric functions
  #TODO: remove and use the one in the CGMTPE package
  MAE <- function(Obs,Sim){
    Resid <- (Obs-Sim)
    Num <- sum(abs(Resid),na.rm=T)
    Den <- sum(!is.na(Resid))
    return(Num / Den)
  }
  rMAE <- function(Obs,Sim){
    Num <- MAE(Obs,Sim)
    Den <- mean(Obs,na.rm=T)
    return(Num / Den)
  }
  MSE <- function(Obs,Sim){
    Resid <- (Obs-Sim)
    Num <- sum(Resid^2,na.rm=TRUE)
    Den <- sum(!is.na(Resid),na.rm=TRUE)
    return(Num / Den)
  }
  RMSE <- function(Obs,Sim){
    return(sqrt(MSE(Obs,Sim)))
  }
  rRMSE <- function(Obs,Sim){
    Num <- RMSE(Obs,Sim)
    Den <- mean(Obs,na.rm=T)
    return(Num / Den)
  }
  # Step 1: compute error on biomass and yield
  max_sim_phase <- max(sim$NumPhase)
  last_day_sim <- max(sim$ObsPlantDate[sim$NumPhase == max_sim_phase], na.rm=T)
  last_day_obs <- max(obs$obsplantdate, na.rm = T)
  if( last_day_sim >= last_day_obs) {
    max_sim_yield <- sim$GrainYieldPop[sim$ObsPlantDate == last_day_obs]
    max_sim_biomass <- sim$DryMatAboveGroundPop[sim$ObsPlantDate == last_day_obs]
    rRMSE_biomass = rRMSE(max(obs$drymatabovegroundpopfin, na.rm=T),max_sim_biomass)
  } else {
    tmp_obs = obs[obs$obsplantdate < last_day_sim,]
    max_sim_yield <- max(sim$GrainYieldPop)
    day_max_biomass <- max(tmp_obs$obsplantdate[ tmp_obs$drymatabovegroundpop == max(tmp_obs$drymatabovegroundpop, na.rm=T) ], na.rm=T)
    max_sim_biomass <- sim$DryMatAboveGroundPop[ sim$ObsPlantDate == day_max_biomass ]
    rRMSE_biomass = rRMSE(max(tmp_obs$drymatabovegroundpop, na.rm=T),max_sim_biomass)
  }
  # Step 2: reduce result dataframe to match traits and dates of obs
  sim <- rsamara::rcpp_reduceResults(sim, obs)
  # Step 2: compute other errors
  weights <- c(1,3,1,1,1,1)
  names(weights) <- c("ABG","lai","plantheight","leavesnumber","biomass","grain_yield")
  score_sum <- 0
  score_count <- 0
  if (!is.na(rRMSE_biomass)) {
    score_sum = weights["biomass"] * rRMSE_biomass
    score_count = weights["biomass"]
  }
  RMAE_ABG = rMAE(obs$drymatabovegroundpop,sim$drymatabovegroundpop)
  if (!is.na(RMAE_ABG)) {
    score_sum = weights["ABG"] * RMAE_ABG
    score_count = weights["ABG"]
  }
  RMAE_lai = rMAE(head(obs$lai,-1),head(sim$lai,-1))
  if (!is.na(RMAE_lai)) {
    score_sum = weights["lai"] * RMAE_lai
    score_count = weights["lai"]/2
  }
  RMAE_plantheight = 0
  if("plantheight" %in% colnames(obs)){
    RMAE_plantheight = rMAE(head(obs$plantheight,-1),head(sim$plantheight,-1))
  }
  if (!is.na(RMAE_plantheight)) {
    score_sum = weights["plantheight"] * RMAE_plantheight
    score_count = weights["plantheight"]
  }
  RMAE_leavesnumber = 0
  if("haunindex" %in% colnames(obs)){
    RMAE_leavesnumber = rMAE(obs$haunindex,sim$haunindex)
  }
  if (!is.na(RMAE_leavesnumber)) {
    score_sum = weights["leavesnumber"] * RMAE_leavesnumber
    score_count = weights["leavesnumber"]
  }
  rRMSE_grain_yield = 0
  if("grainyieldpop" %in% colnames(obs)) {
    rRMSE_grain_yield = rRMSE(max(obs$grainyieldpop, na.rm=T),max_sim_yield)
  }
  if (!is.na(rRMSE_grain_yield)) {
    score_sum = weights["grain_yield"] * rRMSE_grain_yield
    score_count = weights["grain_yield"]
  }
  score <- score_sum / score_count
  return(score)
}
fitness <- function(p) { #computes fitness for a given estimated parameter set
  fitScore <- 0
  for(k in 1:length(itks)) {
    # Step 1: create simulation
    if(rsamara::sim_exist_idx(k) == 0) {
      param <- as.data.frame(parameters[[k]], stringsAsFactors=FALSE)
      weather <- as.data.frame(weathers[[k]], stringsAsFactors=FALSE)
      simuEnv$simuInit[[k]] <- TRUE
      rsamara::init_sim_idx_simple(k, param, weather)
    }
    # Step 2 : update simulation with estimated parameters values
    rsamara::update_sim_idx(k, p, paramOfInterest)
    # Step 3 : run simulation
    sim <- rsamara::run_sim_idx(k)
    # Step 4 : compute fitness
    for(l in 1:length(blocks)) {
      obs <- observations[[k]][[l]]
      colnames(obs)[4] <- "haunindex" #tmp fix
      obs <- rsamara::rcpp_reduceVobs(obs, sim)
      viScore <- get_score(obs,sim)
      fitScore <- fitScore + viScore
    }
  }
  return(fitScore)
}
estim_param <- function() {
  for(variety in varieties) {
    # create new simulation environment
    simuEnv <<- new.env()
    simuEnv$simuInit <<- list()

    # retrieve data for given variety
    genotype <<- genotypes[[match(variety,varieties)]]
    weathers <<- list()
    observations <<- list()
    parameters <<- list()
    for(i in 1:length(itks)) { #get data of all itks
      varietyData <<- construct_data(variety,genotype,itks[[i]])
      parameters[[i]] <<- varietyData[["parameters"]]
      weathers[[i]] <<- varietyData[["weathers"]]
      observations[[i]] <<- varietyData[["observations"]]
      simuEnv$simuInit[[i]] <<- FALSE
    }
    boundsData <<- estimParam[which(estimParam$Genotype == variety),]
    lowerBounds <<- as.numeric(boundsData[which(boundsData$Value == "MinValue"),
                                         -c(1,2)])
    upperBounds <<- as.numeric(boundsData[which(boundsData$Value == "MaxValue"),
                                         -c(1,2)])
    paramBounds <<- matrix(c(lowerBounds,upperBounds),ncol=2)

    # create new parallel cluster
    cl <<- makeCluster(nbCores)
    clusterEvalQ(cl, library(rsamara))
    clusterExport(cl,
                  varlist=c("paramOfInterest", "simuEnv", "variety", "itks",
                            "observations", "weathers", "parameters", "blocks",
                            "get_score"),
                  envir=simuEnv)

    # run estimation algorithm
    DEParams <<- DEoptim.control(itermax = maxIter,
                                strategy = 2,
                                trace = 1,
                                NP = 10*length(paramOfInterest))
    #, cluster=cl)
    result <<- DEoptim(fitness, lower=paramBounds[,1], upper=paramBounds[,2],
                      control=DEParams)

    # retrieve results
    bestMem <<- result$optim$bestmem
    names(bestMem) <<- tolower(paramOfInterest)
    bestValue <<- result$optim$bestval
    finalParams <<- parameters
    finalParams <<- parameters[[1]]
    for(ip in 1:length(parameters)) {
      tmpParam <<- parameters[[ip]]
      tmpParam[tolower(paramOfInterest)] <<- bestMem
      finalParams <<- rbind(finalParams,tmpParam)
    }

    # save results
    write.csv(bestMem,paste0(workDir,"results/",variety,".csv"))
    write.csv(finalParams, paste0(workDir,"results/all_params/",variety,".csv"))
    stopCluster(cl)
  }

}

## Step 1 : import the data
#import_data()

## Step 2 : perform estimation
#estim_param()
