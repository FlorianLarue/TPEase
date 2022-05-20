
#' @title Generate weather file for Samara from Marksim
#' @description test
#' @param variety test
#' @return test
#' @examples
#' test
#' @export
#' @importFrom raster getData
generateClimate <- function(lon, lat, rcp, year, yearNb, modelNb, path,
                            pathCLI=NA) {
  if(is.na(pathCLI)) {
    pathCLI <- path
  }

  #Extract data from worldclim
  coordinates <- cbind(lon,lat)
  grid.tmin = getData('worldclim', var='tmin', res=0.5, lat=lat, lon=lon)
  grid.tmax = getData('worldclim', var='tmax', res=0.5, lat=lat, lon=lon)
  grid.prec = getData('worldclim', var='prec', res=0.5, lat=lat, lon=lon)
  tmin=extract(grid.tmin,coordinates)
  tmax=extract(grid.tmax,coordinates)
  prec=extract(grid.prec,coordinates)
  prec[1,prec[1,]>999] <- 999

  if(!is.na(tmin[1])) { #if these data exists
    #Transform data in Marksim readable format and write the data in CLI file
    mdata <- matrix (rep(0,7*12),nrow=12,ncol=7)
    mdata[,1] <- seq(1:12)
    mdata[,2] <- rep(-99,12)
    mdata[,3] <- tmax/10
    mdata[,4] <- tmin/10
    mdata[,5] <- prec
    mdata[,6] <- rep(-99,12)
    mdata[,7] <- rep(-99,12)

    mdata2 <- cbind(mdata[,1],format(mdata[,-1],nsmall=1))
    if(file.exists(paste0(pathCLI,"/Anywhe.cli"))) {
      file.remove(paste0(pathCLI,"/Anywhe.cli"))
    }
    out <- file(paste0(pathCLI,"/Anywhe.cli"),open="a")
    writeLines("*CLIMATE : ANYWHERE", out)
    writeLines("@ INSI      LAT     LONG  ELEV   TAV   AMP  SRAY  TMXY  TMNY  RAIY", out)
    writeLines(paste("  0000 ",sprintf("%07.2f", lat),"",sprintf("%07.3f", lon),
                     "  -99   -99   -99   -99   -99   -99   -99"),out)
    writeLines("@START  DURN  ANGA  ANGB REFHT WNDHT SOURCE", out)
    writeLines("     0     0  0.25  0.50  0.00  0.00 Calculated_from_daily_data",out)
    writeLines("@ GSST GSDU",out)
    writeLines("    0   365",out)
    writeLines("",out)
    writeLines("*MONTHLY AVERAGES",out)
    writeLines("@MONTH  SAMN  XAMN  NAMN  RTOT  RNUM  SHMN",out)
    write.fwf(mdata2,out,rownames=FALSE, colnames=FALSE,width=c(6,rep(5,6)),
              justify="right")
    close(out)

    #Run marksim with user parameters
    shell(paste0('MarkSim_Standalone_v2.exe ', path, 'Data ', pathCLI, 'CLI ',
                 modelNb, ' ', rcp, ' ', year, ' ', yearNb,' 1337'))

    #Transform data in Samara readable format
    tab <- data.frame()
    nlines <<- c()
    dirname <- paste0(pathCLI,"/ANYWHE_",modelNb,"_",rcp,"_",year)
    files <- dir(dirname,pattern="ANYW[0-9][0-9][0-9]")
    for(f in files) {
      line=read.table(paste(dirname,"/",f,sep=""),head=T,skip=3)
      nlines <<-c(nlines,nrow(line))
      tab <- rbind(tab,line)
    }
    tab$day_in_year=tab$X.DATE %% 1000
    tab$year=(tab$X.DATE-tab$day_in_year)/1000
    colnames(tab) <- c("date","radiation","tmax","tmin","rainfall","day_in_year","year")
    vectNA <- rep(-999,nrow(tab))
    tabNA <- data.frame(tmoy=vectNA,rhmax=vectNA,rhmin=vectNA,rhmoy=vectNA,windtot=vectNA,sunshine=vectNA,eto=vectNA)
    tab_meteo_init <- cbind(tab,tabNA)
    tab_meteo <- tab_meteo_init[,c("date","tmax","tmin","tmoy","rhmax","rhmin","rhmoy","windtot","sunshine","radiation","eto","rainfall")]
    et0_estim_vect <- et0vect(tab_meteo$tmin,tab_meteo$tmax,tab_meteo$radiation)
    tab_meteo$eto <- et0_estim_vect
    tab_meteo$weatherdate <- format(seq.Date(from=as.Date("01/01/2001",format="%d/%m/%Y"),to=as.Date("31/12/2099",format="%d/%m/%Y"),by='days'),"%d/%m/%Y")
    tab_meteo$wscode <- rep(1,nrow(tab_meteo))
    meteo_samara <- tab_meteo[,c("wscode", "weatherdate",	"tmin",	"tmax",	"tmoy",	"rhmin",	"rhmax",	"rhmoy",	"rainfall",	"windtot",	"radiation",	"sunshine",	"eto")]

    return(meteo_samara)
  } else {
    print(paste0("Could not find worldclim data at longitude ",
                 lon," and lat ",lat))
  }
}

#' @title Compute et0
#' @description Compute evapotranspiration et0
#' @param tmin test
#' @param tmax test
#' @param srad test
#' @return A vector with et0 values
#' @examples
#' et0Values <- et0(tmin,tmax,srad)
#' @export
et0 <- function(tmin,tmax,srad){
  td <- 0.6*tmax + 0.4*tmin
  albedo <- 0.2150077
  slang <- srad*23.923
  eeq <- slang*(2.04*10^(-4)-1.83*10^(-4)*albedo)*(td+29.0)
  eo <- eeq*1.1
  index_tmax_sup35 <- tmax>35
  index_tmax_inf5 <- tmax<5
  eo[index_tmax_sup35] <- eeq[index_tmax_sup35]*((tmax[index_tmax_sup35]-35.0)*0.05+1.1)
  eo[index_tmax_inf5] <- eeq[index_tmax_inf5]*0.01*exp(0.18*(tmax[index_tmax_inf5]+20.0))
  return(eo)
}


#' @title Create a TPE analysis object
#' @description Create a TPE analysis object (from R6 class TPEa)
#' @param name test
#' @param model test
#' @param varieties test
#' @param environments test
#' @param latStart test
#' @param lonStart test
#' @param genotypes test
#' @return A R6 class TPE analysis object
#' @examples
#' TPE_analysis <- create_tpe("tpea1","Samara")
#' @export
create_tpe <- function(name="TPEa_1", model="Samara", varieties=NA,
                       environments=NA, latStart=NA, lonStart=NA,
                       genotypes=NA) {
  return(TPEa$new(name, model, varieties, environments, genotypes))
}
