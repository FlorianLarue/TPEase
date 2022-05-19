# Collection of Frequently Used Functions (FUF)
#' @importFrom Rdpack reprompt

#' @title Merge dataframes by Columns
#' @description Allows cbind to work with dataframes with different columns
#' by filling missing values with NA
#' found on https://stackoverflow.com/questions/7962267/
#' @param ... dataframes to be combined by columns
#' @return A matrix combining the \code{...} arguments column-wise
#' @examples
#' a <- data.frame()
#' b <- data.frame(a = 1, b = 2, c = 3)
#' df <- cbind.all(a, b)
#' @export
cmerge.all <- function (...) {
  nm <- list(...)
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow))
  df <- do.call(cbind, lapply(nm, function(x)
    rbind(x, matrix(NA, n - nrow(x), ncol(x)))))
  return(as.data.frame(df))
}

#' @title Merge rows inside dataframe
#' @description Merge rows inside dataframe based on a group by.
#' (requires dplyr library)
#' (temporary fix, need to find better solution)
#' @param df Dataframe with rows to merge
#' @param gb Group by column
#' @return Dataframe with merged rows grouped by \code{gb}
#' @examples
#' df <- data.frame(Date = c(1,1,2,2,3),
#'                 Mesure1 = c(NA,11,17,NA,13),
#'                 Mesure2 = c(10,NA,NA,15,14))
#' dfMerged <- rmerge.all(df,"Date")
#' @export
#' @import dplyr
rmerge.all <- function(df, gb) {
  #replace NA and NaN with empty values to ease the merge
  df[df == "NaN" | is.na(df)] <- ""

  #use dplyr to group by gb and merge values in other columns
  df2 <- df %>%
    group_by(.dots=gb) %>%
    summarise_all(list(~ paste(., collapse = '')))
  df2 <- as.data.frame(df2)

  #replace the empty values with NA
  df2[df2 == ""] <- NA

  return(df2)
}


#' @title Mean Absolute Error
#' @description Computes Mean Absolute Error between observed
#' and simulated values
#'
#' @param Obs Vector (or single value) of observed value(s)
#' @param Sim Vector (or single value) of simulated value(s)
#' @return The mean absolute error of \code{Obs} and \code{Sim}
#' @examples
#' MAE(c(1,1,2), c(1,2,2))
#' MAE(2, 3)
#' @export
MAE <- function(Obs,Sim) {
  Resid <- (Obs-Sim)
  Num <- sum(abs(Resid),na.rm=T)
  Den <- sum(!is.na(Resid))
  return(Num / Den)
}

#' @title Relative Mean Absolute Error
#' @description Computes Relative Mean Absolute Error between observed
#' and simulated values, i.e. the Mean Absolute Error divided by the mean of
#' observations (see \code{\link{MAE}})
#'
#' @param Obs Vector (or single value) of observed value(s)
#' @param Sim Vector (or single value) of simulated value(s)
#' @return The relative mean absolute error of \code{Obs} and \code{Sim}
#' @examples
#' rMAE(c(1,1,2), c(1,2,2))
#' rMAE(2, 3)
#' @export
rMAE <- function(Obs,Sim) {
  Num <- MAE(Obs,Sim)
  Den <- mean(Obs,na.rm=T)
  return(Num / Den)
}

#' @title Mean Squared Error
#' @description Computes Mean Squared Error between observed and
#' simulated values
#'
#' @param Obs Vector (or single value) of observed value(s)
#' @param Sim Vector (or single value) of simulated value(s)
#' @return The mean squared error of \code{Obs} and \code{Sim}
#' @examples
#' MSE(c(1,1,2), c(1,2,2))
#' MSE(2, 3)
#' @export
MSE <- function(Obs,Sim) {
  Resid <- (Obs-Sim)
  Num <- sum(Resid^2,na.rm=TRUE)
  Den <- sum(!is.na(Resid),na.rm=TRUE)
  return(Num / Den)
}

#' @title Root Mean Square Error
#' @description Computes Root Mean Square Error between observed and
#' simulated values, i.e. the squared root of Mean Squared Error
#' (see \code{\link{MSE}})
#'
#' @param Obs Vector (or single value) of observed value(s)
#' @param Sim Vector (or single value) of simulated value(s)
#' @return The root mean square error of \code{Obs} and \code{Sim}
#' @examples
#' RMSE(c(1,1,2), c(1,2,2))
#' RMSE(2, 3)
#' @export
RMSE <- function(Obs,Sim) {
  return(sqrt(MSE(Obs,Sim)))
}

#' @title Relative Root Mean Square Error
#' @description Computes Relative Root Mean Squared Error between observed and
#' simulated values, i.e. the Root Mean Square Error divided by the mean of
#' observations (see \code{\link{RMSE}})
#'
#' @param Obs Vector (or single value) of observed value(s)
#' @param Sim Vector (or single value) of simulated value(s)
#' @return The relative root mean square error of \code{Obs} and \code{Sim}
#' @examples
#' rRMSE(c(1,1,2), c(1,2,2))
#' rRMSE(2, 3)
#' @export
rRMSE <- function(Obs,Sim) {
  Num <- RMSE(Obs,Sim)
  Den <- mean(Obs,na.rm=T)
  return(Num / Den)
}

#' @title Coefficient of determination (R squared)
#' @description Computes Coefficient of Determination between observed and
#' simulated values
#'
#' @param Obs Vector of observed values
#' @param Sim Vector of simulated values
#' @return The R squared of \code{Obs} and \code{Sim}
#' @examples
#' rsquared(c(1,1,2), c(1,2,2))
#' @export
rsquared <- function(Obs, Sim) {
  Corr <- cor(Obs, Sim)
  return(Corr^2)
}


