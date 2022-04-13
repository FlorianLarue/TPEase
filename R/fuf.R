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
