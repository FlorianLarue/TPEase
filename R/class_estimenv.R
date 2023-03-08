#' R6 Class representing an EstimEnv Environment
#'
#' @description
#' The `EstimEnv` object is a subclass of `TPEaseEnv` specific for an
#' estimation process and containing all information of the Environment,
#' defined as a combination of : soil, weather and crop management data as
#' well as a geographical location
#'
#' @import R6
#' @export
EstimEnv <- R6::R6Class("EstimEnv",
  inherit = TPEaseEnv,
  public = list(
  )
)
