#'@include mlp.R

# the internal list of default MLPs
.mlp.default <- list(FunctionalModel.mlp(c(2L)),
                     FunctionalModel.mlp(c(2L, 1L)))

#' @title Get the List of Default MLP-based Models
#' @description Obtain a list of MLP-based models which are reasonable for a
#'   wide variety of applications.
#' @return the list of default MLPs
#' @export FunctionalModel.mlp.default
#' @seealso FunctionalModel.mlp.monotonous
FunctionalModel.mlp.default <- function() .mlp.default



# the internal list of default MLPs
.mlp.monotonous<- list(FunctionalModel.mlp(c(2L), decreasing=TRUE),
                     FunctionalModel.mlp(c(2L, 1L), decreasing=TRUE))

#' @title Get the List of Default Monotonously Decreasing MLP-based Models
#' @description Obtain a list of monotonously decreasing MLP-based models which
#'   are reasonable for a wide variety of applications.
#' @return the list of monotonous MLPs
#' @export FunctionalModel.mlp.monotonous
#' @seealso FunctionalModel.mlp.default
FunctionalModel.mlp.monotonous <- function() .mlp.monotonous
