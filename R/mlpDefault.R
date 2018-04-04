#'@include mlp.R

# the internal list of default MLPs
.mlp.default <- list(FunctionalModel.mlp(c(2L)),
                     FunctionalModel.mlp(c(2L, 1L)))

#' @title Get the List of Default MLP-based Models
#' @description Obtain a list of MLP-based models which are reasonable for a wide variety
#' of applications.
#' @return the list of default MLPs
#' @export FunctionalModel.mlp.default
FunctionalModel.mlp.default <- function() .mlp.default
