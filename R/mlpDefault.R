#'@include mlp.R

# the internal list of default MLPs
.mlp.default <- list(FunctionalModel.mlp(c(1L, 6L, 1L)),
                     FunctionalModel.mlp(c(3L, 3L)),
                     FunctionalModel.mlp(c(2L, 3L, 2L)),
                     FunctionalModel.mlp(c(4L, 2L)),
                     FunctionalModel.mlp(c(2L, 2L)))

#' @title Get the List of Default MLP-based Models
#' @description Obtain a list of MLP-based models which are reasonable for a wide variety
#' of applications.
#' @return the list of default MLPs
#' @export FunctionalModel.mlp.default
FunctionalModel.mlp.default <- function() .mlp.default
