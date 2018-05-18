library("regressoR.functional.models")
context("FunctionalModel.mlp")

test_that("Test FunctionalModel.mlp(c(1))", {
  model <- FunctionalModel.mlp(c(1));
  expect_identical(is.null(model), FALSE);
  expect_identical(is(model, "FunctionalModel"), TRUE)
  expect_identical(model@paramCount, (1L+1L) + (1L + 1L))
  expect_identical(model@paramLower, NULL)
  expect_identical(model@paramUpper, NULL)
  validObject(model)
})

test_that("Test FunctionalModel.mlp(c(1, 2))", {
  model <- FunctionalModel.mlp(c(1, 2));
  expect_identical(is.null(model), FALSE);
  expect_identical(is(model, "FunctionalModel"), TRUE)
  expect_identical(model@paramCount, (1L+1L) + 2L*(1L + 1L) + ((2L*1L) + 1L))
  expect_identical(model@paramLower, NULL)
  expect_identical(model@paramUpper, NULL)
  validObject(model)
})


test_that("Test FunctionalModel.mlp(c(2, 2))", {
  model <- FunctionalModel.mlp(c(2, 2));
  expect_identical(is.null(model), FALSE);
  expect_identical(is(model, "FunctionalModel"), TRUE)
  expect_identical(model@paramCount, (2L+2L) + 2L*(2L + 1L) + ((2L*1L) + 1L))
  expect_identical(model@paramLower, NULL)
  expect_identical(model@paramUpper, NULL)
  validObject(model)
})


test_that("Test FunctionalModel.mlp(c(1)) dec", {
  model <- FunctionalModel.mlp(c(1), decreasing=TRUE);
  expect_identical(is.null(model), FALSE);
  expect_identical(is(model, "FunctionalModel"), TRUE)
  l <- (1L+1L) + (1L + 1L);
  expect_identical(model@paramCount, l)
  expect_length(model@paramLower, l)
  expect_length(model@paramUpper, l)
  validObject(model)
})

test_that("Test FunctionalModel.mlp(c(1, 2)) dec", {
  model <- FunctionalModel.mlp(c(1, 2), decreasing=TRUE);
  expect_identical(is.null(model), FALSE);
  expect_identical(is(model, "FunctionalModel"), TRUE)
  l <- (1L+1L) + 2L*(1L + 1L) + ((2L*1L) + 1L);
  expect_identical(model@paramCount, l)
  expect_length(model@paramLower, l)
  expect_length(model@paramUpper, l)
  validObject(model)
})


test_that("Test FunctionalModel.mlp(c(2, 2)) dec", {
  model <- FunctionalModel.mlp(c(2, 2), decreasing=TRUE);
  expect_identical(is.null(model), FALSE);
  expect_identical(is(model, "FunctionalModel"), TRUE)
  l <- (2L+2L) + 2L*(2L + 1L) + ((2L*1L) + 1L);
  expect_identical(model@paramCount, l)
  expect_length(model@paramLower, l)
  expect_length(model@paramUpper, l)
  validObject(model)
})
