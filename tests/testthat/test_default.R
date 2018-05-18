library("regressoR.functional.models")
context("FunctionalModel.default")

test_that("Test FunctionalModel.default", {
  models <- FunctionalModel.default();
  expect_true(!is.null(models));
  expect_true(is.list(models));
  expect_gt(length(models), 0L);

  for(model in models) {
    expect_is(model, "FunctionalModel");
    expect_true(!is.null(model));
    expect_true(!is.null(model@f));
    expect_gt(model@paramCount, 0L);
    expect_true(is.integer(model@paramCount));
    validObject(model);
  }
})

test_that("Test FunctionalModel.monotonous", {
  models <- FunctionalModel.monotonous();
  expect_true(!is.null(models));
  expect_true(is.list(models));
  expect_gt(length(models), 0L);

  for(model in models) {
    expect_is(model, "FunctionalModel");
    expect_true(!is.null(model));
    expect_true(!is.null(model@f));
    expect_gt(model@paramCount, 0L);
    expect_true(is.integer(model@paramCount));
    validObject(model);
  }
})
