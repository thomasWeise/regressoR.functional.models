library("regressoR.functional.models")
context("FunctionalModel.mlp.dec")

.do.test <- function(model) {
  validObject(model)
  f <- model@f;
  s <- model@paramCount;
  expect_gt(s, 0L);
  l <- model@paramLower;
  expect_length(l, s);
  u <- model@paramUpper;
  expect_length(u, s);

  for(i in 1:100) {
    x <- sort(runif(n=10));
    y <- runif(n=1L, min=-2L, max=2L) - sort(runif(n=length(x)));

    e <- model@estimator(x, y);
    expect_length(e, s);
    expect_true(FunctionalModel.par.check(model, e));
    z <- f(x, e);
    expect_length(z, length(x));
    for(i in 2L:length(z)) {
      expect_lte(z[i], z[i-1L]);
    }
  }
}


test_that("Test Decreasing FunctionalModel.mlp(c(1))", {
  .do.test( FunctionalModel.mlp(c(1), decreasing=TRUE));
})
test_that("Test Decreasing FunctionalModel.mlp(c(2))", {
  .do.test( FunctionalModel.mlp(c(2), decreasing=TRUE));
})
test_that("Test Decreasing FunctionalModel.mlp(c(3))", {
  .do.test( FunctionalModel.mlp(c(3), decreasing=TRUE));
})

test_that("Test Decreasing FunctionalModel.mlp(c(1, 1))", {
  .do.test( FunctionalModel.mlp(c(1, 1), decreasing=TRUE));
})

test_that("Test Decreasing FunctionalModel.mlp(c(1, 2))", {
  .do.test( FunctionalModel.mlp(c(1, 2), decreasing=TRUE));
})

test_that("Test Decreasing FunctionalModel.mlp(c(1, 2))", {
  .do.test( FunctionalModel.mlp(c(1, 3), decreasing=TRUE));
})


test_that("Test Decreasing FunctionalModel.mlp(c(2, 1))", {
  .do.test( FunctionalModel.mlp(c(2, 1), decreasing=TRUE));
})
test_that("Test Decreasing FunctionalModel.mlp(c(2, 1))", {
  .do.test( FunctionalModel.mlp(c(3, 1), decreasing=TRUE));
})

test_that("Test Decreasing FunctionalModel.mlp(c(1, 1, 1))", {
  .do.test( FunctionalModel.mlp(c(1, 1, 1), decreasing=TRUE));
})

test_that("Test Decreasing FunctionalModel.mlp(c(1, 1, 2))", {
  .do.test( FunctionalModel.mlp(c(1, 1, 2), decreasing=TRUE));
})
test_that("Test Decreasing FunctionalModel.mlp(c(1, 1, 3))", {
  .do.test( FunctionalModel.mlp(c(1, 1, 3), decreasing=TRUE));
})

test_that("Test Decreasing FunctionalModel.mlp(c(1, 2, 1))", {
  .do.test( FunctionalModel.mlp(c(1, 3, 1), decreasing=TRUE));
})
test_that("Test Decreasing FunctionalModel.mlp(c(1, 3, 1))", {
  .do.test( FunctionalModel.mlp(c(1, 3, 1), decreasing=TRUE));
})

test_that("Test Decreasing FunctionalModel.mlp(c(2, 1, 1))", {
  .do.test( FunctionalModel.mlp(c(2, 1, 1), decreasing=TRUE));
})
test_that("Test Decreasing FunctionalModel.mlp(c(3, 1, 1))", {
  .do.test( FunctionalModel.mlp(c(3, 1, 1), decreasing=TRUE));
})

test_that("Test Decreasing FunctionalModel.mlp(c(2, 2, 1))", {
  .do.test( FunctionalModel.mlp(c(2, 2, 1), decreasing=TRUE));
})
test_that("Test Decreasing FunctionalModel.mlp(c(3, 2, 1))", {
  .do.test( FunctionalModel.mlp(c(3, 2, 1), decreasing=TRUE));
})
test_that("Test Decreasing FunctionalModel.mlp(c(3, 3, 1))", {
  .do.test( FunctionalModel.mlp(c(3, 3, 1), decreasing=TRUE));
})
test_that("Test Decreasing FunctionalModel.mlp(c(3, 3, 2))", {
  .do.test( FunctionalModel.mlp(c(3, 3, 2), decreasing=TRUE));
})
test_that("Test Decreasing FunctionalModel.mlp(c(3, 3, 4))", {
  .do.test( FunctionalModel.mlp(c(3, 3, 4), decreasing=TRUE));
})

test_that("Test Decreasing FunctionalModel.mlp(c(1, 2, 2))", {
  .do.test( FunctionalModel.mlp(c(1, 2, 2), decreasing=TRUE));
})

test_that("Test Decreasing FunctionalModel.mlp(c(2, 1, 2))", {
  .do.test( FunctionalModel.mlp(c(2, 1, 2), decreasing=TRUE));
})

test_that("Test Decreasing FunctionalModel.mlp(c(2, 3, 2))", {
  .do.test( FunctionalModel.mlp(c(2, 3, 2), decreasing=TRUE));
})
