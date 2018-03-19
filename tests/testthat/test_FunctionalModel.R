library("regressoR.functional.models")
context("FunctionalModel")

test_that("Test FunctionalModel constructor", {
  expect_error(validObject(new ("FunctionalModel")))
  f1<-function(x, par) { x }
  f2<-function(x, par) { x*par[1] }
  ob1<-methods::new ("FunctionalModel", f=f1, paramCount=1L)
  ob2<-methods::new ("FunctionalModel", f=f2, paramCount=3L)
  methods::validObject(ob1)
  methods::validObject(ob2)
  expect_identical(ob1@f, f1)
  expect_identical(ob1@paramCount, 1L)
  expect_identical(ob2@f, f2)
  expect_identical(ob2@paramCount, 3L)
  f3<-function(x) { x }
  grad<-function(x, par) { x*par }
  expect_error(validObject(new ("FunctionalModel", f=f3, paramCount=1L)))
  obj3 <- new ("FunctionalModel", f=f2, paramCount=3L, gradient=grad)
  obj4 <- new ("FunctionalModel", f=f2, paramCount=3L, gradient=grad, paramUpper=c(1,2,3))
  obj5 <- new ("FunctionalModel", f=f2, paramCount=3L, gradient=grad, paramLower=c(1,2,3), paramUpper=c(3,4,5))
  estimator=function(x,y,paramLower,paramUpper) c(3,4,5);
  obj6 <- new ("FunctionalModel", f=f2, paramCount=3L, gradient=grad, paramLower=c(1,2,3), paramUpper=c(3,4,5), estimator=estimator)

  expect_identical(obj3@f, f2);
  expect_identical(obj3@paramCount, 3L);
  expect_identical(obj3@gradient, grad);
  expect_null(obj3@paramLower);
  expect_null(obj3@paramUpper);
  expect_null(obj3@estimator);

  expect_identical(obj4@f, f2);
  expect_identical(obj4@paramCount, 3L);
  expect_identical(obj4@gradient, grad);
  expect_null(obj4@paramLower);
  expect_identical(obj4@paramUpper, c(1, 2, 3));
  expect_null(obj4@estimator);

  expect_identical(obj5@f, f2);
  expect_identical(obj5@paramCount, 3L);
  expect_identical(obj5@gradient, grad);
  expect_null(obj5@estimator);
  expect_identical(obj5@paramLower, c(1, 2, 3));
  expect_identical(obj5@paramUpper, c(3, 4, 5));

  expect_identical(obj6@f, f2);
  expect_identical(obj6@paramCount, 3L);
  expect_identical(obj6@gradient, grad);
  expect_identical(obj6@estimator, estimator);
  expect_identical(obj6@paramLower, c(1, 2, 3));
  expect_identical(obj6@paramUpper, c(3, 4, 5));

  validObject(obj3)
  validObject(obj4)
  validObject(obj5)
  validObject(obj6)
})


test_that("Test FunctionalModel.new ", {
  expect_error(FunctionalModel.new())
  f1<-function(x, par) { x }
  f2<-function(x, par) { x*par[1] }
  ob1<-FunctionalModel.new(f=f1, paramCount=1L)
  ob2<-FunctionalModel.new(f=f2, paramCount=3L)
  methods::validObject(ob1)
  methods::validObject(ob2)
  expect_identical(ob1@f, f1)
  expect_identical(ob1@paramCount, 1L)
  expect_identical(ob2@f, f2)
  expect_identical(ob2@paramCount, 3L)
  f3<-function(x) { x }
  grad<-function(x, par) { x*par }
  expect_error(FunctionalModel.new(f=f3, paramCount=1L))
  obj3 <- FunctionalModel.new(f=f2, paramCount=3L, gradient=grad)
  obj4 <- FunctionalModel.new(f=f2, paramCount=3L, gradient=grad, paramUpper=c(1,2,3))
  obj5 <- FunctionalModel.new(f=f2, paramCount=3L, gradient=grad, paramLower=c(1,2,3), paramUpper=c(3,4,5))
  estimator=function(x,y,paramLower,paramUpper) c(3,4,5);
  obj6 <- FunctionalModel.new(f=f2, paramCount=3L, gradient=grad, paramLower=c(1,2,3), paramUpper=c(3,4,5), estimator=estimator)

  expect_identical(obj3@f, f2);
  expect_identical(obj3@paramCount, 3L);
  expect_identical(obj3@gradient, grad);
  expect_null(obj3@paramLower);
  expect_null(obj3@paramUpper);
  expect_null(obj3@estimator);

  expect_identical(obj4@f, f2);
  expect_identical(obj4@paramCount, 3L);
  expect_identical(obj4@gradient, grad);
  expect_null(obj4@paramLower);
  expect_identical(obj4@paramUpper, c(1, 2, 3));
  expect_null(obj4@estimator);

  expect_identical(obj5@f, f2);
  expect_identical(obj5@paramCount, 3L);
  expect_identical(obj5@gradient, grad);
  expect_null(obj5@estimator);
  expect_identical(obj5@paramLower, c(1, 2, 3));
  expect_identical(obj5@paramUpper, c(3, 4, 5));

  expect_identical(obj6@f, f2);
  expect_identical(obj6@paramCount, 3L);
  expect_identical(obj6@gradient, grad);
  expect_identical(obj6@estimator, estimator);
  expect_identical(obj6@paramLower, c(1, 2, 3));
  expect_identical(obj6@paramUpper, c(3, 4, 5));

  validObject(obj3)
  validObject(obj4)
  validObject(obj5)
  validObject(obj6)
})
