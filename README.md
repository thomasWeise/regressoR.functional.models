# R Package Providing an Blueprints and Tools for Functional Models for Regression

[<img alt="Travis CI Build Status" src="https://img.shields.io/travis/thomasWeise/regressoR.functionalModels/master.svg" height="20"/>](https://travis-ci.org/thomasWeise/regressoR.functionalModels/)

## Introduction
In this package, we provide blueprints and tools for defining functional models for regression. First, there is a base class called `FunctionalModel` which allows specifying a model function with parameters, boundaries for the model parameters, a gradient function and a parameter guesser. Second, there are some utility methods that can be used to validate and guess parameters of such a model. Third, we provide a set of basic model instances of the model blueprints that can be used. This set will be continuously extented in the future.
    
## Installation
You can install the package directl from GitHub by using the package
[`devtools`](http://cran.r-project.org/web/packages/devtools/index.html) as
follows:

    library(devtools)
    install_github("thomasWeise/regressoR.functionalModels")

If `devtools` is not yet installed on your machine, you need to FIRST do

    install.packages("devtools")
    
## License

The copyright holder of this package is Prof. Dr. Thomas Weise (see Contact).
The package is licensed under the  GNU LESSER GENERAL PUBLIC LICENSE Version 3, 29 June 2007.
    
## Contact

If you have any questions or suggestions, please contact
[Prof. Dr. Thomas Weise](http://iao.hfuu.edu.cn/team/director) of the
[Institute of Applied Optimization](http://iao.hfuu.edu.cn/) at
[Hefei University](http://www.hfuu.edu.cn) in
Hefei, Anhui, China via
email to [tweise@hfuu.edu.cn](mailto:tweise@hfuu.edu.cn).
