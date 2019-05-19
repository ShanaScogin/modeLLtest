# modeLLtest [![Build Status](https://travis-ci.com/ShanaScogin/modeLLtest.svg?branch=master)](https://travis-ci.com/ShanaScogin/modeLLtest)
An R Package which implements model comparison tests using cross-validated log-likelihood (CVLL) values. 

# Test Overviews
An R package which implements model comparison tests. `modeLLtest` includes functions for the cross-validated difference in means (CVDM) test and the cross-validated median fit (CVMF) test. Other tools include a function to output a vector of cross-validated log-likelihood (CVLL) values and a function to perform the CVDM test on an input of two CVLL vectors. The relevant papers including details can be found below:

* Harden, J. J., & Desmarais, B. A. (2011). Linear Models with Outliers: Choosing between Conditional-Mean and Conditional-Median Methods. State Politics & Policy Quarterly, 11(4), 371-389.

* Desmarais, B. A., & Harden, J. J. (2012). Comparing partial likelihood and robust estimation methods for the Cox regression model. Political Analysis, 20(1), 113-135.

* Desmarais, B. A., & Harden, J. J. (2014). An unbiased model comparison test using cross-validation. Quality & Quantity, 48(4), 2155-2173.

# Installing The Package
Currently the package can be downloaded with the devtools package in R from GitHub. To do this, install devtools by calling:

```
install.packages("devtools")
```

Now we can install from GitHub with the following line:

```
devtools::install_github("ShanaScogin/modeLLtest")
```

Once you have installed the package, you can access it by calling:

```
library(modeLLtest)
```

After the package is loaded, check out the `?modeLLtest` to see a help file. You can also see the documentation for the functions with `?cvdm`, `?cvll`, `?cvlldiff`, or `?cvmf`. If you have issues or questions, please email me at sscogin@nd.edu.

# Basic Usage
This package has four main functions: `cvdm()`, `cvll()`, `cvmf()`, and `cvlldiff()`. The function `cvdm()` deploys the CVDM test, which uses a bias-corrected Johnson's t-test to choose between the leave-one-out cross-validated log-likelihood outputs of two non-nested models. The function `cvll()` outputs a vector of leave-one-out cross-validated log-likelihoods for a given method. Currently, these functions accommodate linear regression, median regression (from the package `quantreg`), and two methods of robust regression (from the package `MASS`). The`cvlldiff()` function performs the bias-corrected Johnson's t-test on two vectors of cross-validated log-likelihoods. Finally, the `cvmf()` function test between the partial likelihood maximization (PLM) and the iteratively reweighted robust (IRR) methods of estimation for a given application of the Cox model

# Examples
Here are some examples of the functions in this package. First, we'll look at `cvdm()`, which applies cross-validated log-likelihood difference in means (CVDM) test to compare two methods of estimating a formula. The example compares ordinary least squares (OLS) estimation to median regression (MR).

```
library(modeLLtest)
set.seed(123456)
b0 <- .2 # True value for the intercept
b1 <- .5 # True value for the slope
n <- 500 # Sample size
X <- runif(n, -1, 1)

Y <- b0 + b1 * X + rnorm(n, 0, 1) # N(0, 1 error)

obj_cvdm <- cvdm(Y ~ X, data.frame(cbind(Y, X)), method1 = "OLS", method2 = "MR")

obj_cvdm
  
```

Next, let's do the same as we did above, but with `cvll()` and `cvlldiff()`. These are general functions that create vectors of the cross-validated log-likelihood functions and then compute the bias-corrected Johnson's t-test, respectively.

```
library(modeLLtest)
set.seed(123456)
b0 <- .2 # True value for the intercept
b1 <- .5 # True value for the slope
n <- 500 # Sample size
X <- runif(n, -1, 1)

Y <- b0 + b1 * X + rnorm(n, 0, 1) # N(0, 1 error)

obj_cvll_ols <- cvll(Y ~ X, data.frame(cbind(Y, X)), method = "OLS")

obj_cvll_mr <- cvll(Y ~ X, data.frame(cbind(Y, X)), method = "MR")

obj_cvlldiff <- cvlldiff(obj_cvll_ols$cvll, obj_cvll_mr$cvll,
                           obj_cvll_ols$df)

obj_cvlldiff
```

Finally, let's look at the `cvmf()` function. This function compares the partial likelihood maximization (PLM) and the iteratively reweighted robust (IRR) methods of estimation for a given application of the Cox model.

```
library(modeLLtest)
library(survival)

set.seed(12345)
x1 <- rnorm(100) 
x2 <- rnorm(100)
x2e <- x2 + rnorm(100, 0, 0.5)

y <- rexp(100, exp(x1 + x2))
y <- Surv(y) # Changing y into a survival obj with survival package

dat <- data.frame(y, x1, x2e)
form <- y ~ x1 + x2e

results <- cvmf(formula = form, data = dat)

results
```

# What's Happening
Future updates include optimizing functions and adding options to `cvdm()` and `cvll()`. Check back for details. 

# Contact
Please contact sscogin@nd.edu with questions or comments.
