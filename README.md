# modeLLtest [![Build Status](https://travis-ci.com/ShanaScogin/modeLLtest.svg?branch=master)](https://travis-ci.com/ShanaScogin/modeLLtest)
An R Package which implements model comparison tests using cross-validated log likelihood (CVLL) values. 

# Test Overviews
An R package which implements model comparison tests. The tests include the cross-validated difference in means (CVDM) test, the cross-validated median fit (CVMF) test, and bias corrected Vuong tests. The relevant papers including details can be found below:

* Harden, J. J., & Desmarais, B. A. (2011). Linear Models with Outliers: Choosing between Conditional-Mean and Conditional-Median Methods. State Politics & Policy Quarterly, 11(4), 371-389.

* Desmarais, B. A., & Harden, J. J. (2012). Comparing partial likelihood and robust estimation methods for the Cox regression model. Political Analysis, 20(1), 113-135.

* Desmarais, B. A., & Harden, J. J. (2013). Testing for zero inflation in count models: Bias correction for the Vuong test. The Stata Journal, 13(4), 810-835.

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

After the package is loaded, check out the `?modeLLtest` to see a help file. You can also see the documentation for the functions with `?cvdm` or `?cvmf`. If you have issues, please email me.

<!-- # Basic Usage -->

<!-- # Examples -->

# What's Happening
This is a beta version of the modeLLtest package. Currently, the two main model comparison tests, cvdm() and cvmf(), are up and running. Two general functions, cvll() and cvll_dm() are also available. Next steps include adding options to cvdm() and improving the performance of the functions. Check back for details. 

# Contact
Please contact sscogin@nd.edu with questions or comments.
