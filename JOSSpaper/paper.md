---
title: 'modeLLtest: An R Package for Unbiased Model Comparison using Cross Validation '
authors:
- affiliation: 1
  name: Shana Scogin
  orcid: 0000-0002-7801-853X
- affiliation: 2
  name: Sarah Petersen
  orcid: 0000-0002-8811-3485
- affiliation: 1
  name: Jeffrey J. Harden
  orcid: 0000-0001-5337-7918
- affiliation: 3
  name: Bruce A. Desmarais
  orcid: 0000-0002-3031-8883
date: "June 18, 2019"
output: pdf_document
bibliography: cvll.bib
tags:
- model comparison
- model selection
- cross-validation
- leave-one-out
- median regression
- cox proportional hazards model
affiliations:
- index: 1
  name: University of Notre Dame, Department of Political Science
- index: 2
  name: University of Notre Dame, Department of Mathematics
- index: 3
  name: Pennsylvania State University, Department of Political Science
---

# Summary

Selection among statistical models describing the same process is a crucial methodological step in quantitative research. Because different estimators for the same process are commonly available, researchers have developed tests for selecting among models. The R package ``modeLLtest`` implements a variety of such tests using leave-one-out cross-validation (LOOCV) to adjudicate among estimation methods. LOOCV accounts for outliers in data by comparing results across samples in which one observation has been left out [@diebold_comparing_2002].

The R package ``modeLLtest`` has three main functionalities. It implements an unbiased comparison of two linear parametric, non-nested models [@harden_linear_2011; @desmarais_harden_unbiased_2014], a test comparing two estimations of the Cox proportional hazards model - the conventional partial likelihood maximization (PLM) and the iteratively reweighted robust model (IRR) [@desmarais_comparing_2012], and a set of more general functions for comparison of arbitrary models. In addition to making these comparison tests easily accessible in an R package, we improve the running speed of selected functions by making use of C++ to perform LOOCV before pulling the output into R. As a result, these tests now run in a reasonable amount of time on large data sets. Researchers can use the fast LOOCV results as inputs to alternative and new model testing procedures. 

#### Unbiased linear model comparison

When models are parametric and non-nested, the null hypothesis that two models fit the data equally well has often been tested using methods introduced by Vuong [-@vuong_likelihood_1989] and Clarke [-@clarke_testing_2001; -@clarke_nonparametric_2003; -@clarke_simple_2007]. These methods compare the Kullback-Leibler Divergence (KLD) of the two models from the true model that generated the data. However, more recently, Desmarais and Harden [-@desmarais_harden_unbiased_2014] showed the estimate of the KLD used by Vuong, the individual log-likelihood contributions, is biased. They also showed the Clarke test to be inconsistent when applied to the difference in KLDs. As a solution, Desmarais and Harden [-@desmarais_harden_unbiased_2014] derived a test based on LOOCV contributions, creating an unbiased KLD estimate. The implementation of Desmarais and Harden's test comparing two methods of estimation is a major component of ``modeLLtest``. A primary example of the test's use within this context is in the adjudication among ordinary least squares (OLS) regression, robust regression, and the less-commonly used median regression (MR) [@harden_linear_2011; @koenker_quantile_2005; @hao_quantile_2007]. For researchers looking to make more informed decisions regarding model fit rather than relying on conventionally-used methods such as OLS alone, ``modeLLtest`` significantly improves the ease of choosing among estimation methods. 

####  Cox proportional hazards model

The Cox model is a partial parametric model that does not make assumptions about the baseline hazard. The conventional partial likelihood maximization (PLM) and iteratively reweighted robust (IRR) estimation methods represent two alternatives for parameter estimation. Either could be optimal be optimal based on characteristics of the data. Plausible deviations from correct specification and operationalization, which can be caused by issues such as measurement error or omitted variables, can produce substantial bias when the Cox model is estimated by PLM. However, using the IRR estimator in the absence of these problems can result in a loss of efficiency. For this reason, Desmarais and Harden [-@desmarais_comparing_2012] developed a novel cross-validated median fit (CVMF) test for selecting between the two estimators.  The R package ``modeLLtest`` provides implementation of this test with original contributions which include options to specify methods other than Efron approximation (ex. Breslow approx.), specify the truncation or trimming level for the robust estimator, adjust fitting weights, and run the test on only a subset of data. 

#### General tools

In addition to providing the above tests, ``modeLLtest`` also contains tools for model comparison in a more general setting. Given any model specification and estimation method as inputs, ``modeLLtest`` can create a vector of LOOCV log-likelihood estimates. These vectors subsequently can be fed to a function that performs the unbiased Johnson t-test [@johnson1978; see also @desmarais_harden_unbiased_2014] on the two vectors of log-likelihoods in order to select between models. 

#### Functionality and use

The R package ``modeLLtest`` is designed to be used by both researchers and students looking to compare models or create LOOCV log-likelihood estimates for a given model. The ease and speed of implementing selected tests are significantly improved through the use of C++ to obtain LOOCV log-likelihood estimates. The source code is on [GitHub](https://github.com/ShanaScogin/modeLLtest). The documentation can be found on [GitHub Pages](https://github.com/ShanaScogin/modeLLtest/blob/master/README.md). Functions can be tested using data from real-world analyses from Joshi and Mason [-@joshi_mason_2008] and Golder [-@golder_2010].

#### Future work

Future work will include providing implementation for additional models to be used in the CVDM and CVLL functions. 

# Acknowledgements

We thank Andrew Decker for his helpful comments and advice.

# References

