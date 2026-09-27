# modeLLtest 1.0.6

* Registered the S3 print methods for cvdm, cvll, cvlldiff, and cvmf objects in NAMESPACE, so printing a result now shows the formatted output instead of the raw list. No changes to computations or returned objects.
* print.cvmf() now reports the robust estimator's extended Wald test (ewald.test from coxrobust::coxr()) on the "Extended Wald test" line; it previously showed the partial-likelihood Wald statistic.
* Long calls now print on separate lines instead of being run together.
* Added summary() methods for cvdm, cvll, cvlldiff, and cvmf objects, each with a matching print() method. They report the stored test results in more detail (e.g., the test statistic, degrees of freedom, and coefficient tables) and return them as a list for programmatic use.
* TODO: ADD MORE ABOUT CVLL IF ADJUST IT FOR RUNTIME HERE

# modeLLtest 1.0.5
* Updated requirements for C++ (in makevars files) on request from CRAN
* Added a note about the typo in the formula in the paper (but not the code) into documentation for cvdm.R and cvll.R
* Made minor fixes, such as changing if() conditions when comparing class() to string

# modeLLtest 1.0.4

* Took out Travis and updated to gh-actions
* Made minor edits, including removing onload citation and cleaning documentation

# modeLLtest 1.0.3

* Updated typos in data files and cleaned documentation

# modeLLtest 1.0.2

* Added a `NEWS.md` file to track changes to the package
* Updated documentation 

# modeLLtest 1.0.1

* JOSS-accepted release. 
* Added minor changes to increase community access such as CONTRIBUTING file, CODE_OF_CONDUCT file, and issues 

# modeLLtest 1.0.0

* Initial CRAN release
