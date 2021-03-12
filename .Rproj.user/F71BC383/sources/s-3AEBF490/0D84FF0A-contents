# IntroAnalysis
Wrappers for estimating parameters and comparing nested models (conducting hypothesis tests) when working with models suitable for an introductory course on statistical analysis.


## Overview
The `IntroAnalysis` package was constructed to support an introductory statistics course which takes a modeling-based perspective.  As an example, consider making inference about a single mean \(\mu\) in a population.  The classical approach is to provide formulas for a \(1-\alpha\) confidence interval

\[\bar{x} \pm Z_{1-\alpha/2} \frac{s}{\sqrt{n}}\]

or investigating the distribution of the standardized statistic

\[\frac{\bar{x} - \mu_0}{s/\sqrt{n}}.\]

Inference for a single population is then divorced from inference in simple linear regression.

Alternatively, a modeling-based perspective considers the model for the data generating process

\[(\text{Response})_i = \mu + \varepsilon_i\]

where we are willing to impose some conditions on the error terms.  This allows all of the introductory course to be considered from the same perspective.  

The `IntroAnalysis` package provides wrappers for estimating perameters and comparing nested models when working with linear and generalized linear models.  These wrappers provide a single interface for performing statistical inference imposing the classical conditions or via bootstrapping.  


## Example
Suppose we are interested in computing a 95% confidence interval (assuming the population follows a Normal distribution) for the mean \(\mu\).  We assume the response variable `Response` is stored in the dataframe `df`.  We first specify the model for the data generating process

```
mean.model = specify_mean_model(Response ~ 1, data = df)
```

We then estimate the parameters

```
estimate_parameters(
  mean.model, 
  confidence.level = 0.95,
  assume.identically.distributed = TRUE,
  assume.normality = TRUE)
```

The above code specifies the assumptions made for the classic 1-sample t-interval.  

Now, suppose we are interested in testing the hypotheses

\[H_0: \mu = 5 \qquad \text{vs.} \qquad H_1: \mu \neq 5\]

but we are not willing to assume the population is Normally distributed.  Then, we can perform a residual bootstrap.  We have already specified the model for the data generating process above, but we need to specify the model under the null hypothesis

```
mean.model.h0 = specify_mean_model(Response ~ constant(5), data = df)
```

Notice that there are no parameters to estimate in the above model.  Now, we proceed to performing the hypothesis test by comparing the models

```
compare_models(
  mean.model,
  mean.model.h0,
  assume.identically.distributed = TRUE,
  assume.normality = FALSE)
```

The code is similar to a classical t-test, emphasizing the same process but different conditions being imposed on the error term.


## Installation
The package can be installed from Github.

```
library(devtools)
install_github("reyes/IntroAnalysis")
```

Once installed, we recommend starting with the _Essential Inference using Linear Models_ vignette or the _Coding Cookbook_ vignette.
