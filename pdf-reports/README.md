# Analyzing debit card overdrafts

A local bank sought to investigate whether low-income consumers overdrafted more frequently than middle- or upper-income consumers. In this analysis, I use _(a,b,0)_ families and generalized linear models to investigate this hypothesis.

- "analyzing*overdrafts.R" explores using *(a,b,0)\_ families to model probability distributions as well as negative binomial GLM regressions to test for significant differences across income levels.
- "overdraft_GLMs.R" compares Poisson, quasi-Poisson, negative binomial, zero-adjusted, and zero-inflated models (ZAP, ZANB, ZIP, ZINB). I concluded that the negative binomial model was the best model for this particular data set.
- "overdraft_GLMs.pdf" describes the results of "overdraft_GLMs.R"

# Techniques this code uses

- SQL to create tables (using R package _sqldf_)
- _ggplot2_ to visualize overdrafts
- _xtable_ to process tables for LaTeX output
- Bootstrapping
- Chi-square analysis
- Weighted least squares regression
- Negative binomial regression
- Zero-adjusted and zero-inflated regression models
- Akaike information criterion for model selection
- Likelihood ratio tests to compare nested regression models

_Note_: Zero-adjusted poisson (ZAP) or zero-adjusted negative binomial (ZANB) models are also referred to as hurdle models.
