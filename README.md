# monotone-npiv

An implementation of (one version of) the estimator proposed in "Nonparametric Instrumental Variable Estimation Under Monotonicity" by [Chetverikov and Wilhelm (2017)](https://onlinelibrary.wiley.com/doi/abs/10.3982/ECTA13639) and a simulation of its performance for partially flat functions.

To run, make sure you have a base R distribution and the required packages:

```R
install.packages("tidyverse")
install.packages("stargazer")
```

Then, save the scripts in the same directory and run `main.R`.
You can set some simulation parameters at the beginning of `main.R`.
Running will create several (currently 27) `.tex` files containing the simulation results.