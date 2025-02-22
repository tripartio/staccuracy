# staccuracy 0.2.2

-   Add `reg_aucroc()` function to calculate AUC for regression models.
-   Corrected bug in p-value calculation. 
-   Rewrote `sa_diff()` output to separate staccuracies from their differences. 
l-  Use the `(r+1)/(n+1)` p-value calculation from [North et al. (2003)](https://pmc.ncbi.nlm.nih.gov/articles/PMC379244/).

# staccuracy 0.2.0

-   Added `sa_diff()` function for bootstrapped-based comparison of staccuracies.
-   `var_type()` is no longer exported since it is really an internal function.

# staccuracy 0.1.0

-   Initial CRAN submission.
