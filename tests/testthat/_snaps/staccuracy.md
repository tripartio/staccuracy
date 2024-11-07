# sa_diff() works correctly

    Code
      sa_diff(attitude$rating, list(all = predict(lm_attitude_all), madv = predict(
        lm_attitude__a), mcmp = predict(lm_attitude__c)), boot_it = 10)
    Output
      # A tibble: 12 x 11
         staccuracy    pred  diff            lo    mean     hi     p01     p02     p03
         <chr>         <chr> <chr>        <dbl>   <dbl>  <dbl>   <dbl>   <dbl>   <dbl>
       1 WinMAE on MAD all   <NA>       0.672   0.719   0.776  NA      NA      NA     
       2 WinMAE on MAD madv  <NA>       0.640   0.705   0.767  NA      NA      NA     
       3 WinMAE on MAD mcmp  <NA>       0.586   0.635   0.692  NA      NA      NA     
       4 WinMAE on MAD <NA>  all-madv  -0.00660 0.0139  0.0369  0.455   0.727   0.818 
       5 WinMAE on MAD <NA>  all-mcmp   0.0440  0.0840  0.133   0.0909  0.0909  0.0909
       6 WinMAE on MAD <NA>  madv-mcmp  0.0291  0.0702  0.122   0.0909  0.0909  0.182 
       7 WinRMSE on SD all   <NA>       0.684   0.737   0.781  NA      NA      NA     
       8 WinRMSE on SD madv  <NA>       0.670   0.732   0.782  NA      NA      NA     
       9 WinRMSE on SD mcmp  <NA>       0.616   0.670   0.723  NA      NA      NA     
      10 WinRMSE on SD <NA>  all-madv  -0.00781 0.00529 0.0272  0.636   0.909   0.909 
      11 WinRMSE on SD <NA>  all-mcmp   0.0335  0.0666  0.107   0.0909  0.0909  0.182 
      12 WinRMSE on SD <NA>  madv-mcmp  0.0273  0.0613  0.108   0.0909  0.0909  0.182 
      # i 2 more variables: p04 <dbl>, p05 <dbl>

