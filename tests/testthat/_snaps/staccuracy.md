# sa_diff() works correctly

    Code
      sa_diff(attitude$rating, list(all = predict(lm_attitude_all), madv = predict(
        lm_attitude__c)), boot_it = 10)
    Output
      # A tibble: 6 x 11
        staccuracy    pred     type      lo   mean    hi   p01   p02   p03   p04   p05
        <chr>         <chr>    <chr>  <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
      1 WinMAE on MAD all      pred  0.672  0.719  0.776    NA    NA  NA    NA    NA  
      2 WinMAE on MAD madv     pred  0.586  0.635  0.692    NA    NA  NA    NA    NA  
      3 WinMAE on MAD all-madv diff  0.0440 0.0840 0.133     0     0   0     0     0.1
      4 WinRMSE on SD all      pred  0.684  0.737  0.781    NA    NA  NA    NA    NA  
      5 WinRMSE on SD madv     pred  0.616  0.670  0.723    NA    NA  NA    NA    NA  
      6 WinRMSE on SD all-madv diff  0.0335 0.0666 0.107     0     0   0.1   0.1   0.4

