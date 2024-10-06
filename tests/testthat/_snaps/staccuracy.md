# sa_diff() works correctly

    Code
      sa_diff(attitude$rating, list(all = predict(lm_attitude_all), madv = predict(
        lm_attitude__c)), boot_it = 10)
    Output
      # A tibble: 2 x 15
        staccuracy        all_lo all_mn all_hi madv_lo madv_mn madv_hi diff_lo diff_mn
        <chr>              <dbl>  <dbl>  <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
      1 Staccuracy WinMA~  0.672  0.719  0.776   0.586   0.635   0.692  0.0440  0.0840
      2 Staccuarcy WinRM~  0.684  0.737  0.781   0.616   0.670   0.723  0.0335  0.0666
      # i 6 more variables: diff_hi <dbl>, p_01 <dbl>, p_02 <dbl>, p_03 <dbl>,
      #   p_04 <dbl>, p_05 <dbl>

