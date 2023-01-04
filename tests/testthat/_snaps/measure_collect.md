# Print recipe with step_measure_collect works

    Code
      meats_rec
    Output
      Recipe
      
      Inputs:
      
             role #variables
       descriptor          3
      
        100 variables with undeclared roles
      
      Operations:
      
      Collect measurements starts_with("x_")

---

    Code
      meats_long_rec
    Output
      Recipe
      
      Inputs:
      
             role #variables
       descriptor          4
      
        2 variables with undeclared roles
      
      Operations:
      
      Collect measurements channel, transmittance

# Prep recipe with step_measure_collect works

    Code
      meats_prep
    Output
      Recipe
      
      Inputs:
      
             role #variables
       descriptor          3
      
        100 variables with undeclared roles
      
      Training data contained 215 data points and no missing data.
      
      Operations:
      
      Collect measurements x_001, x_002, x_003, x_004, x_005, x_006, x_007... [trained]

---

    Code
      meats_long_prep
    Output
      Recipe
      
      Inputs:
      
             role #variables
       descriptor          4
      
        2 variables with undeclared roles
      
      Training data contained 21500 data points and no missing data.
      
      Operations:
      
      Collect measurements channel, transmittance [trained]

# Bake recipe with step_measure_collect works

    Code
      meats_bake
    Output
      # A tibble: 215 x 5
         water   fat protein .index          .measures
         <dbl> <dbl>   <dbl>  <int> <list<tibble[,2]>>
       1  60.5  22.5    16.7      1          [100 x 2]
       2  46    40.1    13.5      2          [100 x 2]
       3  71     8.4    20.5      3          [100 x 2]
       4  72.8   5.9    20.7      4          [100 x 2]
       5  58.3  25.5    15.5      5          [100 x 2]
       6  44    42.7    13.7      6          [100 x 2]
       7  44    42.7    13.7      7          [100 x 2]
       8  69.3  10.6    19.3      8          [100 x 2]
       9  61.4  19.9    17.7      9          [100 x 2]
      10  61.4  19.9    17.7     10          [100 x 2]
      # ... with 205 more rows

---

    Code
      meats_long_bake
    Output
      # A tibble: 215 x 6
            id water   fat protein .index          .measures
         <int> <dbl> <dbl>   <dbl>  <int> <list<tibble[,2]>>
       1     1  60.5  22.5    16.7      1          [100 x 2]
       2     2  46    40.1    13.5      2          [100 x 2]
       3     3  71     8.4    20.5      3          [100 x 2]
       4     4  72.8   5.9    20.7      4          [100 x 2]
       5     5  58.3  25.5    15.5      5          [100 x 2]
       6     6  44    42.7    13.7      6          [100 x 2]
       7     7  44    42.7    13.7      7          [100 x 2]
       8     8  69.3  10.6    19.3      8          [100 x 2]
       9     9  61.4  19.9    17.7      9          [100 x 2]
      10    10  61.4  19.9    17.7     10          [100 x 2]
      # ... with 205 more rows

# Tidy recipe with step_measure_collect works

    Code
      tidy(meats_rec)
    Output
      # A tibble: 1 x 6
        number operation type            trained skip  id                   
         <int> <chr>     <chr>           <lgl>   <lgl> <chr>                
      1      1 step      measure_collect FALSE   FALSE measure_collect_Bp5vK

---

    Code
      tidy(meats_long_rec)
    Output
      # A tibble: 1 x 6
        number operation type            trained skip  id                   
         <int> <chr>     <chr>           <lgl>   <lgl> <chr>                
      1      1 step      measure_collect FALSE   FALSE measure_collect_RUieL

