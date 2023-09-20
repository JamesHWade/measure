# Print recipe with step_measure_collect works

    Code
      meats_rec
    Message <cliMessage>
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      descriptor:        3
      undeclared role: 100
      
      -- Operations 
      * Collect measurements: starts_with("x_")

---

    Code
      meats_long_rec
    Message <cliMessage>
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      descriptor:      4
      undeclared role: 2
      
      -- Operations 
      * Collect measurements: channel, transmittance

# Prep recipe with step_measure_collect works

    Code
      meats_prep
    Message <cliMessage>
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      descriptor:        3
      undeclared role: 100
      
      -- Training information 
      Training data contained 215 data points and no incomplete rows.
      
      -- Operations 
      * Collect measurements: x_001, x_002, x_003, x_004, x_005, x_006, ... | Trained

---

    Code
      meats_long_prep
    Message <cliMessage>
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      descriptor:      4
      undeclared role: 2
      
      -- Training information 
      Training data contained 21500 data points and no incomplete rows.
      
      -- Operations 
      * Collect measurements: channel, transmittance | Trained

# Bake recipe with step_measure_collect works

    Code
      meats_bake
    Output
      # A tibble: 215 x 2
         .index          .measures
          <int> <list<tibble[,5]>>
       1      1          [100 x 5]
       2      2          [100 x 5]
       3      3          [100 x 5]
       4      4          [100 x 5]
       5      5          [100 x 5]
       6      6          [100 x 5]
       7      7          [100 x 5]
       8      8          [100 x 5]
       9      9          [100 x 5]
      10     10          [100 x 5]
      # i 205 more rows

---

    Code
      meats_long_bake
    Output
      # A tibble: 1 x 2
        .index .measures            
         <int> <list>               
      1      1 <tibble [21,500 x 6]>

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

