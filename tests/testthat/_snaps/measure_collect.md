# Printing recipe with step_measure_collect works

    Code
      rec
    Output
      Recipe
      
      Inputs:
      
             role #variables
       descriptor          3
      
        100 variables with undeclared roles
      
      Operations:
      
      Collect measurements starts_with("x_")

# Prepping recipe with step_measure_collect works

    Code
      prep(rec)
    Message <rlang_message>
      Identifiers: water, fat, and protein
      Measures: x_001, x_002, x_003, x_004, x_005, x_006, x_007, x_008, x_009, x_010, x_011, x_012, x_013, x_014, x_015, x_016, x_017, x_018, ..., x_099, and x_100
      baking prep
      Shape: character
      Shape: wide
      Identifiers: character
      Shape: water, fat, and protein
    Output
      Recipe
      
      Inputs:
      
             role #variables
       descriptor          3
      
        100 variables with undeclared roles
      
      Training data contained 215 data points and no missing data.
      
      Operations:
      
      Collect measurements <none> [trained]

# Baking recipe with step_measure_collect works

    Code
      bake(prep(rec), new_data = NULL)
    Message <rlang_message>
      Identifiers: water, fat, and protein
      Measures: x_001, x_002, x_003, x_004, x_005, x_006, x_007, x_008, x_009, x_010, x_011, x_012, x_013, x_014, x_015, x_016, x_017, x_018, ..., x_099, and x_100
      baking prep
      Shape: character
      Shape: wide
      Identifiers: character
      Shape: water, fat, and protein
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

