# ingest wide format data

    Code
      print(rec_1)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:     3
      predictor: 101
      
      -- Operations 
      * Collate wide analytical measurements: x_001:x_100

---

    Code
      print(summary(rec_1))
    Output
      # A tibble: 104 x 4
         variable    type      role      source  
         <chr>       <list>    <chr>     <chr>   
       1 .sample_num <chr [2]> predictor original
       2 x_001       <chr [2]> predictor original
       3 x_002       <chr [2]> predictor original
       4 x_003       <chr [2]> predictor original
       5 x_004       <chr [2]> predictor original
       6 x_005       <chr [2]> predictor original
       7 x_006       <chr [2]> predictor original
       8 x_007       <chr [2]> predictor original
       9 x_008       <chr [2]> predictor original
      10 x_009       <chr [2]> predictor original
      # i 94 more rows

---

    Code
      print(tidy(rec_1))
    Output
      # A tibble: 1 x 6
        number operation type               trained skip  id    
         <int> <chr>     <chr>              <lgl>   <lgl> <chr> 
      1      1 step      measure_input_wide FALSE   FALSE potato

---

    Code
      print(prep_1)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:     3
      predictor: 101
      
      -- Training information 
      Training data contained 200 data points and no incomplete rows.
      
      -- Operations 
      * Collate wide analytical measurements: x_001, x_002, x_003, ... | Trained

---

    Code
      print(summary(prep_1))
    Output
      # A tibble: 5 x 4
        variable    type      role      source  
        <chr>       <list>    <chr>     <chr>   
      1 .sample_num <chr [2]> predictor original
      2 water       <chr [2]> outcome   original
      3 fat         <chr [2]> outcome   original
      4 protein     <chr [2]> outcome   original
      5 .measures   <chr [1]> measure   derived 

---

    Code
      print(tidy(prep_1))
    Output
      # A tibble: 1 x 6
        number operation type               trained skip  id    
         <int> <chr>     <chr>              <lgl>   <lgl> <chr> 
      1      1 step      measure_input_wide TRUE    FALSE potato

---

    Code
      prep(step_measure_input_wide(recipe(water + fat + protein ~ ., data = na_train),
      x_001:x_100, location_values = 1:2))
    Condition
      Error in `step_measure_input_wide()`:
      Caused by error in `prep()`:
      ! 100 columns were selected as inputs but `location_values` has 2 values.

