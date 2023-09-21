# ingest long format data

    Code
      print(rec_1)
    Message <cliMessage>
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   3
      predictor: 3
      
      -- Operations 
      * Collate long analytical measurements: absorp

---

    Code
      print(summary(rec_1))
    Output
      # A tibble: 6 x 4
        variable    type      role      source  
        <chr>       <list>    <chr>     <chr>   
      1 .sample_num <chr [2]> predictor original
      2 absorp      <chr [2]> predictor original
      3 ind         <chr [2]> predictor original
      4 water       <chr [2]> outcome   original
      5 fat         <chr [2]> outcome   original
      6 protein     <chr [2]> outcome   original

---

    Code
      print(tidy(rec_1))
    Output
      # A tibble: 1 x 6
        number operation type               trained skip  id    
         <int> <chr>     <chr>              <lgl>   <lgl> <chr> 
      1      1 step      measure_input_long FALSE   FALSE potato

---

    Code
      print(prep_1)
    Message <cliMessage>
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   3
      predictor: 3
      
      -- Training information 
      Training data contained 20000 data points and no incomplete rows.
      
      -- Operations 
      * Collate long analytical measurements: absorp, ind | Trained

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
      1      1 step      measure_input_long TRUE    FALSE potato

---

    Code
      recipe(water + fat + protein ~ absorp, data = na_train) %>%
        step_measure_input_long(absorp) %>% prep()
    Error <recipes_error_step>
      Error in `step_measure_input_long()`:
      Caused by error in `prep()`:
      ! 'location' is required for long input data

---

    Code
      recipe(water + fat + protein ~ ., data = na_train) %>% step_measure_input_long(
        dplyr::everything(), location = vars(ind)) %>% prep()
    Error <recipes_error_step>
      Error in `step_measure_input_long()`:
      Caused by error in `check_single_selector()`:
      ! The selection for `...` should only select a single column (6 columns were selected).

---

    Code
      recipe(water + fat + protein ~ ., data = na_train) %>% step_measure_input_long(
        absorp, location = vars(dplyr::everything())) %>% prep()
    Error <recipes_error_step>
      Error in `step_measure_input_long()`:
      Caused by error in `check_single_selector()`:
      ! The selection for `location` should only select a single column (6 columns were selected).

