# output long format data

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
      * Restructure analytical measurements to long format: "<internal data>"

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
      # A tibble: 2 x 6
        number operation type                trained skip  id    
         <int> <chr>     <chr>               <lgl>   <lgl> <chr> 
      1      1 step      measure_input_long  FALSE   FALSE potato
      2      2 step      measure_output_long FALSE   FALSE turnip

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
      Training data contained 400 data points and no incomplete rows.
      
      -- Operations 
      * Collate long analytical measurements: absorp and ind | Trained
      * Restructure analytical measurements to long format: ~"<internal data>" |
        Trained

---

    Code
      print(summary(prep_1))
    Output
      # A tibble: 6 x 4
        variable    type      role      source  
        <chr>       <list>    <chr>     <chr>   
      1 .sample_num <chr [2]> predictor original
      2 water       <chr [2]> outcome   original
      3 fat         <chr [2]> outcome   original
      4 protein     <chr [2]> outcome   original
      5 .measure    <chr [2]> predictor derived 
      6 .location   <chr [2]> predictor derived 

---

    Code
      print(tidy(prep_1))
    Output
      # A tibble: 2 x 6
        number operation type                trained skip  id    
         <int> <chr>     <chr>               <lgl>   <lgl> <chr> 
      1      1 step      measure_input_long  TRUE    FALSE potato
      2      2 step      measure_output_long TRUE    FALSE turnip

