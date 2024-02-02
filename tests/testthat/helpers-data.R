data_meat_long <- function() {
  skip_if_not_installed("modeldata")
  data(meats, package = "modeldata")

  library(tidyr)
  library(dplyr)

  inds <- tibble(
    temp = grep("^x_", names(meats), value = TRUE),
    ind = seq(1, 17, length.out = 100)
  )

  meats_2 <-
    meats %>%
    mutate(.sample_num = row_number()) %>%
    pivot_longer(
      cols = c(x_001:x_100),
      names_to = "temp",
      values_to = "absorp"
    ) %>%
    full_join(inds, by = "temp") %>%
    select(-temp)

  inds <- tibble(
    temp = grep("^x_", names(meats), value = TRUE),
    ind = seq(1, 17, length.out = 100)
  )

  list(
    train = meats_2 %>% filter(.sample_num <= 200),
    test  = meats_2 %>% filter(.sample_num > 200)
  )
}

data_meat_wide <- function() {
  skip_if_not_installed("modeldata")
  data(meats, package = "modeldata")

  library(tidyr)
  library(dplyr)

  meats_2 <-
    meats %>%
    mutate(.sample_num = row_number()) %>%
    relocate(.sample_num)

  list(
    train = meats_2 %>% filter(.sample_num <= 200),
    test = meats_2 %>% filter(.sample_num > 200),
    ind = seq(1, 17, length.out = 100)
  )
}
