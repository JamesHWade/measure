# Small dataset for fast unit tests (3 samples instead of 215)
meats_small <- dplyr::filter(measure::meats_long, id <= 3)

data_meat_long <- function() {
  skip_if_not_installed("modeldata")
  data(meats, package = "modeldata", envir = environment())

  inds <- tibble::tibble(
    temp = grep("^x_", names(meats), value = TRUE),
    ind = seq(1, 17, length.out = 100)
  )

  meats_2 <-
    meats |>
    dplyr::mutate(.sample_num = dplyr::row_number()) |>
    tidyr::pivot_longer(
      cols = c(x_001:x_100),
      names_to = "temp",
      values_to = "absorp"
    ) |>
    dplyr::full_join(inds, by = "temp") |>
    dplyr::select(-temp)

  list(
    train = meats_2 |> dplyr::filter(.sample_num <= 200),
    test = meats_2 |> dplyr::filter(.sample_num > 200)
  )
}

data_meat_wide <- function() {
  skip_if_not_installed("modeldata")
  data(meats, package = "modeldata", envir = environment())

  meats_2 <-
    meats |>
    dplyr::mutate(.sample_num = dplyr::row_number()) |>
    dplyr::relocate(.sample_num)

  list(
    train = meats_2 |> dplyr::filter(.sample_num <= 200),
    test = meats_2 |> dplyr::filter(.sample_num > 200),
    ind = seq(1, 17, length.out = 100)
  )
}
