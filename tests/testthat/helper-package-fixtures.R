install_test_pack <- function(package, lib) {
  fixture <- testthat::test_path("..", "fixtures", package)
  r <- if (.Platform$OS.type == "windows") "R.exe" else "R"
  output <- system2(
    file.path(R.home("bin"), r),
    c(
      "CMD",
      "INSTALL",
      "--no-test-load",
      shQuote(paste0("--library=", lib)),
      shQuote(fixture)
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  testthat::expect_null(
    attr(output, "status"),
    info = paste(output, collapse = "\n")
  )
}
