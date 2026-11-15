native_io_test_link <- function(target, path) {
  created <- tryCatch(
    {
      fs::link_create(target, path)
      TRUE
    },
    error = function(error) FALSE
  )
  if (!created) {
    skip("This filesystem does not allow symbolic links.")
  }
  expect_true(isTRUE(
    fs::file_info(path, follow = FALSE, fail = FALSE)$type == "symlink"
  ))
  invisible(path)
}
