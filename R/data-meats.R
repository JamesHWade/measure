#' Fat, water and protein content of meat samples
#'
#' "These data are recorded on a Tecator Infratec Food and Feed Analyzer
#' working in the wavelength range 850 - 1050 nm by the Near Infrared
#' Transmission (NIT) principle. Each sample contains finely chopped pure meat
#' with different moisture, fat and protein contents.
#'
#' If results from these data are used in a publication we want you to mention
#' the instrument and company name (Tecator) in the publication.  In addition,
#' please send a preprint of your article to
#'
#' Karin Thente, Tecator AB, Box 70, S-263 21 Hoganas, Sweden
#'
#' The data are available in the public domain with no responsibility from the
#' original data source. The data can be redistributed as long as this
#' permission note is attached."
#'
#' "For each meat sample the data consists of a 100 channel spectrum of
#' absorbances and the contents of moisture (water), fat and protein.  The
#' absorbance is -log10 of the transmittance measured by the spectrometer. The
#' three contents, measured in percent, are determined by analytic chemistry."
#'
#' Included here are the meats data transformed to a long format with
#' ```
#' modeldata::meats %>%
#'   rowid_to_column(var = "id") %>%
#'   pivot_longer(cols = starts_with("x_"),
#'                names_to = "channel",
#'                values_to = "transmittance") %>%
#'   mutate(channel = str_extract(channel, "[:digit:]+") %>% as.integer())
#' ```
#'
#' @name meats_long
#' @aliases meats_long
#' @docType data
#' @return \item{meats_long}{a tibble}
#' @keywords datasets
#' @examples
#'
#' data(meats_long)
#' str(meats_long)
NULL
