#' Custom Word template.
#'
#' Loads additional style file for Word for MA223.
#'
#' @param ... additional arguments provided to \@code{word_document}.
#'
#' @export
ma223_word_format <- function(...) {
  # locations of resource files in the package
  pkg_resource <- function(...) {
    system.file(..., package = "IntroAnalysis")
  }

  wordstyle <- pkg_resource("rmarkdown/resources/MA223StyleDoc.docx")

  # call the base word_document function
  rmarkdown::word_document(
    reference_docx = wordstyle,
    ...
  )
}
