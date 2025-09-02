#' Custom Word template.
#'
#' Loads additional style file for Word for MA223 and MA382.
#'
#' @param ... additional arguments provided to [rmarkdown::word_document()].
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


#' Custom Word template.
#'
#' Loads additional style file for Word for MA223 and MA382.
#'
#' @param ... additional arguments provided to [rmarkdown::word_document()].
#'
#' @export
ma382_word_format <- function(...) {
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