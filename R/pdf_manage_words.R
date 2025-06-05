
#########################################################
# manage raw pdf (words)
#########################################################

pdf_delete_pages <- function(pdf, pages = NA) {
  # delete one or more page numbers
  # pages is numeric vector
  filter(pdf, !page_number %in% pages)
}

pdf_remove_header <- function(pdf, max_y) {
  # remove anything with y <= max_y
  pdf[pdf$y_page >= max_y, ]
}

pdf_correct_km2 <- function(pdf) {
  #pdf[pdf$text == "2" & pdf$y < pdf$y_previous, ]$y <- pdf[pdf$text == "2" & pdf$y < pdf$y_previous, ]$y_previous
  pdf[pdf$text == "2" & abs(pdf$y - pdf$y_previous) <= 3, ]$y <- pdf[pdf$text == "2" & abs(pdf$y - pdf$y_previous) <= 3, ]$y_previous
  
  pdf
}

pdf_filter_words_to_match_lines <- function(pdf, lines) {
  return(
    pdf %>%
      filter(y %in% lines$y)
  )
}
