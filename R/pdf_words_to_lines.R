
#########################################################
# raw pdf (words) to lines
#########################################################

pdf_get_lines <- function(pdf) {
  merge(pdf_get_words_per_line(pdf), pdf_words_to_lines(pdf), by = "y")
}

pdf_get_words_per_line <- function(pdf) {
  x <-
    pdf %>%
    group_by(y) %>%
    summarise(
      num_words = n(),
      min_x = min(x),
      max_x = max(x),
      first_word = first(text)
    )
  
  x$y_gap_before <- c(NA, diff(x$y))
  x$y_gap_after  <- c(diff(x$y), NA)
  
  x
}

pdf_words_to_lines <- function(pdf) {
  
  pdf <-
    pdf %>%
    mutate(
      textWithSpace = paste0(text, ifelse(space, " ", ""))
    )
  
  out <- list()
  
  for (y in unique(pdf$y)) {
    
    wordsInLine <- pdf[pdf$y == y, ] %>% arrange(x)
    
    thisLine <- ""
    
    for (i in seq_len(nrow(wordsInLine))) {
      thisLine <- paste0(thisLine, wordsInLine[i, ]$textWithSpace)
    }
    
    out[[y]] <-
      data.frame(
        y = y,
        line = thisLine
      )
  }
  
  bind_rows(out)
}
