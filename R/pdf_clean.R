
##############################################################
# general cleaning functions
##############################################################

pdf_remove_page_numbers <- function(lines, pattern = NA, x_min = 0) {
  # optional pattern to match, e.g. "Page 3 of 4" etc.
  # optional minimum x value, i.e. only remove if centred etc.
  
  # by default removes digits on it's own in a line
  
  return(filter(lines, !(str_detect(line, ifelse(!is.na(pattern), pattern, "^[0-9]+$")) & min_x >= x_min)))
}

replace_characters <- function(x) {
  x %>%
    str_replace_all("–", "-") %>%
    str_replace_all("—", "-") %>%
    str_replace_all("−", "-") %>%
    str_replace_all("‘", "'") %>%
    str_replace_all("’", "'") %>%
    str_replace_all("“", "\"") %>%
    str_replace_all("”", "\"") %>%
    str_replace_all("•", "-") %>%
    str_replace_all("±", "+/-") %>%
    str_replace_all("¼", "1/4") %>%
    str_replace_all("⅓", "1/3") %>%
    str_replace_all("°", "") %>%
    str_replace_all("", "") %>%
    str_replace_all("…", "...")
}

retain_double_line_break <- function(x) {
  x %>%
    str_replace_all("\\n\\n", "##########") %>%
    str_replace_all("\\n", " ") %>%
    str_replace_all("##########", "\\\n\\\n")
}

pdf_align_slightly_offset_words <- function(pdf, offsetThreshold = 1) {
  # pdf %>%
  #   mutate(
  #     y = round_arbitrary(y, offsetThreshold)
  #   )
  
  # find y values that are less than threshold apart and adjust
  
  yValues <-
    pdf %>%
    select(y) %>%
    distinct() %>%
    arrange(y)
  
  yValues$y_gap_previous = c(NA, diff(yValues$y))
  
  yValues <- filter(yValues, y_gap_previous <= offsetThreshold)
  
  pdf %>%
    mutate(
      y = ifelse(y %in% (yValues$y - 1), y + 1, y)
    )
  
}

##############################################################
# string helper functions
##############################################################

concatenate_strings_to_regex_or <- function(str) {
  paste0("(", paste0(str, collapse = "|"), ")")
}

##############################################################
# analysis functions
##############################################################

pdf_y_gap_distribution <- function(lines) {
  table(lines$y_gap_after)
}
