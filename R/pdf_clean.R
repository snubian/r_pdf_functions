
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

##############################################################
# analysis functions
##############################################################

pdf_y_gap_distribution <- function(lines) {
  table(lines$y_gap_after)
}
