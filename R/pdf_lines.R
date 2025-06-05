
#########################################################
# manage lines
#########################################################

pdf_match_line <- function(lines, pattern) {
  # return matching lines
  filter(lines, str_detect(line, pattern))
}

pdf_delete_line <- function(lines, pattern) {
  # delete lines matching given pattern
  # pattern does not need to match entire line
  filter(lines, !str_detect(line, pattern))
}

pdf_replace_line <- function(lines, pattern, replacement) {
  # replaces matching line
  # does not need to match complete line, based on
  # whatever pattern is given by user
  lines %>%
    mutate(
      line = ifelse(str_detect(line, pattern), replacement, line)
    )
}

pdf_clean_lines <- function(lines, f) {
  f(lines)
}

pdf_has_line <- function(lines, pattern) {
  nrow(filter(lines, str_detect(line, pattern))) > 0
}

pdf_concatenate_lines <- function(lines, line_break = TRUE, new_paragraph_y_gap = 17) {
  out <- ""
  
  for (i in seq_len(nrow(lines))) {
    out <-
      paste0(
        out,
        lines[i, ]$line,
        ifelse(line_break, "\n", " ")
      )
    
    if (!is.na(lines[i, ]$y_gap_after)) {
      out <- paste0(out, ifelse(lines[i, ]$y_gap_after > new_paragraph_y_gap, "\n", ""))
    }
  }
  
  str_trim(out)
}