
#########################################################
# manage blocks of text (as opposed to sections)
#########################################################

pdf_delete_block <- function(lines, pattern_from, pattern_to, retain_from_line = FALSE) {
  lineIndexFrom <- which(str_detect(lines$line, pattern_from))
  lineIndexTo <- which(str_detect(lines$line, pattern_to))
  
  if (length(lineIndexFrom) > 0 & length(lineIndexTo) > 0) {
    
    lineIndexFrom <- lineIndexFrom %>% min()
    lineIndexTo <- lineIndexTo[lineIndexTo > lineIndexFrom] %>% min()
    
    return(lines[c(1:(lineIndexFrom - ifelse(retain_from_line, 0, 1)), lineIndexTo:nrow(lines)), ])
  } else {
    return(lines)
  }
}

pdf_extract_block <- function(lines, pattern_from, pattern_to, retain_from_line = FALSE, retain_to_line = FALSE) {
  lineIndexFrom <- which(str_detect(lines$line, pattern_from))
  lineIndexTo <- which(str_detect(lines$line, pattern_to))
  
  if (length(lineIndexFrom) > 0 & length(lineIndexTo) > 0) {
    
    lineIndexFrom <- lineIndexFrom %>% min()
    lineIndexTo <- lineIndexTo[lineIndexTo > lineIndexFrom] %>% min()
    
    return(lines[(lineIndexFrom + ifelse(retain_from_line, 0, 1)):(lineIndexTo - ifelse(retain_to_line, 0, 1)), ])
  } else {
    return(lines)
  }
}
