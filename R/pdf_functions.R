
#########################################################
# load from file
#########################################################

pdf_load <- function(f, pages = NULL) {
  # wrapper for pdftools::pdf_data which grabs some other stuff from pdftools::pdf_pagesize
  # use pages vector to select only specific pages
  
  pdf <-
    pdf_data(f)
  
  if (!is.null(pages)) {
    pdf <- pdf[pages]
  }
  
  pdf <-
    pdf %>%
    pdf_bind_pages() %>%
    rename(
      word_width = width,
      word_height = height
    ) %>%
    merge(pdf_get_page_dimensions(f), by = "page_number") %>%
    mutate(
      y_page = y,
      y = y_page + bottom_cumulative_previous_page
    )
  
  pdf$y_previous <- lag(pdf$y)
  pdf$y_next <- lead(pdf$y)

  return(pdf)
}

pdf_get_page_dimensions <- function(pdf) {
  # get info on page dimensions
  # this is used by pdf_load to merge into raw pdf data
  
  x <-
    pdf_pagesize(pdf) %>%
    mutate(
      page_number = 1:nrow(.),
      bottom_cumulative = cumsum(bottom)
    ) %>%
    dplyr::select(
      page_number,
      top:height,
      bottom_cumulative
    ) %>%
    mutate(
      bottom_cumulative_previous_page = lag(bottom_cumulative)
    )
  
  x[is.na(x)] <- 0
  return(x)
}

#########################################################
# manage raw pdf (words)
#########################################################

pdf_delete_pages <- function(pdf, pages = NA) {
  # delete one or more page numbers
  # pages is numeric vector
  filter(pdf, !page_number %in% pages)
}

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

pdf_get_words_per_line <- function(pdfPage) {
  x <-
    pdfPage %>%
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

pdf_flag_section_headings <-
  function(
    lines,
    headingMinimumGapBefore = 20,
    headingMinimumGapAfter = 20,
    maxWordsInHeading = 8,
    headings = NA
  ) {
  if (is.character(headings)) {
    lines <-
      lines %>%
      mutate(
        section_heading = line %in% headings
      )
  } else {
    lines <-
      lines %>%
      mutate(
        section_heading = ifelse(y_gap_before >= headingMinimumGapBefore & y_gap_after >= headingMinimumGapAfter & num_words <= maxWordsInHeading, TRUE, FALSE),
        section_heading = ifelse(first_word == "Table", TRUE, section_heading),
        section_heading = ifelse(is.na(section_heading), FALSE, section_heading)
      )
  }
    
  return(lines)
}

pdf_words_to_lines <- function(pdfPage) {
  
  pdfPage <-
    pdfPage %>%
    mutate(
      textWithSpace = paste0(text, ifelse(space, " ", ""))
    )
  
  out <- list()
  
  for (y in unique(pdfPage$y)) {
    
    wordsInLine <- pdfPage[pdfPage$y == y, ] %>% arrange(x)
    
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

pdf_get_next_section_heading_y <- function(lines, sectionHeading) {
  if (nrow(filter(lines, line == sectionHeading, section_heading)) == 0) {
    stop(sprintf("Section heading '%s' not found in page", sectionHeading)) 
  }
  
  yStart <- getSectionStartY(lines, sectionHeading)
  
  potentialNextSectionHeadingY <-
    lines %>%
    filter(
      y > yStart,
      section_heading
    ) %>%
    select(
      y
    )
  
  if (nrow(lines) > 0) {
    return(min(potentialNextSectionHeadingY))
  } else {
    return(max(lines$y))
  }
  
}

pdf_get_section_start_y <- function(lines, sectionHeading) {
  if (nrow(filter(lines, line == sectionHeading, section_heading)) == 0) {
    stop(sprintf("Section heading '%s' not found in page", sectionHeading)) 
  }

  # get starting y of section, this is the next line after the section heading  
  lines[which(lines$line == sectionHeading & lines$section_heading) + 1, ]$y
}

pdf_get_section_end_y <- function(lines, sectionHeading) {
  if (nrow(filter(lines, line == sectionHeading, section_heading)) == 0) {
    stop(sprintf("Section heading '%s' not found in page", sectionHeading)) 
  }
  
  # end y of section is the last line before the following section heading,
  # TODO or else use the last line of the page (i.e., maximum value of y for the page)
  lines[lines$y < getNextSectionHeadingY(lines, sectionHeading), ]$y %>% max()
}

pdf_get_section_lines <- function(lines, sectionHeading) {
  if (nrow(filter(lines, line == sectionHeading, section_heading)) == 0) {
    stop(sprintf("Section heading '%s' not found in page", sectionHeading)) 
  }
  
  lines[lines$y >= getSectionStartY(lines, sectionHeading) & lines$y <= getSectionEndY(lines, sectionHeading), ]
}

concatenate_lines <- function(lines, lineBreak = TRUE, newParagraphYGap = 17) {
  out <- ""
    
    for (i in seq_len(nrow(lines))) {
      out <-
        paste0(
          out,
          lines[i, ]$line,
          ifelse(lineBreak, "\n", " "),
          ifelse(lines[i, ]$y_gap_after > newParagraphYGap, "\n", "")
        )
    }
  
  str_trim(out)
}

pdf_get_section_text <- function(lines, sectionHeading, lineBreak = TRUE) {
  if (nrow(filter(lines, line == sectionHeading, section_heading)) == 0) {
    return(NA)
    #stop(sprintf("Section heading '%s' not found in page", sectionHeading)) 
  }
  
  # TODO error if multiple
  
  concatenate_lines(get_page_lines(lines, sectionHeading), lineBreak)
}

pdf_has_Section <- function(lines, sectionHeading) {
  nrow(filter(lines, line == sectionHeading, section_heading)) >= 1
}

pdf_get_lines <- function(pdf) {
  merge(pdf_get_words_per_line(pdf), pdf_words_to_lines(pdf), by = "y")
}

# getPageWithSection <- function(pdf, sectionHeading) {
#   i <- 1
#   
#   repeat {
#     page <- pdf[[i]]
#     
#     if (nrow(page) == 0) {
#       i <- i + 1
#       next
#     }
#     
#     if (pageHasSection(getPageLines(page), sectionHeading)) {
#       return(page)
#     }
#     
#     i <- i + 1
#   }
# }

pdf_remove_page_numbers <- function(lines, pattern = NA, x_min = 0) {
  # optional pattern to match, e.g. "Page 3 of 4" etc.
  # optional minimum x value, i.e. only remove if centred etc.
  
  # by default removes digits on it's own in a line
  
  return(filter(lines, !(str_detect(line, ifelse(!is.na(pattern), pattern, "^[0-9]+$")) & min_x >= x_min)))
}

pdf_bind_pages <- function(pdf) {
  
  for (i in seq_len(length(pdf))) {
    thisPage <-
      pdf[[i]] %>%
      mutate(
        page_number = i
      )
    
    if (i == 1) {
      out <- thisPage
    } else {
      out <- bind_rows(out, thisPage)
    }
    
    # out$y_previous <- lag(out$y)
    # out$y_next <- lead(out$y)
  }
  
  out
}

pdf_y_gap_distribution <- function(lines) {
  table(lines$y_gap_after)
}

pdf_correct_km2 <- function(pdf) {
  #pdf[pdf$text == "2" & pdf$y < pdf$y_previous, ]$y <- pdf[pdf$text == "2" & pdf$y < pdf$y_previous, ]$y_previous
  pdf[pdf$text == "2" & abs(pdf$y - pdf$y_previous) <= 3, ]$y <- pdf[pdf$text == "2" & abs(pdf$y - pdf$y_previous) <= 3, ]$y_previous
  
  pdf
}

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

pdf_remove_header <- function(pdf, max_y) {
  # remove anything with y <= max_y
  pdf[pdf$y_page >= max_y, ]
}



# pdf_get_table_heading_left_y <- function(lines, heading) {
#   # get lines that have this as the 
# 
#   lines %>%
#     filter(
#       str_detect(line, paste0("^", heading))
#     )
# }

pdf_get_table_y_limits <- function(lines, line_before, line_after = NA, min_y_gap_after = 30) {
  yLineBefore <- lines[grepl(line_before, lines$line), ]$y
  
  yLimitMin <- min(lines[lines$y > yLineBefore, ]$y)

  lines <- filter(lines, y >= yLimitMin)
  
  if (!is.na(line_after)) {
    yLineAfter <- lines[grepl(line_after, lines$line), ]$y
    
    return(c(yLimitMin, (yLineAfter - 1)))
    
  } else {
    # no line_after specified, so just go until find gap after table
    i <- 0
    
    repeat {
      i <- i + 1
      if (lines[i, ]$y_gap_after > min_y_gap_after) { break }
    }
    
    return(c(yLimitMin, lines[i, ]$y))
  }
}

pdf_get_table_raw <- function(pdf, lines, line_before, line_after = NA, min_y_gap_after = 30) {
  
  y_limits <-
    lines %>%
    pdf_get_table_y_limits(line_before, line_after, min_y_gap_after)
  
  pdf %>%
    filter(
      y >= y_limits[1],
      y <= y_limits[2]
    )
}

pdf_get_table_heading_y <- function(pdf, headings) {
  lines <- pdf_words_to_lines(pdf)
  
  headingRegex <- paste0("^(", paste0(headings, collapse = "|"), ")")
  
  lines <- filter(lines, str_detect(line, headingRegex))
  
  data.frame(
    heading = names(headings),
    y = lines$y
  )
}

pdf_get_table_content <- function(pdf, table_headings, x_min, f = NULL, y_jiggle = 3) {

  # TODO - better format for output, named cells? or list?
  
  headingY <-
    pdf %>%
    pdf_get_table_heading_y(table_headings)
  
  lines <-
    pdf %>%
    filter(
      x >= x_min
    ) %>%
    #pdf_words_to_lines()
    pdf_get_lines()
  
  if (!is.null(f)) {
    lines <- pdf_clean_lines(lines, f)
  }
  
  out <-
    headingY %>%
    mutate(
      y_next = lead(y),
      content = ""
    )
  
  for (i in 1:nrow(out)) {
    yFrom <- out[i, ]$y - y_jiggle
    yTo <- ifelse(i < nrow(out), out[i, ]$y_next - y_jiggle, (max(lines$y) + 1))
    
    out[i, ]$content = lines[lines$y >= yFrom & lines$y < yTo, ] %>%
      concatenate_lines() %>%
      replace_characters() %>%
      retain_double_line_break()
  }  
  
  out %>%
    dplyr::select(
      heading,
      content
    )
  
}

pdf_clean_lines <- function(lines, f) {
  f(lines)
}

pdf_has_line <- function(lines, pattern) {
  nrow(filter(lines, str_detect(line, pattern))) > 0
}

pdf_filter_words_to_match_lines <- function(pdf, lines) {
  return(
    pdf %>%
      filter(y >= min(lines$y), y <= max(lines$y)) %>%
      arrange(y, x)
  )
}

# need to handle case where table content crosses page!

# table headers on left have characteristic that 


# remove from given line to point where next line 


# give section heading a tag and use that to compile rather than actual heading text

# allow tagging of headings that are not on their own line

# recognise and handle tables, based on x values?

# user provides x value to use as cutoff between table cols, get all to left and right of x

