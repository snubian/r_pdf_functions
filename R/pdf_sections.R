
#########################################################
# work with sections
#########################################################

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
          is_section_heading = line %in% headings
        )
    } else {
      lines <-
        lines %>%
        mutate(
          is_section_heading = ifelse(y_gap_before >= headingMinimumGapBefore & y_gap_after >= headingMinimumGapAfter & num_words <= maxWordsInHeading, TRUE, FALSE),
          is_section_heading = ifelse(first_word == "Table", TRUE, is_section_heading),
          is_section_heading = ifelse(is.na(is_section_heading), FALSE, is_section_heading)
        )
    }
    
    return(lines)
  }

pdf_get_next_section_heading_y <- function(lines, section_heading) {
  if (nrow(filter(lines, line == section_heading, is_section_heading)) == 0) {
    stop(sprintf("Section heading '%s' not found in page", section_heading)) 
  }
  
  yStart <- getSectionStartY(lines, section_heading)
  
  potentialNextSectionHeadingY <-
    lines %>%
    filter(
      y > yStart,
      is_section_heading
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

pdf_get_section_start_y <- function(lines, section_heading) {
  if (nrow(filter(lines, line == section_heading, is_section_heading)) == 0) {
    stop(sprintf("Section heading '%s' not found in page", section_heading)) 
  }
  
  # get starting y of section, this is the next line after the section heading  
  lines[which(lines$line == section_heading & lines$is_section_heading) + 1, ]$y
}

pdf_get_section_end_y <- function(lines, section_heading) {
  if (nrow(filter(lines, line == section_heading, is_section_heading)) == 0) {
    stop(sprintf("Section heading '%s' not found in page", section_heading)) 
  }
  
  # end y of section is the last line before the following section heading,
  # TODO or else use the last line of the page (i.e., maximum value of y for the page)
  lines[lines$y < getNextSectionHeadingY(lines, section_heading), ]$y %>% max()
}

pdf_get_section_lines <- function(lines, section_heading) {
  if (nrow(filter(lines, line == section_heading, is_section_heading)) == 0) {
    stop(sprintf("Section heading '%s' not found in page", section_heading)) 
  }
  
  lines[lines$y >= getSectionStartY(lines, section_heading) & lines$y <= getSectionEndY(lines, section_heading), ]
}

pdf_get_section_text <- function(lines, section_heading, line_break = TRUE) {
  if (nrow(filter(lines, line == section_heading, is_section_heading)) == 0) {
    return(NA)
    #stop(sprintf("Section heading '%s' not found in page", sectionHeading)) 
  }
  
  # TODO error if multiple
  
  concatenate_lines(get_page_lines(lines, section_heading), line_break)
}

pdf_has_Section <- function(lines, section_heading) {
  nrow(filter(lines, line == section_heading, is_section_heading)) >= 1
}
