# getDescription <- function(x) {
#   
#   rowStart <- which(x$text == "Description" & !x$space) + 1
#   
#   rowEnd <- which(x$text == "Distribution") - 1
#   
#   getContent(x, rowStart, rowEnd)
# }
# 
# getDistribution <- function(x) {
#   
#   rowStart <- which(x$text == "Distribution") + 3
#   
#   rowEnd <- min(which(x$text == "Critical")) - 1
#   
#   getContent(x, rowStart, rowEnd)
#   
# }


pdfDeletePages <- function(pdf, pages = NA) {
  # delete one or more page numbers
  # pages is numeric vector
  filter(pdf, !page_number %in% pages)
}

matchLine <- function(lines, pattern) {
  # return matching lines
  filter(lines, str_detect(line, pattern))
}

deleteLine <- function(lines, pattern) {
  # delete matching lines
  filter(lines, !str_detect(line, pattern))
}

getPdfPage <- function(fileName, pageNumber) {
  pdftools::pdf_data(fileName)[[pageNumber]]
}

getWordsPerLine <- function(pdfPage) {
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

flagSectionHeadings <-
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

wordsToLines <- function(pdfPage) {
  
  pdfPage <-
    pdfPage %>%
    mutate(
      textWithSpace = paste0(text, ifelse(space, " ", ""))
    )
  
  out <- list()
  
  for (y in unique(pdfPage$y)) {
    
    wordsInLine <- pdfPage[pdfPage$y == y, ]
    
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

getNextSectionHeadingY <- function(lines, sectionHeading) {
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

getSectionStartY <- function(lines, sectionHeading) {
  if (nrow(filter(lines, line == sectionHeading, section_heading)) == 0) {
    stop(sprintf("Section heading '%s' not found in page", sectionHeading)) 
  }

  # get starting y of section, this is the next line after the section heading  
  lines[which(lines$line == sectionHeading & lines$section_heading) + 1, ]$y
}

getSectionEndY <- function(lines, sectionHeading) {
  if (nrow(filter(lines, line == sectionHeading, section_heading)) == 0) {
    stop(sprintf("Section heading '%s' not found in page", sectionHeading)) 
  }
  
  # end y of section is the last line before the following section heading,
  # TODO or else use the last line of the page (i.e., maximum value of y for the page)
  lines[lines$y < getNextSectionHeadingY(lines, sectionHeading), ]$y %>% max()
}

getSectionLines <- function(lines, sectionHeading) {
  if (nrow(filter(lines, line == sectionHeading, section_heading)) == 0) {
    stop(sprintf("Section heading '%s' not found in page", sectionHeading)) 
  }
  
  lines[lines$y >= getSectionStartY(lines, sectionHeading) & lines$y <= getSectionEndY(lines, sectionHeading), ]
}

concatenateLines <- function(lines, lineBreak = TRUE) {
  out <- ""
    
    for (i in seq_len(nrow(lines))) {
      out <- paste0(out, lines[i, ]$line, ifelse(lineBreak, "\n", " "))
    }
  
  str_trim(out)
}

getSectionText <- function(lines, sectionHeading, lineBreak = TRUE) {
  if (nrow(filter(lines, line == sectionHeading, section_heading)) == 0) {
    stop(sprintf("Section heading '%s' not found in page", sectionHeading)) 
  }
  
  # TODO error if multiple
  
  concatenateLines(getSectionLines(lines, sectionHeading), lineBreak)
}

pageHasSection <- function(lines, sectionHeading) {
  nrow(filter(lines, line == sectionHeading, section_heading)) >= 1
}

getPageLines <- function(pdf) {
  # TODO - don't like having all these calls in here, especially with 20,10 params
  
  wordsPerLine <- getWordsPerLine(pdf)
  
  pageLines <- wordsToLines(pdf)
  
  merge(wordsPerLine, pageLines, by = "y")
}

getPageWithSection <- function(pdf, sectionHeading) {
  i <- 1
  
  repeat {
    page <- pdf[[i]]
    
    if (nrow(page) == 0) {
      i <- i + 1
      next
    }
    
    if (pageHasSection(getPageLines(page), sectionHeading)) {
      return(page)
    }
    
    i <- i + 1
  }
}

removePageNumbers <- function(lines, pattern = NA, x_min = 0) {
  # optional pattern to match, e.g. "Page 3 of 4" etc.
  # optional minimum x value, i.e. only remove if centred etc.
  
  # by default removes single digit on it's own in a line
  
  return(filter(lines, !(str_detect(line, ifelse(!is.na(pattern), pattern, "^[0-9]+$")) & min_x >= x_min)))
}

mergePdfPages <- function(pdf) {
  
  for (i in seq_len(length(pdf))) {
    thisPage <-
      pdf[[i]] %>%
      mutate(
        page_number = i,
        
        y_page = y
      )
    
    if (i == 1) {
      out <- thisPage
    } else {
      thisPage <-
        thisPage %>%
        mutate(
          y = y + max(out$y)
        )
      
      out <- bind_rows(out, thisPage)
    }
  }
  
  out
}
