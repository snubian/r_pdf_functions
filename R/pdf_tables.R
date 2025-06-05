
##############################################################
# generic table extract functions
##############################################################

pdf_clean_table_headings <- function(tbl, num_cols) {
  # problem occurs when multiple words on top line of col heading
  
  # if space is FALSE then we know there is only one word so that's fine
  # if space is TRUE then we want to delete any subsequent words on that line until
  # the next space==FALSE which is the last word on that 
  
  # make assumption that headings are all on same value of y - not necessarily!
  # so get just words having minimum y
  headers <-
    filter(tbl, y <= (min(tbl$y) + 5)) %>%
    arrange(x)
  
  i <- 1
  splits <- vector()
  
  repeat {
    if (!headers[i, ]$space) {
      splits <- c(splits, headers[i, ]$x)
      i <- i + 1
    } else {
      splits <- c(splits, headers[i, ]$x)
      repeat {
        i <- i + 1
        if (!headers[i, ]$space) {
          i <- i + 1
          break
        }
      }
    }
    if (i > nrow(headers)) {
      break
    }
  }
  
  return(splits)
}

pdf_get_table_x_splits <- function(tbl, num_cols) {
  # for given raw table content get column (x) splits
  
  #tbl <- tbl %>% arrange(y, x)
  
  return(pdf_clean_table_headings(tbl, num_cols))
  
  #return(tbl[1:num_cols, ]$x)
}

pdf_get_table_y_splits <- function(tbl, x_splits, y_gap_threshold = 15) {
  # for given raw table content get row (y) splits

  tbl <- tbl %>% filter(x < x_splits[2])
  tbl <- tbl %>% arrange(y)
  tbl$y_gap_previous = c(NA, diff(tbl$y))
  tbl <- tbl %>% filter(y_gap_previous > y_gap_threshold)
  
  # y values for bottom of rows
  return(tbl$y)
}

pdf_get_table_content_from_splits <- function(tbl) {
  # tbl is words filtered to table raw content
  
  colX <- pdf_get_table_x_splits(tbl, 5)
  rowY <- pdf_get_table_y_splits(tbl, colX)
  
  rowY <- c(0, rowY, 1e+06)
  colX <- c(colX, 01000)
  
  out <- data.frame(matrix(NA, nrow = length(rowY) - 2, ncol = length(colX) - 2))
  
  for (thisY in 2:length(rowY)) {
    for (thisX in 2:length(colX)) {
      # get words in cell
      thisCell <-
        tbl %>%
        filter(
          x >= colX[thisX - 1],
          x < colX[thisX],
          y >= rowY[thisY - 1],
          y < rowY[thisY]
        )
      
      if (nrow(thisCell) > 0) {
        thisCell <-
          thisCell %>%
          pdf_get_lines() %>%
          pdf_concatenate_lines(line_break = FALSE)
      } else {
        thisCell <- NA
      }
      
      out[thisY - 1, thisX - 1] <- thisCell
      
    }
  } 
  
  names(out) <- out[1, ] %>% str_to_lower() %>% str_replace_all(" ", "_")
  
  return(out)
}

################################################################
# original two-column (heading/content) table extract functions
# - best for case where we know the row headings
################################################################

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

pdf_extract_table_from_block <- function(pdf, lines, pattern_from, pattern_to) {
  lines %>%
    pdf_extract_block(pattern_from, pattern_to) %>%
    pdf_filter_words_to_match_lines(pdf, .) %>%
    pdf_get_table_content_from_splits()
}
