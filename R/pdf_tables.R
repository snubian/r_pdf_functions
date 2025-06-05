
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

pdf_remove_table_row_by_heading <- function(tbl, skip_row_heading_patterns) {
  headingRegex <-
    skip_row_heading_patterns %>%
    concatenate_strings_to_regex_or()

  rowsToRemove <- which(str_detect(tbl[, 1], headingRegex))
  
  if (length(rowsToRemove) > 0) {
    return(tbl[-rowsToRemove, ])
  } else {
    return(tbl)    
  }

}

pdf_get_table_cell_from_limits <- function(tbl, cell_limits) {
  tbl %>%
    filter(
      x >= cell_limits[1],
      x < cell_limits[2],
      y >= cell_limits[3],
      y < cell_limits[4]
    )
}

pdf_get_merge_limits <- function(heading, merge_specs) {
  for (i in seq_len(nrow(merge_specs))) {
    if (str_detect(heading, merge_specs[i, ]$heading_pattern)) {
      return(c(merge_specs[i, ]$merge_left, merge_specs[i, ]$merge_right))
    }
  }
  
  return(NULL)
}

pdf_get_table_content_from_splits <- function(tbl, skip_row_heading_patterns = NULL, merge_specs = NULL) {
  # tbl is words filtered to table raw content
  
  colX <- c(pdf_get_table_x_splits(tbl, 5), 1e+06)
  rowY <- c(0, pdf_get_table_y_splits(tbl, colX), 1e+06)
  
  out <- data.frame(matrix(NA, nrow = length(rowY) - 2, ncol = length(colX) - 1))
  
  for (thisY in seq_len(length(rowY) - 1)) {
    for (thisX in seq_len(length(colX) - 1)) {
      
      # set cell limits for this iteration
      cellLimits <- c(colX[thisX], colX[thisX + 1], rowY[thisY], rowY[thisY + 1])
      
      skipThisCell <- FALSE
      
      # check if there are merge requirements for this row based on heading
      if (thisX > 1 & !is.null(merge_specs)) {
        thisRowHeadingMergeSpecs <- pdf_get_merge_limits(thisRowHeading, merge_specs)
        
        if (!is.null(thisRowHeadingMergeSpecs)) {
          if (thisX == thisRowHeadingMergeSpecs[1]) {
            cellLimits[1:2] <- colX[thisRowHeadingMergeSpecs + c(0, 1)]
          } else {
            skipThisCell <- TRUE
          }
        }
      }
      
      thisCell <- pdf_get_table_cell_from_limits(tbl, cellLimits)
      
      if (nrow(thisCell) > 0) {
        thisCell <-
          thisCell %>%
          pdf_get_lines() %>%
          pdf_concatenate_lines(line_break = FALSE)
      } else {
        thisCell <- NA
      }
      
      if (thisX == 1 & !is.null(merge_specs)) {
        thisRowHeading <- thisCell
      }
      
      # don't want to write cell for cols following merge
      if (!skipThisCell) {
        out[thisY, thisX] <- thisCell 
      }
      
    }
  } 
  
  names(out) <- out[1, ] %>% str_to_lower() %>% str_replace_all(" ", "_")
  
  if (!is.null(skip_row_heading_patterns)) {
    out <- out %>% pdf_remove_table_row_by_heading(skip_row_heading_patterns)
  }
  
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
