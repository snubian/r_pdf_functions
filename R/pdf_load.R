
#########################################################
# load pdf data from file and combine pages
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
  
  pdf$y_gap_previous <- pdf$y - pdf$y_previous
  pdf$y_gap_next <- pdf$y_next - pdf$y
  
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

