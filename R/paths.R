# R/paths.R
init_week_paths <- function(week = "week_1",
                            pattern = "^week_\\d+$",
                            check_exists = TRUE) {
  # Validate week early
  if (!is.character(week) || length(week) != 1 || is.na(week)) {
    stop("`week` must be a single character string like 'week_1' or 'week_2'.",
         call. = FALSE)
  }
  
  # Safely get the current input (if knitting)
  inp <- tryCatch(knitr::current_input(), error = function(e) NULL)
  
  # If knitting, derive the folder from this Rmd's location
  if (!is.null(inp) && nzchar(inp)) {
    wk_dir <- dirname(normalizePath(inp, winslash = "/", mustWork = FALSE))
    
  } else {
    # Console/interactive fallback uses an option or the default
    label <- getOption("course.week", week)
    
    # Validate the label: must be a single character matching pattern
    if (!is.character(label) || length(label) != 1 || is.na(label) || !grepl(pattern, label)) {
      # Try to list existing week_* dirs to help the user
      root <- tryCatch(here::here(), error = function(e) getwd())
      candidates <- list.files(root, pattern = pattern, full.names = FALSE)
      
      msg <- paste0(
        "Invalid or missing week label.\n",
        "Please supply a character string like 'week_1' or 'week_2'.\n\n",
        "Do one of the following:\n",
        "  • Set an option:   options(course.week = 'week_1')\n",
        "  • Or call:         init_week_paths(week = 'week_1')\n\n",
        "Examples: 'week_1', 'week_2', 'week_10'.\n",
        if (length(candidates)) paste0(
          "\nFound these week folders at the repo root:\n  - ",
          paste(candidates, collapse = "\n  - "), "\n"
        ) else ""
      )
      stop(msg, call. = FALSE)
    }
    
    if (!requireNamespace("here", quietly = TRUE)) {
      stop("Package 'here' is required. Install it with install.packages('here').",
           call. = FALSE)
    }
    wk_dir <- here::here(label)
  }
  
  # Optionally ensure the resolved directory actually exists
  if (check_exists && !dir.exists(wk_dir)) {
    stop(sprintf(
      "The resolved week directory does not exist:\n  %s\n",
      wk_dir
    ), call. = FALSE)
  }
  
  # Helpers you can call anywhere in the Rmd
  week_path  <<- function(...) file.path(wk_dir, ...)
  data_path  <<- function(...) file.path(wk_dir, "data", ...)
  image_path <<- function(...) file.path(wk_dir, "images", ...)
  invisible(wk_dir)
}

