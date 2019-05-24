stop_glue <- function(..., .data = NULL) {
  stop(glue::glue(..., .envir = .data %||% parent.frame()), call. = FALSE)
}

msg_glue <- function(..., .data = NULL) {
  message(glue::glue(..., .envir = .data %||% parent.frame()))
}

# Construct forward-slash seperated URL path
url.path <- function(...) paste(list(...), collapse = "/")


# Default NULL value
`%||%` <- function(x, y) if (is.null(x)) y else x
