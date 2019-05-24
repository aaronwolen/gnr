#' Processs GeneNetwork API responses
#'
#' Convert json content of API responses to a [tibble][tibble::tibble-package].
#'
#' @param res HttpResponse object
#' @noRd
process_response <- function(res) {
  stopifnot(inherits(res, "HttpResponse"))

  # currently the API throws 200 status codes even for invalid calls so we
  # can't rely on raise for status
  res$raise_for_status()

  # until proper status codes are returned check for error field
  parsed <- parse_json(res)

  if (!is.null(parsed$errors)) {
    stop_glue(
      "GeneNetwork error - {title}\n  {detail}",
      .data = parsed$errors[[1]]
    )
  }

  out <- jsonlite::fromJSON(res$parse("UTF-8"), simplifyVector = TRUE)
  tibble::new_tibble(out, nrow = nrow(out))
}


parse_json <- function(res) {
  stopifnot(inherits(res, "HttpResponse"))
  jsonlite::fromJSON(res$parse("UTF-8"), simplifyVector = FALSE)
}
