#' Retrieve Data from GeneNetwork
#'
#' @param species A valid species name (e.g., `"mouse"`). Use `gn_species()`
#'   to retrieve a list of species supported by GeneNetwork.
#' @param group A group name. Use `gn_groups()` to retrieve a list of valid
#'   group names.
#'
#' @examples
#' species <- gn_species()
#' groups <- gn_groups()
#'
#' # retrieve all bxd datasets
#' datasets <- gn_datasets(group = "bxd")
#'
#' # retrieve bxd genotypes
#' genos <- gn_genotypes(group = "bxd")
#' @name genenetwork-data
#' @importFrom readr read_lines read_tsv cols col_character
NULL

#' @describeIn genenetwork-data Table of supported species (e.g., `"mouse"`)
#' @export
gn_species <- function() {
  res <- .gn_request("get", path = "species")
  process_response(res)
}


#' @describeIn genenetwork-data Table of available groups of subjects or
#'   samples (e.g., `"bxd"`).
#' @export
gn_groups <- function(species = NULL) {
  path <- "groups"
  if (!is.null(species)) path <- url.path(check_species(species), path)

  res <- .gn_request("get", path)
  process_response(res)
}


#' @describeIn genenetwork-data Table of available datasets for a particular `group`.
#' @export
gn_datasets <- function(group) {
  if (missing(group)) {
    stop(
      "Must specify a 'group' to retrieve the corresponding datasets. Try:\n",
      "* 'gn_groups()' to obtain a list of valid group names",
      call. = FALSE
    )
  }

  res <- .gn_request("get", url.path("datasets", tolower(group)))
  process_response(res)
}


#' @describeIn genenetwork-data Retrieve genotypes data for a specific `group`.
#' @export
gn_genotypes <- function(group) {
  if (missing(group)) {
    stop(
      "Must specify a 'group' to retrieve the corresponding genotype data Try:\n",
      "* 'gn_groups()' to obtain a list of valid group names",
      call. = FALSE
    )
  }

  # NOTE: currently group must be uppercase
  res <- .gn_request("get", url.path("genotypes", toupper(group)))
  res$raise_for_status()

  # parse manually because contents is tsv, not json
  parsed <- res$parse("UTF-8")

  # remove metadata rows
  out <- readr::read_lines(parsed)
  metadata <- which(grepl("^[@#]", x = out[1:100]))

  readr::read_tsv(
    out,
    skip = max(metadata),
    col_types = readr::cols(Chr = readr::col_character())
  )
}

