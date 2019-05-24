.gn_api_version <-"pre1"

# Return a version specific user agent
user_agent <- function(agent = "gnr") {
  version <- system.file("DESCRIPTION", package = agent, mustWork = FALSE)
  if (file.exists(version)) {
    version <- base::read.dcf(version, "Version")
    sprintf("%s v%s", agent, version)
  } else {
    agent
  }
}


# Appends API version to a specificed path
.gn_api_path <- function(path) {
  sprintf("api/v_%s/%s", .gn_api_version, path)
}

# Construct the OSF API Client
.gn_cli <- function() {
  url <- "http://gn2-zach.genenetwork.org"

  headers <- list(
    `User-Agent` = user_agent()
    # `Accept-Header` = sprintf(
    #   "application/vnd.api+json;version=%s",
    #   .gn_api_version)
  )

  crul::HttpClient$new(
    url = url,
    # opts = list(
    #   encode = "json"
    # ),
    headers = headers
  )
}


.gn_request <-
  function(method,
           path,
           query = list(),
           body = NULL,
           ...) {

  method <- match.arg(method, c("get", "put", "patch", "post", "delete"))
  cli <- .gn_cli()

  cli$retry(
    method,
    .gn_api_path(path),
    query,
    body = body,
    ...
  )
}

.gn_request_cache <- memoise::memoise(.gn_request)
