library(digest)
library(rlang)

## TODO: logger levels

#' @importFrom rlang fn_env fn_env<-
#' @importFrom digest digest
NULL

#' @importFrom vctrs fields field
stable_digest <- function(object) {
  switch(
    typeof(object),
    closure = {
      if ("..stash.closure" %in% names(fn_env(object))) {
        # if the environment of the closure has a ..stash.closure entry, then we know
        # it's a wrapped function. this means, in order to get the stable code,
        # we need to go one step deeper inthat function body...
        stable_digest(fn_env(object)$..stash.closure)
      } else {
        digest(c(names(formals(object)), as.character(body(object))), algo = "xxhash64")
      }
    },
    list = {
      if ("vctrs_rcrd" %in% class(object)) {
        stable_digest(lapply(fields(object), function(field_name) {field(object, field_name)}))
      } else {
        digest(lapply(object, stable_digest), algo = "xxhash64")
      }
    },
    double =,
    character =,
    logical = {digest(object, algo = "xxhash64")},
    { # default (typeof is something else)
      # TODO: add logger level for this value
      # cat("Implicitly digested type: ", typeof(object))
      digest(object, algo = "xxhash64")
    }
  )
}

wrap_closure <- function(auditor, term, ..stash.closure) {
  # Not sure why these needs to be here, but sure...
  term
  ..stash.closure
  function(...) {
    #cat("Closure activated..\n")
    auditor[[term]] <- stable_digest(..stash.closure)
    ..stash.closure(...)
  }
}

#' @importFrom rlang env empty_env
wrap_environment <- function(envir) {
  accessed <- env(empty_env())
  wrapping_env <- env(envir)
  wrapping_env$..stash_accessed <- accessed
  for(term in ls(envir)) {
    env_object <- envir[[term]]
    # if it's a value, digest the value
    # if it's a function, wrap and digest the function...
    if (typeof(env_object) == "closure") {
      wrapping_env[[term]] <- wrap_closure(accessed, term, env_object)
    }
  }
  wrapping_env
}

# R CMD CHECK generates a warning because 'found' is not a bound variable
# it's lifted from a column to a variable in 'stats::aggregate'
# This line of code tells R that this is intentional.
utils::globalVariables("found")

#' Execute a Function Call and Stash the Result
#'
#' Similar to `do.call`, `do.call.stash` constructs and executes a function call.
#' Additionally, `do.call.stash` stores the result indexed by the function body,
#' its arguments, and its dependencies.
#'
#' @param fun Function to apply. Unlike `do.call`, strings naming the function are not supported
#' @param argslist List of arguments to `fun`, in order.
#'
#' @examples
#' \dontrun{
#' heavy_function <- function(x) {
#'   Sys.sleep(3)
#'   mean(x)
#' }
#'
#' result <- NULL
#' system.time({result <- do.call.stash(heavy_function, list(c(1:20)))})
#' result
#' system.time({result <- do.call.stash(heavy_function, list(c(1:20)))})
#' result
#' }
#'
#' @importFrom stats aggregate
#' @export
do.call.stash <- function(fun, argslist) {
  # TODO: break this function up, it's too long.

  # First, look for the default file.
  first_fname <- paste0(
    getOption("stash.cache_path"),
    "/",
    stable_digest(fun),
    "-",
    stable_digest(argslist),
    ".rds"
  )

  envir <- fn_env(fun)

  # If it's a "result",
  metadata <- NULL
  if (file.exists(first_fname)) {
    first_data <- readRDS(first_fname)

    if (first_data$type == "result") {
      # then we're good to go. This body and argument values do not call
      # anything else in the neighboring environment.

      # TODO: log level on this.
      # cat("Loading cached file ", first_fname, "\n")
      return(first_data$data)

    } else if (first_data$type == "dispatch_table") {
      # Ok, we need to do some matching.
      metadata <- first_data$data
      # Check if the relevant environment entries digest correctly.
      df_names <- data.frame(name = unique(metadata$name))
      df_names$digest <- sapply(df_names$name, function(nm) {stable_digest(envir[[nm]])}, USE.NAMES = F)
      df_names$found <- T

      df_merged <- merge(metadata, df_names, by = c("name", "digest"), all.x = T)
      df_merged$found <- !is.na(df_merged$found)

      df_agg <- aggregate(found ~ postfix, df_merged, all)
      df_subset <- subset(df_agg, found)

      # assert there should only be ONE TRUE RESULT, otherwise we have problems
      if (nrow(df_subset) > 1) {
        warning("Improper number of matching rows in metadata table; ignoring cache.\n")
      } else if (nrow(df_subset) == 1) {
        result_fname <- paste0(
          getOption("stash.cache_path"),
          "/",
          stable_digest(fun),
          "-",
          stable_digest(argslist),
          "-",
          df_subset$postfix,
          ".rds"
        )
        # TODO: log level on this.
        # cat("Loading cached file ", result_fname, "\n")
        return(readRDS(result_fname))
      }

    } else {
      warning("Metadata entry could not be read; ignoring cache.\n")
    }
  }

  # The code flow reaches here either if the file does not exist, the specific bunch of environment variables were not present,
  # or the metadata could not be processed reasonably [along with warning.]

  # Computing
  # TODO: log level on this.
  # cat("Computing result to save...\n")

  # Set up the fancy environment:
  wrapped_envir <- wrap_environment(envir)
  fn_env(fun) <- wrapped_envir

  # Run the computation
  result <- do.call(fun, argslist)

  # How many objects were accessed in the environment?
  accessed <- wrapped_envir$..stash_accessed

  # TODO: log level on this.
  # print(env_print(accessed))

  if (length(accessed) == 0) {
    # then just cache the result, this has no sourced dependencies.
    saveRDS(list("type" = "result", "data" = result), file = first_fname)
    # TODO: log level on this.
    # cat("Saving at ", first_fname, "\n")
  } else {
    # ok, do i need a new metadata table?
    if (is.null(metadata)) {
      metadata <- data.frame(postfix = character(), name = character(), digest = character())
    }

    accessed_names <- names(accessed)
    accessed_digest <- sapply(accessed_names, function(nm) {accessed[[nm]]}, USE.NAMES = F)

    # compute postfix... (it doesn't need to be recomputable, just needs to have unlikely collisions)
    postfix <- digest(accessed_digest)

    # add entries to the metadata table
    metadata <- rbind(metadata, data.frame(postfix = postfix, name = accessed_names, digest = accessed_digest))

    result_fname <- paste0(
      getOption("stash.cache_path"),
      "/",
      stable_digest(fun),
      "-",
      stable_digest(argslist),
      "-",
      postfix,
      ".rds"
    )
    # TODO: log level on this.
    # cat("Saving metadata at ", first_fname, " and file at ", result_fname, "\n")

    # save the metadata table and result
    saveRDS(list("type" = "dispatch_table", "data" = metadata), file = first_fname)
    saveRDS(result, file = result_fname)
  }

  return(result)
}
