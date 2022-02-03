# R CMD CHECK generates a warning because 'found' is not a bound variable
# it's lifted from a column to a variable in 'stats::aggregate'
# This line of code tells R that this is intentional.
utils::globalVariables("found")

stash_filepath_fa <- function(fun, argslist) {
  return(c(stable_digest(fun), stable_digest(argslist)))
}

stash_filepath <- function(stash_fa, postfix = NULL) {
  file.path(
    getOption("stash.cache_path"),
    paste0(
      paste0(
        c(stash_fa, postfix),
        collapse = "-"
      ),
      ".rds"
    )
  )
}

lookup_in_dispatch <- function(metadata, envir) {
  # TODO: what does each step here do and why?
  df_names <- data.frame(name = unique(metadata$name))
  df_names$digest <- sapply(df_names$name, function(nm) {stable_digest(envir[[nm]])}, USE.NAMES = F)
  df_names$found <- T

  df_merged <- merge(metadata, df_names, by = c("name", "digest"), all.x = T)
  df_merged$found <- !is.na(df_merged$found)

  df_agg <- aggregate(found ~ postfix, df_merged, all)
  df_subset <- subset(df_agg, found)
  return(df_subset)
}

find_stash <- function(stash_fa, envir) {

  first_fname <- stash_filepath(stash_fa)

  if (!file.exists(first_fname)) {
    # if the file isn't found, we must make it!
    return(list("exists" = F, "metadata" = NULL))
  }

  first_data <- readRDS(first_fname)

  if (first_data$type == "result") {
    # In this case, we're good to go. This body and argument values do not call
    # anything else in the neighboring environment.
    log_info("Loading cached file {first_fname}", namespace = 'stash')
    return(list("exists" = T, "metadata" = NULL, "data" = first_data$data))
  }

  else if (first_data$type == "dispatch_table") {
    # Ok, we need to do some matching.
    metadata <- first_data$data
    dispatch_table_matches <- lookup_in_dispatch(metadata, envir)
    if (nrow(dispatch_table_matches) > 1) {
      log_error("Improper number of matching rows in metadata table; deleting cache.", namespace = 'stash')
      return(list("exists" = F, "metadata" = NULL))
    }

    else if (nrow(dispatch_table_matches) == 1) {
      result_fname <- stash_filepath(stash_fa, dispatch_table_matches$postfix)
      log_info("Loading cached file {result_fname}", namespace = 'stash')
      return(list("exists" = T, "metadata" = metadata, "data" = readRDS(result_fname)))
    }

    else {
      # no matching rows, we must make it.
      return(list("exists" = F, "metadata" = metadata))
    }

  }

  else {
    log_error("Metadata entry could not be read; ignoring cache.", namespace = 'stash')
    return(list("exists" = F, "metadata" = NULL))
  }
}

save_stash <- function(result, metadata, stash_fa, accessed) {
  first_fname <- stash_filepath(stash_fa)

  # How many objects were accessed in the environment?

  if (length(accessed) == 0) {
    # then just cache the result, this has no sourced dependencies.
    saveRDS(list("type" = "result", "data" = result), file = first_fname)
    log_info("Saving at {first_fname}", namespace = 'stash')

  } else {

    accessed_names <- names(accessed)
    accessed_digest <- sapply(accessed_names, function(nm) {accessed[[nm]]}, USE.NAMES = F)

    # compute postfix... (it doesn't need to be recomputable, just needs to have unlikely collisions)
    postfix <- digest(accessed_digest)

    # add entries to the metadata table
    if (is.null(metadata)) {
      metadata <- data.frame(postfix = character(), name = character(), digest = character())
    }
    metadata <- rbind(metadata, data.frame(postfix = postfix, name = accessed_names, digest = accessed_digest))

    result_fname <- stash_filepath(stash_fa, postfix)
    log_info("Saving metadata at {first_fname} and file at {result_fname}.", namespace = 'stash')

    # save the metadata table and result
    saveRDS(list("type" = "dispatch_table", "data" = metadata), file = first_fname)
    saveRDS(result, file = result_fname)
  }
}

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
#' @export do.call.stash
do.call.stash <- function(fun, argslist, parallel = NULL) {

  # --- Look for a match ---
  # setup
  envir <- fn_env(fun)

  # Look for the default file.
  stash_fa <- stash_filepath_fa(fun, argslist)
  search_result <- find_stash(stash_fa, envir)
  metadata <- search_result$metadata
  if (search_result$exists) {
    return(search_result$data)
  }

  # --- Evaluate function ---
  log_info("Computing result to save", namespace = 'stash')

  # Set up the fancy environment to run 'fun' within
  wrapped_envir <- wrap_environment(envir)
  fn_env(fun) <- wrapped_envir

  # Run the computation
  result <- do.call(fun, argslist)

  # --- Save Result ---
  save_stash(result, metadata, stash_fa, wrapped_envir$..stash_accessed)

  return(result)
}
