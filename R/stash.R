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
#' @export do.call.stash
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

      log_info("Loading cached file {first_fname}", namespace = 'stash')
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
        log_error("Improper number of matching rows in metadata table; ignoring cache.", namespace = 'stash')
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
        log_info("Loading cached file {result_fname}", namespace = 'stash')
        return(readRDS(result_fname))
      }

    } else {
      log_error("Metadata entry could not be read; ignoring cache.", namespace = 'stash')
    }
  }

  # The code flow reaches here either if the file does not exist, the specific bunch of environment variables were not present,
  # or the metadata could not be processed reasonably [along with warning.]

  # Computing
  log_info("Computing result to save", namespace = 'stash')

  # Set up the fancy environment:
  wrapped_envir <- wrap_environment(envir)
  fn_env(fun) <- wrapped_envir

  # Run the computation
  result <- do.call(fun, argslist)

  # How many objects were accessed in the environment?
  accessed <- wrapped_envir$..stash_accessed

  if (length(accessed) == 0) {
    # then just cache the result, this has no sourced dependencies.
    saveRDS(list("type" = "result", "data" = result), file = first_fname)
    log_info("Saving at {first_fname}", namespace = 'stash')
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
    log_info("Saving metadata at {first_fname} and file at {result_fname}.", namespace = 'stash')

    # save the metadata table and result
    saveRDS(list("type" = "dispatch_table", "data" = metadata), file = first_fname)
    saveRDS(result, file = result_fname)
  }

  return(result)
}
