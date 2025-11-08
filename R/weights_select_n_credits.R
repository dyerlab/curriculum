#' Parse choose N credits
#'
#' This helper function steps in when there is a "choose N credits" from the list
#'  I had to update it a bit to handle cases where the list of courses is large
#'  (VT & GMU I'm looking at you and your funky class options...) to use a
#'  Monte Carlo approximation when N or the number of credtis gets too big
#'
#' @param x The mapping string to be parsed.
#' @param courses A [data.frame] that has columns named `code` for the course code and `credtis` for the number of credits for the class (required).
#' @param max_enum The number of courses where the estimator will go from brute force combinatorial approch to Monte Carlo approximations.
#'  This gives 2^k-1 subsets and for something like 15 is 32,767, which is reasonable for R on a desktop.  Beyond that, it does the approximation.
#' @param trials The number of iterations to run for Monte Carlo approximation
#' @param seed A random starting seed
#' @returns A [data.frame] where each row is a course with columns for `Course` and `Weight` as defined by the rules above.
#' @export
#' @examples
#' data( vcu_courses )
#' encoding <- "CHEM101[6cred]CHEM102[6cred]PHYS101[6cred]MATH201[6cred]ENGL201"
#' weights_select_n_credits(encoding, vcu_courses)
#' weights_select_n_credits(encoding, vcu_courses, max_enum=3)


weights_select_n_credits <- function(x, courses,
                                     max_enum = 15,
                                     trials = 100000,
                                     seed = NULL) {
  if (!all(c("code", "credits") %in% names(courses))) {
    stop("courses must have columns 'code' and 'credits'")
  }

  .is_minimal <- function(set_idx, credits, N) {
    total <- sum(credits[set_idx])
    if (total < N) return(FALSE)
    for (j in set_idx) {
      if ((total - credits[j]) >= N) return(FALSE)
    }
    TRUE
  }

  .minimal_sets_exact <- function(credits, N) {
    k <- length(credits)
    out <- vector("list", 0L)
    for (i in seq_len(k)) {
      cmb <- utils::combn(k, i, simplify = FALSE)
      for (set in cmb) {
        if (.is_minimal(set, credits, N)) out[[length(out) + 1L]] <- set
      }
    }
    out
  }

  .minimal_sets_mc_counts <- function(credits, N, trials, seed = NULL) {
    if (!is.null(seed)) set.seed(seed)
    k <- length(credits)
    p <- min(1, N / sum(credits))

    incl_counts <- numeric(k)
    valid_minimal <- 0L

    for (t in seq_len(trials)) {
      inc <- stats::runif(k) < p
      if (!any(inc)) next
      tot <- sum(credits[inc])
      if (tot < N) next
      sel_idx <- which(inc)
      # minimality check
      drop_ok <- TRUE
      for (j in sel_idx) {
        if ((tot - credits[j]) >= N) { drop_ok <- FALSE; break }
      }
      if (!drop_ok) next
      valid_minimal <- valid_minimal + 1L
      incl_counts[inc] <- incl_counts[inc] + 1
    }

    list(incl = incl_counts, nsets = valid_minimal)
  }

  # Parse N credits and course/group tokens
  N <- as.numeric(gsub("[^0-9]", "", regmatches(x, regexpr("\\[[0-9]+cred\\]", x))))
  if (length(N) == 0 || is.na(N))
    stop("Encoded string must include [Ncred].")

  # Split on [Ncred] and drop empties
  tokens <- unlist(strsplit(x, "\\[[0-9]+cred\\]"))
  tokens <- tokens[tokens != ""]

  # Parse each token for &-joined groups
  groups <- lapply(tokens, function(tok) strsplit(tok, "\\s*&\\s*")[[1]])
  group_names <- sapply(groups, paste, collapse = "_&_")

  # Lookup credits for each course and sum within groups
  group_credits <- sapply(groups, function(cs) {
    idx <- match(cs, courses$code)
    if (any(is.na(idx))) {
      missing <- cs[is.na(idx)]
      stop(paste("Courses not found in 'courses':", paste(missing, collapse = ", ")))
    }
    sum(courses$credits[idx])
  })
  names(group_credits) <- group_names

  k <- length(group_credits)
  if (!is.null(seed)) set.seed(seed)

  # CASE 1: Exact enumeration (small number of groups)
  if (k <= max_enum) {
    mins <- .minimal_sets_exact(group_credits, N)
    if (length(mins) == 0L) {
      # return all 0 weights
      expanded <- unlist(groups)
      return(data.frame(Course = expanded, Weight = rep(0, length(expanded)), row.names = NULL))
    }

    counts <- numeric(k)
    for (i in seq_along(mins)) counts[mins[[i]]] <- counts[mins[[i]]] + 1
    weights_group <- counts / length(mins)
    names(weights_group) <- group_names

    # Expand group weights back to courses
    expanded <- do.call(rbind, lapply(seq_along(groups), function(i) {
      data.frame(
        Course = groups[[i]],
        Weight = rep(weights_group[i], length(groups[[i]])), row.names = NULL
      )
    }))

    return(expanded)
  }


  # CASE 2: Monte Carlo approximation (large number of groups)
  mc <- .minimal_sets_mc_counts(group_credits, N, trials, seed)
  if (mc$nsets == 0L) {
    expanded <- unlist(groups)
    return( data.frame( Course = as.character(expanded), Weight = as.numeric(rep(0, length(expanded))), row.names = NULL))
  }

  weights_group <- mc$incl / mc$nsets
  names(weights_group) <- group_names

  # Expand group weights to individual courses
  expanded <- do.call(rbind, lapply(seq_along(groups), function(i) {
    data.frame(
      Course = as.character(groups[[i]]),
      Weight = as.numeric(rep(weights_group[i], length(groups[[i]]))), row.names = NULL
    )
  }))

  return( expanded )
}
