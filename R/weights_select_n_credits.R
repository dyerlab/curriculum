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

  # Bail on bad data
  if (!all(c("code", "credits") %in% names(courses)))
    stop("courses must have columns 'code' and 'credits'")

  # Parse N and course codes
  N <- as.numeric(gsub("[^0-9]", "", regmatches(x, regexpr("\\[[0-9]+cred\\]", x))))
  if (length(N) == 0 || is.na(N))
    stop("Encoded string must include [Ncred].")

  course_codes <- unlist(strsplit(x, "\\[[0-9]+cred\\]"))
  course_codes <- course_codes[course_codes != ""]


  #Some Helper Functions

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
    # enumerate by subset size (small → large)
    for (i in seq_len(k)) {
      cmb <- utils::combn(k, i, simplify = FALSE)
      for (set in cmb) {
        if (.is_minimal(set, credits, N)) out[[length(out)+1L]] <- set
      }
    }
    out
  }

  .minimal_sets_mc_counts <- function(credits, N, trials, seed = NULL) {
    if (!is.null(seed)) set.seed(seed)
    k <- length(credits)
    p <- min(1, N / sum(credits))  # target expected credits ~ N

    incl_counts <- numeric(k)
    sizes <- integer(0L)
    valid_minimal <- 0L

    for (t in seq_len(trials)) {
      inc <- stats::runif(k) < p
      if (!any(inc)) next
      tot <- sum(credits[inc])
      if (tot < N) next
      sel_idx <- which(inc)
      # minimality: removing any one keeps sum < N
      drop_ok <- TRUE
      for (j in sel_idx) {
        if ((tot - credits[j]) >= N) { drop_ok <- FALSE; break }
      }
      if (!drop_ok) next

      valid_minimal <- valid_minimal + 1L
      incl_counts[inc] <- incl_counts[inc] + 1
      sizes <- c(sizes, length(sel_idx))
    }

    list(incl = incl_counts,
         nsets = valid_minimal,
         avg_size = if (valid_minimal > 0) mean(sizes) else 0)
  }




  idx <- match(course_codes, courses$code)
  if (any(is.na(idx))) {
    missing <- course_codes[is.na(idx)]
    stop(paste("Courses not found in 'courses':", paste(missing, collapse = ", ")))
  }

  cr <- courses$credits[idx]
  names(cr) <- course_codes
  k <- length(course_codes)

  if (!is.null(seed)) set.seed(seed)

  # Exact enumeration (minimal sets only)
  if (k <= max_enum) {
    mins <- .minimal_sets_exact(cr, N)
    if (length(mins) == 0L) {
      return(data.frame(Course = course_codes, Weight = rep(0, k), row.names = NULL))
    }

    counts <- numeric(k)
    for (i in seq_along(mins)) {
      counts[mins[[i]]] <- counts[mins[[i]]] + 1
    }

    # per-course inclusion frequency over minimal sets
    weights <- counts / length(mins)

    # ensure names and return as data.frame
    names(weights) <- course_codes
    return(data.frame(Course = course_codes,
                      Weight = as.numeric(weights),
                      row.names = NULL))
  }

  # Monte Carlo fallback (minimal sets only)
  mc <- .minimal_sets_mc_counts(cr, N, trials, seed)
  if (mc$nsets == 0L) {
    return(data.frame(Course = course_codes, Weight = rep(0, k), row.names = NULL))
  }
  weights <- mc$incl / mc$nsets
  names(weights) <- course_codes

  data.frame(Course = course_codes,
             Weight = as.numeric(weights),
             row.names = NULL)
}
