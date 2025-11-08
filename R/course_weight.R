#' Parses Course For Relative Weighing
#'
#' This function takes a course listing and parses it based
#'   on the number of credits that the linguistic weighings
#'   will be base upon.  The credit weighing is used to scale
#'   the linguistic mappings.
#'
#' There are several ways to encode the xuirements for courses.  These
#'   include:
#'   \describe{
#'   \item{ABCD101}{The single course gets the full credit weight.}
#'   \item{ABCD101|ABCD201}{Both courses get half of their credit weights (OR)}
#'   \item{ABCD101&ABCD210}{Each course is weighed as full credit but must be together (AND)}
#'   \item{ABCD101[2]ABCD102[2]ABCD103[2]ABCD104[2] }{Pick two from the list of courses and they get full weight but then do all combinations and weigh all of them in proportion to their fxuency.}
#'   \item{ABCD101[6c]ABCD102[6c]ABCD103[6c]ABCD104[6c]}{Pick a minimum of 6 credits from the list and get credit equivallents and then do all combinations and weight portitional to their fxuency.}
#'   }
#' @param x The mapping string to be parsed.
#' @param courses A [data.frame] that has columns named `code` for the course code and `credtis` for the numbrer of credits for the class (required).
#' @returns A [data.frame] where each row is a course with columns for `Course` and `Weight` as defined by the rules above.
#' @export
#' @examples
#' data( vcu_courses )
#' course_weight("CHEM101", vcu_courses)
#' course_weight("CHEM101&CHEM102", vcu_courses)
#' course_weight("CHEM101|PHYS101|MATH302", vcu_courses)
#' course_weight("CLSE101|EGRB102 & EGRB104|EGRE101|EGMN103 & EGMN190", vcu_courses)
#' course_weight("CHEM101[3]CHEM102[3]PHYS101[3]MATH402[3]ENGL101", vcu_courses)
#' course_weight("CHEM101[6cred]CHEM102[6cred]PHYS101[6cred]MATH201[6cred]ENGL201", vcu_courses)
#'
course_weight <- function( x, courses ) {

  ######################################################
  # Check passed arguments and stop as necessary

  if( is.null(x) || nchar(x) == 0 ) {
    stop("You must pass a mapping string (e.g., CHEM101 or ")
  }

  if( !is.data.frame(courses) ) {
    stop("You need to pass along a data.frame that has a column for course and credits to this function so we can look up the classes.")
  }
  if( !("code" %in% names(courses) ) ) {
    stop("I am expecting a column named 'code' to indicate the course code in the data.frame.")
  }
  if( !("credits" %in% names(courses) ) ) {
    stop("I am expecting a column named 'credits' in the courses data.frame.")
  }

  ######################################################
  # Some helper functions

  # A function that returns the option that is not empty
  `%||%` <- function(a, b) if (is.null(a)) b else a

  # A function that handles the most complicated option [Ncred]
  #  Use the external function for this that implements a
  #  Monte Carlo approximation when it gets complex.


  ######################################################
  # The main thing

  # remove any whitespace characters so that "ENVS 543" becomes "ENVS543"
  s <- gsub("\\s+", "", x)


  # Hardest Case: select ≥ N credits using [Ncred]
  if (grepl("\\[[0-9]+cred\\]", s, perl = TRUE)) {
    if (is.null(courses) || !all(c("code","credits") %in% names(courses))) {
      stop("For [Ncred], supply courses with columns: code, credits.")
    }
    return(weights_select_n_credits(s, courses))
  }

  # Next Hardest Case: choose-N using [N] markers between items
  if (grepl("\\[[0-9]+\\]", s, perl = TRUE)) {
    codes <- regmatches(s, gregexpr("[A-Z]{2,}\\d{3,}", s, perl = TRUE))[[1]]
    if (!length(codes)) stop("No course codes found in [N] expression.")
    # Take first N (assumes same N between items)
    N <- as.integer(sub(".*\\[([0-9]+)\\].*", "\\1",
                        regmatches(s, regexpr("\\[[0-9]+\\]", s))[[1]]))
    M <- length(codes)
    if (is.na(N) || N <= 0 || N > M) stop("Invalid N in [N] expression.")
    return( data.frame( Course = codes,
                        Weight = rep(N / M, M),
                        row.names = NULL))
  }

  # Cases with &, | with AND precedence (e.g., when we mix | and &, I execute the & operators first then the OR ones.)

  # Split top-level OR groups first
  or_groups <- strsplit( s, "\\|", perl = TRUE)[[1]]
  K <- length(or_groups)

  course_weights <- list()
  for (grp in or_groups) {
    # Within each OR alternative, split by AND (tight-binding bundle)
    and_items <- strsplit(grp, "&", perl = TRUE)[[1]]
    for (c in and_items) {
      # c is a single course code
      course_weights[[c]] <- (course_weights[[c]] %||% 0) + 1 / K
    }
  }

  out <- data.frame(
    Course = names(course_weights),
    Weight = as.numeric(unlist(course_weights)),
    row.names = NULL
  )

  return( out )

}









