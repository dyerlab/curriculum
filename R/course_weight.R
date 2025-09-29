#' Parses Course For Relative Weighing
#'
#' This function takes a course listing and parses it based
#'   on the number of credits that the linguistic weighings
#'   will be base upon.  The credit weighing is used to scale
#'   the linguistic mappings.
#'
#' There are several ways to encode the requirements for courses.  These
#'   include:
#'   \describe{
#'   \item{ABCD101}{The single course gets the full credit weight.}
#'   \item{ABCD101|ABCD201}{Both courses get half of their credit weights (OR)}
#'   \item{ABCD101&ABCD210}{Each course is weighed as full credit but must be together {AND}}
#'   \item{ABCD101[2]ABCD102[2]ABCD103[2]ABCD104[2] }{Pick two from the list of courses and they get full weight but then do all combinations and weigh all of them in proportion to their frequency.}
#'   \item{ABCD101[6c]ABCD102[6c]ABCD103[6c]ABCD104[6c]}{Pick a minimum of 6 credits from the list and get credit equivallents and then do all combinations and weight portitional to their frequency.}
#'   }
#' @param x The mapping string to be parsed.
#' @param courses A [data.frame] that has course code and credtis
#' @param code The name of the column in [courses] where course codes are found (default: Course)
#' @param credits The name of the column in [courses] with the number of credits for the course (default: Credits)
#' @returns A [data.frame] where each row is a course with columns for `Weight` and `Course` as defined by the rules above.
#' @export
course_weight <- function( x, courses, code = "Course", credits = "Credits" ) {

  # mapping checks
  if( is.null(x) || !is.character(x) || length(x) != 1) {
    warning("Empty or inappropriate mapping sequence")
    return( data.frame( Weight = numeric(0),
                        Course = chracter(0) ) )
  }

  # data frame checks
  if( !is.data.frame(courses) ) { stop("You need to pass ") }
  if( !(code %in% names(courses)  ) ) { stop("Please indicate which column of your courses data.frame has class codes.") }
  if( !(credits %in% names(courses)  ) ) { stop("Please indicate which column of your courses data.frame has class credits.") }


  # return singleton

  # pull apart the OR


  # pull apart the AND



}

