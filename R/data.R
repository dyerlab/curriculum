#' VCU Course Bulletin
#'
#' This R data set has listings for 2863 course bulletins with relevant
#'   information for each class that is on the books.  These classes were
#'   downloaded from the VCU Bulletin in spring 2025 and the classes
#'   were parsed from the raw PDF.
#'
#' @format this is a [data.frame] with 2863 rows and 6 variables:
#' \describe {
#'   \item{code}{The internal designation for the class (e.g., 'BIOL 101').}
#'   \item{title}{The official course title.}
#'   \item{credits}{The number of credits that the university gives for this course.}
#'   \item{bulletin}{The raw university bulletin text for the class}
#'   \item{contact}{Contact hours per week as lecture, workship, laboratory, etc.}
#'   \item{prerequs}{Any pre-requisites required for this course.}
#' }
"vcu_courses"


#' Semester schedule
#'
#' An example of a set of course offerings for a semester including the
#'   names of the courses, their titles, instructors, and meeting locations.
#'
#' @format This is a [data.frame] with 69 rows and 35 variables:
#' \describe {
#'  \item{Term}{The year and semester code.}
#'  \item{Code}{The course id number including the academic program abbreviation and the course number (e.g., 'BIOL 101')}
#'  \item{Title}{The officieal course title}
#'  \item{Credits}{The number of credit hours }
#'  \item{Instructor}{The last name of the individual tasked with teaching the course.}
#'  \item{Enrollment}{The number of students in the course.}
#'  \item{From}{Start date (as date object).}
#'  \item{To}{End date (as date object).}
#'  \item{Days}{Weekdays as (MWF, TR, etc.)}
#'  \item{Start}{Time to start the class each session (e.g., 1200).}
#'  \item{End}{Time the course ends each time (e.g., 1300).}
#'  \item{Location}{The building and room number for the course.}
#' }
#'
"schedule"
