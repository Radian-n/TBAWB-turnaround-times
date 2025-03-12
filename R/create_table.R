#' Prints a dataframe using tinytables
#'
#' @param dataframe A datafrane. The turnaround date dataframe to print.
#'
#' @returns An tinytable object.
#' @export
#'
#' @examples
#' dataframe <- mtcars[1, 1:3]
#' create_table(dataframe)
#' # Creates table in viewer pane.
create_table <- function(dataframe) {
  assertthat::assert_that(
    is.data.frame(dataframe),
    msg = glue::glue("`dataframe` must be of type dataframe, not of type: {typeof(dataframe)}.")
  )

  n_cols <- ncol(dataframe)

  turnaround_table <- dataframe |>
    # Make columns fixed widths
    tinytable::tt(width = rep(1 / n_cols, n_cols)) |>
    # Make header row light grey background
    tinytable::style_tt(
      i = 1,
      colour = "black",
      background = "#F2F2F2"
    ) |>
    tinytable::style_tt(
      fontsize = 0.9,
      bootstrap_class = "table mytable",
      # Remove top and bottom padding from table rows
      # Make header row normal
      # Make 1st row slightly bold
      bootstrap_css_rule = "
        .mytable th, .mytable td {
          padding: .25rem .25rem;
        }
        
        .mytable th {
          font-weight: 400;
        }

        .mytable td {
          font-weight: 600;
        }
      "
    )

  return(turnaround_table)
}
