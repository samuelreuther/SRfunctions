#' Remove columns with unique value
#'
#' Clean up data.frame by removing unnecessary columns, e.g. columns that have only 1 value.
#'
#' @param df data.frame
#' @param remove_na Boolean: should NA values be removed before counting different values?
#' @param silent Boolean
#'
#' @return data.frame
#'
#' @examples
#' # removes the variable "remove" because all values are "1"
#' SR_remove_column_with_unique_value(df = data.frame(numeric = c(0, NA, 0.3, 0.5, NA, 1),
#'                                                    character = c("a", "b", NA, "d", NA, "f"),
#'                                                    integer = c(0L, NA, NA, NA, NA, NA),
#'                                                    remove = c(1, 1, 1, 1, 1, 1)))
#'
#' # in addition removes variable "integer" because it only has the values "0" and "NA"
#' SR_remove_column_with_unique_value(df = data.frame(numeric = c(0, NA, 0.3, 0.5, NA, 1),
#'                                                    character = c("a", "b", NA, "d", NA, "f"),
#'                                                    integer = c(0L, NA, NA, NA, NA, NA),
#'                                                    remove = c(1, 1, 1, 1, 1, 1)),
#'                                    remove_na = TRUE)
#'
#' @export
SR_remove_column_with_unique_value <- function(df,
                                               remove_na = FALSE, silent = FALSE) {
  if ((nrow(df) <= 1)) {
    return(df)
  } else {
    if (!silent) {
      # print summary before processing
      print("Dimensions of data.frame before processing:")
      print(dim(df))
    }
    # save column names for the comparison later
    temp <- names(df)
    # filter columns with only 1 distinct value
    df <- Filter(function(x) (dplyr::n_distinct(x, na.rm = remove_na) > 1), df)
    # df <- Filter(function(x)(length(unique(x)) > 1), df[1:nrow(df),])
    if (!silent) {
      # print summary after processing
      print("Dimensions of data.frame after processing:")
      print(dim(df))
      print("Removed columns:")
      print(setdiff(temp, names(df)))
    }
    df <- droplevels(df)
    #
    # return data.frame
    return(df)
  }
}
