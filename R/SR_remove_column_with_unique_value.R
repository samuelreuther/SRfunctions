SR_remove_column_with_unique_value <- function(df, remove_na = F, silent = F) {
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
    df <- Filter(function(x) (n_distinct(x, na.rm = remove_na) > 1), df)
    # df <- Filter(function(x)(length(unique(x)) > 1), df[1:nrow(df),])
    if (!silent) {
      # print summary after processing
      print("Dimensions of data.frame after processing:")
      print(dim(df))
      print("Removed columns:")
      print(setdiff(temp, names(df)))
    }
    df <- droplevels(df)
    # return data.frame
    return(df)
  }
}
