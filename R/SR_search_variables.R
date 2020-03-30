#' Search variable(s) in a data.frame
#'
#' Finds matching and partial matching variables and prints a summary of it/them.
#'
#' @param df data.frame
#' @param var string
#'
#' @return Null.
#'
#' @example
#' SR_search_variables(df = data.frame(CUSTOMER_ID = 1001:1010,
#'                                     CUSTOMER_NAME = LETTERS[1:10],
#'                                     ADRESSE_ID = 1:10),
#'                     var = "id")
#'
#' @export
SR_search_variables <- function(df, var) {
  # search all matching variables in data.frame
  var_names <- names(df)[grepl(var, names(df))]
  var_names <- c(var_names, names(df)[grepl(tolower(var), tolower(names(df)))])
  # var_names <- unique(var_names)
  #
  # data.frame of found/matched variables
  xx_temp <- df %>% select(one_of(var_names)) %>% data.frame()
  #
  # return results
  if (ncol(xx_temp) >= 1) {
    # Return variable names
    # var_names
    #
    # position of found variables
    cat("Position of variable(s) in df: "); cat("\n")
    cat(c(1:length(names(df)))[grepl(paste(var_names, collapse = "|"), names(df))])
    cat("\n"); cat("\n")
    #
    # class of found variables
    cat("Class of variable(s): "); cat("\n")
    print(sapply(xx_temp, function(x) class(x)))
    cat("\n")
    #
    # number of distinct values
    cat("Number of distinct values of variable(s): "); cat("\n")
    print(xx_temp %>% summarise_all(n_distinct))
    cat("\n")
    #
    # summary of variables
    cat("Summary of variable(s): "); cat("\n")
    print(summary(xx_temp %>% mutate_if(is.character, as.factor)))
    cat("\n")
    # head and tail of variables
    cat("Head and tail of variable(s): "); cat("\n")
    print(rbind(df %>% select(one_of(var_names)) %>% head(5),
                df %>% select(one_of(var_names)) %>% tail(5)))
    # missings
    # print(xx_temp %>% summarise_all(is.na)); print(" ")
  } else {
    print("No variable found.")
  }
  return(invisible(NULL))
}
