#' Shows memory usage
#'
#' Shows memory usage of objects in session/memory.
#'
#' @param pos integer
#' @param pattern data.frame
#' @param order.by character
#' @param decreasing boolean
#' @param head boolean
#' @param n integer
#'
#' @return data.frame
#'
#' @examples
#' SR_memory_usage()
#'
#' @export
SR_memory_usage <- function(pos = 1,
                            pattern,
                            order.by,
                            decreasing = FALSE,
                            head = FALSE,
                            n = 5) {
  napply <- function(names, fn) sapply(names, function(x) fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.prettysize <- napply(names, function(x) {
    utils::capture.output(print(utils::object.size(x), units = "auto")) })
  obj.size <- napply(names, utils::object.size)
  obj.dim <- t(napply(names, function(x) as.numeric(dim(x))[1:2]))
  if (length(obj.dim) == 0) return(invisible(NULL))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
  names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
  if (!missing(order.by)) out <- out[order(out[[order.by]], decreasing = decreasing), ]
  if (head) out <- head(out, n)
  return(out)
}
