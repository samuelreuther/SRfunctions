# Function to backup files of a specific directory
#
SR_backup_files <- function(path_from, path_to, exclude = NULL, include = NULL) {
  #
  # measure time
  start_time <- Sys.time()
  #
  # initialize count variables
  new <- 0
  update <- 0
  no_update <- 0
  skipped <- 0
  skipped_files <- list()
  updated_files <- list()
  new_files <- list()
  #
  # list of existing files
  files_from <- list.files(path = path_from, full.names = TRUE, recursive = TRUE,
                           all.files = TRUE, no.. = TRUE)
  # files_from <- list.files(path = path_from, full.names = TRUE, recursive = TRUE)
  #
  # clean up name if "invalid multibyte string"
  for (i in seq_along(files_from)) {
    e <- try(nchar(files_from[i]), silent = TRUE)
    if (is(e, "try-error")) {
      p_load(stringi)
      files_from[i] <- files_from[i] %>%
        stri_enc_toutf8(.) %>%
        stri_unescape_unicode(.) %>%
        gsub("�", "X", ., fixed = TRUE)
    }
    rm(e)
  }; rm(i)
  #
  # exclude files
  if (!is.null(exclude)) {
    for (i in seq_along(exclude)) {
      files_from <- files_from[!grepl(exclude[i], files_from, fixed = TRUE)]
    }
  }
  #
  # include files
  if (!is.null(include)) {
    files_from_ <- c()
    for (i in seq_along(include)) {
      files_from_ <- c(files_from_, files_from[grepl(include[i], files_from)])
    }
    files_from <- files_from_
  }
  #
  #
  #
  ### loop through all files
  #
  for (i in seq_along(files_from)) {
    # for (i in 1:100) {
    #
    # print information
    # if (i == 1 & length(files_from) > 1000) { cat(paste0("Remaining files: ", length(files_from))) }
    # if ((length(files_from) - i) %% 1000 == 0) { cat(paste0(" ", length(files_from) - i)) }
    # if (i == length(files_from) & length(files_from) > 1000) { cat("\n") }
    #
    # get names
    filename_from <- files_from[i]
    filename_to <- gsub(path_from, path_to, files_from[i])
    # basename(filename_to)
    # dirname(filename_to)
    #
    # skip if filename_to is too long for windows (260 characters)
    if (nchar(filename_to) >= 260) {
      skipped <- skipped + 1
      skipped_files <- c(skipped_files, filename_to)
      next
    }
    #
    # check if file exists in to
    if (!file.exists(filename_to)) {
      # create dir if necessary
      if (!dir.exists(dirname(filename_to))) {
        dir.create(dirname(filename_to), recursive = TRUE)
      }
      # copy file
      file.copy(filename_from, filename_to, copy.date = TRUE)
      new <- new + 1
      new_files <- c(new_files, filename_to)
    } else {
      # get information for this file
      file_info_from <- file.info(filename_from)
      file_info_to <- file.info(filename_to)
      # if mtime_from > mtime_to then overwrite existing file
      if (file_info_from$mtime > file_info_to$mtime + 1) {
        file.copy(filename_from, filename_to, overwrite = TRUE, copy.date = TRUE)
        update <- update + 1
        updated_files <- c(updated_files, filename_to)
      } else {
        no_update <- no_update + 1
      }
      rm(file_info_from, file_info_to)
    }
  }
  # print summary
  end_time <- Sys.time()
  cat(paste0("Backup of directory:   ", path_from,
             "   (", round(as.numeric(difftime(end_time, start_time, units = "mins")), 2), " mins).\n",
             "New files:         ", new, "\n",
             "Updated files:     ", update, "\n",
             "Skipped files:     ", skipped, "\n",
             "Not updated files: ", no_update, "\n"))
  # assign file lists to global environment
  assign("files_from", files_from, envir = .GlobalEnv)
  assign("skipped_files", skipped_files, envir = .GlobalEnv)
  assign("new_files", new_files, envir = .GlobalEnv)
  assign("updated_files", updated_files, envir = .GlobalEnv)
}
#
#
#
# New function
#
# list.files of path_from and path_to
# New file: TRUE if file exists in path_to
# Move file: if file exists in another path_to
# Check for double files
# update: get date from path_to if file exists first and update if newer date
#
#
#
# duplicated_files <- function(path_from, path_to, exclude = NULL, include = NULL) {
#   #
#   # measure time
#   start_time <- Sys.time()
#   #
#   # list of existing files_from
#   files_from <- list.files(path = path_from, full.names = TRUE, recursive = TRUE)
#   # exclude files
#   if (!is.null(exclude)) {
#     for (i in seq_along(exclude)) {
#       files_from <- files_from[!grepl(exclude[i], files_from, fixed = TRUE)]
#     }
#   }
#   # include files
#   if (!is.null(include)) {
#     for (i in seq_along(include)) {
#       files_from <- files_from[grepl(include[i], files_from)]
#     }
#   }
#   #
#   # list of existing files_to
#   files_to <- list.files(path = path_to, full.names = TRUE, recursive = TRUE)
#   # exclude files
#   if (!is.null(exclude)) {
#     for (i in seq_along(exclude)) {
#       files_to <- files_to[!grepl(exclude[i], files_to, fixed = TRUE)]
#     }
#   }
#   # include files
#   if (!is.null(include)) {
#     for (i in seq_along(include)) {
#       files_to <- files_to[grepl(include[i], files_to)]
#     }
#   }
#   #
#   # print summary
#   end_time <- Sys.time()
#   cat(paste0("Backup of directory:   ", path_from,
#              "   (", round(as.numeric(difftime(end_time, start_time, units = "mins")), 2), " mins).\n"))
#   #
#   # browser()
#   #
#   # merge files_from and files_to
#   duplicated_files <- full_join(data.frame(files_from) %>%
#                                   mutate(files_from = as.character(files_from),
#                                          filename = basename(files_from)) %>%
#                                   select(filename, files_from),
#                                 data.frame(files_to) %>%
#                                   mutate(files_to = as.character(files_to),
#                                          filename = basename(files_to)) %>%
#                                   select(filename, files_to),
#                                 by = "filename") %>%
#     filter(is.na(files_from) | is.na(files_to))
#   #
#   # assign file lists to global environment
#   assign("duplicated_files", duplicated_files, envir = .GlobalEnv)
#   assign("files_from", files_from, envir = .GlobalEnv)
#   assign("files_to", files_to, envir = .GlobalEnv)
# }
#
# duplicated_files(path_from = "D:/Pictures/2018",
#                  path_to   = "F:/Rossam/Pictures/2018")
#
# # duplicated_files(path_from = "D:/Pictures/2017",
# #                  path_to   = "F:/Rossam/Pictures/2017")
#
# data.frame(files_from) %>%
#   mutate(files_from = as.character(files_from),
#          filename = basename(files_from)) %>%
#   group_by(filename) %>%
#   count() %>%
#   arrange(-n) %>%
#   View()
