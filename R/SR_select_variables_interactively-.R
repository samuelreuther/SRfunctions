# # source: https://www.r-bloggers.com/building-a-column-selecter/
# #
# SR_select_variables_interactively <- function(df, n = 100) {
#   stopifnot(is.data.frame(df))
#   df_head <- utils::head(df, n)
#   #
#   ui <- miniUI::miniPage(miniUI::gadgetTitleBar("Have your pick"),
#                          miniUI::miniContentPanel(DT::dataTableOutput("selection_df", height = "100%")))
#
#   server <- function(input, output, session){
#     options(DT.options = list(pageLength = 10))
#     output$selection_df <- DT::renderDataTable(df_head, server = FALSE,
#                                                selection = list(target = "column"))
#     shiny::observeEvent(input$done, shiny::stopApp(input$selection_df_columns_selected))
#   }
#   #
#   cols_selected <- shiny::runGadget(ui, server)
#   #
#   # return dplyr codedf %>% select(datetime, workingday) in *.R-file
#   df_name <- deparse(substitute(df))
#   colnames_selected <- colnames(df)[cols_selected] %>% paste(collapse = ", ")
#   rstudioapi::insertText(paste0(df_name, " %>% dyplr::select(", colnames_selected, ")"))
# }
