# source: https://www.r-bloggers.com/building-a-column-selecter/
#
SR_select_variables_interactively <- function(df, n = 100) {
  # load some libraries
  p_load(shiny, miniUI, dplyr, DT)
  #
  # return_dplyr_code <- match.arg(return_dplyr_code)
  stopifnot(is.data.frame(df))
  df_head <- head(df, n)
  #
  ui <- miniPage(gadgetTitleBar("Have your pick"),
                 miniContentPanel(dataTableOutput("selection_df", height = "100%")))

  server <- function(input, output, session){
    options(DT.options = list(pageLength = 10))
    output$selection_df <- renderDataTable(df_head, server = F, selection = list(target = "column"))
    observeEvent(input$done, stopApp(input$selection_df_columns_selected))
  }
  #
  cols_selected <- runGadget(ui, server)
  #
  # return dplyr codedf %>% select(datetime, workingday) in *.R-file
  df_name <- deparse(substitute(df))
  colnames_selected <- colnames(df)[cols_selected] %>% paste(collapse = ", ")
  rstudioapi::insertText(paste0(df_name, " %>% select(", colnames_selected, ")"))
}
