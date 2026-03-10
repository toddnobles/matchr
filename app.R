library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(stringr)
library(readr)

ui <- page_navbar(
  title = "matchR: Entity Matching Tool",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  nav_panel(
    "Matching Interface",
    layout_sidebar(
      sidebar = sidebar(
        width = 350,
        card(
          card_header("Filtering Options"),
          checkboxInput("hide_matched", "Hide Already Matched Records", value = FALSE),
          hr(),
          card_header("Data Inputs"),
          fileInput("file1", "Upload CSV 1", accept = ".csv"),
          uiOutput("id_select_ui1"),
          uiOutput("pre_match_ui1"),
          hr(),
          fileInput("file2", "Upload CSV 2", accept = ".csv"),
          uiOutput("id_select_ui2"),
          uiOutput("pre_match_ui2")
        ),
        card(
          card_header("Display Columns"),
          uiOutput("col_select_ui1"),
          uiOutput("col_select_ui2")
        ),
        card(
          card_header("Export"),
          downloadButton("download_matches", "Download Results", class = "btn-success w-100")
        )
      ),
      # Main Area
      card(
        card_header("Matching Controls"),
        layout_column_wrap(
          width = 1/3,
          radioButtons("confidence", "Match Confidence", 
                       choices = c("1 - Definite" = "1", "2 - Probable" = "2", "3 - Possible" = "3"),
                       inline = TRUE),
          textInput("match_comment", "Optional Comment"),
          actionButton("match_btn", "Record Match", class = "btn-primary mt-4")
        )
      ),
      layout_column_wrap(
        width = 1/2,
        card(
          card_header("CSV 1"),
          textInput("search1", "Search / Filter CSV 1"),
          DTOutput("table1")
        ),
        card(
          card_header("CSV 2"),
          textInput("search2", "Search / Filter CSV 2"),
          DTOutput("table2")
        )
      )
    )
  ),
  nav_panel(
    "Recorded Matches",
    card(
      DTOutput("matches_table"),
      actionButton("clear_matches", "Clear All Matches", class = "btn-danger mt-3")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive values to store data and matches
  data1 <- reactiveVal(NULL)
  data2 <- reactiveVal(NULL)
  matches <- reactiveVal(NULL)
  
  # Load data
  observeEvent(input$file1, {
    req(input$file1)
    df <- read_csv(input$file1$datapath)
    if (!"matchR_row_id" %in% names(df)) {
      df$matchR_row_id <- as.character(seq_len(nrow(df)))
    }
    data1(df)
  })
  
  observeEvent(input$file2, {
    req(input$file2)
    df <- read_csv(input$file2$datapath)
    if (!"matchR_row_id" %in% names(df)) {
      df$matchR_row_id <- as.character(seq_len(nrow(df)))
    }
    data2(df)
  })
  
  # ID Selection UI
  output$id_select_ui1 <- renderUI({
    req(data1())
    selectInput("id_col1", "Unique ID Field (CSV 1)", choices = names(data1()), selected = "matchR_row_id")
  })
  
  output$id_select_ui2 <- renderUI({
    req(data2())
    selectInput("id_col2", "Unique ID Field (CSV 2)", choices = names(data2()), selected = "matchR_row_id")
  })

  # Pre-existing match field selection
  output$pre_match_ui1 <- renderUI({
    req(data1())
    selectInput("pre_match_col1", "Pre-existing Match Field (optional)", choices = c("None", names(data1())), selected = "None")
  })

  output$pre_match_ui2 <- renderUI({
    req(data2())
    selectInput("pre_match_col2", "Pre-existing Match Field (optional)", choices = c("None", names(data2())), selected = "None")
  })
  
  # Column selection UI
  output$col_select_ui1 <- renderUI({
    req(data1())
    selectizeInput("cols1", "Fields from CSV 1", 
                   choices = names(data1()), 
                   selected = names(data1())[1:min(3, ncol(data1()))],
                   multiple = TRUE)
  })
  
  output$col_select_ui2 <- renderUI({
    req(data2())
    selectizeInput("cols2", "Fields from CSV 2", 
                   choices = names(data2()), 
                   selected = names(data2())[1:min(3, ncol(data2()))],
                   multiple = TRUE)
  })
  
  # Helper for filtering
  filter_df <- function(df, cols, search_text, hide_matched, id_col, pre_match_col, session_ids) {
    if (is.null(df)) return(NULL)
    
    # Text search
    if (nzchar(search_text) && length(cols) > 0) {
      df <- df %>%
        filter(if_any(all_of(cols), ~str_detect(as.character(.), fixed(search_text, ignore_case = TRUE))))
    }
    
    # Hide already matched
    if (hide_matched) {
      # Filter session matches
      if (!is.null(session_ids) && length(session_ids) > 0) {
        df <- df %>% filter(!(as.character(.[[id_col]]) %in% session_ids))
      }
      # Filter pre-existing matches
      if (!is.null(pre_match_col) && pre_match_col != "None") {
        df <- df %>% filter(is.na(.[[pre_match_col]]) | as.character(.[[pre_match_col]]) == "")
      }
    }
    df
  }

  # Reactives for filtered data
  filtered_data1 <- reactive({
    req(data1(), input$cols1, input$id_col1)
    session_ids <- if(!is.null(matches())) as.character(matches()$id1) else character(0)
    filter_df(data1(), input$cols1, input$search1, input$hide_matched, 
                    input$id_col1, input$pre_match_col1, session_ids)
  })

  filtered_data2 <- reactive({
    req(data2(), input$cols2, input$id_col2)
    session_ids <- if(!is.null(matches())) as.character(matches()$id2) else character(0)
    filter_df(data2(), input$cols2, input$search2, input$hide_matched, 
                    input$id_col2, input$pre_match_col2, session_ids)
  })

  # Table 1 Rendering (Static structure)
  output$table1 <- renderDT({
    req(data1(), input$cols1)
    df <- isolate(filtered_data1())
    
    datatable(
      df[, input$cols1, drop = FALSE],
      selection = "single",
      options = list(
        scrollY = "400px",
        paging = FALSE,
        scrollX = TRUE,
        searching = FALSE,
        info = FALSE,
        stateSave = TRUE
      ),
      rownames = FALSE
    )
  })
  
  # Update Table 1 via Proxy
  observe({
    req(data1(), input$cols1)
    df <- filtered_data1()
    proxy <- dataTableProxy("table1")
    replaceData(proxy, df[, input$cols1, drop = FALSE], resetPaging = FALSE, rownames = FALSE)
  })
  
  # Table 2 Rendering (Static structure)
  output$table2 <- renderDT({
    req(data2(), input$cols2)
    df <- isolate(filtered_data2())
    
    datatable(
      df[, input$cols2, drop = FALSE],
      selection = "single",
      options = list(
        scrollY = "400px",
        paging = FALSE,
        scrollX = TRUE,
        searching = FALSE,
        info = FALSE,
        stateSave = TRUE
      ),
      rownames = FALSE
    )
  })

  # Update Table 2 via Proxy
  observe({
    req(data2(), input$cols2)
    df <- filtered_data2()
    proxy <- dataTableProxy("table2")
    replaceData(proxy, df[, input$cols2, drop = FALSE], resetPaging = FALSE, rownames = FALSE)
  })
  
  # Record Match
  observeEvent(input$match_btn, {
    req(input$table1_rows_selected, input$table2_rows_selected, input$file1, input$file2)
    
    df1_filt <- filtered_data1()
    df2_filt <- filtered_data2()
    
    row1 <- df1_filt[input$table1_rows_selected, ]
    row2 <- df2_filt[input$table2_rows_selected, ]
    
    # Get filenames for prefixes
    name1 <- tools::file_path_sans_ext(input$file1$name)
    name2 <- tools::file_path_sans_ext(input$file2$name)
    
    # Extract values for matched fields
    val1 <- row1 %>% select(all_of(input$cols1)) %>% rename_with(~paste0(name1, "_", .))
    val2 <- row2 %>% select(all_of(input$cols2)) %>% rename_with(~paste0(name2, "_", .))
    
    id1_val <- as.character(row1[[input$id_col1]])
    id2_val <- as.character(row2[[input$id_col2]])
    
    new_match <- data.frame(
      id1 = id1_val,
      id2 = id2_val,
      confidence = input$confidence,
      comment = input$match_comment,
      timestamp = as.character(Sys.time()),
      stringsAsFactors = FALSE
    ) %>%
      bind_cols(val1, val2)
    
    if (is.null(matches())) {
      matches(new_match)
    } else {
      matches(bind_rows(matches(), new_match))
    }
    
    updateTextInput(session, "match_comment", value = "")
    showNotification(paste("Match recorded:", id1_val, "<->", id2_val), type = "message")
  })
  
  # Matches Table
  output$matches_table <- renderDT({
    req(matches())
    datatable(matches(), options = list(pageLength = 20, scrollX = TRUE))
  })
  
  # Clear Matches
  observeEvent(input$clear_matches, {
    matches(NULL)
  })
  
  # Download
  output$download_matches <- downloadHandler(
    filename = function() {
      paste("matches-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(matches(), file)
    }
  )
}

shinyApp(ui, server)
