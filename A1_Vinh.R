library(shiny)
library(shinyjs)
library(rmarkdown)

# Define the rubric structure
rubric_data <- list(
  "Question 2" = list(
    "a" = list(total = 2, items = list("Correct graph" = c(0, 2), "Well labled" = c(0, -0.5))),
    "b" = list(total = 3, items = list("Correct values" = c(0, 3), "1 error" = c(0, -0.5), "2 errors" = c(0, -1), "3 errors" = c(0, -1.5), "4+ errors" = c(0, -2))),
    "c" = list(total = 1, items = list("Correct matrix" = c(0, 1))),
    "d" = list(total = 2, items = list("Correct weights" = c(0, 2), "1 error" = c(0, -0.5), "2+ errors" = c(0, -1))),
    "e" = list(total = 3, items = list("Correct values" = c(0, 3), "1 error" = c(0, -0.5), "2+ errors" = c(0, -1))),
    "f" = list(total = 3, items = list("Correct VAR's" = c(0, 3), "1 error" = c(0, -0.5), "2+ errors" = c(0, -1), "Negative values" = c(0, -1), "$ units missing" = c(0, -0.5))),
    "g" = list(total = 6, items = list("Correct values" = c(0, 6), "1 error" = c(0, -0.5), "2 errors" = c(0, -1), "3 errors" = c(0, -1.5), "4 errors" = c(0, -2), "5 errors" = c(0, -2.5), "6 errors" = c(0, -3)))
  ),
  "Question 3" = list(
    "a" = list(total = 4, items = list("Correct values pre Dec 11" = c(0, 1, 2), "Correct value Post Jan12" = c(0,1, 2), "Hats missing" = c(0, -0.5, -1))),
    "b" = list(total = 6, items = list("Hypotheses" = c(0, 0.5, 1), "Test stat and distribution under the null" = c(0, 0.5, 1), "T calcs" = c(0, 0.5, 1), "p-values" = c(0, 0.5, 1), "Correct decision" = c(0, 1), "Conclusion in context" = c(0, 0.5, 1))),
    "c" = list(total = 4, items = list("Hypotheses" = c(0, 0.5), "Test stat and distribution under the null" = c(0, 0.5), "J calcs" = c(0, 0.5, 1), "Decisions" = c(0, 0.5, 1), "Sensible explanation" = c(0, 0.5, 1), "Use F-test" = c(0, -1)))
  )
)

clean_id <- function(x) gsub("[^[:alnum:]]", "_", x)

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      body { background-color: #fcfcfc; color: #333; font-family: 'Segoe UI', sans-serif; }
      .rubric-row { display: flex; align-items: center; padding: 10px; border-bottom: 1px solid #e0e0e0; }
      .row-odd { background-color: #f9f9f9; } 
      .row-even { background-color: #f0f4f7; } 
      .label-col { width: 340px; font-weight: 400; color: #444; }
      .radio-col { flex-grow: 1; }
      .question-box { background-color: white; padding: 30px; border-radius: 4px; margin-bottom: 30px; border: 1px solid #ddd; }
      .part-header { background-color: #546e7a; color: white; padding: 8px 18px; display: flex; justify-content: space-between; margin-top: 25px; font-weight: bold; }
      
      /* Updated Question Total Styling */
      .q-header-row { 
        display: flex; 
        justify-content: space-between; 
        align-items: center; 
        border-bottom: 3px solid #546e7a; 
        padding-bottom: 10px;
        margin-bottom: 20px; 
      }
      .q-total-badge { 
        font-size: 1.5em; 
        font-weight: 800;
        color: #1b5e20; 
        background: #e8f5e9; 
        padding: 10px 20px; 
        border-radius: 8px;
        border: 1px solid #c8e6c9;
      }
      
      .external-feedback-title { 
        display: block; 
        font-weight: bold; 
        font-size: 1.1em; 
        color: #3498db; 
        margin-top: 25px; 
        margin-bottom: 8px; 
      }
      .action-area { background-color: #eee; padding: 20px; border-radius: 8px; margin-top: 20px; text-align: right; }
      .btn-warning { background-color: #ffb74d; color: #fff; margin-right: 5px;}
      .btn-primary { background-color: #2e7d32; color: #fff; margin-right: 5px;}
      .btn-info { background-color: #546e7a; color: #fff; }
    "))
  ),
  
  titlePanel("Marking Rubric: Q 2 and 3"),
  
  fluidRow(
    column(10, offset = 1,
           textInput("student_id", "Student ID / Name:", width = "400px"),
           hr(),
           
           lapply(names(rubric_data), function(q_name) {
             div(class = "question-box",
                 div(class = "q-header-row",
                     h2(q_name, style="margin:0; font-weight: bold;"),
                     uiOutput(paste0(clean_id(q_name), "_overall_total"))
                 ),
                 lapply(names(rubric_data[[q_name]]), function(p_name) {
                   part <- rubric_data[[q_name]][[p_name]]
                   div(
                     div(class = "part-header", span(paste0("Part ", p_name, ")")), uiOutput(paste0(clean_id(q_name), "_", p_name, "_total"), inline = TRUE)),
                     lapply(seq_along(names(part$items)), function(i) {
                       item_name <- names(part$items)[i]
                       row_class <- if(i %% 2 == 1) "row-odd" else "row-even"
                       div(class = paste("rubric-row", row_class),
                           div(class = "label-col", item_name),
                           div(class = "radio-col",
                               radioButtons(inputId = paste0(clean_id(q_name), "_", p_name, "_", clean_id(item_name)),
                                            label = NULL, choices = part$items[[item_name]], selected = 0, inline = TRUE)))
                     })
                   )
                 }),
                 
                 div(class = "external-feedback-title", "Feedback"),
                 textAreaInput(paste0(clean_id(q_name), "_comment"), label = NULL, rows = 3, width = '100%')
             )
           }),
           
           div(class = "action-area",
               actionButton("reset_all", "Reset All Fields", class = "btn-warning"),
               downloadButton("download_html", "Download HTML Report", class = "btn-primary"),
               downloadButton("download_txt", "Download TXT (Summary)", class = "btn-info"))
    )
  )
)

server <- function(input, output, session) {
  
  # Function to calculate scores
  get_q_score_calc <- function(q_name) {
    total <- 0
    for (p_name in names(rubric_data[[q_name]])) {
      part <- rubric_data[[q_name]][[p_name]]
      p_score <- sum(sapply(names(part$items), function(it) {
        as.numeric(input[[paste0(clean_id(q_name), "_", p_name, "_", clean_id(it))]] %||% 0)
      }))
      total <- total + p_score
    }
    return(total)
  }
  
  observe({
    lapply(names(rubric_data), function(q_name) {
      
      # Part Totals
      lapply(names(rubric_data[[q_name]]), function(p_name) {
        output[[paste0(clean_id(q_name), "_", p_name, "_total")]] <- renderUI({
          part <- rubric_data[[q_name]][[p_name]]
          score <- sum(sapply(names(part$items), function(it) {
            as.numeric(input[[paste0(clean_id(q_name), "_", p_name, "_", clean_id(it))]] %||% 0)
          }))
          span(paste0(score, " / ", part$total))
        })
      })
      
      # Question Overall Totals (Large)
      output[[paste0(clean_id(q_name), "_overall_total")]] <- renderUI({
        q_max <- sum(sapply(rubric_data[[q_name]], function(p) p$total))
        q_score <- get_q_score_calc(q_name)
        div(class = "q-total-badge", paste0("Total: ", q_score, " / ", q_max))
      })
    })
  })
  
  observeEvent(input$reset_all, {
    for (q_name in names(rubric_data)) {
      for (p_name in names(rubric_data[[q_name]])) {
        for (item_name in names(rubric_data[[q_name]][[p_name]]$items)) {
          updateRadioButtons(session, paste0(clean_id(q_name), "_", p_name, "_", clean_id(item_name)), selected = 0)
        }
      }
      updateTextAreaInput(session, paste0(clean_id(q_name), "_comment"), value = "")
    }
    updateTextInput(session, "student_id", value = "")
  })
  
  # Download Handlers
  output$download_html <- downloadHandler(
    filename = function() { paste0("Feedback_", clean_id(input$student_id), ".html") },
    content = function(file) {
      temp_report <- file.path(tempdir(), "report.Rmd")
      report_src <- c(
        "---", "title: 'Assessment Feedback'", "output: html_document", "---",
        "<style>.feedback-box { background-color: #f4f7f9; border-left: 6px solid #546e7a; padding: 15px; margin-top: 5px; border-radius: 4px; } .feedback-title { color: #3498db; font-weight: bold; font-size: 1.1em; margin-bottom: 5px; display: block; } .feedback-text { color: #3498db; font-style: italic; font-weight: 500; }</style>",
        paste0("### Student: ", input$student_id),
        paste0("Date: ", format(Sys.Date(), "%d %B %Y")), "\n---"
      )
      
      for (q_name in names(rubric_data)) {
        q_max <- sum(sapply(rubric_data[[q_name]], function(p) p$total))
        report_src <- c(report_src, paste0("## ", q_name, " (Total: ", get_q_score_calc(q_name), " / ", q_max, ")"))
        for (p_name in names(rubric_data[[q_name]])) {
          part <- rubric_data[[q_name]][[p_name]]
          score <- sum(sapply(names(part$items), function(it) as.numeric(input[[paste0(clean_id(q_name), "_", p_name, "_", clean_id(it))]] %||% 0)))
          report_src <- c(report_src, paste0("- **Part ", p_name, "**: ", score, " / ", part$total))
        }
        fb_content <- input[[paste0(clean_id(q_name), "_comment")]]
        if(fb_content == "") fb_content <- "No specific feedback provided."
        report_src <- c(report_src, "<div class='feedback-box'>", "<span class='feedback-title'>Feedback</span>", paste0("<span class='feedback-text'>", fb_content, "</span>"), "</div>\n")
      }
      writeLines(report_src, temp_report)
      rmarkdown::render(temp_report, output_file = file, envir = new.env(parent = globalenv()))
    }
  )
  
  output$download_txt <- downloadHandler(
    filename = function() { paste0("Summary_", clean_id(input$student_id), ".txt") },
    content = function(file) {
      txt_content <- paste0("MARKING SUMMARY\nStudent: ", input$student_id, "\nDate: ", Sys.Date(), "\n", paste(rep("-", 30), collapse=""), "\n\n")
      for (q_name in names(rubric_data)) {
        q_total_max <- sum(sapply(rubric_data[[q_name]], function(p) p$total))
        q_score <- get_q_score_calc(q_name)
        fb_content <- input[[paste0(clean_id(q_name), "_comment")]]
        if(fb_content == "") fb_content <- "No feedback provided."
        txt_content <- paste0(txt_content, q_name, " TOTAL: ", q_score, " / ", q_total_max, "\n")
        txt_content <- paste0(txt_content, "Feedback: ", fb_content, "\n\n")
      }
      writeLines(txt_content, file)
    }
  )
}

shinyApp(ui, server)