

#____________________________________________________________________________
#  Report
#____________________________________________________________________________


mod_09_report_ui <- function(id) {

  ns <- NS(id)

  tagList(

    fluidRow(
      column(
        width = 12,
        h2(strong("Report"), style = "padding-left: 25px;color: black;"),
        hr(class = "custom-hr-thick")
      )
    ),

    fluidRow(
      column(
        width = 5,
        div(
          selectInput(
            inputId = ns("selected_chunk_report"),
            label = "Code chunks to include:",
            selected = NULL,
            choices = NULL,
            multiple = TRUE
          ),
          style = "padding-left: 20px;white-space: nowrap;"
        ),
      )
    ),
    fluidRow(
      column(
        width = 4,
        div(
          uiOutput(ns("html_report")),
          style = "padding-left: 20px;"
        )
      ),
      column(
        width = 8,
        downloadButton(
          outputId = ns("Rmd_source"),
          label = strong("RMarkdown"),
          class = "custom-download-button"
        ),
        tippy::tippy_this(
          ns("Rmd_source"),
          "Download a R Markdown source file.",
          theme = "light-border"
        )
      )
    ),
    br(),
    div(
      verbatimTextOutput(ns("rmd_chunk_output")),
      style = "padding-left: 20px;padding-right: 20px;"
    ),
    br()

  )

}

mod_09_report_serv <- function(id, submit_button, logs, selected_model,
                               llm_response, llm_prompt, use_python,
                               counter, sample_temp, code_error, python_to_html,
                               current_data) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Dropdown to pick what chunks to include in report
    observeEvent(submit_button(), {
      req(logs$code != "")

      choices <- seq_along(logs$code_history)
      names(choices) <- paste0("Chunk #", choices)

      updateSelectInput(
        session = session,
        inputId = "selected_chunk_report",
        label = "Chunks to include (Use backspace to delete):",
        selected = "All chunks without errors",
        choices = c(
          "All chunks",
          "All chunks without errors",
          choices
        )
      )
    })

    # collect all RMarkdown chunks
    Rmd_total <- reactive({
      req(llm_prompt())

      # Initialize script with model and credits
      Rmd_script <- ""
      Rmd_script <- paste0(
        "\nDeveloped by [Steven Ge](https://twitter.com/StevenXGe) using API access via the
        [openai](https://cran.rstudio.com/web/packages/openai/index.html) package to
        [OpenAI's](https://cran.rstudio.com/web/packages/openai/index.html) \"",
        selected_model(), "\" model.",
        "\n\nRTutor Website: [https://RTutor.ai](https://RTutor.ai)\n",
        "Source code: [GitHub.](https://github.com/gexijin/RTutor)\n\n"
      )

      # If user uploaded data, insert the file reading script
      # based on the file type
      # if (input$select_data == uploaded_data) {
      #   file_name <- input$user_file$name
      #   file_type <- user_data()$file_type
      #   read_commands <- list(
      #     "read_excel" = paste0("# install.packages(readxl)\nlibrary(readxl)\ndf <- read_excel(\"", file_name, "\")"),
      #     "read.csv" = paste0("df <- read.csv(\"", file_name, "\")"),
      #     "read.table" = paste0("df <- read.table(\"", file_name, "\", sep = \"\t\", header = TRUE)")
      #   )
      #   Rmd_script <- paste0(
      #     Rmd_script, "\n### 0. Read File\n```{R, eval = FALSE}\n", read_commands[[file_type]], "\n```\n"
      #   )
      # }

      # Add initial data chunk
      Rmd_script <- paste0(
        # Get the data from the params list for every chunk-----------
        # Do not change this without changing the output$Rmd_source function
        # this chunk is removed for local knitting.
        Rmd_script,
        "```{R, echo = FALSE}\n",
        "df <- params$df\ndf2 <- params$df2\n",
        "```\n"
      )

      # save chunks in 'ix' based on user's selected chunks
      if ("All chunks" %in% input$selected_chunk_report) {
        ix <- seq_along(logs$code_history)
      } else if ("All chunks without errors" %in% input$selected_chunk_report) {
        ix <- which(!sapply(logs$code_history, `[[`, "error"))
      } else {
        ix <- as.integer(input$selected_chunk_report)
      }

      # Append all selected chunks (ix) to the RMarkdown script
      Rmd_script <- paste0(
        Rmd_script,
        paste0(
          sapply(ix, function(i) logs$code_history[[i]]$rmd),
          collapse = "\n"
        )
      )

      # Return the total script
      return(Rmd_script)
    })


    # RMarkdown chunk for the current request
    Rmd_chunk <- reactive({
      req(llm_response()$cmd, llm_prompt())

      # Initialize Rmd_script
      Rmd_script <- if (use_python()) {   # add necessary setup when using Python
        "```{R}\nlibrary(reticulate)\n```\n```{python, message=FALSE}\ndf = r.df\n```\n"
      } else {   # using R, don't need setup
        ""
      }

      # User's request
      # remove unnecessary commands (pre_text) from the prompt
      request_text <- gsub(
        paste0("\n|", pre_text, "|.*"),
        "",
        llm_prompt()
      )
      # Collapse the result into a single string
      request_text <- paste(request_text, collapse = " ")

      # Append request & model info to RMarkdown script
      Rmd_script <- paste0(
        Rmd_script,
        "\n### ", counter$requests, ". ", request_text,
        "\n", names(selected_model()), " (Temperature = ", sample_temp(), ")\n"
      )

      # Set code chunk evaluation status (based on R or Python)
      eval_status <- if (use_python()) {
        if (python_to_html() == -1) ", eval = FALSE" else ""
      } else {
        if (code_error()) ", eval = FALSE" else ""
      }

      # Get code chunk
      cmd <- llm_response()$cmd
      # If an empty first line exists -> remove it
      if (nchar(cmd[1]) == 0) {
        cmd <- cmd[-1]
      }

      # Add code to script
      Rmd_script <- paste0(
        Rmd_script,
        "```{",
        ifelse(use_python(), "python", "R"),  # coding language
        eval_status, "}",   # evaluation status
        paste(cmd, collapse = "\n"),   # code chunk
        "\n```\n"
      )

      # Indicate error if any
      if (code_error()) {
        Rmd_script <- paste0(Rmd_script, "** Error **  \n")
      }

      return(Rmd_script)
    })

    output$html_report <- renderUI({
      req(llm_response()$cmd)
      tagList(
        actionButton(
          inputId = ns("report"),
          label = strong("Session Report"),
          class = "custom-action-button"
        ),
        tippy::tippy_this(
          ns("report"),
          "Render a HTML report for this session.",
          theme = "light-border"
        )
      )
    })

    output$rmd_chunk_output <- renderText({
      req(Rmd_chunk())
      Rmd_total()
    })

    # Markdown report
    output$Rmd_source <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = "RTutor.Rmd",
      content = function(file) {
        Rmd_script <- paste0(
          "---\n",
          "title: \"RTutor report\"\n",
          "author: \"RTutor, Powered by ChatGPT\"\n",
          "date: \"", date(), "\"\n",
          "output: html_document\n",
          "---\n",
          # this chunk is not needed when downloading the Rmd and knit locally
          gsub(
            "```\\{R, echo = FALSE\\}\ndf <- params\\$df\n```\n",
            "",
            Rmd_total()
          )
        )
        writeLines(Rmd_script, file)
      }
    )


    report_file <- reactiveVal(NULL)

    observeEvent(input$report, {
      req(!use_python(), !is.null(current_data()),
        llm_response()$cmd, llm_prompt()
      )

      withProgress(message = "Generating Report (5 minutes)", {
        incProgress(0.2)

        # Initialize tempReport and output_file
        tempReport <- file.path(tempdir(), "report.Rmd")
        tempReport <- gsub("\\", "/", tempReport, fixed = TRUE)
        output_file <- gsub("Rmd$", "html", tempReport)

        # Create RMarkdown Header and Content
        Rmd_script <- paste0(
          # Header
          # ensure spacing & indentation is in YAML format
          "---\n",
          "title: \"RTutor.ai report\"\n",
          "author: \"RTutor v.", release, ", Powered by ChatGPT\"\n",
          "date: \"", date(), "\"\n",
          "output: html_document\n",
          "params:\n  df: \n  df2: \n",
          "printcode:\n  label: \"Display Code\"\n",
          "  value: TRUE\n  input: checkbox\n",
          "---\n\n### ",
          # Content
          Rmd_total()
        )

        write(Rmd_script, file = tempReport, append = FALSE)

        # Prepare parameters for rendering the RMarkdown
        params <- list(df = iris) # dummy
        df2 <- NULL
        # if (!is.null(current_data_2())) {
        #   df2 <- current_data_2()
        # }
        # if uploaded, use that data
        if (!is.null(current_data())) {
          params <- list(
            df = current_data(),
            df2 = df2
          )
        }

        # Render Report
        tryCatch({
          rmarkdown::render(
            input = tempReport, # markdown location
            output_file = output_file,
            params = params,
            envir = new.env(parent = globalenv())
          )
          report_file(output_file)

          # Show modal with download button
          showModal(modalDialog(
            title = "Successfully rendered the report!",
            downloadButton(outputId = ns("download_report"), label = "Download"),
            easyClose = TRUE
          ))
        }, error = function(e) {
          showNotification(
            "Error when generating the report. Please try again.\nError Message: ", e$message,
            id = ns("report_error"),
            duration = 5,
            type = "error"
          )
        })
      })
    })

    # Markdown report
    output$download_report <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = "RTutor_report.html",
      content = function(file) {
        validate(
          need(!is.null(report_file()), "File not found.")
        )
        file.copy(from = report_file(), to = file, overwrite = TRUE)
      }
    )

    # Markdown report
    output$report_dsdsdf <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = "RTutor_report.html",
      content = function(file) {
        withProgress(message = "Generating Report ...", {
          incProgress(0.2)

          # Initialize tempReport
          tempReport <- file.path(tempdir(), "report.Rmd")
          tempReport <- gsub("\\", "/", tempReport, fixed = TRUE)

          req(llm_response()$cmd, llm_prompt())

          # Create RMarkdown header & content
          Rmd_script <- paste0(
            # Header
            # ensure spacing & indentation is in YAML format
            "---\n",
            "title: \"RTutor.ai report\"\n",
            "author: \"RTutor v.", release, ", Powered by ChatGPT\"\n",
            "date: \"", date(), "\"\n",
            "output: html_document\n",
            "params:\n  df: null\n  df2: null\n",
            "printcode:\n  label: \"Display Code\"\n",
            "  value: TRUE\n  input: checkbox\n",
            "---\n\n### ",
            # Content
            Rmd_total()
          )

          write(Rmd_script, file = tempReport, append = FALSE)

          # Prepare parameters for rendering the RMarkdown
          params <- list(df = iris) # dummy
          df2 <- NULL
          # if (!is.null(current_data_2())) {
          #   df2 <- current_data_2()
          # }
          # if uploaded, use that data
          if (!is.null(current_data())) {
            params <- list(
              df = current_data(),
              df2 = df2
            )
          }

          # Knit the document, passing in the `params` list, and eval it in a
          # child of the global environment (isolates the code in the document
          # from the code in the app).
          rmarkdown::render(
            input = tempReport, # markdown_location,
            output_file = file,
            params = params,
            envir = new.env(parent = globalenv())
          )
        })
      }
    )

    # Return reactive values so they can be used outside the module
    return(
      list(
        Rmd_chunk = Rmd_chunk
      )
    )

  })
}
