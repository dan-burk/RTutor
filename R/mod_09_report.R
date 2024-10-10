

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
                               openAI_response, openAI_prompt, use_python,
                               counter, sample_temp, code_error, python_to_html,
                               current_data) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Dropdown to pick what chunks to include in report
    observeEvent(submit_button(), {
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

      Rmd_script <- ""

      # if first chunk
      Rmd_script <- paste0(
        Rmd_script,
        # Get the data from the params list-----------
        "\nDeveloped by [Steven Ge](https://twitter.com/StevenXGe) using API
         access via the
        [openai](https://cran.rstudio.com/web/packages/openai/index.html)
        package to
        [OpenAI's](https://cran.rstudio.com/web/packages/openai/index.html) \"",
        selected_model(),
        "\" model.",
        "\n\nRTutor Website: [https://RTutor.ai](https://RTutor.ai)",
        "\nSource code: [GitHub.](https://github.com/gexijin/RTutor)\n"
      )

      # if the first chunk & data is uploaded,
      # insert script for reading data
      # if (input$select_data == uploaded_data) {

      #   # Read file
      #   file_name <- input$user_file$name
      #   if (user_data()$file_type == "read_excel") {
      #     txt <- paste0(
      #       "# install.packages(readxl)\nlibrary(readxl)\ndf <- read_excel(\"",
      #       file_name,
      #       "\")"
      #     )

      #   }
      #   if (user_data()$file_type == "read.csv") {
      #     txt <- paste0(
      #       "df <- read.csv(\"",
      #       file_name,
      #       "\")"
      #     )
      #   }
      #   if (user_data()$file_type == "read.table") {
      #     txt <- paste0(
      #       "df <- read.table(\"",
      #       file_name,
      #       "\", sep = \"\t\", header = TRUE)"
      #     )
      #   }

      #   Rmd_script <- paste0(
      #     "\n### 0. Read File\n",
      #     "```{R, eval = FALSE}\n",
      #     txt,
      #     "\n```\n"
      #   )
      # }

      Rmd_script <- paste0(
        Rmd_script,
        # Get the data from the params list for every chunk-----------
        # Do not change this without changing the output$Rmd_source function
        # this chunk is removed for local knitting.
        "```{R, echo = FALSE}\n",
        "df <- params$df\ndf2 <- params$df2\n",
        "```\n"
      )

      #------------------Add selected chunks
      if ("All chunks" %in% input$selected_chunk_report) {
        ix <- seq_along(logs$code_history)
      } else if ("All chunks without errors" %in% input$selected_chunk_report) {
        ix <- c()
        for (i in seq_along(logs$code_history)) {
          if (!logs$code_history[[i]]$error) {
            ix <- c(ix, i)
          }
        }
      } else {  # selected
        ix <- as.integer(input$selected_chunk_report)
      }

      for (i in ix) {
        Rmd_script <- paste0(Rmd_script, "\n", logs$code_history[[i]]$rmd)
      }

      return(Rmd_script)
    })


    # Markdown chunk for the current request
    Rmd_chunk <- reactive({
      req(openAI_response()$cmd)
      req(openAI_prompt())

      Rmd_script <- ""

      if (use_python()) {
        Rmd_script <- paste0(
          Rmd_script,
          "```{R}\n",
          "library(reticulate)\n",
          "```\n",
          "```{python, message=FALSE}\n",
          "df = r.df\n",
          "```\n"
        )
      }

      # User request----------------------
      Rmd_script  <- paste0(
        Rmd_script,
        "\n### ",
        counter$requests,
        ". ",
        paste(
          #remove pre-inserted commands
          gsub(
            paste0(
              "\n|",
              pre_text,
              "|",
              after_text,
              ".*"
            ),
            "",
            openAI_prompt()
          ),
          collapse = " "
        ),
        paste0(
          "\n ",
          names(selected_model()),
          " (Temperature=",
          sample_temp(),
          ")"
        ),
        "\n"
      )

      # R Markdown code chunk----------------------
      if (!use_python()) {  # R code chunk
        #if error when running the code, do not run
        if (code_error() == TRUE) {
          Rmd_script <- paste0(
            Rmd_script,
            "```{R, eval = FALSE}"
          )
        } else {
          Rmd_script <- paste0(
            Rmd_script,
            "```{R}"
          )
        }
      } else {  # Python code chunk
        #if error when running the code, do not run
        if (python_to_html() == -1) {
          Rmd_script <- paste0(
            Rmd_script,
            "```{python, eval = FALSE}"
          )
        } else {
          Rmd_script <- paste0(
            Rmd_script,
            "```{python}"
          )
        }
      }
      cmd <- openAI_response()$cmd
      # remove empty line
      if (nchar(cmd[1]) == 0) {
        cmd <- cmd[-1]
      }

      # Add R code
      Rmd_script <- paste0(
        Rmd_script,
        paste(
          cmd,
          collapse = "\n"
        ),
        "\n```\n"
      )

      # indicate error
      if (code_error()) {
        Rmd_script <- paste0(
          Rmd_script,
          "** Error **  \n"
        )
      }

      return(Rmd_script)
    })

    output$html_report <- renderUI({
      req(openAI_response()$cmd)
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
          "date: \"",
          date(), "\"\n",
          "output: html_document\n",
          "---\n",
          # this chunk is not needed when they download the Rmd and knit locally
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
      #req(input$select_data != no_data)
      req(!use_python())
      req(!is.null(current_data()))


      withProgress(message = "Generating Report (5 minutes)", {
        incProgress(0.2)
        tempReport <- file.path(tempdir(), "report.Rmd")
        # tempReport
        tempReport <- gsub("\\", "/", tempReport, fixed = TRUE)

        req(openAI_response()$cmd)
        req(openAI_prompt())
        output_file <- gsub("Rmd$", "html", tempReport)

        #RMarkdown file's Header
        Rmd_script <- paste0(
          "---\n",
          "title: \"RTutor.ai report\"\n",
          "author: \"RTutor v.",
          release,
          ", Powered by ChatGPT\"\n",
          "date: \"",
          date(), "\"\n",
          "output: html_document\n",
          "params:\n",
          "  df:\n",
          "  df2:\n",
          "printcode:\n",
          "  label: \"Display Code\"\n",
          "  value: TRUE\n",
          "  input: checkbox\n",
          "---\n"
        )

        Rmd_script <- paste0(
          Rmd_script,
          "\n\n### "
        )

        # R Markdown code chunk----------------------

        # Add R code
        Rmd_script <- paste(
          Rmd_script,
          Rmd_total()
        )

        write(
          Rmd_script,
          file = tempReport,
          append = FALSE
        )

        # Set up parameters to pass to Rmd document
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


        req(params)

        tryCatch({
          rmarkdown::render(
            input = tempReport, # markdown_location,
            output_file = output_file,
            params = params,
            envir = new.env(parent = globalenv())
          )
        },
        error = function(e) {
          showNotification(
            ui = paste("Error when generating the report. Please try again."),
            id = ns("report_error"),
            duration = 5,
            type = "error"
          )
        },
        finally = {
          report_file(output_file)
          # show modal with download button
          showModal(modalDialog(
            title = "Successfully rendered the report!",
            downloadButton(
              outputId = ns("download_report"),
              label = "Download"
            ),
            easyClose = TRUE
          ))
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

          tempReport <- file.path(tempdir(), "report.Rmd")
          # tempReport
          tempReport <- gsub("\\", "/", tempReport, fixed = TRUE)

          req(openAI_response()$cmd)
          req(openAI_prompt())

          #RMarkdown file's Header
          Rmd_script <- paste0(
            "---\n",
            "title: \"RTutor.ai report\"\n",
            "author: \"RTutor v.",
            release,
            ", Powered by ChatGPT\"\n",
            "date: \"",
            date(), "\"\n",
            "output: html_document\n",
            "params:\n",
            "  df:\n",
            "  df2:\n",
            "printcode:\n",
            "  label: \"Display Code\"\n",
            "  value: TRUE\n",
            "  input: checkbox\n",
            "---\n"
          )

          Rmd_script <- paste0(
            Rmd_script,
            "\n\n### "
          )

          # R Markdown code chunk----------------------

          # Add R code
          Rmd_script <- paste(
            Rmd_script,
            Rmd_total()
          )

          write(
            Rmd_script,
            file = tempReport,
            append = FALSE
          )

          # Set up parameters to pass to Rmd document
          params <- list(df = iris) # dummy
          df2 <- NULL
          # if (!is.null(current_data_2())) {
          #   df2 <- current_data_2()
          # }
          # # if uploaded, use that data
          # req(input$select_data)
          # if (input$select_data != no_data) {
          #   params <- list(
          #     df = current_data(),
          #     df2 = df2
          #   )
          # }


          req(params)
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
