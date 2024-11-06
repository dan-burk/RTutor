#____________________________________________________________________________
#  Data Types
#____________________________________________________________________________


mod_15_data_types_ui <- function(id) {

  ns <- NS(id)

  fluidRow(
    column(
      width = 12,
      tags$label("2. Modify Data Fields",
      style = "font-size: 18px;font-weight: bold;color: #333;display: block;margin-bottom: 5px;")
    ),
    br(),
    column(
      width = 12,
      actionButton(ns("data_edit_modal"), "Data Types"),
      align = "left",
      tags$head(tags$style(
        "#data_edit_modal{font-size: 18px;color: #000;background-color: #F6FFF5;border-color: #90BD8C}"
      ))
    )
  )

}



mod_15_data_types_serv <- function(id, modal_closed, run_env, run_env_start, current_data, logs) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    show_pop_up <- function() {
      showModal(
        modalDialog(
          title = "Verify Data Types (Important!)",
          tags$head(
            tags$style(HTML("
                #data_type_window {
                    height: 400px;  /* Adjust the height as needed */
                    overflow-y: auto;  /* Enables vertical scrolling */
                    padding: 10px;
                    border-radius: 5px;
                }
                .modal-footer {
                  display: flex;
                  justify-content: space-between;
                  align-items: center;
                }
                .button-group {
                  display: flex;
                  gap: 10px;  /* Adjusts the space between the buttons */
                  justify-content: flex-start;
                  flex-grow: 1;
                }
            "))
          ),
          # Custom CSS to make the chat area scrollable
          tabsetPanel(
            tabPanel("Dataset 1",
              div(id = "data_type_window", uiOutput(ns("column_type_ui"))),
              h4("If a column represents categories, choose 'Factor', even if
              it contains numbers. For columns that are numbers, but with few unique values, RTutor
              automatically converts them to factors. See Settings.",
              style = "color: blue")
            ),
            tabPanel("Dataset 2",
              condition = "1",
              div(id = "data_type_window", uiOutput(ns("column_type_ui_2"))),
              h4("If a column represents categories, choose 'Factor', even if
              it contains numbers. For columns that are numbers, but with few unique values, RTutor
              automatically converts them to factors. See Settings.",
              style = "color: blue")
            )
          ),
          br(),
          size = 'l',
          footer = tagList(
            div(class = "button-group",
              actionButton(ns("revert_data"), label = "Revert to Original Data")#,
              # conditionalPanel(
              #   condition = "output.column_type_ui_2 != null",  # Replace with your actual condition
              #   actionButton("revert_data2", label = "Revert to Original Data2")
              # )
            ),
            actionButton(ns("dismiss_modal"), label = "Dismiss")
          ),
          easyClose = TRUE
        )
      )
    }

    # Trigger the pop-up when a file is uploaded or when button is clicked
    observeEvent(input$data_edit_modal, {
      show_pop_up()

    })

    observeEvent(input$dismiss_modal, {
      modal_closed(TRUE)
      shiny::removeModal()
    })

    observeEvent(modal_closed(), {
      req(modal_closed())
      shiny::showNotification(
        "Know thy enemy. Exploratory your data at the EDA tab first.",
        duration = 10
      )
      modal_closed(FALSE)
    })


    output$column_type_ui <- renderUI({
      req(current_data())
      # req(input$select_data) #Not Implemented YET
      column_names <- names(current_data())
      examples <- capture.output(str(current_data()))
      examples <- examples[-1]
      examples <- gsub(" \\$ ", "", examples)
      withProgress(message = "Verifying data types ...", {
        incProgress(0.3)
        lapply(seq_along(column_names), function(i) {
          column_name <- column_names[i]
          fluidRow(
            column(
              width = 3,
              selectInput(
                inputId = ns(paste0("column_type_", i)),
                label = NULL,
                choices = c("Character" = "character",
                            "Numeric" = "numeric",
                            "Integer" = "integer",
                            "Date" = "Date",
                            "Factor" = "factor"),
                selected = class(current_data()[[i]])
              )
            ),
            column(
              width = 9,
              align = "left",
              style = "margin-top: -5px;",
              h5(examples[i])
            )
          )

        })
      })
    })


    observe({
      browser()
      req(current_data())
      req(input$revert_data == 0) #Negative: If we hit revert data button, we don't want to run all this junk
                                  #Positive: Run as long as we don't hit revert_data button
      req(is.null(input$submit_button) || input$submit_button == 0 || as.integer(input$selected_chunk) == length(logs$code_history))
        #Negative: If we revert a chunk we don't want to run all this junk
        #Positive: Run as long as our selected chunk is the current maximum code chunk.
          #(i.e. If submit_button == 0 then selected chunk would be NULL and maximum code chunk is 0\NULL)
      # req(run_data_process())
      for (i in seq_along(current_data())) {
        col_type <- input[[paste0("column_type_", i)]]
        if (!is.null(col_type)) {
          updated_data <- isolate(current_data())

          # when converting to factor the as function gives an error
          if(col_type == "factor") {
            updated_data[[i]] <- as.factor(updated_data[[i]])
          } else if (col_type == "Date") {
            updated_data[[i]] <- lubridate::parse_date_time(
              updated_data[[i]],
              orders = c("mdy", "dmy", "ymd")
            )
            updated_data[[i]] <- as.Date(updated_data[[i]])
          } else if(col_type == "numeric" & class(updated_data[[i]]) == "factor"){
            updated_data[[i]] <- as(as.character(updated_data[[i]]), col_type)
          } else {
            updated_data[[i]] <- as(updated_data[[i]], col_type)
          }
          current_data(updated_data)
          isolate({
            # run_env(rlang::env(run_env(), df = current_data()))
            # run_env_start(as.list(run_env()))

            #Code to update environment and not overwrite
            existing_vars <- as.list(run_env())
            run_env(list2env(existing_vars))
            run_env_start(as.list(run_env()))
          })
        }
      }

    })



    observeEvent(input$revert_data, {
      print("Observe 'Revert Data'")

        # run_data_process(FALSE)
        # current_data(original_data()) #Update current_data() to be the original_data()
        column_names <- names(current_data())
        lapply(seq_along(column_names), function(i) {
          column_name <- column_names[i]
          updateSelectInput(
            session = session,
            inputId = ns(paste0("column_type_", i)),
            label = NULL,
            choices = c("Character" = "character",
                        "Numeric" = "numeric",
                        "Integer" = "integer",
                        "Date" = "Date",
                        "Factor" = "factor"),
            selected = class(current_data()[[i]])
          )

        })

        existing_vars <- as.list(run_env())
        run_env(list2env(existing_vars))
        run_env_start(as.list(run_env()))

      # Close modal
      modal_closed(TRUE)
      shiny::removeModal()

      # Show message modal..or something

        shiny::showModal(
          shiny::modalDialog(
            size = "s",
            easyClose	= TRUE,
            h5("Successfully Reverted to Original Data")
          )
        )

    })




    # Return all reactive values so they can be used outside the module
    return(
      list(
        data_edit_modal = reactive(input$data_edit_modal),
        modal_closed = modal_closed, #Already Reactive, so wraping is likely redundant
        run_env = run_env,
        run_env_start = run_env_start
      )
    )

  })
}