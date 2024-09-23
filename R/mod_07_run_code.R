

#____________________________________________________________________________
#  Run the code, data prep, show code
#____________________________________________________________________________


mod_07_run_code_serv <- function(id, run_env, run_env_start, run_result, submit_button,
                                 reverted, logs, use_python, selected_dataset_name,
                                 current_data) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    ### Run the code ###

    # stores the results after running the generated code.
    # returns error indicator and message
    # Sometimes returns NULL, even when code runs fine. Especially when
    # a base R plot is generated.

    observeEvent(
      eventExpr = {
        submit_button()  # when submit is clicked
        reverted()       # or when a previous code chunk is selected
        logs$code
      }, {
      req(logs$code != "")
      req(!use_python())
      result <- NULL
      console_output <- NULL
      error_message <- NULL

      withProgress(message = "Running the code ...", {
        incProgress(0.4)

        run_env_start(as.list(run_env())) # keep a copy of the crime scene

        result <- tryCatch({
          eval_result <- eval(
            parse(text = clean_cmd(logs$code, selected_dataset_name(), file.exists(on_server))),
            envir = run_env()
          )
          console_output <- capture.output(print(eval_result))
          eval_result                      # without this the interactive plots does not work
        }, error = function(e) {
          list(error_message = e$message)  # won't work if not inside a list!
        })

       # update the error message, if any
        if(length(names(result)) != 0) {
          if(names(result)[1] == "error_message") {
            error_message <- result$error_message
          }
        }

        # Run with error
        if(!is.null(error_message)) {
          run_env(list2env(run_env_start()))  # revert the environment
        }

        run_result(list(
          result = result,
          console_output = console_output,
          error_message = error_message
        ))
      })
    })

  
    ### Data Prep ###

    # Convert data columns to factors
    # Treat the columns that look like a category as a category.
    # This applies to columns that contain numbers but have very few unique values.
    # The default is that these conversions are on.
    convert_to_factor <- reactive({
      convert <- TRUE # default, to turn off: use 'convert <- FALSE'

      # if (!is.null(input$numeric_as_factor)) {
      #   convert <- input$numeric_as_factor
      # }

      return(convert)
    })

    max_proportion_factor <- reactive({
      max_proportion <- unique_ratio  # default

      # if (!is.null(input$max_proportion_factor)) {
      #   max_proportion <- input$max_proportion_factor
      # }
      # if(max_proportion < 0.05) {
      #   max_proportion <- 0.05
      # }
      # if(max_proportion > 0.5) {
      #   max_proportion <- 0.5
      # }

      return(max_proportion)
    })


    max_levels_factor <- reactive({
      max_levels_1 <- max_levels_factor_conversion  # default

      # if (!is.null(input$max_levels_factor)) {
      #   max_levels_1 <- input$max_levels_factor
      # }
      # if (max_levels_1 < 2) {
      #   max_levels_1 <- 2
      # }
      # if (max_levels_1 > 100) {
      #   max_levels_1 <- 100
      # }

      return(max_levels_1)
    })

    observeEvent(available_datasets[[selected_dataset_name()]], {
      req(available_datasets[[selected_dataset_name()]])

      library(tidyverse)
      data <- current_data()
      eval(parse(text = paste0("df <- data")))

      if (convert_to_factor()) {
        df <- numeric_to_factor(
          df,
          max_levels_factor(),
          max_proportion_factor()
        )
      }

      # if the first column looks like id
      if (
        length(unique(df[, 1])) == nrow(df) &&  # all unique...what about duplicate ID's??
        is.character(df[, 1])  # first column is character
      ) {
        row.names(df) <- df[, 1]
        df <- df[, -1]
      }

      # sometimes no row is left after processing
      if (is.null(df)) {  # no_data
        current_data(NULL)
      } else if (nrow(df) == 0) {
        current_data(NULL)
      } else {  # there is data in the dataframe
        current_data(df)
      }

      # add the data to the current environment
      run_env(rlang::env(run_env(), df = current_data()))
      run_env_start(as.list(run_env()))
    })

    # Return reactive values so they can be used outside the module
    return(
      list(
        convert_to_factor = convert_to_factor,
        max_proportion_factor = max_proportion_factor,
        max_levels_factor = max_levels_factor
      )
    )
  })
}