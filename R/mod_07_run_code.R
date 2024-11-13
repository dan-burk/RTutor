

#____________________________________________________________________________
#  Run the code, data prep, show code
#____________________________________________________________________________


mod_07_run_code_serv <- function(id, run_env, run_env_start, run_result, submit_button,
                                 reverted, logs, use_python, selected_dataset_name,
                                 current_data, convert_to_factor, max_proportion_factor,
                                 max_levels_factor) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ### Run the code ###

    # stores the results after running the generated code.
    # returns error indicator and message
    # Sometimes returns NULL, even when code runs fine. Especially when
    # a base R plot is generated.

    observeEvent(
      eventExpr = list(
        submit_button(),  # when submit is clicked
        reverted(),       # or when a previous code chunk is selected
        logs$code
      ), {
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
        if (length(names(result)) != 0) {
          if (names(result)[1] == "error_message") {
            error_message <- result$error_message
          }
        }

        # Run with error
        if (!is.null(error_message)) {
          run_env(list2env(run_env_start()))  # revert the environment
        }

        run_result(
          list(
            result = result,
            console_output = console_output,
            error_message = error_message
          )
        )
      })
    })

  })
}