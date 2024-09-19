

#____________________________________________________________________________
#  LLM Component Management
#____________________________________________________________________________


mod_04_llm_mgmt_serv <- function(id, submit_button, input_text, selected_dataset_name) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ____________________
    ### API Key ###
    # ____________________

    # Clean up API key characters
    clean_api_key <- function(api_key) {
      # remove spaces
      api_key <- gsub(" ", "", api_key)
      return(api_key)
    }

    # Get API key from environment variable
    api_key_global <- Sys.getenv("OPEN_API_KEY")
    key_source <- "from OS environment variable."

    # If there is an key file in the current folder, use that instead
    if (file.exists(file.path(getwd(), "api_key.txt"))) {
      api_key_file <- readLines(file.path(getwd(), "api_key.txt"))
      api_key <- clean_api_key(api_key_file)

      api_key_global <- api_key_file
      key_source <- "from file."
    }

    # API Key for the session
    api_key_session <- reactive({

      api_key <- api_key_global
      session_key_source <- key_source
        return(
          list(
            api_key = api_key,
            key_source = session_key_source
          )
        )
    })

    # Only save API key if app is running locally
    observeEvent(submit_button(), {
      # if too short, do not send.
      if (nchar(input_text()) < min_query_length) {
        showNotification(
          paste(
            "Request too short! Should be more than ",
            min_query_length,
            " characters."
          ),
          duration = 10
        )
      }
      # if too long, do not send
      if (nchar(input_text()) > max_query_length) {
        showNotification(
          paste(
            "Request too long! Should be less than ",
            max_query_length,
            " characters."
          ),
          duration = 10
        )
      }
      # if no file is selected, do not send
      if (is.null(available_datasets[[selected_dataset_name()]])) {
        showNotification(
          paste("No file found. Please select a dataset and try again."),
          duration = 10
        )
      }
    })


    # ____________________
    ### LLM Parameters ###
    # ____________________

    # Get sample temperature for LLM model
    sample_temp <- reactive({

      temperature <- default_temperature # default

      return(temperature)
    })

    # Get selected LLM model
    selected_model <- reactive({

      model <- language_models[default_model]  # gpt-4, default
      names(model) <- names(language_models)[language_models == model]  # Get the name of the model for display

      return(model)
    })


    # Return reactive values so they can be used outside the module
    return(
      list(
        api_key_session = api_key_session,
        sample_temp = sample_temp,
        selected_model = selected_model
      )
    )
  })
}