###################################################
# RTutor.AI, a Shiny app for chating with your data
# Author: Xijin Ge    gexijin@gmail.com
# Dec. 6-12, 2022.
# No warranty and not for commercial use.
###################################################

#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {


  #                             1. Module 02
  #____________________________________________________________________________
  #  Loading data
  #____________________________________________________________________________

  # 'Load Data' module
  mod_02 <- mod_02_load_data_serv("load_data")

  # Rename the reactive values for easier use
  input_text <- reactive({  mod_02$input_text() })
  selected_dataset_name <- reactive({ mod_02$selected_dataset_name()  })
  submit_button <- reactive({ mod_02$submit_button()  })
  reset_button <- reactive({  mod_02$reset_button() })
  use_python <- reactive({  FALSE })


  #                             2. Module 03
  #____________________________________________________________________________
  #   Main Panel
  #____________________________________________________________________________
  
  # 'Main Panel' module
  tabs <- reactive({ input$tabs })

  mod_03 <- mod_03_main_panel_serv(
    id = "main_panel",
    openAI_response = openAI_response,
    logs = logs,
    code_error = code_error,
    run_result = run_result,
    run_env_start = run_env_start,
    submit_button = submit_button,
    use_python = use_python,
    tabs = tabs,
    current_data = current_data,
    selected_dataset_name = selected_dataset_name
  )

  # Rename the reactive values for easier use
  selected_chunk <- reactive({  mod_03$selected_chunk() })


  #                             3. Module 04
  #____________________________________________________________________________
  #   LLM Component Management
  #____________________________________________________________________________

  # 'LLM Mgmt' module
  mod_04 <- mod_04_llm_mgmt_serv(
    id = "llm_mgmt",
    submit_button = submit_button, 
    input_text = input_text, 
    selected_dataset_name = selected_dataset_name
  )

  # Rename the reactive values for easier use
  api_key_session <- reactive({  mod_04$api_key_session() })
  sample_temp <- reactive({  mod_04$sample_temp() })
  selected_model <- reactive({  mod_04$selected_model() })


  #                             4. Module 5
  #____________________________________________________________________________
  # API Request & Response
  #____________________________________________________________________________

  openAI_prompt <- reactive({
    req(submit_button())
    req(available_datasets[[selected_dataset_name()]])
    req(input_text())
    isolate({ # so that it does not do it twice with each submit
      prep_input(input_text(), selected_dataset_name(), current_data(), use_python(), logs$id, selected_model())
    })
  })

  relevancy_response <- reactiveVal(TRUE) # Initializing relevancy_response to be TRUE as a reactive variable
  meta_data_res <- meta_data()
  meta_data_csv_res <- meta_data_csv()

  openAI_response <- reactive({
    req(submit_button())

    isolate({  # so that it will not respond to text, until submitted
      req(input_text())
      prepared_request <- openAI_prompt()
      req(prepared_request)
      req(available_datasets[[selected_dataset_name()]])  # require user to select a dataset

      # when submit is clicked, but no data is uploaded.
      if(selected_dataset_name() == uploaded_data) {
        req(user_data())
      }

      # Loading spinner, displays jokes
      shinybusy::show_modal_spinner(
        spin = "orbit",
        text = sample(jokes, 1),
        color = "#000000"
      )

      start_time <- Sys.time()

      # Send to openAI
      tryCatch(
        if(selected_model() == "text-davinci-003") { # completion model: davinci-text-003
          response <- openai::create_completion(
            engine_id = selected_model(),
            prompt = prepared_request,
            openai_api_key = api_key_session()$api_key,
            max_tokens = 1000,
            temperature = sample_temp()
          )
        } else {

          prompt_total <- list()

          # System role: You are an experienced programmer, etc
          if (!is.null(system_role)) {
            if (nchar(system_role) > 10) {
              prompt_total <- append(
                prompt_total,
                list(list(
                  role = "system",
                  content = system_role

                ))
              )
            }
          }

          # add history, first, if any
          if (length(logs$code_history) > 0) {

            # if there's history, identify and load dataset
            df_name <- available_datasets[[selected_dataset_name()]]
            selected_file_path <- paste0(data_path, df_name)
            df <- readRDS(selected_file_path)
            if (convert_to_factor()) {
              df <- numeric_to_factor(
                df,
                max_levels_factor(),
                max_proportion_factor()
              )
            }

            # update the current_data() reactive value
            current_data(df)
            selected_file(df_name) # update selected_file() reactive value
            # update runtime environment with new data frame
            run_env(rlang::env(run_env(), df = current_data(), df_name = selected_file()))
            run_env_start(as.list(run_env()))
            

            # HISTORY #
            # manage context length. If it is too long, remove the oldest ones, except the first one
            history_tokens <- sapply(
              1:length(logs$code_history),
              function(i) {
                if(i == 1) {
                  logs$code_history[[i]]$prompt_tokens + logs$code_history[[i]]$output_tokens
                } else {
                  # since the chat history includes previous prompt and output
                  logs$code_history[[i]]$prompt_tokens + logs$code_history[[i]]$output_tokens  - logs$code_history[[i - 1]]$prompt_tokens - logs$code_history[[i - 1]]$output_tokens
                }
              }
            )

            #cumulative from backwards
            cum_sum <- rev(cumsum(rev(history_tokens)))
                                                                  # new request               # first one
            cutoff <-  max_content_length - tokens(prepared_request) - history_tokens[1]

            cum_sum[1] <- 0 # do not remove the first one
            included <- which(cum_sum < cutoff)  # 1, 4, 5, 6, 7

            # add each chunk, only keep chunk
            history <- list()
            for(i in included) {
              history <- append(
                history,
                list(list(role = "user", content = logs$code_history[[i]]$prompt_all))
              )

              #append error message. Only the last one
              # prevent error status are not logged correctly
              code_plus_error <- logs$code_history[[i]]$raw
              if(i == length(logs$code_history) && code_error()) {
                code_plus_error <- paste0(
                  code_plus_error,
                  "\n\nError: ",
                  run_result()$error_message
                )
              }

              history <- append(
                history,
                list(list(role = "assistant", content = code_plus_error))
              )
            }
            prompt_total <- append(prompt_total, history)

            # RELEVANCY AGENT #
            # Is the user's question relevant?
            # Construct prompt
            sub_meta_data_csv <- meta_data_csv_res %>%
              filter(file_name == df_name) %>% 
              mutate(file_name = case_when(
                file_name == df_name ~ "df"
            ))
            sub_meta_data_json <- jsonlite::toJSON(sub_meta_data_csv)

            relevancy_prompt <- list(list(
              role = "user",
              content = paste(
                "Determine if the current prompt is relevant to any of the previous prompts. The prompt is relevant if it is a followup question for the analysis on the current dataset. If the prompt is a question about a different dataset it is not relevant. The prompt is also relevant if it is a modification for the visualizations. If it is relevant, respond with 'True'. Otherwise, respond with 'False'. Current prompt: ",
                input_text(),
                "Current dataset: ",
                sub_meta_data_json
              ) # AND relevant to the current dataset
            ))
            prompt_total_test <- append(prompt_total, relevancy_prompt)
            prompt_total_test[[1]][[2]] <- paste("Act as an experienced data analyst. Determine if the following prompts are relevant to the metadata: ", meta_data_res)

            # ChatGPT API
            response <- openai::create_chat_completion(  # chat model: gpt-3.5-turbo, gpt-4
              model = selected_model(),
              openai_api_key = api_key_session()$api_key,
              #max_tokens = 500,
              temperature = sample_temp(),
              messages = prompt_total_test
            )

            # Store True or False

            yn <- tolower(response$choices$message.content) == "true"
            relevancy_response(yn) # Update relevancy_response with TRUE\FALSE from OpenAI

          } else {  # if first prompt,  identify and load dataset

            # user selected file
            df_name <- available_datasets[[selected_dataset_name()]]

            # show message for 10s with the fine name
            showNotification(
              paste("Selected dataset: ", selected_dataset_name()),
              duration = 10
            )

            selected_file_path <- paste0(data_path, df_name)
            df <- readRDS(selected_file_path)
            if (convert_to_factor()) {
              df <- numeric_to_factor(
                df,
                max_levels_factor(),
                max_proportion_factor()
              )
            }
            # update the current_data() reactive value
            current_data(df)
            selected_file(df_name) # update the selected_file() reactive value
            # update runtime environment with new data frame
            run_env(rlang::env(run_env(), df = current_data(), df_name = selected_file()))
            run_env_start(as.list(run_env()))

            # Is the user's question relevant? -- relevancy agent
            # Construct prompt

            sub_meta_data_csv <- meta_data_csv_res %>%
              filter(file_name == df_name) %>% 
              mutate(file_name = case_when(
                file_name == df_name ~ "df"
            ))
            sub_meta_data_json <- jsonlite::toJSON(sub_meta_data_csv)

            relevancy_prompt <- list()
            relevancy_prompt <- append(
              relevancy_prompt,
              list(list(
                role = "system",
                content = paste("Act as an experienced data analyst. Determine if the following prompts are relevant to the metadata: ",
                meta_data_res)
              ))
            )
            relevancy_prompt <- append(
              relevancy_prompt,
              list(list(
                role = "user",
                content = paste(
                "Determine if the current prompt is relevant to the selected dataset. If it is relevant, respond with 'True'. Otherwise, respond with 'False'. Current prompt: ",
                input_text(),
                "Current dataset: ",
                sub_meta_data_json
              ) # AND relevant to the current dataset
              ))
            )

            # ChatGPT API
            response <- openai::create_chat_completion(  # chat model: gpt-3.5-turbo, gpt-4
              model = selected_model(),
              openai_api_key = api_key_session()$api_key,
              #max_tokens = 500,
              temperature = sample_temp(),
              messages = relevancy_prompt
            )

            # Store True or False
            yn <- tolower(response$choices$message.content) == "true"
            relevancy_response(yn)

          } # end first user prompt

          if (relevancy_response()) {
            prepared_request = prep_input(input_text(), selected_dataset_name(), current_data(), use_python(), logs$id, selected_model())

            # Subsetting Meta Data csv file to send in with prompt
            sub_meta_data_csv <- meta_data_csv_res %>%
              filter(file_name == df_name) %>% 
              mutate(file_name = case_when(
                file_name == df_name ~ "df"
                ))
            sub_meta_data_json <- jsonlite::toJSON(sub_meta_data_csv)

            # add new user prompt
            prompt_total <- append(
              prompt_total,
              list(list(
                role = "user",
                content = paste(
                  prepared_request,
                  # additional_info,
                  # system_role_growth,
                  system_role_date,
                  # "If user mentions growth, then ensure...",
                  "Available datasets: \"\"\"",
                  sub_meta_data_json,
                  "\"\"\""
                  )
                ))
              )

            response <- openai::create_chat_completion(  # chat model: gpt-3.5-turbo, gpt-4
              model = selected_model(),
              openai_api_key = api_key_session()$api_key,
              #max_tokens = 500,
              temperature = sample_temp(),
                messages = prompt_total
            )

              # to make the returned code at the same spot, as davinci model.
              response$choices[1, 1] <- response$choices$message.content


            } else {
              
              response <- openai::create_chat_completion(  # chat model: gpt-3.5-turbo, gpt-4
                model = selected_model(),
                openai_api_key = api_key_session()$api_key,
                # max_tokens = 500,
                temperature = sample_temp(),
                messages = list(list(
                  role = "user",
                  content = paste("Return this exact statement:",
                  "print('Please ask a question related to HMCL dataset", selected_dataset_name(),"and try again. (Reset to select a different dataset)')")
                ))
              )

              # to make the returned code at the same spot, as davinci model.
              response$choices[1, 1] <- response$choices$message.content
              relevancy_response(TRUE) # Reinitiate the relevancy to be TRUE

            } # end relevancy agent

        },
        error = function(e) {
          # remove spinner, show message for 5s, & reload
          shinybusy::remove_modal_spinner()
          shiny::showModal(api_error_modal)
          Sys.sleep(5)
          session$reload()

          list(
            error_value = -1,
            message = capture.output(print(e$message)),
            error_status = TRUE
          )
        }
      )

      error_api <- FALSE
      # if error returns true, otherwise
      # that slot does not exist, returning false.
      # or be NULL
      error_api <- tryCatch(
        !is.null(response$error_status),
        error = function(e) {
          return(TRUE)
        }
      )

      error_message <- NULL
      if(error_api) {
        cmd <- NULL
        response <- NULL
        error_message <- response$message
      } else {
        cmd <- response$choices[1, 1]
      }

      api_time <- difftime(
        Sys.time(),
        start_time,
        units = "secs"
      )[[1]]

      if(0) {
        # if more than 10 requests, slow down. Only on server.
        if(counter$requests > 20 && file.exists(on_server)) {
          Sys.sleep(counter$requests / 5 + runif(1, 0, 5))
        }
        if(counter$requests > 50 && file.exists(on_server)) {
          Sys.sleep(counter$requests / 10 + runif(1, 0, 10))
        }
      }

      if(counter$requests > 100 && file.exists(on_server)) {
        Sys.sleep(counter$requests / 40 + runif(1, 0, 40))
      }

      shinybusy::remove_modal_spinner()

    # update usage via global reactive value/ ouput token is twice as expensive
    counter$tokens_current <- response$usage$completion_tokens + response$usage$prompt_tokens
    counter$requests <- counter$requests + 1
    counter$time <- round(api_time, 0)
    counter$costs_total <- counter$costs_total +
      api_cost(response$usage$prompt_tokens, response$usage$completion_tokens, selected_model())

      return(
        list(
          cmd = polish_cmd(cmd),
          response = response,
          time = round(api_time, 0),
          error = error_api,
          error_message = error_message
        )
      )
    })
  })



  #                             5. Module 06
  #____________________________________________________________________________
  #  Error handling, record keeping/chunk history
  #____________________________________________________________________________


  # a modal shows api connection error
  api_error_modal <- shiny::modalDialog(
    title = "API connection error!",
    tags$h4("Is the API key is correct?", style = "color:red"),
    tags$h4("How about the WiFi?", style = "color:red"),
    tags$h4("Maybe the openAI.com website is taking forever to respond.", style = "color:red"),
    tags$h5("If you keep having trouble, send us an email.", style = "color:red"),
    tags$h4(
      "Auto-reset ...",
      style = "color:blue; text-align:right"
    ),
    easyClose = TRUE,
    size = "s"
  )

  # show a warning message when reached 10c, 20c, 30c ...
  observeEvent(submit_button(), {
    req(file.exists(on_server))
    req(!openAI_response()$error)

    cost_session <-  counter$costs * 10
    if (cost_session %% 5  == 0 & cost_session != 0) {
      shiny::showModal(
        shiny::modalDialog(
          size = "s",
          easyClose	= TRUE,
          h4(
            paste0(
              "Cumulative API Cost reached ",
              cost_session,
              "¢"
            )
          ),
          h4("Slow down. Please try to use your own API key.")
        )
      )
    }
  })

  # Error when running the generated code
  code_error <- reactive({
    error_status <- FALSE
    req(submit_button() != 0) #Require the submit button to be pushed
    if(!use_python()) { # R
      return(!is.null(run_result()$error_message) && run_result()$error_message != "")
    } else { # Python
      return(python_to_html() == -1)
    }
  })

  # Show notification when error
  observeEvent(code_error(), {
    # show notification message
    if(code_error()) {
      showNotification(
        "Resubmit the same request to see if ChatGPT can resolve the error.
        If that fails, change the request.",
        duration = 10
      )
    }
  })

 # Defining & initializing the reactiveValues object
  logs <- reactiveValues(
    id = 0, # 1, 2, 3, id for code chunk
    code = "", # cumulative code
    raw = "",  # cumulative orginal code for print out
    last_code = "", # last code for Rmarkdown
    language = "", # Python or R
    code_history = list(), # keep all code chunks
  )

  # Defining & initializing the reactiveValues object
  counter <- reactiveValues(
    costs_total = 0, # cummulative cost
    requests = 0, # cummulative requests
    tokens_current = 0,  # tokens for current query
    time = 0 # response time for current
  )

  observeEvent(submit_button(), {

    logs$id <- logs$id + 1

    logs$code <-  openAI_response()$cmd

    logs$raw <- openAI_response()$cmd #openAI_response()$response$choices[1, 1]
    # remove one or more blank lines in the beginning.
    logs$raw <- gsub("^\n+", "", logs$raw)
    logs$last_code <- ""
    logs$language <- ifelse(use_python(), "Python", "R")

    # A list holds current request
    current_code <- list(
      id = logs$id,
      code = logs$code,
      raw = logs$raw, # for print
      prompt = input_text(),
      prompt_all = openAI_prompt(), # entire prompt, as sent to openAI
      error = code_error(),
      error_message = run_result()$error_message,
      language = ifelse(use_python(), "Python", "R"),
      # saves the rendered file in the logs object.
      html_file = ifelse(use_python(), python_to_html(), -1),
      prompt_tokens = openAI_response()$response$usage$prompt_tokens,
      output_tokens = openAI_response()$response$usage$completion_tokens,
      # save a copy of the data in the environment as a list.
      # if save environment, only reference is saved.
      # This needs more memory, but works.
      env = run_env_start() # it is a list;
    )

    logs$code_history <- append(logs$code_history, list(current_code))

    choices <- 1:length(logs$code_history)
    names(choices) <- paste0("Chunk #", choices)

    # update chunk choices
    updateSelectInput(
      session = session,
      inputId = "main_panel-selected_chunk",
      label = "AI generated code:",
      choices = choices,
      selected = logs$id
    )
  })

  # change value when a previous code chunk is selected
  reverted <- reactiveVal(0)

  # change code when past code is selected
  observeEvent(selected_chunk(), {
    req(selected_chunk())
    id <- as.integer(selected_chunk())
    logs$code <- logs$code_history[[id]]$code
    logs$raw <- logs$code_history[[id]]$raw

    # Switch to previous chunks
    if(id < length(logs$code_history)) {
      # convert list to environment;
      # update the run_env reactive value.
      # restore the environment to the before  running the ith chunk
      run_env(list2env(logs$code_history[[id]]$env))

      # enable re-calculation of the code
      reverted(reverted() + 1)

      showNotification(
        ui = paste("Switched back to chunk #", id,
        ". Any change in the data is also reverted." ),
        id = "revert_chunk",
        duration = 5,
        type = "warning"
      )
    }

    updateTextInput(
      session,
      "input_text",
      value = logs$code_history[[id]]$prompt
    )

    # change language
    if(submit_button() != 0) {
      updateCheckboxInput(
        session = session,
        inputId = "use_python",
        value = (logs$code_history[[id]]$language == "Python")
      )
    }

  })


  #                            6. Module 07
  #____________________________________________________________________________
  # Run the code, data prep, show code
  #____________________________________________________________________________


  ### Initialize reactives ###

  # the current data & file name
  current_data <- reactiveVal(NULL)
  selected_file <- reactiveVal(NULL)

  # define a reactive variable that holds an R environment
  # This is needed for the Rmd chunk
  run_env <- reactiveVal(new.env())

  # a list stores all data objects before running the code
  run_env_start <- reactiveVal(list())
  # define a reactive variable. Reactive function not returning error
  run_result <- reactiveVal(list())


  # "Run Code" module
  mod_07 <- mod_07_run_code_serv(
    id = "run_code",
    run_env = run_env,
    run_env_start = run_env_start,
    run_result = run_result,
    submit_button = submit_button,
    reverted = reverted,
    logs = logs,
    use_python = use_python,
    selected_dataset_name = selected_dataset_name,
    current_data = current_data,
    selected_file = selected_file
  )

  # Rename the reactive values for easier use
  convert_to_factor <- reactive({  mod_07$convert_to_factor() })
  max_proportion_factor <- reactive({  mod_07$max_proportion_factor() })
  max_levels_factor <- reactive({  mod_07$max_levels_factor() })


  #                             7. Module 08
  #____________________________________________________________________________
  #  Miscellaneous
  #____________________________________________________________________________

  # 'Misc' module
  mod_08 <- mod_08_misc_serv(
    id = "misc",
    reset_button = reset_button,
    submit_button = submit_button,
    logs = logs,
    use_python = use_python,
    current_data = current_data,
    selected_dataset_name = selected_dataset_name
  )

  # Rename the reactive values for easier use
  python_to_html <- reactive({  mod_08$python_to_html() })

  pdf(NULL) # otherwise, base R plots sometimes do not show


}
