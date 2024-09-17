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


  #                             1.
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


  #                             2.
  #____________________________________________________________________________
  # API key management
  #____________________________________________________________________________
  
  # Api key for the session
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

  output$session_api_source <- renderText({
    txt <- api_key_session()$api_key

    # The following is essential for correctly getting the
    # environment variable on Linux!!! Don't ask.
    tem <- Sys.getenv("OPEN_API_KEY")
    paste0(
      "Current API key: ",
      substr(txt, 1, 4),
      ".....",
      substr(txt, nchar(txt) - 4, nchar(txt)),
      " (",
      api_key_session()$key_source,
      ")"
    )
  })

  # only save key, if app is running locally.
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
    # if too long, do not send.
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
    # if no file is selected, do not send.
    if (is.null(available_datasets[[selected_dataset_name()]])) {
      showNotification(
        paste("No file found. Please select a dataset and try again."),
        duration = 10
      )
    }
 })


  #                             3.
  #____________________________________________________________________________
  # Send API Request, handle API errors
  #____________________________________________________________________________

  sample_temp <- reactive({
      temperature <- default_temperature #default
      if (!is.null(input$temperature)) { #user supplied temperature
         temperature <- input$temperature
      }
      return(temperature)
  })

  selected_model <- reactive({
      model <- language_models[default_model] #gpt-4
      if (!is.null(input$language_model)) { #user supplied model
         model <- input$language_model
      }
      # get the name of the model for display
      names(model) <- names(language_models)[language_models == model]
      return(model)
  })

  openAI_prompt <- reactive({
    req(submit_button())
    req(available_datasets[[selected_dataset_name()]])
    req(input_text())
    isolate({ # so that it does not do it twice with each submit
      prep_input(input_text(), selected_dataset_name(), current_data(), use_python(), logs$id, selected_model())
    })

  })

  relevancy_response <- reactiveVal(TRUE) #Initializing relevancy_response to be TRUE as a reactive varaible.
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
                max_proptortion_factor()
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
                max_proptortion_factor()
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

  output$openAI <- renderText({
    req(openAI_response()$cmd)
    res <- logs$raw
    res <- gsub("```", "", res)
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
      rmd = Rmd_chunk(),
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
      inputId = "selected_chunk",
      label = "AI generated code:",
      choices = choices,
      selected = logs$id
    )
  })

  # change code when past code is selected.
  observeEvent(input$selected_chunk, {
    # req(run_result())
    req(input$selected_chunk)
    id <- as.integer(input$selected_chunk)
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

 # Defining & initializing the reactiveValues object
  counter <- reactiveValues(
    costs_total = 0, # cummulative cost
    requests = 0, # cummulative requests
    tokens_current = 0,  # tokens for current query
    time = 0 # response time for current
  )


  #                            5.
  #____________________________________________________________________________
  # Run the code, shows plots, code, and errors
  #____________________________________________________________________________

  # define a reactive variable that holds an R environment
  # This is needed for the Rmd chunk.
  run_env <- reactiveVal(new.env())

  # a list stores all data objects before running the code.
  run_env_start <- reactiveVal(list())

  # stores the results after running the generated code.
  # return error indicator and message
  # Sometimes returns NULL, even when code run fine. Especially when
  # a base R plot is generated.

  # define a reactive variable. Reactive function not returning error
  run_result <- reactiveVal(list())

  # change value when a previous code chunk is selected.
  reverted <- reactiveVal(0)

  observeEvent(
    eventExpr = {
      submit_button()  # when submit is clicked
      reverted()           # or when a previous code chunk is selected
      logs$code
    }, {
    req(logs$code != "")
    req(!use_python())
    # req(relevancy_response())
    result <- NULL
    console_output <- NULL
    error_message <- NULL

    withProgress(message = "Running the code ...", {
      incProgress(0.4)

      run_env_start(as.list(run_env())) # keep a copy of the crime scene

      result <- tryCatch({
        eval_result <- eval(
          #parse(text = "log('error')"),
          parse(text = clean_cmd(logs$code, selected_dataset_name(), file.exists(on_server))),
          envir = run_env()
        )
        console_output <- capture.output(print(eval_result))
        eval_result  # without this the interactive plots does not work
      }, error = function(e) {
        list(error_message = e$message) # won't work if not inside a list!!!!
      })

      # update the error message, if any
      if(length(names(result)) != 0) {
        if(names(result)[1] == "error_message") {
          error_message <- result$error_message
        }
      }

      # Run with error
      if(!is.null(error_message)) {
        run_env(list2env(run_env_start())) # revert the environment
      }

      run_result(list(
        result = result,
        console_output = console_output,
        error_message = error_message
      ))
    })
  })


  # Error when run the generated code?
  code_error <- reactive({
    error_status <- FALSE
    req(submit_button() != 0) #Require the submit button to be pushed
    if(!use_python()) { # R
      return(!is.null(run_result()$error_message) && run_result()$error_message != "")
    } else { # Python
      return(python_to_html() == -1)
    }
  })

  output$error_message <- renderUI({
    req(code_error())
    req(logs$code)
    if(code_error()) {
      h4(paste("Error!", run_result()$error_message), style = "color:red")
    } else {
      return(NULL)
    }
  })

  output$console_output <- renderText({
    req(!code_error())
    paste(run_result()$console_output, collapse = "\n")
  })

  output$result_plot <- renderPlot({
    req(!code_error())
    req(logs$code)
    # req(relevancy_response())
    # Check if the result is not a ggplot or a known plot type
    if (inherits(run_result()$result, "ggplot") || is.null(run_result()$console_output)) {
      return(run_result()$result)
    } else {
      # If the result is not a ggplot (e.g., corrplot), re-evaluate the command_string,
      #under the parent environment of the run_env()
      tmp_env <- list2env(run_env_start())
      tryCatch({
        eval_result <- eval(
          parse(text = clean_cmd(logs$code, selected_dataset_name(), file.exists(on_server))),
          envir = tmp_env
        )
      })
    }
  })

  output$result_plotly <- plotly::renderPlotly({
    req(!code_error())
    req(!use_python())
    req(
      is_interactive_plot() ||   # natively interactive
      turned_on(input$make_ggplot_interactive)
    )

    g <- run_result()$result
    # still errors some times, when the returned list is not a plot
    if(is.character(g) || is.data.frame(g) || is.numeric(g)) {
      return(NULL)
    } else {
      return(g)
    }
  })

  output$result_CanvasXpress <- canvasXpress::renderCanvasXpress({
    req(!code_error())
    req(!use_python())

    g <- run_result()$result
    if (
      turned_on(input$make_cx_interactive) &&
      !is.character(g) &&
      !is.data.frame(g) &&
      !is.numeric(g)
    ) {
      g <- canvasXpress::canvasXpress(g)
    } else {
      g <- canvasXpress::canvasXpress(destroy = TRUE)
    }
    return(g)
  })

  # Remind user to uncheck.
  observe({

    req(input$make_cx_interactive && input$tabs == "Home")
    showNotification(
      ui = paste("Please uncheck the CanvasXpress
      box before proceeding to the next request."),
      id = "uncheck_canvasXpress",
      duration = NULL,
      type = "error"
    )
  })

  # Remove messages if the tab changes --------
  observe({
    req(!input$make_cx_interactive || input$tabs != "Home")
    removeNotification("uncheck_canvasXpress")
  })


  output$plot_ui <- renderUI({
    req(submit_button())
    req(!use_python())
    req(!code_error())
    req(logs$code)
    # req(relevancy_response())
    if (
      is_interactive_plot() ||   # natively interactive
      turned_on(input$make_ggplot_interactive) # converted
    ){
      plotly::plotlyOutput("result_plotly")
    } else if (
      turned_on(input$make_cx_interactive) # converted
    ) {
      canvasXpress::canvasXpressOutput("result_CanvasXpress")
    } else {
      plotOutput("result_plot")
    }
  })

  observe({
    # hide it by default
    shinyjs::hideElement(id = "make_ggplot_interactive")
    updateCheckboxInput(
      session = session,
      inputId = "make_ggplot_interactive",
      label = "Interactive via plotly",
      value = FALSE
    )

    req(!code_error())
    req(logs$code)
    txt <- paste(openAI_response()$cmd, collapse = " ")

    if (inherits(run_result()$result, "ggplot") && # if  ggplot2, and it is
      !is_interactive_plot() && #not already an interactive plot, show
       # if there are too many data points, don't do the interactive
      !(dim(current_data())[1] > max_data_points && grepl("geom_point|geom_jitter", txt))
    ) {
      shinyjs::showElement(id = "make_ggplot_interactive")
    }
  })

  observe({
    # hide it by default
    shinyjs::hideElement(id = "make_cx_interactive")
    updateCheckboxInput(
      session = session,
      inputId = "make_cx_interactive",
      label = "Interactive via CanvasXpress",
      value = FALSE
    )

    req(!code_error())
    req(logs$code)
    txt <- paste(openAI_response()$cmd, collapse = " ")

    if (inherits(run_result()$result, "ggplot") && # if  canvasXpress, and it is
      !is_interactive_plot() && #not already an interactive plot, show
       # if there are too many data points, don't do the interactive
      !(dim(current_data())[1] > max_data_points && grepl("geom_point|geom_jitter", txt))
    ) {
      shinyjs::showElement(id = "make_cx_interactive")
    }
  })


  is_interactive_plot <- reactive({
    # only true if the plot is interactive, natively.
    req(submit_button())
    req(logs$code)
    req(!code_error())
    if (inherits(run_result()$result, "plotly")) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })

  output$tips_interactive <- renderUI({
    req(submit_button())
    req(openAI_response()$cmd)
    if(is_interactive_plot() ||   # natively interactive
      turned_on(input$make_ggplot_interactive)
     ) {
      tagList(
        p("Mouse over to see values. Select a region to zoom.
        Click on the legends to deselect a group.
        Double click a category to hide all others.
        Use the menu on the top right for other functions."
        )
      )
    } else if (turned_on(input$make_cx_interactive)) {
      tagList(
        p("To reset, press ESC. Or mouse over the top,
        then click the reset button on the top left.
        Mouse over to see values. Select a region to zoom.
        Click on the legends to deselect a group.
        Double click a category to hide all others.
        Use the menu on the top right for other functions.
        Right click for more options."
        )
      )
    }
  })

  # had to use this. Otherwise, the checkbox returns to false
  # when the popup is closed and openned again.
  convert_to_factor <- reactive({
      convert <- TRUE #default
      if (!is.null(input$numeric_as_factor)) {
        convert <- input$numeric_as_factor
      }
      return(convert)
  })

  max_proptortion_factor <- reactive({
      max_proptortion <- unique_ratio #default
      if(!is.null(input$max_proptortion_factor)) {
        max_proptortion <- input$max_proptortion_factor
      }
      if(max_proptortion < 0.05) {
        max_proptortion <- 0.05
      }
      if(max_proptortion > 0.5) {
        max_proptortion <- 0.5
      }
      return(max_proptortion)
  })


   max_levels_factor <- reactive({
      max_levels_1 <- max_levels_factor_conversion #default
      if (!is.null(input$max_levels_factor)) {
        max_levels_1 <- input$max_levels_factor
      }
      if (max_levels_1 < 2) {
        max_levels_1 <- 2
      }
      if (max_levels_1 > 100) {
        max_levels_1 <- 100
      }
      return(max_levels_1)
  })

  # The current data & file name
  current_data <- reactiveVal(NULL)
  selected_file <- reactiveVal(NULL)

  observeEvent(available_datasets[[selected_dataset_name()]], {
    req(available_datasets[[selected_dataset_name()]])

    if(selected_dataset_name() == uploaded_data) {
      eval(parse(text = paste0("df <- user_data()$df")))
    } else if(selected_dataset_name() == no_data){
      df <- NULL # as.data.frame("No data selected or uploaded.")
    } else {
      # otherwise built-in data is unavailable when running from R package.
      library(tidyverse)
      data <- current_data()
      eval(parse(text = paste0("df <- data")))
    }

    if (convert_to_factor()) {
      df <- numeric_to_factor(
        df,
        max_levels_factor(),
        max_proptortion_factor()
      )
    }

    # if the first column looks like id?
    if(
      length(unique(df[, 1])) == nrow(df) &&  # all unique...what about duplicate ID's??
      is.character(df[, 1])  # first column is character
    ) {
       row.names(df) <- df[, 1]
       df <- df[, -1]
    }

    # sometimes no row is left after processing.
    if(is.null(df)) { # no_data
      current_data(NULL)
    } else if(nrow(df) == 0) {
      current_data(NULL)
    } else { # there are data in the dataframe

      current_data(df)
    }
    # add the data to the current environment
    run_env(rlang::env(run_env(), df = current_data()))
    run_env_start(as.list(run_env()))
  })

  output$data_table_DT <- DT::renderDataTable({
    req(current_data())
    DT::datatable(
      current_data(),
      options = list(
        lengthMenu = c(5, 20, 50, 100),
        pageLength = 10,
        dom = 'ftp',
        scrollX = "400px"
      ),
      rownames = FALSE
    )
  })

  output$data_table <- renderTable({
    req(current_data())

    current_data()[
      1:min(20, nrow(current_data())),
      ]
  })

  output$data_size <- renderText({
    req(!is.null(current_data()))
    paste(
      dim(current_data())[1], "rows X ",
      dim(current_data())[2], "columns"
    )
  })

  output$data_structure <- renderPrint({
    req(!is.null(current_data()))
    str(current_data())
  })

  output$data_summary <- renderText({
    req(!is.null(current_data()))
    paste(
      capture.output(
        summary(current_data())
      ),
      collapse = "\n"
    )
  })

  # plotting missing values
  output$missing_values <- plotly::renderPlotly({
    req(!is.null(current_data()))
    p <- missing_values_plot(current_data())
    if(!is.null(p)) {
      plotly::ggplotly(p)
    } else {
      return(NULL)
    }
  })

  # data frame summary
  output$dfSummary <- renderText({
    req(current_data())
    res <- capture.output(summarytools::dfSummary(current_data()))
    res <- paste(res, collapse = "\n")
    return(res)
  })

  observe({
    if(selected_dataset_name() != no_data && !is.null(current_data())) {
    shinyjs::show(id = "first_file")
    } else {
      shinyjs::hide(id = "first_file")
    }
  })

  # Add a download button for current_data()
  output$download_data <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(current_data(), file, row.names = FALSE)
    }
  )



  #                                 6.
  #____________________________________________________________________________
  #  Reports
  #____________________________________________________________________________

  observeEvent(submit_button(), {
    choices <- 1:length(logs$code_history)
    names(choices) <- paste0("Chunk #", choices)
    updateSelectInput(
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
    "\nDeveloped by [Steven Ge](https://twitter.com/StevenXGe) using API access via the
[openai](https://cran.rstudio.com/web/packages/openai/index.html)
    package  to
    [OpenAI's](https://cran.rstudio.com/web/packages/openai/index.html) \"",
    selected_model(),
    "\" model.",
    "\n\nRTutor Website: [https://RTutor.ai](https://RTutor.ai)",
    "\n"
  )

  # if the first chunk & data is uploaded,
  # insert script for reading data
  if (selected_dataset_name() == uploaded_data) {

    # Read file
    file_name <- input$user_file$name
    if(user_data()$file_type == "read_excel") {
      txt <- paste0(
        "# install.packages(readxl)\nlibrary(readxl)\ndf <- read_excel(\"",
        file_name,
        "\")"
      )

    }
    if (user_data()$file_type == "read.csv") {
      txt <- paste0(
        "df <- read.csv(\"",
        file_name,
        "\")"
      )
    }
    if (user_data()$file_type == "read.table") {
      txt <- paste0(
        "df <- read.table(\"",
        file_name,
        "\", sep = \"\t\", header = TRUE)"
      )
    }

    Rmd_script <- paste0(
      "\n### 0. Read File\n",
      "```{R, eval = FALSE}\n",
      txt,
      "\n```\n"
    )
  }

  Rmd_script <- paste0(
    Rmd_script,
    # Get the data from the params list for every chunk-----------
    # Do not change this without changing the output$Rmd_source function
    # this chunk is removed for local knitting.
    "\n```{R, echo = FALSE}\n",
    "df <- params$df\n",
    "```\n"
  )

  #------------------Add selected chunks
  if("All chunks" %in% input$selected_chunk_report) {
      ix <- 1:length(logs$code_history)
  } else if("All chunks without errors" %in% input$selected_chunk_report) {
    ix <- c()
    for (i in 1:length(logs$code_history)) {
      if(!logs$code_history[[i]]$error) {
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

    if(use_python()) {
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
        # remove pre-inserted commands
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
    if(!use_python()) {  # R code chunk
      # if error when running the code, do not run
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
      # if error when running the code, do not run
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
    if(nchar(cmd[1]) == 0) {
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
        inputId = "report",
        label = "HTML Session Report"
      ),
      tippy::tippy_this(
        "report",
        "Render a HTML report file for this session.",
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
    req(selected_dataset_name() != no_data)
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

      # RMarkdown file's Header
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
        "printcode:\n",
        "  label: \"Display Code\"\n",
        "  value: TRUE\n",
        "  input: checkbox\n",
        "---\n"
      )
      Rmd_script <- paste0(
        Rmd_script,
        # Get the data from the params list for every chunk-----------
        # Do not change this without changing the output$Rmd_source function
        # this chunk is removed for local knitting.
        "\n```{R, echo = FALSE}\n",
        "df <- params$df\n",
        "```\n"
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
      # if uploaded, use that data
      req(available_datasets[[selected_dataset_name()]])
      df <- current_data()
      if (selected_dataset_name() != no_data) {
        params <- list(
          df = df
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
            id = "report_error",
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
              outputId = "download_report",
              label = "Download"
            ),
            easyClose = TRUE
          ))
        }
      )
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

        # RMarkdown file's Header
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

        # if uploaded, use that data
        req(available_datasets[[selected_dataset_name()]])
        if (selected_dataset_name() != no_data) {
          params <- list(
            df = current_data()
          )
        }

        req(params)
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(
          input = tempReport, # markdown_location,
          output_file = file,
          params = params,
          envir = new.env(parent = globalenv())
        )
      })
    }
  )


#                                      7.
#______________________________________________________________________________
#
#  General UI, observers, etc.
#______________________________________________________________________________


  # limit max file size to 10MB, if it is running on server
  if (file.exists(on_server)) { #server
    options(shiny.maxRequestSize = 50 * 1024^2) # 50 MB
  } else { # local
    options(shiny.maxRequestSize = 10000 * 1024^2) # 10 GB
  }

  pdf(NULL) # otherwise, base R plots sometimes do not show.

  observeEvent(reset_button(), {
    # reset session
    session$reload()
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

  # Display RTutor Version
  output$RTutor_version <- renderUI({
    h4(paste("RTutor Version", release))
  })

  output$RTutor_version_main <- renderUI({
    tagList(
      h3(paste("RTutor.ai ", release))
    )
  })

  # 'About' tab FAQ's and answers
  output$faq_list <- renderUI({
    faq_items <- lapply(seq_len(nrow(faqs)), function(i) {
      tags$div(
        class = "faq-item",
        tags$h5(
          class = "faq-question",
          faqs$question[i]
        ),
        tags$p(
          class = "faq-answer",
          faqs$answer[i]
        )
      )
    })
    tagList(faq_items)
  })

  # Python
  output$python_markdown <- renderUI({
    req(openAI_response()$cmd)
    req(use_python())

    id <- as.integer(input$selected_chunk)
    rendered <- logs$code_history[[id]]$html_file
    req(rendered)

    if (rendered == -1) {
      p("Error!")
    } else {
      includeHTML(rendered)
    }
  })

  # file is rendered and stored in the html_file variable in logs$code_history
  python_to_html <- reactive({
    req(submit_button())
    req(logs$language == "Python")
    req(use_python())

    isolate({
      python_html(
        python_code = logs$code,
        select_data = available_datasets[[selected_dataset_name()]],
        current_data = current_data()
      )
    })
  })

}
