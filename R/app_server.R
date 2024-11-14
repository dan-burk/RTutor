###################################################
# RTutor.AI, a Siny app for chating with your data
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

  pdf(NULL) # otherwise, base R plots sometimes do not show

  ### Initialize reactives ###

  # the current data
  current_data <- reactiveVal(NULL)
  current_data_2 <- reactiveVal(NULL)
  original_data <- reactiveVal(NULL)

  # define a reactive variable that holds an R environment
  # This is needed for the Rmd chunk
  run_env <- reactiveVal(new.env())

  # a list stores all data objects before running the code
  run_env_start <- reactiveVal(list())
  # define a reactive variable. Reactive function not returning error
  run_result <- reactiveVal(list())


  #                             1. Module 02
  #____________________________________________________________________________
  #  Loading data
  #____________________________________________________________________________

  # 'Load Data' module
  mod_02 <- mod_02_load_data_serv(
    id = "load_data",
    chunk_selection = chunk_selection,
    current_data = current_data,
    original_data = original_data,
    run_env = run_env,
    run_env_start = run_env_start,
    submit_button = submit_button,
    convert_to_factor = convert_to_factor,
    max_proportion_factor = max_proportion_factor,
    max_levels_factor = max_levels_factor
    #Arguments needed for mod_15_data_types_serv() called in mod_02
    # modal_closed = modal_closed,
    # run_env = run_env,
    # run_env_start = run_env_start,
    # current_data = current_data,
    # current_data_2 = current_data_2,
    # original_data = original_data,
    # logs = logs
  )

  # (remove extra reactive wrap!!!)
  # Rename the reactive values for easier use
  selected_dataset_name <- reactive({ mod_02$selected_dataset_name()  })
  # submit_button <- reactive({ mod_02$submit_button()  })
  # reset_button <- reactive({  mod_02$reset_button() })
  use_python <- reactive({  FALSE })
  user_file <- mod_02$user_file


  #                             2. Module 03
  #____________________________________________________________________________
  #   Send Request
  #____________________________________________________________________________

  # 'Send Request' module
  mod_03 <- mod_03_send_request_serv(
    id = "send_request",
    chunk_selection = chunk_selection,
    user_file = user_file,
    selected_dataset_name = selected_dataset_name
  )

  input_text <- reactive({  mod_03$input_text() })
  submit_button <- reactive({ mod_03$submit_button()  })
  reset_button <- reactive({  mod_03$reset_button() })


  #                             3. Module 04
  #____________________________________________________________________________
  #   Main Panel
  #____________________________________________________________________________

  # 'Main Panel' module
  tabs <- reactive({ input$tabs })

  chunk_selection <- reactiveValues(
    chunk_choices = NULL,
    selected_chunk = NULL,
    past_prompt = NULL
  )

  mod_04 <- mod_04_main_panel_serv(
    id = "main_panel",
    llm_response = llm_response,
    logs = logs,
    code_error = code_error,
    run_result = run_result,
    run_env_start = run_env_start,
    submit_button = submit_button,
    use_python = use_python,
    tabs = tabs,
    current_data = current_data,
    selected_dataset_name = selected_dataset_name,
    chunk_selection = chunk_selection
  )



  #                             4. Module 5
  #____________________________________________________________________________
  #   LLMs
  #____________________________________________________________________________

  mod_05 <- mod_05_llms_serv(
    id = "llms",
    submit_button = submit_button,
    input_text = input_text,
    selected_dataset_name = selected_dataset_name,
    api_key = api_key,
    sample_temp = sample_temp,
    selected_model = selected_model,
    logs = logs,
    counter = counter,
    api_error_modal = api_error_modal,
    code_error = code_error,
    current_data = current_data,
    run_env = run_env,
    run_env_start = run_env_start,
    run_result = run_result,
    use_python = use_python,
    send_head = send_head
  )

  # Rename the reactive values for easier use
  llm_prompt <- reactive({  mod_05$llm_prompt() })
  llm_response <- reactive({  mod_05$llm_response() })


  #                             5. Module 06
  #____________________________________________________________________________
  #  Error handling, record keeping/chunk history
  #____________________________________________________________________________


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

  # Intitialize, change value when a previous code chunk is selected
  reverted <- reactiveVal(0)

  # "Errors & History" module
  mod_06 <- mod_06_error_hist_serv(
    id = "errors_and_history",
    submit_button = submit_button,
    llm_response = llm_response,
    logs = logs,
    counter = counter,
    reverted = reverted,
    use_python = use_python,
    run_result = run_result,
    python_to_html = python_to_html,
    input_text = input_text,
    llm_prompt = llm_prompt,
    run_env = run_env,
    run_env_start = run_env_start,
    chunk_selection = chunk_selection,
    Rmd_chunk = Rmd_chunk
  )

  # Rename the reactive values for easier use
  api_error_modal <- reactive({  mod_06$api_error_modal() })
  code_error <- reactive({  mod_06$code_error() })



  #                            6. Module 07
  #____________________________________________________________________________
  # Run the code, data prep, show code
  #____________________________________________________________________________


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
    current_data = current_data
  )

  #                             8. Module 09
  #____________________________________________________________________________
  #  Report Tab
  #____________________________________________________________________________

  # 'Report' module
  mod_09 <- mod_09_report_serv(
    id = "report",
    submit_button = submit_button,
    logs = logs,
    selected_model = selected_model,
    llm_response = llm_response,
    input_text = input_text,
    use_python = use_python,
    counter = counter,
    sample_temp = sample_temp,
    code_error = code_error,
    python_to_html = python_to_html,
    current_data = current_data
  )

  # Rename the reactive values for easier use
  Rmd_chunk <- reactive({  mod_09$Rmd_chunk() })



  #                             9. Module 10
  #____________________________________________________________________________
  #  Exploratory Data Analysis Tab
  #____________________________________________________________________________

  # 'EDA' module
  mod_10 <- mod_10_eda_serv(
    id = "eda",
    selected_dataset_name = selected_dataset_name,
    use_python = use_python,
    current_data = current_data,
    logs = logs
  )



  #                             9. Module 11
  #____________________________________________________________________________
  #  Settings Tab
  #____________________________________________________________________________

  # 'Settings' module
  mod_11 <- mod_11_settings_serv(
    id = "sett",
    submit_button = submit_button,
    logs = logs,
    current_data = current_data,
    llm_prompt = llm_prompt,
    code_error = code_error
  )

  # Rename the reactive values for easier use
  api_key <- mod_11$api_key
  sample_temp <- mod_11$sample_temp
  selected_model <- mod_11$selected_model
  use_python <- mod_11$use_python
  convert_to_factor <- mod_11$convert_to_factor
  max_proportion_factor <- mod_11$max_proportion_factor
  max_levels_factor <- mod_11$max_levels_factor
  send_head <- mod_11$send_head

  mod_12 <- mod_12_about_serv(
    id = 'about'
  )

  mod_13 <- mod_13_faq_serv(
    id = 'faq'
  )

  #                             10. Module 15??
  #____________________________________________________________________________
  #  Data Types Modal
  #____________________________________________________________________________

  modal_closed <- reactiveVal(FALSE)

  mod_15 <- mod_15_data_types_serv(
    id = "data_edit_modal",
    modal_closed = modal_closed,
    run_env = run_env,
    run_env_start = run_env_start,
    current_data = current_data,
    current_data_2 = current_data_2,
    original_data = original_data,
    logs = logs,
    user_file
  )

  modal_closed <- mod_15$modal_closed
  show_pop_up <- mod_15$show_pop_up



  # File is rendered and stored in the html_file variable in logs$code_history
  python_to_html <- reactive({
    req(submit_button())
    req(logs$language == "Python")
    req(use_python())

    isolate({
      python_html(
        python_code = logs$code,
        select_data = selected_dataset_name(),
        current_data = current_data()
      )
    })
  })

}
