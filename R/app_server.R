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
  mod_02 <- mod_02_load_data_serv(
    id = "load_data",
    chunk_selection = chunk_selection
  )

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

  chunk_selection <- reactiveValues(
    chunk_choices = NULL,
    selected_chunk = NULL,
    past_prompt = NULL
  )

  mod_03 <- mod_03_main_panel_serv(
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
  #   LLMs
  #____________________________________________________________________________

  mod_05 <- mod_05_llms_serv(
    id = "llms",
    submit_button = submit_button,
    input_text = input_text,
    selected_dataset_name = selected_dataset_name,
    api_key_session = api_key_session,
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
    convert_to_factor = convert_to_factor,
    max_proportion_factor = max_proportion_factor,
    max_levels_factor = max_levels_factor
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


  ### Initialize reactives ###

  # the current data
  current_data <- reactiveVal(NULL)

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
    current_data = current_data
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
    llm_prompt = llm_prompt,
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


}
