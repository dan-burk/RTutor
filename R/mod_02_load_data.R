#____________________________________________________________________________
#  Loading Data
#____________________________________________________________________________


mod_02_load_data_ui <- function(id) {

  ns <- NS(id)

  tagList(
    # CSS Styles
    tags$head(tags$style(HTML(paste0("
      #", ns("user_selected_dataset"), " {background-color: #F6FFF5;border-color: #90BD8C;color: #000;} 
      .control-label[for='", ns("user_selected_dataset"), "'] { font-size: 18px; font-weight: bold; }

      .control-label[for='", ns("user_file"), "'] { font-size: 18px; font-weight: bold; }

      hr {border-top: 1px solid #90BD8C;}
    ")))),

    # Display selected dataset
    conditionalPanel(
      condition = paste0("output['", ns("show_selected_dataset"), "'] === 'show'"), #paste0("input['", ns("submit_button"), "'] >= 1")
      fluidRow(
        column(
          width = 12,
          # textOutput(ns("selected_dataset"))
          uiOutput(ns("selected_dataset"))
        )
      ),
      hr(class = "custom-hr")
    ),
    conditionalPanel(
      condition = paste0("output['", ns("show_option1"), "'] === 'show'"), #paste0("input['", ns("submit_button"), "'] == 0 || input['", ns("user_selected_dataset"), "'] === 'Select a dataset:'")
      fluidRow(
          column(
            width = 6,
            selectInput(
              inputId = ns("user_selected_dataset"),
              label = HTML("<span style='font-size: 18px; font-weight: bold;'>1. Select Dataset</span>"),
              choices = available_datasets, #names(available_datasets)
              selected = "Select a dataset:",
              multiple = FALSE
            ) #Historically called demo_data_ui
          ),
          column(
            width = 6,
            uiOutput(ns("data_upload_ui"))#,
            # uiOutput("data_upload_ui_2")
          )
        ),
        hr(class = "custom-hr")
    )
  )
}



mod_02_load_data_serv <- function(id, chunk_selection,
  current_data,
  original_data,
  run_env,
  run_env_start,
  submit_button,
  convert_to_factor,
  max_proportion_factor,
  max_levels_factor
  # modal_closed,
  # run_env,
  # run_env_start,
  # current_data,
  # current_data_2,
  # original_data,
  # logs
  ) { #, show_pop_up, modal_closed

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## limit max file size to 10MB, if it is running on server
    # if (file.exists(on_server)) { # server
    #   options(shiny.maxRequestSize = 50 * 1024^2) # 50 MB
    # } else { # local
    #   options(shiny.maxRequestSize = 10000 * 1024^2) # 10 GB
    # }


    output$data_upload_ui <- renderUI({

      # Hide this input box after the first run.
      req(submit_button() == 0 || !is.null(input$user_selected_dataset)) #RHS is for when accidental hit submit
      # req(is.null(input$user_file))
      fileInput(
        inputId = ns("user_file"),
        label = "Upload",
        accept = c(
          "text/csv",
          "text/comma-separated-values",
          "text/tab-separated-values",
          "text/plain",
          ".csv",
          ".tsv",
          ".txt",
          ".xls",
          ".xlsx"
        )
      )
    })

    user_data <- reactive({
      req(input$user_file)
      in_file <- input$user_file
      in_file <- in_file$datapath
      req(!is.null(in_file))

      isolate({
        df <- data.frame()
        file_type <- "read_excel"
        # Excel file ---------------
        if (grepl("xls$|xlsx$", in_file, ignore.case = TRUE)) {
          try(
            df <- readxl::read_excel(in_file)
          )
          df <- as.data.frame(df)
        } else {
          #CSV --------------------
          try(
            df <- read.csv(in_file)
          )
          file_type <- "read.csv"
          # Tab-delimented file ----------
          if (ncol(df) <= 1) { # unable to parse with comma
            try(
              df <- read.table(
                in_file,
                sep = "\t",
                header = TRUE
              )
            )
            file_type <- "read.table"
          }
        }

        if (ncol(df) == 0) { # no data read in. Empty
          return(NULL)
        } else {
          # clean column names
          df <- df %>% janitor::clean_names()
          return(
            list(
              df = df,
              file_type = file_type
            )
          )
        }
      })
    })

    observeEvent(input$user_file, {
      updateSelectInput(
        session,
        inputId = "user_selected_dataset", #Used to be select_data, do NOT NOT NOT put ns() around this ID. It screws up everything!
        choices = available_datasets, #names(available_datasets)
        selected = user_upload
      )
    }, ignoreInit = TRUE, once = TRUE)



    # output$file_uploaded <- reactive({
    #   return(!is.null(input$user_file))
    # })
    # outputOptions(output, 'file_uploaded', suspendWhenHidden = FALSE)


    observeEvent(input$user_selected_dataset, {
      # req(input$user_selected_dataset) #Require selected dataset NOT be NULL, pointless condition
      
      if(input$user_selected_dataset == user_upload) {
        eval(parse(text = paste0("df <- user_data()$df")))
        # orig_data = df
      } else if(input$user_selected_dataset %in% c(no_data, "Select a dataset:")){
        df <- NULL #as.data.frame("No data selected or uploaded.")
      }else {
        # otherwise built-in data is unavailable when running from R package.
        df <- get(input$user_selected_dataset) #available_datasets[[input$user_selected_dataset]]
      }

      #  else if(input$select_data == rna_seq){
      #   df <- rna_seq_data()
      # } 

      if (convert_to_factor()) { #Not impleented Yet
        df <- numeric_to_factor(
          df,
          max_levels_factor(),
          max_proportion_factor()
        )
      }

      # if the first column looks like id? Tbh rudamentary logic.
      if(!is.null(df)){
        if(
          length(unique(df[, 1])) == nrow(df) &&  # all unique
          is.character(df[, 1])  # first column is character
        ) {
          row.names(df) <- df[, 1]
          df <- df[, -1]
        }
      }


      # sometimes no row is left after processing.
      if(is.null(df)) { # no_data
        current_data(NULL)
      } else if(nrow(df) == 0) {
        current_data(NULL)
      } else { # there are data in the dataframe

        current_data(df)
        original_data(df)
      }

      isolate({
        existing_vars <- as.list(run_env())
        run_env(list2env(existing_vars))
        run_env_start(as.list(run_env()))
      })

    })


    # Display selected dataset
    # output$selected_dataset <- renderText({
    #   req(input$submit_button)
    #   req(available_datasets[[input$user_selected_dataset]])

    #   txt <- paste0(input$user_selected_dataset, ".  Reset to switch.")
    #   return(txt)
    # })

    output$selected_dataset <- renderUI({
      req(submit_button())
      # when submit is clicked, but no data is uploaded.

      if (input$user_selected_dataset == user_upload) {
        if (is.null(input$user_file)) {
          txt <- "No file uploaded! Please Reset and upload your data first."
        } else {
          txt <- "Dataset: Uploaded."
        }
      } else if (input$user_selected_dataset == "Select a dataset:") {
        # txt <- "Data Set Not Selected! Please Reset and Select a Dataset."
        txt <- NULL
      } else {
        txt <- paste0("Selected Dataset: ", input$user_selected_dataset)
      }

      return(HTML(paste0("<span style='font-size: 18px;font-weight: bold;
                        white-space: nowrap;'>", txt, "</span>")))
    })

    #Creating a condition based on input from mod_16
    output$show_selected_dataset <- renderText({
      if(submit_button() >= 1){
        return("show")
      }else{
        return("hide")
      }
    })
    # Ensures this runs in background even when not called in UI
    outputOptions(output, "show_selected_dataset", suspendWhenHidden = FALSE)

    output$show_option1 <- renderText({
      # Check both conditions: submit_button() from mod_16 and user_selected_dataset from this module
      if(submit_button() == 0 || input$user_selected_dataset == "Select a dataset:"){
        return("show")
      }else{
        return("hide")
      }
    })
    # Ensures this runs in background even when not called in UI
    outputOptions(output, "show_option1", suspendWhenHidden = FALSE)


    # Return all reactive values so they can be used outside the module
    return(
      list(
        selected_dataset_name = reactive(input$user_selected_dataset),
        user_file = reactive(input$user_file)
      )
    )

  })
}