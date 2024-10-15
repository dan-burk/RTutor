#____________________________________________________________________________
#  Loading Data
#____________________________________________________________________________


mod_02_load_data_ui <- function(id) {

  ns <- NS(id)

  tagList(
    # CSS Styles
    tags$head(tags$style(HTML(paste0("
      #", ns("user_selected_dataset"), " {background-color: #F6FFF5;border-color: #90BD8C;color: #000;} 
      #", ns("user_selected_dataset"), "-label { font-weight: normal; }

      hr {border-top: 1px solid #90BD8C;}

      textarea {width: 100%;background-color: #F6FFF5;border-color: #90BD8C;}

      #", ns("submit_button"), " {font-size: 16px;color: blue !important;background-color: #F6FFF5;border-color: #90BD8C;}

      #", ns("reset_button"), " {font-size: 16px;color: red;background-color: #F6FFF5;border-color: #90BD8C;}
    ")))),

    # Select a dataset
    conditionalPanel(
      condition = paste0("input['", ns("submit_button"), "'] == 0 || input['", ns("user_selected_dataset"), "'] === 'Select a dataset:'"),
      selectInput(
        ns('user_selected_dataset'),
        label = NULL,
        choices = names(available_datasets),
        multiple = FALSE,
        selectize = FALSE
      )
    ),
    # Display selected dataset
    conditionalPanel(
      condition = paste0("input['", ns("submit_button"), "'] >= 1"),
      fluidRow(
        column(
          width = 12,
          textOutput(ns("selected_dataset"))
        )
      )
    ),
    hr(),

    # User Input Text Box
    tags$textarea(
      id = ns("input_text"),
      placeholder = "Hi! I am your AI assistant. Select a dataset first then ask questions. See examples below.",
      rows = 8
    ),

    # Example Prompts
    uiOutput(ns("prompt_ui")),
    hr(),

    fluidRow(
      column(
        width = 12,
        div(
          style = "display: flex; justify-content: space-between;",
          div(
            # Submit Button
            actionButton(ns("submit_button"), strong("Submit")),

            tippy::tippy_this(
              "submit_button",
              "ChatGPT can return different results for the same request.",
              theme = "light-border"
            )
          ),
          div(
            # Reset Button
            actionButton(ns("reset_button"), strong("Reset")),

            tippy::tippy_this(
              "reset_button",
              "Reset before asking a new question. Clears data objects, chat history, & code chunks.",
              theme = "light-border"
            )
          )
        )
      )#,
      # # API keys and Python options
      # conditionalPanel(
      #   condition = "0",
      #   column(
      #     width = 4,
      #     checkboxInput(ns("use_python"), "Python", value = FALSE)
      #   )
      # )
    ),

    fluidRow(
      column(
        width = 12,
        hr()
      )
    )
  )
}



mod_02_load_data_serv <- function(id, chunk_selection) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Display selected dataset
    output$selected_dataset <- renderText({
        req(input$submit_button)
        req(!is.null(available_datasets[[input$user_selected_dataset]]))

        txt <- paste0(input$user_selected_dataset, ".  Reset to switch.")
        return(txt)
    })


    # Load previous prompts based on selected chunk
    observeEvent(chunk_selection$selected_chunk, {
      req(chunk_selection$past_prompt)

      updateTextAreaInput(
        session,
        inputId = "input_text",
        value = chunk_selection$past_prompt
      )
    })

    # Load demo prompts based on selected data
    observeEvent(input$demo_prompt, {
      req(available_datasets[[input$user_selected_dataset]])

      updateTextAreaInput(
        session,
        inputId = "input_text",
        value = input$demo_prompt
      )
    })

    # Display demo prompts (example requests)
    output$prompt_ui <- renderUI({
      req(input$user_selected_dataset)

      choices <- switch(input$user_selected_dataset,
        "No Data" = demo$requests[demo$data == "No Data"],
        "Iris" = demo$requests[demo$data == "Iris"],
        "MTCars" = demo$requests[demo$data == "MTCars"],
        "Air Quality" = demo$requests[demo$data == "Air Quality"],
        "Diamonds" = demo$requests[demo$data == "Diamonds"],
        "CO2" = demo$requests[demo$data == "CO2"],
        "Tooth Growth" = demo$requests[demo$data == "Tooth Growth"],
        "Pressure" = demo$requests[demo$data == "Pressure"],
        "Chick Weights" = demo$requests[demo$data == "Chick Weights"],
        demo$requests[demo$data == "Select a dataset:"]
      )

      names(choices) <- demo$name[match(choices, demo$requests)]

      # # subset based on R or Python
      # if (input$use_python) {
      #   choices <- choices[demo$Python == 1]
      # } else {
      #   choices <- choices[demo$R == 1]
      # }

      tagList(
        # CSS Styles
        tags$head(tags$style(HTML(paste0("
          .padding {padding-top: 10px;padding-left: 10px;padding-bottom: 10px;}

          #", ns("demo_prompt"), "+div .selectize-input {background-color: #F6FFF5 !important;border-color: #90BD8C !important;color: #000 !important;}
          #", ns("demo_prompt"), "+div .selectize-dropdown {background-color: #F6FFF5 !important;border-color: #90BD8C !important;color: #000 !important;}            
        ")))),

        fluidRow(
          column(
            width = 3,
            div("Examples:", class = "padding")
          ),
          column(
            width = 9,
            align = "left",
            selectInput(
              inputId = ns("demo_prompt"),
              choices = choices,
              label = NULL
            )
          )
        )
      )
    })


    # Return all reactive values so they can be used outside the module
    return(
      list(
        input_text = reactive(input$input_text),
        selected_dataset_name = reactive(input$user_selected_dataset),
        submit_button = reactive(input$submit_button),
        reset_button = reactive(input$reset_button)#,
        # use_python = reactive(input$use_python)
      )
    )

  })
}