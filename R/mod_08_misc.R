

#____________________________________________________________________________
#  Miscellaneous
#____________________________________________________________________________


mod_08_misc_ui <- function(id) {

    ns <- NS(id)

    tagList(
      hr(),
      p("Developed by RTutor LLC"),
      p("RTutor uses ",
        a(
          "OpenAI's",
          href = "https://openai.com/",
          target = "_blank"
        ),
        " powerful large language models",
        " to translate natural language into R code, which is then excuted.",
        "You can request your analysis, just like asking a real person.",
        "Your results can be downloaded as an HTML report or RMarkdown file in minutes!"
      ),
      p("NO WARRANTY! Some of the scripts run but may yield incorrect result. 
        Please use the auto-generated code as a starting 
        point for further refinement and validation."
      ),
      hr()
    )
}


mod_08_misc_serv <- function(id, reset_button, submit_button, logs, use_python,
                             current_data, selected_dataset_name) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # limit max file size to 10MB, if it is running on server
    if (file.exists(on_server)) { # server
      options(shiny.maxRequestSize = 50 * 1024^2) # 50 MB
    } else { # local
      options(shiny.maxRequestSize = 10000 * 1024^2) # 10 GB
    }

    observeEvent(reset_button(), {
      # reset session
      session$reload()
    })


    # File is rendered and stored in the html_file variable in logs$code_history
    python_to_html <- reactive({
      req(submit_button())
      req(logs$language == "Python")
      req(use_python())

      isolate({
        python_html(
          python_code = logs$code,
          select_data = input$user_selected_dataset, #available_datasets[[selected_dataset_name()]]
          current_data = current_data()
        )
      })
    })


    # Return reactive values so they can be used outside the module
    return(
      list(
        python_to_html = python_to_html
      )
    )
  })
}