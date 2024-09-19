###################################################
# RTutor.AI, a Shiny app for chating with your data
# Author: Xijin Ge    gexijin@gmail.com
# Dec. 6-12, 2022.
# No warranty and not for commercial use.
###################################################


#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),

    ### Style Module ###
    mod_01_style_ui("mod_01_style_ui.R"),

    navbarPage(
      HTML('<span style="color: black;">HMCL</span>'),
      id = "tabs",

      tabPanel(
        title = "Home",

        sidebarLayout(
          ### Sidebar ###
          sidebarPanel(
            mod_02_load_data_ui("load_data")
          ),

          ### Main Panel ###
          mainPanel(
            mod_03_main_panel_ui("main_panel")
          )
        )
      ), #tabPanel

      #############################
      # 'About' Tab Panel
      tabPanel(
        title = "About",
        value = "About",
        hr(),
        p("Developed by RTutor LLC for HeroMotor Corp."),
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
        hr(),

        # FAQ drop down component
        fluidRow(
          column(
            width = 8,

            h4(style = "font-weight: bold;", "Frequently Asked Questions"),
            uiOutput("faq_list"),
            tags$style(HTML("
              .faq-answer {
                display: none;
                padding-left: 10px;
              }
              .faq-question {
                cursor: pointer;
                padding: 5px;
                border: 1px solid #ccc;
                background-color: #f1f1f1;
              }
            ")),
            tags$script(HTML('
              $(document).on("click", ".faq-question", function() {
                var answer = $(this).next(".faq-answer");
                if (answer.is(":visible")) {
                  answer.hide();
                } else {
                  answer.show();
                }
              });
            '))
          )
        ),
        hr()
      )
    ),

    tags$head(includeHTML(app_sys("app", "www", "ga.html")))
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www", app_sys("app/www")
  )

  tags$head(
    favicon(
      ico = "icon",
      rel = "shortcut icon",
      resources_path = "www",
      ext = "png"
    ),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "RTutor 0.98"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
