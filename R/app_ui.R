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
    
    mod_01_style_ui("mod_01_style_ui.R"),  # Include the style module

    navbarPage(
      HTML('<span style="color: black;">HMCL</span>'),
      id = "tabs",
      tabPanel(
        title = "Home",
        # move notifications and progress bar to the center of screen
        tags$head(
          tags$style(
            HTML(".shiny-notification {
                  width: 300px;
                  position:fixed;
                  top: calc(90%);
                  left: calc(10%);
                  }
                  "
                )
            )
        ),
        # Embed the CSS directly in the UI
        tags$style("
          .modal-dialog {
            position: absolute;
            bottom: 0;
          }
        "),

        ##########################################################
        ####### Sidebar
        ##########################################################
        sidebarLayout(
          sidebarPanel(
            mod_02_load_data_ui("load_data")
          ),

      ###############################################################################
      # Main
      ###############################################################################
          mainPanel(
            shinyjs::useShinyjs(),

            conditionalPanel(
              condition = "input['load_data-submit_button'] == 0",
              fluidRow(
                column(
                  width = 9,
                  h3(style = "font-weight: bold;", "Hero MotoCorp Data Portal (v0.01)"),
                  h4("Based on the RTutor platform. Work in progress in proof of concept stage. Feedbacks welcome."),
                  br(),
                  br(),
                  h4("Be aware of the limitations of the generative AI."),
                  br(),
                  h4(
                    "Start by watching a short ",
                    a(
                      "video!",
                      href = "https://youtu.be/a-bZW26nK9k",
                      target = "_blank"
                    )
                  ),
                  align = "left"
                ),
                column(
                  width = 3,
                  img(
                    src = "www/logo.png",
                    width = "155",
                    height = "77"
                  ),
                  align = "left"
                )
              ),

            ),

            conditionalPanel(
              condition = "input['load_data-submit_button'] != 0",
              fluidRow(
                column(
                  width = 4,
                  selectInput(
                    inputId = "selected_chunk",
                    label = "AI generated code:",
                    selected = NULL,
                    choices = NULL
                  ),
                  tippy::tippy_this(
                    "selected_chunk",
                    "You can go back to any previous code chunk and continue from there. The data will also be reverted to that point.",
                    theme = "light-border"
                  )
                ),
                column(
                  width = 8,
                  checkboxInput(
                    inputId = "show_code",
                    label = "Show code",
                    value = FALSE
                  ),
                  align = "right"
                )
              ),

              # show code based on the checkbox
              conditionalPanel(
                condition = "input.show_code == 1",
                verbatimTextOutput("openAI")
              ),

              conditionalPanel(
                condition = "true",  # "input['load_data-use_python'] == 0",

                uiOutput("error_message"),

                # shows error message in local machine, but not on the server
                verbatimTextOutput("console_output"),
                uiOutput("plot_ui"),
                fluidRow(
                  column(
                    width = 5,
                    checkboxInput(
                      inputId = "make_ggplot_interactive",
                      label = NULL,
                      value = FALSE
                    ),
                    align = "right"
                  ),
                  column(
                    width = 5,
                    checkboxInput(
                      inputId = "make_cx_interactive",
                      label = NULL,
                      value = FALSE
                    ),
                    align = "left"
                  )
                ),
                br(),
                uiOutput("tips_interactive"),
              ),
              conditionalPanel(
                condition = "input['load_data-use_python'] == 1",
                uiOutput("python_markdown")
              )
            )
          ) #mainPanel
        ) #sideBarpanel
      ), #tabPanel

      #############################
      # 'Data' Tab Panel
      tabPanel(
        title = div(id = "data_tab", "Data"),
        value = "Data",
        tippy::tippy_this(
          "data_tab",
          "Dataset Preview",
          theme = "light-border"
        ),
        shinyjs::hidden(
          div(
            id = "first_file",
            hr(),
            h4("Dataset:  df"),
            textOutput("data_size"),
            DT::dataTableOutput("data_table_DT")
          )
        )
      ),

      #############################
      # 'Report' Tab Panel
      tabPanel(
        title = div(id = "report_tab", "Report"),
        value = "Report",
        tippy::tippy_this(
          "report_tab",
          "Download a Results Report",
          theme = "light-border"
        ),
        br(),
        selectInput(
          inputId = "selected_chunk_report",
          label = "Code chunks to include:",
          selected = NULL,
          choices = NULL,
          multiple = TRUE
        ),
        fluidRow(
          column(
            width = 6,
            uiOutput("html_report")
          ),
          column(
            width = 6,
            downloadButton(
              outputId = "Rmd_source",
              label = "RMarkdown"
            ),
            tippy::tippy_this(
              "Rmd_source",
              "Download a R Markdown source file.",
              theme = "light-border"
            )
          )
        ),
        br()
      ),

      #############################
      # 'EDA' Tab Panel
      tabPanel(
        title = div(id = "eda_tab", "EDA"),
        value = "EDA",
        tippy::tippy_this(
          "eda_tab",
          "Exploratory Data Analysis",
          theme = "light-border"
        ),
        tabsetPanel(
          tabPanel(
            title = "Basic",
            h4("Data structure: df"),
            verbatimTextOutput("data_structure"),
            hr(),
            h4("Data summary: df"),
            verbatimTextOutput("data_summary"),
            plotly::plotlyOutput("missing_values", width = "60%")
          ),
          tabPanel(
            title = "Summary",
            verbatimTextOutput("dfSummary"),
            h4(
              "Generated by the ",
              a(
                "summarytools",
                href="https://cran.r-project.org/web/packages/summarytools/vignettes/introduction.html",
                target = "_blank"
              ),
              "package using the command:summarytools::dfSummary(df)."
            )
          )
        )
      ),

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
