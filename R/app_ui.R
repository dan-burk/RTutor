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

    ## Add color to UI
    tags$head(
      tags$style(HTML("
        /* navbar */
        .navbar {
          background-color: #C1E2BE;border-color: #90BD8C; color: #181818;font-weight: bold;
        }

        /* tabs */
        .navbar-default .navbar-nav > li > a {
          background-color: #C1E2BE;border-color: #9AC596;color: #181818;
        }

        /* active tab */
        .navbar-default .navbar-nav > .active > a, 
        .navbar-default .navbar-nav > .active > a:focus, 
        .navbar-default .navbar-nav > .active > a:hover {
          background-color: #A0BB9E;color: #181818;font-weight: bold;
        }

        /* sidebar panel */
        .well {
          background-color: #C1E2BE;border-color: #90BD8C;
      "))
    ),

    navbarPage(
      HTML('<span style="color: black;">RTutor</span>'),
       #  windowTitle = "RTutor",
       # theme = bslib::bs_theme(bootswatch = "darkly"),
      id = "tabs",
      tabPanel(
        title = "Home",
        div(
          id = "load_message",
          h2("Chat with your data via AI ..."),
        ),
        #uiOutput("use_heyshiny"), # remove it
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
        # tags$li(
        #   class = "navbar-text",
        #   tags$img(src = "www/logo.png", height = "30px", style = "margin-top: 5px;")
        # ),

  ##########################################################
  ####### Sidebar
  ##########################################################
        sidebarLayout(
          sidebarPanel(
            #uiOutput("timer_ui"),

            # Select a Dataset
            tags$head(tags$style(
              "#user_selected_dataset{background-color: #F6FFF5;border-color: #90BD8C;color: #000;}"
            )),
            tags$head(tags$style(
              "#user_selected_dataset-label { font-weight: normal; }"
            )),
            conditionalPanel(
              condition = "input.submit_button == 0",
              selectInput(inputId = 'user_selected_dataset',
                label = HTML('<span style="color: black;">Select a Dataset for Analysis</span>'),
                choices = names(available_datasets),
                multiple=FALSE,
                selectize=FALSE,
            )
            ),
            # Show selected dataset
            conditionalPanel(
              condition = "input.submit_button >= 1",
              fluidRow(
                column(
                  width = 12,
                  textOutput("selected_dataset")
                )
              )
            ),
            #   # Reset Button
            #   column(
            #     width = 6
            #   )
            # ),

            conditionalPanel(
              condition = "0",
              fluidRow(
                column(
                  width = 12,
                  uiOutput("demo_data_ui")
                ),
                column(
                  width = 6,
                  uiOutput("data_upload_ui")
                  ,uiOutput("data_upload_ui_2")
                )
              )
            ),

            # Horizontal Line
            tags$style(HTML("hr{border-top: 1px solid #90BD8C;}")),
            hr(),
            # User Input Text Box
            tags$style(HTML("
              textarea {
                width: 100%;
                background-color: #F6FFF5;
                border-color: #90BD8C;
              }
            ")),
            tags$textarea(
              id = "input_text",
              placeholder = NULL,
              rows = 8, ""
            ),
            # Example Prompts
            uiOutput("prompt_ui"),

            # Horizontal Line
            tags$style(HTML("hr{border-top: 1px solid #90BD8C;}")),
                hr(),

            fluidRow(
              column(
                width = 12,
                div(
                  style = "display: flex; justify-content: space-between;",
                  div(
                    # Submit Button
                    actionButton("submit_button", strong("Submit")),
                    tags$head(tags$style(
                      "#submit_button{font-size: 16px;color: blue;background-color: #F6FFF5;border-color: #90BD8C;}"
                    )),
                    tippy::tippy_this(
                      "submit_button",
                      "ChatGPT can return different results for the same request.",
                      theme = "light-border"
                    )
                  ),
                  div(
                    # Reset Button
                    actionButton(inputId = "reset_button", label = strong("Reset")),
                    tags$head(tags$style(
                      "#reset_button{font-size: 16px;color: red;background-color: #F6FFF5;border-color: #90BD8C;}"
                    )),
                    tippy::tippy_this(
                      "reset_button",
                      "Reset before asking a new question. Clears data objects, chat history, & code chunks.",
                      theme = "light-border"
                    )
                  )
                )
              ),
              # API keys and Python options
              conditionalPanel(
                condition = "0",
                column(
                  width = 4,
                  actionButton("api_button", "Settings")
                ),
                column(
                  width = 4,
                  checkboxInput("use_python", "Python", value = FALSE)
                )
              )
            ),

            fluidRow(
              column(12,
                # Horizontal Line
                tags$style(HTML("hr{border-top: 1px solid #90BD8C;}")),
                hr()
              )
            ),

            # # Show available datasets
            # tags$div(
            #   style = "border: 1px solid #ccc; padding: 0px 10px 4px 10px;",
            #   tags$h5(style = "font-weight: bold;", "Available Datasets"),
            #   tags$textarea(
            #     paste(names(available_datasets)[-1], collapse = "\n"),
            #     style = "width: 100%; resize: none;",
            #     rows = 12,
            #     readonly = TRUE
            #   )
            # ),

            # User FYI
            conditionalPanel( #hide
              condition = "0",
              br(),
              textInput(
                inputId = "ask_question",
                label = NULL,
                placeholder = "Q&A: Ask about the code, result, error, or statistics in general.",
                value = ""
              ),

              tippy::tippy_this(
                "ask_question",
                "Walk me through this code. What does this result mean? 
                What is this error about? Explain logistic regression. 
                List R packages for time series analysis. 
                Hit Enter to send the request.",
                theme = "light-border"
              ),
              shinyjs::hidden(actionButton("ask_button", strong("Ask RTutor")))
            ),

            # Data options
            conditionalPanel( #hide
              condition = "0",
              fluidRow(
                column(
                  width = 4,
                  tags$head(tags$style(
                    "#data_edit_modal{background-color: #F6FFF5;border-color: #90BD8C;}"
                  )),
                  actionButton("data_edit_modal", "Data Types")
                ),
                column(
                  width = 4,
                  tags$head(tags$style(
                    "#data_desc_modal{background-color: #F6FFF5;border-color: #90BD8C;}"
                  )),
                  actionButton("data_desc_modal", "Description")
                ),
                column(
                  width = 4,
                  tags$head(tags$style(
                    "#download_data{background-color: #F6FFF5;border-color: #90BD8C;}"
                  )),
                  # download data
                  downloadButton("download_data", "Data")
                ),
              )
            ),

            # User FYI and feedback options
            conditionalPanel(
              condition = "0",
              textOutput("usage"),
              textOutput("total_cost"),
              textOutput("temperature"),
              #uiOutput("slava_ukraini"),
              #br(),
              textOutput("retry_on_error"),
              checkboxInput("Comments", "Comments & questions"),
              tags$style(type = "text/css", "textarea {width:100%}"),
              tags$textarea(
                id = "user_feedback",
                placeholder = "Any questions? Suggestions? Things you like, don't like? Leave your email if you want to hear back from us.",
                rows = 4,
                ""
              ),
              radioButtons("helpfulness", "How useful is RTutor?",
                c(
                  "Not at all",
                  "Slightly",
                  "Helpful",
                  "Extremely"
                ),
                selected = "Slightly"
              ),
              radioButtons("experience", "Your experience with R:",
                c(
                  "None",
                  "Beginner",
                  "Intermediate",
                  "Advanced"
                ),
                selected = "Beginner"
              ),
              actionButton("save_feedbck", "Save Feedback")
            ),
          ),

      ###############################################################################
      # Main
      ###############################################################################
          mainPanel(
            shinyjs::useShinyjs(),

            conditionalPanel(
              condition = "input.submit_button == 0",
              fluidRow(
                column(
                  width = 9,
                  h3(style = "font-weight: bold;", "Hero MotoCorp Data Portal (demo)"),
                  h4("Based on available datasets shared by HMCL"),
                  align = "left"
                ),
                column(
                  width = 3,
                  img(
                    src = "www/logo.png",
                    width = "155",
                    height = "77"
                  ),
                  align = 'left'
                )
              ),
            ),
            conditionalPanel(
              condition = "input.submit_button != 0",
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
                condition = "input.use_python == 0",

                uiOutput("error_message"),
                #uiOutput("send_error_message"),
                #strong("Results:"),

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
                condition = "input.use_python == 1",
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
        ),
        shinyjs::hidden(
          div(
            id = "second_file",
            hr(),
            h4("2nd dataset: df2     (Must specify, e.g. 'create a piechart of X in df2.')"),
            textOutput("data_size_2"),
            DT::dataTableOutput("data_table_DT_2")

          )
        )
        #,tableOutput("data_table"),
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
        br(),
        verbatimTextOutput("rmd_chunk_output")
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
            plotly::plotlyOutput("missing_values", width = "60%"),
            shinyjs::hidden(
              div(
                id = "second_file_summary",
                br(),hr(),
                h4("Data structure: df2"),
                verbatimTextOutput("data_structure_2"),
                br(),hr(),
                h4("Data summary: df2"),
                verbatimTextOutput("data_summary_2"),
                plotly::plotlyOutput("missing_values_2", width = "60%")
              )
            )
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
          ),
          # Hide extra EDA tabs for simplicity
          conditionalPanel(
            condition = "0",
            tabPanel(
              title = "Table1",
              uiOutput("table1_inputs"),
              verbatimTextOutput("table1"),
              h4(
                "Generated by the CreateTableOne() function in the",
                a(
                  "tableone",
                  href="https://cran.r-project.org/web/packages/tableone/vignettes/introduction.html",
                  target = "_blank"
                ),
                "package."
              )
            ),

            tabPanel(
              title = "Categorical",
              h4(
                "Generated by the plot_bar() function in the",
                a(
                  "DataExplorer",
                  href="https://cran.r-project.org/web/packages/DataExplorer/vignettes/dataexplorer-intro.html",
                  target = "_blank"
                ),
                "package."
              ),
              plotOutput("distribution_category")
            ),
            tabPanel(
              title = "Numerical",
              h4(
                "Generated by the plot_qq() and plot_histogram() functions in the",
                a(
                  "DataExplorer",
                  href="https://cran.r-project.org/web/packages/DataExplorer/vignettes/dataexplorer-intro.html",
                  target = "_blank"
                ),
                "package."
              ),
              plotOutput("qq_numeric"),
              plotOutput("distribution_numeric"),

            ),
            tabPanel(
              title = "Correlation",
              h4(
                "Generated by the corr_plot() functions in the",
                a(
                  "corrplot",
                  href="https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html",
                  target = "_blank"
                ),
                "package. Blanks indicate no significant correlations."
              ),
              plotOutput("corr_map")
            ),
            tabPanel(
              title = "GGpairs",
              uiOutput("ggpairs_inputs"),
              h4(""),
              h4(
                "Please wait 1 minutes for this plot to be generated by the ggpairs() functions in the",
                a(
                  "GGally",
                  href="https://cran.r-project.org/web/packages/GGally/index.html",
                  target = "_blank"
                ),
                "package."
              ),
              plotOutput("ggpairs")
            ),
            tabPanel(
              title = "EDA Reports",
              h4("Comprehensive EDA (exploratory data analysis)"),
              uiOutput("eda_report_ui")
            )
          ),
        )
      ),

      #############################
      # 'About' Tab Panel
      tabPanel(
        title = "About",
        value = "About",
        uiOutput("RTutor_version"),
        hr(),
        p("RTutor uses ",
          a(
            "OpenAI's",
            href = "https://openai.com/",
            target = "_blank"
          ),
          " powerful large language models",
          " to translate natural language into R code, which is then excuted.",
          "You can request your analysis,
          just like asking a real person.",
          # "Upload a data file (CSV, TSV/tab-delimited text files, and Excel) 
          # and just analyze it in plain English.",
          "Your results can be downloaded as an HTML report or RMarkdown file in minutes!"
        ),
        p("NO WARRANTY! Some of the scripts run but may yield incorrect result. 
        Please use the auto-generated code as a starting 
        point for further refinement and validation."
        ),

        # hr(),
        p(" Written by Dr. Steven Ge",
          # a(
          #   "(Twitter, ",
          #   href = "https://twitter.com/StevenXGe",
          #   target = "_blank"
          # ),
          # a(
          #   "LinkedIn),",
          #   href = "https://www.linkedin.com/in/steven-ge-ab016947/",
          #   target = "_blank"
          # ),       
          " as part of RTutor LLC." #"For feedback, please email",
          # a(
          #   "gexijin@gmail.com.",
          #   href = "mailto:gexijin@gmail.com?Subject=RTutor"
          # ),
          # " Source code at ",
          # a(
          #   "GitHub,",
          #   href = "https://github.com/gexijin/RTutor"
          # ),
          # " from where you can also find 
          # instruction to install RTutor as an R package. 
          # The RTutor website and the source code is free for non-profit organizations ONLY. Licensing is required for commercial use."
        ),
        # h4("For businesses, RTutor can be customized and locally installed to  
        # easily gain insights from your data (files, SQL databases, or APIs) at a low cost. We will be happy to discuss."),

        hr(),
        # p("RTutor went viral on ", 
        #     a(
        #       "LinkedIn, ",
        #       href = "https://www.linkedin.com/feed/update/urn:li:activity:7008179918844956672/"
        #     ), 
        #     a(
        #       "Twitter, ",
        #       href = "https://twitter.com/StevenXGe/status/1604861481526386690"
        #     ),
        #     a(
        #       "Twitter(Physacourses),",
        #       href = "https://twitter.com/Physacourses/status/1602730176688832513?s=20&t=z4fA3IPNuXylm3Vj8NJM1A"
        #     ),
        #     " and ",
        #     a(
        #       "Facebook (Carlo Pecoraro).",
        #       href = "https://www.facebook.com/physalia.courses.7/posts/1510757046071330"
        #     )
        # ),

        # hr(),

        # uiOutput("package_list"),

        # hr(),

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
          )#,

          # # Site Update Log component
          # column(
          #   width = 6,
          #   h4(style = "font-weight: bold;", "Site Updates Log"),
          #   tableOutput("site_updates_table")
          # )
        ),

        hr(),

        # # Session Info Section
        # fluidRow(
        #   column(
        #     width = 12,
        #     uiOutput("session_info")
        #   )
        # ),
      ),

#      tabPanel(
#        title = "Disqus",
#        value = "Disqus",
#        div(
#        tags$head(includeHTML(app_sys("app", "www", "disqus.html")))
#        )
#      )
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
