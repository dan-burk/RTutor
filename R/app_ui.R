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
    shinyjs::useShinyjs(),

    ### Style Module ###
    mod_01_style_ui("mod_01_style_ui.R"),

    navbarPage(
      HTML('<span style="color: black;">HMCL</span>'),
      id = "tabs",

      ### 'Home' Tab Panel ###
      tabPanel(
        title = "Home",

        sidebarLayout(
          ### Sidebar ###
          sidebarPanel(
            # Placeholder for Load Data Module
            mod_02_load_data_ui("load_data"), #Split into Load and Chat box, then place load_data above data_edit_modal
            mod_15_data_types_ui("data_edit_modal")
          ),

          ### Main Panel ###
          mainPanel(
            mod_03_main_panel_ui("main_panel")
          )
        )
      ), #tabPanel

      ### 'Report' Tab Panel ###
      tabPanel(
        title = "Report",
        value = "Report",
        tippy::tippy_this(
          "report_tab",
          "Download a Results Report",
          theme = "light-border"
        ),

        ### Report Module ###
        mod_09_report_ui("report")
      ),

      ### 'EDA' Tab Panel ###
      tabPanel(
        title = "EDA",
        value = "EDA",
        tippy::tippy_this(
          "eda_tab",
          "Exploratory Data Analysis",
          theme = "light-border"
        ),

        ### EDA Module ###
        mod_10_eda_ui("eda")
      ),

      ### 'Settings' Tab Panel ###
      tabPanel(
        title = "Settings",
        value = "Settings",
        ### Settings Module ###
        #mod_11_settings_ui("sett")
      ),

      ### 'About' Tab Panel ###
      tabPanel(
        title = "About",
        value = "About",

        ### Miscellaneous Module ###
        mod_08_misc_ui("misc")
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
