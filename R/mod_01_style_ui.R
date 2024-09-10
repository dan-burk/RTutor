# style_module.R

# Define a UI module for the HTML styles
mod_01_style_ui <- function(id) {
  ns <- shiny::NS(id)

  # Add color to UI
  tags$head(tags$style(HTML("
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
  ")))
  
}