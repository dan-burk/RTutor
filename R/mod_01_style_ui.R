# style_module.R

# Define a UI module for the HTML styles
mod_01_style_ui <- function(id) {
  ns <- shiny::NS(id)

  # Add color to UI
  tags$head(tags$style(HTML("
    /* navbar */
    .navbar {background-color: #C1E2BE;border-color: #90BD8C; color: #181818;font-weight: bold;}

    /* tabs */
    .navbar-default .navbar-nav > li > a {background-color: #C1E2BE;
      border-color: #9AC596;color: #181818;font-size: 16px;}

    /* active tab */
    .navbar-default .navbar-nav > .active > a, .navbar-default .navbar-nav > .active > a:focus,
    .navbar-default .navbar-nav > .active > a:hover {background-color: #A0BB9E;color: #181818;font-weight: bold;}

    /* sidebar panel */
    .well {background-color: #C1E2BE;border-color: #90BD8C;}

    /* selectInput & actionButton */
    .custom-select-input, .custom-action-button, .custom-download-button
    {font-size: 24px;color: #000;background-color: #C1E2BE;border-color: #90BD8C;}

    /* selectInput extra customization */
    .selectize-input, .selectize-dropdown {background-color: #F6FFF5 !important;
      border-color: #90BD8C !important;color: #000 !important; font-size: 16px;}

    /* horizontal line (hr()) */
    .custom-hr{border-top: 1px solid #90BD8C;}
    .custom-hr-thick{border-top: 3px solid #90BD8C;}

    /* textarea, textInput, numericInput */
    textarea, input[type = 'text'], input[type='number']
    {width: 100%;background-color: #F6FFF5;border-color: #90BD8C;font-size: 16px;}

    /* tippy this pop-ups */
    .tippy-content {font-size: 15px !important;}

    /* policy styles */
    .policy {background-color: #ededed;background-size: cover;background-position: center;
    min-height: 500px;margin: 0 !important;padding-top: 0px;display: flex;justify-content: center;
    border: 50px solid #bedbb7;color: #262626;text-align: left;flex-direction: column;}
    .policy h1 {font-size: 40px;padding-top: 90px;margin-left: 125px;font-weight: bold;}
    .policy h2 {font-size: 25px;padding-top: 40px;margin-left: 125px;font-weight: bold;}
    .policy h3 {font-size: 20px;padding-top: 20px;margin-left: 125px;font-weight: bold;}
    .policy p {font-size: 17px;margin-top: 20px;margin-right: 125px;margin-left: 125px;}

    body {padding-bottom: 50px;}

    /* Responsive styles, for mobile browsing */
    @media (max-width: 1000px) {
      .productIntro h2{margin: 25px;font-size: 40px;}
      .productIntro p {margin: 25px;}
      .twocol .column.left {margin: 25px !important;padding: 20px !important;align-items: flex-start;}
      .twocol .column.right {margin: 25px !important;padding: 0px;}
      .twocol .column.left h1 {font-size: 40px !important;}
      .twocol .column.left h2 {font-size: 25px !important;}
      .policy h1, .policy h2, .policy h3 {margin: 25px !important;padding: 10px !important;}
      .policy h1 {font-size: 35px;}
      .policy h2 {font-size: 28px;}
      .policy h3 {font-size: 21px;}
      .policy p{margin: 25px !important;}
    }
  ")))
}
