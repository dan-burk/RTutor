###################################################
# RTutor.AI, a Shiny app for chating with your data
# Author: Xijin Ge    gexijin@gmail.com
# Dec. 6-12, 2022.
# No warranty and not for commercial use.
###################################################


###################################################
# Global Variables
###################################################

release <- "0.98" # RTutor
no_data <- "no_data" # no data is uploaded or selected
user_upload <- "User Upload" # data is uploaded by user, used to be called uploaded_data
min_query_length <- 6  # minimum # of characters
max_query_length <- 2000 # max # of characters
language_models <- c("gpt-4o-2024-08-06",  "gpt-4o-mini", "gpt-3.5-turbo")
names(language_models) <- c("GPT-4o", "GPT-4o mini", "GPT-3.5 Turbo")
default_model <- "GPT-4o"  # "GPT-4 Turbo"   # "ChatGPT"   # "GPT-4 (03/23)"
max_content_length <- 3000 # max tokens:  Change according to model !!!!
default_temperature <- 0.2
pre_text <- "Write correct, efficient R code to answer this prompt:"
pre_text_python <- "Write correct, efficient Python code."
after_text <- "Use the df data frame."
max_data_points <- 10000  # max number of data points for interactive plot
max_levels_factor_conversion <- 5 # Numeric columns will be converted to factor if less than or equal to this many levels
# if a column is numeric but only have a few unique values, treat as categorical
unique_ratio <- 0.05   # number of unique values / total # of rows
max_eda_levels <- 12 # max number of levels in categorical varaible for EDA, ggairs
max_eda_var <- 20 # maximum num of variables in EDA
sqlitePath <- "../../data/usage_data.db" # folder to store the user queries, generated R code, and running results
sqltable <- "usage"

# additional prompts to send to ChatGPT
system_role <- "Act as an experienced data scientist and statistician. You will write R code following instructions. Do not provide explanation.
Try to produce a plot when possible. ggplot2 is preferred. Make the plot visually appealing. If multiple plots are generated, try to combine them into one."

# If this file exists, running on the server. Otherwise local. This is used to change app behavior.
on_server <- "on_server.txt"



###################################################################
# Load Data & Demo Prompts
###################################################################

######### Load Built-In Data with Base R #########

# Create a list of available datasets to print on the sidebar
# available_datasets <- list(
#   "Select a dataset:" = NULL,
#   "User Upload" = NULL, #user_upload
#   "No Data" = no_data,
#   "Iris" = "iris",
#   "MTCars" = "mtcars",
#   "Air Quality" = "airquality",
#   "Diamonds" = "diamonds",
#   "CO2" = "CO2",
#   "Tooth Growth" = "ToothGrowth",
#   "Pressure" = "pressure",
#   "Chick Weights" = "ChickWeight"
# )

available_datasets <- c("Select a dataset:", no_data, "iris", "mtcars", "airquality", "diamonds",
  "CO2", "ToothGrowth", "pressure", "ChickWeight", user_upload
  )
names(available_datasets) <- c("Select a dataset:", "No Data", "Iris", "MTCars",
  "Air Quality", "Diamonds", "CO2", "Tooth Growth", "Pressure", "Chick Weights", "User Upload")

# load demo requests for different datasets (demo questions)
demo <- read.csv(app_sys("app", "www", "demo_questions.csv"))

# load jokes
jokes <- demo[
  which(demo$data == "jokes"),
  "requests"
]


###################################################################
# Prepare User Input & Command & API Key
###################################################################


#' Prepare User input.
#'
#' The response from GPT3 sometimes contains strings that are not R commands.
#'
#' @param txt A string that stores the user input.
#' @param selected_data Name of the dataset.
#' @param df the data frame
#' @param use_python  whether or not using python instead of R
#' @param chunk_id  first or not? First chunk add data description
#'
#' @return Returns a cleaned up version, so that it could be sent to GPT.
prep_input <- function(txt, selected_data, df, use_python, chunk_id, send_head = TRUE) { #df2 = NULL, df2_name = NULL

  if (is.null(txt) || is.null(selected_data)) {
    return(NULL)
  }
  # if too short, do not send.
  if (nchar(txt) < min_query_length || nchar(txt) > max_query_length) {
    return(NULL)
  }

  # remove extra space at the end.
  txt <- gsub(" *$|\n*$", "", txt)
  # some times it is like " \n "
  txt <- gsub(" *$|\n*$", "", txt)
  # if last character is not a period. Add it. Otherwise,
  # Davinci will try to complete a sentence.
  if (!grepl("\\.$|?", txt)) {
    txt <- paste(txt, ".", sep = "")
  }

  if (!is.null(selected_data)) {
    if (selected_data != no_data) {

      # variables mentioned in request
      relevant_var <- sapply(
        colnames(df),
        function(x) {
          # hwy. class
          grepl(
            tolower(
              paste0(
                " ", # proceeding space
                x,
                "[ |\\.|,|?]" # ending space, comma, period, or question mark
              )
            ),
            txt
          )
        }
      )
      relevant_var <- names(relevant_var)[relevant_var]

      data_info <- describe_df(
        df, 
        list_levels = TRUE, 
        relevant_var = relevant_var,
        send_head = send_head
      )
      # # Always add 'use the df data frame.'
      txt <- paste(txt, after_text)

      n_words <- tokens(data_info)
      # #if it is the first chunk;  or if data description is short
      more_info <- chunk_id <= 1 || n_words < 200

      # # in a session, sometimes the first chunk has the id of 0. sometimes 1?????

      # # add data descrdiption
      # # if it is not the first chunk and data description is long, do not add.
      if (more_info && !(chunk_id > 1 && n_words > 600)) {
        txt <- paste(txt, data_info)
      }
      
      # # if there is a second data frame, add that too.
      # if(!is.null(df2)) {
      #   if(is.null(df2_name)) {
      #     df2_name <- "df2"
      #   } 

      #   # 2nd data must be specificall called
      #   if(grepl(df2_name, txt)) {
      #     data_info_2 <- describe_df(
      #       df2, 
      #       list_levels = TRUE, 
      #       relevant_var = relevant_var,
      #       send_head = send_head
      #     )
      #     data_info_2 <- gsub("df data frame", paste0(df2_name, " data frame"), data_info_2)

      #     n_words <- tokens(data_info_2)
      #     if (more_info && !(chunk_id > 1 && n_words > 600)) {
      #       txt <- paste(txt, data_info_2)
      #     }
      #   }
      # }

    }
  }

  txt <- paste(
    ifelse(
      use_python,
      pre_text_python,
      pre_text
    ),
    txt
  )

  # replace newline with space.
  txt <- gsub("\n", " ", txt)
  return(txt)
}


#' Describe data frame
#'
#' Returns information on data frame describing columns.
#'
#' @param df a data frame
#' @param list_levels whether to list levels for factors
#' @param relevant_var  a list of variables mentioned by the user
#' @return Returns a cleaned up version, so that it could be executed as R command.
describe_df <- function(df, list_levels = FALSE, relevant_var = NULL, send_head = TRUE) {

  data_info <- ""
  numeric_index <- sapply(
    df,
    function(x) {
      if (is.numeric(x)) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    }
  )

  numeric_var <- colnames(df)[numeric_index]
  cat_var <- colnames(df)[!numeric_index]

  # calculate total number of unique levels
  total_levels <- sapply(cat_var, function(x) {length(unique(df[, x]))})
  # remove columns that are names, strings, etc
  cat_var <- cat_var[total_levels < nrow(df) * 0.8]

  # numeric variables
  if (length(numeric_var) == 1) {
    data_info <- paste0(
      data_info,
      "The df data frame has a column ",
      numeric_var,
      " that contains a numeric variable. "
    )
  } else if (length(numeric_var) > 1) {
    data_info <- paste0(
      data_info,
      "The df data frame contains these numeric variables: ",
      paste0(
        numeric_var[1:(length(numeric_var) - 1)],
        collapse = ", "
      ),
      ", and ",
      numeric_var[length(numeric_var)],
      ". "
    )
  }
  # Categorical variables-----------------------------
  # numeric variables
  if (length(cat_var) == 1) {
    data_info <- paste0(
      data_info,
      "The df data frame has a column ",
      cat_var,
      " that contains a categorical variable. "
    )
  } else if (length(cat_var) > 1) {
    data_info <- paste0(
      data_info,
      "The df data frame contains these categorical variables: ",
      paste0(
        cat_var[1:(length(cat_var) - 1)],
        collapse = ", "
      ),
      ", and ",
      cat_var[length(cat_var)],
      ". "
    )
  }
  
  if(list_levels & length(relevant_var) > 0) {

    # only list for categorical variables specified in user prompt
    relevant_cat_var <- intersect(relevant_var, cat_var)
  # describe the levels in categorical variable
    for (var in relevant_cat_var) {
      max_lelvels_description <- 4
      ix <- match(var, colnames(df))
      factor_levels <- sort(table(df[, ix]), decreasing = TRUE)
      factor_levels <- names(factor_levels)

      # have more than 6 levels?
      many_levels <- FALSE

      if (length(factor_levels) > max_lelvels_description) {
        many_levels <- TRUE
        factor_levels <- factor_levels[1:max_lelvels_description]
      }

      last_level <- factor_levels[length(factor_levels)]
      factor_levels <- factor_levels[-1 * length(factor_levels)]
      tem <- paste0(
        factor_levels,
        collapse = "', '"
      )
      if (!many_levels) { # less than 6 levels
        factor_levels <- paste0("'", tem, "', and '", last_level, "'")
      } else { # more than 6 levels
        factor_levels <- paste0(
          "'",
          tem,
          "', '",
          last_level,
          "', etc"
        )
      }
      data_info <- paste0(
        data_info,
        "The categorical variable ",
        var,
        " has these levels: ",
        factor_levels,
        ". "
      )
    }
  }
  
  if(send_head) {
    #randomly select 5 rows, print out, convert to string
    sample_rows <- paste0(
      capture.output(as.data.frame(df[sample(nrow(df), 5),])), 
      collapse = "\n"
    )
    # if too long, use only 2 rows
    if(nchar(sample_rows) > 1000) {
      sample_rows <- paste0(
        capture.output(as.data.frame(df[sample(nrow(df), 2),])), 
        collapse = "\n"
      )
    }
    sample_rows <- paste(
      "The df data frame looks like this: \n",
      sample_rows
    )
    
    # if still too long, skip
    if(nchar(sample_rows) > 2000) {
      sample_rows <- ""
    }

    data_info <- paste0(
      data_info,
      sample_rows    
    )
  }


  return(data_info)
}


#' Clean up R commands generated by GTP
#'
#' The response from GTP3 sometimes contains strings that are not R commands.
#'
#' @param cmd A string that stores the completion from GTP3.
#' @param selected_data, name of the selected dataset.
#' @param on_server, whether or not running on the server.
#' @return Returns a cleaned up version, so it can be executed as an R command.
clean_cmd <- function(cmd, selected_data, on_server = FALSE) {
  req(cmd)
  # simple way to check
  if (grepl("That model is currently overloaded with other requests.|Error:", cmd)) {
    return(NULL)
  }
  # Use cat to converts \n to newline
  # use capture.output to get the string
  cmd <- capture.output(
    cat(cmd)
  )
  cmd <- polish_cmd(cmd)

  # replace install.packages by "#install.packages"
  cmd <- gsub("install.packages", "#install.packages", cmd)

  # prevent running system commands, malicious
  # system("...")  --> #system("...")
  cmd <- gsub("system *\\(", "#system\\()", cmd)
  cmd <- gsub("source *\\(", "#source\\()", cmd)
  cmd <- gsub("unlink *\\(", "#unlink\\()", cmd)
  cmd <- gsub(
    "(link|dir|link)_(create|delete|chmod|chown|move) *\\(",
    "#MASKED_FILE_OPERATION\\(", cmd
  )

  # use pacman, load if installed; otherwise install it first then load.
  if (!on_server) {
    cmd <- gsub("library\\(", "pacman::p_load\\(", cmd)
  }
  #if (selected_data != no_data) {
  #  cmd <- c("df <- as.data.frame(current_data())", cmd)
  #}

  return(cmd)

}


#' Remove Markdown and explanation
#'
#' The response from GTP3 sometimes contains strings that are not R commands.
#'
#' @param cmd A string that stores the completion from GTP3.
#' @return Returns a cleaned up version, so that it could be executed as R command.
polish_cmd <- function(cmd) {

  cmd <- gsub("``` *", "```", cmd)
  #                    ```{python} ```{r}               ```python                    ^```
  cmd <- gsub(".*(```\\{(PYTHON|Python|python|R|r|bash|sql|js|rcpp|css)\\}|```(PYTHON|Python|python|R|r|bash|sql|js|rcpp|css)|^```)", "", cmd)

  # remove anything after ```
  cmd <- gsub("```.*", "", cmd)

  # sometimes ChatGPT returns \r\n as new lines. The \r causes error.
  cmd <- gsub("\r", "", cmd)

  return(paste0("\n", cmd))
}


#' Estimate tokens from text
#'
#'
#' @param text a string
#'
#' @return a number
#'
tokens <- function(text) {
  # Approximate tokenization by splitting on spaces and punctuations
  tokens <- unlist(strsplit(text, "[[:space:]]|[[:punct:]]"))

  # Filter out empty tokens
  tokens <- tokens[nchar(tokens) > 0]

  # Further split longer tokens (this is a very crude approximation)
  long_tokens <- tokens[nchar(tokens) > 3]
  additional_tokens <- sum(nchar(long_tokens) %/% 4)

  total_tokens <- length(tokens) + additional_tokens

  return(total_tokens)
}


#' Estimate API cost
#'
#'
#' @param prompt_tokens a number
#' @param completion_tokens a number
#' @param selected_model a string
#'
#' @return a number
#'
api_cost <- function(prompt_tokens, completion_tokens, selected_model) {
  if (grepl("gpt-4", selected_model)) { # gpt4
    # input token $0.03 / 1k token, Output is $0.06 / 1k for GPT-4
    completion_tokens * 6e-5 + prompt_tokens  * 3e-5
  } else {
    # ChatGPT
    completion_tokens * 2e-6 + prompt_tokens  * 1.5e-6
  }
}


###################################################################
# Prepare Data
###################################################################


#' Returns true only defined and has a value of true
#'
#' This is used when some the input variables are not defined globally,
#' But could be turned on. if you use if(input$selected), it will
#' give an error.
#'
#' @param x
#'
#' @return Returns TRUE or FALSE
turned_on <- function(x) {

  # length = 0 when NULL, length = 3 if vector
  if (length(x) != 1) {
    return(FALSE)
  } else {

    # contain logical value?
    if (!is.logical(x)) {
      return(FALSE)
    } else {
      # return the logical value.
      return(x)
    }
  }
}


#' Returns a data frame with some numeric columns with fewer levels
#' converted as factors
#'
#'
#' @param df a data frame
#' @param max_levels_factor  max levels, defaults to max_levels
#' @param max_proportion_factor max proportion
#'
#' @return Returns a data frame
numeric_to_factor <- function(df, max_levels_factor, max_proportion_factor) {
  # some columns looks like numbers but have few levels
  # convert these to factors

  convert_index <- sapply(
    df,
    function(x) {
      if (
        (is.numeric(x) || is.character(x)) &&
          # if there are few unique values compared to total values
          length(unique(x)) / length(x) < max_proportion_factor &&
          length(unique(x)) <= max_levels_factor  # less than 12 unique values
        # relcassify numeric variable as categorical
      ) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    }
  )

  convert_var <- colnames(df)[convert_index]
  for (var in convert_var) {
    eval(
      parse(  # df$cyl <- as.factor(df$cyl)
        text = paste0("df$", var, " <- as.factor(df$", var, ")")
      )
    )
  }
  return(df)

}


###################################################################
# Miscellaneous
###################################################################


#' Plot missing values
#'
#'
#' @param df a dataframe
#'
#' @return a plot
#'
#ploting missing values
missing_values_plot <- function(df) {
  req(!is.null(df))

  # Calculate the total number of missing values per column
  missing_values <- sapply(df, function(x) sum(is.na(x)))

  # Calculate the number of cases with at least one missing value
  cases_with_missing <- sum(apply(df, 1, function(x) any(is.na(x))))

  # Check if there are any missing values
  if (all(missing_values == 0)) {
    return(NULL)
  } else {
    # Create a data frame for plotting
    missing_data_df <- data.frame(
      Column = c(names(missing_values), "At Least One Missing"),
      MissingValues = c(missing_values, cases_with_missing)
    )
    # Calculate the percentage of missing values per column
    # missing_percentage <- (missing_values / nrow(df)) * 100
    # Plot the number of missing values for all columns with labels
    ggplot(missing_data_df, aes(x = Column, y = MissingValues, fill = Column)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = sprintf("%.0f%%", MissingValues / nrow(df) * 100)), hjust = -5) + # Add labels to the bars
      # geom_text(aes(label = sprintf("%.2f%%", MissingPercentage)), hjust = -0.3) +
      coord_flip() + # Makes the bars horizontal
      labs(title = "Number of Missing Values by Column", x = "Column", y = "Number of Missing Values") +
      scale_fill_brewer(palette = "Set3") + # Use a color palette for different bars
      theme(legend.position = "none", axis.title.y = element_blank()) + # Remove the legend
      scale_y_continuous(expand = expansion(mult = c(0, 0.2))) # Extend the y-axis limits by 10%
  }
}


# RMarkdown file's Header for knit python chunks
Rmd_script_python <-
  "---
  output: html_fragment
  params:
    df:
  printcode:
    label: \"Display Code\"
    value: TRUE
    input: checkbox
  ---

  ```{r, echo=FALSE, message=FALSE, warning=FALSE}
  library(reticulate)
  df <- params$df
  ```

  ```{python, echo = FALSE, message=FALSE}
  df = r.df
  ```

  #### Results:"


#' Generate html file from Python code
#'
#'
#' @param python_code, a chunk of code
#' @param html_file file name for output
#' @param select_data selected_dataset_name
#' @param current_data   current_data()
#'
#' @return -1 if failed. If success, the the designated html file is written
#'
python_html <- function(python_code, select_data, current_data) {
  withProgress(message = "Running Python...", {
    incProgress(0.2)
    temp_rmd <- paste0(tempfile(), "_temp.Rmd")

    # html file is generated using the same file name except the extension
    html_file <- gsub("Rmd$", "html", temp_rmd)

    Rmd_script <- paste0(
      Rmd_script_python,
      "\n```{python, echo=FALSE}\n",
      python_code,
      "\n```\n"
    )
    write(
      Rmd_script,
      file = temp_rmd,
      append = FALSE
    )

    # Set up parameters to pass to Rmd document
    params <- list(df = iris) # dummy

    # if uploaded, use that data
    req(select_data)
    if (select_data != no_data) {
      params <- list(
        df = current_data
      )
    }

    req(params)
    # Knit the document, passing in the `params` list, and eval it in a
    # child of the global environment (this isolates the code in the document
    # from the code in this app).
    try(
      rmarkdown::render(
        input = temp_rmd, # markdown_location,
        params = params,
        envir = new.env(parent = globalenv())
      )
    )
  })  # progress bar

  if (file.exists(html_file)) {
    return(html_file)
  } else {
    return(-1)
  }
}


# #' Creates a SQLite database file for collecting user data
# #'
# #' The data file should be stored in the ../../data folder inside
# #' the container. From outside in the RTutor_server folder,
# #' it is in data folder.
# #'  Only works on local machines. Not on linux.
# #' @return nothing
# create_usage_db <- function() {
#   # if db does not exist, create one
#   if(!file.exists(sqlitePath)) {
#     db <- RSQLite::dbConnect(RSQLite::SQLite(), gsub(".*/", "", sqlitePath))
#     txt <- sprintf(
#       paste0(
#       "CREATE TABLE ",
#         sqltable,
#         "(\n",
#         "date DATE NOT NULL,
#         time TIME NOT NULL,
#         request varchar(5000),
#         code varchar(5000),
#         error int ,
#         data_str varchar(5000))"
#       )
#     )
#       # Submit the update query and disconnect
#       RSQLite::dbExecute(db, txt)
#       RSQLite::dbDisconnect(db)
#   }
# }
# # To create a database under Ubuntu
# # sudo apt update
# # sudo apt install sqlite3
# # cd ~/Rtutor_server/data
# # sudo  sqlite3 usage_data.db
# # CREATE TABLE usage (
# #        date DATE NOT NULL,
# #        time TIME NOT NULL,
# #        request varchar(5000),
# #        code varchar(5000),
# #        error int,
# #        data_str varchar(5000),
# #       dataset varchar(100));
# # sudo chmod a+w usage_data.db
# # note that error column, 1 means error, 0 means no error, success.
#' Saves user queries, code, and error status
#'
#'
#' @param date Date in the format of "2023-01-04"
#' @param time Time "13:05:12"
#' @param request, user request
#' @param code AI generated code
#' @param error status, TRUE, error
#' @param chunk, id, from 1, 2, ...
#' @param api_time  time in seconds for API response
#' @param tokens  total completion tokens
#' @param filename name of the uploaded file
#' @param filesize size
#'
#' @return nothing
# save_data <- function(
#   date, time, request, code, error_status,
#   data_str, dataset, session, filename,
#   filesize, chunk, api_time, tokens, language
# ) {
#   # if db does not exist, create one
#   if (file.exists(sqlitePath)) {
#     # Connect to the database
#     db <- RSQLite::dbConnect(RSQLite::SQLite(), sqlitePath, flags = RSQLite::SQLITE_RW)
#     # Construct the update query by looping over the data fields
#     txt <- sprintf(
#       "INSERT INTO %s (%s) VALUES ('%s')",
#       sqltable,
#       "date, time, request, code, error, data_str, dataset, session, filename, filesize, chunk, api_time, tokens, language",
#       paste(
#         c(
#           as.character(date),
#           as.character(time),
#           clean_txt(request),
#           clean_txt(code),
#           as.integer(error_status),
#           clean_txt(data_str),
#           dataset,
#           session,
#           filename,
#           filesize,
#           chunk,
#           api_time,
#           tokens,
#           language
#         ),
#         collapse = "', '"
#       )
#     )
#     # Submit the update query and disconnect
#     try(
#       RSQLite::dbExecute(db, txt)
#     )
#     RSQLite::dbDisconnect(db)
#   }
# }
# SQLite command to create feedback table
# "CREATE TABLE feedback (
#        date DATE NOT NULL,
#        time TIME NOT NULL,
#        helpfulness varchar(50),
#        experience varchar(50),
#        comments varchar(5000)); "
#' Save user feedback
#'
#'
#' @param date Date in the format of "2023-01-04"
#' @param time Time "13:05:12"
#' @param comments, user request
#' @param helpfulness rating
#' @param experience  R experience
#'
#' @return nothing
# save_comments <- function(date, time, comments, helpfulness, experience) {
#   # if db does not exist, create one
#   if (file.exists(sqlitePath)) {
#     # Connect to the database
#     db <- RSQLite::dbConnect(RSQLite::SQLite(), sqlitePath, flags = RSQLite::SQLITE_RW)
#     # Construct the update query by looping over the data fields
#     txt <- sprintf(
#       "INSERT INTO %s (%s) VALUES ('%s')",
#       "feedback",
#       "date, time, comments, helpfulness, experience",
#       paste(
#         c(
#           as.character(date),
#           as.character(time),
#           clean_txt(comments),
#           helpfulness,
#           experience
#         ),
#         collapse = "', '"
#       )
#     )
#     # Submit the update query and disconnect
#     try(
#       RSQLite::dbExecute(db, txt)
#     )
#     RSQLite::dbDisconnect(db)
#   }
# }


# Create a data frame with questions and answers for FAQ section
# Used in faq_list component
faqs <- data.frame(
  question = c(
    "What is RTutor.ai?",
    "How does RTutor.ai work?",
    "Who is it for?",
    "How do you make sure the results are correct?",
    "Why do I get different results with the same request?",
    "Can people without R coding experience use RTutor for statistical analysis?",
    "Can this replace statisticians or data scientists?",
    "How do I write my request effectively?",
    "Can I install R packages in the AI generated code?"
  ),
  answer = c(
    "RTutor.ai is an artificial intelligence (AI)-based app that enables users to interact with their data via natural language. Users ask questions about or request analyses in English. The app generates and runs R code to answer that question with plots and numeric results.",  #After uploading a dataset, users ask questions about or request analyses in English. The app generates and runs R code to answer that question with plots and numeric results.",
    "The requests are structured and sent to OpenAI’s AI system, which returns R code. The R code is cleaned up and executed in a Shiny environment, showing results or error messages. Multiple requests are logged to produce an R Markdown file, which can be knitted into an HTML report. This enables record keeping and reproducibility.",
    "The primary goal is to help people with some R experience to learn R or be more productive. RTutor can be used to quickly speed up the coding process using R. It gives you a draft code to test and refine. Be wary of bugs and errors.",
    "Try to word your question differently and try the same request several times. Then users can double-check to see if they get the same results from different runs.",  #A higher temperature parameter will give diverse choices. Then users can double-check to see if they get the same results from different runs.",
    "OpenAI’s language model has a certain degree of randomness when giving results, controlled by a 'temperature' parameter. Though this is set low, the app still may produce varying results.", #"OpenAI’s language model has a certain degree of randomness that could be adjusted by parameters called 'temperature'. Set this in Settings.",
    "Not entirely. This is because the generated code can be wrong. However, it could be used to quickly conduct data visualization and exploratory data analysis (EDA). Just be mindful of this experimental technology.",
    "No. But RTutor can make them more efficient.",
    "Imagine you have a summer intern, a college student who took one semester of statistics and R. You send the intern emails with instructions, and he/she sends back code and results. The intern is not experienced, thus error-prone, but is hard-working. Thanks to AI, this intern is lightning-fast and nearly free.",
    "No. But we are working to pre-install all the top 5000 most frequently used R packages on the server. Chances are that your favorite package is already installed."#,
  ),
  stringsAsFactors = FALSE
)
