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
uploaded_data <- "User Upload" # used for drop down
no_data <- "no_data" # no data is uploaded or selected
names(no_data) <- "No data (examples)"
min_query_length <- 6  # minimum # of characters
max_query_length <- 2000 # max # of characters
language_models <- c("gpt-4-turbo", "gpt-4o", "gpt-4-1106-preview", "gpt-3.5-turbo", "gpt-3.5-turbo-16k", "gpt-3.5-turbo-0301", "gpt-4", "gpt-4-0314", "text-davinci-003")
names(language_models) <- c("GPT-4 Turbo", "GPT-4o", "GPT-4 Turbo (11/23)", "ChatGPT", "ChatGPT 16k", "ChatGPT (03/23)", "GPT-4", "GPT-4 (03/23)", "Davinci")
default_model <- "GPT-4 Turbo" #"GPT-4 Turbo (11/23)" # "GPT-4o"  # "ChatGPT" #   "GPT-4 (03/23)"
max_content_length <- 3000 # max tokens:  Change according to model !!!!
default_temperature <- 0.2
pre_text <- "Write correct, efficient R code to answer this prompt:"
pre_text_python <- "Write correct, efficient Python code."
after_text <- "Use the df data frame."
max_data_points <- 10000  # max number of data points for interactive plot
max_levels_factor_conversion <- 5 # Numeric columns will be converted to factor if less than or equal to this many levels
# if a column is numeric but only have a few unique values, treat as categorical
unique_ratio <- 0.05   # number of unique values / total # of rows
sqlitePath <- "../../data/usage_data.db" # folder to store the user queries, generated R code, and running results
sqltable <- "usage"

# additional prompts to send to ChatGPT
system_role <- "Act as an experienced data scientist and statistician. You will write R code following instructions. Do not provide explanation.
Try to produce a plot when possible. ggplot2 is preferred. Make the plot visually appealing. If multiple plots are generated, try to combine them into one."
system_role_date <- "Assume Today's date is 2024-03-26."

# If this file exists, running on the server. Otherwise local. This is used to change app behavior.
on_server <- "on_server.txt"



###################################################################
# Load Data & Demo Prompts
###################################################################


######### Folder/Data Path with RDS files #########
# if environmental variable is not set, use relative path
# set the HMCL_DATA environment variable to the data folder such as C:/data/HMCL/
data_path <- Sys.getenv("HMCL_DATA")[1]
# if not defined in the environment, use too levels above
if (nchar(data_path) == 0) {
  data_path <- "/srv/data/" # linux; change to your path if not using HMCL_DATA environment variable
}

# load data
Sales_Masked_Rtutor <- readRDS(paste0(data_path, "Sales_Masked_Rtutor.rds"))
Dispatch_Masked_Rtutor <- readRDS(paste0(data_path, "Dispatch_Masked_Rtutor.rds"))
Vahan_Share_Masked_Rtutor <- readRDS(paste0(data_path, "Vahan_Masked_Rtutor.rds"))

# Create list of available datasets to print on sidebar
available_datasets <- list(
  "Select a dataset:" = NULL,
  "Sales Data" = "Sales_Masked_Rtutor.rds",
  "Registration Data" = "Vahan_Masked_Rtutor.rds",
  "Dispatch Data" = "Dispatch_Masked_Rtutor.rds"
)

# load meta data, from JSON file
meta_data <- function() {
  readr::read_file(paste0(data_path, "metadata.json"))
}
# load meta data, from csv file
meta_data_csv <- function() {
  read.csv(paste0(data_path, "hmcl_metadata.csv"))
}


# A file, demo requests for different datasets, demo questions
demo <- read.csv(app_sys("app", "www", "demo_questions.csv"))

# extract jokes
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
prep_input <- function(txt, selected_data, df, use_python, chunk_id, selected_model) {

  if(is.null(txt) || is.null(selected_data)) {
    return(NULL)
  } 
  # if too short, do not send. 
  if(nchar(txt) < min_query_length || nchar(txt) > max_query_length) {
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
            paste0(
              " ", # proceeding space
              x,
              "[ |\\.|,|?]" # ending space, comma, period, or question mark
            ),
          txt
          )
        }
      )
      relevant_var <- names(relevant_var)[relevant_var]

      # Always add 'use the df data frame.'
      txt <- paste(txt, after_text)

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


#' Clean up R commands generated by GTP
#'
#' The response from GTP3 sometimes contains strings that are not R commands.
#'
#' @param cmd A string that stores the completion from GTP3.
#' @param selected_data, name of the selected dataset. 
#' @param on_server, whether or not running on the server.
#' @return Returns a cleaned up version, so that it could be executed as R command.
clean_cmd <- function(cmd, selected_data, on_server = FALSE) {
  req(cmd)
  # simple way to check
  if(grepl("That model is currently overloaded with other requests.|Error:", cmd)) {
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
  if(!on_server) {
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
  if(grepl("gpt-4", selected_model)) { # gpt4
    # input token $0.03 / 1k token, Output is $0.06 / 1k for GPT-4
    completion_tokens * 6e-5+ prompt_tokens  * 3e-5
  } else {
    # ChatGPT
    completion_tokens * 2e-6+ prompt_tokens  * 1.5e-6 
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
numeric_to_factor <- function(df, max_levels_factor, max_proptortion_factor) {
  # some columns looks like numbers but have few levels
  # convert these to factors

  convert_index <- sapply(
    df,
    function(x) {
      if (
        (is.numeric(x) || is.character(x)) &&
        # if there are few unique values compared to total values
        length(unique(x)) / length(x) < max_proptortion_factor &&
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
      parse(   #df$cyl <- as.factor(df$cyl)
        text = paste0("df$", var, " <- as.factor(df$", var, ")")
      )
    )
  }
  return(df)

}


###################################################################
# Miscellaneous
###################################################################


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
    
    if(file.exists(html_file)) {
      return(html_file)       
    } else {
      return(-1)
    }
}

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
