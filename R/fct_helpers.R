###################################################
# RTutor.AI, a Shiny app for chating with your data
# Author: Xijin Ge    gexijin@gmail.com
# Dec. 6-12, 2022.
# No warranty and not for commercial use.
###################################################



###################################################
# Global variables
###################################################

release <- "0.98" # RTutor
uploaded_data <- "User Upload" # used for drop down
no_data <- "no_data" # no data is uploaded or selected
names(no_data) <- "No data (examples)"
rna_seq <- "rna_seq"  # RNA-Seq read counts
names(rna_seq) <- "RNA-Seq"
min_query_length <- 6  # minimum # of characters
max_query_length <- 2000 # max # of characters
language_models <- c("gpt-4-turbo", "gpt-4o", "gpt-4-1106-preview", "gpt-3.5-turbo", "gpt-3.5-turbo-16k", "gpt-3.5-turbo-0301", "gpt-4", "gpt-4-0314", "text-davinci-003")
names(language_models) <- c("GPT-4 Turbo", "GPT-4o", "GPT-4 Turbo (11/23)", "ChatGPT", "ChatGPT 16k", "ChatGPT (03/23)", "GPT-4", "GPT-4 (03/23)", "Davinci")
default_model <- "GPT-4 Turbo" #"GPT-4 Turbo (11/23)" # "GPT-4o"  # "ChatGPT" #   "GPT-4 (03/23)"
max_content_length <- 3000 # max tokens:  Change according to model !!!!
max_content_length_ask <- 3000 # max tokens:  Change according to model !!!!
default_temperature <- 0.2
pre_text <- "Write correct, efficient R code to answer this prompt:"
pre_text_python <- "Write correct, efficient Python code."
after_text <- "Use the df data frame."
max_char_question <- 1000 # max n. of characters in the Q&A
max_eda_levels <- 12 # max number of levels in categorical varaible for EDA, ggairs
max_eda_var <- 20 # maximum num of variables in EDA
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
growth_instruct <- "Calculate growth rate! Ensure all date and time manipulations are dynamically handled based on the data. Try using dplyr::lag() for 
time comparisons. Ensure to include a fair comparison period of equal length. Do not use print() to display tables."
decline_instruct <- "Calculate decline rate! Ensure all date and time manipulations are dynamically handled based on the data. Try using dplyr::lag() for 
time comparisons. Ensure to include a fair comparison period of equal length. Do not use print() to display tables."
forecast_instruct <- "Ensure the output includes a table of forecasted sales with confidence intervals, and validate the forecast accuracy against 
historical data. Plot the time progression, doesn't have to be ggplot2. Do not use print() to display tables."
compare_instruct <- "Compare the specified metrics to identify differences/similarities. Focus on key metrics like performance indicators. Highlight 
significant variations."
performance_instruct <- "Analyze performance metrics for the specified period. Include key performance indicators (KPIs). Compare current performance to 
historical data to identify trends."
trend_instruct <- "Identify and analyze trends over the specified timeframe. Use time series analysis to detect patterns and changes in the data. 
Highlight upward or downward trends."
m_over_m_instruct <- "Conduct month-over-month analysis comparing the data between consecutive months. Calculate percentage changes and identify 
significant increases or decreases. Highlight any recurring monthly patterns or anomalies."
y_over_y_instruct <- "Conduct year-over-year analysis comparing the data from the same period in different years. Calculate growth rate and 
percentage changes. Highlight any significant long-term trends/shifts in the data."

system_relevancy <- "Act as an experienced data analyst. You will determine two things. Decide if the following prompt: 1. is relevant to the 
 current data and 2. is a follow-up inquiry to the previous one (including modifications to visualizations, follow-up analysis, etc.)."
user_relevancy <- "If either 1. or 2. is true, respond with only 'True'. If the prompt is asking about something not in the current data AND isn't a 
 follow-up or modification to visualizations, respond with only 'False'. "

system_role_tutor <- "Act as a professor of statistics, computer science and mathematics. 
You will respond like answering questions by students. If the question is in languages other than English, respond in that language. 
If the question is not remotely related to your expertise, respond with 'No comment'."
# system_role_growth <- "If 'growth' or 'decline' is in the prompt, CALCULATE growth rate! Ensure all date and time manipulations are 
# dynamically handled based on the data. Try using dplyr::lag() for time comparisons. Ensure to include a fair comparison period of equal length."

# voice input parameters
wake_word <- "Tutor" #Tutor, Emma, Note that "Hey Cox" does not work very well.
# this triggers the submit button
action_verbs <- c(
  "now",
  "over",
  "do it",
  "do it now",
  "go ahead",
  "submit",
  "what are you waiting for"
)

#RMarkdown file's Header for knit python chunks
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

# if this file exists, running on the server. Otherwise local.
# this is used to change app behavior.
on_server <- "on_server.txt"


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

# load meta data, from JSON file
meta_data <- function() {
  readr::read_file(paste0(data_path, "metadata.json"))
}
# load meta data, from csv file
meta_data_csv <- function() {
  read.csv(paste0(data_path, "hmcl_metadata.csv"))
}


#' Move an element to the front of a vector
#'
#' The response from GPT3 sometimes contains strings that are not R commands.
#'
#' @param v is the vector
#' @param e is the element
#'
#' @return Returns a reordered vector
move_front <- function(v, e){
  ix <- which(v == e)

  # if found, move to the beginning.
  if(length(ix) != 0) {
    v <- v[-ix]
    v <- c(e, v)
  }
  return(v)
}


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



###################################################################
# Prepare data
###################################################################

# A file, demo requests for different datasets, demo questions
demo <- read.csv(app_sys("app", "www", "demo_questions.csv"))

# extract demo questions
ix <- which(demo$data == "questions")
demo_questions <- demo$requests[ix]
names(demo_questions) <- demo$name[ix]

# extract jokes
jokes <- demo[
  which(demo$data == "jokes"),
  "requests"
]

# prepare a list of available data sets that are built-in
datasets <- data()$results[, 3] # name of datasets
datasets <- gsub(" .*", "", datasets)

datasets <- sort(datasets)

# if dataset is not data frame or matrix, remove.
ix <- sapply(
  datasets, 
  function(x) {
    eval(
      parse(
        text = paste(
          "is.data.frame(",
          x,
          ") | is.matrix(",
           x, 
           ")"
        )
      )
    )
  }
)
datasets <- datasets[which(ix)]

datasets <- move_front(datasets, "state.x77")
datasets <- move_front(datasets, "iris")
datasets <- move_front(datasets, "mtcars")


# append a dummy value, used when user upload their data.
datasets <- c(datasets, uploaded_data)
# move it to 2nd place
datasets <- move_front(datasets, uploaded_data)

# append a dummy value, used when user do not use any data
datasets <- c(datasets, rna_seq)
# move it to 2nd place
datasets <- move_front(datasets, rna_seq)

# append a dummy value, used when user do not use any data
datasets <- c(datasets, no_data)
# move it to 2nd place
datasets <- move_front(datasets, no_data)

datasets <- move_front(datasets, "diamonds")
# default
datasets <- move_front(datasets, "mpg")

datasets <- setNames(datasets, datasets)

names(datasets)[match("mpg", datasets)] <- "mpg (examples)"
names(datasets)[match("diamonds", datasets)] <- "diamonds (examples)"
names(datasets)[match(rna_seq, datasets)] <- "RNA-Seq (examples)"

colnames(mpg) <- c("maker", "model", "dis", "year", "cylinder", 
  "transmission", "drive", "city", "highway", "fuel", "type")

#' Clean up API key character
#'
#' The response from GPT3 sometimes contains strings that are not R commands.
#'
#' @param api_key is a character string
#'
#' @return Returns a string with api key.
clean_api_key <- function(api_key) {
  # remove spaces
  api_key <- gsub(" ", "", api_key)
  return(api_key)
}


#' Validate API key character
#'
#' The response from GPT3 sometimes contains strings that are not R commands.
#'
#' @param api_key is a character string
#'
#' @return Returns TRUE or FALSE
validate_api_key <- function(api_key) {
  valid <- TRUE
  # if 51 characters, use the one in the file
  if (nchar(api_key) != 51) {
    valid <- FALSE
  }
  return(valid)
}


# get API key from environment variable.
api_key_global <- Sys.getenv("OPEN_API_KEY")
key_source <- "from OS environment variable."

# If there is an key file in the current folder, use that instead.
if (file.exists(file.path(getwd(), "api_key.txt"))) {
  api_key_file <- readLines(file.path(getwd(), "api_key.txt"))
  api_key <- clean_api_key(api_key_file)

  # if valid, replace with file
  if(validate_api_key(api_key_file)) {
    api_key_global <- api_key_file
    key_source <- "from file."
  }
}


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


#' Generate html file from Python code
#' 
#'
#' @param python_code, a chunk of code 
#' @param html_file file name for output
#' @param select_data input$select data
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


# Create list of available datasets to print on sidebar
available_datasets <- list(
  "Select a dataset:" = NULL,
  "Sales Data" = "Sales_Masked_Rtutor.rds",
  "Registration Data" = "Vahan_Masked_Rtutor.rds",
  "Dispatch Data" = "Dispatch_Masked_Rtutor.rds"
)

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

