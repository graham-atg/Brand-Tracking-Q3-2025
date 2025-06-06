##############################################################################
#1 load_survey_data() takes the necessary parameters (API key, base URL, survey 
#ID, and output filename) and calls the other helper functions.
#2	load_libraries(): This function loads the required R packages:qualtRics, sjlabelled, tidyverse, janitor, and skimr.
#3	set_api_credentials(): This function sets the Qualtrics API credentials using the provided API key and base URL.
#4	fetch_survey_data(): This function fetches the survey data from Qualtrics using the provided survey ID.
#5	save_survey_responses(): This function saves the survey data as a CSV file with the given filename.
#6	read_csv_and_get_colnames(): This function reads the CSV file, gets the column names, and returns the column names and the raw data.
#7	main(): This is the main function that orchestrates the entire process. It calls the helper functions in the correct order to load the survey data, save it as a CSV file, and print the column names.
##############################################################################

load_survey_data <- function(api_key = NULL, base_url = NULL, surveyID) {
  # Load necessary libraries
  load_libraries <- function() {
    library(qualtRics)
    library(sjlabelled)
    library(tidyverse)
    library(janitor)
    library(skimr)
    library(magrittr)
  }
  
  # Set API credentials
  set_api_credentials <- function(api_key, base_url) {
    qualtrics_api_credentials(api_key = api_key,
                              base_url = base_url,
                              overwrite = TRUE,
                              install = TRUE)
  }
  
  # Fetch survey data
  fetch_survey_data <- function(surveyID) {
    tryCatch(
      {
        fetch_survey(surveyID = surveyID, 
                     #unanswer_recode = 0,
                     #unanswer_recode_multi = 0,
                     force_request = TRUE)
      },
      error = function(e) {
        message("Error fetching survey data: ", e$message)
        message("Please check your API key and base URL.")
        stop("Qualtrics API authentication error")
      }
    )
  }
  
  # Save survey responses as a CSV file
  save_survey_responses <- function(data, filename) {
    write.csv(data, file = filename, row.names = FALSE)
  }
  
  # Read CSV file and get column names
  read_csv_and_get_colnames <- function(filename, skip_rows = 3) {
    col_names <- colnames(readr::read_csv(filename, n_max = 0))
    raw_data <- readr::read_csv(filename, col_names = col_names, skip = skip_rows)
    list(col_names = col_names, raw_data = raw_data)
  }
  
  # Define columns to remove
  cols_to_remove <- c(
    "StartDate", "EndDate", "Status", "IPAddress", "Progress", "Duration (in seconds)",
    "Finished", "RecordedDate", "RecipientLastName", "RecipientFirstName", "RecipientEmail",
    "ExternalReference", "LocationLatitude", "LocationLongitude", "DistributionChannel",
    "UserLanguage", "Q_RecaptchaScore", "Q_RelevantIDDuplicate", "Q_RelevantIDDuplicateScore",
    "Q_RelevantIDFraudScore", "Q_RelevantIDLastStartDate", "SC0", "opp", "Q_TotalDuration",
    "RISN", "rid", "LS", "V", "CMRID", "Q_CHL", "Q_BallotBoxStuffing", "Redix", "tg", "wave",
    "transaction_id", "SVID", "PS", "ResponseID", "PID", "psid", "K2", "med", "orderNumber",
     "term", "pureSpectrumRedirectUrl", "pureSpectrumSignatureValue"
  )
  
  # Load API key and base URL from .Renviron file
  readRenviron("~/.Renviron")
  
  # Set API credentials
  if (is.null(api_key) || is.null(base_url)) {
    api_key <- Sys.getenv("QUALTRICS_API_KEY")
    base_url <- Sys.getenv("QUALTRICS_BASE_URL")
  }
  set_api_credentials(api_key, base_url)
  
  # Fetch survey data
  survey_data <- fetch_survey_data(surveyID)
  
  # Save survey responses as a CSV file
  save_survey_responses(survey_data, "datafile.csv")
  
  # Check overlap with actual column names in data file
  load_libraries()
  csv_info <- read_csv_and_get_colnames("datafile.csv")
  col_names <- csv_info$col_names
  cols_to_remove_id <- col_names %in% cols_to_remove
  cols_to_remove <- col_names[cols_to_remove_id]
  
  # Remove the unwanted Qualtrics columns
  raw_data <- csv_info$raw_data
  raw_data <- dplyr::select(raw_data, !all_of(cols_to_remove))
  
  # Initial cleaning: Convert column names to tidyverse style guide and remove empty rows and columns
  cleaned_data <- janitor::clean_names(raw_data)
  dplyr::glimpse(cleaned_data)
  
  # Define function to delete columns with substring and retain
  delete_columns_with_substring_and_retain <- function(df, substring_to_delete, substring_to_retain) {
    deleted_columns <- vector("character", length = 0)
    for (col in names(df)) {
      if (grepl(substring_to_delete, col) && !grepl(substring_to_retain, col)) {
        df <- df[, !(names(df) %in% col)]
        deleted_columns <- c(deleted_columns, col)
      }
    }
    cat("Deleted columns:", paste(deleted_columns, collapse = ", "), "\n")
    return(df)
  }
  
  # Delete columns with substring 'DO' and retain columns with 'DO_DO'
  cleaned_data <- delete_columns_with_substring_and_retain(cleaned_data, '_do_', '_do_do_')
  
  # Delete columns with substring 'ADO' and retain columns with 'ADO_ADO'
  cleaned_data <- delete_columns_with_substring_and_retain(cleaned_data, '_ado_', '_ado_ado_')
  
  return(cleaned_data)
}

# Example usage
cleaned_data <- load_survey_data(
  api_key = "7kgP8J0RqagC4VJYSuEog6jxzY015PUozbBgbgAt",
  base_url = "sjc1.qualtrics.com/",
  surveyID = "SV_e4mRrs250rX66aO"
)

print(cleaned_data)










