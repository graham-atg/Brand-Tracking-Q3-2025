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




# Define function to load survey data
load_survey_data <- function(api_key, base_url, surveyID, filename) {
  
  # Load necessary libraries
  load_libraries <- function() {
    library(qualtRics)
    library(sjlabelled)
    library(tidyverse)
    library(janitor)
    library(skimr)
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
    fetch_survey(surveyID = surveyID, force_request = TRUE)
  }
  
  # Save survey responses as a CSV file
  save_survey_responses <- function(data, filename) {
    write.csv(data, file = filename, row.names = FALSE)
  }
  
  # Read CSV file and get column names
  read_csv_and_get_colnames <- function(filename, skip_rows = 3) {
    col_names <- colnames(read_csv(filename, n_max = 0))
    raw_data <- read_csv(filename, col_names = col_names, skip = skip_rows)
    list(col_names = col_names, raw_data = raw_data)
  }
  
  # Define columns to remove
  cols_to_remove <- c("StartDate",
                      "EndDate",
                      "Status",
                      "IPAddress",
                      "Progress",
                      "Duration (in seconds)",
                      "Finished",
                      "RecordedDate",
                      "RecipientLastName",
                      "RecipientFirstName",
                      "RecipientEmail",
                      "ExternalReference",
                      "LocationLatitude",
                      "LocationLongitude",
                      "DistributionChannel",
                      "UserLanguage",
                      "Q_RecaptchaScore",
                      "Q_RelevantIDDuplicate",
                      "Q_RelevantIDDuplicateScore",
                      "Q_RelevantIDFraudScore",
                      "Q_RelevantIDLastStartDate",
                      "SC0",
                      "opp",
                      "Q_TotalDuration",
                      "RISN",
                      "rid",
                      "LS",
                      "V",
                      "CMRID",
                      "Q_CHL",
                      "Q_BallotBoxStuffing",
                      "Redix",
                      "tg",
                      "wave",
                      "transaction_id",
                      "SVID",
                      "PS",
                      "ResponseID",
                      "PID",
                      "psid",
                      "K2",
                      "med",
                      "orderNumber",
                      "gc",
                      "term",
                      "pureSpectrumRedirectUrl",
                      "pureSpectrumSignatureValue")
  
  # Check overlap with actual column names in data file
  csv_info <- read_csv_and_get_colnames(filename)
  col_names <- csv_info$col_names
  cols_to_remove_id <- col_names %in% cols_to_remove
  cols_to_remove <- col_names[cols_to_remove_id]
  
  # Remove the unwanted Qualtrics columns
  filename <- csv_info$raw_data %>%
    dplyr::select(!all_of(cols_to_remove))
  
  # Initial cleaning: Convert column names to tidyverse style guide and remove empty rows and columns
  filename <- filename %>%
    clean_names()
  
  glimpse(filename)
  
  # Define function to delete columns with substring and retain
  delete_columns_with_substring_and_retain <- function(df, substring_to_delete, substring_to_retain) {
    # Initialize an empty list to store the deleted column names
    deleted_columns <- vector("character", length = 0)
    
    # Loop through columns in the dataframe
    for (col in names(df)) {
      # Check if the column contains the substring to delete
      if (grepl(substring_to_delete, col) && !grepl(substring_to_retain, col)) {
        # Delete the column from the dataframe
        df <- df[, !(names(df) %in% col)]
        # Add the deleted column name to the list
        deleted_columns <- c(deleted_columns, col)
      }
    }
    
    # Print the list of deleted columns
    cat("Deleted columns:", paste(deleted_columns, collapse = ", "), "\n")
    
    # Return the modified dataframe
    return(df)
  }
  
  # Delete columns with substring 'DO' and retain columns with 'DO_DO'
  filename <- delete_columns_with_substring_and_retain(filename, '_do_', '_do_do_')
  
  # Delete columns with substring 'ADO' and retain columns with 'ADO_ADO'
  filename <- delete_columns_with_substring_and_retain(filename, '_ado_', '_ado_ado_')
  
  return(filename)
}

# Main function to orchestrate the process
main <- function(api_key, base_url, surveyID, filename) {
  load_libraries()
  set_api_credentials(api_key, base_url)
  survey_data <- fetch_survey_data(surveyID)
  save_survey_responses(survey_data, filename)
  csv_info <- read_csv_and_get_colnames(filename)
  cleaned_data <- load_survey_data(api_key, base_url, surveyID, filename)
  return(cleaned_data)
}



# Example usage of the load_survey_data pipeline
load_survey_data(api_key = "7kgP8J0RqagC4VJYSuEog6jxzY015PUozbBgbgA",
                 base_url = "sjc1.qualtrics.com/",
                 surveyID = "SV_e4mRrs250rX66aO",
                 filename = "DATAFILE.csv")