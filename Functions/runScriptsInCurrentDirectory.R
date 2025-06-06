# The runScriptsInDirectory function executes all the R scripts present in a specified directory.
# If no directory is specified, it defaults to the 'Functions' directory in the current working directory.
# It also takes an optional argument skipScripts as a character vector, which allows specific scripts to be skipped during execution.
# The function attempts to run each script with a 15-second timeout using tryCatch.
# If a script exceeds the timeout, it prints an error message indicating that the script was not executed.
# The function also provides status updates during script execution.

setwd("/Users/grahamalbert/git/Brand_Tracking_Combined_Q1_2024/")

runScriptsInCurrentDirectory <- function(directory = "Functions", skipScripts = character(0)) {
  # Get the list of R script files in the specified directory
  scriptFiles <- list.files(directory, pattern = ".R$", full.names = TRUE)
  
  # Check if there are any script files
  if (length(scriptFiles) == 0) {
    cat(paste("No R script files found in the '", directory, "' directory.\n", sep = ""))
    return(NULL)
  }
  
  # Iterate through the script files and run each one
  for (scriptFile in scriptFiles) {
    # Check if the script should be skipped
    if (basename(scriptFile) %in% skipScripts) {
      cat("Skipping script:", scriptFile, "\n")
      next
    }
    
    cat("Running script:", scriptFile, "\n")
    
    # Use tryCatch to run the script with a 15-second timeout
    tryCatch({
      system.time({
        source(scriptFile, local = TRUE)
      })$elapsed
    }, error = function(e) {
      cat("Script", scriptFile, "was not run (timed out).\n")
    })
    
    cat("Script", scriptFile, "completed.\n")
  }
}


# Specify the scripts to skip (replace with your script filenames)
scriptsToSkip <- c("create_summary_table.R","runScriptsInCurrentDirectory.R")

# Call the function to run scripts in the current directory, skipping specified scripts
runScriptsInCurrentDirectory(directory = "Functions",skipScripts = scriptsToSkip)
