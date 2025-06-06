# Call the function with the file path
install_and_load_packages_from_file("required_packages.txt")

# revised install_and_load_packages.

# install_and_load_packages_from_file.R
install_and_load_packages_from_file <- function(file_path) {
  # Read the list of packages from the specified text file
  listOfPackages <- scan(file_path, character(), quote = "")
  
  # Install and load packages
  for (pkg in listOfPackages) {
    if (!pkg %in% installed.packages()) {
      install.packages(pkg, dependencies = TRUE)
    }
    tryCatch({
      library(pkg, character.only = TRUE)
      cat(paste("Loaded package: ", pkg, "\n"))
    }, error = function(e) {
      cat(paste("Error loading package: ", pkg, "\n"))
      cat(paste("Error message: ", conditionMessage(e), "\n"))
    })
  }
}

# Call the function with the file path
install_and_load_packages_from_file("~/git/Brand_Tracking_Combined_Q4_2023/Functions/required_packages.txt")

