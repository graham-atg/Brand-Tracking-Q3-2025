#install_and_load_packages_from_file function reads a list of required packages
#from a text file specified by file_path, and then proceeds to install and load each of these packages.
#If a package is not already installed, it installs the package with dependencies.
#If there is an error while loading the package, it captures the error message. 
#The inputs for this function are file_path (the path to the text file containing the list of required packages).

#This function loads all packages needs for data analysis which are contained in a
# txt file.
# install_require_paclakge.R:
# Provide the file path to your "required_packages.txt" file
# install_packages_from_file("required_packages.txt")
install_and_load_packages_from_file <- function(file_path) {
  # Read the list of packages from the specified text file
  listOfPackages <- scan(file_path, character(), quote = "")
  
  # Install and load packages
  for (pkg in listOfPackages) {
    if (!pkg %in% installed.packages()) {
      install.packages(pkg, dependencies = TRUE)
    }
    library(pkg, character.only = TRUE)
  }
}

# Call the function with the file path
install_and_load_packages_from_file("required_packages.txt")

# revised install_and_load_packages.

#The revised version of install_and_load_packages_from_file includes error handling 
#using tryCatch to capture and display any errors that occur while loading the packages. 
#It prints the error message along with a notification for each loaded package or 
#encountered error during the package loading process.


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
install_and_load_packages_from_file("/Users/grahamalbert/git/Brand_Tracking_Combined_Q1_2024/Functions/required_packages.txt")

