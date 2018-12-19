# this install function is causing an error for a couple of packages 
# that i had to install manually 


install_if_missing_and_load <- function(package) {
    if (!require(package, character.only = TRUE)) {
        print(paste("[+] Installing:", package))
        install.packages(package)
    }
    library(package, character.only = TRUE)
}
