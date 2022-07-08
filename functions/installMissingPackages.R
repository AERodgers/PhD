# author: "Antoin Eoin Rodgers"
# date: '2022-05-06'
# Bank of personal functions

###  Install Missing Packages ##################################################
installMissingPackages <- function (package_list)
{
  # installs packages from  package_list which are not already installed in.
  installed_packages <-
    package_list %in% rownames(installed.packages())

  if (any(installed_packages == FALSE))
  {
    install.packages(package_list[!installed_packages])
  }

  # Packages loading
  invisible(lapply(
    package_list,
    suppressPackageStartupMessages(library),
    character.only = TRUE
  ))

}
