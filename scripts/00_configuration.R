# Install tidyverse if you haven't already: install.packages("tidyverse")
library(tidyverse)
library(gridExtra)

# Most University/Business accounts use "OneDriveCommercial" 
# or "OneDrive". 
# This logic works for both computers I am using - private and work computer.
onedrive_path <- Sys.getenv("OneDriveCommercial")
if (onedrive_path == "") onedrive_path <- Sys.getenv("OneDrive")
