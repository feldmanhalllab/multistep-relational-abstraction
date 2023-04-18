if (!require(devtools, quietly = TRUE)) install.packages("devtools")

library(devtools)

if (
  !require(tidyverse, quietly = TRUE) || packageVersion("tidyverse") < "1.3.2"
) install_version(package = "tidyverse", version = "1.3.2")

if (
  !require(here, quietly = TRUE) || packageVersion("here") < "1.0.1"
) install_version(package = "here", version = "1.0.1")

if (
  !require(tictoc, quietly = TRUE) || packageVersion("tictoc") < "1.0.1"
) install_version(package = "tictoc", version = "1.0.1")

if (
  !require(Cairo, quietly = TRUE) || packageVersion("Cairo") < "1.6.0"
) install_version(package = "Cairo", version = "1.6.0")