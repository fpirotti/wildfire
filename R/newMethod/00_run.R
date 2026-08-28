message("Starting processing")
source(file.path(this.path::this.dir(), "00_functions.R"))


message("step 01")
source("01_step.R")
## ORGANIZES TRAINING AND CREATES MODEL ----
message("step 02")
# source("02_step.R")
## CREATES MODEL  ----
message("step 03")
# source("03_step.R")
## CREATES FUEL MAP  ----
message("step 04")
 source("04_step.R")
## CREATES FUEL MAP  ----
message("step 05")
# source("05_step_validation_and_consensus.R")


