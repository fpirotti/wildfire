library(reticulate)
use_virtualenv("/opt/python-envs/torch-env", required = TRUE)
torch <- import("torch")
torch$cuda$is_available()



