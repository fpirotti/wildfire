
################################################################################
# MODEL TRAINING ##########################
################################################################################

source(file.path(this.path::this.dir(), "00_functions.R"))
#########################################################
######################## LOAD TRAINING ##################
#########################################################
if(!exists("DT.all") || !is.data.frame(DT.all)){
  stop("data frame for training not present, did you run the previous steps?")
}
library(h2o)

# Start H2O
h2o.init(
  nthreads = 150,
  max_mem_size = "500G"
)
# h2o.shutdown(F)
# Convert to H2OFrame
DT.all$macro.class<- NULL
hf <- as.h2o(DT.all)

y <- "class"
x <- setdiff(colnames(hf), c(y, "weight"))
hf[, y] <- h2o.asfactor(hf[, y])


automl <- h2o.automl(
  x = x,
  y = y,
  training_frame = hf,
    max_models = 20,              # or set max_runtime_secs
    seed = 123,
    include_algos = c("XGBoost"),
    balance_classes = TRUE,
    nfolds = 5,
    sort_metric = "mean_per_class_error",
    keep_cross_validation_predictions = TRUE
)

automl@leaderboard
# Best model
best_model <- automl@leader
h2o.saveModel(best_model, path = "model", export_cross_validation_predictions = T, filename = "finalModel")

cv_ids <- best_model@model$variable_importances

# Performance metrics
perf <- h2o.performance(best_model)
prettyPrint(perf, agg=T)
