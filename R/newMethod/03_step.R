
################################################################################
# MODEL TRAINING ##########################
################################################################################
# bm <- h2o.loadModel(file.path(this.path::this.dir(),"model/finalModel"))
# bm@parameters

source(file.path(this.path::this.dir(), "00_functions.R"))
#########################################################
######################## LOAD TRAINING ##################
#########################################################

message("#################################")

message(getwd())
if( !file.exists("DT.all.parquet")){
  stop("data frame for training not present, DT.all.rda not found - did you run the previous steps?")
}
library(xgboost)
library(arrow)
message("importing parquet")
DT.all <- arrow::read_parquet( "DT.all.parquet" )
message("imported parquet")
y <- "class"
x <- setdiff(colnames(DT.all), c(y, "latTile"))
params <- list(
  objective = "multi:softprob",
  num_class = nlevels(DT.all$class),
  eval_metric = c("merror","mlogloss"),
  max_depth = 9,
  verbosity = 2,
  learning_rate = 0.1,
  subsample = 0.8,
  colsample_bytree = 0.9,
  colsample_bylevel = 0.6,
  min_child_weight = 20,
  lambda = 100,
  device="cuda"
)
runCV <- function(){

  folds <- lapply(
    sort(unique(DT.all$latTile)),
    function(f) which(DT.all$latTile == f)
  )

  message(length(folds), " folds")

  message(nrow(DT.all), " rows to matrix")
  X <- as.matrix(DT.all[, ..x])
  y <- as.integer(DT.all$class)- 1L

  # table(DT.all$class)
  # table(y)

  message("XGB Matrix preparation")
  dtrain <- xgboost::xgb.DMatrix(
    X,
    label = y
  )

  message("CROSS valid start")
  cv <- xgb.cv(
    params = params,
    data = dtrain,
    nrounds = 600,
    folds = folds,
    early_stopping_rounds = 30
  )

  message("CROSS valid FINISHED")
  saveRDS(cv, file="xgb_cv_cuda.rds")
}

if(!file.exists("xgb_cv_cuda.rds")) runCV()

message("Loading CV")
cv <- readRDS("xgb_cv_cuda.rds")
best_nrounds <- cv$evaluation_log[
  which.min(cv$evaluation_log$test_merror_mean),
  iter
]

if(!file.exists("xgb_final.model")) {
  message("STARTING FINAL TRAINING")
  message("creating xgboost matrix")
  dtrain <- xgboost::xgb.DMatrix(
    as.matrix(DT.all[, ..x]),
    label = as.integer(DT.all$class)- 1L
  )
  message("starting training with ", best_nrounds, " rounds")
  final_model <- xgboost::xgb.train(
    params = params,
    data = dtrain,
    nrounds = best_nrounds,
    verbose = 2
  )
  message("finisched training with ", best_nrounds, " rounds")
  xgboost::xgb.save(
    final_model,
    "xgb_final.model"
  )
} else {
  final_model <- xgboost::xgb.load("xgb_final.model")
}
## predict
# class_levels <- levels(DT.all$class)
# pred_class <- max.col(prob_mat)
# pred_class <- factor(
#   class_levels[pred_class],
#   levels = class_levels
# )
