

library(h2o)
library(data.table)
library(dplyr)
library(ROSE)
h2o.init()


creditcard <- data.table::fread("creditcard.csv/creditcard.csv")
# Vars <-  creditcard %>% dplyr::select( V17,V10,V11,V7,V3,V16,V12, V14,Class,Time,V5,V25)
Vars <-  creditcard 


data <- as.h2o(Vars)

split <- h2o.splitFrame(data,ratios = 0.8,seed = 123)


split_calib <- h2o.splitFrame(data,ratios = c(0.6,0.3),seed=123)
train <- split_calib[[1]]
test  <- split_calib[[2]]
calib <- split_calib[[3]]

# 
train$Class <- as.factor(train$Class)
test$Class <- as.factor(test$Class)
# 

x <- setdiff(names(train),"Class")
y <- "Class"


set.seed(123)
Train <- as.data.frame(train)
# Train$Class
Train_over <- ovun.sample(Class~.,Train ,method = "over")$data |>
  as.h2o()
Train_under <- ovun.sample(Class~.,Train,method = "under",p=0.3)$data |> as.h2o()
Train_both <- ovun.sample(Class~.,Train ,method = "both",p=0.3)$data |> as.h2o()


Train_over$Class <- as.factor(Train_over$Class)
Train_both$Class <- as.factor(Train_both$Class)

# ------------------------------------------------------------------------------------------------------------------------------------------------------


# -------------------------------------------------------------------------------------------------------------------------------------------------------

























auto_both <- h2o.automl(
  x = x,
  y = y,
  training_frame = Train_both,
  validation_frame = test,
  leaderboard_frame = test,  # Utilisation de test pour le leaderboard
  nfolds = 0,
  seed = 123,
  max_models = 100,
  project_name = "vars_both",
  keep_cross_validation_predictions = TRUE,
  keep_cross_validation_models = TRUE, # Pour économiser de la mémoire
  keep_cross_validation_fold_assignment = TRUE, # Pas nécessaire si on ne stacke pas
  # exclude_algos = c("DeepLearning"),  # On exclut DeepLearning pour éviter des temps longs
  include_algos = c("GBM", "GLM", "StackedEnsemble","DRF"), # S'assurer que les modèles essentiels sont inclus
  sort_metric = "AUCPR",
  max_runtime_secs = 5000,
  max_runtime_secs_per_model = 300  # Limite pour chaque modèle
  # stopping_metric = "AUCPR",
  # stopping_tolerance = 0.001,  # Arrêt si l'amélioration est minime
  # stopping_rounds = 5
)


auto_over <- h2o.automl(
  x = x,
  y = y,
  training_frame = Train_over,
  validation_frame = test,
  leaderboard_frame = test,  # Utilisation de test pour le leaderboard
  nfolds = 0,
  seed = 123,
  max_models = 200,
  project_name = "vars_over",
  keep_cross_validation_predictions = TRUE,
  keep_cross_validation_models = TRUE, # Pour économiser de la mémoire
  keep_cross_validation_fold_assignment = TRUE, # Pas nécessaire si on ne stacke pas
  # exclude_algos = c("DeepLearning"),  # On exclut DeepLearning pour éviter des temps longs
  include_algos = c("GBM", "GLM", "StackedEnsemble","DRF"), # S'assurer que les modèles essentiels sont inclus
  sort_metric = "AUCPR",
  max_runtime_secs = 6000,
  max_runtime_secs_per_model = 250  # Limite pour chaque modèle
  # stopping_metric = "AUCPR",
  # stopping_tolerance = 0.001,  # Arrêt si l'amélioration est minime
  # stopping_rounds = 5
)






auto_none <- h2o.automl(
  x = x,
  y = y,
  balance_classes = TRUE,
  training_frame = train,
  validation_frame = test,
  leaderboard_frame = test,  # Utilisation de test pour le leaderboard
  nfolds = 0,
  seed = 123,
  max_models = 200,
  project_name = "vars_none",
  keep_cross_validation_predictions = TRUE,
  keep_cross_validation_models = TRUE, # Pour économiser de la mémoire
  keep_cross_validation_fold_assignment = TRUE, # Pas nécessaire si on ne stacke pas
  # exclude_algos = c("DeepLearning"),  # On exclut DeepLearning pour éviter des temps longs
  # include_algos = c("GBM", "GLM", "StackedEnsemble","DRF"), # S'assurer que les modèles essentiels sont inclus
  sort_metric = "AUCPR",
  max_runtime_secs = 6000,
  max_runtime_secs_per_model = 240  # Limite pour chaque modèle
  # stopping_metric = "AUCPR",
  # stopping_tolerance = 0.001,  # Arrêt si l'amélioration est minime
  # stopping_rounds = 5
)



best_glm_both_auto<- h2o.get_best_model(auto_over,"glm",criterion = "aucpr")
#"GLM_1_AutoML_13_20250314_180418"
four_best_model_both_id <- auto_both@leaderboard$model_id[1:4]

# GBM_grid_1_AutoML_12_20250314_172420_model_21
# 2  GBM_grid_1_AutoML_12_20250314_172420_model_5
# 3 GBM_grid_1_AutoML_12_20250314_172420_model_90
# 4 GBM_grid_1_AutoML_12_20250314_172420_model_51

best_both1 <- h2o.getModel("GBM_grid_1_AutoML_12_20250314_172420_model_21")
best_both2 <- h2o.getModel("GBM_grid_1_AutoML_12_20250314_172420_model_5") #fixette
best_both3 <- h2o.getModel("GBM_grid_1_AutoML_12_20250314_172420_model_90") # fixete
best_both4 <- h2o.getModel("GBM_grid_1_AutoML_12_20250314_172420_model_51")

path <- "C:/TER2/TER/best_model/vars/both"


h2o.saveModel(best_both1)
h2o.saveModel(best_both2)
h2o.saveModel(best_both3)
h2o.saveModel(best_both4)

four_best_model_over_id <- auto_over@leaderboard$model_id[1:4]

### sortie 
#  GBM_grid_1_AutoML_13_20250314_180418_model_89
# 2 GBM_grid_1_AutoML_13_20250314_180418_model_149
# 3   GBM_grid_1_AutoML_13_20250314_180418_model_2
# 4 GBM_grid_1_AutoML_13_20250314_180418_model_113

best_over1 <- h2o.getModel("GBM_grid_1_AutoML_13_20250314_180418_model_89")
best_over2 <- h2o.getModel("GBM_grid_1_AutoML_13_20250314_180418_model_149")
best_over3 <- h2o.getModel("GBM_grid_1_AutoML_13_20250314_180418_model_2") #fixette
best_over4 <- h2o.getModel("GBM_grid_1_AutoML_13_20250314_180418_model_113")
 
path <- "C:/TER2/TER/best_model/vars/over"

h2o.saveModel(best_over1)
h2o.saveModel(best_over2)
h2o.saveModel(best_over3)
h2o.saveModel(best_OVER4)

four_best_model_none_id <- auto_none@leaderboard$model_id[1:4]
# GBM_grid_1_AutoML_14_20250314_195334_model_40
# 2 GBM_grid_1_AutoML_14_20250314_195334_model_26
# 3 GBM_grid_1_AutoML_14_20250314_195334_model_73
# 4 GBM_grid_1_AutoML_14_20250314_195334_model_59

best_none1 <- h2o.getModel("GBM_grid_1_AutoML_14_20250314_195334_model_40") # fixette 
best_none2 <- h2o.getModel("GBM_grid_1_AutoML_14_20250314_195334_model_26")
best_none3 <- h2o.getModel("GBM_grid_1_AutoML_14_20250314_195334_model_73")
best_none4 <- h2o.getModel("GBM_grid_1_AutoML_14_20250314_195334_model_59")


path <- "C:/TER2/TER/best_model/vars/none"
h2o.saveModel(best_none1,path = path)
h2o.saveModel(best_none2,path = path)
h2o.saveModel(best_none3,path = path)
h2o.saveModel(best_none4,path = path)


models <- c(best_none1, best_over3, best_both2, best_both3)

performance_all_model(best_none1,1.219807e-03,"both") |>  pander::pander()
h2o.confusionMatrix(best_none1,test,threshold=1.219807e-03)
View_perf(best_none1,test)
