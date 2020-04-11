# NOT RUN - Anomaly detection ---------------------------------------------
#
# SR_anomaly_detection <- function() {
  # ## Run H2O model
  # p_load(h2o)
  # localH2O <- h2o.init(max_mem_size = "10g", nthreads = -1)
  # train <- as.h2o(df[, 2:12], destination_frame = "train.hex")
  # model <- h2o.deeplearning(x = names(train), training_frame = train, autoencoder = TRUE,
  #                           reproducible = TRUE, seed = 1234, hidden = c(10, 10), epochs = 10)
  # h2o.shutdown(prompt = FALSE)
  # # interesting per feature error scores
  # anom_per_feature <- as.data.frame(h2o.anomaly(model, train, per_feature = TRUE))
  # anom <- as.data.frame(h2o.anomaly(model, train, per_feature = FALSE))
  # anomalies <- cbind(anom, anom_per_feature)
  # rm(anom, anom_per_feature)
  # anomalies <- anomalies[order(-anomalies$Reconstruction.MSE), ]; View(anomalies)
  # temp <- anomalies[1,] / anomalies %>% summarise_all(mean)
  # temp <- as.data.frame(t(temp))
  # temp$variable <- gsub("reconstr_", "", rownames(temp))
  # temp$variable <- gsub("Reconstruction", "", temp$variable)
  # temp$variable <- gsub(".MSE", "", temp$variable)
  # temp$variable <- gsub(".SE", "", temp$variable)
  # ggplot(temp, aes(x = variable, y = temp[, 1])) + geom_bar(stat = "identity", position = "dodge")
  # rm(anomalies, temp, localH2O, model, train)
  #
  #
  #
  # ## Run unsupervised RF model
  # p_load(randomForest)
  # rf <- randomForest(temp2, proximity = TRUE)
  # varImpPlot(rf, sort = TRUE, scale = TRUE) # Importance of each predictor
  #
  # # Show outliers
  # prox <- as.data.frame(rf$proximity)
  # prox$mean <- rowMeans(prox)
  # prox <- prox[order(prox$mean), ]
  # plot(prox$mean)
  # temp[rownames(prox)[1:2], ]
  # rm(prox)
  #
  #
  #
  ## AnomalyDetection
  # devtools::install_github("twitter/AnomalyDetection")
  # p_load(AnomalyDetection)
  #
  #
  #
  # ## outlier detection with k-NN
  # http://www.rdatamining.com/examples/outlier-detection
  #
  #
  #
  # Anomyly Detection in H2O
  # http://learn.h2o.ai/content/hands-on_training/anomaly_detection.html
  # => plot "the good, the bad (median), and the ugly"!!!
  # ae_model <- h2o.deeplearning(x=predictors,
  #                              y=42, #response (ignored - pick any non-constant column)
  #                              data=train_hex,
  #                              activation="Tanh",
  #                              autoencoder=TRUE,
  #                              hidden=c(50),
  #                              ignore_const_cols=FALSE,
  #                              epochs=1)
  # test_rec_error <- as.data.frame(h2o.anomaly(test_hex, ae_model))
  # test_features_deep <- h2o.deepfeatures(test_hex, ae_model, layer=1); summary(test_features_deep)

  # ## Outlier detection: MARS and trim the data with high leverage (Cook's D)
  #   - trimmen
  #   - use statistical model to predict values of outlier observation and after that we can impute it with predicted values.
# }
