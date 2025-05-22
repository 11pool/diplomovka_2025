
### Mass data analysis

method_list <- list(
  ML = c('lda', 'qda', 'svm'),
  clust = c('kmeans', 'kmedoids', 'mclust'),
  method = c('pair', 'marginal', 'single'),
  step = 2,
  max_depth = 4
)

method_variable_list <- list(
  ML = c('lda', 'qda', 'svm'),
  clust = 'mclust',
  method = c('pair', 'marginal', 'single'),
  step = list(2:9),
  max_depth = 4
)


## MclustDA on land and MNIST
#mclustda<-MclustDA(data=mnist$test$images, class=mnist$test$labels)
summary(mclustda)
mclustda_predict<-predict(mclustda, newdata = mnist$test$images)
sum(diag(table(mclustda_predict$classification, mnist$test$labels)))/length(mnist$test$labels)

mclustda_land<-MclustDA(data=land[,-37], class=land[,37])
summary(mclustda_land)
mclustda_predict_land<-predict(mclustda_land, newdata = land_val[,-37])
sum(diag(table(mclustda_predict_land$classification, land_val[,37])))/length(land_val[,37])
#test_test<-Mclust(data=mnist$test$images[mnist$test$labels==4,],G=2:6)
summary(test_test)



####MNIST####
# mnist bad because cov singularities and other bads, pendigits too easy
mnist <- read_mnist()
pendigits <- read.csv("~/pendigits.tra", header=FALSE)



my_test <- diplomovka(data=mnist$test$images, lables=mnist$test$labels, DA=1)


lda(x=mnist$test$images, grouping = mnist$test$labels)
qda(x=mnist$test$images, grouping = mnist$test$labels)

mnist_test_norm <- mnist$test$images/255
lda(x=mnist_test_norm, grouping = mnist$test$labels)
qda(x=mnist_test_norm, grouping = mnist$test$labels)

mnist_test_norm  <- mnist_test_norm [, colSums(mnist_test_norm  != 0) > 0]
lda(x=mnist_test_norm, grouping = mnist$test$labels)
qda(x=mnist_test_norm, grouping = mnist$test$labels)
unique(mnist_test_norm[,585])

#### mnist_pca ####
## remove constant pixels and do pca

# cols_to_remove <- apply(mnist$train$images, 2, function(col) all(col == 0))
# mnist$train$images <- mnist$train$images[, !cols_to_remove]

#mnistpca_train <- prcomp(mnist$train$images, retx = TRUE, center = TRUE, scale. = TRUE)

sum(mnistpca_train$sdev[1:484])/sum(mnistpca_train$sdev)
# first over 90% alpha

mnistpca_wrapper <- function(ML, clust, method, step, max_depth){
  diplomovka_rewrite(mnistpca_train$x[,1:484], mnist$train$labels, ML, clust, method, step, max_depth)
}

# run data on main function through wrappers
mnistpca_diplomovka <- test_methods(mnist$train$labels, method_list, mnistpca_wrapper)

mnistpca_variable_diplomovka <- test_variable_methods(mnist$train$labels, method_variable_list, mnistpca_wrapper)

# mnist$test$images <- mnist$test$images[, !cols_to_remove]
validation_scaled <- scale(mnist$test$images, center = mnistpca_train$center, scale = mnistpca_train$scale)
mnistpca_val <- validation_scaled %*% mnistpca_train$rotation
mnistpca_val <- mnistpca_val[,1:484]
load("~/mnistpca.RData")
mnistpca_valid_uncut<-lapply(results, function(x) predict(x$model, mnistpca_val))

load("~/mnistpca_var.RData")
mnistpca_valid_uncut<-lapply(results, function(x) predict(x$model, mnistpca_val))

mnistpca_validation <- validate_methods(mnistpca_valid_uncut, mnist$test$labels, method_list)

mnistpca_variable_validation <- validate_variable_methods(mnistpca_valid_uncut, mnist$test$labels, method_variable_list)

#mnistpca_validation[mnistpca_validation[,'ML']=='lda',"Accuracy_innit"]<-sum(diag(table(predict(lda(mnistpca_train$x[,1:484], mnist$train$labels), mnistpca_val)$class, mnist$test$labels)))/length(mnist$test$labels)
#mnistpca_validation[mnistpca_validation[,'ML']=='svm',"Accuracy_innit"]<-sum(diag(table(predict(svm(mnistpca_train$x[,1:484], mnist$train$labels, type='C-classification', scale = FALSE, kernel='linear'), mnistpca_val), mnist$test$labels)))/length(mnist$test$labels)

####pen-based####
# too easy
pen_model <- diplomovka(pendigits[,-17], pendigits[,17], DA=1)
results <- predict(pen_model, pendigits[,-17])$class
confusion <-table(simplify_and_order(results, as.factor(pendigits[,17])), pendigits[,17])
confusion

pen_clustDA <- MclustDA(data=pendigits[,-17], class=pendigits[,17])
summary(pen_clustDA)
# classification error: 0.0084

pen_wrapper <- function(ML, clust, method, step, max_depth){
  diplomovka_rewrite(pendigits[,-17], pendigits[,17], ML, clust, method, step, max_depth)
}
pen_diplomovka <- test_methods(pendigits[,17], method_list, pen_wrapper)


#####wines####
# too easy
library(HDclassif)
data('wine')

wines_wrapper <- function(ML, clust, method, step, max_depth){
  diplomovka_rewrite(wine[,-1], wine[,1], ML, clust, method, step, max_depth)
}
wines_diplomovka <- test_methods(wine[,1], method_list, wines_wrapper)

####Land####
# same idea as mnist_pca
land <- read.table("~/sat.trn", quote="\"", comment.char="")
land_val <- read.table("~/sat.tst", quote="\"", comment.char="")
land_wrapper <- function(ML, clust, method, step, max_depth){
  diplomovka_rewrite(land[,-37], land[,37], ML, clust, method, step, max_depth)
}
land_diplomovka <- test_methods(land[,37], method_list, land_wrapper)


land_variable_diplomovka <- test_variable_methods(land[,37], method_variable_list, land_wrapper)

load("~/land.RData")
land_valid_uncut<-lapply(results, function(x) predict(x$model, land_val[,-37]))

load("~/land_var.RData")
land_valid_uncut<-lapply(results, function(x) predict(x$model, land_val[,-37]))

land_validation <- validate_methods(land_valid_uncut, land_val[,37], method_list)

land_variable_validation <- validate_variable_methods(land_valid_uncut, land_val[,37], method_variable_list)
#land_validation[land_validation[,'ML']=='lda',"Accuracy_innit"]<-sum(diag(table(predict(lda(land[,-37], land[,37]), land_val[,-37])$class, land_val[,37])))/length(land_val[,37])
#land_validation[land_validation[,'ML']=='svm',"Accuracy_innit"]<-sum(diag(table(predict(svm(land[,-37],land[,37], type='C-classification', scale = FALSE, kernel='linear'), land_val[,-37]), land_val[,37])))/length(land_val[,37])

land_variable_validation_cut <- land_variable_validation
land_variable_validation_cut[,"Accuracy_error"] <- rep(NA, 9)
land_variable_validation_cut[,"after/innit"] <- rep(NA, 9)

lapply(results, function(x) plot(MlClust_dendrogram(x, 'acc'), center=TRUE) )
qda_cut <- results[[2]]$fitted
qda_cut <- shear(qda_cut, '1_3')
qda_cut <- shear(qda_cut, '1_4')
qda_cut <- shear(qda_cut, '4_2_2_2')
qda_cut <- shear(qda_cut, '7_1_1')
sum(diag(table( simplify_and_order(predict(qda(land[,-37], qda_cut), land_val[,-37])$class, as.factor(land_val[,37])) , land_val[,37])))/length(land_val[,37])

# plot(sapply(tester_single$history, function(x) x$acc), type= 'l')


####sanity check####
set.seed(123)
sanity_norm_data<-function(rows,mu, sigma){
  row <- floor(rows/length(mu))
  dim <- length(mu[[1]])
  result <- data.frame(matrix(nrow=row*length(mu), ncol = dim+1))
  for (i in 1:length(mu)){
    result[(((i-1)*row)+1):(i*row),1:dim]<-mvrnorm(row, mu[[i]], sigma[[i]])
    result[(((i-1)*row)+1):(i*row),dim+1]<-names(mu)[i]
  }
  return(result)
}

sanity_norm_data_A <- sanity_norm_data(1000, list('1'=c(-4,0), '2'=c(0,0), '3'=c(4,0)), 
                                       list(matrix(c(1,0.5,0.5,1), 2), matrix(c(1,0,0,1),2), matrix(c(0.5,0.2,0.2, 2), 2)))
plot(sanity_norm_data_A[,1:2], col = c(rep('red',333), rep('blue', 333), rep('red',333)))


split <- sort(sample(1:nrow(sanity_norm_data_A), size = nrow(sanity_norm_data_A)*0.8))
anitsplit<-setdiff(1:nrow(sanity_norm_data_A), split)
plot(sanity_norm_data_A[split,1:2], col = c(rep('red',sum(sanity_norm_data_A[split,3]=='1')), rep('blue', sum(sanity_norm_data_A[split,3]=='2')), rep('red',sum(sanity_norm_data_A[split,3]=='3'))))
plot(sanity_norm_data_A[anitsplit,1:2], col = c(rep('red',sum(sanity_norm_data_A[anitsplit,3]=='1')), rep('blue', sum(sanity_norm_data_A[anitsplit,3]=='2')), rep('red',sum(sanity_norm_data_A[anitsplit,3]=='3'))))

sanity_norm_data_A[sanity_norm_data_A[,3]=='3',3] <- '1'
sanity_lda<-lda(sanity_norm_data_A[split,1:2], sanity_norm_data_A[split,3])

sanity_lda_pred<-predict(sanity_lda, sanity_norm_data_A[split,1:2])
table(sanity_lda_pred$class, sanity_norm_data_A[split,3])

sanity_lda_pred<-predict(sanity_lda, sanity_norm_data_A[anitsplit,1:2])
table(sanity_lda_pred$class, sanity_norm_data_A[anitsplit,3])

our_lda <- diplomovka_rewrite(sanity_norm_data_A[split,1:2], sanity_norm_data_A[split,3], 'lda', 'kmeans', 'pair', 2, 2)

sanity_lda_pred<-predict(our_lda$model, sanity_norm_data_A[anitsplit,1:2])
table(sanity_lda_pred$class, sanity_norm_data_A[anitsplit,3])
MlClust_conf_small(sanity_lda_pred$class, sanity_norm_data_A[anitsplit,3])
plot(MlClust_dendrogram(our_lda, 'acc'), center=TRUE)
plot(MlClust_dendrogram(our_lda, 'time'), center=TRUE)

plot_lda_boundary <- function(lda_model, predictors, true_labels, orig) {
  # Define grid for plotting
  x_min <- min(predictors[,1]) - 1
  x_max <- max(predictors[,1]) + 1
  y_min <- min(predictors[,2]) - 1
  y_max <- max(predictors[,2]) + 1
  
  x_seq <- seq(x_min, x_max, length.out = 200)
  y_seq <- seq(y_min, y_max, length.out = 200)
  
  grid <- expand.grid(X1 = x_seq, X2 = y_seq)
  grid <- as.data.frame(grid)
  colnames(grid) <- colnames(predictors)
  
  # Predict class probabilities on the grid
  grid$pred <- predict(lda_model, grid)$class
  for (i in 1:length(orig)){
    grid$pred <- as.vector(shear(grid$pred, orig[i]))
  }
  
  # Plot the data points
  plot(predictors, col = as.numeric(true_labels), pch = 19, xlab = names(predictors)[1], ylab = names(predictors)[2])
  
  # Add decision boundary
  contour(x_seq, y_seq, matrix(as.numeric(grid$pred), nrow = 200), levels = 1.5, add = TRUE, drawlabels = FALSE, col = "black", lwd = 2)
}
boundary <- function(model, data, class = NULL, predict_type = "class",
                     resolution = 100, showgrid = TRUE, orig,...) {
  
  if(!is.null(class)) cl <- data[,class] else cl <- 1
  data <- data[,1:2]
  k <- length(unique(cl))
  
  plot(data, col = as.integer(cl)+1L, pch = as.integer(cl)+1L, ...)
  
  # make grid
  r <- sapply(data, range, na.rm = TRUE)
  xs <- seq(r[1,1], r[2,1], length.out = resolution)
  ys <- seq(r[1,2], r[2,2], length.out = resolution)
  g <- cbind(rep(xs, each=resolution), rep(ys, time = resolution))
  colnames(g) <- colnames(r)
  g <- as.data.frame(g)
  
  ### guess how to get class labels from predict
  ### (unfortunately not very consistent between models)
  p <- predict(model, g, type = predict_type)
  if(is.list(p)) p <- p$class
  p <- as.factor(p)
  for (i in 1:length(orig)){
    p<-shear(p, orig[i])
  }
  
  if(showgrid) points(g, col = as.integer(p)+1L, pch = ".")
  
  z <- matrix(as.integer(p), nrow = resolution, byrow = TRUE)
  contour(xs, ys, z, add = TRUE, drawlabels = FALSE,
          lwd = 2, levels = (1:(k-1))+.5)
  
  invisible(z)
}
plot_lda_boundary(our_lda$model, sanity_norm_data_A[anitsplit,1:2], sanity_norm_data_A[anitsplit,3], orig=c('1', '2'))
boundary(our_lda$model, sanity_norm_data_A[split,], class='X3', main='lda', orig=c('1', '2'))



cesta <- file.path("C:", "diplo_obr", paste("main_idea", ".pdf", sep = ""));

# nastavenie veľkosti plátna a kódovania znakov (slovenských písmen)
pdf(file=cesta, width=(15.5*0.3937), height=(2*15.5*0.618*0.3937), encoding="CP1250.enc");
par(mfrow=c(2,1), mar=c(4.5,4.5,3,1)+.1, mgp = c(3.0, 1, 0))

plot(sanity_norm_data_A[anitsplit,1:2], col = c(rep('red',which(sanity_norm_data_A[anitsplit,3]=='2')[1]-1), rep('blue', sum(sanity_norm_data_A[anitsplit,3]=='2')), rep('red',sum(sanity_norm_data_A[anitsplit,3]=='1')-which(sanity_norm_data_A[anitsplit,3]=='2')[1])))
boundary(our_lda$model, sanity_norm_data_A[anitsplit,], class='X3', orig=c('1', '2'))

dev.off()

cesta <- file.path("C:", "diplo_obr", paste("dendr", ".pdf", sep = ""));

# nastavenie veľkosti plátna a kódovania znakov (slovenských písmen)
pdf(file=cesta, width=(15.5*0.3937), height=(15.5*0.618*0.3937), encoding="CP1250.enc");
par(mfrow=c(1,2), mar=c(4.5,4.5,3,1)+.1, mgp = c(3.0, 1, 0))

plot(MlClust_dendrogram(our_lda, 'time'), center=TRUE)
plot(MlClust_dendrogram(our_lda, 'acc'), center=TRUE)

dev.off()


# todo, data so 4 classes 1, 2, 1, 2
sanity_norm_data_B <- sanity_norm_data(1000, list('1'=c(-4,0), '2'=c(0,0), '3'=c(4,0), '4'=c(8,0)), 
                                       list(matrix(c(1,0.5,0.5,1), 2), matrix(c(1,0,0,1),2), matrix(c(0.5,0.2,0.2, 2), 2), matrix(c(2,0.4,0.4, 1), 2)))
plot(sanity_norm_data_B[,1:2], col = c(rep('red',250), rep('blue', 250), rep('red',250), rep('blue', 250)))

split <- sort(sample(1:nrow(sanity_norm_data_B), size = nrow(sanity_norm_data_B)*0.8))
anitsplit<-setdiff(1:nrow(sanity_norm_data_A), split)
plot(sanity_norm_data_B[split,1:2], col = c(rep('red',sum(sanity_norm_data_B[split,3]=='1')), rep('blue', sum(sanity_norm_data_B[split,3]=='2')), rep('red',sum(sanity_norm_data_B[split,3]=='3')), rep('blue',sum(sanity_norm_data_B[split,3]=='4'))))
plot(sanity_norm_data_B[anitsplit,1:2], col = c(rep('red',sum(sanity_norm_data_B[anitsplit,3]=='1')), rep('blue', sum(sanity_norm_data_B[anitsplit,3]=='2')), rep('red',sum(sanity_norm_data_B[anitsplit,3]=='3')), rep('blue',sum(sanity_norm_data_B[anitsplit,3]=='4'))))

sanity_norm_data_B[sanity_norm_data_B[,3]=='3',3] <- '1'
sanity_norm_data_B[sanity_norm_data_B[,3]=='4',3] <- '2'
sanity_qda<-qda(sanity_norm_data_B[split,1:2], sanity_norm_data_B[split,3])

sanity_qda_pred<-predict(sanity_qda, sanity_norm_data_B[split,1:2])
table(sanity_qda_pred$class, sanity_norm_data_B[split,3])

sanity_qda_pred<-predict(sanity_qda, sanity_norm_data_B[anitsplit,1:2])
table(sanity_qda_pred$class, sanity_norm_data_B[anitsplit,3])

plot_lda_boundary(sanity_qda, sanity_norm_data_B[anitsplit,1:2], sanity_norm_data_B[anitsplit,3], orig=c('1', '2'))
boundary(sanity_qda, sanity_norm_data_B[split,], class='X3', main='qda', orig=c('1', '2'))

our_qda <- diplomovka_rewrite(sanity_norm_data_B[split,1:2], sanity_norm_data_B[split,3], 'qda', 'kmeans', 'pair', 2, 5)

sanity_qda_pred<-predict(our_qda$model, sanity_norm_data_B[anitsplit,1:2])
table(sanity_qda_pred$class, sanity_norm_data_B[anitsplit,3])
MlClust_conf_small(sanity_qda_pred$class, sanity_norm_data_B[anitsplit,3])
plot(MlClust_dendrogram(our_qda, 'acc'), center=TRUE)



plot_lda_boundary(our_qda$model, sanity_norm_data_B[anitsplit,1:2], sanity_norm_data_B[anitsplit,3], orig=c('1', '2'))
boundary(our_qda$model, sanity_norm_data_B[split,], class='X3', main='qda', orig=c('1', '2'))


####cutting####
# manual cutting based on acc trees
#land: svm mclust marginal (best valid), svm kmedoids pair (best train),
      #qda kmedoids single (best val), qda kmedoids marginal (best train)
#MNIST: svm kmedoids pair (best valid), svm kmeans pair (second best train, ak vide cas)

result <- diplomovka_rewrite(land[,-37],land[,37],'qda', 'kmedoids', 'single',2, 4)
plot(MlClust_dendrogram(result, 'acc', first_bump = 0.04), center=TRUE)
sheared <- shear(result$fitted, '1_1') # 1_1, 1_2, 3_1, 5_2, 7_2_2
new <- qda(land[,-37], sheared)
pred <- predict(new, land_val[,-37])
sum(diag(table(simplify_and_order(pred$class, as.factor(land_val[,37])), land_val[,37])))/length(land_val[,37])
# 0.866

result <- diplomovka_rewrite(land[,-37],land[,37],'qda', 'kmedoids', 'marginal',2, 4)
plot(MlClust_dendrogram(result, 'acc', first_bump = 0.04), center=TRUE)
sheared <- shear(result$fitted, '1_1') # 1_1, 1_2, 3_1, 3_2, 7_2
new <- qda(land[,-37], sheared)
pred <- predict(new, land_val[,-37])
sum(diag(table(simplify_and_order(pred$class, as.factor(land_val[,37])), land_val[,37])))/length(land_val[,37])
# 0.8555


result <- diplomovka_rewrite(land[,-37],land[,37],'qda', 'kmedoids', 'pair',2, 4)
plot(MlClust_dendrogram(result, 'acc', first_bump = 0.04), center=TRUE)
sheared <- shear(result$fitted, '1_1') # 1_1, 1_2, 4_2, 5_1
new <- qda(land[,-37], sheared)
pred <- predict(new, land_val[,-37])
sum(diag(table(simplify_and_order(pred$class, as.factor(land_val[,37])), land_val[,37])))/length(land_val[,37])
# 0.8585

result <- diplomovka_rewrite(land[,-37],land[,37],'qda', 'kmeans', 'single',2, 4)
plot(MlClust_dendrogram(result, 'acc', first_bump = 0.04), center=TRUE)
sheared <- shear(result$fitted, '7_1') # 7_1, 7_2
new <- qda(land[,-37], sheared)
pred <- predict(new, land_val[,-37])
sum(diag(table(simplify_and_order(pred$class, as.factor(land_val[,37])), land_val[,37])))/length(land_val[,37])
# 0.846

result <- diplomovka_rewrite(land[,-37],land[,37],'qda', 'kmeans', 'marginal',2, 4)
plot(MlClust_dendrogram(result, 'acc', first_bump = 0.04), center=TRUE)
sheared <- shear(result$fitted, '5') # 5, 7, 3
new <- qda(land[,-37], sheared)
pred <- predict(new, land_val[,-37])
sum(diag(table(simplify_and_order(pred$class, as.factor(land_val[,37])), land_val[,37])))/length(land_val[,37])
# 0.8485


result <- diplomovka_rewrite(land[,-37],land[,37],'qda', 'kmeans', 'pair',2, 4)
plot(MlClust_dendrogram(result, 'acc', first_bump = 0.04), center=TRUE)
sheared <- shear(result$fitted, '4_2') # 4_2, 5, 7_2
new <- qda(land[,-37], sheared)
pred <- predict(new, land_val[,-37])
sum(diag(table(simplify_and_order(pred$class, as.factor(land_val[,37])), land_val[,37])))/length(land_val[,37])
# 0.851

result <- diplomovka_rewrite(land[,-37],land[,37],'qda', 'mclust', 'single',2, 4)
plot(MlClust_dendrogram(result, 'acc', first_bump = 0.04), center=TRUE)
sheared <- shear(result$fitted, '7_2') # 7_2
new <- qda(land[,-37], sheared)
pred <- predict(new, land_val[,-37])
sum(diag(table(simplify_and_order(pred$class, as.factor(land_val[,37])), land_val[,37])))/length(land_val[,37])
# 0.84

result <- diplomovka_rewrite(land[,-37],land[,37],'qda', 'mclust', 'marginal',2, 4)
plot(MlClust_dendrogram(result, 'acc', first_bump = 0.04), center=TRUE)
sheared <- shear(result$fitted, '7_2') #7_2
new <- qda(land[,-37], sheared)
pred <- predict(new, land_val[,-37])
sum(diag(table(simplify_and_order(pred$class, as.factor(land_val[,37])), land_val[,37])))/length(land_val[,37])
# 0.835


result <- diplomovka_rewrite(land[,-37],land[,37],'qda', 'mclust', 'pair',2, 4)
plot(MlClust_dendrogram(result, 'acc', first_bump = 0.04), center=TRUE)
sheared <- shear(result$fitted, '7_2') # 7_2
new <- qda(land[,-37], sheared)
pred <- predict(new, land_val[,-37])
sum(diag(table(simplify_and_order(pred$class, as.factor(land_val[,37])), land_val[,37])))/length(land_val[,37])
#0.8435

result <- diplomovka_rewrite(land[,-37],land[,37],'svm', 'mclust', 'marginal',2, 4)
plot(MlClust_dendrogram(result, 'acc', first_bump = 0.01), center=TRUE)
sheared <- shear(result$fitted, '7') # 7
new <- svm(x=land[,-37], y=sheared)
pred <- predict(new, land_val[,-37])
sum(diag(table(simplify_and_order(pred, as.factor(land_val[,37])), land_val[,37])))/length(land_val[,37])
# 0.8815


result <- diplomovka_rewrite(land[,-37],land[,37],'svm', 'mclust', 'single',2, 4)
plot(MlClust_dendrogram(result, 'acc', first_bump = 0.01), center=TRUE)
sheared <- shear(result$fitted, '4_2_1') # 4_2_1
new <- svm(x=land[,-37], y=sheared)
pred <- predict(new, land_val[,-37])
sum(diag(table(simplify_and_order(pred, as.factor(land_val[,37])), land_val[,37])))/length(land_val[,37])
# 0.8485

result <- diplomovka_rewrite(land[,-37],land[,37],'svm', 'mclust', 'pair',2, 4)
plot(MlClust_dendrogram(result, 'acc', first_bump = 0.02), center=TRUE)
sheared <- shear(result$fitted, '3') # 3, 4
new <- svm(x=land[,-37], y=sheared)
pred <- predict(new, land_val[,-37])
sum(diag(table(simplify_and_order(pred, as.factor(land_val[,37])), land_val[,37])))/length(land_val[,37])
# 0.884


result <- diplomovka_rewrite(land[,-37],land[,37],'svm', 'kmedoids', 'pair',2, 4)
plot(MlClust_dendrogram(result, 'acc', first_bump = 0.01), center=TRUE)
# nothing to shear
pred <- predict(result$model, land_val[,-37])
sum(diag(table(simplify_and_order(pred, as.factor(land_val[,37])), land_val[,37])))/length(land_val[,37])
# 0.8615


result <- diplomovka_rewrite(land[,-37],land[,37],'svm', 'kmedoids', 'single',2, 4)
plot(MlClust_dendrogram(result, 'acc', first_bump = 0.01), center=TRUE)
sheared <- shear(result$fitted, '3') # 3, 4
new <- svm(x=land[,-37], y=sheared)
pred <- predict(new, land_val[,-37])
sum(diag(table(simplify_and_order(pred, as.factor(land_val[,37])), land_val[,37])))/length(land_val[,37])
# mess

result <- diplomovka_rewrite(land[,-37],land[,37],'svm', 'kmedoids', 'marginal',2, 4)
plot(MlClust_dendrogram(result, 'acc', first_bump = 0.01), center=TRUE)
sheared <- shear(result$fitted, '3') # 3, 4
new <- svm(x=land[,-37], y=sheared)
pred <- predict(new, land_val[,-37])
sum(diag(table(simplify_and_order(pred, as.factor(land_val[,37])), land_val[,37])))/length(land_val[,37])


result <- diplomovka_rewrite(land[,-37],land[,37],'svm', 'kmeans', 'pair',2, 4)
plot(MlClust_dendrogram(result, 'acc', first_bump = 0.01), center=TRUE)
sheared <- shear(result$fitted, '3') # 3, 4
new <- svm(x=land[,-37], y=sheared)
pred <- predict(new, land_val[,-37])
sum(diag(table(simplify_and_order(pred, as.factor(land_val[,37])), land_val[,37])))/length(land_val[,37])



result <- diplomovka_rewrite(land[,-37],land[,37],'svm', 'kmeans', 'single',2, 4)
plot(MlClust_dendrogram(result, 'acc', first_bump = 0.01), center=TRUE)
sheared <- shear(result$fitted, '3') # 3, 4
new <- svm(x=land[,-37], y=sheared)
pred <- predict(new, land_val[,-37])
sum(diag(table(simplify_and_order(pred, as.factor(land_val[,37])), land_val[,37])))/length(land_val[,37])


result <- diplomovka_rewrite(land[,-37],land[,37],'svm', 'kmeans', 'marginal',2, 4)
plot(MlClust_dendrogram(result, 'acc', first_bump = 0.01), center=TRUE)
sheared <- shear(result$fitted, '3') # 3, 4
new <- svm(x=land[,-37], y=sheared)
pred <- predict(new, land_val[,-37])
sum(diag(table(simplify_and_order(pred, as.factor(land_val[,37])), land_val[,37])))/length(land_val[,37])



