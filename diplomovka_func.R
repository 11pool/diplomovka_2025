## Libraries for all scripts
library(dslabs)
library(mclust)
library(MASS)
library(stats)
library(cluster)
library(stringr)
library(e1071)
library(dendextend)

library(purrr)
# load("~/diplomovka_new.RData")

#### helper functions####
## prakticke pomocne funkcie

# faktor 1_1, 1_2, 2, 3_1_1, 3_1_2 na faktor 1, 1, 2, 3, 3
# levels zoradene podal order by
# ciel: lahke vytvorenie confusion matrix pomocou 
# table(simplify_and_order(prediction, real), real)
simplify_and_order <- function(f, order_by) {
  # Ensure both inputs are factors
  if (!is.factor(f) || !is.factor(order_by)) stop("Both inputs must be factors")
  
  # Simplify the levels of the first factor
  new_levels <- sub("_.*$", "", levels(f))  # Simplify levels of `f`
  
  f <- factor(f, levels = levels(f), labels = new_levels)  # Map simplified levels back
  
  #debug
  f0<-f
  
  # Order the levels of `f` based on the levels of `order_by`
  ordered_levels <- levels(order_by)
  f <- factor(f, levels = ordered_levels)
  
  #debug, commented only for testing data
  #if (sum(is.na(f))>0) stop("Missings present. Debug")
  if (sum(f!=f0)>0) stop('Factors are different after reordering. Debug')
  
  return(f)
}

# helper na detekciu singularit pre hlavnu funkciu
rank_deficiency <- function(x, grouping, group) {
  result <- vector(mode = "logical", length = length(group))
  i<-1
  
  for (g in group){
    sub_data <- x[grouping == g, , drop = FALSE]
    
    if (nrow(sub_data)==1) {result[i] <- TRUE
    next}
    
    cov_matrix <- cov(sub_data)
    rank_cov <- qr(cov_matrix)$rank
    full_rank <- min(dim(cov_matrix))  # Expected full rank
    
    if (rank_cov < full_rank) result[i] <- TRUE # FALSE at innitiation
    i <- i+1
  }
  return(result)
}

# rekurzivna generacia stromu z historie algoritmu (podla iteracie)
build_tree <- function(history, max_height ,parent = NaN, i=2) {
  
  # Find all entries where the current parent matches
  split_index <- which(sapply(history, function(x) {
    if (is.nan(parent)) {
      is.nan(x$parent)  # Correctly check for NA parent
    } else {
      x$parent == parent  # Standard equality check
    }
  }))
  
  if (length(split_index) == 0) { # split not found, must be leaf node
    tree <- list()
    
    attr(tree, 'leaf') <- TRUE
    attr(tree, 'members') <- 1
    attr(tree, 'height') <- max_height-history[[i]]$time
    attr(tree, 'label') <- parent
    if(i==1){
      attr(tree, 'edgetext') <- parent
    }
    
    return(tree)  
  }
  
  # Build the tree recursively
  tree <- list()
  total_members <- 0
  attr(tree, 'height') <- max_height-history[[split_index]]$time+1
  
  child_name <- history[[split_index]]$children  # Children of this node
  for (child in child_name) {
    subtree <- build_tree(history, max_height ,child, i=split_index)
    tree[[child]] <- subtree
    total_members <- total_members + attr(subtree, 'members')  # Accumulate members
  }
  
  attr(tree, 'members') <- total_members
  attr(tree, 'label') <- parent
  if(i==2 & !is.nan(parent)){
    attr(tree, 'edgetext') <- parent
  }
  return(tree)
}

# rekurzivna generacia stromu z historie algoritmu (podla accuracy)
build_tree_acc_trial <- function(history, true_heights ,parent = NaN, i=2) {
  
  # Find all entries where the current parent matches
  split_index <- which(sapply(history, function(x) {
    if (is.nan(parent)) {
      is.nan(x$parent)  # Correctly check for NA parent
    } else {
      x$parent == parent
    }
  }))
  
  # index where parent is as child
  before <- history[[i]]$parent

  if (history[[i]]$samestep_created==1){
    backlook <- 1
  }
  else{
    now_first <- history[[i]]$pos==1
    backlook <- c(1,2)[c(now_first, !now_first)]
  }
  
  if (length(split_index) == 0) { # split not found, must be leaf node
    tree <- list()
    attr(tree, 'leaf') <- TRUE
    attr(tree, 'members') <- 1
    # (1-acc_parent) - ((1-acc_me) - (1-acc_model_one_step_before_me))
    attr(tree, 'height') <- true_heights[[toString(before)]] - history[[i]]$acc + history[[i-backlook]]$acc
    attr(tree, 'label') <- parent
    if(i==2){
      attr(tree, 'edgetext') <- parent
    }
    
    return(tree)  
  }
  
  # Build the tree recursively
  tree <- list()
  total_members <- 0
  attr(tree, 'height') <- true_heights[[toString(before)]] - history[[i]]$acc + history[[i-backlook]]$acc
  true_heights[[toString(parent)]] <- true_heights[[toString(before)]] - history[[i]]$acc + history[[i-backlook]]$acc
  
  child_name <- history[[split_index]]$children  # Children of this node
  for (child in child_name) {
    subtree <- build_tree_acc_trial(history, true_heights=true_heights ,child, i=split_index)
    tree[[child]] <- subtree
    total_members <- total_members + attr(subtree, 'members')  # Accumulate members
  }
  
  attr(tree, 'members') <- total_members
  attr(tree, 'label') <- parent
  if(i==2 & !is.nan(parent)){
    attr(tree, 'edgetext') <- parent
  }
  return(tree)
}


# wrapper pre build_tree funkcie
MlClust_dendrogram <- function(object, metric, first_bump=0.05) {
  max_height <- which.max(sapply(object$history, function(x){x$time} ))
  if (metric=='time') tree<-build_tree(history = object$history, max_height=max_height)
  if (metric=='acc') {
    history_acc <- vector("list", length(object$history) + 1)
    true_heights <- vector("list", length(object$history) + 1)
    names(true_heights) <- c('NA', sapply(object$history, function(x) toString(x$parent)))
    history_acc[[1]] <- list(parent=NA, children=NaN, acc= object$history[[1]]$acc-first_bump, samestep_created=1, pos=1)
    history_acc[2:(length(object$history)+1)]<- object$history
    true_heights$'NA' <- 1-object$history[[1]]$acc+first_bump
    true_heights$'NaN' <- 1-object$history[[1]]$acc+first_bump
    tree <- build_tree_acc_trial(history = history_acc, true_heights=true_heights)
  }
  class(tree)<-'dendrogram'
  # tree <- as.dendrogram(tree)
  
  return(tree)
}

# wrapper for simplify_and_order
MlClust_conf_small <- function(fitted, target){
  # this if is just to play nice with data analysis functions (no nan breaks)
  if (length(levels(fitted))==0){
    return(matrix(nrow = 3, ncol = 3))
  }
  table(simplify_and_order(fitted, as.factor(target)), target)
}

# for cutting based on trees; 1_1_1, 1_1_3, 1_1_2, 1_2 -> cut at 1_1 ->
# -> 1_1, 1_1, 1_1, 1_2
shear <- function(lables, cut){
  
  char_vec <- as.character(lables)
  
  char_vec <- ifelse(grepl(paste0("^", cut), char_vec), cut, char_vec)
  
  return(factor(char_vec))
}

## pomocne funkcie na masovu analyzu dat
test_methods <- function(target, methods, wrapper){
  grid <- expand.grid(methods)
  grid[] <- lapply(grid, function(x) if (is.factor(x)) as.character(x) else x)
  
  results <- pmap(grid, wrapper)
  save(results, file = '~/results.RData')
  rows <- nrow(grid)
  grid['Classification_error_innit'] <- rep(NA, rows)
  grid['Classification_error'] <- rep(NA, rows)
  n_data <- length(target)
  C_e <- ncol(grid)

  
  for (i in 1:rows){
    #print(grid[i, -c(C_e-1, C_e)])
    #print(i/rows)
    grid[i,'Classification_error_innit'] <- sum(results[[i]]$conf_innit - diag(diag(results[[i]]$conf_innit)))/n_data
    conf <- MlClust_conf_small(results[[i]]$fitted, target)
    grid[i,'Classification_error'] <- sum(conf - diag(diag(conf)))/n_data
  }
  grid['after/innit'] <- grid$Classification_error/grid$Classification_error_innit
  return(grid)
}

test_variable_methods <- function(target, methods, wrapper){
  steps <- methods$step
  # steps <- rep(steps, rows)
  methods <- methods[names(methods) != 'step']
  grid <- expand.grid(methods)
  grid[] <- lapply(grid, function(x) if (is.factor(x)) as.character(x) else x)
  grid <- as.list(grid)
  rows <- length(grid$ML)
  
  grid[['step']] <-  rep(steps, rows)
  steps <- unlist(steps)
  
  results <- pmap(grid, wrapper)
  save(results, file = '~/results_var.RData')
  
  grid <- grid[names(grid) != 'step']
  grid <- as.data.frame(grid)
  grid['step'] <- rep(paste(as.character(min(steps)),as.character(max(steps)), sep = ':',collapse = ''), rows)
  grid['Accuracy_innit'] <- rep(NA, rows)
  grid['Accuracy_error'] <- rep(NA, rows)
  n_data <- length(target)
  C_e <- ncol(grid)
  
  
  for (i in 1:rows){
    #print(grid[i, -c(C_e-1, C_e)])
    #print(i/rows)
    grid[i,'Accuracy_innit'] <- sum(diag(results[[i]]$conf_innit))/n_data
    conf <- MlClust_conf_small(results[[i]]$fitted, target)
    grid[i,'Accuracy_error'] <- sum(diag(conf))/n_data
  }
  grid['after/innit'] <- grid$Accuracy_error/grid$Accuracy_innit
  return(grid)
}

validate_methods <- function(uncut, target, methods){
  grid <- expand.grid(methods)
  grid[] <- lapply(grid, function(x) if (is.factor(x)) as.character(x) else x)
  
  rows <- nrow(grid)
  grid['Accuracy_innit'] <- rep(NA, rows)
  grid['Accuracy_error'] <- rep(NA, rows)
  n_data <- length(target)
  C_e <- ncol(grid)
  
  
  for (i in 1:rows){
    #print(grid[i, -c(C_e-1, C_e)])
    #print(i/rows)
    # grid[i,'Accuracy_innit'] <- sum(diag(results[[i]]$conf_innit))/n_data
    if (grid[i,'ML'] %in% c('lda', 'qda')) fitted <-  uncut[[i]]$class
    else fitted <-  uncut[[i]]
    conf <- MlClust_conf_small(fitted, target)
    grid[i,'Accuracy_error'] <- sum(diag(conf))/n_data
  }
  grid['after/innit'] <- grid$Accuracy_error/grid$Accuracy_innit
  return(grid)
}

validate_variable_methods <- function(uncut, target, methods){
  steps <- methods$step
  # steps <- rep(steps, rows)
  methods <- methods[names(methods) != 'step']
  grid <- expand.grid(methods)
  grid[] <- lapply(grid, function(x) if (is.factor(x)) as.character(x) else x)
  grid <- as.list(grid)
  rows <- length(grid$ML)
  
  grid[['step']] <-  rep(steps, rows)
  steps <- unlist(steps)
  
  grid <- grid[names(grid) != 'step']
  grid <- as.data.frame(grid)
  grid['step'] <- rep(paste(as.character(min(steps)),as.character(max(steps)), sep = ':',collapse = ''), rows)
  grid['Accuracy_innit'] <- rep(NA, rows)
  grid['Accuracy_error'] <- rep(NA, rows)
  n_data <- length(target)
  C_e <- ncol(grid)
  
  
  for (i in 1:rows){
    #print(grid[i, -c(C_e-1, C_e)])
    #print(i/rows)
    
    if (grid[i,'ML'] %in% c('lda', 'qda')) fitted <-  uncut[[i]]$class
    else fitted <-  uncut[[i]]
    
    # grid[i,'Accuracy_innit'] <- sum(diag(results[[i]]$conf_innit))/n_data
    conf <- MlClust_conf_small(fitted, target)
    grid[i,'Accuracy_error'] <- sum(diag(conf))/n_data
  }
  grid['after/innit'] <- grid$Accuracy_error/grid$Accuracy_innit
  return(grid)
}
