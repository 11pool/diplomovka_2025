
#### Main Function - our method ####


diplomovka_rewrite <- function(data, lables, ML = 'qda', clust='mclust', method = 'pair', step = 2, max_depth = 5){
  
  #### Input check and clean ####
  if (length(step)>1 && clust!='mclust') stop('Vector steps are only supported for the mclust clustering method.')
  if (any(step%%1 != 0) || any(step < 1)) stop("A vector step must contain only integers that must be greater than 0")
  if (length(step)==1 && step < 2) stop("An integer step must be greater than 1")

  lables <- as.factor(lables)
  if (any(grepl("_", levels(lables)))) stop('Your labels seem to include an underscore.(_) 
                                            This function uses underscores for tracking clusterings. Please rename your lables.')
  # line for debugs
  # print( c(ML, clust, method, step, max_depth))
  
  #### function versions for specific methods ####
  classifier <- list(
    'lda' = function (data, lables) lda(x=data, grouping=lables),
    'qda' = function (data, lables) qda(x=data, grouping=lables),
    'svm' = function (data, lables) svm(x=data, y=lables, type='C-classification', scale = FALSE, kernel='linear')
  )
  if (!ML %in% names(classifier)) stop("Allowed ML methods are: ", paste(names(classifier), collapse = " "))
  
  
  clusterer <- list(
    "mclust"   = function(data, step) Mclust(data = data, G = step, verbose = FALSE),
    "kmeans"   = function(data, step) kmeans(x = data, centers = step),
    "kmedoids" = function(data, step) pam(x = data, k = step, diss = FALSE)
  )
  if (!clust %in% names(clusterer)) stop('Allowed clustering methods are: ', paste(names(clusterer), collapse = " "))
  
  selector <- list(
    'pair' = function(conf_temp) arrayInd(which.max(conf_temp), dim(conf_temp)),
    'marginal' = function(conf_temp) {
      marg <- colSums(conf_temp)
      return(marg[which.max(marg)])
      },
    'single' = function(conf_temp) {
      marg <- apply(conf_temp, 2,max)
      return(marg[which.max(marg)])
      }
  )
  if (!method %in% names(selector)) stop('Allowed cluster selection methods are: ',paste(names(selector), collapse = " "))
  
  bad_rank_innit <- list(
    'lda' = function (data, lables) rank_deficiency(data, rep(1, nrow(data)), c(1)),
    'qda' = function (data, lables) rank_deficiency(data, lables, levels(lables)),
    'svm' = function (data, lables) FALSE
  )
  bad_rank_innit_msg <- list(
    'lda' = function(lables, deficient) stop('Rank defficiency at innitial classification.'),
    'qda' = function(lables, deficient) stop('Rank defficiency at innitial classification for groups: ', paste(levels(lables)[deficient]), collapse = " ")
  )
  # lda does not need to be reviewed every time since the covariance matrix is independent of labels (more or less; weigted sums are largely fine)
  bad_rank <- list(
    'lda' = function (data, lables, namez) FALSE,
    'qda' = function (data, lables, namez) rank_deficiency(data, lables, namez),
    'svm' = function (data, lables, namez) FALSE
  )
  fitt <- list(
    'lda' = function (model, data) predict(model, data)$class,
    'qda' = function (model, data) predict(model, data)$class,
    'svm' = function (model, data) predict(model, data)
  )
  relable <- list (
    'kmeans' = function(class, clustering) paste0(class,"_", as.character(clustering$cluster)),
    'kmedoids' = function(class, clustering) paste0(class,"_", as.character(clustering$clustering)),
    'mclust' = function(class, clustering) paste0(class,"_", as.character(clustering$classification))
  )
  
  
  #### init ####
  n_data <- length(lables)
  final <- list()
  history <- vector("list", max_depth*length(levels(lables))+3) # way more than usually needed, could be better; rest is dropped at the end
  history[[1]]<-list(parent = NaN, children = levels(lables), time = 1, acc = NA, samestep_created=1, pos=1)
  
  # initial classification
  deficient <- bad_rank_innit[[ML]](data, lables)
  # Switch comments on these ifs when not using mass data analysis functions
  #if (any(deficient)) bad_rank_innit_msg[[ML]](lables, deficient)
  if (any(deficient)){
    final$fitted <- rep(NA, nrow(data))
    final$conf_innit <- matrix(nrow = 3, ncol = 3)
    return(final)
  }
  model <- classifier[[ML]](data, lables)
  
  # initial confusion matrix
  results <- fitt[[ML]](model, data)
  conf_matrix<-table(results, lables)
  final$conf_innit <- conf_matrix
  history[[1]]$acc <- sum(diag(conf_matrix))/n_data
  
  # main loop prep
  continue = TRUE # break loop flag
  i<-2 # time tracker, 2 because 1 is in history[[1]] above
  allowed <- levels(lables) # labels fit for sub-clustering
  
  #### main loop ####
  ## split into 2 versions: 1 clustering at a time and 2 clusterings at a time
  # 1 clust at a time:
  if (method %in% c('single', 'marginal')){
    while (continue) {
      # remove labels that have less data than the step size (clustering breaks)
      allowed <- setdiff(allowed,names(which(table(lables) < min(step))))
      
      if (length(allowed) == 0){
        continue = FALSE
        print('Ending early. All future subdivisions would result in rank deficiency or are too small for clustering.')
        break
      }
      
      # get next label to subcluster
      conf_temp <- conf_matrix[allowed,allowed, drop=FALSE]
      diag(conf_temp) <- 0
      worst <- selector[[method]](conf_temp)
      if (worst == 0) {
        continue = FALSE
        print('Ending with perfect conf. matrix. Check for overfitting.')
        break
      }
      
      class <- names(worst)
      clustering <- clusterer[[clust]](data[lables==class,], step)
      new_lables <- relable[[clust]](class, clustering)
      namez <- unique(new_lables, nmax = max(step))
      
      # check rank deficiency before classification, skip to next iteration if bad
      bad <- bad_rank[[ML]](data[lables==class,], new_lables, namez)
      allowed <- setdiff(allowed, class) # if succeeds then replaced, if not then don't want
      if(any(bad)) next

      # relabel and write history
      levels(lables) <- c(levels(lables), namez)
      lables[lables == class] <- new_lables
      allowed <- c(allowed, namez)
      lables <- factor(lables, levels = setdiff(levels(lables), class))
        
      history[[i]] <- list(parent = class, time = i, children = namez, acc=NA, samestep_created = 1, pos=1)

      
      # classification again
      model <- classifier[[ML]](data, lables)
      results <- fitt[[ML]](model, data)
      conf_matrix<-table(results, lables)
      history[[i]]$acc <- sum(diag(conf_matrix))/n_data
      i <- i+1

      # stopping condition
      depth <- max(str_count(levels(lables), "_"))
      if (depth == max_depth) continue <- FALSE
    }
  }
  
  # 2 clust at a time:
  if (method %in% c('pair')){
    while (continue) {
      # remove labels that have less data than the step size (clustering breaks)
      allowed <- setdiff(allowed,names(which(table(lables) < min(step))))
      
      if (length(allowed) == 0){
        continue = FALSE
        print('Ending early. All future subdivisions would result in rank deficiency or are too small for clustering.')
        break
      }
      
      # get next label to subcluster
      conf_temp <- conf_matrix[allowed,allowed, drop=FALSE]
      diag(conf_temp) <- 0
      worst_ind <- selector[[method]](conf_temp)
      
      if (conf_temp[worst_ind] == 0) {
        continue = FALSE
        print('Ending with perfect conf. matrix. Check for overfitting.')
        break
      }

      class_1 <- rownames(conf_temp)[worst_ind[1]]
      class_2 <- colnames(conf_temp)[worst_ind[2]]
      
      clustering_1 <- clusterer[[clust]](data[lables==class_1,], step)
      new_lables_1 <- relable[[clust]](class_1, clustering_1)
      namez_1 <- unique(new_lables_1, nmax = max(step))
      clustering_2 <- clusterer[[clust]](data[lables==class_2,], step)
      new_lables_2 <- relable[[clust]](class_2, clustering_2)
      namez_2 <- unique(new_lables_2, nmax = max(step))
      
      # check rank deficiency before classification, skip to next iteration if bad,
      # relabel and write history

      bad_1 <- bad_rank[[ML]](data[lables==class_1,], new_lables_1, namez_1)
      bad_2 <- bad_rank[[ML]](data[lables==class_2,], new_lables_2, namez_2)
      allowed <- setdiff(allowed, c(class_1, class_2)) # if succeeds then replaced, if not then don't want
      if(any(bad_1) && any(bad_2)) next
      if (!any(bad_1)){
        allowed <- c(allowed, namez_1)
        levels(lables) <- c(levels(lables), namez_1)
        lables[lables == class_1] <- new_lables_1
        
        history[[i]] <- list(parent = class_1, time = i, children = namez_1, acc=NA, samestep_created=NA, pos=NA)
        i <- i+1
      }
      if (!any(bad_2)){
        allowed <- c(allowed, namez_2)
        levels(lables) <- c(levels(lables), namez_2)
        lables[lables == class_2] <- new_lables_2
        
        history[[i]] <- list(parent = class_2, time = c(i-1,i)[c(!any(bad_1), any(bad_1))], children = namez_2, acc=NA, samestep_created=NA, pos=NA)
        i <- i+1
      }

      lables <- factor(lables, levels = setdiff(levels(lables), c(class_1, class_2)[c(!any(bad_1), !any(bad_2))] ))
      
      # classification again
      model <- classifier[[ML]](data, lables)
      results <- fitt[[ML]](model, data)
      conf_matrix<-table(results, lables)
      # both created: write to older; otherwise: write to the only one created 
      history[[c(i-2, i-1)[c(!any(bad_1) && !any(bad_1), !(!any(bad_1) && !any(bad_1)))]]]$acc <- sum(diag(conf_matrix))/n_data
      history[[c(i-2, i-1)[c(!any(bad_1) && !any(bad_1), !(!any(bad_1) && !any(bad_1)))]]]$samestep_created <- c(2, 1)[c(!any(bad_1) && !any(bad_1), !(!any(bad_1) && !any(bad_1)))]
      history[[c(i-2, i-1)[c(!any(bad_1) && !any(bad_1), !(!any(bad_1) && !any(bad_1)))]]]$pos <- 1
      # written to newer in case line before wrote to older, otherwise harmless rewrite
      history[[i-1]]$acc <- sum(diag(conf_matrix))/n_data
      history[[i-1]]$samestep_created <- c(2, 1)[c(!any(bad_1) && !any(bad_1), !(!any(bad_1) && !any(bad_1)))]
      history[[i-1]]$pos <- c(2, 1)[c(!any(bad_1) && !any(bad_1), !(!any(bad_1) && !any(bad_1)))]
      
      # stopping condition
      depth <- max(str_count(levels(lables), "_"))
      if (depth == max_depth) continue <- FALSE
    }
  }
  
  
  #### final results ####
  final$model <- model
  final$conf <- conf_matrix
  final$fitted <- results
  history <- history[1:(i-1)] # drop empty lists
  final$history <- history
  print(Sys.time())
  return(final)
}

