#Main Function
Main_function<-function(files,K,top_words){
  #Getting the vocab from the docs
  
  temp2<-unlist(files)
  
  docs<-strsplit(temp2, split=' ', perl = T)    
  
  t1 <- vector(mode = "list", length = length(docs))
  fre_tab <- vector(mode = "list", length = length(docs))
  fre_tab2 <- vector(mode = "list", length = length(docs))
  
  for (j in 1:length(docs)) {
    t1[[j]] <- data.frame(table(docs[[j]]))
    fre_tab[[j]]<-t(t1[[j]][2])
    colnames(fre_tab[[j]])<- as.character(t1[[j]][,1])
    fre_tab2[[j]] <-data.frame(fre_tab[[j]])
  }
  
  allNms <- unique(unlist(lapply(fre_tab2, names)))
  
  bag_of_words<-do.call(rbind,
                        c(lapply(fre_tab2,
                                 function(x) data.frame(c(x, sapply(setdiff(allNms, names(x)),
                                                                    function(y) NA)))),
                          make.row.names=FALSE))
  
  
  bag_of_words[is.na(bag_of_words)]<-0
  
  vocab <- unique(unlist(docs))
  
  ## Replace words in documents with wordIDs
  for (i in 1:length(docs)) {
    docs[[i]]<- match(docs[[i]],vocab)
  }
  
  #Setting parameters
  alpha <- 5/K
  Beta <- .01 
  iterations <- 500 
  
  # Generate null word-topic count matrix.
  word_topic_count<-matrix(0,K,length(vocab))
  
  # Create an empty topic list with same dimensions as document list
  topic_assignment<-sapply(docs, function(x) rep(0, length(x)))
  
  #Assign topics to each word in the docs list randomly and then update the count word_topic count matrix
  for (line in 1:length(docs)) {
    for (word in 1:length(docs[[line]])) {
      topic_assignment[[line]][word]<-sample(1:K,1)
      word_index <-  docs[[line]][word]
      topic_index <- topic_assignment[[line]][word]  
      word_topic_count[topic_index,word_index]<-word_topic_count[topic_index, word_index] + 1
    }
    
  }
  
  #Document topic count
  document_topic <- matrix(0, length(docs), K)
  for (eachdoc in 1:length(docs)) {
    for (eachtopic in 1:K) {
      document_topic[eachdoc,eachtopic]<- sum(topic_assignment[[eachdoc]]==eachtopic)
    }
    
  }
  p_temp<-c()
  
  for(i in 1:iterations){
    for (eachdoc in 1:length(docs)) {
      for (eachword in 1:length(docs[[eachdoc]])) {
        t0 <- topic_assignment[[eachdoc]][eachword] #Pick up topic id of the word
        word_id <- docs[[eachdoc]][eachword] # Pick up word id of word
        document_topic[eachdoc,t0] <- document_topic[eachdoc,t0]-1 
        word_topic_count[t0,word_id] <- word_topic_count[t0,word_id]-1
        #for (t in 1:K) {
        #  p_temp[t]<-((document_topic[eachdoc,t] + alpha) / ( (K * alpha) + sum(document_topic[eachdoc,1:K]))) * ((word_topic_count[t,1] + Beta) / ((length(vocab) * Beta) + (rowSums(word_topic_count)[t])))  
        #}
        denom_a <- sum(document_topic[eachdoc,]) + K * alpha 
        denom_b <- rowSums(word_topic_count) + (length(vocab) * Beta) 
        p_temp <- ((word_topic_count[,word_id] + Beta) / denom_b) * ((document_topic[eachdoc,] + alpha) / denom_a) 
        t1 <- sample(1:K,1,prob = p_temp/sum(p_temp))
        topic_assignment[[eachdoc]][eachword] <- t1
        document_topic[eachdoc,t1] <- document_topic[eachdoc,t1] + 1 
        word_topic_count[t1,word_id] <- word_topic_count[t1,word_id] + 1
        
      }
      
    }
    
  }
  
  #word_distribution <- (word_topic_count + Beta) / ((rowSums(word_topic_count)+(word_topic_count*Beta))) # topic probabilities per word
  word_distribution<-word_topic_count
  colnames(word_distribution) <- vocab
  colnames(word_topic_count)<-vocab
  top_5<-t(apply(word_distribution,1,function(x) names(x)[order(x,na.last=NA, decreasing = T)][1:top_words]))
  theta<-document_topic
  #theta <- (document_topic+alpha) / ((rowSums(document_topic))+ (K*alpha))
  theta<-data.frame(theta)
  write.csv(top_5,'FinalOutput.csv')
  #return(list(document_topic, word_topic_count))
  return(list(theta,bag_of_words,top_5))
}


W_Map_test <- function(X,Y) {
  
  #Taking random sample
  N<-nrow(X)
  test_size<-round((1/3)*N)
  train_size<- N- test_size
  random_sample<-sample(N,test_size, replace = F)
  
  #Test Data
  X_test<-X[random_sample,]
  Y_test<-data.frame(Y[random_sample,])
  
  #Train Data
  X_train<-X[-random_sample,]
  Y_train<-data.frame(Y[-random_sample,])
  
  #Converting the data frames into matrices for computation
  X_train<-as.matrix(X_train)
  Y_train<-as.matrix(Y_train)
  X_test<-as.matrix(X_test)
  #Y_test<-as.matrix(Y_test)
  
  #Adding the intercept term to the test set 
  x0_test = rep(1,nrow(Y_test)) #bias
  X_test = cbind(x0_test,X_test)
  
  training_set_sizes<-c()
  W_map_result<-c()
  iterations<-c()
  run_time<-c()
  for (i in seq(0.1,1,0.1)) {
    #This gives the 10 training set sizes at each index of training_set_sizes
    training_set_sizes[10*i]<-round(i*train_size)
    
    #Adding the intercept term to training data
    x0 = rep(1,training_set_sizes[10*i]) #bias
    training_data = cbind(x0,X_train[1:training_set_sizes[10*i],])
    
    #Calculating Wmap for the given training set size
    Betas<- matrix(NA,ncol=1,nrow=ncol(training_data)) #Store Betas in this matrix
    Betas[,1] <- rep(0, ncol(training_data)) #starting values
    
    j<-2
    start_time <- Sys.time()
    repeat {
      a <- plogis( as.matrix(training_data) %*% Betas[,j-1])
      R <- diag(c(a*(1-a)))
      S.inv <-solve((1/100))
      first_derivative <- crossprod(training_data,(Y_train[1:training_set_sizes[10*i],] - a)) - as.matrix(Betas[,j-1])%*% S.inv
      temp1<-crossprod(training_data,R)%*%training_data
      diag(temp1)<-diag(temp1)+S.inv
      Hessian<- - temp1
      Hessian_inverse<-solve(Hessian)
      Betas <- cbind(Betas,Betas[,j-1] - (Hessian_inverse%*%first_derivative))
      if (all(abs(Betas[,j]-Betas[,j-1]) < (1e-3) )) break
      if (j>100) stop("Failure to find root after 100 iterations.")
      j<-j+1
    }
    end_time <- Sys.time()
    run_time[10*i]<-end_time - start_time
    
    #Checking the error for the given training set size with its wmap
    accuracy_table<-as.data.frame(plogis(X_test %*% t(Betas)[nrow(t(Betas)),]))
    colnames(accuracy_table)[1]<-"Sigmoid_Values"
    accuracy_table$prediction<-ifelse(accuracy_table$Sigmoid_Values >=0.5, 1,0)
    accuracy_table$actual_values<-Y_test
    accuracy_table$error<-ifelse(accuracy_table$prediction==accuracy_table$actual_values, 0,1)
    errors<-sum(accuracy_table$error)
    W_map_result[10*i]<-errors/nrow(Y_test)
    iterations[10*i]<-j-1
    
  }
  return(data.frame(cbind(Error_rate=W_map_result,iterations, run_time)))
  
}

GraphPlot<-function(sums, X, title ){
  N<-nrow(X)
  test_size<-round((1/3)*N)
  train_size<- N- test_size
  
  trainsetsize<-c()
  
  for (i in seq(0.1,1,0.1)) {
    trainsetsize[10*i]<-round(i*train_size)
  }
  
  ErrorsSample<- data.frame(sums[seq(1, length(sums), 3)])
  Accuracy<- 1- ErrorsSample
  #IterationsSample<- data.frame(sums[seq(2, length(sums), 3)])
  #run_time_sample<-data.frame(sums[seq(3, length(sums), 3)])
  
  error_mean<-apply(ErrorsSample,1, mean)
  accuracy_mean<-1 - error_mean
  #error_stddeviation<-apply(ErrorsSample,1, sd)
  accuracy_stderror<-apply(Accuracy,1, function(x){sd(x)/sqrt(length(x))})
  #IterationsSample_mean<-apply(IterationsSample,1, mean)
  #Run_time_mean<-apply(run_time_sample,1, mean)
  #error_rate<-error_mean/nrow(test_size)
  
  plot(trainsetsize,accuracy_mean  , type = 'o',
       
       ylim=range(c(accuracy_mean - accuracy_stderror, accuracy_mean + accuracy_stderror)),
       pch=19, xlab="Train Set Size", ylab="Accuracy +/- SD Error",
       main=title)
  arrows(trainsetsize, accuracy_mean-accuracy_stderror, trainsetsize, accuracy_mean+accuracy_stderror, length=0.05, angle=90, code=3)
  #return(data.frame(trainsetsize,accuracy_mean))
  #return(data.frame(trainsetsize,IterationsSample_mean,Run_time_mean,error_mean))
  #return(Accuracy)
}







