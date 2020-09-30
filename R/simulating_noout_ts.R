simulate_noout <- function(dd=5, len=500, sd=2000){
  set.seed(sd)
  N=dd
  TT=len
  K = 2
  loggamma = matrix(NA,TT+1,N)
  x = matrix(NA,TT+1,K)

  A = matrix(rnorm(N*K,0,0.3),N,K)  #0.1
  B = matrix(c(0.8,0,0,0.5),K,K)
  C = matrix(c(0,0,0,0),K,K)   # No outliers as C = 0


  mu0 = as.matrix(c(0.3,0.7))  #-1
  mu = solve((diag(1,K)-B))%*%mu0
  x[1,] = mu
  loggamma[1,] = rep(0,N)
  Ind = matrix(as.numeric(runif((TT+1)*K)>0.995),TT+1,K)
  timeIndex = which(Ind==1,arr.ind = T)

  sigmaeps = c(0.1,0.1)+0.3 #Increse to make variance in proportions larger decrease to make it smaller

  for (t in 2:(TT+1)){
    x[t,] = mu0+B%*%as.matrix(x[t-1,])+as.matrix(sigmaeps*rnorm(K,0,1))+C%*%as.matrix(Ind[t,])
    loggamma[t,] = A%*%x[t,]
  }

  timeIndex[,1] = timeIndex[,1]-1
  Gamma = exp(loggamma[2:(TT+1),])
  prand <- t(apply(Gamma, 1, function(x) x/sum(x)))
  coord_obj <- get_coords(prand)
  new_coords <- coord_obj$y

  return(new_coords)
}

no_outlier_scores_1 <- function(dd, N, ncomp, sd, rept = 1, m1){
  dd <- dd +1
  thres1 <- rep(0, rept)
  thres2 <- rep(0, rept)
  for(i in 1:rept){
    y <- simulate_noout(dd, N, sd)
    out <- tsout_ensemble(y, m1, ncomp=ncomp)
    scores <- out$all$Total_Score
    if(length(scores)>0){
      thres2[i] <- max(scores)
      thres1[i] <- quantile(scores, probs=0.95)
      # The two thresholds are equal - go for the std dev of the scores - max - std(scores)
      if(thres2[i]==thres1[i]){
        thres1[i] <- thres2[i] - sd(scores)
      }
    }else{
      thres2 <- thres1 <- NA
    }

  }
  ret <- list()
  ret$thres1 <- mean(thres1)
  ret$thres2 <- mean(thres2)
  ret$ens <- out
  return(ret)

}

no_outlier_scores_2 <- function(x, ncomp, outobj, rat=0.1, m1){
  # If outobj$outliers does not have any records then use outobj$all
  lenout <- dim(outobj$all)[1]
  if(lenout==0){
    ret <- list()
    ret$thres1 <- 0
    ret$thres2 <- 0
    ret$ens <- 0
    return(ret)
  }
  lenall <- dim(x)[1]
  ratio <- lenout/lenall
  if(ratio<rat){
    outlier_candidates <- outobj$all
  }else{
    # the outobj$all have more than 10% of the whole dataset
    len <- ceiling(lenall/10)
    ord <- order(outobj$all$Total_Score, decreasing=TRUE)
    sorted_out <- outobj$all[ord, ]
    outlier_candidates <- sorted_out[1:len, ]
  }
  if(dim(outlier_candidates)[1]==1){
    out_inds <- outlier_candidates[1]
  }else{
    out_inds <- outlier_candidates$Indices
  }


  alt_x <- x
  alt_x[out_inds, ] <- NA
  for(kkk in 1:dim(x)[2]){
    alt_x[ ,kkk] <- forecast::na.interp(alt_x[ ,kkk])
  }

  out <- tsout_ensemble(alt_x, m1, ncomp=ncomp)
  scores <- out$all$Total_Score
  if(length(scores)>0){
    thres2 <- max(scores)
    thres1 <- quantile(scores, probs=0.95)
  }else{
    thres1 <- thres2 <- 0
  }

  ret <- list()
  ret$thres1 <- thres1
  ret$thres2 <- thres2
  ret$ens <- out
  return(ret)
}
