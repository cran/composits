tsout_ensemble_1M <- function(x, pre=0, ncomp = NULL){ #, wts = NULL
  # x is generally more than 1 dimensional
  # pre = 0 is no pre-processing
  # pre = 1 is dobin pre-processing
  # pre = 2 is pca pre-processing
  # pre = 3 is invariant coordinate selection
  # pre = 4 is independent components analysis

  x <- as.data.frame(x)
  ercoords <- FALSE
  d <- dim(x)[2]
  if(is.null(ncomp)){
    ncomp <- ceiling(d/2)  # d #
  }

  if(d!=1){
    if(pre==0){
      # no pre-processing
      y <- x
    }else if(pre==1){
      # dobin pre-processing
      dobout <- dobin::dobin(x)
      y <- dobout$coords[ ,1:ncomp]
      wts <- 1/(1:ncomp)
      wts <- wts/sum(wts)
      load_vectors <- dobout$vec[ ,1:ncomp]
    }else if(pre==2){
      # pca pre-processing
      pca <- prcomp(x)
      y <- pca$x[ ,1:ncomp]
      wts <- pca$sdev[1:ncomp]
      wts <- wts/sum(wts)
      load_vectors <- pca$rotation[ ,1:ncomp]
    }else if(pre==3){
      ercoords <-  tryCatch({
        # invariant coordinate selection
        icsobj <- ICS::ics(x)
        y <- ICS::ics.components(icsobj)
        y <- y[ ,1:ncomp]
        wts <- icsobj@gKurt[1:ncomp]
        load_vectors0 <- icsobj@UnMix[ ,1:ncomp]
        tt <- apply(load_vectors0 , 2, function(x) sum(x^2))
        load_vectors <- sweep(load_vectors0, 2, FUN="/", sqrt(tt))
        if(sum(is.nan(wts))>0){
          print("ICS weights are NaN. So setting them to 1, 1/2, 1/3, etc . . .")
          wts <- 1/(1:ncomp)
        }

        wts <- wts/sum(wts)
        ercoords <- FALSE
      },error = function(c){
        ercoords <- TRUE
        load_vectors <- NA
        print("Error in computing invariant coordinates - ICS!")
        return(ercoords)
      })
    }else if(pre==4){
      # independent component analysis
      ica<-fastICA::fastICA(x, n.comp=ncomp)
      y <- ica$S
      wts <- rep(1, ncomp)
      wts <- wts/sum(wts)
      load_vectors0 <- ica$K %*% ica$W
      tt <- apply(load_vectors0 , 2, function(x) sum(x^2))
      load_vectors <- sweep(load_vectors0, 2, FUN="/", sqrt(tt))
    }
  }else{
    # If it is only one dimensional
    y <- x
  }

  out_indices <- matrix(0, ncol=ncomp, nrow=dim(x)[1])
  outmat <- matrix(0, ncol=4, nrow=dim(x)[1])
  outmat3D <- array(0, dim=c(dim(x)[1], 4, ncomp))
  colnames(outmat) <- c("forecast", "tsoutliers", "otsad", "anomalize" )
  if(ercoords){
    # Error in computing coordinates - Returning zeros or empty vectors
    print("Its HERE!!!")
    wts <- 1/(1:ncomp)
    result <- list()
    result$summary <-  c()
    result$tab <- out_indices
    result$score <- rep(0,dim(x)[1])
    result$normscore <- rep(0,dim(x)[1])
    result$outmat <- outmat
    result$load_vectors <- NA
    result$outmat3D <- outmat3D
    result$wts <- wts
  }else{
    # No error in computing coordinates

    for(i in 1:dim(y)[2]){
      y1 <- y[ ,i]
      tsout <- uv_tsout_ens(y1)
      out <- tsout$outliers
      outmat <- outmat + tsout$outmat
      outmat3D[ , ,i] <- outmat
      if(length(out)>0){
        for(jj in 1:length(out)){
          out_indices[out[jj],i] <- out_indices[out[jj],i] +1
        }
      }
      if(i==1){
        allout<- out
      }else{
        allout <- c(allout, out)
      }
    }

    scores <- out_indices %*% wts
    scores_norm <- scores/max(scores)
    result <- list()
    result$summary <-  table(allout)
    result$tab <- out_indices
    result$score <- scores
    result$normscore <- scores_norm
    result$outmat <- outmat
    result$load_vectors <- load_vectors
    result$outmat3D <- outmat3D
    result$wts <- wts
  }
  return(result)
}
