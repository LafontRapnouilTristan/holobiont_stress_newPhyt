pairwise_letterZ <- 
  function(data,x,y,significance = .05){
    wilcx_rslt <- with(data,pairwise.wilcox.test(x = y, g = x,p.adjust.method = 'holm'))
    temp <- wilcx_rslt$p.value
    
    # Make a square matrix to populate with the alpha values.
    n <- nrow(temp)
    mat.names <- c(colnames(temp), rownames(temp)[n])
    my.mat <- matrix(data = NA, nrow = n+1, ncol = n+1)
    colnames(my.mat) <- mat.names
    rownames(my.mat) <- mat.names
    
    # Add diagonal.
    for (i in 1:nrow(my.mat)) {
      my.mat[ i, i] <- 0
    }
    
    # Get vector of p.values
    stat <- na.exclude(as.vector(wilcx_rslt$p.value))
    
    # Add other cells to square matrix.
    k=1
    for (j in 1:(nrow(my.mat)-1)) {
      for (i in ((j+1):nrow(my.mat))) {
        my.mat[i,j] <-  my.mat[j,i] <- stat[k]
        k=k+1
      }
    }
    cld_list <- multcompView::multcompLetters(my.mat, threshold=significance)
    cld_vec <- as.vector(cld_list$Letters)
    return(cld_vec)
    
  }

