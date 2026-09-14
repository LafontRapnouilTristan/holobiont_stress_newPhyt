cld_bxplot_wilcox <- function(data, x, y, significance = 0.05, plot, nudge = 0.05){
    if (kruskal.test(y,x)$p.value<0.05){
        box_rslt <- with(data,graphics::boxplot(y~x ,plot = F))
        
        loc_vec <- box_rslt$stats[nrow(box_rslt$stats),]
        
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
        cld_vec <- multcompView::multcompLetters(my.mat, threshold=significance)
        
        #df with the letters
        x_df <- c(1:length(cld_vec$Letters))
        y_df <- box_rslt$stats[nrow(box_rslt$stats),]
        cbd <- cld_vec$Letters
        ltr_df <- data.frame(x_df, y_df, cbd)
        
        #get coord of the graph
        plt1 <- ggplot2::ggplot_build(plot)
        xmin = plt1 $layout$panel_params[[1]]$x.range[1]
        xmax = plt1 $layout$panel_params[[1]]$x.range[2]
        ymin = plt1 $layout$panel_params[[1]]$y.range[1]
        ymax = plt1 $layout$panel_params[[1]]$y.range[2]
        lmts <- list(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
        y.range <- lmts$ymax - lmts$ymin
        y.nudge <- nudge * y.range
        
        cld <- list(ltr_df = ltr_df, y.nudge = y.nudge)
        
        return(plot +
                   annotate(geom="text", x=cld$ltr_df$x_df, y=cld$ltr_df$y_df+cld$y.nudge, label=cld$ltr_df$cbd))
    }
    else {return(plot)}
}