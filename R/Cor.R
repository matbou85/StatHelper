
library(ggplot2)

Cor <- function(x,
                y,
                quiet = FALSE){
  
  # Check user input --------------------------------------------------------
  if(inherits(x, "data.frame")){
    stop("'x' should be a vector")
  }
  if(!inherits(x, c("double", "numeric"))){
    stop("'x' should be a numeric vector")
  }
  if(!inherits(y, c("double", "numeric"))){
    stop("'y' should be a numeric vector")
  }
  ## correlation tests
  norm1 <- shapiro.test(x)
  norm2 <- shapiro.test(y)
  if((shapiro.test(x)$p > 0.05) && (shapiro.test(y)$p > 0.05)){
  msg <- cat(paste0("Suggest: According to the data the user provided a ", crayon::red$bold$underline("Pearson test"), " is suggested because the data are not significantly different (Shapiro-Wilk normality test > 0.05)."))
    out1 <- boxplot.stats(x)$out
    out2 <- boxplot.stats(y)$out
    test <- cor.test(x, y, method = "pearson")
    plot <- ggplot(mapping = aes(x=x, y=y)) + 
              geom_point(shape=18, color="black", size = 3)+
              theme_classic() +
              geom_smooth(method=lm,  linetype="dashed",
                  color="darkred", fill="blue") +
              annotate("text", x=Inf, y=Inf, 
               label= paste0("R = ", format(round(test[["estimate"]], digits=2), nsmall = 1),", p = ", signif(test[["p.value"]], digits=3)), 
               vjust=1, hjust=1, size = 6)
    return(list(msg, norm1, norm2, test, plot))
  }else if(length(out1) != 0 | length(out2) != 0){
    msg <- cat(paste0("Suggest: According to the data the user provided a ", crayon::red$bold$underline("Kendall test"), " is suggested because the data are significantly different (Shapiro-Wilk normality test < 0.05). Furthermore, outliers were detected."))
    test <- cor.test(x, y, method = "kendall")
    plot <- ggplot(mapping = aes(x=x, y=y)) + 
      geom_point(shape=18, color="black", size = 3)+
      theme_classic() +
      geom_smooth(method=lm,  linetype="dashed",
                  color="darkred", fill="blue") +
      annotate("text", x=Inf, y=Inf, 
               label= paste0("R = ", format(round(test[["estimate"]], digits=2), nsmall = 1),", p = ", signif(test[["p.value"]], digits=3)), 
               vjust=1, hjust=1, size = 6)
    return(list(msg, norm1, norm2, test, plot))
  }else {
  msg <- cat(paste0("Suggest: According to the data the user provided a ", crayon::red$bold$underline("Spearman test"), " is suggested because the data are significantly different (Shapiro-Wilk normality test < 0.05). Furthermore, no outliers were detected."))
  test <- cor.test(x, y, method = "spearman")
  plot <- ggplot(mapping = aes(x=x, y=y)) + 
    geom_point(shape=18, color="black", size = 3)+
    theme_classic() +
    geom_smooth(method=lm,  linetype="dashed",
                color="darkred", fill="blue") +
    annotate("text", x=Inf, y=Inf, 
             label= paste0("R = ", format(round(test[["estimate"]], digits=2), nsmall = 1),", p = ", signif(test[["p.value"]], digits=3)), 
             vjust=1, hjust=1, size = 6)
    return(list(msg, norm1, norm2, test, plot))
  }
}


Cor(x = iris$Sepal.Length, y = iris$Sepal.Width)
