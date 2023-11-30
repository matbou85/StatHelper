#' Correlation test decision helper
#'
#' @description `Cor` is a statistical package to help the user making the right decision on which correlation test to perform on the data provided as input.
#'
#' @param x As input data. The input should be numeric.
#' @param y As input data. The input should be numeric.
#' @param quiet If the user wants to add the suggested test as a result. Default = TRUE
#'
#' @return A suggestion message on which statistical test to perform, some tests and a ggplot object for some tests (if quiet = FALSE).
#'
#' @examples
#' # The following example is based on the iris dataset:
#'
#' ## Examples
#' Cor(x = iris$Sepal.Length, y = iris$Sepal.Width)
#'
#' @import ggplot2
#'
#' @export


Cor <- function(x,
                y,
                quiet = TRUE){
  
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
  norm1 <- stats::shapiro.test(x)
  norm2 <- stats::shapiro.test(y)
  out1 <- boxplot.stats(x)$out
  out2 <- boxplot.stats(y)$out
  if((stats::shapiro.test(x)$p > 0.05) && (stats::shapiro.test(y)$p > 0.05)){
  msg <- cat(paste0("Suggest: According to the data the user provided a ", crayon::red$bold$underline("Pearson test"), " is suggested because the data are not significantly different (Shapiro-Wilk normality test > 0.05)."))
    test <- stats::cor.test(x, y, method = "pearson")
    plot <- ggplot(mapping = aes(x=x, y=y)) + 
              geom_point(shape=18, color="black", size = 3)+
              theme_classic() +
              geom_smooth(method=lm,  linetype="dashed",
                  color="darkred", fill="blue") +
              annotate("text", x=Inf, y=Inf, 
               label= paste0("R = ", format(round(test[["estimate"]], digits=2), nsmall = 1),", p = ", signif(test[["p.value"]], digits=3)), 
               vjust=1, hjust=1, size = 6)
    if(quiet){
      return(list(msg))
    }else {
      return(list(msg, norm1, norm2, test, plot))
    }
  }else if(length(out1) != 0 | length(out2) != 0){
    msg <- cat(paste0("Suggest: According to the data the user provided a ", crayon::red$bold$underline("Kendall test"), " is suggested because the data are significantly different (Shapiro-Wilk normality test < 0.05). Furthermore, outliers were detected."))
    test <- stats::cor.test(x, y, method = "kendall")
    plot <- ggplot(mapping = aes(x=x, y=y)) + 
      geom_point(shape=18, color="black", size = 3)+
      theme_classic() +
      geom_smooth(method=lm,  linetype="dashed",
                  color="darkred", fill="blue") +
      annotate("text", x=Inf, y=Inf, 
               label= paste0("R = ", format(round(test[["estimate"]], digits=2), nsmall = 1),", p = ", signif(test[["p.value"]], digits=3)), 
               vjust=1, hjust=1, size = 6)
    if(quiet){
      return(list(msg))
    }else {
      return(list(msg, norm1, norm2, test, plot))
    }
  }else {
  msg <- cat(paste0("Suggest: According to the data the user provided a ", crayon::red$bold$underline("Spearman test"), " is suggested because the data are significantly different (Shapiro-Wilk normality test < 0.05). Furthermore, no outliers were detected."))
  test <- stats::cor.test(x, y, method = "spearman")
  plot <- ggplot(mapping = aes(x=x, y=y)) + 
    geom_point(shape=18, color="black", size = 3)+
    theme_classic() +
    geom_smooth(method=lm,  linetype="dashed",
                color="darkred", fill="blue") +
    annotate("text", x=Inf, y=Inf, 
             label= paste0("R = ", format(round(test[["estimate"]], digits=2), nsmall = 1),", p = ", signif(test[["p.value"]], digits=3)), 
             vjust=1, hjust=1, size = 6)
    if(quiet){
     return(list(msg))
    }else {
     return(list(msg, norm1, norm2, test, plot))
    }
  }
}