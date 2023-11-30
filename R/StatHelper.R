#' Statistical test decision helper
#'
#' @description `StatHelper` is a statistical package to help the user making the right decision on which statistical test to perform on the data provided as input.
#'
#' @param x As input data.
#' @param group The user should provide here the grouping of the rows if a dataframe was provided, otherwise a vector.
#' @param paired The user can indictae here if the data are paired. Default = FALSE.
#' @param var1 The user can provide a first variable to be included in the analysis.
#' @param var2 The user can provide a second variable to be included in the analysis.
#' @param var3 The user can provide a third variable to be included in the analysis.
#' @param quiet If the user wants to add the suggested test as a result. Default = TRUE.
#' @param mu Only necessary if the user wants to adjust the theoretical mean. Default = 25.
#'
#'
#' @return A suggestion message on which statistical test to perform, some tests and a ggplot object for some tests (if quiet = FALSE).
#'
#' @examples
#' # The following example is based on the iris dataset:
#'
#' ## Example 1 McNemar Test
#' ### McNemar Test
#'   set.seed(150)
#'   data <- data.frame(before = sample(c("Positive",
#'                                        "Positive",
#'                                        "Positive",
#'                                        "Positive",
#'                                        "Negative"),
#'                                      300, replace = TRUE),
#'                      after = sample(c("Positive",
#'                                       "Positive",
#'                                       "Positive",
#'                                       "Positive",
#'                                       "Negative"),
#'                                     300, replace = TRUE))
#'
#'   StatHelper(x = data, var1 = data$before, var2 = data$after, paired = TRUE)
#'
#' ## Example 2
#' ### One-way Anova
#'   anova <- PlantGrowth
#'   anova$group <- ordered(anova$group,
#'                          levels = c("ctrl", "trt1", "trt2"))
#' StatHelper(x = anova, group = anova$group, var1 = anova$weight)
#'
#' @import varhandle
#' @import ggpubr
#' @importFrom RVAideMemoire cochran.qtest
#' @import broom
#' @import dplyr
#' @import rstatix
#' @importFrom cli cli_alert_warning
#' @importFrom car leveneTest
#' @import crayon
#'
#' @export

StatHelper <- function(x = NULL,
                     group = NULL,
                     paired = FALSE,
                     var1 = FALSE,
                     var2 = FALSE,
                     var3 = FALSE,
                     quiet = TRUE,
                     mu = 25){

  # preprocessing -----------------------------------------------------------
  ## construct our x to be dataframe
  {
    if(is.vector(x)){
      x <- data.frame(x = x, stringsAsFactors = FALSE)
    }
    else if(inherits(x, "matrix")){
      x <- data.frame(x)
    }
  }
  
  # the green tree ----------------------------------------------------------
  if(!missing(var1) && missing(var2) && missing(var3) && missing(group) && all(varhandle::check.numeric(var1))){
    res <- stats::shapiro.test(var1)
    if(res$p.value > 0.05){
      msg <- cat(paste0("Because only one column was given and the normality test showed that the distribution of the data are not significantly different (Shapiro-Wilk normality test > 0.05), the function assume that a ", crayon::red$bold$underline("One Sample t-test"), " is most appropriate."))
      plot1 <- ggqqplot(var1, ylab = "",
               ggtheme = theme_minimal())
      test <- stats::t.test(var1, mu = mu)
      if(quiet){
        return(msg)
      }else {
        return(list(msg, res, plot1, test))
      }
    }else if(res$p.value < 0.05){
      msg <- cat(paste0("Because only one column was given and the normality test showed that the distribution of the data are significantly different (Shapiro-Wilk normality test < 0.05), the function assume that a ", crayon::red$bold$underline("One Sample Wilcoxon test"), " is most appropriate."))
      plot1 <- ggqqplot(var1, ylab = "",
                        ggtheme = theme_minimal())
      test <- stats::wilcox.test(var1, mu = mu)
      if(quiet){
        return(msg)
      }else {
        return(list(msg, res, plot1, test))
      }
    }

  # the blue tree -----------------------------------------------------------

# paired samples ----------------------------------------------------------
  }else if (paired){
    if(missing(var3) && sum(!sapply(cbind.data.frame(var1, var2), function(.){ all(varhandle::check.numeric(.)) })) == 2L){
      msg <- cat(paste0("Because the samples are paired and two character vectors were provided by the user then a ", crayon::red$bold$underline("McNemar's test"), " seems to be nost appropriate."))
      test <- stats::mcnemar.test(var1, var2)
      if(quiet){
        return(msg)
      }else {
        return(list(msg, test))
      }
    }else if(!missing(var3) && sum(!sapply(cbind.data.frame(var1, var2, var3), function(.){ all(varhandle::check.numeric(.)) })) == 3L){
      msg <- cat(paste0("Because the samples are paired and three character vectors were provided by the user then a ", crayon::red$bold$underline("Cochran's Q test"), " seems to be nost appropriate."))
      test <- RVAideMemoire::cochran.qtest(var1 ~ var2 | var3,
                    data = x)
      if(quiet){
        return(msg)
      }else {
        return(list(msg, test))
      }
    }else if(all(!varhandle::check.numeric(group))){
      if((length(unique(group)) == 2L) && (length(group) == length(var1))){
        xx <- cbind.data.frame(var1, group)
        res <- xx %>%
          dplyr::group_by(group) %>% 
          dplyr::summarise(p.value = stats::shapiro.test(var1)$p.value)
        if(nrow(x) < 30L){
          if(all(res$p.value > 0.05, na.rm = T) == T){
            msg <- cat(paste0("Because the samples are paired and two columns were given as input (group and var1) and the normality test showed that the distribution of the data are not significantly different (Shapiro-Wilk normality test > 0.05), the function assume that a ", crayon::red$bold$underline("Paired t-test"), " is most appropriate."))
            stat1 <- stats::t.test(var1 ~ group, data = x, paired = TRUE)
            if(quiet){
              return(msg)
            }else {
              return(list(msg, res, stat1))
            }
          }
        }else if(all(res$p.value > 0.05, na.rm = T) == F){
          msg <- cat(paste0("Because the samples are paired and two columns were given as input (group and var1) and the normality test showed that the distribution of the data are significantly different (Shapiro-Wilk normality test < 0.05), the function assume that a ", crayon::red$bold$underline("Paired Wilcoxon test"), " is most appropriate."))
          test <- stats::wilcox.test(var1 ~ group,
                      data = x,
                      paired = TRUE,
                      alternative = "greater")
          if(quiet){
            return(msg)
          }else {
            return(list(msg, res, test))
          }
        }
      }else if((length(unique(group)) > 2L) && (length(group) == length(var1))){
        xx <- cbind.data.frame(var1, var2, group)
        res <- xx %>%
          dplyr::group_by(group) %>% 
          dplyr::summarise(p.value = stats::shapiro.test(var1)$p.value)
        if(all(res$p.value > 0.05, na.rm = T) == T){
          msg <- cat(paste0("Because the samples are paired and three or more groups were given as input (group and var1) and the normality test showed that the distribution of the data are not significantly different (Shapiro-Wilk normality test > 0.05), the function assume that a ", crayon::red$bold$underline("repeated measures ANOVA test"), " is most appropriate."))
          plot1 <- ggqqplot(var1, ylab = "",
                            ggtheme = theme_minimal())
          res.aov <- rstatix::anova_test(data = xx, dv = var1, wid = var2, within = group)
          test <- rstatix::get_anova_table(res.aov)
          if(quiet){
            return(msg)
          }else {
            return(list(msg, plot1, test))
          }
        }else if(any(res$p.value < 0.05, na.rm = T) == T){
          msg <- cat(paste0("Because the samples are paired and three or more groups were given as input (group and var1) and the normality test showed that the distribution of the data are significantly different (Shapiro-Wilk normality test < 0.05), the function assume that a ", crayon::red$bold$underline("Friedman test"), " is most appropriate."))
          plot1 <- ggqqplot(var1, ylab = "",
                            ggtheme = theme_minimal())
          test <- rstatix::friedman_test(data = xx, var1 ~ group |var2)
          if(quiet){
            return(msg)
          }else {
            return(list(msg, plot1, test))
          }
        }
      }
    }
  
# Independent samples -----------------------------------------------------
  }else if(!missing(var1) && !missing(var2) && missing(group) && (sum(sapply(cbind.data.frame(var1, var2), function(.){ all(varhandle::check.numeric(.)) })) == 2L)){
    msg <- cat(paste0("Because the samples are independent and two character columns were given as input (var1 and var2), the function assume that a ", crayon::red$bold$underline("Fisher's exact test"), " is most appropriate."))
    test <- stats::fisher.test(var1, var2,
      alternative="two.sided")
    if(quiet){
      return(msg)
    }else {
      return(list(msg, test))
    }
  }else if(missing(var1) && missing(var2) && missing(var3) & missing(group) && (sum(sapply(x, function(.){ all(varhandle::check.numeric(.)) })) > 2L) & ncol(x) > 2){
    msg <- cat(paste0("Because the samples are independent and several numeric columns were given as input, the function assume that a ", crayon::red$bold$underline("Chi-square test of independence"), " is most appropriate."))
    test <- stats::chisq.test(x, simulate.p.value = TRUE)
    if(quiet){
      return(msg)
    }else {
      return(list(msg, test))
    }
  }else if (all(!varhandle::check.numeric(group))){
    if((length(unique(group)) == 2L) && (length(group) == length(var1))){
      xx <- cbind.data.frame(var1, group)
      res <- xx %>%
        dplyr::group_by(group) %>% 
        dplyr::summarise(p.value = stats::shapiro.test(var1)$p.value)
      var <- stats::var.test(var1 ~ group, data = x)
      if((all(res$p.value > 0.05, na.rm = T) == T) && var$p.value > 0.05){
        msg <- cat(paste0("Because the samples are independent and two groups were given as input (group and var1), the normality test showed that the distribution of the data are not significantly different (Shapiro-Wilk normality test > 0.05), and the variance test showed no significance between variances (F-test > 0.05), the function assume that a ", crayon::red$bold$underline("student's t-test"), " is most appropriate."))
        var <- stats::var.test(var1 ~ group, data = x)
        stat1 <- stats::t.test(var1 ~ group, var.equal = T)
        plot1 <- ggqqplot(var1, ylab = "",
                          ggtheme = theme_minimal())
        if(quiet){
          return(msg)
        }else {
          return(list(msg, plot1, res, var, stat1))
        }
      }
      else if((all(res$p.value > 0.05, na.rm = T) == T) && var$p.value < 0.05){
        msg <- cat(paste0("Because the samples are independent and two groups were given as input (group and var1), the normality test showed that the distribution of the data are not significantly different (Shapiro-Wilk normality test > 0.05), and the variance test showed significance between variances (F-test < 0.05), the function assume that a ", crayon::red$bold$underline("Welch's test"), " is most appropriate."))
        stat1 <- stats::t.test(var1 ~ group, var.equal = T)
        plot1 <- ggqqplot(var1, ylab = "",
                          ggtheme = theme_minimal())
        if(quiet){
          return(msg)
        }else {
          return(list(msg, plot1, res, var, stat1))
        }
      }
      else if(any(res$p.value < 0.05, na.rm = T) == T){
        msg <- cat(paste0("Because the samples are independent and two groups were given as input (group and var1), the normality test showed that the distribution of the data are significantly different (Shapiro-Wilk normality test < 0.05), the function assume that a ", crayon::red$bold$underline("Mann Whitney U-test"), " is most appropriate."))
        stat1 <- stats::wilcox.test(var1 ~ group,
                    data=x, exact = F)
        plot1 <- ggqqplot(var1, ylab = "",
                          ggtheme = theme_minimal())
        if(quiet){
          return(msg)
        }else {
          return(list(msg, plot1, res, var, stat1))
        }
      } 
    }else if((length(unique(group)) > 2L) && (length(group) == length(var1))){
      xx <- cbind.data.frame(var1, group)
      var <- car::leveneTest(var1 ~ group, data = x)
      res <- xx %>%
        dplyr::group_by(group) %>% 
        dplyr::summarise(p.value = stats::shapiro.test(var1)$p.value)
      if (any(var$`Pr(>F)` > 0.05, na.rm = T) == T && all(res$p.value > 0.05, na.rm = T) == T){
        res.aov <- stats::aov(var1 ~ group,
            data = x)
        test <- summary(res.aov)
        plot <- plot(res.aov, 2)
        msg <- cat(paste0("Because the samples are independent and three or more groups were given as input (group and var1) and the normality test showed that the distribution of the data are not significantly different (Shapiro-Wilk normality test > 0.05), and the variance test showed significance between variances (Levene's test > 0.05), the function assume that a ", crayon::red$bold$underline("one-way ANOVA test"), " is most appropriate. A post-hoc test might be appropriate (See Tukey, Dunnett etc.)."))
        if(quiet){
          return(msg)
        }else {
          return(list(norm, test, plot, msg))
        }
      }else if(any(var$`Pr(>F)` < 0.05, na.rm = T) == T){
        norm <- xx %>%
          dplyr::group_by(group) %>% 
          dplyr::summarise(p.value = stats::shapiro.test(var1)$p.value)
        test <- stats::oneway.test(var1 ~ group,
                               data=x,
                               var.equal=FALSE)
        plot1 <- ggqqplot(var1, ylab = "",
                          ggtheme = theme_minimal())
        msg <- cat(paste0("Because the samples are independent and three or more groups were given as input (group and var1) and the normality test showed that the distribution of the data are not significantly different (Shapiro-Wilk normality test > 0.05), and the variance test showed significance between variances (Levene's test < 0.05), the function assume that a ", crayon::red$bold$underline("Welch's ANOVA test"), " is most appropriate. A post-hoc test might be appropriate."))
        if(quiet){
          return(msg)
        }else {
          return(list(norm, test, plot1, msg))
        }
      }else if(any(res$p.value < 0.05, na.rm = T) == T){
        test <- stats::kruskal.test(var1 ~ group,
                             data = x)
        msg <- cat(paste0("Because the samples are independent and three or more groups were given as input (group and var1) and the normality test showed that the distribution of the data are not significantly different (Shapiro-Wilk normality test < 0.05), the function assume that a ", crayon::red$bold$underline("Kruskal-Wallis test"), " is most appropriate. A post-hoc test might be appropriate (See Dunn)."))
        if(quiet){
          return(msg)
        }else {
          return(list(test, msg))
        }
      }
    }
  else{
    stop("group should be character vector")
  }
  }
  
  # the red tree ------------------------------------------------------------
  else{
    cli::cli_alert_warning("This function for now does not handle more than two variables.")
  }
  
}