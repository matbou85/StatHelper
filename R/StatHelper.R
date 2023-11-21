
library(varhandle)
library(ggpubr)
library(RVAideMemoire)
# library(broom)
library(dplyr)
library(car)


StatHelper <- function(x = NULL,
                     y = NULL,
                     group = NULL,
                     paired = FALSE,
                     var1 = FALSE,
                     var2 = FALSE,
                     var3 = FALSE,
                     quiet = FALSE,
                     mu = 25,
                     ...){

  # preprocessing -----------------------------------------------------------
  ## construct our x to be dataframe
  {
    if(is.vector(x)){
      x <- data.frame(x = x, stringsAsFactors = FALSE)
      # if(is.null(y)){
      #   stop("If the 'x' is not a tabular data (i.e data.frame or matrix), then y should be provided")
      # }
    }
    else if(inherits(x, "matrix")){
      x <- data.frame(x)
    }
  }
  
  # the green tree ----------------------------------------------------------
  if(!missing(var1) & missing(var2) & missing(var3) & missing(group) & all(varhandle::check.numeric(var1))){
    res <- shapiro.test(var1)
    if(res$p.value > 0.05){
      plot1 <- ggqqplot(var1, ylab = "",
               ggtheme = theme_minimal())
      test <- t.test(var1, mu = mu)
      return(list(plot1, test))
    }else if(res$p.value < 0.05){
      plot1 <- ggqqplot(var1, ylab = "",
                        ggtheme = theme_minimal())
      test <- wilcox.test(var1, mu = mu)
      return(list(plot1, test))
    }

  # the blue tree -----------------------------------------------------------

# paired samples ----------------------------------------------------------
  }else if (paired){
    if(missing(var3) & sum(!sapply(cbind.data.frame(var1, var2), function(.){ all(varhandle::check.numeric(.)) })) == 2L){
      msg <- "Because the samples are paired and two character vectors were provided by the user then a McNemar's test seems to be nost appropriate."
      test <- mcnemar.test(var1, var2)
      return(list(msg, test))
    }else if(!missing(var3) & sum(!sapply(cbind.data.frame(var1, var2, var3), function(.){ all(varhandle::check.numeric(.)) })) == 3L){
      msg <- "Because the samples are paired and three character vectors were provided by the user then a Cochran's Q test seems to be nost appropriate."
      test <- cochran.qtest(var1 ~ var2 | var3,
                    data = x)
      return(list(msg, test))
    }else if(all(!varhandle::check.numeric(group))){
      if((length(unique(group)) == 2L) & (length(group) == length(var1))){
        xx <- cbind.data.frame(var1, group)
        res <- xx %>%
          group_by(group) %>% 
          summarise(p.value = shapiro.test(var1)$p.value)
        if(nrow(x) < 30L){
          if(all(res$p.value > 0.05) == T){
            norm <- xx %>%
              group_by(group) %>% 
              summarise(p.value = shapiro.test(var1)$p.value)
            stat1 <- t.test(var1 ~ group, data = x, paired = TRUE)
            return(list(norm, stat1))
          }
        }else if(all(res$p.value > 0.05) == F){
          wilcox.test(var1 ~ group,
                      data = x,
                      paired = TRUE,
                      alternative = "greater")
        }
      }else if((length(unique(group)) > 2L) & (length(group) == length(var1))){
        xx <- cbind.data.frame(var1, var2, group)
        res <- xx %>%
          group_by(group) %>% 
          summarise(p.value = shapiro.test(var1)$p.value)
        if(all(res$p.value > 0.05) == T){
          plot1 <- ggqqplot(var1, ylab = "",
                            ggtheme = theme_minimal())
          res.aov <- anova_test(data = xx, dv = var1, wid = var2, within = group)
          test <- get_anova_table(res.aov)
          return(list(plot1, test))
        }else if(any(res$p.value < 0.05) == T){
          plot1 <- ggqqplot(var1, ylab = "",
                            ggtheme = theme_minimal())
          test <- friedman_test(data = xx, var1 ~ group |var2)
          return(list(plot1, test))
        }
      }
    }
  }
# Independent samples -----------------------------------------------------
  else if(!missing(var1) & !missing(var2) &missing(group) & (sum(sapply(cbind.data.frame(var1, var2), function(.){ all(varhandle::check.numeric(.)) })) == 2L)){
        fisher.test(var1, var2,
                    alternative="two.sided")
  }else if(missing(var1) & missing(var2) & missing(var3) & missing(group) & (sum(sapply(x, function(.){ all(varhandle::check.numeric(.)) })) > 2L) & ncol(x) > 2){
    chisq.test(x, simulate.p.value = TRUE)
  }else if (all(!varhandle::check.numeric(group))){
    if((length(unique(group)) == 2L) & (length(group) == length(var1))){
      xx <- cbind.data.frame(var1, group)
      res <- xx %>%
        group_by(group) %>% 
        summarise(p.value = shapiro.test(var1)$p.value)
      var <- var.test(var1 ~ group, data = x)
      if((all(res$p.value > 0.05) == T) & var$p.value > 0.05){
        norm <- xx %>%
          group_by(group) %>% 
          summarise(p.value = shapiro.test(var1)$p.value)
        var <- var.test(var1 ~ group, data = x)
        stat1 <- t.test(var1 ~ group, var.equal = T)
        plot1 <- ggqqplot(var1, ylab = "",
                          ggtheme = theme_minimal())
        return(list(plot1, norm, var, stat1))
      }
      else if((all(res$p.value > 0.05) == T) & var$p.value < 0.05){
        norm <- xx %>%
          group_by(group) %>% 
          summarise(p.value = shapiro.test(var1)$p.value)
        var <- var.test(var1 ~ group, data = x)
        stat1 <- t.test(var1 ~ group, var.equal = T)
        plot1 <- ggqqplot(var1, ylab = "",
                          ggtheme = theme_minimal())
        return(list(plot1, norm, var, stat1))
      }
      else if(any(res$p.value < 0.05) == T){
        norm <- xx %>%
          group_by(group) %>% 
          summarise(p.value = shapiro.test(var1)$p.value)
        var <- var.test(var1 ~ group, data = x)
        stat1 <- wilcox.test(var1 ~ group,
                    data=x, exact = F)
        plot1 <- ggqqplot(var1, ylab = "",
                          ggtheme = theme_minimal())
        return(list(plot1, norm, var, stat1))
      } 
    }else if((length(unique(group)) > 2L) & (length(group) == length(var1))){
      xx <- cbind.data.frame(var1, group)
      var <- leveneTest(var1 ~ group, data = x)
      res <- xx %>%
        group_by(group) %>% 
        summarise(p.value = shapiro.test(var1)$p.value)
      if (any(var$`Pr(>F)` > 0.05) & all(res$p.value > 0.05) == T){
        norm <- xx %>%
          group_by(group) %>% 
          summarise(p.value = shapiro.test(var1)$p.value)
        res.aov <- aov(var1 ~ group,
            data = x)
        test <- summary(res.aov)
        plot <- plot(res.aov, 2)
        msg <- message("A post-hoc test might be appropriate")
        return(list(norm, test, plot, msg))
      }else if(any(var$`Pr(>F)` < 0.05)){
        norm <- xx %>%
          group_by(group) %>% 
          summarise(p.value = shapiro.test(var1)$p.value)
        test <- oneway.test(var1 ~ group,
                               data=x,
                               var.equal=FALSE)
        plot1 <- ggqqplot(var1, ylab = "",
                          ggtheme = theme_minimal())
        msg <- message("A post-hoc test might be appropriate (See Tukey, Dunnett etc.)")
        return(list(norm, test, plot1, msg))
      }else if(any(res$p.value < 0.05) == T){
        test <- kruskal.test(var1 ~ group,
                             data = x)
        msg <- message("A post-hoc test might be appropriate (See Dunn)")
        return(list(test, msg))
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




StatHelper(x = mtcars2, var1 = mtcars2$mpg, mu = 25) ## one sample t.test
StatHelper(x = data, var1 = data$before, var2 = data$after, paired = T) ## McNemar Test
StatHelper(x = Data, var1 = Data$Response, var2 = Data$Practice, var3 = Data$Student, paired = T) ## Cochran’s Q test
StatHelper(x = mtcars3) ## chi-square test of independence
StatHelper(x = Matriz, var1 = Matriz$Heron, var2 = Matriz$Egret) ## Fisher exact test
StatHelper(x = sleep2, group = sleep2$group, var1 = sleep2$extra) ## t.test for independent samples
StatHelper(x = welsh, group = welsh$group, var1 = welsh$x) ## Welsh t.test for independent samples
StatHelper(x = Mann, group = Mann$Speaker, var1 = Mann$Likert) ## Mann-Whitney U-test
StatHelper(x = my_data, group = my_data$group, var1 = my_data$weight, paired = T) ## paired-sample t.test
StatHelper(x = anova, group = anova$group, var1 = anova$weight) ## anova t.test
StatHelper(x = selfesteem, group = selfesteem$time, var1 = selfesteem$score, var2 = selfesteem$id, paired = TRUE) ## paired-sample t.test




