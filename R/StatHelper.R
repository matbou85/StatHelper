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
                     ...){

  # # collect information -----------------------------------------------------
  # {
  #   init_col_classes <- sapply(x, class)
  #   init_col_can_be_numeric <- sapply(x, function(.){ all(varhandle::check.numeric(.)) })
  # }


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
  if(ncol(x) == 1L){
    if(all()){
      res <- my_chisq.test(x)
    }
  }else if(ncol(x) == 2L & nrow(x) == 2L){
    prop.test(
      x = var1, # counts of successes
      n = var2, # total counts
      p = p # expected proportion
    )
    binom.test(
      x = var1, # counts of successes
      n = var2, # total counts
      p = p # expected proportion
    )
  }else if (nrow(x) == 1L){
    chisq.test(dat$Freq, # observed frequencies
               p = df$Expected_relative_freq # expected proportions
    )
    
  }
  
  # the blue tree -----------------------------------------------------------

# paired samples ----------------------------------------------------------
  else if (paired){
    if((sum(!sapply(x, function(.){ all(varhandle::check.numeric(.)) })) == 2L) & ncol(x) == 2L){
      mcnemar.test(var1, var2)
    }else if((sum(!sapply(x, function(.){ all(varhandle::check.numeric(.)) })) != 2L) & ncol(x) != 2L){
      cochran.qtest(var1 ~ var2 | var3,
                    data = x)
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
            stat1 <- t.test(weight ~ group, data = my_data, paired = TRUE)
            return(list(norm, stat1))
          }
        }else if(all(res$p.value > 0.05) == F){
          wilcox.test(var1 ~ group,
                      data = x,
                      paired = TRUE,
                      alternative = "greater")
        }
      }
    }
  }
# Independent samples -----------------------------------------------------
  else if((sum(sapply(x, function(.){ all(varhandle::check.numeric(.)) })) == 2L) & ncol(x) == 2L){
        fisher.test(x,
                    alternative="two.sided")
  }else if ((all(sapply(x, function(.){ all(varhandle::check.numeric(.)) }))) & ncol(x) > 2){
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
    }else if((length(unique(group)) != 2L) & (length(group) == length(var1))){
      norm <- leveneTest(weight ~ group, data = my_data)
      if (any(norm$`Pr(>F)` > 0.05)){
        aov(var1 ~ group,
            data = x)
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
  
  
  # return(x)
}

