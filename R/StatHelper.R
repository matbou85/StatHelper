
library(varhandle)
library(ggpubr)

StatHelp <- function(x,
                     y = NULL,
                     group = NULL,
                     paired = FALSE,
                     var1 = FALSE,
                     var2 = FALSE,
                     quiet = FALSE,
                     ...){

  ## collect information -----------------------------------------------------
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
      x = 12, # counts of successes
      n = 15, # total counts (12 + 3)
      p = 0.5 # expected proportion
    )
    binom.test(
      x = 12, # counts of successes
      n = 15, # total counts (12 + 3)
      p = 0.5 # expected proportion
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
      mcnemar.test(x[[1]], x[[2]])
    }
  }

# Independent samples -----------------------------------------------------
  else if((sum(sapply(x, function(.){ all(varhandle::check.numeric(.)) })) == 2L) & ncol(x) == 2L){
        fisher.test(x,
                    alternative="two.sided")
  }else if ((all(sapply(x, function(.){ all(varhandle::check.numeric(.)) }))) & ncol(x) > 2){
    chisq.test(x, simulate.p.value = TRUE)
  }else if (is.character(group)){
    if((length(unique(group)) == 2L) & (length(group) == length(var1))){
      if(shapiro.test(var1)$p > 0.05){
        norm1 <- shapiro.test(var1)
        stat1 <- t.test(var1 ~ group, var.equal = T)
        plot1 <- ggqqplot(var1, ylab = "",
                          ggtheme = theme_minimal())
        return(list(plot1, norm1, stat1))
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




# my_chisq.test <- function(x){
#   
# }



# StatHelp(df = iris) ## 
StatHelp(x = xtab) ## prop.test
StatHelp(x = mtcars2) ## one sample t.test
StatHelp(x = iris2) ## One sample Chi-test


StatHelp(x = data, paired = T) ## McNemar Test
StatHelp(x = mtcars3) ## chi-square test of independence
StatHelp(x = Matriz) ## Fisher exact test
StatHelp(x = sleep2, group = sleep2$group, var1 = sleep2$extra) ## t.test for independent samples

# StatHelp(x = mtcars4, cor = T) ## correlation



## For prop.test
{
  grp.size <- c( 106, 113, 156, 102 )
  smokers  <- c( 50, 100, 139, 80 )
  no.smokers <- grp.size - smokers
  xtab <- as.data.frame(rbind(
    smokers,
    no.smokers
  ))
  dimnames(xtab) <- list(
    Smokers = c("Yes", "No"),
    Groups = c("grp1", "grp2", "grp3", "grp4")
  )
  xtab <- xtab[,c(1,2)]
  xtab
  # library(data.table)
  # setDT(xtab, keep.rownames = TRUE)[]
  # xtab
  # load iris dataset
  dat <- iris
  
  # create size variable
  dat$size <- ifelse(dat$Sepal.Length < median(dat$Sepal.Length),
                     "small", "big"
  )
  
  # show first 5 observations
  head(dat, n = 5)
  dat <- table(dat$size)
  df <- as.data.frame.matrix(dat)
  # one-proportion test
  test <- prop.test(
    x = 77, # number of successes
    n = 150, # total number of trials (77 + 73)
    p = 0.5 # we test for equal proportion so prob = 0.5 in each group
  )
  test
  
}



## For t.test
{
  library(data.table)
  class(mtcars)
  mtcars2 <- copy(mtcars)
  # mtcars2 <- setDT(mtcars2, keep.rownames = TRUE)[]
  mtcars2$HeaderName <- row.names(mtcars2)
  length(mtcars2$rn)
  length(unique(mtcars2$rn))
  length(unique(mtcars2$mpg))
  mtcars2 <- mtcars2[,c(1,12)]
  norm <- shapiro.test(mtcars2$mpg)
  test <- t.test(mtcars2[[1]], mu = 20)
  length(unique(mtcars2[,sapply(mtcars2, is.character)])) > 2
  length(unique(mtcars2$HeaderName)) > 2 
  
  length(unique(mtcars2[,sapply(mtcars2, is.character)])) > 2
  suppressWarnings({shapiro.test(mtcars2$mpg)})
  t.test(x, mu = mu)
  
}

## For One sample Chi-square test 
{
  library(data.table)
  class(iris)
  iris2 <- copy(iris)
  iris2 <- as.data.frame(iris2[,2])
  # table(iris2$Species)
  chisq.test(iris2)
  
}


## Mc Nemar Test
{
  set.seed(150)
  data <- data.frame(before = sample(c("Positive",
                                       "Positive",
                                       "Positive",
                                       "Positive",
                                       "Negative"),
                                     300, replace = TRUE),
                     after = sample(c("Positive",
                                      "Positive",
                                      "Positive",
                                      "Positive",
                                      "Negative"),
                                    300, replace = TRUE))
  
  data
  mcnemar.test(data$before, data$after)
  ncol(data[sapply(data, is.character)]) == 2
  sum(!sapply(data, function(.){ all(varhandle::check.numeric(.)) }))
  ncol(data) == 2
  mcnemar.test(data[[1]], data[[2]])
}


## For Chi-square test 
{
  library(data.table)
  class(mtcars)
  mtcars3 <- copy(mtcars)
  chisq.test(mtcars3)
  
}

##For t.test
{
  sleep2 <- copy(sleep)
  sleep2$group <- gsub("1", "A", gsub("2", "B", sleep$group))
  sleep2
  group <- sleep2[[2]]
  var1 <- sleep2[[1]]
  length(unique(group)) == 2L 
  length(group) == length(var1)
  is.character(group)
  shapiro.test(var1)$p > 0.05
  shapiro.test(var1)
  t.test(var1 ~ group, var.equal = T)
}




## Fisher exact test
{
  Input =("
 Site          Heron  Egret      
 Vegetation    15     8
 Shoreline     20     5
 Water         14     7
 Structures     6     1
")
  
  Matriz = as.matrix(read.table(textConnection(Input),
                                header=TRUE,
                                row.names=1))
  
  Matriz
  Matriz <- as.data.frame(Matriz)
  
  fisher.test(Matriz,
              alternative="two.sided")
  
}

## Correlation
{
  class(mtcars)
  mtcars4 <- copy(mtcars)
  mtcars4 <- mtcars4[,c(1,6)]
  
  
}








