
library("epitools")
library("moments")
library("fBasics")

## Tool per l'EDA

### Funzioni analisi univariata


#```{r funzione-summary-categoriali}
summary_categorial <- function(variable, data=NULL, stats=F, plots=T, compact=T) {
  
  if (!is.null(data)) {
    varname <- variable
    variable <- data[[variable]]
    len <- nrow(data)
  } else {
    len <- length(variable)
    varname <- deparse(substitute(variable))
  }
  
  # (eventuale) print delle statistiche
  if (isTRUE(stats)) {
    print(paste("SINTESI DI", varname))
    print(summary(variable))
    print(table(variable)/len)
  }
  
  if (isTRUE(plots)) {
    if (isTRUE(compact)) {
      par(mfrow=c(1, 2))
    } else {
      par(mfrow=c(1, 3))
      
      # Barplot con freq assolute
      barplot(table(variable), main = paste("Distib. (abs) di", varname), xlab = varname)
    }
    
    # Barplot con freq relative
    barplot(table(variable)/len, main = paste("Distib. (rel) di", varname), xlab = varname)
    
    # grafico a torta
    pie(table(variable))
  
    par(mfrow=c(1, 1))
  }
}
#```


#```{r funzione-summary-numeriche}
summary_numerical <- function(variable, data=NULL, stats=F, plots=T, normCurve=T, compact=T) {
  
  if (!is.null(data)) {
    varname <- variable
    variable <- data[[variable]]
  } else {
    varname <- deparse(substitute(variable))
  }
  
  # (eventuale) print delle statistiche
  if (isTRUE(stats)) {
    
    print(paste("SINTESI DI", varname))
    
    print(summary(variable))
    
    print("Varianza e Deviazione Standard")
    print(var(variable))
    print(sqrt(var(variable)))
  }
  
  
  
  if(isTRUE(plots)) {
    if(!isTRUE(compact)) {  
    
      par(mfrow=c(1, 3))
      
      # istogramma con media e mediana
      hist(variable, main = paste("Hist (abs) di ", varname), xlab = varname)
      abline(v=mean(variable), col="blue")
      abline(v=median(variable), col="green")
    } else {
      par(mfrow=c(1, 2))
    }
    
    # boxplot
    boxplot(variable)
    
    
    # istogramma delle probabilità con curva di densità e distrib. normale sovrapposta
    x <- seq(min(variable), max(variable), length.out=2500)
    hist(variable, probability=T, main = paste("Hist (rel) di ", varname), xlab = varname)
    abline(v=mean(variable), col="blue")
    abline(v=median(variable), col="green")
    lines(density(variable), col="orange")
    
    # (eventuale) plot della normale
    if(isTRUE(normCurve)) {
      lines(x, dnorm(x, mean(variable), sd(variable)), col="red")
    }
    
    par(mfrow=c(1,1))
  }
}
#```


#```{r funzione-controllo-normalita}
normality_check <- function(variable, data=NULL, stats=F, # normTest=F, 
                            statsTest=F, plots=T, compact=T) {
  
  
  if (!is.null(data)) {
    varname <- variable
    variable <- data[[variable]]
  } else {
    varname <- deparse(substitute(variable))
  }
  
  # (eventuale) print delle statistiche
  if (isTRUE(stats)) {
    
    print(paste("NORMALITA' DI", varname))
    print("Media e Varianza del modello: ")
    print(mean(variable))
    print(var(variable))
    
    print("Ind. assimmetria e curtosi: ")
    print(moments::skewness(variable))
    print(moments::kurtosis(variable))
  }
  
  # (eventuale) test sulle statistiche
  if (isTRUE(statsTest)) {
    print(fBasics::dagoTest(variable))
  }
  
  # (eventuale) test di normalità
  # if (isTRUE(normTest)) {
    # print(ks.test(variable, pnorm))
  # }
  
  # (eventuale) qq-plot
  if (isTRUE(plots)) {
    
    # (eventuale) curva di densità con normale
    if(!isTRUE(compact)) {
      
      par(mfrow=c(1,2))
      
      x <- seq(min(variable), max(variable), length.out=2500)
      hist(variable, probability=T, main = paste("Hist (rel) di ", varname), xlab = varname)
      abline(v=mean(variable), col="blue")
      abline(v=median(variable), col="green")
      lines(density(variable), col="orange")
      lines(x, dnorm(x, mean(variable), sd(variable)), col="red")
    }
    
    qqnorm(variable)
    qqline(variable, col="red")
    
    par(mfrow=c(1,1))
  }
}
#```


### Analisi mutlivariata

#```{r funzione-correlazione-num-num}
correlation_check_num <- function(x, y, data=NULL, 
                                  stats=F, statsTest=F, 
                                  whichStats=c("pearson", "kendal"), 
                                  plots=T) {
  
  if (!is.null(data)) {
    xname <- x
    yname <- y
    x <- data[[x]]
    y <- data[[y]]
  } else {
    xname <- deparse(substitute(x))
    yname <- deparse(substitute(y))
  }
  
  # (eventuale) print delle statistiche
  if (isTRUE(stats)) {
    
    print(paste("ANALISI PRELIMINARE DELLA CORRELAZIONE", yname, "~", xname))
    
    print("Covarianza: ")
    print(cov(x, y))
    print("Indice di Pearson e di Spearman: ")
    for (s in whichStats) {
      print(cor(x, y, method = s))
    }
  }
  
  # (eventuali) test sulle statistiche
  if (isTRUE(statsTest)) {
    print(cor.test(x, y, method = s))
  }
  
  
  if (isTRUE(plots)) {
    # plot di correlazione tra var numeriche
    plot(y~x, main=paste(yname, "~", xname), xlab = xname, ylab = yname)
    abline(lm(y~x), col="red")
    lines(lowess(y~x), col="black")
  }
}
#```

#```{r funzione-correlazione-fattore-numero}
correlation_check_numfactor <- function(xfactor, y, data=NULL, stats=F, plots=T) {
  
  if (!is.null(data)) {
    xname <- xfactor
    yname <- y
    xfactor <- data[[xfactor]]
    y <- data[[y]]
  } else {
    xname <- deparse(substitute(xfactor))
    yname <- deparse(substitute(y))
  }
  
  # (eventuali) statistiche per gruppo 
  if (isTRUE(stats)) {
    
    print(paste("ANALISI PRELIMINARE DELLA CORRELAZIONE", yname, "~", xname))
    
    print("Media per gruppo: ")
    print(tapply(y, xfactor, mean))
    
    print("Mediana per gruppo: ")
    print(tapply(y, xfactor, median))
    
    print("Varianza per gruppo: ")
    print(tapply(y, xfactor, var))
    
    print("Deviazione standard per gruppo: ")
    print(tapply(y, xfactor, sd))
  }
  
  if (isTRUE(plots)) {
    boxplot(y~xfactor, main=paste(yname, "~", xname), xlab = xname, ylab = yname)
  }
  
}
#```

#```{r funzione-correlazione-fattore-fattore}
correlation_check_categorial <- function(x, y, data=NULL, stats=F, statsTest=F, plots=T) {
  
  if (!is.null(data)) {
    xname <- x
    yname <- y
    x <- data[[x]]
    y <- data[[y]]
  } else {
    xname <- deparse(substitute(x))
    yname <- deparse(substitute(y))
  }
  
  # (eventuali) statistiche per gruppo 
  if (isTRUE(stats)) {
    
    print(paste("ANALISI PRELIMINARE DELLA CORRELAZIONE", yname, "~", xname))
    
    print("Frequenze congiunte assolute")
    print(epitools::table.margins(table(x, y)))
    
    print("Frequenze congiunte relative")
    print(epitools::table.margins(table(x, y)/length(x)))
  }
  
  # (eventuale) test chi-quadro
  if (isTRUE(statsTest)) {
    print(chisq.test(table(x, y)))
  }
  
  # (eventuale) grafico a mosaico
  if(isTRUE(plots)) {
    mosaicplot(table(x, y), main=paste(yname, "~", xname), color = T)
  }
}
#```




