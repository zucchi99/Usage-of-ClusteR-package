
library("epitools")
library("moments")
library("fBasics")

## Tool per l'EDA

### Funzioni analisi univariata

#```{r summary-num-and-factor}
summary.num <- function(x, data=NULL, calcVar=T, calcIndexes=T) {
  
  if (!is.null(data)) {
    x <- data[[x]]
  } else {
    varname <- deparse(substitute(x))
  }
  
  s <- summary(x)
    
  if (isTRUE(calcVar)) {
      s["Var."] <- var(x)
      s["StdDev."] <- sd(x)
  }
    
  if (isTRUE(calcIndexes)) {
      s["Skew."] <- moments::skewness(x)
      s["Kurt."] <- moments::kurtosis(x)
  }
    
  return(s)
}
summary.factor <- function(x, data=NULL, calcRelFreq=T) {
  
  if (!is.null(data)) {
    x <- data[[x]]
  } else {
    varname <- deparse(substitute(x))
  }
  
  if (isTRUE(calcRelFreq)) {
    
    s <- rbind(table(x), table(x)/length(x))
    # s <- c()
    # s[["Abs"]] <- 
    # s[["Rel"]] <- 
    
    rownames(s) <- c("F.Abs.", "F.Rel.")
      
  return(s)
  } else {
    return(table(x))
  }
}
#```



#```{r summary-all}
summary.all <- function(x, calcVar=T, calcIndexes=T, calcRelFreq=T) {
  
  if (is.data.frame(x)) {
    
    res <- c()
    
    for (n in x.names(x)) {
      res[[n]] <- summary.all(x[[n]])
    }
    
    return(res)
    
  } else {
    
    if(is.factor(x)) {
      summary.num(x, calcVar=calcVar, calcIndexes=calcIndexes)
    } else {
      summary.factor(x, calcRelFreq=calcRelFreq)
    }
    
    
  }
}
#```


#```{r funzione-summary-categoriali}
gsummary.factor <- function(x, data=NULL, stats=F, 
                              plots=c("rel", "pie"), # values: abs | rel | pie  
                              pieLabels=T
                               ) {
  
  if (!is.null(data)) {
    varname <- x
    x <- data[[x]]
    len <- nrow(data)
  } else {
    len <- length(x)
    varname <- deparse(substitute(x))
  }
  
  # (eventuale) print delle statistiche
  if (isTRUE(stats)) {
    print(paste("SINTESI DI", varname))
    print(summary.factor(x))
  }
  
  if("abs" %in% plots) {
    # Barplot con freq assolute
      barplot(table(x), main = paste("Frequenze di", varname), xlab = varname)
  }
  
  if("rel" %in% plots) {
    # Barplot con freq relative
    barplot(table(x)/len, main = paste("Probabilità di", varname), xlab = varname)
  }
  
  if("pie" %in% plots) {
    # grafico a torta
    tbl.as.df <- as.data.frame(table(x))
    
    tbl.as.df$Prob <- tbl.as.df$Freq/len
    
    if(isTRUE(pieLabels)) {
      tbl.as.df$labels <- paste(tbl.as.df[[1]], ": f=", tbl.as.df$Freq, ", p=", round(tbl.as.df$Prob, 2), sep="")  
    } else {
      tbl.as.df$labels <- tbl.as.df[[1]]
    }
    
    pie(table(x), labels=tbl.as.df$labels, main=varname)
  }
}
# gsummary.factor(mtcars$am, stats = T)
#```




#```{r funzione-summary-numeriche}
gsummary.num <- function(x, data=NULL, stats=F, 
                         plots=c("rel", "box"), # values: abs | rel | box 
                         normCurve=F    # set T to overlay a norm curve over rel
                         ) {
  
  if (!is.null(data)) {
    varname <- x
    x <- data[[x]]
  } else {
    varname <- deparse(substitute(x))
  }
  
  # (eventuale) print delle statistiche
  s <- summary(x)
  
  if (isTRUE(stats)) {
    
    print(paste("SINTESI DI", varname))
    
    print(summary.num(x, calcVar = T, calcIndexes = T))
  }
  
  if("abs" %in% plots) {
    # istogramma con media e mediana
    hist(x, main = paste("Frequenze di", varname), xlab = varname)
    abline(v=mean(x), col="red")
    abline(v=median(x), col="green")
  }
  
  if("rel" %in% plots) {
    # istogramma delle probabilità con curva di densità e distrib. normale sovrapposta
    hist(x, probability=T, main = paste("Densità di", varname), xlab = varname)
    abline(v=mean(x), col="red")
    abline(v=median(x), col="green")
    lines(density(x), col="blue")
    
    if(isTRUE(normCurve)) {
      x <- seq(min(x), max(x), length.out=2500)
      lines(x, dnorm(x, mean(x), sd(x)), col="red")
    }
  }
  
  if("box" %in% plots) {
    # boxplot
    boxplot(x, main=varname, horizontal = T)
  }
}
# gsummary.num(mtcars$mpg, stats=T, normCurve = F)
#```





#```{r funzione-controllo-normalita}
gsummary.norm <- function(x, data=NULL, stats=F,
                            statsTest=F, # set true to show tests on symmetry and kurtosis
                            plots=c("qq") # valori: curve, qq
                            ) {
  
  
  if (!is.null(data)) {
    varname <- x
    x <- data[[x]]
  } else {
    varname <- deparse(substitute(x))
  }
  
  # (eventuale) print delle statistiche
  if (isTRUE(stats)) {
    
    print(paste("NORMALITA' DI", varname))
    print(summary.num(x, calcVar = T, calcIndexes = T)[c("Mean", "Var.", "Skew.", "Kurt.")])
  }
  
  # (eventuale) test sulle statistiche
  if (isTRUE(statsTest)) {
    print(fBasics::dagoTest(x))
  }
  
  if ("curve" %in% plots) {
    # istogramma delle probabilità con curva di densità e distrib. normale sovrapposta
    hist(x, probability=T, main = paste("Densità di", varname), xlab = varname)
    abline(v=mean(x), col="red")
    abline(v=median(x), col="green")
    lines(density(x), col="blue")
    
    x <- seq(min(x), max(x), length.out=2500)
    lines(x, dnorm(x, mean(x), sd(x)), col="red")
  }
  
  if ("qq" %in% plots) {
    qqnorm(x)
    qqline(x, col="red")
  }
}
# gsummary.norm(mtcars$mpg, plots = c("curve", "qq"), stats=T, statsTest = T)
#```


### Analisi mutlivariata

#```{r funzione-correlazione-num-num}
cor.check.num <- function(x, y, data=NULL, 
                                  stats=F, 
                                  whichStats=c("pearson", "spearman"), # pearson, spearman, kenadal
                                  whichTests=c(), # pearson, spearman, kenadal
                                  doPlot=T) {
  
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
    
    s <- table(c())
    
    # s["Cov."] <- cov(x, y)
    
    for (stat in whichStats) {
      
      s[stat] <- cor(x, y, method = stat)
    }
    
    print(s)
  }
  
  # (eventuali) test sulle statistiche
  for (t in whichTests) {
    print(cor.test(x, y, method = t))
  }
  
  
  if (isTRUE(doPlot)) {
    # plot di correlazione tra var numeriche
    plot(y~x, main=paste(yname, "~", xname), xlab = xname, ylab = yname)
    abline(lm(y~x), col="red")
    lines(lowess(y~x), col="blue")
  }
}
# cor.check.num(x="disp", y="mpg", data=mtcars, stats = T, whichTests = c("pearson"))
#```



#```{r}
# pairs(mtcars[, c("mpg", "disp", "hp", "drat")], panel=panel.smooth)
# cormatrix <- cor(mtcars[, c("mpg", "disp", "hp", "drat")], method = "pearson")
# cormatrix
#```




#```{r funzione-correlazione-fattore-numero}
cor.check.factor.num <- function(xfactor, y, data=NULL, stats=F, doPlot=T, doPlotLines=F, 
                                 doMeanTest=F, doAnova=F) {
  
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
    
    print(tapply(y, xfactor, summary.num))
    
    # s[["Mean"]] <- tapply(y, xfactor, mean)
    # s[["Median"]] <- tapply(y, xfactor, median)
    # s[["Var."]] <- tapply(y, xfactor, var)
    # s[["Std.Dev."]] <- tapply(y, xfactor, sd)
    # print(s)
  }
  
  if(isTRUE(doMeanTest)) {
    print(t.test.between.groups(xname, yname, data=data))
  }
  
  if(isTRUE(doAnova)) {
    m <- aov(y~xfactor)
    print(summary(m))
    # print(summary.lm(m))
  }
  
  # (eventua)
  
  if (isTRUE(doPlot)) {
    boxplot(y~xfactor, main=paste(yname, "~", xname), xlab = xname, ylab = yname, col=NULL)
    
    if (isTRUE(doPlotLines)) {
      abline(lm(y~xfactor), col="red")
      # lines(lowess(y~xfactor), col="blue")
    }
  }
}
# cor.check.factor.num(xfactor = "carb", y="mpg", data=mtcars, stats=T, doPlot=T, doPlotLines=T)
#```

#```{r funzione-correlazione-fattore-fattore}
cor.check.factor <- function(x, y, data=NULL, stats=F, doTest=F, doPlot=T) {
  
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
    
    s <- c()
    
    s[["Freq."]] <- epitools::table.margins(table(x, y))
    s[["Prob."]] <- epitools::table.margins(table(x, y)/length(x))
    
    print(s)
  }
  
  # (eventuale) test chi-quadro
  if (isTRUE(doTest)) {
    print(chisq.test(table(x, y)))
  }
  
  # (eventuale) grafico a mosaico
  if(isTRUE(doPlot)) {
    mosaicplot(table(x, y), main=paste(yname, "~", xname), color = T)
  }
}
# cor.check.factor("cyl", "am", data=mtcars, stats = T, doTest = T, doPlot = T)
#```

#```{r}
t.test.between.groups <- function(groupname, varname, data, 
                                  type="pairs" # pairs | base | progressive
                                  ) {
  
  if (!is.null(data)) {
    group <- data[[groupname]]
    y <- data[[varname]]
  } else {
    print("ERROR, data è obbligatorio")
  }
  
  # estraggo i lv. 
  lvls <- levels(factor(data[[groupname]]))
    
  res <- c()
  
  if (type=="pairs") {
    
    # versione 1: confronti tra tutte le coppie di gruppi
    i<-1
    while(i<=length(lvls)-1) {
      
      j<-i+1
      while(j<=length(lvls)){
        res[[paste("<", lvls[i], ",", lvls[j], ">", sep="")]] <- 
          t.test(data[data[[groupname]]==lvls[i], varname], data[data[[groupname]]==lvls[j], varname])
        j<-j+1
      }
      
      i<-i+1
    }
    
    return(res)
    
  } else if (type=="base") {
    
    # versione 2: confronti progressivi tra tutti i gruppi e quello di base
    i<-2
    while(i<=length(lvls)) {
      
      res[[paste("<", lvls[1], ",", lvls[i], ">", sep="")]] <- 
          t.test(data[data[[groupname]]==lvls[1], varname], data[data[[groupname]]==lvls[i], varname])
      
      i<-i+1
    }
    
    return(res)
  
  } else if (type=="progressive") {
    # versione 3: confronti tra ogni gruppo e quello successivo 
    i<-1
    while(i<=length(lvls)-1) {
      
      res[[paste("<", lvls[i], ",", lvls[i+1], ">", sep="")]] <- 
          t.test(data[data[[groupname]]==lvls[i], varname], data[data[[groupname]]==lvls[i+1], varname])
      
      i<-i+1
    }
    
    return(res)
  }
  
  return("ERROR: nessun opzione type selezionata. Scegli tra pairs, base e progressive.")
}
#```

## Altro

#```{r}
glm.mse.train <- function(glmodel) {
  return(sum(resid(glmodel)^2)/length(glmodel[["fitted.values"]]))
}
glm.mse.test.cv <- function(glmodel) {
  return(boot::cv.glm(glmodel[["model"]], glmodel)$delta[2])
}
#```








