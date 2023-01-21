
# dataset
# by library
library(DAAG)
data(possum)
# by direct import as list
caff.marital <- matrix(c(652,1537,598,242,36,46,38,21,218,327,106,67),nrow=3,byrow=T)
colnames(caff.marital) <- c("0","1-150","151-300",">300")
rownames(caff.marital) <- c("Married","Prev.married","Single")
names(dimnames(caff.marital)) <- c("Marital status","Caffeine consumption in mg/day")

# dataset use
table = t(caff.marital)
column = "Married"
colors = c("white","grey80","grey50","black")
table

# FUNCTIONS

# create range column (same as python range) (useful as x-axis for a function)
seq(from = 45, to = 60, by = 0.01)

# transpose table
t(table)

# scale data in range (0,1) 
table_scaled = prop.table(table, 2)

# with: Evaluate an Expression in a Data Environment
# for example filter a column on value of another column
totlngth_filtered <- with(possum, totlngth[sex=="f"])
totlngth_filtered

# summary(column): 
# - if numeric:     min, mean, median, max, quantiles of the column
# - if categorical: count per category
# summary(table): summary(col) for each column of table
summary(possum$totlngth)
summary(possum$sex)

# probability density functions (to be used inside a line):
# NB: to be used with seq(start, end, step)
# normal   distribution: dnorm,  pnorm,  qnorm,  rnorm
# binomial distribution: dbinom, pbinom, qbinom, rbinom
# d ==> density/distribution
# p ==> 
# q ==> quantile
  # NB lower.tail = TRUE:  si considera coda sx
  # NB lower.tail = FALSE: si considera coda dx
  # stesso risultato si ottiene con qnorm(1-p, left, right, lower.tail = FALSE)
# r ==> simulate a sample con dimension n

# density of a column (to be used inside a line):
# density

# PLOTS

# set graphical parameters
rows = 1
cols = 1
char_size = 1
bottom = left = top = right = 5 #margins
par(
  mfrow = c(rows,cols),  # set grid size (for seeing multiple plots)
  mex   = char_size,     # default char size is multiplied by char_size
  mar   = c(bottom, left, top, right), # plot margins to border
  pty   = "s" # square  area plot
  #pty  = "m" # maximum area plot (default)
)

# barplot
barplot(
  table_scaled,
  beside      = TRUE,  # columns side by side
  #beside     = FALSE, # columns stacked (default)
  legend.text = rownames(table), 
  col         = colors
)

# pie
slices = t(table)[column,]
pie(
  slices,
  #labels = lbls, # legenda campi
  main    = "Pie Chart Title",
  col     = colors
)

# histogram
hist(
  nhtemp, 
  freq  = FALSE, # relative frequencies of the column (min-max scaled)
  #freq = TRUE,  # absolute frequencies of the column (default)
  main  = "Temperatures"
)

# LINES
# add straight line to a plot: y = a*x + b
abline(
  v   = mean(nhtemp), # column
  col = 'green',      # color
  lwd = 2             # line width
)
# add density line
lines(
  density(nhtemp),
  lwd=2
)
# add line of a function (for ex. normal distribution)
lines(
  seq(45, 60, 0.01),
  dnorm(
    seq(45,60,0.01),
    mean(nhtemp),
    sqrt(var(nhtemp))
  ),
  col='red', lwd=2
)

# boxplot
boxplot(
  # column(y) ~ groupby_category(x)
  footlgth ~ sex,
  data = possum,
  xlab = "Sex",
  ylab = "Foot Length"
)

# scatter
plot(
  totlngth ~ hdlngth, 
  data = possum,
  pch = 16
)    

