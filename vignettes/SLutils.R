## ----packages, include=FALSE---------------------------------------------
library(SLutils)
breakpoints <- SLutils::breakpoints
library(magrittr)
library(ggplot2)
library(car)
library(jsonlite)
library(microbenchmark)

## ------------------------------------------------------------------------
numextract("I am born in 1975 and I am 41 years old.")

## ------------------------------------------------------------------------
numextract(c("3.5 ml", "7 ml"))

## ------------------------------------------------------------------------
numextract(factor(c("Batch 101", "Batch 217")))

## ------------------------------------------------------------------------
numsextract("I am born in 1975 and I am 41 years old.")

## ---- error=TRUE---------------------------------------------------------
Numerize(" -3.5")
Numerize("Batch 101")

## ---- error=TRUE---------------------------------------------------------
Numerize(c("-3.5", "42"))
Numerize(c("-3.5", "fourty-two"))

## ------------------------------------------------------------------------
Numerize(factor(c("00", "01")))

## ------------------------------------------------------------------------
dat <- data.frame(u = 1:4,
                  v = c(2.5, -4, 0, 10),
                  w = c("a","b","c","d"),
                  x = factor(c("100", "100", "101", "110")),
                  y = factor(c("Test A", "Test A", "Test B", "Test C")),
                  z = c("1", "1.4", "-3", "   0"),
                  stringsAsFactors = FALSE)
str(dat)

## ------------------------------------------------------------------------
Numerize.df(dat) %>% str

## ------------------------------------------------------------------------
Numerize.df(dat, factors=FALSE) %>% str

## ------------------------------------------------------------------------
Numerize.df(dat, colnames=c("x", "z")) %>% str

## ------------------------------------------------------------------------
isNumerizable(c("1.5", "  3.2 "))
isNumerizable(factor(c("3.5", "4")))
isNumerizable(c("100", "AAA"))

## ------------------------------------------------------------------------
dat <- data.frame(x = 1:4,
                  y = c("a", "b", "c", "d"),
                  z = c("1", "1.4", "-3", "   0"))
isNumerizable.df(dat)

## ------------------------------------------------------------------------
coltypes(dat)

## ------------------------------------------------------------------------
ndecimals(13.451)
ndecimals(2)
ndecimals(pi)
ndecimals(1.23e-20)

## ------------------------------------------------------------------------
nfloordigits(13.451)
nfloordigits(2)
nfloordigits(-43.4)
nfloordigits(23.15e6)

## ------------------------------------------------------------------------
prettyNumber(pi, digits=4)
prettyNumber(123456789, digits=7)
prettyNumber(12.5e9, digits=7)
prettyNumber(0.00000012, digits=7)

## ----number2words--------------------------------------------------------
numbers2words(c(2, 34, 175))

## ----formatClatex--------------------------------------------------------
formatClatex(1.234567, digits=4)
formatClatex(1.234567e-12, digits=4)

## ------------------------------------------------------------------------
numbers2words(c(2, 17, 211))

## ------------------------------------------------------------------------
enumerate(letters[1:2])
enumerate(letters[1:4])

## ------------------------------------------------------------------------
x <- Sys.Date()
is_date(x)
is_date(factor(x))
x <- strftime(c("2016-06-21", "2016-11-22"), usetz=TRUE, tz="UTC")
is_date(x)
is_date(as.POSIXct(x, tz="UTC"))

## ------------------------------------------------------------------------
x <- Sys.Date()
possibly_date(x)
x <- strftime(c("2016-06-21", "2016-11-22"), usetz=TRUE, tz="UTC")
possibly_date(x)
possibly_date(factor(x))
possibly_date(as.POSIXct(x, tz="UTC"))
x <- Sys.time()
possibly_date(x)
x <- "2016/11/11"
possibly_date(x)
x <- "a"
possibly_date(x)
x <- "11nov1980"
possibly_date(x)

## ------------------------------------------------------------------------
as.Date("11nov1980", "%d%b%Y") # gives NA in some locales
Sys.setlocale("LC_TIME", "C")
as.Date("11nov1980", "%d%b%Y")

## ------------------------------------------------------------------------
guessDateFormat(c("2016/06/21", "2016/11/22"))
guessDateFormat("11nov1980")

## ------------------------------------------------------------------------
guessDateFormat("abc")

## ------------------------------------------------------------------------
guessDateFormat(c("2016/06/21", "2016/11/22"), returnDates=TRUE)

## ---- eval=FALSE---------------------------------------------------------
#  XLSXwrite0(iris, mtcars, file="test.xlsx",
#            author="John Doe", title="MyTitle")

## ------------------------------------------------------------------------
( flatlist <- jsonlite::fromJSON("{\"COL1\":[1,null,2],\"COL2\":[\"c\",4],\"Date\":[\"2017-02-12\"]}", simplifyVector=FALSE) )

## ------------------------------------------------------------------------
comments <- jsonlite::fromJSON("{\"COL1\":[\"hello\",null,null],\"COL2\":[null,\"bye\"],\"Date\":[null]}", simplifyVector=FALSE)

## ------------------------------------------------------------------------
xlsx <- tempfile(fileext=".xlsx")
writeXLSX(flatlist, file=xlsx, comments=comments, sheetname="Sheet1")

## ------------------------------------------------------------------------
sheet <- getExcelSheet(xlsx, sheet="Sheet1")
sheetToJSON(sheet)

## ------------------------------------------------------------------------
treadXLSX(sheet)

## ------------------------------------------------------------------------
hreadXLSX(xlsx, sheet="Sheet1", what="data", output="flatlist")

hreadXLSX(xlsx, sheet="Sheet1", what="data", output="dataframe")

hreadXLSX(xlsx, sheet="Sheet1", what="data", output="JSON")

## ------------------------------------------------------------------------
hreadXLSX(xlsx, sheet="Sheet1", what="data,comments", output="flatlist")

hreadXLSX(xlsx, sheet="Sheet1", what="data,comments", output="JSON")

## ------------------------------------------------------------------------
library(microbenchmark)
microbenchmark(
  tidyxl = treadXLSX(getExcelSheet(xlsx, sheet="Sheet1")),
  haskell = hreadXLSX(xlsx, sheet="Sheet1", what="data,comments,types", output="flatlist"),
  times=20
)
microbenchmark(
  tidyxl = sheetToJSON(getExcelSheet(xlsx, sheet="Sheet1")),
  haskell = hreadXLSX(xlsx, sheet="Sheet1", what="data,comments,types", output="JSON"),
  times=20
)

## ------------------------------------------------------------------------
xlsx <- system.file("extdata", "datasets.xlsx", package="readxl")
library(microbenchmark)
microbenchmark(
  tidyxl = treadXLSX(getExcelSheet(xlsx, sheet="iris")),
  haskell = hreadXLSX(xlsx, sheet="iris", what="data,comments,types", output="flatlist"),
  times=20
)
microbenchmark(
  tidyxl = sheetToJSON(getExcelSheet(xlsx, sheet="iris")),
  haskell = hreadXLSX(xlsx, sheet="iris", what="data,comments,types", output="JSON"),
  times=20
)

## ------------------------------------------------------------------------
fit <- lm(yield ~ block + N + block:N, data=npk)
typeIItable(fit)

## ---- eval=require(car)--------------------------------------------------
car::Anova(fit)

## ------------------------------------------------------------------------
ncp <- 40
pt(q=30, df=10, ncp=ncp)
pT(q=30, df=10, ncp=ncp)
qt(p=0.5, df=10, ncp=ncp)
qT(p=0.5, df=10, ncp=ncp)

## ----headdat-------------------------------------------------------------
data(dataBP)
head(dataBP)

## ----bpggplot1, fig.width=fscale*6, fig.height=fscale*3.5----------------
gg <- ggplot(dataBP, aes(x=date, y=y)) + geom_point()
gg

## ----deg0_onebp----------------------------------------------------------
# find breakpoint
bp <- breakpoints(y~1, dat=dataBP, onebreak=TRUE)			
# number of breakpoints (here, 0 or 1)
( n.bp <- length(bp$breakpoint) )
paste("Breakpoint:", dataBP$date[bp$breakpoint])

## ----bpggplot_deg0_onebp, fig.width=fscale*6, fig.height=fscale*3.5------
breakdates <- c(dataBP$date[1], dataBP$date[bp$breakpoint], tail(dataBP$date,1))
x <- breakdates[1:2]
xend <- breakdates[2:3]
y <- yend <- bp$coef[1:2]
segs <- data.frame(x,xend,y,yend)
gg + 
  geom_segment(data=data.frame(mygroup=c(NA,NA)),
               aes(x=segs[,1], y=segs[,3], xend=segs[,2], yend=segs[,4]), 
               colour="red", linetype="solid", size=1.2) 

## ----deg0_severalbp------------------------------------------------------
# find breakpoints
bp <- breakpoints(y~1, dat=dataBP, onebreak=FALSE, nbreaks.max=NULL)			
# number of breakpoints
( n.bp <- length(bp$breakpoint) )
paste("Breakpoints:", paste(dataBP$date[bp$breakpoint], collapse=", "))

## ----bpggplot_deg0_severalbp, fig.width=fscale*6, fig.height=fscale*3.5----
breakdates <- c(dataBP$date[1], dataBP$date[bp$breakpoint], tail(dataBP$date,1))
x <- breakdates[1:(n.bp+1)]
xend <- breakdates[1+1:(n.bp+1)]
y <- yend <- bp$coef[1:(n.bp+1)]
segs <- data.frame(x,xend,y,yend)
gg + 
  geom_segment(data=data.frame(mygroup=rep(NA, n.bp+1)),
               aes(x=segs[,1], y=segs[,3], xend=segs[,2], yend=segs[,4]), 
               colour="red", linetype="solid", size=1.2) 

## ----deg1_onebp----------------------------------------------------------
dataBP <- transform(dataBP, dateID=as.integer(date))
# find breakpoint
bp <- breakpoints(y~dateID, dat=dataBP, onebreak=TRUE)			
# number of breakpoints (here, 0 or 1)
( n.bp <- length(bp$breakpoint) )
paste("Breakpoint:", dataBP$date[bp$breakpoint])

## ----bpggplot_deg1_onebp, fig.width=fscale*6, fig.height=fscale*3.5------
breakdates <- c(dataBP$date[1], dataBP$date[bp$breakpoint], tail(dataBP$date,1))
nbreakdates <- c(dataBP$dateID[1], dataBP$dateID[bp$breakpoint], tail(dataBP$dateID,1))
a <- bp$coef[,1]
b <- bp$coef[,2]
x <- breakdates[1:2]
xend <- breakdates[2:3]
y <- a+b*nbreakdates[1:2]
yend <- a+b*nbreakdates[2:3]
segs <- data.frame(x,xend,y,yend)
gg + 
  geom_segment(data=data.frame(mygroup=rep(NA,2)),
               aes(x=segs[,1], y=segs[,3], xend=segs[,2], yend=segs[,4]), 
               colour="red", linetype="solid", size=1.2) 

## ----deg1_severalbp------------------------------------------------------
# find breakpoints
bp <- breakpoints(y~dateID, dat=dataBP, onebreak=FALSE, nbreaks.max=NULL)			
# number of breakpoints 
( n.bp <- length(bp$breakpoint) )
paste("Breakpoints:", paste(dataBP$date[bp$breakpoint], collapse=", "))

## ----bpggplot_deg1_severalbp, fig.width=fscale*6, fig.height=fscale*3.5----
breakdates <- c(dataBP$date[1], dataBP$date[bp$breakpoint], tail(dataBP$date,1))
nbreakdates <- c(dataBP$dateID[1], dataBP$dateID[bp$breakpoint],  tail(dataBP$dateID,1))
a <- bp$coef[,1]
b <- bp$coef[,2]
x <- breakdates[1:(n.bp+1)]
xend <- breakdates[1+1:(n.bp+1)]
y <- a+b*nbreakdates[1:(n.bp+1)]
yend <- a+b*nbreakdates[1+1:(n.bp+1)]
segs <- data.frame(x,xend,y,yend)
gg + 
  geom_segment(data=data.frame(mygroup=rep(NA,n.bp+1)),
               aes(x=segs[,1], y=segs[,3], xend=segs[,2], yend=segs[,4]), 
               colour="red", linetype="solid", size=1.2) 

## ---- eval=FALSE---------------------------------------------------------
#  makeRepo("U:/CRANrepo", name="myCRANrepo")

## ---- eval=FALSE---------------------------------------------------------
#  addPackage("U:/CRANrepo", "mypackage.tar.gz")

## ------------------------------------------------------------------------
string2letters("hello")

## ------------------------------------------------------------------------
charseq(12)

## ------------------------------------------------------------------------
charseq(12, prefix="graph", suffix=".png")

## ------------------------------------------------------------------------
M1 <- diag(2); M2 <- matrix(2, nrow=3, ncol=3)
blockdiag(M1,M2)
blockdiag_list(list(M1,M2,M1))

## ---- eval=FALSE---------------------------------------------------------
#  Sys.which("convert")
#  ##                              convert
#  ## "C:\\Windows\\system32\\convert.exe"

## ----Rplot01-------------------------------------------------------------
# generates a pdf figure
pdf("imgs/Rplot01.pdf", width=5, height=5)
curve(x^2, from=-1, to=1, axes=FALSE, ylab=NA, col="red")
xlabs_at <- seq(-1, 1, by=0.5)
axis(1, at=xlabs_at)
ylabs_at <- seq(0, 1, by=0.2)
axis(2, at=ylabs_at)
text(0.65, 0.8, expression(f(x)==x^2))
dev.off()
# converts to png
pspdf2img("imgs/Rplot01.pdf", format="png", moreopts="-trim -resize 30%")

## ----Rplot02, message=FALSE----------------------------------------------
# first write a function creating the plot
plotCode <- function(){
  curve(x^2, from=-1, to=1, axes=FALSE, xlab="$x$", ylab=NA, col="red")
  xlabs_at <- seq(-1, 1, by=0.5)
  axis(1, at=xlabs_at, labels=dollarify()(xlabs_at))
  ylabs_at <- seq(0, 1, by=0.2)
  axis(2, at=ylabs_at, labels=dollarify()(ylabs_at))
  text(0.65, 0.8, "$f(x)=x\\\\^2$") # note the four backslashes!
  return(invisible())
}
# create the LaTeX file with default preamble for pdflatex
plot2tikz(plotCode, filename="Rplot02", outdir="./imgs", 
          compile=TRUE, clean=TRUE,
          documentDeclaration ="\\documentclass[12pt]{standalone}\n",
          width=5, height=5)

## ----convertRplot02------------------------------------------------------
pspdf2img("imgs/Rplot02.pdf", format="png", moreopts="-trim -resize 30%")

## ------------------------------------------------------------------------
library(ggplot2)
set.seed(666)
DF <- data.frame(date = rep(Sys.Date()+1:10, 2),
                 y = rpois(20, 50),
                 group = gl(2, 10, labels=c("A","B")))
base_size <- 20
gg <- ggplot(DF, aes(x=date, y=y)) +
  geom_point(size=3, color="white", shape=21, fill="green") +
  scale_y_continuous(labels=dollarify()) +
  scale_x_date(date_breaks="2 days", labels=datify()) +
  # the labeller below is a trick to control the width of the strips
  # see http://stackoverflow.com/questions/17825201/is-there-a-way-to-increase-the-height-of-the-strip-text-bar-in-a-facet
  facet_grid(group ~ ., labeller=labeller(group=setNames(paste0("\ngroup ", levels(DF$group), "\n"), levels(DF$group)))) +
  xlab("\\large\\textsf{date}") +
  ylab("\\Large $y$") +
  theme(panel.background = element_rect(fill="black", color=NA),
        panel.grid.major = element_line(color="blue4", size=1, linetype="dashed"),
        panel.grid.minor = element_line(color="blue", size=0.5, linetype="dashed"),
        panel.border = element_rect(fill=NA, color="red"),
        panel.spacing = unit(1, "lines"),
        strip.background = element_rect(fill="grey30", color="grey10"),
        strip.text = element_text(color="white", size=base_size, lineheight=0.5),
        plot.background = element_rect(color="black", fill="black"),
        plot.title = element_text(size=base_size*1.2, color="white"),
        plot.margin = margin(t=1, r=1, b=1, l=1, "lines"),
        axis.line = element_blank(),
        axis.text.x = element_text(face="bold", angle=45, vjust=0.5, size=base_size*0.6, color="white", lineheight=0.9),
        axis.text.y = element_text(face="bold", size=base_size*0.8, color="white", lineheight=0.9, hjust=1),
        axis.ticks = element_line(colour = "white", size=0.2),
        axis.title.x = element_text(size = base_size*0.9, color="white", vjust=0),
        axis.title.y = element_text(face="bold", size=base_size*0.8, color="white", angle=90, vjust=1),
        axis.ticks.length = unit(0.3, "lines"),
        axis.text = element_text(margin=unit(0.5, "lines"))
  )

## ----Rplot03, message=FALSE, warning=FALSE-------------------------------
plot2tikz(function() print(gg), 
          filename="Rplot03", outdir="./imgs", 
          compile=TRUE, clean=TRUE, overwrite = TRUE,
          packages=c("\\usepackage[ddmmyyyy]{datetime}\n"),
          documentDeclaration ="\\documentclass[12pt]{standalone}\n",
          width=7, height=5)

## ----convertRplot03------------------------------------------------------
pspdf2img("imgs/Rplot03.pdf", format="png", moreopts="-resize 30%")

## ---- eval=FALSE---------------------------------------------------------
#  browseURL(system.file("scianimator", "example", "scianim_example.html", package="SLutils"))

## ---- eval=FALSE---------------------------------------------------------
#  # taken from ?tikz2png
#  plotCode <- function(n){
#    curve(x^n, from=-1, to=1, axes=FALSE,
#          ylab=NA, xlab="$x$",
#          main=sprintf("$f(x)=x\\\\^%s$", n))
#    xlabs_at <- seq(-1, 1, by=0.5)
#    axis(1, at=xlabs_at, labels=dollarify()(xlabs_at))
#    ylabs_at <- seq(ifelse(n%%2==0, 0, -1), 1, by=0.2)
#    axis(2, at=ylabs_at, labels=dollarify()(ylabs_at))
#    return(invisible())
#  }
#  # write images
#  imgs <- tikz2png(plotCode, 1:10, bg="white")
#  # create scianimation
#  scianim("scianim_example", imgs=imgs, theme="dark")
#  # remove files
#  file.remove(list.files(pattern="^Rplot"))

## ---- include=FALSE------------------------------------------------------
knitr::knit_exit()

