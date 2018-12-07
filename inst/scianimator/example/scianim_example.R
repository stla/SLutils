## set working directory ##

# taken from ?tikz2png
plotCode <- function(n){
  curve(x^n, from=-1, to=1, axes=FALSE,
        ylab=NA, xlab="$x$",
        main=sprintf("$f(x)=x\\\\^%s$", n))
  xlabs_at <- seq(-1, 1, by=0.5)
  axis(1, at=xlabs_at, labels=dollarify()(xlabs_at))
  ylabs_at <- seq(ifelse(n%%2==0, 0, -1), 1, by=0.2)
  axis(2, at=ylabs_at, labels=dollarify()(ylabs_at))
  return(invisible())
}

# write images
imgs <- tikz2png(plotCode, 1:10, bg="white")

# create scianim
scianim("scianim_example", imgs=imgs, theme="dark")

# remove files
file.remove(list.files(pattern="^Rplot"))
