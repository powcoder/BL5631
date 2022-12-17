BiocManager::install("IRanges")
library(IRanges)
help(package="IRanges")
ir <- IRanges(start=c(1,3,5),width=c(3,5,7))

width(ir)

mid(ir)
ir
ir[1]
reduce(ir)
disjoin(ir)
browseVignettes('IRanges')

plotRanges <- function(x, xlim=x, main=deparse(substitute(x)),
                       col="black", sep=0.5, ...) {
    height <- 1
    if (is(xlim, "IntegerRanges"))
      xlim <- c(min(start(xlim)), max(end(xlim)))
    bins <- disjointBins(IRanges(start(x), end(x) + 1))
    plot.new()
    plot.window(xlim, c(0, max(bins)*(height + sep)))
    ybottom <- bins * (sep + height) - height
    rect(start(x)-0.5, ybottom, end(x)+0.5, ybottom + height, col=col, ...)
    title(main)
    axis(1)
    }
ir
plotRanges(ir)


ir <- IRanges(start=c(1,3,7,9),end=c(4,4,8,10))
ir
par(mfrow=c(2,1))
plotRanges(ir)
plotRanges(reduce(ir))

plotRanges(ir)
plotRanges(disjoin(ir))

ir
plotRanges(reduce(ir))


#plot(mid(ir))


ir1
resize(ir1,width=1)

?promoters


ir1 <- IRanges(start=c(1,3,5),width=1)
ir2 <- IRanges(start=c(4,5,6),width=1)
union(ir1,ir2)

intersect(ir1,ir2)

ir1 <- IRanges(start=c(1,4,8),end=c(3,7,10))
ir2 <- IRanges(start=c(3,4),width=3)
plotRanges(ir1)
plotRanges(ir2)
findOverlaps(ir1,ir2)

countOverlaps(ir1,ir2)

nearest(ir1,ir2)

plotRanges(c(ir1,ir2))
plotRanges(ir2)


data.frame(irange=ir1)
df = DataFrame(irange=ir1)
