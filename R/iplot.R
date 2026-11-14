#
# interactive plot for ppp objects using shiny
#

iplot <- function(x, ...) {
  UseMethod("iplot")
}

iplot.ppp <- function(x, ..., xname) {
  if(missing(xname))
    xname <- short.deparse(substitute(x))
  verifyclass(x, "ppp")
  if(markformat(x) %in% c("hyperframe", "list"))
    marks(x) <- as.data.frame(as.hyperframe(marks(x)))
  if(markformat(x) == "dataframe" && ncol(marks(x)) > 1) {
    warning("Using only the first column of marks")
    marks(x) <- marks(x)[,1L]
  }
  mtype <- if(is.multitype(x)) "multitype" else
           if(is.marked(x)) "marked" else "unmarked"
  pointmap <- if(is.marked(x)) "marks" else "o"
  choices <- c("Open circles"="o", "Filled circles"="bullet",
               "Crosshairs"="plus")
  if(is.marked(x))
    choices <- c(setNames("marks", if(mtype == "multitype")
                         "Symbols depending on mark" else
                         "Circles proportional to mark"), choices)
  markscale <- 1
  if(mtype == "marked") {
    marx <- marks(x)
    markscale <- mark.scale.default(marx[is.finite(marx)], as.owin(x))
  }
  controls <- list(
    if(mtype == "multitype")
      shiny::checkboxInput("split", "Split according to marks", FALSE),
    shiny::radioButtons("pointmap", "How to plot points", choices,
                        selected=pointmap),
    shiny::sliderInput("charsize", "Symbol expansion factor (cex)",
                       min=0, max=5, value=1, step=0.05),
    if(mtype == "marked")
      shiny::sliderInput("markscale", "Mark scale factor", min=markscale/10,
                         max=10*markscale, value=markscale))
  panel <- list(x=x, w=as.owin(x), xname=xname, mtype=mtype)
  read.controls <- function(input) {
    list(split=isTRUE(input$split),
         pointmap=.sg.value(input$pointmap, pointmap),
         charsize=.sg.value(input$charsize, 1),
         markscale=.sg.value(input$markscale, markscale))
  }
  .sg.plot.app(panel, controls, read.controls, .sg.draw.ppp,
               function() summary(x))
  }

.sg.draw.ppp <- function(panel) {
  use.marks <- TRUE
  pch <- 16
  switch(panel$pointmap,
         marks={
           use.marks <- TRUE
           pch <- NULL
         }, 
         o = {
           use.marks <- FALSE
           pch <- 1
         }, 
         bullet = {
           use.marks <- FALSE
           pch <- 16
         },
         plus = {
           use.marks <- FALSE
           pch <- 3
         })
  # scale and clip the pattern
  x <- panel$x
  w     <- panel$w
  z     <- panel$zoomfactor
  if(is.null(z)) z <- 1
  ce    <- panel$zoomcentre
  bb    <- panel$bb
  bbmid <- panel$bbmid
  scalex <- shift(scalardilate(shift(x, -ce), z), bbmid)
  scalew <- shift(scalardilate(shift(w, -ce), z), bbmid)
  scalex <- scalex[, bb]
  scalew <- intersect.owin(scalew, bb, fatal=FALSE)
  # determine what is plotted under the clipped pattern
  blankargs <- list(type="n")
  dashargs  <- list(lty=3, border="red")
  panel.begin <- 
    if(is.null(scalew)) {
      # empty intersection; just create the plot space
      layered(bb,          plotargs=list(blankargs))
    } else if(identical(bb, scalew)) {
      if(z == 1) {
        # original state
        # window is rectangular 
        # plot the data window as a solid black rectangle
        layered(bb, scalew,  plotargs=list(blankargs, list(lwd=2)))
      } else {
        # zoom view is entirely inside window
        # plot the clipping region as a red dashed rectangle
        layered(bb, plotargs=list(dashargs))
      }
    } else {
      # field of view is not a subset of window
      # plot the clipping region as a red dashed rectangle
      # Then add the data window
      layered(bb, scalew, plotargs=list(dashargs, list(invert=TRUE)))
    }

  ## draw it
  opa <- par(ask=FALSE)
  on.exit(par(opa))
  if(panel$mtype == "multitype" && panel$split) {
    scalex <- split(scalex, un=(panel$pointmap != "marks"))
    plot(scalex, main=panel$xname, 
         use.marks=use.marks, pch=pch, cex=panel$charsize,
         panel.begin=panel.begin)
  } else {
    # draw scaled & clipped window
    plot(panel.begin, main=panel$xname)
    # add points
    if(panel$mtype == "marked" && panel$pointmap == "marks") {
      plot(scalex, add=TRUE, use.marks=use.marks, markscale=panel$markscale)
    } else {
      plot(scalex, add=TRUE, use.marks=use.marks, pch=pch, cex=panel$charsize)
    }
  }
  panel
}
