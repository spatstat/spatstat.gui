#
# interactive plot 
#

iplot.default <- function(x, ..., xname) {
 if(missing(xname))
    xname <- short.deparse(substitute(x))
  iplot(as.layered(x), ..., xname=xname)
  }
  
iplot.layered <- function(x, ..., xname, visible) {
  if(missing(xname))
    xname <- short.deparse(substitute(x))
  verifyclass(x, "layered")
  if(length(x) == 0L)
    stop("x must contain at least one layer")
  if(missing(visible) || is.null(visible))
    visible <- rep(TRUE, length(x))
  if(!is.logical(visible) || anyNA(visible) ||
     !(length(visible) %in% c(1L, length(x))))
    stop("visible must be a logical vector of length 1 or length(x), without NA")
  visible <- rep(visible, length.out=length(x))
  prepared <- .sg.prepare.layers(x, visible)
  x <- .sg.freeze.colourmaps(prepared$x)
  visible <- prepared$visible
  lnames <- names(x)
  if(is.null(lnames) || any(!nzchar(lnames)))
    lnames <- paste("Layer", seq_along(x))
  ids <- as.character(seq_along(x))
  controls <- list(shiny::checkboxGroupInput(
    "layers", "Select layers to plot", choices=setNames(ids, lnames),
    selected=ids[visible]))
  panel <- list(x=x, w=as.owin(x), xname=xname)
  read.controls <- function(input) {
    # Shiny sends NULL when all checkboxes have been deselected.
    list(which=ids %in% input$layers)
  }
  .sg.plot.app(panel, controls, read.controls, .sg.draw.layered,
               function() lapply(x, summary))
}

.sg.prepare.layers <- function(x, visible) {
  # Expand each network in place, preserving plot arguments and visibility.
  layers <- args <- list()
  shown <- logical(0)
  nms <- character(0)
  pl <- layerplotargs(x)
  nm <- names(x)
  if(is.null(nm)) nm <- rep("", length(x))
  for(i in seq_along(x)) {
    xi <- x[[i]]
    pieces <- if(inherits(xi, "lpp"))
      list(as.psp(as.linnet(xi)), as.ppp(xi)) else
      list(if(inherits(xi, "linnet")) as.psp(xi) else xi)
    count <- length(pieces)
    layers <- c(layers, pieces)
    arg <- if(length(pl) >= i) pl[[i]] else list()
    args <- c(args, rep(list(arg), count))
    shown <- c(shown, rep(visible[i], count))
    nms <- c(nms, if(count == 2L && nzchar(nm[i]))
             paste(nm[i], c("network", "points")) else rep(nm[i], count))
  }
  names(layers) <- nms
  list(x=layered(LayerList=layers, plotargs=unname(args)), visible=shown)
}

.sg.draw.layered <- function(panel) {
  # scale and clip the pattern
  x <- panel$x[panel$which]
  w     <- panel$w
  z     <- panel$zoomfactor
  if(is.null(z)) z <- 1
  ce    <- panel$zoomcentre
  bb    <- panel$bb
  bbmid <- panel$bbmid
  scalex <- if(length(x)>0) shift(scalardilate(shift(x, -ce), z), bbmid) else NULL
  scalew <- shift(scalardilate(shift(w, -ce), z), bbmid)
  if(length(scalex)>0) scalex <- scalex[, bb]
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
  
  # draw it
  opa <- par(ask=FALSE)
  on.exit(par(opa))
  plot(panel.begin, main=panel$xname)
  if(length(scalex)>0) plot(scalex, add=TRUE)
  panel
}

.sg.freeze.colourmaps <- function(x) {
  # tweak a layered object to ensure that
  # the colours of image layers don't change with zoom/pan
  isim <- unlist(lapply(x, is.im))
  if(any(isim)) {
    # ensure there are plotargs
    pl <- attr(x, "plotargs")
    if(is.null(pl))
      pl <- rep.int(list(list()), length(x))
    # make sure the plotargs include 'zlim'
    for(i in which(isim)) {
      x.i <- x[[i]]
      if(x.i$type %in% c("integer", "real")) 
        pl[[i]] <- resolve.defaults(pl[[i]], list(zlim=range(x.i)))
    }
    # put back
    attr(x, "plotargs") <- pl
  }
  return(x) 
}
