# Tests for the Shiny interfaces.
library(spatstat.gui)

# public entry points build apps without opening a GUI.
local({
  objects <- list(cells, amacrine, longleaf, as.owin(cells),
                  density(cells), as.psp(as.linnet(simplenet)),
                  simplenet, runiflpp(5, simplenet))
  for(x in objects)
    stopifnot(inherits(iplot(x), "shiny.appobj"))
  stopifnot(inherits(istat(cells), "shiny.appobj"))
  stopifnot(inherits(istat(cells[FALSE]), "shiny.appobj"))
})

# navigation and controls use session-local data coordinates.
local({
  app <- iplot(amacrine)
  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(zoom_in=0, zoom_out=0, up=0, down=0, left=0,
                      right=0, reset=0, redraw=0, split=FALSE)
    initial <- panel()$zoomcentre
    widths <- sidelengths(panel()$bb)
    session$setInputs(zoom_in=1)
    stopifnot(isTRUE(all.equal(panel()$zoomfactor, 2)))
    session$setInputs(right=1)
    stopifnot(isTRUE(all.equal(panel()$zoomcentre, initial + c(widths[1]/8, 0))))
    target <- initial + c(0.1, 0.2)
    previous <- panel()$zoomcentre
    session$setInputs(plot_click=list(x=target[1], y=target[2]))
    stopifnot(isTRUE(all.equal(panel()$zoomcentre, previous + (target-initial)/2)))
    session$setInputs(split=TRUE)
    previous <- panel()$zoomcentre
    session$setInputs(plot_click=list(x=target[1]+0.1, y=target[2]))
    stopifnot(isTRUE(all.equal(panel()$zoomcentre, previous)))
    session$setInputs(xname="Edited title", pointmap="plus", charsize=2)
    stopifnot(isTRUE(all.equal(panel()$xname, "Edited title")))
    stopifnot(isTRUE(all.equal(panel()$pointmap, "plus")))
    stopifnot(isTRUE(all.equal(panel()$charsize, 2)))
    session$setInputs(reset=1)
    stopifnot(isTRUE(all.equal(panel()$zoomfactor, 1)))
    stopifnot(isTRUE(all.equal(panel()$zoomcentre, initial)))
    session$setInputs(summary_button=1)
    stopifnot(grepl("[Pp]oint", output$summary))
  })
  # A second session from the same app must start with untouched state.
  shiny::testServer(app$serverFuncSource(), {
    stopifnot(isTRUE(all.equal(panel()$zoomfactor, 1)))
    stopifnot(isTRUE(all.equal(panel()$xname, "amacrine")))
    stopifnot(isTRUE(all.equal(panel()$zoomcentre, panel()$bbmid)))
  })
})

# layer expansion preserves order, arguments and visibility.
local({
  pts <- runiflpp(5, simplenet)
  x <- layered(window=as.owin(simplenet), network=pts, points=cells,
               plotargs=list(list(col="grey"), list(col="blue"), list(pch=3)))
  out <- spatstat.gui:::.sg.prepare.layers(x, c(FALSE, TRUE, FALSE))
  stopifnot(length(out$x) == 4)
  stopifnot(isTRUE(all.equal(out$visible, c(FALSE, TRUE, TRUE, FALSE))))
  stopifnot(inherits(out$x[[1]], "owin"))
  stopifnot(inherits(out$x[[2]], "psp"))
  stopifnot(inherits(out$x[[3]], "ppp"))
  stopifnot(isTRUE(all.equal(out$x[[4]], cells)))
  stopifnot(isTRUE(all.equal(layerplotargs(out$x)[[2]]$col, "blue")))
  stopifnot(isTRUE(all.equal(layerplotargs(out$x)[[3]]$col, "blue")))
  error <- try(iplot(x, visible=c(TRUE, NA, FALSE)), silent=TRUE)
  stopifnot(inherits(error, "try-error"), grepl("visible", error))
  error <- try(iplot(x, visible=c(TRUE, FALSE)), silent=TRUE)
  stopifnot(inherits(error, "try-error"), grepl("visible", error))
})

# all layers may be hidden and image colour ranges remain fixed.
local({
  x <- layered(image=density(cells), points=cells)
  app <- iplot(x, visible=c(TRUE, FALSE))
  html <- app$httpHandler(list(PATH_INFO="/", REQUEST_METHOD="GET"))$content
  stopifnot(grepl('value="1" checked="checked"', html, fixed=TRUE))
  stopifnot(grepl('value="2"/>', html, fixed=TRUE))
  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(layers="1")
    stopifnot(isTRUE(all.equal(panel()$which, c(TRUE, FALSE))))
    stopifnot(isTRUE(all.equal(layerplotargs(panel()$x)[[1]]$zlim, range(x[[1]]))))
    session$setInputs(layers=character(0))
    stopifnot(isTRUE(all.equal(panel()$which, c(FALSE, FALSE))))
    invisible(output$plot)
    session$setInputs(layers=c("1", "2"))
    stopifnot(isTRUE(all.equal(panel()$which, c(TRUE, TRUE))))
    invisible(output$plot)
  })
})

# statistic sessions defer and reuse simulations.
local({
  set.seed(42)
  app <- istat(cells)
  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(stat="data", envel="none")
    stopifnot(is.null(panel()$simx))
    session$setInputs(stat="density", envel="pointwise")
    stopifnot(is.null(panel()$simx))
    session$setInputs(stat="Kest")
    bank <- panel()$simx
    stopifnot(length(bank) == 39)
    session$setInputs(stat="Lest", envel="simultaneous")
    stopifnot(identical(panel()$simx, bank))
    invisible(output$plot)
    session$setInputs(envel="none", stat="pcf", pcfbw=0.02)
    stopifnot(is.null(panel()$simx))
    stopifnot(isTRUE(all.equal(panel()$pcfbw, 0.02)))
    invisible(output$plot)
  })
})

# point-pattern symbols, marks and splitting render.
local({
  for(x in list(cells, amacrine, longleaf)) {
    app <- iplot(x)
    shiny::testServer(app$serverFuncSource(), {
      for(symbol in c("o", "bullet", "plus")) {
        session$setInputs(pointmap=symbol, charsize=1.5)
        invisible(output$plot)
      }
      if(is.marked(x)) {
        session$setInputs(pointmap="marks")
        invisible(output$plot)
      }
      if(is.multitype(x)) {
        session$setInputs(split=TRUE)
        invisible(output$plot)
      }
    })
  }
})
