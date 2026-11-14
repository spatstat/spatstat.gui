#
# interactive analysis of point patterns
#

istat <- function(x, xname) {
  if(missing(xname))
    xname <- short.deparse(substitute(x))
  verifyclass(x, "ppp")
  rect <- as.rectangle(as.owin(x))
  winwid <- min(diff(rect$xrange), diff(rect$yrange))
  sigma0 <- winwid/8
  # Keep a finite slider range even for an empty pattern.
  pcfbw0 <- 0.15/sqrt(5 * max(1, npoints(x))/area(as.owin(x)))
  choices <- c("Data"="data", "Kernel smoothed"="density",
               "K-function"="Kest", "L-function"="Lest",
               "Pair correlation"="pcf", "Inhomogeneous K"="Kinhom",
               "Inhomogeneous L"="Linhom", "Empty space function F"="Fest",
               "Nearest neighbour function G"="Gest", "J-function"="Jest")
  ui <- shiny::fluidPage(
    shiny::titlePanel(paste0("istat(", xname, ")")),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::radioButtons("stat", "Statistic", choices, selected="data"),
        shiny::radioButtons("envel", "Simulation envelopes",
          c("No simulation envelopes"="none", "Pointwise under CSR"="pointwise",
            "Simultaneous under CSR"="simultaneous"), selected="none"),
        shiny::conditionalPanel(
          condition="input.stat === 'density' || input.stat === 'Kinhom' || input.stat === 'Linhom'",
          shiny::sliderInput("sigma", "Smoothing bandwidth (sigma)",
                             min=winwid/80, max=winwid/2, value=sigma0)),
        shiny::conditionalPanel(
          condition="input.stat === 'pcf'",
          shiny::sliderInput("pcfbw", "Pair correlation bandwidth (bw)",
                             min=pcfbw0/10, max=4*pcfbw0, value=pcfbw0)),
        .sg.summary.controls()),
      shiny::mainPanel(shiny::plotOutput("plot", height="600px"),
                       shiny::verbatimTextOutput("summary"))))
  server <- function(input, output, session) {
    # A single simulation bank is reused when parameters/statistics change.
    simulations <- shiny::reactive({
      envelope(x, fun=NULL, nsim=39, verbose=FALSE,
                   internal=list(csr=TRUE, eject="patterns"))
    })
    panel <- shiny::reactive({
      stat <- .sg.value(input$stat, "data")
      envel <- .sg.value(input$envel, "none")
      list(x=x, xname=xname, stat=stat, envel=envel,
           sigma=.sg.value(input$sigma, sigma0),
           pcfbw=.sg.value(input$pcfbw, pcfbw0),
           simx=if(!(stat %in% c("data", "density")) && envel != "none")
                  simulations() else NULL)
    })
    output$plot <- shiny::renderPlot({
      do.istat(panel())
      invisible(NULL)
    }, execOnResize=TRUE)
    .sg.summary.server(input, output, session, function() summary(x))
  }
  shiny::shinyApp(ui, server)
}

do.istat <- function(panel) { 
  x     <- panel$x
  xname <- panel$xname
  envel <- panel$envel
  stat  <- panel$stat
  sigma <- panel$sigma
  simx  <- panel$simx
  if(stat=="data") {
    plot(x, main=xname)
    return(panel)
  }
  out <- 
    switch(envel,
           none=switch(stat,
             density=density(x, sigma=sigma),
             Kest=Kest(x),
             Lest=Lest(x), 
             pcf=pcf(x, bw=panel$pcfbw),
             Kinhom=Kinhom(x, sigma=sigma),
             Linhom=Linhom(x, sigma=sigma),
             Fest=Fest(x),
             Gest=Gest(x),
             Jest=Jest(x)),
           pointwise=switch(stat,
             density=density(x, sigma=sigma),
             Kest=envelope(x, Kest, nsim=39, simulate=simx, verbose=FALSE),
             Lest=envelope(x, Lest, nsim=39, simulate=simx, verbose=FALSE),
             pcf=envelope(x, pcf, bw=panel$pcfbw, nsim=39, simulate=simx, verbose=FALSE),
             Kinhom=envelope(x, Kinhom, nsim=39, sigma=sigma, simulate=simx, verbose=FALSE),
             Linhom=envelope(x, Linhom, nsim=39, sigma=sigma, simulate=simx, verbose=FALSE),
             Fest=envelope(x, Fest, nsim=39, simulate=simx, verbose=FALSE),
             Gest=envelope(x, Gest, nsim=39, simulate=simx, verbose=FALSE),
             Jest=envelope(x, Jest, nsim=39, simulate=simx, verbose=FALSE)),
           simultaneous=switch(stat,
             density=density(x, sigma=sigma),
             Kest=envelope(x, Kest, nsim=19, global=TRUE, simulate=simx, verbose=FALSE),
             Lest=envelope(x, Lest, nsim=19, global=TRUE, simulate=simx, verbose=FALSE),
             pcf=envelope(x, pcf, bw=panel$pcfbw, nsim=19, global=TRUE, simulate=simx, verbose=FALSE),
             Kinhom=envelope(x, Kinhom, nsim=19, sigma=sigma, global=TRUE, simulate=simx, verbose=FALSE),
             Linhom=envelope(x, Linhom, nsim=19, sigma=sigma, global=TRUE, simulate=simx, verbose=FALSE),
             Fest=envelope(x, Fest, nsim=19, global=TRUE, simulate=simx, verbose=FALSE),
             Gest=envelope(x, Gest, nsim=19, global=TRUE, simulate=simx, verbose=FALSE),
             Jest=envelope(x, Jest, nsim=19, global=TRUE, simulate=simx, verbose=FALSE))
           )
  ## determine main title
  auxargs <- switch(stat,
                    density = "sigma",
                    pcf     = "bw",
                    Kinhom  = "sigma",
                    Linhom  = "sigma",
                    NULL)
  gstring <- paste(c(xname, auxargs), collapse=", ")
  main <- paste0(stat, paren(gstring))
  ## plot it
  plot(out, main=main)
  if(stat == "density")
    points(x)
  return(panel)
}

