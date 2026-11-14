#
# Shared Shiny components
#

.sg.value <- function(value, default) {
  if(is.null(value)) default else value
}

.sg.summary.controls <- function() {
  shiny::tagList(
    shiny::actionButton("summary_button", "Show summary information"),
    shiny::actionButton("quit", "Quit"))
}

.sg.summary.server <- function(input, output, session, summary.fun) {
  output$summary <- shiny::renderPrint({
    shiny::req(input$summary_button > 0)
    print(summary.fun())
  })
  # Stop runApp() so that Quit returns control to the R prompt.
  shiny::observeEvent(input$quit, shiny::stopApp(), ignoreInit=TRUE)
}

.sg.plot.app <- function(base, controls, read.controls, draw, summary.fun) {
  bb <- as.rectangle(base$w)
  bbmid <- unlist(centroid.owin(bb))
  base$bb <- bb
  base$bbmid <- bbmid
  ui <- shiny::fluidPage(
    shiny::titlePanel(paste0("iplot(", base$xname, ")")),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::textInput("xname", "Plot title", base$xname),
        controls,
        shiny::h4("Navigation"),
        shiny::actionButton("up", "Up"),
        shiny::actionButton("left", "Left"),
        shiny::actionButton("right", "Right"),
        shiny::actionButton("down", "Down"),
        shiny::br(),
        shiny::actionButton("zoom_in", "Zoom in"),
        shiny::actionButton("zoom_out", "Zoom out"),
        shiny::actionButton("reset", "Reset"),
        shiny::actionButton("redraw", "Redraw"),
        shiny::hr(), .sg.summary.controls()),
      shiny::mainPanel(
        shiny::plotOutput("plot", height="600px", click="plot_click"),
        shiny::verbatimTextOutput("summary"))))
  server <- function(input, output, session) {
    nav <- shiny::reactiveValues(zoomfactor=1, zoomcentre=bbmid, redraw=0L)
    pan <- function(direction) {
      nav$zoomcentre <- nav$zoomcentre +
        direction * sidelengths(bb)/(4 * nav$zoomfactor)
    }
    shiny::observeEvent(input$up, pan(c(0, 1)), ignoreInit=TRUE)
    shiny::observeEvent(input$down, pan(c(0, -1)), ignoreInit=TRUE)
    shiny::observeEvent(input$left, pan(c(-1, 0)), ignoreInit=TRUE)
    shiny::observeEvent(input$right, pan(c(1, 0)), ignoreInit=TRUE)
    shiny::observeEvent(input$zoom_in, {
      nav$zoomfactor <- min(nav$zoomfactor * 2, 2^20)
    }, ignoreInit=TRUE)
    shiny::observeEvent(input$zoom_out, {
      nav$zoomfactor <- max(nav$zoomfactor / 2, 2^-20)
    }, ignoreInit=TRUE)
    shiny::observeEvent(input$reset, {
      nav$zoomfactor <- 1
      nav$zoomcentre <- bbmid
    }, ignoreInit=TRUE)
    shiny::observeEvent(input$redraw, {
      nav$redraw <- nav$redraw + 1L
    }, ignoreInit=TRUE)
    shiny::observeEvent(input$plot_click, {
      if(isTRUE(input$split)) {
        shiny::showNotification(
          "Mouse interaction is not supported when the point pattern is split.",
          type="message")
      } else {
        click <- input$plot_click
        shiny::req(is.finite(click$x), is.finite(click$y))
        nav$zoomcentre <- nav$zoomcentre +
          (c(click$x, click$y) - bbmid)/nav$zoomfactor
      }
    })
    panel <- shiny::reactive({
      nav$redraw
      modifyList(base, c(read.controls(input), list(
        xname=.sg.value(input$xname, base$xname),
        zoomfactor=nav$zoomfactor, zoomcentre=nav$zoomcentre)))
    })
    output$plot <- shiny::renderPlot({
      draw(panel())
      invisible(NULL)
    }, execOnResize=TRUE)
    .sg.summary.server(input, output, session, summary.fun)
  }
  shiny::shinyApp(ui, server)
}
