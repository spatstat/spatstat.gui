\name{iplot} 
\alias{iplot}
\alias{iplot.ppp}
\alias{iplot.layered}
\alias{iplot.linnet}
\alias{iplot.lpp}
\alias{iplot.default}
\title{Point and Click Interface for Displaying Spatial Data}
\description{
  Plot spatial data
  with interactive (point-and-click) control over the plot.
}
\usage{
 iplot(x, ...)

 \method{iplot}{ppp}(x, ..., xname)

 \method{iplot}{linnet}(x, ..., xname)

 \method{iplot}{lpp}(x, ..., xname)

 \method{iplot}{layered}(x, ..., xname, visible)

 \method{iplot}{default}(x, ..., xname)
}
\arguments{
  \item{x}{
    The spatial object to be plotted.
    An object of class \code{"ppp"}, \code{"psp"}, \code{"im"},
    \code{"owin"}, \code{"linnet"}, \code{"lpp"} or \code{"layered"}.
  }
  \item{\dots}{Ignored.}
  \item{xname}{
    Optional. Character string to use as the title of the dataset.
  }
  \item{visible}{
    Optional. Logical vector indicating which layers of
    \code{x} should initially be turned on (visible).
  }
}
\value{
  A Shiny application object of class \code{"shiny.appobj"}.
  Print the object in an interactive R session to launch it, or pass it
  to \code{shiny::runApp}. The app can also be embedded in another Shiny UI.
}
\details{
  The function \code{iplot} generates a plot of the spatial dataset \code{x}
  and allows interactive control over the appearance of the plot
  using a point-and-click interface.

  The function \code{iplot} is generic, with methods for
  for point patterns (\code{iplot.ppp}),
  layered objects (\code{iplot.layered})
  and a default method. The default method will handle objects of
  class \code{"psp"}, \code{"im"} and \code{"owin"}
  at least.

  The application is displayed in a web browser. The spatial dataset
  \code{x} is displayed in the middle of the window using the
  appropriate \code{plot} method.

  The left side of the window contains
  buttons and sliders allowing the user to change the plot parameters.

  The sidebar also contains navigation controls for
  zooming (changing magnification), panning (shifting the
  field of view relative to the data), and redrawing.
  Summary information is displayed below the plot.
  The Quit button stops the running Shiny application and returns control
  to R. This ends the application for all connected browser sessions.

  If the user clicks in the area where the point pattern is displayed,
  the field of view will be re-centred at the point that was clicked.
}
\seealso{
  \code{\link{istat}}
}
\examples{
   app <- iplot(cells)
   if(interactive()) shiny::runApp(app)

   # Other supported objects:
   multitype_app <- iplot(amacrine)
      L <- layered(D=distmap(cells), P=cells,
            plotargs=list(list(ribbon=FALSE), list(pch=16)))
   layered_app <- iplot(L)
}
\author{
  \spatstatAuthors.
}
\keyword{spatial}
\keyword{hplot}
