\name{istat}
\alias{istat}
\title{Point and Click Interface for Exploratory Analysis of Point Pattern}
\description{
  Compute various summary functions for a point pattern
  using a point-and-click interface.
}
\usage{
 istat(x, xname)
}
\arguments{
  \item{x}{
    The spatial point pattern to be analysed.
    An object of class \code{"ppp"}.
  }
  \item{xname}{
    Optional. Character string to use as the title of the dataset.
  }
}
\value{
  A Shiny application object of class \code{"shiny.appobj"}.
  Print it interactively or pass it to \code{shiny::runApp} to launch it.
}
\details{
  This command creates a Shiny (point-and-click) interface
  which offers a choice of spatial summary functions
  that can be applied to the point pattern \code{x}.

  The selected summary function is computed for the point pattern
  \code{x} and plotted in a web browser.
  Summary information is displayed in the browser.
  The Quit button stops the running Shiny application and returns control
  to R. This ends the application for all connected browser sessions.

  The selection of functions includes
  \code{\link[spatstat.explore]{Kest}}, \code{\link[spatstat.explore]{Lest}},
  \code{\link[spatstat.explore]{pcf}},
  \code{\link[spatstat.explore]{Fest}} ,\code{\link[spatstat.explore]{Gest}}
  and \code{\link[spatstat.explore]{Jest}}.
  For the function \code{\link[spatstat.explore]{pcf}} it is possible to control
  the bandwidth parameter \code{bw}.
  
  There is also an option to show simulation envelopes of
  the summary function.
}
\section{Note}{
   Before adjusting the bandwidth parameter \code{bw},
   it is advisable to select \emph{No simulation envelopes}
   to save a lot of computation time.
}
\seealso{
  \code{\link{iplot}}
}
\examples{
   if(interactive()) {
      istat(swedishpines)
   }
}
\author{
  \spatstatAuthors.
}
\keyword{spatial}
\keyword{hplot}
