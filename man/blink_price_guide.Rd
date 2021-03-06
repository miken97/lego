% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bricklink-blink_price_guide.R
\name{blink_price_guide}
\alias{blink_price_guide}
\title{Current Items for Sale}
\usage{
blink_price_guide(set_id, url_only = FALSE)
}
\arguments{
\item{set_id}{A numeric scalar or character. unique Lego set identifier.}

\item{url_only}{A logical scalar. If \code{TRUE}, URL is returned. Defaults to \code{FALSE}.}
}
\value{
A data frame with three columns containing price and quantity of used items for sale.
}
\description{
Scrape currently for sale new or used items from the bricklink price guide.
}
\examples{
\dontrun{
bricklink_price_guide(set_id = 10255)
}
}
