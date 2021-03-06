% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lego-scrape_lego.R
\name{current_lego_themes}
\alias{current_lego_themes}
\title{Current Lego Theme}
\usage{
current_lego_themes(country_code = "en-ca")
}
\arguments{
\item{country_code}{A character string consisting of the country and language code. Defaults to \code{"en-ca"}.}
}
\value{
A dataframe of currently available Lego themes from \href{https://www.lego.com/en-ca/themes}{Lego}.
}
\description{
Scrape currently available themes from \href{https://www.lego.com/en-ca/themes}{Lego}.
}
\examples{
\dontrun{
current_lego_themes()
}
}
