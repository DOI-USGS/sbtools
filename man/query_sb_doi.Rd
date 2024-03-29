% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/query_sb.R
\name{query_sb_doi}
\alias{query_sb_doi}
\title{Query SB for specific DOI (Digital Object Identifier)}
\usage{
query_sb_doi(doi, ..., limit = 20)
}
\arguments{
\item{doi}{DOI to search for as character}

\item{...}{Additional parameters are passed on to \code{\link[httr]{GET}}}

\item{limit}{Maximum number of returned items. Will do paging to retrieve
results when limit is over 1000. Use with caution, queries 10k results
are slow.}
}
\value{
A list of \code{\link{sbitem}} objects. List of length 0 
means no matches were found.
}
\description{
Queries for ScienceBase items with a specific DOI identifier. 
In ScienceBase, these are stored as additional unique identifiers.
}
\examples{
\donttest{
#Two example DOI-specific queries
query_sb_doi('10.5066/F7M043G7')

query_sb_doi('10.5066/F7Z60M35')
}
}
