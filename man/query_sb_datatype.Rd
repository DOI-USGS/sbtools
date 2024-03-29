% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/query_sb.R
\name{query_sb_datatype}
\alias{query_sb_datatype}
\title{Query SB for specific data type}
\usage{
query_sb_datatype(datatype, ..., limit = 20)
}
\arguments{
\item{datatype}{Character string indicating datatype. See \code{\link{sb_datatypes}} for full
list of available datatypes.}

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
Queries ScienceBase for items with matching datatype.
}
\examples{
\donttest{
#query for items with WFS Layer data
query_sb_datatype('Static Map Image')

#query for US Topo maps
query_sb_datatype('Map Service')
}

}
