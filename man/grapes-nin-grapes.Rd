% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nin.R
\name{\%nin\%}
\alias{\%nin\%}
\title{\%nin\% function}
\usage{
lhs \%nin\% rhs
}
\arguments{
\item{lhs}{left hand side}

\item{rhs}{right hand side}
}
\value{
The function returns a vector of the same length as \code{lhs}.
}
\description{
Evaluates whether the left hand side argument is not in the right hand side argument.
}
\details{
The \code{\%nin\%} function negates the \code{\%in\%} operator.
}
\examples{
c(1,2,3) \%nin\% c(1,2)
}
\author{
Mathijs Deen
}
