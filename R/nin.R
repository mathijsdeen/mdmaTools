#' @title \%nin\% function
#' @description Evaluates whether the left hand side argument is not in the right hand side argument. As such, it negates the \code{\%in\%} operator.
#' @param lhs left hand side
#' @param rhs right hand side
#' @return The function returns
#' \item{out}{a vector of the same length as \code{lhs}.}
#' @details blabla
#' @examples
#' c(1,2,3) %nin% c(1,2)
#' @author Mathijs Deen
#' @export
`%nin%` <- function(lhs, rhs){
  `%notin%` <- Negate(`%in%`)
  return(lhs %notin% rhs)
}
