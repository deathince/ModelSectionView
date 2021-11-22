
#' Title
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
dummymodel <- function(id = 0){
  if (id == 0){
    name = "cube"
    points = matrix(c(1,1,1,1,-1,-1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,-1,1,-1,1,-1,1,-1), nrow = 8, ncol = 3)
    connections = matrix(c(1,0))
  }
  return(list(name = name, points = points, connections = connections))
}


simplifymodel <- function(points, connections){


  return (list(points = points, connections = connections))
}



baseshape <- function(points = 2){

  return (list(name = name, points = points, connections = connections))
}
