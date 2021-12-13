
#' generate dummy model function
#' this function will pull some preset from written model
#'
#' @param id the preset id
#'
#' @return
#' list$name     the name of id corresponded preset.
#' list$points     the points located in this preset.
#' list$surface      the surface model contained
#' @export
#'
#' @examples
dummymodel <- function(id = 0){
  if (id == 0){
    name = "cube"
    lists = baseshapenew(preset = 1)
    points = shift(lists$points,c(0,0,1))
    surface = lists$surface
    lists = baseshapenew(preset = 1)
    lists = join(points,surface,shift(lists$points,c(0,0,-1)),lists$surface)
    points = lists$points
    surface = lists$surface
    lists = baseshapenew(preset = 1)
    temp = spin(lists$points, 1)
    lists = join(points,surface,shift(temp,c(1,0,0)),lists$surface)
    points = lists$points
    surface = lists$surface
    lists = baseshapenew(preset = 1)
    temp = spin(lists$points, 1)
    lists = join(points,surface,shift(temp,c(-1,0,0)),lists$surface)
    points = lists$points
    surface = lists$surface
    lists = baseshapenew(preset = 1)
    temp = spin(lists$points, 2)
    lists = join(points,surface,shift(temp,c(0,-1,0)),lists$surface)
    points = lists$points
    surface = lists$surface
    lists = baseshapenew(preset = 1)
    temp = spin(lists$points, 2)
    lists = join(points,surface,shift(temp,c(0,1,0)),lists$surface)
    points = lists$points
    surface = lists$surface
    final = simplifymodel(points,surface)
    points = final$points
    surface = final$surface
  }
  else{
    boxbig = dummymodel()
    boxsmall = dummymodel()
    smallpoints = scalingmodel(boxsmall$points,c(0.5,0.5,0.5))
    lists = join(boxbig$points,boxbig$surface,smallpoints,boxsmall$surface)
    name = "box in a box"
    points = lists$points
    surface = lists$surface
  }
  return(list(name = name, points = points, surface = surface))
}

#' Title
#'
#' @inherit simplifymodel
#' @param plane the plane constant variable x y z store as c(x,y,z) that we want to cut though the model. default is c(0,0,1) or xy plane
#' @param k  the scaler function that make the plane shift. equations is x + y + z = k default k = 0
#'
#' @return  the function do not return anything but draw a graph
#' @export
#'
#' @examples
sectionview <- function(points, surface, plane = c(0,0,1), k = 0){
  surfacecount = nrow(surface)
  draw = matrix(ncol = 6)
  for (iter in 1:surfacecount){
    cursurface = surface[iter, ]
    pointmat = points[cursurface, ]
    side = sign(rowSums(t(t(pointmat) * plane)) - k)
    if (abs(sum(side)) < 2){
      if (side[1] == 0 ){
        if (side[2] == 0){
          draw = rbind(draw, append(pointmat[1,], pointmat[2,]))
        }
        else{
          const = sum(pointmat[2,] * plane - k)
          t = sum((pointmat[3,] - pointmat[2,]) * plane)
          anss = -const / t
          interpoint = pointmat[2, ] + anss * (pointmat[3,] - pointmat[2,])
          draw = rbind(draw, append(pointmat[1,], interpoint))
        }
      }
      else if(side[1] == 1 ){
        if (side[2] == 0){
          if (side[3] == 0){
            draw = rbind(draw, append(pointmat[2,], pointmat[3,]))
          }
          else{
            const = sum(pointmat[1,] * plane - k)
            t = sum((pointmat[3,] - pointmat[1,]) * plane)
            anss = -const / t
            interpoint = pointmat[1, ] + anss * (pointmat[3,] - pointmat[1,])
            draw = rbind(draw, append(pointmat[2,], interpoint))
          }
        }
        else if (side[2] == 1){
          const = sum(pointmat[3,] * plane - k)
          t = sum((pointmat[1,] - pointmat[3,]) * plane)
          anss = -const / t
          interpoint = pointmat[3, ] + anss * (pointmat[1,] - pointmat[3,])
          const2 = sum(pointmat[2,] * plane - k)
          t2 = sum((pointmat[3,] - pointmat[2,]) * plane)
          anss2 = -const / t
          interpoint2 = pointmat[2, ] + anss * (pointmat[3,] - pointmat[2,])
          draw = rbind(draw, append(interpoint, interpoint2))
        }
        else{
          const = sum(pointmat[2,] * plane - k)
          t = sum((pointmat[1,] - pointmat[2,]) * plane)
          anss = -const / t
          interpoint = pointmat[2, ] + anss * (pointmat[1,] - pointmat[2,])

          if (side[3] == 0){
            draw = rbind(draw,append(interpoint, pointmat[3, ]))
          }
          else if (side[3] == 1){
            const2 = sum(pointmat[2,] * plane - k)
            t2 = sum((pointmat[3,] - pointmat[2,]) * plane)
            anss2 = -const / t
            interpoint2 = pointmat[2, ] + anss * (pointmat[3,] - pointmat[2,])
            draw = rbind(draw, append(interpoint2, interpoint))

          }
          else{
            const2 = sum(pointmat[3,] * plane - k)
            t2 = sum((pointmat[1,] - pointmat[3,]) * plane)
            anss2 = -const / t
            interpoint2 = pointmat[3, ] + anss * (pointmat[1,] - pointmat[3,])
            draw = rbind(draw, append(interpoint2, interpoint))

          }
        }
      }
      else{
        if (side[2] == 0){
          if (side[3] == 0){
            draw = rbind(draw, append(pointmat[2,], pointmat[3,]))
          }
          else{
            const = sum(pointmat[1,] * plane - k)
            t = sum((pointmat[3,] - pointmat[1,]) * plane)
            anss = -const / t
            interpoint = pointmat[1, ] + anss * (pointmat[3,] - pointmat[1,])
            draw = rbind(draw, append(pointmat[2,], interpoint))
          }
        }
        else if (side[2] == -1){
          const = sum(pointmat[3,] * plane - k)
          t = sum((pointmat[1,] - pointmat[3,]) * plane)
          anss = -const / t
          interpoint = pointmat[3, ] + anss * (pointmat[1,] - pointmat[3,])
          const2 = sum(pointmat[2,] * plane - k)
          t2 = sum((pointmat[3,] - pointmat[2,]) * plane)
          anss2 = -const / t
          interpoint2 = pointmat[2, ] + anss * (pointmat[3,] - pointmat[2,])
          draw = rbind(draw, append(interpoint, interpoint2))
        }
        else{
          const = sum(pointmat[2,] * plane - k)
          t = sum((pointmat[1,] - pointmat[2,]) * plane)
          anss = -const / t
          interpoint = pointmat[2, ] + anss * (pointmat[1,] - pointmat[2,])

          if (side[3] == 0){
            draw = rbind(draw,append(interpoint, pointmat[3, ]))
          }
          else if (side[3] == -1){
            const2 = sum(pointmat[2,] * plane - k)
            t2 = sum((pointmat[3,] - pointmat[2,]) * plane)
            anss2 = -const / t
            interpoint2 = pointmat[2, ] + anss * (pointmat[3,] - pointmat[2,])
            draw = rbind(draw, append(interpoint2, interpoint))

          }
          else{
            const2 = sum(pointmat[3,] * plane - k)
            t2 = sum((pointmat[1,] - pointmat[3,]) * plane)
            anss2 = -const / t
            interpoint2 = pointmat[3, ] + anss * (pointmat[1,] - pointmat[3,])
            draw = rbind(draw, append(interpoint2, interpoint))

          }
        }
      }
    }
  }
  draw = draw[-1, ]
  if (nrow(draw)==0){
    print("the plane did not cut though the plane")
    return(draw)
  }
  if (which.max(plane) == 1){
    temp = draw[,c(2,3,5,6)]
    minx = min(temp[,c(1,3)])
    maxx = max(temp[,c(1,3)])
    miny = min(temp[,c(2,4)])
    maxy = max(temp[,c(2,4)])
    plot(temp[1,c(1,3)],temp[1,c(2,4)],"l",xlim = c(minx,maxx), ylim = c(miny,maxy))
    for (iter in 2:nrow(temp)){
      lines(temp[iter,c(1,3)],temp[iter,c(2,4)])
    }
  }
  else if (which.max(plane) == 2){
    temp = draw[,c(1,3,4,6)]
    minx = min(temp[,c(1,3)])
    maxx = max(temp[,c(1,3)])
    miny = min(temp[,c(2,4)])
    maxy = max(temp[,c(2,4)])
    plot(temp[1,c(1,3)],temp[1,c(2,4)],"l",xlim = c(minx,maxx), ylim = c(miny,maxy))
    for (iter in 2:nrow(temp)){
      lines(temp[iter,c(1,3)],temp[iter,c(2,4)])
    }
  }
  else if (which.max(plane) == 3){
    temp = draw[,c(1,2,4,5)]
    minx = min(temp[,c(1,3)])
    maxx = max(temp[,c(1,3)])
    miny = min(temp[,c(2,4)])
    maxy = max(temp[,c(2,4)])
    plot(temp[1,c(1,3)],temp[1,c(2,4)],"l",xlim = c(minx,maxx), ylim = c(miny,maxy))
    for (iter in 2:nrow(temp)){
      lines(temp[iter,c(1,3)],temp[iter,c(2,4)])
    }
  }
  return(draw)
}





#' simplify model function
#' combining duplicated or close control points
#'
#' @param points  the P * 3 matrix that is generated by other base functions
#' @param surface the N * 3 matrix indicate the surface component
#'
#' @return
#' points    the (P - x) * 3 matrix, combined x pair points in simplifying procedure
#' connections   the (P - x) * (P - x) matrix indicate the connections between new points
#' @export
#'
#' @examples
simplifymodel <- function(points, surface){
  # this function need to have loop of loop to iterate every pair of points, therefore using C++ is recommended
  npoint = nrow(points)
  removes = c()
  for (iter in 1:(npoint - 1)){
    # get current number
    currow = points[iter,]

    # calculate the distance to unreached points
    distance = rowSums((t(t(points[(iter + 1):nrow(points), ,drop = FALSE]) - currow))^2)

    # select points that are very close to current point
    samepoint = which(distance < 0.01) + iter

    # change the all connection plot to current point
    surface [surface %in% samepoint] = iter

    # remove duplicated points
    removes = append(removes,samepoint)

    # since we shrink the points matrix, we need to check the length every time
  }
  if ( length(removes) != 0){
    points = points[-removes,]
  }
  return (list(points = points, surface = surface))
}




#' the new surface generator function
#'
#' @param preset a scaler that will pull preset model. 1 = square,
#' @param controlpoints the 3 * 3 matrix that each row present 3 points location
#'
#' @return   controlpoints  same as input if no exception exist, or a group of preset points with matrix size n * 3
#'           surface        an n * 3 matrix  that represent which points create triangle surface.
#' @export
#'
#' @examples
baseshapenew <-function(preset = NULL, controlpoints = NULL){
  if(is.null(preset)){
    size = length(controlpoints)
    if (size < 3){
      stop("the control points can not create a surface")
    }
    else if (size > 3){
      # perform safe check that all control points lay on same surface
      stop("the control points is more than 3")
    }
    surface = matrix(c(1,2,3), nrow = 1)
    return(list(points = controlpoints, surface = surface))
  }
  if (preset == 1){
    # square
    controlpoints = matrix(c(1,1,0,1,-1,0,-1,1,0,-1,-1,0), byrow = TRUE, ncol = 3)
    surface = matrix(c(1,2,3,2,3,4),byrow = TRUE, ncol = 3)
    return(list(points = controlpoints, surface = surface))
  }
  if (preset == 2){
    # triangle
    controlpoints = matrix(c(1,1,0,1,-1,0,-1,0,0), byrow = TRUE, ncol = 3)
    surface = matrix(c(1,2,3),byrow = TRUE, ncol = 3)
    return(list(points = controlpoints, surface = surface))
  }
}

#' shift function
#'
#' @param points the P * 3 matrix that is generated by other base functions
#' @param shift an len 3 vector indicating how much we want to shift, default is (0,0,0)
#' @param shiftto an len 3 vector indicate where do we want to shift model to. default is NULL
#'
#' @return the points that is shifted by shift amount. or one of the closest point shift to shiftto location.
#' @export
#'
#' @examples
#' shift(matrix(c(0,0,1),nrow = 1),c(1,1,1)) # return matrix(1,1,2) that every point in matrix shift (1,1,1) direction
shift <- function(points,shift = c(0,0,0), shiftto = NULL){
  # perform shift if no shiftto location is given.
  if (is.null(shiftto)){
    if (length(shift) != 3){
      return(points)
    }
    newpoint = t(t(points) + shift)
    return(newpoint)
  }
  else{
    # find the distance
    if (length(shiftto) != 3){
      # error location to shift
      return(points)
    }
    distance = rowSums(t(t(points) - shiftto))
    shift = -1 * points(which.min(distance))
    newpoint = t(t(points) + shift)
    return(newpoint)
  }
}


#' spin function
#' rotate model at (0,0,0) axis with provided direction
#' @param points  the P * 3 matrix that is generated by other base functions
#' @param direction  an integer that is either 0, 1, 2 corresponding xy, xz, yz plane we perform rotate
#' @param angle    how much we rotate the graph in radiant, default is half pi radiant or 90 degree
#'
#' @return newpoints   the P * 3 matrix of points after the rotation is performed.
#' @export
#'
#' @examples
spin <- function(points, direction = 0, angle = pi / 2){
  # depending on the direction, transforming the point on a plane with polar system
  # then perform spin, transform back to xyz axis system

  newpoints = points
  if (direction == 0){
    # xy plane
    distance = sqrt(points[, 1]^2 + points[, 2]^2)
    signs = sign(points[, 1])
    indangle = atan(points[, 2] / (points[, 1]))
    signs[which (signs >= 0)] = 0
    indangle = indangle + pi * (signs)
    newpoints[, 1] = distance * cos(indangle + angle)
    newpoints[, 2] = distance * sin(indangle + angle)
  }
  else if (direction == 1){
    # xz plane
    distance = sqrt(points[, 1]^2 + points[, 3]^2)
    signs = sign(points[, 1])
    indangle = atan(points[, 3] / (points[, 1]))
    signs[which (signs >= 0)] = 0
    indangle = indangle + pi * (signs)
    #((-1 * (indangle + angle) %/% pi + 0.5) * 2) * signs *
    newpoints[, 1] = distance * cos(indangle + angle)
    newpoints[, 3] = distance * sin(indangle + angle)
  }
  else if (direction == 2){
    # yz plane
    distance = sqrt(points[, 3]^2 + points[, 2]^2)

    signs = sign(points[, 3])
    indangle = atan(points[, 2] / points[, 3])
    signs[which (signs >= 0)] = 0
    indangle = indangle + pi * (signs)
    newpoints[, 3] = distance * cos(indangle + angle)
    newpoints[, 2] = distance * sin(indangle + angle)
  }


  return(newpoints)
}

#' join model function
#'
#' @param points1 the model1 control points
#' @param surface1 the model1 surface
#' @param points2 the model2 control points
#' @param surface2 the model2 surface
#'
#' @return points the combined control points
#'         surface the combined surface
#' @export
#'
#' @examples
join <- function(points1, surface1, points2, surface2){
  if(ncol(points1) != ncol(points2)){
    stop("the vertex is incorrect")
  }
  if (ncol(surface1) != ncol(surface2)){
    stop("the surface is incorrect")
  }
  len = nrow(points1)
  return (list(points = rbind(points1,points2), surface = rbind(surface1,(surface2+len))))
}


#' scalingmodel function
#'
#' @param points in control vertex that is generate by other function
#' @param scalar how much we want to change as scalar in each direction, default is c(1,1,1)
#' @param center the center point scaling to, default is c(0,0,0)
#'
#' @return  newpoints  the scaled points
#' @export
#'
#' @examples
scalingmodel <- function(points, scalar = c(1,1,1), center = c(0,0,0)){
  if (length(center) != 3){
    stop("incorrect center variable")
  }

  newpoints = t((t(points) - center) * scalar + center)
  return(newpoints)
}
