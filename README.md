# Project - Section view of a 3d model.

created by Tianxiang Chu for study purpose.

# Introduction

this package is designed to help generate simple 3d model with very
basic shape. the model is stored as 2 matrix of points and connections.
Also, package can generate a graph of models' section view in given plane.

# installation instruction.

the project will be posted on github, using devtools to install from github
using following line

devtools::install_github("deathince/ModelSectionView",build_vignettes = TRUE)

# GEtting start

If there is a model ready to use. That is a V * 3 matrix indicating the location of each vertex
and S * 3 matrix indicate every triangle surface that is created by corresponding 3 vertices --
in one row, row with (1,2,3) indicates the surface constructed by 1,2,3rd vertex in vertex matrix.
simply call sectionview(vertexmatrix, surfacematrix, plane = c(0,0,1), k = 0) which will return an
xy plane section view. the default plane equation is 0x + 0y + 1z = k = 0.  its highly recommended to cut
along axis direction.

if there is no model ready, it is good to start by calling dummymodel, there are preset such as a cube in the dummymodel.
simply call the function and return corresponding vertex point and surface.

there package also provide tools from drawing model from 0.creating new triangles with bashshapenew function and
using spin, scale, join, shift to modify new model.
