# Project - Section view of a 3d model.

created by Tianxiang Chu for study purpose.

# Introduction

this package is designed to help generate simple 3d model with very
basic shape. the model is stored as 2 matrix of points and connections.
Also, package can generate a graph of models' section view in given plane.

# installation instruction.

the project will be posted on github, using devtools to install from github
using following line

devtools::install_github("deathince/ModelSectionView")

# the on going work

I have create most the functions' skeletons. Many of them are designed
to generate some model as user's favor. I need to wrap up the unfinished 
functions and maybe transfer some code to C++ for better looping
Also, I did not write function that generate section view yet. I should 
have it updated in the later work.

I talked with my geometric modeling's instructor. He points out that the
best way to store 3d model in my case is to store the surface instead of 
edges. Otherwise there is no guarantee of correctness of section view. 
therefore, I need to change way of storing data as vertex and triangle 
surface that contain 3 vertex at most.
