# Translate matrix so that t0 is at origin: ------------------------------------
translate_matrix = function(X) {
  # This function translates an n x 3 matrix, so the first point is at the
  # origin and time zero. We assume the matrix is already ordered by t.
  # 
  # Inputs: X - a numeric matrix with three columns
  # Outputs: the translated matrix
  
  cbind( X[, 1] - X[1, 1], X[,2] - X[1, 2], X[ ,3] - X[1, 3] )  
  #X = matrix(ncol = 3, nrow = nrow(X))
  #for (i in 1:3) {
  #  X[, i] = X[, i] - X[1, i]
  #}
  #return(X)
}

# Find angle in radian between final position and the origin: ------------------
get_theta = function(xy) {
  # This function computes the angle in radians between final position (x, y)
  # of the x-axis and the origin.
  #
  # Inputs: xy - a numeric vector with length of 2
  # Outputs: an angle in radian betwenn [-pi, pi]

  return(atan2(xy[2], xy[1]))
}

# Rotation function: -----------------------------------------------------------
rotate_xy = function(theta, X) {
  # This function rotates the (x, y) coordinates to let final point lies along 
  # the positive x-axis
  #
  # Inputs: theta - angle in radian ranges in [-pi, pi] to rotate (x, y)
  #         X - an n x 3 numeric matrix
  # Outputs: an n x 3 matrix after rotation
  
  r = matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), 2, 2)
  return( cbind( X[, c(1, 2)] %*% r, X[, 3] ) )
}

# Combine above functions to normlaize a trajectory: ---------------------------
normalize_matrix = function(X) {
  # This function normalizes an n x 3 matrix to let it begin at the origin and
  # end at the positive x-axis.
  #
  # Inputs: X - an n x 3 numeric matrix
  # Outputs: an n x 3 numeric matrix after normalization
  
  X_tran = translate_matrix(X)
  xy = as.numeric(X_tran[nrow(X_tran), c(1,2)])
  theta = get_theta(xy)
  rotate_xy( theta = theta, X = X_tran )
}

# Compute measures: ------------------------------------------------------------
# Total distance function
get_dist = function(X_norm) {
  # This function computes the total Euclidean distance traveled
  #
  # Inputs: X_norm - an n x 3 numeric matrix after normalization
  # Outputs: a value represents total distance traveled
  
  sum( sqrt( diff( X_norm[, 1])^2 + diff( X_norm[, 2])^2 ) )
}

# Absolute area under curve
get_auc = function(x, y, absy = TRUE) {
  # This function computes the area under curve of (x, y) integrated using 
  # trapezoidal ruleand it allows cancellation in x and not in y.
  #
  # Inputs: x, y - numeric vectors
  # Outputs: total area under curve
  
  if (absy) {
    return( sum( ( abs( y[-length(y)] ) + abs( y[-1] ) ) * diff(x) * 1/2 ) )
  } else {
    return( sum( ( y[-length(y)] + y[-1] ) * diff(x) * 1/2 ) )
  }
}

# Compute curvature mearsure in combination of above two functions: ------------
get_curvature = function(X_norm) {
  # This function computes the following curvature measures from an n x 3 
  # normalized matrix.
  #
  # Inputs: X_norm - an n x 3 numeric matrix
  # Outputs: 
  #   dist - the total Euclidean distance traveled along the trajectory
  #   max_abs_dev - the maximum absolute deviation from the secant line 
  #                 representing a direct path from the first to last point.
  #   avg_abs_dev - the average abosolute deviation form the secant line.
  #   auc - the area under curve by using the trapezoidal rule.

  list(
    "dist" = get_dist(X_norm),
    "max_dev" = max( abs(X_norm[, 2]) ),
    "mean_dev" = mean( abs(X_norm[, 2]) ),
    "area" = get_auc( X_norm[,1], X_norm[,2] )
    )
}