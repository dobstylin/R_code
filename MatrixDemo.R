Z = matrix( c( 1, 1, 1, 1, 1, 0, 1, 9), nrow=4, ncol=2)
Y = matrix( c( 6, 0, 8, 0), nrow=4, ncol=1)
M = matrix( c( 1, 11, 0, 11, 09, 3, 0, 35, 42), nrow=3, ncol=3)
N = matrix( c( -50, 0, 50, -50, 50, 5, 0, 5, 50), nrow=3, ncol=3)
v = matrix( c( 45, 6, 9), nrow=3, ncol=1)
w = matrix( c( 2, 6, 0), nrow=3, ncol=1)
scalar = t(v)%*%w
scalar
product = -3*w
product
product = M %*% v
product
sum = M + N
sum
diff = M - N
diff
t(Z)%*%Z
solve((t(Z)%*%Z))
t(Z)%*%Y
beta = solve((t(Z)%*%Z)) %*% (t(Z)) %*% Y 
beta
det(t(Z)%*%Z)
