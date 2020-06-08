N <- 1000 # Total cards
K <- 10   # Cards per species
S <- 100  # Total species

card_list <- expand.grid( s = 1:S, c = 1:K )
sims <- 100
n_unique <- matrix( NA, N, sims )

for ( j in 1:sims ){ 
  for( i in 1:N){ 
    n_unique[i, j] <- length( unique( card_list[ sample(1:N, i, replace = F ) ,  ]$s ))
  }
}

n_unique[1:100, 1:3]


matplot( n_unique, col = 'lightgray', lty = 1, pch = 1)
points(1:N, rowMeans( n_unique), type = 'l', col = 'red' )

