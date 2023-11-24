Sousgrad <- function(f,domain,x0,t0) {

  x_k <- x0 #point courant
  t_k <- t0 #pas courant
  k <- 0 #nombre d'iterations
  while (TRUE) {    #calcul du sous-gradient
    g <- approxfun(f,domain)    # cree une fonction linéaire
    dg <- D(g,"x")    #calcule de la derivée de dg
    g_k <- dg(x_k) #calcul le sous-gradient au point x_k
    # Mise a jour du point courant
    x_k <- x_k - t_k * g_k # Met a jour le point courant
    x_k <- min(max(x_k, domain1), domain2)# Le projette sur le domaine
    # Mise a jour du pas
    k <- k + 1 # Incremente le nombre d’iterations
    t_k <- t0 / sqrt(k) # Met a jour le pas
    # Critere d’arret
    if (abs(g_k) < 1e-6 || abs(f(x_k) - f(x_k-1)) < 1e-6 || k > 1000)
    { break # Sort de la boucle
    }
  }

  return(x_k) # Retourne le resultat
  cat("La valeur minimale de la fonction est", f(x_k), "au point", x_k, "\n") # Affiche la valeur minimale
  cat("Le nombre d’itérations effectuees est", k, "\n") # Affiche le nombre d’itérations

}


