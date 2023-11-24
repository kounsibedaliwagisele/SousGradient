
#fonction d'entrainement du reseau de neurone avec l'algorithme du sous gradient
train_neuronal_network<- function(image,labels,num_epochs,learning_rate){
  #initialisation des poids et biais avec des valeurs aléatoires
  weights<-runif(ncol(images))
  biais<- runif(1)
  # boucle d'entrainement sur le nombre d'epoque
  for (epoch in 1:num_epochs) {
    #calcul de la propagation avant et calcul de la prediction
    predictions<- predict_neuronal_network(images,weights,biais)
    #calcul de la perte avec la fonction perte avec l'entropie
    loss<-mean_squared_error(predictions,labels)
    #calcul du sous gradient de la fonction perte
    d_weights<-2*(predictions-labels)*images


  }
}
