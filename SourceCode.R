library("queueing")
#Reading Tables
N1 <- read.csv('DemandaAltaNodo1.csv',header = T) #"Demanda alta" is spanish for high demand, "Nodo" is Spanish for Node 
k <- 200
#Pre-locating the Outcomes to be calculated
Outcomes <- as.data.frame(matrix(c(rep(0,dim(N1)[1]*k*8)),dim(N1)[1]*k,8))
colnames(Outcomes) <- c("t","i","Rho","Lq","Throughput","L","W","Wq")
counter <- 0
for (t in 1:dim(N1)[1]) { #t is the time in hours
  for (i in 1:k) {  #i is the No. of customers
    counter <- counter+1
    lambda <- N1$Tasa_media_llegadas[t] #"Tasa media de llegadas" is Spanish for mean arrival rate
    mu <- N1$Tasa_media_.servicio[t] #"Tasa media de servicio" is Spanish for mean service rate
    #Setting up the MMCK model's inputs
    i_mmck <- NewInput.MMCK(lambda, mu, c=i, k=i)  
    #Building the model
    o_mmck <- QueueingModel(i_mmck)
    #Retrieving Outcomes
    Outcomes[counter,] <- c(t,i,o_mmck$RO,o_mmck$Lq,o_mmck$Throughput,o_mmck$L,
                            o_mmck$W,o_mmck$Wq)
  }
}
##Determining optimum capacity (period-wise) and plotting throughput results
#Pre-locating the Optimum Capacity Vector
Optimum_Capacity <- as.data.frame(c(rep(0,dim(N1)[1])),)
par(mfrow=c(3,4))
for (t in 1:dim(N1)[1]) {
  temp <- Outcomes[(k*(t-1)+1):(k*t),]
  #Here, we use a cutoff of 99.33% of the maximum Throughput to define the optimum*
  x <- min(temp$i[temp$Throughput>=(1-exp(-5))*max(temp$Throughput)]) 
  Optimum_Capacity[t,1] <- x
   plot(temp$i, temp$Throughput, main=paste("t =",toString(t),"h"),
        xlab="Capacity (Customers)", ylab="Throughput")
   abline(v=x, col="green") #Green vertical line for optimum capacity
   abline(v=177, col="red") #Red vertical line for epidemiology-based capacity threshold
}
colnames(Optimum_Capacity) <- "Opt_Cap"
Optimum_Capacity$time <- 1:dim(N1)[1]
Optimum_Capacity <- as.data.frame(Optimum_Capacity)
## Plotting period-wise optimum capacity + epidemiology-based capacity threshold
library(ggplot2)
ggplot(Optimum_Capacity, aes(time, Opt_Cap)) +
  xlab("time (h)")+
  ylab("Optimum Capacity (Customers)")+
  geom_point()+
  geom_line()+
  #Adding horizontal dashed red line for the epidemiology-based capacity threshold
  geom_hline(yintercept=177, linetype="dashed", color = "red")