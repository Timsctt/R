df<-iris
a<-0.08


n<-150
Xbar<-colSums(df$Species/n)
sum[df$Species,versicolor]
Ibinf<-Xbar-qnorm(1-a/2)*s/sqrt(n)
Ibsup<-Xbar+qnorm(1-a/2)*s/sqrt(n)
