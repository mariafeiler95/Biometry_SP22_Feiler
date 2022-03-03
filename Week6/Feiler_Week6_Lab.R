data.all <- list()
for (i in 1:10) {
        data.all[[i]] <- rnorm(10)
}

p.values <- matrix(ncol = 10,
                   nrow = 10
                   )
for (i in 1:9){
        for (j in (i+1):10){
                p.values[i,j] <- t.test(data.all[[i]],
                                        data.all[[j]])$p.value
        }
}

false.positives <- sum(p.values<0.05, na.rm = TRUE)

k <- 45
new.threshold.B <- 0.05/k

false.positives.B <- sum(p.values < new.threshold.B,
                         na.rm = TRUE)

new.threshold.DS <- 1-((1-0.05)^(1/k))

p.values.all<-c()
min.p.values.all<-c()
for (k in 1:1000){
        data.null<-list()
        for (i in 1:10)
        {
                data.null[[i]]<-rnorm(10)  #Note the double brackets for a list
        }
        p.values.null<-matrix(ncol=10,nrow=10)
        for (i in 1:9)
        {
                for (j in (i+1):10)
                {
                        p.values.null[i,j]<-t.test(data.null[[i]],data.null[[j]])$p.value 
                }
        }
        p.values.all<-c(p.values.all,c(p.values.null)[!is.na(c(p.values.null))])
        min.p.values.all<-c(min.p.values.all,min(c(p.values.null)[!is.na(c(p.values.null))]))
}
new.threshold.R<-quantile(min.p.values.all,probs=c(0.05))

false.positives.R<-sum(p.values<new.threshold.R,na.rm=T)
