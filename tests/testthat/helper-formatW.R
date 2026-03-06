g<-sample(paste0("g", 1:3), 10, replace = TRUE)
s<-sample(c(0, 1), 10, replace = TRUE)
udata<-data.frame(id = 1:10, sex = s, gc = g, t1 = rnorm(10),
              t2 = rnorm(10, 5), t3 = rnorm(10, 100, 12))
traits<-c(4:6)

d1<-udata[, -traits]
i<-2
while(i <= length(traits)){
  d1<-rbind(d1,udata[, -traits])
  i <- i + 1
}

d2<-unlist(lapply(udata[, traits], c))

d3<-rep(1:length(traits), each = length(d2)/length(traits))
