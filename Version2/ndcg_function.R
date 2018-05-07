ndcg <- function(label, id, predictions, k){
  d <- data.frame(relevance = label, id=id, order = predictions, one=1)
  d <- d[order(d$id, -d$relevance),]
  d$co<-with(d, unlist(tapply(one, id, cumsum)))
  d$v<-with(d, (2^relevance-1)/(log(co+1, 2)))
  dcg0<-with(d[d$co<=k,], unlist(tapply(v, id, sum)))
  d<-d[order(d$id, -d$order),]
  d$co<-with(d, unlist(tapply(one, id, cumsum)))
  d$v<-with(d, (2^relevance-1)/(log(co+1, 2)))
  dcg<-with(d[d$co<=k,], unlist(tapply(v, id, sum)))
  return(mean(dcg/dcg0))
}

