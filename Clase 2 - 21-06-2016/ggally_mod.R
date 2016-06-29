cor_fun <- function(data, mapping, method="pearson", ndp=2, sz=10, stars=TRUE, ...){

  x <- data[,as.character(mapping$x)]
  y <- data[,as.character(mapping$y)]

  corr <- cor.test(x, y, method=method)
  est <- corr$estimate
  lb.size <- sz* abs(est)

  if(stars){
    stars <- c("***", "**", "*", "")[findInterval(corr$p.value, c(0, 0.001, 0.01, 0.05, 1))]
    lbl <- paste0(round(est, ndp), stars)
  }else{
    lbl <- round(est, ndp)
  }

  ggplot(data=data, mapping=mapping) +
    annotate("text", x=mean(x), y=mean(y), label=lbl, size=lb.size,...)+
    theme(panel.grid = element_blank())
}
