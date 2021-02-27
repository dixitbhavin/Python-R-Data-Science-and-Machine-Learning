Path="C:/Users/dixit/Desktop/"
library(crp.CSFP)
portfolio<- read.csv(paste(Path,"portfolio.csv",sep=""))
portfolio
rating.scale<- read.csv(paste(Path,"rating_pd.csv",sep=""))
rating.scale
MyModel<-init(path.in=Path,path.out=Path,port.name="portfolio.csv",
              rating.scale.name = "rating_pd.csv",sec.var.name = "pd_sector_var.csv",
              portfolio = portfolio,rating.scale = rating.scale,sec.var = data.frame(Var=runif(3,1,3)),
              alpha = c(0.95,0.99,0.999),export.to.file = TRUE,calc.rc = TRUE,
              Niter.max = 0,PLOT.scale = 1e+06,PLOT.range.x = c(0, 0))
str(MyModel)
MyModel=crp.CSFP(MyModel)
