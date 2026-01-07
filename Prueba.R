# este es un documento de prueba

#install.packages(
#  "INLA",
#  repos = c(getOption("repos"),
#            INLA = "https://inla.r-inla-download.org/R/stable"),
#  dep = TRUE
#)
#https://www.paulamoraga.com/book-spatial/
#https://ramanbala.github.io/dynamic-time-series-models-R-INLA/preface.html#software-information-and-conventions  
library(INLA)
library(tidyverse)
library(MASS)
summary(cement)
####MODELOS LINEALES
#la hipotesis nula es que los coefecientes sean iguales a 0

fit1<-lm(y ~ x1 + x2 + x3 + x4, cement)
summary(fit1)
fit2<-lm(y ~ x2 + x3 + x4, cement)
summary(fit2)
fit3<-lm(y ~ x1, cement)
summary(fit3)
fit4<-lm(y ~ x1+x2, cement)
summary(fit4)
####INLA
df<- rbind(cement, data.frame(x1 = 24, x2 = 24, x3 = 24, x4 = 24, y  = NA))
formula_1 <- y ~ 0 + 1 + x1 + x2 + x3 + x4
model_1_1 <- inla(formula = formula_1, 
                  family  = "gaussian", #distribución del error y estamos trabajando que los x_t son campos Gaussianos
                  data    = df,
                  control.predictor = list(compute = TRUE),
                  control.compute=list(return.marginals.predictor=TRUE))# Should the marginals for the linear predictor be computed?
summary(model_1_1)

#intercepto-------------------------
alpha<-model_1_1$marginals.fixed[[1]]
ggplot(data.frame(inla.smarginal(alpha)),aes(x,y))+
  geom_line()+
  labs(x="", y="", title="Posterior of alpha")

#el valor del cuantil, que alcanzamos una probabilidad del 5% 
qq<-inla.qmarginal(0.05,alpha)
qq

inla.dmarginal(0,alpha)

#presición
sigma_2<-inla.tmarginal(fun=function(x){1/x},marginal=model_1_1$marginals.hyperpar$`Precision for the Gaussian observations`)
ggplot(data.frame(inla.smarginal(sigma_2)))+
  geom_line(aes(x,y))+labs(x="", y="", title="Posterior of the variance")

#marginals fitted values
#cada elemento de la lista cirresoibde  a una observacion
post_fitted<-model_1_1$marginals.fitted.values
post_margin<-data.frame(do.call(rbind, post_fitted))
post_margin$cement<-rep(names(post_fitted),times=sapply(post_fitted,nrow))
ggplot(post_margin)+geom_line(aes(x,y))+facet_wrap(~cement, ncol=5)+labs(x="", y="Density")



#CAP # del libro dinamic time series models
source("functions_custom.R")
sim.rw1 <-
  simulation.from.rw1(
    sample.size = 500,
    burn.in = 100,
    level = 0,
    drift = 0,
    V = 0.5,
    W = 0.25,
    plot.data = TRUE,
    seed = 123457
  )
sim.rw1$sim.plot

y.rw1 <- sim.rw1$sim.data
n <- length(y.rw1)
id.w <- 1:n
rw1.dat <- cbind.data.frame(y.rw1, id.w)

formula.rw1 <- y.rw1 ~ f(id.w, model = "rw1",
                         constr = FALSE) - 1 #no queremos intercepto
model.rw1 <- inla(
  formula.rw1,
  family = "gaussian", #los Y son familia gaussina
  data = rw1.dat,
  control.predictor = list(compute = TRUE)
)
summary(model.rw1)$random.model
summary(model.rw1)$hyperpar
head(summary(model.rw1)$linear.predictor)

abline(v = -0.06686987)
plot(
  density(phis[3, ]),
  type = "l",
  main="",
  xlab = expression(phi[3]),
  ylab = "density"
)
abline(v=-0.02494843)

# cambios
