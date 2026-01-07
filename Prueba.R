# este es un documento de prueba

#Librerías
library(tsibble)
library(dplyr)
library(lubridate)
library(fpp3)
library(tidyverse)
library(leaflet)
library(zoo)
library(fable)
library(imputeTS)
library(ggplot2)
library(tidyr)
library(stringr)
library(patchwork)
library(feasts)
library(knitr)
library(leaflet)
library(readxl)
#install.packages("GGally")
library(GGally)
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

sigma2.v.dist <- inla.tmarginal(
  fun = function(x)
    exp(-x),
  marginal = model.rw1$
    internal.marginals.hyperpar$`Log precision for the Gaussian observations`
)
plot(
  sigma2.v.dist,
  type = "l",
  xlab = expression(paste(
    "Observation noise variance ", sigma[v] ^ 2, sep = " "
  )),
  ylab = "density"
)

sigma2.v.hat <-
  inla.emarginal(
    fun = function(x)
      exp(-x),
    marginal = model.rw1$internal.marginals.hyperpar$
      `Log precision for the Gaussian observations`
  )
sigma2.w.hat <-
  inla.emarginal(
    fun = function(x)
      exp(-x),
    marginal = model.rw1$internal.marginals.hyperpar$`Log precision for id`
  )
cat(paste(
  "Estimated observation noise variance, sigma2.v",
  round(sigma2.v.hat, 2),
  sep = " = "
),
"\n")
## Estimated observation noise variance, sigma2.v = 0.61
cat(paste(
  "Estimated state noise variance, sigma2.w",
  round(sigma2.w.hat, 2),
  sep = " = "
))
## Estimated state noise variance, sigma2.w = 0.22

head(model.rw1$summary.random$id.w)

#gráficar
plot.data <- model.rw1$summary.random$id %>%
  dplyr::select(time = ID, mean,
                "0.025quant", "0.975quant")
multiline.plot(plot.data, xlab = "t", ylab = "posterior mean",
               line.color = c("black", "red", "red"),
               line.size = 0.6)
fit <- model.rw1$summary.linear.predictor$mean
df <- as_tibble(cbind.data.frame(time = 1:n, y.rw1, fit))
multiline.plot(
  plot.data = df,
  title = "",
  xlab = "t",
  ylab = "y",
  line.type = "dashed",
  line.color = c("red", "blue"),
  line.size = 0.6
)


####### modelo AR(1) con niveles más ruido
sim.ar1.level <-
  simulation.from.ar(
    sample.size = 500,
    burn.in = 100,
    phi = 0.6,
    level = 1.4,
    drift = 0,
    V = 0.2,
    W = 0.1,
    plot.data = TRUE,
    seed = 123457
  )
sim.ar1.level$sim.plot
#----
y.ar1.level<-sim.ar1.level$sim.data
n<-length(y.ar1.level)
id.x<-1:n
data.ar1.level<-cbind.data.frame(y.ar1.level, id.x)
formular.ar1.level<-y.ar1.level~f(id.x,model="ar1",constr=FALSE)
model.ar1.level<-inla(formular.ar1.level,family="gaussian", data=data.ar1.level, control.predictor = list(compute=TRUE))
#distribución de los hiperparámetros
format.inla.out(model.ar1.level$summary.hyperpar[,c(1:2)])
format.inla.out(model.ar1.level$summary.hyperpar[,c(3:5)])
#con lo anterior podemos encontrar el valor de phi del ar(1), 3.5

# intercepto del modelo
format.inla.out(model.ar1.level$summary.fixed[,c(1:5)])
inla.hpdmarginal(0.95, model.ar1.level$marginals.fixed$`(Intercept)`)

###### model AR(p) con niveles de ruido
sim.ar3 <-
  simulation.from.ar(
    sample.size = 500,
    burn.in = 100,
    phi = c(0.5,-0.75,0.25),
    level = 10,
    drift = 0,
    V = 1.25,
    W = 0.05,
    plot.data = TRUE,
    seed = 123457
  )
sim.ar3$sim.plot
##3-------------------
y.ar3<-sim.ar3$sim.data
n<-length(y.ar3)
id.x<-1:n
ar3.dat<-cbind.data.frame(y.ar3, id.x)
formula.ar3 <- y.ar3 ~ f(id.x, 
                         model="ar",
                         order=3,
                         constr=FALSE)
model.ar3 <- inla(formula.ar3,
                  family = "gaussian",
                  data = ar3.dat,
                  control.predictor = list(compute=TRUE)
)
#hiperparámetros
format.inla.out(model.ar3$summary.hyperpar[,c(1:2)])

format.inla.out(model.ar3$summary.hyperpar[,c(3:5)])
#intercepto
format.inla.out(model.ar3$summary.fixed[,c(1:5)])
#valor phis en media
pacf<-model.ar3$summary.hyperpar$mean[3:5]
phi<-inla.ar.pacf2phi(pacf)
print(phi)
#distribucion de los phis
n.samples <- 10000
pacfs <- inla.hyperpar.sample(n.samples, model.ar3)[, 3:5]
phis <- apply(pacfs, 1L, inla.ar.pacf2phi)
par(mfrow = c(1, 3))
plot(
  density(phis[1, ]),
  type = "l",
  main="",
  xlab = expression(phi[1]),
  ylab = "density"
)
abline(v = -0.05012475)
plot(
  density(phis[2, ]),
  type = "l",
  main="",
  xlab = expression(phi[2]),
  ylab = "density"
)
abline(v = -0.06686987)
plot(
  density(phis[3, ]),
  type = "l",
  main="",
  xlab = expression(phi[3]),
  ylab = "density"
)
abline(v=-0.02494843)
