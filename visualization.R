
par(mfrow=c(1,1)) # mfrow:행을 중심으로 그래프 채움, mfcol: 열을 중심으로 그래프 채움

a=range(0:10, by=5)
a <- seq(0,100, 5)
a
a <- seq(0,10,0.01)

par(mfrow=c(2,2))

library(ggplot2)

plot(sin(a))
plot(cos(a))
plot(sin(a)*cos(a))
plot(tanh(a))


iris %>% head()

iris %>% ggplot(aes(x=))