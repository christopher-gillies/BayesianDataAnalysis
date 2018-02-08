y = function(x,mu=0,sigma=1) {
  exp( - ((x - mu)^2) / (2 * sigma^2) )
}

nc = function(sigma=1) {
  sqrt(2 * pi * sigma^2)
}

integrate(y,-10,10)
nc()

y(0)/dnorm(0)

integrate(function(x) { y(x,sigma=2) },-10,10)
nc(sigma=2)

prior = function(t,mean=0,sd=1) {
  dnorm(t,mean=mean,sd=sd) * nc(sd)
}

likelihood = function(x,t) {
  dnorm(x,mean = t) * nc(1)
}

joint = function(x,t,prior_mean=0,prior_sd=1) {
  likelihood(x,t) * prior(t,mean=prior_mean,sd=prior_sd)
}

# unormalized joint

joint_un = function(x,t,prior_mean=0,prior_sd=1) {
  y(x,mu=t,sigma=1) * y(t,mu=0,sigma=1)
}

posterior = function(t,y,sd=1,prior_mean=0,prior_sd=1) {
  m = (1/prior_sd^2 * prior_mean + 1/sd^2 * y) / (1/prior_sd^2 + 1/sd^2)
  s2 = 1/(1/prior_sd^2 + 1/sd^2)
  dnorm(t,mean=m,sd=sqrt(s2))
}

posterior_nc = function(sd=1,prior_sd=1) {
  s2 = 1/(1/prior_sd^2 + 1/sd^2)
  nc(sqrt(s2))
}

plot( seq(-10,10,0.1), joint(1,seq(-10,10,0.1),prior_mean=0,prior_sd = 1),type="l" )

#integrate(function(t) { joint(1,t,prior_sd = 10) },lower=-10,upper=10 )
joint_nc =  integrate(function(t) { joint(1,t) },lower=-10,upper=10 )$value
joint_un_nc = integrate(function(t) { joint_un(1,t) },lower=-10,upper=10 )$value

plot( seq(-10,10,0.1), joint(1,seq(-10,10,0.1) ,prior_mean=0,prior_sd = 1) / joint_nc ,type="l" )

integrate(function(t) { joint(1,t) / joint_nc },lower=-10,upper=10 )$value

plot( seq(-10,10,0.1), posterior(seq(-10,10,0.1),1) ,type="l" )

joint(1,1) / posterior(1,1)
joint(1,2) / posterior(2,1)