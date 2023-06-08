install.packages(c("StanHeaders", "rstan", "brms", "cmdstanr"), dependencies = TRUE)
library(brms)
#library(cmdstanr)
install.packages(c("boot", "ggdist"))

options(mc.cores = parallel::detectCores())

 #What is the effect of weather on coati sleep?
#sleep variables: SPT, TST, sleep efficiency
#weather variables: rain, temp, humidity (, wind)

# Model1. How does rain affect sleep efficiency?
#We r building a model to predict sleep eff based on rain.
#sleep efficiency ~ rain 
#(sleep eff is a function of rain)
#sleep efficiency follows a beta distribution therefore our likelihood function (model family) will be beta.
#we have repeated measure of the same animals therefore we will have a mixed model structure. this means we add a random effect of individual.
# sleep efficiency ~ rain + (1 | animal ID). this is our random effect + rain
# to include a random slope (each animal having own sensitivity ot rain), we replace 1 w rain so: sleep efficiency ~ rain + (rain | animal ID). 

get_prior(sleep_eff ~ rain + (rain | tag), 
          data = sleep_per_nona[complete.cases(sleep_per_nona[c("sleep_eff","rain","tag")]), ],  #defining our data
          family = Beta(link = "logit"))
plot(density(x = boot::inv.logit(ggdist::rstudent_t(10000, 3, 0, 1.45)))) #plotting a student t distribution which is normal AND w/ long tails, and has degrees of freedom as additional parameters (always 3). this displays it as probabilities. gives us optimal flatness
#plot(density(x = boot::inv.logit(rnorm(10000, 0, 1.45)))) #NORMAL and not student it. its not as flat bc tails dont give it the cleaner edge on top
#rstudent_t, rnorm, rgamma, rbeta r the most useful distributions w diff parameters.
#plot(density(x = ggdist::rstudent_t(1000, 3, 0, 2))). #this is not on the probability scale!

model1 = brm(sleep_eff ~ rain + (rain | tag), 
             data = sleep_per_nona[complete.cases(sleep_per_nona[c("sleep_eff","rain","tag")]), ],  #defining our data
             family = Beta(link = "logit"),   #defining our model
             prior = c(                       #setting priors
               prior(student_t(3, 0, 1.45), class = Intercept),
               prior(student_t(3, 0, 1.45), class = b),
               prior(student_t(3, 0, 1.45), class = sd)
                      ),
             save_pars = save_pars(all = TRUE), #all parameters that r calculated by brms get saved so we can use them afterwards e.g. for model comparisons.
             iter = 2000,                     #tell brms how many iterations we want, to try. 2000 standard for brms
             chains = 4,                      #4 is standard
             control = list(max_treedepth = 10, adapt_delta = .8),  #in case we have errors at the end of our model fitting bc of biases due to a structure, allows us to manipulate them
             sample_prior = "only"           
             )


# Model2. How does temperature affect sleep efficiency?
#We r building a model to predict sleep eff based on temp.
#sleep efficiency ~ temp
#(sleep eff is a function of temp)
#we can't understand temp without understanding rain bc it has an effect on temp therefore we include rain in this model. sleep efficiency ~ temp+rain
#sleep efficiency follows a beta distribution therefore our likelihood function (model family) will be beta. 
#we have repeated measure of the same animals therefore we will have a mixed model structure.this means we add a random effect of individual.
# sleep efficiency ~ temp + (1 | animal ID). this is our random effect + temp
# to include a random slope (each animal having own sensitivity of temp, we replace 1 w temp so: sleep efficiency ~ temp + (temp | animal ID) 

# Model3. How does rel humidity affect sleep efficiency?
#We r building a model to predict sleep eff based on rh.
#sleep efficiency ~ rh
#(sleep eff is a function of rh)
#we can't understand rh without understanding rain and temp therefore we include rain and temp in this model. sleep efficiency ~ rh +temp+rain
#sleep efficiency follows a beta distribution therefore our likelihood function (model family) will be beta.
#we have repeated measure of the same animals therefore we will have a mixed model structure. this means we add a random effect of individual.
# sleep efficiency ~ rh + (1 | animal ID). this is our random effect + rh
# to include a random slope (each animal having own sensitivity ot rh), we replace 1 w rh so: sleep efficiency ~ rh + (rh | animal ID). 

#do same models with SPT and TST. only difference is the shape

# Model4. How does rain affect TST?
#We r building a model to predict TST based on rain.
#TST ~ rain 
#(TST is a function of rain)
#TST follows a skewed normal distribution therefore our likelihood function (model family) will be skewed normal.
#we have repeated measure of the same animals therefore we will have a mixed model structure. this means we add a random effect of individual.
# TST ~ rain + (1 | animal ID). this is our random effect + rain
# to include a random slope (each animal having own sensitivity ot rain), we replace 1 w rain so: TST ~ rain + (rain | animal ID). 

# Model5. How does temperature affect TST?
#We r building a model to predict TST based on temp.
#TST ~ temp
#(TST is a function of temp)
#we can't understand temp without understanding rain bc it has an effect on temp therefore we include rain in this model. TST ~ temp+rain
#TST follows a skewed normal distribution therefore our likelihood function (model family) will be skewed normal.
#we have repeated measure of the same animals therefore we will have a mixed model structure.this means we add a random effect of individual.
# TST ~ temp + (1 | animal ID). this is our random effect + temp
# to include a random slope (each animal having own sensitivity of temp, we replace 1 w temp so: TST ~ temp + (temp | animal ID). 

# Model6. How does rel humidity affect TST?
#We r building a model to predict TST based on rh.
#TST ~ rh
#(TST is a function of rh)
#we can't understand rh without understanding rain and temp therefore we include rain and temp in this model. TST ~ rh +temp+rain
#TST follows a skewed normal distribution therefore our likelihood function (model family) will be skewed normal.
#we have repeated measure of the same animals therefore we will have a mixed model structure. this means we add a random effect of individual.
# TST ~ rh + (1 | animal ID). this is our random effect + rh
# to include a random slope (each animal having own sensitivity ot rh), we replace 1 w rh so: TST ~ rh + (rh | animal ID). 



# Model7. How does rain affect SPT?
#We r building a model to predict SPT based on rain.
#SPT ~ rain 
#(SPT is a function of rain)
#SPT could be skewed or a mixture distribution so we'll try both and go with what fits best => result r 2 models, A and B. A being skewed normal or skewed T and B would be mixture of 2 normals.
#we have repeated measure of the same animals therefore we will have a mixed model structure. this means we add a random effect of individual.
# SPT ~ rain + (1 | animal ID). this is our random effect + rain
# to include a random slope (each animal having own sensitivity ot rain), we replace 1 w rain so: SPT ~ rain + (rain | animal ID). 

# Model8. How does temperature affect SPT?
#We r building a model to predict SPT based on temp.
#SPT ~ temp
#(SPT is a function of temp)
#we can't understand temp without understanding rain bc it has an effect on temp therefore we include rain in this model. SPT ~ temp+rain
#SPT could be skewed or a mixture distribution so we'll try both and go with what fits best => result r 2 models, A and B. A being skewed normal or skewed T and B would be mixture of 2 normals.
#we have repeated measure of the same animals therefore we will have a mixed model structure.this means we add a random effect of individual.
# SPT ~ temp + (1 | animal ID). this is our random effect + temp
# to include a random slope (each animal having own sensitivity of temp, we replace 1 w temp so: SPT ~ temp + (temp | animal ID). 

# Model9. How does rel humidity affect SPT?
#We r building a model to predict SPT based on rh.
#SPT ~ rh
#(SPT is a function of rh)
#we can't understand rh without understanding rain and temp therefore we include rain and temp in this model. SPT ~ rh +temp+rain
#SPT could be skewed or a mixture distribution so we'll try both and go with what fits best => result r 2 models, A and B. A being skewed normal or skewed T and B would be mixture of 2 normals.
#we have repeated measure of the same animals therefore we will have a mixed model structure. this means we add a random effect of individual.
# SPT ~ rh + (1 | animal ID). this is our random effect + rh
# to include a random slope (each animal having own sensitivity ot rh), we replace 1 w rh so: SPT ~ rh + (rh | animal ID). 