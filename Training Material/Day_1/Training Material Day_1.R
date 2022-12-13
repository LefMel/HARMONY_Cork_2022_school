# HARMONY CA18208 Training School 

# Diagnostic test evaluation with Bayesian Latent Class Models


# 17 - 18 November 2022
# Cork, Ireland


# Day 1 Material

# Set your working directory, with the setwd() function in the Day_1 folder that is in the material you download.

## Exercise 1 Running a basic JAGS model in R - Steps
# 1.	Open a new .txt file in the R session (File -> New File -> Text File)
# 2.	Write the model in new text file and	save this file as “basic_model.bug”

# You can skip this step, because the basic_model.bug file is is already in your working directory.
# 3.	Attach the “runjags” package 

library(runjags)

# 4.	Define the required input data:
Positives <- 20
TotalTests <- 100

# 5.	Define the initial data for the MCMC chains:
prevalence <- list(chain1=0.05, chain2=0.95)

# 6.	 Run the model
results <- run.jags('basic_model.bug', adapt = 0, n.chains=2, burnin=0, sample=100)

# 7.	 Plot and view the results
plot(results)
results
summary(results)

# 8.	 Increase the adapt iterations (to 1000) burn in (to 5000) and samples (to 10000) look at how these impact on the outputs
results <- run.jags('basic_model.bug', adapt = 1000, n.chains=2, burnin=5000, sample=10000)
plot(results)
results


## Exercise 2 Running a basic JAGS model in R - Steps

# 1.	Load the dataset that is as a csv file (ex2_data.csv) into the working directory
df <- read.csv("ex2_data.csv")
# 2.	Create a contingency table / 2x2 table based on the frequency of positives/negatives to test1,test2
table(df$test1, df$test2, dnn=c("test1", "test2"))
# 4.	Write the model and save as “basic_lca.bug”
# You can skip this step, because the basic_lca.bug file is is already in your working directory.

# Prior Part
# You can use the betaexpert() function from the prevalence R-package to estimate the a,b parameters of the beta distribution.
install.packages("prevalence")
library(prevalence)
?betaExpert
betaExpert(0.8, lower=0.6, p=0.95)
#alpha            beta    
#  1 14.84425 4.461062

# 5. Provide the data of the cell counts 
t = c(56, 69, 19, 356)
n=500

# 6. Provide initial values and run the model

inits1 <- list(se1=0.05, se2=0.05, pi=0.7, sp1=0.05, sp2=0.05)

inits2 <- list(se1=0.95, se2=0.5, pi=0.1, sp1=0.95, sp2=0.95)

results <- run.jags('basic_lca.bug', adapt = 1000, n.chains=2, 
                    burnin=1000, sample=10000, inits=list(inits1, inits2))
plot(results, vars = "se1")
plot(results, vars= "pi")
plot(results, vars = "sp1")

plot(results, vars = "se2")
plot(results, vars = "sp2")

results
summary(results)

# 7.	Things to evaluate – how sensitive is your model to the priors? – 
#How does changing the priors impact on posterior estimates, degree of autocorrelation, effective sample size?

# Play with the betaExpert function and see how changing the input affects the a,b parameters and the posterior distribution.

