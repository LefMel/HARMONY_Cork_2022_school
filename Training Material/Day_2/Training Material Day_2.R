# HARMONY CA18208 Training School 

# Diagnostic test evaluation with Bayesian Latent Class Models


# 17 - 18 November 2022
# Cork, Ireland


# Day 2 Material

# Exercise 3 - Two test, two population model

# 1.	Load the dataset that is as a csv file (ex3_data.csv) in your working directory
df <- read.csv("ex3_data.csv")

# 2.	Create a contingency table for each population
table(df$test1, df$test2, df$Pop, dnn=c('test1', 'test2', 'Population'))

# 3. Write the model and save as “2_pop_mod.bug”
# You can skip this step, because the 2_pop_mod.bug file is is already in your working directory.


# 4.	Provide the data of the cell counts
t1 = c(32, 35, 11, 172)
n1=250

t2 = c(11, 16, 0, 223)
n2=250


# 5.	Provide initial values and run the model
se1=list(chain1=0.05, chain2=0.95)

results <- run.jags('2_pop_mod.bug', adapt = 1000, n.chains=2, 
                    burnin=1000, sample=10000)

plot(results, vars = "se1")
results

# 6.	Change the priors to beta(1,1) one by one and see what happens to your estimate – 
#how does the sensitivity of this model to the priors compare with the priors for the one population model?

# You will need to open the "2_pop_mod.bug" file and change the dbetas() in the prior part one by one
# REMEMBER to save the bug file each time and repeat steps 3-5 again for each scenario.

###contingency tables
df %>% mutate(x111 = ifelse(test1==1 & test2==1 & test3==1, 1, 0),
              x100 = ifelse(test1==1 & test2==0 & test3==0, 1, 0),
              x010 = ifelse(test1==0 & test2==1 & test3==0, 1, 0),
              x001 = ifelse(test1==0 & test2==0 & test3==1, 1, 0),
              x110 = ifelse(test1==1 & test2==1 & test3==0, 1, 0),
              x101 = ifelse(test1==1 & test2==0 & test3==1, 1, 0),
              x011 = ifelse(test1==0 & test2==1 & test3==1, 1, 0),
              x000 = ifelse(test1==0 & test2==0 & test3 == 0, 1, 0)) %>% 
  group_by(Population) %>% 
  summarise(x111=sum(x111), x100=sum(x100), x010=sum(x010),
            x001=sum(x001), x110=sum(x110), x101=sum(x101),
            x011=sum(x011), x000=sum(x000))
                        


se2=list(chain1=0.2, chain2=0.8)

t1 = c(0, 3, 6, 2, 0, 1, 7, 131)
n1=150

t2 = c(3, 8, 3, 11, 0, 0, 11, 164)
n2=200

results <- run.jags('3_test_mod.bug', adapt = 1000, n.chains=2, 
                    burnin=1000, sample=10000)

plot(results, vars = "se3")
results

# Exercises 4 - 5 

# Now we are going to introduce the concept of conditional dependence between tests. Please have a close look at the 
# related reading material titled "3.BLCM Assumptions_LMessam" and have a look at the following references:
# 1 - Bayesian Approaches to Modeling the Conditional Dependence Between Multiple Diagnostic Tests - Nandini Dendukuri (2004) "https://doi.org/10.1111/j.0006-341X.2001.00158.x"
# 2- On the interpretation of test sensitivity in the two-test two-population problem: Assumptions matter - Wesley O.Johnson (2009) "https://doi.org/10.1016/j.prevetmed.2009.06.006"


# Exercise 4 - Three tests without conditional dependence between tests

# 1.	Load the dataset that is as a csv file (ex45_data.csv) in your working directory
df <- read.csv("ex45_data.csv")
install.packages("dplyr")
library(dplyr)

# 2.	Create a contingency table for each population
###contingency tables
df %>% mutate(x111 = ifelse(test1==1 & test2==1 & test3==1, 1, 0),
              x100 = ifelse(test1==1 & test2==0 & test3==0, 1, 0),
              x010 = ifelse(test1==0 & test2==1 & test3==0, 1, 0),
              x001 = ifelse(test1==0 & test2==0 & test3==1, 1, 0),
              x110 = ifelse(test1==1 & test2==1 & test3==0, 1, 0),
              x101 = ifelse(test1==1 & test2==0 & test3==1, 1, 0),
              x011 = ifelse(test1==0 & test2==1 & test3==1, 1, 0),
              x000 = ifelse(test1==0 & test2==0 & test3 == 0, 1, 0)) %>% 
  group_by(Population) %>% 
  summarise(x111=sum(x111), x100=sum(x100), x010=sum(x010),
            x001=sum(x001), x110=sum(x110), x101=sum(x101),
            x011=sum(x011), x000=sum(x000))


# 3. Write the model and save as “3_test_mod.bug”
# You can skip this step, because the 3_test_mod.bug file is is already in your working directory.


# 4.	Provide the data of the cell counts
t1 = c(0, 3, 6, 2, 0, 1, 7, 131)
n1=150

t2 = c(3, 8, 3, 11, 0, 0, 11, 164)
n2=200


# 5.	Provide initial values and run the model
se2=list(chain1=0.2, chain2=0.8)

results <- run.jags('3_test_mod.bug', adapt = 1000, n.chains=2, 
                    burnin=1000, sample=10000)

plot(results, vars = "se1")
plot(results, vars = "se2")
plot(results, vars = "se3")
plot(results, vars = "pi1")
plot(results, vars = "pi2")
plot(results, vars = "sp1")
plot(results, vars = "sp2")
plot(results, vars = "sp3")

results

# Exercise 5 - three tests with conditional dependence
#Now we'll assume that tests 2 and 3 are conditionally dependent (for example the measure the same "thing/antibodies") 
# so we expect the test from the first test to carry information on the result to the second test.
# We need to take into account this dependency by including parameters/terms that capture this conditional dependence

# 1.	Load the dataset that is as a csv file (ex45_data.csv) in your working directory
df <- read.csv("ex45_data.csv")
#install.packages("dplyr")
library(dplyr)

# 2.	Create a contingency table for each population
###contingency tables
df %>% mutate(x111 = ifelse(test1==1 & test2==1 & test3==1, 1, 0),
              x100 = ifelse(test1==1 & test2==0 & test3==0, 1, 0),
              x010 = ifelse(test1==0 & test2==1 & test3==0, 1, 0),
              x001 = ifelse(test1==0 & test2==0 & test3==1, 1, 0),
              x110 = ifelse(test1==1 & test2==1 & test3==0, 1, 0),
              x101 = ifelse(test1==1 & test2==0 & test3==1, 1, 0),
              x011 = ifelse(test1==0 & test2==1 & test3==1, 1, 0),
              x000 = ifelse(test1==0 & test2==0 & test3 == 0, 1, 0)) %>% 
  group_by(Population) %>% 
  summarise(x111=sum(x111), x100=sum(x100), x010=sum(x010),
            x001=sum(x001), x110=sum(x110), x101=sum(x101),
            x011=sum(x011), x000=sum(x000))


# 3. Write the model and save as “3_test_cov.bug”
# You can skip this step, because the 3_test_cov.bug file is is already in your working directory.


# 4.	Provide the data of the cell counts
t1 = c(0, 3, 6, 2, 0, 1, 7, 131)
n1=150

t2 = c(3, 8, 3, 11, 0, 0, 11, 164)
n2=200


# 5.	Provide initial values and run the model
se2=list(chain1=0.2, chain2=0.8)

results <- run.jags('3_test_cov.bug', adapt = 1000, n.chains=2, 
                    burnin=1000, sample=10000)

plot(results, vars = "se1")
plot(results, vars = "se2")
plot(results, vars = "se3")
plot(results, vars = "pi1")
plot(results, vars = "pi2")
plot(results, vars = "sp1")
plot(results, vars = "sp2")
plot(results, vars = "sp3")
plot(results, vars = "CovSe23")
plot(results, vars = "CovSp23")


results



