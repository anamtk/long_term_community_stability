model{

  #model loops throguh species, transects (or any other site
  #variable you want), and the years for that site, which
  #can depend on the site (not all sites have to be surveyed in
  #the same years)
  for(k in 1:n.species){
    for(i in 1:n.transects){
      for(t in n.start[i]:n.end[i]){
        
        #The biological process model is similar across MSAM models
        #biological process model
        N[k,i,t] ~ dpois(lambda[k,i,t])
        
        log(lambda[k,i,t]) <- b0.species[k] + 
          eps.site[Site.ID[i], k] +
          eps.year[Year.ID[t], k]
        
        for(r in 1:n.rep[i,t]){ #for the number of surveys on each transect in each year
          # Observation model
          #The observation model depends on the dataset and 
          #which detection covariates you are interested in incorporating
          logit(p[k,i,t,r]) <- a0[k] + #species-level intercept
            a1.Vis*vis[i,t,r] + #visibility effect, not species dependent
            a2.Size*size[k] #size effect, not species dependent
            
            #abundance is binomial based on detection probability
            #and total true abundance at the site
            y[k,i,t,r] ~ dbin(p[k,i,t,r], N[k,i,t])
            
            #this is replicated data under the model that is used
            #to assess model goodness of fit, or - how well
            #do model predicted from the median estimate of the model
            #line up with observed data 
            y.rep[k,i,t,r] ~ dbin(p[k,i,t,r], N[k,i,t])
            
        }
        
      }
      
    }
    
    #SPECIES-LEVEL PRIORS:
    
    #Note: Priors for all but detection covariate effects
    #are the same across all MSAM/MSOM models
    
    #Detection intercept and slopes
    a0[k] ~ dnorm(mu.a0,tau.a0)
    
    #"baseline" detection at vis = 0 and size = 0 
    #on standardized scale
    p0[k] <- ilogit(a0[k])
    
    #species-level intercept - 
    #currently non-identifiable:
    b0.species[k] ~ dnorm(mu.b0species, tau.b0species)
    #compute the identifiable species-level intercept:
    #TRACK THIS ONE for convergence
    b0.star[k] <- b0.species[k] + ave.eps.site[k] + ave.eps.year[k]

  }
  
  #SITE W/IN SPECIES RANDOM EFFECTS
  #sites nested within species (sites sum to zero w/in each species)
  for(s in 1:n.species){
    for(p in 1:n.sites){
      #non-identifiable random effect
      eps.site[p,s] ~ dnorm(0, tau.eps.site)
      #identifiable site random effect (monitor this)
      eps.site.star[p,s] <- eps.site[p,s] - ave.eps.site[s]
    }
    #mean site level random effects within each species
    ave.eps.site[s] <- mean(eps.site[,s])
  }
  
  #YEARS W/IN SPECIES RANDOM EFFECTS
  #sites nested within species (sites sum to zero w/in each species)
  for(s in 1:n.species){
    for(p in 1:n.years){
      #non-identifiable random effect
      eps.year[p,s] ~ dnorm(0, tau.eps.year)
      #identifiable year random effect (monitor this)
      eps.year.star[p,s] <- eps.year[p,s] - ave.eps.year[s]
    }
    #mean year level random effects within each species
    ave.eps.year[s] <- mean(eps.year[,s])
  }
  
  #initial occupancy
  #Detection intercept
  mu.a0 ~ dnorm(0, 0.001)
  tau.a0 <- pow(sig.a0, -2)
  sig.a0 ~ dunif(0, 50)
  
  #species-level abundance
  mu.b0species ~ dnorm(0, 0.001)
  tau.b0species <- pow(sig.b0species, -2)
  sig.b0species ~ dunif(0,50)
  
  #site and year variances
  sig.eps.site ~ dunif(0, 10)
  tau.eps.site <- pow(sig.eps.site, -2)
  sig.eps.year ~ dunif(0, 10)
  tau.eps.year <- pow(sig.eps.year, -2)
  
  #covariate effect priors
  a1.Vis ~ dnorm(0, 0.001)
  a2.Size ~ dnorm(0, 0.001)
  
  #missing data 
  #SOme data for visibility are missing, so we're imputing them
  for(i in 1:n.transects){
    for(t in n.start[i]:n.end[i]){
      for(r in 1:n.rep[i,t]){
        #missing data in the visibility column
        vis[i,t,r] ~ dnorm(mu.missingvis, tau.missingvis)
      }
    }
  }
  
  #PRIORS FOR IMPUTING MISSING DATA
  #Priors for mean and tau of missing covariates in the model
  mu.missingvis ~ dunif(-10, 10)
  sig.missingvis ~ dunif(0, 20)
  tau.missingvis <- pow(sig.missingvis, -2)
 
  
  #BRAY CURTIS DERIVED QUANTIIES
  #Note: these are the derived "stability" metrics
  #for any abundance models (MSAM)
  #lots of ways to calculate this, but I did this way
  #IF WE WANT TO partition components of Bray:
  for(i in 1:n.transects){
    for(t in (n.start[i]+1):n.end[i]){
      for(k in 1:n.species){
        # num individuals in both time periods per species
        a[k,i,t] <- min(N[k,i,t-1], N[k,i,t])
        # num individuals only in first time point
        b[k,i,t] <- N[k,i,t-1] - a[k,i,t]
        # num individuals only in second time point
        c[k,i,t] <- N[k,i,t] - a[k,i,t]
      }
      #for all years 2 onward:
      #total number of shared individuals across time periods
      A[i,t] <- sum(a[,i,t])
      #total number of individuals in only first time period
      B[i,t] <- sum(b[,i,t])
      #total number of individuals in only second time period
      C[i,t] <- sum(c[,i,t])
      
      #total bray-curtis (B+C)/(2A+B+C)
      num[i,t] <- B[i,t] + C[i,t]
      denom1[i,t] <- 2*A[i,t]+B[i,t]+C[i,t]
      #if all values are zero - this just keeps the eqn. from
      #dividing by zero
      denom[i,t] <- ifelse(denom1[i,t]==0,1, denom1[i,t])
      bray[i,t] <- num[i,t]/denom[i,t]
      
      #These quantities are not used in the paper, but are 
      #extensions of bray-curtis. They are based on equations
      #from Baselga 2016:
      #https://doi.org/10.1111/2041-210X.12693
      #how much is dissimilarity shaped by
      # individuals of one species being replaced by individuals
      #of another species?
      num.bal[i,t] <- min(B[i,t], C[i,t])
      denom.bal1[i,t] <- A[i,t] + num.bal[i,t]
      #sometimes this leads to dividing by zero, so
      #we can bypass that with this next line
      denom.bal[i,t] <- ifelse(denom.bal1[i,t] == 0,1, denom.bal1[i,t])
      bray_balanced[i,t] <- num.bal[i,t]/denom.bal[i,t]
      
      #how much is dissimilarity shaped by
      # individuals that are lost without substitution?
      bray_gradient[i,t] <- bray[i,t] - bray_balanced[i,t]
    }
  }
  
  

  
}