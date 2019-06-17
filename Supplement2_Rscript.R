# Two randomizations prepared in R version 3.1.2 & used to generate figure 2 of the following article: 
# authors: Wright, SJ, O Calderón, A Hernandéz, M Detto and PA Jansen. 
# title: Interspecific associations in seed arrival and seedling recruitment in a Neotropical forest

# <function> 
# <name> 
# randomization1
# </name> 
# <description> 
# Randomization to evaluate the null hypothesis that  species arrive independently for each census. <br> 
# Randomization 1 assigns species to traps randomly for each census. <br>
# The response metric is the variance of the number of species over traps for each census. <br>
# The observed variance is compared to 999 randomized variances for each census. <br> 
# </description> 
# <arguments> 
# The function arguments are (1) one input file and (2) one output file. <br> 
# The input file has one row for each observed census-trap-species combination & holds the following variables: <br> 
# <ul>
# <li>census = a unique integer for each census <br>
# <li>trap = a unique integer for each trap <br>
# <li>sp = a mnemonic that uniquely represents a species <br>
# <ul>
# The output file holds the following variables: <br>
# <ul>
# <li>census = a unique integer for each census <br>
# <li>obsvar = the numeric observed variance of number of species captured over traps <br>
# <li>count = an integer between 0 and 999 (number of randomized variances < observed variance) <br>
# <li>n_sp =  an integer equal to the number of species captured over all traps <br>
# <li>n_records = an integer equal to the number of trap-species combinations encountered <br> 
# </ul>
# </arguments> 
# <sample> 
# No example is given. <br>
# </sample> 
# <source> 
randomization1=function(infile, outfile) {
  # Calculate the observed variance of the number of species over traps for each census. Store in "obs".
  a = read.table(infile,header=T,as.is=T)
  ntraps = dim(as.data.frame(table(a$trap)))[1]
  b = as.data.frame(table(a$census,a$trap))
  names(b) = c("census","trap","n_sp")
  obs = as.data.frame.table(tapply(b$n_sp,b$census,var))
  names(obs) = c("census","obsvar")
  obs$count = 0; obs$n_sp = 0; obs$n_records = 0
  # Calculate the number of traps for each species-census combination. Store in "cts".
  cts = as.data.frame(table(a$sp,a$census))
  names(cts) = c("sp","census","n_traps")
  cts = cts[cts$n_traps>0,]
  # Change variable types from factor to character. This cuts run time by about 60%.
  obs$census = as.character(obs$census)
  cts$census = as.character(cts$census)
  cts$sp = as.character(cts$sp)
  # Initiate the randomization.
  x = 1:ntraps
  for (census in 1:dim(obs)[1]) {
    print(census)
    thiscen = obs$census[census]
    thiscendata = cts[cts$census==thiscen,]
    thiscensp = dim(thiscendata)[1]
    for (permutation in 1:999) {
      tally = rep(0,ntraps)
      for (sp in 1:thiscensp) {
        randomtraps = sample(x, thiscendata$n_traps[sp], replace = FALSE, prob = NULL)
        tally[randomtraps] = tally[randomtraps] + 1
      } # End species loop
      obs$count[obs$census==thiscen] = ifelse(obs$obsvar[obs$census==thiscen]>var(tally), obs$count[obs$census==thiscen]+1, obs$count[obs$census==thiscen])
    } # End permutation loop.
    obs$n_sp[obs$census==thiscen] = thiscensp
    obs$n_records[obs$census==thiscen] = sum(thiscendata$n_traps)
  } # End census loop.
  write.table(obs,file=outfile,row.names=F,sep="\t")
} # End function randomization1
# </source> 
# </function> 

# <function> 
# <name> 
# randomization2
# </name> 
# <description> 
# Randomization to evaluate the null hypothesis that species arrive independently for each census. <br> 
# Randomization 2 attempts to maintain the observed spatial pattern of seed fall for each species. <br>
# A random integer (from 1 to the number of traps) is added to trap for each species and census. <br>
# This preserves seed fall spatial pattern to the extent that trap numbers capture trap location. <br>
# Traps are numbered sequentially along 5 trails in the BCI 50-ha plot. <br>
# Only the 6 breaks between trails disrupt the randomized spatial pattern. <br> 
# Otherwise, randomization 2 is similar to randomization 1. <br>
# The response metric is the variance of the number of species over traps for each census. <br>
# The observed variance is compared to 999 randomized variances for each census. <br> 
# </description> 
# <arguments> 
# The function arguments are (1) one input file and (2) one output file. <br> 
# The input file has one row for each census-trap-species combination & the following variables: <br> 
# <ul>
# <li>census = a unique integer for each census <br>
# <li>trap = a unique integer for each trap (includes all values between 1 and the number of traps) <br>
# <li>sp = a mnemonic that uniquely represents a species <br>
# <ul>
# The output file 'holds the following variables: <br>
# <ul>
# <li>census = a unique integer for each census <br>
# <li>obsvar = the numeric observed variance of number of species captured over traps <br>
# <li>count = integer between 0 and 999 (number of randomized variances < observed variance)  <br>
# <li>n_sp =  an integer equal to the number of species captured over all traps <br>
# <li>n_records = an integer equal to the number of trap-species combinations encountered <br> 
# </ul>
# </arguments> 
# <sample> 
# No example is given. <br>
# </sample> 
# <source> 
randomization2=function(infile, outfile) {
  # Calculate the observed variance of the number of species over traps for each census ("obs").
  a = read.table(infile,header=T,as.is=T)
  ntraps = dim(as.data.frame(table(a$trap)))[1]
  b = as.data.frame(table(a$census,a$trap))
  names(b) = c("census","trap","n_sp")
  obs = as.data.frame.table(tapply(b$n_sp,b$census,var))
  names(obs) = c("census","obsvar")
  obs$census = as.character(obs$census)
  obs$count = 0; obs$n_sp = 0; obs$n_records = 0
  # Initiate the randomization.
  for (census in 1:dim(obs)[1]) {
    print(census)
    thiscen = obs$census[census]
    thiscendata = a[a$census==thiscen,]
    thiscensp = as.data.frame(dimnames(table(thiscendata$sp)))
    names(thiscensp)="sp"
    thiscennumsp = dim(thiscensp)[1]
    for (permutation in 1:999) {
      r=as.data.frame(as.integer(floor(runif(thiscennumsp,1,ntraps))))
      names(r)="r"
      r=cbind(thiscensp,r)
      u=merge(thiscendata,r,by="sp",all.x=T)
      u$rtrap=(u$trap+u$r)%%ntraps
      v=as.vector(table(u$rtrap)) # number of species in each trap with 1 or more randomized species
      w=as.vector(rep(0,ntraps-length(v)))
      x=c(v,w) 
      obs$count[obs$census==thiscen]=ifelse(obs$obsvar[obs$census==thiscen]>var(x), obs$count[obs$census==thiscen]+1, obs$count[obs$census==thiscen])
    } # End permutation loop.
    obs$n_sp[obs$census==thiscen] = thiscennumsp
    obs$n_records[obs$census==thiscen] = dim(thiscendata)[1]
  } # End census loop.
  write.table(obs,file=outfile,row.names=F,sep="\t")
} # End function randomization2
# </source> 
# </function>
