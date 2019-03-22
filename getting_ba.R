load("bci.full7.rdata")
sp.BAlist <- readRDS('sp.BAlist.RDS')
sp.Glist <- readRDS("sp.Glist.RDS")


bci.full7 %>%
  names(.)

ba_2010 <- data.frame(sp = attributes(sp.Glist[[1]]$rate)$dimnames[[1]], ba = sp.BAlist[[7]]$ba$all) 

species_list <- read_csv()

#write.csv(ba_2010, "C:/Users/ahanb/OneDrive/Documents/rec_submodel/ba_2010.csv")

rba_df <- data.frame(sp = species_list$sp, r_ba = rep(0,length(species_list$sp)))

for(i in 1:nrow(species_list)){
  
  spp <- species_list$sp[i]
  mindbh <- 0.5 * species_list$dmax[i]
  rba_df[i,2] <- bci.full7 %>%
    select(sp, dbh, status) %>%
    filter(sp == spp,
           status == "A",
           dbh > mindbh) %>%
    rowwise() %>%
    mutate(ba = ba(dbh = dbh, dbhunit = "mm")) %>%
    .$ba %>% sum(.)
  
  print(paste0("sp",i,"of",length(species_list$sp)))
}

write.csv(rba_df, "rba_df.csv")

