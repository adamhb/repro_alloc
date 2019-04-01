library(magrittr)
library(tidyverse)
library(vegan)

#function to convert Latin names to "sp code names"
Latin2sp <- function(Latin.name){
  if(is.character(Latin.name) != T){Latin.name <- as.character(Latin.name)} 
  
  sp <- sp_code[sp_code$Latin == Latin.name,]$sp
  
  if(Latin.name %in% sp_code$Latin == F){return(Latin.name)}else{
    return(sp)}
}

########LOADING DATA###############
#loading the bci census data
load("bcifull.RData")
#loading bci wood density data
load('wsg.ctfs.Rdata')
#loading bci species table
load('bci.spptable.rdata')
#loading drought indices derived from Engelbrecht et al. 2005; DOI 10.1007/s00468-004-0393-0
d_indices <- read.csv("drought_indices_Engelbrecht_2007.csv")

load('bci.full7.rdata')
#loading harms habitat associations
#"C:\Users\ahanb\OneDrive\Documents\data\Non_Driver_Data"
sp.BAlist <- readRDS('sp.BAlist.RDS')
sp.Glist <- readRDS('sp.Glist.RDS')



#defining canopy species according to Powell et al., 2018; doi: 10.1111/nph.15271
canopy_species_bci <- bci.full %>% select(sp, dbh) %>% filter(dbh > 200) %>% .$sp %>% unique(.)
canopy_species_bci <- as.data.frame(canopy_species_bci)
names(canopy_species_bci) <- "sp"


#assigning species to early vs. late PFTs (determined in other script from wsg following Powell et al., 2018; doi: 10.1111/nph.15271

wsg.ctfs3$Latin <- paste(wsg.ctfs3$genus, wsg.ctfs3$species)
temp <- merge(wsg.ctfs3,bci.spptable, by = "Latin")[,c(1,2,5)]
names(temp)[3] <- "sp"
temp[temp$Latin == "Trema integerrima",]$sp <- "tremin"
canopy_species <- merge(as.data.frame(canopy_species_bci),temp, by = "sp") %>% na.omit(.)
pft <- rep(NA, length(canopy_species$sp))
pft[canopy_species$wsg >= 0.49] <- "late"
pft[canopy_species$wsg < 0.49] <- "early"
canopy_species$pft <- pft


#adding drought indices from Engelbrecht et al. 2005 DOI 10.1007/s00468-004-0393-0
pfts_sept_2018 <- merge(d_indices, canopy_species, all.y = T)

#adding data from Harms 2001 on whats positively or negatively associated with the plateau

#importing the habitat associations from the harms 2001 paper
harms_pft <- read.csv("harms_habitat_associations.csv")
harms_pft$sp <- gsub(pattern = "\n", replacement = " ", x = harms_pft$sp)
harms_pft$stat <- as.character(harms_pft$stat)
names(harms_pft) <- c("Latin", "dpft")

#merging the species list with the Harms data
pfts_sept_2018 <- merge(pfts_sept_2018, harms_pft, by = "Latin", all.x = T)


#adding regression coefficients of soil moisture associations from Condit et al., 2012 https://www.pnas.org/content/110/13/5064

# importing the moisture response data from the PNAS paper
moistr_resp <- read.table("TreeCommunityDrySeasonSpeciesResponse.txt", sep = '\t', header = T)
moistr_resp <- moistr_resp %>% select(Latin, occur, Inter, Moist, Moist.2)
moistr_resp$Latin <- lapply(X = strsplit(x = as.character(moistr_resp$Latin), split = " (", fixed = T), `[[`, 1) %>% unlist(.)


write.csv(moistr_resp[,c(1,3)], "condit_moisture_response.csv")

#merging this new list back with the pft data
pfts_sept_2018 <- merge(pfts_sept_2018, y = moistr_resp[,c("Latin", "Moist")], by = "Latin", all.x = T)
pfts_sept_2018$engel_dpft <- rep(0, length(pfts_sept_2018$Latin))

quantile(d_indices$d_index, probs = c(.33, .66), na.rm = T)

pfts_sept_2018 <- pfts_sept_2018 %>% mutate_at(c(2:4), funs(replace(., is.na(.), 0)))

pfts_sept_2018[pfts_sept_2018$d_index < 14.2 & pfts_sept_2018$d_index > 0,]$engel_dpft <- "dt"
pfts_sept_2018[pfts_sept_2018$d_index > 33.35,]$engel_dpft <- "di"

names(pfts_sept_2018)[c(5,6,8)] <- c("e_vs_l","harms_dt_vs_di", "engel_dt_vs_di")

pfts_sept_2018 <- pfts_sept_2018 %>% select(Latin, sp, wsg, e_vs_l, d_index, engel_dt_vs_di, harms_dt_vs_di, Moist)


#adding the PNAS drought pft designation
quantile(pfts_sept_2018$Moist, na.rm = T)
pfts_sept_2018 <- pfts_sept_2018 %>% mutate_at(c(7:8), funs(replace(., is.na(.), 0)))

PNAS_dt_vs_di <- rep(0, length(pfts_sept_2018$Latin))

PNAS_dt_vs_di[pfts_sept_2018$Moist < 0] <- "dt"
PNAS_dt_vs_di[pfts_sept_2018$Moist > -0.0001] <- "di"


pfts_sept_2018$PNAS_dt_vs_di <- PNAS_dt_vs_di


#creating final dt_vs_di pfts
dpft <- c()
for(i in 1:length(pfts_sept_2018$Latin)){
  
  dpft[i] <- pfts_sept_2018$PNAS_dt_vs_di[i]
  
  if(pfts_sept_2018$harms_dt_vs_di[i] != 0){dpft[i] <- pfts_sept_2018$harms_dt_vs_di[i]}
  
  if(pfts_sept_2018$engel_dt_vs_di[i] != 0){dpft[i] <- pfts_sept_2018$engel_dt_vs_di[i]}
  
}
  
#  if(pfts_sept_2018$engel_dt_vs_di[i] != 0){dpft[i] <- pfts_sept_2018$engel_dt_vs_di[i]}else{
#    if(pfts_sept_2018$harms_dt_vs_di != 0){dpft[i] <- pfts_sept_2018$harms_dt_vs_di[i]}else{
#      dpft[i] <- pfts_sept_2018$PNAS_dt_vs_di[i]}
#  }
#}

pfts_sept_2018$dpft <- dpft


pfts_sept_2018 %>%
  dplyr::select(c(1,2,3,6,7,9,10))


write.csv(pfts_sept_2018, file = paste0("pfts",Sys.Date(),".csv"))

#acknowledging the ones that we don't have drought tolerance data for
no_drought_tolerance_data <- pfts_sept_2018[pfts_sept_2018$dpft == "0",]
no_drought_tolerance_data$dpft <- sample(x = c("dt","di"), size = length(no_drought_tolerance_data$sp), replace = T)  
write.csv(no_drought_tolerance_data[,c(1,2,3,4,10)], file = "random_assignments.csv")


#randomly assigning those few trees to either drought tolerance or intolerant
#pfts_sept_2018[pfts_sept_2018$dpft != "di" | pfts_sept_2018$dpft != "dt" ,]$dpft <- runif(n = 9,min = 0,max = 1)
#change <- as.logical((is.na(as.numeric(pfts_sept_2018$dpft))*-1)+1)
#pfts_sept_2018$dpft[change][1:4] <- "dt"
#pfts_sept_2018$dpft[change][5:9] <- "di" 

#the pft list we use in November 2018
#pfts_nov_2018 <- pfts_sept_2018 %>% mutate(pft = paste0(e_vs_l,dpft)) %>% select(Latin, sp, pft)
#length(pfts_nov_2018$Latin)


#calculating Maximum dbh for each species (mean of three largest individuals) using the mean of the six largest individuals
# max_dbh_vals <- bci.full %>%
#   select(dbh, sp) %>%
#   #filter(sp %in% Late) %>%
#   na.omit(.) %>%
#   group_by(sp) %>%
#   .[order(.$dbh, decreasing = TRUE),] %>%
#   do(head(.,6)) %>% group_by(sp) %>%
#   summarise(dmax = mean(dbh)) %>%
#   arrange(., dmax) #%>%
#   #.$dmax %>% mean(.)


#calculating Maximum dbh for each species (mean of three largest individuals) using the 95th percentile of dbh as the "dmax"

max_dbh_vals <- bci.full %>%
  select(dbh, sp) %>%
  #filter(sp %in% Late) %>%
  na.omit(.) %>%
  group_by(sp) %>%
  summarise(dmax = quantile(dbh, probs = 0.95)) 
  
  
#adding max dbh values to the species list
species_list <- merge(max_dbh_vals, pfts_sept_2018, all.y = T)


#getting a list of basal area from the sp.BAlist data for 2010
ba_2010 <- data.frame(sp = attributes(sp.Glist[[1]]$rate)$dimnames[[1]], ba = sp.BAlist[[7]]$ba$all) 


#write.csv(ba_2010, "C:/Users/ahanb/OneDrive/Documents/rec_submodel/ba_2010.csv")


#function to find basal area from dbh
ba <- function (dbh, dbhunit = "mm") 
{
  if (dbhunit == "mm") 
    return(pi * (dbh/2000)^2)
  if (dbhunit == "cm") 
    return(pi * (dbh/200)^2)
}


#filling in the reproductive basal area for each species

#creating a data frame with all the species in the species list
rba_df <- data.frame(sp = species_list$sp, r_ba = rep(0,length(species_list$sp)), ba = rep(0,length(species_list$sp)), r_abun = rep(0,length(species_list$sp)))


#filling in the reproductive basal area for each species in the 2010 census
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
  
  rba_df[i,3] <- bci.full7 %>%
    select(sp, dbh, status) %>%
    filter(sp == spp,
           status == "A") %>%
    rowwise() %>%
    mutate(ba = ba(dbh = dbh, dbhunit = "mm")) %>%
    .$ba %>% sum(., na.rm = T) 
  
  
  rba_df[i,4] <- bci.full7 %>%
    select(sp, dbh, status) %>%
    filter(sp == spp,
           status == "A",
           dbh > mindbh) %>%
    .$dbh %>% length(.)

  print(paste0("sp",i,"of",length(species_list$sp)))
}



species_list_final <- species_list %>%
  left_join(rba_df) %>%
  mutate(pft = paste0(e_vs_l,dpft)) %>%
  filter(r_abun >= 50)

species_balance <- species_list %>%
  left_join(rba_df) %>%
  mutate(pft = paste0(e_vs_l,dpft)) %>%
  filter(r_abun >= 50) %>%
  select(sp, e_vs_l, pft, dmax, r_abun, ba, r_ba) %>%
  group_by(pft) %>%
  summarise(n = length(pft))


species_list_for_joe <- species_list %>%
  left_join(rba_df) %>%
  mutate(pft = paste0(e_vs_l,dpft)) %>%
  filter(r_abun >= 50) %>%
  select(c(1,3,4,5,6,7,8,9,10,11))

write.csv(species_list_for_joe, file = "dt_vs_di_for_joe.csv")










