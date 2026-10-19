#------------------------------------------------------#
#### Stopover Status ####

load("C:/Users/lwilde2/Desktop/RDH Database/Processed Data/BBMMS/Fall/Stopovers/RDH_FallStopovers_2014to2023.RData")
SO_F_all <- Corr_all
# writes a series of for loops to go through create a blank dataframe and iterate through each id to create a matrix, then can extract what proportion of each of the years points on its own migration stopovers are in the other ones too

#make the point layer and the SO layer within same crs
SO_F_all <- st_transform(SO_F_all, crs = st_crs(mig.FallMigration.sf)$input)

SO_F_all_sep <- SO_F_all %>% mutate(AID = str_sub(id_yr, 0,3)) %>% filter(layer == 1) %>% st_cast(., "POLYGON")

#combine stops that are within 5 km of each other, takes two steps to get rid of overlap and dual counts
# ATTN:: This code combines stops that are within 5000 n if 
SO_F_all_sep_comb <- SO_F_all_sep[0,0]

for(i in unique(SO_F_all_sep$id_yr)){
  #i = unique(SO_all_sep$id_yr)[3]
  x <- SO_F_all_sep %>% filter(id_yr == i) 
  
  for(j in 1:nrow(x)){
    # j = 11
    
    x$grp[j] <- st_is_within_distance(x$geometry[j], x, dist = 5000)
    
    
  }#j
  x$grp.c <- as.numeric(factor(as.character(x$grp)))
  
  xx <- x %>% group_by(grp.c) %>% summarise(geometry = st_union(geometry), layer = 1, id_yr = unique(id_yr), AID = unique(AID)) %>% ungroup()
  xx$grp1 <- NA
  
  rm(x)
  
  for(h in 1:nrow(xx)){
    #h = 1
    
    xx$grp1[h] <- st_is_within_distance(xx$geometry[h], xx, dist = 5000)
    
  }#h
  
  xx$grp1.c <- as.numeric(factor(as.character(xx$grp1)))
  xx <- xx %>% dplyr::select(-c(grp1,grp.c))
  xx <- xx %>% group_by(grp1.c) %>% summarise(geometry = st_union(geometry), layer = 1, id_yr = unique(id_yr), AID = unique(AID)) %>% ungroup()
  xx <- xx %>% dplyr::select(layer, id_yr, AID, grp1.c, geometry)
  SO_F_all_sep_comb <- rbind(SO_F_all_sep_comb, xx)
  rm(xx)
}#i

SO_F_all_sep_comb

mapview(SO_F_all_sep)

save(SO_F_all_sep_comb, file = "C:/Users/lwilde2/Desktop/RDH Database/Processed Data/RDH_FallStopovers_14t23_MergedAt5k_20240826.RData")
