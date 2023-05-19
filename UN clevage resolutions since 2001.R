rawdataexcel <- read.csv(choose.files())

ga_ind_pak <- rawdataexcel %>%
  select(ccode, unres, vote, year, session, me, nu, di, hr, co, ec, descr, importantvote) %>% 
  filter(ccode %in% c(750, 770) & year > 2016) %>% 
  mutate(ccode = recode(ccode, "750" = "IND", "770" = "PAK"))

ga_sco5_west <- rawdataexcel %>%
  select(ccode, unres, vote, year, session, me, nu, di, hr, co, ec, descr, importantvote) %>% 
  filter(ccode %in% c(710, 365, 2, 220, 200, 705, 703, 702, 704) & year > 2000) %>% 
  mutate(ccode = recode(ccode, "710" = "CHN", "365" = "RUS", "2" = "USA", "220" = "FRA", "200" = "GBR",
                        "703" = "KGZ", "705" = "KAZ", "702" = "TJK", "704" = "UZB"))

not_2 <- vector(length = length(ga_ind_pak[,2]), mode = "character")
not_9 <- vector(length = length(ga_sco5_west[,2]), mode = "character")
unique_gaunres <- unique(ga_sco5_west$unres)
unique_gaunres_ip <- unique(ga_ind_pak$unres)

for (i in 1:(length(unique_gaunres_ip))) {
  {log_vec_ga <- ga_ind_pak[2] == unique_gaunres_ip[i]
  } 
  if (2 == sum(log_vec_ga == TRUE)){
    not_2[i] <- NA
  } else{
    not_2[i] <- ga_ind_pak[(which.max(log_vec_ga %in% TRUE)),2]
  }
}

for (i in 1:(length(unique_gaunres))) {
  {log_vec_ga <- ga_sco5_west[2] == unique_gaunres[i]
  } 
  if (9 == sum(log_vec_ga == TRUE)){
    not_9[i] <- NA
  } else{
    not_9[i] <- ga_sco5_west[(which.max(log_vec_ga %in% TRUE)),2]
  }
}

not_2 <- not_2[!is.na(not_2)]
not_2 <- not_2[not_2 != ""]

not_9 <- not_9[!is.na(not_9)]
not_9 <- not_9[not_9 != ""]


indexs <- c("","2nd","3rd","4th", "5th", "6th", "7th", "8th", "9th", "10th") 

for(i in 1:length(not_9)){
  pos_fault <- which(ga_sco5_west[,2] %in% not_9[i])
  uni.desc <- unique(ga_sco5_west[pos_fault,12])
  for(j in 1:length(uni.desc)){
    pos.uni.desc <- which(ga_sco5_west[,12] %in% uni.desc[j])
    pos.uni.desc <- pos.uni.desc[pos.uni.desc %in% pos_fault]
    for(k in 1:length(pos.uni.desc)){
      ga_sco5_west[pos.uni.desc[k],2] <- paste(ga_sco5_west[pos.uni.desc[k],2], indexs[j])
    }
  }
}

for(i in 1:length(not_2)){
  pos_fault <- which(ga_ind_pak[,2] %in% not_2[i])
  uni.desc <- unique(ga_ind_pak[pos_fault,12])
  for(j in 1:length(uni.desc)){
    pos.uni.desc <- which(ga_ind_pak[,12] %in% uni.desc[j])
    pos.uni.desc <- pos.uni.desc[pos.uni.desc %in% pos_fault]
    for(k in 1:length(pos.uni.desc)){
      ga_ind_pak[pos.uni.desc[k],2] <- paste(ga_ind_pak[pos.uni.desc[k],2], indexs[j])
    }
  }
}

unique_gaunres_ind <- unique(ga_sco5_west$unres)
west_set <- vector(length = (length(unique_gaunres)), mode = "character")
sco_set <- vector(length = (length(unique_gaunres)), mode = "character")

for (i in 1:(length(unique_gaunres_ind))) {
  log_vec_ga <- ga_sco5_west[,2] == unique_gaunres_ind[i]
  pos_gaunres <- which(log_vec_ga %in% TRUE)
  for (j in 1:length(pos_gaunres)){
    if(ga_sco5_west[(pos_gaunres[j]),1] == "CHN") {
      chn_vote <- ga_sco5_west[(pos_gaunres[j]), 3]
    } else { if (ga_sco5_west[(pos_gaunres[j]), 1] == "RUS") {
      rus_vote <- ga_sco5_west[(pos_gaunres[j]),3]
    } else { if (ga_sco5_west[(pos_gaunres[j]), 1] == "KAZ") {
      kaz_vote <- ga_sco5_west[(pos_gaunres[j]), 3]
    } else { if (ga_sco5_west[(pos_gaunres[j]), 1] == "KGZ") {
      kgz_vote <- ga_sco5_west[(pos_gaunres[j]), 3]
    } else { if (ga_sco5_west[(pos_gaunres[j]), 1] == "TJK") {
      tjk_vote <- ga_sco5_west[(pos_gaunres[j]), 3]
    } else { if (ga_sco5_west[(pos_gaunres[j]), 1] == "UZB") {
      uzb_vote <- ga_sco5_west[(pos_gaunres[j]), 3]
    } else { if (ga_sco5_west[(pos_gaunres[j]), 1] == "FRA") {
      fra_vote <- ga_sco5_west[(pos_gaunres[j]), 3]
    }  else { if (ga_sco5_west[(pos_gaunres[j]), 1] == "GBR") {
      gbr_vote <- ga_sco5_west[(pos_gaunres[j]), 3]
    } else {usa_vote <- ga_sco5_west[(pos_gaunres[j]),3]
    }}}}}}}}
  }
  
  if((chn_vote != 1) &
     (rus_vote != 1) & 
     (kaz_vote != 1) &
     (tjk_vote != 1) &
     (kgz_vote != 1) &
     (uzb_vote != 1) &
     (gbr_vote == 1) &
     (fra_vote == 1) &
     (usa_vote == 1)){
    west_set[i] <- unique_gaunres_ind[i]
  } else { if((chn_vote == 1) &
              (rus_vote == 1) & 
              (kaz_vote == 1) &
              (tjk_vote == 1) &
              (kgz_vote == 1) &
              (uzb_vote == 1) &
              (gbr_vote != 1) &
              (fra_vote != 1) &
              (usa_vote != 1)){
    sco_set[i] <- unique_gaunres_ind[i]
  } else {sco_set[i] <- west_set[i] <- NA}
  }
}

west_set <- west_set[!is.na(west_set)]
west_set <- west_set[west_set != ""]

sco_set <- sco_set[!is.na(sco_set)]
sco_set <- sco_set[sco_set != ""]


for(i in 1:length(west_set)){
  log_vec_ga <- west_set[i] == ga_ind_pak[,2]
  pos_vote <- which(log_vec_ga %in% TRUE)
  if(length(pos_vote) != 0){
    for(j in 1:length(pos_vote)){
      if(ga_ind_pak[(pos_vote[j]),1] == "IND"){
        ind_vote <- ga_ind_pak[pos_vote[j],3]
      } else {pak_vote <- ga_ind_pak[pos_vote[j],3]}
    } 
    if(ind_vote != 1 & pak_vote != 1){
      west_set[i] <- west_set[i]
    } else {west_set[i] <- NA}
  } else {west_set[i] <- west_set[i]}
}


for(i in 1:length(sco_set)){
  log_vec_ga <- ga_ind_pak[,2] == sco_set[i]
  pos_vote <- which(log_vec_ga %in% TRUE)
  if(length(pos_vote) != 0){
    for(j in 1:length(pos_vote)){
      if(ga_ind_pak[(pos_vote[j]),1] == "IND"){
        ind_vote <- ga_ind_pak[pos_vote[j],3]
      } else {pak_vote <- ga_ind_pak[pos_vote[j],3]}
    } 
    if(ind_vote == 1 & pak_vote == 1){
      sco_set[i] <- sco_set[i]
    } else {sco_set[i] <- NA}
  } else {sco_set[i] <- sco_set[i]} 
}

west_set <- west_set[!is.na(west_set)]
west_set <- west_set[west_set != ""]

sco_set <- sco_set[!is.na(sco_set)]
sco_set <- sco_set[sco_set != ""]

length(west_set)
length(sco_set)

ga_clev_westsup <- ga_sco5_west[0,]

for(i in 1:(length(west_set))){
  ga_clev_westsup[i,] <- ga_sco5_west[which.max(ga_sco5_west[,2] %in% west_set[i]),]
}


ga_clev_scosup <- ga_sco5_west[0,]

for(i in 1:(length(sco_set))){
  ga_clev_scosup[i,] <- ga_sco5_west[which.max(ga_sco5_west[,2] %in% sco_set[i]),]
}

sampsco <- sample(x = c(1:length(sco_set)), size = 20, replace = FALSE)
sampwest <- sample(x = c(1:length(west_set)), size = 20, replace = FALSE)

west_sample <- vector(length = 20, mode = "character")
sco_sample <- vector(length = 20, mode = "character")

for(i in 1:length(sampsco)){
  sco_sample[i] <- sco_set[sampsco[i]]
}

for(i in 1:length(sampwest)){
  west_sample[i] <- west_set[sampwest[i]]
}

print(sco_sample)
print(west_sample)