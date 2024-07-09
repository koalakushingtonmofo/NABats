knitr::opts_chunk$set(echo = TRUE)

rm(list=ls())
graphics.off()
setwd("/Users/kushshanker/Documents/Yale/Year 2/2023 Fall/ENV 603/NABats")
library(dplyr); library(ggplot2); library(sf); library(lubridate)

cov = st_read("NABat_grid_covariates.dbf")
epfu = read.csv("EPFU_gridcell_occupancy.csv"); epfu = epfu[,2:4]
laci = read.csv("LACI_gridcell_occupancy.csv"); laci = laci[,2:4]
lano = read.csv("LANO_gridcell_occupancy.csv"); lano = lano[,2:4]
mylu = read.csv("MYLU_gridcell_occupancy.csv"); mylu = mylu[,2:4]
myle = read.csv("MYLE_gridcell_occupancy.csv"); myle = myle[,2:4]
mygr = read.csv("MYGR_gridcell_occupancy.csv"); mygr = mygr[,2:4]
myev = read.csv("MYEV_gridcell_occupancy.csv"); myev = myev[,2:4]
myse = read.csv("MYSE_gridcell_occupancy.csv"); myse = myse[,2:4]
myth = read.csv("MYTH_gridcell_occupancy.csv"); myth = myth[,2:4]
myvo = read.csv("MYVO_gridcell_occupancy.csv"); myvo = myvo[,2:4]
myyu = read.csv("MYYU_gridcell_occupancy.csv"); myyu = myyu[,2:4]
pesu = read.csv("PESU_gridcell_occupancy.csv"); pesu = pesu[,2:4]

ggplot() +
  geom_sf(data = cov, aes(fill = mean_temp), colour = alpha("red",0))

occupancies = list(epfu,laci,lano,mylu,myle,mygr,myev,myse,myth,myvo,myyu,pesu)
bats = Reduce(function(x, y) merge(x, y, by=c("grts","year"), all=TRUE), occupancies)
colnames(bats) = c("grts","year",paste("occ_",c("EPFU","LACI","LANO","MYLU","MYLE","MYFR","MYEV","MYSE","MYTH","MYVO","MYYU","PESU"), sep=""))
bats$year = ymd(bats$year, truncated = 2L)

write.csv(bats, "all_gridcell_occupancy.csv")

lm_data = bats %>% select(-grts) %>% 
  group_by(year) %>% summarise(across(where(is.numeric), mean, na.rm=T))

lm_results = data.frame("Species" = colnames(lm_data)[2:13], 
                        b = NA, p = NA, CI_lower = NA, CI_upper = NA)

j = 1
for (i in lm_data[2:13]) {
   model = lm(i ~ lm_data$year)
   lm_results$b[j] = summary(model)$coefficients[2]
   lm_results$p[j] = summary(model)$coefficients[8]
   lm_results[j, c("CI_lower", "CI_upper")] = confint(model, 'lm_data$year', level=0.95)
   j = j + 1
}
