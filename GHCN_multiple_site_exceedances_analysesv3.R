options(noaakey = "nbMVIUDxTfHgVpquPsTfJfYhKGNMxQBS")
library(rgdal)
library(rnoaa)
library(dplyr)
library(ggplot2)
library(ggmap)
library(maps)
library(maptools)
data(wrld_simpl)
source('C:/Users/luc/Desktop/basic_R_functions/geographic_functions.R')
setwd('C:/Users/luc/Desktop/NYT/GHCN/')

############################################################################################################
# Set study parameters and create directories
rl=20
prob=1/rl
N_years=rl*3
windows=seq(1, 7, by=2)
first_year=1890
last_year=2017
nyears=last_year-first_year+1
study_name='V2NY'
region='NY'
region_name="New York"
directory=paste0(study_name, '/rl_', rl, '_yrs/')
station_data_directory=paste0(directory, 'station_data/')
hit_TS_directory=paste0(directory, 'hit_TS/')
plot_directory=paste0(directory, 'plots/')
dir.create(station_data_directory, showWarnings = T, recursive = T)
dir.create(hit_TS_directory, showWarnings = F, recursive = F)
dir.create(plot_directory, showWarnings = F, recursive = F)

############################################################################################################
# Get site info
df_sites0=read.csv('../TRI_2016_NY.csv', stringsAsFactors = F)
#colnames(df_sites)[42:53]

# Only keep unique sites with solid or water releases
df_sites0=df_sites0[(apply(df_sites0[,42:53], MARGIN = 1, FUN = sum)!=0), ]

df_sites1=data.frame(TRI_id=df_sites0$TRI_FACILITY_ID, name=df_sites0$FACILITY_NAME,
                    county=df_sites0$COUNTY, state=df_sites0$ST, 
                    latitude=df_sites0$LATITUDE, longitude=df_sites0$LONGITUDE, 
                    releases=(df_sites0$ON.SITE_RELEASE_TOTAL-df_sites0$X5.1_FUGITIVE_AIR-df_sites0$X5.2_STACK_AIR))


df_sites <- df_sites1 %>% group_by(TRI_id, name, county, state, latitude, longitude) %>%
  summarize(Sum_releases = sum(releases))

############################################################################################################
# Get GHCN info
# station_data <- ghcnd_stations()
# save(station_data, file='GHCN/ghcn_station_data')
load('ghcn_station_data')

# Select stations abiding to the minimum number of years requirement and plot them
station_data2<-station_data %>% filter(state == region, element == "PRCP", (last_year-first_year)>N_years)

png(paste0(plot_directory, 'station_map','_', N_years, '_yrs_of_data','.png'))
map <- map_data("state", region = region_name)
df=data.frame(lat=station_data2$latitude, lon=station_data2$longitude, Facility="Gauge")
df=rbind(df, data.frame(lat=df_sites$latitude, lon=df_sites$longitude, Facility="TRI site"))
p <- ggplot()+
  geom_polygon(data=map, aes(x=long, y=lat, group = group),colour="black", fill="white")+
  geom_point(data=df, aes(x=lon, y=lat, colour = Facility))+
  ggtitle(paste0('Locations of GHCN gauges with ', N_years, '_yrs_of_data', ' and 2016 TRI sites'))+
  theme_bw()
print(p)
dev.off()

# Select stations with less than 10% missing data and load and save the precipitation and snow GHCN data
save_stat_data1<-function(stat_data=station_data2, output_path=station_data_directory){
  #record=data.frame(id=NA, latitude=NA, logitude=NA, elevation=NA, first_year=NA, last_year=NA)
  for(id in stat_data$id){
    prcp_df=meteo_tidy_ghcnd(id, var=c('PRCP', 'SNOW'))
    ndays=as.numeric(prcp_df$date[nrow(prcp_df)]-prcp_df$date[1])
    # keep only data with less than 10% missing days over recod period
    if((ndays-nrow(prcp_df)+sum(is.na(prcp_df$prcp)))/ndays<0.1){
      save(prcp_df, file=paste0(output_path, id))
    }
  }
}


if(length(list.files(station_data_directory)) ==0){
  save_stat_data1(stat_data=station_data2, output_path=station_data_directory)
}


############################################################################################################
# Compute and save hit time series for each sation on record for given a window w and a return level rl

# Get info for the stations previously selected
my_stations=data.frame(stations_ids=list.files(station_data_directory), stringsAsFactors = F)
for(i in 1:nrow(my_stations)){
  a=station_data2 %>% filter(id == my_stations$stations_ids[i])
  my_stations$longitude[i] = a$longitude
  my_stations$latitude[i] = a$latitude
  
}
df_sites$station_id=NA
my_stations$nsites=NA
for(i in 1:nrow(df_sites)){
  Idx=findIdxLonLat(valueLon = df_sites$longitude[i], valueLat = df_sites$latitude[i], 
                    matrix.LongLat = my_stations[, 2:3])
  df_sites$station_id[i] = my_stations$stations_ids[Idx]
}
for (i in 1:nrow(my_stations)){
  my_stations$nsites[i]=sum(df_sites$station_id==my_stations$stations_ids[i])
}

for (i in 1:nrow(my_stations)){
  data=station_data2 %>% filter(id==my_stations$stations_ids[i])
  my_stations$first_year[i]=data$first_year
  my_stations$last_year[i]=data$last_year
}
save_hit_TS<-function(w, rl, my_stations, station_data_directory, output_path){
  prob=1/rl
  my_stations$inact_years=NA
  for (i in 1:nrow(my_stations)){
    station_id = my_stations$stations_ids[i]
    load(paste0(station_data_directory, station_id))
    all_days=data.frame(date=as.Date(c(as.numeric(prcp_df$date[1]-as.Date('1800-01-01')):
                                         as.numeric(prcp_df$date[nrow(prcp_df)]-as.Date('1800-01-01'))), 
                                     origin='1800-01-01'))
    prcp_df=merge(prcp_df, all_days, by='date', all.y=T)
    prcp_df$year=format(prcp_df$date, '%Y')
    prcp_df$accumulated <- stats::filter(prcp_df$prcp, rep(1,w), sides=1)
    prcp_df %>% group_by(year) %>%
      summarise(prcp_df_yearly=max(accumulated, na.rm=T)) -> prcp_df_yearly_max
    
    prcp_df_yearly_max$prcp_df_yearly2=prcp_df_yearly_max$prcp_df_yearly
    prcp_df_yearly_max$prcp_df_yearly2[prcp_df_yearly_max$prcp_df_yearly2==-Inf]=NA
    
    threshold=quantile(prcp_df_yearly_max$prcp_df_yearly, (1-prob), na.rm=T)
    threshold2=quantile(prcp_df_yearly_max$prcp_df_yearly2, (1-prob), na.rm=T)
    
    prcp_df$extreme <- ifelse(prcp_df$accumulated >= threshold2,1,0)
    
    prcp_df$extreme_uniq = 0
    for(dd in c(which(prcp_df$extreme==1))){
      if(sum(prcp_df$extreme_uniq[(dd-w+1):(dd-1)], na.rm=T) < 1){
        prcp_df[dd, c("extreme_uniq")] = 1
      }
    }
    prcp_df %>% group_by(year) %>% 
      summarise(extremes=sum(extreme_uniq, na.rm=T)) -> extreme_counts
    my_stations$inact_years[i]=list(prcp_df_yearly_max$year[is.na(prcp_df_yearly_max$prcp_df_yearly2)])
    extreme_counts$extremes[is.na(prcp_df_yearly_max$prcp_df_yearly2)]=NA
    save(extreme_counts, file=paste0(output_path, station_id, '_w_', w, '_rl_', rl, '_yrs'))
  }
  
  colnames(my_stations)[ncol(my_stations)]=paste0('inact_years', w)
  
  
  
  
  return(my_stations)
}

for(w in windows){
  output_path=paste0(hit_TS_directory, 'w', w, 'rl', rl, '/stations/')
  dir.create(output_path, showWarnings = T, recursive = T)
  #if(length(list.files(output_path)) ==0){
    my_stations=save_hit_TS(w, rl, my_stations, station_data_directory, output_path)
  #}
}

load(paste0(station_data_directory,'USC00300343'))
load(paste0(output_path,'USC00300343_w_3_rl_20_yrs'))
# load(paste0(output_path,'USC00300254_w_7_rl_20_yrs'))
# load(paste0(station_data_directory, 'USC00300254'))
############################################################################################################
# Analysis - constant number of active sites
matrix.LongLat = cbind(my_stations$longitude, my_stations$latitude)
# Info for reanalysis data to use to fill the gaps
file_path='C:/Users/luc/Desktop/climate_data/20CR_land/'
file_list='20CR_land.config.files.txt'
last_rean_year=2014
source('../m.R')


for(w in windows){
dir.create(paste0(hit_TS_directory, 'w', w, 'rl', rl, '/sites/'), recursive=T)
i=1
  # Add list of stations within a range of 100 km for each site, ordered from smallest to largest distance
  for (i in 1:nrow(df_sites)) {
    df=data.frame(stations=my_stations$stations_ids, distance=distKmLonLat(df_sites$longitude[i], df_sites$latitude[i], 
                                                                           matrix.LongLat))
    df$keep=(df$distance<=100)
    df=df[df$keep==1,]
    df=df[order(df$distance),]
    df_sites$stations[i]=list(as.character(df$stations))
  }
  
  # Compute and save hit time series from station hits time series, using stations within 100 km and, 
  # starting with closest stations and then filling gaps with stations further away and ultimately reanalysis.
  i=1
  if(length(list.files(paste0(hit_TS_directory, 'w', w, 'rl', rl, '/sites/')))!=nrow(df_sites)){
    df_hits0=data.frame(year=(first_year:last_year))
    for (i in 1:nrow(df_sites)) {
      site_stations=unlist(df_sites$stations[i])
      df_hits=get(load(paste0(hit_TS_directory, 'w', w, 'rl', rl, '/stations/', site_stations[1],'_w_', w, '_rl_', rl, '_yrs')))
      df_hits=merge(df_hits, df_hits0, by='year', all.x=T, all.y=T)
      counter=1
      while((sum(is.na(df_hits$extremes))>0) & (counter<=(length(site_stations)))){
        for(k in 2:length(site_stations)){
          df_hits3=get(load(paste0(hit_TS_directory, 'w', w, 'rl', rl, '/stations/', site_stations[k],'_w_', w, '_rl_', rl, '_yrs')))
          df=merge(df_hits, df_hits3, by='year', all.x=T, all.y=T)
          df$extremes.x[is.na(df$extremes.x)]=df$extremes.y[is.na(df$extremes.x)]
          df_hits=data.frame(year=df$year, extremes=df$extremes.x)
          counter=counter+1
        }
      }
      df_hits$year=as.numeric(as.character(df_hits$year))
      df_hits=df_hits[df_hits$year>=first_year & df_hits$year<=last_year,]
      if(sum(is.na(df_hits$extremes))>0){
        info=data.frame(gridblock_lon=df_sites$longitude[i],
                        gridblock_lat=df_sites$latitude[i],
                        prob=prob, 
                        w=w,
                        start_year=first_year,
                        end_year=last_year,
                        nyears=nyears,
                        file_path=file_path,
                        file_list=file_list,
                        side='right',
                        long.sys=1)
        extremes_rean=m1(info)
        extremes_rean=data.frame(year=first_year:last_rean_year, extremes=extremes_rean)
        df_hits$extremes[is.na(df_hits$extremes)]=extremes_rean$extremes[is.na(df_hits$extremes)]
        
      }
      
      save(df_hits, file=paste0(hit_TS_directory, 'w', w, 'rl', rl, '/sites/', df_sites$TRI_id[i]))  
      
    }
  }
  
}


ratios=c()
nsim=1000
# Compute total hit time series and compare to simulation
for(w in windows){
  site_hit_files=list.files(paste0(hit_TS_directory, 'w', w, 'rl', rl, '/sites/'))
  file=site_hit_files[1]
  df_hits_tot=data.frame(year=c(first_year:last_year), hits=0)
  for (file in site_hit_files){
    load(paste0(hit_TS_directory, 'w', w, 'rl', rl, '/sites/', file))
    df_hits_tot$hits=df_hits_tot$hits+df_hits$extremes
  }
  
  # independent sites synthetic case
  sites_synth_results=matrix(0, nyears, nsim)
  for (k in 1:nsim){
    for (i in 1:nyears){
      sites_synth_results[i,k]=sum(rbinom(nrow(df_sites),1, prob))
    }
  }
  M_sites=apply(sites_synth_results, MARGIN=2, FUN=max)
  ratios=c(ratios, max(df_hits_tot$hits)/quantile(M_sites,0.99))
  png(paste0(plot_directory, 'Independent sites vs reality ','_w_', w, '_rl_', rl, '_yrs','.png'))
  plot(df_hits_tot$year, df_hits_tot$hits, col='red', type='l', 
       main = paste0(' Site hits vs chance: ', nrow(df_sites), ' sites', w, '-day events, ', rl, ' yrs rl'), 
       xlab='Year', ylab='# hits')
  for(k in 1:ncol(sites_synth_results)){
    lines(df_hits_tot$year, sites_synth_results[,k], col="grey")
  }
  lines(df_hits_tot$year, df_hits_tot$hits, col='red')
  dev.off()
  
  
}


############################################################################################################
# Analysis - active sites = active stations
df_sites$station_id=NA
for(i in 1:nrow(df_sites)){
  Idx=findIdxLonLat(valueLon = df_sites$longitude[i], valueLat = df_sites$latitude[i], 
                    matrix.LongLat = my_stations[, 2:3])
  df_sites$station_id[i] = my_stations$stations_ids[Idx]
}

ratios2=c()
ratios3=c()
for(w in windows){
  df_sites2=get(load(paste0(hit_TS_directory, 'w', w, 'rl', rl, '/stations/', df_sites$station_id[1], '_w_', w, '_rl_', rl, '_yrs')))
  
  df_sites2$nb_sites=1
  df_sites2$nb_sites[is.na(df_sites2$extremes)]=0
  df_sites2$extremes[is.na(df_sites2$extremes)]=0
  for(i in 2:nrow(df_sites)){
    df1=df_sites2
    df2=get(load(paste0(hit_TS_directory, 'w', w, 'rl', rl, '/stations/', df_sites$station_id[i], '_w_', w, '_rl_', rl, '_yrs')))
    df2$nb_sites=1
    df2$nb_sites[is.na(df2$extremes)]<-0
    df_sites2=merge(df1, df2, by='year', all.x=T, all.y=T)
    if(sum(is.na(df_sites2))>0){
      df_sites2[is.na(df_sites2)]=0
    }
    
    df_sites2=data.frame(year=df_sites2$year, extremes=df_sites2$extremes.x+df_sites2$extremes.y, 
                         nb_sites=df_sites2$nb_sites.x+df_sites2$nb_sites.y)
  }
  df_sites2$year=as.numeric(as.character(df_sites2$year))
  df_sites2=df_sites2[order(df_sites2$year),]
  
  # independent sites synthetic case
  nyears2=max(df_sites2$year)-min(df_sites2$year)+1
  sites_synth_results=matrix(0, nyears2, nsim)
  for (k in 1:nsim){
    for (i in 1:nyears2){
      sites_synth_results[i,k]=sum(rbinom(df_sites2$nb_sites[i],1, prob))
    }
  }
  
  df_sites3=cbind(df_sites2, sites_synth_results)
  M_sites=apply(sites_synth_results, MARGIN=2, FUN=max)
  ratios2=c(ratios2, max(df_hits_tot$hits)/quantile(M_sites,0.99))
  
  png(paste0(plot_directory, 'Number of active sites over time: ', w, '-day events, ', rl, ' yrs rl','.png'))
  plot(df_sites3$year, df_sites3$nb_sites, col='red', type='l', 
       main = paste0('Number of active sites over time: ', w, '-day events, ', rl, ' yrs rl'), 
       xlab='Year', ylab='# active sites')
  dev.off()
  
  
  png(paste0(plot_directory, 'Active sites = Active stations - Independent sites vs reality ','_w_', w, '_rl_', rl, '_yrs','.png'))
  plot(df_sites3$year, df_sites3$extremes, col='red', type='l', 
       main = paste0(' Site hits vs chance: ', nrow(df_sites), ' TRI sites ', w, '-day events, ', rl, ' yrs rl'), 
       xlab='Year', ylab='# hits')
  for(k in 1:ncol(sites_synth_results)){
    lines(df_sites3$year, df_sites3[,(3+k)], col='grey')
  }
  lines(df_sites3$year, df_sites3$extremes, col='red')
  dev.off()
  
  
  
  intermediary_synth_results=matrix(0, nyears2, nsim)
  for (k in 1:nsim){
    for (i in 1:nyears2){
      hits=0
      year=(min(df_sites2$year)-1)+i
      index=which(colnames(my_stations)==paste0('inact_years', w))
      stations<-my_stations[which(my_stations$last_year>=year & my_stations$first_year<=year) & 
                              !grepl(year, my_stations[, index]),1]
      for(stat in stations){
        nsites=my_stations$nsites[which(my_stations$stations_ids==stat)]
        hits=hits+rbinom(1,1, prob)*nsites
      }
      intermediary_synth_results[i,k]=hits
    }
  }
  df_inter=cbind(df_sites2, intermediary_synth_results)
  M_inter=apply(intermediary_synth_results, MARGIN=2, FUN=max)
  ratios3=c(ratios3, max(df_hits_tot$hits)/quantile(M_inter,0.99))
  png(paste0(plot_directory, 'Active sites = Active stations - Independent stations vs reality ','_w_', w, '_rl_', rl, '_yrs','.png'))
  plot(df_sites3$year, df_sites3$extremes, col='red', type='l', 
       main = paste0(' Site hits vs chance: ', nrow(df_sites), ' TRI sites ', w, '-day events, ', rl, ' yrs rl'), 
       xlab='Year', ylab='# hits')
  for(k in 1:ncol(sites_synth_results)){
    lines(df_inter$year, df_inter[,(3+k)], col='grey')
  }
  lines(df_inter$year, df_inter$extremes, col='red')
  dev.off()
  
}
ratios=rbind(w, ratios)
ratios2=rbind(w, ratios2)
ratios3=rbind(w, ratios3)
write.csv(ratios, file=paste0(plot_directory, 'ratios','_w_', w, '_rl_', rl, '_yrs'))
write.csv(ratios2, file=paste0(plot_directory, 'ratios2','_w_', w, '_rl_', rl, '_yrs'))
write.csv(ratios3, file=paste0(plot_directory, 'ratios3','_w_', w, '_rl_', rl, '_yrs'))

############################################################################################################
# Analysis - stations
station_ratio=data.frame(windows=windows, ratio99=rep(NA, length(windows)))
for(w in windows){
  df_stations=get(load(paste0(hit_TS_directory, 'w', w, 'rl', rl, '/stations/', my_stations$stations_ids[1],'_w_', w, '_rl_', rl, '_yrs')))
  df_stations$nb_stat=1
  df_stations$nb_sites[is.na(df_stations$extremes)]=0
  for(i in 2:nrow(my_stations)){
    df1=df_stations
    df2=get(load(paste0(hit_TS_directory, 'w', w, 'rl', rl, '/stations/', my_stations$stations_ids[i],'_w_', w, '_rl_', rl, '_yrs')))
    df2$nb_stat=1
    df2$nb_stat[is.na(df2$extremes)]=0
    df_stations=merge(df1, df2, by='year', all.x=T, all.y=T)
    df_stations[is.na(df_stations)]<-0
    df_stations=data.frame(year=df_stations$year, extremes=df_stations$extremes.x+df_stations$extremes.y, 
                           nb_stat=df_stations$nb_stat.x+df_stations$nb_stat.y)
  }
  df_stations$year=as.numeric(as.character(df_stations$year))
  df_stations=df_stations[order(df_stations$year),]
  
  # independent stations synthetic case
  nyears=nrow(df_stations)
  stations_synth_results=matrix(0, nyears, nsim)
  for (k in 1:nsim){
    for (i in 1:nyears){
      stations_synth_results[i,k]=sum(rbinom(df_stations$nb_stat[i],1, prob))
    }
  }
  
  png(paste0(plot_directory, 'Number of active stations over time_', w, '_day events_', rl,'_yrs rl','.png'))
  plot(df_stations$year, df_stations$nb_stat, col='red', type='l', 
       main = paste0('Number of active sites over time: ', w, '-day events, ', rl, ' yrs rl'), 
       xlab='Year', ylab='# active sites')
  dev.off()
  
  
  df_stations2=cbind(df_stations, stations_synth_results)
  M_stations=apply(stations_synth_results, MARGIN=2, FUN=max)
  station_ratio$ratio99[which(station_ratio$windows==w)]=max(df_stations$extremes)/quantile(M_stations,0.99)
  
  png(paste0(plot_directory, 'Active stations - Independent stations','_w_', w, '_rl_', rl, '_yrs','.png'))
  plot(df_stations2$year, df_stations2$extremes, col='red', type='l', 
       main = paste0(' Stations hits vs chance: ', nrow(my_stations), ' stations'), 
       xlab='Year', ylab='# hits')
  for(k in 1:ncol(stations_synth_results)){
    lines(df_stations2$year, df_stations2[,(3+k)], col="grey")
  }
  lines(df_stations2$year, df_stations2$extremes, col='red')
  dev.off()
  
}

write.csv(station_ratio, file=paste0(plot_directory, 'station_ratio','_rl_', rl, '_yrs.csv'))




