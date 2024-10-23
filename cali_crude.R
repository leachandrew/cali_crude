library(tabulizer)
library(tidyverse)
library(janitor)
library(zoo)
library(ggrepel)
library(viridis)
library(ggthemes)
library(scales)

# CARB data -----------------------------------------------------------

# set_png("masnadi_fig_1.png",width=1500)
# ggplot(data=masnadi_fig_1,) +
#   geom_point(aes(country_factor,vol_w_ci))+
#   geom_errorbar(width=.5, aes(x=country_factor,ymin=ci_5, ymax=pmin(ci_95,40)), colour="darkred") +
#   scale_y_continuous(expand=c(0,0),limits=c(0,40.1))+
#   geom_hline(aes(yintercept = 10.3,colour="Global Average"),linetype="dashed")+
#   scale_colour_manual("",values=colors_tableau10())+  
#   ajl_line()+
#   guides(fill=guide_legend(nrow=1,byrow=TRUE))+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.25,size=7,face="bold"),
#         axis.text.y = element_text(size=7,face="bold"),
#         plot.subtitle = element_text(size = 12, face = "italic"),
#         legend.text = element_text(colour="black", size = 10, face = "bold"),
#   plot.caption = element_text(size = 11, face = "italic"))+
#   labs(x=NULL,y=expression('Upstream carbon intensity '*'(gCO'[2]*'e/MJ crude oil)'),
#        title="Country-level upstream carbon intensity (2015)",
#        subtitle="Volume-weighted mean and 5 and 95% confidence intervals (upper confidence intervals truncated at 40g/MJ)",
#        caption="Source: Masnadi et al. 2018. Global carbon intensity of crude oil production. Science. 31 Aug 2018: Vol. 361, Issue 6405, pp. 851-853.")
# dev.off()


# Location of CARB carbon intensity pdf file
#2017 file has 2015, 2016, and 2017 data
location <- 'https://www.arb.ca.gov/fuels/lcfs/crude-oil/2017_crude_average_ci_value_final.pdf'
location<- "2017_crude_average_ci_value_final.pdf"
# Extract the 2017 data
out <- extract_tables(location)
#need to fix the last table in the data for 2017
adds<-out[[8]]
out[[8]]<-NULL
cali_data<-data.frame(do.call("rbind", out),stringsAsFactors = F)
#we'll have to paste this back into the bottom of the 2015 data where the error was (see  PDF p 7)
adds<-as_tibble(adds)%>%mutate(X0="")%>% select(X0,everything())

#2015 data
cali_2015<-cali_data %>% slice(4:228)
total_vol=cali_2015$X4[2]
avg_ci=cali_2015$X3[2]
#cali_data <-cali_data %>% slice(-c(1:3))
names(cali_2015)<-cali_2015[1,]
cali_2015<-cali_2015[-1,]
cali_2015[1,1]<-"All"
cali_2015<-clean_names(cali_2015)
cali_2015<-cali_2015%>% 
  rename(volume = 4,ci_g_mj=3)%>%mutate(volume=as.numeric(gsub(",","",volume)))%>%
  filter(!is.na(volume))%>%
  mutate(country_state=ifelse(country_state=="",NA,country_state),#set blanks to NA
         country_state=na.locf(country_state), #use zoo to fill forward
         ci_g_mj=as.numeric(ci_g_mj),
         year=2015,
         total_volume=as.numeric(gsub(",","",total_vol)),
         avg_ci=as.numeric(avg_ci))

cali_2016<-cali_data %>% slice(229:453)
total_vol=cali_2016$X4[2]
avg_ci=cali_2016$X3[2]
#cali_data <-cali_data %>% slice(-c(1:3))
names(cali_2016)<-cali_2016[1,]
cali_2016<-cali_2016[-1,]
cali_2016[1,1]<-"All"
cali_2016<-clean_names(cali_2016)
cali_2016<-cali_2016%>% 
  rename(volume = 4,ci_g_mj=3)%>%mutate(volume=as.numeric(gsub(",","",volume)))%>%
  filter(!is.na(volume))%>%
  mutate(country_state=ifelse(country_state=="",NA,country_state),#set blanks to NA
         country_state=na.locf(country_state), #use zoo to fill forward
         ci_g_mj=as.numeric(ci_g_mj),
         year=2016,
         total_volume=as.numeric(gsub(",","",total_vol)),
         avg_ci=as.numeric(avg_ci))


cali_2017<-cali_data %>% slice(454:n())
total_vol=cali_2017$X4[2]
avg_ci=cali_2017$X3[2]
#cali_data <-cali_data %>% slice(-c(1:3))
names(cali_2017)<-cali_2017[1,]
cali_2017<-cali_2017[-1,]
cali_2017[1,1]<-"All"
cali_2017<-clean_names(cali_2017)
cali_2017<-cali_2017%>% 
  rename(volume = 4,ci_g_mj=3)%>%mutate(volume=as.numeric(gsub(",","",volume)))%>%
  filter(!is.na(volume))%>%
  mutate(country_state=ifelse(country_state=="",NA,country_state),#set blanks to NA
         country_state=na.locf(country_state), #use zoo to fill forward
         ci_g_mj=as.numeric(ci_g_mj),
         year=2017,
         total_volume=as.numeric(gsub(",","",total_vol)),
         avg_ci=as.numeric(avg_ci))


#2018 data


location<- "2018_crude_average_ci_value_final.pdf"
# Extract the 2018 data
out <- extract_tables(location)
cali_data<-data.frame(do.call("rbind", out),stringsAsFactors = F)
total_vol=cali_data$X4[3]
avg_ci=cali_data$X4[2]
cali_data <-cali_data %>% slice(-c(1:3))
names(cali_data)<-cali_data[1,]
cali_data<-cali_data[-1,]
cali_data[1,1]<-"All"
cali_data<-clean_names(cali_data)

cali_data<-cali_data%>% 
  rename(volume = 4)%>%mutate(volume=as.numeric(gsub(",","",volume)))%>%
  filter(!is.na(volume))%>%
  mutate(country_state=ifelse(country_state=="",NA,country_state),#set blanks to NA
         country_state=na.locf(country_state), #use zoo to fill forward
         ci_g_mj=as.numeric(ci_g_mj),
         year=2018,
         total_volume=as.numeric(gsub(",","",total_vol)),
         avg_ci=as.numeric(avg_ci)
         )

cali_2018<-cali_data


location<- "2019_crude_average_ci_value_final.pdf"
# Extract the 2019 data
out <- extract_tables(location)
cali_data<-data.frame(do.call("rbind", out),stringsAsFactors = F)
total_vol=cali_data$X4[3]
avg_ci=cali_data$X4[2]
cali_data <-cali_data %>% slice(-c(1:4))
names(cali_data)<-cali_data[1,]
cali_data<-cali_data[-1,]
cali_data[1,1]<-"All"
cali_data<-clean_names(cali_data)

cali_data<-cali_data%>% 
  rename(volume = 4)%>%mutate(volume=as.numeric(gsub(",","",volume)))%>%
  filter(!is.na(volume))%>%
  mutate(country_state=ifelse(country_state=="",NA,country_state),#set blanks to NA
         country_state=na.locf(country_state), #use zoo to fill forward
         ci_g_mj=as.numeric(ci_g_mj),
         year=2019,
         total_volume=as.numeric(gsub(",","",total_vol)),
         avg_ci=as.numeric(avg_ci)
         )
cali_2019<-cali_data

#extract the 2020 data

location<- "2020_crude_average_ci_value_final.pdf"
# Extract the 2019 data
out <- extract_tables(location)
cali_data<-data.frame(do.call("rbind", out),stringsAsFactors = F)
total_vol=cali_data$X4[3]
avg_ci=cali_data$X4[2]
cali_data <-cali_data %>% slice(-c(1:4))
names(cali_data)<-cali_data[1,]
cali_data<-cali_data[-1,]
cali_data[1,1]<-"All"
cali_data<-clean_names(cali_data)


cali_data<-cali_data%>% 
  rename(volume = 4)%>%mutate(volume=as.numeric(gsub(",","",volume)))%>%
  filter(!is.na(volume))%>%
  mutate(country_state=ifelse(country_state=="",NA,country_state),#set blanks to NA
         country_state=na.locf(country_state), #use zoo to fill forward
         ci_g_mj=as.numeric(ci_g_mj),
         year=2020,
         total_volume=as.numeric(gsub(",","",total_vol)),
         avg_ci=as.numeric(avg_ci)
  )
cali_2020<-cali_data

#extract the 2021 data

location<- "2021_crude_average_ci_value_final.pdf"
# Extract the 2021 data
out <- extract_tables(location)
cali_data<-data.frame(do.call("rbind", out),stringsAsFactors = F)
total_vol=cali_data$X4[3]
avg_ci=cali_data$X4[2]
cali_data <-cali_data %>% slice(-c(1:4))
names(cali_data)<-cali_data[1,]
cali_data<-cali_data[-1,]
cali_data[1,1]<-"All"
cali_data<-clean_names(cali_data)

#extract the 2021 data
cali_data<-cali_data%>% 
  rename(volume = 4)%>%mutate(volume=as.numeric(gsub(",","",volume)))%>%
  filter(!is.na(volume))%>%
  mutate(country_state=ifelse(country_state=="",NA,country_state),#set blanks to NA
         country_state=na.locf(country_state), #use zoo to fill forward
         ci_g_mj=as.numeric(ci_g_mj),
         year=2021,
         total_volume=as.numeric(gsub(",","",total_vol)),
         avg_ci=as.numeric(avg_ci)
  )
cali_2021<-cali_data





location<- "2022_crude_average_ci_value_final.pdf"
# Extract the 2021 data
out <- extract_tables(location)
cali_data<-data.frame(do.call("rbind", out),stringsAsFactors = F)
total_vol=cali_data$X4[3]
avg_ci=cali_data$X4[2]
cali_data <-cali_data %>% slice(-c(1:4))
names(cali_data)<-cali_data[1,]
cali_data<-cali_data[-1,]
cali_data[1,1]<-"All"
cali_data<-clean_names(cali_data)

cali_data<-cali_data%>% 
  rename(volume = 4)%>%mutate(volume=as.numeric(gsub(",","",volume)))%>%
  filter(!is.na(volume))%>%
  mutate(country_state=ifelse(country_state=="",NA,country_state),#set blanks to NA
         country_state=na.locf(country_state), #use zoo to fill forward
         ci_g_mj=as.numeric(ci_g_mj),
         year=2022,
         total_volume=as.numeric(gsub(",","",total_vol)),
         avg_ci=as.numeric(avg_ci)
  )
cali_2022<-cali_data


download.file("https://ww2.arb.ca.gov/sites/default/files/classic/fuels/lcfs/crude-oil/2023_crude_average_ci_calculation__initial.pdf",destfile = "2023_crude_average_ci_calculation__initial.pdf",mode="wb")
location<- "2023_crude_average_ci_calculation__initial.pdf"
# Extract the 2023 data
out <- extract_tables(location)

max_cols <- max(sapply(out, function(x) ncol(as.data.frame(x))))

cali_data <- lapply(out, function(x) {
  df <- as.data.frame(x, stringsAsFactors = FALSE)
  # print(ncol(df))
  # Add an NA column as the first column
  if (ncol(df) < max_cols) {  # Adjusting to include the new first column
    print("adding column")
    df<- cbind("",df)
    colnames(df) <- paste0("V", 1:ncol(df))  # Consistent column names for binding later
  }
  
  return(df)
})

cali_data<-bind_rows(cali_data)

total_vol=cali_data$V4[3]
avg_ci=cali_data$V3[7]

cali_data <-cali_data %>% slice(-c(1:4))
cali_data[1,3]<-"ci_g_mj"
cali_data[1,4]<-"volume"
names(cali_data)<-cali_data[1,]
cali_data<-cali_data[-1,]
cali_data<-cali_data[-1,]
cali_data<-clean_names(cali_data)
cali_data[1,1]<-"All"
cali_data[1,4]<-total_vol

cali_data<-cali_data%>% 
  rename(volume = 4)%>%mutate(volume=as.numeric(gsub(",","",volume)))%>%
  filter(!is.na(volume))%>%
  mutate(country_state=ifelse(country_state=="",NA,country_state),#set blanks to NA
         country_state=na.locf(country_state), #use zoo to fill forward
         ci_g_mj=as.numeric(ci_g_mj),
         year=2023,
         total_volume=as.numeric(gsub(",","",total_vol)),
         avg_ci=as.numeric(avg_ci)
  )
cali_2023<-cali_data






#all the california data
cali_data<-bind_rows(cali_2015,cali_2016,cali_2017,cali_2018,cali_2019,cali_2020,cali_2021,cali_2022,cali_2023)



graph_data<-cali_data %>% filter(country_state!="All")%>%
         mutate(Canada=grepl("Canada",country_state),US=grepl("US",country_state),
         Type=interaction(Canada,US),
         Type=fct_recode(Type,"International"="FALSE.FALSE",
                         "Canada"="TRUE.FALSE",
                         "US"="FALSE.TRUE") ) %>% 
  group_by(year)%>%
  arrange(.,ci_g_mj) %>% 
  mutate(crude_num=1:n(),cum_prod=cumsum(volume))
  
  canada_data<-cali_data %>% filter(country_state=="Canada") %>%
    group_by(year)%>%
    summarize(Canada_avg=sum(ci_g_mj*volume)/sum(volume))



#graph_data$test<-graph_data$cum_prod/365/10^6
  library(stringr)
  ggplot(graph_data%>% filter(year>=2015)%>%
           mutate(crude_label=str_wrap(crude_name,width = 12))) +
    geom_rect(mapping=aes(xmin=(cum_prod-volume)/365/10^3,xmax=cum_prod/365/10^3,ymin=0,ymax=ci_g_mj,colour=Type,fill=Type))+
    
    #geom_label_repel(data=graph_data %>% group_by(year) %>% mutate(crude_label=paste(str_wrap(crude_name,width = 12),"\n(",ci_g_mj,"g/MJ)",sep = ""))%>%filter(Canada==TRUE),
    #                 aes(x=c(cum_prod-volume/2)/365/10^3,y=ci_g_mj,label = crude_label),
    #                 size = 2.5, ylim = c(35, 50),direction="both",force=2,box.padding = 1)+
    
    #geom_label_repel(data=graph_data %>% group_by(year)%>%slice(which.min(abs(ci_g_mj-avg_ci))),
    #                 aes(x=c(cum_prod-volume/2)/365/10^3,y=ci_g_mj,label=paste("California ",year,"\naverage\n(",avg_ci,"g/MJ)",sep="")),
    #                 size = 2.5,nudge_y =10,nudge_x = 120,color="dodgerblue")+
    
    #geom_label_repel(data=graph_data %>% group_by(year)%>%slice(which.min(abs(ci_g_mj-10.3))),
    #                 aes(x=c(cum_prod-volume/2)/365/10^3,y=ci_g_mj),label="Masnadi et al. (2018)\nglobal average\n(10.3 g/MJ)",
    #                 size = 2.5,nudge_y =10,nudge_x = -50,color="dodgerblue")+
    scale_fill_viridis("",discrete=T,,option = "D",labels=c("International Crudes","Canadian Crudes","US Crudes"))+  
    scale_colour_viridis("",discrete=T,option = "D",labels=c("International Crudes","Canadian Crudes","US Crudes"))+  
    scale_x_continuous(breaks = pretty_breaks(n=5),expand=c(0,0))+
    #expand_limits(x=1900)+
    facet_wrap(~year,nrow = 1)+
    #scale_y_continuous(expand=c(0,0),limits=c(-20,300))+
    theme_tufte()+theme(legend.position = "bottom",
                        legend.text = element_text(colour="black", size = 12),
                        plot.caption = element_text(size = 10, face = "italic",hjust=0),
                        plot.title = element_text(size=16,face = "bold"),
                        plot.subtitle = element_text(size = 10),
                        #panel.grid.minor = element_blank(),
                        text = element_text(size = 20,face = "bold"),
                        axis.text.y = element_text(size = 12,face = "bold", colour="black"),
                        #axis.text.x = element_blank(),
                        axis.text.x = element_text(size = 12, colour = "black"),
                        strip.text.x = element_text(size = 12, colour = "black", angle = 0),
                        axis.title.y = element_text(size = 15,face = "bold", colour="black"),
                        axis.title.x = element_text(size = 14,face = "bold", colour="black"),
                        plot.margin=unit(c(1,3,1,1), 'cm'),
                        NULL)+
    coord_cartesian(clip = 'off')+
    guides(fill=guide_legend(nrow=1,byrow=TRUE))+
    labs(#x=expression("Refinery Crude Supply, Thousands of Barrels per Day"),y=expression('Oil carbon intensity '*'(gCO'[2]*'e/MJ)'),
         #title="2015-19 California Carbon Intensity of Refinery Crude Supply",
         #subtitle="Difference between solid fill and outline is change in bids due to proposed federal fuel-specific OBPS.\nAssumed OBPS is 800kg/MWh for coal, 370kg/MWh for gas.",
         #caption="Source: CARB (2017-2020) data at https://www.arb.ca.gov/fuels/lcfs/crude-oil/crude-oil.htm. Graph by Andrew Leach"
      NULL)

    ggsave("cali_crude_tab.png",width=20,height=18,dpi=300,bg="white")
    ggsave("cali_crude_small.png",width=20,height=18,dpi=150,bg="white")

    blakes_blue="dodgerblue"  

year_vec<-seq(2015,2023,1)
year_index<-2022
for(year_index in year_vec)
{
    canada_avg<-canada_data$Canada_avg[grep(year_index,canada_data$year)]
  ggplot(graph_data%>%filter(year==year_index) %>%mutate(crude_label=str_wrap(crude_name,width = 12))) +
      geom_rect(mapping=aes(xmin=(cum_prod-volume)/365/10^3,xmax=cum_prod/365/10^3,ymin=0,ymax=ci_g_mj,colour=Type,fill=Type))+
      
      geom_label_repel(data=graph_data %>%filter(year==year_index) %>%mutate(crude_label=paste(str_wrap(crude_name,width = 12),"\n(",ci_g_mj,"g/MJ)",sep = ""))%>%filter(Canada==TRUE),
                       aes(x=c(cum_prod-volume/2)/365/10^3,y=ci_g_mj,label = crude_label),
                       size = 3.5, ylim = c(35, 50),xlim = c(0, 1500),
                       direction="both",force=2,box.padding = 1,max.overlaps = 100)+
    geom_label_repel(data=graph_data %>%filter(year==year_index,crude_name=="Bakken") %>%mutate(crude_label=paste("US ",str_wrap(crude_name,width = 12),"\n(",ci_g_mj,"g/MJ)",sep = "")),
                     aes(x=c(cum_prod-volume/2)/365/10^3,y=ci_g_mj,label = crude_label),
                     size = 3.5, ylim = c(35, 50),xlim = c(0, 1500),
                     direction="both",force=2,box.padding = 1,max.overlaps = 100,color=blakes_blue)+
    
    geom_label_repel(data=graph_data %>% filter(year==year_index) %>%group_by(year)%>%slice(which.min(abs(ci_g_mj-canada_avg))),
                     aes(x=c(cum_prod-volume/2)/365/10^3,y=ci_g_mj,label=paste("Canadian imports ",year,"\naverage\n(",round(canada_avg,2),"g/MJ)",sep="")),
                     size = 3.5,nudge_y =5,nudge_x = -120,color=blakes_blue)+
    

    geom_label_repel(data=graph_data %>% filter(year==year_index) %>%group_by(year)%>%slice(which.min(abs(ci_g_mj-avg_ci))),
                       aes(x=c(cum_prod-volume/2)/365/10^3,y=ci_g_mj,label=paste("California ",year,"\naverage\n(",avg_ci,"g/MJ)",sep="")),
                       size = 3.5,nudge_y =10,nudge_x = -120,color=blakes_blue)+
      
      geom_label_repel(data=graph_data %>% filter(year==year_index) %>%group_by(year)%>%slice(which.min(abs(ci_g_mj-10.3))),
                       aes(x=c(cum_prod-volume/2)/365/10^3,y=ci_g_mj),label="Masnadi et al. (2018)\nglobal average\n(10.3 g/MJ)",
                       size = 3.5,nudge_y =10,nudge_x = 0,color=blakes_blue)+
      scale_fill_viridis("",discrete=T,,option = "D",labels=c("International Crudes","Canadian Crudes","US Crudes"))+  
      scale_colour_viridis("",discrete=T,option = "D",labels=c("International Crudes","Canadian Crudes","US Crudes"))+  
      
      
      scale_x_continuous(breaks = pretty_breaks(n=15),expand=c(0,0))+
      #expand_limits(x=1600)+
      #expand_limits(y=55)+
      #facet_wrap(~year,ncol = 1)+
      #scale_y_continuous(expand=c(0,0),limits=c(-20,300))+
      theme_tufte()+theme(legend.position = "bottom",
                          legend.text = element_text(colour="black", size = 12),
                          plot.caption = element_text(size = 10, face = "italic",hjust=0),
                          plot.title = element_text(size=16,face = "bold"),
                          plot.subtitle = element_text(size = 10),
                          #panel.grid.minor = element_blank(),
                          text = element_text(size = 20,face = "bold"),
                          axis.text.y = element_text(size = 12,face = "bold", colour="black"),
                          #axis.text.x = element_blank(),
                          axis.text.x = element_text(size = 12, colour = "black"),
                          strip.text.x = element_text(size = 12, colour = "black", angle = 0),
                          axis.title.y = element_text(size = 15,face = "bold", colour="black"),
                          axis.title.x = element_text(size = 14,face = "bold", colour="black"),
                          #top-right-bottom-left
                          plot.margin=unit(c(1,3,1,1), 'cm'),
                          NULL)+
      coord_cartesian(clip = 'off')+
      guides(fill=guide_legend(nrow=1,byrow=TRUE))+
      labs(x=expression("Refinery Crude Supply, Thousands of Barrels per Day"),y=expression('Oil carbon intensity '*'(gCO'[2]*'e/MJ)'),
           title=paste("Carbon Intensity of California Refinery Crude Supply,",year_index),
           #subtitle="Difference between solid fill and outline is change in bids due to proposed federal fuel-specific OBPS.\nAssumed OBPS is 800kg/MWh for coal, 370kg/MWh for gas.",
           caption="Source: CARB (2015-2023) data at https://www.arb.ca.gov/fuels/lcfs/crude-oil/crude-oil.htm. Graph by Andrew Leach")
     ggsave(paste("cali_crude_",year_index,".png",sep=""),width=16,height=9,dpi=300,bg="white")
    ggsave(paste("cali_crude_",year_index,"_small.png",sep=""),width=16,height=9,dpi=150,bg="white")
}   
    
    
