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


#all the california data
cali_data<-bind_rows(cali_2015,cali_2016,cali_2017,cali_2018,cali_2019)



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
    summarize(Canada_avg=sum(ci_g_mj*volume)/sum(volume))



#graph_data$test<-graph_data$cum_prod/365/10^6
  library(stringr)
  ggplot(graph_data%>%mutate(crude_label=str_wrap(crude_name,width = 12))) +
    geom_rect(mapping=aes(xmin=(cum_prod-volume)/365/10^3,xmax=cum_prod/365/10^3,ymin=0,ymax=ci_g_mj,colour=Type,fill=Type))+
    
    geom_label_repel(data=graph_data %>%mutate(crude_label=paste(str_wrap(crude_name,width = 12),"\n(",ci_g_mj,"g/MJ)",sep = ""))%>%filter(Canada==TRUE),
                     aes(x=c(cum_prod-volume/2)/365/10^3,y=ci_g_mj,label = crude_label),
                     size = 2.5, ylim = c(35, 50),direction="both",force=2,box.padding = 1)+
    
    geom_label_repel(data=graph_data %>% group_by(year)%>%slice(which.min(abs(ci_g_mj-avg_ci))),
                     aes(x=c(cum_prod-volume/2)/365/10^3,y=ci_g_mj,label=paste("California ",year,"\naverage\n(",avg_ci,"g/MJ)",sep="")),
                     size = 2.5,nudge_y =10,nudge_x = 120,color=blakes_blue)+
    
    geom_label_repel(data=graph_data %>% group_by(year)%>%slice(which.min(abs(ci_g_mj-10.3))),
                     aes(x=c(cum_prod-volume/2)/365/10^3,y=ci_g_mj),label="Masnadi et al. (2018)\nglobal average\n(10.3 g/MJ)",
                     size = 2.5,nudge_y =10,nudge_x = -50,color=blakes_blue)+
    scale_fill_viridis("",discrete=T,,option = "D",labels=c("International Crudes","Canadian Crudes","US Crudes"))+  
    scale_colour_viridis("",discrete=T,option = "D",labels=c("International Crudes","Canadian Crudes","US Crudes"))+  
    
    
    scale_x_continuous(breaks = pretty_breaks(n=5),expand=c(0,0))+
    expand_limits(x=1900)+
    facet_wrap(~year,ncol = 1)+
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
                        NULL)+
    guides(fill=guide_legend(nrow=1,byrow=TRUE))+
    labs(x=expression("Refinery Crude Supply in 2019, Thousands of Barrels per Day"),y=expression('Oil carbon intensity '*'(gCO'[2]*'e/MJ)'),
         title="2015-19 California Carbon Intensity of Refinery Crude Supply",
         #subtitle="Difference between solid fill and outline is change in bids due to proposed federal fuel-specific OBPS.\nAssumed OBPS is 800kg/MWh for coal, 370kg/MWh for gas.",
         caption="Source: CARB (2017-2020) data at https://www.arb.ca.gov/fuels/lcfs/crude-oil/crude-oil.htm. Graph by Andrew Leach")

    ggsave("cali_crude_tab.png",width=20,height=18,dpi=600)
    ggsave("cali_crude_small.png",width=20,height=18,dpi=150)
    











data_2017<-cali_data


# Location of CARB 2018 carbon intensity pdf file
download.file(destfile = "2018_crude_average_ci_value_edits.pdf",'https://www.arb.ca.gov/fuels/lcfs/crude-oil/2018_crude_average_ci_value_final.pdf',mode="wb")
#location <- 'https://ww3.arb.ca.gov/fuels/lcfs/crude-oil/2018_preliminary_draft_calculation_crude_average_ci_value.pdf'
#location<- "2017_crude_average_ci_value_edits.pdf"
location<- "2018_crude_average_ci_value_edits.pdf"

# Extract the 2018 data table
out <- extract_tables(location)
out[[1]]<-NULL
cali_data<-data.frame(do.call("rbind", out),stringsAsFactors = F)

colnames(cali_data)<-cali_data[1,]
cali_data<-clean_names(cali_data[-1,])
cali_data<-cali_data[-grep("Country",cali_data[,1]),]
cali_data[1,1]<-"All"
cali_data$country_state[cali_data$country_state==""]<- NA
cali_data$country_state <- na.locf(cali_data$country_state)
cali_data[,4]<-gsub(",","",cali_data[,4])
cali_data[,-c(1,2)] <- sapply( cali_data[,-c(1,2)], as.numeric )


cali_data<-cali_data %>% group_by(country_state) %>%
  mutate(country_state_avg=sum(ci_g_mj*x2018_volume_bbl)/sum(x2018_volume_bbl)) %>% ungroup()

graph_data<-cali_data %>% filter(country_state!="All",!is.na(x2018_volume_bbl)) %>%
  mutate(Canada=grepl("Canada",country_state),US=grepl("US",country_state),
         Type=interaction(Canada,US),
         Type=fct_recode(Type,"International"="FALSE.FALSE",
                         "Canada"="TRUE.FALSE",
                         "US"="FALSE.TRUE") ) %>% arrange(.,ci_g_mj) %>% 
  mutate(crude_num=1:n(),cum_prod=cumsum(x2018_volume_bbl))


graph_data$test<-graph_data$cum_prod/365/10^6
graph_data$label<-gsub(" \\(all grades\\)","",graph_data$crude_name)
graph_data$label<-str_wrap(graph_data$label,width = 15)

set_png("cali_crude_tab.png")
ggplot(graph_data) +
  geom_rect(mapping=aes(xmin=(cum_prod-x2018_volume_bbl)/365/10^3,xmax=cum_prod/365/10^3,ymin=0,ymax=ci_g_mj,colour=Type,fill=Type))+
  geom_text_repel(data=filter(graph_data,country_state=="Canada"),
  aes(x=cum_prod/365/10^3,y=ci_g_mj,label=label),direction="x",vjust=0,angle=90,
              size = 4.5,segment.size = 0.5,nudge_y = 10) +
  geom_text_repel(data=filter(graph_data,crude_name=="West Texas Intermediate"),
                  aes(x=cum_prod/365/10^3,y=ci_g_mj,label=label),direction="x",vjust=0,angle=90,
                  size = 4.5,segment.size = 0.5,nudge_y = 25) +
  
  geom_text_repel(data=filter(graph_data,crude_name=="Maya"),
                  aes(x=cum_prod/365/10^3,y=ci_g_mj,label=label),direction="x",vjust=0,angle=90,
                  size = 4.5,segment.size = 0.5,nudge_y = 25,nudge_x = -100) +
  
  scale_fill_viridis("",discrete=T,,option = "D",labels=c("International Crudes","Canadian Crudes","US Crudes"))+  
  scale_colour_viridis("",discrete=T,option = "D",labels=c("International Crudes","Canadian Crudes","US Crudes"))+  
  
  
  #scale_color_manual("",values=c("black","firebrick","blue"))+   
  scale_x_continuous(expand=c(0,0),breaks = pretty_breaks())+
  #scale_y_continuous(expand=c(0,0),limits=c(-20,300))+
  ajl_hourly()+
  guides(fill=guide_legend(nrow=1,byrow=TRUE))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.25,size=7,face="bold"),
        axis.text.y = element_text(size=7,face="bold"),
        plot.subtitle = element_text(size = 12, face = "italic"),
        legend.text = element_text(colour="black", size = 10, face = "bold"),
        plot.caption = element_text(size = 11, face = "italic"))+
  
  guides(fill=guide_legend(nrow=1,byrow=TRUE))+
  labs(x=expression("Refinery Crude Supply in 2018, Thousands of Barrels per Day"),y=expression('Oil carbon intensity '*'(gCO'[2]*'e/MJ)'),
       title="2018 California Carbon Intensity of Refinery Crude Supply",
       #subtitle="Difference between solid fill and outline is change in bids due to proposed federal fuel-specific OBPS.\nAssumed OBPS is 800kg/MWh for coal, 370kg/MWh for gas.",
       caption="\nSource: CARB (2018) data at https://www.arb.ca.gov/fuels/lcfs/crude-oil/crude-oil.htm.")
dev.off()



canada_data<-cali_data %>% filter(country_state=="Canada") %>%
  mutate(Canada_avg=sum(ci_g_mj*x2018_volume_bbl)/sum(x2018_volume_bbl))

cali_crude_data<-cali_data %>% filter(country_state=="US California*") %>%
  mutate(cali_avg=sum(ci_g_mj*x2018_volume_bbl)/sum(x2018_volume_bbl)) %>% arrange(ci_g_mj) %>%
  mutate(merit=cumsum(x2018_volume_bbl)/sum(x2018_volume_bbl))



#2012 data
location<-"https://ww3.arb.ca.gov/fuels/lcfs/crude-oil/2012-crude-ave-ci.pdf"
out <- extract_tables(location)
#out[[1]]<-NULL
cali_data<-data.frame(do.call("rbind", out),stringsAsFactors = F)
cali_data$X1[2]<-"All"
cali_data$X1[cali_data$X1==""]<- NA
cali_data$X1<- na.locf(cali_data$X1)

colnames(cali_data)<-cali_data[1,]
cali_data<-cali_data[-1,]
cali_data<-clean_names(cali_data)
cali_data[,4]<-gsub(",","",cali_data[,4])
cali_data[,-c(1,2)] <- sapply( cali_data[,-c(1,2)], as.numeric )

graph_data<-cali_data %>% filter(country_state!="All",!is.na(x2018_volume_bbl)) %>%
  mutate(Canada=grepl("Canada",country_state),US=grepl("US",country_state),
         Type=interaction(Canada,US),
         Type=fct_recode(Type,"International"="FALSE.FALSE",
                         "Canada"="TRUE.FALSE",
                         "US"="FALSE.TRUE") ) %>% arrange(.,ci_g_mj) %>% 
  mutate(crude_num=1:n(),cum_prod=cumsum(x2018_volume_bbl))

canada_data<-cali_data %>% filter(country_state=="Canada") %>%
  mutate(Canada_avg=sum(ci_g_mj*x2012_volume_bbl)/sum(x2012_volume_bbl))


#2013 data
#location<-https://ww3.arb.ca.gov/fuels/lcfs/crude-oil/2013-crude-ave-ci.pdf
#2014 data
#location<-https://ww3.arb.ca.gov/fuels/lcfs/crude-oil/2014_crude_average_ci_value_final.pdf
#2015 data
#location<-https://ww3.arb.ca.gov/fuels/lcfs/crude-oil/2015_crude_average_ci_value_final.pdf
#2016 data
#location<-https://ww3.arb.ca.gov/fuels/lcfs/crude-oil/2016_crude_average_ci_value_final.pdf




# Country-level social cost of carbon -------------------------------------




# cscc-database-2018
# 
# Database of "Country-level social cost of carbon"
# 
# run: specifies the climate damages model ('bhm_lr': Burke, Hsiang & Miguel (2015) (BHM) long-run (5-lag) , pooled model;'bhm_richpoor_lr': BHM long-run, income-dependent (rich-poor) model; 'bhm_sr': BHM short-run (no lag), pooled model;'bhm_richpoor_sr': BHM short-run, income-dependent model; 'djo': Dell, Jones & Olken (2012) alternative impact function)
# 
# dmgfuncpar: specifies whether full damage uncertainty is included in the simulation ('estimates': simulations use central damage parameters; 'bootstrap': damage function uncertainty is analyzed using bootstrapping)
# 
# climate: specifies whether full climate projection uncertainty is included in the simulation ('expected': simulations use central climate projection parameters; 'uncertain': climate uncertainty is analyzed using bootstrapping)
# 
# SSP: shared socioeconomic pathway -- socioeconomic projections including the country-level population and GDP projections
# 
# RCP: representative concentration pathway -- climate forcing scenario
# 
# N: number of generated runs
# 
# ISO3: country code, for global values use 'WLD'
# 
# prtp: pure rate of time preference, 'NA' for fixed discounting
# 
# eta: elasticity of marginal utility, 'NA' for fixed discounting
# 
# dr: fixed discount rate, 'NA' for endogenous/Ramsey-rule discounting cases
# 
# 16.7%: low (16.7% percentile) CSCC
# 
# 50%	: median CSCC
# 
# 83.3%: high (83.3% percentile) CSCC
# 
# Note: The 3 statistics (low, median, high CSCC) are the median of the posterior distribution of the statistics after bayesian bootstrapping, i.e. for low, the median of the posterior distribution of the 16.7% percentile.
# 
# Cite: Ricke, K., L. Drouet, K. Caldeira and M. Tavoni. "Country-level social cost of carbon" (2018)



clscc_data <- read.csv(file="cscc_full_v1.csv")
names(clscc_data)<-
  c("run","dmg_fun","climate","SSP","RCP","N","Country","PRTP","ETA","discount","Pctl_low","Pctl_50","Pctl_high")

wld_data<-clscc_data %>% filter(Country=="WLD")



tbl_data<-wld_data %>% group_by(SSP,RCP,climate) %>% summarize(rge_50_l=range(Pctl_50)[1],rge_50_h=range(Pctl_50)[2])
tbl_data<-tbl_data %>% group_by(SSP) %>% mutate(duplicate=duplicated(RCP)) %>% ungroup()
tbl_data[[1]]<-as.character(tbl_data[[1]])
first <- !duplicated(tbl_data[[1]])
tbl_data[[1]][!first] <- ""
tbl_data[[2]]<-as.character(tbl_data[[2]])
tbl_data$RCP[tbl_data$duplicate]<-""
tbl_data$duplicate<-NULL






xtable(tbl_data)


require(xtable)

# set up data frame
df <- data.frame(c(replicate(2, c("L1")), replicate(2, c("L2"))),
                 replicate(4, "b"),
                 replicate(4, runif(4, 1, 10)) )

# only needed if first column consists of numbers
df[[1]] <- as.character(df[[1]])

rle.lengths <- rle(df[[1]])$lengths
first <- !duplicated(df[[1]])
df[[1]][!first] <- ""



# define appearance of \multirow
df[[1]][first] <-
  paste0("\\midrule\\multirow{", rle.lengths, "}{*}{\\textbf{", df[[1]][first], "}}")

strCaption <- paste0("\\textbf{Table Whatever} This table is just produced with some ",
                     "random data and does not mean anything. Just to show you how ",
                     "things work.")

# set up xtable output
print(xtable(df, digits = c(0, 0, 0, 3, 1, 0, 6), # first zero "represents" row numbers which we skip later
             align = "lllrr|rr",  # align and put a vertical line (first "l" again represents column of row numbers)
             caption = strCaption, label = "testTable"),
      size = "footnotesize", #Change size; useful for bigger tables "normalsize" "footnotesize"
      include.rownames = FALSE, #Don't print rownames
      include.colnames = FALSE, #We create them ourselves
      caption.placement = "top", #"top", NULL
      hline.after=NULL, #We don't need hline; we use booktabs
      floating=TRUE, # whether \begin{Table} should be created (TRUE) or not (FALSE)
      sanitize.text.function = force, # Important to treat content of first column as latex function
      add.to.row = list(pos = list(-1,
                                   2,
                                   nrow(df)),
                        command = c(paste("\\toprule \n",  # NEW row
                                          "\\multicolumn{2}{c}{} & \\multicolumn{2}{c}{\\textbf{colLabel1}} & \\multicolumn{2}{c}{colLabel2} \\\\\n",
                                          "\\cmidrule(l){3-4} \\cmidrule(l){5-6}\n",
                                          " & & a1 & a2 & a3 & a4 \\\\\n", # NEW row 
                                          "\\midrule \n"
                        ),
                        paste("\\cmidrule(l){3-4} \\cmidrule(l){5-6}\n" # we may also use 'pos' and 'command' to add a midrule
                        ),
                        paste("\\bottomrule \n"  # paste is used as it is more flexible regarding adding lines
                        )
                        )
      )
)





# Cost of avoided emissions -----------------------------------------------


#get break-even cost data from Eriksson (2018)

temp_data <- read_xlsx("Figure_1_data_for_Andrew_Leach.xlsx",range = "A1:J189")
names(temp_data)[1]<-"Region"
temp_data<-temp_data %>% select(-X__2) %>% filter(`Life Cycle Category`!="Abandoned") %>% 
  arrange(Price)
temp_data<-rbind(temp_data,temp_data) %>% arrange(Price) %>%
  mutate(Q_sum=cumsum(Qty),Q_sum=Q_sum/1000,Qty=Qty/1000)
temp_data$Producing<-factor(temp_data$`Life Cycle Category`)
#levels(oil_data$Producing)[-grep("Producing",levels(oil_data$Producing))]
temp_data$Producing<-fct_other(oil_data$Producing, keep = c("Producing"),other_level = "Not currently producing" )


ggplot(arrange(temp_data,Q_sum)) +
  #geom_rect(mapping=aes(xmin=merit2-size,xmax=merit2,ymin=-10,ymax=bid_adj,fill=Plant_Type))+
  geom_rect(mapping=aes(xmin=Q_sum-Qty,xmax=Q_sum,ymin=-1,ymax=Price,colour=interaction(Region,Producing,sep = ", "),fill=interaction(Region,Producing,sep = ", ")))+
  #geom_rect(mapping=aes(xmin=merit2-size,xmax=price,ymin=-10,ymax=bid_adj,colour=Plant_Type),fill=NA)+
  #geom_vline(aes(xintercept=actual_ail-gen+hourly_exports-hourly_imports,colour="Net Internal Load"),linetype=2,size=1)+
  #geom_vline(aes(xintercept=8015.738-gen,colour="Net Internal Gen"),linetype=2,size=1)+
  #geom_hline(aes(yintercept=actual_posted_pool_price),colour="dodgerblue",linetype=2,size=1)+
  scale_fill_manual("",values=colors_tableau10())+  
  scale_colour_manual("",values=colors_tableau10())+  
  #scale_color_manual("",values=c("black","firebrick","blue"))+   
  #scale_x_continuous(expand=c(0,0),breaks = seq(0,13000,3000),limits = c(0,13000))+
  #scale_y_continuous(expand=c(0,0),limits=c(-20,300))+
  ajl_line()+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))+
  labs(x=paste("Oil Production in 2030, Billion Barrels"),y="Breakeven Oil Price (Brent Equivalent, $US/bbl)",
       title="Hypothetical Oil Production Cost Curve",
       #subtitle="Difference between solid fill and outline is change in bids due to proposed federal fuel-specific OBPS.\nAssumed OBPS is 800kg/MWh for coal, 370kg/MWh for gas.",
       caption="Source: Derived from Eriksson (2018).")

#add social costs

temp_data<- temp_data %>% mutate(
  ghg_shock=rnorm(n(),50^0.5,10^.5)^2+Price*5, #mean 50kg/bbl
  soc_cost=Price+ghg_shock*50/1000
)


set_png("hypo_crude_cost.png")
ggplot(arrange(temp_data,Q_sum)) +
  #geom_rect(mapping=aes(xmin=merit2-size,xmax=merit2,ymin=-10,ymax=bid_adj,fill=Plant_Type))+
  geom_rect(mapping=aes(xmin=Q_sum-Qty,xmax=Q_sum,ymin=-1,ymax=Price,colour=Producing,fill=Producing))+
  geom_rect(mapping=aes(xmin=Q_sum-Qty,xmax=Q_sum,ymin=-1,ymax=soc_cost,colour=Producing),fill=NA)+
  #geom_rect(mapping=aes(xmin=Q_sum-Qty,xmax=Q_sum,ymin=-1,ymax=Price,colour=Producing,fill=NA))+
  #geom_rect(mapping=aes(xmin=merit2-size,xmax=price,ymin=-10,ymax=bid_adj,colour=Plant_Type),fill=NA)+
  #geom_vline(aes(xintercept=actual_ail-gen+hourly_exports-hourly_imports,colour="Net Internal Load"),linetype=2,size=1)+
  #geom_vline(aes(xintercept=8015.738-gen,colour="Net Internal Gen"),linetype=2,size=1)+
  #geom_hline(aes(yintercept=actual_posted_pool_price),colour="dodgerblue",linetype=2,size=1)+
  scale_fill_manual("",values=colors_tableau10(),labels=c("Region 1","Region 2"))+  
  scale_colour_manual("",values=colors_tableau10(),labels=c("Region 1, Price + Social Cost","Region 2, Price + Social Cost"))+  
  #scale_color_manual("",values=c("black","firebrick","blue"))+   
  #scale_x_continuous(expand=c(0,0),breaks = seq(0,13000,3000),limits = c(0,13000))+
  #scale_y_continuous(expand=c(0,0),limits=c(-20,300))+
  ajl_line()+
  theme(legend.key.width=unit(3,"line"))+
  guides(fill=guide_legend(nrow=1,byrow=TRUE),color=F)+
  labs(x=paste("Oil Production in 2030, Millions of Barrels Per Day"),y="Breakeven Oil Price (Brent Equivalent, $US/bbl)",
       title="Hypothetical Oil Production Private Break-Even Cost and GHG Cost Curve",
       #subtitle="Difference between solid fill and outline is change in bids due to proposed federal fuel-specific OBPS.\nAssumed OBPS is 800kg/MWh for coal, 370kg/MWh for gas.",
       caption="Source: Author's calculations (hypothetical)")
dev.off()



# Stylized illustration of pricing versus not
plotdata<-data.frame(
  prod_cost=seq(1,10),
  ext_cost=c(1,3,6,12,15,23,12,26,8,29),
  abate=1
) %>% 
  mutate(cost=10*prod_cost,soc_cost=cost+ext_cost) %>%
  arrange(cost) %>%
  mutate(total=cumsum(abate),
         position=ifelse(row_number()>1,abate/2+lag(total,1),abate/2),
         cheap=(cost<=50),
         expensive=(prod_cost %in% sample(10,5))) %>%
  arrange(soc_cost) %>%
  mutate(total_soc=cumsum(abate),
         position_soc=ifelse(row_number()>1,abate/2+lag(total,1),abate/2),
         cheap_soc=(soc_cost<=50),
         expensive_soc=(soc_cost %in% sample(10,5))) %>%
  gather(choice,adopt,-position,-cost) %>%
  group_by(choice) %>%
  mutate(label=paste0("Total Cost: $",sum(cost*(adopt==T)))) %>%
  ungroup() %>% 
  mutate(choice=ifelse(choice=="cheap","Efficient (Least-Cost) Approach",
                       "Inefficient (Higher-Cost) Approach"))


#set_png(file_sent = "tombe_macs.png")
ggplot(plotdata,aes(position,cost,width=1))+
  geom_col(fill="dodgerblue",color="white")+
  geom_col(data=plotdata%>% filter(cost>50),fill="firebrick",color="white")+
  geom_hline(yintercept=0,size=1)+
  #facet_wrap(~choice)+
  geom_text(data=plotdata %>% filter(cost==20),aes(x=1,hjust=0,y=175,label=label),
            color="firebrick",fontface="bold")+
  #mytheme+
  theme(strip.background = element_rect(fill="gray90"))+
  scale_y_continuous(label=dollar)+
  labs(x="Emissions Reductions",y="Cost per Tonne",title="How We Lower Emissions Matters",
       subtitle="Displays two ways to lower GHGs by the same amount, but at different total costs.
       ")
#dev.off()
# Illustrating cap-and-trade versus carbon tax
plotdata2<-plotdata %>%
  mutate(adopt=(cost<=100),
         choice=ifelse(choice=="Efficient (Least-Cost) Approach","Carbon Tax","Cap-and-Trade"),
         price=ifelse(choice=="Carbon Tax",100,NA)) 
#set_png(file_sent = "tombe_tax_ct.png")
ggplot(plotdata2,aes(position,cost,width=1))+
  geom_col(fill="dodgerblue",color="white")+
  geom_col(data=plotdata2 %>% filter(adopt==T),fill="firebrick",color="white")+
  geom_hline(yintercept=0,size=1)+
  facet_wrap(~choice)+
  geom_hline(aes(yintercept=ifelse(choice=="Carbon Tax",100,NA)),color="darkgreen",size=2)+
  geom_vline(aes(xintercept=ifelse(choice=="Cap-and-Trade",5,NA)),color="darkgreen",size=2)+
  geom_text(data=plotdata2 %>% filter(cost==20),size=3,hjust=0,color="firebrick",
            aes(x=0,y=160,label=ifelse(choice=="Carbon Tax","Government Sets Price.\nFirms Choose to Emit or Not",
                                       "Government Sets Quantity.\nFirms Compete for\nEmissions Permits")))+
  #mytheme+
  theme(strip.background = element_rect(fill="gray90"))+
  scale_y_continuous(label=dollar)+
  labs(x="Emissions Reductions",y="Cost per Tonne",title="Policy Options to Induce Least-Cost Abatement Choices",
       subtitle="Displays two ways to motivate the least-cost emission reduction options be chosen.
       ")
#dev.off()


#get data from IWG (2016)

iwg_data <- read_xlsx("IWG.xlsx")
all_years<-data.frame(seq(2015,2073)) 
names(all_years)<-"Year"
all_years <- all_years %>%left_join(iwg_data)
#use a quadratic interpolation and extrapolation
linearMod <- lm(`5% Average` ~ Year+I(Year^2), data=all_years)  # build linear regression model on full data
all_years$`5% Average` <- predict(linearMod, all_years)  # predict values

linearMod <- lm(`3% Average` ~ Year+I(Year^2), data=all_years)  # build linear regression model on full data
all_years$`3% Average` <- predict(linearMod, all_years)  # predict values

linearMod <- lm(`2.5% Average` ~ Year+I(Year^2), data=all_years)  # build linear regression model on full data
all_years$`2.5% Average` <- predict(linearMod, all_years)  # predict values

linearMod <- lm(`High Impact (95th pct at 3%)` ~ Year+I(Year^2), data=all_years)  # build linear regression model on full data
all_years$`High Impact (95th pct at 3%)` <- predict(linearMod, all_years)  # predict values

ggplot(all_years)+
  geom_point(data=iwg_data,aes(Year,`5% Average`,colour="5% avg"),size=3)+
  geom_line(aes(Year,`5% Average`,colour="5% avg"))+
  geom_point(data=iwg_data,aes(Year,`3% Average`,colour="3% avg"),size=3)+
  geom_line(aes(Year,`3% Average`,colour="3% avg"))+
  geom_point(data=iwg_data,aes(Year,`2.5% Average`,colour="2.5% avg"),size=3)+
  geom_line(aes(Year,`2.5% Average`,colour="2.5% avg"))+
  geom_point(data=iwg_data,aes(Year,`High Impact (95th pct at 3%)`,colour="3%, 95th pctl"),size=3)+
  geom_line(aes(Year,`High Impact (95th pct at 3%)`,colour="3%, 95th pctl"))+
scale_color_manual("Discount rate and sampling rule",values=colors_tableau10())  +
ajl_line()+
  labs(x=paste("Year"),y="Social Cost of Carbon ($US2007/t)",
       title="IWG (2016) Social Cost of Carbon",
       subtitle="Points are provided by IWG (2016) and lines are quadratic interpolation and extrapolation results",
       #subtitle="Difference between solid fill and outline is change in bids due to proposed federal fuel-specific OBPS.\nAssumed OBPS is 800kg/MWh for coal, 370kg/MWh for gas.",
       caption="Source: IWG(2016), graph by Andrew Leach.")
#convert to 2018 dollars using GDP deflator and one year model assumed inflation of 2%
all_years[,-1]<-lapply(all_years[,-1],function(i) i*1.17*1.02)
linearMod <- lm(`5% Average` ~ Year+I(Year^2), data=all_years)  # build linear regression model on full data
ggplot(all_years)+
  geom_point(data=iwg_data,aes(Year,`5% Average`*1.17*1.02,colour="5% avg"),size=3)+
  geom_line(aes(Year,`5% Average`,colour="5% avg"))+
  geom_point(data=iwg_data,aes(Year,`3% Average`*1.17*1.02,colour="3% avg"),size=3)+
  geom_line(aes(Year,`3% Average`,colour="3% avg"))+
  geom_point(data=iwg_data,aes(Year,`2.5% Average`*1.17*1.02,colour="2.5% avg"),size=3)+
  geom_line(aes(Year,`2.5% Average`,colour="2.5% avg"))+
  geom_point(data=iwg_data,aes(Year,`High Impact (95th pct at 3%)`*1.17*1.02,colour="3%, 95th pctl"),size=3)+
  geom_line(aes(Year,`High Impact (95th pct at 3%)`,colour="3%, 95th pctl"))+
  scale_color_manual("Discount rate and sampling rule",values=colors_tableau10())  +
  ajl_line()+
  labs(x=paste("Year"),y="Social Cost of Carbon ($US2018/t)",
       title="IWG (2016) Social Cost of Carbon",
       subtitle="Points are provided by IWG (2016) and lines are quadratic interpolation and extrapolation results",
       #subtitle="Difference between solid fill and outline is change in bids due to proposed federal fuel-specific OBPS.\nAssumed OBPS is 800kg/MWh for coal, 370kg/MWh for gas.",
       caption="Source: IWG(2016), graph by Andrew Leach.")

write.xlsx(all_years, file="IWG_ipl.xlsx", asTable = FALSE)
 
names(temp_data)[1]<-"Region"
temp_data<-temp_data %>% select(-X__2) %>% filter(`Life Cycle Category`!="Abandoned") %>% 
  arrange(Price)
temp_data<-rbind(temp_data,temp_data) %>% arrange(Price) %>%
  mutate(Q_sum=cumsum(Qty),Q_sum=Q_sum/1000,Qty=Qty/1000)
temp_data$Producing<-factor(temp_data$`Life Cycle Category`)
#levels(oil_data$Producing)[-grep("Producing",levels(oil_data$Producing))]
temp_data$Producing<-fct_other(oil_data$Producing, keep = c("Producing"),other_level = "Not currently producing" )



















