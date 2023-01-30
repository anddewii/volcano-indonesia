fig <- function(width, heigth){
     options(repr.plot.width = width, repr.plot.height = heigth)
}

#add library
library(tidyverse)
library(dplyr)
library(lubridate)
library(janitor)
library(skimr)
library(cowplot)
library(colorspace) 
library(ggrepel) 
options(warn=-1, message=FALSE)

olcano_events <- read_csv("../input/volcano-events-in-indonesia-13002021/volcano-events-1300-2021.csv", show_col_type=FALSE)

#standardize column names
volcano <- volcano_events %>% clean_names()
#glimpse(volcano)

#missing data
volcano <- volcano %>% mutate(vei = if_else(is.na(vei), 2, vei),
                               total_deaths = if_else(is.na(total_deaths), 0, total_deaths))
glimpse(volcano)

#by island 
fig(12, 7)
erupth_island <- volcano %>% 
  group_by(island) %>% 
  summarize(cnt_mount=n())

ggplot(data=erupth_island, aes(x=reorder(island, -cnt_mount), y=cnt_mount, fill=island)) +
  geom_bar(stat="identity", width=0.9, color="white") +
  geom_text(aes(label=cnt_mount), position=position_stack(vjust=0.5), size=5) +
  labs(title="Number of Eruptions by Island") +
  xlab("Island") +
  ylab("Number of Mountains") +
  theme(legend.position = "none", text = element_text(size=18))



#Top Ten by mountains
fig(12, 7)
erupth_mountain <- volcano %>% group_by(name) %>%
    summarize(cnt_bymount=n()) %>%
    arrange(desc(cnt_bymount))

top10_erupth_mountain <- erupth_mountain%>%
    top_n(10) %>% arrange(desc(cnt_bymount))

ggplot(data=top10_erupth_mountain, aes(x=reorder(name, -cnt_bymount), y=cnt_bymount, fill=name))+
    geom_bar(stat="identity", width=0.9, color="white")+
    scale_x_discrete(guide=guide_axis(angle=45))+
    geom_text(aes(label=cnt_bymount), position=position_stack(vjust=0.5), size=5)+
    labs(title="Top 10 Montain Eruption") +
    xlab("Mountains Name")+
    ylab("Eruoptions")+
    theme(legend.position = "none", text = element_text(size=18))


erupth_vei <- volcano %>% group_by(vei) %>% summarise(cnt_vei=n())

ggplot(data=erupth_vei, aes(x=reorder(vei, -cnt_vei), y=cnt_vei, fill=vei))+
    geom_bar(stat="identity", width=0.8, color="white", position = position_dodge(width=0.1))+
    geom_text(aes(label=cnt_vei), position=position_stack(vjust=0.5), size=5)+
    labs(title="Eruptions by VEI Score")+
    xlab("VEI Score")+
    ylab("Eruptions")+
    theme(legend.position = "none", text = element_text(size=18))


#Eruptions by Mountain Type
erupth_type <- volcano %>% 
  group_by(type) %>% 
  summarise(cnt_bytype=n())
ggplot(data=erupth_type, aes(x=reorder(type, -cnt_bytype), y=cnt_bytype, fill=type)) +
      geom_bar(stat="identity", width=0.8, color="white", position = position_dodge(width = 0.1)) +
      geom_text(aes(label=cnt_bytype), position=position_stack(vjust=0.5), size=5) +
      scale_x_discrete(guide = guide_axis(angle=45)) +
      labs(title="Eruptions by Mountain Type") +
      xlab("Mountains Type") +
      ylab("Number of mountains") +
      theme(legend.position = "none", text = element_text(size=18))

#By Mountain Elevation
fig(12,7)
erupt_elevation<- volcano %>% select(name, elevation_m) %>% distinct()
ggplot(data=erupt_elevation, aes(x=reorder(name, -elevation_m), y=elevation_m, fill=name)) +
  geom_bar(stat="identity", width=0.8, color="white", position = position_dodge(width = 0.1)) +
  scale_x_discrete(guide = guide_axis(angle=45)) +
  # geom_text(aes(label=elevation_m), position=position_stack(vjust=0.5), size=5) +
  labs(title="Eruptions by Mountain Elevation") +
  xlab("Mountains Name") +
  ylab("Elevation") +
  theme(legend.position = "none", text = element_text(size=15))
  
#Deadly Eruptions
erupt_death <- volcano %>% select(name, erupt_date, total_deaths) %>% arrange(desc(total_deaths)) %>% top_n(10)
ggplot(data=erupt_death, aes(x=reorder(name, -total_deaths), y=total_deaths, fill=name)) +
  geom_bar(stat="identity", width=0.9, color="white") +
  scale_x_discrete(guide = guide_axis(angle=45)) +
  geom_text(aes(label=total_deaths), position=position_stack(vjust=0.5), size=5) +
  labs(title="Total Deaths by Mountain") +
  xlab("Mountains Name") +
  ylab("Deaths") +
  theme(legend.position = "none", text = element_text(size=15))
  
#Normal Distribution Analysis
mounts <- volcano %>% select(year) %>% filter(year>1900) 

ggplot(mounts, aes(year)) +
  geom_histogram(aes(x=year, y=..density..), bins = 50, fill="#d3d3d3", color="black") +
  stat_function(fun=dnorm, args = list(mean=mean(mounts$year), sd=sd(mounts$year)), color="red") +
  geom_density(color="blue") +
  ggtitle("Histogram of Eruptions") +
  xlab("Year") +
  ylab("Density") +
  theme_bw()
  
#Normal distribution chart based on mountains elevation
elv_mounts <- volcano %>% select(elevation_m)
ggplot(elv_mounts, aes(elevation_m)) +
  geom_histogram(aes(x=elevation_m, y=..density..), bins = 50, fill="#d3d3d3", color="black") +
  stat_function(fun=dnorm, args = list(mean=mean(elv_mounts$elevation_m), sd=sd(elv_mounts$elevation_m)), color="red") +
  geom_density(color="blue") +
  ggtitle("Histogram of Eruptions") +
  xlab("Elevation (m)") +
  ylab("Density") +
  theme_bw()
  
#QQ Analysis
y <- quantile(mounts$year, c(0.25, 0.75))
x <- qnorm(c(0.25, 0.75))
slope <- diff(y)/diff(x)
int <- y[1] - slope * x[1]

ggplot(data=mounts, aes(sample=year))+
    stat_qq()+
    
y <- quantile(elv_mounts$elevation_m, c(0.25, 0.75))
x <- qnorm(c(0.25, 0.75))
slope <- diff(y)/diff(x)
int <- y[1] - slope * x[1]

ggplot(data=elv_mounts, aes(sample=elevation_m))+
    stat_qq()+
    geom_abline(intercept = int, slope = slope, color = "blue", size = 2, alpha = 0.5)
    
    
#Box Plot
p <- ggplot(volcano, aes(island, y=elevation_m))
p + geom_boxplot(fill = "white", colour="#3366FF")

#Elevation Corelation
fig(12, 7)
island_cols <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#999999", "#889EAF", "#D4B499")

elevation <- volcano
ggplot(data=elevation, aes(x+year, y+elevation_m))+
    geom_point(aes(color= island, fill=island, size=total_deaths), alpha=0.5, shape=21)+
    geom_smooth(aes(color="y ~ log(x)", fill="y ~ log (x)"), 
               method = 'loess', formula=y~log(x), se=FALSE, fullrange=TRUE)+
    scale_size_continuous(range= c(3, 10))+
    scale_color_manual(values = darken(island_cols, 0.5))+
    labs(title="Total Eruptions in the Pst 30 Years")
    
#Volcano Map
require("maps")
fig(20, 9)
island_cols <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#999999", "#889EAF", "#D4B499")
type_cols <-c("#6F69AC", "#95DAC1", "#FFEBA1", "#FD6F96")
mapdata <- elevation %>% dplyr:: group_by(island, type, name, longitude, latitude)%>%
    dplyr::summarise(sum_erpt = n(), m_vei = mean(vei), m_elev = max(elevation_m), m_deaths = max(total_deaths), .groups="drop")%>%
    select(island, type, name, m_vei, m_elev, longitude, latitude, m_deaths)

global <-map_data('world')

ggplot()+geom_polygon(data=global, aes(x=long, y=lat, group=group), fill="gray85", color="gray80")+
    coord_fixed(1.3)+
    xlim(94, 142) +
    ylim(-11,7.5)+
    geom_point(data=mapdata, aes(x= longitude, y=latitude, color= type, fill=type, size= m_deaths),
              aplha=0.8, show.legend= F, shape=24)+
    geom_text_repel(data= mapdata, aes(x= longitude, y=latitude, label=name),
                   color= "grey30",
                   size= 11/.pt,
                   point.padding=.6,
                   min.segment.lenght= 0,
                   maax.overlaps= 1000,
                   seed= 7654)+
    ggtitle("Indonesia Volcano Eruption Map") +
    scale_color_manual(values = darken(type_cols, 0.7))+
    scale_fill_manual(values= type_cols)+
    scale_size_continuous(range= c(3, 9))+
    guides(size= guide_legend("Total Deaths"))+
    guides(color= guide_legend("Type"))+
    theme_minimal_hgrid(12, rel_small= 1)+
    theme(legend.position = "bottom",
         legend.justification = "right",
         legend.text = element_text(size=9),
         legend.box.spacing = unit(3, "pt"))
