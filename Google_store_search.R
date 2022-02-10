library(gtrendsR)
library(usmap)
library(ggplot2)
library(tidyverse)



#identify the category id
categories[grepl(pattern = 'store',tolower(categories$name)),]

#a<-gtrends('store',geo = c("US-MI"),time = 'today 12-m',category = 73,low_search_volume = TRUE)
#a$related_topics
#a$related_queries

countries%>%
  filter(country_code=='US')%>%
  slice(2:52)->states

#store search in last 12 mos
out=lapply(states$sub_code,function(x){
  a=gtrends(keyword = 'store',geo = x,'today 12-m',category = 0)
  a$related_topics
})

as.data.frame(do.call(rbind,out))%>%
  filter(subject==100 & related_topics=='top')%>%
  distinct(geo,.keep_all = TRUE)%>%
  mutate(state=sub('.*-','',geo))->store_trend

store_trend

dc_map<-plot_usmap(data = store_trend,
                   values = 'value',labels = TRUE,include = 'DC') 
dc_map[[1]]


plot_usmap(data = store_trend,
           values = 'value',labels = FALSE,exclude = 'DC')+
  geom_polygon(aes(x=((20*(x-min(x))+1.2*min(x))),y=(20*(y-max(y))+max(y)),fill=value),
               data=dc_map[[1]],color='black')+
  geom_text(aes(x=1.3*min(x),y=1.4*max(y)),data=dc_map[[1]],label='DC',size=3)+
  scale_fill_brewer(name = " Past 12 Months Most Google Searched \n Topics Associated With \"Store\"",palette='Set1')+
  theme(panel.background=element_blank(),
        plot.title = element_text(size=20),
        legend.position = 'top',
        legend.title = element_text(size=10,face = "bold"),
        legend.text  = element_text(size=8,face = "bold"),
        legend.key.size = unit(6,'mm'))

