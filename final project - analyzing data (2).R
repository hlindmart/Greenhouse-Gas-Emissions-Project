# importing libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(plyr)
library(RColorBrewer)
library(gridExtra)
library(grid)
library(ggrepel)

# installing new package
install.packages("ggthemes")
library(ggthemes)

# importing datasets
ghg<-read.csv('/Users/hlind/Documents/Bootcamp/Module 10 - Final Project/ghgClean.csv')
ghgEcon<-read.csv('/Users/hlind/Documents/Bootcamp/Module 10 - Final Project/ghgEcon.csv')
ghgInv<-read.csv('/Users/hlind/Documents/Bootcamp/Module 10 - Final Project/ghgInv.csv')

# viewing data
View(ghg)
View(ghgEcon)
View(ghgInv)

# plotting total ghg emissions by country and year - wo LULUCF
ghgInv %>% 
  dplyr::group_by(Category,Year) %>% 
  dplyr::summarize(sumValue=sum(Value)) %>% 
  ggplot(aes(x=factor(Year),y=sumValue,group=Category)) + 
  geom_line(aes(color=Category),size=1.5) + 
  geom_point(size=1.,color='black') + 
  scale_color_brewer(name='',palette='Paired') + 
  theme(legend.position='right') + 
  theme_fivethirtyeight() + 
  theme(axis.text.x = element_text(size=8,angle=45),legend.text= element_text(size=10)) + 
  labs(title='GHG Emissions', subtitle= 'summed by all countries per year') + 
  guides(color=guide_legend(ncol=3))
## there was a significant drop in 2009 in ghg and co2 emissions wo LULUCF, why?

# plotting total ghg emissions by country and year - w LULUCF
ghg %>%
  dplyr::group_by(Country,Year) %>% 
  dplyr::summarize(sumValue=sum(Total.GHG.Emissions.w.LULUCF)) %>% 
  ggplot(aes(x=factor(Year),y=sumValue,group=Country)) + 
  geom_line(aes(color=Country),size=1.5) + 
  geom_point(size=1.,color='black') + 
  theme(axis.text.x = element_text(size=8,angle=45),legend.text= element_text(size=10)) +
  labs(title='GHG Emissions with LULUCF', subtitle= 'summed by all countries per year') + 
  guides(color=guide_legend(ncol=3))
## even though there was a drop in 2009, china shows an increase that continues to an uptick through the years

# comparing total co2 w and wo LULUCF
ggplot(ghg, aes(x=Year, y=Total.CO2.w.LULUCF)) +
  geom_line(aes(color='CO2 w LULUCF')) +
  geom_point(aes(color='CO2 w LULUCF')) +
  geom_line(aes(x=Year, y=Total.CO2.wo.LULUCF, color='CO2 wo LULUCF')) +
  geom_point(aes(x=Year, y=Total.CO2.wo.LULUCF, color='CO2 wo LULUCF')) +
  labs(title="Total CO2 Emissions", subtitle="with and without LULUCF") +
  xlab("Year") + ylab("CO2 emissions") +
  theme(plot.title=element_text(face="bold", hjust= 0.5, size= 16)) +
  theme(plot.subtitle=element_text(face="italic", hjust = 0.5, size = 10))
## emissions seem higher wo LULUCF - emissions w LULUCF see a rise in 2006

# getting column names
colnames(ghg)

# comparing total methane w and wo LULUCF
ggplot(ghg, aes(x=Year, y=Total.Methane.w.LULUCF)) +
  geom_line(aes(color='Methane w LULUCF')) +
  geom_point(aes(color='Methane w LULUCF')) +
  geom_line(aes(x=Year, y=Total.Methane..MtCO2e., color='Methane wo LULUCF')) +
  geom_point(aes(x=Year, y=Total.Methane..MtCO2e., color='Methane wo LULUCF')) +
  labs(title="Total Methane Emissions", subtitle="with and without LULUCF") +
  xlab("Year") + ylab("Methane emissions") +
  theme(plot.title=element_text(face="bold", hjust= 0.5, size= 16)) +
  theme(plot.subtitle=element_text(face="italic", hjust = 0.5, size = 10))
## methane emissions seem equal w and wo LULUCF and in some years methane emissions w LULUCF are slightly higher than wo

# comparing total nitrous oxide w and wo LULUCF
ggplot(ghg, aes(x=Year, y=Total.Nitrous.Oxide.w.LULUCF)) +
  geom_line(aes(color='N2O w LULUCF')) +
  geom_point(aes(color='N2O w LULUCF')) +
  geom_line(aes(x=Year, y=Total.Nitrous.Oxide..MtCO2e., color='N2O wo LULUCF')) +
  geom_point(aes(x=Year, y=Total.Nitrous.Oxide..MtCO2e., color='N2O wo LULUCF')) +
  labs(title="Total Nitrous Oxide Emissions", subtitle="with and without LULUCF") +
  xlab("Year") + ylab("N2O emissions") +
  theme(plot.title=element_text(face="bold", hjust= 0.5, size= 16)) +
  theme(plot.subtitle=element_text(face="italic", hjust = 0.5, size = 10))
## n2o emissions seem equal w and wo LULUCF - 2009 saw the highest out of all the years, why?

# comparing mean v median for year 2000 - CO2 emissions
## dropping EU rows in order to condense data
ghgInv1<-ghgInv[!(ghgInv$Country=="European Union"),]

coWO<-ghgInv1%>% 
  filter(Category=='CO2 emissions wo LULUCF') %>% 
  group_by(Year) %>% mutate(meanVal=mean(Value), medianVal=median(Value))

# creating histogram
coHist<-coWO %>% 
  filter(Year==2000) %>% 
  ggplot(aes(x=Value)) + 
  geom_histogram(bins=100) + 
  geom_vline(xintercept=coWO$meanVal,color="#377EB8",linewidth=2) + 
  geom_vline(xintercept=coWO$medianVal,color="#E41A1C",linewidth=2) + 
  theme_fivethirtyeight() + theme(legend.position='None') + 
  theme(legend.text= element_text(size=8)) + 
  labs(title= 'Mean v Median',subtitle ='of CO2 emissions wo LULUCF, Year 2000, all Countries')

# creating line plot
coLine<-ghgInv1 %>% 
  filter(Category=='CO2 emissions wo LULUCF') %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarize(medianValue=median(Value),meanValue=mean(Value)) %>% 
  reshape2::melt(id=c('Year')) %>% 
  ggplot(aes(x=factor(Year),y=value,color=variable,group=variable)) + 
  geom_line(linewidth=1.5,alpha=.85) + 
  geom_point(size=1.,color='black') + 
  theme_fivethirtyeight() +
  theme(axis.text.x=element_text(size=8,angle=45),axis.text.y=element_text(size=8)) + 
  ylim(0,400000)

coHist + annotation_custom(grob=ggplotGrob(coLine), xmin=1.5e6, xmax=5.5e6, ymin=2.5, ymax=12)
## are there outliers?
### the mean decreases over time, but the median is still somewhat same

# comparing mean v median for year 2009 - CO2 emissions
# creating histogram
coHist9<-coWO %>% 
  filter(Year==2009) %>% 
  ggplot(aes(x=Value)) + 
  geom_histogram(bins=100) + 
  geom_vline(xintercept=coWO$meanVal,color="#FF006E",linewidth=2) + 
  geom_vline(xintercept=coWO$medianVal,color="#3A86FF",linewidth=2) + 
  theme_fivethirtyeight() + theme(legend.position='None') + 
  theme(legend.text= element_text(size=8)) + 
  labs(title= 'Mean v Median',subtitle ='of CO2 emissions wo LULUCF, Year 2009, all Countries')

# creating line plot
coLine9<-ghgInv1 %>% 
  filter(Category=='CO2 emissions wo LULUCF') %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarize(medianValue=median(Value),meanValue=mean(Value)) %>% 
  reshape2::melt(id=c('Year')) %>% 
  ggplot(aes(x=factor(Year),y=value,color=variable,group=variable)) + 
  geom_line(linewidth=1.5,alpha=.85) + 
  geom_point(size=1.,color='black') + 
  theme_fivethirtyeight() +
  theme(axis.text.x=element_text(size=8,angle=45),axis.text.y=element_text(size=8)) + 
  ylim(0,400000)

coHist9 + annotation_custom(grob=ggplotGrob(coLine9), xmin=1.5e6, xmax=5.5e6, ymin=2.5, ymax=12)
## there are outliers!
### median stays somewhat the same - will it be for all years?

# checking median for ALL years (2000 - 2014)
ghgInv1 %>% 
  dplyr::group_by(Year,Category) %>% 
  dplyr::summarize(medianValue=median(Value)) %>% 
  ggplot(aes(x=factor(Year), y=medianValue, color=Category, group=Category)) + 
  geom_line(linewidth=1.5,alpha=.85) + geom_point(size=1., color='black') + 
  theme_fivethirtyeight() + 
  guides(color=guide_legend(ncol=3)) + 
  ggtitle(label='Worldwide Greenhouse Gas Emissions', subtitle='in kilotons CO2 equivalent. Value is median taken over countries per year.') +
  theme(text=element_text(size=16, family="Arial"))
## top 3 contributors seem to be co2 emissions, ghg emissions including co2, and ghg emissions
### co2 emissions had a significant drop in 2005, 2009 and from 2012 it's continued to decrease exponentially
#### ghg emissions including co2 are the highest numbers constant
  
# installing and forcing packages
install.packages("extrafont")
library(extrafont)
extrafont::font_import()
require(extrafont)
require(ggplot2)

# checking population
ghgEcon %>%
  dplyr::group_by(Country,Year) %>% 
  dplyr::summarize(sumValue=sum(Population)) %>% 
  ggplot(aes(x=factor(Year), y=sumValue,group=Country)) + 
  geom_line(aes(color=Country), linewidth=1.5) + 
  geom_point(size=1.,color='black') + 
  theme(axis.text.x = element_text(size=8,angle=45), legend.text=element_text(size=10)) +
  labs(title='Population', subtitle= 'summed by all countries per year') + 
  guides(color=guide_legend(ncol=3))
## US, China, and Japan are the countries w highest population and even though 2009 ghg emissions dropped, population increased
### are these 3 countries the top contributors of gases as well?

View(ghgInv1)

# global contributions of co2 emissions wo LULUCF
ghgInv1 %>% 
  filter(Category=='CO2 emissions wo LULUCF') %>% 
  dplyr::group_by(Year, Country) %>% 
  ggplot(aes(x=factor(Year), y=Value)) + 
  geom_histogram(aes(fill=Country), stat='identity', position="fill", size=1.5) + 
  theme_fivethirtyeight() + theme(text=element_text(family="Arial Narrow", size=10)) +
  theme(legend.position='bottom', legend.text=element_text(size=10)) + 
  ggtitle(label='CO2 emissions wo LULUCF', subtitle='contribution by country and year.') + 
  guides(fill=guide_legend(ncol=13)) +
  scale_fill_manual(name="",values=colorRampPalette(brewer.pal(9,'Paired'))(42))
## US, Russia, and Japan are the top 3 countries with largest impact of co2 emissions
### US has the biggest contributions of these emissions even with being a smaller country than Russia

# checking US data by itself
ghgInv1 %>% 
  filter(Country=='United States of America') %>% 
  dplyr::group_by(Category, Year) %>% 
  ggplot(aes(x=factor(Year), y=Value, group=Category)) + 
  geom_line(aes(color=Category), linewidth=1.5) + geom_point(size=1.,color='black') + 
  scale_color_brewer(name='',palette='Paired') + theme(legend.position='right') + 
  theme_fivethirtyeight() + theme(legend.position='bottom', legend.text=element_text(size=10)) +
  theme(text=element_text(family="Arial Narrow", size=12)) +
  ggtitle(label='USA GHG Emissions', subtitle='in kilotons CO2 equivalent') + guides(color=guide_legend(ncol=3))
## there's an increase of ghg emissions before they drastically dropped (recession?) and have seen an uptick since

colnames(ghgEcon)

# checking US population data by itself
ghgEcon %>%
  filter(Country=='United States') %>%
  dplyr::group_by(Country, Year) %>% 
  ggplot(aes(x=factor(Year), y=Population)) + 
  geom_line(aes(color=Year), linewidth=1.5, show.legend= FALSE) + 
  geom_point(size=1.,color='black') +
  theme(axis.text.x = element_text(size=8,angle=45)) +
  labs(title='USA Population', subtitle='by year.') + 
  guides(color=guide_legend(ncol=3))
## population was increasing just as much as ghg emissions were, but in the years where they dropped, population continued to grow