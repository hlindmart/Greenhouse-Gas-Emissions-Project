# importing libraries
library(ggplot2)
library(dplyr)
library(tidyr)

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