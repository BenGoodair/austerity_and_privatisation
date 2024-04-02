####Start####
####load libraries####
if (!require("pacman")) install.packages("pacman")

pacman::p_load(devtools, dplyr, tidyverse, tidyr, stringr,  curl, RColorBrewer, ggforce, ggarchery, car)

####data####

df <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/austerity_and_privatisation/main/Data/main_data.csv"))
benefits_plot <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/austerity_and_privatisation/main/Data/socialsecurity_data.csv"))
LA_allocation <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/austerity_and_privatisation/main/Data/LA_data.csv"))

####ANALYSIS####

###dag creation####


dag <- ggplot()+
  geom_arrowsegment(aes(x = 0, xend = 25, y = 100, yend = 75), arrow_positions = 0.5) + 
  geom_arrowsegment(aes(x = 0, xend = 100, y = 100, yend = 50), arrow_positions = 0.5) + 
  geom_arrowsegment(aes(x = 0, xend = 25, y = 0, yend = 25), arrow_positions = 0.5) + 
  geom_arrowsegment(aes(x = 0, xend = 100, y = 0, yend = 50), arrow_positions = 0.5) + 
  geom_arrowsegment(aes(x = 25, xend = 50, y = 25, yend = 50), arrow_positions = 0.5) + 
  geom_arrowsegment(aes(x = 25, xend = 50, y = 75, yend = 50), arrow_positions = 0.5) + 
  geom_arrowsegment(aes(x = 50, xend = 100, y = 50, yend = 50), arrow_positions = 0.5) +
  geom_circle(aes(x0 = 0, y0 = 100, r = 10),
              inherit.aes = FALSE, fill="white")+
  geom_circle(aes(x0 = 0, y0 = 0, r = 10),
              inherit.aes = FALSE, fill="white")+
  geom_circle(aes(x0 = 25, y0 = 25, r = 9),
              inherit.aes = FALSE, fill="white", colour = "white")+
  geom_circle(aes(x0 = 25, y0 = 75, r = 11),
              inherit.aes = FALSE, fill="white", colour = "white")+
  geom_circle(aes(x0 = 50, y0 = 50, r = 10),
              inherit.aes = FALSE, fill="white")+
  geom_circle(aes(x0 = 100, y0 = 50, r = 10),
              inherit.aes = FALSE, fill="white")+
  annotate("text", label = "Austerity",
           x = -15, y = 50, size = 9.5, colour = "black", angle = 90)+
  annotate("text", label = "Restricted\nNHS budget",
           x = 0, y = 100, size = 4.8, colour = "black")+
  annotate("text", label = "Cuts to social\nservices and\nbenefits",
           x = 0, y = 0, size = 4.8, colour = "black")+
  annotate("text", label = "Privatisation",
           x = 50, y = 50, size = 4.8, colour = "black")+
  annotate("text", label = "Treatable\nMortality",
           x = 100, y = 50, size = 4.8, colour = "black")+
  annotate("text", label = "Pressure for\ncheap services",
           x = 25, y = 75, size = 4.8, colour = "black", angle=-45)+
  annotate("text", label = "Greater need",
           x = 25, y = 25, size = 4.8, colour = "black", angle=45)+
  theme_void()+
  # coord_cartesian(ylim=c(4, 22), xlim=c(-100,30))+
  coord_fixed()+
  theme(panel.background = element_rect(fill = "white"))
#ggsave("plots/paper_3/dagtest.png", plot=dag, width=10, height=10, dpi=600)



####three dags together####

dag1 <- ggplot()+
  geom_arrowsegment(aes(x = 0, xend = 25, y = 100, yend = 75), arrow_positions = 0.5) + 
  geom_arrowsegment(aes(x = 0, xend = 100, y = 100, yend = 50), arrow_positions = 0.5) + 
  geom_arrowsegment(aes(x = 0, xend = 25, y = 0, yend = 25), arrow_positions = 0.5) + 
  geom_arrowsegment(aes(x = 0, xend = 100, y = 0, yend = 50), arrow_positions = 0.5) + 
  geom_arrowsegment(aes(x = 25, xend = 50, y = 25, yend = 50), arrow_positions = 0.5) + 
  geom_arrowsegment(aes(x = 25, xend = 50, y = 75, yend = 50), arrow_positions = 0.5) + 
  geom_arrowsegment(aes(x = 50, xend = 100, y = 50, yend = 50), arrow_positions = 0.5) +
  geom_circle(aes(x0 = 0, y0 = 100, r = 12),
              inherit.aes = FALSE, fill="white")+
  geom_circle(aes(x0 = 0, y0 = 0, r = 12),
              inherit.aes = FALSE, fill="white")+
  geom_circle(aes(x0 = 25, y0 = 25, r = 9),
              inherit.aes = FALSE, fill="white", colour = "white")+
  geom_circle(aes(x0 = 25, y0 = 75, r = 11),
              inherit.aes = FALSE, fill="white", colour = "white")+
  geom_circle(aes(x0 = 50, y0 = 50, r = 12),
              inherit.aes = FALSE, fill="white")+
  geom_circle(aes(x0 = 100, y0 = 50, r = 12),
              inherit.aes = FALSE, fill="white")+
  annotate("text", label = "Austerity",
           x = -15, y = 50, size = 9.5, colour = "black", angle = 90)+
  annotate("text", label = "Restricted\nNHS budget",
           x = 0, y = 100, size = 4.8, colour = "black")+
  annotate("text", label = "Cuts to\nsocial services\nand\nbenefit payments",
           x = 0, y = 0, size = 4.8, colour = "black")+
  annotate("text", label = "NHS\nPrivatisation",
           x = 50, y = 50, size = 4.8, colour = "black")+
  annotate("text", label = "Treatable\nMortality",
           x = 100, y = 50, size = 4.8, colour = "black")+
  annotate("text", label = "Pressure for\ncheap services",
           x = 25, y = 75, size = 4.8, colour = "black", angle=-45)+
  annotate("text", label = "Greater need",
           x = 25, y = 25, size = 4.8, colour = "black", angle=45)+
  theme_void()+
  # coord_cartesian(ylim=c(4, 22), xlim=c(-100,30))+
  coord_fixed()+
  theme(panel.background = element_rect(fill = "white"))+
  labs(title="Scenario 1")+ theme(plot.title = element_text(size=22))

dag2 <- ggplot()+
  geom_arrowsegment(aes(x = 0, xend = 25, y = 100, yend = 75), arrow_positions = 0.5) + 
  geom_arrowsegment(aes(x = 0, xend = 100, y = 100, yend = 50), arrow_positions = 0.5) + 
  geom_arrowsegment(aes(x = 0, xend = 25, y = 0, yend = 25), arrow_positions = 0.5) + 
  geom_arrowsegment(aes(x = 0, xend = 100, y = 0, yend = 50), arrow_positions = 0.5) + 
  geom_arrowsegment(aes(x = 25, xend = 50, y = 25, yend = 50), arrow_positions = 0.5) + 
  geom_arrowsegment(aes(x = 25, xend = 50, y = 75, yend = 50), arrow_positions = 0.5) + 
  geom_circle(aes(x0 = 0, y0 = 100, r = 12),
              inherit.aes = FALSE, fill="white")+
  geom_circle(aes(x0 = 0, y0 = 0, r = 12),
              inherit.aes = FALSE, fill="white")+
  geom_circle(aes(x0 = 25, y0 = 25, r = 9),
              inherit.aes = FALSE, fill="white", colour = "white")+
  geom_circle(aes(x0 = 25, y0 = 75, r = 11),
              inherit.aes = FALSE, fill="white", colour = "white")+
  geom_circle(aes(x0 = 50, y0 = 50, r = 12),
              inherit.aes = FALSE, fill="white")+
  geom_circle(aes(x0 = 100, y0 = 50, r = 12),
              inherit.aes = FALSE, fill="white")+
  annotate("text", label = "Austerity",
           x = -15, y = 50, size = 9.5, colour = "black", angle = 90)+
  annotate("text", label = "Restricted\nNHS budget",
           x = 0, y = 100, size = 4.8, colour = "black")+
  annotate("text", label = "Cuts to\nsocial services\nand\nbenefit payments",
           x = 0, y = 0, size = 4.8, colour = "black")+
  annotate("text", label = "NHS\nPrivatisation",
           x = 50, y = 50, size = 4.8, colour = "black")+
  annotate("text", label = "Treatable\nMortality",
           x = 100, y = 50, size = 4.8, colour = "black")+
  annotate("text", label = "Pressure for\ncheap services",
           x = 25, y = 75, size = 4.8, colour = "black", angle=-45)+
  annotate("text", label = "Greater need",
           x = 25, y = 25, size = 4.8, colour = "black", angle=45)+
  theme_void()+
  # coord_cartesian(ylim=c(4, 22), xlim=c(-100,30))+
  coord_fixed()+
  theme(panel.background = element_rect(fill = "white"))+
  labs(title="Scenario 2")+ theme(plot.title = element_text(size=22))

dag3 <- ggplot()+
  geom_arrowsegment(aes(x = 0, xend = 100, y = 100, yend = 50), arrow_positions = 0.5) + 
  geom_arrowsegment(aes(x = 0, xend = 100, y = 0, yend = 50), arrow_positions = 0.5) + 
  geom_arrowsegment(aes(x = 0, xend = 100, y = 50, yend = 50), arrow_positions = 0.5) +
  geom_circle(aes(x0 = 0, y0 = 100, r = 12),
              inherit.aes = FALSE, fill="white")+
  geom_circle(aes(x0 = 0, y0 = 0, r = 12),
              inherit.aes = FALSE, fill="white")+
  geom_circle(aes(x0 = 0, y0 = 50, r = 12),
              inherit.aes = FALSE, fill="white")+
  geom_circle(aes(x0 = 100, y0 = 50, r = 12),
              inherit.aes = FALSE, fill="white")+
  annotate("text", label = "Conservative State Retrenchment",
           x = -15, y = 50, size = 8.5, colour = "black", angle = 90)+
  annotate("text", label = "Restricted\nNHS budget",
           x = 0, y = 100, size = 4.8, colour = "black")+
  annotate("text", label = "Cuts to\nsocial services\nand\nbenefit payments",
           x = 0, y = 0, size = 4.8, colour = "black")+
  annotate("text", label = "NHS\nPrivatisation",
           x = 0, y = 50, size = 4.8, colour = "black")+
  annotate("text", label = "Treatable\nMortality",
           x = 100, y = 50, size = 4.8, colour = "black")+
  theme_void()+
  # coord_cartesian(ylim=c(4, 22), xlim=c(-100,30))+
  coord_fixed()+
  theme(panel.background = element_rect(fill = "white"))+
  labs(title="Scenario 3")+ theme(plot.title = element_text(size=22))

bothdags <- cowplot::plot_grid(dag1, dag2, ncol=2)
bottomdags <- cowplot::plot_grid(NULL, dag3, NULL, ncol=3, rel_widths = c(0.25,0.5,0.25))
dags <- cowplot::plot_grid(bothdags, bottomdags, ncol=1)
ggsave("C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/austerity_and_privatisation/Figures/dags_all.jpeg", plot=dags, width=20, height=20, dpi=300)



####plot 1####

lm2 <- (lm(deflated_per_person_allocation~as.double(year), data= df[which(df$year<2011),]))
lm3 <- (lm(deflated_per_person_allocation~as.double(year), data= df[which(df$year>2009&df$year<2014),]))

val <- mean(df[which(df$year==2013),]$deflated_per_person_allocation, na.rm=T)-mean(df[which(df$year==2014),]$deflated_per_person_allocation, na.rm=T)+lm3$coefficients[2]

plot1 <- ggplot(df[df$year<2014,])+
  stat_summary(data=df[which(df$year<2014),],aes(x=factor(year), y=deflated_per_person_allocation))+
  theme_bw()+
  labs(x="Year", y="NHS allocation pre-2013 reforms\n(£,000s per person)", title = "Allocation",
       caption = "Prices adjusted to 2020 values using GDP deflator")+
  geom_smooth(data =df[which(df$year<2011),],aes(group=1,x=factor(year), y=deflated_per_person_allocation), colour="red",fill="lightsalmon", method = "lm"  )+
  geom_smooth(data =df[which(df$year>2009&df$year<2014),],aes(group=1, x=factor(year), y=deflated_per_person_allocation), colour="blue", fill="mediumpurple1", method = "lm"  )+
  #geom_smooth(data =df[which(df$year>2013),],aes(group=1, x=factor(year), y=deflated_per_person_allocation+val), colour="blue",fill="mediumpurple1", method = "lm")+
  geom_vline(xintercept="2010", linetype="dotted")+
  geom_segment(colour="red",linetype="dashed",aes(x = 7, y = lm2$coefficients[1]+(lm2$coefficients[2]*2010), xend = 10, yend = lm2$coefficients[1]+lm2$coefficients[2]*2013))+
  coord_cartesian(ylim=c(1.2, 2.5))+
  annotate("text", label = "Austerity policies implemented",
           x = 6.7, y = 1.5, size = 2.5, colour = "black", angle=90)+
  annotate("text", label = "New Labour Governments",
           x = 2.5, y = 1.52, size = 2.5, colour = "red", angle=35)+
  annotate("text", label = "Con-LD Government",
           x = 8.5, y = 1.85, size = 2.5, colour = "blue", angle=19)

lag <- df[which(df$year<2011),]
lag <- lag %>%
  group_by(CCG_Name) %>%
  summarise(avg_percent_change = mean((diff(deflated_per_person_allocation, na.pad = TRUE) / lag(deflated_per_person_allocation, default = NA)) , na.rm = TRUE))

mean(lag$avg_percent_change, na.rm=T)


plot2 <- ggplot(df[df$year>2013&df$year<2021,])+
  stat_summary(data=df[which(df$year>2013&df$year<2021),],aes(x=factor(year), y=deflated_per_person_allocation))+
  geom_segment(colour="red",linetype="dashed",aes(x = 1, y = mean(df[df$year==2014,]$deflated_per_person_allocation, na.rm=T), xend = 7, yend = mean(df[df$year==2014,]$deflated_per_person_allocation, na.rm=T)+(mean(df[df$year==2014,]$deflated_per_person_allocation, na.rm=T)*(mean(lag$avg_percent_change, na.rm=T))*6)))+
  theme_bw()+
  labs(x="Year", y="NHS allocation post-2013 reforms\n(£,000s per person)", title = "Allocation",
       caption = "Prices adjusted to 2020 values using GDP deflator")+
  geom_smooth(data =df[which(df$year>2013&df$year<2021),],aes(group=1,x=factor(year), y=deflated_per_person_allocation), colour="blue",fill="mediumpurple1", method = "lm"  )+
  coord_cartesian(ylim=c(1, 2))+
  annotate("text", label = "Rate of growth under New Labour",
           x = 3.5, y = 1.54, size = 2.5, colour = "red", angle=30)

plot3 <- ggplot(df[df$year>2013&df$year<2021,])+
  stat_summary(data=df[which(df$year>2013&df$year<2021),],aes(x=factor(year), y=balance))+
  theme_bw()+
  labs(x="Year", y="CCG's Account Balance\n(£,000s)", title = "Balance",
       caption = "Prices adjusted to 2020 values using GDP deflator")+
  geom_smooth(data =df[which(df$year>2013&df$year<2021),],aes(group=1,x=factor(year), y=balance), colour="blue",fill="mediumpurple1", method = "lm"  )
  

once <- cowplot::plot_grid(plot1, plot2, ncol=2 ,labels=c("A", "B"))
twice <- cowplot::plot_grid(NULL, plot3, NULL ,ncol=3, rel_widths = c(0.4, 1.2, 0.4) ,labels=c("", "C", ""))
all <- cowplot::plot_grid(once, twice, ncol=1)


ggsave("C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/austerity_and_privatisation/Figures/CCG_Austerity_plot_rev.jpeg", plot=all, width=12, height=12, dpi=300)


####plot 2####

dfchange <- df[c("CCG_Code","CCG_Name", "deflated_per_person_allocation","deflated_per_person_allocation_la","deflated_per_person_benefits", "Private_Sector_Procurement_Spend", "year")]

dfchange <- dfchange[dfchange$year>2013,]

lagged <- dfchange[c("CCG_Name", "deflated_per_person_allocation", "deflated_per_person_allocation_la", "deflated_per_person_benefits", "Private_Sector_Procurement_Spend", "year")]
lagged$year <- as.character(as.double(as.character(lagged$year))+1)

names(lagged)[names(lagged)=="Private_Sector_Procurement_Spend"] <- "lagged_profit"
names(lagged)[names(lagged)=="deflated_per_person_allocation"] <- "lagged_ccg"
names(lagged)[names(lagged)=="deflated_per_person_allocation_la"] <- "lagged_la"
names(lagged)[names(lagged)=="deflated_per_person_benefits"] <- "lagged_bens"

dfchange <- merge(dfchange, lagged, by=c("CCG_Name", "year"),all.x=T)

dfchange$profitchange <- dfchange$Private_Sector_Procurement_Spend-dfchange$lagged_profit
dfchange$ccgchange <- dfchange$deflated_per_person_allocation-dfchange$lagged_ccg
dfchange$lachange <- dfchange$deflated_per_person_allocation_la-dfchange$lagged_la
dfchange$benschange <- dfchange$deflated_per_person_benefits-dfchange$lagged_bens

dfchange <- dfchange %>%dplyr::group_by(CCG_Code, CCG_Name)%>%
  dplyr::summarise(profitchange = mean(profitchange, na.rm = TRUE),
                   ccgchange = mean(ccgchange, na.rm = TRUE),
                   lachange = mean(lachange, na.rm = TRUE),
                   benschange = mean(benschange, na.rm = TRUE))






a <- ggplot(dfchange[dfchange$profitchange>-10&dfchange$profitchange<100,], aes(x=profitchange, y=ccgchange))+
  geom_point(aes( alpha=0.3))+
  theme_bw()+
  labs(title = "NHS Allocation" ,x="", y="Change in CCG Allocation\n(£000s, per capita)")+
  stat_smooth(method="lm")+
  geom_vline(xintercept = 0, linetype="dashed")+
  geom_hline(yintercept = 0, linetype="dashed")+
  theme(axis.text.x =element_blank(), legend.position = "none")

b <- ggplot(dfchange[dfchange$profitchange>-10&dfchange$profitchange<100,], aes(x=profitchange, y=benschange))+
  geom_point(aes( alpha=0.3))+
  theme_bw()+
  labs(title = "Benefit Allocation" ,x="Change in for-profit outsourcing\n(% points)", 
       y="Change in Benefit Allocation\n(£000s, per capita)",
       caption = "Changes in allocations are reported in annual average changes to per capita £s\nChanges in for-profit outsourcing reported in average annual % point changes\nAll reported for period 2013-2020")+
  stat_smooth(method="lm")+
  geom_vline(xintercept = 0, linetype="dashed")+
  geom_hline(yintercept = 0, linetype="dashed")+
  theme( legend.position = "none")

c <- ggplot(dfchange[dfchange$profitchange>-10&dfchange$profitchange<100,], aes(x=profitchange, y=lachange))+
  geom_point(aes( alpha=0.3))+
  theme_bw()+
  labs(title = "LA Allocation" ,x="", y="Change in LA Allocation\n(£000, per capitas)")+
  stat_smooth(method="lm")+
  geom_vline(xintercept = 0, linetype="dashed")+
  geom_hline(yintercept = 0, linetype="dashed")+
  theme(axis.text.x =element_blank(), legend.position = "none")


fig2 <- cowplot::plot_grid(a,c,b,ncol=1, rel_heights = c(0.3,0.3,0.4))
#fig1 <- cowplot::plot_grid(fig1, x, ncol=2, rel_widths = c(0.8,0.2))

ggsave("C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/austerity_and_privatisation/Figures/figure_2.jpeg", plot=fig2, width=10, height=12, dpi=300)

####Regression Table 1####

pdf <- df %>% dplyr::select(deflated_per_person_benefits,
                            deflated_per_person_allocation,
                            deflated_per_person_allocation_la,
                            Private_Sector_Procurement_Spend,
                            CCG_Name,
                            CCGpop,
                            total_spend_10millions,
                            year,
                            preventable_mortality_rate, 
                            treatable_mortality_rate)%>%
  dplyr::distinct()%>%
  dplyr::filter(!is.na(deflated_per_person_benefits),
                !is.na(Private_Sector_Procurement_Spend),
                !is.na(CCG_Name),
                !is.na(year),
                !is.na(treatable_mortality_rate),
                !is.na(preventable_mortality_rate),
                Private_Sector_Procurement_Spend>=0)
pdf <- plm::pdata.frame(unique(pdf),index=c("CCG_Name", "year") )
#pdf <- pdf[complete.cases(pdf),]

one <-plm(Private_Sector_Procurement_Spend~lag(log(deflated_per_person_allocation))+CCGpop+total_spend_10millions, data=pdf,method="within", effect = "twoways")
two <-plm(Private_Sector_Procurement_Spend~lag(log(deflated_per_person_allocation_la))+CCGpop+total_spend_10millions, data=pdf,method="within", effect = "twoways")
three <-plm(Private_Sector_Procurement_Spend~lag(log(deflated_per_person_benefits))+CCGpop+total_spend_10millions, data=pdf,method="within", effect = "twoways")


onesum <- as.list(modelsummary(one, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
twosum <- as.list(modelsummary(two, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
threesum <- as.list(modelsummary(three, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))

#FEmisssum <- as.list(modelsummary(FEmiss, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
#FEmissconsum <- as.list(modelsummary(FEmisscon, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))


onesum$tidy$p.value <- coef_test(one, vcov = "CR2", cluster = df$CCG_Name, test = "Satterthwaite")$p
onesum$tidy$std.error <- coef_test(one, vcov = "CR2", cluster = df$CCG_Name, test = "Satterthwaite")$SE
onesum$tidy$conf.low <- onesum$tidy$estimate-(1.96*coef_test(one, vcov = "CR2", cluster = df$CCG_Name, test = "Satterthwaite")$SE)
onesum$tidy$conf.high <- onesum$tidy$estimate+(1.96*coef_test(one, vcov = "CR2", cluster = df$CCG_Name, test = "Satterthwaite")$SE)
onesum$tidy$estimate <- onesum$tidy$estimate

twosum$tidy$p.value <- coef_test(two, vcov = "CR2", cluster = df$CCG_Name, test = "Satterthwaite")$p
twosum$tidy$std.error <- coef_test(two, vcov = "CR2", cluster = df$CCG_Name, test = "Satterthwaite")$SE
twosum$tidy$conf.low <- twosum$tidy$estimate-(1.96*coef_test(two, vcov = "CR2", cluster = df$CCG_Name, test = "Satterthwaite")$SE)
twosum$tidy$conf.high <- twosum$tidy$estimate+(1.96*coef_test(two, vcov = "CR2", cluster = df$CCG_Name, test = "Satterthwaite")$SE)
twosum$tidy$estimate <- twosum$tidy$estimate


threesum$tidy$p.value <- coef_test(three, vcov = "CR2", cluster = df$CCG_Name, test = "Satterthwaite")$p
threesum$tidy$std.error <- coef_test(three, vcov = "CR2", cluster = df$CCG_Name, test = "Satterthwaite")$SE
threesum$tidy$conf.low <- threesum$tidy$estimate-(1.96*coef_test(three, vcov = "CR2", cluster = df$CCG_Name, test = "Satterthwaite")$SE)
threesum$tidy$conf.high <- threesum$tidy$estimate+(1.96*coef_test(three, vcov = "CR2", cluster = df$CCG_Name, test = "Satterthwaite")$SE)
threesum$tidy$estimate <- threesum$tidy$estimate


cm <- c("lag(log(deflated_per_person_allocation))" = "CCG Allocation (£000s per capita)",
        "lag(log(deflated_per_person_allocation_la))" = "LA Allocation (£000s per capita)",
        "lag(log(deflated_per_person_benefits))" = "Benefit Allocation (£000s per capita)" )

rows <- tribble(~term,          ~`For-profit Outsourcing (%) [.95 ci]`,  ~`p-value`,~`For-profit Outsourcing (%) [.95 ci]`,  ~`p-value`,  ~`For-profit Outsourcing (%) [.95 ci]`,  ~`p-value`,
                'CCG Fixed Effects', 'Yes',  'Yes', 'Yes',  'Yes','Yes',  'Yes',
                'Time Fixed Effects','Yes','Yes','Yes',  'Yes','Yes','Yes',
                'Clustered Standard Errors', 'Yes','Yes', 'Yes',  'Yes','Yes','Yes',
                'Control variables', 'Yes','Yes', 'Yes',  'Yes','Yes','Yes')


table <- modelsummary(list("For-profit Outsourcing (%) [.95 ci]"=onesum,"p-value"=onesum,"For-profit Outsourcing (%) [.95 ci]"=twosum,"p-value"=twosum,"For-profit Outsourcing (%) [.95 ci]"=threesum,"p-value"=threesum),
                      coef_omit = "Intercept|dept|year", add_rows = rows, coef_map = cm,
                      fmt = 4, estimate = c("{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value"), statistic = NULL,
                      notes = list('Table reports results from multivariate longitudinal regression models.',
                                   'Robust SEs are clustered at CCG level and use a bias-reduced linearization estimator (CR2)',
                                   'Lag of one year applied to allocation variables which are also log transformed'),
                      output = "gt") 
#add_header_above(c(" ", "Fixed Effects" = 2, "First Differences" = 2, "Covariate Balancing (1)" = 2, "Covariate Balancing (2)" = 2, "Multi-Level Model" = 2))

#gtsave(table, "Regressions/paper_3/FE_privatisation_logs.html")

####Confounding####

pdf <- df %>% dplyr::select(deflated_per_person_benefits,
                            Treatable_Mortality_Rate,
                            GDHI_per_person,
                            Private_Sector_Procurement_Spend,
                            deflated_per_person_allocation,
                            deflated_per_person_allocation_la,
                            CCG_Name,
                            CCGpop,
                            total_spend_10millions,
                            year,
                            preventable_mortality_rate, 
                            treatable_mortality_rate)%>%
  dplyr::distinct()%>%
  dplyr::filter(!is.na(deflated_per_person_benefits),
                !is.na(Treatable_Mortality_Rate),
                !is.na(Private_Sector_Procurement_Spend),
                !is.na(CCG_Name),
                !is.na(year),
                !is.na(treatable_mortality_rate),
                !is.na(GDHI_per_person),
                Private_Sector_Procurement_Spend>=0)
pdf <- plm::pdata.frame(unique(pdf),index=c("CCG_Name", "year") )
#pdf <- pdf[complete.cases(pdf),]
dfint <- pdf[which(pdf$year!=2013),]

one <-plm(log(Treatable_Mortality_Rate)~lag(Private_Sector_Procurement_Spend)+total_spend_10millions+GDHI_per_person+CCGpop, data=dfint, index=c("CCG_Name", "year") ,method="within", effect = "twoways")
two <- plm(log(Treatable_Mortality_Rate)~lag(Private_Sector_Procurement_Spend)+lag(log(deflated_per_person_allocation))+total_spend_10millions+GDHI_per_person+CCGpop, data=dfint, index=c("CCG_Name", "year") ,method="within", effect = "twoways")
three <- plm(log(Treatable_Mortality_Rate)~lag(Private_Sector_Procurement_Spend)+lag(log(deflated_per_person_allocation_la))+total_spend_10millions+GDHI_per_person+CCGpop, data=dfint, index=c("CCG_Name", "year") ,method="within", effect = "twoways")
four <- plm(log(Treatable_Mortality_Rate)~lag(Private_Sector_Procurement_Spend)+lag(log(deflated_per_person_benefits))+total_spend_10millions+GDHI_per_person+CCGpop, data=dfint, index=c("CCG_Name", "year") ,method="within", effect = "twoways")
five <- plm(log(Treatable_Mortality_Rate)~lag(Private_Sector_Procurement_Spend)+lag(log(deflated_per_person_benefits))+lag(log(deflated_per_person_allocation))+lag(log(deflated_per_person_allocation_la))+total_spend_10millions+GDHI_per_person+CCGpop, data=dfint, index=c("CCG_Name", "year") ,method="within", effect = "twoways")


onesum <- as.list(modelsummary(one, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
twosum <- as.list(modelsummary(two, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
threesum <- as.list(modelsummary(three, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
foursum <- as.list(modelsummary(four, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
fivesum <- as.list(modelsummary(five, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))

#FEmisssum <- as.list(modelsummary(FEmiss, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
#FEmissconsum <- as.list(modelsummary(FEmisscon, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))


onesum$tidy$p.value <- coef_test(one, vcov = "CR2", cluster = dfint$CCG_Name, test = "Satterthwaite")$p
onesum$tidy$std.error <- coef_test(one, vcov = "CR2", cluster = dfint$CCG_Name, test = "Satterthwaite")$SE
onesum$tidy$conf.low <- onesum$tidy$estimate-(1.96*coef_test(one, vcov = "CR2", cluster = dfint$CCG_Name, test = "Satterthwaite")$SE)
onesum$tidy$conf.high <- onesum$tidy$estimate+(1.96*coef_test(one, vcov = "CR2", cluster = dfint$CCG_Name, test = "Satterthwaite")$SE)
onesum$tidy$estimate <- onesum$tidy$estimate

twosum$tidy$p.value <- coef_test(two, vcov = "CR2", cluster = dfint$CCG_Name, test = "Satterthwaite")$p
twosum$tidy$std.error <- coef_test(two, vcov = "CR2", cluster = dfint$CCG_Name, test = "Satterthwaite")$SE
twosum$tidy$conf.low <- twosum$tidy$estimate-(1.96*coef_test(two, vcov = "CR2", cluster = dfint$CCG_Name, test = "Satterthwaite")$SE)
twosum$tidy$conf.high <- twosum$tidy$estimate+(1.96*coef_test(two, vcov = "CR2", cluster = dfint$CCG_Name, test = "Satterthwaite")$SE)
twosum$tidy$estimate <- twosum$tidy$estimate


threesum$tidy$p.value <- coef_test(three, vcov = "CR2", cluster = dfint$CCG_Name, test = "Satterthwaite")$p
threesum$tidy$std.error <- coef_test(three, vcov = "CR2", cluster = dfint$CCG_Name, test = "Satterthwaite")$SE
threesum$tidy$conf.low <- threesum$tidy$estimate-(1.96*coef_test(three, vcov = "CR2", cluster = dfint$CCG_Name, test = "Satterthwaite")$SE)
threesum$tidy$conf.high <- threesum$tidy$estimate+(1.96*coef_test(three, vcov = "CR2", cluster = dfint$CCG_Name, test = "Satterthwaite")$SE)
threesum$tidy$estimate <- threesum$tidy$estimate


foursum$tidy$p.value <- coef_test(four, vcov = "CR2", cluster = dfint$CCG_Name, test = "Satterthwaite")$p
foursum$tidy$std.error <- coef_test(four, vcov = "CR2", cluster = dfint$CCG_Name, test = "Satterthwaite")$SE
foursum$tidy$conf.low <- foursum$tidy$estimate-(1.96*coef_test(four, vcov = "CR2", cluster = dfint$CCG_Name, test = "Satterthwaite")$SE)
foursum$tidy$conf.high <- foursum$tidy$estimate+(1.96*coef_test(four, vcov = "CR2", cluster = dfint$CCG_Name, test = "Satterthwaite")$SE)
foursum$tidy$estimate <- foursum$tidy$estimate

fivesum$tidy$p.value <- coef_test(five, vcov = "CR2", cluster = dfint$CCG_Name, test = "Satterthwaite")$p
fivesum$tidy$std.error <- coef_test(five, vcov = "CR2", cluster = dfint$CCG_Name, test = "Satterthwaite")$SE
fivesum$tidy$conf.low <- fivesum$tidy$estimate-(1.96*coef_test(five, vcov = "CR2", cluster = dfint$CCG_Name, test = "Satterthwaite")$SE)
fivesum$tidy$conf.high <- fivesum$tidy$estimate+(1.96*coef_test(five, vcov = "CR2", cluster = dfint$CCG_Name, test = "Satterthwaite")$SE)
fivesum$tidy$estimate <- fivesum$tidy$estimate


cm <- c("lag(Private_Sector_Procurement_Spend)" = "For-profit outsourcing (%)",
        "lag(log(deflated_per_person_allocation))" = "CCG Allocation (? per capita)",
        "lag(log(deflated_per_person_allocation_la))" = "LA Allocation (? per capita)",
        "lag(log(deflated_per_person_benefits))" = "Benefit Allocation (? per capita)")
rows <- tribble(~term,          ~`ln. Treatable Mortality [.95 ci]`,  ~`p-value`,~`ln. Treatable Mortality [.95 ci]`,  ~`p-value`,  ~`ln. Treatable Mortality [.95 ci]`,  ~`p-value`, ~`ln. Treatable Mortality [.95 ci]`,  ~`p-value`, ~`ln. Treatable Mortality [.95 ci]`,  ~`p-value`,
                'CCG Fixed Effects', 'Yes',  'Yes', 'Yes',  'Yes','Yes',  'Yes','Yes',  'Yes','Yes',  'Yes',
                'Time Fixed Effects','Yes','Yes','Yes',  'Yes','Yes','Yes','Yes',  'Yes','Yes',  'Yes',
                'Clustered Standard Errors', 'Yes','Yes', 'Yes',  'Yes','Yes','Yes','Yes',  'Yes','Yes',  'Yes',
                'Control variables', 'Yes','Yes', 'Yes',  'Yes','Yes','Yes','Yes',  'Yes','Yes',  'Yes')


table <- modelsummary(list("ln. Treatable Mortality [.95 ci]"=onesum,"p-value"=onesum,"ln. Treatable Mortality [.95 ci]"=twosum,"p-value"=twosum,"ln. Treatable Mortality [.95 ci]"=threesum,"p-value"=threesum,"ln. Treatable Mortality [.95 ci]"=foursum,"p-value"=foursum,"ln. Treatable Mortality [.95 ci]"=fivesum,"p-value"=fivesum),
                      coef_omit = "Intercept|dept|year", add_rows = rows, coef_map = cm,
                      fmt = 4, estimate = c("{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value"), statistic = NULL,
                      notes = list('Table reports results from multivariate longitudinal regression models.',
                                   'Robust SEs are clustered at CCG level and use a bias-reduced linearization estimator (CR2)',
                                   'Lag of one year applied to allocation variables',
                                   'Tr. mortality and allocations are log transformed, "Ln" denotes the natural log of outcome variable.',
                                   'Control variables are household income, total commissioner spend and population size'),
                      output = "gt")
# add_header_above(c(" ", "Main finding" = 2, "Controlling for Austerity" = 2, "CCG allocation interaction" = 2, "LA allocation interaction" = 2, "Benefit allocation interaction" = 2))

gtsave(table, "Regressions/paper_3/FE_confounding_logged.html")







####Resulting Dag####

# library(dagitty)
# library(ggdag)
# 
# dag <- dagitty("dag{y <- z -> x}")
# tidy_dagitty(dag)
# 
# dagify(
#   Mortality ~ Austerity + Privatisation,
#   Privatisation ~ Austerity
# ) %>%
#   node_canonical() %>%
#   ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
#   geom_dag_point() +
#   geom_dag_edges_diagonal() +
#   geom_dag_text() +
#   theme_dag()+
#   theme(text=element_text(size=2))

library(ggforce)
library(ggarchery)

dag <- ggplot()+
  geom_arrowsegment(aes(x = 0, xend = 100, y = 100, yend = 50), arrow_positions = 0.5) + 
  geom_arrowsegment(aes(x = 0, xend = 100, y = 0, yend = 50), arrow_positions = 0.5) + 
  geom_arrowsegment(aes(x = 0, xend = 100, y = 50, yend = 50), arrow_positions = 0.5) +
  geom_circle(aes(x0 = 0, y0 = 100, r = 12),
              inherit.aes = FALSE, fill="white")+
  geom_circle(aes(x0 = 0, y0 = 0, r = 12),
              inherit.aes = FALSE, fill="white")+
  geom_circle(aes(x0 = 0, y0 = 50, r = 12),
              inherit.aes = FALSE, fill="white")+
  geom_circle(aes(x0 = 100, y0 = 50, r = 12),
              inherit.aes = FALSE, fill="white")+
  annotate("text", label = "Conservative State Retrenchment",
           x = -15, y = 50, size = 8.5, colour = "black", angle = 90)+
  annotate("text", label = "Restricted\nNHS budget",
           x = 0, y = 100, size = 4.8, colour = "black")+
  annotate("text", label = "Cuts to\nsocial services\nand\nwelfare payments",
           x = 0, y = 0, size = 4.8, colour = "black")+
  annotate("text", label = "NHS\nPrivatisation",
           x = 0, y = 50, size = 4.8, colour = "black")+
  annotate("text", label = "Treatable\nMortality",
           x = 100, y = 50, size = 4.8, colour = "black")+
  theme_void()+
  # coord_cartesian(ylim=c(4, 22), xlim=c(-100,30))+
  coord_fixed()+
  theme(panel.background = element_rect(fill = "white"))
ggsave("C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/austerity_and_privatisation/Figures/dag_final.jpeg", plot=dag, width=10, height=10, dpi=600)





####APPENDIX BABYYYY####

####austerity plots####

lm4 <- lm(deflated_per_person_benefits~time, data= benefits_plot[which(benefits_plot$time<9),])

plot3 <- ggplot(benefits_plot)+
  stat_summary(aes(x=factor(time), y=deflated_per_person_benefits))+
  theme_nice()+
  labs(x="Year", y="Benefits Expenditure\n(£ per household, by LA)", title = "Benefits and tax credits")+
  geom_smooth(data =benefits_plot[which(benefits_plot$time<9),],aes(group=1,x=factor(time), y=deflated_per_person_benefits), colour="red",fill="lightsalmon", method = "lm")+
  geom_segment(colour="red",linetype="dashed",aes(x = 7, y = lm4$coefficients[1]+(lm4$coefficients[2]*8), xend = 48, yend = lm4$coefficients[1]+lm4$coefficients[2]*48))+
  geom_smooth(data =benefits_plot[which(benefits_plot$time>7),],aes(group=1, x=factor(time), y=deflated_per_person_benefits), colour="blue", fill="mediumpurple1", method = "lm"  )+
  geom_vline(xintercept=7, linetype="dotted")+
  #  coord_cartesian(ylim=c(0.1, 0.8))+
  scale_x_discrete(breaks = c(0,4,8,12,16,20,24,28,32,36,40,44,48), labels = c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020))+
  annotate("text", label = "Austerity policies implemented",
           x = 6.5, y = 290, size = 2.5, colour = "black", angle=90)+
  annotate("text", label = "New Labour Government",
           x = 4, y = 429, size = 2.5, colour = "red", angle=20)+  
  annotate("text", label = "Pre-austerity trajectory",
           x = 20, y = 465, size = 2.5, colour = "red", angle=20)+
  annotate("text", label = "Conservative Governments",
           x = 20, y = 370, size = 2.5, colour = "blue", angle=-25)

ggsave("plots/paper_3/appendix_bens_plot.png", plot=plot3, width=10, height=8, dpi=600)





lm1 <- (lm(deflated_per_person_allocation_la~year, data= LA_allocation[which(LA_allocation$year<2011),]))

plot1 <- ggplot(LA_allocation)+
  stat_summary(aes(x=factor(year), y=deflated_per_person_allocation_la))+
  theme_nice()+
  labs(x="Year", y="Regional LA allocation\n(£,000s per person)", title = "Local Authorities",
       caption = "Prices adjusted to 2020 values using GDP deflator\nRight-hand y-axis adjusted for NHS due to changes in the commissioner responsibilities making absolute values incomparable\nBenefits include universal credit and all components it replaced plus carers allowance and Personal Independence Payment")+
  geom_smooth(data =LA_allocation[which(LA_allocation$year<2011),],aes(group=1,x=factor(year), y=deflated_per_person_allocation_la), colour="red",fill="lightsalmon", method = "lm"  )+
  geom_segment(colour="red",linetype="dashed",aes(x = 3, y = lm1$coefficients[1]+(lm1$coefficients[2]*2010), xend = 13, yend = lm1$coefficients[1]+lm1$coefficients[2]*2020))+
  geom_smooth(data =LA_allocation[which(LA_allocation$year>2009),],aes(group=1, x=factor(year), y=deflated_per_person_allocation_la), colour="blue", fill="mediumpurple1", method = "lm"  )+
  geom_vline(xintercept="2010", linetype="dotted")+
  coord_cartesian(ylim=c(0.1, 0.8))+
  annotate("text", label = "Austerity policies implemented",
           x = 2.8, y = 0.25, size = 2.5, colour = "black", angle=90)+
  annotate("text", label = "New Labour Government",
           x = 2, y = 0.43, size = 2.5, colour = "red", angle=13)+  
  annotate("text", label = "Pre-austerity trajectory",
           x = 8.5, y = 0.665, size = 2.5, colour = "red", angle=13)+
  annotate("text", label = "Conservative Governments",
           x = 8.5, y = 0.37, size = 2.5, colour = "blue", angle=-20)

ggsave("plots/paper_3/appendix_la_plot.png", plot=plot1, width=10, height=8, dpi=600)


####summary tables####

table_data <- as.data.frame(df) %>%
  dplyr::select(Private_Sector_Procurement_Spend, deflated_per_person_allocation,deflated_per_person_allocation_la, deflated_per_person_benefits, Treatable_Mortality_Rate, balance, outsourcing_treats)

names(table_data)[names(table_data)=="Treatable_Mortality_Rate"] <- "Treatable Mortality Rate"
names(table_data)[names(table_data)=="Private_Sector_Procurement_Spend"] <- "For-Profit Outsourcing (%)"
names(table_data)[names(table_data)=="deflated_per_person_allocation"] <- "CCG Allocation (£000s per capita)"
names(table_data)[names(table_data)=="deflated_per_person_allocation_la"] <- "LA Allocation (£000s per capita)"
names(table_data)[names(table_data)=="deflated_per_person_benefits"] <- "Benefit Expenditure (£000s per capita)"
names(table_data)[names(table_data)=="balance"] <- "CCG account balance (£000s)"
names(table_data)[names(table_data)=="outsourcing_treats"] <- "Treatment outsourcing (%)"

table_data$`CCG account balance (£000s)` <-as.numeric(table_data$`CCG account balance (£000s)`)

#only summarise cases which will be used in model#
table_data <- table_data[complete.cases(table_data),]

#Calculate descriptive stats
descriptivestats <- stat.desc(table_data)
t_descriptivestats <- as.data.frame(t(descriptivestats))

#manually create Source column
t_descriptivestats$Source <- c("ONS", "Rahal & Mohan, (2022).", "NHS England", "MHCLG" , "DwP", "NHS England", "NHS England")

#manually Create IQR

t_descriptivestats$IQR <-  c(IQR(table_data$`Treatable Mortality Rate`) ,IQR(table_data$`For-Profit Outsourcing (%)`),IQR(table_data$`CCG Allocation (£000s per capita)`),IQR(table_data$`LA Allocation (£000s per capita)`),IQR(table_data$`Benefit Expenditure (£000s per capita)`),IQR(table_data$`CCG account balance (£000s)`),IQR(table_data$`Treatment outsourcing (%)`))

#Create columns
is.num <- sapply(t_descriptivestats, is.numeric)

t_descriptivestats[is.num] <- lapply(t_descriptivestats[is.num], round, 2)

t_descriptivestats$minmax <- with(t_descriptivestats,  paste0(min ,"/ ", max, "")) 
t_descriptivestats$meanse <- with(t_descriptivestats,  paste0(mean ," (", SE.mean, ")")) 
t_descriptivestats$mediqr <- with(t_descriptivestats,  paste0(median ," (", IQR, ")")) 

#write table
Table_1 <-    
  knitr::kable(t_descriptivestats[c("minmax","meanse","mediqr","Source")],
               format = "html", # default
               digits = 2,        # specify decimal places
               caption = "Study Variables",
               col.names = c( "Min/ Max", "Mean (SD)","Median (IQR)","Source"),
               row.names = T,
               
               # align = c("c","c","c","r")
               # padding = 2     # inner spacing
  )%>%kable_styling(font_size = 10)%>%save_kable("plots/paper_3/appendix_desc_table.html")




table_data <- as.data.frame(df) %>%
  dplyr::select(year, Private_Sector_Procurement_Spend, deflated_per_person_allocation,deflated_per_person_allocation_la, deflated_per_person_benefits, Treatable_Mortality_Rate, balance, outsourcing_treats)

names(table_data)[names(table_data)=="Treatable_Mortality_Rate"] <- "Treatable Mortality Rate"
names(table_data)[names(table_data)=="Private_Sector_Procurement_Spend"] <- "For-Profit Outsourcing (%)"
names(table_data)[names(table_data)=="deflated_per_person_allocation"] <- "CCG Allocation (£000s per capita)"
names(table_data)[names(table_data)=="deflated_per_person_allocation_la"] <- "LA Allocation (£000s per capita)"
names(table_data)[names(table_data)=="deflated_per_person_benefits"] <- "Benefit Expenditure (£000s per capita)"
names(table_data)[names(table_data)=="balance"] <- "CCG account balance (£000s)"
names(table_data)[names(table_data)=="outsourcing_treats"] <- "Treatment outsourcing (%)"

table_data$`CCG account balance (£000s)` <-as.numeric(table_data$`CCG account balance (£000s)`)

#only summarise cases which will be used in model#
table_data <- table_data[complete.cases(table_data$`CCG Allocation (£000s per capita)`),]
table_data <- table_data[table_data$year>2013,]


table2 <- 
  tbl_summary(
    table_data,
    by = c(year), # split table by group
    missing = "no",
    type = all_continuous() ~ "continuous2",
    statistic = all_continuous() ~ c("{N_nonmiss}",
                                     "{mean} ({median})", 
                                     "{min}, {max} ({sd})")# don't list missing data separately
  ) %>%
  add_n() %>% # add column with total number of non-missing observations
  #add_overall() %>%
  add_p() %>% # test for a difference between groups
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Year**") %>%
  modify_caption("**Table. Variable descriptives by Year**") %>%
  modify_header(label = "**Variable**")%>%
  bold_labels()

table2 %>%
  as_gt() %>%
  gt::gtsave(filename = "plots/paper_3/appendix_desc_table_year.html")








table_data <- as.data.frame(df) %>%
  dplyr::select(CCG_Name, Private_Sector_Procurement_Spend, deflated_per_person_allocation,deflated_per_person_allocation_la, deflated_per_person_benefits, Treatable_Mortality_Rate, balance, outsourcing_treats)

names(table_data)[names(table_data)=="Treatable_Mortality_Rate"] <- "Treatable Mortality Rate"
names(table_data)[names(table_data)=="Private_Sector_Procurement_Spend"] <- "For-Profit Outsourcing (%)"
names(table_data)[names(table_data)=="deflated_per_person_allocation"] <- "CCG Allocation (£000s per capita)"
names(table_data)[names(table_data)=="deflated_per_person_allocation_la"] <- "LA Allocation (£000s per capita)"
names(table_data)[names(table_data)=="deflated_per_person_benefits"] <- "Benefit Expenditure (£000s per capita)"
names(table_data)[names(table_data)=="balance"] <- "CCG account balance (£000s)"
names(table_data)[names(table_data)=="outsourcing_treats"] <- "Treatment outsourcing (%)"

table_data$`CCG account balance (£000s)` <-as.numeric(table_data$`CCG account balance (£000s)`)


table_data <- table_data%>%
  dplyr::group_by(CCG_Name)%>%
  group_by(CCG_Name) %>%
  summarise_all(mean, na.rm = TRUE)

Table_1 <-    
  knitr::kable(table_data,
               #format = "html", # default
               #  digits = 2,        # specify decimal places
               caption = "Study Variables (means)",
               # col.names = T,
               # row.names = F,
               # align = c("c","c","c","r")
               # padding = 2     # inner spacing
  )%>%kable_styling(font_size = 10)%>%
  kableExtra::save_kable(file = "plots/paper_3/appendix_desc_table_ccg.html")


####Full table 2####

pdf <- df %>% dplyr::select(deflated_per_person_benefits,
                            deflated_per_person_allocation,
                            deflated_per_person_allocation_la,
                            Private_Sector_Procurement_Spend,
                            CCG_Name,
                            CCGpop,
                            total_spend_10millions,
                            GDHI_per_person,
                            year,
                            Treatable_Mortality_Rate)%>%
  dplyr::distinct()%>%
  dplyr::filter(!is.na(deflated_per_person_benefits),
                !is.na(Private_Sector_Procurement_Spend),
                !is.na(CCG_Name),
                !is.na(year),
                !is.na(Treatable_Mortality_Rate),
                !is.na(GDHI_per_person),
                Private_Sector_Procurement_Spend>=0,
                year!=2013)
pdf <- plm::pdata.frame(unique(pdf),index=c("CCG_Name", "year") )

one <-plm(log(Treatable_Mortality_Rate)~lag(Private_Sector_Procurement_Spend)+total_spend_10millions+GDHI_per_person+CCGpop, data=pdf ,method="within", effect = "twoways")
two <- plm(log(Treatable_Mortality_Rate)~lag(Private_Sector_Procurement_Spend)+lag(log(deflated_per_person_allocation))+total_spend_10millions+GDHI_per_person+CCGpop, data=pdf ,method="within", effect = "twoways")
three <- plm(log(Treatable_Mortality_Rate)~lag(Private_Sector_Procurement_Spend)+lag(log(deflated_per_person_allocation_la))+total_spend_10millions+GDHI_per_person+CCGpop, data=pdf,method="within", effect = "twoways")
four <- plm(log(Treatable_Mortality_Rate)~lag(Private_Sector_Procurement_Spend)+lag(log(deflated_per_person_benefits))+total_spend_10millions+GDHI_per_person+CCGpop, data=pdf ,method="within", effect = "twoways")
five <- plm(log(Treatable_Mortality_Rate)~lag(Private_Sector_Procurement_Spend)+lag(log(deflated_per_person_benefits))+lag(log(deflated_per_person_allocation))+lag(log(deflated_per_person_allocation_la))+total_spend_10millions+GDHI_per_person+CCGpop, data=pdf ,method="within", effect = "twoways")
car::vif(five)

onesum <- as.list(modelsummary(one, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
twosum <- as.list(modelsummary(two, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
threesum <- as.list(modelsummary(three, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
foursum <- as.list(modelsummary(four, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
fivesum <- as.list(modelsummary(five, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))

#FEmisssum <- as.list(modelsummary(FEmiss, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
#FEmissconsum <- as.list(modelsummary(FEmisscon, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))


onesum$tidy$p.value <- coef_test(one, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$p
onesum$tidy$std.error <- coef_test(one, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE
onesum$tidy$conf.low <- onesum$tidy$estimate-(1.96*coef_test(one, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE)
onesum$tidy$conf.high <- onesum$tidy$estimate+(1.96*coef_test(one, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE)
onesum$tidy$estimate <- onesum$tidy$estimate

twosum$tidy$p.value <- coef_test(two, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$p
twosum$tidy$std.error <- coef_test(two, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE
twosum$tidy$conf.low <- twosum$tidy$estimate-(1.96*coef_test(two, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE)
twosum$tidy$conf.high <- twosum$tidy$estimate+(1.96*coef_test(two, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE)
twosum$tidy$estimate <- twosum$tidy$estimate


threesum$tidy$p.value <- coef_test(three, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$p
threesum$tidy$std.error <- coef_test(three, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE
threesum$tidy$conf.low <- threesum$tidy$estimate-(1.96*coef_test(three, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE)
threesum$tidy$conf.high <- threesum$tidy$estimate+(1.96*coef_test(three, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE)
threesum$tidy$estimate <- threesum$tidy$estimate


foursum$tidy$p.value <- coef_test(four, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$p
foursum$tidy$std.error <- coef_test(four, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE
foursum$tidy$conf.low <- foursum$tidy$estimate-(1.96*coef_test(four, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE)
foursum$tidy$conf.high <- foursum$tidy$estimate+(1.96*coef_test(four, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE)
foursum$tidy$estimate <- foursum$tidy$estimate

fivesum$tidy$p.value <- coef_test(five, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$p
fivesum$tidy$std.error <- coef_test(five, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE
fivesum$tidy$conf.low <- fivesum$tidy$estimate-(1.96*coef_test(five, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE)
fivesum$tidy$conf.high <- fivesum$tidy$estimate+(1.96*coef_test(five, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE)
fivesum$tidy$estimate <- fivesum$tidy$estimate


cm <- c("lag(Private_Sector_Procurement_Spend)" = "For-profit outsourcing (%)",
        "lag(log(deflated_per_person_allocation))" = "CCG Allocation (£ per capita)",
        "lag(log(deflated_per_person_allocation_la))" = "LA Allocation (£ per capita)",
        "lag(log(deflated_per_person_benefits))" = "Benefit Allocation (£ per capita)",
        "total_spend_10millions" = "Total CCG Spend (£10ms)",
        "GDHI_per_person" = "Average Income (£)",
        "CCGpop" = "CCG Population")

rows <- tribble(~term,          ~`ln. Treatable Mortality [.95 ci]`,  ~`p-value`,~`ln. Treatable Mortality [.95 ci]`,  ~`p-value`,  ~`ln. Treatable Mortality [.95 ci]`,  ~`p-value`, ~`ln. Treatable Mortality [.95 ci]`,  ~`p-value`, ~`ln. Treatable Mortality [.95 ci]`,  ~`p-value`,
                'CCG Fixed Effects', 'Yes',  'Yes', 'Yes',  'Yes','Yes',  'Yes','Yes',  'Yes','Yes',  'Yes',
                'Time Fixed Effects','Yes','Yes','Yes',  'Yes','Yes','Yes','Yes',  'Yes','Yes',  'Yes',
                'Clustered Standard Errors', 'Yes','Yes', 'Yes',  'Yes','Yes','Yes','Yes',  'Yes','Yes',  'Yes',
                'Control variables', 'Yes','Yes', 'Yes',  'Yes','Yes','Yes','Yes',  'Yes','Yes',  'Yes')


table <- modelsummary(list("ln. Treatable Mortality [.95 ci]"=onesum,"p-value"=onesum,"ln. Treatable Mortality [.95 ci]"=twosum,"p-value"=twosum,"ln. Treatable Mortality [.95 ci]"=threesum,"p-value"=threesum,"ln. Treatable Mortality [.95 ci]"=foursum,"p-value"=foursum,"ln. Treatable Mortality [.95 ci]"=fivesum,"p-value"=fivesum),
                      coef_omit = "Intercept|dept|year", add_rows = rows, coef_map = cm,
                      fmt = 4, estimate = c("{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value"), statistic = NULL,
                      notes = list('Table reports results from multivariate longitudinal regression models.',
                                   'Robust SEs are clustered at CCG level and use a bias-reduced linearization estimator (CR2)',
                                   'Lag of one year applied to allocation variables',
                                   'Tr. mortality and allocations are log transformed, "Ln" denotes the natural log of outcome variable.',
                                   'Control variables are household income, total commissioner spend and population size'),
                      output = "gt")
# add_header_above(c(" ", "Main finding" = 2, "Controlling for Austerity" = 2, "CCG allocation interaction" = 2, "LA allocation interaction" = 2, "Benefit allocation interaction" = 2))

gtsave(table, "Regressions/paper_3/FE_confounding_full.html")

####Moderating####




pdf <- df %>% dplyr::select(deflated_per_person_benefits,
                            deflated_per_person_allocation,
                            deflated_per_person_allocation_la,
                            Private_Sector_Procurement_Spend,
                            CCG_Name,
                            CCGpop,
                            total_spend_10millions,
                            GDHI_per_person,
                            year,
                            Treatable_Mortality_Rate)%>%
  dplyr::distinct()%>%
  dplyr::filter(!is.na(deflated_per_person_benefits),
                !is.na(Private_Sector_Procurement_Spend),
                !is.na(CCG_Name),
                !is.na(year),
                !is.na(Treatable_Mortality_Rate),
                !is.na(GDHI_per_person),
                Private_Sector_Procurement_Spend>=0,
                year!=2013)
pdf <- plm::pdata.frame(unique(pdf),index=c("CCG_Name", "year") )


one <-plm(log(Treatable_Mortality_Rate)~lag(Private_Sector_Procurement_Spend)+total_spend_10millions+GDHI_per_person+CCGpop, data=pdf ,method="within", effect = "twoways")
two <- plm(log(Treatable_Mortality_Rate)~lag(Private_Sector_Procurement_Spend)*lag(log(deflated_per_person_allocation))+total_spend_10millions+GDHI_per_person+CCGpop, data=pdf ,method="within", effect = "twoways")
three <- plm(log(Treatable_Mortality_Rate)~lag(Private_Sector_Procurement_Spend)*lag(log(deflated_per_person_allocation_la))+total_spend_10millions+GDHI_per_person+CCGpop, data=pdf,method="within", effect = "twoways")
four <- plm(log(Treatable_Mortality_Rate)~lag(Private_Sector_Procurement_Spend)*lag(log(deflated_per_person_benefits))+total_spend_10millions+GDHI_per_person+CCGpop, data=pdf ,method="within", effect = "twoways")


onesum <- as.list(modelsummary(one, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
twosum <- as.list(modelsummary(two, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
threesum <- as.list(modelsummary(three, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
foursum <- as.list(modelsummary(four, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))

#FEmisssum <- as.list(modelsummary(FEmiss, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
#FEmissconsum <- as.list(modelsummary(FEmisscon, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))


onesum$tidy$p.value <- coef_test(one, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$p
onesum$tidy$std.error <- coef_test(one, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE
onesum$tidy$conf.low <- onesum$tidy$estimate-(1.96*coef_test(one, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE)
onesum$tidy$conf.high <- onesum$tidy$estimate+(1.96*coef_test(one, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE)
onesum$tidy$estimate <- onesum$tidy$estimate

twosum$tidy$p.value <- coef_test(two, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$p
twosum$tidy$std.error <- coef_test(two, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE
twosum$tidy$conf.low <- twosum$tidy$estimate-(1.96*coef_test(two, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE)
twosum$tidy$conf.high <- twosum$tidy$estimate+(1.96*coef_test(two, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE)
twosum$tidy$estimate <- twosum$tidy$estimate


threesum$tidy$p.value <- coef_test(three, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$p
threesum$tidy$std.error <- coef_test(three, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE
threesum$tidy$conf.low <- threesum$tidy$estimate-(1.96*coef_test(three, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE)
threesum$tidy$conf.high <- threesum$tidy$estimate+(1.96*coef_test(three, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE)
threesum$tidy$estimate <- threesum$tidy$estimate


foursum$tidy$p.value <- coef_test(four, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$p
foursum$tidy$std.error <- coef_test(four, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE
foursum$tidy$conf.low <- foursum$tidy$estimate-(1.96*coef_test(four, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE)
foursum$tidy$conf.high <- foursum$tidy$estimate+(1.96*coef_test(four, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE)
foursum$tidy$estimate <- foursum$tidy$estimate



cm <- c("lag(Private_Sector_Procurement_Spend)" = "For-profit outsourcing (%)",
        "lag(log(deflated_per_person_allocation))" = "CCG Allocation (£ per capita)",
        "lag(log(deflated_per_person_allocation_la))" = "LA Allocation (£ per capita)",
        "lag(log(deflated_per_person_benefits))" = "Benefit Allocation (£ per capita)",
        "lag(Private_Sector_Procurement_Spend):lag(log(deflated_per_person_allocation))"= "Interaction: Outsourcing*CCG_Allocation",
        "lag(Private_Sector_Procurement_Spend):lag(log(deflated_per_person_allocation_la))"= "Interaction: Outsourcing*LA_Allocation",
        "lag(Private_Sector_Procurement_Spend):lag(log(deflated_per_person_benefits))"= "Interaction: Outsourcing*Bens_Allocation"
)

rows <- tribble(~term,          ~`ln. Treatable Mortality [.95 ci]`,  ~`p-value`,~`ln. Treatable Mortality [.95 ci]`,  ~`p-value`,  ~`ln. Treatable Mortality [.95 ci]`,  ~`p-value`, ~`ln. Treatable Mortality [.95 ci]`,  ~`p-value`,
                'CCG Fixed Effects', 'Yes',  'Yes', 'Yes',  'Yes','Yes',  'Yes','Yes',  'Yes',
                'Time Fixed Effects','Yes','Yes','Yes',  'Yes','Yes','Yes','Yes',  'Yes',
                'Clustered Standard Errors', 'Yes','Yes', 'Yes',  'Yes','Yes','Yes','Yes',  'Yes',
                'Control variables', 'Yes','Yes', 'Yes',  'Yes','Yes','Yes','Yes',  'Yes')


table <- modelsummary(list("ln. Treatable Mortality [.95 ci]"=onesum,"p-value"=onesum,"ln. Treatable Mortality [.95 ci]"=twosum,"p-value"=twosum,"ln. Treatable Mortality [.95 ci]"=threesum,"p-value"=threesum,"ln. Treatable Mortality [.95 ci]"=foursum,"p-value"=foursum),
                      coef_omit = "Intercept|dept|year", add_rows = rows, coef_map = cm,
                      fmt = 4, estimate = c("{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value"), statistic = NULL,
                      notes = list('Table reports results from multivariate longitudinal regression models.',
                                   'Robust SEs are clustered at CCG level and use a bias-reduced linearization estimator (CR2)',
                                   'Lag of one year applied to allocation variables',
                                   'Tr. mortality and allocations are log transformed, "Ln" denotes the natural log of outcome variable.',
                                   'Control variables are household income, total commissioner spend and population size'),
                      output = "gt")
# add_header_above(c(" ", "Main finding" = 2, "Controlling for Austerity" = 2, "CCG allocation interaction" = 2, "LA allocation interaction" = 2, "Benefit allocation interaction" = 2))

gtsave(table, "Regressions/paper_3/FE_interaction.html")


####Avoidable mort on confound####



pdf <- df %>% dplyr::select(deflated_per_person_benefits,
                            deflated_per_person_allocation,
                            deflated_per_person_allocation_la,
                            Private_Sector_Procurement_Spend,
                            CCG_Name,
                            CCGpop,
                            total_spend_10millions,
                            GDHI_per_person,
                            year,
                            avoidable_mortality_rate)%>%
  dplyr::distinct()%>%
  dplyr::filter(!is.na(deflated_per_person_benefits),
                !is.na(Private_Sector_Procurement_Spend),
                !is.na(CCG_Name),
                !is.na(year),
                !is.na(avoidable_mortality_rate),
                !is.na(GDHI_per_person),
                Private_Sector_Procurement_Spend>=0,
                year!=2013)
pdf <- plm::pdata.frame(unique(pdf),index=c("CCG_Name", "year") )

one <-plm(log(avoidable_mortality_rate)~lag(Private_Sector_Procurement_Spend)+total_spend_10millions+GDHI_per_person+CCGpop, data=pdf ,method="within", effect = "twoways")
two <- plm(log(avoidable_mortality_rate)~lag(Private_Sector_Procurement_Spend)+lag(log(deflated_per_person_allocation))+total_spend_10millions+GDHI_per_person+CCGpop, data=pdf ,method="within", effect = "twoways")
three <- plm(log(avoidable_mortality_rate)~lag(Private_Sector_Procurement_Spend)+lag(log(deflated_per_person_allocation_la))+total_spend_10millions+GDHI_per_person+CCGpop, data=pdf,method="within", effect = "twoways")
four <- plm(log(avoidable_mortality_rate)~lag(Private_Sector_Procurement_Spend)+lag(log(deflated_per_person_benefits))+total_spend_10millions+GDHI_per_person+CCGpop, data=pdf ,method="within", effect = "twoways")
five <- plm(log(avoidable_mortality_rate)~lag(Private_Sector_Procurement_Spend)+lag(log(deflated_per_person_benefits))+lag(log(deflated_per_person_allocation))+lag(log(deflated_per_person_allocation_la))+total_spend_10millions+GDHI_per_person+CCGpop, data=pdf ,method="within", effect = "twoways")


onesum <- as.list(modelsummary(one, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
twosum <- as.list(modelsummary(two, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
threesum <- as.list(modelsummary(three, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
foursum <- as.list(modelsummary(four, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
fivesum <- as.list(modelsummary(five, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))

#FEmisssum <- as.list(modelsummary(FEmiss, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
#FEmissconsum <- as.list(modelsummary(FEmisscon, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))


onesum$tidy$p.value <- coef_test(one, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$p
onesum$tidy$std.error <- coef_test(one, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE
onesum$tidy$conf.low <- onesum$tidy$estimate-(1.96*coef_test(one, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE)
onesum$tidy$conf.high <- onesum$tidy$estimate+(1.96*coef_test(one, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE)
onesum$tidy$estimate <- onesum$tidy$estimate

twosum$tidy$p.value <- coef_test(two, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$p
twosum$tidy$std.error <- coef_test(two, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE
twosum$tidy$conf.low <- twosum$tidy$estimate-(1.96*coef_test(two, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE)
twosum$tidy$conf.high <- twosum$tidy$estimate+(1.96*coef_test(two, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE)
twosum$tidy$estimate <- twosum$tidy$estimate


threesum$tidy$p.value <- coef_test(three, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$p
threesum$tidy$std.error <- coef_test(three, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE
threesum$tidy$conf.low <- threesum$tidy$estimate-(1.96*coef_test(three, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE)
threesum$tidy$conf.high <- threesum$tidy$estimate+(1.96*coef_test(three, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE)
threesum$tidy$estimate <- threesum$tidy$estimate


foursum$tidy$p.value <- coef_test(four, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$p
foursum$tidy$std.error <- coef_test(four, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE
foursum$tidy$conf.low <- foursum$tidy$estimate-(1.96*coef_test(four, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE)
foursum$tidy$conf.high <- foursum$tidy$estimate+(1.96*coef_test(four, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE)
foursum$tidy$estimate <- foursum$tidy$estimate

fivesum$tidy$p.value <- coef_test(five, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$p
fivesum$tidy$std.error <- coef_test(five, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE
fivesum$tidy$conf.low <- fivesum$tidy$estimate-(1.96*coef_test(five, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE)
fivesum$tidy$conf.high <- fivesum$tidy$estimate+(1.96*coef_test(five, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE)
fivesum$tidy$estimate <- fivesum$tidy$estimate


cm <- c("lag(Private_Sector_Procurement_Spend)" = "For-profit outsourcing (%)",
        "lag(log(deflated_per_person_allocation))" = "CCG Allocation (£ per capita)",
        "lag(log(deflated_per_person_allocation_la))" = "LA Allocation (£ per capita)",
        "lag(log(deflated_per_person_benefits))" = "Benefit Allocation (£ per capita)")

rows <- tribble(~term,          ~`ln. Avoidable Mortality [.95 ci]`,  ~`p-value`,~`ln. Avoidable Mortality [.95 ci]`,  ~`p-value`,  ~`ln. Avoidable Mortality [.95 ci]`,  ~`p-value`, ~`ln. Avoidable Mortality [.95 ci]`,  ~`p-value`, ~`ln. Avoidable Mortality [.95 ci]`,  ~`p-value`,
                'CCG Fixed Effects', 'Yes',  'Yes', 'Yes',  'Yes','Yes',  'Yes','Yes',  'Yes','Yes',  'Yes',
                'Time Fixed Effects','Yes','Yes','Yes',  'Yes','Yes','Yes','Yes',  'Yes','Yes',  'Yes',
                'Clustered Standard Errors', 'Yes','Yes', 'Yes',  'Yes','Yes','Yes','Yes',  'Yes','Yes',  'Yes',
                'Control variables', 'Yes','Yes', 'Yes',  'Yes','Yes','Yes','Yes',  'Yes','Yes',  'Yes')


table <- modelsummary(list("ln. Avoidable Mortality [.95 ci]"=onesum,"p-value"=onesum,"ln. Avoidable Mortality [.95 ci]"=twosum,"p-value"=twosum,"ln. Avoidable Mortality [.95 ci]"=threesum,"p-value"=threesum,"ln. Avoidable Mortality [.95 ci]"=foursum,"p-value"=foursum,"ln. Avoidable Mortality [.95 ci]"=fivesum,"p-value"=fivesum),
                      coef_omit = "Intercept|dept|year", add_rows = rows, coef_map = cm,
                      fmt = 4, estimate = c("{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value"), statistic = NULL,
                      notes = list('Table reports results from multivariate longitudinal regression models.',
                                   'Robust SEs are clustered at CCG level and use a bias-reduced linearization estimator (CR2)',
                                   'Lag of one year applied to allocation variables',
                                   'Tr. mortality and allocations are log transformed, "Ln" denotes the natural log of outcome variable.',
                                   'Control variables are household income, total commissioner spend and population size'),
                      output = "gt")
# add_header_above(c(" ", "Main finding" = 2, "Controlling for Austerity" = 2, "CCG allocation interaction" = 2, "LA allocation interaction" = 2, "Benefit allocation interaction" = 2))

gtsave(table, "Regressions/paper_3/FE_confounding_avoid.html")


####Balance####

pdf <- df %>% dplyr::select(balance,
                            Private_Sector_Procurement_Spend,
                            CCG_Name,
                            CCGpop,
                            total_spend_10millions,
                            GDHI_per_person,
                            year,
                            Treatable_Mortality_Rate)%>%
  dplyr::mutate(balance = as.numeric(balance))%>%
  dplyr::distinct()%>%
  dplyr::filter(!is.na(balance),
                !is.na(Private_Sector_Procurement_Spend),
                !is.na(CCG_Name),
                !is.na(year),
                !is.na(Treatable_Mortality_Rate),
                !is.na(GDHI_per_person),
                Private_Sector_Procurement_Spend>=0,
                year!=2013)
pdf <- plm::pdata.frame(unique(pdf),index=c("CCG_Name", "year") )


one <-plm(Private_Sector_Procurement_Spend~lag(balance), data=pdf ,method="within", effect = "twoways")
two <- plm(log(Treatable_Mortality_Rate)~lag(Private_Sector_Procurement_Spend)+lag(balance)+total_spend_10millions+GDHI_per_person+CCGpop, data=pdf ,method="within", effect = "twoways")
three <- plm(log(Treatable_Mortality_Rate)~lag(Private_Sector_Procurement_Spend)*lag(balance)+total_spend_10millions+GDHI_per_person+CCGpop, data=pdf,method="within", effect = "twoways")


onesum <- as.list(modelsummary(one, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
twosum <- as.list(modelsummary(two, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
threesum <- as.list(modelsummary(three, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))



onesum$tidy$p.value <- coef_test(one, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$p
onesum$tidy$std.error <- coef_test(one, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE
onesum$tidy$conf.low <- onesum$tidy$estimate-(1.96*coef_test(one, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE)
onesum$tidy$conf.high <- onesum$tidy$estimate+(1.96*coef_test(one, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE)
onesum$tidy$estimate <- onesum$tidy$estimate

twosum$tidy$p.value <- coef_test(two, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$p
twosum$tidy$std.error <- coef_test(two, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE
twosum$tidy$conf.low <- twosum$tidy$estimate-(1.96*coef_test(two, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE)
twosum$tidy$conf.high <- twosum$tidy$estimate+(1.96*coef_test(two, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE)
twosum$tidy$estimate <- twosum$tidy$estimate


threesum$tidy$p.value <- coef_test(three, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$p
threesum$tidy$std.error <- coef_test(three, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE
threesum$tidy$conf.low <- threesum$tidy$estimate-(1.96*coef_test(three, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE)
threesum$tidy$conf.high <- threesum$tidy$estimate+(1.96*coef_test(three, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE)
threesum$tidy$estimate <- threesum$tidy$estimate




cm <- c("lag(Private_Sector_Procurement_Spend)" = "For-profit outsourcing (%)",
        "lag(balance)" = "CCG Balance (£000s)",
        "lag(Private_Sector_Procurement_Spend):lag(balance)" = "Interaction: Outsourcing*Balance")

rows <- tribble(~term,          ~`For-profit Outsourcing (%) [.95 ci]`,  ~`p-value`,~`ln. Treatable Mortality [.95 ci]`,  ~`p-value`,  ~`ln. Treatable Mortality [.95 ci]`,  ~`p-value`,
                'CCG Fixed Effects', 'Yes',  'Yes', 'Yes',  'Yes','Yes',  'Yes',
                'Time Fixed Effects','Yes','Yes','Yes',  'Yes','Yes','Yes',
                'Clustered Standard Errors', 'Yes','Yes', 'Yes',  'Yes','Yes','Yes',
                'Control variables', 'Yes','Yes', 'Yes',  'Yes','Yes',  'Yes')


table <- modelsummary(list("For-profit Outsourcing (%) [.95 ci]"=onesum,"p-value"=onesum,"ln. Treatable Mortality [.95 ci]"=twosum,"p-value"=twosum,"ln. Treatable Mortality [.95 ci]"=threesum,"p-value"=threesum),
                      coef_omit = "Intercept|dept|year", add_rows = rows, coef_map = cm,
                      fmt = 4, estimate = c("{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value"), statistic = NULL,
                      notes = list('Table reports results from multivariate longitudinal regression models.',
                                   'Robust SEs are clustered at CCG level and use a bias-reduced linearization estimator (CR2)',
                                   'Lag of one year applied to independent variables',
                                   'Tr. mortality are log transformed, "Ln" denotes the natural log of outcome variable.',
                                   'Control variables are household income, total commissioner spend and population size'),
                      output = "gt")
# add_header_above(c(" ", "Main finding" = 2, "Controlling for Austerity" = 2, "CCG allocation interaction" = 2, "LA allocation interaction" = 2, "Benefit allocation interaction" = 2))

gtsave(table, "Regressions/paper_3/FE_balance.html")

####Treatments####


pdf <- df %>% dplyr::select(deflated_per_person_benefits,
                            deflated_per_person_allocation,
                            deflated_per_person_allocation_la,
                            outsourcing_treats,
                            GDHI_per_person,
                            CCG_Name,
                            CCGpop,
                            total_spend_10millions,
                            year,
                            total_treats, 
                            treatable_mortality_rate)%>%
  dplyr::distinct()%>%
  dplyr::filter(!is.na(deflated_per_person_benefits),
                !is.na(outsourcing_treats),
                !is.na(CCG_Name),
                !is.na(year),
                !is.na(GDHI_per_person),
                !is.na(treatable_mortality_rate),
                !is.na(total_treats))
pdf <- plm::pdata.frame(unique(pdf),index=c("CCG_Name", "year") )
#pdf <- pdf[complete.cases(pdf),]

one <-plm(outsourcing_treats~lag(log(deflated_per_person_allocation))+CCGpop+total_spend_10millions, data=pdf,method="within", effect = "twoways")
two <-plm(outsourcing_treats~lag(log(deflated_per_person_allocation_la))+CCGpop+total_spend_10millions, data=pdf,method="within", effect = "twoways")
three <-plm(outsourcing_treats~lag(log(deflated_per_person_benefits))+CCGpop+total_spend_10millions, data=pdf,method="within", effect = "twoways")
four <- plm(log(treatable_mortality_rate)~lag(outsourcing_treats)+lag(log(deflated_per_person_allocation))+total_treats+GDHI_per_person+CCGpop, data=pdf ,method="within", effect = "twoways")
five <- plm(log(treatable_mortality_rate)~lag(outsourcing_treats)+lag(log(deflated_per_person_allocation_la))+total_treats+GDHI_per_person+CCGpop, data=pdf,method="within", effect = "twoways")
six <- plm(log(treatable_mortality_rate)~lag(outsourcing_treats)+lag(log(deflated_per_person_benefits))+total_treats+GDHI_per_person+CCGpop, data=pdf ,method="within", effect = "twoways")




onesum <- as.list(modelsummary(one, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
twosum <- as.list(modelsummary(two, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
threesum <- as.list(modelsummary(three, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
foursum <- as.list(modelsummary(four, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
fivesum <- as.list(modelsummary(five, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
sixsum <- as.list(modelsummary(six, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))

#FEmisssum <- as.list(modelsummary(FEmiss, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
#FEmissconsum <- as.list(modelsummary(FEmisscon, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))


onesum$tidy$p.value <- coef_test(one, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$p
onesum$tidy$std.error <- coef_test(one, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE
onesum$tidy$conf.low <- onesum$tidy$estimate-(1.96*coef_test(one, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE)
onesum$tidy$conf.high <- onesum$tidy$estimate+(1.96*coef_test(one, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE)
onesum$tidy$estimate <- onesum$tidy$estimate

twosum$tidy$p.value <- coef_test(two, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$p
twosum$tidy$std.error <- coef_test(two, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE
twosum$tidy$conf.low <- twosum$tidy$estimate-(1.96*coef_test(two, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE)
twosum$tidy$conf.high <- twosum$tidy$estimate+(1.96*coef_test(two, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE)
twosum$tidy$estimate <- twosum$tidy$estimate


threesum$tidy$p.value <- coef_test(three, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$p
threesum$tidy$std.error <- coef_test(three, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE
threesum$tidy$conf.low <- threesum$tidy$estimate-(1.96*coef_test(three, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE)
threesum$tidy$conf.high <- threesum$tidy$estimate+(1.96*coef_test(three, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE)
threesum$tidy$estimate <- threesum$tidy$estimate


foursum$tidy$p.value <- coef_test(four, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$p
foursum$tidy$std.error <- coef_test(four, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE
foursum$tidy$conf.low <- foursum$tidy$estimate-(1.96*coef_test(four, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE)
foursum$tidy$conf.high <- foursum$tidy$estimate+(1.96*coef_test(four, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE)
foursum$tidy$estimate <- foursum$tidy$estimate

fivesum$tidy$p.value <- coef_test(five, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$p
fivesum$tidy$std.error <- coef_test(five, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE
fivesum$tidy$conf.low <- fivesum$tidy$estimate-(1.96*coef_test(five, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE)
fivesum$tidy$conf.high <- fivesum$tidy$estimate+(1.96*coef_test(five, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE)
fivesum$tidy$estimate <- fivesum$tidy$estimate


sixsum$tidy$p.value <- coef_test(six, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$p
sixsum$tidy$std.error <- coef_test(six, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE
sixsum$tidy$conf.low <- sixsum$tidy$estimate-(1.96*coef_test(six, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE)
sixsum$tidy$conf.high <- sixsum$tidy$estimate+(1.96*coef_test(six, vcov = "CR2", cluster = pdf$CCG_Name, test = "Satterthwaite")$SE)
sixsum$tidy$estimate <- sixsum$tidy$estimate



cm <- c("lag(outsourcing_treats)" = "Treatments outsourced (%)",
        "lag(log(deflated_per_person_allocation))" = "CCG Allocation (£ per capita)",
        "lag(log(deflated_per_person_allocation_la))" = "LA Allocation (£ per capita)",
        "lag(log(deflated_per_person_benefits))" = "Benefit Allocation (£ per capita)")

rows <- tribble(~term,          ~`Treatments Outsourced (%) [.95 ci]`,  ~`p-value`,~`Treatments Outsourced (%) [.95 ci]`,  ~`p-value`,  ~`Treatments Outsourced (%) [.95 ci]`,  ~`p-value`, ~`ln. Treatable Mortality [.95 ci]`,  ~`p-value`, ~`ln. Treatable Mortality [.95 ci]`,  ~`p-value`, ~`ln. Treatable Mortality [.95 ci]`,  ~`p-value`,
                'CCG Fixed Effects', 'Yes',  'Yes', 'Yes',  'Yes','Yes',  'Yes','Yes',  'Yes','Yes',  'Yes','Yes',  'Yes',
                'Time Fixed Effects','Yes','Yes','Yes',  'Yes','Yes','Yes','Yes',  'Yes','Yes',  'Yes','Yes',  'Yes',
                'Clustered Standard Errors', 'Yes','Yes', 'Yes',  'Yes','Yes','Yes','Yes',  'Yes','Yes',  'Yes','Yes',  'Yes',
                'Control variables', 'Yes','Yes', 'Yes',  'Yes','Yes','Yes','Yes',  'Yes','Yes','Yes',  'Yes',  'Yes')


table <- modelsummary(list("Treatments Outsourced (%) [.95 ci]"=onesum,"p-value"=onesum,"Treatments Outsourced (%) [.95 ci]"=twosum,"p-value"=twosum,"Treatments Outsourced (%) [.95 ci]"=threesum,"p-value"=threesum,"ln. Treatable Mortality [.95 ci]"=foursum,"p-value"=foursum,"ln. Treatable Mortality [.95 ci]"=fivesum,"p-value"=fivesum,"ln. Treatable Mortality [.95 ci]"=sixsum,"p-value"=sixsum ),
                      coef_omit = "Intercept|dept|year", add_rows = rows, coef_map = cm,
                      fmt = 4, estimate = c("{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value"), statistic = NULL,
                      notes = list('Table reports results from multivariate longitudinal regression models.',
                                   'Robust SEs are clustered at CCG level and use a bias-reduced linearization estimator (CR2)',
                                   'Lag of one year applied to allocation variables',
                                   'Tr. mortality and allocations are log transformed, "Ln" denotes the natural log of outcome variable.',
                                   'Control variables are household income, total commissioner spend and population size'),
                      output = "gt")
# add_header_above(c(" ", "Main finding" = 2, "Controlling for Austerity" = 2, "CCG allocation interaction" = 2, "LA allocation interaction" = 2, "Benefit allocation interaction" = 2))

gtsave(table, "Regressions/paper_3/FE_treatments.html")

#### treatments plot ####

if (!require("pacman")) install.packages("pacman")

pacman::p_load(devtools, dplyr, tidyverse, tidyr, stringr,  curl, RColorBrewer)




rm(list=setdiff(ls(), c("")))


options(scipen=999)


####download datasets####
dfin <-  read.csv(curl("https://raw.githubusercontent.com/BenGoodair/NHS_privatisation_treatments/f44c3d46df8645ce7d853df84646f13b5dbf8f04/Data/Provider_admitted.csv"))
dfout <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/NHS_privatisation_treatments/f44c3d46df8645ce7d853df84646f13b5dbf8f04/Data/Provider_non_admitted.csv"))

dfin <- dfin%>%dplyr::select(-X, -nhs)
dfout <- dfout%>%dplyr::select(-X, -nhs)

names(dfout)[names(dfout)=="Total.All"] <- "Total.nonadmitted"
names(dfin)[names(dfin)=="Total.All"] <- "Total.admitted"

names(dfout)[names(dfout)=="within18"] <- "within18.nonadmitted"
names(dfin)[names(dfin)=="within18"] <- "within18.admitted"

provider_data <- merge(dfin, dfout, by=c("month", "year","Provider.Org.Code", "Provider.Org.Name", "Sector", "Treatment.Function.Name", "Treatment.Function.Code"), all=T)
provider_data[is.na(provider_data)] <- 0

provider_data$Total.All <- as.numeric(provider_data$Total.nonadmitted)+as.numeric(provider_data$Total.admitted)

provider_data$within18_all <- (((as.numeric(provider_data$within18.admitted)*as.numeric(provider_data$Total.admitted))+(as.numeric(provider_data$within18.nonadmitted)*as.numeric(provider_data$Total.nonadmitted)))/(as.numeric(provider_data$Total.All)))*100




provider_data <- provider_data%>%mutate(monthno = ifelse(provider_data$month=="january", 1,
                                                         ifelse(provider_data$month=="february", 2,
                                                                ifelse(provider_data$month=="march", 3,
                                                                       ifelse(provider_data$month=="april", 4,
                                                                              ifelse(provider_data$month=="may", 5,
                                                                                     ifelse(provider_data$month=="june", 6,
                                                                                            ifelse(provider_data$month=="july", 7,
                                                                                                   ifelse(provider_data$month=="august", 8,
                                                                                                          ifelse(provider_data$month=="september", 9,
                                                                                                                 ifelse(provider_data$month=="october", 10,
                                                                                                                        ifelse(provider_data$month=="november", 11,
                                                                                                                               ifelse(provider_data$month=="december", 12,NA)))))))))))))

provider_data$yearno <- as.double(provider_data$year)-2011

provider_data$time <- provider_data$monthno+(12*provider_data$yearno)-3

check <- unique(provider_data[c("Sector", "Provider.Org.Name")])

provider_data$Sector[provider_data$Provider.Org.Name=="TETBURY HOSPITAL TRUST LTD"] <- "Independent"
provider_data$Sector[provider_data$Provider.Org.Name=="FOSCOTE COURT (BANBURY) TRUST LTD"] <- "Independent"




rm(list=setdiff(ls(), c("provider_data")))

####Qual datavis####

col_strip <- brewer.pal(11, "RdBu")

df4 <- aggregate(.~Sector+time, data=provider_data[c("Sector", "time", "Total.All")], sum)
df4 <- df4 %>%  pivot_wider(names_from = Sector, values_from = c(Total.All))
df4$outsourcing <- (df4$Independent/(df4$NHS+df4$Independent))*100

#df4 <-df4[which(df4$time<109),]
theme_strip <- theme_minimal()+
  theme(axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
  )

warmingstripes <- ggplot(df4,
                         aes(x = time, y = 1, fill = outsourcing))+
  geom_tile()+
  labs(x="Year", y="Independent treatments (%)", title = "")+
  scale_x_continuous(breaks=c(10,22,34,46,58,70,82,94, 106, 118, 130),
                     labels=c("2012","2013",  "2014", "2015", "2016","2017", "2018", "2019", "2020", "2021", "2022"))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_gradientn(colors = rev(col_strip), name = "Independent Sector\nTreatments (%)")+
  guides(fill = guide_colorbar(barwidth = 1))+
  theme_strip+
  theme(text=element_text(size=10), legend.key.size = unit(0.9, "cm"))



my_breaks <- c(2, 3,4, 5, 6,7, 8, 9)


blog <- ggplot(data=df4, aes(x=time, y=outsourcing)) +
  
  # geom_line() +
  geom_area(fill="navyblue")+
  theme_bw()+
  geom_vline(xintercept = 108, linetype="dashed")+
  annotate("text", x=105, y=9, label=" National\nCOVID-19 lockdown", angle=90, size=3)+
  theme(legend.position="bottom",legend.key.width = unit(2, 'cm'),legend.key.size = unit(0.5, "cm"))+
  labs(x="Year", y="Private Sector\nTreatments (%)", title = "")+
  scale_x_continuous(breaks=c(10,22,34,46,58,70,82,94, 106, 118, 130),
                     labels=c("2012","2013",  "2014", "2015", "2016","2017", "2018", "2019", "2020", "2021", "2022"))

df <- provider_data

df$Total.All <- as.numeric(df$Total.All)
df$Treatment.Function.Name <- gsub(" Service", "", df$Treatment.Function.Name)

df$Treatment.Function.Name[df$Treatment.Function.Name=="Trauma and Orthopaedic"] <- "Trauma & Orthopaedics"
df$Treatment.Function.Name[df$Treatment.Function.Name=="Neurosurgical"] <- "Neurosurgery"
df$Treatment.Function.Name[df$Treatment.Function.Name=="Elderly Medicine"] <- "Geriatric Medicine"
df$Treatment.Function.Name[df$Treatment.Function.Name=="Thoracic Medicine"] <- "Respiratory Medicine"
df$Treatment.Function.Name[df$Treatment.Function.Name=="General Medicine"] <- "General Internal Medicine"

df$Treatment.Function.Name[df$Treatment.Function.Name=="ENT"] <- "Ear Nose and Throat"
df$Treatment.Function.Name[df$Treatment.Function.Name=="Ear, Nose & Throat (ENT)"] <- "Ear Nose and Throat"

df <- aggregate(.~time+Treatment.Function.Name+Sector,  data=df[c("time","Treatment.Function.Name","Sector", "Total.All")], sum)

df <- df %>%  pivot_wider(names_from = Sector, values_from = c(Total.All))
df$outsourcing <- (df$Independent/(df$NHS+df$Independent))*100

df <- df %>% mutate(Treatment.Function.Name2 = (ifelse(Treatment.Function.Name=="Gastroenterology", "Gastroenterology",
                                                       ifelse(Treatment.Function.Name=="Ear Nose and Throat", "Ear Nose and Throat",
                                                              ifelse(Treatment.Function.Name=="Trauma & Orthopaedics", "Trauma & Orthopaedics",
                                                                     ifelse(Treatment.Function.Name=="General Surgery", "General Surgery",
                                                                            ifelse(Treatment.Function.Name=="Gynaecology", "Gynaecology",
                                                                                   ifelse(Treatment.Function.Name=="Ophthalmology", "Ophthalmology", NA))))))))


df <- df[- grep("Other", df$Treatment.Function.Name),]
df <- df[- grep("Total", df$Treatment.Function.Name),]


my_breaks <- c(0, 0.5, 1,2, 5, 10,20, 30)



plot2 <- ggplot(df[complete.cases(df$Treatment.Function.Name),], aes(x=time, y=reorder(Treatment.Function.Name,outsourcing), fill=outsourcing))+
  geom_tile(color="transparent" )+theme_bw()+
  scale_fill_gradientn(colors = rev(col_strip), name = "Private Sector Treatments\n(%, logged colour scale)",trans = scales::pseudo_log_trans(sigma = 0.4, base=2), breaks=my_breaks, labels = my_breaks)+
  scale_x_continuous(breaks=c(10,22,34,46,58,70,82,94, 106, 118, 130),
                     labels=c("2012","2013",  "2014", "2015", "2016","2017", "2018", "2019", "2020", "2021", "2022"))+
  labs(x="Year", y="Treatment", fill = "NHS Treatments Outsourced\n(%)")+
  theme(axis.text.y = element_text(size=8) ,legend.key.width = unit(2, 'cm'),legend.key.size = unit(0.5, "cm"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),plot.caption = element_text(hjust=0,  size=8),
        legend.position = "bottom")




yes1 <- cowplot::plot_grid(NULL, blog,  ncol=2, rel_widths = c(0.07, 0.9))

yes <- cowplot::plot_grid(yes1, plot2, ncol=1)

ggsave("C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/austerity_and_privatisation/Figures/outsourcing_by_treatments.png", plot=yes, width=12, height=8, dpi=600)


####value of balance lost####

mean(df[df$year==2014,]$balance, na.rm=T)-mean(df[df$year==2021,]$balance, na.rm=T)


####vif####

five <- plm(log(Treatable_Mortality_Rate) ~ lag(Private_Sector_Procurement_Spend) + lag(log(deflated_per_person_benefits)) + 
                             lag(log(deflated_per_person_allocation)) + lag(log(deflated_per_person_allocation_la)) + 
                             total_spend_10millions + GDHI_per_person + CCGpop, 
                      data = pdf, method = "within", effect = "twoways")
# Function to compute VIF manually
calculate_vif <- function(model) {
   require(car)
   x <- model$model[, -1]  # Remove the intercept
   vifs <- sapply(seq_len(ncol(x)), function(i) {
     vif(lm(x[, i] ~ . - 1, data = x))
   })
   names(vifs) <- colnames(x)
   return(vifs)
 }
# Compute VIF for each variable
vif_values <- calculate_vif(five)

# View VIF values
print(vif_values)

####deleted for revisions#####
# 
# 
# lm2 <- (lm(deflated_per_person_allocation~as.double(year), data= df[which(df$year<2011),]))
# lm3 <- (lm(deflated_per_person_allocation~as.double(year), data= df[which(df$year>2009&df$year<2014),]))
# 
# val <- mean(df[which(df$year==2013),]$deflated_per_person_allocation, na.rm=T)-mean(df[which(df$year==2014),]$deflated_per_person_allocation, na.rm=T)+lm3$coefficients[2]
# 
# plot1 <- ggplot(df)+
#   stat_summary(data=df[which(df$year<2014),],aes(x=factor(year), y=deflated_per_person_allocation))+
#   stat_summary(data=df[which(df$year>2013),],aes(x=factor(year), y=deflated_per_person_allocation+val))+
#   theme_nice()+
#   labs(x="Year", y="NHS allocation pre-2013 reforms\n(?,000s per person)", title = "Austerity in England's NHS: Lower growth to commissioner's allocations",
#        caption = "Prices adjusted to 2020 values using GDP deflator\nRight-hand y-axis adjusted due to changes in the commissioner responsibilities making absolute values incomparable")+
#   geom_smooth(data =df[which(df$year<2011),],aes(group=1,x=factor(year), y=deflated_per_person_allocation), colour="red",fill="lightsalmon", method = "lm"  )+
#   geom_smooth(data =df[which(df$year>2009&df$year<2014),],aes(group=1, x=factor(year), y=deflated_per_person_allocation), colour="blue", fill="mediumpurple1", method = "lm"  )+
#   geom_smooth(data =df[which(df$year>2013),],aes(group=1, x=factor(year), y=deflated_per_person_allocation+val), colour="blue",fill="mediumpurple1", method = "lm")+
#   geom_vline(xintercept="2010", linetype="dotted")+
#   geom_segment(colour="red",linetype="dashed",aes(x = 7, y = lm2$coefficients[1]+(lm2$coefficients[2]*2010), xend = 17, yend = lm2$coefficients[1]+lm2$coefficients[2]*2020))+
#   coord_cartesian(ylim=c(1.2, 3))+
#   annotate("rect", xmin=10, xmax=11, ymin=1.1, ymax=3, alpha=0.4, fill="lightgrey") +
#   annotate("text", label = "Major NHS reforms",
#            x = 10.5, y = 2, size = 2.5, colour = "black", angle=90)+
#   annotate("text", label = "Austerity policies implemented",
#            x = 6.7, y = 1.5, size = 2.5, colour = "black", angle=90)+
#   annotate("text", label = "New Labour Governments",
#            x = 2.5, y = 1.52, size = 2.5, colour = "red", angle=35)+
#   annotate("text", label = "Pre-austerity trajectory",
#            x = 14.5, y = 2.65, size = 2.5, colour = "red", angle=35)+
#   annotate("text", label = "Con-LD Government",
#            x = 8.5, y = 1.85, size = 2.5, colour = "blue", angle=19)+
#   annotate("text", label = "Conservative Governments\n(values re-scaled to pre-2013 levels)",
#            x = 14.5, y = 2, size = 2.5, colour = "blue", angle=5)+
#   scale_y_continuous(name = "Pre-2013\n(allocation, ?,000s per person)",
#                      sec.axis = sec_axis(~.-val, name="Post-2013\n(adjusted allocation, ?,000s per person)")
#   )
# 
# 
# #ggsave("plots/paper_3/CCG_Austerity_plot.png", plot=plot1, width=10, height=8, dpi=600)


