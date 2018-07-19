libraries(c("ggplot2", "dplyr", "tidyr", "stringr", "ggthemes", "readr", "zoo", "WDI"))
runprofile()

# up to 2016 data
data <- read_csv("D:/Users/hiattt/Downloads/data.csv")

the.list <- data$X1[1:14]

data %>% slice(1:14) %>% gather("year", "n", 2:8) %>% transmute(Area=factor(X1, levels=the.list), Year=as.numeric(year), value=as.numeric(n)) %>% ggplot(aes(Year, value, group=Area)) + geom_line() + facet_wrap(~Area) + labs(x="", y="Average score") + theme(text=element_text(size=rel(4.5)))

# spaghetti plot
data %>% slice(1:14) %>% gather("year", "n", 2:8) %>% transmute(Area=factor(X1, levels=the.list), Year=as.numeric(year), value=as.numeric(n)) %>% ggplot(aes(Year, value, group=Area)) + geom_line() + labs(x="", y="Average score") + theme(text=element_text(size=rel(4.5)))



#throw out
ihrar <- read.csv("D:/Users/hiattt/OneDrive - World Health Organization/Work files/Requests/M & E/Annual reporting/IHR01,IHR02,IHR03,IHR04,IHR05,IHR06,IHR07,IHR08,IHR09,IHR10,IHR11,IHR12,IHR13 (1).csv", header = FALSE, stringsAsFactors = FALSE)


tops <- ihrar %>% slice(1:2) %>% gather()

tops <- data.frame(capacity=ihrar[1,], year=ihrar[2,])
                   
                   ihrar[1:2,] %>% gather()
tops


ihrar %>% gather(X,"capacity") %>% head()

###############################################

# Define country to look at
ciso <- "KOR"
cty <- n %>% filter(iso3==ciso) %>% select(country) %>% head(1) %>% as.character()


coreg <- tb %>% filter(year==2012) %>% select(country, iso3, g_whoregion)

# by country
ihrar <- read.csv("D:/Users/hiattt/OneDrive - World Health Organization/Work files/Requests/M & E/Annual reporting/IHR01,IHR02,IHR03,IHR04,IHR05,IHR06,IHR07,IHR08,IHR09,IHR10,IHR11,IHR12,IHR13 (1).csv", header = FALSE, stringsAsFactors = FALSE)

data_head <- ihrar[1,]
data_neck <- ihrar[2,]
data_hn <- paste(data_head, data_neck, sep="-")
colnames(ihrar) <- data_hn
colnames(ihrar)[1] <- "Country"
ihr <- ihrar %>% slice(3:nrow(ihrar)) %>% gather(var, value, -Country)
# colnames(ihr) <- c("Country", "var", "value")
ihrc <- ihr %>% separate(var, c("Capacity", "Year"), "-") %>% left_join(coreg, by=c(Country="country")) %>% filter(g_whoregion %in% c("SEA", "WPR"))  %>% mutate(Year= as.numeric(Year), Score=as.numeric(value), Capacity=factor(Capacity, levels=c("Legislation", "Coordination", "Surveillance", "Response",  "Preparedness", "Risk communication", "Human resources", "Laboratory", "Points of entry", "Zoonosis", "Food safety", "Chemical", "Radionuclear", "Crude average")))

# Look at specific country
ihrc %>% filter(iso3==ciso) %>% ggplot(aes(Year, Score, group=Capacity)) + geom_point(size=0.8) + geom_line(size=1) + facet_wrap(~Capacity, ncol=3) + labs(x="", title=paste("IHR annual reporting scores for", head(ihrc[ihrc$iso3==ciso, "Country"],1))) + theme(text=element_text(size=12))

ggsave(paste0("D:/Users/hiattt/delete/ARgraph_", ciso, ".png"), width = 5, height = 6)

# 2017 scores only
ihrc %>% filter(iso3==ciso, Year==2017) 
ihrc %>% filter(iso3==ciso, Year==2017) %>% arrange(-Score) 

# Compare countries
ihrc %>% filter(Year==2017, g_whoregion=="WPR", !is.na(Score)) %>% ggplot(aes(Score, iso3, group=iso3)) + geom_point() + facet_wrap(~Capacity) 



# by region
ihrar <- read.csv("D:/Users/hiattt/OneDrive - World Health Organization/Work files/Requests/M & E/Annual reporting/IHR01,IHR02,IHR03,IHR04,IHR05,IHR06,IHR07,IHR08,IHR09,IHR10,IHR11,IHR12,IHR13 (2).csv", header = FALSE, stringsAsFactors = FALSE)

data_head <- ihrar[1,]
data_neck <- ihrar[2,]
data_hn <- paste(data_head, data_neck, sep="")
colnames(ihrar) <- data_hn
colnames(ihrar)[1] <- "WHOregion"
ihr <- ihrar %>% slice(3:nrow(ihrar)) %>% gather(var, value, -WHOregion)
colnames(ihr) <- c("WHOregion", "var", "value")
ihrb <- ihr %>% separate(var, c("Capacity", "Year"), " 2") %>% mutate(Year= as.numeric(paste(2, Year, sep="")), Score=as.numeric(value)) %>% filter(WHOregion %in% c("South-East Asia", "Western Pacific"))

# plot
caps <- c("Legislation", "Coordination", "Surveillance", "Response", 
          "Preparedness", "Risk communication", "Human resources", "Laboratory", 
          "Points of entry", "Zoonosis", "Food safety", "Chemical", "Radionuclear", "Crude average")

ihrb %>% filter(Capacity %in% caps[1:6]) %>% ggplot(aes(Year, Score, group=WHOregion)) + geom_line(size=1.5) + facet_grid(Capacity~WHOregion) + labs(x="") + theme_hc() + theme(text=element_text(size=16))

ihrb %>% filter(Capacity %in% caps[7:13]) %>% ggplot(aes(Year, Score, group=WHOregion)) + geom_line(size=1) + facet_grid(Capacity~WHOregion) + labs(x="") + theme_hc() + theme(text=element_text(size=16))

# reporting
ihrc %>% filter(!is.na(Score)) %>% group_by(g_whoregion, Year) %>% summarise(Reporting=n()/13) %>% ggplot(aes(Year, Reporting, group=g_whoregion)) + geom_line(size=1) + facet_grid(~g_whoregion) + labs(x="") + theme_hc() + theme(text=element_text(size=16))

# Trend using interpolation for missing values
# data.frame(x,y) %>% arrange(x) %>% mutate(inty=na.approx(y, rule=2))
ihrc.int <- ihrc %>% group_by(g_whoregion, Country, Capacity) %>% arrange(g_whoregion, Country, Capacity, Year) %>% mutate(ScoreM=na.approx(Score, rule=2))

p1 <- ihrc.int %>% group_by(g_whoregion, Capacity, Year) %>% summarise(Scorex=mean(ScoreM))
p2 <- p1 %>% group_by(g_whoregion, Year) %>% summarise(Scorex=mean(Scorex), Capacity="Crude average") %>% transform(Capacity=factor(Capacity, levels=caps)) %>% bind_rows(p1)

p2 %>% ggplot(aes(Year, Scorex, alpha=Capacity, color=Capacity, size=Capacity), group=g_whoregion) + geom_line() + facet_grid(~g_whoregion) + labs(x="", y="Average score") + theme_hc() + theme(text=element_text(size=16)) + scale_size_manual(values=c(rep(1,13), 2)) + scale_color_manual(values=c(rep("black",13), "lime green")) 

#combined sea and wpr
p11 <- ihrc.int %>% group_by(Capacity, Year) %>% summarise(Scorex=mean(ScoreM))
p12 <- p11 %>% group_by(Year) %>% summarise(Scorex=mean(Scorex), Capacity="Crude average")  %>% transform(Capacity=factor(Capacity, levels=caps)) %>% bind_rows(p11)
p12 %>% 
  ggplot(aes(Year, Scorex, alpha=Capacity, color=Capacity, size=Capacity)) + geom_line() + labs(x="", y="Average score") + theme_hc() + theme(text=element_text(size=16)) + scale_size_manual(values=c(rep(1,13), 2)) + scale_color_manual(values=c(rep("black",13), "lime green")) 

#############################################
# JEE score analysis
jeaa <- read.csv("M:/Private/IHR/IHR M&E/JEE final report/JEE worldwide summary statistics 25May2018.csv", header = TRUE, stringsAsFactors = FALSE)

# Get World Bank country classification
inc <- WDI(extra = TRUE, end=2018) %>% filter(year==2017) %>% select(iso3=iso3c, income)

jea <- jeaa %>% left_join(inc)

# reshape data and split variable names
jec <- jea %>% filter(!is.na(Year.JEE.conducted)) %>% select(income, WHO.region, Country, iso3, P.1.1:RE.2) %>% gather(Indicator, Score, -income, -WHO.region, -Country, -iso3) %>% mutate(Area4=str_extract(Indicator, "([A-Z]|[a-z])+"), Element=str_extract(Indicator, "[0-9]+")) %>% 
  mutate(Area2=if_else(Area4 %in% c("PoE", "CE", "RE"), "Other", Area4)) %>% 
  mutate(Area=factor(Area2, levels=c("P", "D", "R", "Other"), labels = c("Prevent", "Detect", "Respond", "Other")))

# pull in file to map Indicators to names
jef <- read.csv("D:/Users/hiattt/OneDrive - World Health Organization/Work files/Requests/M & E/JEE/AR areas.csv", stringsAsFactors = FALSE) %>% select(-Area)

jeg <- jec %>% select(-Element) %>% left_join(jef) %>% mutate(Element=factor(Element, levels=unique(jef$Element)))

# summarise by Area and WHO region
jed <- jec %>% group_by(WHO.region, Area) %>% summarise(Score=mean(Score, na.rm=TRUE)) # NA's removed from UR Tanz

# Make a global summary
jee <- jec %>% group_by(Area) %>% summarise(Score=mean(Score, na.rm=TRUE)) %>% mutate(WHO.region="Global") %>% bind_rows(jed)

# summarise again, this time by element and WHO region
jei <- jeg %>% group_by(WHO.region, Area, Element) %>% summarise(Score=mean(Score, na.rm=TRUE)) # NA's removed from UR Tanz

jej <- jeg %>% group_by(Area, Element) %>% summarise(Score=mean(Score, na.rm=TRUE)) %>% mutate(WHO.region="Global") %>% bind_rows(jei)

# regional cat fight for who's the best
jee %>% ggplot(aes(Area, Score, color=WHO.region, group=WHO.region)) + geom_line(size=1.5) + theme_fivethirtyeight()
jee %>% ggplot(aes(Area, Score, fill=WHO.region, group=WHO.region)) + geom_bar(stat = "identity", position="dodge") + theme_fivethirtyeight()

### Country comparison

# Create subset with aggregate WPR and Global
jeh <- jej %>% filter(WHO.region %in% c("WPR", "Global")) %>% mutate(Entity=WHO.region)

# Plot country vs region vs global
jeg %>% filter(Country==cty) %>% group_by(Country, Area, Element) %>% summarise(Score=mean(Score, na.rm=TRUE)) %>% 
  mutate(Entity=Country) %>% bind_rows(jeh) %>% 
  ggplot(aes(Element, Score, color=Entity, group=Element)) + geom_bar(stat = "identity", position="dodge") + facet_wrap(~Area, nrow=1) + theme_fivethirtyeight() # This one's a failure

# With Areas and Elements together
jeg %>% filter(iso3==ciso) %>% group_by(Country, Area, Element) %>% summarise(Score=mean(Score, na.rm=TRUE)) %>% 
  mutate(Entity=Country) %>% bind_rows(jeh) %>% 
  ggplot(aes(paste(Area, Element, sep=": "), Score, color=Entity, group=Entity)) + geom_point(size=2) + labs(color="") + coord_flip() + geom_line()

# This time with elements only
jeg %>% filter(iso3==ciso) %>% group_by(Country, Area, Element) %>% summarise(Score=mean(Score, na.rm=TRUE)) %>% 
  mutate(Entity=Country) %>% bind_rows(jeh) %>% 
  ggplot(aes(Element, Score, color=Entity, group=Entity)) + geom_point(size=2) + labs(color="") + coord_flip() + geom_line()

# Ordered by Score for clarity
jeg %>% filter(iso3==ciso) %>% group_by(Country, Area, Element) %>% summarise(Score=mean(Score, na.rm=TRUE)) %>% 
  mutate(Entity=Country) %>% bind_rows(jeh) %>% 
  ggplot(aes(reorder(Element, Score), Score, color=Entity, group=Entity)) + geom_point(size=1) + labs(color="", x="Element") + coord_flip() + geom_line()

### Compare with only countries from the same income group
# Get the income group of the selected country
ctyincome <- jea %>% filter(iso3==ciso) %>% select(income) %>% as.character()

# summarise by income and region and Element
jek <- jeg %>% filter(income==ctyincome) %>% group_by(WHO.region, Area, Element) %>% summarise(Score=mean(Score, na.rm=TRUE)) # NA's removed from UR Tanz

jel <- jeg %>% filter(income==ctyincome) %>% group_by(Area, Element) %>% summarise(Score=mean(Score, na.rm=TRUE)) %>% mutate(WHO.region="Global") %>% bind_rows(jek)

jem <- jel %>% filter(WHO.region %in% c("WPR", "Global")) %>% mutate(Entity=WHO.region)

# Plot comparing country vs region vs global by same income

jeg %>% filter(iso3==ciso) %>% group_by(Country, Area, Element) %>% summarise(Score=mean(Score, na.rm=TRUE)) %>% 
  mutate(Entity=Country) %>% bind_rows(jem) %>% 
  mutate(Element = factor(Element, levels = rev(levels(Element)))) %>% 
  ggplot(aes(Element, Score, color=Entity, group=Entity, shape=Entity)) + geom_point(size=2) + labs(color="", shape="", x="", title=paste("JEE score comparison\n for", cty, "and other\n", ctyincome, "countries")) + coord_flip() 
ggsave(paste0("D:/Users/hiattt/delete/JEEgraph_", ciso, ".png"), width = 7, height = 6)

# Compare difference from global average
jen <- jem %>% select(-WHO.region) %>% spread(Entity, Score) %>% mutate(WPR_diff=WPR-Global)

jeo <- jeg %>% filter(iso3==ciso) %>% group_by(Country, Area, Element) %>% summarise(Score=mean(Score, na.rm=TRUE)) %>% inner_join(jen) %>% mutate(Country_diff=Score-Global, Global_diff=0) %>% gather(Entity, Difference, WPR_diff, Country_diff, Global_diff) %>% transmute(Entity=str_replace(Entity, "_diff", ""), Entity=if_else(Entity=="Country", Country, Entity), Element=Element, Difference)

jeo %>% mutate(Element = factor(Element, levels = rev(levels(Element)))) %>% 
  ggplot(aes(Element, Difference, color=Entity, group=Entity)) + geom_point(size=2) + labs(color="", shape="", x="", title=paste("JEE score difference\n for", cty, "compared to the \nglobal mean of other\n", ctyincome, "countries")) + coord_flip() 

jeo %>% filter(Entity!="WPR", Entity!="Global") %>% mutate(Element = factor(Element, levels = rev(levels(Element)))) %>% 
  ggplot(aes(Element, Difference)) + geom_bar(stat="identity") + labs(y="Difference in scores", x="", title=paste0("JEE score difference \nfor ", cty, " compared to the \nglobal mean of other\n", ctyincome, " countries")) + coord_flip() 

ggsave(paste0("D:/Users/hiattt/delete/JEEgraph2_", ciso, ".png"), width = 7, height = 6)


### Annex table
# JEE
jeb <- jea %>% filter(Country==cty) %>% select(P.1.1:RE.2) %>% gather(Indicator, Score) 

write.cb(jeb)

# AR

ihrc %>% filter(iso3==ciso) %>% select(Year, Score, Capacity) %>% spread(Year, Score) %>% write.cb()

