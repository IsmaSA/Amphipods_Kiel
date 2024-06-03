################################################################
################          ELZA                  ################
################################################################


setwd("C:/Users/Propietario/Desktop/Escritorio/Elza_data")
df<- read_excel("DATA FOR ISMAEL.xlsx")
df1<- read_excel("DATA FOR ISMAEL.xlsx", sheet = "Sampling Geomar 2022")
df2<- read_excel("DATA FOR ISMAEL.xlsx", sheet = "Sampling Trave 2022")

head(df)

df$location <- "Falkestein"
df1$location <- "Geomar"
df2$location <- "Trave"

df <- df[c(1:12), c(1:16,28)]
df1 <- df1[c(1:12), c(1:14,26)]
df2 <- df2[c(1:12), c(1:11,23)]

#Remove those unidentified if you wish
df <- df[-c(15)]
df1 <- df1[-c(13)]
df2 <- df2[-c(10)]


df<- df %>%
  pivot_longer(!c(`Sampling Date`,Salinity,pH,`Water Temperature`,`Oxygen (%)`,`Sampling month`,
                  `Any Gammarids`, location), 
               names_to = "Taxa", values_to = "Abundance")


df1<- df1 %>%
  pivot_longer(!c(`Sampling Date`,Salinity,pH,`Water Temperature`,`Oxygen (%)`,`Sampling month`,
                  `Any Gammarids`, location), 
               names_to = "Taxa", values_to = "Abundance")


df2<- df2 %>%
  pivot_longer(!c(`Sampling Date`,Salinity,pH,`Water Temperature`,`Oxygen (%)`,`Sampling month`,
                  `Any Gammarids`, location), 
               names_to = "Taxa", values_to = "Abundance")



data <- bind_rows(df,df1,df2)

data$Abundance <- ifelse(is.na(data$Abundance), 0, data$Abundance)

data$origin <- ifelse(data$Taxa %in% c("Grandidierella japonica","Gammarus tigrinus"), 
                      "alien", "native")



####################################################################################
####################                 FIGURE 2                     ##################
####################################################################################
data$`Sampling month` <- factor(data$`Sampling month`, 
                                levels = c("April","May","June","July","August","September","October",
                                           "November","December","January","February","March"))

col1<- c("darkorchid2","aquamarine3","bisque3","brown2","darkgoldenrod2","forestgreen",
         "dodgerblue2","lightpink3","gray0")

col2<- c("darkorchid2","bisque3","brown2","forestgreen",
         "dodgerblue2","lightpink3","gray0")

col3<- c("aquamarine3","yellowgreen", "dodgerblue2","gray0")
unique(data$location)

p1<- ggplot(data[data$location=="Falkestein",], aes(`Sampling month`, Abundance, fill=Taxa)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.75)+ ylim(0,300)+
  theme_classic()+ theme_cleveland()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_manual(values=col1)+ guides(fill = guide_legend(reverse = TRUE))


p2<- ggplot(data[data$location=="Geomar",], aes(`Sampling month`, Abundance, fill=Taxa)) +
  geom_bar(stat = "identity", position = "dodge",width = 0.75)+ ylim(0,300)+
  theme_classic()+ theme_cleveland()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_manual(values=col2)+ guides(fill = guide_legend(reverse = TRUE))


p3<- ggplot(data[data$location=="Trave",], aes(`Sampling month`, Abundance, fill=Taxa)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.75)+
  theme_classic()+ theme_cleveland()+ ylim(0,300)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_manual(values=col3)+ guides(fill = guide_legend(reverse = TRUE))


plot_grid( p1, p2,p3,
           labels = c("a)", "b)","c)"), ncol = 1)


####################################################################################
####################                 FIGURE 3                     ##################
####################################################################################

p1<- ggplot(data[data$location=="Falkestein",], aes(`Sampling month`, Abundance, fill=Taxa)) +
  geom_bar(stat = "identity", position = "fill", width = 0.75)+ 
  theme_classic()+ theme_cleveland()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_manual(values=col1)+ guides(fill = guide_legend(reverse = TRUE))


p2<- ggplot(data[data$location=="Geomar",], aes(`Sampling month`, Abundance, fill=Taxa)) +
  geom_bar(stat = "identity", position = "fill",width = 0.75)+ 
  theme_classic()+ theme_cleveland()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_manual(values=col2)+ guides(fill = guide_legend(reverse = TRUE))


p3<- ggplot(data[data$location=="Trave",], aes(`Sampling month`, Abundance, fill=Taxa)) +
  geom_bar(stat = "identity", position = "fill", width = 0.75)+
  theme_classic()+ theme_cleveland()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_manual(values=col3)+ guides(fill = guide_legend(reverse = TRUE))


plot_grid( p1, p2,p3,
           labels = c("a)", "b)","c)"), ncol = 1)


####################################################################################
####################                 FIGURE 4                     ##################
####################################################################################

setwd("C:/Users/isma-/OneDrive/Escritorio/Elza_data")
df<- read_excel("DATA FOR ISMAEL.xlsx")
df1<- read_excel("DATA FOR ISMAEL.xlsx", sheet = "Sampling Geomar 2022")
df2<- read_excel("DATA FOR ISMAEL.xlsx", sheet = "Sampling Trave 2022")

head(df)

df$location <- "Falkestein"
df1$location <- "Geomar"
df2$location <- "Trave"

df <- df[c(1:12), c(1:16,28)]
df1 <- df1[c(1:12), c(1:14,26)]
df2 <- df2[c(1:12), c(1:11,23)]


df <- df[-c(15)]
df1 <- df1[-c(13)]
df2 <- df2[-c(10)]


df <- df[c(6:14)]
df1 <- df1[c(6:12)]
df2 <- df2[c(6:9)]


df[is.na(df)] <- 0
df1[is.na(df1)] <- 0
df2[is.na(df2)] <- 0


df$total_abundance <- rowSums(df[, -1])
df1$total_abundance <- rowSums(df1[, -1])
df2$total_abundance <- rowSums(df2[, -1])


# Calculate proportion (pi) of each species in the community for each month
amphipods_proportions <- as.data.frame(lapply(df[, -c(1, ncol(df))], 
                                              function(x) x / df$total_abundance))

amphipods_proportions1 <- as.data.frame(lapply(df1[, -c(1, ncol(df1))], 
                                               function(x) x / df1$total_abundance))

amphipods_proportions2 <- as.data.frame(lapply(df2[, -c(1, ncol(df2))], 
                                               function(x) x / df2$total_abundance))

# Calculate Simpson diversity index
simpson_index <- 1 - rowSums(amphipods_proportions ^2)
simpson_index1 <- 1 - rowSums(amphipods_proportions1 ^2)
simpson_index2 <- 1 - rowSums(amphipods_proportions2 ^2)


# Add Simpson diversity index to the original data frame
df$Simpson_Diversity_Index <- simpson_index
df1$Simpson_Diversity_Index <- simpson_index1
df2$Simpson_Diversity_Index <- simpson_index2


############ PLot boxplot #####
df$location <- "Falkestein"
df1$location <- "Geomar"
df2$location <- "Trave"



p1<- ggplot(df, aes(x=location, y=simpson_index, fill=location)) + 
  ## add half-violin from {ggdist} package
  ggdist::stat_halfeye(
    ## custom bandwidth
    adjust = .5, 
    ## adjust height
    width = .6, 
    ## move geom to the right
    justification = -.2, 
    ## remove slab interval
    .width = 0, 
    point_colour = NA
  ) + 
  geom_boxplot(
    width = .25, 
    outlier.shape = NA
  ) +
  geom_point(
    size = 1.3,
    alpha = .3,
    position = position_jitter(
      seed = 1, width = .1
    )
  ) + 
  coord_cartesian(xlim = c(1.2, NA), clip = "off") + theme_bw()+
  theme(text = element_text(size=15), legend.position="none") + ylim(0,0.7) 


p2<- ggplot(df1, aes(x=location, y=simpson_index1, fill=location)) + 
  ## add half-violin from {ggdist} package
  ggdist::stat_halfeye(
    ## custom bandwidth
    adjust = .5, 
    ## adjust height
    width = .6, 
    ## move geom to the right
    justification = -.2, 
    ## remove slab interval
    .width = 0, 
    point_colour = NA
  ) + 
  geom_boxplot(
    width = .25, 
    outlier.shape = NA
  ) +
  geom_point(
    size = 1.3,
    alpha = .3,
    position = position_jitter(
      seed = 1, width = .1
    )
  ) + 
  coord_cartesian(xlim = c(1.2, NA), clip = "off") + theme_bw()+
  theme(text = element_text(size=15), legend.position="none") + ylim(0,0.7) 


p3<- ggplot(df2, aes(x=location, y=simpson_index2, fill=location)) + 
  ## add half-violin from {ggdist} package
  ggdist::stat_halfeye(
    ## custom bandwidth
    adjust = .5, 
    ## adjust height
    width = .6, 
    ## move geom to the right
    justification = -.2, 
    ## remove slab interval
    .width = 0, 
    point_colour = NA
  ) + 
  geom_boxplot(
    width = .25, 
    outlier.shape = NA
  ) +
  geom_point(
    size = 1.3,
    alpha = .3,
    position = position_jitter(
      seed = 1, width = .1
    )
  ) + 
  coord_cartesian(xlim = c(1.2, NA), clip = "off") + theme_bw()+
  theme(text = element_text(size=15), legend.position="none") + ylim(0,0.7) 


plot_grid( p1, p2,p3,
           labels = c("a)", "b)","c)"), ncol = 3)



####################################################################################
####################                 FIGURE 5                     ##################
####################################################################################


data1<- data %>% group_by(`Sampling month`, origin, location) %>% 
  summarise(Total_abundance =sum(Abundance, na.rm=T))
data1

month_order <- c("April", "May", "June", "July", "August", "September", 
                 "October", "November", "December", "January", "February", "March")

data1$`Sampling month` <- factor(data1$`Sampling month`, levels = month_order)

data1$origin[data1$origin=="alien"] <- "non-native"
data1$origin[data1$origin=="non-native"] <- "Non-native amphipods"
data1$origin[data1$origin=="native"] <- "Native amphipods"

data1$location[data1$location=="Falkestein"] <- "Falckenstein beach"
data1$location[data1$location=="Geomar"] <- "Downtown Kiel"
data1$location[data1$location=="Trave"] <- "Dassower See"


data$origin[data$origin=="alien"] <- "non-native"
data$origin[data$origin=="non-native"] <- "Non-native amphipods"
data$origin[data$origin=="native"] <- "Native amphipods"

data$location[data$location=="Falkestein"] <- "Falckenstein beach"
data$location[data$location=="Geomar"] <- "Downtown Kiel"
data$location[data$location=="Trave"] <- "Dassower See"


location <- c("Falckenstein beach", "Downtown Kiel", "Dassower See")
data1$location <- factor(data1$location, levels = location)


data1$location <- factor(data1$location)
data$location <- factor(data$location, levels = levels(data1$location))

color_mapping <- c("Native amphipods" = "blue", "Non-native amphipods" = "red")
data <- data %>% filter(Abundance> 0)
p2<- ggplot(data1, aes(`Sampling month`, Total_abundance, group=as.factor(origin)))  +
  geom_line(aes(colour=origin), size=1) + 
  geom_point(data = data, aes(x = `Sampling month`, y = Abundance, colour = origin)) + 
   theme_cleveland()+ theme_classic2()+   scale_color_manual(values = color_mapping) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,color="black"))+
  theme(axis.text.y = element_text(color="black"))+ 
  scale_y_continuous(breaks = seq(0, max(data1$Total_abundance) + 50, by = 50))+
  theme(text = element_text(size = 12, color="black")) + 
  facet_wrap(~location) + ylab("Species abundance")+
  xlab("Sampling months")+ theme(strip.text = element_text(size = 14))

p2




####################################################################################
####################                 MODELLING PART               ##################
####################################################################################


#### Abundance ----

data1<- data %>% group_by(`Sampling month`, location) %>% 
  summarise(Total_abundance =sum(Abundance, na.rm=T))
data1


colnames(data1)[1] <- "Months"
str(data1)
data1$Months <- as.character(data1$Months)

unique(data1$Months)

data1$Months[data1$Months == "April"] <- "1"
data1$Months[data1$Months == "May"] <- "2"
data1$Months[data1$Months == "June"] <- "3"
data1$Months[data1$Months == "July"] <- "4"
data1$Months[data1$Months == "August"] <- "5"
data1$Months[data1$Months == "September"] <- "6"
data1$Months[data1$Months == "October"] <- "7"
data1$Months[data1$Months == "November"] <- "8"
data1$Months[data1$Months == "December"] <- "9"
data1$Months[data1$Months == "January"] <- "10"
data1$Months[data1$Months == "February"] <- "11"
data1$Months[data1$Months == "March"] <- "12"

str(data1)

data2 <- data1 %>% filter(Total_abundance > 0)
data2<- data1 %>% group_by(Months, location) %>% 
  summarise(Total_abundance =sum(Total_abundance, na.rm=T))




data$`Sampling Date` <- as.Date(data$`Sampling Date`, format = "%d/%m/%Y")
data$`Sampling month` <- factor(data$`Sampling month`, levels = c("April", "May", "June", "July", "August", "September",
                                                                  "October", "November", "December", "January", "February", "March"))

data$Year <- format(data$`Sampling Date`, "%Y")


abundance_data <- data %>% 
  dplyr::select(`Sampling Date`, `Sampling month`, Year, 
                location = location, Abundance = Abundance) %>%
  arrange(`Sampling Date`)

abundance_data1 <- abundance_data %>% group_by(`Sampling Date`, Year,location) %>%
  summarise(Abundance =sum(Abundance))
colnames(abundance_data1)[1] <- "Date"
abundance_data1$Date <- as.factor(abundance_data1$Date)
abundance_data1$Date <- as.Date(as.character(abundance_data1$Date))
abundance_data1$Date_num <- as.numeric(difftime(abundance_data1$Date, min(abundance_data1$Date), units = "days"))


abundance_data$location <- as.character(abundance_data$location)

model_gammarus <- list()
for (loc in unique(abundance_data$location)) {
  local_data <- filter(abundance_data, location == loc)
  local_data1 <- local_data %>% group_by(`Sampling Date`, Year) %>%
    summarise(Abundance =sum(Abundance))
  local_data1$Year <- as.factor(local_data1$Year)
  colnames(local_data1)[1] <- "Date"
  local_data1 <- dplyr::ungroup(local_data1)
  local_data1$Date <- as.factor(local_data1$Date)
  local_data1$Date <- as.Date(as.character(local_data1$Date))
  local_data1$Date_num <- as.numeric(difftime(local_data1$Date, min(local_data1$Date), units = "days"))
  
  model_gammarus[[loc]] <- gam(Abundance ~ s(Date_num, bs = "cs") + Year, 
                               data = local_data1, 
                               family = nb())
}
summary(model_gammarus[["Falckenstein beach"]])
summary(model_gammarus[["Downtown Kiel"]])
summary(model_gammarus[["Dassower See"]])

p1<- plot_model(model_gammarus[["Falckenstein beach"]], terms = "Date_num",  type = "pred")
p1 <- p1 +
  scale_x_continuous(name = "Date", breaks = c(0, 27,58,92,119,155,182,216,239,279,302,329),
                     labels = c("April", "May", "June", "July", "August", "September",  "October", "November", "December", "January", "February", "March")) +
  theme_bw() + xlab("Sampling months") + ylab("Total species abundance")+ ggtitle("")+ scale_y_continuous(breaks = seq(0, 500 + 50, by = 50))+
  theme( axis.title.x = element_text(size = 12),
         axis.title.y = element_text(size = 12),
         axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
         panel.grid = element_blank(),
         axis.text = element_text(size = 10, color="black") )
layers <- p1$layers
layers[[1]]$aes_params$colour <- "blue"  
layers[[2]]$aes_params$fill <- "royalblue1"  
p1$layers <- layers
p1 <- p1 + geom_point(data = abundance_data1[abundance_data1$location=="Falckenstein beach",], aes(x = Date_num, y = Abundance), colour = "blue")


p2<- plot_model(model_gammarus[["Downtown Kiel"]], terms = "Date_num",  type = "pred", line.color = "blue")
p2 <- p2 +
  scale_x_continuous(name = "Date", breaks = c(0, 27,58,92,119,155,182,216,239,279,302,329),
                     labels = c("April", "May", "June", "July", "August", "September",  "October", "November", "December", "January", "February", "March")) +
  theme_bw() + xlab("Sampling months") + ylab("Total species abundance")+ ggtitle("")+ scale_y_continuous(
    breaks = seq(0, 500 + 50, by = 50),
    limits = c(0, 550))+
  theme( axis.title.x = element_text(size = 12),
         axis.title.y = element_text(size = 12),
         axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
         panel.grid = element_blank(),
         axis.text = element_text(size = 10, color="black") ) 
p2$layers <- layers
p2 <- p2 + geom_point(data = abundance_data1[abundance_data1$location=="Downtown Kiel",], aes(x = Date_num, y = Abundance), colour = "blue")


p3<- plot_model(model_gammarus[["Dassower See"]], terms = "Date_num",  type = "pred")
p3 <- p3 +
  scale_x_continuous(name = "Date", breaks = c(0, 27,58,92,119,155,182,216,239,279,302,329),
                     labels = c("April", "May", "June", "July", "August", "September",  "October", "November", "December", "January", "February", "March")) +
  theme_bw() + xlab("Sampling months") + ylab("Total species abundance")+ ggtitle("")+ scale_y_continuous(
    breaks = seq(0, 500 + 50, by = 50),
    limits = c(0, 550))+
  theme( axis.title.x = element_text(size = 12),
         axis.title.y = element_text(size = 12),
         axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
         panel.grid = element_blank(),
         axis.text = element_text(size = 10, color="black") ) 
p3$layers <- layers
p3 <- p3 + geom_point(data = abundance_data1[abundance_data1$location=="Dassower See",], aes(x = Date_num, y = Abundance), colour = "blue")


# Model Richness ----

richness_data <- data %>% filter(Abundance > 0) %>%
  group_by(`Sampling Date`, location, Year) %>%
  summarize(Richness = n_distinct(Taxa), .groups = 'drop')
richness_data$Year <- as.factor(richness_data$Year)
richness_data$Date_num <- as.numeric(difftime(richness_data$`Sampling Date`,
                                              min(richness_data$`Sampling Date`),
                                              units = "days"))

richness_data$location  <- as.character(richness_data$location)
model_rich <- list()
for (loc in unique(richness_data$location)) {
  rich_data <- filter(richness_data, location == loc)
  rich_data$Date_num <- rich_data$Date_num- 1
  
  rich_data$Year <- as.factor(rich_data$Year)
  colnames(rich_data)[1] <- "Date"
  rich_data$Date <- as.factor(rich_data$Date)
  rich_data$Date <- as.Date(as.character(rich_data$Date))
  
  model_rich[[loc]] <- gam(Richness ~ s(Date_num, bs = "cs") + Year, 
                           data = rich_data, 
                           family = nb())
}

summary(model_rich[["Falckenstein beach"]])
summary(model_rich[["Downtown Kiel"]])
summary(model_rich[["Dassower See"]])



p4<- plot_model(model_rich[["Falckenstein beach"]], terms = "Date_num",  type = "pred")
p4 <- p4 +
  scale_x_continuous(name = "Date", breaks = c(0, 27,58,92,119,155,182,216,239,279,302,329),
                     labels = c("April", "May", "June", "July", "August", "September",  "October", "November", "December", "January", "February", "March")) +
  theme_bw() + xlab("Sampling months") + ylab("Total species richness")+ ggtitle("")+ scale_y_continuous(
    breaks = seq(0, 10, by = 2),
    limits = c(0, 10))+
  theme( axis.title.x = element_text(size = 12),
         axis.title.y = element_text(size = 12),
         axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
         panel.grid = element_blank(),
         axis.text = element_text(size = 10, color="black") )
p4$layers <- layers
p4 <- p4 + geom_point(data = richness_data[richness_data$location=="Falckenstein beach",], aes(x = Date_num, y = Richness), colour = "blue")

p5<- plot_model(model_rich[["Downtown Kiel"]], terms = "Date_num",  type = "pred", line.color = "blue")
p5 <- p5 +
  scale_x_continuous(name = "Date", breaks = c(0, 27,58,92,119,155,182,216,239,279,302,329),
                     labels = c("April", "May", "June", "July", "August", "September",  "October", "November", "December", "January", "February", "March")) +
  theme_bw() + xlab("Sampling months") + ylab("Total species richness")+ ggtitle("")+ scale_y_continuous(
    breaks = seq(0, 10, by = 2),
    limits = c(0, 10))+
  theme( axis.title.x = element_text(size = 12),
         axis.title.y = element_text(size = 12),
         axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
         panel.grid = element_blank(),
         axis.text = element_text(size = 10, color="black") ) 
p5$layers <- layers
p5 <- p5 + geom_point(data = richness_data[richness_data$location=="Downtown Kiel",], aes(x = Date_num, y = Richness), colour = "blue")


p6<- plot_model(model_rich[["Dassower See"]], terms = "Date_num",  type = "pred")
p6 <- p6 +
  scale_x_continuous(name = "Date", breaks = c(0, 27,58,92,119,155,182,216,239,279,302,329),
                     labels = c("April", "May", "June", "July", "August", "September",  "October", "November", "December", "January", "February", "March")) +
  theme_bw() + xlab("Sampling months") + ylab("Total species richness")+ ggtitle("")+ scale_y_continuous(
    breaks = seq(0, 10, by = 2),
    limits = c(0, 10))+
  theme( axis.title.x = element_text(size = 12),
         axis.title.y = element_text(size = 12),
         axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
         panel.grid = element_blank(),
         axis.text = element_text(size = 10, color="black") ) 
p6$layers <- layers
p6 <- p6 + geom_point(data = richness_data[richness_data$location=="Dassower See",], aes(x = Date_num, y = Richness), colour = "blue")


remotes::install_github("thomasp85/patchwork")
remotes::install_github("tidyverse/ggplot2", ref = remotes::github_pull("5592"))
p1 +p2+p3 +
  p4+p5+p6



# Model native and non-native abundance ----


data11 <- data %>% filter(origin =="native")
data12<- data %>% filter(origin=="alien")


data11 <- data11 %>% filter(Abundance > 0)
data12 <- data12 %>% filter(Abundance > 0)


data11$`Sampling Date` <- as.Date(data11$`Sampling Date`, format = "%d/%m/%Y")
data11$`Sampling month` <- factor(data11$`Sampling month`, levels = c("April", "May", "June", "July", "August", "September",
                                                                      "October", "November", "December", "January", "February", "March"))
data11$Year <- format(data11$`Sampling Date`, "%Y")


abun_data <- data11 %>% 
  dplyr::select(`Sampling Date`, `Sampling month`, Year, 
                location = location, Abundance = Abundance) %>%
  arrange(`Sampling Date`)



# non-native----
data12$`Sampling Date` <- as.Date(data12$`Sampling Date`, format = "%d/%m/%Y")
data12$`Sampling month` <- factor(data12$`Sampling month`, levels = c("April", "May", "June", "July", "August", "September",
                                                                      "October", "November", "December", "January", "February", "March"))
data12$Year <- format(data12$`Sampling Date`, "%Y")
abun_data2 <- data12 %>% 
  dplyr::select(`Sampling Date`, `Sampling month`, Year, 
                location = location, Abundance = Abundance) %>%  arrange(`Sampling Date`)
local_dat <- filter(abun_data2, location == "Trave")
local_dat1 <- local_dat %>% group_by(`Sampling Date`, Year) %>% 
  summarise(Abundance =sum(Abundance))
local_dat1$Year <- as.factor(local_dat1$Year)
colnames(local_dat1)[1] <- "Date"
local_dat1 <- dplyr::ungroup(local_dat1) %>% arrange(Date)
local_dat1$Date <- as.factor(local_dat1$Date)
local_dat1$Date <- as.Date(as.character(local_dat1$Date))
local_dat1$Date_num <- as.numeric(difftime(local_dat1$Date, min(local_dat1$Date), units = "days"))
model_abun[["Trave_alien"]] <- gam(Abundance ~ s(Date_num, bs = "cs", k=10) + Year, 
                                   data = local_dat1, 
                                   family = nb())
# ----
abun_data$location[abun_data$location=="Falkestein"] <- "Falckenstein beach"
abun_data$location[abun_data$location=="Geomar"] <- "Downtown Kiel"
abun_data$location[abun_data$location=="Trave"] <- "Dassower See"

abun_data <- abun_data %>% filter(Abundance > 0) %>%
  group_by(`Sampling Date`, location, Year) %>%
  summarize(Abundance = sum(Abundance), .groups = 'drop')
abun_data$Year <- as.factor(abun_data$Year)
abun_data$Date_num <- as.numeric(difftime(abun_data$`Sampling Date`,
                                              min(abun_data$`Sampling Date`),
                                              units = "days"))

model_abun <- list()
for (loc in unique(abundance_data$location)) {
  local_data <- filter(abun_data, location == loc)
  local_data1 <- local_data %>% group_by(`Sampling Date`, Year) %>%
    summarise(Abundance =sum(Abundance))
  local_data1$Year <- as.factor(local_data1$Year)
  colnames(local_data1)[1] <- "Date"
  local_data1 <- dplyr::ungroup(local_data1)
  local_data1$Date <- as.factor(local_data1$Date)
  local_data1$Date <- as.Date(as.character(local_data1$Date))
  local_data1$Date_num <- as.numeric(difftime(local_data1$Date, min(local_data1$Date), units = "days"))
  
  model_abun[[loc]] <- gam(Abundance ~ s(Date_num, bs = "cs", k=5) + Year, 
                           data = local_data1, 
                           family = nb())
}
summary(model_abun[["Falckenstein beach"]])
summary(model_abun[["Downtown Kiel"]])
summary(model_abun[["Dassower See"]])
summary(model_abun[["Trave_alien"]])


p7<- plot_model(model_abun[["Falckenstein beach"]], terms = "Date_num",  type = "pred")
p7 <- p7 +
  scale_x_continuous(name = "Date", breaks = c(0, 27,58,92,119,155,182,216,239,279,302,329),
                     labels = c("April", "May", "June", "July", "August", "September",  "October", "November", "December", "January", "February", "March")) +
  theme_bw() + xlab("Sampling months") + ylab("Species abundance")+ ggtitle("")+ scale_y_continuous(
    breaks = seq(0, 500 + 50, by = 50),
    limits = c(0, 550))+
  theme( axis.title.x = element_text(size = 12),
         axis.title.y = element_text(size = 12),
         axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
         panel.grid = element_blank(),
         axis.text = element_text(size = 10, color="black") )
p7$layers <- layers
layers <- p7$layers
layers[[1]]$aes_params$colour <- "blue"  
layers[[2]]$aes_params$fill <- "royalblue1"  
p7$layers <- layers
p7 <- p7 + geom_point(data = abun_data[abun_data$location=="Falckenstein beach",], aes(x = Date_num, y = Abundance), colour = "blue")


p8<- plot_model(model_abun[["Downtown Kiel"]], terms = "Date_num",  type = "pred", line.color = "blue")
p8 <- p8 +
  scale_x_continuous(name = "Date", breaks = c(0, 27,58,92,119,155,182,216,239,279,302,329),
                     labels = c("April", "May", "June", "July", "August", "September",  "October", "November", "December", "January", "February", "March")) +
  theme_bw() + xlab("Sampling months") + ylab("Species abundance")+ ggtitle("")+ scale_y_continuous(
    breaks = seq(0, 500 + 50, by = 50),
    limits = c(0, 550))+
  theme( axis.title.x = element_text(size = 12),
         axis.title.y = element_text(size = 12),
         axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
         panel.grid = element_blank(),
         axis.text = element_text(size = 10, color="black") ) 
p8$layers <- layers
layers <- p8$layers
layers[[1]]$aes_params$colour <- "blue"  
layers[[2]]$aes_params$fill <- "royalblue1"  
p8$layers <- layers
p8 <- p8 + geom_point(data = abun_data[abun_data$location=="Downtown Kiel",], aes(x = Date_num, y = Abundance), colour = "blue")

p9<- plot_model(model_abun[["Dassower See"]], terms = "Date_num",  type = "pred")
p9 <- p9 +
  scale_x_continuous(name = "Date", breaks = c(0, 27,58,92,119,155,182,216,239,279,302,329),
                     labels = c("April", "May", "June", "July", "August", "September",  "October", "November", "December", "January", "February", "March")) +
  theme_bw() + xlab("Sampling months") + ylab("Species abundance")+ ggtitle("")+ scale_y_continuous(
    breaks = seq(0, 500 + 50, by = 50),
    limits = c(0, 550))+
  theme( axis.title.x = element_text(size = 12),
         axis.title.y = element_text(size = 12),
         axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
         panel.grid = element_blank(),
         axis.text = element_text(size = 10, color="black") ) 
p9$layers <- layers
layers <- p9$layers
layers[[1]]$aes_params$colour <- "blue"  
layers[[2]]$aes_params$fill <- "royalblue1"  
p9$layers <- layers
p9 <- p9 + geom_point(data = abun_data[abun_data$location=="Dassower See",], aes(x = Date_num, y = Abundance), colour = "blue")

p10<- plot_model(model_abun[["Trave_alien"]], terms = "Date_num",  type = "pred")
p10 <- p10 +
  scale_x_continuous(name = "Date", breaks = c(0, 27,58,92,119,155,182,216,239,279,302,329),
                     labels = c("April", "May", "June", "July", "August", "September",  "October", "November", "December", "January", "February", "March")) +
  theme_bw() + xlab("Sampling months") + ylab("Species abundance")+ ggtitle("")+ scale_y_continuous(
    breaks = seq(0, 500 + 50, by = 50),
    limits = c(0, 550))+
  theme( axis.title.x = element_text(size = 12),
         axis.title.y = element_text(size = 12),
         axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
         panel.grid = element_blank(),
         axis.text = element_text(size = 10, color="black") ) 
lay <- p10$layers
lay[[1]]$aes_params$colour <- "red"  
lay[[2]]$aes_params$fill <- "indianred2"  
p10$lay <- lay
p10 <- p10 + geom_point(data = local_dat1, aes(x = Date_num, y = Abundance), colour = "red")


p7 +p8+p9+ p10










###############################################################################
###################       Test of other factors   #############################
###############################################################################


data_e<- data

data_e$`Sampling Date` <- as.Date(data_e$`Sampling Date`, format = "%d/%m/%Y")
data_e$`Sampling month` <- factor(data_e$`Sampling month`, levels = c("April", "May", "June", "July", "August", "September",
                                                                      "October", "November", "December", "January", "February", "March"))
data_e$Year <- format(data_e$`Sampling Date`, "%Y")


extra <- data_e %>% 
  dplyr::select(`Sampling Date`, `Sampling month`, Year, Salinity,`Water Temperature`,pH,`Oxygen (%)`,Abundance,
                location = location, ) %>%  arrange(`Sampling Date`)



extra2 = extra %>% group_by(`Sampling Date`, location) %>% 
  summarise(Total_abundance =sum(Abundance, na.rm=T),
            Salinity=mean(Salinity, na.rm=T), 
            Oxygen= mean(`Oxygen (%)`, na.rm=T), 
            Temperature= mean(`Water Temperature`, na.rm=T),
            pH =mean(pH, na.rm=T))

colnames(extra2)[1] <- "Date"
extra2 <- dplyr::ungroup(extra2) %>% arrange(Date)
extra2$Date <- as.factor(extra2$Date)
extra2$Date <- as.Date(as.character(extra2$Date))
extra2$Date_num <- as.numeric(difftime(extra2$Date, min(extra2$Date), units = "days"))


plots <- list()

predictors <- names(extra2[,c(4:7)])

for (loc in unique(extra2$location)) {
  extra3 <- extra2[extra2$location == loc, ]
  
  for(pred in predictors) {
    formula <- as.formula(paste("Total_abundance ~", pred))
    model <- gam(formula, data = extra3, family = nb(), method = "REML")
    summary(model)
    
    if(pred =="Salinity"){
      max_y <- 750
      x_title <- "Salinity (ppt)"
    } else if(pred =="Oxygen"){
      max_y <- 2500
      x_title <- "Oxygen (%)"
    }else if(pred =="Temperature"){
      max_y <- 1100
      x_title <- "Temperature (°C)"
    }else if(pred =="pH"){
      max_y <- 600
      x_title <- "pH"
    }
    
    
    p <- plot_model(model, terms = pred, type = "pred")
    p <- p +
      theme_bw() + xlab(x_title) + ylab("Species abundance")+ ggtitle("")+ scale_y_continuous(
        breaks = seq(0, max_y + 50, by = 100),
        limits = c(0, max_y))+
      theme( axis.title.x = element_text(size = 12),
             axis.title.y = element_text(size = 12),
             axis.text.x = element_text(vjust = 1, hjust=1),
             panel.grid = element_blank(),
             axis.text = element_text(size = 10, color="black") ) 
    layers <- p$layers
    layers[[1]]$aes_params$colour <- "blue"  
    layers[[2]]$aes_params$fill <- "royalblue1"  
    p$layers <- layers
    
    if(pred =="Salinity"){
  p <- p + geom_point(data = extra3, aes(x = Salinity, y = Total_abundance), colour = "blue")
    } else if(pred =="Oxygen"){
  p <- p + geom_point(data = extra3, aes(x = Oxygen, y = Total_abundance), colour = "blue")
    }else if(pred =="Temperature"){
  p <- p + geom_point(data = extra3, aes(x = Temperature, y = Total_abundance), colour = "blue")
    }else if(pred =="pH"){
  p <- p + geom_point(data = extra3, aes(x = pH, y = Total_abundance), colour = "blue")
    }
    
    plot_name <- paste(loc, pred, sep = "_")
    plots[[plot_name]] <- p
  }
}

plots
plots[[5]]+ plots[[9]]+ plots[[1]]  # salinity
plots[[7]]+ plots[[11]]+ plots[[3]] # Temp
plots[[6]]+ plots[[10]]+ plots[[2]] # oxygen
plots[[8]]+ plots[[12]]+ plots[[4]] # pH


plot_distribution <-  (plots[[6]] + plots[[10]] + plots[[2]])/ 
  (plots[[8]] + plots[[12]] + plots[[4]])/ 
  (plots[[5]] + plots[[9]] + plots[[1]]) / 
  (plots[[7]] + plots[[11]] + plots[[3]]) 

plot_distribution
ggsave("Figure7.svg", 
       plot = plot_distribution, 
       width = 12, height = 12, device = "svg")


############### CORRECTION #############
# note: use for each of the gam models :)
model_summary <- summary(model3)
smooth_p_values <- model_summary$s.pv
corrected_p_values <- p.adjust(smooth_p_values, method = "holm")


###### Ordination  ############

extra2
extra3 <- na.omit(extra2)

rda <- vegan::rda(extra3$Total_abundance ~ Salinity + Oxygen + Temperature + pH, data = extra3)
summary(rda)
plot(rda)
permutation_test <- anova.cca(rda, permutations = 999)
print(permutation_test
marginal_permutation_test <- anova.cca(rda, by = "margin", permutations = 999)
marginal_permutation_test

rda_scores <- scores(rda, display = "sites")
env_scores <- scores(rda, display = "bp")

rda_scores_df <- as.data.frame(rda_scores)
env_scores_df <- as.data.frame(env_scores)

rda_scores_df$location <- extra3$location

rda_plot <- ggplot(rda_scores_df, aes(x = RDA1, y = PC1, color = location)) +
  geom_point(size = 3) +
  geom_segment(data = env_scores_df, aes(x = 0, y = 0, xend = RDA1, yend = PC1),
               arrow = arrow(length = unit(0.25, "cm")), color = "black") +
  geom_text(data = env_scores_df, aes(x = RDA1, y = PC1, label = rownames(env_scores_df)),
            vjust = 1.5, hjust = 1.5, color = "black") +
  theme_bw() + geom_hline(yintercept=0, linetype="dashed", size=0.5)+ 
  geom_vline(xintercept=0, linetype="dashed", size=0.5)+ 
  labs(  x = "RDA1",  y = "PC1",
    color = "Location" ) +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10, color = "black"),
    plot.title = element_text(size = 14, hjust = 0.5)
  )
combined_plot <- ggMarginal(rda_plot, type = "histogram", bins = 30, groupColour = TRUE, groupFill = TRUE)
