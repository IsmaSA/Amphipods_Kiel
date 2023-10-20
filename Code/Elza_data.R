################################################################
################          ELZA                  ################
################################################################


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


data1<- data %>% group_by(`Sampling month`, origin,location) %>% 
  summarise(Total_abundance =sum(Abundance, na.rm=T))
data1


p2<- ggplot(data1, aes(`Sampling month`, Total_abundance, group=as.factor(origin)))  +
  geom_line(aes(colour=origin), size=1) + theme_cleveland()+ theme_classic2()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  theme(text = element_text(size = 14)) + facet_wrap(~location) 

p2




####################################################################################
####################                 MODELLING PART               ##################
####################################################################################


#### Abundance ####

data1<- data %>% group_by(`Sampling month`, location) %>% 
  summarise(Total_abundance =sum(Abundance, na.rm=T))
data1


colnames(data1)[1] <- "Months"
str(data1)

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

model1 <- gam(Total_abundance ~ s(as.numeric(Months), bs = "cc"), data = data1[data1$location=="Falkestein",],
                 family = nb())

model2 <- gam(Total_abundance ~ s(as.numeric(Months), bs = "cc"), data = data1[data1$location=="Geomar",],
              family = nb())

model3 <- gam(Total_abundance ~ s(as.numeric(Months), bs = "cc"), data = data1[data1$location=="Trave",],
              family = nb())

summary(model1)
summary(model2)
summary(model3)

plot.gam(model1)
gam.check(model2)

model4 <- gam(Total_abundance ~ s(as.numeric(Months), bs = "cc", by=as.factor(location)), 
              data = data1,
              family = "nb")
summary(model4)
op <- par(mfrow=c(3,3), mar=c(4,4,1,1))
plot(model4, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", 
     seWithMean = TRUE, residuals=TRUE)
par(op)
data1$pred_abundance <- predict(model4, type = "response")

# Plot
ggplot(data1, aes(x = as.numeric(Months), y = pred_abundance, color = as.factor(location))) +
  geom_line() +
  geom_point(aes(y = Total_abundance)) +
  labs(x = "Months", y = "Total Abundance") +
  facet_wrap(~location, scales = "free_y")





p1<- plot_model(model1, terms = "Months", type = "pred")

p1<-p1+
  scale_x_continuous("New X-Axis Label", 
                     breaks = c(1, 2, 3,4,5,6,7,8,9,10,11,12), 
                     labels = c("April", "May", "June","July","August","September","October","November","December",
                                "January","February","March"))+
          theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ylim(0,300)


p2<- plot_model(model2, terms = "Months", type = "pred")

p2<-p2+
  scale_x_continuous("New X-Axis Label", 
                     breaks = c(1, 2, 3,4,5,6,7,8,9,10,11,12), 
                     labels = c("April", "May", "June","July","August","September","October","November","December",
                                "January","February","March"))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  + ylim(0,300)


p3<- plot_model(model3, terms = "Months", type = "pred")

p3<-p3+
  scale_x_continuous("Months", 
                     breaks = c(1, 2, 3,4,5,6,7,8,9,10,11,12), 
                     labels = c("April", "May", "June","July","August","September","October","November","December",
                                "January","February","March"))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ylim(0,300)


p1+p2+p3



#### Richness ####

data2<- data %>% filter(Abundance > 0 )
data2<- data2 %>% group_by(`Sampling month`, location, Taxa) %>% 
  summarise(Richness =n()) %>% group_by(`Sampling month`, location) %>% 
  summarise(Richness =n())
data2


colnames(data2)[1] <- "Months"

data2$Months[data2$Months == "April"] <- "1"
data2$Months[data2$Months == "May"] <- "2"
data2$Months[data2$Months == "June"] <- "3"
data2$Months[data2$Months == "July"] <- "4"
data2$Months[data2$Months == "August"] <- "5"
data2$Months[data2$Months == "September"] <- "6"
data2$Months[data2$Months == "October"] <- "7"
data2$Months[data2$Months == "November"] <- "8"
data2$Months[data2$Months == "December"] <- "9"
data2$Months[data2$Months == "January"] <- "10"
data2$Months[data2$Months == "February"] <- "11"
data2$Months[data2$Months == "March"] <- "12"



model1 <- gam(Richness ~ s(as.numeric(Months), bs = "cc"), data = data2[data2$location=="Falkestein",],
              family = nb())

model2 <- gam(Richness ~ s(as.numeric(Months), bs = "cc"), data = data2[data2$location=="Geomar",],
              family = nb())

model3 <- gam(Richness ~ s(as.numeric(Months), bs = "cc"), data = data2[data2$location=="Trave",],
              family = nb())


model4 <- gam(Richness ~ s(as.numeric(Months), bs = "cc", by=as.factor(location)), 
              data = data2,
              family = "nb")

summary(model1)
summary(model2)
summary(model3)
summary(model4)

p1<- plot_model(model1, terms = "Months", type = "pred")

p1<-p1+
  scale_x_continuous("New X-Axis Label", 
                     breaks = c(1, 2, 3,4,5,6,7,8,9,10,11,12), 
                     labels = c("April", "May", "June","July","August","September","October","November","December",
                                "January","February","March"))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ylim(0,7)


p2<- plot_model(model2, terms = "Months", type = "pred")

p2<-p2+
  scale_x_continuous("New X-Axis Label", 
                     breaks = c(1, 2, 3,4,5,6,7,8,9,10,11,12), 
                     labels = c("April", "May", "June","July","August","September","October","November","December",
                                "January","February","March"))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ ylim(0,7)


p3<- plot_model(model3, terms = "Months", type = "pred")

p3<-p3+
  scale_x_continuous("Months", 
                     breaks = c(1, 2, 3,4,5,6,7,8,9,10,11,12), 
                     labels = c("April", "May", "June","July","August","September","October","November","December",
                                "January","February","March"))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ylim(0,7)


p1+p2+p3



#### Native abundance  ####


data11 <- data %>% filter(origin =="native")

data12<- data %>% filter(origin=="alien")

data11<- data11 %>% group_by(`Sampling month`, location) %>% 
  summarise(Total_abundance =sum(Abundance, na.rm=T))
data11

colnames(data11)[1] <- "Months"

data11$Months[data11$Months == "April"] <- "1"
data11$Months[data11$Months == "May"] <- "2"
data11$Months[data11$Months == "June"] <- "3"
data11$Months[data11$Months == "July"] <- "4"
data11$Months[data11$Months == "August"] <- "5"
data11$Months[data11$Months == "September"] <- "6"
data11$Months[data11$Months == "October"] <- "7"
data11$Months[data11$Months == "November"] <- "8"
data11$Months[data11$Months == "December"] <- "9"
data11$Months[data11$Months == "January"] <- "10"
data11$Months[data11$Months == "February"] <- "11"
data11$Months[data11$Months == "March"] <- "12"



data12<- data12 %>% group_by(`Sampling month`, location) %>% 
  summarise(Total_abundance =sum(Abundance, na.rm=T))
data12

colnames(data12)[1] <- "Months"

data12$Months[data12$Months == "April"] <- "1"
data12$Months[data12$Months == "May"] <- "2"
data12$Months[data12$Months == "June"] <- "3"
data12$Months[data12$Months == "July"] <- "4"
data12$Months[data12$Months == "August"] <- "5"
data12$Months[data12$Months == "September"] <- "6"
data12$Months[data12$Months == "October"] <- "7"
data12$Months[data12$Months == "November"] <- "8"
data12$Months[data12$Months == "December"] <- "9"
data12$Months[data12$Months == "January"] <- "10"
data12$Months[data12$Months == "February"] <- "11"
data12$Months[data12$Months == "March"] <- "12"

data12 <- data12 %>% filter(Total_abundance>0)

model1 <- gam(Total_abundance ~ s(as.numeric(Months), bs = "cc"), 
              data = data11[data11$location=="Falkestein",],
              family = nb())

model2 <- gam(Total_abundance ~ s(as.numeric(Months), bs = "cc"), 
              data = data11[data11$location=="Geomar",],
              family = nb())

model3 <- gam(Total_abundance ~ s(as.numeric(Months), bs = "cc"), 
              data = data11[data11$location=="Trave",],
              family = nb())

model4 <- gam(Total_abundance ~ s(as.numeric(Months), bs = "cc", by=as.factor(location)), 
              data = data11,
              family = "nb")

summary(model1)
summary(model2)
summary(model3)
summary(model4)

p1<- plot_model(model3, terms = "Months", type = "pred")

p1<-p1+
  scale_x_continuous("New X-Axis Label", 
                     breaks = c(1, 2, 3,4,5,6,7,8,9,10,11,12), 
                     labels = c("April", "May", "June","July","August","September","October","November","December",
                                "January","February","March"))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ ylim(0,300)


p2<- plot_model(model2, terms = "Months", type = "pred")

p2<-p2+
  scale_x_continuous("New X-Axis Label", 
                     breaks = c(1, 2, 3,4,5,6,7,8,9,10,11,12), 
                     labels = c("April", "May", "June","July","August","September","October","November","December",
                                "January","February","March"))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ ylim(0,300)


p3<- plot_model(model3, terms = "Months", type = "pred")

p3<-p3+
  scale_x_continuous("Months", 
                     breaks = c(1, 2, 3,4,5,6,7,8,9,10,11,12), 
                     labels = c("April", "May", "June","July","August","September","October","November","December",
                                "January","February","March"))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ ylim(0,300)


p1+p2+p3



#### alien  ####

model3 <- gam(Total_abundance ~ s(as.numeric(Months), bs = "cc"), 
              data = data12[data12$location=="Trave",],
              family = nb())
summary(model3)
p3<- plot_model(model3, terms = "Months", type = "pred")

p3<-p3+
  scale_x_continuous("Months", 
                     breaks = c(1, 2, 3,4,5,6,7,8,9,10,11,12), 
                     labels = c("April", "May", "June","July","August","September","October","November","December",
                                "January","February","March"))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


p3



###############################################################################
###################       Test of other factors   #############################
###############################################################################


data1<- data %>% group_by(`Sampling month`, location) %>% 
  summarise(Total_abundance =sum(Abundance, na.rm=T),
            Salinity=mean(Salinity, na.rm=T), 
            Oxygen= mean(`Oxygen (%)`, na.rm=T), 
            Temperature= mean(`Water Temperature`, na.rm=T),
            pH =mean(pH, na.rm=T))
data1


hist(data1$Total_abundance)
hist(data1$Salinity)
hist(data1$Oxygen) #negbin
hist(data1$Temperature) #negbin
hist(data1$pH) #neg bin



colnames(data1)[1] <- "Months"


model <- gam(Total_abundance ~ s(Salinity, by = as.factor(location)), data = data1, family = "gaussian")
model1 <- gam(Total_abundance ~ s(Oxygen, by = as.factor(location)), data = data1, family = "nb")
model2 <- gam(Total_abundance ~ s(Temperature, by = as.factor(location)), data = data1, family = "nb")
model3 <- gam(Total_abundance ~ s(pH, by = as.factor(location)) , data = data1, family = "nb")

gam.check(model)
summary(model) # yes
summary(model1) #no
summary(model2) #yes Trave is different
summary(model3) # no

op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
gam.check(model)

plot_model(model, terms = "Salinity", type = "pred")

p1<- ggplot(data1[data1$location=="Falkestein", ], aes(Salinity, Total_abundance)) +
  geom_smooth(method = "gam") + theme_bw() 

p2<- ggplot(data1[data1$location=="Geomar", ], aes(pH, Total_abundance)) +
  geom_smooth(method = "gam")+ theme_bw()+ylim(0,200)

p3<- ggplot(data1[data1$location=="Trave", ], aes(Temperature, Total_abundance)) +
  geom_smooth(method = "gam")+ theme_bw() +ylim(0,200)

p1+p2+p3


# Assuming you have a GAM model object named 'model' with the smooth term s(Temperature):as.factor(location)Trave

# Generate a sequence of values for Temperature
temp_seq <- seq(min(data1$Temperature), max(data1$Temperature), length.out = 100)

# Predict the values of the smooth term for Trave level of location
pred_vals <- predict(model2, newdata = data.frame(Temperature = temp_seq, location = "Trave"), se = TRUE)

# Extract the predicted values and standard errors
pred <- pred_vals$fit
se <- pred_vals$se

# Plot the smooth function with 95% confidence interval
plot(temp_seq, pred, type = "l", lwd = 2, xlab = "Temperature", ylab = "Predicted Total_abundance", main = "Smooth Function of Temperature (Trave)")
lines(temp_seq, pred + 1.96 * se, lty = "dashed", col = "blue") # Upper 95% confidence interval
lines(temp_seq, pred - 1.96 * se, lty = "dashed", col = "blue") # Lower 95% confidence interval




############### CORRECTION #############


# Assuming 'model' is your GAM model object
model_summary <- summary(model3)

# Extract p-values for the smooth terms
smooth_p_values <- model_summary$s.pv

# Apply Holm's correction
corrected_p_values <- p.adjust(smooth_p_values, method = "holm")

print(corrected_p_values)









































### Overall 
data1<- data %>% group_by(`Sampling month`) %>% 
  summarise(Total_abundance =sum(Abundance, na.rm=T))
data1
unique(data1$`Sampling month`)
data1$`Sampling month` <- factor(data1$`Sampling month`, 
                                 levels = c("April","May","June","July","August","September","October",
                                            "November","December","January","February","March"))

p1<-ggplot(data1, aes(`Sampling month`, Total_abundance, group=1))  +
  geom_line(size=1)+ theme_cleveland()+ theme_classic2()+ 
   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  theme(text = element_text(size = 14)) 


data1<- data %>% group_by(`Sampling month`, origin) %>% 
  summarise(Total_abundance =sum(Abundance, na.rm=T))
data1

unique(data1$`Sampling month`)
data1$`Sampling month` <- factor(data1$`Sampling month`, 
                    levels = c("April","May","June","July","August","September","October",
                               "November","December","January","February","March"))


p2<- ggplot(data1, aes(`Sampling month`, Total_abundance, group=as.factor(origin)))  +
  geom_line(aes(colour=origin), size=1) + theme_cleveland()+ theme_classic2()+
   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  theme(text = element_text(size = 14))  

p1+p2 


####Split in locations
data1<- data %>% group_by(`Sampling month`, location) %>% 
  summarise(Total_abundance =sum(Abundance, na.rm=T))
data1

p1 <- ggplot(data1, aes(`Sampling month`, Total_abundance, group=1))  +
  geom_line(size=1)+ theme_cleveland()+ theme_classic2()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  theme(text = element_text(size = 14)) + facet_wrap(~location)


data1<- data %>% group_by(`Sampling month`, origin,location) %>% 
  summarise(Total_abundance =sum(Abundance, na.rm=T))
data1


p2<- ggplot(data1, aes(`Sampling month`, Total_abundance, group=as.factor(origin)))  +
  geom_line(aes(colour=origin), size=1) + theme_cleveland()+ theme_classic2()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  theme(text = element_text(size = 14)) + facet_wrap(~location) 

p1+p2 



########### Gam models
colnames(data1)[1] <- "Months"
str(data1)

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

gam_model <- gam(Total_abundance ~ s(as.numeric(Months), bs = "cc"), data = data1,
                 family = nb())

# Visualize the fitted model
plot(gam_model, pages = 1, rug = FALSE)

op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
gam.check(gam_model)
par(op)
summary.gam(gam_model)
op <- par(mfrow=c(3,3), mar=c(4,4,1,1))
plot(gam_model, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
par(op)


########### Check the variability of the predictors
data <- bind_rows(df,df1,df2)

data$Abundance <- ifelse(is.na(data$Abundance), 0, data$Abundance)


fit <- lm(Abundance ~ Salinity + pH + `Water Temperature`+ `Oxygen (%)`, data = data)

vif_values <- car::vif(fit)
vif_values

######### Model selection
boxplot(data0$Salinity)
boxplot(data0$`Water Temperature`)
boxplot(data0$pH)
boxplot(data0$`Oxygen (%)`)

predictors <- as.matrix(data0[, c("Salinity", "pH","Water Temperature","Oxygen (%)")])

data0 <- data[,c(2:8,10)]
colnames(data0)[2] <- "Water_temp"
colnames(data0)[4] <- "Oxygen"
colnames(data0)[5] <- "Months"

{
  data0$Months[data0$Months == "April"] <- "1"
  data0$Months[data0$Months == "May"] <- "2"
  data0$Months[data0$Months == "June"] <- "3"
  data0$Months[data0$Months == "July"] <- "4"
  data0$Months[data0$Months == "August"] <- "5"
  data0$Months[data0$Months == "September"] <- "6"
  data0$Months[data0$Months == "October"] <- "7"
  data0$Months[data0$Months == "November"] <- "8"
  data0$Months[data0$Months == "December"] <- "9"
  data0$Months[data0$Months == "January"] <- "10"
  data0$Months[data0$Months == "February"] <- "11"
  data0$Months[data0$Months == "March"] <- "12"
  } #change names of the months


data00<- data0 %>% group_by(Months, location) %>% summarise(Abundance=sum(Abundance),
                                         Salinity = mean(Salinity),
                                         pH= mean(pH),
                                         Water_temp= mean(Water_temp),
                                         Oxygen=mean(Oxygen, na.rm=T)
                                         )

hist(data00$Abundance)
mean(data00$Abundance)

data00$location<- as.factor(data00$location)
gam_model <- gam(Abundance ~   s(as.numeric(Months), bs = "cc") + 
                               s(location, bs="re"), 
                               data = data00, family = nb())

anova(gam_model)
summary(gam_model)

gam_model1 <- gam(Abundance ~  s(Salinity)+s(location, bs="re")+s(as.numeric(Months), bs = "cc"), 
                 data = data00, family = nb())  # nada per casi
summary(gam_model1) #location 23.7%

gam_model2 <- gam(Abundance ~   s(pH) +s(location, bs="re")+s(as.numeric(Months), bs = "cc"),  ### nada, malisimo
                 data = data00, family = nb())
summary(gam_model2) #Months and residual location 24.4%

gam_model3 <- gam(Abundance ~     s(Water_temp)+ +s(location, bs="re")+s(as.numeric(Months), bs = "cc"),   ### nada 
                 data = data00, family = nb())
summary(gam_model3) #nada; 23.2%

gam_model4 <- gam(Abundance ~   s(Oxygen)+ s(location, bs="re")+s(as.numeric(Months), bs = "cc"), ### nada el peor modelo
                 data = data00, family = nb())
summary(gam_model4) #nada 18.8%


AIC(gam_model1,  gam_model2,  gam_model3,  gam_model4) #Modelo 4, pero es basura, asi que 3 y 1 



######  delta AIC
models <- list(gam_model1,  gam_model2,  gam_model3,  gam_model4)
aics <- sapply(models, AIC)
delta_aic <- aics - min(aics)
delta_aic

### plot 
op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
p1<- plot.gam(gam_model1, select = 1, scheme = 1, rug = TRUE,main = "Model1_Salinity")
p2<- plot.gam(gam_model2, select = 1, scheme = 1, rug = TRUE,main = "Model2_pH")
p3<- plot.gam(gam_model3, select = 1, scheme = 1, rug = TRUE,main = "Model3_Water_temp")
p4<-  plot.gam(gam_model4, select = 1, scheme = 1, rug = TRUE,main = "Model4_Oxygen")



####### For each location
unique(data00$location)

data00_Falk <- data00 %>% filter(location=="Falkestein")
data00_Geo<- data00 %>% filter(location=="Geomar")
data00_Trave<- data00 %>% filter(location=="Trave")


gam_model1 <- gam(Abundance ~  s(as.numeric(Months), bs = "cc"), 
                  data = data00_Falk, family = nb())  
summary(gam_model1) #nada 3.64%


gam_model2 <- gam(Abundance ~  s(as.numeric(Months), bs = "cc"), 
                  data = data00_Geo, family = nb())  
summary(gam_model2) #nada 0.001%

gam_model3 <- gam(Abundance ~  s(as.numeric(Months), bs = "cc"), 
                  data = data00_Trave, family = nb())  
summary(gam_model3) #nada 20.5%



### Effect of Alien species

data_alien<- data %>% group_by(`Sampling month`, location, origin) %>% 
  summarise(Abundance=sum(Abundance),
         Salinity = mean(Salinity),
          pH= mean(pH),
         Water_temp= mean(`Water Temperature`),
         Oxygen=mean(`Oxygen (%)`, na.rm=T)
)


colnames(data_alien)[1]<- "Months"
{
  data_alien$Months[data_alien$Months == "April"] <- "1"
  data_alien$Months[data_alien$Months == "May"] <- "2"
  data_alien$Months[data_alien$Months == "June"] <- "3"
  data_alien$Months[data_alien$Months == "July"] <- "4"
  data_alien$Months[data_alien$Months == "August"] <- "5"
  data_alien$Months[data_alien$Months == "September"] <- "6"
  data_alien$Months[data_alien$Months == "October"] <- "7"
  data_alien$Months[data_alien$Months == "November"] <- "8"
  data_alien$Months[data_alien$Months == "December"] <- "9"
  data_alien$Months[data_alien$Months == "January"] <- "10"
  data_alien$Months[data_alien$Months == "February"] <- "11"
  data_alien$Months[data_alien$Months == "March"] <- "12"
} #change names of the months
str(data_alien)


colnames(data_alien)

data_alien$origin <- as.factor(data_alien$origin)
data_alien$Months <- as.numeric(data_alien$Months)

gam_alien <- gam(Abundance ~  s(Months, bs = "cc")+ 
                 s(location, bs="re")+ 
                  origin, 
                  data = data_alien, family = nb()   )  
summary(gam_alien) #nada 3.64%
str(data_alien)













###############################################################################
###################               GLM             #############################
###############################################################################

data1<- data %>% group_by(`Sampling month`, location) %>% 
  summarise(Total_abundance =sum(Abundance, na.rm=T))
data1
unique(data1$`Sampling month`)
data1$`Sampling month` <- factor(data1$`Sampling month`, 
                                 levels = c("April","May","June","July","August","September","October",
                                            "November","December","January","February","March"))


hist(data1$Total_abundance)


unique(data1$location)
colnames(data1)[1] <- "month"

model1 <- glm.nb(Total_abundance ~ sin(2 * pi * (as.numeric(month) / 12)),
                data = data1[data1$location=="Falkestein",])
summary(model1)

model2 <- glm.nb(Total_abundance ~ sin(2 * pi * (as.numeric(month) / 12)),
                 data = data1[data1$location=="Geomar",])
summary(model2)

model3 <- glm.nb(Total_abundance ~ sin(2 * pi * (as.numeric(month) / 12)),
                 data = data1[data1$location=="Trave",])
summary(model3)



p1<- plot_model(model1, type = "pred", terms = "month") + theme_sjplot()
p2<- plot_model(model2, type = "pred", terms = "month") + theme_sjplot()
p3<- plot_model(model3, type = "pred", terms = "month") + theme_sjplot()

p1+p2+p3



data2<- data %>% filter(Abundance > 0 )
data2<- data2 %>% group_by(`Sampling month`, location, Taxa) %>% 
  summarise(Richness =n()) %>% group_by(`Sampling month`, location) %>% 
  summarise(Richness =n())
data2

colnames(data2)[1] <- "month"

hist(data2$Richness)
data2<- data2 %>% drop_na()

{
  data2$month[data2$month == "April"] <- "1"
  data2$month[data2$month == "May"] <- "2"
  data2$month[data2$month == "June"] <- "3"
  data2$month[data2$month == "July"] <- "4"
  data2$month[data2$month == "August"] <- "5"
  data2$month[data2$month == "September"] <- "6"
  data2$month[data2$month == "October"] <- "7"
  data2$month[data2$month == "November"] <- "8"
  data2$month[data2$month == "December"] <- "9"
  data2$month[data2$month == "January"] <- "10"
  data2$month[data2$month == "February"] <- "11"
  data2$month[data2$month == "March"] <- "12"
} #change names of the months
data21<- data2 %>% filter(location=="Falkestein")
model11 <- glm.nb(Richness ~ sin(2 * pi * (as.numeric(month) / 12)),
                 data = data21)
summary(model11)

model22 <- glm.nb(Richness ~ sin(2 * pi * (as.numeric(month) / 12)),
                 data = data2[data2$location=="Geomar",])
summary(model22)

model33 <- glm.nb(Richness ~ sin(2 * pi * (as.numeric(month) / 12)),
                 data = data2[data2$location=="Trave",])
summary(model33)



p11<- plot_model(model11, type = "pred", terms = "month") + theme_sjplot()
p22<- plot_model(model22, type = "pred", terms = "month") + theme_sjplot()
p33<- plot_model(model33, type = "pred", terms = "month") + theme_sjplot()

p11+p22+p33


#######3 Abundance native vs alien 

data0 <- data %>% filter(origin=="native")
data00 <- data %>% filter(origin=="alien")


data1<- data0 %>% group_by(`Sampling month`, location) %>% 
  summarise(Total_abundance =sum(Abundance, na.rm=T))
data1
unique(data1$`Sampling month`)
data1$`Sampling month` <- factor(data1$`Sampling month`, 
                                 levels = c("April","May","June","July","August","September","October",
                                            "November","December","January","February","March"))


hist(data1$Total_abundance)


unique(data1$location)
colnames(data1)[1] <- "month"

model1 <- glm.nb(Total_abundance ~ sin(2 * pi * (as.numeric(month) / 12)),
                 data = data1[data1$location=="Falkestein",])
summary(model1)

model2 <- glm.nb(Total_abundance ~ sin(2 * pi * (as.numeric(month) / 12)),
                 data = data1[data1$location=="Geomar",])
summary(model2)

model3 <- glm.nb(Total_abundance ~ sin(2 * pi * (as.numeric(month) / 12)),
                 data = data1[data1$location=="Trave",])
summary(model3)



p1<- plot_model(model1, type = "pred", terms = "month") + theme_sjplot()
p2<- plot_model(model2, type = "pred", terms = "month") + theme_sjplot()
p3<- plot_model(model3, type = "pred", terms = "month") + theme_sjplot()

p1+p2+p3





data1<- data00 %>% group_by(`Sampling month`, location) %>% 
  summarise(Total_abundance =sum(Abundance, na.rm=T))
data1
unique(data1$`Sampling month`)
data1$`Sampling month` <- factor(data1$`Sampling month`, 
                                 levels = c("April","May","June","July","August","September","October",
                                            "November","December","January","February","March"))


hist(data1$Total_abundance)


unique(data1$location)
colnames(data1)[1] <- "month"

model1 <- glm.nb(Total_abundance ~ sin(2 * pi * (as.numeric(month) / 12)),
                 data = data1[data1$location=="Falkestein",])
summary(model1)

model2 <- glm.nb(Total_abundance ~ sin(2 * pi * (as.numeric(month) / 12)),
                 data = data1[data1$location=="Geomar",])
summary(model2)

model3 <- glm.nb(Total_abundance ~ sin(2 * pi * (as.numeric(month) / 12)),
                 data = data1[data1$location=="Trave",])
summary(model3)



p1<- plot_model(model1, type = "pred", terms = "month") + theme_sjplot()
p2<- plot_model(model2, type = "pred", terms = "month") + theme_sjplot()
p3<- plot_model(model3, type = "pred", terms = "month") + theme_sjplot()

p1+p2+p3






