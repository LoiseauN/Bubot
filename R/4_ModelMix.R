
###########################################################################################
#################### start to do some mix model analyses ##################################
###########################################################################################

library("lme4")
library("plyr")
############################### GLM mixte ########################################

#At the video scale


# combine the data in a dataframe to make it easier
number.dat=data.frame(number.ind,
                      depth=as.numeric(species.site.matrix$site.data$depth),
                      habitat.substrat=habit.mca$ind$coord[,2],
                      habitat.bicenose=habit.mca$ind$coord[,1],
                      Island=island.name,
                      site=site.name,
                      replicate=species.site.matrix$site.data$Sample.code,
                      subreplicate=species.site.matrix$site.data$Sample.name)


number.dat <- merge(hab_pc,number.ind)
#################################################################
############### cleaning dataset ################################
#################################################################

####### check replication with count function
# summary number of replicates
repl=count(number.dat$replicate)
# get a list of sample with replication
to.select=repl[!repl$freq==1,1]

# remove sample without replication
new.number.dat=number.dat[!is.na(match(number.dat$replicate,to.select)),]

# remove juan de nova data
new.number.dat=new.number.dat[!new.number.dat$Island=="Juan_de_nova",]
new.number.dat$Island


############### do LMM or GLMM ########################
library(nlme)

hist(new.number.dat$number.ind)
hist(log10(new.number.dat$number.ind))

#create a model with all effects and interaction
individuals.full=lme(log10(number.ind)~depth*Island*habitat.substrat*habitat.bicenose, random= ~1|site/replicate,data=new.number.dat)

#create a model without interactions
individuals.full.no.inter=lme(log10(number.ind)~depth+Island+habitat.substrat+habitat.bicenose, random= ~1|site/replicate,data=new.number.dat)

# plot residuals of full model
plot(individuals.full.no.inter)

# plot qqplot
qqnorm(residuals(individuals.full.no.inter))
qqline(residuals(individuals.full.no.inter))

# plot hist of residuals
hist(residuals(individuals.full.no.inter))

# collect AIC
sort(c(full.inter=summary(individuals.full)$AICtab[1],
       full=summary(individuals.full.no.inter)$AICtab[1]))

# test for significance for interaction
anova(individuals.full,individuals.full.no.inter)

## do some plotting to check assumptions
# plot residuals of full model
plot(fitted(individuals.full),residuals(individuals.full))














Morais_fam_plot = Morais_fam %>%
  merge(Morais,by="Family") %>%
  #Calculating temperature corrected and mass corrected logK
  mutate(TempCorrectLogK = logK - SlopeInvTkb * InvTkb,
         TempStandardLogK = logK - SlopeInvTkb * InvTkb + SlopeInvTkb * mean(InvTkb),
         MassCorrectLogK = logK - SlopeLogMmax * logMmax,
         MassStandardLogK = logK - SlopeLogMmax * logMmax * mean(logMmax)) %>%
  #max and min logMmax and InvTkb for all fish
  mutate(MaxlogMmaxFixe = max(logMmax),
         MinlogMmaxFixe = min(logMmax),
         MaxInvTkbFixe = max(InvTkb),
         MinInvTkbFixe = min(InvTkb),
         MeanInvTkb = mean(InvTkb),
         MeanlogMmax = mean(logMmax))%>%
  #Fixed effects for all fish according to the fish level (from Morais_fish Rdata)
  mutate(InterceptFixe = 20.11202,
         SlopelogMmaxFixe = -0.2305791,
         SlopeInvTkbFixe = 	-0.5000861) %>%
  #grouping by family and calculating min and max logMmax and InvTkb for each family
  group_by(Family) %>%
  mutate(MaxlogMmax = max(logMmax),
         MinlogMmax = min(logMmax),
         MaxInvTkb = max(InvTkb),
         MinInvTkb = min(InvTkb))%>%
  ungroup()

# make the temperature-corrected (growth is corrected by temperature, 
#it is a growth rate independant of temperature, it is a residual) plot


nth_element <- function(vector, starting_position, n) { 
  vector[seq(starting_position, length(vector), n)] 
}
coli <-sample(nth_element(luv_colours$col, 1, 13)[-c(1,51)])



p.temperature.corrected = ggplot(Morais_fam_plot, aes(x=logMmax, y=TempCorrectLogK, colour=Family)) +
  geom_point(size=1) +
  scale_colour_manual(values = coli)+
  labs(title="logMmax predictor effect", x="logMmax",y="Temperature-corrected logK") +
  geom_segment(aes(x=MinlogMmax, xend=MaxlogMmax, y=Intercept + SlopeLogMmax*MinlogMmax, 
                   yend=Intercept + SlopeLogMmax*MaxlogMmax,size="Family (random effect)")) +
  geom_segment(aes(x=MinlogMmaxFixe, xend=MaxlogMmaxFixe, 
                   y=InterceptFixe + SlopelogMmaxFixe*MinlogMmaxFixe, 
                   yend=InterceptFixe + SlopelogMmaxFixe*MaxlogMmaxFixe,
                   size="Fish in general (fixed effect)"),colour="black") +
  scale_size_manual(name="Predictions", values=c("Family (random effect)"=0.7, "Fish in general (fixed effect)"=1.2))+
  
  theme_bw(base_size=10) +
  theme(
    plot.title = element_text(color="black", size=25, face="bold.italic",hjust=0.5),
    axis.title.x = element_text(color="black", size=14, face="bold"),
    axis.title.y = element_text(color="black", size=14, face="bold")
  ) +
  labs(title="Temperature predictor effect",
       x='1/kT',
       y="Mass-corrected log K")

