pkgs <- c('reshape2','mFD','viridis','data.table','ggplot2')
nip <- pkgs[!(pkgs %in% installed.packages())]
nip <- lapply(nip, install.packages, dependencies = TRUE)
ip   <- unlist(lapply(pkgs, require, character.only = TRUE, quietly = TRUE))

# Plot alpha
ind <- c("sp_richn","fdis","fmpd","fnnd","feve","fric","fdiv","fori","fspe",
         "biomass","hill_taxo_entropy","hill_fonct_richess",
         "hill_fonct_entropy","hill_phylo_richess","hill_phylo_entropy")

df <- reshape2::melt(alpha_div_all, id.vars="depth")
df$value <- as.numeric(df$value)
df <- df[df$variable %in% ind,]

ggplot(df,aes(x = depth, y = value, color = variable )) +
  geom_point() +
  facet_wrap(~ variable, scales = "free") +
  theme_bw()+
  stat_smooth()+
    theme(legend.position = "none")


###PLot Beta




df <- reshape2::melt(beta_hill[,-c(1,2)], id.vars="diff_depth")
df$value <- as.numeric(df$value)


ggplot(df,aes(x = diff_depth, y = value, color = variable )) +
  geom_point() +
  facet_wrap(~ variable, scales = "free",ncol = 2) +
  theme_bw()+
  stat_smooth()+
  theme(legend.position = "none")

