#> Info ====
# Author: Tristan Lafont Rapnouil
# Co-authors: Anne-Sophie Benoiston, Jocelyn Cazal, Régis Céréghino,
#             Eliane Louisana, Heidy Schimann, Marie Simonin, 
#             Lucie Zinger, Céline Leroy
# Year: 2026
# Goal: Analysis and figures for Holobiont Under Stress paper

#> Packages ====

easypackages::libraries(c("dplyr","magrittr","ggplot2",
                          "phyloseq","microeco","patchwork",
                          "ggvegan","vegan","mgcv"))

#> Custom functions ====

#Get pairwise comparison letters
source("custom_fct/pairwise_letterZ.R")
#A function to ad compact letter significance display on a ggplot boxplot graph
source("custom_fct/cld_bxplot_wilcox.R")


#> Setup & data ====

#set treatments colors
trtmt_col <- c("R-W"="#1B7939", 
               "I-W"="#5CAE63",
               "P-W"="#D9F1D5",
               "P-D"="#E5D4E8",
               "I-D"="#C3A4CF",
               "R-D"="#742881") 
#set variable categories colors
var_cat_col <- c("biomass"="burlywood3", 
                 "nutrient"="darkcyan",
                 "photosynthesis"="darkolivegreen4",
                 "structural"="chocolate3",
                 "water"="darkslateblue",
                 "reproduction"="yellow3") 

## Soil Chemistry ====

#load
soil_chi <- read.csv2("data/Chimie_sols.csv") 
#rename
colnames(soil_chi) <- c("ID","Mass","N_pct","C_pct","N_mg","C_mg","ConN","P_mgkg","pH_h2o") #rename
#set treatments label
soil_chi$trmt <- c(rep("P",3),rep("I",3),rep("R",3)) 
#transform to numeric
soil_chi%<>% mutate(across(2:9, as.numeric)) 
#set factor levels order (poor to rich)
soil_chi %<>% mutate(trmt = forcats::fct_relevel(trmt,"P","I","R")) 

## Microbial data ====
# Create variable to loop over
datasets <- c("micro_16s","micro_its")

# Get cleaned microdata
micro_16s <- readRDS("./data/micro_data_16s.rds")
micro_its <- readRDS("./data/micro_data_its.rds")

# Create hellinger transformed communities
for(i in datasets){
    data_tmp <- get(i)
    data_tmp <- clone(data_tmp) # break link between data_tmp and og file
    rownames(data_tmp$sample_table) <- data_tmp$sample_table$sample_id
    data_tmp$tidy_dataset()
    data_tmp$otu_table <- t(labdsv::hellinger(t(data_tmp$otu_table)))
    data_tmp$sample_table %<>% mutate(nb_reads = colSums(data_tmp$otu_table),
                                      nb_motus = colSums(data_tmp$otu_table>0))
    assign(paste0(i,"_hell"),data_tmp) 
}


## Leaf traits ====

#load
data_leaf <- read.csv("data/data_art1.csv")
#treatment as factor
data_leaf$Trtmt <- as.factor(data_leaf$Trtmt)
#change treatment code
levels(data_leaf$Trtmt) <- c("R-D","I-D","P-D","R-W","I-W","P-W") 
#define stress order of treatments 
stress_order <- data_leaf %>%
    group_by(Trtmt)%>%
    summarise(mean_Flowers=mean(Flowers))%>%
    arrange(desc(mean_Flowers))
#Use stress order to order levels of treatment
#in leaf data
data_leaf %<>% 
    mutate(Trtmt = forcats::fct_relevel(Trtmt,as.vector(stress_order$Trtmt))) # change level order to increasing stress (decreasing fitness)
#in 16s
micro_16s$sample_table %<>%
    mutate(Trtmt=forcats::fct_relevel(Trtmt,as.vector(stress_order$Trtmt)))
#in ITS
micro_its$sample_table %<>%
    mutate(Trtmt=forcats::fct_relevel(Trtmt,as.vector(stress_order$Trtmt)))
rm(stress_order)

#Compute Leaf Mass Area
data_leaf %<>%
    mutate(LMA = DM/(((8*pi*((8)/2)^2))*10^-6)) # Leaf surface = 8 disk of 8 mm diameter => 8disks*pi*((diam/2)^2) * 10^-6 to get m^2

## Sample sizes ====

dl <- data_leaf%>%
    group_by(Trtmt)%>%
    summarise(sample_size=n(),
              data_type="Phenotype") 
db <- micro_16s$sample_table%>%
    group_by(Trtmt)%>%
    summarise(sample_size=n(),
              data_type="16s") 
df <- micro_its$sample_table%>%
    group_by(Trtmt)%>%
    summarise(sample_size=n(),
              data_type="ITS")
d <- rbind(dl,db)%>%
    rbind(df)

ss_summary <- d %>%
    ggplot(aes(x=Trtmt,y=data_type,fill=Trtmt,label=sample_size))+
    geom_tile(color="white")+
    geom_text()+
    ggpubr::theme_classic2()+
    scale_fill_manual(values=trtmt_col)+
    theme(axis.title = element_blank(),
          legend.position = "none",
          text=element_text(face="bold"),
          plot.title = element_text(hjust=.5,size=12))+
    ggtitle("Sample size")+
    scale_y_discrete(expand = expansion())+
    scale_x_discrete(expand = expansion())

## Set up trait names ====

#list of used traits
trait_list_glob <- c("Leaves", 
                     "Pl_H",
                     "LL_H",
                     "LL_W",
                     "LMA",
                     "LDMC",
                     "mu_thick",
                     "sto_dens",
                     "chloro",
                     "FvFm",
                     "ETRmax",
                     "Water_pot",
                     "RWC",
                     "C.N_leaf",
                     "SS_leaf",
                     # "Starch_leaf",  #Remove starch 'cause detection limit
                     "Cmg",
                     "Nmg",
                     "Pmg",
                     "Flowers")

#get corresponding column indices
Nbcol <- match(trait_list_glob,colnames(data_leaf))

#create clean names used for plots
trait_df_name <- data.frame(rawnames=trait_list_glob,
                            cleannames=c('Leaves',
                                         'Plant height',
                                         'Leaf length',
                                         'Leaf width',
                                         'LMA', 
                                         'LDMC',
                                         'Thickness',
                                         'Stomata',
                                         'Chlorophyll',
                                         'Fv/Fm',
                                         'ETRmax',
                                         'Water potential',
                                         'RWC',
                                         'C/N',
                                         'Soluble sugar',
                                         'Carbon',
                                         'Nitrogen',
                                         'Phosphorus',
                                         "Flowers")) 

# write units used for plots legends
trait_df_name$unit <- c(bquote(""),
                        bquote("(cm)"),
                        bquote("(cm)"),
                        bquote("(cm)"),
                        bquote("(g.m"^-2~")"),
                        bquote("(g.g"^1~")"),
                        bquote("(mm)"),
                        bquote("(mm"^2~")"),
                        bquote("(mg.g"^-1~")"),
                        bquote(""),
                        bquote("(µmol.m"^-2~".s"^-1~")"),
                        bquote("(MPa)"),
                        bquote("(%)"),
                        bquote(""),
                        bquote("(µg.mg"^-1~")"),
                        bquote("(mg.g"^-1~")"),
                        bquote("(mg.g"^-1~")"),
                        bquote("(mg.g"^-1~")"),
                        bquote(""))

# set variable categories
trait_df_name$var_cat <- as.factor(c("biomass", 
                                     "biomass",
                                     "biomass",
                                     "biomass",
                                     "structural",
                                     "structural",
                                     "structural",
                                     "structural",
                                     "photosynthesis",
                                     "photosynthesis",
                                     "photosynthesis",
                                     "water",
                                     "water",
                                     "nutrient",
                                     "nutrient",
                                     "nutrient",
                                     "nutrient",
                                     'nutrient',
                                     'reproduction'))

# also store colors
trait_df_name%<>%
    left_join(data.frame(var_cat=c("biomass", 
                                   "nutrient",
                                   "photosynthesis",
                                   "structural",
                                   "water",
                                   "reproduction",
                                   "otus"),
                         colorz=c("burlywood3",
                                  "darkcyan",
                                  "darkolivegreen4",
                                  "chocolate3",
                                  "darkslateblue",
                                  "yellow3",
                                  "darkred")))

#> Plots & Stats ====

## Soil chemistry ====

# Soil Nitrogen
N <- ggplot(soil_chi,aes(x=trmt,y=N_mg/Mass, color=trmt))+ 
    geom_point(size=3,alpha=.8)+
    scale_color_manual(values=c("#ffe1b0","#6b5e49","#0f0d0a"), labels=c("Poor","Intermediate","Rich"))+
    ggpubr::theme_classic2()+
    labs(color="Soils (n=3)",
         y=bquote(bold("Nitrogen (mg.mg "[DM]~")")),
         x="")+
    theme(text = element_text(face="bold"))

# Soil Carbon
C <- ggplot(soil_chi,aes(x=trmt,y=C_mg/Mass, color=trmt))+ 
    geom_point(size=3,alpha=.8)+
    scale_color_manual(values=c("#ffe1b0","#6b5e49","#0f0d0a"))+
    ggpubr::theme_classic2()+
    labs(color="Soil treatments",
         y=bquote(bold("Carbon (mg.mg "[DM]~")")),
         x="")+
    theme(legend.position="none",
          text = element_text(face="bold"))

# Soil Phosphorus
P <- ggplot(soil_chi,aes(x=trmt,y=P_mgkg/1000, color=trmt))+
    geom_point(size=3,alpha=.8)+
    scale_color_manual(values=c("#ffe1b0","#6b5e49","#0f0d0a"))+
    ggpubr::theme_classic2()+
    labs(color="Soil treatments",
         y=bquote(bold("Phosphorus (mg.mg "[DM]~")")),
         x="")+
    theme(legend.position="none",
          text = element_text(face="bold"))

leg <- ggpubr::get_legend(N) #Extract legend

soil_chi_plot <- (C|N+theme(legend.position="none"))/(P|leg)

## Plant phenotype ====

### Bxplots ====

pbxp_traits <- NULL
for(i in Nbcol){ # for all variable in Nbcol  
    
    # get unit
    unitofvar <- trait_df_name$unit[match(names(data_leaf)[i],trait_df_name$rawnames)][[1]] # get units
    # get var
    var <-trait_df_name$cleannames[match(names(data_leaf)[i],trait_df_name$rawnames)] # get the variable
    # build plot
    plot <- local({
        i<-i
        ggplot(data_leaf,aes(x=data_leaf[,i],y=Trtmt,fill=Trtmt))+ # plot the data
            geom_boxplot()+
            scale_fill_manual(values=trtmt_col,name="Treatments")+
            ggpubr::theme_classic2()+
            ylab("")+
            xlab(bquote(bold(~.(var)~.(unitofvar))))+
            theme(axis.text.x = element_text(angle = -90,hjust=1,vjust=.5),
                  axis.title.x = element_text(face="bold",color=arrange(trait_df_name,var_cat)[which(arrange(trait_df_name,var_cat)$rawnames==names(data_leaf)[i]),"colorz"]), # color X title according to variable category
                  axis.text.y = element_blank(),
                  axis.ticks.y = element_blank())})
    # store plot
    pbxp_traits[[names(data_leaf)[i]]] <- plot
}

#run KW & WC
kw_traits <- data_leaf %>%
    tidyr::pivot_longer(cols=all_of(Nbcol)) %>%
    group_by(name) %>%
    rstatix::kruskal_test(value ~ Trtmt)

wcx_traits <- data_leaf %>%   
    tidyr::pivot_longer(cols=all_of(Nbcol)) %>%
    group_by(name) %>%
    rstatix::pairwise_wilcox_test(
        value ~ Trtmt,
        p.adjust.method = "holm") 

# Store pairwise cld
df_letters_traits <- data.frame()
for(traits in unique(wcx_traits$name)){
    
    letters <- multcompView::multcompLetters(setNames(wcx_traits[wcx_traits$name==traits,]$p.adj,
                                                      paste(gsub("-","",wcx_traits[wcx_traits$name==traits,]$group1),
                                                            gsub("-","",wcx_traits[wcx_traits$name==traits,]$group2), 
                                                            sep = "-")))$Letters
    df_letters_traits <- reshape2::melt(letters) %>%
        rename(cld=value) %>%
        mutate(Trtmt=paste0(substring(rownames(.),1,1),
                            "-",
                            substring(rownames(.),2,2)),
               name=traits) %>%
        rbind(df_letters_traits)
}

# Edit plots
for(traits in names(pbxp_traits)){
    # If KW was siginificant add posthoc cld
    if(kw_traits[kw_traits$name==traits,]$p<0.05){
        pbxp_traits[[traits]] <-local({
            traits<-traits
            pbxp_traits[[traits]]+
                geom_text(data = filter(df_letters_traits,name==traits),
                          aes(y=Trtmt,
                              x=max(pbxp_traits[[traits]]$data[traits])+0.05*(max(pbxp_traits[[traits]]$data[traits])-min(pbxp_traits[[traits]]$data[traits])),
                              label = cld))})
        # If not, don't
    }else{
        pbxp_traits[[traits]] <-local({
            traits<-traits
            pbxp_traits[[traits]]})
    }
}

# Combine plots
ptrait_range <-wrap_plots(pbxp_traits[(c(1:19)[-14])])+
    plot_layout(guides="collect",widths = c(1,1,1))&
    theme(axis.title = element_text(face="bold"),
          axis.text = element_text(face="bold"),
          legend.text = element_text(face = "bold"))

pflowers <- pbxp_traits$Flowers$data %>%
    ggplot(aes(x=Trtmt,y=Flowers,fill=Trtmt))+
    geom_boxplot(outliers = F)+
    geom_jitter(alpha=.6,width=.2,fill=alpha("black",.6),size=2)+
    scale_fill_manual(values=trtmt_col)+
    ggpubr::theme_classic2()+
    theme(text=element_text(face="bold"),
          legend.position = "none",
          axis.title.x = element_blank())+
    geom_text(data=pbxp_traits$Flowers$layers$geom_text$data,aes(label=cld,
                                                                 x=Trtmt,
                                                                 y=pull(summarise(group_by(mutate(pbxp_traits$Flowers$data,range=(max(Flowers) - min(Flowers))) ,Trtmt) ,
                                                                                  coords=max(Flowers) + 0.05 * max(range)))),
              fontface="bold",
              color="black")

### PoV Traits ====

# Traits partition of variance between treatments
pov_list <- NULL #to store
for(traits in trait_df_name$rawnames[-14]){ #for all traits
    # get the trait values
    col <- data_leaf[,match(traits,colnames(data_leaf))] 
    # add treatment info
    data_temp <- data.frame(trait=col, drought=data_leaf$drought,substrate=data_leaf$substrate) 
    # variance partitioning
    pov_list[[traits]] <- POV::POV(trait~drought+substrate,data_temp) 
}
# transform to ggplot compatible dataframe
pov_list %<>% 
    purrr::map(~mutate(.,var_source=rownames(.)))%>%
    purrr::map(~reshape2::melt(.))
# merge into one dataframe
pov_merged <- bind_rows(pov_list, .id = "trait") 
# rename variable and order factors
new_pov <- pov_merged%>% 
    filter(variable=='% of total')%>%
    arrange(value)%>%
    mutate(var_source=as.factor(var_source),
           rawnames=as.factor(trait))%>%
    mutate(var_source=forcats::fct_relevel(var_source,
                                           "Common",
                                           "Between drought",
                                           "Between substrate",
                                           "Within drought",
                                           "Within substrate"))

var_order_common <- filter(new_pov,var_source=="Common")%>%
    arrange(desc(value))%>%
    mutate(order_com=1:nrow(.))%>%
    dplyr::select(trait,order_com)
var_order_drought <- filter(new_pov,var_source=="Between drought")%>%
    arrange(desc(value))%>%
    mutate(order_drought=1:nrow(.))%>%
    dplyr::select(trait,order_drought)
var_order_substrate <- filter(new_pov,var_source=="Between substrate")%>%
    arrange(desc(value))%>%
    mutate(orde_sbustrater=1:nrow(.))%>%
    dplyr::select(trait,orde_sbustrater)

new_pov <- plyr::join_all(list(new_pov,var_order_common,var_order_drought,var_order_substrate),by="trait")
new_pov <- left_join(new_pov,trait_df_name)

#build the graph
bar_traits_var <- new_pov%>% 
    mutate(cleannames = forcats::fct_reorder(cleannames,var_cat))%>% # reorder levels
    mutate(var_source = forcats::fct_relevel(var_source,"Common", after = Inf ))%>% # reorder levels
    ggplot( aes(x=cleannames,y=value , fill= var_source)) + # plot
    geom_bar(position =position_stack(reverse=T),stat="identity")+
    scale_fill_manual(values=paletteer::paletteer_d("nationalparkcolors::Acadia"), name="")+
    ggpubr::theme_classic2()+
    xlab("")+
    ylab("% of variance")+ # theme
    theme(axis.text.x = element_text(margin=margin(t=5),hjust=1))+
    scale_y_continuous(expand=c(0,0))+
    theme(legend.position = "bottom",
          legend.justification = c("left"),
          legend.box.just = "left",
          legend.margin=margin(1,1,1,1),
          legend.box.margin=margin(1,1,1,1),
          legend.box.spacing = unit(0, "pt"),
          legend.key.size = unit(0.4, 'cm'),
          legend.text = element_text(size=8),
          legend.spacing.x = unit(1, "mm"),
          legend.spacing.y = unit(1, "mm"))+
    coord_flip()

bar_traits_var <- (bar_traits_var+theme(axis.text = element_text(face="bold"), # transform to bold text
                                        axis.title.x = element_text(face="bold"),
                                        legend.text = element_text(face='bold')) | 
                       trait_df_name%>% # add variable categories color
                       mutate(cleannames = forcats::fct_reorder(cleannames,var_cat))%>%
                       mutate(var_cat = forcats::fct_relevel(var_cat,
                                                             c("biomass",'nutrient','photosynthesis','structural','water','reproduction')))%>%
                       ggplot(aes(x=1,y=cleannames,fill=var_cat))+
                       geom_tile()+
                       theme_void()+
                       scale_fill_manual(values = var_cat_col, 
                                         name="Variable categories")+
                       theme(legend.text = element_text(face="bold"),
                             legend.title = element_text(face='bold')))+
    plot_layout(widths = c(15,1))

### PCA ====

# Run PCA on leaf traits but on C/N ratio as its redundant with C and N
PCA_plant_trait <- FactoMineR::PCA(data_leaf[,Nbcol[-c(14)]],
                                   scale.unit = T, 
                                   graph  = F,
                                   ncp=length(Nbcol)-1)

#### Test ====
#check the hypothesis that dispersion is roughly the same between my groups in multivariate space
dis <- vegdist(data_leaf[,Nbcol[-c(14)]],method="euclidean") #we create a distance matrix between our individuals
mod <- betadisper(dis,data_leaf$Trtmt) #we calculate the multivariate dispersions i.e. average distance to centroids
anova(mod) #now we check that they are no difference between our groups i.e. our individuals have the same dispersion in the multivariate space
#p-value = 0.002314   significative so our group dispersion is heterogeneous between habitats which might contribute to treatment significative effect
k = 1000
adonpca_res1 <- adonis2(scale(data_leaf[,Nbcol[-c(14)]])~data_leaf$drought*data_leaf$substrate, perm = k, na.rm = T, method = "euclidean",p.adjust.methods="holm",by="terms" ) 
adonpca_res1%<>%
    mutate(Term=c("drought","substrate","drought:substrate","Residual","Total"),.before =1) %>%
    flextable::flextable()

pairadonpca_res1 <- pairwiseAdonis::pairwise.adonis(scale(data_leaf[,Nbcol[-c(14)]]),data_leaf$Trtmt,p.adjust.m = "holm", sim.method = "euclidean", perm=k)
pairadonpca_res1%<>%
    mutate(SumsOfSqs=round(SumsOfSqs,digits = 3),
           R2=round(R2,digits = 3),
           F.Model=round(F.Model,digits = 3),
           p.value=ifelse(p.value<0.0001,"<0.0001",round(p.value,digits = 3)),
           p.adjusted=ifelse(p.adjusted<0.0001,"<0.0001",round(p.adjusted,digits = 3)))%>%
    select(-sig)%>%
    flextable::flextable()%>%
    flextable::bold(i=~`p.adjusted`<0.05,j=7)

#### Biplot ====

#get ind df
df_PCA_ind <- data.frame(PCA_plant_trait$ind$coord)
df_PCA_ind$Trtmt <- data_leaf$Trtmt
#get var df
df_PCA_var <- data.frame(PCA_plant_trait$var$coord)
df_PCA_var %<>%
    mutate(rawnames=rownames(.)) %>%
    left_join(trait_df_name)
#get scaling factor for var for neat plotting
scaling_factor <- min(((max(df_PCA_ind$Dim.1) - min(df_PCA_ind$Dim.1))/
                           (max(df_PCA_var$Dim.1) - min(df_PCA_var$Dim.1))),
                      ((max(df_PCA_ind$Dim.1) -   min(df_PCA_ind$Dim.1))/
                           (max(df_PCA_var$Dim.2) - min(df_PCA_var$Dim.2))))
#get axis explained variance
PCA_eig <- PCA_plant_trait$eig[1:2,2]

#plot
plot_pca <- 
    ggplot(df_PCA_ind,aes(x=Dim.1,y=Dim.2))+
    ggConvexHull::geom_convexhull(aes(fill=Trtmt,color=Trtmt),alpha=0.2,show.legend = F)+
    geom_point(aes(color=Trtmt,shape=Trtmt))+
    scale_color_manual(values=trtmt_col,name = "Treatment")+
    scale_fill_manual(values=trtmt_col,name = "Treatment")+
    scale_shape_manual(values = c(17,17,17,19,19,19),name = "Treatment")+
    
    geom_segment(data=df_PCA_var,aes(x=0,
                                     y=0,
                                     xend = Dim.1*(.7*scaling_factor),
                                     yend = Dim.2*(.7*scaling_factor)),
                 arrow = arrow(type = "closed",
                               angle=20,
                               length = unit(5,"pt")))+
    
    geom_text(data=df_PCA_var,aes(x = Dim.1*(.72*scaling_factor),
                                  y = Dim.2*(.72*scaling_factor),
                                  label=cleannames),
              color=df_PCA_var$colorz,
              fontface="bold")+
    
    geom_vline(xintercept = 0,linetype=2)+
    geom_hline(yintercept = 0,linetype=2)+
    
    xlab(paste0("PCA 1 (",round(PCA_eig[1],digits=1),"%)"))+
    ylab(paste0("PCA 2 (",round(PCA_eig[2],digits=1),"%)"))+
    
    ggpubr::theme_classic2()+
    theme(panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA), 
          legend.background = element_rect(fill = "transparent"),
          legend.key.size = unit(4, 'pt'), 
          legend.key.height = unit(4, 'pt'), 
          legend.key.width = unit(4, 'pt'), 
          legend.title = element_text(size=12),
          legend.text = element_text(size=10), 
          axis.title = element_text(size = 14),
          plot.margin = margin(2, 2, 2, 2),
          title = element_blank(),
          text=element_text(face="bold"),
          legend.position = "inside",
          legend.position.inside = c(.1, .1))

#### Contribution =====
# Plot variable contribution to 2 first components and correlogram with farther components
df_contrib_var <- mutate(rename_with(data.frame(PCA_plant_trait$var$contrib),
                                     ~stringr::str_c(.,"_contrib"),
                                     everything()),
                         rawnames=rownames(data.frame(PCA_plant_trait$var$contrib))) %>%
    left_join(df_PCA_var)
# Combine
contrib_var <- df_contrib_var %>%
    arrange(desc(Dim.1_contrib)) %>%
    mutate(cleannames=forcats::fct_inorder(cleannames)) %>%
    ggplot(aes(x=cleannames,y=Dim.1_contrib,fill=cleannames))+
    geom_bar(stat = "identity")+
    scale_fill_manual(values= set_names(trait_df_name$colorz,trait_df_name$cleannames))+
    ggpubr::theme_classic2()+
    scale_y_continuous(expand=c(0,0))+
    scale_x_discrete(expand=c(0,0))+
    theme(axis.text.x = element_text(angle=45,vjust=1,hjust=1),
          legend.position = "none",
          axis.title.x = element_blank(),
          text=element_text(face="bold"))+
    ggtitle("PCA 1")+
    ylab('Contribution (%)')+
    geom_hline(yintercept = 100/(length(Nbcol)-1),color="red",linetype=2)+
    
    df_contrib_var %>%
    arrange(desc(Dim.2_contrib)) %>%
    mutate(cleannames=forcats::fct_inorder(cleannames)) %>%
    ggplot(aes(x=cleannames,y=Dim.2_contrib,fill=cleannames))+
    geom_bar(stat = "identity")+
    scale_fill_manual(values= set_names(trait_df_name$colorz,trait_df_name$cleannames))+
    ggpubr::theme_classic2()+
    scale_y_continuous(expand=c(0,0))+
    scale_x_discrete(expand=c(0,0))+
    theme(axis.text.x = element_text(angle=45,vjust=1,hjust=1),
          legend.position = "none",
          axis.title.x = element_blank(),
          text=element_text(face="bold"))+
    ggtitle("PCA 2")+
    ylab('Contribution (%)')+
    geom_hline(yintercept = 100/(length(Nbcol)-1),color="red",linetype=2)+
    
    plot_layout(axes="collect")


#### Correlogram ====
rownames(PCA_plant_trait$var$cos2) <- trait_df_name$cleannames[match(rownames(PCA_plant_trait$var$cos2),
                                                                     trait_df_name$rawnames)]
corrplot_pca_traits <- ggcorrplot::ggcorrplot(PCA_plant_trait$var$cos2,
                                              method = "square", 
                                              tl.col = "black")+
    ggpubr::theme_classic2()+
    theme(axis.text.x = element_text(angle=45,vjust=1,hjust=1),
          axis.title = element_blank(),
          text=element_text(face="bold"))

#### PoV PCA ====

pov_list2 <- NULL #to store
for(traits in colnames(PCA_plant_trait$ind$coord[,1:3])){ #for PCA 1:3
    # add treatment info
    data_temp <- data.frame(trait=PCA_plant_trait$ind$coord[,traits], 
                            drought=data_leaf$drought,
                            substrate=data_leaf$substrate) 
    # variance partitioning
    pov_list2[[traits]] <- POV::POV(trait~drought*substrate,data_temp)
}
# transform to ggplot compatible dataframe
pov_list2 %<>%
    purrr::map(~mutate(.,var_source=rownames(.)))%>%
    purrr::map(~reshape2::melt(.))
# merge into one dataframe
pov_merged2 <- bind_rows(pov_list2, .id = "PCA") 
# rename variable and order factors
new_pov2 <- pov_merged2%>%
    filter(variable=='% of total')%>%
    arrange(value)%>%
    mutate(var_source=as.factor(var_source),
           rawnames=as.factor(PCA))%>%
    mutate(var_source=forcats::fct_relevel(var_source,
                                           "Common",
                                           "Between drought",
                                           "Between substrate",
                                           "Within drought",
                                           "Within substrate"))

var_order_common2 <- filter(new_pov2,var_source=="Common")%>%
    arrange(desc(value))%>%
    mutate(order_com=1:nrow(.))%>%
    dplyr::select(PCA,order_com)
var_order_drought2 <- filter(new_pov2,var_source=="Between drought")%>%
    arrange(desc(value))%>%
    mutate(order_drought=1:nrow(.))%>%
    dplyr::select(PCA,order_drought)
var_order_substrate2 <- filter(new_pov2,var_source=="Between substrate")%>%
    arrange(desc(value))%>%
    mutate(orde_sbustrater=1:nrow(.))%>%
    dplyr::select(PCA,orde_sbustrater)
#Join
new_pov2 <- plyr::join_all(list(new_pov2,var_order_common2,var_order_drought2,var_order_substrate2),by="PCA")

#build the graph
bar_traits_var2 <- new_pov2%>% 
    filter(PCA!="Dim.3")%>% # Keep PCA 1 and 2
    mutate(PCA=gsub("Dim.","PCA ",PCA))%>%
    mutate(var_source = forcats::fct_relevel(var_source,"Common", after = Inf ))%>% # reorder levels
    ggplot( aes(x=PCA,y=value , fill= var_source)) + # plot
    geom_bar(position =position_stack(reverse=T),stat="identity")+
    scale_fill_manual(values=c(paletteer::paletteer_d("nationalparkcolors::Acadia"),"grey"), name="")+
    ggpubr::theme_classic2()+
    xlab("")+
    ylab("% of variance")+ # theme
    theme(axis.text.x = element_text(margin=margin(t=5),hjust=1))+
    scale_y_continuous(expand=c(0,0))+
    theme(legend.position = "right",
          legend.margin=margin(c(1,1,1,1)),
          legend.box.margin=margin(1,1,1,1),
          legend.box.spacing = unit(0, "pt"),
          legend.key.size = unit(0.4, 'cm'),
          legend.text = element_text(size=8),
          legend.spacing.x = unit(1, "mm"),
          legend.spacing.y = unit(1, "mm"),
          text = element_text(face = "bold"))


####One dim PCA ====
monopca <- FactoMineR::PCA(df_PCA_ind[,c(1,2)],ncp=1,graph = T)
monoind <- monopca$ind$coord %>%
    data.frame()
monoind$Trtmt <- data_leaf$Trtmt
monoind$ID <- data_leaf$ID
monoind %<>%
    rename(monodim=Dim.1)

## Microbial communities ====

### Composition ====

# create trans_abund and first plots
comp_barplots <- NULL
for(i in datasets){ # for both datasets (its and 16s)
    tmp <- get(i) # get the data
    tmp <- clone(tmp)
    #explicit non assigned
    tmp$tax_table%<>%
        mutate(Genus=ifelse(is.na(Genus),"g__Not assigned",Genus),
               Class=ifelse(is.na(Class),"c__Not assigned",Class),
               Order=ifelse(is.na(Order),"o__Not assigned",Order),
               Family=ifelse(is.na(Family),"f__Not assigned",Family))
    
    rownames(tmp$sample_table) <- tmp$sample_table$sample_id
    # abundance @class level. The top 12 most abundant are kept, others are grouped
    trans_tmp <- trans_abund$new(dataset = tmp, taxrank = "Class", ntaxa = 12) 
    p_class <- trans_tmp$plot_bar(others_color = "grey70",facet = "Trtmt", xtext_keep = FALSE, legend_text_italic = FALSE) # plot it
    
    trans_tmp <- trans_abund$new(dataset = tmp, taxrank = "Order", ntaxa = 12) # @order level
    p_order <- trans_tmp$plot_bar(others_color = "grey70",facet = "Trtmt", xtext_keep = FALSE, legend_text_italic = FALSE)
    
    trans_tmp <- trans_abund$new(dataset = tmp, taxrank = "Family", ntaxa = 12) # @family
    p_family <- trans_tmp$plot_bar(others_color = "grey70",facet = "Trtmt", xtext_keep = FALSE, legend_text_italic = FALSE)
    
    trans_tmp <- trans_abund$new(dataset = tmp, taxrank = "Genus", ntaxa = 12) # @genus
    p_genus <- trans_tmp$plot_bar(others_color = "grey70",facet = "Trtmt", xtext_keep = FALSE, legend_text_italic = FALSE)
    
    plot_comb <- (p_class|p_order)/(p_family|p_genus) # merge
    comp_barplots[[i]] <- plot_comb # store
}
# Edit plot to order according to Flowers > Plant height

tax_bac_col <- c("grey60","#666666",rev(colorRampPalette(paletteer::paletteer_d("MetBrewer::Austria"))(11)))
# scales::show_col(tax_bac_col)

tax_bac_col[6] <- "cornflowerblue"
tax_bac_col[5] <- "#fbe6c5"
tax_bac_col[4] <- "#ff8811"
tax_bac_col[9] <- "chartreuse3"

tax_fun_col <- c("grey60","#666666",
  rev(paletteer::paletteer_d("ggthemes::Classic_Green_Orange_12")[2:12])) 
# scales::show_col(tax_fun_col)

# Bacteria
compbac <-  
    # get the relative abundance data from previous plot
    comp_barplots$micro_16s[[1]][[1]]$data%>% 
    ungroup()%>%
    arrange(Flowers,Pl_H)%>%
    mutate(Sample=forcats::fct_inorder(Sample))%>%
    mutate(Taxonomy=forcats::fct_relevel(as.factor(Taxonomy),"Others","Not assigned"))%>%
    ggplot(aes(x=Sample,y=Abundance))+ # plot
    geom_bar(aes(fill=Taxonomy),stat="identity",position = "stack",width = 1)+
    scale_fill_manual(values=tax_bac_col,
                      name="Bacterial Class")+ # expand Dark2 palette to encompass 12 genera
    scale_y_continuous(expand = c(0,0),
                       sec.axis=sec_axis(~.*(max(comp_barplots$micro_16s[[1]][[1]]$data$Flowers)/100),name="Flowers"))+
    ggpubr::theme_classic2()+
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank())+
    ylab("Relative abundance (%)")+
    theme(legend.position = "right",
          legend.direction = "vertical",
          legend.title = element_text(hjust = 0,face = "bold"),
          legend.title.position = "top", 
          legend.text = element_text(size = 7),
          legend.key.size = unit(.5, "cm"),
          legend.key.spacing = unit(0,"cm"),
          text = element_text(face="bold"),
          axis.text.y.right = element_text(color = "#649eff"),
          axis.title.y.right = element_text(color = "#649eff"))+
    guides(fill = guide_legend(override.aes = list(size = 0.25),ncol = 1))+
    ggtitle("Bacteria")+
    geom_point(data=unique(comp_barplots$micro_16s[[1]][[1]]$data[,c("sample_id","Flowers")]),
               aes(x=sample_id,y=Flowers/(max(comp_barplots$micro_16s[[1]][[1]]$data$Flowers)/100)),
               show.legend="none",
               alpha=1,
               fill="#649eff",
               color="black",
               shape=21,
               size=2) 
# Fungi
compfung <-   
    comp_barplots$micro_its[[1]][[1]]$data%>%
    ungroup()%>%
    arrange(Flowers,Pl_H)%>%
    mutate(Sample=forcats::fct_inorder(Sample))%>%
    mutate(Taxonomy=forcats::fct_relevel(as.factor(Taxonomy),"Others","Not assigned"))%>%
    ggplot(aes(x=Sample,y=Abundance))+
    geom_bar(aes(fill=Taxonomy),stat="identity",position = "stack",width = 1)+
    scale_fill_manual(values=tax_fun_col,name="Fungal Class")+ # expand Dark2 palette to encompass 12 genera
    scale_y_continuous(expand = c(0,0),
                       sec.axis=sec_axis(~.*(max(comp_barplots$micro_its[[1]][[1]]$data$Flowers)/100),name="Flowers"))+
    geom_point(data=unique(comp_barplots$micro_its[[1]][[1]]$data[,c("sample_id","Flowers")]),
               aes(x=sample_id,y=Flowers/(max(comp_barplots$micro_its[[1]][[1]]$data$Flowers)/100)),
               show.legend="none",
               alpha=1,
               fill="#649eff",
               color="black",
               shape=21,
               size=2)  +
    ggpubr::theme_classic2()+
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank())+
    ylab("Relative abundance (%)")+
    theme(legend.position = "right",
          legend.direction = "vertical",
          legend.title = element_text(hjust = 0,face = "bold"),
          legend.title.position = "top", 
          legend.text = element_text(size = 7),
          legend.key.size = unit(.5, "cm"),
          legend.key.spacing = unit(0,"cm"),
          text = element_text(face="bold") ,
          axis.text.y.right = element_text(color = "#649eff"),
          axis.title.y.right = element_text(color = "#649eff"))+
    guides(fill = guide_legend(override.aes = list(size = 0.25),ncol = 1))+
    ggtitle("Fungi")


combined_comp <- (compbac/compfung)+plot_layout(guides="collect")

### Alpha-div ====

# Relation alpha div to fitness
flower_richness_list <- NULL
for (i in datasets){
    # get data
    data_tmp <- get(i)
    data_tmp <- clone(data_tmp)
    trtmt <- data_tmp$sample_table$Trtmt
    Flowers <- data_tmp$sample_table$Flowers
    # compute richness index
    sr <- hillR::hill_taxa(t(data_tmp$otu_table), q = 0, MARGIN = 1, base = exp(1))
    shan <- hillR::hill_taxa(t(data_tmp$otu_table), q = 1, MARGIN = 1, base = exp(1))
    invsimp <- hillR::hill_taxa(t(data_tmp$otu_table), q = 2, MARGIN = 1, base = exp(1))
    #store
    data_tmp$sample_table$sr <- sr
    data_tmp$sample_table$shan <- shan
    data_tmp$sample_table$invsimp <- invsimp
    # save to environment
    assign(i,data_tmp)
    # run lm
    lm_outs <- summary(lm(cbind(sr,shan,invsimp)~Flowers))
    #loop over the different index
    for(index in c("sr","shan","invsimp")){
        # set titles
        title <- switch (index,
                         "sr" = paste0("Observed ",ifelse(grepl("16",i),"bacterial","fungal")," OTUs richness"),
                         "shan" = paste0(ifelse(grepl("16",i),"Bacterial","Fungal")," Shannon entropy"),
                         "invsimp" = paste0(ifelse(grepl("16",i),"Bacterial","Fungal")," inverse Simpson index"))
        # get corresponding lm outs
        lm_outs_tmp <- lm_outs[[which(grepl(index,names(lm_outs)))]]
        # plot
        flower_richness_list[[i]][[index]]<- data_tmp$sample_table%>%
            ggplot(aes(x=Flowers,y=.data[[index]],color=Trtmt))+
            geom_point()+
            scale_color_manual(values=trtmt_col)+
            ggpubr::theme_classic2()+
            ylab(title)+
            labs(color="Treatment")+
            theme(legend.key.size = unit(1.7, 'cm'),
                  legend.title = element_text(size=14),
                  legend.text = element_text(size=12),
                  axis.title.x = element_blank())+
            guides(color=guide_legend(ncol=2,
                                      title.hjust=0.5))+
            {if(lm_outs_tmp$coefficients[2,4]<0.05)geom_smooth(method="lm",color="black")}+
            annotate(geom = "text",
                     label=paste0("pval: ",
                                  round(lm_outs_tmp$coefficients[2,4],digits=3),
                                  collapse = "\n"),
                     x=min(Flowers)+(max(Flowers)-min(Flowers))*0.05,
                     y=max(data_tmp$sample_table[index])+(max(data_tmp$sample_table[index])-min(data_tmp$sample_table[index]))*0.2,
                     hjust="left",vjust="Top",size=3,fontface=2)+
            annotate(geom="text",
                     label=as.character(as.expression(substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                                                                 list(a = format(unname(coef(lm_outs_tmp)[1]), digits = 2),
                                                                      b = format(unname(coef(lm_outs_tmp)[2]), digits = 2),
                                                                      r2 = format(lm_outs_tmp["r.squared"]$r.squared, digits = 3))))),
                     x=min(Flowers)+(max(Flowers)-min(Flowers))*0.05,
                     y=max(data_tmp$sample_table[index])+(max(data_tmp$sample_table[index])-min(data_tmp$sample_table[index]))*0.15,
                     hjust="left",vjust="Top",size=3,parse=T)
    }
} 

#alpha div ~ trtmt
bxp_alpha_trtmt <- list()
kw_list <- list()
pwwcx_list <- list()
for(i in datasets){
    
    # get data
    data_tmp <- get(i)
    data_tmp <- clone(data_tmp)
    #run KW & WC
    kw_index <- data_tmp$sample_table %>%
        tidyr::pivot_longer(cols=all_of(c("sr", "invsimp","shan"))) %>%
        group_by(name) %>%
        rstatix::kruskal_test(value ~ Trtmt)
    
    wcx_index <- data_tmp$sample_table %>%   
        tidyr::pivot_longer(cols=all_of(c("sr", "invsimp","shan"))) %>%
        group_by(name) %>%
        rstatix::pairwise_wilcox_test(
            value ~ Trtmt,
            p.adjust.method = "holm") 
    
    df_letters_index <- data.frame()
    for(index in unique(wcx_index$name)){
        letters <- multcompView::multcompLetters(setNames(wcx_index[wcx_index$name==index,]$p.adj,
                                                          paste(gsub("-","",wcx_index[wcx_index$name==index,]$group1),
                                                                gsub("-","",wcx_index[wcx_index$name==index,]$group2), 
                                                                sep = "-")))$Letters
        
        df_letters_index <- reshape2::melt(letters) %>%
            rename(cld=value) %>%
            mutate(Trtmt=paste0(substring(rownames(.),1,1),
                                "-",
                                substring(rownames(.),2,2)),
                   name=index) %>%
            rbind(df_letters_index)
    }
    df_letters_index$name <- rep(c("Observed\nrichness","Shannon\nentropy","Inverse\nsimpson"),each=6)
    # Save tests
    kw_list[[i]] <- kw_index %>%
        mutate(.before=1,Test="KW",Kingdom=ifelse(i=="micro_16s","Bacteria","Fungi")) %>%
        flextable::flextable() 
    
    pwwcx_list[[i]] <- wcx_index %>%
        mutate(.before=1,Test="KW",Kingdom=ifelse(i=="micro_16s","Bacteria","Fungi")) %>%
        flextable::flextable() 
    
    # Build plots
    stat_coord <- pull(summarise(group_by(tidyr::pivot_longer(data_tmp$sample_table,cols=all_of(c("sr", "invsimp","shan"))),name),coord=max(value)),coord)
    kw_index$value <- stat_coord
    kw_index$name <- c("Inverse\nsimpson","Shannon\nentropy","Observed\nrichness")
    kw_index$name <- forcats::fct_relevel(kw_index$name,c("Observed\nrichness","Shannon\nentropy","Inverse\nsimpson"))
    bxp_alpha_trtmt[[i]] <- data_tmp$sample_table %>%
        tidyr::pivot_longer(cols=all_of(c("sr", "invsimp","shan"))) %>%
        mutate(name=ifelse(name=="sr",
                           "Observed\nrichness",ifelse(name=="shan",
                                                       "Shannon\nentropy","Inverse\nsimpson"))) %>%
        arrange(desc(value)) %>%
        mutate(name=forcats::fct_relevel(name,c("Observed\nrichness","Shannon\nentropy","Inverse\nsimpson"))) %>%
        ggplot(aes(x=Trtmt,y=value))+
        geom_boxplot(outliers = F,aes(fill=Trtmt))+
        geom_jitter(width = .2)+
        geom_text(data=kw_index,aes(x=1,y=value+(.025*value),label=paste0("KW pval= ",round(p,digits = 2))),
                  fontface=if_else(kw_index$p<0.05,"bold","plain"),
                  hjust=0,
                  size=3)+
        facet_wrap(~name,scales="free")+
        scale_fill_manual(values=trtmt_col,name="Treatment")+
        ggpubr::theme_classic2()+
        theme(axis.ticks.x = element_blank(),
              axis.text.x = element_blank(),
              axis.title.x = element_blank(),
              text=element_text(face="bold"),
              legend.position = "none")
    
    if(any(kw_index$p<0.05)){
        bxp_alpha_trtmt[[i]] <- bxp_alpha_trtmt[[i]] +
            geom_text(data=mutate(filter(df_letters_index,
                                         name%in%kw_index$name[kw_index$p<0.05]),
                                  name=forcats::fct_relevel(name,c("Observed\nrichness",
                                                                   "Shannon\nentropy",
                                                                   "Inverse\nsimpson")),
                                  value=stat_coord[kw_index$p<0.05]-(.05*stat_coord[kw_index$p<0.05])),
                      aes(x=Trtmt,y=value,
                          label = cld))
    }
    
}

### Beta-div ====

#### Rolling windows ====

list_beta_droll <- list()
list_beta_proll <- list()
for(i in datasets){
    # get hellinger transformed data
    dtmp_hell <- get(paste0(i,"_hell"))
    dtmp_hell <- clone(dtmp_hell)
    # arrange according to increasing fitness
    dtmp_hell$sample_table %<>% 
        arrange(Flowers,Pl_H)
    dtmp_hell$otu_table <- dtmp_hell$otu_table[,dtmp_hell$sample_table$sample_id] 
    # Run the moving window analysis with a window size of 5,10,15% of th samples
    
    for(ws_prop in c(0.05,0.1,0.15)){
        
        rollwin <- CommEcol::betaRegDisp(t(dtmp_hell$otu_table),
                                         dtmp_hell$sample_table$Flowers,
                                         ws=round(ws_prop*nrow(dtmp_hell$sample_table)),
                                         method.1 = "horn")
        rollwin <- as.data.frame(rollwin)
        # remove samples at the gradient end
        rollwin$sample_id <- dtmp_hell$sample_table$sample_id[1:(length(dtmp_hell$sample_table$sample_id)-round(ws_prop*nrow(dtmp_hell$sample_table))+1)]
        # add sample info
        rollwin %<>%
            left_join(dtmp_hell$sample_table)
        #run lm
        lm_eqn <- function(df){
            m <- lm(mean.diss.focal ~ Flowers, rollwin);
            eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                             list(a = format(unname(coef(m)[1]), digits = 2),
                                  b = format(unname(coef(m)[2]), digits = 2),
                                  r2 = format(summary(m)$r.squared, digits = 3)))
            as.character(as.expression(eq));
        }
        # get mean dissimilarity of the focal sample to the others of the window
        mean.diss.focal <- rollwin$mean.diss.focal
        #get the gradient
        grad <- rollwin$grad
        #store
        list_beta_droll[[i]][[paste0("ws:",round(ws_prop*nrow(dtmp_hell$sample_table)))]] <- rollwin
        #store plot
        list_beta_proll[[i]][[paste0("ws:",round(ws_prop*nrow(dtmp_hell$sample_table)))]] <- rollwin %>%
            ggplot(aes(x=grad,y=mean.diss.focal,colour = Trtmt))+
            geom_point()+
            scale_color_manual(values=trtmt_col)+
            ggpubr::theme_classic2()+
            xlab("Flowers")+
            ylab("Rolling window mean Morisita-Horn distance")+
            labs(color="Treatment")+
            theme(text=element_text(face="bold"),
                  legend.key.size = unit(1.7, 'cm'),
                  legend.title = element_text(size=14),
                  legend.text = element_text(size=12))+
            guides(color=guide_legend(ncol=2,
                                      title.hjust=0.5))+
            #plot fit only if significant
            {if(!gtools::stars.pval(summary(lm(data=rollwin,mean.diss.focal~ grad))$coefficients[2,4])%in%c("."," "))geom_smooth(method="lm",color="black")}+
            # add lm outs
            annotate(geom = "text",label=paste0("pval: ",
                                                ifelse(gtools::stars.pval(summary(lm(data=rollwin,
                                                                                     mean.diss.focal~ grad))$coefficients[2,4])%in%c("."," "),
                                                       "NS",                                    gtools::stars.pval(summary(lm(data=rollwin,mean.diss.focal~ grad))$coefficients[2,4])),
                                                collapse = "\n"),
                     x=min(grad)+(max(grad)-min(grad))*0.05,
                     y=max(mean.diss.focal)+(max(mean.diss.focal)-min(mean.diss.focal))*0.2,
                     hjust="left",vjust="Top",size=3,fontface=2)+
            annotate(geom="text",
                     label=lm_eqn(rollwin),
                     x=min(grad)+(max(grad)-min(grad))*0.05,
                     y=max(mean.diss.focal)+(max(mean.diss.focal)-min(mean.diss.focal))*0.15,
                     hjust="left",vjust="Top",size=3,parse=T)+
            ggtitle(ifelse(grepl("16s",i),"Bacteria","Fungi"))
        
    }
}

#### Prevalence group relab ====

prev_relab <- list()
prev_relab_flowers <- list()
for(i in datasets){
    #get data
    data_tmp <- get(i)
    data_tmp <- clone(data_tmp)
    # build df 4 plot
    df <-  
        data_tmp$otu_table%>%
        mutate(label=gsub("M","",rownames(.)))%>%
        reshape2::melt()%>%
        dplyr::rename(sample_id=variable)%>%
        # remove otu absent in samples
        filter(value>0) %>%
        # get otu prevalence
        group_by(label)%>%
        mutate(prev=sum(value>0))%>%
        # discretise prevalence into <5% of samples, >80% or inbetween
        ungroup()%>%
        mutate(prev_cat=ifelse(prev<0.05*nrow(data_tmp$sample_table),
                               "5%<.",
                               ifelse(prev>.8*nrow(data_tmp$sample_table),
                                      ".>80%",
                                      "5%<.<80%")))%>%
        # relevel
        mutate(prev_cat=forcats::fct_relevel(prev_cat,".>80%","5%<.<80%","5%<."))%>%
        # add sample info
        left_join(data_tmp$sample_table)%>%
        # get relab and flowers
        group_by(sample_id,prev_cat)%>%
        summarise(relab=sum(value/nb_reads),
                  Flowers=unique(Flowers))%>%
        ungroup()%>%
        # arrange according to fitness
        mutate(sample_id=forcats::fct_relevel(sample_id,levels(data_tmp$sample_table$sample_id)))
    
    # plot
    prev_relab[[i]] <- df %>%
        ggplot(aes(x=sample_id,y=relab*100,fill=prev_cat))+
        geom_bar(stat = "identity",width=1)+
        ggpubr::theme_classic2()+
        theme(text=element_text(face="bold"),
              axis.text.x = element_blank(),
              axis.title.x = element_blank(),
              axis.ticks.x = element_blank())+
        scale_y_continuous(expand = expansion(mult = c(0,0)))+
        scale_fill_manual(values = c("darkorange","darkorchid4","cornflowerblue"),name="OTUs prevalence")+
        ylab("Cumulative relative abundance (%)")+
        ggtitle(ifelse(grepl("16s",i),"Bacteria","Fungi"))
    
    # Lm cumulative relab of groups ~ fitness
    lm_outs <- summary(lm(cbind(`.>80%`,`5%<.`,`5%<.<80%`)*100~Flowers, tidyr::pivot_wider(df,names_from = prev_cat,values_from = relab)))
    sig_prev_cat <- gsub("Response ","",names(lm_outs)[which(c(lm_outs[[1]]$coefficients[2,4],
                                                               lm_outs[[2]]$coefficients[2,4],
                                                               lm_outs[[3]]$coefficients[2,4])<0.05)])
    
    # xtract info for plot
    df_text <- data.frame(prev_cat=gsub("Response ","",names(lm_outs)),
                          label=c(as.character(as.expression(substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2~" | pval ="~pval, 
                                                                        list(a = format(unname(coef(lm_outs[[1]])[1]), digits = 2),
                                                                             b = format(unname(coef(lm_outs[[1]])[2]), digits = 2),
                                                                             r2 = format(lm_outs[[1]]["r.squared"]$r.squared, digits = 2),
                                                                             pval = format(ifelse(unname(coef(lm_outs[[1]])[8])>0.05,
                                                                                                  "NS",
                                                                                                  round(unname(coef(lm_outs[[1]])[8]),digits=3))))))),
                                  as.character(as.expression(substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2~" | pval ="~pval, 
                                                                        list(a = format(unname(coef(lm_outs[[2]])[1]), digits = 2),
                                                                             b = format(unname(coef(lm_outs[[2]])[2]), digits = 2),
                                                                             r2 = format(lm_outs[[2]]["r.squared"]$r.squared, digits = 2),
                                                                             pval = format(ifelse(unname(coef(lm_outs[[2]])[8])>0.05,
                                                                                                  "NS",
                                                                                                  round(unname(coef(lm_outs[[2]])[8]),digits=3))))))),
                                  as.character(as.expression(substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2~" | pval ="~pval, 
                                                                        list(a = format(unname(coef(lm_outs[[3]])[1]), digits = 2),
                                                                             b = format(unname(coef(lm_outs[[3]])[2]), digits = 2),
                                                                             r2 = format(lm_outs[[3]]["r.squared"]$r.squared, digits = 2),
                                                                             pval = format(ifelse(unname(coef(lm_outs[[3]])[8])>0.05,
                                                                                                  "NS",
                                                                                                  round(unname(coef(lm_outs[[3]])[8]),digits=3)))))))),
                          x=c(10,10,10),
                          y=c(90,95,100))
    
    #plot
    prev_relab_flowers[[i]] <- df%>%
        # dplyr::filter(prev_cat!="5%<.<80%")%>%
        ggplot(aes(x=Flowers,y=relab*100,color=prev_cat))+
        geom_point()+
        geom_smooth(data=subset(df,prev_cat%in%sig_prev_cat),show.legend = F,method="lm")+
        ggpubr::theme_classic2()+
        theme(text=element_text(face="bold"))+
        scale_color_manual(values = c("darkorange","darkorchid4","cornflowerblue"),name="OTUs prevalence")+
        ylab("Cumulative relative abundance (%)")+
        xlab("Flowers")+
        ggtitle(ifelse(grepl("16s",i),"Bacteria","Fungi"))+
        geom_text(data=df_text,aes(label = label,x=x,y=y,color=prev_cat),
                  hjust="left",vjust="Top",size=3,parse=T)+
        scale_y_continuous(limits = c(0,100),expand = expansion(add=c(0,10)))+
        scale_x_continuous(expand = expansion(add=c(0,5)))
    
}

#### PCoA ====

methods_dist <- c("horn","bray","jaccard")
pbetadisp_list <- list()
permanova_list <- list()
pcoas_list <- NULL
pcoas_taxa_list <- NULL
ordi_list <- NULL
shepard_list <- NULL

for (i in datasets){
    
    for(m in methods_dist){
        #get corresponding data
        if(m=="horn"){
            dtmp <- get(paste0(i,"_hell"))}
        else{dtmp <- get(i)}
        rownames(dtmp$sample_table) <- dtmp$sample_table$sample_id
        
        #check the hypothesis that dispersion is roughly the same between my groups in multivariate space
        dis <- vegan::vegdist(t(dtmp$otu_table),method=m) #we create a distance matrix between our individuals
        mod <- vegan::betadisper(dis,dtmp$sample_table$Trtmt) #we calculate the multivarite dispersions i.e. average distance to centroids
        disp_test <- anova(mod) #now we check that they are no difference between our groups i.e. our idnividuals have the same dispersion in the multivariate space
        
        
        tmp_title <- switch (m,
                             "horn" = "(Morisita-Horn)",
                             "bray" = "(Bray-Curtis)",
                             "jaccard" = "(Jaccard)")
        
        #plot betadisper
        dftmp <- data.frame(distance2centroid=mod$distances,
                            Trtmt=mod$group)
        
        pbetadisp_list[[i]][[m]] <- dftmp %>%
            mutate(Trtmt=forcats::fct_relevel(Trtmt,levels(data_leaf$Trtmt)))%>%
            ggplot(aes(x=Trtmt,y=distance2centroid,fill=Trtmt))+
            geom_boxplot(outliers = F)+
            geom_point(position = position_jitter(width = .2))+
            ggpubr::theme_classic2()+
            ylab(paste0("Distance to treatment centroid\n",tmp_title))+
            theme(axis.title.x = element_blank(),
                  text = element_text(face="bold"))+
            scale_fill_manual(values=trtmt_col,name = "Treatments")
        
        
        pbetadisp_list[[i]][[m]] <- cld_bxplot_wilcox(data = dftmp,
                                                      x = dftmp$Trtmt,
                                                      y =  dftmp$distance2centroid,
                                                      plot = pbetadisp_list[[i]][[m]],
                                                      nudge = .030)
        
        #Permanova
        
        #global
        adon.res1 <- vegan::adonis2(t(dtmp$otu_table)~dtmp$sample_table$drought*dtmp$sample_table$substrate, #*dtmp$sample_table$drought*dtmp$sample_table$substrate
                                    perm = 999, 
                                    method = m,
                                    p.adjust.methods="holm",
                                    by="terms" ) 
        
        permanova_list[[i]][[m]][["global"]] <- adon.res1%>%
            mutate(Term=c("Drought","Substrate","Drought:Substrate","Residual","Total"),
                   .before =1) %>%
            rename(pval=`Pr(>F)`)%>%
            flextable::flextable()%>%
            flextable::bold(i=~`pval`<0.05,j=6)
        #pairwise
        pair.adon.res1 <- pairwiseAdonis::pairwise.adonis(t(dtmp$otu_table),
                                                          dtmp$sample_table$Trtmt,
                                                          p.adjust.m = "holm", 
                                                          sim.method = m, 
                                                          perm=999)
        
        permanova_list[[i]][[m]][["pairwise"]]  <- pair.adon.res1%>%
            flextable::flextable()%>%
            flextable::width(width = c(1,.5,1.5,1.5,1.5,1.5,1.5,.5)) %>%
            flextable::bold(i=~`p.adjusted`<0.05,j=7)
        
        # Ordination
        # run pcoa
        orditmp <- phyloseq::ordinate(file2meco::meco2phyloseq(dtmp), "PCoA", m,binary=ifelse(m=="jaccard",TRUE,FALSE)) # run pcoa 
        ordi_list[[i]][[m]] <- orditmp
        # get sample coordinates
        pcoa_sites <- phyloseq::plot_ordination(file2meco::meco2phyloseq(dtmp), orditmp,type = "sites",axes = c(1,2),color = "Trtmt")
        # get taxa coordinates
        pcoa_taxa <- phyloseq::plot_ordination(file2meco::meco2phyloseq(dtmp), orditmp,type = "taxa",axes = c(1,2),color="Phylum")
        
        # get scores
        orditmp$scores <- orditmp$vectors
        # envfit host traits
        envfitted <- vegan::envfit(orditmp,
                                   dtmp$sample_table[,c(trait_df_name$rawnames[-which(trait_df_name$rawnames=="C.N_leaf")])],
                                   permutations=999)
        # get eigenvalue
        expl1 <- orditmp$values$Relative_eig[1]*100
        expl2 <- orditmp$values$Relative_eig[2]*100
        
        # get environment goordinates
        en_coord_cont <- as.data.frame(vegan::scores(envfitted, "vectors")) * ggvegan:::arrow_mul(vegan::scores(envfitted, "vectors"),
                                                                                                  orditmp$vectors)
        # add trait info
        en_coord_cont%<>%
            mutate(rawnames=rownames(.))%>%
            left_join(trait_df_name)%>%
            left_join(data.frame(rawnames=rownames(envfitted$vectors$arrows),
                                 r2=envfitted$vectors$r,
                                 psign=envfitted$vectors$pvals))
        #df for plot
        df <- pcoa_sites$data
        #plot
        gg <- ggplot(data = df, aes(x = Axis.1, y = Axis.2)) + 
            geom_point(data = df, aes(colour = Trtmt,shape=Trtmt), size = 3, alpha = 1) + 
            scale_colour_manual(values = trtmt_col) + 
            labs(colour = "Treatment")+
            ylab(paste0("PCoA 2 (",round(expl2,2),"%)"))+
            xlab(paste0("PCoA 1 (",round(expl1,2),"%)"))+
            scale_shape_manual(values=c(17,17,17,19,19,19),name="Treatment")+
            ggConvexHull::geom_convexhull(df,mapping=aes(x=Axis.1,y=Axis.2,fill = Trtmt, colour = Trtmt),
                                          alpha=0.2)+
            scale_fill_manual(values=trtmt_col,name="Treatment")
        
        # get df taxa
        df_taxa <- pcoa_taxa$data
        # replace levels with top8
        df_taxa %<>%
            mutate(class4plot=ifelse(gsub("c__","",Class)%in%levels(combined_comp[[ifelse(i=="micro_16s",1,2)]]$data$Taxonomy),
                                     gsub("c__","",Class),
                                     ifelse(is.na(Class),"Not assigned","Others"))) %>%
            mutate(class4plot=forcats::fct_relevel(class4plot,levels(combined_comp[[ifelse(i=="micro_16s",1,2)]]$data$Taxonomy)))
        
        pal_tmp <- switch (i,
                           "micro_16s" =  tax_bac_col,
                           "micro_its" = tax_fun_col) 

        
        layout <- c(
            area(1,1,8,8),
            area(1,9,8,10),
            area(9,1,10,8),
            area(9,10,9,10)
        )
        
        gg_taxa <- ggplot(data = df_taxa, aes(x = Axis.1, y = Axis.2)) + 
            geom_point(data = df_taxa, aes(colour = class4plot), size = 3, alpha = 1) + 
            ylab(paste0("PCoA 2 (",round(expl2,2),"%)"))+
            xlab(paste0("PCoA 1 (",round(expl1,2),"%)"))+
            scale_color_manual(values=pal_tmp,name=ifelse(i=="micro_16s","Bacterial class","Fungal class"))+
            theme(legend.key.size = unit(6,"pt"),
                  legend.text = element_text(size=6),
                  legend.box.margin = margin(),
                  legend.margin = margin(),
                  legend.direction = "vertical")+
            df_taxa %>%
            mutate(class4plot=forcats::fct_relevel(class4plot,
                                                   c(as.vector(pull(arrange(summarise(group_by(df_taxa,class4plot),
                                                                                      tmp=mean(Axis.2)),
                                                                            desc(tmp)),
                                                                    class4plot))))) %>%
            ggplot(aes(y=Axis.2,x=class4plot,fill=class4plot))+
            geom_boxplot()+
            scale_fill_manual(values=rev(pal_tmp),
                              name=ifelse(i=="micro_16s","Bacterial class","Fungal class"))+
            theme_bw()+
            theme(legend.position = "none",
                  axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  axis.title = element_blank())+
            
            df_taxa %>%
            mutate(class4plot=forcats::fct_relevel(class4plot,
                                                   c(as.vector(pull(arrange(summarise(group_by(df_taxa,class4plot),
                                                                                      tmp=mean(Axis.1)),
                                                                            desc(tmp)),
                                                                    class4plot))))) %>%
            ggplot(aes(x=Axis.1,y=class4plot,fill=class4plot))+
            geom_boxplot()+
            scale_fill_manual(values=pal_tmp,
                              name=ifelse(i=="micro_16s","Bacterial class","Fungal class"))+
            theme_bw()+
            theme(legend.position = "none",
                  axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  axis.title = element_blank())+
            
            plot_spacer()+
            plot_layout(guides = "collect",
                        design = layout)
        
        
        pcoas_taxa_list[[i]][[m]] <- gg_taxa
        
        # all traits envfit or only significant
        for(all_traits_or_no in c("alltraits","notalltraits")){
            
            if(all_traits_or_no=="notalltraits"){
                en_coord_cont2 <- en_coord_cont%>%
                    filter(psign<0.05)
            }else{
                en_coord_cont2 <- en_coord_cont
            }
            
            pcoas_list[[i]][[m]][[all_traits_or_no]] <- gg+
                geom_segment(aes(x = 0, y = 0, xend = Axis.1, yend = Axis.2), 
                             data = en_coord_cont2, linewidth =1, colour = "grey65",arrow = arrow(length = unit(0.2,"cm")),lty=ifelse(en_coord_cont2$psign<0.05,1,2)) +
                ggrepel::geom_text_repel(data = en_coord_cont2, aes(x = Axis.1, y = Axis.2), 
                                         fontface = "bold",label=en_coord_cont2$cleannames,color=en_coord_cont2$colorz) + 
                geom_vline(xintercept=0,lty=2)+
                geom_hline(yintercept=0,lty=2)+
                ggpubr::theme_classic2()+
                theme(text=element_text(face="bold"))+
                ggtitle(ifelse(grepl("16s",i),"Bacterial communities","Fungal communities"))
            
        }
        
        #Shepard
        dist_mat <- as.matrix(vegdist(t(dtmp$otu_table),
                                      method = m))
        pcoa_out <- orditmp$vectors
        dist_pcoa <- as.matrix(vegdist(pcoa_out[,1:2],
                                       method = "euclidean")) #,diag=T,upper=T
        
        dist_mat[upper.tri(dist_mat, diag=FALSE)] <- NA
        dist_pcoa[upper.tri(dist_pcoa, diag=FALSE)] <- NA
        
        shepard_list[[i]][[m]] <- dist_mat%>%
            as.data.frame()%>%
            mutate(sample_id=rownames(.))%>%
            reshape2::melt()%>%
            rename(dist=value)%>%
            left_join(rename(pcoa_dist=value,reshape2::melt(mutate(as.data.frame(dist_pcoa),sample_id=rownames(as.data.frame(dist_pcoa))))))%>%
            na.omit()%>%
            ggplot(aes(x=dist,y=pcoa_dist))+
            geom_point()+
            geom_abline(linetype=2,color="red")+
            ggpubr::theme_classic2()+
            xlab(paste0("Distance ",tmp_title))+
            ylab("Eeuclidean distance on\nPCoA 1&2")+
            geom_smooth(method="lm")+
            # coord_fixed()+
            ggpubr::stat_regline_equation(color="blue")+
            scale_y_continuous(expand = expansion(),limits = c(0,1))+
            scale_x_continuous(expand = expansion(),limits = c(0,1))+
            ggtitle(ifelse(grepl("16s",i),"Bacterial communities","Fungal communities"))
        
        
    }
}


## Trait/Microbiota covar ====

### dbRDA ====

#### Run dbRDA and tests ====
dbrda_mod_list <- list()
anova_glob <- list()
anova_margin <- list()

for(i in datasets){ 
    #datasets
    dtmp <- get(i)
    dtmp <- clone(dtmp)
    dtmp_hell <- get(paste0(i,"_hell"))
    dtmp_hell <- clone(dtmp_hell)
    
    # build dbrda dataframe
    df_dbrda <- as.data.frame(dtmp_hell$sample_table[,c("Trtmt",colnames(data_leaf)[Nbcol[-14]])]) # select all variables wanted
    df_dbrda[,colnames(data_leaf)[Nbcol[-14]]] <- df_dbrda[,colnames(data_leaf)[Nbcol[-14]]]
    df_dbrda$variable <- c(gsub("_.*$","",dtmp_hell$sample_table$sample_id))
    
    dbrda_formula <- formula(paste("t(dtmp_hell$otu_table)~", paste0(names(data_leaf)[Nbcol[-14]],collapse = "+")))
    
    # run the dbrda
    dbrda_mod <- vegan::dbrda(dbrda_formula,
                              data=df_dbrda, 
                              distance="horn") #Condition(Trtmt)+ 
    #store
    dbrda_mod_list[[i]] <- dbrda_mod
    
    #Test dbrda model
    #global effect of the model
    anova_glob_tmp <- anova(dbrda_mod)
    anova_glob[[i]] <- anova_glob_tmp%>%
        flextable::flextable()%>%
        flextable::bold(i=~`Pr(>F)`<0.05,j=4)
    
    anova_margin_tmp <- anova(dbrda_mod,by="margin") 
    anova_margin[[i]][["table"]] <- anova_margin_tmp%>%
        flextable::flextable()%>%
        flextable::bold(i=~`Pr(>F)`<0.05,j=4)
   
    #plot inertia outs
 
    anova_margin[[i]][["inertia"]] <- anova_margin_tmp %>%
        broom::tidy() %>%
        rename(rawnames=term) %>%
        left_join(trait_df_name) %>%
        mutate(pct_sos=round((SumOfSqs/sum(SumOfSqs)*100),digits=4)) %>%
        filter(rawnames!="Residual") %>%
        arrange(desc(pct_sos)) %>%
        mutate(cleannames=forcats::fct_inorder(cleannames)) %>%
        ggplot(aes(x=cleannames,y=pct_sos,fill=var_cat))+
        geom_bar(stat = "identity")+
        ggpubr::theme_classic2()+
        scale_fill_manual(values=var_cat_col,name="Trait categorie")+
        theme(axis.text.x = element_text(angle=45,vjust=1,hjust=1))+
        ggtitle(ifelse(grepl("16s",i),"Bacteria","Fungi"))+
        xlab("")+
        ylab("% sum of squares")+
        theme(text=element_text(face="bold"))+
        scale_y_continuous(expand = expansion(add=c(0,0)))
}

#### Plot dbRDA ====


pdbrda_host_list <- list()
for(i in datasets){
    
    # get datasets
    dbrda_mod <- dbrda_mod_list[[i]]
    data_tmp <- get(i)
    
    # get eigenvalue
    eig1_constrained <- round((dbrda_mod$CCA$eig[1]/sum(dbrda_mod$CCA$eig))*100,digits=2)
    eig2_constrained <- round((dbrda_mod$CCA$eig[2]/sum(dbrda_mod$CCA$eig))*100,digits=2)
    eig1_glob <- round((dbrda_mod$CCA$eig[1]/(sum((dbrda_mod$CCA$eig))+sum(dbrda_mod$CA$eig)))*100,digits=2)
    eig2_glob <- round((dbrda_mod$CCA$eig[2]/(sum((dbrda_mod$CCA$eig))+sum(dbrda_mod$CA$eig)))*100,digits=2)
    
    # extract df for ggplot
    fort_dbrda<- fortify(dbrda_mod)
    
    # Get scores
    vars <- ggvegan:::get_dimension_names(fort_dbrda)
    df_dbrda <- fort_dbrda %>%
        left_join(dplyr::rename(data_tmp$sample_table,
                                label=sample_id)) %>%
        filter(score%in%c("biplot","sites"))
    
    #scale variable scores
    want <- df_dbrda$score=="biplot"
    mul <- ggvegan:::arrow_mul(df_dbrda[want, vars, drop = FALSE], 
                               df_dbrda[!want, vars, drop = FALSE])
    df_dbrda[want, vars] <- mul * df_dbrda[want, vars]
    
    df_dbrda_var <- df_dbrda[want,]
    df_dbrda_var %<>%
        rename(rawnames=label) %>%
        left_join(trait_df_name) %>%
        mutate(sig=ifelse(cleannames%in%filter(anova_margin[[i]]$inertia$data,p.value<0.05)$cleannames,"sig","nonsig"))
    
    #Plot
    dbrda_plot_tmp <- ggplot(fort_dbrda,
                             aes(x = dbrda1, y = dbrda2))+
        geom_point(data = df_dbrda[which(df_dbrda$score=="sites"),],
                   mapping=aes(x=dbrda1,y=dbrda2,
                               color=stage(Flowers, 
                                           after_scale = alpha(color, 1))),
                   size=3)+
        scale_color_gradientn(colours=c("#9a133d","lightpink","#c2ceaa","#8bac54"))+ 
        geom_segment(data = df_dbrda_var, # add arrows for variables
                     aes(x = 0, y = 0, xend=dbrda1, yend=dbrda2,linetype=sig),
                     linewidth = .75,
                     arrow = arrow(length = unit(4, "pt"),angle = 30),
                     colour = "black",
                     show.legend = F) +
        scale_linetype_manual(values = c(2,1))+
        # coord_fixed()+ # fix proportion between axis units
        geom_text(data = df_dbrda_var, # add variable names at the end of arrows
                  aes(x = ifelse(dbrda1<0,dbrda1*1.01,dbrda1*1.01), # nudge a bit the coordinates so that they're not on the arrows
                      y = ifelse(dbrda2<0,dbrda2*1.01,dbrda2*1.01),
                      label = cleannames),
                  color= df_dbrda_var$colorz,
                  fontface="bold")+
        theme_classic()+
        xlab(paste0("dbRDA 1 (",eig1_constrained,"%)"))+
        ylab(paste0("dbRDA 2 (",eig2_constrained,"%)"))+
        guides(fill="none")  +
        geom_hline(yintercept=0,linetype = "dashed")+
        geom_vline(xintercept=0,linetype = "dashed")+
        ggtitle(paste0(ifelse(grepl("16s",i),"Bacteria - constrained: ","Fungi - constrained: "),round((summary(dbrda_mod_list[[i]])$constr.chi/summary(dbrda_mod_list[[i]])$tot.chi)*100,digits=2),"%"))+
        theme(plot.title = element_text(face="bold"))+
        theme(text=element_text(face="bold"))
    #sotre
    pdbrda_host_list[[i]] <- dbrda_plot_tmp
}


#### Plot bacterial clusters ====

##### Define clusters ====
df_dbrda_list <- list()
dfbp_list <- list()
for(i in datasets){ 
    #datasets
    dtmp <- get(i)
    dtmp <- clone(dtmp)
    dtmp_hell <- get(paste0(i,"_hell"))
    dtmp_hell <- clone(dtmp_hell)
    
    dbrda_mod <- dbrda_mod_list[[i]]
    data_tmp <- get(i)
    
    # get eigenvalue
    eig1_constrained <- round((dbrda_mod$CCA$eig[1]/sum(dbrda_mod$CCA$eig))*100,digits=2)
    eig2_constrained <- round((dbrda_mod$CCA$eig[2]/sum(dbrda_mod$CCA$eig))*100,digits=2)
    eig1_glob <- round((dbrda_mod$CCA$eig[1]/(sum((dbrda_mod$CCA$eig))+sum(dbrda_mod$CA$eig)))*100,digits=2)
    eig2_glob <- round((dbrda_mod$CCA$eig[2]/(sum((dbrda_mod$CCA$eig))+sum(dbrda_mod$CA$eig)))*100,digits=2)
    
    # extract df for ggplot
    sppscores(dbrda_mod) <- t(dtmp_hell$otu_table)
    fort_dbrda <- fortify(dbrda_mod)
    
    # scale variable scores
    vars <- ggvegan:::get_dimension_names(fort_dbrda)
    
    df_dbrda <- fort_dbrda %>%
        left_join(dplyr::rename(data_tmp$sample_table,label=sample_id))
    df_dbrda <- df_dbrda[df_dbrda$score%in%c("sites","biplot"),]
    want <- df_dbrda$score=="biplot"
    
    df_dbrda_var <- df_dbrda[which(df_dbrda$score=="biplot"),]
    df_dbrda_var %<>%
        rename(rawnames=label) %>%
        left_join(trait_df_name) %>%
        mutate(sig=ifelse(cleannames%in%filter(anova_margin[[i]]$inertia$data,p.value<0.05)$cleannames,"sig","nonsig"))
    
    # Get species coordinates
    d_sp <- data.frame(dbrda_mod$CCA$v)
    names(d_sp) <- tolower(names(d_sp))
    d_sp %<>%
        mutate(label=gsub("^M","",rownames(.)))
    
    mul <- ggvegan:::arrow_mul(df_dbrda[want, vars, drop = FALSE],
                               d_sp[!want, vars, drop = FALSE])
    df_dbrda[want, vars] <- mul * df_dbrda[want, vars]
    
    # Add OTU info
    d_sp2 <- dtmp$otu_table %>%
        data.frame() %>%
        mutate(label=rownames(.))%>%
        reshape2::melt()%>%
        filter(value!=0)%>%
        mutate(sample_id=gsub("X","",variable))%>%
        left_join(data_tmp$sample_table[,c("sample_id","nb_reads")]) %>%
        mutate(value=(value/nb_reads)*100)%>%
        group_by(label)%>%
        summarise(prevalence=n(),
                  relab=mean(value))%>% #all samples rarefied at data_tmp$sample_table$nb_reads
        ungroup()%>%
        mutate(label=gsub("^M","",label))
    
    # Join
    d_sp%<>%
        left_join(d_sp2)
    
    # Partition on coordinates
    maxK <- 15
    rownames(d_sp) <- d_sp$label
    
    bp_tmp <-  NbClust::NbClust(d_sp[,c("dbrda1","dbrda2")],distance = "euclidean",method = "kmeans",min.nc = 2,max.nc = maxK)
    
    dfbp <-  bp_tmp$Best.partition%>%
        reshape2::melt()%>%
        mutate(label=rownames(.),
               value=paste0("clust_",value))%>%
        dplyr::rename(cluster=value)%>%
        left_join(d_sp)
    
    df_dbrda_list[[i]][["otus"]] <- df_dbrda
    df_dbrda_list[[i]][["vars"]] <- df_dbrda_var
    dfbp_list[[i]] <- dfbp
    
}

##### Plot'em ====

#Graph setup

# Arrange cluster by mean position on dbrda1 (for left to right readings)
new_clust_16s <- dfbp_list$micro_16s%>%
    group_by(cluster)%>%
    summarise(avg_dbrda1=mean(dbrda1))%>%
    arrange(avg_dbrda1)%>%
    mutate(new_clust=paste0('Bac ',row_number()))

dfbp_list$micro_16s <- dfbp_list$micro_16s%>%
    left_join(new_clust_16s)%>%
    dplyr::rename(og_clust=cluster,
                  cluster=new_clust)

new_clust_its <- dfbp_list$micro_its%>%
    group_by(cluster)%>%
    summarise(avg_dbrda1=mean(dbrda1))%>%
    arrange(avg_dbrda1)%>%
    mutate(new_clust=paste0('Fun ',row_number()))

dfbp_list$micro_its <- dfbp_list$micro_its%>%
    left_join(new_clust_its)%>%
    dplyr::rename(og_clust=cluster,
                  cluster=new_clust)

#set cluster colors
col_bac <- c("Bac 1"="#5763f5ff","Bac 2"="#fdc067ff","Bac 3"="#6ec5abff","Bac 4"="#ff6983")
col_fun <- c("Fun 1"="#4d8cb0","Fun 2"="#c6671d","Fun 3"="#ba4d81")


###### Gams and dbRDA ====
clust_cum_df_list <- list()
pdbrda_taxa_list <- list()

p_list <- list()
pqbinom_list <- list()   
gam_list <- list()
gam_df <- data.frame()
p_gam_full <- list()
for(i in datasets){
    #get community data
    dtmp <- get(i)
    dtmp <- clone(dtmp)
    # get dbrda data
    dbrda_mod <- dbrda_mod_list[[i]]
    
    # get eigenvalue
    eig1_constrained <- round((dbrda_mod$CCA$eig[1]/sum(dbrda_mod$CCA$eig))*100,digits=2)
    eig2_constrained <- round((dbrda_mod$CCA$eig[2]/sum(dbrda_mod$CCA$eig))*100,digits=2)
    eig1_glob <- round((dbrda_mod$CCA$eig[1]/(sum((dbrda_mod$CCA$eig))+sum(dbrda_mod$CA$eig)))*100,digits=2)
    eig2_glob <- round((dbrda_mod$CCA$eig[2]/(sum((dbrda_mod$CCA$eig))+sum(dbrda_mod$CA$eig)))*100,digits=2)
    
    # extract df 
    df_dbrda <- df_dbrda_list[[i]]$otus
    dfbp <- dfbp_list[[i]]
    # get color
    # was set after first tests so we know how many groups will be identified with clustering fcts
    colors_dbrda <- switch (i,
                            "micro_16s" = col_bac,
                            "micro_its"= col_fun) 
    
    # get traits info
    df_dbrda_traits <- df_dbrda_list[[i]]$vars
    
    # Plot dbrda --
    dbrda_plot_tmp <- 
        ggplot(df_dbrda, aes(x = dbrda1, y = dbrda2))+
        geom_point(data = dfbp,
                   shape=21,
                   mapping=aes(x=dbrda1,y=dbrda2,
                               color=stage(cluster, 
                                           after_scale = alpha(color, 1)),
                               fill = stage(cluster, 
                                            after_scale = alpha(fill, .3)),
                               size=prevalence))+
        scale_fill_manual(values = colors_dbrda,name="OTUs", 
                          aesthetics = c("color", "fill")) +
        scale_size(range = c(1,10),
                   limits = c(1,ifelse(i=="micro_16s",62,80)),
                   breaks = switch(i,"micro_16s"=c(5,10,20,40,60),
                                   "micro_its"=c(5,10,20,40,60,75)))+
        geom_segment(data = df_dbrda_traits, # add arrows for variables
                     aes(x = 0, y = 0, xend=dbrda1, yend=dbrda2,linetype = sig),
                     lineend = "round",
                     linejoin = "round",
                     linewidth = .75,
                     arrow = arrow(length = unit(4, "pt"),
                                   angle = 30),
                     colour = "grey65",
                     show.legend = F) +
        scale_linetype_manual(values = c(2,1))+
        
        # coord_fixed()+ # fix proportion between axis units
        geom_text(data = df_dbrda_traits, # add variable names at the end of arrows
                  aes(x = ifelse(dbrda1<0,dbrda1*1.01,dbrda1*1.01), # nudge a bit the coordinates so that they're not on the arrows
                      y = ifelse(dbrda2<0,dbrda2*1.01,dbrda2*1.01),
                      label = cleannames),
                  color= df_dbrda_traits$colorz,
                  fontface="bold")+
        ggpubr::theme_classic2()+
        xlab(paste0("dbRDA 1 (",eig1_constrained,"%)"))+
        ylab(paste0("dbRDA 2 (",eig2_constrained,"%)"))+
        guides(fill="none")  +
        geom_hline(yintercept=0,linetype = "dashed")+
        geom_vline(xintercept=0,linetype = "dashed")+
        ggtitle(paste0(ifelse(grepl("16s",i),"Bacteria - constrained: ","Fungi - constrained: "),round((summary(dbrda_mod_list[[i]])$constr.chi/summary(dbrda_mod_list[[i]])$tot.chi)*100,digits=2),"%"))+
        theme(plot.title = element_text(face="bold"))+
        theme(text=element_text(face="bold"))+  
        guides(fill = guide_legend(override.aes = list(shape = 21, size = 5, alpha=1)),
               size = guide_legend(override.aes = list(shape = 21, fill="black",alpha=1)))
    
    # store
    pdbrda_taxa_list[[i]] <- dbrda_plot_tmp
    
    # Plot extra info on clusters --
    
    # cluster size
    p_rich <- 
        dfbp%>%
        group_by(cluster)%>%
        summarise(Richness=n())%>%
        ggplot(aes(x=cluster,y=Richness,fill=cluster))+
        geom_bar(stat="identity")+
        scale_fill_manual(values = colors_dbrda,name="OTUs")+
        ggpubr::theme_classic2()+
        theme(text=element_text(face="bold"))+
        ylab("Number of OTUs")+
        scale_y_log10(expand=expansion())+
        theme(axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank())
    # cluster mean non null relative abundance
    p_relab <- 
        dfbp%>%
        ggplot(aes(x=cluster,y=relab,fill=cluster))+
        geom_boxplot(outliers = F)+
        geom_jitter(width=.1,alpha=.3,color="grey30")+
        scale_fill_manual(values = colors_dbrda,name="OTUs",guide = "none")+
        ggpubr::theme_classic2()+
        theme(text=element_text(face="bold"))+
        ylab("Mean non null\nrelative abundance (%)")+
        scale_y_log10(expand=expansion(),limits=c(10e-3,50))+
        theme(axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank())
    
    p_relab <- cld_bxplot_wilcox(data = dfbp,
                                 x = dfbp$cluster,
                                 y =  dfbp$relab,
                                 plot = p_relab,
                                 nudge = 5)
    # cluster prevalence
    p_prev <-
        dfbp%>%
        ggplot(aes(x=cluster,y=(prevalence/ifelse(grepl("16s",i),61,79))*100,fill=cluster))+
        geom_boxplot(outliers = F)+
        geom_jitter(width=.1,alpha=.3,color="grey30")+
        ggpubr::theme_classic2()+
        ylab("OTU prevalence (% of samples)")+
        theme(text=element_text(face="bold"),
              axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank())+
        scale_fill_manual(values = colors_dbrda,name="OTUs",guide = "none")
    
    p_prev <- cld_bxplot_wilcox(data = dfbp,
                                x = dfbp$cluster,
                                y =  (dfbp$prevalence/ifelse(grepl("16s",i),61,79))*100,
                                plot = p_prev,
                                nudge = .05)
    
    # Cluster cumulative abundance
    
    # build df
    df_pca <- data.frame(PCA_plant_trait$ind$coord)
    df_pca$ID <- data_leaf$ID
    df_pca %<>%
        select(ID, Dim.1,Dim.2)
    
    clust_ab <- dtmp$otu_table%>%
        data.frame()%>%
        mutate(label=gsub("^M","",rownames(.)))%>%
        reshape2::melt()%>%
        mutate(number=gsub("^X|_.*$","",variable))%>%
        left_join(mutate(dtmp$sample_table,number=gsub("^X|_.*$","",sample_id)))%>%
        left_join(dfbp)
    
    clust_cum_ab <- clust_ab %>%
        group_by(variable) %>%
        mutate(relab=(value/nb_reads)*100) %>%
        group_by(variable,cluster) %>%
        summarise(cum_relab=sum(relab)) %>%
        left_join(mutate(dtmp$sample_table,variable=paste0("X",sample_id))) %>%
        mutate(cluster=as.factor(cluster)) %>%
        mutate(clust_prop = cum_relab/100)
    
    clust_cum_ab %<>%
        left_join(df_pca)
    
    for(traits_i in c(names(data_leaf)[Nbcol],"Dim.1","Dim.2")){
        # Run gam 
        formula <- paste("clust_prop ~ cluster+ s(",
                         traits_i,
                         ",k = 3, by = cluster, bs='cs')",
                         sep="")
        
        gam_tmp <- gam(formula(formula),
                       data = clust_cum_ab,
                       family = quasibinomial(link = "logit"),
                       # family = gaussian(link = "identity"),
                       method = "REML")
        #store mod
        gam_list[[i]][[traits_i]][["mod"]] <- gam_tmp
        #test of K
        gam_list[[i]][[traits_i]][["basis_dim"]] <- k.check(gam_tmp, subsample=5000, n.rep=400) %>% 
            data.frame() %>% 
            mutate(param=rownames(.),.before=1) %>%
            flextable::flextable()
        #test for concurvity
        
        named_vec <- if(i=="micro_16s"){c("Measure",
                                          "Parameter",
                                          paste0(traits_i,":Bac1"),
                                          paste0(traits_i,":Bac2"),
                                          paste0(traits_i,":Bac3"),
                                          paste0(traits_i,":Bac4"))}else{
                                              c("Measure",
                                                "Parameter",
                                                paste0(traits_i,":Fun1"),
                                                paste0(traits_i,":Fun2"),
                                                paste0(traits_i,":Fun3"))    
                                          }
        
        
        names(named_vec) <- if(i=="micro_16s"){c("measure",
                                                 "para",
                                                 paste0("s.",traits_i,"..clusterBac.1"),
                                                 paste0("s.",traits_i,"..clusterBac.2"),
                                                 paste0("s.",traits_i,"..clusterBac.3"),
                                                 paste0("s.",traits_i,"..clusterBac.4"))}else{
                                                     c("measure",
                                                       "para",
                                                       paste0("s.",traits_i,"..clusterFun.1"),
                                                       paste0("s.",traits_i,"..clusterFun.2"),
                                                       paste0("s.",traits_i,"..clusterFun.3"))    
                                                 }
        
        gam_list[[i]][[traits_i]][["concurvity"]] <- concurvity(gam_tmp,full = T)%>% 
            data.frame() %>% 
            mutate(measure=rownames(.),.before=1) %>%
            flextable::flextable() %>% 
            flextable::labelizor(labels = named_vec)%>%
            flextable::set_formatter(values = function(x) {
                formatC(x, format = "e", digits = 3)
            })
        # Diag
        resid <- residuals(gam_tmp, type = "response")
        linpred <- if (is.matrix(gam_tmp$linear.predictors) && !is.matrix(resid)) {napredict(gam_tmp$na.action, gam_tmp$linear.predictors[, 1])}else {napredict(gam_tmp$na.action, gam_tmp$linear.predictors)}
        fitted <- fitted(gam_tmp)
        response <- gam_tmp$y
        
        qqtmp <- mgcViz::qq(mgcViz::getViz(gam_tmp),
                            a.qqpoi = list("shape" = 19,size=3),
                            a.ablin = list("linetype" = 2))
        
        gam_list[[i]][[traits_i]][["diag"]] <- qqtmp+
            theme(plot.title = element_blank())+
            ggplot(data = cbind(linpred,resid),aes(x=linpred,y=resid))+
            geom_point()+
            geom_hline(yintercept = 0,linetype=2,color="red")+
            theme_linedraw()+
            ylab("Residuals")+
            xlab("Linear predictors")+
            
            ggplot(data = cbind(linpred,resid),aes(x=resid))+
            geom_histogram()+
            geom_vline(xintercept = 0,linetype=2,color="red")+
            theme_linedraw()+
            ylab("Counts")+
            xlab("Residuals")+
            scale_y_continuous(expand=expansion())+
            
            ggplot(data = cbind(fitted,response),aes(x=fitted,y=response))+
            geom_point()+
            geom_abline(linetype=2,color="red")+
            theme_linedraw()+
            ylab("Response")+
            xlab("Fitted")
        
        # Pvals
        gam_list[[i]][[traits_i]][["intercepts_sig"]] <- tidy(gam_tmp,parametric = T)
        gam_list[[i]][[traits_i]][["terms_sig"]] <-  tidy(gam_tmp) 
        gam_df <- tidy(gam_tmp) %>%
            mutate(rawnames=traits_i,
                   cluster=stringr::str_extract(term,"(?<=cluster).*+$")) %>%
            rbind(gam_df)
    }
    
    
    for(traits_i in c(names(data_leaf)[Nbcol],"Dim.1","Dim.2")){
        
        
        traits_i <- traits_i
        colors_dbrda <- colors_dbrda
        clust_cum_ab <- clust_cum_ab
        tmp_fit <- marginaleffects::plot_predictions(gam_list[[i]][[traits_i]][["mod"]],
                                                     type='response',
                                                     condition = c(traits_i,"cluster"))+
            scale_color_manual(values = colors_dbrda)+
            scale_fill_manual(values = colors_dbrda)
        
        kk <- ggplot_build(tmp_fit)
        
        varclean <- if(traits_i%in%trait_df_name$rawnames){
            trait_df_name$cleannames[which(trait_df_name$rawnames==traits_i)]
        }else{
            traits_i
        }
        unitofvar <- if(traits_i%in%trait_df_name$rawnames){
            trait_df_name$unit[which(trait_df_name$rawnames==traits_i)][[1]]
        }else{
            NA
        }
        dtmp <-  kk@plot@data %>%
            filter(cluster%in%pull(filter(gam_df,rawnames==traits_i&p.value<0.05),cluster)) %>%
            mutate(cluster=forcats::fct_relevel(cluster,names(colors_dbrda))) %>%
            mutate(estimate=estimate*100) %>%
            mutate(conf.high=ifelse(conf.high*100>100,100,conf.high*100),
                   conf.low=ifelse(conf.low<0,0,conf.low*100))
        p <-  
            
            ggplot(dtmp)+
            geom_line(aes(x=.data[[traits_i]],
                          y=estimate,
                          color=cluster),
                      show.legend = F)+
            geom_ribbon(aes(x=.data[[traits_i]], 
                            y=estimate,
                            ymax=conf.high, 
                            ymin=conf.low,
                            fill = cluster), 
                        alpha=0.2,
                        show.legend = F)+
            geom_point(data = clust_cum_ab,
                       aes(x = .data[[traits_i]],
                           y = clust_prop*100, 
                           color = cluster),
                       size = 2,
                       alpha = 0.4) +
            scale_color_manual(values = colors_dbrda,drop = F)+
            scale_fill_manual(values = colors_dbrda)+
            ggpubr::theme_classic2() +
            theme(text = element_text(size = 15, face = "bold"),
                  axis.title = element_text(face = "bold"),
                  axis.text = element_text(face = "bold", color = "black"),
                  legend.text = element_text(size = 10),
                  legend.position = "right") +
            guides(color = guide_legend(ncol = 1))+
            xlab(bquote(bold(~.(varclean)~.(unitofvar))))+
            ylab("Relative abundance (%)")+
            
            geom_hline(yintercept = summarise(group_by(clust_cum_ab,cluster),
                                              meany=mean(clust_prop*100))$meany,
                       color = colors_dbrda,
                       linetype=2)+
            
            scale_y_continuous(expand = expansion(),limits = c(-0.01*100,1*100))
        
        pqbinom_list[[i]][[traits_i]] <- p
        
    }
    
    pqbinom_list[[i]] <- pqbinom_list[[i]][c(names(data_leaf)[Nbcol][-14],"Dim.1","Dim.2")]
    
    p_gam_full[[i]] <- ggplot_build(wrap_plots(pqbinom_list[[i]])+
                                        plot_layout(guides="collect",
                                                    axes="collect",
                                                    ncol = 3))@plot
    p_list[[i]] <- p_relab + p_prev + p_rich
}
###### Relative abundance ====

relab_clusters_list <- list()
for(i in datasets){
    
    data_tmp <- get(i)
    data_tmp <- clone(data_tmp)
    
    # get color
    colors_dbrda <- switch (i,
                            "micro_16s" = col_bac,
                            "micro_its"= col_fun)
    
    
    OTU_data <- pdbrda_taxa_list[[i]]$layers[[1]]$data %>%
        left_join(mutate(data_tmp$tax_table,label=gsub("M","",rownames(data_tmp$tax_table))))
    
    pdata <- data_tmp$otu_table %>%
        mutate(label=gsub("M","",rownames(.))) %>%
        reshape2::melt() %>%
        dplyr::rename(sample_id=variable) %>%
        left_join(data_tmp$sample_table) %>%
        mutate(relab=(value/nb_reads)*100) %>%
        
        left_join(OTU_data[,c("label","cluster","dbrda1","dbrda2")]) %>%
        group_by(sample_id,cluster) %>% 
        summarise(relab_clust=sum(relab),
                  Flowers=mean(Flowers),
                  Nmg=mean(Nmg),
                  Pl_H=mean(Pl_H)) %>%
        ungroup()   %>%
        left_join(data_tmp$sample_table)%>%
        arrange(Flowers,Pl_H) %>%
        mutate(sample_id=forcats::fct_inorder(sample_id))
    
    p <- 
        pdata %>%
        ggplot(aes(y=sample_id,x=relab_clust,fill=cluster))+
        geom_bar(stat = "identity",width=1)+
        theme_void()+
        scale_fill_manual(values=colors_dbrda,name="Cluster",guide="none")+
        theme(text=element_text(face="bold"),
              axis.line.x = element_line(),
              axis.text.x = element_text(),
              axis.ticks.x = element_line(),
              axis.ticks.length.x = unit(.2,"line"),
              axis.title.x = element_text(),
              plot.margin = margin(1,10,1,10),
              axis.title.x.top = element_text(color="#649eff"),
              axis.text.x.top = element_text(color="#649eff"),
              legend.position = "none")+
        xlab("Relative abundance (%)")+
        scale_x_continuous(expand = expansion(add=2),
                           sec.axis=sec_axis(~.*(max(pdata$Flowers)/100),name="Flowers"))+
        geom_point(data=unique(pdata[,c("sample_id","Flowers","cluster","Trtmt")]),aes(y=sample_id,x=Flowers/(max(Flowers)/100)),
                   show.legend=F,
                   alpha=1,
                   fill="#649eff",
                   color="black",
                   shape=21,
                   size=2)
    

    relab_clusters_list[[i]] <- p
 
}

###### Taxonomy ====

pcluster_tax <- list()

for(i in datasets){
    #datasets
    data_tmp <- get(i)
    data_tmp <- clone(data_tmp)
    
    OTU_data <- pdbrda_taxa_list[[i]]$layers[[1]]$data %>%
        left_join(mutate(data_tmp$tax_table,
                         tot_reads=rowSums(data_tmp$otu_table),
                         label=gsub("M","",
                                    rownames(data_tmp$tax_table))))
   
    # % diversity
    OTU_data <- OTU_data %>%
        group_by(cluster,Class) %>%
        summarise(nperClass=n(),
                  tot_reads=sum(tot_reads)) %>%
        ungroup()%>%
        group_by(cluster) %>%
        mutate(tot_cluster=sum(nperClass),
               tot_reads_cluster=sum(tot_reads))%>%
        mutate(pct=nperClass/tot_cluster,
               pct_reads=tot_reads/tot_reads_cluster)%>%
        ungroup()%>%
        group_by(Class) %>%
        mutate(tot_Class=sum(nperClass)) %>%
        arrange(tot_Class) %>%
        mutate(Class=ifelse(is.na(Class)|Class=="c__","Not assigned",gsub("c__","",Class))) %>%
        ungroup()%>%
        mutate(Class4plot=forcats::fct_inorder(Class))%>%
        mutate(Class_order=as.numeric(Class4plot)) %>%
        mutate(Class4plot=ifelse(Class_order<max(Class_order)-8,"Others",as.character.factor(Class4plot)))%>%
        mutate(Class4plot=forcats::fct_relevel(Class4plot,"Others","Not assigned",after=Inf))%>%
        group_by(cluster,Class4plot) %>%
        summarise(nperClass=sum(nperClass),
                  pct=sum(pct)*100,
                  pct_reads=sum(pct_reads)*100,
                  tot_cluster=unique(tot_cluster)) %>%
        ungroup()
    # get color
    color_tax <- switch (i,
        "micro_16s" = tax_bac_col,
        "micro_its" = tax_fun_col)
    #legend from compositional plot
    g <- ggplot_build(combined_comp[[ifelse(i=="micro_16s",1,2)]])
    gleg <- data.frame(fill = color_tax,
                       Class = g$plot$scales$scales[[1]]$get_labels()) 
    
    OTU_data %<>%
        mutate(Class=Class4plot) %>% 
        left_join(gleg)
    #names of colorless taxa
    no_col <- OTU_data %>%
        filter(is.na(fill)) %>%
        select(Class) %>%
        distinct() %>%
        pull(Class)
    
    new_col <- c("cornflowerblue","lightyellow","pink","cyan3","darkolivegreen2")[length(no_col)]
    
    colors_tax <- c(gleg$fill,new_col)
    names(colors_tax) <- c(gleg$Class,no_col)
    # get color
    colors_dbrda <- switch (i,
                            "micro_16s" = col_bac,
                            "micro_its"= col_fun)
    
    pcluster_tax[[i]] <- 
    OTU_data %>%
        ggplot(aes(x=cluster,y=pct_reads,fill=Class4plot))+
        geom_bar(stat = "identity")+
        # geom_text(aes(label=nperClass),
        #           position = position_stack(vjust = c(.5)),
        #           size=3)+
        theme_classic()+
        xlab("")+
        ylab("% of Reads")+
        scale_fill_manual(name="Class",values=colors_tax)+
        theme(text=element_text(face="bold"))+
        scale_y_continuous(expand = expansion())
}

##### Repartition across treatments ====

pcluster_trtmt <- 
    micro_16s$otu_table%>%
    mutate(label=gsub("^M","",rownames(.)))%>%
    reshape2::melt()%>%
    dplyr::rename(sample_id=variable)%>%
    left_join(dfbp_list$micro_16s[,c("label","cluster")])%>%
    group_by(sample_id,cluster)%>%
    summarise(reads=sum(value))%>%
    left_join(micro_16s$sample_table)%>%
    mutate(relab=reads/nb_reads)%>%
    ggplot(aes(x=cluster,y=(relab)*100))+
    ggstats::geom_stripped_cols(odd='grey70',even='grey90')+
    geom_boxplot(outliers = F,aes(color=Trtmt),fill=NA)+
    geom_point(position=position_jitterdodge(jitter.width = 0.1),
               alpha=.9,aes(fill=Trtmt),shape=21)+
    scale_fill_manual(values=trtmt_col,guide="none")+
    scale_color_manual(values=trtmt_col,guide="none")+
    ggpubr::theme_classic2()+
    # ggh4x::facet_wrap2(~"Bacteria")+
    theme(axis.title.x = element_blank(),
          text=element_text(face="bold"))+
    ylab("Cluster's cumulative\nrelative abundance (%)")+
    scale_x_discrete(expand = expansion())+

    micro_its$otu_table%>%
    mutate(label=gsub("^M","",rownames(.)))%>%
    reshape2::melt()%>%
    dplyr::rename(sample_id=variable)%>%
    left_join(dfbp_list$micro_its[,c("label","cluster")])%>%
    group_by(sample_id,cluster)%>%
    summarise(reads=sum(value))%>%
    left_join(micro_its$sample_table)%>%
    mutate(relab=reads/nb_reads)%>%
    ggplot(aes(x=cluster,y=(relab)*100))+
    ggstats::geom_stripped_cols(odd='grey70',even='grey90')+
    geom_boxplot(outliers = F,aes(color=Trtmt),fill=NA)+
    geom_point(position=position_jitterdodge(jitter.width = 0.1),
               alpha=.9,aes(fill=Trtmt),shape=21)+
    scale_fill_manual(values=trtmt_col,guide="none")+
    scale_color_manual(values=trtmt_col,guide="none")+
    ggpubr::theme_classic2()+
    # ggh4x::facet_wrap2(~"Bacteria")+
    theme(axis.title.x = element_blank(),
          text=element_text(face="bold"))+
    ylab("Cluster's cumulative\nrelative abundance (%)")+
    scale_x_discrete(expand = expansion())


##Soil microbiota ====


###Composition====
soil_16s <- readRDS("./data/micro_data_16s_soil.rds")
soil_its <- readRDS("./data/micro_data_its_soil.rds")
datasoils <- c("soil_16s","soil_its")
# comp plots
comp_soil_list <- list()
pcoa_soil_list <- list()
rich_soil_list <- list()
for (i in datasoils){
    #datasets
    data_tmp <- get(i)
    data_tmp <- clone(data_tmp)
    
    # % diversity
    doil_tmp <- data_tmp$otu_table %>%
        mutate(OTU_id=rownames(.)) %>%
        reshape2::melt() %>%
        left_join(mutate(data_tmp$tax_table,OTU_id=rownames(data_tmp$tax_table))) %>%
        group_by(variable,Class) %>%
        summarise(nperSampleClass=n(),
                  tot_reads=sum(value)) %>%
        ungroup()%>%
        group_by(variable) %>%
        mutate(tot_sample=sum(nperSampleClass),
               tot_reads_sample=sum(tot_reads))%>%
        mutate(pct=nperSampleClass/tot_sample,
               pct_reads=tot_reads/tot_reads_sample)%>%
        ungroup()%>%
        group_by(Class) %>%
        mutate(tot_Class=sum(nperSampleClass)) %>%
        arrange(tot_Class) %>%
        mutate(Class=ifelse(is.na(Class)|Class=="c__","Not assigned",gsub("c__","",Class))) %>%
        ungroup()%>%
        mutate(Class4plot=forcats::fct_inorder(Class))%>%
        mutate(Class_order=as.numeric(Class4plot)) %>%
        mutate(Class4plot=ifelse(Class_order<max(Class_order)-8,"Others",as.character.factor(Class4plot)))%>%
        mutate(Class4plot=forcats::fct_relevel(Class4plot,"Others","Not assigned",after=Inf))%>%
        group_by(variable,Class4plot) %>%
        summarise(nperSampleClass=sum(nperSampleClass),
                  pct=sum(pct)*100,
                  pct_reads=sum(pct_reads)*100,
                  tot_reads_sample=unique(tot_reads_sample)) %>%
        ungroup()
    
    # get color
    color_tax <- switch (i,
                         "soil_16s" = tax_bac_col,
                         "soil_its" = tax_fun_col)
    #legend from compositional plot
    g <- ggplot_build(combined_comp[[ifelse(i=="soil_16s",1,2)]])
    gleg <- data.frame(fill = color_tax,
                       Class = g$plot$scales$scales[[1]]$get_labels()) 
    
    doil_tmp %<>%
        mutate(Class=Class4plot) %>% 
        left_join(gleg)
    #names of colorless taxa
    no_col <- doil_tmp %>%
        filter(is.na(fill)) %>%
        select(Class) %>%
        distinct() %>%
        pull(Class)
    
    new_col <- c("cornflowerblue","lightyellow","pink","cyan3","darkolivegreen2")[1:length(no_col)]
    
    colors_tax <- doil_tmp[,c("Class4plot","fill")] %>%
        distinct() %>%
        na.omit()
    
    if(length(no_col)>0){
        colors_tax <-  rbind(colors_tax,data.frame(Class4plot=no_col,fill=new_col))
        colors_tax4plot <- colors_tax$fill
        names(colors_tax4plot) <- colors_tax$Class4plot
    }else{
        colors_tax4plot <- colors_tax$fill
        names(colors_tax4plot) <- colors_tax$Class4plot 
    }
    
    comp_soil_list[[i]] <- doil_tmp %>%
        left_join(rename(data_tmp$sample_table,variable=sample_id)) %>%
        ungroup()%>%
        mutate(Trtmt=ifelse(Trtmt=="P",
                            "Poor\nsoil (P)",
                            ifelse(Trtmt=="I",
                                   "Intermediate\nsoil (I)",
                                   "Rich\nsoil (R)")))%>%
        mutate(Trtmt=forcats::fct_relevel(Trtmt, c("Poor\nsoil (P)",
                                                       "Intermediate\nsoil (I)",
                                                       "Rich\nsoil (R)")))%>%
        ggplot(aes(x=variable,y=pct_reads,fill=Class4plot))+
        geom_bar(stat="identity")+
        scale_fill_manual(values=colors_tax4plot,name="Class")+
        facet_wrap(~Trtmt,scales="free_x")+
        ggpubr::theme_classic2()+
        scale_y_continuous(expand=c(0,0))+
        theme(axis.text.x = element_blank(),
              axis.title.x = element_blank(),
              axis.ticks.x = element_blank(),
              text=element_text(face="bold"))+
        ylab("Relative abundance (%)")
    
    rich_soil_list[[i]] <- 
    data_tmp$sample_table %>%
        ggplot(aes(x=Trtmt,y=nb_motus,color=Trtmt))+
        geom_point(size=3)+
        scale_color_manual(values=c("#ffe1b0","#6b5e49","#0f0d0a"),name="Treatments")+
        ggpubr::theme_classic2()+
        theme(axis.text.x = element_blank(),
              axis.title.x = element_blank(),
              axis.ticks.x = element_blank(),
              text=element_text(face="bold"))+
        ylab("Number of OTUs")
    
    # Pcoa soil
    data_tmp_hell <- clone(data_tmp)
    rownames(data_tmp_hell$tax_table) <- data_tmp_hell$tax_table$label
    data_tmp_hell$otu_table <- t(labdsv::hellinger(t(data_tmp_hell$otu_table)))
    data_tmp_hell$cal_betadiv(method = "horn")
    data_tmp_hell$sample_table <- mutate(data_tmp_hell$sample_table,Trtmt=forcats::fct_relevel(Trtmt,c("P","I","R")))
    rownames(data_tmp_hell$tax_table) <- rownames(data_tmp_hell$otu_table)
    t1 <- trans_beta$new(dataset = data_tmp_hell, group = "Trtmt", measure = "horn")
    
    t1$cal_ordination(method = "PCoA")
    # plot the PCoA result 
    pcoa_soil_list[[i]] <- 
        t1$plot_ordination(plot_color = "Trtmt", plot_type = c("point"))+
        ggpubr::theme_classic2()+
        scale_color_manual(values=c("#ffe1b0","#6b5e49","#0f0d0a"), 
                           labels=c("Poor","Intermediate","Rich"),
                           name="Soils")+
        theme(text=element_text(face="bold"))+
        ggtitle(ifelse(grepl("16s",i),"Bacteria","Fungi"))
    
}

### Overlap with plant ====


soil_plots_list <- list()
# formula <- "relab ~ issoil + s(Dim.1,k = 3, by = issoil, bs='cs')"

for (i in datasets){
    
    #get data
    dtmp <- get(i)
    dtmp <- clone(dtmp)
    
    if(grepl("16s",i)){
        dsoil <- soil_16s
    }else{
        dsoil <- soil_its
    }
    dsoil <- clone(dsoil)
    
    # Get sequences found in each original sooils
    soilPseq <- gsub("s__","",dsoil$tax_table$sequence[which(rowSums(dsoil$otu_table[,1:3])>0)])
    soilPSseq <- gsub("s__","",dsoil$tax_table$sequence[which(rowSums(dsoil$otu_table[,4:6])>0)])
    soilSPseq <- gsub("s__","",dsoil$tax_table$sequence[which(rowSums(dsoil$otu_table[,7:9])>0)])
    
    # prepare dataframes
    comtmp <- dtmp$otu_table %>%
        mutate(sequence=dtmp$tax_table$sequence)%>%
        reshape2::melt()
    
    soiltmp <- dsoil$otu_table %>%
        mutate(sequence=gsub("s__","",dsoil$tax_table$sequence))%>%
        reshape2::melt()
    
    # distance to original soil
    all <- rbind(comtmp,soiltmp)    
    
    comall <- all %>%
        tidyr::pivot_wider(names_from = variable,values_from = value) %>%
        data.frame()
    rownames(comall)  <- comall$sequence
    
    comall[is.na(comall)] <- 0
    comall <- comall[,-1]
    comall_hell <- labdsv::hellinger(t(comall))
    
    pcoasoilplant <- wcmdscale(vegdist(comall_hell,method = "horn"))
    
    dad <- vegdist(comall_hell,method = "horn")
    dad <- as.matrix(dad)
    dad[upper.tri(dad, diag=FALSE)] <- NA
    
    soil_plots_list[["dist2soil"]][[i]] <- dad%>%
        as.data.frame()%>%
        mutate(sample_id=gsub("^X","",rownames(.)))%>%
        reshape2::melt()%>%
        mutate(variable=gsub("^X","",variable)) %>%
        rename(dist=value)%>%
        left_join(select(dtmp$sample_table,Trtmt,sample_id))%>%
        rename(Trtmt_1=Trtmt,
               sample_1=sample_id,
               sample_id=variable)%>%
        left_join(select(dtmp$sample_table,Trtmt,sample_id))%>%
        rename(Trtmt_2=Trtmt,
               sample_2=sample_id)%>%
        filter(!is.na(dist))%>%
        mutate(substrate1=ifelse(grepl("SP",sample_1),"P",
                                 ifelse(grepl("PS",sample_1),"I",
                                        ifelse(grepl("P",sample_1),"R",stringr::str_extract(Trtmt_1,"^[A-Z]{1}")))),
               substrate2=ifelse(grepl("SP",sample_2),"P",
                                 ifelse(grepl("PS",sample_2),"I",
                                        ifelse(grepl("P",sample_2),"R",stringr::str_extract(Trtmt_2,"^[A-Z]{1}")))))%>%
        
        mutate(sample_type1=ifelse(is.na(Trtmt_1),"soil","leaves"),
               sample_type2=ifelse(is.na(Trtmt_2),"soil","leaves")) %>%
        filter(sample_type1=="soil") %>%
        filter(sample_type2=="leaves")%>%
        filter(substrate1==substrate2) %>%
        group_by(sample_2) %>%
        mutate(meandist=mean(dist)) %>%
        select(sample_2,Trtmt_2,meandist)%>%
        distinct() %>%
        ggplot(aes(x=Trtmt_2,y=meandist))+
        geom_boxplot(outliers = F,aes(fill=Trtmt_2))+
        geom_point(position = position_jitter(width = .3,height = 0))+
        scale_fill_manual(values = trtmt_col)+
        labs(y="Distance to original soil\n(Morisita-Horn)",
             x="",
             fill="Treatment")+
        ggpubr::theme_classic2()+
        theme(text = element_text(face="bold"))
    


    #Plot relab of soil derived OTUs in our treatments
    
    soil_plots_list[["soilderived"]][[i]] <- comtmp %>%
        left_join(select(dtmp$tax_table,otu_id,sequence))%>%
        left_join(select(rename(dtmp$sample_table,variable=sample_id),variable,Trtmt,substrate))%>%
        mutate(issoil=ifelse((substrate=="P"&sequence%in%soilPseq)|(substrate=="SP"&sequence%in%soilSPseq)|(substrate=="PS"&sequence%in%soilPSseq),"yes","no")) %>%
        # mutate(issoil=ifelse(sequence%in%soiltmp$sequence,"yes","no")) %>%
        filter(value>0)%>%
        group_by(variable,issoil)%>%
        summarise(totval=sum(value))%>% 
        group_by(variable) %>%
        mutate(relab=totval/sum(totval))%>%
        ungroup()%>%
        rename(sample_id=variable) %>%
        left_join(dtmp$sample_table)%>%
        ggplot(aes(x=Trtmt,y=relab*100))+
        geom_boxplot(aes(fill=issoil),alpha=.6)+
        geom_point(position=position_jitterdodge(jitter.width = 0.5),
                   aes(fill = issoil),
                   show.legend = F,
                   shape=21,
                   color=scales::col_mix("black", "white", 0.302))+
        scale_fill_manual(values=c("darkseagreen",
                                   "#572d13"))+
        # scale_color_manual(values=c("darkseagreen",
        #                            "#572d13"))+
        labs(y="Relative abundance (%)",
             x="",
             fill="Found in soil")+
        ggpubr::theme_classic2()+
        theme(text = element_text(face="bold"))
    
    # GAM relab soil associated OTUs
    # build df
    
    #add pca coord info
    df_pca <- data.frame(PCA_plant_trait$ind$coord)
    df_pca$ID <- data_leaf$ID
    df_pca %<>%
        select(ID, Dim.1,Dim.2)
    
    clust_absoil <- comtmp %>%
        left_join(select(dtmp$tax_table,otu_id,sequence))%>%
        left_join(select(rename(dtmp$sample_table,variable=sample_id),variable,Trtmt,substrate))%>%
        mutate(issoil=ifelse((substrate=="P"&sequence%in%soilPseq)|(substrate=="SP"&sequence%in%soilSPseq)|(substrate=="PS"&sequence%in%soilPSseq),"yes","no")) %>%
        # mutate(issoil=ifelse(sequence%in%soiltmp$sequence,"yes","no")) %>%
        filter(value>0)%>%
        group_by(variable,issoil)%>%
        summarise(totval=sum(value))%>% 
        group_by(variable) %>%
        mutate(relab=totval/sum(totval))%>%
        ungroup()%>%
        rename(sample_id=variable) %>%
        left_join(dtmp$sample_table) %>%
        mutate(issoil=as.factor(issoil)) %>%
        left_join(df_pca) %>%
        left_join(monoind)
    
    # Run gam 
    
    for(traits_i in c(names(data_leaf)[Nbcol],"Dim.1","Dim.2","monodim")){
        # Run gam 
        formula <- paste("relab ~ issoil + s(",
                         traits_i,
                         ",k = 3, by = issoil, bs='cs')",
                         sep="")
        
        
        gam_tmp <- gam(formula(formula),
                       data = clust_absoil,
                       family = quasibinomial(link = "logit"),
                       # family = gaussian(link = "identity"),
                       method = "REML")
        #store mod
        gam_soilab <- gam_tmp
        soil_plots_list[["GAM"]][["theGam"]][[i]][[traits_i]] <- gam_soilab
        #test of K
        soil_plots_list[["GAM"]][["kcheck"]][[i]][[traits_i]] <- 
            k.check(gam_tmp, subsample=5000, n.rep=400) %>% 
            data.frame() %>% 
            mutate(param=rownames(.),.before=1) %>%
            flextable::flextable() #k.index < 1 may indicate too low k value as returned by gam.check
        #test for concurvity
        soil_plots_list[["GAM"]][["concurvity"]][[i]][[traits_i]] <- 
            concurvity(gam_tmp,full = T)%>% 
            data.frame() %>% 
            mutate(measure=rownames(.),.before=1) %>%
            rename(Measure=measure,
                   Parameter=para) %>%
            rename(!!paste0(stringr::str_extract(colnames(.)[3],
                                                 "(?<=s\\.).+(?=\\.\\.)"),":NotFoundInSoil"):=3) %>%
            rename(!!paste0(stringr::str_extract(colnames(.)[4],
                                                 "(?<=s\\.).+(?=\\.\\.)"),":FoundInSoil"):=4) %>% 
            flextable::flextable()%>%
            flextable::set_formatter(values = function(x) {
                formatC(x, format = "e", digits = 3)
            }) # high values (say above .7-.8) can indicate "colinearity"/"concurvity" (not exactly the same but similar idea)
        
        # plot Diag
        resid <- residuals(gam_tmp, type = "response")
        linpred <- if (is.matrix(gam_tmp$linear.predictors) && !is.matrix(resid)) {napredict(gam_tmp$na.action, gam_tmp$linear.predictors[, 1])}else {napredict(gam_tmp$na.action, gam_tmp$linear.predictors)}
        fitted <- fitted(gam_tmp)
        response <- gam_tmp$y
        
        qqtmp <- mgcViz::qq(mgcViz::getViz(gam_tmp),
                            a.qqpoi = list("shape" = 19,size=3),
                            a.ablin = list("linetype" = 2))
        
        soil_plots_list[["GAM"]][["diagplot"]][[i]][[traits_i]] <- 
            qqtmp+
            theme(plot.title = element_blank())+
            ggplot(data = cbind(linpred,resid),aes(x=linpred,y=resid))+
            geom_point()+
            geom_hline(yintercept = 0,linetype=2,color="red")+
            theme_linedraw()+
            ylab("Residuals")+
            xlab("Linear predictors")+
            
            ggplot(data = cbind(linpred,resid),aes(x=resid))+
            geom_histogram()+
            geom_vline(xintercept = 0,linetype=2,color="red")+
            theme_linedraw()+
            ylab("Counts")+
            xlab("Residuals")+
            scale_y_continuous(expand=expansion())+
            
            ggplot(data = cbind(fitted,response),aes(x=fitted,y=response))+
            geom_point()+
            geom_abline(linetype=2,color="red")+
            theme_linedraw()+
            ylab("Response")+
            xlab("Fitted")
        
        # plot gamfit
        # get Pvals
        var <- unique(stringr::str_extract(tidy(gam_tmp)$term,"(?<=s\\().+(?=\\))"))
        gam_df <- tidy(gam_tmp) %>%
            mutate(rawnames=var,
                   issoil=stringr::str_extract(term,"(?<=issoil).*+$")) 
        
        #pot fit
        tmp_fit <- marginaleffects::plot_predictions(gam_tmp,
                                                     type='response',
                                                     condition = c(var,"issoil"))+
            scale_color_manual(values = c("darkseagreen",
                                          "#572d13"))+
            scale_fill_manual(values = c("darkseagreen",
                                         "#572d13"))
        
        #get to ggplot and custom
        kk <- ggplot_build(tmp_fit)
        
        varclean <- if(var%in%trait_df_name$rawnames){
            trait_df_name$cleannames[which(trait_df_name$rawnames==var)]
        }else{
            var
        }
        unitofvar <- if(var%in%trait_df_name$rawnames){
            trait_df_name$unit[which(trait_df_name$rawnames==var)][[1]]
        }else{
            NA
        }
        soil_plots_list[["GAM"]][["gamsoil"]][[i]][[traits_i]] <- 
            kk@plot@data %>%
            filter(issoil%in%pull(filter(gam_df,rawnames==var&p.value<0.05),issoil)) %>%
            
            mutate(estimate=estimate*100) %>%
            mutate(conf.high=ifelse(conf.high*100>100,100,conf.high*100),
                   conf.low=ifelse(conf.low<0,0,conf.low*100)) %>%
            ggplot()+
            geom_line(aes(x = .data[[var]],y=estimate,color=issoil),show.legend = F)+
            geom_ribbon(aes(x=.data[[var]], y=estimate, ymax=conf.high, ymin=conf.low,fill = issoil), 
                        alpha=0.2,show.legend = F)+
            geom_point(data = clust_absoil,
                       aes(x = .data[[var]], y = relab*100, color = issoil),
                       size = 2,
                       alpha = 0.4) +
            scale_color_manual(values = c("darkseagreen",
                                          "#572d13"),drop = F)+
            scale_fill_manual(values = c("darkseagreen",
                                         "#572d13"))+
            ggpubr::theme_classic2() +
            theme(text = element_text(size = 15, face = "bold"),
                  axis.title = element_text(face = "bold"),
                  axis.text = element_text(face = "bold", color = "black"),
                  legend.text = element_text(size = 10),
                  legend.position = "right") +
            guides(color = guide_legend(ncol = 1))+
            xlab(bquote(bold(~.(varclean)~.(unitofvar))))+
            ylab("Relative abundance (%)")+
            
            geom_hline(yintercept = summarise(group_by(clust_absoil,issoil),meany=mean(relab*100))$meany,
                       color = c("darkseagreen",
                                 "#572d13"),
                       linetype=2)+
            
            scale_y_continuous(expand = expansion(),limits = c(-0.01*100,1*100))
        
    }
    
}

wrap_plots(soil_plots_list[["GAM"]][["gamsoil"]]$micro_16s)+
    plot_layout(guides="collect",axes="collect")
wrap_plots(soil_plots_list[["GAM"]][["gamsoil"]]$micro_its)+
    plot_layout(guides="collect",axes="collect")
### Overlap with bac clusters ====
# Get sequences found in each original soils

pclust_soil_list <- list()
for(i in datasets){
    
    soil_tmp <- get(paste0("soil_",gsub("micro_","",i)))
    soil_tmp <- clone(soil_tmp)
    data_tmp <- get(i)
    data_tmp <- clone(data_tmp)
    # Get sequences found in each original soils
    soilPseq <- gsub("s__","",soil_tmp$tax_table$sequence[which(rowSums(soil_tmp$otu_table[,1:3])>0)])
    soilPSseq <- gsub("s__","",soil_tmp$tax_table$sequence[which(rowSums(soil_tmp$otu_table[,4:6])>0)])
    soilSPseq <- gsub("s__","",soil_tmp$tax_table$sequence[which(rowSums(soil_tmp$otu_table[,7:9])>0)])
    
    
    df_soils <- pdbrda_taxa_list[[i]]$layers[[1]]$data %>%
        left_join(select(mutate(data_tmp$tax_table,label=gsub("M","",otu_id)),label,sequence)) %>%
        mutate(is_found_in_rich=ifelse(sequence%in%soilPseq,1,0)) %>%
        mutate(is_found_in_inter=ifelse(sequence%in%soilPSseq,1,0)) %>%
        mutate(is_found_in_poor=ifelse(sequence%in%soilSPseq,1,0)) %>%
        group_by(label) %>%
        mutate(count_different_soils=sum(c(is_found_in_rich,
                                           is_found_in_inter,
                                           is_found_in_poor))) %>%
        mutate(is_in_one_soil=ifelse(count_different_soils>0,1,0)) %>%
        ungroup() %>%
        group_by(cluster) %>%
        summarise(Yes=sum(is_in_one_soil),
                  No=n()-sum(is_in_one_soil)) 
    
    if(i=="micro_16s"){
        df_rect <- df_soils %>%
            rename(ymax=Yes) %>%
            mutate(ymin=0,
                   xmax=c(1.45,2.45,3.45,4.45),
                   xmin=c(0.55,1.55,2.55,3.55),
                   cluster=c("Bac 1","Bac 2","Bac 3","Bac 4")) %>%
            left_join(p_rich$data)
    }else{
        df_rect <- df_soils %>%
            rename(ymax=Yes) %>%
            mutate(ymin=0,
                   xmax=c(1.45,2.45,3.45),
                   xmin=c(0.55,1.55,2.55),
                   cluster=c("Fun 1","Fun 2","Fun 3")) %>%
            left_join(p_rich$data)    
    }
    
    pclust_soil_list[[i]][["pclust_richsoil"]] <- local({
        i<-i
        df_rect %>%
            ggplot(aes(xmin=xmin,
                       xmax=xmax,
                       ymin=ymin,
                       ymax=Richness,
                       fill=cluster,
                       color=switch(i,
                                    "micro_16s"=c("Yes","No","Yes","No"),
                                    "micro_its"=c("Yes","No","Yes"))))+
            geom_rect(alpha=.3,show.legend = F)+
            scale_fill_manual(values=switch(i,
                                            "micro_16s"=col_bac,
                                            "micro_its"=col_fun
            ))+
            geom_rect(data=df_rect,mapping=aes(xmin=xmin,
                                               xmax=xmax,
                                               ymin=ymin,
                                               ymax=ymax ,
                                               fill=cluster))+
            scale_y_log10()+
            ggpubr::theme_classic2()+
            theme(axis.text.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.title.x = element_blank(),
                  text=element_text(face="bold"))+
            geom_text(inherit.aes = F,data=switch(i,
                                                  "micro_16s"=data.frame(x=c(1,1,2,2,3,3,4,4,
                                                                             1,2,3,4),
                                                                         y=c(0.5,7.5,1,6.5,228.5,1928,12,29.5,
                                                                             20,20,4200,50)),
                                                  "micro_its"=data.frame(x=c(1,1,2,2,3,3,
                                                                             1,2,3),
                                                                         y=c(8.5,30,85,900,2.5,7.5,
                                                                             50,2200,15))),
                      mapping=aes(x=x,
                                  y=y,
                                  label=switch(i,
                                               "micro_16s"=c(paste0(1," (",round((1/13)*100,digits=2),"%)"),
                                                             paste0(12," (",round((12/13)*100,digits=2),"%)"),
                                                             paste0(2," (",round((2/10)*100,digits=2),"%)"),
                                                             paste0(8," (",round((8/10)*100,digits=2),"%)"),
                                                             paste0(457,"\n(",round((457/3398)*100,digits=2),"%)"),
                                                             paste0(2941,"\n(",round((2941/3398)*100,digits=2),"%)"),
                                                             paste0(24,"\n(",round((24/34)*100,digits=2),"%)"),
                                                             paste0(10," (",round((10/34)*100,digits=2),"%)"),
                                                             13,10,3398,34),
                                               "micro_its"=c(paste0(24," (",round((24/41)*100,digits=2),"%)"),
                                                             paste0(17," (",round((17/41)*100,digits=2),"%)"),
                                                             paste0(174," (",round((174/2159)*100,digits=2),"%)"),
                                                             paste0(1985," (",round((1985/2159)*100,digits=2),"%)"),
                                                             paste0(5,"\n(",round((5/10)*100,digits=2),"%)"),
                                                             paste0(5,"\n(",round((5/10)*100,digits=2),"%)"),
                                                             41,2159,10))),
                      fontface="bold",
                      color=switch (i,
                                    "micro_16s" = c(rep("black",8),col_bac),
                                    "micro_its" = c(rep("black",6),col_fun)),
                      size=switch (i,
                                   "micro_16s" = c(rep(3,8),rep(5,4)),
                                   "micro_its" = c(rep(3,6),rep(5,3))))+
            ylab("Number of OTUs")+
            scale_color_manual(values=c(NA,NA),name="Detected in\nsoil samples")+
            guides(color=guide_legend(override.aes = list(alpha=c(.2,1))))+
            theme(legend.position = "inside",
                  legend.position.inside = c(.2,.8))+
            guides(fill=F)
        
    })
    
    pclust_soil_list[[i]][["pclust_distrsoil"]] <- local({
        i<-i
        soil_tmp$otu_table%>%
            mutate(label=gsub('M','',rownames(.)))%>%
            reshape2::melt()%>%
            dplyr::rename(sample_id=variable)%>% 
            left_join(dplyr::select(mutate(soil_tmp$tax_table,
                                           label=gsub('M','',rownames(soil_tmp$tax_table)),
                                           sequence=gsub("s__","",sequence)),label,sequence))%>%
            left_join(select(left_join(dfbp_list[[i]],
                                       select(mutate(select(data_tmp$tax_table,sequence,otu_id),
                                                     label=gsub("M","",otu_id)),-c(otu_id))),-label))%>%
            
            filter(value>0)%>%
            
            left_join(soil_tmp$sample_table)%>%
            group_by(Trtmt,cluster)%>%  
            summarise(count=length(unique(label)),
                      meanreads=mean(value),
                      minreads=min(value),
                      maxreads=max(value),
                      reads=sum(value))%>%
            ungroup() %>%
            group_by(Trtmt) %>%
            
            mutate(cluster_cum_relab=reads/sum(reads),
                   otu_mean_relab=meanreads/sum(reads),
                   otu_min_relab=minreads/sum(reads),
                   otu_max_relab=maxreads/sum(reads))%>%
            mutate(cluster=ifelse(is.na(cluster),"Not found in plants",cluster))%>%
            filter(cluster!="Not found in plants") %>%
            ungroup() %>%
            mutate(Trtmt=ifelse(Trtmt=="P",
                                "Poor\nsoil (P)",
                                ifelse(Trtmt=="I",
                                       "Intermediate\nsoil (I)",
                                       "Rich\nsoil (R)")))%>%
            mutate(Trtmt=forcats::fct_relevel(Trtmt, c("Poor\nsoil (P)",
                                                       "Intermediate\nsoil (I)",
                                                       "Rich\nsoil (R)"))) %>%
            arrange(Trtmt,cluster) %>%
            mutate(xcoord=switch(i,
                                 "micro_16s"=c(1:4,1:4,3,4),
                                 "micro_its"=c(1:3,1:3,1:3))) %>% 
            
            
            ggplot()+
            geom_rect(aes(xmin=xcoord-.25,
                          xmax=xcoord+.25,
                          ymin=0,
                          ymax=cluster_cum_relab*100,
                          fill=cluster,
                          color=cluster),
                      alpha=.2,
                      show.legend = F)+
            geom_segment(aes(x=xcoord,
                             xend=xcoord,
                             yend=otu_max_relab*100,
                             y=otu_min_relab*100,
                             color=cluster),show.legend = F)+
            geom_point(aes(x=xcoord,
                           y=otu_mean_relab*100, 
                           fill=cluster),
                       shape=23,size=2,show.legend = F)+
            
            geom_text(aes(x=xcoord,
                          y=(cluster_cum_relab*100)+(cluster_cum_relab*100)*1,
                          label=count,
                          color=cluster),
                      fontface="bold",
                      show.legend = F)+
            
            scale_y_log10(breaks=c(.01,.1,1,10,50))+
            facet_wrap(~Trtmt)+
            scale_fill_manual(values = switch(i,
                                              "micro_16s"=col_bac,
                                              "micro_its"=col_fun))+
            scale_color_manual(values = switch(i,
                                               "micro_16s"=col_bac,
                                               "micro_its"=col_fun))+
            ggpubr::theme_classic2()+
            theme(axis.text.x = element_blank(),
                  axis.title.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  text=element_text(face="bold"),
                  legend.title = element_blank())+
            ylab("Relative\nabundance\nin soils (%)")+
            geom_point(aes(x=1,y=1, size = switch(i,
                                                  "micro_16s"=rep(c("Individual\nOTUs"),10),
                                                  "micro_its"=rep(c("Individual\nOTUs"),9))),alpha=0)+
            geom_line(aes(x=1,y=1, linetype = switch(i,
                                                     "micro_16s"=rep(c("Individual\nOTUs"),10),
                                                     "micro_its"=rep(c("Individual\nOTUs"),9))),alpha=0)+
            
            geom_point(aes(x=1,y=-1,shape=switch(i,
                                                 "micro_16s"=rep(c("Cumulated"),10),
                                                 "micro_its"=rep(c("Cumulated"),9))),
                       fill=alpha("black",.2),
                       color="black")+
            
            guides(shape=guide_legend(override.aes = list(shape=22,
                                                          # alpha=.2,
                                                          # fill="black",
                                                          size=4,
                                                          # color="black",
                                                          stroke=1)),
                   size=guide_legend(override.aes = list(shape=23,
                                                         fill="black",
                                                         size=2,
                                                         alpha=1,
                                                         color="black")))
        
    })
    
}


#> Clean & Save env ====
rm(list = ls()[grepl("tmp", ls())])
gc()
save.image("env_post_analysis.RData")
#> GGsaves ====

## MAIN ====

##Fig I ====
# PCA
ggsave(plot=plot_pca,
       filename = "results/figures/main/pca_plant.svg", # save to svg for aesthetic edition with Inkscape: https://inkscape.org/
       device = "svg",
       width = 9, 
       height = 7,
       units = "in")

ggsave("results/figures/main/POV_pca_dim.svg",bar_traits_var2,height = 4,width = 5)

ggsave("results/figures/main/flowers_trtmt.svg",pflowers,height = 3.18, width = 3.18)



##Fig II ====


# PCoA plots with betadisp insets (a & f)
ptmp16s <- pcoas_list$micro_16s$horn$alltraits
ptmp16s$layers <- ptmp16s$layers[c(1,5,6)]
ptmp16s <- ptmp16s+
    annotate(geom="text",label=paste0("R2=",
                                      round(sum(pull(as_tibble(permanova_list$micro_16s$horn$global$body$dataset),R2)[1:3]),digits=2)),
             fontface="bold",
             x=-0.8,
             y=.71,
             hjust=0)+
    theme(plot.title=element_blank())

ptmp16s2 <- pbetadisp_list$micro_16s$horn+
    ylab("\u03b2-dispersion")+
    theme(legend.position = "none",
          axis.line.x = element_blank(),
          axis.line.y = element_line(linewidth = .5),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(color="black",
                                      fill=NA,
                                      linewidth = .5),
          text=element_text(face="bold",size=8))

ptmp16s2$layers <- ptmp16s2$layers[c(1,3)]
ptmp16s2$layers$annotate$aes_params$size <- 3
ptmp16s3 <- 
    cowplot::ggdraw() +
    cowplot::draw_plot(ptmp16s+
                           theme(legend.position = "none")) +
    cowplot::draw_plot(ptmp16s2,
                       x = .15, y = .55, width = .35, height = .35)


ptmpits <- pcoas_list$micro_its$horn$alltraits
ptmpits$layers <- ptmpits$layers[c(1,5,6)]
ptmpits <- ptmpits+
    annotate(geom="text",label=paste0("R2=",
                                      round(sum(pull(as_tibble(permanova_list$micro_its$horn$global$body$dataset),R2)[1:3]),digits=2)),
             fontface="bold",
             x=-0.8,
             y=.5,
             hjust=0)+
    theme(plot.title=element_blank())

ptmpits2 <- pbetadisp_list$micro_its$horn+
    ylab("\u03b2-dispersion")+
    theme(legend.position = "none",
          axis.line.x = element_blank(),
          axis.line.y = element_line(linewidth = .5),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(color="black",
                                      fill=NA,
                                      linewidth = .5),
          text=element_text(face="bold",size=8))

ptmpits2$layers <- ptmpits2$layers[c(1,3)]
ptmpits2$layers$annotate$aes_params$size <- 3
ptmpits3 <- 
    cowplot::ggdraw() +
    cowplot::draw_plot(ptmpits+
                           theme(legend.position = "none")) +
    cowplot::draw_plot(ptmpits2,
                       x = .15, y = .55, width = .35, height = .35)


# Composition plot (b & g)

combined_comp <- combined_comp

# Observed richness (c & h)
prich16s <- flower_richness_list$micro_16s$sr
prichits <- flower_richness_list$micro_its$sr

# Community dissimilarity (d & i)
proll16s <- list_beta_proll$micro_16s$`ws:3` #5% samples windows
prollits <- list_beta_proll$micro_its$`ws:4`

# Prevalence groups (e & j)
pprev16s <- prev_relab_flowers$micro_16s
pprevits <- prev_relab_flowers$micro_its

# Design for patchwork
design <- "
AF
BG
CH
DI
EJ
"
# Patch it

P <- 
    ptmp16s3+ 
    
    (combined_comp[[1]]+
         ylab("Class relative\nabundance (%)")+
         theme(plot.title = element_blank(),
               legend.position = "inside",
               legend.position.inside = c(.7,.2),
               legend.title = element_blank(),
               legend.background = element_rect(fill = fill_alpha("white",.4)),
               legend.key = element_blank())+
         guides(fill=guide_legend(theme(legend.direction = "horizontal"),
                                  ncol = 3,
                                  reverse = T)))+
    (prich16s+
         scale_x_continuous(expand = expansion(),limits = c(-5,510))+ #,limits = c(-8,4.5)
         theme(plot.title = element_blank(),
               axis.text.x = element_blank(),
               legend.position = "none",
               text = element_text(face="bold")))+
    (proll16s+
         scale_x_continuous(expand = expansion(),limits = c(-5,510))+ #,limits = c(-8,4.5)
         theme(plot.title = element_blank(),
               legend.title = element_text(size=12),
               legend.position = "none",
               axis.title.y = element_text(hjust = .5))+
         ylab("Community dissimilarity\n(rolling window mean)"))+
    (pprev16s+
         scale_x_continuous(expand = expansion(),limits = c(-5,510))+ #,limits = c(-8,4.5)
         theme(plot.title = element_blank(),
               axis.title.x = element_blank(),
               axis.text.x = element_blank(),
               legend.position = "none",
               axis.title.y = element_text(hjust = .5))+
         ylab("OTUs prevalence groups\ncumulative relative abundance (%)"))+
    
    ptmpits3+
    (combined_comp[[2]]+
         theme(plot.title = element_blank(),
               legend.position = "inside",
               legend.position.inside = c(.7,.2),
               legend.title = element_blank(),
               legend.background = element_rect(fill = fill_alpha("white",.4)),
               legend.key = element_blank())+
         guides(fill=guide_legend(theme(legend.direction = "horizontal"),
                                  ncol = 3,
                                  reverse = T)))+
    (prichits+
         scale_x_continuous(expand = expansion(),limits = c(-5,510))+ #,limits = c(-8,4.5)
         theme(plot.title = element_blank(),
               axis.text.x = element_blank(),
               legend.position = "none",
               legend.title = element_text(size=12),
               text = element_text(face="bold")))+
    (prollits+
         scale_x_continuous(expand = expansion(),limits = c(-5,510))+ #,limits = c(-8,4.5)
         theme(plot.title = element_blank(),
               axis.title.y = element_blank(),
               legend.position = "none",
               legend.title = element_text(size=12)))+
    (pprevits+
         scale_x_continuous(expand = expansion(),limits = c(-5,510))+ #,limits = c(-8,4.5)
         theme(plot.title = element_blank(),
               axis.title = element_blank(),
               axis.text.x = element_blank(),
               legend.position = "none"))+
    
    plot_layout(widths = c(1,1),heights = c(1,1,1,1,1),design = design)+
    plot_annotation(tag_level = "a")

ggsave("results/figures/main/com_div.svg",P,width = 14,height = 15)
 
##Fig III ====

layout <- c(
    area(1,1,8,8),
    
    area(9,1,10,5),
    area(9,6,10,8),
    
    area(11,1,12,2),
    area(11,3,12,4),
    area(11,5,12,6),
    area(11,7,12,8)
)

P <- 
    pdbrda_taxa_list$micro_16s +
    theme(legend.position="inside",
          legend.position.inside = c(.5,.2),
          legend.box = "horizontal",
          legend.background = element_blank(),
          legend.key = element_blank()) +
    labs(tag="a")+
    inset_element(pclust_soil_list$micro_16s$pclust_richsoil+
                      labs(tag="b")+
                      theme(legend.position = "inside",
                            legend.position.inside = c(.1,.9),
                            legend.text = element_text(size=6),
                            legend.title = element_text(size=8),
                            legend.background = element_blank(),
                            legend.key = element_blank(),
                            axis.title.y = element_text(size=8)), left = .42, bottom = 0.6, right = 0.6, top = .9) + 
    
    inset_element(pclust_soil_list$micro_16s$pclust_distrsoil +
                      theme(legend.position="inside",
                            legend.position.inside = c(.5,-0.2),
                            legend.background = element_blank(),
                            legend.key = element_blank(),
                            legend.spacing = unit(5,"pt"),
                            axis.title.y = element_text(size=8))+
                      ylab("Relative abundance in soils (%)")+
                      labs(tag="c") , left = .62, bottom = 0.6, right = 0.8, top = .9)+
    inset_element(p_list$micro_16s[[2]]+
                      theme(legend.position = "none",
                            axis.title.y = element_text(size=8)) +
                      labs(tag="d"), left = .82, bottom = 0.6, right = 1, top = .9)+
    inset_element(p_list$micro_16s[[1]]+
                      theme(legend.position = "none",
                            axis.title.y = element_text(size=8)) +
                      labs(tag="e"), left = .82, bottom = 0.2, right = 1, top = .5)+
    relab_clusters_list$micro_16s+
    coord_flip()+
    ggpubr::theme_classic2()+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(),
          axis.line.y = element_line(),
          axis.text.y.right = element_text(color="cornflowerblue"),
          axis.ticks.y = element_line(),
          axis.ticks.length.y = unit(5,"pt"),
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y.left = element_text(angle=90),
          axis.title.y.right = element_text(angle=-90,color="cornflowerblue")) +
    labs(tag="f")+
    
    pcluster_tax$micro_16s+
    theme(legend.position="bottom",
          legend.key = element_blank(),
          legend.background = element_blank(),
          legend.key.size = unit(6, 'pt'),
          legend.title = element_text(size=8),
          legend.text = element_text(size=6),
          legend.box.spacing = margin())+
    labs(tag="g")+
    
    p_gam_full$micro_16s[[3]]+
    theme(legend.position = "none") +
    labs(tag="h")+
    p_gam_full$micro_16s[[16]]+
    theme(legend.position = "none",
          axis.text.y = element_blank(),
          axis.title.y = element_blank()) +
    labs(tag="i")+
    p_gam_full$micro_16s[[19]]+
    theme(legend.position = "none",
          axis.text.y = element_blank(),
          axis.title.y = element_blank()) +
    labs(tag="j")+
    p_gam_full$micro_16s[[20]]+
    theme(legend.position = "none",
          axis.text.y = element_blank(),
          axis.title.y = element_blank()) +
    labs(tag="k")+
    plot_layout(design=layout)&
    theme(text=element_text(face="bold",size=12))



ggsave("results/figures/main/fig3.svg",P,width = 12,height = 15)

## To be added with inkscape
coord_stars <- cumsum(100*pull(as_tibble(permanova_list$micro_16s$horn$global$body$dataset),R2)[c(4,1,2,3)])

ppermanova <-
    ggplot(data=tibble(yy=100*pull(as_tibble(permanova_list$micro_16s$horn$global$body$dataset),R2)[1:4],
                       fillfill=forcats::fct_relevel(as.factor(pull(as_tibble(permanova_list$micro_16s$horn$global$body$dataset),Term)[1:4]),
                                                     c("Residuals","Drought:Substrate","Substrate","Drought"))),
           aes(x=1,
               y=yy,
               fill=fillfill))+
    geom_col(stat="identity")+
    geom_text(aes(y=c(NA,
                      mean(c(coord_stars[1],coord_stars[2])),
                      mean(c(coord_stars[2],coord_stars[3])),
                      mean(c(coord_stars[3],coord_stars[4]))),
                  x=1,
                  label = "**"),
              color="white",
              size=8)+
    theme_void()+
    scale_fill_manual(values=rev(c("darkgrey","#FED789FF","#023743FF","#476F84FF")),
                      name="Terms")+
    coord_flip()+
    theme(text=element_text(face="bold"))

ggsave("results/figures/main/pcoa_perma_16s.svg",ppermanova,width = 3,height = 1)

coord_stars <- cumsum(100*pull(as_tibble(permanova_list$micro_its$horn$global$body$dataset),R2)[c(4,1,2,3)])

ppermanova <-
    ggplot(data=tibble(yy=100*pull(as_tibble(permanova_list$micro_its$horn$global$body$dataset),R2)[1:4],
                       fillfill=forcats::fct_relevel(as.factor(pull(as_tibble(permanova_list$micro_its$horn$global$body$dataset),Term)[1:4]),
                                                     c("Residuals","Drought:Substrate","Substrate","Drought"))),
           aes(x=1,
               y=yy,
               fill=fillfill))+
    geom_col(stat="identity")+
    geom_text(aes(y=c(NA,
                      mean(c(coord_stars[1],coord_stars[2])),
                      mean(c(coord_stars[2],coord_stars[3])),
                      mean(c(coord_stars[3],coord_stars[4]))),
                  x=1,
                  label = "**"),
              color="white",
              size=8)+
    theme_void()+
    scale_fill_manual(values=rev(c("darkgrey","#FED789FF","#023743FF","#476F84FF")),
                      name="Terms")+
    coord_flip()+
    theme(text=element_text(face="bold"))

ggsave("results/figures/main/pcoa_perma_its.svg",ppermanova,width = 3,height = 1)

## SUPPLEMENTARY ====


## Fig. S1 ====
ggsave("results/figures/draft/supp/fig/S1.png",
       soil_chi_plot, 
       height = 4.6,width=4.6)


## Fig. S2 ====

ggsave("results/figures/draft/supp/fig/S2.png",
wrap_plots(rich_soil_list$soil_16s+
               ggtitle("Bacteria")+
               rich_soil_list$soil_its+
               ggtitle("Fungi")+
               plot_layout(guides="collect")&
               theme(legend.position = "none"))+
    (wrap_plots(pcoa_soil_list)+
         plot_layout(guides='collect'))+
    wrap_plots(comp_soil_list)+
    
    plot_layout(ncol=1)+
    plot_annotation(tag_levels = "a")&
    theme(legend.text = element_text(size=8),
          legend.key.size = unit(10,"pt")),width = 11, height = 7.5)

## Fig. S3 ====
ggsave("results/figures/draft/supp/fig/S3.png",
       ss_summary,
       height = 2.5,
       width = 4)

## Fig. S4 ====
ptrait_range+
    plot_annotation(tag_levels = "a")&
    theme(text=element_text(face="bold")) 
    ggsave("results/figures/draft/supp/fig/S4.png",ptrait_range+
               plot_annotation(tag_levels = "a")&
               theme(text=element_text(face="bold")),width = 12, height = 14)
## Fig. S5 ====

layout <- c(
    area(1,1,11,11),
    area(1,12,11,22),
    
    area(12,1,22,11),
    
    area(12,12,22,20),
    area(12,21,22,22)
)

p <- contrib_var+
    corrplot_pca_traits+
    theme(axis.text = element_text(size=10))+
    bar_traits_var[[1]]+
    theme(legend.position = "right",
          legend.direction = "vertical",
          text=element_text(face="bold"),
          axis.title.x = element_text(vjust = 15))+
    bar_traits_var[[2]]+
    plot_layout(guides = "collect",design = layout)+
    plot_annotation(tag_levels = list(c("a","b","c","d","")))&
    theme(element_text(face = "bold"))

ggsave("results/figures/draft/supp/fig/S5.png",p,width = 12.3,height = 9.69)

## Fig. S6 ====

ggsave("results/figures/draft/supp/fig/S6.png",
bxp_alpha_trtmt$micro_16s+
    ggtitle("Bacteria")+
    theme(legend.position = "right")+
    bxp_alpha_trtmt$micro_its+
    ggtitle("Fungi")+
    plot_layout(nrow = 2, guides = "collect"),
width = 8,
height = 6)

## Fig. S7 ====
ggsave("results/figures/draft/supp/fig/S7.png",
       anova_margin$micro_16s$inertia+
           ylab("% dbRDA sum\nof squares"),
       width = 6.3,
       height = 3.3)
## Fig. S8 ====

P <- pdbrda_taxa_list$micro_its +
    theme(legend.position="inside",
          legend.position.inside = c(.8,.2),
          legend.box = "horizontal",
          legend.background = element_blank(),
          legend.key = element_blank()) +
    labs(tag="a")+
    inset_element(pclust_soil_list$micro_its$pclust_richsoil+
                      labs(tag="b")+
                      theme(legend.position = "inside",
                            legend.position.inside = c(.1,.9),
                            legend.text = element_text(size=6),
                            legend.title = element_text(size=8),
                            legend.background = element_blank(),
                            legend.key = element_blank(),
                            axis.title.y = element_text(size=8)),
                  left = .05, bottom = 0.7, right = .2, top = 1) + 
    
    inset_element(pclust_soil_list$micro_its$pclust_distrsoil +
                      theme(legend.position="inside",
                            legend.position.inside = c(.5,-0.2),
                            legend.background = element_blank(),
                            legend.key = element_blank(),
                            legend.spacing = unit(5,"pt"),
                            axis.title.y = element_text(size=8))+
                      ylab("Relative abundance\nin soils (%)")+
                      labs(tag="c") , 
                  left = .21, bottom = 0.7, right = 0.36, top = 1)+
    inset_element(p_list$micro_its[[2]]+
                      theme(legend.position = "none",
                            axis.title.y = element_text(size=8)) +
                      labs(tag="d")+
                      ylab("OTU prevalence\n(% of samples)"),
                  left = .05, bottom = 0.4, right = .2, top = .69)+
    inset_element(p_list$micro_its[[1]]+
                      theme(legend.position = "none",
                            axis.title.y = element_text(size=8)) +
                      labs(tag="e")+
                      ylab("Mean non-null relative\nabundance (%)"), 
                  left = .21, bottom = 0.4, right = .36, top = .69)+
    relab_clusters_list$micro_its+
    coord_flip()+
    ggpubr::theme_classic2()+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(),
          axis.line.y = element_line(),
          axis.text.y.right = element_text(color="cornflowerblue"),
          axis.ticks.y = element_line(),
          axis.ticks.length.y = unit(5,"pt"),
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y.left = element_text(angle=90),
          axis.title.y.right = element_text(angle=-90,color="cornflowerblue")) +
    labs(tag="f")+
    
    pcluster_tax$micro_its+
    theme(legend.position="bottom",
          legend.key = element_blank(),
          legend.background = element_blank(),
          legend.key.size = unit(6, 'pt'),
          legend.title = element_text(size=8),
          legend.text = element_text(size=6),
          legend.box.spacing = margin())+
    labs(tag="g")+
    
    p_gam_full$micro_its[[3]]+
    theme(legend.position = "none") +
    labs(tag="h")+
    p_gam_full$micro_its[[16]]+
    theme(legend.position = "none",
          axis.text.y = element_blank(),
          axis.title.y = element_blank()) +
    labs(tag="i")+
    p_gam_full$micro_its[[19]]+
    theme(legend.position = "none",
          axis.text.y = element_blank(),
          axis.title.y = element_blank()) +
    labs(tag="j")+
    p_gam_full$micro_its[[20]]+
    theme(legend.position = "none",
          axis.text.y = element_blank(),
          axis.title.y = element_blank()) +
    labs(tag="k")+
    plot_layout(design=layout)&
    theme(text=element_text(face="bold",size=12))

ggsave("results/figures/draft/supp/fig/S8.svg",P,width = 12,height = 15)

## Fig. S9 ====

ggsave("results/figures/draft/supp/fig/S9.png",
       wrap_plots(p_gam_full$micro_16s)+
           plot_layout(guides = "collect", axes="collect")+
           plot_annotation(tag_levels = 'a'),
       width = 12,
       height = 14)

## Fig. S10 ====
ggsave("results/figures/draft/supp/fig/S10.png",
       
(micro_16s$otu_table%>%
    mutate(label=gsub("^M","",rownames(.)))%>%
    reshape2::melt()%>%
    dplyr::rename(sample_id=variable)%>%
    left_join(dfbp_list$micro_16s[,c("label","cluster")])%>%
    group_by(sample_id,cluster)%>%
    summarise(reads=sum(value))%>%
    left_join(micro_16s$sample_table)%>%
    mutate(relab=reads/nb_reads)%>%
    filter(cluster=='Bac 4') %>%
    # filter(Trtmt%in%c("R-D")) %>%
    mutate(ishighB4=ifelse(relab<0.15,"no","yes")) %>%
    mutate(ishighB4=forcats::fct_relevel(ishighB4,"yes","no")) %>%
    tidyr::pivot_longer(names(data_leaf)[Nbcol][-14]) %>%
    left_join(rename(trait_df_name,name=rawnames)) %>%
    ggplot(aes(x=ishighB4,y=value))+
    geom_boxplot(outliers = F)+
    geom_point(aes(color=ishighB4))+
    scale_color_manual(values=c("yes"="red","no"="darkgreen"),name=">15% of Bac 4\nrelative abundance")+
    facet_wrap(~cleannames,scale="free_y")+
    ylab("Trait value")+
    xlab('')+
    ggtitle("All plants"))/

(micro_16s$otu_table%>%
    mutate(label=gsub("^M","",rownames(.)))%>%
    reshape2::melt()%>%
    dplyr::rename(sample_id=variable)%>%
    left_join(dfbp_list$micro_16s[,c("label","cluster")])%>%
    group_by(sample_id,cluster)%>%
    summarise(reads=sum(value))%>%
    left_join(micro_16s$sample_table)%>%
    mutate(relab=reads/nb_reads)%>%
    filter(cluster=='Bac 4') %>%
    filter(Trtmt%in%c("R-D")) %>%
    mutate(ishighB4=ifelse(relab<0.15,"no","yes")) %>%
    mutate(ishighB4=forcats::fct_relevel(ishighB4,"yes","no")) %>%
    tidyr::pivot_longer(names(data_leaf)[Nbcol][-14]) %>%
    left_join(rename(trait_df_name,name=rawnames)) %>%
    ggplot(aes(x=ishighB4,y=value))+
    geom_boxplot(outliers = F)+
    geom_point(aes(color=ishighB4))+
    scale_color_manual(values=c("yes"="red","no"="darkgreen"),name=">15% of Bac 4\nrelative abundance")+
    facet_wrap(~cleannames,scale="free_y")+
    ylab("Trait value")+
    xlab('')+
    ggtitle("Only R-D plants"))+
    
    plot_layout(guides = "collect")+
    plot_annotation(tag_levels = "a"),

width = 9,
height = 10)


#> Flextable saves ====

## TS1 ====
flex_pairadonpca_res1 <- pairadonpca_res1%>%
    flextable::add_footer_lines(values = "Results of the pairwise PERMANOVA performed on the plant trait PCA. Treatments are: R-W, Rich-Wet; I-W, Intermediate-Wet; P-W, Poor-Wet; P-D, Poor-Dry; I-D, Intermediate-Dry and R-D, Rich-Dry. Significant adjusted (α<0.05) p values are in bold.")%>%
    flextable::fontsize(part = "footer", size=6)  %>%
    flextable::align(align = "justify", part = "footer")  


flextable::save_as_docx(flex_pairadonpca_res1,
                        path="results/figures/draft/supp/table/TS1.docx")  
## TS2 ====
 
# This is Old School code but i'm nostalgic of the phd student I was so I kept it like that

#Build data frame and run KW tests
Stat_Frame <- matrix(0,18,4)
data2 <- data_leaf[,c(1:5,Nbcol[-14])]
col_idx <- 5
for ( i in data2[,c(6:23)]){
    col_idx <- col_idx + 1
    name <- names(data2[col_idx])
    row <- as.numeric(match(name, names(data2)))
    #kruskal
    a <- kruskal.test(i~data2$Trtmt)
    chi2kw <- a$statistic[[1]]
    df <- a$parameter[[1]]
    pval <- a$p.value
    Stat_Frame[row-5, 1] <- name
    Stat_Frame[row-5, 2] <- df
    Stat_Frame[row-5, 3] <- chi2kw
    Stat_Frame[row-5, 4] <- pval
}
colnames(Stat_Frame) <- c("var","df","chi2","krusk_pval")

# Get mean and sd for each traits
Mean_sd_Frame <- matrix(0,18,15)
col_idx <- 5
for ( i in data2[,c(6:23)]){
    col_idx <- col_idx + 1
    name <- names(data2[col_idx])
    moy <- as.numeric(tapply(i,data2$Trtmt,mean,na.rm = T))
    sd <- as.numeric(tapply(i,data2$Trtmt,sd,na.rm = T))
    row <- as.numeric(match(name, names(data2)))
    mean_glob <- as.numeric(mean(i,na.rm = T))
    sd_glob <- as.numeric(sd(i,na.rm = T))
    coef_var <- (sd_glob/mean_glob)*100
    Mean_sd_Frame[row-5,1] <- name
    Mean_sd_Frame[row-5,2] <- signif(moy[1],digits = 3)
    Mean_sd_Frame[row-5,4] <- signif(moy[2],digits = 3)
    Mean_sd_Frame[row-5,6] <- signif(moy[3],digits = 3)
    Mean_sd_Frame[row-5,8] <- signif(moy[4],digits = 3)
    Mean_sd_Frame[row-5,10] <- signif(moy[5],digits = 3)
    Mean_sd_Frame[row-5,12] <- signif(moy[6],digits = 3)
    
    Mean_sd_Frame[row-5,3] <- signif(sd[1],digits = 3)
    Mean_sd_Frame[row-5,5] <- signif(sd[2],digits = 3)
    Mean_sd_Frame[row-5,7] <- signif(sd[3],digits = 3)
    Mean_sd_Frame[row-5,9] <- signif(sd[4],digits = 3)
    Mean_sd_Frame[row-5,11] <- signif(sd[5],digits = 3)
    Mean_sd_Frame[row-5,13] <- signif(sd[6],digits = 3)
    
    Mean_sd_Frame[row-5,14] <- signif(mean_glob,digits = 3)
    Mean_sd_Frame[row-5,15] <- signif(sd_glob,digits = 3)
    
}

colnames(Mean_sd_Frame) <- c("var",
                             "P-D_mu","P-D_sd",
                             "I-D_mu","I-D_sd",
                             "R-D_mu", "R-D_sd",
                             "P-W_mu","P-W_sd",
                             "I-W_mu","I-W_sd",
                             "R-W_mu", "R-W_sd",
                             "global_mu", "global_sd")
Mean_sd_Frame <- as.data.frame(Mean_sd_Frame)

letter_comp <- matrix(0,18,7)

# Run pairwise wilcox tests and get cld
col_idx <- 5
for (i in data2[,c(6:23)]){
    
    col_idx <- col_idx + 1
    cld_vec <- pairwise_letterZ(data2,data2$Trtmt,i)
    # cld_vec <- cld_vec[[1]]
    name <- names(data2[col_idx])
    row <- as.numeric(match(name, names(data2)))
    
    a <- ifelse(kruskal.test(i,data2$Trtmt)$p.value<0.05,1,0) 
    
    letter_comp[row-5,1] <- name
    ifelse(a==1,letter_comp[row-5,2] <- cld_vec[1],letter_comp[row-5,2] <- " ")
    ifelse(a==1,letter_comp[row-5,3] <- cld_vec[2],letter_comp[row-5,3] <- " ")
    ifelse(a==1,letter_comp[row-5,4] <- cld_vec[3],letter_comp[row-5,4] <- " ")
    ifelse(a==1,letter_comp[row-5,5] <- cld_vec[4],letter_comp[row-5,5] <- " ")
    ifelse(a==1,letter_comp[row-5,6] <- cld_vec[5],letter_comp[row-5,6] <- " ")
    ifelse(a==1,letter_comp[row-5,7] <- cld_vec[6],letter_comp[row-5,7] <- " ")
}

letter_df <- as.data.frame(letter_comp)
names(letter_df) <- c("var","P-D","I-D","R-D","P-W","I-W","R-W")
 
# Merge all these informations in one data frame
Mean_sd_Frame %<>%           
    mutate(`KW-chi`=as.numeric(Stat_Frame[,3]),
           `P-val`=as.character(Stat_Frame[,4])) %>%
    dplyr::select(-c(global_mu,global_sd)) 
 
Mean_sd_Frame %<>% 
    tibble::add_column(.after = 3,V2=ifelse(letter_df$`P-D`!= " ",paste0("(",letter_df$`P-D`,")")," "))%>%
    tibble::add_column(.after = 6,V3=ifelse(letter_df$`I-D`!= " ",paste0("(",letter_df$`I-D`,")")," "))%>%
    tibble::add_column(.after = 9,V4=ifelse(letter_df$`R-D`!= " ",paste0("(",letter_df$`R-D`,")")," "))%>%
    tibble::add_column(.after = 12,V5=ifelse(letter_df$`P-W`!= " ",paste0("(",letter_df$`P-W`,")")," "))%>%
    tibble::add_column(.after = 15,V6=ifelse(letter_df$`I-W`!= " ",paste0("(",letter_df$`I-W`,")")," "))%>%
    tibble::add_column(.after = 18,V7=ifelse(letter_df$`R-W`!= " ",paste0("(",letter_df$`R-W`,")")," "))
 
NewVarNames <-  trait_df_name$cleannames
 
options(scipen=999)
Mean_SD_Flex <-   Mean_sd_Frame%>%
    # dplyr::slice(-14)%>%
    mutate(var = NewVarNames[-14])%>%
    tidyr::unite("P-D",c(`P-D_mu`,`P-D_sd`),sep=" ± ")%>%
    tidyr::unite("I-D",c(`I-D_mu`,`I-D_sd`),sep=" ± ")%>%
    tidyr::unite("R-D",c(`R-D_mu`,`R-D_sd`),sep=" ± ")%>%
    tidyr::unite("P-W",c(`P-W_mu`,`P-W_sd`),sep=" ± ")%>%
    tidyr::unite("I-W",c(`I-W_mu`,`I-W_sd`),sep=" ± ")%>%
    tidyr::unite("R-W",c(`R-W_mu`,`R-W_sd`),sep=" ± ")%>%  
    tidyr::unite("P-D",c(`P-D`,V2), sep = " ")%>%
    tidyr::unite("I-D",c(`I-D`,V3), sep = " ")%>%
    tidyr::unite("R-D",c(`R-D`,V4), sep = " ")%>%
    tidyr::unite("P-W",c(`P-W`,V5), sep = " ")%>%
    tidyr::unite("I-W",c(`I-W`,V6), sep = " ")%>%
    tidyr::unite("R-W",c(`R-W`,V7), sep = " ")%>%
    mutate(`P-val`=round(as.numeric(`P-val`),digits=6))%>%
    mutate(`KW-chi`=round(as.numeric(`KW-chi`),digits=3))%>%
    mutate(`P-val`=ifelse(`P-val`<0.0001,"<0.0001",`P-val`))%>%
    flextable::flextable()%>%
    flextable::fontsize(size=8, part = "body")%>%
    flextable::height_all(height = .3 )%>%
    # theme_vanilla()%>%
    flextable::add_header_row(top = T,values = c("Traits","Mean ± SD","Significance" ), colwidths = c(1,6,2))%>%
    flextable::set_header_labels("var"=NA,"variation_coeff"="%")%>%
    flextable::align(align = "center", part = "all")%>%
    flextable::compose(j=1,i=10, value = flextable::as_paragraph("F",flextable::as_sub("v"),"F",flextable::as_sub("m")))%>%
    flextable::compose(j=1,i=11, value = flextable::as_paragraph("ETR",flextable::as_sub("max")))%>%
    flextable::bold(i=~`P-val`<0.05,j=9)%>%
    flextable::fontsize(i=2,part="header",size=10)%>%
    flextable::fontsize(i=1,part="header",size=12)%>%
    flextable::width(j=1:9,width = c(2,3,3,3,3,3,3,1.5,2.5),unit = "cm")%>%
    flextable::add_footer_lines(values = "Statistical summary table. For each trait the mean ± standard deviation (SD) is displayed for the three treatments. The associated Kruskal Wallis Chi² and P-values are shown. Significant P-values (<0.05) are in bold. Letters within brackets indicates significant pairwise differences, Wilcoxon pairwise test (\U03B1 < 0.05). Treatments are: R-W, Rich-Wet; I-W, Intermediate-Wet; P-W, Poor-Wet; P-D, Poor-Dry; I-D, Intermediate-Dry and R-D, Rich-Dry. Traits are: the Number of leaves, Plant height (cm),  longest leaf length (leaf length, cm), longest leaf width (leaf width, cm), leaf mass area (LMA, gm-2),  leaf dry matter content (LDMC, g g-1), Stomatal density (Stomata mm-2), leaf chlorophyll content (Chlorophyll, mg g-1), maximum quantum yield of photosystem II (Fv/Fm), maximum electron transport rate (ETRmax, µmol photon m-2 s-1), Midday leaf water potential (Water potential, MPa), relative water content (RWC, %),  leaf Soluble sugars, Carbon, Nitrogen and Phosphorus content (mg mg-1) and Number of Flowers.")%>%
    flextable::fontsize(part = "footer", size=6)%>%
    flextable::set_table_properties(width=0)
 
spec_properties <- officer::prop_section(
    page_size = officer::page_size(orient = "portrait",
                          width = (30/2.54), height = (30/2.54)),
    type = "continuous",
    page_margins = officer::page_mar(bottom = .05,
                            top = .05,
                            right = 1,
                            left = 1,
                            header = 0.5,
                            footer = 0.5,
                            gutter = 0.5)
)

flextable::save_as_docx(Mean_SD_Flex,path="results/figures/draft/supp/table/TS2.docx", pr_section = spec_properties)

flextable::save_as_docx(Mean_SD_Flex,
                        path="results/figures/draft/supp/table/TS2.docx",
                        pr_section = spec_properties)
## TS3 ====

micro_16s$otu_table%>%
    mutate(label=rownames(micro_16s$otu_table))%>%
    reshape2::melt()%>%
    group_by(label)%>%
    summarise(value=sum(value))%>%
    left_join(mutate(micro_16s$tax_table,label=rownames(micro_16s$tax_table)))%>%
    group_by(Class)%>%
    summarise(value=sum(value),
              count=n())%>%
    ungroup()%>%
    mutate(relab=(value/sum(micro_16s$otu_table))*100,
           relcount=(count/nrow(micro_16s$otu_table))*100)%>%
    arrange(desc(relab))%>%
    mutate(Class=ifelse(Class=="c__","unknown",gsub("c__","",Class)))%>%
    dplyr::rename(Class=Class,
                  Tot_reads=value,
                  Unique_sequence=count,
                  Proportion_reads=relab,
                  Proportion_sequences=relcount)%>%
    write.csv2("results/figures/draft/supp/table/TS3.csv",row.names = F)
## TS4 ====

micro_its$otu_table%>%
    mutate(label=rownames(micro_its$otu_table))%>%
    reshape2::melt()%>%
    group_by(label)%>%
    summarise(value=sum(value))%>%
    left_join(mutate(micro_its$tax_table,label=rownames(micro_its$tax_table)))%>%
    group_by(Class)%>%
    summarise(value=sum(value),
              count=n())%>%
    ungroup()%>%
    mutate(relab=(value/sum(micro_its$otu_table))*100,
           relcount=(count/nrow(micro_its$otu_table))*100)%>%
    arrange(desc(relab))%>%
    mutate(Class=ifelse(Class=="c__","unknown",gsub("c__","",Class)))%>%
    dplyr::rename(Class=Class,
                  Tot_reads=value,
                  Unique_sequence=count,
                  Proportion_reads=relab,
                  Proportion_sequences=relcount)%>%
    write.csv2("results/figures/draft/supp/table/TS4.csv",row.names = F)
## TS5 ====
permanova_list$micro_16s$horn$global$body$dataset %>%
    mutate(Taxa=c('Bacteria','','','',''),.before = 1) %>%
    rbind(permanova_list$micro_its$horn$global$body$dataset %>%
                  mutate(Taxa=c('Fungi','','','',''),.before = 1)) %>%
    mutate(across(c(4,5,6),~round(.x,digits = 3))) %>%
    flextable::flextable() %>%
    flextable::bold(i=~`pval`<0.05,j=7) %>%
    flextable::align(align = "center", part = "all") %>% 
    flextable::add_footer_lines(values = "PERMANOVA outputs for the effects of drought, substrate and their interaction of microbial communities. R2 sum to 0.34 and 0.20 for bacterian and fungi respectively. Bold p values indicate significance (α<0.05).")%>%
    flextable::fontsize(part = "footer", size=6)  %>%
    flextable::fontsize(size=8, part = "body")%>%
    flextable::fontsize(size=10, part = "header")%>%
    flextable::width(j=1:7,width = c(2,3,1,3,3,2,2),unit = "cm") %>%
    flextable::align(align = "justify", part = "footer") %>%
    flextable::save_as_docx(path="results/figures/draft/supp/table/TS5.docx")


## TS6 ====

flextable::flextable(select(rbind(mutate(kw_list$micro_16s$body$dataset,
                                         name=c("Inverse simpson","Shannon entropy","Observed richness")),
                                  mutate(kw_list$micro_its$body$dataset,
                                         name=c("Inverse simpson","Shannon entropy","Observed richness"))),-Test)) %>%
    flextable::bold(i=~`p`<0.05,j=7)%>%
    flextable::align(align = "center", part = "all") %>%    
    flextable::add_footer_lines(values = "Effects of the treatments on α-diversity indices of the Hill number framework  (different q values): observed richness (q=0), exponential of the Shannon index (q=1) and Inverse Simpson (q=2). Bold p values indicate significance (α<0.05).")%>%
    flextable::fontsize(part = "footer", size=6)  %>%
    flextable::fontsize(size=8, part = "body")%>%
    flextable::fontsize(size=10, part = "header")%>%
    flextable::width(j=1:8,width = c(3,3,2,1,3,1,3,3),unit = "cm") %>%
    flextable::align(align = "justify", part = "footer") %>%
    flextable::save_as_docx(path="results/figures/draft/supp/table/TS6.docx")


## TS7 ====
flex_dbrda_glob <- anova_glob$micro_16s$body$dataset %>%
    data.frame() %>%
    mutate(marker=c("Bacteria",""),
           `% inertia`=c(round((SumOfSqs[1]/sum(SumOfSqs))*100,digits = 3),""),
           ` `=c("Model","Residuals"))%>%
    select(c(5,7,3,1,6,4)) %>%
    rename(Taxa=1,`F stat`=3,Df=4,`P-val`=6) %>%
    add_row()%>%
    bind_rows(anova_glob$micro_its$body$dataset %>%
                  data.frame() %>%
                  mutate(marker=c("Fungi",""),
                         `% inertia`=c(round((SumOfSqs[1]/sum(SumOfSqs))*100,digits = 3),""),
                         ` `=c("Model","Residuals"))%>%
                  select(c(5,7,3,1,6,4)) %>%
                  rename(Taxa=1,`F stat`=3,Df=4,`P-val`=6))%>%
    flextable::flextable()%>%
    flextable::fontsize(size=8, part = "body")%>%
    flextable::height_all(height = .1 ) %>%
    flextable::align(align = "center", part = "all") %>%
    flextable::bold(i=~`P-val`<0.05,j=6)%>%
    flextable::add_header_row(top = T,values = "ANOVA on dbRDA",colwidths = 6 )%>%
    flextable::fontsize(i=1,part="header",size=12)%>%
    flextable::width(j=1:6,width = c(3,3,3,1,3,3),unit = "cm") %>%
    flextable::add_footer_lines(values = "Global ANOVA summary table for dbRDA (Morisita-Horn distance on Hellinger transformed microbial communities). For each marker the F statistic, the % of total inertia explained and associated P-value (bold when significant, α<0.05).")%>%
    flextable::fontsize(part = "footer", size=6)  %>%
    flextable::align(align = "justify", part = "footer")

flextable::save_as_docx(flex_dbrda_glob,
                        path="results/figures/draft/supp/table/TS7.docx")


## TS8 ====

flex_dbrda_marg <- 
    anova_margin$micro_16s$table$body$dataset %>%
    data.frame() %>%
    mutate(rawnames=rownames(.)) %>%
    left_join(select(trait_df_name,c(rawnames,cleannames))) %>%
    select(6,3,1,2,4) %>%
    mutate(cleannames=ifelse(is.na(cleannames),"Residual",cleannames)) %>% 
    mutate(`% inertia`=round((SumOfSqs/sum(SumOfSqs))*100,digits=3),.before=5) %>% 
    rename(Variable=1,
           `P-val`=6) %>%
    mutate(Variable=forcats::fct_relevel(Variable,c(levels(anova_margin$micro_16s$inertia$data$cleannames),"Residual"))) %>%
    flextable::flextable() %>%
    flextable::bold(i=~`P-val`<0.05,j=6)%>%
    flextable::add_header_row(top = T,values = "Term-wise ANOVA on bacteria dbRDA",colwidths = 6 )%>%
    flextable::fontsize(i=1,part="header",size=12)%>%
    flextable::width(j=1:6,width = c(3,3,1,3,3,3),unit = "cm")%>%
    flextable::fontsize(size=8, part = "body")%>%
    flextable::height_all(height = .05 ) %>%
    flextable::align(align = "center", part = "all") %>%
    flextable::add_footer_lines(values = "Trait-wise (\"margin\") ANOVA summary table for dbRDA (Morisita-Horn distance on Hellinger transformed bacterial communities). For each trait the F statistic, Freedom degrees, the % of total inertia explained and associated P-value (bold when significant, α<0.05).")%>%
    flextable::fontsize(part = "footer", size=6)  %>%
    flextable::align(align = "justify", part = "footer")

    flextable::save_as_docx(flex_dbrda_marg,
                            path="results/figures/draft/supp/table/TS8.docx")
## TS9 ====
binded_gamsig <- data.frame()
for(ttt in names(gam_list$micro_16s)){
    binded_gamsig <- rbind(binded_gamsig,gam_list$micro_16s[[ttt]]$terms_sig)
}

binded_gamsig %>%
    mutate(rawnames=stringr::str_extract(term,"(?<=\\().*(?=\\))"),
           .before=1) %>%
    mutate(Cluster=stringr::str_extract(term,"Bac\\s[0-9]{1}"),
           .before=2) %>%
    filter(rawnames!="C.N_leaf") %>%
    left_join(select(rename(trait_df_name,Variable=cleannames),rawnames,Variable))%>%
    mutate(Variable=ifelse(is.na(Variable),paste0("Traits PC",gsub("Dim\\.","",rawnames)),Variable)) %>%
    select(c(8,2,4:7)) %>%
    rename(`P-value`=p.value)%>%
    group_by(Variable) %>%
    mutate(Variable=ifelse(row_number()==1,Variable,""))%>%
    ungroup() %>%
    mutate(across(.cols = c(3,5,6), .fns = ~round(.x,digits = 3)))%>%
    flextable::flextable()%>%
    flextable::bold(i=~`P-value`<0.05,j=6)%>%
    flextable::width(j=1:6,width = c(2,4,3,2.5,2.5,4),unit = "cm")%>%
    flextable::align(align = "center", part = "all")%>%
    flextable::add_footer_lines(values = "Summary table of the GAM models fitted smoothed terms between bacterial clusters (Bac 1,2,3 and 4, Fig 3) cumulative abundance and plant traits for all 18 traits and the two first dimensions of the trait PCA (Fig. 1c). edf is the Effective Degree of Freedom which represents the complexity of the fits (1 being a straight line, 2 a quadratic curve and higher values corresponding to more wiggly curves). Ref.df and F are statistics used in an ANOVA testing the significance of the smooth. Significant P-values (α<0.05) are displayed in bold but are only approximate and need to be confronted with visual examination of the fits. Traits are: Number of leaves, Plant height (cm),  longest leaf length (leaf length, cm), longest leaf width (leaf width, cm) , leaf Soluble sugars, Carbon, Nitrogen and Phosphorus content (mg mg-1), leaf chlorophyll content (Chlorophyll, mg g-1), maximum quantum yield of photosystem II (Fv/Fm), maximum electron transport rate (ETRmax, µmol photon m-2 s-1), Number of Flowers, leaf mass area (LMA, gm-2),  leaf dry matter content (LDMC, g g-1), Stomatal density (Stomata mm-2), Midday leaf water potential (Water potential, MPa), relative water content (RWC, %), and, PC1 and 2 (Trait PC1 and PC2) of the overall PCA displayed in Fig 1.")%>%
    flextable::fontsize(part = "footer", size=6) %>%
    flextable::align(align = "justify", part = "footer") %>%
    flextable::save_as_docx(path="results/figures/draft/supp/table/TS9.docx")

## TS10 ====

pdbrda_taxa_list$micro_16s$layers[[1]]$data %>%
    select(label,cluster,prevalence,relab) %>%
    mutate(prevalence_pct=(prevalence/nrow(micro_16s$sample_table))*100) %>%
    distinct() %>%
    left_join(mutate(micro_16s$tax_table,
                     label=gsub("M","",rownames(micro_16s$tax_table)))) %>%
    mutate(cluster=forcats::fct_relevel(cluster,c("Bac 1",
                                                  "Bac 4",
                                                  "Bac 2",
                                                  "Bac 3"))) %>%
    arrange(cluster,desc(relab),desc(prevalence)) %>%
    select(-contains(".1")) %>%
    select(-otu_id) %>%
    relocate(sequence,cluster) %>%
    write.csv("results/figures/draft/supp/table/TS10.csv",
              row.names = F)


## TS11 ====

pdbrda_taxa_list$micro_its$layers[[1]]$data %>%
    select(label,cluster,prevalence,relab) %>%
    mutate(prevalence_pct=(prevalence/nrow(micro_its$sample_table))*100) %>%
    distinct() %>%
    left_join(mutate(micro_its$tax_table,
                     label=gsub("M","",rownames(micro_its$tax_table)))) %>%
    mutate(cluster=forcats::fct_relevel(cluster,c("Fun 3",
                                                  "Fun 1",
                                                  "Fun 2"))) %>%
    arrange(cluster,desc(relab),desc(prevalence)) %>%
    select(-contains(".1")) %>%
    select(-otu_id) %>%
    relocate(sequence,cluster) %>%
    write.csv("results/figures/draft/supp/table/TS11.csv",
              row.names = F)
#> Session info ====
save.image("env_post_analysis.RData")
writeLines(capture.output(sessionInfo()), "results/sessionInfo.txt")
