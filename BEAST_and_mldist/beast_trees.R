library(ggplot2)
library(ggtree)
library(treeio)


beast_tree_G <- read.beast("Gfa_beauti-15Gfa_1Eve.tre")
beast_tree_G@phylo$tip.label

beast_tree_G@data$MRCAtextcolor <- ifelse(beast_tree_G@data$height > 1, T, F)

ggtree(beast_tree_G) + 
  geom_tiplab(linetype='dashed', linesize=.3, fontface="italic") + 
  geom_text(aes(label = round(height, 1), color = MRCAtextcolor), nudge_x = -.2, nudge_y = .15) + 
  geom_range("length_0.95_HPD", color="purple", size=2, alpha=.5) + 
#  scale_x_continuous(labels = abs, breaks = -10:0) + 
  scale_color_manual(values = c("white", "black"), guide = 'none') + 
  #ggtitle("") + 
  xlim(-1.5,10) + 
  theme(plot.title = element_text(hjust = 0.5)) #+ theme_tree2()

## G. fasciatus, 8 

beast_tree_G <- read.beast("Gfa_beauti_8-8Gfa_1Eve.tre")
beast_tree_G@phylo$tip.label

beast_tree_G@data$MRCAtextcolor <- ifelse(beast_tree_G@data$height > 1, T, F)

ggtree(beast_tree_G) + 
  geom_tiplab(linetype='dashed', linesize=.3, fontface="italic") + 
  geom_text(aes(label = round(height, 1), color = MRCAtextcolor), nudge_x = -.2, nudge_y = .15) + 
  geom_range("length_0.95_HPD", color="purple", size=2, alpha=.5) + 
  #  scale_x_continuous(labels = abs, breaks = -10:0) + 
  scale_color_manual(values = c("white", "black"), guide = 'none') + 
  #ggtitle("") + 
  xlim(-1.5,15) + 
  theme(plot.title = element_text(hjust = 0.5)) #+ theme_tree2()



### And now E. verrucosus

beast_tree_E <- read.beast("Eve_beauti-8Eve_1Gfa.tre")

beast_tree_E@data$MRCAtextcolor <- ifelse(beast_tree_E@data$height > 1, T, F)

ggtree(beast_tree_E) + 
  geom_tiplab(linetype='dashed', linesize=.3, fontface="italic") + 
  geom_text(aes(label = round(height, 1), color = MRCAtextcolor), nudge_x = -.3, nudge_y = .15) + 
  geom_range("length_0.95_HPD", color="purple", size=2, alpha=.5) + 
  #scale_x_continuous(labels = abs, breaks = -10:0) + 
  xlim(-1,20) + 
  scale_color_manual(values = c("white", "black"), guide = 'none') +
  theme(plot.title = element_text(hjust = 0.5)) #+ theme_tree2()

