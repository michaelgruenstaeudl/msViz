#!/usr/bin/R
#author = "Michael Gruenstaeudl, PhD"
#copyright = "Copyright (C) 2016 Michael Gruenstaeudl"
#contributors = c("Michael Gruenstaeudl")
#email = "m.gruenstaeudl@fu-berlin.de"
#version = "2016.04.20.1600"

library(strap)
library(phyloch)
library(tools)

# INPUT
# Pterocarya:
inf_tree = "/home/michael_science/git/michaelgruenstaeudl_msViz/example/Pterocarya_DivDating_2016.04.20_SpeciationYule_combined_postBurnin.MCC.tre"
#inf_char=NA
inf_char = "/home/michael_science/git/michaelgruenstaeudl_msViz/example/Pterocarya_occurrence.csv"
# Pyrus:
#inf_tree = "/home/michael_science/git/michaelgruenstaeudl_msViz/example/Pyrus_DivDating_2016.04.20_combined_postBurnin_MCC.tre"
#inf_char=NA
#inf_char = "/home/michael_science/git/michaelgruenstaeudl_msViz/example/Pyrus_occurrence.csv"

# GREYSHADE MAPPING
# Pterocarya:
char_states = c(0,1,2,3) # Adjust according to inf_char
# Pyrus:
#char_states = c(0,1,2,3,4) # Adjust according to inf_char
my_colors = gray.colors(length(char_states))
mapping = data.frame(states=char_states, colors=my_colors)

# OUTPUT
outf = paste(file_path_sans_ext(inf_tree), ".svg", sep='')

# READ INPUT
tree = read.beast(inf_tree)
if (! is.na(inf_char)) {
    m = read.csv(inf_char, header=FALSE)
    char_dict = m[,2]
    names(char_dict) = m[,1]
}

# SET THE AGE OF THE ROOT
#tree$root.time = max(dist.nodes(tree))
# Pterocarya:
tree$root.time = 19.12 # Is mean root age of MCC tree
# Pyrus:
#tree$root.time = 82.67 # Is mean root age of MCC tree

# START TO SAVE GRAPHIC
svg(outf, width=7, height=12)

    # PLOT THE TREE WITH A GEOLOGICAL TIME SCALE
    geoscalePhylo(tree=ladderize(tree, right=FALSE),
                  label.offset=3.0,
                  cex.age=0.9,
                  cex.tip=0.9,
                  cex.ts=0.9,
                  width=2,
                  # Pterocarya:
                  x.lim=c(-20, 30),
                  # Pyrus:
                  #x.lim=c(-20, 100),
                  #x.lim=c(-12.33662, 48.07662), # Phyloch message: "HPD bar(s) for nodes 26 exceed(s) plot region. Try setting "x.lim" to c(-12.33662, 48.07662)"
                  ts.col=FALSE,
                  units=c("Period","Epoch"))

    # PLOT THE 95% CONFIDENCE INTERVALS
    HPDbars(tree,
            label="height_95%_HPD",
            col=gray(0.02,0.2), # gray(0.01,0.15) generates transparency
            broken=FALSE)

    # IMPORTANT: Adding the node dots must occur after HPD bars have been added
    # Add black outer dots around every node
    node.support(tree$height_median,
                mode="dots",
                cex=1.0,
                col="black")

    # ADD BLACK OUTER DOTS AROUND EVERY NODE
    node.support(tree$height_median,
                mode="dots",
                cex=0.5,
                col="white")

    # Plot the 95% confidence intervals - Via Phyloch-internal function
    #node.support(tree$height_median,
    #             pos = "above",
    #             col = "black",
    #             digits=2,
    #             cex=0.7)

    # PLOT TIP CHARACTER STATES
    if (exists('char_dict')) {
        tip_states = tree$tip.label
        used_states = char_dict[tip_states] # Extract those tips that are also in tree.
        colored_used_states = factor(used_states,
                                     levels = mapping$states,
                                     labels = mapping$colors)
        colored_used_states = as.vector(colored_used_states) # Option 'bg' of command 'tiplabels' needs a vector.
        tiplabels(frame='rect',
                  pch=22,
                  col=NA,
                  bg=colored_used_states,
                  cex=1.5,
                  adj=2.0)
    }

    # PLOT THE 95% CONFIDENCE INTERVALS - VIA APE-INTERNAL FUNCTION
    info_string = paste(round(tree$height_median, digits=2),
                        " [",
                        round(tree$`height_95%_HPD_MAX`, digits=2),
                        "-",
                        round(tree$`height_95%_HPD_MIN`, digits=2),
                        "]",
                        sep="")
    nodelabels(info_string, frame="none", cex=0.7)

# END TO SAVE GRAPHIC
dev.off()

