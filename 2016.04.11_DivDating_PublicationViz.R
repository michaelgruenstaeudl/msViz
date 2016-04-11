#!/usr/bin/R
#author = "Michael Gruenstaeudl, PhD"
#copyright = "Copyright (C) 2016 Michael Gruenstaeudl"
#contributors = c("Michael Gruenstaeudl")
#email = "m.gruenstaeudl@fu-berlin.de"
#version = "2016.04.11.1300"

library(strap)
library(phyloch)

# Read tree
tree = read.beast("~/Desktop/Pterocarya_2016.04.08_NormCalPnt_MCC.tre")

# Set the age of the root
#tree$root.time = max(dist.nodes(tree))
tree$root.time = 20.74 # Is mean root age of MCC tree

# Start to save graphic
svg("~/Desktop/test.svg", width=7, height=12)

    # Plot the tree with a geological time scale
    geoscalePhylo(tree=ladderize(tree, right=FALSE),
                  label.offset=0.5,
                  cex.age=0.9,
                  cex.tip=0.9,
                  cex.ts=0.9,
                  width=2,
                  x.lim=c(-20, 35),
                  #x.lim=c(-12.33662, 48.07662), # Phyloch message: "HPD bar(s) for nodes 26 exceed(s) plot region. Try setting "x.lim" to c(-12.33662, 48.07662)"
                  ts.col=FALSE,
                  units=c("Period","Epoch"))

    # Plot the 95% confidence intervals
    HPDbars(tree,
            label="height_95%_HPD",
            broken=FALSE,
            col=gray(0.01,0.2)) # gray(0.01,0.15) generates transparency

    # IMPORTANT: Adding the node dots must occur after HPD bars have been added
    # Add black outer dots around every node
    node.support(tree$height_median,
                mode="dots",
                cex=1.0,
                col="black")

    # Add black outer dots around every node
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

    # Plot the 95% confidence intervals - Via ape-internal function
    info_string = paste(round(tree$height_median, digits=2),
                        " [",
                        round(tree$`height_95%_HPD_MAX`, digits=2),
                        "-",
                        round(tree$`height_95%_HPD_MIN`, digits=2),
                        "]",
                        sep="")
    nodelabels(info_string, frame="none", cex=0.7)

# End to save graphic
dev.off()
