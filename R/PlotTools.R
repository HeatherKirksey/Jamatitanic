# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    suppressMessages(suppressWarnings(library(grid)))
    library(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
        print(plots[[1]])
        
    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
    }
}

## Sizes a series of ggplot graphics.  For details see:
## https://stackoverflow.com/questions/30571198/
## how-achieve-identical-facet-sizes-and-scales-in-several-multi-facet-ggplot2-grap/30571289#30571289
set_panel_size <- function(p=NULL, g=ggplotGrob(p), file=NULL, 
                           margin = unit(1,"mm"),
                           width=unit(4, "cm"), 
                           height=unit(4, "cm")){

  panels <- grep("panel", g$layout$name)
  panel_index_w<- unique(g$layout$l[panels])
  panel_index_h<- unique(g$layout$t[panels])
  nw <- length(panel_index_w)
  nh <- length(panel_index_h)

if(getRversion() < "3.3.0"){

   # the following conversion is necessary
   # because there is no `[<-`.unit method
   # so promoting to unit.list allows standard list indexing
   g$widths <- grid:::unit.list(g$widths)
   g$heights <- grid:::unit.list(g$heights)

   g$widths[panel_index_w] <-  rep(list(width),  nw)
   g$heights[panel_index_h] <- rep(list(height), nh)

} else {

   g$widths[panel_index_w] <-  rep(width,  nw)
   g$heights[panel_index_h] <- rep(height, nh)

}

  if(!is.null(file))
    ggsave(file, g, 
           width = convertWidth(sum(g$widths) + margin, 
                                unitTo = "in", valueOnly = TRUE),
           height = convertHeight(sum(g$heights) + margin,  
                                  unitTo = "in", valueOnly = TRUE))

  invisible(g)
}

print.fixed <- function(x) grid.draw(x)