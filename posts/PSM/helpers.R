# helper functions
plot_theme <- function(p,aspect=1) {
    line_size   = 0.5
    text_size   = 5
    tick_length = 2.5
    p + theme(plot.background = element_blank(),
              aspect.ratio    = aspect,
              legend.position = "none",
              panel.border     = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              line = element_line(color="black",linewidth=line_size/2.13,lineend="round"),
              text = element_text(color="black",size=text_size,hjust=0.5,vjust=0.5,angle=0,lineheight=0.9),
              axis.ticks        = element_line(color="black"),
              axis.ticks.length = unit(tick_length,"points"),
              axis.line         = element_line(),
              axis.title        = element_blank(),
              axis.text         = element_text(color="black",size=text_size),
              panel.spacing     = unit(c(0, 0,   0,   0), "points"),
              axis.title.x      = element_blank(),
              axis.title.y      = element_blank(),
              plot.margin       = unit(c(0, 0, 0, 0), "points")
    )
}
printp = function(p) {
    p = p + theme_bw()
    print(plot_theme(p))
}
mround <- function(x,base){ 
    base*round(x/base) 
} 
pth = function(p,style=0) {
    p = p + theme_bw();
    if(style == 1) {
        p = p + theme(axis.title.y=element_blank(),axis.text.y=element_blank()) # ,legend.position="none")
    }
    return(p)
}

label_to_percent = function(str) {
    str = sprintf("%f",str*100)
    parse(text=str)
}
label_to_identity = function(str) {
    return(str)
}
label_to_integer = function(str) {
    str = sprintf("%.0f",str)
    return(str)
}
ceilToFraction = function(num, den = 1) {
    x = den * ceiling( num / den)
    return(x)
}
floorToFraction = function(num, den = 1) {
    x = den * floor(num / den)
    return(x)
}


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
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
        suppressWarnings(print(plots[[1]]))
        
    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            
            suppressWarnings(print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                                             layout.pos.col = matchidx$col)))
        }
    }
}
save <- function(p,file){
    pdf(file, width=3, height=3, useDingbats=FALSE)
    printp(p)
    msg = dev.off()
}
printf <- function(...) invisible(print(sprintf(...)))

sig = function(x,y_min=0,y_max=1,x_mid=0.5,x_scale=1) {
    return(y_min + (y_max-y_min)/(1+exp((x_mid-x)/x_scale)))
}