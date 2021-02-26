library(ggplot2)
library(cowplot)

circleFun <- function(center, diameter, npoints){
    r = diameter / 2
    tt = seq(0,2*pi,length.out = npoints)
    xx = center[1] + r * cos(tt)
    yy = center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
}

genetic_drift <- function(N, initial.freq=0.5){
    # generate vector with starting frequencies
    x = c(rep(1, round(N*initial.freq, 0)), rep(0, round(N*(1-initial.freq), 0)))
    # sample from a binomial distribution
    # x = rbinom(N, 1, initial.freq)
    # i is generation zero at this point
    i = 1
    # get the frequency
    freq_1 = sum(x == 1)/N
    # generate a circle for plot
    #circle = circleFun(center=c(0,0), diameter=10, npoints=100)
    # empty data frame
    df = make_df_gen(N, x, i, freq_1)
    # stop until allele goes extinct (freq_1 == 0)
    while (freq_1 != 0){
        # sample randomely from the population with replacement
        x = sample(x, replace=T)
        # calculate frequency
        freq_1 = sum(x == 1)/N
        # increase to next generation
        i = i+1
        # make a data frame
        df = rbind(df, make_df_gen(N, x, i, freq_1))
        # plot population
        #plot_population(df, circle)
        # stop if allele goes to fixation
        if (freq_1 == 1){
            break
        }
    }
    return(df)
}

# function to generate a data frame with
# random xy coordinates drawn from a normal
# distribution
make_df_gen <- function(N, x, i, freq){
    df = data.frame(x=rnorm(N), y=rnorm(N), allele=c("A","a")[ factor(x, levels=c(0,1)) ], generation=i, frequency=freq)
    return(df)
}

# function to plot a population on a petri dish
plot_population <- function(df, gen, N){
    circle = circleFun(center=c(0,0), diameter=10, npoints=100)
    df = subset(df, generation == gen)
    df$allele = factor(df$allele, c("a","A"))
    p = ggplot(df, aes(x, y)) + 
        geom_point(aes(color=allele), size=7, alpha=0.7) + 
        geom_path(data=circle, color="black", size=3) + 
        scale_color_brewer(type="qual", palette=1, name="allele", drop=FALSE) + 
        theme_minimal() +
      #  scale_x_continuous( expand=c(0,.1)) +
        theme(axis.title=element_blank(), axis.text=element_blank()) +
        theme(title=element_text(size=16), legend.title=element_text(size=16), legend.text=element_text(size=14)) +
        labs(title=paste("Population size:",N,", Generation:",gen))
    return(p)
}

# function to plot the frequency of an allele
plot_frequency <- function(df, N){
    df = unique(df[,3:5])
    last_generation = df[nrow(df),"generation"]
    if (df[nrow(df),"frequency"] == 0){
        col = "red"
    } else {
        col = "green"
    } 
    p = ggplot(data=df, aes(x=generation, frequency)) +
        geom_point(color=col, size=5) +
        geom_line(color=col, size=1) +
        geom_hline(yintercept=c(0,0.5,1), linetype=2, size=0.5) +
        geom_vline(xintercept=last_generation, size=1.5, color=col, linetype=2) +
        annotate(geom="label", label=last_generation, x=last_generation, y=0.5, size=7, hjust=1) +
        ylim(0,1) +
        labs(title=paste("Population size:",N)) +
        theme(text=element_text(size=20))
    return(p)
}
