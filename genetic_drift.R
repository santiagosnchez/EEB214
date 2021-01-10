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
        # make a data frame
        df = rbind(df, make_df_gen(N, x, i, freq_1))
        # plot population
        #plot_population(df, circle)
        # increase to next generation
        i = i+1
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
    df = data.frame(x=rnorm(N), y=rnorm(N), allele=c("A","a")[ factor(x) ], generation=i, frequency=freq)
    return(df)
}