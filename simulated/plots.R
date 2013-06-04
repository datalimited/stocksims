# plots.R - DESC
# plots.R

# Copyright 2003-2013 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id: $
# Created:
# Modified:


# 
val <- lapply(sims, function(x) cbind(x$val, as.data.frame(x$catch)))
val <- do.call('rbind', val)

ggplot(val, aes(year, data)) + geom_point() + stat_smooth(se=TRUE) + facet_grid(TS+ED+AR~LH+UR+ID)




ggplot(qdf, aes(x = year, y = Q50)) +
    geom_line(size = 2, color = 'navyblue') +
    geom_ribbon(aes(ymin = Q25, ymax = Q75), fill = 'blue', alpha = 0.4) +
    geom_ribbon(aes(ymin = Q5, ymax = Q25), fill = 'blue', alpha = 0.2) +
    geom_ribbon(aes(ymin = Q75, ymax = Q95), fill = 'blue', alpha = 0.2) +
    labs(x = 'Year', y = 'Y')
