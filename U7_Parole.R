library(ggplot2)

parole <- read.csv("C:\\Users\\wborkows\\AppData\\Local\\Temp\\Rtmp67LUo3\\data52e87afa4dac")

table(parole$male, parole$violator)
14 / (64 + 14)

table(parole$state, parole$crime)

ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5, color = 'blue')
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5, color = 'blue') + facet_grid(male~.)
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5, color = 'blue') + facet_grid(.~male)

ggplot(data = parole, aes(x = age, fill = male)) + geom_histogram(binwidth = 5)
colorPalette = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
ggplot(data = parole, aes(x = age, fill = male)) + geom_histogram(binwidth = 5) + scale_fill_manual(values=colorPalette)

ggplot(data = parole, aes(x = age, fill = male)) + geom_histogram(binwidth = 5, position = "identity", alpha =0.5) + scale_fill_manual(values=colorPalette)

ggplot(data = parole, aes(x = time.served)) + geom_histogram(binwidth = 1, color = 'blue')

ggplot(data = parole, aes(x = time.served)) + geom_histogram(binwidth = 1, color = 'blue') + facet_grid(crime~.)

ggplot(data = parole, aes(x = time.served, fill = crime)) + geom_histogram(binwidth = 1, position = "identity", alpha =0.5) + scale_fill_manual(values=colorPalette)
