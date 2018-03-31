stim.dat = read.csv("combined stimuli.csv")

apply(stim.dat[ , -c(1:6, 14, 30)], 2, mean, na.rm = TRUE)
apply(stim.dat[ , -c(1:6, 14, 30)], 2, sd, na.rm = TRUE)
apply(stim.dat[ , -c(1:6, 14, 30)], 2, min, na.rm = TRUE)
apply(stim.dat[ , -c(1:6, 14, 30)], 2, max, na.rm = TRUE)
