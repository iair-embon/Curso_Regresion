# E = k * h^a => a = 0.8, k = 1051
# linear regression: log(E) = log(k) + a*log(h) => log(E) = 6.957 + 0.8 * log(h)
# [E/1.1, E*1.1] => [log(E)-log(1.1), log(E)+log(1.1)]
# log(1.1) = 1.96*sigma => sigma = 0.0486
# R^2 = 1 - sigma_predicted^2/sigma_pop^2 => R^2 = 1 - 0.0486^2/0.05^2 = 0.0552