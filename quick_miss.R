data <- readRDS('_targets/objects/tax_structure_and_sdg')
vars <- setdiff(names(data), c('country', 'year'))
miss <- numeric(length(vars))
names(miss) <- vars
for(i in seq_along(vars)) miss[i] <- mean(is.na(data[[vars[i]]])) * 100
write.csv(data.frame(var=names(miss), pct=round(miss,1)), 'miss.csv', row.names=F)
cat("Done\n")
