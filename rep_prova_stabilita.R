## Dataset ripetuti rimaneggiato

df <- readRDS("C:/Users/UappaSguappa/Desktop/R_projects/stool/results/repeated_samples/repeated_df.rds")
df <- df[1:6,]
cts <- readRDS("C:/Users/UappaSguappa/Desktop/R_projects/stool/results/repeated_samples/repeated_cts_norm.rds")

mirna <- read.delim("C:/Users/UappaSguappa/Desktop/R_projects/stool/data/downstream/filter_mirna.txt") %>% 
  dplyr::distinct(miRNA)


#Rinomina ripetuti

colnames(cts)[c(1,7)] <- c("sub.1.1", "sub.1.2")
colnames(cts)[c(2,8)] <- c("sub.2.1", "sub.2.2")
colnames(cts)[c(3,9)] <- c("sub.3.1", "sub.3.2")
colnames(cts)[c(4,10)] <- c("sub.4.1", "sub.4.2")
colnames(cts)[c(5,11)] <- c("sub.5.1", "sub.5.2")
colnames(cts)[c(6,12)] <- c("sub.6.1", "sub.6.2")

df[,"vov"][1:6] <- c("sub.1.1", "sub.2.1", "sub.3.1", "sub.4.1", "sub.5.1", "sub.6.1")
df[,"cel"][1:6] <- c("sub.1.2", "sub.2.2", "sub.3.2", "sub.4.2", "sub.5.2", "sub.6.2")

rownames(df) <- df$vov

#Wilcoxon paired

cts <- cts[rownames(cts) %in% mirna$miRNA,]

before <- cts$sub.6.1
after <- cts$sub.6.2

my_data <- data.frame( 
  group = rep(c("before", "after"), each = 152),
  weight = c(before,  after)
)

res.6.1 <- wilcox.test(before, after, paired = TRUE)



# Plot paired data
pd <- paired(before, after)
plot(pd, type = "profile") + theme_bw()

