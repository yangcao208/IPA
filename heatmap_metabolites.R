
install.packages("heatmap.plus")
install.packages("RColorBrewer")

library(heatmap.plus)
library(RColorBrewer)

#open excel file, selcet and copy the data (ctrl+c), then type R code to import data from clipboard into R
metabolites <- read.table(file = "clipboard", sep = "\t", header = TRUE, stringsAsFactors = F)
class(metabolites)
a <- data.matrix(metabolites)

my_palette <- colorRampPalette(c("blue", "white", "red"))
#main = "Metabolites in HFpEF vs Chow", cexCol = 0.8, cexRow = 0.4,
#cellwidth = 50, cellheight = 30, fontsize = 12, display_numbers = TRUE
a_scaled <- t(scale(t(a)))
rownames(a_scaled) = metabolites[,1]
heatmap.plus(a_scaled[,2:17],
             labels_row = "class",
             col = my_palette(n = 20),
             Colv = NA, 
             margins = c(15,15))
