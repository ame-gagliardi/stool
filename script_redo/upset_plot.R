# Common miRNA #

# Data load

load("C:/Users/amedeo/Desktop/R_Projects/stool/results/age/age.rda")
age <- res
rm(res, res_1, res_2, res_3)

load("C:/Users/amedeo/Desktop/R_Projects/stool/results/sex/sex.rda")
sex <- res
rm(res)

load("C:/Users/amedeo/Desktop/R_Projects/stool/results/smoke/smoke.rda")
smoke <- res
rm(res, res_1, res_2)

load("C:/Users/amedeo/Desktop/R_Projects/stool/results/alcohol/alcohol.rda")
alcohol <- res
rm(res, res_1, res_2, res_3)

load("C:/Users/amedeo/Desktop/R_Projects/stool/results/bmi/bmi.rda")
bmi <- res
rm(res)

load("C:/Users/amedeo/Desktop/R_Projects/stool/results/ncigs/ncigs.rda")
cigs <- res
rm(res)

# load("C:/Users/amedeo/Desktop/R_Projects/stool/results/wine.rda")
# wine <- res
# rm(res, res_1, res_2, res_3)

load("C:/Users/amedeo/Desktop/R_Projects/stool/results/pshysical/phys_act.rda")
phys_act <- res

rm(res, res_1, res_2, res_3, coldata)

# VENN

age <- age[which(age$baseMean >= 15 & age$padj <0.05),]
age <- age$miRNA 

sex <- sex[which(sex$baseMean >= 15 & sex$padj <0.05),]
sex <- sex$miRNA 

alc <- alcohol[which(alcohol$baseMean >= 15 & alcohol$padj <0.05),]
alc <- alc$miRNA 

bmi <- bmi[which(bmi$baseMean >= 15 & bmi$padj <0.05),]
bmi <- bmi$miRNA 

cigs <- cigs[which(cigs$baseMean >= 15 & cigs$padj <0.05),]
cigs <- cigs$miRNA

smoke <- smoke[which(smoke$baseMean >= 15 & smoke$padj <0.05),]
smoke <- smoke$miRNA

# wine <- wine[which(wine$baseMean >= 15 & wine$padj <0.05),]
# wine <- wine$miRNA

phys <- phys_act[which(phys_act$baseMean >= 15 & phys_act$padj <0.05),]
phys <- phys$miRNA

## UPSET

lt <- list(set1 = age,
           set2 = sex,
           set3 = smoke,
           set4 = bmi,
           set5 = cigs,
           set6 = alc,
           set7 = phys)

m <- make_comb_mat(lt, mode = "intersect")
m <- m[comb_degree(m) > 1]
ss <- set_size(m)
cs <- comb_size(m)
set_name(m) <- c("Age", "Sex", "Smoke", "BMI", "Ncigs", "Alcohol", "Physical")

ht <- UpSet(m,
            set_order = order(ss),
            comb_order = order(comb_degree(m), -cs),
            top_annotation = HeatmapAnnotation(
              "miRNAs intersections" = anno_barplot(cs,
                      ylim = c(0, max(cs)*1.1),
                      border = FALSE, 
                      gp = gpar(fill = "black"), 
                      height = unit(10, "cm")
              ),
              annotation_name_side = "left", 
              annotation_name_rot = 90),
            left_annotation = rowAnnotation(
              "#miRNAs in each set" = anno_barplot(-ss, 
                      baseline = 0,
                      axis_param = list(
                      at = c(0, -10, -15, -20, -25, -30, -35, -40, -45),
                      labels = c(0, 10, 15, 20, 25, 30, 35, 40, 45),
                      labels_rot = 0),
                      border = FALSE, 
                      gp = gpar(fill = "black"), 
                      width = unit(4, "cm")
              ),
              set_name = anno_text(set_name(m), 
                      location = 0.5, 
                      just = "center",
                      width = max_text_width(set_name(m)) + unit(4, "mm"))
            ), 
            right_annotation = NULL,
            show_row_names = FALSE)

ht = draw(ht)
od = column_order(ht)

ComplexHeatmap::decorate_annotation("miRNAs intersections", {
  grid.text(cs[od], x = seq_along(cs), y = unit(cs[od], "native") + unit(2, "pt"), 
            default.units = "native", just = c("left", "bottom"), 
            gp = gpar(fontsize = 6, col = "#404040"), rot = 45)
})
            




