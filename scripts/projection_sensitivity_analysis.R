#== USER SECTION ============================
source("scripts/user_section.R")

#== INITIALISE ==============================
source("scripts/init.R")
coal_carbonization <- matrix(NA, nrow = length(u_coal_loadFactor), ncol=length(u_ratios_coal_pc_s))
n_cases            <- length(u_ratios_coal_pc_s) * length(u_coal_loadFactor)
iter               <- 1

#== GET DATA ============================
source("scripts/get_data.R")

#== PROCESS DATA ============================
for (k_coalRatios in u_ratios_coal_pc_s) {
  for (k_coallf  in u_coal_loadFactor) {
    
    if (k_coalRatios <= 1.0) {
      k_coalpcr <- ifelse(k_coalRatios == 0.0, 0.00001, k_coalRatios)
      k_coalsr  <- 0.00001
    } else {
      k_coalpcr <- 1.0
      k_coalsr  <- k_coalRatios - 1.0
    }
    
    cat(paste0(iter, "/", n_cases, " (LF:", k_coallf, ", PCr:", k_coalpcr, ", Sr:", k_coalsr, ")\n"))
    
    p_data <- process_data(data_pp$coal, data_pp, tmp_ssa, data_lf, data_ef, data_emi, k_coallf, k_coalpcr, k_coalsr)
    
    #-- Compute Kaya factors for carbonization --------
    #kaya_factors <- plot_kayaDecompositionFFext2_10year(p_data, c(-50, 200))
    kaya_factors <- get_kayaDecompositionFFext2_10year(p_data, MODE="share")#, RECOMPUTE_TOTALS=TRUE
    if (is.na(kaya_factors$print$Ck[4])) stop("Ck is NA")
    
    id_row <- which(abs(k_coallf - u_coal_loadFactor) == min(abs(k_coallf - u_coal_loadFactor)))
    id_col <- which(abs(k_coalRatios - u_ratios_coal_pc_s) == min(abs(k_coalRatios - u_ratios_coal_pc_s)))
    coal_carbonization[id_row, id_col] <- kaya_factors$print$Ck[4]
    
    
    iter <- iter + 1
    
  }
}


#== PLOT DATA ====================================
par(las=1)
contour(x = u_coal_loadFactor, y = u_ratios_coal_pc_s, z = coal_carbonization, 
        xaxs="i", yaxs="i",
        xlab="Load factor", ylab="Success rate of pre-construction and shelved coal projects")
points(0.6, 2, col="red", bg="red", pch=21)
points(0.3, 0.3, col="blue", bg="blue", pch=21)

# Transform data to long form
mtrx.melt <- reshape2::melt(coal_carbonization, id.vars = c("lf", "pcr"), measure.vars = "ck")
names(mtrx.melt) <- c("lf", "pcr", "ck")
# Return data to numeric form
mtrx.melt$lf  <- rep(u_coal_loadFactor, length(u_ratios_coal_pc_s))  #as.numeric(stringr::str_sub(mtrx.melt$lf, stringr::str_locate(mtrx.melt$lf, "=")[1,1] + 1))
mtrx.melt$pcr <- rep(u_ratios_coal_pc_s, each=length(u_coal_loadFactor))  #as.numeric(stringr::str_sub(mtrx.melt$pcr, stringr::str_locate(mtrx.melt$pcr, "=")[1,1] + 1))

svglite::svglite(file="../plots/FigureS3.svg")
plot <- ggplot()  +
  theme_bw() +
  xlab("Load factor") +
  ylab("Ratio PC") +
  stat_contour(data = mtrx.melt, aes(x = lf, y = pcr, z = ck, colour = ..level..), 
               #breaks = round(quantile(mtrx.melt$ck, seq(0, 1, 0.1)), 0), 
               breaks = seq(0,140,10),
               size = 1) +
  scale_color_gradient2(name = "Carbonization level [MtCO2/yr]", low = "green", mid = "black",
                        high = "red", midpoint = 0, space = "Lab",
                        na.value = "grey50", guide = "colourbar") +
  theme(legend.justification=c(1, 0), legend.position=c(1, 0)) +  
  geom_point(aes(x = 0.30, y = 0.30), shape = 1, size = 2.5, color = "green", bg="green") +  
  geom_point(aes(x = 0.6, y = 2.0),  shape = 1, size = 2.5, color = "red", bg="red") 

direct.label(plot, "bottom.pieces")
dev.off()

p_data <- process_data(data_pp$coal, data_pp, tmp_ssa, data_lf, data_ef, data_emi, 0.48, 1, 1)
plot_kayaDecompositionFFext2_10year(p_data, MODE="share", c(-90, 180), iName="all")
kd_All <- plot_kayaDecompositionFFext2_5year(p_data, MODE="share", c(-90, 180), iName="all")
p <- kd_All$plot + theme(text=element_text(size=18))
ggsave(p, filename = "Figure3_All.svg", width=16, height=10)

p_data <- process_data(data_pp$coal, data_pp, tmp_ssa, data_lf, data_ef, data_emi, 0.6, 1, 0.00001)
plot_kayaDecompositionFFext2_10year(p_data, MODE="share", c(-90, 180))
p_data <- process_data(data_pp$coal, data_pp, tmp_ssa, data_lf, data_ef, data_emi, 0.6, 2/3, 0.00001)
plot_kayaDecompositionFFext2_10year(p_data, MODE="share", c(-90, 180))
p_data <- process_data(data_pp$coal, data_pp, tmp_ssa, data_lf, data_ef, data_emi, 0.6, 1/2, 0.00001)
plot_kayaDecompositionFFext2_10year(p_data, MODE="share", c(-90, 180))
p_data <- process_data(data_pp$coal, data_pp, tmp_ssa, data_lf, data_ef, data_emi, 0.6, 1/3, 0.00001)
plot_kayaDecompositionFFext2_10year(p_data, MODE="share", c(-90, 200), iName="33pct")
p_data <- process_data(data_pp$coal, data_pp, tmp_ssa, data_lf, data_ef, data_emi, 0.6, 0.00001, 0.00001)
plot_kayaDecompositionFFext2_10year(p_data, MODE="share", c(-90, 180))

p_data <- process_data(data_pp$coal, data_pp, tmp_ssa, data_lf, data_ef, data_emi, 0.4, 1, 1)
plot_kayaDecompositionFFext2_10year(p_data, MODE="share", c(-90, 180))
p_data <- process_data(data_pp$coal, data_pp, tmp_ssa, data_lf, data_ef, data_emi, 0.4, 2/3, 0.00001)
plot_kayaDecompositionFFext2_10year(p_data, MODE="share", c(-90, 180))
p_data <- process_data(data_pp$coal, data_pp, tmp_ssa, data_lf, data_ef, data_emi, 0.4, 1/2, 0.00001)
plot_kayaDecompositionFFext2_10year(p_data, MODE="share", c(-90, 180))
p_data <- process_data(data_pp$coal, data_pp, tmp_ssa, data_lf, data_ef, data_emi, 0.4, 1/3, 0.00001)
plot_kayaDecompositionFFext2_10year(p_data, MODE="share", c(-90, 180))
p_data <- process_data(data_pp$coal, data_pp, tmp_ssa, data_lf, data_ef, data_emi, 0.4, 0.00001, 0.00001)
plot_kayaDecompositionFFext2_10year(p_data, MODE="share", c(-90, 180))

p_data <- process_data(data_pp$coal, data_pp, tmp_ssa, data_lf, data_ef, data_emi, 0.3, 0.3, 0.00001)
plot_kayaDecompositionFFext2_10year(p_data, MODE="share", c(-90, 180))
kd_30pct <- plot_kayaDecompositionFFext2_5year(p_data, MODE="share", c(-90, 180), iName="30pct")
p <- kd_30pct$plot + theme(text=element_text(size=18))
ggsave(p, filename = "Figure3_30pct.svg", width=16, height=10)

# Transform data to long form
mtrx.melt <- reshape2::melt(coal_carbonization, id.vars = c("lf", "pcr"), measure.vars = "ckc")
names(mtrx.melt) <- c("lf", "pcr", "ckc")
# Return data to numeric form
#mtrx.melt$lf  <- as.numeric(stringr::str_sub(mtrx.melt$lf, stringr::str_locate(mtrx.melt$lf, "=")[1,1] + 1))
#mtrx.melt$pcr <- as.numeric(stringr::str_sub(mtrx.melt$pcr, stringr::str_locate(mtrx.melt$pcr, "=")[1,1] + 1))

ggplot(mtrx.melt, aes(x = lf, y = pcr, z = ckc)) + stat_contour()


# Create ten segments to be colored in
mtrx.melt$equalSpace <- cut(mtrx.melt$ckc, 10)
# Sort the segments in ascending order
breaks <- levels(unique(mtrx.melt$equalSpace))
# Plot

ggplot(mtrx.melt, aes(x = lf, y = pcr, z = ckc)) +
  stat_contour(geom = "polygon", aes(fill = ..level..)) +
  geom_tile(aes(fill = ckc)) +
  stat_contour(bins = 15) +
  xlab("Load factor") +
  ylab("Ratio PC") +
  guides(fill = guide_colorbar(title = "CkC"))

plot3 <- ggplot() +
  geom_tile(data = mtrx.melt, aes(lf, pcr, ckc, fill = equalSpace)) +
  geom_contour(color = "white", alpha = 0.5) +
  theme_bw() +
  xlab("Load factor") +
  ylab("Ratio PC") +
  scale_fill_manual(values = c("#35978f", "#80cdc1", "#c7eae5", "#f5f5f5", 
                               "#f6e8c3", "#dfc27d", "#bf812d", "#8c510a",
                               "#543005", "#330000"),
                    name = "¼ Mi. Time (s)", breaks = breaks, labels = breaks)


plot4 <- ggplot()  +
  theme_bw() +
  xlab("Load factor") +
  ylab("Ratio PC") +
  stat_contour(data = mtrx.melt, aes(x = lf, y = pcr, z = ckc, colour = ..level..), 
               breaks = round(quantile(mtrx.melt$ckc, seq(0, 1, 0.1)), 0), size = 1) +
  scale_color_continuous(name = "Levels") +
  theme(legend.justification=c(1, 0), legend.position=c(1, 0))

plot5 <- direct.label(plot4, "bottom.pieces")
plot5



## Warning in max(vapply(evaled, length, integer(1))): no non-missing
## arguments to max; returning -Inf


#save(out_data_mc, file="out_data_mc_PLATTSBB2018.RData")


