library(here)
here::here()


# Open clean tracks data
tracks <- read.csv('data/tidy/GP010016_fast_17.csv', row.names = 1)

library(plyr)
library(tidyr)
library(dplyr)


sizes_means <- tracks %>% group_by(Crab_ID) %>% summarise(mean_size_w = mean(Width, na.rm = TRUE),
                                                     mean_size_h = mean(Height, na.rm = TRUE),
                                                     mean_size_a = mean(Area, na.rm = TRUE))
sizes_means


sizes_medians <- tracks %>% group_by(Crab_ID) %>% summarise(medians_size_w = median(Width, na.rm = TRUE),
                                                     medians_size_h = median(Height, na.rm = TRUE),
                                                     medians_size_a = median(Area, na.rm = TRUE))
sizes_medians


tracks$Crab_ID <- factor(tracks$Crab_ID, c("crab_1", "crab_2","crab_3","crab_4","crab_5",
                                         "crab_6","crab_7","crab_8","crab_9","crab_10",
                                         "crab_11","crab_12","crab_13",
                                         "crab_14","crab_15","crab_16","crab_17"))

library(ggplot2)


plot(data = tracks, tracks$Width ~ tracks$Height)
boxplot(tracks$Height)
boxplot(tracks$Width)


new_data <- tracks[,c(1, 14, 15)]
new_data <- gather(new_data, Type, Value, Width:Height, factor_key = TRUE)


load(file = "data/tidy/GP010016_fast_id_colors_brew_17.Rdata")


violin2 <- ggplot(new_data, aes(x = Crab_ID, y=Value)) +
  geom_violin(trim = FALSE, fill = "gray") +
  facet_wrap(~Crab_ID, scales = "free_x") +
  # scale_color_manual(name = "Crab individuals", values = id_colors_brew) +
  ylab("Size estimate (cm)") +
  stat_summary(fun.y = "mean", geom = "point", size = 1, fill = "blue", aes(shape = "Mean"), show.legend  = TRUE) +
  stat_summary(fun.y = "median", geom = "point", size = 1, fill = "red", aes(shape = "Median"), show.legend = TRUE) +
  # guides() +
  scale_shape_manual("Central tendency", values = c(1, 3)) +
  theme(panel.background = element_blank(),
        legend.key = element_rect(colour = NA, fill = NA),
        legend.position = c(0.7, 0.10),
        legend.direction = "horizontal",
        legend.text = element_text(size = 8),
        legend.title = element_text(size =8),
        # axis.line = element_line(color = "black", size = .5),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.line =  element_line(color = "black", size = .5),
        strip.background = element_blank(),
        # strip.text = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 8),
        axis.text = element_text(size = 8))
violin2 + guides(shape = guide_legend(nrow = 2))
# ggsave('figures/violin_central_tendency_17.png', width = 10, height = 20, units = 'cm', dpi = 300)


sizes_means_ce <- new_data %>% group_by(Crab_ID) %>% summarise(mean_size = mean(Value, na.rm = TRUE))
sizes_means_ce

sizes_medians_ce <- new_data %>% group_by(Crab_ID) %>% summarise(median_size = median(Value, na.rm = TRUE))
sizes_medians_ce


hist_3 <- ggplot(new_data, aes(Value)) +
  geom_histogram(bins = 20, fill = "gray", color = "gray") +
  facet_wrap(~Crab_ID) +
  geom_vline(data = sizes_means_ce, aes(xintercept = mean_size), linetype = "solid", color = "red", size = 0.5) +
  geom_vline(data = sizes_medians_ce, aes(xintercept = median_size), linetype = "solid", color = "black", size = 0.5) +
  scale_x_continuous(expand = c(0,0), breaks = seq(0, 4, 0.5), limits = c(0, 5)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(panel.background = element_blank(),
        legend.position = "none",
        # axis.line = element_line(color = "black", size = .5),
        panel.grid.major = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = .5),
        strip.background = element_blank(),
        #strip.text = element_blank(),
        axis.title.x = element_text(size = 6),
        axis.title.y = element_text(size = 6),
        axis.text = element_text(size = 6))
hist_3
# ggsave('figures/hist_size_both_axes_17.png', width = 15, height = 8, units = 'cm', dpi = 300)


for (i in unique(new_data$Crab_ID)){
  # print(i)
  id <- i
  vect <- subset(new_data, Crab_ID == i)
  test_t <- t.test(vect$Value, conf.level = 0.95)
  norm.test <- shapiro.test(vect$Value)
  confi_low <- test_t$conf.int[1]
  confi_high <- test_t$conf.int[2]
  print(paste0(round(confi_low, digits = 2), "-", round(confi_high, digits = 2), " ", id,
               ", Shapiro test p-value = ", norm.test[2]$p.value))
  gc()
}


for (i in unique(new_data$Crab_ID)){
  # print(i)
  id <- i
  vect <- subset(new_data, Crab_ID == i)
  test_t <- wilcox.test(vect$Value, conf.level = 0.95, conf.int = TRUE)
  confi_low <- test_t$conf.int[1]
  confi_high <- test_t$conf.int[2]
  print(paste0(round(confi_low, digits = 2), "-", round(confi_high, digits = 2), " ", id,
               ", Wilcoxon Signed-Rank test p-value = ", test_t$p.value))
  gc()
}


# Create a datframe with the meta information for each individual
crabs_sizes <- new_data %>% 
  group_by(Crab_ID) %>% 
  dplyr::summarise(mean_size = mean(Value, na.rm = TRUE))

meta_crabs <- tracks %>%
  group_by(Crab_ID) %>%
  dplyr::summarise(Species_co = n(),
            Sex_co = n(),
            Handedness_co = n())

meta_crabs2 <- aggregate(tracks[,c(11:13)], by = list(value = tracks$Crab_ID), unique)

# Individual two was measured on the field: right handed
meta_crabs2$Handedness[meta_crabs2$value == "crab_2"] <- "Right"

# All individual were identified in the field as Uca seismella but these were indeed Tubuca polita
meta_crabs2$Species <- as.character(as.factor(meta_crabs2$Species))
meta_crabs2$Species[meta_crabs2$Species == "Uca seismella"] <- "Tubuca polita"


crabs_info <- merge(crabs_sizes, meta_crabs2, by.x = "Crab_ID", by.y = "value")


crabs_info[] <- lapply(crabs_info, function(x) replace(x, grep("[?]", x), "Unknown"))
str(crabs_info)
crabs_info <- droplevels(crabs_info)


crabs_info$mean_size <- round(as.numeric(crabs_info$mean_size), digits = 2)
write.csv(crabs_info, file = here::here('data/tidy', 'crabs_meta_info.csv'))


measured_sizes <- read.csv(file = here::here('data/raw', 'GP010016_crabs_sizes.csv'), row.names = 1)


cor.test(measured_sizes$propodus, measured_sizes$carapace, alternative = "two.sided", conf.level = 0.95)

crabs_sizes_2 <- crabs_sizes
crabs_sizes_2$propodus <- measured_sizes$propodus
crabs_sizes_2$carapace <- measured_sizes$carapace
crabs_sizes_2$ID <- seq(1, 17, 1)

model1 <- lm(propodus ~ mean_size, data = crabs_sizes_2)
summary(model1)
par(mfrow = c(2,2))
plot(model1)

model2 <- lm(carapace ~ mean_size, data = crabs_sizes_2)
summary(model2)
plot(model2)
confint(model2)

dev.off()
# Model evaluation

which(complete.cases(crabs_sizes_2))
dat.model <- crabs_sizes_2[which(complete.cases(crabs_sizes_2)),]

boxplot(dat.model$carapace)
boxplot(dat.model$mean_size)

qqnorm(dat.model$carapace)
qqline(dat.model$carapace)
qqnorm(dat.model$mean_size)
qqline(dat.model$mean_size)

shapiro.test(dat.model$carapace)
shapiro.test(dat.model$mean_size)

plot(dat.model$carapace ~ dat.model$mean_size, xlab ="Crab size estimate (cm)", ylab = "Carapace width (cm)")
cor(dat.model$carapace, dat.model$mean_size)

my.fittted.val <- predict(model2)
par(pty="s") #makes the scatterplot 's'quare shaped
plot(my.fittted.val, dat.model$carapace, xlab="Fitted values of carapace width", ylab="Carapace width")
abline(0,1, lty=2)

## Residual analysis: normality
zresid <- resid(model2)
qqnorm(zresid)
qqline(zresid)
shapiro.test(zresid)

## Residual analysis: homoscedastic
plot(my.fittted.val, zresid, xlab="Fitted carapace width", ylab="Residual")
abline(h=0, lty=2)

## Residual analysis: independence
### residuals sequence
plot(zresid, type='b', ylab="Residual")
abline(h=0, lty=2, col='grey')
### residuals lag
plot(zresid[1:9],zresid[2:10],xlab="residual",ylab="residual lagged by 1")

## Data and model with best fit and prediction
plot(dat.model$mean_size, dat.model$carapace, xlab ="Crab size estimate (cm)", ylab = "Carapace width (cm)")
abline(model2)
xgrid <- seq(min(dat.model$mean_size), max(dat.model$mean_size), length.out=10)
xgridframe <- data.frame(mean_size=xgrid)
zpi <- predict(model2,newdata=xgridframe, interval="prediction")
zpi # column1 = the fit, column 2 = lower interval, column 3 = upper interval. 
matlines(xgridframe, zpi[,-1], col=2, lty=3)

## Data and model with best fit and confidence
plot(dat.model$mean_size, dat.model$carapace, xlab ="Crab size estimate (cm)", ylab = "Carapace width (cm)")
abline(model2)
xgrid <- seq(min(dat.model$mean_size), max(dat.model$mean_size), length.out=10)
xgridframe <- data.frame(mean_size=xgrid)
zpi <- predict(model2,newdata=xgridframe, interval="confidence")
zpi # column1 = the fit, column 2 = lower interval, column 3 = upper interval. 
matlines(xgridframe, zpi[,-1], col=2, lty=3)


model2$coefficients[[1]]


predicted <- data.frame(predic = predict(model2, crabs_sizes_2), size_estimate = crabs_sizes_2$mean_size)
predicted2 <- data.frame(size_estimate = crabs_sizes_2$mean_size, 
                         predic = predict(model2, crabs_sizes_2, interval = "confidence"))

min(crabs_sizes_2$mean_size)
max(crabs_sizes_2$mean_size)
seq(min(crabs_sizes_2$mean_size), max(crabs_sizes_2$mean_size), 0.05)


text0_plot <- expression(hat(y)[i] == hat(beta)[0] + hat(beta)[1]*x[i] + epsilon[i])
round(model2$coefficients[[1]], 2)
text1_plot <- expression(hat(beta)[0] == 0.73)
round(model2$coefficients[[2]], 2)
text2_plot <- expression(hat(beta)[1] == 0.49)
text3_plot <- paste0("R ^ 2 == ", round(summary(model2)$r.squared , 2))
summary(model2)$fstatistic
text4_plot <- expression("F"[paste("1,8")] == 14.84317)
text5_plot <- paste0("p-value = ", round(pf(summary(model2)$fstatistic[1],
                                            summary(model2)$fstatistic[2],
                                            summary(model2)$fstatistic[3], 
                                            lower.tail = FALSE), 5))


ggplot(subset(crabs_sizes_2, !is.na(propodus))) +
  geom_point(aes(x = mean_size, y = propodus, colour = "si_vs_pr"), size = 3) +
  geom_point(aes(x = mean_size, y = propodus), shape =1, colour = "black", size = 3) +
  geom_point(aes(x = mean_size, y = carapace, colour = "si_vs_ca"), size = 3) +
  geom_point(aes(x = mean_size, y = carapace), shape =1, colour = "black", size = 3) +
  stat_smooth(aes(x = mean_size, y = carapace), method = "lm", col = "orange") +
  geom_point(data = predicted, aes(x = size_estimate, y = predic), color = "black", size = 1) +
  geom_line(data = subset(predicted2, size_estimate >= 1.17), aes(x = size_estimate, y = predic.lwr), # Do not plot confidence outside data
            color = "orange", size = 1, linetype = "dashed") +
  geom_line(data = subset(predicted2, size_estimate >= 1.17), aes(x = size_estimate, y = predic.upr), # Do not plot confidence outside data
            color = "orange", size = 1, linetype = "dashed") +
  coord_equal() +
  scale_x_continuous("Size estimate (cm)", breaks = seq(0, 3, 0.25), limits = c(1,3)) +
  scale_y_continuous("Crab size (cm)", breaks = seq(0, 3, 0.25), limits = c(1,3)) +
  scale_color_manual("Relationship", 
                     values = c(si_vs_pr = "lightblue", si_vs_ca = "orange"),
                     labels = c("Size estimate vs Carapace width",
                                "Size estimate vs Propodus length")) + 
  annotate("text", x= 2.48, y = 2.15, parse = TRUE, label = text0_plot, hjust = 0) +
  annotate("text", x= 2.48, y = 2, parse = TRUE, label = text1_plot, hjust = 0) +
  annotate("text", x= 2.48, y = 1.85, parse = TRUE, label = text2_plot, hjust = 0) +
  annotate("text", x= 2.48, y = 1.70, parse = TRUE, label = text3_plot, hjust = 0) +
  annotate("text", x= 2.48, y = 1.55, parse = TRUE, label = text4_plot, hjust = 0) +
  annotate("text", x= 2.48, y = 1.40, parse = FALSE, label = text5_plot, hjust = 0) +
  theme(panel.background = element_blank(),
        legend.position = c(0.75, 0.1),
        legend.key = element_rect(colour = NA, fill = NA),
        # axis.line = element_line(color = "black", size = .5),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.line =  element_line(color = "black", size = .5),
        strip.background = element_blank(),
        # strip.text = element_blank(),
        axis.title.x = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 8),
        axis.text = element_text(size = 8))
# ggsave('figures/linear_model_17_sizes_model_info.png', width = 15, height = 15, units = 'cm', dpi = 300)


# Compose plot
violin2_c <- ggplot(new_data, aes(x = Crab_ID, y=Value)) +
  geom_violin(trim = FALSE, fill = "gray") +
  facet_wrap(~Crab_ID, scales = "free_x", ncol = 6) +
  # scale_color_manual(name = "Crab individuals", values = id_colors_brew) +
  ylab("Size estimate (cm)") +
  stat_summary(fun.y = "mean", geom = "point", size = 1, fill = "blue", aes(shape = "Mean"), show.legend  = TRUE) +
  stat_summary(fun.y = "median", geom = "point", size = 1, fill = "red", aes(shape = "Median"), show.legend = TRUE) +
  # guides() +
  scale_shape_manual("", values = c(1, 3)) +
  theme(panel.background = element_blank(),
        legend.key = element_rect(colour = NA, fill = NA),
        legend.position = c(0.92, 0.10),
        legend.direction = "horizontal",
        legend.text = element_text(size = 8),
        legend.title = element_text(size =8),
        legend.background = element_rect(colour = NA, fill = NA),
        axis.ticks.x = element_blank(),
        # axis.line = element_line(color = "black", size = .5),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.line =  element_line(color = "black", size = .5),
        strip.background = element_blank(),
        # strip.text = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 8),
        axis.text = element_text(size = 8))
violin2_c <- violin2_c + guides(shape = guide_legend(nrow = 3))


library(ggrepel)
model.plot <- ggplot(subset(crabs_sizes_2, !is.na(propodus))) +
  geom_point(aes(x = mean_size, y = propodus, colour = "si_vs_pr"), size = 3) +
  geom_point(aes(x = mean_size, y = propodus), shape =1, colour = "black", size = 3) +
  geom_point(aes(x = mean_size, y = carapace, colour = "si_vs_ca"), size = 3) +
  geom_point(aes(x = mean_size, y = carapace), shape =1, colour = "black", size = 3) +
  geom_text_repel(data = subset(crabs_sizes_2, !(ID %in% c(14, 5, 1, 4, 15))), 
                  aes(x = mean_size, y = carapace, label = ID), 
                  force = 1,
                  nudge_y = -0.2,
                  direction = "both", 
                  segment.size = 0.2,
                  segment.colour = "gray50") +
  geom_text_repel(data = subset(crabs_sizes_2, ID %in% c(14, 5, 1, 4, 15)), 
                  aes(x = mean_size, y = carapace, label = ID),
                  force = 1,
                  nudge_y = 0.1,
                  direction = "both",
                  segment.size = 0.1,
                  segment.colour = "gray50") +
  geom_text_repel(aes(x = mean_size, y = propodus, label = ID), 
                  force = 1,
                  direction = "both", 
                  segment.size = 0.1,
                  segment.colour = "gray50") +
  stat_smooth(aes(x = mean_size, y = carapace), method = "lm", col = "orange") +
  geom_point(data = predicted, aes(x = size_estimate, y = predic), color = "black", size = 2, alpha = 0.5) +
  geom_line(data = subset(predicted2, size_estimate >= 1.17), aes(x = size_estimate, y = predic.lwr), # Do not plot confidence outside data
            color = "orange", size = 1, linetype = "dashed") +
  geom_line(data = subset(predicted2, size_estimate >= 1.17), aes(x = size_estimate, y = predic.upr), # Do not plot confidence outside data
            color = "orange", size = 1, linetype = "dashed") +
  coord_equal() +
  scale_x_continuous("Size estimate (cm)", breaks = seq(0, 3, 0.25), limits = c(1,3)) +
  scale_y_continuous("Crab size (cm)", breaks = seq(0, 3, 0.25), limits = c(1,3)) +
  scale_color_manual("Relationship", 
                     values = c(si_vs_pr = "lightblue", si_vs_ca = "orange"),
                     labels = c("Size estimate vs Carapace width",
                                "Size estimate vs Propodus length")) + 
  annotate("text", x= 2.48, y = 2.15, parse = TRUE, label = text0_plot, hjust = 0) +
  annotate("text", x= 2.48, y = 2, parse = TRUE, label = text1_plot, hjust = 0) +
  annotate("text", x= 2.48, y = 1.85, parse = TRUE, label = text2_plot, hjust = 0) +
  annotate("text", x= 2.48, y = 1.70, parse = TRUE, label = text3_plot, hjust = 0) +
  annotate("text", x= 2.48, y = 1.55, parse = TRUE, label = text4_plot, hjust = 0) +
  annotate("text", x= 2.48, y = 1.40, parse = FALSE, label = text5_plot, hjust = 0) +
  theme(panel.background = element_blank(),
        legend.position = c(0.75, 0.1),
        legend.key = element_rect(colour = NA, fill = NA),
        # axis.line = element_line(color = "black", size = .5),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.line =  element_line(color = "black", size = .5),
        strip.background = element_blank(),
        # strip.text = element_blank(),
        axis.title.x = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 8),
        axis.text = element_text(size = 8))
model.plot

model.plot.v2 <- ggplot(subset(crabs_sizes_2, !is.na(propodus))) +
  geom_point(aes(x = mean_size, y = propodus, colour = "si_vs_pr"), size = 3) +
  geom_point(aes(x = mean_size, y = propodus), shape =1, colour = "black", size = 3) +
  geom_point(aes(x = mean_size, y = carapace, colour = "si_vs_ca"), size = 3) +
  geom_point(aes(x = mean_size, y = carapace), shape =1, colour = "black", size = 3) +
  # geom_text_repel(data = subset(crabs_sizes_2, !(ID %in% c(14, 5, 1, 4, 15))), 
  #                 aes(x = mean_size, y = carapace, label = ID), 
  #                 force = 1,
  #                 nudge_y = -0.2,
  #                 direction = "both", 
  #                 segment.size = 0.2,
  #                 segment.colour = "gray50") +
  # geom_text_repel(data = subset(crabs_sizes_2, ID %in% c(14, 5, 1, 4, 15)), 
  #                 aes(x = mean_size, y = carapace, label = ID),
  #                 force = 1,
  #                 nudge_y = 0.1,
  #                 direction = "both",
  #                 segment.size = 0.1,
  #                 segment.colour = "gray50") +
  # geom_text_repel(aes(x = mean_size, y = propodus, label = ID), 
  #                 force = 1,
  #                 direction = "both", 
  #                 segment.size = 0.1,
  #                 segment.colour = "gray50") +
  stat_smooth(aes(x = mean_size, y = carapace), method = "lm", col = "orange") +
  # geom_point(data = predicted, aes(x = size_estimate, y = predic), color = "black", size = 2, alpha = 0.5) +
  # geom_line(data = subset(predicted2, size_estimate >= 1.17), aes(x = size_estimate, y = predic.lwr), # Do not plot confidence outside data
  #           color = "orange", size = 1, linetype = "dashed") +
  # geom_line(data = subset(predicted2, size_estimate >= 1.17), aes(x = size_estimate, y = predic.upr), # Do not plot confidence outside data
  #           color = "orange", size = 1, linetype = "dashed") +
  coord_equal() +
  scale_x_continuous("Size estimate (cm)", breaks = seq(0, 3, 0.25), limits = c(1,3)) +
  scale_y_continuous("Measured CW/PL (cm)", breaks = seq(0, 3, 0.25), limits = c(1,3)) +
  scale_color_manual("", 
                     values = c(si_vs_pr = "lightblue", si_vs_ca = "orange"),
                     labels = c("Size estimate vs Carapace width",
                                "Size estimate vs Propodus length")) + 
  # annotate("text", x= 2.48, y = 2.15, parse = TRUE, label = text0_plot, hjust = 0) +
  annotate("text", x= 2.48, y = 2.15, parse = TRUE, label = text1_plot, hjust = 0, size =3) +
  annotate("text", x= 2.48, y = 2.05, parse = TRUE, label = text2_plot, hjust = 0, size =3) +
  annotate("text", x= 2.48, y = 1.95, parse = TRUE, label = text3_plot, hjust = 0, size =3) +
  annotate("text", x= 2.48, y = 1.85, parse = TRUE, label = text4_plot, hjust = 0, size =3) +
  annotate("text", x= 2.48, y = 1.75, parse = FALSE, label = text5_plot, hjust = 0, size =3) +
  theme(panel.background = element_blank(),
        legend.position = c(0.75, 0.1),
        legend.key = element_rect(colour = NA, fill = NA),
        # axis.line = element_line(color = "black", size = .5),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.line =  element_line(color = "black", size = .5),
        strip.background = element_blank(),
        # strip.text = element_blank(),
        axis.title.x = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 8),
        axis.text = element_text(size = 8))
model.plot.v2


library(gridExtra)
g <- grid.arrange(violin2_c, model.plot, nrow = 1, widths = c(1.25, 1.5))
g
# ggsave('figures/compose_figure_sizes.png', g, width = 25, height = 15, units = 'cm', dpi = 300)

g.v2 <- grid.arrange(violin2_c, model.plot.v2, nrow = 1, widths = c(1.25, 1.5))
g.v2
ggsave('figures/Fig_crabs_sizes_v2.png', g.v2, width = 25, height = 15, units = 'cm', dpi = 300)


library(ggpubr)
g1 <- ggarrange(violin2_c, model.plot, ncol = 2, labels = c("(A)", "(B)"), widths = c(1, 1.6))
g1
# ggsave('figures/compose_figure_sizes_ggpubr.png', width = 25, height = 15, units = 'cm', dpi = 300)

g1.v2 <- ggarrange(violin2_c, model.plot.v2, ncol = 2, labels = c("(A)", "(B)"), widths = c(1, 1.6))
g1.v2
ggsave('figures/Fig_crabs_sizes_v2_ggpubr_v2.png', width = 25, height = 15, units = 'cm', dpi = 300)
