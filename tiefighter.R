#Based on yanking out all coeffs with
#model$coefficients %>% data.frame
subz <- readRDS('thedata.rds')

#Flag for p < 0.05
subz$sig <- ifelse(subz$Pr...t.. < 0.05, 1, 0)

#In case all are significant.
#In a bit of a rush... must be better way to fix manual shape to actual value?
if(mean(subz$sig)==1) {
  sigpoints <- c(16,4)} else {
    sigpoints <- c(4,16)}

ggplot(subz, aes(x = zones, y = Estimate, colour = rich, shape = factor(sig))) +
  geom_point(size = 5, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = minn, ymax = maxx),width = 0.5, position = 'dodge') +
  coord_flip() +
  scale_shape_manual(values= sigpoints) +
  facet_wrap(~source, nrow = 1, scales = 'free_x') +
  ggtitle(paste0('Coefficient: ',coefftouse, ' [xij sums to 100 all Scotland]')) +
  theme(plot.title = element_text(face="bold",hjust = 0.5)) +
  guides(shape = guide_legend(title='p < 0.05'))

ggsave('save.png', dpi = 300, height = 6, width = 3 + (numsources * 3))
