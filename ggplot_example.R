ggplot(extinction) +
  geom_vline(xintercept = 93.9) +
  geom_rect(data = interval_order, aes(xmin = max_ma, xmax = min_ma, ymin = -1.9, ymax = -1.7),
            color="black", fill = interval_order$color) +
  geom_text(data = interval_order, aes(x = midpoint, y = -1.8, label = strtrim(interval_name, 2))) +
  geom_line(data = extinction, aes(x = time, y = extinction)) +
  geom_point(data = extinction, aes(x = time, y = extinction, fill = AIC), shape=21, size=3) +
  scale_x_reverse() +
  scale_fill_gradient2(low = "#5e3c99", mid = "white", high = "#e66101") +
  xlab("Age (Ma)") + ylab("Extinction rate") +
  theme_classic() +
  theme(axis.text = element_text(size=16),
        axis.title = element_text(size=17))
