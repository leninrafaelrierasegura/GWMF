pp = graph$plot_function(X = cov, vertex_size = 0) + 
  theme_minimal() + 
  theme(text = element_text(family = "Palatino"))
pp
ggsave("~/Desktop/log_sd_y.png", plot = pp, dpi = 300)


pp = graph$plot_function(X = cov_for_mean_to_plot, vertex_size = 0) + 
  theme_minimal() + 
  theme(text = element_text(family = "Palatino"))
pp
ggsave("~/Desktop/mean_y.png", plot = pp, dpi = 300)


ggsave("~/Desktop/new_tadpole_upto1.5.png", plot = p, dpi = 300)
