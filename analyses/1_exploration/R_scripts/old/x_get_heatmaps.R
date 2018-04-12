

#############Save US vs.non-US heat map###############
english_countries = c("Australia", "Canada", "United Kingdom", "United States")

counts = d %>%
  #data.table() %>%
  mutate(lang.status = ifelse(country %in% english_countries, "english", "non_english")) %>%
  count(x, y, lang.status) %>%
  mutate(n = log(as.numeric(n)))  # take log

counts.trimmed = counts %>%
  filter(x > 10 & y > 10 & x < 245 & y < 245) %>% #remove edges
  group_by(lang.status) %>%
  summarize(med = median(n),
            sd = sd(n)) %>%
  left_join(counts) %>%
  group_by(lang.status) %>%
  filter(n<(med + 1.5*sd) & n>(med-1.5*sd)) # remove outliers

counts.scaled = counts.trimmed %>%
  group_by(lang.status) %>%
  summarize(min = min(n),
            max = max(n)) %>%
  left_join(counts.trimmed) %>%
  group_by(lang.status) %>%
  mutate(n.scaled =(n-min)/(max-min)) %>%
  data.table()

ns_lang = by.key.id %>%
  ungroup() %>%
  mutate(lang.status = ifelse(country %in% english_countries, "english", "non_english")) %>%
  count(lang.status)

heat.maps = ggplot(counts.scaled, aes(x,y)) + 
  facet_wrap(~lang.status) +
  geom_raster(aes(fill = n.scaled), interpolate = TRUE) + 
  scale_fill_gradient2(midpoint = .5, low = "green", mid = "white", high = "red") +
  scale_y_continuous(limits = c(10,245)) + 
  scale_x_continuous(limits = c(10,245))+
  ggtitle(WORD) +
  geom_text(data=data.frame(x=36, y=240, label=c(ns_lang[ns_lang$lang.status == "english", "n"][[1]],
                                                 ns_lang[ns_lang$lang.status == "non_english", "n"][[1]]), 
                            lang.status = c("english", "non_english")), 
            aes(x,y,label=label), inherit.aes = FALSE) +
  theme_blank() +
  theme(legend.position = "none",
        strip.text= element_text(size = 10))

pdf(paste0("heatmaps/", WORD, "_lang_heatmap.pdf"), width = 8, height = 4.5)
print(heat.maps)
dev.off()
