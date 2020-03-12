library(tidyverse)
library(reshape2)
library(ggthemes)
library(ggrepel)
library(RColorBrewer)
library(ChannelAttribution)
library(markovchain)
library(visNetwork)
library(expm)
library(stringr)
library(purrrlyr)




set.seed(99669966)
df_raw <- data.frame(customer_id = paste0('id', sample(c(1:20000), replace = TRUE)), date = as.Date(rbeta(80000, 0.7, 10) * 100, origin = "2016-01-01"), channel = paste0('channel_', sample(c(0:7), 80000, replace = TRUE, prob = c(0.2, 0.12, 0.03, 0.07, 0.15, 0.25, 0.1, 0.08))) ) %>%
  group_by(customer_id) %>%
  mutate(conversion = sample(c(0, 1), n(), prob = c(0.975, 0.025), replace = TRUE)) %>%
  ungroup() %>%
  dmap_at(c(1, 3), as.character) %>%
  arrange(customer_id, date)

df_raw


df_raw <- df_raw %>%
  mutate(channel = ifelse(channel == 'channel_2', NA, channel))



df_paths <- df_raw %>%
  group_by(customer_id) %>%
  mutate(path_no = ifelse(is.na(lag(cumsum(conversion))), 0, lag(cumsum(conversion))) + 1) %>%
  ungroup()

df_paths



## For first purchaser only ##

df_paths_1 <- df_paths %>%
  filter(path_no == 1) %>%
  select(-path_no)

df_paths_1




##### replace some channels #####
df_path_1_clean <- df_paths_1 %>%
  # removing NAs
  filter(!is.na(channel)) %>%
  
  # adding order of channels in the path
  group_by(customer_id) %>%
  mutate(ord = c(1:n()),
         is_non_direct = ifelse(channel == 'channel_6', 0, 1),
         is_non_direct_cum = cumsum(is_non_direct)) %>%
  
  # removing Direct (channel_6) when it is the first in the path
  filter(is_non_direct_cum != 0) %>%
  
  # replacing Direct (channel_6) with the previous touch point
  mutate(channel = ifelse(channel == 'channel_6', channel[which(channel != 'channel_6')][is_non_direct_cum], channel)) %>%
  
  ungroup() %>%
  select(-ord, -is_non_direct, -is_non_direct_cum)



df_path_1_clean





df_path_1_clean <- df_path_1_clean %>%
  group_by(customer_id) %>%
  mutate(uniq_channel_tag = ifelse(length(unique(channel)) == 1, TRUE, FALSE)) %>%
  ungroup()








df_path_1_clean_multi <- df_path_1_clean %>%
  filter(uniq_channel_tag == FALSE) %>%
  select(-uniq_channel_tag)

# computing time lapses from the first contact to conversion/last contact
df_multi_paths_tl <- df_path_1_clean_multi %>%
  group_by(customer_id) %>%
  summarise(path = paste(channel, collapse = ' > '),
            first_touch_date = min(date),
            last_touch_date = max(date),
            tot_time_lapse = round(as.numeric(last_touch_date - first_touch_date)),
            conversion = sum(conversion)) %>%
  ungroup()



# distribution plot
ggplot(df_multi_paths_tl %>% filter(conversion == 1), aes(x = tot_time_lapse)) +
  theme_minimal() +
  geom_histogram(fill = '#4e79a7', binwidth = 1)

# cumulative distribution plot
ggplot(df_multi_paths_tl %>% filter(conversion == 1), aes(x = tot_time_lapse)) +
  theme_minimal() +
  stat_ecdf(geom = 'step', color = '#4e79a7', size = 2, alpha = 0.7) +
  geom_hline(yintercept = 0.95, color = '#e15759', size = 1.5) +
  geom_vline(xintercept = 23, color = '#e15759', size = 1.5, linetype = 2)





#reporting date as of January 13, 2016

### for generic probabilistic model ###
df_multi_paths_tl_1 <- melt(df_multi_paths_tl[c(1:50), ] %>% select(customer_id, first_touch_date, last_touch_date, conversion),
                            id.vars = c('customer_id', 'conversion'),
                            value.name = 'touch_date') %>%
  arrange(customer_id)
rep_date <- as.Date('2016-01-13', format = '%Y-%m-%d')

ggplot(df_multi_paths_tl_1, aes(x = as.factor(customer_id), y = touch_date, color = factor(conversion), group = customer_id)) +
  theme_minimal() +
  coord_flip() +
  geom_point(size = 2) +
  geom_line(size = 0.5, color = 'darkgrey') +
  geom_hline(yintercept = as.numeric(rep_date), color = '#e15759', size = 2) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = as.numeric(rep_date), ymax = Inf, alpha = 0.01, color = 'white', fill = 'white') +
  theme(legend.position = 'bottom',
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  guides(colour = guide_legend(override.aes = list(size = 5)))





#1) time lapse from the first contact,

#2) time lapse between the conversion date and a previous contact.


df_multi_paths_tl_2 <- df_path_1_clean_multi %>%
  group_by(customer_id) %>%
  mutate(prev_touch_date = lag(date)) %>%
  ungroup() %>%
  filter(conversion == 1) %>%
  mutate(prev_time_lapse = round(as.numeric(date - prev_touch_date)))

# distribution
ggplot(df_multi_paths_tl_2, aes(x = prev_time_lapse)) +
  theme_minimal() +
  geom_histogram(fill = '#4e79a7', binwidth = 1)

# cumulative distribution
ggplot(df_multi_paths_tl_2, aes(x = prev_time_lapse)) +
  theme_minimal() +
  stat_ecdf(geom = 'step', color = '#4e79a7', size = 2, alpha = 0.7) +
  geom_hline(yintercept = 0.95, color = '#e15759', size = 1.5) +
  geom_vline(xintercept = 12, color = '#e15759', size = 1.5, linetype = 2)



#For tot_time_lapse > 20 & prev_touch > 10


df_multi_paths_tl_3 <- df_path_1_clean_multi %>%
  group_by(customer_id) %>%
  mutate(prev_time_lapse = round(as.numeric(date - lag(date)))) %>%
  summarise(path = paste(channel, collapse = ' > '),
            tot_time_lapse = round(as.numeric(max(date) - min(date))),
            prev_touch_tl = prev_time_lapse[which(max(date) == date)],
            conversion = sum(conversion)) %>%
  ungroup() %>%
  mutate(is_fruitless = ifelse(conversion == 0 & tot_time_lapse > 20 & prev_touch_tl > 10, TRUE, FALSE)) %>%
  filter(conversion == 1 | is_fruitless == TRUE)

df_multi_paths_tl_3


#models for 
#multi-channel paths only for the reporting period (e.g. 90 days) in the example. Therefore, paths include a minimum of 2 touches with 2 dates and last touch date is equal to conversion date.
#1) Criteria #1 - 23 days period between those 2 or more dates (between 1st and last touches) covers 95% of customers with conversions.
#2) Criteria #2 - 12 days period between last 2 touches (between last and previous touches) covers 95% of customers with conversions.
#These rules I used for defining fruitless paths.


##### Generic Probabilistic Model #####
df_all_paths_compl <- df_multi_paths_tl_3 %>%
  
  mutate(null_conversion = ifelse(conversion == 1, 0, 1))





res=choose_order(df_all_paths_compl, var_path="path", var_conv="conversion",
                 var_null="null_conversion")
#plot auc and penalized auc
plot(res$auc$order,res$auc$auc,type="l",xlab="order",ylab="pauc",main="AUC")
lines(res$auc$order,res$auc$pauc,col="red")
legend("right", legend=c("auc","penalized auc"),
       col=c("black","red"),lty=1)
## End(Not run)


mod_attrib_complete <- markov_model(
  df_all_paths_compl,
  var_path = 'path',
  var_conv = 'conversion',
  var_null = 'null_conversion', order = 1,
  out_more = TRUE
)

trans_matrix_prob <- mod_attrib_complete$transition_matrix %>%
  dmap_at(c(1, 2), as.character)

trans_matrix_prob

mod_attrib_complete$removal_effects
mod_attrib_complete$result







#### Calculate ROAS and CPA

calculation = mod_attrib_complete$result

calculation <- data.frame(total_cost = c(15000, 21000, 22000, 10000, 20000, 5000), calculation)

calculation$chanel_weight <- calculation$total_conversions / sum(calculation$total_conversions)

calculation$cost_weight <- calculation$total_cost / sum(calculation$total_cost)

calculation$roas <- calculation$chanel_weight / calculation$cost_weight

calculation$optimal_budget = calculation$total_cost * calculation$roas

calculation$CPA = calculation$total_cost / calculation$total_conversions



calculation$CPA
calculation$optimal_budget
calculation$total_cost






# Create an ordered graph showing conversions attributed to each channel
g_channel_performance <- ggplot(calculation, aes(x = channel_name, y = total_conversions, fill = channel_name)) + 
  geom_bar(stat = "identity", width = 0.6) +
  ylim(0, 350) +
  scale_fill_manual(values = c("#CE2D4F",
                               "#A14DA0",
                               "#9D79BC",
                               "#7F96FF",
                               "#A9CEF4",
                               "#2d35ce")) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 9, angle = 30, hjust = 0.6, face = "bold")) +
  theme(panel.grid.major.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = round(total_conversions, 0)), fontface = "bold", size = 4, vjust = -1) + 
  labs(x = "", y = "Conversions") +
  ggtitle("Channel Performance") +
  guides(fill=FALSE)

g_channel_performance







df_g2 = calculation[, c("channel_name", "total_cost", "optimal_budget")]
df_g2 = melt(df_g2, id = "channel_name")

g_budget_allocation <- ggplot(df_g2, aes(x = channel_name, y = value, fill = variable)) + 
  geom_bar(stat = "identity", width = 0.6, position = position_dodge(width = 0.7)) +
  scale_fill_manual(labels = c("Current Budget", "Optimal Budget"), values = c("#FFD166", "#04A777")) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10, angle = 30, hjust = 0.6, face = "bold")) +
  theme(panel.grid.major.x = element_blank()) +
  geom_text(aes(label = round(value, 0)), 
            fontface = "bold", size = 3.5, 
            vjust = -0.5, position = position_dodge(width = 0.75)) +
  labs(x = "", y = "Budget $") +
  ggtitle("Budget Allocation") +
  theme(plot.title = element_text(hjust = 0.5))

g_budget_allocation




############## visualizations ##############
# transition matrix heatmap for "real" data
df_plot_trans <- mod_attrib_complete$transition_matrix

cols <- c("#e7f0fa", "#c9e2f6", "#95cbee", "#0099dc", "#4ab04a", "#ffd73e", "#eec73a",
          "#e29421", "#e29421", "#f05336", "#ce472e")
t <- max(df_plot_trans$transition_probability)

ggplot(df_plot_trans, aes(y = channel_from, x = channel_to, fill = transition_probability)) +
  theme_minimal() +
  geom_tile(colour = "white", width = .9, height = .9) +
  scale_fill_gradientn(colours = cols, limits = c(0, t),
                       breaks = seq(0, t, by = t/4),
                       labels = c("0", round(t/4*1, 2), round(t/4*2, 2), round(t/4*3, 2), round(t/4*4, 2)),
                       guide = guide_colourbar(ticks = T, nbin = 50, barheight = .5, label = T, barwidth = 10)) +
  geom_text(aes(label = round(transition_probability, 2)), fontface = "bold", size = 4) +
  theme(legend.position = 'bottom',
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 20, face = "bold", vjust = 2, color = 'black', lineheight = 0.8),
        axis.title.x = element_text(size = 24, face = "bold"),
        axis.title.y = element_text(size = 24, face = "bold"),
        axis.text.y = element_text(size = 8, face = "bold", color = 'black'),
        axis.text.x = element_text(size = 8, angle = 90, hjust = 0.5, vjust = 0.5, face = "plain")) +
  ggtitle("Transition matrix heatmap")















##### viz #####
edges <-
  data.frame(
    from = trans_matrix_prob$channel_from,
    to = trans_matrix_prob$channel_to,
    label = round(trans_matrix_prob$transition_probability, 2),
    font.size = trans_matrix_prob$transition_probability * 100,
    width = trans_matrix_prob$transition_probability * 15,
    shadow = TRUE,
    arrows = "to",
    color = list(color = "#95cbee", highlight = "red")
  )

nodes <- data_frame(id = c( c(trans_matrix_prob$channel_from), c(trans_matrix_prob$channel_to) )) %>%
  distinct(id) %>%
  arrange(id) %>%
  mutate(
    label = id,
    color = ifelse(
      label %in% c('(start)', '(conversion)'),
      '#4ab04a',
      ifelse(label == '(null)', '#ce472e', '#ffd73e')
    ),
    shadow = TRUE,
    shape = "box"
  )

visNetwork(nodes,
           edges,
           height = "2000px",
           width = "100%",
           main = "Markov Chain Visualized") %>%
  visIgraphLayout(randomSeed = 666333) %>%
  visNodes(size = 5) %>%
  visOptions(highlightNearest = TRUE)








df_dummy <- data.frame(channel_from = c('(start)', '(conversion)', '(null)'),
                       channel_to = c('(start)', '(conversion)', '(null)'),
                       n = c(0, 0, 0),
                       tot_n = c(0, 0, 0),
                       perc = c(0, 1, 1))




#Let’s assume that we are going to attract 1000 visits from channel_5 and we want to model how many conversions we will obtain or see what channels customers will make contact with in several steps. The script involves manipulating the matrix of transition probabilities associated with the Markov chain.

#Once we have the transition matrix computed, we can project in what state (channel) that customer contact will be, for example, in 5 steps (5th degree) or how many conversion we can obtain (we use 100,000 degrees to makes sure that all customers transited through the transition matrix).



##### modeling states and conversions #####
# transition matrix preprocessing
trans_matrix_complete <- mod_attrib_complete$transition_matrix
trans_matrix_complete <- rbind(trans_matrix_complete, df_dummy %>%
                                 mutate(transition_probability = perc) %>%
                                 select(channel_from, channel_to, transition_probability))
trans_matrix_complete$channel_to <- factor(trans_matrix_complete$channel_to, levels = c(levels(trans_matrix_complete$channel_from)))
trans_matrix_complete <- dcast(trans_matrix_complete, channel_from ~ channel_to, value.var = 'transition_probability')
trans_matrix_complete[is.na(trans_matrix_complete)] <- 0
rownames(trans_matrix_complete) <- trans_matrix_complete$channel_from
trans_matrix_complete <- as.matrix(trans_matrix_complete[, -1])

trans_matrix_complete


# creating empty matrix for modeling
model_mtrx <- matrix(data = 0,
                     nrow = nrow(trans_matrix_complete), ncol = 1,
                     dimnames = list(c(rownames(trans_matrix_complete)), '(start)'))
# adding modeling number of visits
model_mtrx['channel_5', ] <- 1000

c(model_mtrx) %*% (trans_matrix_complete %^% 5) # after 5 steps
c(model_mtrx) %*% (trans_matrix_complete %^% 100000) # after 100000 steps



