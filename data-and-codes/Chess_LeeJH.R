library(tidyverse)
library(ggplot2)
library(reshape2)
library(DirichletReg)
library(ggrepel)

# Preprocessing and Visualization

lichess <- read_csv("/Users/Lee/SportsBigData/200k_blitz_rapid_classical_bullet.csv")

ggplot(lichess, aes(Category))+
  geom_bar()+
  scale_x_discrete(limits=c("Bullet", "Blitz", "Rapid", "Classical"))
  
lichess %>%
  filter(Category == "Classical" | Category == "Rapid") %>%
  filter(BlackElo >= 2000 & WhiteElo >= 2000) %>%
  mutate(EloDiff = WhiteElo - BlackElo) %>%
  select(BlackElo, WhiteElo, EloDiff, ECO, Opening, Result) -> lichess

lichess %>%
  filter(abs(EloDiff) < 200) -> lichess

lichess %>%
  group_by(ECO) %>%
  summarize(N = n()) %>%
  mutate(OverTen = as.numeric(N >= 10)) -> ECO_10

lichess %>%
  inner_join(ECO_10, by = "ECO") -> lichess

lichess %>%
  filter(OverTen == 1) %>%
  select(BlackElo, WhiteElo, EloDiff, ECO, Opening, Result) -> lichess

lichess %>%
  mutate(Win = as.numeric(Result == "1-0"),
         Draw = as.numeric(Result == "1/2-1/2"),
         Loss = as.numeric(Result == "0-1")) -> lichess

ggplot(lichess, aes(ECO))+
  geom_bar()

lichess %>%
  group_by(ECO) %>%
  summarise(Draw.Rate = sum(Draw)/length(Draw)) %>%
  arrange(-Draw.Rate) -> DRAWISH

View(DRAWISH)

lichess %>%
  group_by(ECO) %>%
  summarise(Win.Rate = sum(Win + 0.5*Draw)/length(Draw)) %>%
  arrange(-Win.Rate) -> WIN.RATE

WIN.RATE %>%
  inner_join(ECO_10, by = "ECO") -> WIN.RATE

ggplot(WIN.RATE, aes(x=ECO, y=Win.Rate))+
  geom_bar(stat="identity")+
  scale_x_discrete(limits=WIN.RATE$ECO)

ggplot(WIN.RATE, aes(x=N, y=Win.Rate))+
  geom_point(size=3)

# Prior

stockfish <- read_csv("/Users/Lee/SportsBigData/Stockfish_Simulation.csv")

stockfish %>%
  mutate(Win_MC = as.numeric(G1 == "1-0") + as.numeric(G2 == "1-0") +
           as.numeric(G3 == "1-0") + as.numeric(G4 == "1-0") +
           as.numeric(G5 == "1-0") + as.numeric(G6 == "1-0") +
           as.numeric(G7 == "1-0") + as.numeric(G8 == "1-0") +
           as.numeric(G9 == "1-0") + as.numeric(G10 == "1-0"),
         Draw_MC = as.numeric(G1 == "1/2-1/2") + as.numeric(G2 == "1/2-1/2") +
           as.numeric(G3 == "1/2-1/2") + as.numeric(G4 == "1/2-1/2") +
           as.numeric(G5 == "1/2-1/2") + as.numeric(G6 == "1/2-1/2") +
           as.numeric(G7 == "1/2-1/2") + as.numeric(G8 == "1/2-1/2") +
           as.numeric(G9 == "1/2-1/2") + as.numeric(G10 == "1/2-1/2"),
         Loss_MC = as.numeric(G1 == "0-1") + as.numeric(G2 == "0-1") +
           as.numeric(G3 == "0-1") + as.numeric(G4 == "0-1") +
           as.numeric(G5 == "0-1") + as.numeric(G6 == "0-1") +
           as.numeric(G7 == "0-1") + as.numeric(G8 == "0-1") +
           as.numeric(G9 == "0-1") + as.numeric(G10 == "0-1")) -> stockfish

# Data

lichess %>%
  group_by(ECO) %>%
  summarise(Win_data = sum(Win),
            Draw_data = sum(Draw),
            Loss_data = sum(Loss)) -> data

# Posterior

data %>%
  inner_join(stockfish, by = "ECO") %>%
  group_by(ECO) %>%
  summarise(Win_post = Win_data + Win_MC,
            Draw_post = Draw_data + Draw_MC,
            Loss_post = Loss_data + Loss_MC) -> posterior

posterior_melt <- melt(posterior[,c('ECO','Win_post','Draw_post', 'Loss_post')],id.vars = 1)

ggplot(posterior_melt,aes(x=ECO, y=value, fill=factor(variable,levels=c('Loss_post','Draw_post', 'Win_post'))))+
  geom_bar(position="fill", stat="identity")+
  theme(legend.position="top")+
  theme(legend.title = element_blank())+
  labs(y = "Win / Draw / Loss Rates")

# Drawish Tendency

posterior %>%
  mutate(WL_post = Win_post + Loss_post,
         post_Mean = Draw_post / (Draw_post+WL_post),
         post_lb = qbeta(0.025, Draw_post, WL_post),
         post_ub = qbeta(0.975, Draw_post, WL_post)) %>%
  arrange(Draw_post) -> DRAWISH_post

ggplot(DRAWISH_post, aes(x = ECO, ymin = post_lb, ymax = post_ub)) +
  geom_linerange() + geom_point(aes(y = post_Mean))+
  labs(x = "ECO", y = "95% Posterior Interval of Drawrate")

# Win Rates

posterior$MC_Mean = rep(0, nrow(posterior))
posterior$MC_lb = rep(0, nrow(posterior))
posterior$MC_ub = rep(0, nrow(posterior))

for(i in 1:nrow(posterior)){
  w = posterior$Win_post[i]
  d = posterior$Draw_post[i]
  l = posterior$Loss_post[i]
  MC_Sampling = rdirichlet(50000, c(w,d,l))
  MC_Winrate = as.vector(MC_Sampling %*% c(1, 0.5, 0))
  posterior$MC_lb[i] = quantile(MC_Winrate, 0.025)
  posterior$MC_ub[i] = quantile(MC_Winrate, 1-0.025)
  posterior$MC_Mean[i] = mean(MC_Winrate)
}

ggplot(posterior, aes(x = ECO, ymin = MC_lb, ymax = MC_ub)) +
  geom_linerange() + geom_point(aes(y = MC_Mean))+
  labs(x = "ECO", y = "95% Posterior Interval of Winrate")

WIN.RATE %>%
  inner_join(posterior, by='ECO') %>%
  arrange(Win.Rate)-> WIN.RATE

ggplot(WIN.RATE, aes(x=Win.Rate, ymin=MC_lb, ymax=MC_ub))+
  geom_linerange() + geom_point(aes(y=MC_Mean))+
  geom_abline(slope = 1, intercept = 0)+
  labs(x="observed mean", y="posterior mean")+
  geom_text_repel(aes(Win.Rate, MC_Mean, label = ECO))

# Win Rates with moderator delta

posterior %>%
  select(ECO, Win_post, Draw_post, Loss_post) -> posterior_delta

posterior_delta = posterior_delta[rep(seq_len(nrow(posterior_delta)), 11), ]

posterior_delta$delta = rep(seq(0,1,0.1), each = 38)

for(i in 1:nrow(posterior_delta)){
  w = posterior_delta$Win_post[i]
  d = posterior_delta$Draw_post[i]
  l = posterior_delta$Loss_post[i]
  delta = posterior_delta$delta[i]
  MC_Sampling = rdirichlet(50000, c(w,d,l))
  Winrate_delta = as.vector(MC_Sampling %*% c(1, delta, 0))
  posterior_delta$Mean_delta[i] = mean(Winrate_delta)
}

posterior_delta %>%
  group_by(delta) %>%
  mutate(rank_delta = rank(-Mean_delta)) -> posterior_delta

posterior_delta %>%
  select(ECO, delta, rank_delta) -> RANK_DELTA

RANK_DELTA <- pivot_wider(RANK_DELTA, names_from = delta, values_from = ECO) 

RANK_DELTA %>%
  arrange(rank_delta) -> RANK_DELTA

View(RANK_DELTA)

ggplot(posterior_delta,aes(x=delta, y=-rank_delta, color=factor(ECO))) +
  geom_line(size = 1) + 
  geom_point(aes(y=-rank_delta)) +
  geom_text(data = filter(posterior_delta, delta == 0),
            aes(-0.01, -rank_delta, label = rank_delta), size=2.5, color='black')+
  geom_text(data = filter(posterior_delta, delta == 1),
            aes(1.02, -rank_delta, label = ECO), size=2.5)+
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  ggtitle("Opening Tier in Rankings as delta Changes [White]")


## Repeat for Black

posterior %>%
  select(ECO, Win_post, Draw_post, Loss_post) -> posterior_black

posterior_black = posterior_black[rep(seq_len(nrow(posterior_black)), 11), ]

posterior_black$delta = rep(seq(0,1,0.1), each = 38)

for(i in 1:nrow(posterior_black)){
  w = posterior_black$Loss_post[i]
  d = posterior_black$Draw_post[i]
  l = posterior_black$Win_post[i]
  delta = posterior_black$delta[i]
  MC_Sampling = rdirichlet(50000, c(w,d,l))
  Winrate_delta = as.vector(MC_Sampling %*% c(1, delta, 0))
  posterior_black$Mean_delta[i] = mean(Winrate_delta)
}

posterior_black %>%
  group_by(delta) %>%
  mutate(rank_delta = rank(-Mean_delta)) -> posterior_black

posterior_black %>%
  select(ECO, delta, rank_delta) -> RANK_BLACK

RANK_BLACK <- pivot_wider(RANK_BLACK, names_from = delta, values_from = ECO) 

RANK_BLACK %>%
  arrange(rank_delta) -> RANK_BLACK

View(RANK_BLACK)

ggplot(posterior_black,aes(x=delta, y=-rank_delta, color=factor(ECO))) +
  geom_line(size = 1) + 
  geom_point(aes(y=-rank_delta)) +
  geom_text(data = filter(posterior_black, delta == 0),
            aes(-0.01, -rank_delta, label = rank_delta), size=2.5, color='black')+
  geom_text(data = filter(posterior_black, delta == 1),
            aes(1.02, -rank_delta, label = ECO), size=2.5)+
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  ggtitle("Opening Tier in Rankings as delta Changes [Black]")