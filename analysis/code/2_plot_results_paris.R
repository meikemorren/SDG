
# from excel
OSDG <- c(15,
          15,
          15,
          15,
          15,
          15,
          15,
          15,
          15,
          15,
          15,
          15,
          14,
          15,
          15,
          13,
          0
)
OSDG1 <- c(8,
           3,
           7,
           15,
           6,
           5,
           8,
           32,
           9,
           14,
           5,
           5,
           11,
           8,
           3,
           21,
           0
)
Aurora <-c(25,
           14,
           9,
           6,
           13,
           9,
           5,
           36,
           7,
           7,
           8,
           4,
           10,
           10,
           8,
           27,
           39
)
ChatGPTada <- c(12,
                17,
                13,
                20,
                17,
                17,
                14,
                13,
                17,
                14,
                18,
                10,
                13,
                12,
                16,
                14,
                0
)

Auroralow<-c(7,
             6,
             4,
             2,
             2,
             2,
             1,
             14,
             2,
             3,
             0,
             0,
             2,
             4,
             2,
             12,
             19
)
OSDG1low<-c(1,
            2,
            3,
            6,
            0,
            1,
            1,
            8,
            1,
            3,
            1,
            0,
            4,
            3,
            0,
            9,
            0
)
Chatlow <-c(2,
            8,
            8,
            6,
            5,
            5,
            3,
            4,
            5,
            8,
            5,
            3,
            4,
            7,
            5,
            4,
            0
)
OSDGlow <- c(5,
             5,
             8,
             6,
             5,
             5,
             5,
             5,
             5,
             5,
             5,
             5,
             4,
             5,
             5,
             4,
             0
)

OSDGhigh <-c(10,
             9,
             7,
             9,
             10,
             10,
             9,
             10,
             7,
             6,
             8,
             4,
             8,
             10,
             5,
             0,
             0
)

Aurorahigh <-c(14,
               6,
               4,
               4,
               10,
               6,
               4,
               20,
               5,
               3,
               6,
               1,
               5,
               5,
               3,
               10,
               16
)
OSDG1high <-c(6,
              1,
              4,
              7,
              6,
              4,
              6,
              21,
              6,
              8,
              2,
              2,
              6,
              4,
              1,
              5,
              0
)

Chathigh<-c(9,
            8,
            5,
            13,
            12,
            10,
            10,
            8,
            9,
            4,
            10,
            4,
            8,
            5,
            6,
            1,
            0
)
OSDGhighpos <-c(5,
                4,
                2,
                4,
                5,
                5,
                4,
                5,
                3,
                3,
                3,
                3,
                3,
                5,
                0,
                0,
                0
                
)

Aurorahighpos <-c(10,
                  2,
                  1,
                  3,
                  5,
                  5,
                  2,
                  8,
                  2,
                  1,
                  2,
                  1,
                  4,
                  5,
                  1,
                  1,
                  1
)
OSDG1highpos <-c(4,
                 0,
                 1,
                 5,
                 4,
                 4,
                 3,
                 9,
                 2,
                 6,
                 0,
                 2,
                 3,
                 4,
                 0,
                 1,
                 0
                 
)

Chathighpos<-c(7,
               4,
               2,
               4,
               5,
               5,
               5,
               3,
               3,
               2,
               3,
               2,
               4,
               5,
               0,
               0,
               0
)
df<-as.data.frame(c(Aurora, ChatGPTada, OSDG, OSDG1))
colnames(df)[1]<-"Freq"
df['Model'] <- c(rep("Aurora", 17),
                 rep("ChatGPT (ada)", 17),
                 rep("OSDG (initial)", 17),
                 rep("OSDG (estimated)", 17))
df["SDG"] <- rep(1:17,4)
df["low"] <- c(Auroralow, Chatlow, OSDGlow, OSDG1low)
df["high"] <- c(Aurorahigh, Chathigh, OSDGhigh, OSDG1high)
df["highpos"] <- c(Aurorahighpos, Chathighpos, OSDGhighpos, OSDG1highpos)

ID = 1:17

df %>% 
  # filter(Model == "Aurora") %>%
  ggplot(aes(SDG,Freq))+
  geom_col()+
  theme(plot.title = element_text(size=14),
        axis.text.x = element_text(angle = 90, size=8, vjust = 0.5),
        axis.text.y = element_text(size=10),
        axis.title.y = element_blank()) +
  scale_x_continuous("SDG", labels = as.character(ID), breaks = ID)+
  facet_grid(~Model)+
  theme(strip.text = element_text(
    size = 15, color = "dark green"))


df %>% 
  # filter(Model == "Aurora") %>%
  ggplot(aes(SDG,low))+
  geom_col()+
  theme(plot.title = element_text(size=14),
        axis.text.x = element_text(angle = 90, size=8, vjust = 0.5),
        axis.text.y = element_text(size=10),
        axis.title.y = element_blank()) +
  scale_x_continuous("SDG", labels = as.character(ID), breaks = ID)+
  facet_grid(~Model)+
  theme(strip.text = element_text(
    size = 15, color = "dark green"))


df %>% 
  # filter(Model == "Aurora") %>%
  ggplot(aes(SDG,high))+
  geom_col()+
  theme(plot.title = element_text(size=14),
        axis.text.x = element_text(angle = 90, size=8, vjust = 0.5),
        axis.text.y = element_text(size=10),
        axis.title.y = element_blank()) +
  scale_x_continuous("SDG", labels = as.character(ID), breaks = ID)+
  facet_grid(~Model)+
  theme(strip.text = element_text(
    size = 15, color = "dark green"))

df %>% 
  # filter(Model == "Aurora") %>%
  ggplot(aes(SDG,highpos))+
  geom_col()+
  theme(plot.title = element_text(size=14),
        axis.text.x = element_text(angle = 90, size=8, vjust = 0.5),
        axis.text.y = element_text(size=10),
        axis.title.y = element_blank()) +
  scale_x_continuous("SDG", labels = as.character(ID), breaks = ID)+
  facet_grid(~Model)+
  theme(strip.text = element_text(
    size = 15, color = "dark green"))
