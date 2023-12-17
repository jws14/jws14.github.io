# 10 most frequently quaked countries
quaked <- earthquake %>% 
  group_by(Country , Lands) %>% 
  summarize(count =n()) %>% 
  arrange(desc(count)) %>% 
  filter(row_number() <=10) %>% 
  head(quaked, n = 10)


# the month with the most earthquakes occuring 
earthquake$Month <- rowSums(earthquake[,4:5])
earthquake$Month[1:6055] <- "NOVEMBER"
earthquake$Month[6056:14543] <- "OCTOBER"
earthquake$Month[14544:23404] <- "SEPTEMBER"
earthquake$Month[23405:31631] <- "AUGUST"
earthquake$Month[31632:40673] <- "JULY"
earthquake$Month[40674:48647] <- "JUNE"
earthquake$Month[48648:53539] <- "MAY"

densemonth <- earthquake %>% 
  group_by(Month) %>% 
  summarize(count =n()) %>% 
  arrange(desc(count)) %>% 
  filter(row_number() <=7)


# visualization
dense <- earthquake %>% 
  group_by(Month,Country,Lands) %>% 
  summarize(count =n()) %>% 
  arrange(desc(count)) %>% 
  filter(row_number() <=7) %>% 
  head(dense, n=15)


ggplot(dense) +
  geom_bar(aes(y = Country, x = stat(count),
               group = 1)) +
  facet_wrap(Month ~ . , ncol = 5)


ggplot(dense,
       aes(y = Month,
           fill = Country)) +
  geom_bar(show.legend = T)


ggplot(dense) +
  geom_bar(aes(y = Country, 
               x = stat(prop), group = 1)) +
  facet_grid(Month ~.)

# Summary Statistics 
summagnitude <- earthquake %>% 
  summarize(
    mean_magnitude = mean(Magnitude, na.rm = T), 
    sd_magnitude = sd(Magnitude),
    q1_magnitude = quantile(Magnitude, probs = .25, na.rm = T),
    median_magnitude = median(Magnitude),
    q3_magnitude = quantile(Magnitude, probs = .75, na.rm =  T),
    min_magnitude = min(Magnitude, na.rm = T),
    max_magnitude = max(Magnitude, na.rm = T)
  )

skim(earthquake$Magnitude)
skim(earthquake$Depth)

# shows the frequencey of how often a level of Magnitude and depth occurs
ggplot(earthquake, aes(x = log(Magnitude))) +
  geom_freqpoly(bins = 100)

ggplot(earthquake, aes(x = Magnitude)) +
  geom_freqpoly(bins = 100)

ggplot(earthquake, aes(x = log(Depth))) +
  geom_histogram(bins = 50)

ggplot(earthquake, aes(x = Depth)) +
  geom_histogram(bins = 50)
 
# how depth and magnitude differ in our visualizations

ggplot(earthquake, aes(x = Magnitude, y = Depth )) +
  geom_line() +
  labs(title = "Relationship between Magnitude and Depth",
       x = "Magnitude",
       y = "Depth")
# THE DATA IS TOO GIANT IN ORDER TO PLOT INTO A 
# LINE PLOT BUT WE CAN SEE THAT THE MAGNITUDE 
# BETWEEN 3 AND 4 HAS THE GREATEST DEPTH 


# Create a histogram with facets
ggplot(earthquake, aes(x = Magnitude)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black", alpha = 0.7) +
  facet_wrap(~Month, scales = "free_y") +
  labs(title = "Histogram of Magnitude by month", x = "Magnitude", y = "Count")


# Manginutde and depth
ggplot(earthquake,
       aes(x = Magnitude, y = Depth)) +
  geom_point(alpha = .05, size = .75) +
  geom_smooth() +
  geom_smooth(method = lm, color = 'red')

# magnitude and depth
ggplot(earthquake,
       aes(x = Magnitude, y = Depth)) +
  geom_hex() +
  geom_smooth() +
  geom_smooth(method = lm, color = 'red')


ggplot(earthquake) +
  geom_bar(aes(x = Magnitude, 
               y = stat(count), group = 1)) 
# _____________________________________________________

earthquake$Season <- rowSums(earthquake[,4:5])
earthquake$Season[48648:53539] <- "SPRING"
earthquake$Season[23405:48647] <- "SUMMER"
earthquake$Season[1:23404] <- "FALL"


# which season is more earthquake prone 
denseseason <- earthquake %>% 
  group_by(Season) %>% 
  summarize(count =n()) %>% 
  arrange(desc(count)) %>% 
  filter(row_number() <=3)

# visualization
ggplot(earthquake) +
  geom_bar(aes(x = Month, y = stat(count),
               group = 1)) +
  facet_wrap(Season ~ . , ncol = 5)



