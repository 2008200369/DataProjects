########### Intro to tidyverse #########
install.packages("gapminder")
library(gapminder)
library(dplyr)
library(tidyverse)

#filter
gapminder %>% 
  filter(country=="United States",year==2007)

#sorting
gapminder %>% 
  filter(country=="United States",year==2007) %>%
  arrange(lifeExp) #default is ascending 
gapminder %>% 
  arrange(desc(lifeExp))

#change variables
gapminder %>%
  mutate(lifeExpMonths= 12*lifeExp)

#summarize
gapminder %>% 
  filter(year==1957) %>% 
  summarize(medianLifeExp=median(lifeExp), maxGdpPercap=(max(gdpPercap)))

#group_by
gapminder %>%
  group_by(year) %>%
  summarize(medianLifeExp=median(lifeExp),maxGdpPercap=max(gdpPercap))



#ggplot
library(ggplot2)
ggplot(gapminder_1952, aes(x = pop, y = gdpPercap)) +
  geom_point()
# Change this plot to put the x-axis on a log scale
ggplot(gapminder_1952, aes(x = pop, y = lifeExp)) +
  geom_point()+ scale_x_log10()
# Scatter plot comparing pop and lifeExp, with color representing continent
ggplot(gapminder_1952,aes(x=pop, y=lifeExp, color=continent))+geom_point()+scale_x_log10()
# Add the size aesthetic to represent a country's gdpPercap
ggplot(gapminder_1952, aes(x = pop, y = lifeExp, color = continent,size=gdpPercap)) +
  geom_point() +
  scale_x_log10()
# Add the size aesthetic to represent a country's gdpPercap
ggplot(gapminder_1952, aes(x = pop, y = lifeExp)) +
  geom_point() +
  scale_x_log10()+
  facet_wrap(~continent)
# Expand limit in y
ggplot(by_year, aes(x=year,y=medianLifeExp)) +geom_point() +expand_limits(y=0)
# Create a line plot showing the change in medianGdpPercap over time
ggplot(by_year,aes(x=year, y=medianGdpPercap))+geom_line()+expand_limits(y=0)
# Create a bar plot showing medianGdp by continent
ggplot(by_continent,aes(x=continent,y=medianGdpPercap))+geom_col()
#histogram
ggplot(gapminder_1952,aes(x=pop))+geom_histogram(binwidth=10)+scale_x_log10()
#boxplot
ggplot(gapminder_1952,aes(x=continent, y=gdpPercap))+geom_boxplot()+scale_y_log10()
#add title
ggplot(gapminder_1952, aes(x = continent, y = gdpPercap)) +
  geom_boxplot() +
  scale_y_log10() +ggtitle("Comparing GDP per capita across continents")



############# Join tables ###############
# Complete the code to join artists to bands
bands2 <- left_join(bands, artists, by = c("first", "last")) #left_join: join things to the things on the left
bands3 <- right_join(artists,bands, by = c("first", "last"))
# Check that bands3 is equal to bands2
setequal(bands3, bands2)
# Join albums to songs using inner_join()
inner_join(songs,albums,by="album")
# Join bands to artists using full_join()
full_join(artists,bands,by=c("first","last"))
artists %>%  full_join(bands, by = c("first", "last"))#same results as the previous command


#using pipe to left join artists to bands then filter for guitars then select first, last and band
bands %>% 
  left_join(artists, by = c("first", "last")) %>% 
  filter(instrument == "Guitar") %>% 
  select(first,last,band)

#semi_join:filter data from the first dataset based on information in a second dataset.
#you can use a semi-join to determine the number of albums in the albums dataset that were made by a band in the bands dataset.
albums %>% 
  # Collect the albums made by a band
  semi_join(bands, by ="band") %>% 
  # Count the albums made by a band
  nrow()

#anti_join: use to check mispelling, showing which rows will not be matched to a second dataset by a join.
artists %>% 
  anti_join(bands, by = c("first","last"))


#union, intersect, setdiff: used for datasets that have same variables
#union: delete duplicated cases and stack them together
#Use a set operation to create a dataset with every song contained on aerosmith and/or greatest_hits.
aerosmith %>% 
  union(greatest_hits) %>% 
  nrow()

#Use a set operation to create a dataset containing every track that is in both Aerosmith and Greatest Hits.
aerosmith %>% 
  intersect(greatest_hits)

#select cases in the first table and not in the second table
live_songs %>% setdiff(greatest_songs)



# Find songs in at least one of live_songs and greatest_songs but not in both
all_songs <- union(live_songs,greatest_songs)
common_songs <- intersect(live_songs,greatest_songs)
setdiff(all_songs,common_songs)

#Note:When your datasets contain the same variables, a setdiff() does the same thing as an anti_join() that uses every column as a key.





#rbind, cbind
# Bind side_one and side_two into a single dataset
side_one %>% 
  bind_rows(side_two)

#Bind jimi by rows into a single data frame. As you do, save the data frame names as a column named album by specifying the .id argument to bind_rows().
#Left join discography to the results to make a complete data frame.
jimi %>% 
  bind_rows( ,.id="album") %>% 
  left_join(discography, by = "album")


hank_years %>% 
  # Reorder hank_years alphabetically by song title
  arrange(song) %>% 
  # Select just the year column
  select(year) %>% 
  # Bind the year column
  bind_cols(hank_charts) %>% 
  # Arrange the finished dataset chronologically by year, then alphabetically by song title within each year.
  arrange(year,song)


#key variable years are factors in 70s, need to convert to numeric then bind with 60s
seventies %>% 
  # Coerce seventies$year into a useful numeric
  mutate(year = as.numeric(as.character(year))) %>% 
  # Bind the updated version of seventies to sixties
  bind_rows(sixties) %>% 
  arrange(year)



#data_frame
# Make combined data frame using data_frame()
data_frame(year= hank_year,song= hank_song,peak=hank_peak) %>% 
  # Extract songs where peak equals 1
  filter(peak==1)

# Convert the hank list into a data frame
as_data_frame(hank) %>% 
  # Extract songs where peak equals 1
  filter(peak==1)


#If the primary key of your dataset is stored in row.names, you will have trouble joining it to other datasets.
#need to change row names to a column
library(tibble)
rownames_to_column(name of the table,var="name to add")
#or
stage_songs %>% 
  # Add row names as a column named song
  rownames_to_column(var="song") %>% 
  # Left join stage_writers to stage_songs
  left_join(stage_writers,by="song")

#missing values in key column
seventies %>%
  filter(!is.na(year)) %>%
  left_join(sixties, by="year")


#mismatched key names:
left_join(members, plays, by = c("col_name_in_1_table" = "col_name_in_2_table"))

#conflicting names: same name in different datasets
left_join(members, plays, by = "name",suffix = c("1","2")) #will return: name, plays1, plays2
#or use rename(data, new_name = old_name)
movie_years %>% 
  # Left join movie_studios to movie_years
  left_join(movie_studios,by = "movie") %>% 
  # Rename the columns: artist and studio
  rename( "artist" = name.x, "studio" = name.y) 


#join multiple tables use "purrr" package
install.packages("purrr")
library("purrr")
list(data1,data2,data3) %>%
  reduce(left_join, by = "name")

#Use reduce() to return just the rows of more_artists that appear in all three datasets. "filtering join
list(more_artists, more_bands, supergroups) %>% 
  # Return rows of more_artists in all three datasets
  reduce(semi_join, by = c("first","last"))

#similar to how merge() works
merge(bands, artists, by = c("first", "last"), all.x = TRUE) %>%
  arrange(band) #left_join, same results as:
bands %>%
  left_join(artists, by = c("first", "last")) 

############# dplyr connection to SQL ###########33



#connect to a database then manipulate the database from R use dplyr code
install.packages("DBI")
src_sqlite()
src_mysql()
src_postgres()

#import results to r
collect()

#more info
vignette("databases",package ="dplyr")



########## Case study #################
library(dplyr)
library(Lahman) #football database
LahmanData
# Find variables in common
library(purrr)
lahmanData %>%
  reduce(intersect)

lahmanNames %>%  
  # Bind the data frames in lahmanNames into a single table that includes a new column called dataframe, which contains the names of the data frames.
  bind_rows(,.id="dataframe") %>%
  # Group the result by var
  group_by(var) %>%
  # Tally the number of appearances
  tally() %>%
  # Filter the data
  filter(n>=2) %>% 
  # Arrange the results
  arrange(desc(n))

#find the datasets that has playerID
lahmanNames %>% 
  # Bind the data frames
  bind_rows(,.id="dataframe") %>%
  # Filter the results
  filter(var=="playerID") %>% 
  # Extract the dataframe variable
  '$'(dataframe)

#find unique rows for particular columns of dataset Master
Master %>% 
# Return one row for each distinct player
distinct(playerID,nameFirst,nameLast)

#check salary

#how many games did unsalaried players play
players %>% 
  # Find all players who do not appear in Salaries
  anti_join(Salaries, by = "playerID") %>% 
  # Join them to Appearances
  left_join(Appearances, by = "playerID") %>% 
  # Calculate total_games for each player
  group_by(playerID) %>%
  summarize(total_games=sum(G_all,na.rm=TRUE)) %>%
  filter(!is.na(total_games)) %>%
  # Arrange in descending order by total_games
  arrange(desc(total_games))

#whether any players were nominated before they completely retired. 
Appearances %>% 
  # Filter Appearances against nominated
  semi_join(nominated, by = "playerID") %>% 
  # Find last year played by player
  group_by(playerID) %>% 
  summarize(last_year = max(yearID)) %>% 
  # Join to full HallOfFame
  left_join(HallOfFame, by = "playerID") %>% 
  # Filter for unusual observations
  filter(last_year > yearID | last_year == yearID)
