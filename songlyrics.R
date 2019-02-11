library(devtools)
#Install Github function to get download the R package from github
devtools::install_github("josiahparry/geniusR")
## Downloading GitHub repo josiahparry/geniusR@master
## from URL https://api.github.com/repos/josiahparry/geniusR/zipball/master

## Installing geniusR
## '/Library/Frameworks/R.framework/Resources/bin/R' --no-site-file  \
##   --no-environ --no-save --no-restore --quiet CMD INSTALL  \
##   '/private/var/folders/85/9ygtlz0s4nxbmx3kgkvbs5g80000gn/T/Rtmpl3bwRx/devtools33c73e3f989/JosiahParry-geniusR-5907d82'  \
##   --library='/Library/Frameworks/R.framework/Versions/3.4/Resources/library'  \
##   --install-tests

library(geniusR)
library(tidyverse)

swift_lyrics <- genius_album(artist="Taylor Swift", album="Reputation")
lorde_lyrics <- genius_album(artist="Lorde", album="Melodrama")

library(tidytext)
tidy_swift <- swift_lyrics %>%
  unnest_tokens(word,lyric) %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE)

head(tidy_swift)

tidy_lorde <- lorde_lyrics %>%
  unnest_tokens(word,lyric) %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE)

head(tidy_lorde)

tidy_swift <- tidy_swift %>%
  rename(swift_n = n) %>%
  mutate(swift_prop = swift_n/sum(swift_n))

tidy_lorde <- tidy_lorde %>%
  rename(lorde_n = n) %>%
  mutate(lorde_prop = lorde_n/sum(lorde_n))

compare_words <- tidy_swift %>%
  full_join(tidy_lorde, by = "word")

summary(compare_words)

ggplot(compare_words, aes(x=swift_prop, y=lorde_prop)) +
  geom_abline() +
  geom_text(aes(label=word), check_overlap=TRUE, vjust=1.5) +
  labs(y="Lorde", x="Taylor Swift") + theme_classic()
