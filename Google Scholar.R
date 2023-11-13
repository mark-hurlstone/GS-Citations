# Google Scholar Citation Information For Promotion Application

library(PEIP)
library(pracma)
library(scholar)
library(tidyverse)
library(ggplot2)

# Retrieve my Google Scholar profile and citation metrics
myID <- "Y3VEQboAAAAJ&hl"
myProfile <- get_profile(myID)
myCitations = get_citation_history(myID)

# Google Scholar IDs for Katie Alcock, Tom Beesley, Francesca Citron, Robert Davies
# Margriet Groen, Calum Hartley, Sally Linkenauger, Helen Nuthall, Jared Piaza, Stefan Vogt
# Missing values: Jill Lany and Chris Walton (no Google Scholar Accounts)
srID <- c("5s8tS1oAAAAJ&hl","bZAgwTQAAAAJ&hl","Oqp5IKgAAAAJ&hl","1NqbU6UAAAAJ&hl",
          "m0X1ONUAAAAJ&hl","3AfgtqgAAAAJ&hl","JrmMdBYAAAAJ&hl","gOBYRU4AAAAJ&hl",
          "xpabkcIAAAAJ&hl","D84eSdgAAAAJ&hl","C6RZsgYAAAAJ&hl","YSa-M3YAAAAJ",
          "WejYCqQAAAAJ&hl")

# Initialize variable for holding average SL h-index
srHIndex <- zeros(1,1)

# Initialize variable for holding average SL i10-index
sri10Index <- zeros(1,1)

# Initialize variable for holding average SL citations
srCitations <- zeros(1,nrow(myCitations))

for (i in 1:length(srID)) {
  
  ID <- srID[i]
  profile <- get_profile(ID)
  getCitations = get_citation_history(ID)[,2]
  
  # Only include citations from the past 11 years (Google Scholar default)
  # Add zero-padding to "getCitations" if the researcher only began accruing
  # citations after the start of the 11 year window
  if (length(getCitations) < nrow(myCitations)){
    diff <- length(myCitations[,2]) - length(getCitations)
    getCitations <- c(zeros(1,diff),getCitations) 
  }

  # Now retrieve the citations
  getCitations <- getCitations[(length(getCitations)-10):length(getCitations)] 
  
  # Update variables
  srHIndex <- srHIndex + profile$h_index
  sri10Index <- sri10Index + profile$i10_index
  srCitations = srCitations + getCitations
  
}
# Calculate averge h-index
srHIndex = srHIndex/length(srID)  

# Calculate averge i10-index
sri10Index = sri10Index/length(srID)  

# Calculate averge citation frequency
srCitations = srCitations/length(srID)  

# Create dataframe combining my citations with the SL average
citations <- rbind(myCitations[,1],myCitations[,2],srCitations)

# Transpose array
citations <- t(citations)

# Convert to a dataframe to facilitate plotting
df <- citations %>%
  as_tibble() %>%
  setNames(c('Year','Hurlstone','SL'))

# Get start and end dates for plotting 
start <- myCitations[1,1] 
end <- myCitations[nrow(myCitations),1]

# Plot citation history
ggplot(data = df, mapping = aes(x = Year, y = SL, color = as.factor(SL), fill = as.factor(SL))) + 
  geom_bar(stat = "identity", width = 0.5, position = position_dodge()) +
  scale_colour_manual(values=c(rep("#355C7D",nrow(myCitations)))) +
  scale_fill_manual(values=c(rep("#355C7D",nrow(myCitations)))) +
  geom_line(aes(x = Year, y = Hurlstone, group = 1), size = 1, color = "#F67280") + 
  geom_point(aes(x = Year, y = Hurlstone, group = 1), size = 3, color = "#F67280") +
  scale_y_continuous(expand = c(0,0), limits = c(0,550), breaks = seq(0,550,100)) +
  labs(x = "Year", y = "Google Scholar Citation Frequency") +
  scale_x_continuous(breaks = seq(start,end,1)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position = "none",
        axis.title.x = element_text(size = 14, face = "bold"), 
        axis.text.x = element_text(size = 12, color = "black"),
        axis.title.y = element_text(size = 14, face = "bold"), 
        axis.text.y = element_text(size = 12, color = "black"),
        legend.title = element_text(size = 14, face = "bold") 
  ) 
ggsave("GoogleScholarFigure.pdf", width = 8, height = 5)
