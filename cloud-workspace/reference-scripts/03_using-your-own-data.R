# To use your own data, upload it to the "data" folder 
# using the "Upload" button in the Files tab in the bottom
# right corner of the interface. Then work on it as you 
# would do normally! 
my_data <- readxl::read_excel("data/my-data.xlsx")

# If you prefer to use data stored somewhere online as your
# starting point, you can do that too! 
# Here's an example using the GBBO dataset from a recent TidyTuesday challenge.
my_data <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-25/episodes.csv")

