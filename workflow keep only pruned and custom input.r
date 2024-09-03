library(ape)

# Read the tree file
tree <- read.tree("C:/Users/chiti/Desktop/coding/PhD code/tree files/Primates_genus.nwk")

# Allow ordinal suffixes for numbers (e.g., 1st, 2nd, 3rd, 4th, etc.) for formlity.
ordinal <- function(x) {
  if (x %% 10 == 1 && x %% 100 != 11) {
    return(paste0(x, "st"))
  } else if (x %% 10 == 2 && x %% 100 != 12) {
    return(paste0(x, "nd"))
  } else if (x %% 10 == 3 && x %% 100 != 13) {
    return(paste0(x, "rd"))
  } else {
    return(paste0(x, "th"))
  }
}

# Prompt for the user to input the number of species they want to work with
num_tips <- as.integer(readline(prompt = "Enter the number of species you will be working with: "))

# Initialize an empty vector to store the names of the speices the user is working with
tips <- vector("character", length = num_tips)

# Loop to prompt the user to input each species they are working with
for (i in 1:num_tips) {
  tips[i] <- readline(prompt = paste("Enter the name of the", ordinal(i), "species you are working with: "))
}

# Convert species to lowercase (or uppercase) for case-insensitive matching
tips <- tolower(tips)

# Convert tree species labels to lowercase (or uppercase) for comparison
tree$tip.label <- tolower(tree$tip.label)

# Check if all provided species are in the tree
valid_tips <- tips %in% tree$tip.label
if (any(!valid_tips)) {
  stop("Some of the entered tips are not present in the tree: ", paste(tips[!valid_tips], collapse = ", "))
}

# Get the most recent common ancestor (MRCA) node for the specified species
ancestor_node <- getMRCA(tree, tips)
if (is.na(ancestor_node)) {
  stop("The specified tips do not have a common ancestor or are not present in the tree.")
}

# Extract the clade corresponding to the MRCA node
pruned_tree <- extract.clade(tree, node = ancestor_node)

# Write the pruned tree to a file
write.tree(pruned_tree, file = "C:/Users/chiti/Desktop/coding/PhD code/tree files/Pruned tree.nwk")

# Plot the pruned tree
plot(pruned_tree, main = "Pruned Tree")

# Add node labels to the plot
nodelabels(pruned_tree$node.label, cex = 0.5, frame = "none")