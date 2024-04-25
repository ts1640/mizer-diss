
# Load Mizer package.
library(mizer)

# Data frame contains 12 rows and 7 columns, each row is a species.
# Data set contains no gear column; assumed each species fished by a separate gear
NS_species_params

# Selectivity function - size-based model is weight based.
# Length - weight parameters 'a' & 'b' are used to convert between length and weight
NS_species_params$a <- c(0.0058, 0.0023, 0.006, 0.0056, 0.0078, 0.0062, 0.0072, 
                         0.0079, 0.0078, 0.0066, 0.0068, 0.0066)
NS_species_params$b <- c(3.09, 3.07, 3.06, 3.09, 3.09, 3.06, 3.07, 3.05, 3.07, 
                         3.08, 3.08, 3.06)

# Defining gear parameters, gears are species specific.
#'Sigmoid_length' selectivity function 'l25' gives a selectivity of 25% and 'l50' gives selectivity of 50%.
gear_params <- 
  data.frame(species = NS_species_params$species,
             gear = NS_species_params$species,
             sel_func = "sigmoid_length",
             l25 =  c(7.6, 9.8, 8.7, 10.1, 11.5, 19.8, 16.4, 19.8, 11.5,
                      19.1, 13.2, 35.3),
             l50 = c(8.1, 11.8, 12.2, 20.8, 17.0, 29.0, 25.8, 29.0, 17.0,
                     24.3, 22.9, 43.6))
 
gear_params


# Catchability set at a default of 1 for all species and their gear
gear_params$catchability <- 1

gear_params
# Set parameters
# Interaction coefficient between predator and prey species in the North Sea.
# Format A 12 x 12 matrix.
NS_interaction
params_temporal_harvest <- newMultispeciesParams(NS_species_params, NS_interaction, gear_params = gear_params)

NS_species_params

# Periodic harvest
# Create effort array - fishing is turned off then on for all gears every 5 years.
times <- seq(from = 0, to = 50, by = 1)
gear_names <- NS_species_params$species
effort_array <- array(NA, dim = c(length(times), length(gear_names)),
                      dimnames = list(time = times, gear = gear_names))

effort_array[0:5, NS_species_params$species] <- 0
effort_array[6:10, NS_species_params$species] <- 0.5
effort_array[11:15, NS_species_params$species] <- 0
effort_array[16:20, NS_species_params$species] <- 0.5
effort_array[21:25, NS_species_params$species] <- 0
effort_array[26:30, NS_species_params$species] <- 0.5
effort_array[31:35, NS_species_params$species] <- 0
effort_array[36:40, NS_species_params$species] <- 0.5
effort_array[41:45, NS_species_params$species] <- 0
effort_array[46:50, NS_species_params$species] <- 0.5

effort_array

# Project forward with selected fishing effort
# How much is scaled by? The study area of north sea?

sim_params_temporal_harvest <- project(params_temporal_harvest, effort = effort_array , dt = 0.1)




# Summary plot
plot(sim_params_temporal_harvest)
plot(sim_params_temporal_harvest_double)

### Periodic Harvest Double Intensity ###

times <- seq(from = 0, to = 50, by = 1)
gear_names <- NS_species_params$species
effort_array_double <- array(NA, dim = c(length(times), length(gear_names)),
                      dimnames = list(time = times, gear = gear_names))

effort_array_double[0:5, NS_species_params$species] <- 0
effort_array_double[6:10, NS_species_params$species] <- 1
effort_array_double[11:15, NS_species_params$species] <- 0
effort_array_double[16:20, NS_species_params$species] <- 1
effort_array_double[21:25, NS_species_params$species] <- 0
effort_array_double[26:30, NS_species_params$species] <- 1
effort_array_double[31:35, NS_species_params$species] <- 0
effort_array_double[36:40, NS_species_params$species] <- 1
effort_array_double[41:45, NS_species_params$species] <- 0
effort_array_double[46:50, NS_species_params$species] <- 1
effort_array_double
# Project forward with selected fishing effort (double)
sim_params_temporal_harvest_double <- project(params_temporal_harvest, effort = effort_array_double , dt = 0.1)

# Summary plot
plot(sim_params_temporal_harvest_double)



### Constant harvest ###

# Set parameters for constant harvest
params_constant_harvest <- newMultispeciesParams(NS_species_params, NS_interaction, gear_params = gear_params)

# Project model
sim_params_constant_harvest <- project(params_constant_harvest, effort = 0.5 , dt = 0.1, t_max = 50)

# Summary plot
plot(sim_params_constant_harvest)

effort_array

### No harvest ###

sim_params_no_harvest <- project(params_constant_harvest, effort = 0 , dt = 0.1, t_max = 50)

plot(sim_params_no_harvest)


## Calculate yield for the constant harvest scenario ##
constant_harvest_yield <- getYield(sim_params_constant_harvest)

### YEILD BETWEEN 17 - 52 YEARS ###

# Subset yield data from year 17 to 52
constant_harvest_yield_subset <- constant_harvest_yield[16:50, ]
constant_harvest_yield_subset
# Total yield between year 17 and 52 per species #
species_code_cod <- "Cod"
species_code_saithe <- "Saithe"
species_code_haddock <- "Haddock"
species_code_plaice <- "Plaice"
species_code_gurnard <- "Gurnard"
species_code_sole <- "Sole"
species_code_whiting <- "Whiting"
species_code_dab <- "Dab"
species_code_herring <- "Herring"
species_code_n.pout <- "N.pout"
species_code_sandeel <- "Sandeel"
species_code_sprat <- "Sprat"
# Cod
cod_data_constant2 <- constant_harvest_yield_subset[, species_code_cod]
cod_sum2 <- sum(cod_data_constant2)
cod_data_constant2
effort_array
# Saithe
saithe_data_constant2 <- constant_harvest_yield[16:50, species_code_saithe]
saithe_sum2 <- sum(saithe_data_constant2)
saithe_data_constant2
# Haddock
haddock_data_constant2 <- constant_harvest_yield[16:50, species_code_haddock]
haddock_sum2 <- sum(haddock_data_constant2)

# Plaice
plaice_data_constant2 <- constant_harvest_yield[16:50, species_code_plaice]
plaice_sum2 <- sum(plaice_data_constant2)

# Gurnard
gurnard_data_constant2 <- constant_harvest_yield[16:50, species_code_gurnard]
gurnard_sum2 <- sum(gurnard_data_constant2)

# Sole
sole_data_constant2 <- constant_harvest_yield[16:50, species_code_sole]
sole_sum2 <- sum(sole_data_constant2)

# Whiting
whiting_data_constant2 <- constant_harvest_yield[16:50, species_code_whiting]
whiting_sum2 <- sum(whiting_data_constant2)

# Dab
dab_data_constant2 <- constant_harvest_yield[16:50, species_code_dab]
dab_sum2 <- sum(dab_data_constant2)

# Herring
herring_data_constant2 <- constant_harvest_yield[16:50, species_code_herring]
herring_sum2 <- sum(herring_data_constant2)

# N.pout
npout_data_constant2 <- constant_harvest_yield[16:50, species_code_n.pout]
npout_sum2 <- sum(npout_data_constant2)

# Sandeel
sandeel_data_constant2 <- constant_harvest_yield[16:50, species_code_sandeel]
sandeel_sum2 <- sum(sandeel_data_constant2)

# Sprat
sprat_data_constant2 <- constant_harvest_yield[16:50, species_code_sprat]
sprat_sum2 <- sum(sprat_data_constant2)


# Create a dataframe to store the results
results_yield_constant2 <- data.frame(
  Species = c("Cod", "Saithe", "Haddock", "Plaice", "Gurnard", "Sole", 
              "Whiting", "Dab", "Herring", "N.pout", "Sandeel", "Sprat"),
  Total_Yield = c(cod_sum2, saithe_sum2, haddock_sum2, plaice_sum2, gurnard_sum2,
                  sole_sum2, whiting_sum2, dab_sum2, herring_sum2, npout_sum2,
                  sandeel_sum2, sprat_sum2)
)


year_ranges <- list(c("15", "16", "17", "18", "19"),
                    c("25", "26", "27", "28", "29"),
                    c("35", "36", "37", "38", "39"),
                    c("45", "46", "47", "48", "49"))


cod_mean_constant <- numeric(length(year_ranges))

# Loop through each range of years
for (i in seq_along(year_ranges)) {
  # Subset the data for the current range of years and calculate the mean
  subset_data <- cod_data_constant2[year_ranges[[i]]]
  cod_mean_constant[i] <- mean(subset_data)
}

# Calculate the mean of the mean values
overall_mean_cod_constant <- mean(cod_mean_constant)

# Print the overall mean
print(overall_mean_cod_constant)


# Initialize an empty vector to store the means for saithe
saithe_mean_constant <- numeric(length(year_ranges))

# Loop through each range of years
for (i in seq_along(year_ranges)) {
  # Subset the data for the current range of years and calculate the mean for saithe
  subset_data <- saithe_data_constant2[year_ranges[[i]]]  # Replace saithe_data_constant2 with the actual data for saithe
  saithe_mean_constant[i] <- mean(subset_data)
}

# Calculate the mean of the mean values for saithe
overall_mean_saithe_constant <- mean(saithe_mean_constant)

# Print the overall mean for saithe
print(overall_mean_saithe_constant)



# Initialize an empty vector to store the means for haddock
haddock_mean_constant <- numeric(length(year_ranges))

# Loop through each range of years
for (i in seq_along(year_ranges)) {
  # Subset the data for the current range of years and calculate the mean for haddock
  subset_data <- haddock_data_constant2[year_ranges[[i]]]  # Replace haddock_data_constant2 with the actual data for haddock
  haddock_mean_constant[i] <- mean(subset_data)
}

# Calculate the mean of the mean values for haddock
overall_mean_haddock_constant <- mean(haddock_mean_constant)

# Print the overall mean for haddock
print(overall_mean_haddock_constant)

# Initialize an empty vector to store the means for plaice
plaice_mean_constant <- numeric(length(year_ranges))

# Loop through each range of years
for (i in seq_along(year_ranges)) {
  # Subset the data for the current range of years and calculate the mean for plaice
  subset_data <- plaice_data_constant2[year_ranges[[i]]]  # Replace plaice_data_constant2 with the actual data for plaice
  plaice_mean_constant[i] <- mean(subset_data)
}

# Calculate the mean of the mean values for plaice
overall_mean_plaice_constant <- mean(plaice_mean_constant)

# Print the overall mean for plaice
print(overall_mean_plaice_constant)



# Initialize an empty vector to store the means for gurnard
gurnard_mean_constant <- numeric(length(year_ranges))

# Loop through each range of years
for (i in seq_along(year_ranges)) {
  # Subset the data for the current range of years and calculate the mean for gurnard
  subset_data <- gurnard_data_constant2[year_ranges[[i]]]  # Replace gurnard_data_constant2 with the actual data for gurnard
  gurnard_mean_constant[i] <- mean(subset_data)
}

# Calculate the mean of the mean values for gurnard
overall_mean_gurnard_constant <- mean(gurnard_mean_constant)

# Print the overall mean for gurnard
print(overall_mean_gurnard_constant)


# Initialize an empty vector to store the means for sole
sole_mean_constant <- numeric(length(year_ranges))

# Loop through each range of years
for (i in seq_along(year_ranges)) {
  # Subset the data for the current range of years and calculate the mean for sole
  subset_data <- sole_data_constant2[year_ranges[[i]]]  # Replace sole_data_constant2 with the actual data for sole
  sole_mean_constant[i] <- mean(subset_data)
}

# Calculate the mean of the mean values for sole
overall_mean_sole_constant <- mean(sole_mean_constant)

# Print the overall mean for sole
print(overall_mean_sole_constant)


# Initialize an empty vector to store the means for whiting
whiting_mean_constant <- numeric(length(year_ranges))

# Loop through each range of years
for (i in seq_along(year_ranges)) {
  # Subset the data for the current range of years and calculate the mean for whiting
  subset_data <- whiting_data_constant2[year_ranges[[i]]]  # Replace whiting_data_constant2 with the actual data for whiting
  whiting_mean_constant[i] <- mean(subset_data)
}

# Calculate the mean of the mean values for whiting
overall_mean_whiting_constant <- mean(whiting_mean_constant)

# Print the overall mean for whiting
print(overall_mean_whiting_constant)



# Initialize an empty vector to store the means for dab
dab_mean_constant <- numeric(length(year_ranges))

# Loop through each range of years
for (i in seq_along(year_ranges)) {
  # Subset the data for the current range of years and calculate the mean for dab
  subset_data <- dab_data_constant2[year_ranges[[i]]]  # Replace dab_data_constant2 with the actual data for dab
  dab_mean_constant[i] <- mean(subset_data)
}

# Calculate the mean of the mean values for dab
overall_mean_dab_constant <- mean(dab_mean_constant)

# Print the overall mean for dab
print(overall_mean_dab_constant)



# Initialize an empty vector to store the means for herring
herring_mean_constant <- numeric(length(year_ranges))

# Loop through each range of years
for (i in seq_along(year_ranges)) {
  # Subset the data for the current range of years and calculate the mean for herring
  subset_data <- herring_data_constant2[year_ranges[[i]]]  # Replace herring_data_constant2 with the actual data for herring
  herring_mean_constant[i] <- mean(subset_data)
}

# Calculate the mean of the mean values for herring
overall_mean_herring_constant <- mean(herring_mean_constant)

# Print the overall mean for herring
print(overall_mean_herring_constant)


# Initialize an empty vector to store the means for npout
npout_mean_constant <- numeric(length(year_ranges))

# Loop through each range of years
for (i in seq_along(year_ranges)) {
  # Subset the data for the current range of years and calculate the mean for npout
  subset_data <- npout_data_constant2[year_ranges[[i]]]  # Replace npout_data_constant2 with the actual data for npout
  npout_mean_constant[i] <- mean(subset_data)
}

# Calculate the mean of the mean values for npout
overall_mean_npout_constant <- mean(npout_mean_constant)

# Print the overall mean for npout
print(overall_mean_npout_constant)

# Initialize an empty vector to store the means for sandeel
sandeel_mean_constant <- numeric(length(year_ranges))

# Loop through each range of years
for (i in seq_along(year_ranges)) {
  # Subset the data for the current range of years and calculate the mean for sandeel
  subset_data <- sandeel_data_constant2[year_ranges[[i]]]  # Replace sandeel_data_constant2 with the actual data for sandeel
  sandeel_mean_constant[i] <- mean(subset_data)
}

# Calculate the mean of the mean values for sandeel
overall_mean_sandeel_constant <- mean(sandeel_mean_constant)

# Print the overall mean for sandeel
print(overall_mean_sandeel_constant)



# Initialize an empty vector to store the means for sprat
sprat_mean_constant <- numeric(length(year_ranges))

# Loop through each range of years
for (i in seq_along(year_ranges)) {
  # Subset the data for the current range of years and calculate the mean for sprat
  subset_data <- sprat_data_constant2[year_ranges[[i]]]  # Replace sprat_data_constant2 with the actual data for sprat
  sprat_mean_constant[i] <- mean(subset_data)
}

# Calculate the mean of the mean values for sprat
overall_mean_sprat_constant <- mean(sprat_mean_constant)

# Print the overall mean for sprat
print(overall_mean_sprat_constant)


# Create a data frame to store the mean yields
mean_yield_constant2 <- data.frame(
  Species = c("Cod", "Saithe", "Haddock", "Plaice", "Gurnard", "Sole", 
              "Whiting", "Dab", "Herring", "N.pout", "Sandeel", "Sprat"),
  Total_Yield = c(overall_mean_cod_constant, overall_mean_saithe_constant, overall_mean_haddock_constant, overall_mean_plaice_constant,
                  overall_mean_gurnard_constant, overall_mean_sole_constant, overall_mean_whiting_constant, overall_mean_dab_constant,
                  overall_mean_herring_constant, overall_mean_npout_constant, overall_mean_sandeel_constant, overall_mean_sprat_constant)
)

# Print the data frame
print(mean_yield_constant2)


######
# Calculate yield for the temporal harvest scenario
temporal_harvest_yield <- getYield(sim_params_temporal_harvest)

# Total yield between year 17 and 52 for each species
# Cod
cod_data_temporal2 <- temporal_harvest_yield[16:50, species_code_cod]
cod_sum_temporal2 <- sum(cod_data_temporal2)

# Saithe
saithe_data_temporal2 <- temporal_harvest_yield[16:50, species_code_saithe]
saithe_sum_temporal2 <- sum(saithe_data_temporal2)

# Haddock
haddock_data_temporal2 <- temporal_harvest_yield[16:50, species_code_haddock]
haddock_sum_temporal2 <- sum(haddock_data_temporal2)

# Plaice
plaice_data_temporal2 <- temporal_harvest_yield[16:50, species_code_plaice]
plaice_sum_temporal2 <- sum(plaice_data_temporal2)

# Gurnard
gurnard_data_temporal2 <- temporal_harvest_yield[16:50, species_code_gurnard]
gurnard_sum_temporal2 <- sum(gurnard_data_temporal2)

# Sole
sole_data_temporal2 <- temporal_harvest_yield[16:50, species_code_sole]
sole_sum_temporal2 <- sum(sole_data_temporal2)

# Whiting
whiting_data_temporal2 <- temporal_harvest_yield[16:50, species_code_whiting]
whiting_sum_temporal2 <- sum(whiting_data_temporal2)

# Dab
dab_data_temporal2 <- temporal_harvest_yield[16:50, species_code_dab]
dab_sum_temporal2 <- sum(dab_data_temporal2)

# Herring
herring_data_temporal2 <- temporal_harvest_yield[16:50, species_code_herring]
herring_sum_temporal2 <- sum(herring_data_temporal2)

# N.pout
npout_data_temporal2 <- temporal_harvest_yield[16:50, species_code_n.pout]
npout_sum_temporal2 <- sum(npout_data_temporal2)

# Sandeel
sandeel_data_temporal2 <- temporal_harvest_yield[16:50, species_code_sandeel]
sandeel_sum_temporal2 <- sum(sandeel_data_temporal2)

# Sprat
sprat_data_temporal2 <- temporal_harvest_yield[16:50, species_code_sprat]
sprat_sum_temporal2 <- sum(sprat_data_temporal2)

# Create a dataframe to store the results
results_yield_temporal2 <- data.frame(
  Species = c("Cod", "Saithe", "Haddock", "Plaice", "Gurnard", "Sole", 
              "Whiting", "Dab", "Herring", "N.pout", "Sandeel", "Sprat"),
  Total_Yield_temporal2 = c(cod_sum_temporal2, saithe_sum_temporal2, haddock_sum_temporal2, plaice_sum_temporal2, gurnard_sum_temporal2,
                            sole_sum_temporal2, whiting_sum_temporal2, dab_sum_temporal2, herring_sum_temporal2, npout_sum_temporal2,
                            sandeel_sum_temporal2, sprat_sum_temporal2)
)
# Calculate mean for each species
# calculate the mean for specific ranges of years (fishing years only)
# Define the ranges of years


# Initialize an empty vector to store the means
cod_mean_temporal2 <- numeric(length(year_ranges))

# Loop through each range of years
for (i in seq_along(year_ranges)) {
  # Subset the data for the current range of years and calculate the mean
  subset_data <- cod_data_temporal2[year_ranges[[i]]]
  cod_mean_temporal2[i] <- mean(subset_data)
}

# Calculate the mean of the mean values
overall_mean_cod <- mean(cod_mean_temporal2)

# Print the overall mean
print(overall_mean_cod)

# Calculate the mean for saithe
saithe_mean_temporal2 <- numeric(length(year_ranges))

# Loop through each range of years
for (i in seq_along(year_ranges)) {
  # Subset the data for the current range of years and calculate the mean
  subset_data <- saithe_data_temporal2[year_ranges[[i]]]
  saithe_mean_temporal2[i] <- mean(subset_data)
}

# Calculate the overall mean for saithe
overall_mean_saithe <- mean(saithe_mean_temporal2)

# Print the overall mean for saithe
print(overall_mean_saithe)


haddock_mean_temporal2 <- numeric(length(year_ranges))

# Loop through each range of years
for (i in seq_along(year_ranges)) {
  # Subset the data for the current range of years and calculate the mean
  subset_data <- haddock_data_temporal2[year_ranges[[i]]]
  haddock_mean_temporal2[i] <- mean(subset_data)
}

# Calculate the overall mean for haddock
overall_mean_haddock <- mean(haddock_mean_temporal2)

# Print the overall mean for haddock
print(overall_mean_haddock)

# Calculate the mean for plaice
plaice_mean_temporal2 <- numeric(length(year_ranges))

# Loop through each range of years
for (i in seq_along(year_ranges)) {
  # Subset the data for the current range of years and calculate the mean
  subset_data <- plaice_data_temporal2[year_ranges[[i]]]
  plaice_mean_temporal2[i] <- mean(subset_data)
}

# Calculate the overall mean for plaice
overall_mean_plaice <- mean(plaice_mean_temporal2)

# Print the overall mean for plaice
print(overall_mean_plaice)


# Calculate the mean for gurnard
gurnard_mean_temporal2 <- numeric(length(year_ranges))

# Loop through each range of years
for (i in seq_along(year_ranges)) {
  # Subset the data for the current range of years and calculate the mean
  subset_data <- gurnard_data_temporal2[year_ranges[[i]]]
  gurnard_mean_temporal2[i] <- mean(subset_data)
}

# Calculate the overall mean for gurnard
overall_mean_gurnard <- mean(gurnard_mean_temporal2)

# Print the overall mean for gurnard
print(overall_mean_gurnard)

# Calculate the mean for sole
sole_mean_temporal2 <- numeric(length(year_ranges))

# Loop through each range of years
for (i in seq_along(year_ranges)) {
  # Subset the data for the current range of years and calculate the mean
  subset_data <- sole_data_temporal2[year_ranges[[i]]]
  sole_mean_temporal2[i] <- mean(subset_data)
}

# Calculate the overall mean for sole
overall_mean_sole <- mean(sole_mean_temporal2)

# Print the overall mean for sole
print(overall_mean_sole)



# Calculate the mean for whiting
whiting_mean_temporal2 <- numeric(length(year_ranges))

# Loop through each range of years
for (i in seq_along(year_ranges)) {
  # Subset the data for the current range of years and calculate the mean
  subset_data <- whiting_data_temporal2[year_ranges[[i]]]
  whiting_mean_temporal2[i] <- mean(subset_data)
}

# Calculate the overall mean for whiting
overall_mean_whiting <- mean(whiting_mean_temporal2)

# Print the overall mean for whiting
print(overall_mean_whiting)

# Calculate the mean for dab
dab_mean_temporal2 <- numeric(length(year_ranges))

# Loop through each range of years
for (i in seq_along(year_ranges)) {
  # Subset the data for the current range of years and calculate the mean
  subset_data <- dab_data_temporal2[year_ranges[[i]]]
  dab_mean_temporal2[i] <- mean(subset_data)
}

# Calculate the overall mean for dab
overall_mean_dab <- mean(dab_mean_temporal2)

# Print the overall mean for dab
print(overall_mean_dab)


# Calculate the mean for herring
herring_mean_temporal2 <- numeric(length(year_ranges))

# Loop through each range of years
for (i in seq_along(year_ranges)) {
  # Subset the data for the current range of years and calculate the mean
  subset_data <- herring_data_temporal2[year_ranges[[i]]]
  herring_mean_temporal2[i] <- mean(subset_data)
}

# Calculate the overall mean for herring
overall_mean_herring <- mean(herring_mean_temporal2)

# Print the overall mean for herring
print(overall_mean_herring)


# Calculate the mean for npout
npout_mean_temporal2 <- numeric(length(year_ranges))

# Loop through each range of years
for (i in seq_along(year_ranges)) {
  # Subset the data for the current range of years and calculate the mean
  subset_data <- npout_data_temporal2[year_ranges[[i]]]
  npout_mean_temporal2[i] <- mean(subset_data)
}

# Calculate the overall mean for npout
overall_mean_npout <- mean(npout_mean_temporal2)

# Print the overall mean for npout
print(overall_mean_npout)


# Calculate the mean for sandeel
sandeel_mean_temporal2 <- numeric(length(year_ranges))

# Loop through each range of years
for (i in seq_along(year_ranges)) {
  # Subset the data for the current range of years and calculate the mean
  subset_data <- sandeel_data_temporal2[year_ranges[[i]]]
  sandeel_mean_temporal2[i] <- mean(subset_data)
}

# Calculate the overall mean for sandeel
overall_mean_sandeel <- mean(sandeel_mean_temporal2)

# Print the overall mean for sandeel
print(overall_mean_sandeel)


# Calculate the mean for sprat
sprat_mean_temporal2 <- numeric(length(year_ranges))

# Loop through each range of years
for (i in seq_along(year_ranges)) {
  # Subset the data for the current range of years and calculate the mean
  subset_data <- sprat_data_temporal2[year_ranges[[i]]]
  sprat_mean_temporal2[i] <- mean(subset_data)
}

# Calculate the overall mean for sprat
overall_mean_sprat <- mean(sprat_mean_temporal2)

# Print the overall mean for sprat
print(overall_mean_sprat)

# Create a data frame to store the mean yields
mean_yield_temporal2 <- data.frame(
  Species = c("Cod", "Saithe", "Haddock", "Plaice", "Gurnard", "Sole", 
              "Whiting", "Dab", "Herring", "N.pout", "Sandeel", "Sprat"),
  Total_Yield_temporal2 = c(overall_mean_cod, overall_mean_saithe, overall_mean_haddock, overall_mean_plaice,
                            overall_mean_gurnard, overall_mean_sole, overall_mean_whiting, overall_mean_dab,
                            overall_mean_herring, overall_mean_npout, overall_mean_sandeel, overall_mean_sprat)
)

# Print the data frame
print(mean_yield_temporal2)


###
# Calculate yield for the temporal harvest double scenario
Temporal_harvest_double_yield <- getYield(sim_params_temporal_harvest_double)

# Total yield between year 17 and 52 for each species
# Cod
cod_data_temporal_double2 <- Temporal_harvest_double_yield[16:50, species_code_cod]
cod_sum_temporal_double2 <- sum(cod_data_temporal_double2)


# Saithe
saithe_data_temporal_double2 <- Temporal_harvest_double_yield[16:50, species_code_saithe]
saithe_sum_temporal_double2 <- sum(saithe_data_temporal_double2)


# Haddock
haddock_data_temporal_double2 <- Temporal_harvest_double_yield[16:50, species_code_haddock]
haddock_sum_temporal_double2 <- sum(haddock_data_temporal_double2)


# Plaice
plaice_data_temporal_double2 <- Temporal_harvest_double_yield[16:50, species_code_plaice]
plaice_sum_temporal_double2 <- sum(plaice_data_temporal_double2)


# Gurnard
gurnard_data_temporal_double2 <- Temporal_harvest_double_yield[16:50, species_code_gurnard]
gurnard_sum_temporal_double2 <- sum(gurnard_data_temporal_double2)


# Sole
sole_data_temporal_double2 <- Temporal_harvest_double_yield[16:50, species_code_sole]
sole_sum_temporal_double2 <- sum(sole_data_temporal_double2)


# Whiting
whiting_data_temporal_double2 <- Temporal_harvest_double_yield[16:50, species_code_whiting]
whiting_sum_temporal_double2 <- sum(whiting_data_temporal_double2)


# Dab
dab_data_temporal_double2 <- Temporal_harvest_double_yield[16:50, species_code_dab]
dab_sum_temporal_double2 <- sum(dab_data_temporal_double2)


# Herring
herring_data_temporal_double2 <- Temporal_harvest_double_yield[16:50, species_code_herring]
herring_sum_temporal_double2 <- sum(herring_data_temporal_double2)


# N.pout
npout_data_temporal_double2 <- Temporal_harvest_double_yield[16:50, species_code_n.pout]
npout_sum_temporal_double2 <- sum(npout_data_temporal_double2)


# Sandeel
sandeel_data_temporal_double2 <- Temporal_harvest_double_yield[16:50, species_code_sandeel]
sandeel_sum_temporal_double2 <- sum(sandeel_data_temporal_double2)


# Sprat
sprat_data_temporal_double2 <- Temporal_harvest_double_yield[16:50, species_code_sprat]
sprat_sum_temporal_double2 <- sum(sprat_data_temporal_double2)
sprat_mean_temporal_double2 <- mean(sprat_data_temporal_double2)




# Initialize an empty vector to store the means
cod_mean_temporal_double2 <- numeric(length(year_ranges))

# Loop through each range of years
for (i in seq_along(year_ranges)) {
  # Subset the data for the current range of years and calculate the mean
  subset_data <- cod_data_temporal_double2[year_ranges[[i]]]
  cod_mean_temporal_double2[i] <- mean(subset_data)
}

# Calculate the mean of the mean values
overall_mean_cod_double <- mean(cod_mean_temporal_double2)

# Print the overall mean
print(overall_mean_cod_double)



# Initialize an empty vector to store the means
saithe_mean_temporal_double2 <- numeric(length(year_ranges))

# Loop through each range of years
for (i in seq_along(year_ranges)) {
  # Subset the data for the current range of years and calculate the mean
  subset_data <- saithe_data_temporal_double2[year_ranges[[i]]]
  saithe_mean_temporal_double2[i] <- mean(subset_data)
}

# Calculate the mean of the mean values
overall_mean_saithe_double <- mean(saithe_mean_temporal_double2)

# Print the overall mean
print(overall_mean_saithe_double)


# Initialize an empty vector to store the means
haddock_mean_temporal_double2 <- numeric(length(year_ranges))

# Loop through each range of years
for (i in seq_along(year_ranges)) {
  # Subset the data for the current range of years and calculate the mean
  subset_data <- haddock_data_temporal_double2[year_ranges[[i]]]
  haddock_mean_temporal_double2[i] <- mean(subset_data)
}

# Calculate the mean of the mean values
overall_mean_haddock_double <- mean(haddock_mean_temporal_double2)

# Print the overall mean
print(overall_mean_haddock_double)



# Initialize an empty vector to store the means
plaice_mean_temporal_double2 <- numeric(length(year_ranges))

# Loop through each range of years
for (i in seq_along(year_ranges)) {
  # Subset the data for the current range of years and calculate the mean
  subset_data <- plaice_data_temporal_double2[year_ranges[[i]]]
  plaice_mean_temporal_double2[i] <- mean(subset_data)
}

# Calculate the mean of the mean values
overall_mean_plaice_double <- mean(plaice_mean_temporal_double2)

# Print the overall mean
print(overall_mean_plaice_double)


# Initialize an empty vector to store the means
gurnard_mean_temporal_double2 <- numeric(length(year_ranges))

# Loop through each range of years
for (i in seq_along(year_ranges)) {
  # Subset the data for the current range of years and calculate the mean
  subset_data <- gurnard_data_temporal_double2[year_ranges[[i]]]
  gurnard_mean_temporal_double2[i] <- mean(subset_data)
}

# Calculate the mean of the mean values
overall_mean_gurnard_double <- mean(gurnard_mean_temporal_double2)

# Print the overall mean
print(overall_mean_gurnard_double)



# Initialize an empty vector to store the means
sole_mean_temporal_double2 <- numeric(length(year_ranges))

# Loop through each range of years
for (i in seq_along(year_ranges)) {
  # Subset the data for the current range of years and calculate the mean
  subset_data <- sole_data_temporal_double2[year_ranges[[i]]]
  sole_mean_temporal_double2[i] <- mean(subset_data)
}

# Calculate the mean of the mean values
overall_mean_sole_double <- mean(sole_mean_temporal_double2)

# Print the overall mean
print(overall_mean_sole_double)



# Initialize an empty vector to store the means
whiting_mean_temporal_double2 <- numeric(length(year_ranges))

# Loop through each range of years
for (i in seq_along(year_ranges)) {
  # Subset the data for the current range of years and calculate the mean
  subset_data <- whiting_data_temporal_double2[year_ranges[[i]]]
  whiting_mean_temporal_double2[i] <- mean(subset_data)
}

# Calculate the mean of the mean values
overall_mean_whiting_double <- mean(whiting_mean_temporal_double2)

# Print the overall mean
print(overall_mean_whiting_double)


# Initialize an empty vector to store the means
dab_mean_temporal_double2 <- numeric(length(year_ranges))

# Loop through each range of years
for (i in seq_along(year_ranges)) {
  # Subset the data for the current range of years and calculate the mean
  subset_data <- dab_data_temporal_double2[year_ranges[[i]]]
  dab_mean_temporal_double2[i] <- mean(subset_data)
}

# Calculate the mean of the mean values
overall_mean_dab_double <- mean(dab_mean_temporal_double2)

# Print the overall mean
print(overall_mean_dab_double)




# Initialize an empty vector to store the means
herring_mean_temporal_double2 <- numeric(length(year_ranges))

# Loop through each range of years
for (i in seq_along(year_ranges)) {
  # Subset the data for the current range of years and calculate the mean
  subset_data <- herring_data_temporal_double2[year_ranges[[i]]]
  herring_mean_temporal_double2[i] <- mean(subset_data)
}

# Calculate the mean of the mean values
overall_mean_herring_double <- mean(herring_mean_temporal_double2)

# Print the overall mean
print(overall_mean_herring_double)




# Initialize an empty vector to store the means
npout_mean_temporal_double2 <- numeric(length(year_ranges))

# Loop through each range of years
for (i in seq_along(year_ranges)) {
  # Subset the data for the current range of years and calculate the mean
  subset_data <- npout_data_temporal_double2[year_ranges[[i]]]
  npout_mean_temporal_double2[i] <- mean(subset_data)
}

# Calculate the mean of the mean values
overall_mean_npout_double <- mean(npout_mean_temporal_double2)

# Print the overall mean
print(overall_mean_npout_double)


# Initialize an empty vector to store the means
sandeel_mean_temporal_double2 <- numeric(length(year_ranges))

# Loop through each range of years
for (i in seq_along(year_ranges)) {
  # Subset the data for the current range of years and calculate the mean
  subset_data <- sandeel_data_temporal_double2[year_ranges[[i]]]
  sandeel_mean_temporal_double2[i] <- mean(subset_data)
}

# Calculate the mean of the mean values
overall_mean_sandeel_double <- mean(sandeel_mean_temporal_double2)

# Print the overall mean
print(overall_mean_sandeel_double)



# Initialize an empty vector to store the means
sprat_mean_temporal_double2 <- numeric(length(year_ranges))

# Loop through each range of years
for (i in seq_along(year_ranges)) {
  # Subset the data for the current range of years and calculate the mean
  subset_data <- sprat_data_temporal_double2[year_ranges[[i]]]
  sprat_mean_temporal_double2[i] <- mean(subset_data)
}

# Calculate the mean of the mean values
overall_mean_sprat_double <- mean(sprat_mean_temporal_double2)

# Print the overall mean
print(overall_mean_sprat_double)




# Create a dataframe to store the results
results_yield_temporal_double2 <- data.frame(
  Species = c("Cod", "Saithe", "Haddock", "Plaice", "Gurnard", "Sole", 
              "Whiting", "Dab", "Herring", "N.pout", "Sandeel", "Sprat"),
  Total_Yield_temporal_double2 = c(cod_sum_temporal_double2, saithe_sum_temporal_double2, haddock_sum_temporal_double2, plaice_sum_temporal_double2, gurnard_sum_temporal_double2,
                                   sole_sum_temporal_double2, whiting_sum_temporal_double2, dab_sum_temporal_double2, herring_sum_temporal_double2, npout_sum_temporal_double2,
                                   sandeel_sum_temporal_double2, sprat_sum_temporal_double2)
)

mean_yield_temporal_double2 <- data.frame(
  Species = c("Cod", "Saithe", "Haddock", "Plaice", "Gurnard", "Sole", 
              "Whiting", "Dab", "Herring", "N.pout", "Sandeel", "Sprat"),
  Total_Yield_temporal_double2 = c(overall_mean_cod_double, overall_mean_saithe_double, overall_mean_haddock_double, overall_mean_plaice_double, overall_mean_gurnard_double,
                                   overall_mean_sole_double, overall_mean_whiting_double, overall_mean_dab_double, overall_mean_herring_double, overall_mean_npout_double,
                                   overall_mean_sandeel_double, overall_mean_sprat_double)
)
results_yield_temporal_double2
mean_yield_temporal_double2



### Results of yield ###

#Comparison of Yield for Each Species 17-52 years
results_yield_temporal2
results_yield_temporal_double2
results_yield_constant2

# Update results_yield_temporal2 data frame
results_yield_temporal2$Total_Yield_temporal2 <- c(1.111272e+13, 7.646452e+12, 3.558627e+12, 7.797930e+12, 
                                                   2.834566e+11, 5.133025e+11, 7.599808e+11, 5.592043e+10, 
                                                   3.505911e+12, 1.441549e+12, 2.626494e+13, 4.213775e+11)

# Update results_yield_temporal_double2 data frame
results_yield_temporal_double2$Total_Yield_temporal_double2 <- c(1.726223e+13, 1.302894e+13, 6.642721e+12, 
                                                                 9.140335e+12, 5.450181e+11, 1.055298e+12, 
                                                                 1.439067e+12, 1.141940e+11, 6.418686e+12, 
                                                                 2.476114e+12, 3.896742e+13, 1.125854e+12)

# Update results_yield_constant2 data frame
results_yield_constant2$Total_Yield <- c(1.196352e+13, 1.090003e+13, 7.232288e+12, 1.688815e+13, 
                                         3.833233e+11, 9.192288e+11, 1.623709e+12, 7.247408e+10, 
                                         4.725909e+12, 2.146848e+12, 2.636662e+13, 4.005697e+11)

# Create a data frame with the species and total yield data
# Create combined results v3 data frame
combined_results_v6 <- data.frame(
  Species = c("Cod", "Saithe", "Haddock", "Plaice", "Gurnard", "Sole", "Whiting", "Dab", "Herring", "N.pout", "Sandeel", "Sprat"),
  Total_Yield_Temporal2 = c(1.111272e+13, 7.646452e+12, 3.558627e+12, 7.797930e+12, 
                            2.834566e+11, 5.133025e+11, 7.599808e+11, 5.592043e+10, 
                            3.505911e+12, 1.441549e+12, 2.626494e+13, 4.213775e+11),
  Total_Yield_Temporal_Double2 = c(1.726223e+13, 1.302894e+13, 6.642721e+12, 
                                   9.140335e+12, 5.450181e+11, 1.055298e+12, 
                                   1.439067e+12, 1.141940e+11, 6.418686e+12, 
                                   2.476114e+12, 3.896742e+13, 1.125854e+12),
  Total_Yield_Constant2 = c(1.196352e+13, 1.090003e+13, 7.232288e+12, 1.688815e+13, 
                            3.833233e+11, 9.192288e+11, 1.623709e+12, 7.247408e+10, 
                            4.725909e+12, 2.146848e+12, 2.636662e+13, 4.005697e+11)
)


# Write the combined results v3 data frame to a CSV file
write.csv(combined_results_v6, "combined_results_v6.csv", row.names = FALSE)

combined_results_v6


# Load necessary libraries
library(ggplot2)
library(reshape2)

# Reshape the data frame for plotting
combined_results_melted <- melt(combined_results_v6, id.vars = "Species", variable.name = "Scenario", value.name = "Total_Yield")
combined_results_melted


# Plotting total yields across different fishing scenarios per species, from 17 to 52 years #

# Stat test?? #

# 
library(ggplot2)

# Your original ggplot code
ggplot(combined_results_melted, aes(x = Species, y = Total_Yield, fill = Scenario)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "",
       x = "Species",
       y = "Total Yield (g/year)") +
  scale_fill_manual(name = "Scenario", 
                    values = c("Total_Yield_Temporal2" = "#619CFF", "Total_Yield_Temporal_Double2" = "#00BA38", "Total_Yield_Constant2" = "#F8766D"),
                    labels = c("Total_Yield_Temporal2" = "Periodic Harvest", "Total_Yield_Temporal_Double2" = "Periodic Harvest Double Fishing Intensity", "Total_Yield_Constant2" = "Constant Harvest")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.title.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12)) +
  geom_text(aes(label = sprintf("%.2e", Total_Yield)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 4,  # Increased font size
            angle = 90,
            hjust = 0.5,
            color = "black")  # Changed text color



# Plot mean yield of total yield from 17 - 52 years. Stat test it and plot Standard Error # 

mean_yield_temporal_double2
mean_yield_temporal2
mean_yield_constant2

# Merge the data frames
# Creating data frames
# Mean yield for each species across temporal data sets
mean_yield_cod <- c(8.631116e+11, 5.556359e+11, 341701921303)
mean_yield_saithe <- c(6.514470e+11, 3.823226e+11, 311606294075)
mean_yield_haddock <- c(3.321361e+11, 1.779314e+11, 206647509055)
mean_yield_plaice <- c(4.570168e+11, 3.898965e+11, 478606986599)
mean_yield_gurnard <- c(2.725090e+10, 1.417283e+10, 10983764898)
mean_yield_sole <- c(5.276490e+10, 2.566512e+10, 26869112581)
mean_yield_whiting <- c(7.195333e+10, 3.799904e+10, 46435866843)
mean_yield_dab <- c(5.709699e+09, 2.796022e+09, 2108058276)
mean_yield_herring <- c(3.209343e+11, 1.752956e+11, 135748133966)
mean_yield_npout <- c(1.238057e+11, 7.207745e+10, 61322289940)
mean_yield_sandeel <- c(1.948371e+12, 1.313247e+12, 755352110725)
mean_yield_sprat <- c(5.629270e+10, 2.106888e+10, 11720350828)

# Create the data frame
combined_means_v3 <- data.frame(
  Species = c("Cod", "Saithe", "Haddock", "Plaice", "Gurnard", "Sole", "Whiting", "Dab", "Herring", "N.pout", "Sandeel", "Sprat"),
  Total_Yield_Temporal2 = c(mean_yield_cod[2], mean_yield_saithe[2], mean_yield_haddock[2], mean_yield_plaice[2], mean_yield_gurnard[2], mean_yield_sole[2], mean_yield_whiting[2], mean_yield_dab[2], mean_yield_herring[2], mean_yield_npout[2], mean_yield_sandeel[2], mean_yield_sprat[2]),
  Total_Yield_Temporal_Double2 = c(mean_yield_cod[1], mean_yield_saithe[1], mean_yield_haddock[1], mean_yield_plaice[1], mean_yield_gurnard[1], mean_yield_sole[1], mean_yield_whiting[1], mean_yield_dab[1], mean_yield_herring[1], mean_yield_npout[1], mean_yield_sandeel[1], mean_yield_sprat[1]),
  Total_Yield_Constant2 = c(mean_yield_cod[3], mean_yield_saithe[3], mean_yield_haddock[3], mean_yield_plaice[3], mean_yield_gurnard[3], mean_yield_sole[3], mean_yield_whiting[3], mean_yield_dab[3], mean_yield_herring[3], mean_yield_npout[3], mean_yield_sandeel[3], mean_yield_sprat[3])
)

# Print the combined data frame
print(combined_means_v3)


combined_means_v3



# Write the combined results to a CSV file
write.csv(combined_means_v3, file = "combined_means_v3.csv", row.names = FALSE)

# Melt data
combined_means_melted <- melt(combined_means_v3, id.vars = "Species", variable.name = "Scenario", value.name = "Total_Yield")
combined_means_melted

# Plot mean yield 
ggplot(combined_means_melted, aes(x = Species, y = Total_Yield, fill = Scenario)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = sprintf("%.2e", Total_Yield)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 4,  # Increased font size
            angle = 90,
            hjust = 0.5,
            color = "black") +  # Changed text color
  labs(title = "",
       x = "Species",
       y = "Mean Total Yield (g/year)") +
  scale_fill_manual(name = "Scenario", 
                    values = c("Total_Yield_Temporal2" = "#619CFF", "Total_Yield_Temporal_Double2" = "#00BA38", "Total_Yield_Constant2" = "#F8766D"),
                    labels = c("Total_Yield_Temporal2" = "Periodic Harvest", "Total_Yield_Temporal_Double2" = "Periodic Harvest Double Fishing Intensity", "Total_Yield_Constant2" = "Constant Harvest")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.title.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12))




####
# Calculate the sum of total yield for each scenario
sum_total_yield <- aggregate(Total_Yield ~ Scenario, data = combined_results_melted, FUN = sum)


library(ggplot2)
# bar chart
ggplot(sum_total_yield, aes(x = Scenario, y = Total_Yield)) +
  geom_bar(stat = "identity", fill = c("#619CFF", "#00BA38", "#F8766D")) +
  geom_text(data = sum_total_yield, aes(x = Scenario, y = Total_Yield, 
                                        label = sprintf("%.2e", Total_Yield)), 
            vjust = -0.5, size = 4, color = "black") +  # Add numeric labels for total values
  labs(title = "",
       x = "Scenario",
       y = "Total Yield (g/year)") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),  # Increase x-axis label size
        axis.title.x = element_text(size = 12),  # Increase x-axis title size
        legend.position = "none") +
  scale_x_discrete(labels = c("Periodic Harvest", "Periodic Harvest Double Fishing Intensity", "Constant Harvest"))  # Specify your x-axis labels here





# Extracting species specific yield data for plots

# Matrix of yields

yield_data_constant <- getYield(sim_params_constant_harvest)
yield_data_temporal <- getYield(sim_params_temporal_harvest)
yield_data_temporal_double <- getYield(sim_params_temporal_harvest_double)

# species code
species_code_cod <- "Cod" # done
species_code_saithe <- "Saithe" # done
species_code_haddock <- "Haddock" # Done
species_code_plaice <- "Plaice" # done
species_code_gurnard <- "Gurnard" # done
species_code_sole <- "Sole" #done
species_code_whiting <- "Whiting" # done
species_code_dab <- "Dab" # done
species_code_herring <- "Herring" # done
species_code_n.pout <- "N.pout" #done
species_code_sandeel <- "Sandeel"
species_code_sprat <- "Sprat"



library(ggplot2)
library(scales)  # For scientific notation formatting

# Extract data for cod
cod_data_constant <- yield_data_constant[0:50, species_code_cod]
cod_data_temporal <- yield_data_temporal[0:50, species_code_cod]
cod_data_temporal_double <- yield_data_temporal_double[0:50, species_code_cod]

# Define the years variable
years <- 0:49  # Adjusted to include 50 years

# Create a data frame for cod
cod_df <- data.frame(
  years = years,
  constant_yield = cod_data_constant,
  temporal_yield = cod_data_temporal,
  temporal_double_yield = cod_data_temporal_double
)

# Calculate the maximum value for the y-axis
max_y_cod <- max(max(cod_df$constant_yield), max(cod_df$temporal_yield), max(cod_df$temporal_double_yield))

# Calculate the breaks for the y-axis
y_breaks_cod <- seq(0, max_y_cod, length.out = 25)

# Plotting
ggplot(cod_df, aes(x = years)) +
  geom_line(aes(y = constant_yield, color = "Constant"), size = 1) +
  geom_line(aes(y = temporal_yield, color = "Temporal"), size = 1) +
  geom_line(aes(y = temporal_double_yield, color = "Temporal Double"), size = 1) +
  labs(x = "Years", y = "Yield of Cod (g / year)", color = "Legend") +
  scale_x_continuous(breaks = seq(0, 49, by = 1), labels = seq(0, 49, by = 1)) +  # Manually set breaks and labels for x-axis
  scale_y_continuous(breaks = y_breaks_cod, labels = scientific_format()) +  # Set the breaks for the y-axis and scientific notation
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "grey", linetype = "dotted"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12),  
        axis.text.y = element_text(size = 12),  
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14),  
        legend.title = element_text(size = 14),  
        legend.text = element_text(size = 12),  
        plot.title = element_text(size = 16)) +  
  ggtitle("") +
  scale_color_manual(values = c("Constant" = "#F8766D", "Temporal" = "#00BA38", "Temporal Double" = "#619CFF"),
                     labels = c("Constant harvest", "Periodic Harvest", "Periodic Harvest Double Intensity"))


library(ggplot2)
library(scales)  # For scientific notation formatting

# Extract data for saithe
saithe_data_constant <- yield_data_constant[0:50, species_code_saithe]
saithe_data_temporal <- yield_data_temporal[0:50, species_code_saithe]
saithe_data_temporal_double <- yield_data_temporal_double[0:50, species_code_saithe]

# Define the years variable
years <- 0:49  # Adjusted to include 50 years

# Create a data frame for saithe
saithe_df <- data.frame(
  years = years,
  constant_yield = saithe_data_constant,
  temporal_yield = saithe_data_temporal,
  temporal_double_yield = saithe_data_temporal_double
)

# Calculate the maximum value for the y-axis
max_y_saithe <- max(max(saithe_df$constant_yield), max(saithe_df$temporal_yield), max(saithe_df$temporal_double_yield))

# Calculate the breaks for the y-axis
y_breaks_saithe <- seq(0, max_y_saithe, length.out = 25)

# Plotting
ggplot(saithe_df, aes(x = years)) +
  geom_line(aes(y = constant_yield, color = "Constant"), size = 1) +
  geom_line(aes(y = temporal_yield, color = "Temporal"), size = 1) +
  geom_line(aes(y = temporal_double_yield, color = "Temporal Double"), size = 1) +
  labs(x = "Years", y = "Yield of Saithe (g / year)", color = "Legend") +
  scale_x_continuous(breaks = seq(0, 49, by = 1), labels = seq(0, 49, by = 1)) +  # Manually set breaks and labels for x-axis
  scale_y_continuous(breaks = y_breaks_saithe, labels = scientific_format()) +  # Set the breaks for the y-axis and scientific notation
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "grey", linetype = "dotted"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12),  
        axis.text.y = element_text(size = 12),  
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14),  
        legend.title = element_text(size = 14),  
        legend.text = element_text(size = 12),  
        plot.title = element_text(size = 16)) +  
  ggtitle("") +
  scale_color_manual(values = c("Constant" = "#F8766D", "Temporal" = "#00BA38", "Temporal Double" = "#619CFF"),
                     labels = c("Constant harvest", "Periodic Harvest", "Periodic Harvest Double Intensity"))


library(ggplot2)
library(scales)  # For scientific notation formatting

# Extract data for haddock
haddock_data_constant <- yield_data_constant[0:50, species_code_haddock]
haddock_data_temporal <- yield_data_temporal[0:50, species_code_haddock]
haddock_data_temporal_double <- yield_data_temporal_double[0:50, species_code_haddock]

# Define the years variable
years <- 0:49  # Adjusted to include 50 years

# Create a data frame for haddock
haddock_df <- data.frame(
  years = years,
  constant_yield = haddock_data_constant,
  temporal_yield = haddock_data_temporal,
  temporal_double_yield = haddock_data_temporal_double
)

# Calculate the maximum value for the y-axis
max_y <- max(max(haddock_df$constant_yield), max(haddock_df$temporal_yield), max(haddock_df$temporal_double_yield))

# Calculate the breaks for the y-axis
y_breaks <- seq(0, max_y, length.out = 25)

# Plotting
ggplot(haddock_df, aes(x = years)) +
  geom_line(aes(y = constant_yield, color = "Constant"), size = 1) +
  geom_line(aes(y = temporal_yield, color = "Temporal"), size = 1) +
  geom_line(aes(y = temporal_double_yield, color = "Temporal Double"), size = 1) +
  labs(x = "Years", y = "Yield of Haddock (g / year)", color = "Legend") +
  scale_x_continuous(breaks = seq(0, 49, by = 1), labels = seq(0, 49, by = 1)) +  # Manually set breaks and labels for x-axis
  scale_y_continuous(breaks = y_breaks, labels = scientific_format()) +  # Set the breaks for the y-axis and scientific notation
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "grey", linetype = "dotted"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12),  
        axis.text.y = element_text(size = 12),  
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14),  
        legend.title = element_text(size = 14),  
        legend.text = element_text(size = 12),  
        plot.title = element_text(size = 16)) +  
  ggtitle("") +
  scale_color_manual(values = c("Constant" = "#F8766D", "Temporal" = "#00BA38", "Temporal Double" = "#619CFF"),
                     labels = c("Constant harvest", "Periodic Harvest", "Periodic Harvest Double Intensity"))




library(ggplot2)
library(scales)  # For scientific notation formatting

# Extract data for plaice
plaice_data_constant <- yield_data_constant[0:50, species_code_plaice]
plaice_data_temporal <- yield_data_temporal[0:50, species_code_plaice]
plaice_data_temporal_double <- yield_data_temporal_double[0:50, species_code_plaice]

# Define the years variable
years <- 0:49  # Adjusted to include 50 years

# Create a data frame for plaice
plaice_df <- data.frame(
  years = years,
  constant_yield = plaice_data_constant,
  temporal_yield = plaice_data_temporal,
  temporal_double_yield = plaice_data_temporal_double
)

# Calculate the maximum value for the y-axis
max_y_plaice <- max(max(plaice_df$constant_yield), max(plaice_df$temporal_yield), max(plaice_df$temporal_double_yield))

# Calculate the breaks for the y-axis
y_breaks_plaice <- seq(0, max_y_plaice, length.out = 25)

# Plotting
ggplot(plaice_df, aes(x = years)) +
  geom_line(aes(y = constant_yield, color = "Constant"), size = 1) +
  geom_line(aes(y = temporal_yield, color = "Temporal"), size = 1) +
  geom_line(aes(y = temporal_double_yield, color = "Temporal Double"), size = 1) +
  labs(x = "Years", y = "Yield of Plaice (g / year)", color = "Legend") +
  scale_x_continuous(breaks = seq(0, 49, by = 1), labels = seq(0, 49, by = 1)) +  # Manually set breaks and labels for x-axis
  scale_y_continuous(breaks = y_breaks_plaice, labels = scientific_format()) +  # Set the breaks for the y-axis and scientific notation
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "grey", linetype = "dotted"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12),  
        axis.text.y = element_text(size = 12),  
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14),  
        legend.title = element_text(size = 14),  
        legend.text = element_text(size = 12),  
        plot.title = element_text(size = 16)) +  
  ggtitle("") +
  scale_color_manual(values = c("Constant" = "#F8766D", "Temporal" = "#00BA38", "Temporal Double" = "#619CFF"),
                     labels = c("Constant harvest", "Periodic Harvest", "Periodic Harvest Double Intensity"))



library(ggplot2)
library(scales)  # For scientific notation formatting

# Extract data for gurnard
gurnard_data_constant <- yield_data_constant[0:50, species_code_gurnard]
gurnard_data_temporal <- yield_data_temporal[0:50, species_code_gurnard]
gurnard_data_temporal_double <- yield_data_temporal_double[0:50, species_code_gurnard]

# Define the years variable
years <- 0:49  # Adjusted to include 50 years

# Create a data frame for gurnard
gurnard_df <- data.frame(
  years = years,
  constant_yield = gurnard_data_constant,
  temporal_yield = gurnard_data_temporal,
  temporal_double_yield = gurnard_data_temporal_double
)

# Calculate the maximum value for the y-axis
max_y_gurnard <- max(max(gurnard_df$constant_yield), max(gurnard_df$temporal_yield), max(gurnard_df$temporal_double_yield))

# Calculate the breaks for the y-axis
y_breaks_gurnard <- seq(0, max_y_gurnard, length.out = 25)

# Plotting
ggplot(gurnard_df, aes(x = years)) +
  geom_line(aes(y = constant_yield, color = "Constant"), size = 1) +
  geom_line(aes(y = temporal_yield, color = "Temporal"), size = 1) +
  geom_line(aes(y = temporal_double_yield, color = "Temporal Double"), size = 1) +
  labs(x = "Years", y = "Yield of Gurnard (g / year)", color = "Legend") +
  scale_x_continuous(breaks = seq(0, 49, by = 1), labels = seq(0, 49, by = 1)) +  # Manually set breaks and labels for x-axis
  scale_y_continuous(breaks = y_breaks_gurnard, labels = scientific_format()) +  # Set the breaks for the y-axis and scientific notation
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "grey", linetype = "dotted"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12),  
        axis.text.y = element_text(size = 12),  
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14),  
        legend.title = element_text(size = 14),  
        legend.text = element_text(size = 12),  
        plot.title = element_text(size = 16)) +  
  ggtitle("") +
  scale_color_manual(values = c("Constant" = "#F8766D", "Temporal" = "#00BA38", "Temporal Double" = "#619CFF"),
                     labels = c("Constant harvest", "Periodic Harvest", "Periodic Harvest Double Intensity"))



library(ggplot2)
library(scales)  # For scientific notation formatting

# Extract data for sole
sole_data_constant <- yield_data_constant[0:50, species_code_sole]
sole_data_temporal <- yield_data_temporal[0:50, species_code_sole]
sole_data_temporal_double <- yield_data_temporal_double[0:50, species_code_sole]

# Define the years variable
years <- 0:49  # Adjusted to include 50 years

# Create a data frame for sole
sole_df <- data.frame(
  years = years,
  constant_yield = sole_data_constant,
  temporal_yield = sole_data_temporal,
  temporal_double_yield = sole_data_temporal_double
)

# Calculate the maximum value for the y-axis
max_y_sole <- max(max(sole_df$constant_yield), max(sole_df$temporal_yield), max(sole_df$temporal_double_yield))

# Calculate the breaks for the y-axis
y_breaks_sole <- seq(0, max_y_sole, length.out = 25)

# Plotting
ggplot(sole_df, aes(x = years)) +
  geom_line(aes(y = constant_yield, color = "Constant"), size = 1) +
  geom_line(aes(y = temporal_yield, color = "Temporal"), size = 1) +
  geom_line(aes(y = temporal_double_yield, color = "Temporal Double"), size = 1) +
  labs(x = "Years", y = "Yield of Sole (g / year)", color = "Legend") +
  scale_x_continuous(breaks = seq(0, 49, by = 1), labels = seq(0, 49, by = 1)) +  # Manually set breaks and labels for x-axis
  scale_y_continuous(breaks = y_breaks_sole, labels = scientific_format()) +  # Set the breaks for the y-axis and scientific notation
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "grey", linetype = "dotted"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12),  
        axis.text.y = element_text(size = 12),  
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14),  
        legend.title = element_text(size = 14),  
        legend.text = element_text(size = 12),  
        plot.title = element_text(size = 16)) +  
  ggtitle("") +
  scale_color_manual(values = c("Constant" = "#F8766D", "Temporal" = "#00BA38", "Temporal Double" = "#619CFF"),
                     labels = c("Constant harvest", "Periodic Harvest", "Periodic Harvest Double Intensity"))




library(ggplot2)
library(scales)  # For scientific notation formatting

# Extract data for whiting
whiting_data_constant <- yield_data_constant[0:50, species_code_whiting]
whiting_data_temporal <- yield_data_temporal[0:50, species_code_whiting]
whiting_data_temporal_double <- yield_data_temporal_double[0:50, species_code_whiting]

# Define the years variable
years <- 0:49  # Adjusted to include 50 years

# Create a data frame for whiting
whiting_df <- data.frame(
  years = years,
  constant_yield = whiting_data_constant,
  temporal_yield = whiting_data_temporal,
  temporal_double_yield = whiting_data_temporal_double
)

# Calculate the maximum value for the y-axis
max_y_whiting <- max(max(whiting_df$constant_yield), max(whiting_df$temporal_yield), max(whiting_df$temporal_double_yield))

# Calculate the breaks for the y-axis
y_breaks_whiting <- seq(0, max_y_whiting, length.out = 25)

# Plotting
ggplot(whiting_df, aes(x = years)) +
  geom_line(aes(y = constant_yield, color = "Constant"), size = 1) +
  geom_line(aes(y = temporal_yield, color = "Temporal"), size = 1) +
  geom_line(aes(y = temporal_double_yield, color = "Temporal Double"), size = 1) +
  labs(x = "Years", y = "Yield of Whiting (g / year)", color = "Legend") +
  scale_x_continuous(breaks = seq(0, 49, by = 1), labels = seq(0, 49, by = 1)) +  # Manually set breaks and labels for x-axis
  scale_y_continuous(breaks = y_breaks_whiting, labels = scientific_format()) +  # Set the breaks for the y-axis and scientific notation
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "grey", linetype = "dotted"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12),  
        axis.text.y = element_text(size = 12),  
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14),  
        legend.title = element_text(size = 14),  
        legend.text = element_text(size = 12),  
        plot.title = element_text(size = 16)) +  
  ggtitle("") +
  scale_color_manual(values = c("Constant" = "#F8766D", "Temporal" = "#00BA38", "Temporal Double" = "#619CFF"),
                     labels = c("Constant harvest", "Periodic Harvest", "Periodic Harvest Double Intensity"))




library(ggplot2)
library(scales)  # For scientific notation formatting

# Extract data for dab
dab_data_constant <- yield_data_constant[0:50, species_code_dab]
dab_data_temporal <- yield_data_temporal[0:50, species_code_dab]
dab_data_temporal_double <- yield_data_temporal_double[0:50, species_code_dab]

# Define the years variable
years <- 0:49  # Adjusted to include 50 years

# Create a data frame for dab
dab_df <- data.frame(
  years = years,
  constant_yield = dab_data_constant,
  temporal_yield = dab_data_temporal,
  temporal_double_yield = dab_data_temporal_double
)

# Calculate the maximum value for the y-axis
max_y_dab <- max(max(dab_df$constant_yield), max(dab_df$temporal_yield), max(dab_df$temporal_double_yield))

# Calculate the breaks for the y-axis
y_breaks_dab <- seq(0, max_y_dab, length.out = 25)

# Plotting
ggplot(dab_df, aes(x = years)) +
  geom_line(aes(y = constant_yield, color = "Constant"), size = 1) +
  geom_line(aes(y = temporal_yield, color = "Temporal"), size = 1) +
  geom_line(aes(y = temporal_double_yield, color = "Temporal Double"), size = 1) +
  labs(x = "Years", y = "Yield of Dab (g / year)", color = "Legend") +
  scale_x_continuous(breaks = seq(0, 49, by = 1), labels = seq(0, 49, by = 1)) +  # Manually set breaks and labels for x-axis
  scale_y_continuous(breaks = y_breaks_dab, labels = scientific_format()) +  # Set the breaks for the y-axis and scientific notation
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "grey", linetype = "dotted"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12),  
        axis.text.y = element_text(size = 12),  
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14),  
        legend.title = element_text(size = 14),  
        legend.text = element_text(size = 12),  
        plot.title = element_text(size = 16)) +  
  ggtitle("") +
  scale_color_manual(values = c("Constant" = "#F8766D", "Temporal" = "#00BA38", "Temporal Double" = "#619CFF"),
                     labels = c("Constant harvest", "Periodic Harvest", "Periodic Harvest Double Intensity"))



library(ggplot2)
library(scales)  # For scientific notation formatting

# Extract data for herring
herring_data_constant <- yield_data_constant[0:50, species_code_herring]
herring_data_temporal <- yield_data_temporal[0:50, species_code_herring]
herring_data_temporal_double <- yield_data_temporal_double[0:50, species_code_herring]

# Define the years variable
years <- 0:49  # Adjusted to include 50 years

# Create a data frame for herring
herring_df <- data.frame(
  years = years,
  constant_yield = herring_data_constant,
  temporal_yield = herring_data_temporal,
  temporal_double_yield = herring_data_temporal_double
)

# Calculate the maximum value for the y-axis
max_y_herring <- max(max(herring_df$constant_yield), max(herring_df$temporal_yield), max(herring_df$temporal_double_yield))

# Calculate the breaks for the y-axis
y_breaks_herring <- seq(0, max_y_herring, length.out = 25)

# Plotting
ggplot(herring_df, aes(x = years)) +
  geom_line(aes(y = constant_yield, color = "Constant"), size = 1) +
  geom_line(aes(y = temporal_yield, color = "Temporal"), size = 1) +
  geom_line(aes(y = temporal_double_yield, color = "Temporal Double"), size = 1) +
  labs(x = "Years", y = "Yield of Herring (g / year)", color = "Legend") +
  scale_x_continuous(breaks = seq(0, 49, by = 1), labels = seq(0, 49, by = 1)) +  # Manually set breaks and labels for x-axis
  scale_y_continuous(breaks = y_breaks_herring, labels = scientific_format()) +  # Set the breaks for the y-axis and scientific notation
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "grey", linetype = "dotted"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12),  
        axis.text.y = element_text(size = 12),  
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14),  
        legend.title = element_text(size = 14),  
        legend.text = element_text(size = 12),  
        plot.title = element_text(size = 16)) +  
  ggtitle("") +
  scale_color_manual(values = c("Constant" = "#F8766D", "Temporal" = "#00BA38", "Temporal Double" = "#619CFF"),
                     labels = c("Constant harvest", "Periodic Harvest", "Periodic Harvest Double Intensity"))




library(ggplot2)
library(scales)  # For scientific notation formatting

# Extract data for Northern Pout
pout_data_constant <- yield_data_constant[0:50, species_code_n.pout]
pout_data_temporal <- yield_data_temporal[0:50, species_code_n.pout]
pout_data_temporal_double <- yield_data_temporal_double[0:50, species_code_n.pout]

# Define the years variable
years <- 0:49  # Adjusted to include 50 years

# Create a data frame for Northern Pout
pout_df <- data.frame(
  years = years,
  constant_yield = pout_data_constant,
  temporal_yield = pout_data_temporal,
  temporal_double_yield = pout_data_temporal_double
)

# Calculate the maximum value for the y-axis
max_y_pout <- max(max(pout_df$constant_yield), max(pout_df$temporal_yield), max(pout_df$temporal_double_yield))

# Calculate the breaks for the y-axis
y_breaks_pout <- seq(0, max_y_pout, length.out = 25)

# Plotting
ggplot(pout_df, aes(x = years)) +
  geom_line(aes(y = constant_yield, color = "Constant"), size = 1) +
  geom_line(aes(y = temporal_yield, color = "Temporal"), size = 1) +
  geom_line(aes(y = temporal_double_yield, color = "Temporal Double"), size = 1) +
  labs(x = "Years", y = "Yield of N.pout (g / year)", color = "Legend") +
  scale_x_continuous(breaks = seq(0, 49, by = 1), labels = seq(0, 49, by = 1)) +  # Manually set breaks and labels for x-axis
  scale_y_continuous(breaks = y_breaks_pout, labels = scientific_format()) +  # Set the breaks for the y-axis and scientific notation
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "grey", linetype = "dotted"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12),  
        axis.text.y = element_text(size = 12),  
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14),  
        legend.title = element_text(size = 14),  
        legend.text = element_text(size = 12),  
        plot.title = element_text(size = 16)) +  
  ggtitle("") +
  scale_color_manual(values = c("Constant" = "#F8766D", "Temporal" = "#00BA38", "Temporal Double" = "#619CFF"),
                     labels = c("Constant harvest", "Periodic Harvest", "Periodic Harvest Double Intensity"))


library(ggplot2)
library(scales)  # For scientific notation formatting

# Extract data for Sandeel
sandeel_data_constant <- yield_data_constant[0:50, species_code_sandeel]
sandeel_data_temporal <- yield_data_temporal[0:50, species_code_sandeel]
sandeel_data_temporal_double <- yield_data_temporal_double[0:50, species_code_sandeel]

# Define the years variable
years <- 0:49  # Adjusted to include 50 years

# Create a data frame for Sandeel
sandeel_df <- data.frame(
  years = years,
  constant_yield = sandeel_data_constant,
  temporal_yield = sandeel_data_temporal,
  temporal_double_yield = sandeel_data_temporal_double
)

# Calculate the maximum value for the y-axis
max_y_sandeel <- max(max(sandeel_df$constant_yield), max(sandeel_df$temporal_yield), max(sandeel_df$temporal_double_yield))

# Calculate the breaks for the y-axis
y_breaks_sandeel <- seq(0, max_y_sandeel, length.out = 25)

# Plotting
ggplot(sandeel_df, aes(x = years)) +
  geom_line(aes(y = constant_yield, color = "Constant"), size = 1) +
  geom_line(aes(y = temporal_yield, color = "Temporal"), size = 1) +
  geom_line(aes(y = temporal_double_yield, color = "Temporal Double"), size = 1) +
  labs(x = "Years", y = "Yield of Sandeel (g / year)", color = "Legend") +
  scale_x_continuous(breaks = seq(0, 49, by = 1), labels = seq(0, 49, by = 1)) +  # Manually set breaks and labels for x-axis
  scale_y_continuous(breaks = y_breaks_sandeel, labels = scientific_format()) +  # Set the breaks for the y-axis and scientific notation
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "grey", linetype = "dotted"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12),  
        axis.text.y = element_text(size = 12),  
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14),  
        legend.title = element_text(size = 14),  
        legend.text = element_text(size = 12),  
        plot.title = element_text(size = 16)) +  
  ggtitle("") +
  scale_color_manual(values = c("Constant" = "#F8766D", "Temporal" = "#00BA38", "Temporal Double" = "#619CFF"),
                     labels = c("Constant harvest", "Periodic Harvest", "Periodic Harvest Double Intensity"))




library(ggplot2)
library(scales)  # For scientific notation formatting

# Extract data for Sprat
sprat_data_constant <- yield_data_constant[0:50, species_code_sprat]
sprat_data_temporal <- yield_data_temporal[0:50, species_code_sprat]
sprat_data_temporal_double <- yield_data_temporal_double[0:50, species_code_sprat]

# Define the years variable
years <- 0:49  # Adjusted to include 50 years

# Create a data frame for Sprat
sprat_df <- data.frame(
  years = years,
  constant_yield = sprat_data_constant,
  temporal_yield = sprat_data_temporal,
  temporal_double_yield = sprat_data_temporal_double
)

# Calculate the maximum value for the y-axis
max_y_sprat <- max(max(sprat_df$constant_yield), max(sprat_df$temporal_yield), max(sprat_df$temporal_double_yield))

# Calculate the breaks for the y-axis
y_breaks_sprat <- seq(0, max_y_sprat, length.out = 25)

# Plotting
ggplot(sprat_df, aes(x = years)) +
  geom_line(aes(y = constant_yield, color = "Constant"), size = 1) +
  geom_line(aes(y = temporal_yield, color = "Temporal"), size = 1) +
  geom_line(aes(y = temporal_double_yield, color = "Temporal Double"), size = 1) +
  labs(x = "Years", y = "Yield of Sprat (g / year)", color = "Legend") +
  scale_x_continuous(breaks = seq(0, 49, by = 1), labels = seq(0, 49, by = 1)) +  # Manually set breaks and labels for x-axis
  scale_y_continuous(breaks = y_breaks_sprat, labels = scientific_format()) +  # Set the breaks for the y-axis and scientific notation
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "grey", linetype = "dotted"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12),  
        axis.text.y = element_text(size = 12),  
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14),  
        legend.title = element_text(size = 14),  
        legend.text = element_text(size = 12),  
        plot.title = element_text(size = 16)) +  
  ggtitle("") +
  scale_color_manual(values = c("Constant" = "#F8766D", "Temporal" = "#00BA38", "Temporal Double" = "#619CFF"),
                     labels = c("Constant harvest", "Periodic Harvest", "Periodic Harvest Double Intensity"))






#######
# Extract biomass data
biomass_temporal <- getBiomass(sim_params_temporal_harvest)
biomass_temporal_double <- getBiomass(sim_params_temporal_harvest_double)
biomass_constant <- getBiomass(sim_params_constant_harvest)
biomass_no_harvest <- getBiomass(sim_params_no_harvest)



# Calculate Shannon's index using the diversity function from the vegan package
library(vegan)

shannon_index_temporal <- diversity(biomass_temporal, index = "shannon")
shannon_index_temporal_double <- diversity(biomass_temporal_double, index = "shannon")
shannon_index_constant <- diversity(biomass_constant, index = "shannon")
shannon_index_no_harvest <- diversity(biomass_no_harvest, index = "shannon")

#
# Print the calculated Shannon's indices
print(shannon_index_temporal)
print(shannon_index_temporal_double)
print(shannon_index_constant)
print(shannon_index_no_harvest)

# Create data frames
shannon_temporal_df <- as.data.frame(shannon_index_temporal)
shannon_temporal_double_df <- as.data.frame(shannon_index_temporal_double)
shannon_constant_df <- as.data.frame(shannon_index_constant)
shannon_no_harvest_df <- as.data.frame(shannon_index_no_harvest)
# Extracting data for Shannon's index
shannon_temporal_df <- shannon_temporal_df[1:50, ]
shannon_temporal_double_df <- shannon_temporal_double_df[1:50, ]
shannon_constant_df <- shannon_constant_df[1:50, ]
shannon_no_harvest_df <- shannon_no_harvest_df[1:50, ]
# Define the number of breaks on the y-axis

library(ggplot2)

# Combine the data into a single data frame
shannon_df <- data.frame(
  years = 0:49,
  temporal_double = shannon_temporal_double_df,
  temporal = shannon_temporal_df,
  no_harvest = shannon_no_harvest_df,
  constant_harvest = shannon_constant_df
)

# Rename the columns
names(shannon_df) <- c("Years", "Temporal Double", "Temporal", "No Harvest", "Constant Harvest")


# Find the range of the Shannon's Index values
shannon_range <- range(shannon_df[, -1])


# Define custom colors for the bars
library(ggplot2)
library(patchwork)



# Define bar colors
bar_colors <- c("Periodic Harvest Double Fishing Intensity" = "#619CFF", "Periodic Harvest" = "#00BA38", "No Harvest" = "grey", "Constant Harvest" = "#F8766D")

# Plot the data as separate bar graphs within the same panel
plot1 <- ggplot(shannon_df, aes(x = years)) +
  geom_bar(aes(y = temporal_double, fill = "Periodic Harvest Double Fishing Intensity"), stat = "identity", width = 0.5) +
  labs(x = "Time step (Year)", y = "Shannon's Index") +
  facet_wrap(~"", scales = "free_y") +
  theme_minimal() +
  scale_fill_manual(values = bar_colors["Periodic Harvest Double Fishing Intensity"]) +
  theme(
    panel.grid.major = element_line(color = "grey", linetype = "dotted"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    plot.title = element_text(size = 16),
    legend.title = element_blank(),  # Remove legend title "fill"
    legend.position = "bottom",  # Adjust legend position
    legend.text = element_text(size = 12)  # Increase legend text size
  ) +
  scale_x_continuous(breaks = seq(min(shannon_df$years), max(shannon_df$years), by = 2))  # Add breaks every 2 years

plot2 <- ggplot(shannon_df, aes(x = years)) +
  geom_bar(aes(y = temporal, fill = "Periodic Harvest"), stat = "identity", width = 0.5) +
  labs(x = "Time step (Year)", y = "Shannon's Index") +
  facet_wrap(~"", scales = "free_y") +
  theme_minimal() +
  scale_fill_manual(values = bar_colors["Periodic Harvest"]) +
  theme(
    panel.grid.major = element_line(color = "grey", linetype = "dotted"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    plot.title = element_text(size = 16),
    legend.title = element_blank(),  # Remove legend title "fill"
    legend.position = "bottom",  # Adjust legend position
    legend.text = element_text(size = 12)  # Increase legend text size
  ) +
  scale_x_continuous(breaks = seq(min(shannon_df$years), max(shannon_df$years), by = 2))  # Add breaks every 2 years

plot3 <- ggplot(shannon_df, aes(x = years)) +
  geom_bar(aes(y = no_harvest, fill = "No Harvest"), stat = "identity", width = 0.5) +
  labs(x = "Time step (Year)", y = "Shannon's Index") +
  facet_wrap(~"", scales = "free_y") +
  theme_minimal() +
  scale_fill_manual(values = bar_colors["No Harvest"]) +
  theme(
    panel.grid.major = element_line(color = "grey", linetype = "dotted"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    plot.title = element_text(size = 16),
    legend.title = element_blank(),  # Remove legend title "fill"
    legend.position = "bottom",  # Adjust legend position
    legend.text = element_text(size = 12)  # Increase legend text size
  ) +
  scale_x_continuous(breaks = seq(min(shannon_df$years), max(shannon_df$years), by = 2))  # Add breaks every 2 years

plot4 <- ggplot(shannon_df, aes(x = years)) +
  geom_bar(aes(y = constant_harvest, fill = "Constant Harvest"), stat = "identity", width = 0.5) +
  labs(x = "Time step (Year)", y = "Shannon's Index") +
  facet_wrap(~"", scales = "free_y") +
  theme_minimal() +
  scale_fill_manual(values = bar_colors["Constant Harvest"]) +
  theme(
    panel.grid.major = element_line(color = "grey", linetype = "dotted"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    plot.title = element_text(size = 16),
    legend.title = element_blank(),  # Remove legend title "fill"
    legend.position = "bottom",  # Adjust legend position
    legend.text = element_text(size = 12)  # Increase legend text size
  ) +
  scale_x_continuous(breaks = seq(min(shannon_df$years), max(shannon_df$years), by = 2))  # Add breaks every 2 years

# Arrange plots side by side
plots <- plot1 + plot2 + plot3 + plot4

# Print the plots
plots


## plotting abundance (biomass (g)) ##

# Too complex
plotBiomass(sim_params_constant_harvest, start_time = 16, end_time = 50)

plotBiomass(sim_params_temporal_harvest, start_time = 16, end_time = 50)

plotBiomass(sim_params_temporal_harvest_double, start_time = 16, end_time = 50)

plotBiomass(sim_params_no_harvest, start_time = 16, end_time = 50)

# Comparisons 

# Need to plot biomass for species across different scenarios

# Biomass and abundance

biomass_temporal <- getBiomass(sim_params_temporal_harvest)
biomass_temporal


biomass_temporal_double <- getBiomass(sim_params_temporal_harvest_double)
biomass_temporal_double


biomass_constant <- getBiomass(sim_params_constant_harvest)
biomass_constant



biomass_no_harvest <- getBiomass(sim_params_no_harvest)
biomass_no_harvest



# Create a data frame for biomass data
# Create a data frame for biomass data
biomass_df <- data.frame(
  Time_Step = 0:50,  # Assuming there are 52 time steps
  Cod_Temporal = biomass_temporal[, "Cod"],
  Plaice_Temporal = biomass_temporal[, "Plaice"],
  Saithe_Temporal = biomass_temporal[, "Saithe"],
  Sprat_Temporal = biomass_temporal[, "Sprat"],
  Sandeel_Temporal = biomass_temporal[, "Sandeel"],
  N.pout_Temporal = biomass_temporal[, "N.pout"],
  Dab_Temporal = biomass_temporal[, "Dab"],
  Herring_Temporal = biomass_temporal[, "Herring"],
  Whiting_Temporal = biomass_temporal[, "Whiting"],
  Sole_Temporal = biomass_temporal[, "Sole"],
  Gurnard_Temporal = biomass_temporal[, "Gurnard"],
  Haddock_Temporal = biomass_temporal[, "Haddock"],
  Cod_Temporal_Double = biomass_temporal_double[, "Cod"],
  Plaice_Temporal_Double = biomass_temporal_double[, "Plaice"],
  Saithe_Temporal_Double = biomass_temporal_double[, "Saithe"],
  Sprat_Temporal_Double = biomass_temporal_double[, "Sprat"],
  Sandeel_Temporal_Double = biomass_temporal_double[, "Sandeel"],
  N.pout_Temporal_Double = biomass_temporal_double[, "N.pout"],
  Dab_Temporal_Double = biomass_temporal_double[, "Dab"],
  Herring_Temporal_Double = biomass_temporal_double[, "Herring"],
  Whiting_Temporal_Double = biomass_temporal_double[, "Whiting"],
  Sole_Temporal_Double = biomass_temporal_double[, "Sole"],
  Gurnard_Temporal_Double = biomass_temporal_double[, "Gurnard"],
  Haddock_Temporal_Double = biomass_temporal_double[, "Haddock"],
  Cod_Constant = biomass_constant[, "Cod"],
  Plaice_Constant = biomass_constant[, "Plaice"],
  Saithe_Constant = biomass_constant[, "Saithe"],
  Sprat_Constant = biomass_constant[, "Sprat"],
  Sandeel_Constant = biomass_constant[, "Sandeel"],
  N.pout_Constant = biomass_constant[, "N.pout"],
  Dab_Constant = biomass_constant[, "Dab"],
  Herring_Constant = biomass_constant[, "Herring"],
  Whiting_Constant = biomass_constant[, "Whiting"],
  Sole_Constant = biomass_constant[, "Sole"],
  Gurnard_Constant = biomass_constant[, "Gurnard"],
  Haddock_Constant = biomass_constant[, "Haddock"],
  Cod_No_Harvest = biomass_no_harvest[, "Cod"],
  Plaice_No_Harvest = biomass_no_harvest[, "Plaice"],
  Saithe_No_Harvest = biomass_no_harvest[, "Saithe"],
  Sprat_No_Harvest = biomass_no_harvest[, "Sprat"],
  Sandeel_No_Harvest = biomass_no_harvest[, "Sandeel"],
  N.pout_No_Harvest = biomass_no_harvest[, "N.pout"],
  Dab_No_Harvest = biomass_no_harvest[, "Dab"],
  Herring_No_Harvest = biomass_no_harvest[, "Herring"],
  Whiting_No_Harvest = biomass_no_harvest[, "Whiting"],
  Sole_No_Harvest = biomass_no_harvest[, "Sole"],
  Gurnard_No_Harvest = biomass_no_harvest[, "Gurnard"],
  Haddock_No_Harvest = biomass_no_harvest[, "Haddock"]
)

# plotting relevant years
biomass_df_subset <- biomass_df[16:50, ]

## Plotting K strategists 

# Load the ggplot2 library
library(ggplot2)

par(mfrow = c(1, 1))
effort_array_double
biomass_df_subset

# Find the range of the Shannon's Index values
library(ggplot2)

library(ggplot2)

# Define the range of biomass data
biomass_range <- range(biomass_df_subset[, -1])

# Calculate breaks for the y-axis
y_breaks <- seq(biomass_range[1], biomass_range[2], length.out = 18)

# Plot the biomass of different cod scenarios with modified legend names
ggplot(biomass_df_subset, aes(x = Time_Step)) +
  geom_rect(aes(xmin = 15, xmax = 19, ymin = -Inf, ymax = Inf), fill = "lightgrey", alpha = 0.5) + # Add shaded rectangle
  geom_rect(aes(xmin = 25, xmax = 29, ymin = -Inf, ymax = Inf), fill = "lightgrey", alpha = 0.5) + 
  geom_rect(aes(xmin = 35, xmax = 39, ymin = -Inf, ymax = Inf), fill = "lightgrey", alpha = 0.5) +
  geom_rect(aes(xmin = 45, xmax = 49, ymin = -Inf, ymax = Inf), fill = "lightgrey", alpha = 0.5) + 
  geom_line(aes(y = Cod_Temporal_Double, color = "Cod Periodic Double Intensity")) +
  geom_line(aes(y = Cod_Constant, color = "Cod Constant")) +
  geom_line(aes(y = Cod_No_Harvest, color = "Cod No Harvest")) +
  geom_segment(aes(x = 25, xend = 25, y = Cod_Constant, yend = max(Cod_Constant, Cod_Temporal_Double)), linetype = "dashed", color = "black") +  # Add vertical dotted black line
  geom_text(aes(x = 25, y = max(Cod_Constant, Cod_Temporal_Double), label = "Eb"), vjust = 12, hjust = 1.3, color = "black") + # Add label for Eb notation
  geom_segment(aes(x = 25, xend = 30, y = max(Cod_Temporal_Double), yend = 3.446612e+11), linetype = "dashed", color = "purple") +  # Add diagonal dotted purple line
  geom_text(aes(x = 24, y = max(Cod_Constant, Cod_Temporal_Double), label = "Eh"), vjust = 14, hjust = -2.8, color = "purple") +
  geom_segment(aes(x = 25, xend = 30, y = 3.446612e+11, yend = 3.446612e+11), linetype = "dashed", color = "purple") +
  geom_segment(aes(x = 30, xend = 30, y = Cod_Constant, yend = min(Cod_Constant, Cod_Temporal_Double)), linetype = "dashed", color = "orange") + 
  geom_text(aes(x = 25, y = max(Cod_Constant, Cod_Temporal_Double), label = "Ea"), vjust = 32, hjust = -4.5, color = "orange") + 
  geom_text(aes(x = 25, y = max(Cod_Constant, Cod_Temporal_Double), label = "Pb2"), vjust = -0.5, hjust = 1, color = "grey40") + 
  geom_text(aes(x = 25, y = max(Cod_Constant, Cod_Temporal_Double), label = "Ob2"), vjust = 30, hjust = 1, color = "grey40") + 
  geom_text(aes(x = 25, y = max(Cod_Constant, Cod_Temporal_Double), label = "Oa2"), vjust = 27.5, hjust = -2.1, color = "grey40") + 
  geom_text(aes(x = 25, y = max(Cod_Constant, Cod_Temporal_Double), label = "Pa2"), vjust = 35, hjust = -3.2, color = "grey40") + 
  geom_text(aes(x = 25, y = max(Cod_Constant, Cod_Temporal_Double), label = "Pr2"), vjust = 10, hjust = -3.5, color = "grey40") +
  geom_text(aes(x = 25, y = max(Cod_Constant, Cod_Temporal_Double), label = "PH 2"), vjust = -3, hjust = -0.6, color = "grey40") +
  geom_text(aes(x = 25, y = max(Cod_Constant, Cod_Temporal_Double), label = "Pb3"), vjust = -0.5, hjust = -4.4, color = "grey40") + 
  geom_text(aes(x = 25, y = max(Cod_Constant, Cod_Temporal_Double), label = "Ob3"), vjust = 30, hjust = -4, color = "grey40") + 
  geom_text(aes(x = 25, y = max(Cod_Constant, Cod_Temporal_Double), label = "Oa3"), vjust = 27.5, hjust = -6.8, color = "grey40") + 
  geom_text(aes(x = 25, y = max(Cod_Constant, Cod_Temporal_Double), label = "Pa3"), vjust = 35, hjust = -8.8, color = "grey40") + 
  geom_text(aes(x = 25, y = max(Cod_Constant, Cod_Temporal_Double), label = "Pr3"), vjust = 10, hjust = -10, color = "grey40") +
  geom_text(aes(x = 25, y = max(Cod_Constant, Cod_Temporal_Double), label = "PH 3"), vjust = -3, hjust = -5.3, color = "grey40") +
  geom_text(aes(x = 25, y = max(Cod_Constant, Cod_Temporal_Double), label = "PH 1"), vjust = -3, hjust = 4.5, color = "grey40") +
  geom_text(aes(x = 25, y = max(Cod_Constant, Cod_Temporal_Double), label = "PH 4"), vjust = -3, hjust = -10, color = "grey40") +
  geom_point(data = data.frame(x = c(25, 30, 25, 30, 35, 35, 40, 40), y = c(2.317106e+12, 344659476681, 697484287582, 697493621553, 2.320849e+12, 697490774653, 344668408966, 697490865216)), aes(x, y), color = "black", size = 2, shape = 4) +
  labs(title = "",
       x = "Time Step (years)",
       y = "Biomass (g)",
       color = "Scenario") +
  scale_color_manual(values = c( 
    "Cod Periodic Double Intensity" = "#619CFF", 
    "Cod Constant" = "#F8766D", 
    "Cod No Harvest" = "Dark Grey")) +
  scale_y_continuous(breaks = y_breaks, labels = scales::scientific_format()) +  # Set custom breaks and scientific notation for y-axis
  scale_x_continuous(breaks = seq(0, 49, by = 2), labels = seq(0, 49, by = 2)) +
  theme_minimal()



# returns year 35 and 40
biomass_temporal_double[36, "Cod"]
biomass_temporal_double[41, "Cod"]
biomass_temporal_double
biomass_constant[36, "Cod"]
biomass_constant[41, "Cod"]

#35 year = fishing on (Pb2)(Ob2)
# 2.320849e+12 - Cod PH double
# 697490774653 - cod constant

#40 year = fishing off (Pa2)(Oa2)
# 344668408966 - Cod PH double
# 697490865216 - cod constant


# Calulating benefits for PH 1 double

# Cod

Ea_cod_temporal_double <- log(biomass_temporal_double[31, "Cod"]/biomass_constant[31, "Cod"])
Ea_cod_temporal_double
Eb_cod_temporal_double <- log(biomass_temporal_double[26, "Cod"]/biomass_constant[26, "Cod"])
Eb_cod_temporal_double
Eh_cod_temporal_double <- log((biomass_temporal_double[31, "Cod"]/biomass_temporal_double[26, "Cod"])/(biomass_constant[31, "Cod"]/biomass_constant[26, "Cod"]))
Eh_cod_temporal_double
# Sprat

Ea_sprat_temporal_double <- log(biomass_temporal_double[31, "Sprat"]/biomass_constant[31, "Sprat"])
Ea_sprat_temporal_double
Eb_sprat_temporal_double <- log(biomass_temporal_double[26, "Sprat"]/biomass_constant[26, "Sprat"])
Eb_sprat_temporal_double
Eh_sprat_temporal_double <- log((biomass_temporal_double[31, "Sprat"]/biomass_temporal_double[26, "Sprat"])/(biomass_constant[31, "Sprat"]/biomass_constant[26, "Sprat"]))
Eh_sprat_temporal_double
# N.pout

Ea_npout_temporal_double <- log(biomass_temporal_double[31, "N.pout"]/biomass_constant[31, "N.pout"])
Ea_npout_temporal_double 
Eb_npout_temporal_double <- log(biomass_temporal_double[26, "N.pout"]/biomass_constant[26, "N.pout"])
Eb_npout_temporal_double
Eh_npout_temporal_double <- log((biomass_temporal_double[31, "N.pout"]/biomass_temporal_double[26, "N.pout"])/(biomass_constant[31, "N.pout"]/biomass_constant[26, "N.pout"]))
Eh_npout_temporal_double
# Herring

Ea_herring_temporal_double <- log(biomass_temporal_double[31, "Herring"]/biomass_constant[31, "Herring"])
Ea_herring_temporal_double
Eb_herring_temporal_double <- log(biomass_temporal_double[26, "Herring"]/biomass_constant[26, "Herring"])
Eb_herring_temporal_double
Eh_herring_temporal_double <- log((biomass_temporal_double[31, "Herring"]/biomass_temporal_double[26, "Herring"])/(biomass_constant[31, "Herring"]/biomass_constant[26, "Herring"]))
Eh_herring_temporal_double
# Dab

Ea_dab_temporal_double <- log(biomass_temporal_double[31, "Dab"]/biomass_constant[31, "Dab"])
Ea_dab_temporal_double
Eb_dab_temporal_double <- log(biomass_temporal_double[26, "Dab"]/biomass_constant[26, "Dab"])
Eb_dab_temporal_double
Eh_dab_temporal_double <- log((biomass_temporal_double[31, "Dab"]/biomass_temporal_double[26, "Dab"])/(biomass_constant[31, "Dab"]/biomass_constant[26, "Dab"]))
Eh_dab_temporal_double
# Whiting

Ea_whiting_temporal_double <- log(biomass_temporal_double[31, "Whiting"]/biomass_constant[31, "Whiting"])
Ea_whiting_temporal_double
Eb_whiting_temporal_double <- log(biomass_temporal_double[26, "Whiting"]/biomass_constant[26, "Whiting"])
Eb_whiting_temporal_double
Eh_whiting_temporal_double <- log((biomass_temporal_double[31, "Whiting"]/biomass_temporal_double[26, "Whiting"])/(biomass_constant[31, "Whiting"]/biomass_constant[26, "Whiting"]))
Eh_whiting_temporal_double
# Sole

Ea_sole_temporal_double <- log(biomass_temporal_double[31, "Sole"]/biomass_constant[31, "Sole"])
Ea_sole_temporal_double
Eb_sole_temporal_double <- log(biomass_temporal_double[26, "Sole"]/biomass_constant[26, "Sole"])
Eb_sole_temporal_double
Eh_sole_temporal_double <- log((biomass_temporal_double[31, "Sole"]/biomass_temporal_double[26, "Sole"])/(biomass_constant[31, "Sole"]/biomass_constant[26, "Sole"]))
Eh_sole_temporal_double
# Gurnard

Ea_greygurnard_temporal_double <- log(biomass_temporal_double[31, "Gurnard"]/biomass_constant[31, "Gurnard"])
Ea_greygurnard_temporal_double
Eb_greygurnard_temporal_double <- log(biomass_temporal_double[26, "Gurnard"]/biomass_constant[26, "Gurnard"])
Eb_greygurnard_temporal_double
Eh_greygurnard_temporal_double <- log((biomass_temporal_double[31, "Gurnard"]/biomass_temporal_double[26, "Gurnard"])/(biomass_constant[31, "Gurnard"]/biomass_constant[26, "Gurnard"]))
Eh_greygurnard_temporal_double
# plaice

Ea_plaice_temporal_double <- log(biomass_temporal_double[31, "Plaice"]/biomass_constant[31, "Plaice"])
Ea_plaice_temporal_double
Eb_plaice_temporal_double <- log(biomass_temporal_double[26, "Plaice"]/biomass_constant[26, "Plaice"])
Eb_plaice_temporal_double
Eh_plaice_temporal_double <- log((biomass_temporal_double[31, "Plaice"]/biomass_temporal_double[26, "Plaice"])/(biomass_constant[31, "Plaice"]/biomass_constant[26, "Plaice"]))
Eh_plaice_temporal_double
# Haddock

Ea_haddock_temporal_double <- log(biomass_temporal_double[31, "Haddock"]/biomass_constant[31, "Haddock"])
Ea_haddock_temporal_double
Eb_haddock_temporal_double <- log(biomass_temporal_double[26, "Haddock"]/biomass_constant[26, "Haddock"])
Eb_haddock_temporal_double
Eh_haddock_temporal_double <- log((biomass_temporal_double[31, "Haddock"]/biomass_temporal_double[26, "Haddock"])/(biomass_constant[31, "Haddock"]/biomass_constant[26, "Haddock"]))
Eh_haddock_temporal_double
# Saithe

Ea_saithe_temporal_double <- log(biomass_temporal_double[31, "Saithe"]/biomass_constant[31, "Saithe"])
Ea_saithe_temporal_double
Eb_saithe_temporal_double <- log(biomass_temporal_double[26, "Saithe"]/biomass_constant[26, "Saithe"])
Eb_saithe_temporal_double
Eh_saithe_temporal_double <- log((biomass_temporal_double[31, "Saithe"]/biomass_temporal_double[26, "Saithe"])/(biomass_constant[31, "Saithe"]/biomass_constant[26, "Saithe"]))
Eh_saithe_temporal_double

# Sandeel

Ea_Sandeel_temporal_double <- log(biomass_temporal_double[31, "Sandeel"]/biomass_constant[31, "Sandeel"])
Ea_Sandeel_temporal_double
Eb_Sandeel_temporal_double <- log(biomass_temporal_double[26, "Sandeel"]/biomass_constant[26, "Sandeel"])
Eb_Sandeel_temporal_double
Eh_Sandeel_temporal_double <- log((biomass_temporal_double[31, "Sandeel"]/biomass_temporal_double[26, "Sandeel"])/(biomass_constant[31, "Sandeel"]/biomass_constant[26, "Sandeel"]))
Eh_Sandeel_temporal_double

#########


# Cod

Ea_cod_temporal <- log(biomass_temporal[31, "Cod"]/biomass_constant[31, "Cod"])
Ea_cod_temporal
Eb_cod_temporal <- log(biomass_temporal[26, "Cod"]/biomass_constant[26, "Cod"])
Eb_cod_temporal
Eh_cod_temporal <- log((biomass_temporal[31, "Cod"]/biomass_temporal[26, "Cod"])/(biomass_constant[31, "Cod"]/biomass_constant[26, "Cod"]))
Eh_cod_temporal

# Sprat

Ea_sprat_temporal <- log(biomass_temporal[31, "Sprat"]/biomass_constant[31, "Sprat"])
Ea_sprat_temporal
Eb_sprat_temporal <- log(biomass_temporal[26, "Sprat"]/biomass_constant[26, "Sprat"])
Eb_sprat_temporal
Eh_sprat_temporal <- log((biomass_temporal[31, "Sprat"]/biomass_temporal[26, "Sprat"])/(biomass_constant[31, "Sprat"]/biomass_constant[26, "Sprat"]))
Eh_sprat_temporal

# N.pout

Ea_npout_temporal <- log(biomass_temporal[31, "N.pout"]/biomass_constant[31, "N.pout"])
Ea_npout_temporal 
Eb_npout_temporal <- log(biomass_temporal[26, "N.pout"]/biomass_constant[26, "N.pout"])
Eb_npout_temporal
Eh_npout_temporal <- log((biomass_temporal[31, "N.pout"]/biomass_temporal[26, "N.pout"])/(biomass_constant[31, "N.pout"]/biomass_constant[26, "N.pout"]))
Eh_npout_temporal

# Herring

Ea_herring_temporal <- log(biomass_temporal[31, "Herring"]/biomass_constant[31, "Herring"])
Ea_herring_temporal
Eb_herring_temporal <- log(biomass_temporal[26, "Herring"]/biomass_constant[26, "Herring"])
Eb_herring_temporal
Eh_herring_temporal <- log((biomass_temporal[31, "Herring"]/biomass_temporal[26, "Herring"])/(biomass_constant[31, "Herring"]/biomass_constant[26, "Herring"]))
Eh_herring_temporal

# Dab

Ea_dab_temporal <- log(biomass_temporal[31, "Dab"]/biomass_constant[31, "Dab"])
Ea_dab_temporal
Eb_dab_temporal <- log(biomass_temporal[26, "Dab"]/biomass_constant[26, "Dab"])
Eb_dab_temporal
Eh_dab_temporal <- log((biomass_temporal[31, "Dab"]/biomass_temporal[26, "Dab"])/(biomass_constant[31, "Dab"]/biomass_constant[26, "Dab"]))
Eh_dab_temporal

# Whiting

Ea_whiting_temporal <- log(biomass_temporal[31, "Whiting"]/biomass_constant[31, "Whiting"])
Ea_whiting_temporal
Eb_whiting_temporal <- log(biomass_temporal[26, "Whiting"]/biomass_constant[26, "Whiting"])
Eb_whiting_temporal
Eh_whiting_temporal <- log((biomass_temporal[31, "Whiting"]/biomass_temporal[26, "Whiting"])/(biomass_constant[31, "Whiting"]/biomass_constant[26, "Whiting"]))
Eh_whiting_temporal

# Sole

Ea_sole_temporal <- log(biomass_temporal[31, "Sole"]/biomass_constant[31, "Sole"])
Ea_sole_temporal
Eb_sole_temporal <- log(biomass_temporal[26, "Sole"]/biomass_constant[26, "Sole"])
Eb_sole_temporal
Eh_sole_temporal <- log((biomass_temporal[31, "Sole"]/biomass_temporal[26, "Sole"])/(biomass_constant[31, "Sole"]/biomass_constant[26, "Sole"]))
Eh_sole_temporal

# Gurnard

Ea_greygurnard_temporal <- log(biomass_temporal[31, "Gurnard"]/biomass_constant[31, "Gurnard"])
Ea_greygurnard_temporal
Eb_greygurnard_temporal <- log(biomass_temporal[26, "Gurnard"]/biomass_constant[26, "Gurnard"])
Eb_greygurnard_temporal
Eh_greygurnard_temporal <- log((biomass_temporal[31, "Gurnard"]/biomass_temporal[26, "Gurnard"])/(biomass_constant[31, "Gurnard"]/biomass_constant[26, "Gurnard"]))
Eh_greygurnard_temporal

# plaice

Ea_plaice_temporal <- log(biomass_temporal[31, "Plaice"]/biomass_constant[31, "Plaice"])
Ea_plaice_temporal
Eb_plaice_temporal <- log(biomass_temporal[26, "Plaice"]/biomass_constant[26, "Plaice"])
Eb_plaice_temporal
Eh_plaice_temporal <- log((biomass_temporal[31, "Plaice"]/biomass_temporal[26, "Plaice"])/(biomass_constant[31, "Plaice"]/biomass_constant[26, "Plaice"]))
Eh_plaice_temporal

# Haddock

Ea_haddock_temporal <- log(biomass_temporal[31, "Haddock"]/biomass_constant[31, "Haddock"])
Ea_haddock_temporal
Eb_haddock_temporal <- log(biomass_temporal[26, "Haddock"]/biomass_constant[26, "Haddock"])
Eb_haddock_temporal
Eh_haddock_temporal <- log((biomass_temporal[31, "Haddock"]/biomass_temporal[26, "Haddock"])/(biomass_constant[31, "Haddock"]/biomass_constant[26, "Haddock"]))
Eh_haddock_temporal

# Saithe

Ea_saithe_temporal <- log(biomass_temporal[31, "Saithe"]/biomass_constant[31, "Saithe"])
Ea_saithe_temporal
Eb_saithe_temporal <- log(biomass_temporal[26, "Saithe"]/biomass_constant[26, "Saithe"])
Eb_saithe_temporal
Eh_saithe_temporal <- log((biomass_temporal[31, "Saithe"]/biomass_temporal[26, "Saithe"])/(biomass_constant[31, "Saithe"]/biomass_constant[26, "Saithe"]))
Eh_saithe_temporal

# Sandeel

Ea_Sandeel_temporal <- log(biomass_temporal[31, "Sandeel"]/biomass_constant[31, "Sandeel"])
Ea_Sandeel_temporal
Eb_Sandeel_temporal <- log(biomass_temporal[26, "Sandeel"]/biomass_constant[26, "Sandeel"])
Eb_Sandeel_temporal
Eh_Sandeel_temporal <- log((biomass_temporal[31, "Sandeel"]/biomass_temporal[26, "Sandeel"])/(biomass_constant[31, "Sandeel"]/biomass_constant[26, "Sandeel"]))
Eh_Sandeel_temporal




effort_array

library(ggplot2)

# Define the range of biomass data

library(ggplot2)

# Define the range of biomass data
biomass_range <- range(biomass_df_subset[, -1])

# Calculate breaks for the y-axis
y_breaks <- seq(biomass_range[1], biomass_range[2], length.out = 18)

# Plot the biomass of different Plaice scenarios with modified legend names
ggplot(biomass_df_subset, aes(x = Time_Step)) +
  geom_rect(aes(xmin = 15, xmax = 19, ymin = -Inf, ymax = Inf), fill = "lightgrey", alpha = 0.5) + # Add shaded rectangle
  geom_rect(aes(xmin = 25, xmax = 29, ymin = -Inf, ymax = Inf), fill = "lightgrey", alpha = 0.5) + 
  geom_rect(aes(xmin = 35, xmax = 39, ymin = -Inf, ymax = Inf), fill = "lightgrey", alpha = 0.5) + 
  geom_rect(aes(xmin = 45, xmax = 49, ymin = -Inf, ymax = Inf), fill = "lightgrey", alpha = 0.5) +
  geom_text(aes(x = 25, y = max(Cod_Constant, Cod_Temporal_Double), label = "PH 1"), vjust = -3, hjust = 4.2, color = "grey40") +
  geom_text(aes(x = 25, y = max(Cod_Constant, Cod_Temporal_Double), label = "PH 2"), vjust = -3, hjust = -0.5, color = "grey40") +
  geom_text(aes(x = 25, y = max(Cod_Constant, Cod_Temporal_Double), label = "PH 3"), vjust = -3, hjust = -5, color = "grey40") +
  geom_text(aes(x = 25, y = max(Cod_Constant, Cod_Temporal_Double), label = "PH 4"), vjust = -3, hjust = -9.6, color = "grey40") +
  geom_line(aes(y = Plaice_Temporal_Double, color = "Plaice Periodic Double Intensity")) +
  geom_line(aes(y = Plaice_Temporal, color = "Plaice Periodic")) + # Added Plaice Temporal line
  geom_line(aes(y = Plaice_Constant, color = "Plaice Constant")) +
  geom_line(aes(y = Plaice_No_Harvest, color = "Plaice No Harvest")) +
  labs(title = "",
       x = "Time Step (years)",
       y = "Biomass (g)",
       color = "Scenario") +
  scale_color_manual(values = c( 
    "Plaice Periodic Double Intensity" = "#619CFF", 
    "Plaice Periodic" = "#00BA38", # Changed from "Saithe Periodic" to "Plaice Periodic"
    "Plaice Constant" = "#F8766D", 
    "Plaice No Harvest" = "Dark Grey")) +
  scale_y_continuous(breaks = y_breaks, labels = scales::scientific_format(), limits = c(0, NA)) +  # Set custom breaks for y-axis
  scale_x_continuous(breaks = seq(0, 49, by = 2), labels = seq(0, 49, by = 2)) +
  theme_minimal() +
  theme(axis.ticks.y = element_line(color = "black"),  # Add ticks to y-axis
        panel.grid.major.y = element_line(color = "grey"))  # Add major grid lines to y-axis




# Define the range of biomass data
Saithe_columns <- c("Saithe_Temporal", "Saithe_Temporal_Double", "Saithe_Constant", "Saithe_No_Harvest")
# Calculate the range of N.pout
Saithe_range <- range(biomass_df_subset[, Saithe_columns])

# Calculate breaks for the y-axis
y_breaks <- seq(Saithe_range[1], Saithe_range[2], length.out = 8)


# Plot the biomass of different Saithe scenarios with modified legend names
ggplot(biomass_df_subset, aes(x = Time_Step)) +
  geom_rect(aes(xmin = 15, xmax = 19, ymin = -Inf, ymax = Inf), fill = "lightgrey", alpha = 0.5) + # Add shaded rectangle
  geom_rect(aes(xmin = 25, xmax = 29, ymin = -Inf, ymax = Inf), fill = "lightgrey", alpha = 0.5) + 
  geom_rect(aes(xmin = 35, xmax = 39, ymin = -Inf, ymax = Inf), fill = "lightgrey", alpha = 0.5) + 
  geom_rect(aes(xmin = 45, xmax = 49, ymin = -Inf, ymax = Inf), fill = "lightgrey", alpha = 0.5) +
  geom_line(aes(y = Saithe_Temporal_Double, color = "Saithe Periodic Double Intensity")) +
  geom_line(aes(y = Saithe_Temporal, color = "Saithe Periodic")) + # Added Saithe Temporal line
  geom_line(aes(y = Saithe_Constant, color = "Saithe Constant")) +
  geom_line(aes(y = Saithe_No_Harvest, color = "Saithe No Harvest")) +
  annotate("text", x = 17, y = max(biomass_df_subset$Saithe_Constant, biomass_df_subset$Saithe_Temporal_Double), label = "PH 1", color = "black") +
  annotate("text", x = 27, y = max(biomass_df_subset$Saithe_Constant, biomass_df_subset$Saithe_Temporal_Double), label = "PH 2", color = "black") +
  annotate("text", x = 37, y = max(biomass_df_subset$Saithe_Constant, biomass_df_subset$Saithe_Temporal_Double), label = "PH 3", color = "black") +
  annotate("text", x = 47, y = max(biomass_df_subset$Saithe_Constant, biomass_df_subset$Saithe_Temporal_Double), label = "PH 4", color = "black") +
  labs(title = "",
       x = "Time Step (years)",
       y = "Biomass (g)",
       color = "Scenario") +
  scale_color_manual(values = c( 
    "Saithe Periodic Double Intensity" = "#619CFF", 
    "Saithe Periodic" = "#00BA38", # Changed from "Saithe Periodic" to "Saithe Periodic"
    "Saithe Constant" = "#F8766D", 
    "Saithe No Harvest" = "Dark Grey")) +
  scale_y_continuous(breaks = y_breaks, labels = scales::scientific_format(), limits = Saithe_range) +  # Set custom breaks for y-axis
  scale_x_continuous(breaks = seq(0, 49, by = 2), labels = seq(0, 49, by = 2)) +
  theme_minimal() +
  theme(axis.ticks.y = element_line(color = "black"),  # Add ticks to y-axis
        panel.grid.major.y = element_line(color = "grey"))  # Add major grid lines to y-axis




# Define the range of biomass data
Sprat_columns <- c("Sprat_Temporal", "Sprat_Temporal_Double", "Sprat_Constant", "Sprat_No_Harvest")
# Calculate the range of Sprat
Sprat_range <- range(biomass_df_subset[, Sprat_columns])

# Calculate breaks for the y-axis
y_breaks <- seq(Sprat_range[1], Sprat_range[2], length.out = 8)

# Plot the biomass of different Sprat scenarios with modified legend names
ggplot(biomass_df_subset, aes(x = Time_Step)) +
  geom_rect(aes(xmin = 15, xmax = 19, ymin = -Inf, ymax = Inf), fill = "lightgrey", alpha = 0.5) + # Add shaded rectangle
  geom_rect(aes(xmin = 25, xmax = 29, ymin = -Inf, ymax = Inf), fill = "lightgrey", alpha = 0.5) + 
  geom_rect(aes(xmin = 35, xmax = 39, ymin = -Inf, ymax = Inf), fill = "lightgrey", alpha = 0.5) + 
  geom_rect(aes(xmin = 45, xmax = 49, ymin = -Inf, ymax = Inf), fill = "lightgrey", alpha = 0.5) +
  geom_line(aes(y = Sprat_Temporal_Double, color = "Sprat Periodic Double Intensity")) +
  geom_line(aes(y = Sprat_Temporal, color = "Sprat Periodic")) + # Added Sprat Temporal line
  geom_line(aes(y = Sprat_Constant, color = "Sprat Constant")) +
  geom_line(aes(y = Sprat_No_Harvest, color = "Sprat No Harvest")) +
  annotate("text", x = 17, y = max(biomass_df_subset$Sprat_Constant, biomass_df_subset$Sprat_Temporal_Double), label = "PH 1", color = "black", vjust = -3) +
  annotate("text", x = 27, y = max(biomass_df_subset$Sprat_Constant, biomass_df_subset$Sprat_Temporal_Double), label = "PH 2", color = "black", vjust = -3) +
  annotate("text", x = 37, y = max(biomass_df_subset$Sprat_Constant, biomass_df_subset$Sprat_Temporal_Double), label = "PH 3", color = "black", vjust = -3) +
  annotate("text", x = 47, y = max(biomass_df_subset$Sprat_Constant, biomass_df_subset$Sprat_Temporal_Double), label = "PH 4", color = "black", vjust = -3) +
  labs(title = "",
       x = "Time Step (years)",
       y = "Biomass (g)",
       color = "Scenario") +
  scale_color_manual(values = c( 
    "Sprat Periodic Double Intensity" = "#619CFF", 
    "Sprat Periodic" = "#00BA38", 
    "Sprat Constant" = "#F8766D", 
    "Sprat No Harvest" = "Dark Grey")) +
  scale_y_continuous(breaks = y_breaks, labels = scales::scientific_format(), limits = Sprat_range) +  # Set custom breaks for y-axis
  scale_x_continuous(breaks = seq(0, 49, by = 2), labels = seq(0, 49, by = 2)) +
  theme_minimal() +
  theme(axis.ticks.y = element_line(color = "black"),  # Add ticks to y-axis
        panel.grid.major.y = element_line(color = "grey"))  # Add major grid lines to y-axis


N.pout_columns <- c("N.pout_Temporal", "N.pout_Temporal_Double", "N.pout_Constant", "N.pout_No_Harvest")


# Calculate the range of N.pout
N.pout_range <- range(biomass_df_subset[, N.pout_columns])

# Calculate breaks for the y-axis
y_breaks <- seq(N.pout_range[1], N.pout_range[2], length.out = 8)

# Plot the biomass of N.pout in different scenarios
ggplot(biomass_df_subset, aes(x = Time_Step)) +
  geom_rect(aes(xmin = 15, xmax = 19, ymin = -Inf, ymax = Inf), fill = "lightgrey", alpha = 0.5) + 
  geom_rect(aes(xmin = 25, xmax = 29, ymin = -Inf, ymax = Inf), fill = "lightgrey", alpha = 0.5) + 
  geom_rect(aes(xmin = 35, xmax = 39, ymin = -Inf, ymax = Inf), fill = "lightgrey", alpha = 0.5) + 
  geom_rect(aes(xmin = 45, xmax = 49, ymin = -Inf, ymax = Inf), fill = "lightgrey", alpha = 0.5) +
  geom_text(aes(x = 15, y = max(N.pout_Constant, N.pout_Temporal_Double), label = "PH 1"), vjust = -0.1, hjust = -0.4, color = "grey40") +
  geom_text(aes(x = 15, y = max(N.pout_Constant, N.pout_Temporal_Double), label = "PH 2"), vjust = -0.1, hjust = -5, color = "grey40") +
  geom_text(aes(x = 15, y = max(N.pout_Constant, N.pout_Temporal_Double), label = "PH 3"), vjust = -0.1, hjust = -9.5, color = "grey40") +
  geom_text(aes(x = 15, y = max(N.pout_Constant, N.pout_Temporal_Double), label = "PH 4"), vjust = -0.1, hjust = -14.2, color = "grey40") +
  geom_line(aes(y = N.pout_Temporal, color = "Periodic")) +
  geom_line(aes(y = N.pout_Temporal_Double, color = "Periodic Double Intensity")) +
  geom_line(aes(y = N.pout_Constant, color = "Constant")) +
  geom_line(aes(y = N.pout_No_Harvest, color = "No Harvest")) +
  labs(title = "",
       x = "Time Step (years)",
       y = "Biomass (g)",
       color = "Scenario") +
  scale_color_manual(values = c("Periodic" = "#00BA38",
                                "Periodic Double Intensity" = "#619CFF",
                                "Constant" = "#F8766D", 
                                "No Harvest" = "Dark Grey"),
                     labels = c("N.pout Periodic", "N.pout Periodic Double Intensity", "N.pout Constant", "N.pout No Harvest")) +
  scale_x_continuous(breaks = seq(0, 49, by = 2), labels = seq(0, 49, by = 2), 
                     name = "Time Step (years)") +  # Set custom breaks and labels for x-axis
  scale_y_continuous(breaks = y_breaks, labels = scales::scientific_format(), limits = N.pout_range) +  # Set custom breaks for y-axis
  
  theme_minimal()



# Define the range of biomass data
Dab_columns <- c("Dab_Temporal", "Dab_Temporal_Double", "Dab_Constant", "Dab_No_Harvest")
# Calculate the range of Dab
Dab_range <- range(biomass_df_subset[, Dab_columns])

# Calculate breaks for the y-axis
y_breaks <- seq(Dab_range[1], Dab_range[2], length.out = 8)

# Plot the biomass of different Dab scenarios with modified legend names
ggplot(biomass_df_subset, aes(x = Time_Step)) +
  geom_rect(aes(xmin = 15, xmax = 19, ymin = -Inf, ymax = Inf), fill = "lightgrey", alpha = 0.5) + # Add shaded rectangle
  geom_rect(aes(xmin = 25, xmax = 29, ymin = -Inf, ymax = Inf), fill = "lightgrey", alpha = 0.5) + 
  geom_rect(aes(xmin = 35, xmax = 39, ymin = -Inf, ymax = Inf), fill = "lightgrey", alpha = 0.5) + 
  geom_rect(aes(xmin = 45, xmax = 49, ymin = -Inf, ymax = Inf), fill = "lightgrey", alpha = 0.5) +
  geom_line(aes(y = Dab_Temporal_Double, color = "Dab Periodic Double Intensity")) +
  geom_line(aes(y = Dab_Temporal, color = "Dab Periodic")) + # Added Dab Temporal line
  geom_line(aes(y = Dab_Constant, color = "Dab Constant")) +
  geom_line(aes(y = Dab_No_Harvest, color = "Dab No Harvest")) +
  annotate("text", x = 17, y = max(biomass_df_subset$Dab_Constant, biomass_df_subset$Dab_Temporal_Double), label = "PH 1", color = "black", vjust = -4) +
  annotate("text", x = 27, y = max(biomass_df_subset$Dab_Constant, biomass_df_subset$Dab_Temporal_Double), label = "PH 2", color = "black", vjust = -4) +
  annotate("text", x = 37, y = max(biomass_df_subset$Dab_Constant, biomass_df_subset$Dab_Temporal_Double), label = "PH 3", color = "black", vjust = -4) +
  annotate("text", x = 47, y = max(biomass_df_subset$Dab_Constant, biomass_df_subset$Dab_Temporal_Double), label = "PH 4", color = "black", vjust = -4) +
  labs(title = "",
       x = "Time Step (years)",
       y = "Biomass (g)",
       color = "Scenario") +
  scale_color_manual(values = c( 
    "Dab Periodic Double Intensity" = "#619CFF", 
    "Dab Periodic" = "#00BA38", 
    "Dab Constant" = "#F8766D", 
    "Dab No Harvest" = "Dark Grey")) +
  scale_y_continuous(breaks = y_breaks, labels = scales::scientific_format(), limits = Dab_range) +  # Set custom breaks for y-axis
  scale_x_continuous(breaks = seq(0, 49, by = 2), labels = seq(0, 49, by = 2)) +
  theme_minimal() +
  theme(axis.ticks.y = element_line(color = "black"),  # Add ticks to y-axis
        panel.grid.major.y = element_line(color = "grey"))  # Add major grid lines to y-axis




## Plotting Intermediate strategists ##
# Define the range of biomass data
Herring_columns <- c("Herring_Temporal", "Herring_Temporal_Double", "Herring_Constant", "Herring_No_Harvest")
# Calculate the range of Herring
Herring_range <- range(biomass_df_subset[, Herring_columns])

# Calculate breaks for the y-axis
y_breaks <- seq(Herring_range[1], Herring_range[2], length.out = 8)

# Plot the biomass of different Herring scenarios with modified legend names
ggplot(biomass_df_subset, aes(x = Time_Step)) +
  geom_rect(aes(xmin = 15, xmax = 19, ymin = -Inf, ymax = Inf), fill = "lightgrey", alpha = 0.5) + # Add shaded rectangle
  geom_rect(aes(xmin = 25, xmax = 29, ymin = -Inf, ymax = Inf), fill = "lightgrey", alpha = 0.5) + 
  geom_rect(aes(xmin = 35, xmax = 39, ymin = -Inf, ymax = Inf), fill = "lightgrey", alpha = 0.5) + 
  geom_rect(aes(xmin = 45, xmax = 49, ymin = -Inf, ymax = Inf), fill = "lightgrey", alpha = 0.5) +
  geom_line(aes(y = Herring_Temporal_Double, color = "Herring Periodic Double Intensity")) +
  geom_line(aes(y = Herring_Temporal, color = "Herring Periodic")) + # Added Herring Temporal line
  geom_line(aes(y = Herring_Constant, color = "Herring Constant")) +
  geom_line(aes(y = Herring_No_Harvest, color = "Herring No Harvest")) +
  annotate("text", x = 17, y = max(biomass_df_subset$Herring_Constant, biomass_df_subset$Herring_Temporal_Double), label = "PH 1", color = "black", vjust = -3) +
  annotate("text", x = 27, y = max(biomass_df_subset$Herring_Constant, biomass_df_subset$Herring_Temporal_Double), label = "PH 2", color = "black", vjust = -3) +
  annotate("text", x = 37, y = max(biomass_df_subset$Herring_Constant, biomass_df_subset$Herring_Temporal_Double), label = "PH 3", color = "black", vjust = -3) +
  annotate("text", x = 47, y = max(biomass_df_subset$Herring_Constant, biomass_df_subset$Herring_Temporal_Double), label = "PH 4", color = "black", vjust = -3) +
  labs(title = "",
       x = "Time Step (years)",
       y = "Biomass (g)",
       color = "Scenario") +
  scale_color_manual(values = c( 
    "Herring Periodic Double Intensity" = "#619CFF", 
    "Herring Periodic" = "#00BA38", 
    "Herring Constant" = "#F8766D", 
    "Herring No Harvest" = "Dark Grey")) +
  scale_y_continuous(breaks = y_breaks, labels = scales::scientific_format(), limits = Herring_range) +  # Set custom breaks for y-axis
  scale_x_continuous(breaks = seq(0, 49, by = 2), labels = seq(0, 49, by = 2)) +
  theme_minimal() +
  theme(axis.ticks.y = element_line(color = "black"),  # Add ticks to y-axis
        panel.grid.major.y = element_line(color = "grey"))  # Add major grid lines to y-axis




# Define the range of biomass data
Whiting_columns <- c("Whiting_Temporal", "Whiting_Temporal_Double", "Whiting_Constant", "Whiting_No_Harvest")
# Calculate the range of Whiting
Whiting_range <- range(biomass_df_subset[, Whiting_columns])

# Calculate breaks for the y-axis
y_breaks <- seq(Whiting_range[1], Whiting_range[2], length.out = 8)

# Plot the biomass of different Whiting scenarios with modified legend names
ggplot(biomass_df_subset, aes(x = Time_Step)) +
  geom_rect(aes(xmin = 15, xmax = 19, ymin = -Inf, ymax = Inf), fill = "lightgrey", alpha = 0.5) + # Add shaded rectangle
  geom_rect(aes(xmin = 25, xmax = 29, ymin = -Inf, ymax = Inf), fill = "lightgrey", alpha = 0.5) + 
  geom_rect(aes(xmin = 35, xmax = 39, ymin = -Inf, ymax = Inf), fill = "lightgrey", alpha = 0.5) + 
  geom_rect(aes(xmin = 45, xmax = 49, ymin = -Inf, ymax = Inf), fill = "lightgrey", alpha = 0.5) +
  geom_line(aes(y = Whiting_Temporal_Double, color = "Whiting Periodic Double Intensity")) +
  geom_line(aes(y = Whiting_Temporal, color = "Whiting Periodic")) + # Added Whiting Temporal line
  geom_line(aes(y = Whiting_Constant, color = "Whiting Constant")) +
  geom_line(aes(y = Whiting_No_Harvest, color = "Whiting No Harvest")) +
  annotate("text", x = 17, y = max(biomass_df_subset$Whiting_Constant, biomass_df_subset$Whiting_Temporal_Double), label = "PH 1", color = "black", vjust = -3) +
  annotate("text", x = 27, y = max(biomass_df_subset$Whiting_Constant, biomass_df_subset$Whiting_Temporal_Double), label = "PH 2", color = "black", vjust = -3) +
  annotate("text", x = 37, y = max(biomass_df_subset$Whiting_Constant, biomass_df_subset$Whiting_Temporal_Double), label = "PH 3", color = "black", vjust = -3) +
  annotate("text", x = 47, y = max(biomass_df_subset$Whiting_Constant, biomass_df_subset$Whiting_Temporal_Double), label = "PH 4", color = "black", vjust = -3) +
  labs(title = "",
       x = "Time Step (years)",
       y = "Biomass (g)",
       color = "Scenario") +
  scale_color_manual(values = c( 
    "Whiting Periodic Double Intensity" = "#619CFF", 
    "Whiting Periodic" = "#00BA38", 
    "Whiting Constant" = "#F8766D", 
    "Whiting No Harvest" = "Dark Grey")) +
  scale_y_continuous(breaks = y_breaks, labels = scales::scientific_format(), limits = Whiting_range) +  # Set custom breaks for y-axis
  scale_x_continuous(breaks = seq(0, 49, by = 2), labels = seq(0, 49, by = 2)) +
  theme_minimal() +
  theme(axis.ticks.y = element_line(color = "black"),  # Add ticks to y-axis
        panel.grid.major.y = element_line(color = "grey"))  # Add major grid lines to y-axis


# Define the range of biomass data
Sole_columns <- c("Sole_Temporal", "Sole_Temporal_Double", "Sole_Constant", "Sole_No_Harvest")
# Calculate the range of Sole
Sole_range <- range(biomass_df_subset[, Sole_columns])

# Calculate breaks for the y-axis
y_breaks <- seq(Sole_range[1], Sole_range[2], length.out = 8)

# Plot the biomass of different Sole scenarios with modified legend names
ggplot(biomass_df_subset, aes(x = Time_Step)) +
  geom_rect(aes(xmin = 15, xmax = 19, ymin = -Inf, ymax = Inf), fill = "lightgrey", alpha = 0.5) + # Add shaded rectangle
  geom_rect(aes(xmin = 25, xmax = 29, ymin = -Inf, ymax = Inf), fill = "lightgrey", alpha = 0.5) + 
  geom_rect(aes(xmin = 35, xmax = 39, ymin = -Inf, ymax = Inf), fill = "lightgrey", alpha = 0.5) + 
  geom_rect(aes(xmin = 45, xmax = 49, ymin = -Inf, ymax = Inf), fill = "lightgrey", alpha = 0.5) +
  geom_line(aes(y = Sole_Temporal_Double, color = "Sole Periodic Double Intensity")) +
  geom_line(aes(y = Sole_Temporal, color = "Sole Periodic")) + # Added Sole Temporal line
  geom_line(aes(y = Sole_Constant, color = "Sole Constant")) +
  geom_line(aes(y = Sole_No_Harvest, color = "Sole No Harvest")) +
  annotate("text", x = 17, y = max(biomass_df_subset$Sole_Constant, biomass_df_subset$Sole_Temporal_Double), label = "PH 1", color = "black", vjust = -3) +
  annotate("text", x = 27, y = max(biomass_df_subset$Sole_Constant, biomass_df_subset$Sole_Temporal_Double), label = "PH 2", color = "black", vjust = -3) +
  annotate("text", x = 37, y = max(biomass_df_subset$Sole_Constant, biomass_df_subset$Sole_Temporal_Double), label = "PH 3", color = "black", vjust = -3) +
  annotate("text", x = 47, y = max(biomass_df_subset$Sole_Constant, biomass_df_subset$Sole_Temporal_Double), label = "PH 4", color = "black", vjust = -3) +
  labs(title = "",
       x = "Time Step (years)",
       y = "Biomass (g)",
       color = "Scenario") +
  scale_color_manual(values = c( 
    "Sole Periodic Double Intensity" = "#619CFF", 
    "Sole Periodic" = "#00BA38", 
    "Sole Constant" = "#F8766D", 
    "Sole No Harvest" = "Dark Grey")) +
  scale_y_continuous(breaks = y_breaks, labels = scales::scientific_format(), limits = Sole_range) +  # Set custom breaks for y-axis
  scale_x_continuous(breaks = seq(0, 49, by = 2), labels = seq(0, 49, by = 2)) +
  theme_minimal() +
  theme(axis.ticks.y = element_line(color = "black"),  # Add ticks to y-axis
        panel.grid.major.y = element_line(color = "grey"))  # Add major grid lines to y-axis



# Define the range of biomass data
Gurnard_columns <- c("Gurnard_Temporal", "Gurnard_Temporal_Double", "Gurnard_Constant", "Gurnard_No_Harvest")
# Calculate the range of Gurnard
Gurnard_range <- range(biomass_df_subset[, Gurnard_columns])

# Calculate breaks for the y-axis
y_breaks <- seq(Gurnard_range[1], Gurnard_range[2], length.out = 8)

# Plot the biomass of different Gurnard scenarios with modified legend names
ggplot(biomass_df_subset, aes(x = Time_Step)) +
  geom_rect(aes(xmin = 15, xmax = 19, ymin = -Inf, ymax = Inf), fill = "lightgrey", alpha = 0.5) + # Add shaded rectangle
  geom_rect(aes(xmin = 25, xmax = 29, ymin = -Inf, ymax = Inf), fill = "lightgrey", alpha = 0.5) + 
  geom_rect(aes(xmin = 35, xmax = 39, ymin = -Inf, ymax = Inf), fill = "lightgrey", alpha = 0.5) + 
  geom_rect(aes(xmin = 45, xmax = 49, ymin = -Inf, ymax = Inf), fill = "lightgrey", alpha = 0.5) +
  geom_line(aes(y = Gurnard_Temporal_Double, color = "Gurnard Periodic Double Intensity")) +
  geom_line(aes(y = Gurnard_Temporal, color = "Gurnard Periodic")) + # Added Gurnard Temporal line
  geom_line(aes(y = Gurnard_Constant, color = "Gurnard Constant")) +
  geom_line(aes(y = Gurnard_No_Harvest, color = "Gurnard No Harvest")) +
  annotate("text", x = 17, y = max(biomass_df_subset$Gurnard_Constant, biomass_df_subset$Gurnard_Temporal_Double), label = "PH 1", color = "black", vjust = -3) +
  annotate("text", x = 27, y = max(biomass_df_subset$Gurnard_Constant, biomass_df_subset$Gurnard_Temporal_Double), label = "PH 2", color = "black", vjust = -3) +
  annotate("text", x = 37, y = max(biomass_df_subset$Gurnard_Constant, biomass_df_subset$Gurnard_Temporal_Double), label = "PH 3", color = "black", vjust = -3) +
  annotate("text", x = 47, y = max(biomass_df_subset$Gurnard_Constant, biomass_df_subset$Gurnard_Temporal_Double), label = "PH 4", color = "black", vjust = -3) +
  labs(title = "",
       x = "Time Step (years)",
       y = "Biomass (g)",
       color = "Scenario") +
  scale_color_manual(values = c( 
    "Gurnard Periodic Double Intensity" = "#619CFF", 
    "Gurnard Periodic" = "#00BA38", 
    "Gurnard Constant" = "#F8766D", 
    "Gurnard No Harvest" = "Dark Grey")) +
  scale_y_continuous(breaks = y_breaks, labels = scales::scientific_format(), limits = Gurnard_range) +  # Set custom breaks for y-axis
  scale_x_continuous(breaks = seq(0, 49, by = 2), labels = seq(0, 49, by = 2)) +
  theme_minimal() +
  theme(axis.ticks.y = element_line(color = "black"),  # Add ticks to y-axis
        panel.grid.major.y = element_line(color = "grey"))  # Add major grid lines to y-axis



# Define the range of biomass data
Haddock_columns <- c("Haddock_Temporal", "Haddock_Temporal_Double", "Haddock_Constant", "Haddock_No_Harvest")
# Calculate the range of Haddock
Haddock_range <- range(biomass_df_subset[, Haddock_columns])

# Calculate breaks for the y-axis
y_breaks <- seq(Haddock_range[1], Haddock_range[2], length.out = 8)

# Plot the biomass of different Haddock scenarios with modified legend names
ggplot(biomass_df_subset, aes(x = Time_Step)) +
  geom_rect(aes(xmin = 15, xmax = 19, ymin = -Inf, ymax = Inf), fill = "lightgrey", alpha = 0.5) + # Add shaded rectangle
  geom_rect(aes(xmin = 25, xmax = 29, ymin = -Inf, ymax = Inf), fill = "lightgrey", alpha = 0.5) + 
  geom_rect(aes(xmin = 35, xmax = 39, ymin = -Inf, ymax = Inf), fill = "lightgrey", alpha = 0.5) + 
  geom_rect(aes(xmin = 45, xmax = 49, ymin = -Inf, ymax = Inf), fill = "lightgrey", alpha = 0.5) +
  geom_line(aes(y = Haddock_Temporal_Double, color = "Haddock Periodic Double Intensity")) +
  geom_line(aes(y = Haddock_Temporal, color = "Haddock Periodic")) + # Added Haddock Temporal line
  geom_line(aes(y = Haddock_Constant, color = "Haddock Constant")) +
  geom_line(aes(y = Haddock_No_Harvest, color = "Haddock No Harvest")) +
  annotate("text", x = 17, y = max(biomass_df_subset$Haddock_Constant, biomass_df_subset$Haddock_Temporal_Double), label = "PH 1", color = "black", vjust = -3) +
  annotate("text", x = 27, y = max(biomass_df_subset$Haddock_Constant, biomass_df_subset$Haddock_Temporal_Double), label = "PH 2", color = "black", vjust = -3) +
  annotate("text", x = 37, y = max(biomass_df_subset$Haddock_Constant, biomass_df_subset$Haddock_Temporal_Double), label = "PH 3", color = "black", vjust = -3) +
  annotate("text", x = 47, y = max(biomass_df_subset$Haddock_Constant, biomass_df_subset$Haddock_Temporal_Double), label = "PH 4", color = "black", vjust = -3) +
  labs(title = "",
       x = "Time Step (years)",
       y = "Biomass (g)",
       color = "Scenario") +
  scale_color_manual(values = c( 
    "Haddock Periodic Double Intensity" = "#619CFF", 
    "Haddock Periodic" = "#00BA38", 
    "Haddock Constant" = "#F8766D", 
    "Haddock No Harvest" = "Dark Grey")) +
  scale_y_continuous(breaks = y_breaks, labels = scales::scientific_format(), limits = Haddock_range) +  # Set custom breaks for y-axis
  scale_x_continuous(breaks = seq(0, 49, by = 2), labels = seq(0, 49, by = 2)) +
  theme_minimal() +
  theme(axis.ticks.y = element_line(color = "black"),  # Add ticks to y-axis
        panel.grid.major.y = element_line(color = "grey"))  # Add major grid lines to y-axis




## Getting predation mortality ##

# will help us see if changes in sizes of Predators specifically cod are causing drops in predation on certain species

# Calculate total predation mortality rate (1/year) on each
# prey species by prey size

pred_mort_27 <- getPredMort(sim_params_temporal_harvest, time_range = 27)

# Calculate predation mortality for the second scenario (time_range = 23)
pred_mort_23 <- getPredMort(sim_params_temporal_harvest, time_range = 23)

# Compare the overall predation mortality values
if (pred_mort_27 > pred_mort_23) {
  cat("Scenario with time_range = 27 has a greater overall predation mortality value.")
} else if (pred_mort_27 < pred_mort_23) {
  cat("Scenario with time_range = 23 has a greater overall predation mortality value.")
} else {
  cat("Both scenarios have the same overall predation mortality value.")
}



library(gridExtra)

# Create plots for each scenario
plot_27_temporal <- plotPredMort(sim_params_temporal_harvest, time_range = 27) +
  labs(title = "A.) Year 27, During Periodic harvest")

plot_27_temporal_double <- plotPredMort(sim_params_temporal_harvest_double, time_range = 27) +
  labs(title = "C.) Year 27, During Double Fishing Intensity Periodic harvest")

plot_23_temporal <- plotPredMort(sim_params_temporal_harvest, time_range = 23) +
  labs(title = "B.) Year 23, Post Periodic Harvest")

plot_23_temporal_double <- plotPredMort(sim_params_temporal_harvest_double, time_range = 23) +
  labs(title = "D.) Year 23, Post Double Fishing Intensity Periodic Harvest")

# Arrange plots on the same panel
grid.arrange(plot_27_temporal, plot_27_temporal_double, plot_23_temporal, plot_23_temporal_double, ncol = 2)

library(gridExtra)

# Create plots for each scenario
plot_constant_harvest <- plotPredMort(sim_params_constant_harvest, time_range = 24:49) +
  labs(title = "A.) Years 24 to 49, Constant Harvest")

plot_no_harvest <- plotPredMort(sim_params_no_harvest, time_range = 24:49) +
  labs(title = "B.) Years 24 to 49, No Harvest")

# Arrange plots on the same panel
grid.arrange(plot_constant_harvest, plot_no_harvest, ncol = 2)


##

library(gridExtra)

# Create plots for each scenario
plot_A <- plotPredMort(sim_params_temporal_harvest, time_range = 25:29) +
  labs(title = "A.) Years 25 to 29, During Periodic Harvest")

plot_B <- plotPredMort(sim_params_temporal_harvest_double, time_range = 25:29) +
  labs(title = "B.) Years 25 to 29, During Double Fishing Intensity Periodic Harvest")

plot_C <- plotPredMort(sim_params_temporal_harvest, time_range = 20:24) +
  labs(title = "C.) Years 20 to 24, Post Periodic Harvest")

plot_D <- plotPredMort(sim_params_temporal_harvest_double, time_range = 20:24) +
  labs(title = "D.) Years 20 to 24, Post Double Fishing Intensity Periodic Harvest")


# Arrange plots on the same panel
grid.arrange(plot_A, plot_B, plot_C, plot_D, ncol = 2)

# Constant
library(gridExtra)

# Create plots for each scenario
plot_A <- plotPredMort(sim_params_constant_harvest, time_range = 1:10) +
  labs(title = "A.) Year 1 to 10, Initial Establishment and Constant Harvest")

plot_B <- plotPredMort(sim_params_no_harvest, time_range = 1:10) +
  labs(title = "B.) Year 1 to 10, Initial Establishment and No Harvest")

# Arrange plots on the same panel
grid.arrange(plot_A, plot_B, ncol = 1)








# # obtaining diet data to infer predation # #


# Extracting the abundance data (n) for all species at a specific time point (time step 15) from the projected simulation object.
# Extracting the prey biomass (n_pp) at the same time point.

# get diet information from this time step
# Returns the rates at which a predator of species i and size w consumes biomass from prey species j
# row corresponds to a predator species, and each column corresponds to a prey species 
# The diet has units of grams/year.




## Diet cod ##
getDiet(params_constant_harvest)

# Initialize an empty list to store diet rates for each year
diet_rates_constant <- list()

# Loop over each year (time step)
for (year in 1:50) {
  # Extract biomass values for the current year
  n <- sim_params_constant_harvest@n[year,,]
  n_pp <- sim_params_constant_harvest@n_pp[year,]
  
  # Get diet information for the current year and store it in the list
  diet_rates_constant[[year]] <- getDiet(params_constant_harvest, n, n_pp, proportion = FALSE)
}

#[1] "Species 1 : Sprat"  done
#[1] "Species 2 : Sandeel" 
#[1] "Species 3 : N.pout" 
#[1] "Species 4 : Herring" # done
#[1] "Species 5 : Dab" # done
#[1] "Species 6 : Whiting" # done
#[1] "Species 7 : Sole" 
#[1] "Species 8 : Gurnard" 
#[1] "Species 9 : Plaice"
#[1] "Species 10 : Haddock" # done
#[1] "Species 11 : Cod"
#[1] "Species 12 : Saithe"
#  it extracts the first row of the data frame stored in diet_rates_constant for the corresponding year, 
# assuming that the first row contains data related to the predator cod

# Initialize an empty list to store predator species for each year

predator_cod <- list()

# Loop over each year
for (year in 1:50) {
  # Extract predator Cod data for the current year
  predator_cod[[year]] <- diet_rates_constant[[year]][11,,]
}
predator_cod
str(predator_cod)
# Example of accessing the predator Cod data for year 14
#The first dimension (w) represents different weight categories of the predator Cod.
#The second dimension (prey) represents different prey species that the predator Cod is consuming

year_1_predator_cod <- predator_cod[[1]]
year_2_predator_cod <- predator_cod[[2]]
year_3_predator_cod <- predator_cod[[3]]
year_4_predator_cod <- predator_cod[[4]]
year_5_predator_cod <- predator_cod[[5]]
year_6_predator_cod <- predator_cod[[6]]
year_7_predator_cod <- predator_cod[[7]]
year_8_predator_cod <- predator_cod[[8]]
year_9_predator_cod <- predator_cod[[9]]
year_10_predator_cod <- predator_cod[[10]]
year_11_predator_cod <- predator_cod[[11]]
year_12_predator_cod <- predator_cod[[12]]
year_13_predator_cod <- predator_cod[[13]]
year_14_predator_cod <- predator_cod[[14]]
year_15_predator_cod <- predator_cod[[15]]
year_16_predator_cod <- predator_cod[[16]]
year_17_predator_cod <- predator_cod[[17]]
year_18_predator_cod <- predator_cod[[18]]
year_19_predator_cod <- predator_cod[[19]]
year_20_predator_cod <- predator_cod[[20]]
year_21_predator_cod <- predator_cod[[21]]
year_22_predator_cod <- predator_cod[[22]]
year_23_predator_cod <- predator_cod[[23]]
year_24_predator_cod <- predator_cod[[24]]
year_25_predator_cod <- predator_cod[[25]]
year_26_predator_cod <- predator_cod[[26]]
year_27_predator_cod <- predator_cod[[27]]
year_28_predator_cod <- predator_cod[[28]]
year_29_predator_cod <- predator_cod[[29]]
year_30_predator_cod <- predator_cod[[30]]
year_31_predator_cod <- predator_cod[[31]]
year_32_predator_cod <- predator_cod[[32]]
year_33_predator_cod <- predator_cod[[33]]
year_34_predator_cod <- predator_cod[[34]]
year_35_predator_cod <- predator_cod[[35]]
year_36_predator_cod <- predator_cod[[36]]
year_37_predator_cod <- predator_cod[[37]]
year_38_predator_cod <- predator_cod[[38]]
year_39_predator_cod <- predator_cod[[39]]
year_40_predator_cod <- predator_cod[[40]]
year_41_predator_cod <- predator_cod[[41]]
year_42_predator_cod <- predator_cod[[42]]
year_43_predator_cod <- predator_cod[[43]]
year_44_predator_cod <- predator_cod[[44]]
year_45_predator_cod <- predator_cod[[45]]
year_46_predator_cod <- predator_cod[[46]]
year_47_predator_cod <- predator_cod[[47]]
year_48_predator_cod <- predator_cod[[48]]
year_49_predator_cod <- predator_cod[[49]]
year_50_predator_cod <- predator_cod[[50]]



# Write data for year 1 to CSV file
write.csv(year_1_predator_cod, file = "year_1_predator_cod_v9.csv", row.names = FALSE)

# Write data for year 2 to CSV file
write.csv(year_2_predator_cod, file = "year_2_predator_cod_v9.csv", row.names = FALSE)

# Write data for year 3 to CSV file
write.csv(year_3_predator_cod, file = "year_3_predator_cod_v9.csv", row.names = FALSE)

# Write data for year 4 to CSV file
write.csv(year_4_predator_cod, file = "year_4_predator_cod_v9.csv", row.names = FALSE)

# Write data for year 5 to CSV file
write.csv(year_5_predator_cod, file = "year_5_predator_cod_v9.csv", row.names = FALSE)

# Write data for year 6 to CSV file
write.csv(year_6_predator_cod, file = "year_6_predator_cod_v9.csv", row.names = FALSE)

# Write data for year 7 to CSV file
write.csv(year_7_predator_cod, file = "year_7_predator_cod_v9.csv", row.names = FALSE)

# Write data for year 8 to CSV file
write.csv(year_8_predator_cod, file = "year_8_predator_cod_v9.csv", row.names = FALSE)

# Write data for year 9 to CSV file
write.csv(year_9_predator_cod, file = "year_9_predator_cod_v9.csv", row.names = FALSE)

# Write data for year 10 to CSV file
write.csv(year_10_predator_cod, file = "year_10_predator_cod_v9.csv", row.names = FALSE)

# Write data for year 11 to CSV file
write.csv(year_11_predator_cod, file = "year_11_predator_cod_v9.csv", row.names = FALSE)

# Write data for year 12 to CSV file
write.csv(year_12_predator_cod, file = "year_12_predator_cod_v9.csv", row.names = FALSE)

# Write data for year 13 to CSV file
write.csv(year_13_predator_cod, file = "year_13_predator_cod_v9.csv", row.names = FALSE)


## Write data for year 14 to CSV file
write.csv(year_14_predator_cod, file = "year_14_predator_cod_v9.csv", row.names = FALSE)

# Write data for year 15 to CSV file
write.csv(year_15_predator_cod, file = "year_15_predator_cod_v9.csv", row.names = FALSE)

# Write data for year 16 to CSV file
write.csv(year_16_predator_cod, file = "year_16_predator_cod_v9.csv", row.names = FALSE)

# Write data for year 17 to CSV file
write.csv(year_17_predator_cod, file = "year_17_predator_cod_v9.csv", row.names = FALSE)

# Write data for year 18 to CSV file
write.csv(year_18_predator_cod, file = "year_18_predator_cod_v9.csv", row.names = FALSE)

# Write data for year 19 to CSV file
write.csv(year_19_predator_cod, file = "year_19_predator_cod_v9.csv", row.names = FALSE)

# Write data for year 20 to CSV file
write.csv(year_20_predator_cod, file = "year_20_predator_cod_v9.csv", row.names = FALSE)

# Write data for year 21 to CSV file
write.csv(year_21_predator_cod, file = "year_21_predator_cod_v9.csv", row.names = FALSE)

# Write data for year 22 to CSV file
write.csv(year_22_predator_cod, file = "year_22_predator_cod_v9.csv", row.names = FALSE)

# Write data for year 23 to CSV file
write.csv(year_23_predator_cod, file = "year_23_predator_cod_v9.csv", row.names = FALSE)

# Write data for year 24 to CSV file
write.csv(year_24_predator_cod, file = "year_24_predator_cod_v9.csv", row.names = FALSE)

# Write data for year 25 to CSV file
write.csv(year_25_predator_cod, file = "year_25_predator_cod_v9.csv", row.names = FALSE)

# Write data for year 26 to CSV file
write.csv(year_26_predator_cod, file = "year_26_predator_cod_v9.csv", row.names = FALSE)

# Write data for year 27 to CSV file
write.csv(year_27_predator_cod, file = "year_27_predator_cod_v9.csv", row.names = FALSE)

# Write data for year 28 to CSV file
write.csv(year_28_predator_cod, file = "year_28_predator_cod_v9.csv", row.names = FALSE)

# Write data for year 29 to CSV file
write.csv(year_29_predator_cod, file = "year_29_predator_cod_v9.csv", row.names = FALSE)

# Write data for year 30 to CSV file
write.csv(year_30_predator_cod, file = "year_30_predator_cod_v9.csv", row.names = FALSE)

# Write data for year 31 to CSV file
write.csv(year_31_predator_cod, file = "year_31_predator_cod_v9.csv", row.names = FALSE)

# Write data for year 32 to CSV file
write.csv(year_32_predator_cod, file = "year_32_predator_cod_v9.csv", row.names = FALSE)

# Write data for year 33 to CSV file
write.csv(year_33_predator_cod, file = "year_33_predator_cod_v9.csv", row.names = FALSE)

# Write data for year 34 to CSV file
write.csv(year_34_predator_cod, file = "year_34_predator_cod_v9.csv", row.names = FALSE)

# Write data for year 35 to CSV file
write.csv(year_35_predator_cod, file = "year_35_predator_cod_v9.csv", row.names = FALSE)

# Write data for year 36 to CSV file
write.csv(year_36_predator_cod, file = "year_36_predator_cod_v9.csv", row.names = FALSE)

# Write data for year 37 to CSV file
write.csv(year_37_predator_cod, file = "year_37_predator_cod_v9.csv", row.names = FALSE)

# Write data for year 38 to CSV file
write.csv(year_38_predator_cod, file = "year_38_predator_cod_v9.csv", row.names = FALSE)

# Write data for year 39 to CSV file
write.csv(year_39_predator_cod, file = "year_39_predator_cod_v9.csv", row.names = FALSE)

# Write data for year 40 to CSV file
write.csv(year_40_predator_cod, file = "year_40_predator_cod_v9.csv", row.names = FALSE)

# Write data for year 41 to CSV file
write.csv(year_41_predator_cod, file = "year_41_predator_cod_v9.csv", row.names = FALSE)

# Write data for year 42 to CSV file
write.csv(year_42_predator_cod, file = "year_42_predator_cod_v9.csv", row.names = FALSE)

# Write data for year 43 to CSV file
write.csv(year_43_predator_cod, file = "year_43_predator_cod_v9.csv", row.names = FALSE)

# Write data for year 44 to CSV file
write.csv(year_44_predator_cod, file = "year_44_predator_cod_v9.csv", row.names = FALSE)

# Write data for year 45 to CSV file
write.csv(year_45_predator_cod, file = "year_45_predator_cod_v9.csv", row.names = FALSE)

# Write data for year 46 to CSV file
write.csv(year_46_predator_cod, file = "year_46_predator_cod_v9.csv", row.names = FALSE)

# Write data for year 47 to CSV file
write.csv(year_47_predator_cod, file = "year_47_predator_cod_v9.csv", row.names = FALSE)

# Write data for year 48 to CSV file
write.csv(year_48_predator_cod, file = "year_48_predator_cod_v9.csv", row.names = FALSE)

# Write data for year 49 to CSV file
write.csv(year_49_predator_cod, file = "year_49_predator_cod_v9.csv", row.names = FALSE)

# Write data for year 50 to CSV file
write.csv(year_50_predator_cod, file = "year_50_predator_cod_v9.csv", row.names = FALSE)


# Read data for year 1 from CSV file
year_1_cod_diet_size_class <- read.csv("year_1_predator_cod_v9.csv")

# Read data for year 2 from CSV file
year_2_cod_diet_size_class <- read.csv("year_2_predator_cod_v9.csv")

# Read data for year 3 from CSV file
year_3_cod_diet_size_class <- read.csv("year_3_predator_cod_v9.csv")

# Read data for year 4 from CSV file
year_4_cod_diet_size_class <- read.csv("year_4_predator_cod_v9.csv")

# Read data for year 5 from CSV file
year_5_cod_diet_size_class <- read.csv("year_5_predator_cod_v9.csv")

# Read data for year 6 from CSV file
year_6_cod_diet_size_class <- read.csv("year_6_predator_cod_v9.csv")

# Read data for year 7 from CSV file
year_7_cod_diet_size_class <- read.csv("year_7_predator_cod_v9.csv")

# Read data for year 8 from CSV file
year_8_cod_diet_size_class <- read.csv("year_8_predator_cod_v9.csv")

# Read data for year 9 from CSV file
year_9_cod_diet_size_class <- read.csv("year_9_predator_cod_v9.csv")

# Read data for year 10 from CSV file
year_10_cod_diet_size_class <- read.csv("year_10_predator_cod_v9.csv")

# Read data for year 11 from CSV file
year_11_cod_diet_size_class <- read.csv("year_11_predator_cod_v9.csv")

# Read data for year 12 from CSV file
year_12_cod_diet_size_class <- read.csv("year_12_predator_cod_v9.csv")

# Read data for year 13 from CSV file
year_13_cod_diet_size_class <- read.csv("year_13_predator_cod_v9.csv")

# Read data for year 14 from CSV file
year_14_cod_diet_size_class <- read.csv("year_14_predator_cod_v9.csv")
year_14_cod_diet_size_class

# Read data for year 15 from CSV file
year_15_cod_diet_size_class <- read.csv("year_15_predator_cod_v9.csv")

# Read data for year 16 from CSV file
year_16_cod_diet_size_class <- read.csv("year_16_predator_cod_v9.csv")

# Read data for year 17 from CSV file
year_17_cod_diet_size_class <- read.csv("year_17_predator_cod_v9.csv")

# Read data for year 18 from CSV file
year_18_cod_diet_size_class <- read.csv("year_18_predator_cod_v9.csv")

# Read data for year 19 from CSV file
year_19_cod_diet_size_class <- read.csv("year_19_predator_cod_v9.csv")

# Read data for year 20 from CSV file
year_20_cod_diet_size_class <- read.csv("year_20_predator_cod_v9.csv")

# Read data for year 21 from CSV file
year_21_cod_diet_size_class <- read.csv("year_21_predator_cod_v9.csv")

# Read data for year 22 from CSV file
year_22_cod_diet_size_class <- read.csv("year_22_predator_cod_v9.csv")

# Read data for year 23 from CSV file
year_23_cod_diet_size_class <- read.csv("year_23_predator_cod_v9.csv")

# Read data for year 24 from CSV file
year_24_cod_diet_size_class <- read.csv("year_24_predator_cod_v9.csv")

# Read data for year 25 from CSV file
year_25_cod_diet_size_class <- read.csv("year_25_predator_cod_v9.csv")

# Read data for year 26 from CSV file
year_26_cod_diet_size_class <- read.csv("year_26_predator_cod_v9.csv")

# Read data for year 27 from CSV file
year_27_cod_diet_size_class <- read.csv("year_27_predator_cod_v9.csv")

# Read data for year 28 from CSV file
year_28_cod_diet_size_class <- read.csv("year_28_predator_cod_v9.csv")

# Read data for year 29 from CSV file
year_29_cod_diet_size_class <- read.csv("year_29_predator_cod_v9.csv")

# Read data for year 30 from CSV file
year_30_cod_diet_size_class <- read.csv("year_30_predator_cod_v9.csv")

# Read data for year 31 from CSV file
year_31_cod_diet_size_class <- read.csv("year_31_predator_cod_v9.csv")

# Read data for year 32 from CSV file
year_32_cod_diet_size_class <- read.csv("year_32_predator_cod_v9.csv")

# Read data for year 33 from CSV file
year_33_cod_diet_size_class <- read.csv("year_33_predator_cod_v9.csv")

# Read data for year 34 from CSV file
year_34_cod_diet_size_class <- read.csv("year_34_predator_cod_v9.csv")

# Read data for year 35 from CSV file
year_35_cod_diet_size_class <- read.csv("year_35_predator_cod_v9.csv")

# Read data for year 36 from CSV file
year_36_cod_diet_size_class <- read.csv("year_36_predator_cod_v9.csv")

# Read data for year 37 from CSV file
year_37_cod_diet_size_class <- read.csv("year_37_predator_cod_v9.csv")

# Read data for year 38 from CSV file
year_38_cod_diet_size_class <- read.csv("year_38_predator_cod_v9.csv")

# Read data for year 39 from CSV file
year_39_cod_diet_size_class <- read.csv("year_39_predator_cod_v9.csv")

# Read data for year 40 from CSV file
year_40_cod_diet_size_class <- read.csv("year_40_predator_cod_v9.csv")

# Read data for year 41 from CSV file
year_41_cod_diet_size_class <- read.csv("year_41_predator_cod_v9.csv")

# Read data for year 42 from CSV file
year_42_cod_diet_size_class <- read.csv("year_42_predator_cod_v9.csv")

# Read data for year 43 from CSV file
year_43_cod_diet_size_class <- read.csv("year_43_predator_cod_v9.csv")

# Read data for year 44 from CSV file
year_44_cod_diet_size_class <- read.csv("year_44_predator_cod_v9.csv")
# Read data for year 45 from CSV file
year_45_cod_diet_size_class <- read.csv("year_45_predator_cod_v9.csv")

# Read data for year 46 from CSV file
year_46_cod_diet_size_class <- read.csv("year_46_predator_cod_v9.csv")

# Read data for year 47 from CSV file
year_47_cod_diet_size_class <- read.csv("year_47_predator_cod_v9.csv")

# Read data for year 48 from CSV file
year_48_cod_diet_size_class <- read.csv("year_48_predator_cod_v9.csv")

# Read data for year 49 from CSV file
year_49_cod_diet_size_class <- read.csv("year_49_predator_cod_v9.csv")

# Read data for year 50 from CSV file
year_50_cod_diet_size_class <- read.csv("year_50_predator_cod_v9.csv")


# Add Year column to each dataframe
year_1_cod_diet_size_class$Year <- 1
year_2_cod_diet_size_class$Year <- 2
year_3_cod_diet_size_class$Year <- 3
year_4_cod_diet_size_class$Year <- 4
year_5_cod_diet_size_class$Year <- 5
year_6_cod_diet_size_class$Year <- 6
year_7_cod_diet_size_class$Year <- 7
year_8_cod_diet_size_class$Year <- 8
year_9_cod_diet_size_class$Year <- 9
year_10_cod_diet_size_class$Year <- 10
year_11_cod_diet_size_class$Year <- 11
year_12_cod_diet_size_class$Year <- 12
year_13_cod_diet_size_class$Year <- 13
year_14_cod_diet_size_class$Year <- 14
year_15_cod_diet_size_class$Year <- 15
year_16_cod_diet_size_class$Year <- 16
year_17_cod_diet_size_class$Year <- 17
year_18_cod_diet_size_class$Year <- 18
year_19_cod_diet_size_class$Year <- 19
year_20_cod_diet_size_class$Year <- 20
year_21_cod_diet_size_class$Year <- 21
year_22_cod_diet_size_class$Year <- 22
year_23_cod_diet_size_class$Year <- 23
year_24_cod_diet_size_class$Year <- 24
year_25_cod_diet_size_class$Year <- 25
year_26_cod_diet_size_class$Year <- 26
year_27_cod_diet_size_class$Year <- 27
year_28_cod_diet_size_class$Year <- 28
year_29_cod_diet_size_class$Year <- 29
year_30_cod_diet_size_class$Year <- 30
year_31_cod_diet_size_class$Year <- 31
year_32_cod_diet_size_class$Year <- 32
year_33_cod_diet_size_class$Year <- 33
year_34_cod_diet_size_class$Year <- 34
year_35_cod_diet_size_class$Year <- 35
year_36_cod_diet_size_class$Year <- 36
year_37_cod_diet_size_class$Year <- 37
year_38_cod_diet_size_class$Year <- 38
year_39_cod_diet_size_class$Year <- 39
year_40_cod_diet_size_class$Year <- 40
year_41_cod_diet_size_class$Year <- 41
year_42_cod_diet_size_class$Year <- 42
year_43_cod_diet_size_class$Year <- 43
year_44_cod_diet_size_class$Year <- 44
year_45_cod_diet_size_class$Year <- 45
year_46_cod_diet_size_class$Year <- 46
year_47_cod_diet_size_class$Year <- 47
year_48_cod_diet_size_class$Year <- 48
year_49_cod_diet_size_class$Year <- 49
year_50_cod_diet_size_class$Year <- 50


# Combine all dataframes into one
combined_data_cod_diet_size_class <- rbind(
  year_1_cod_diet_size_class,
  year_2_cod_diet_size_class,
  year_3_cod_diet_size_class,
  year_4_cod_diet_size_class,
  year_5_cod_diet_size_class,
  year_6_cod_diet_size_class,
  year_7_cod_diet_size_class,
  year_8_cod_diet_size_class,
  year_9_cod_diet_size_class,
  year_10_cod_diet_size_class,
  year_11_cod_diet_size_class,
  year_12_cod_diet_size_class,
  year_13_cod_diet_size_class,
  year_14_cod_diet_size_class,
  year_15_cod_diet_size_class,
  year_16_cod_diet_size_class,
  year_17_cod_diet_size_class,
  year_18_cod_diet_size_class,
  year_19_cod_diet_size_class,
  year_20_cod_diet_size_class,
  year_21_cod_diet_size_class,
  year_22_cod_diet_size_class,
  year_23_cod_diet_size_class,
  year_24_cod_diet_size_class,
  year_25_cod_diet_size_class,
  year_26_cod_diet_size_class,
  year_27_cod_diet_size_class,
  year_28_cod_diet_size_class,
  year_29_cod_diet_size_class,
  year_30_cod_diet_size_class,
  year_31_cod_diet_size_class,
  year_32_cod_diet_size_class,
  year_33_cod_diet_size_class,
  year_34_cod_diet_size_class,
  year_35_cod_diet_size_class,
  year_36_cod_diet_size_class,
  year_37_cod_diet_size_class,
  year_38_cod_diet_size_class,
  year_39_cod_diet_size_class,
  year_40_cod_diet_size_class,
  year_41_cod_diet_size_class,
  year_42_cod_diet_size_class,
  year_43_cod_diet_size_class,
  year_44_cod_diet_size_class,
  year_45_cod_diet_size_class,
  year_46_cod_diet_size_class,
  year_47_cod_diet_size_class,
  year_48_cod_diet_size_class,
  year_49_cod_diet_size_class,
  year_50_cod_diet_size_class
)


write.csv(combined_data_cod_diet_size_class, file = "cod_diet_size_class_all_yearsV4.csv", row.names = FALSE)

# add species names in excel
cod_diet_size_class_all_yearsV1 <- read.csv("cod_diet_size_class_all_yearsV4.csv")
cod_diet_size_class_all_yearsV1



# Initialize an empty list to store extracted data for each year
cod_abundance_year1 <- plotSpectra(sim_params_constant_harvest, time_range = 1, species = c("Cod"), return_data = TRUE)
cod_abundance_year2 <- plotSpectra(sim_params_constant_harvest, time_range = 2, species = c("Cod"), return_data = TRUE)
cod_abundance_year3 <- plotSpectra(sim_params_constant_harvest, time_range = 3, species = c("Cod"), return_data = TRUE)
cod_abundance_year4 <- plotSpectra(sim_params_constant_harvest, time_range = 4, species = c("Cod"), return_data = TRUE)
cod_abundance_year5 <- plotSpectra(sim_params_constant_harvest, time_range = 5, species = c("Cod"), return_data = TRUE)
cod_abundance_year6 <- plotSpectra(sim_params_constant_harvest, time_range = 6, species = c("Cod"), return_data = TRUE)
cod_abundance_year7 <- plotSpectra(sim_params_constant_harvest, time_range = 7, species = c("Cod"), return_data = TRUE)
cod_abundance_year8 <- plotSpectra(sim_params_constant_harvest, time_range = 8, species = c("Cod"), return_data = TRUE)
cod_abundance_year9 <- plotSpectra(sim_params_constant_harvest, time_range = 9, species = c("Cod"), return_data = TRUE)
cod_abundance_year10 <- plotSpectra(sim_params_constant_harvest, time_range = 10, species = c("Cod"), return_data = TRUE)
cod_abundance_year11 <- plotSpectra(sim_params_constant_harvest, time_range = 11, species = c("Cod"), return_data = TRUE)
cod_abundance_year12 <- plotSpectra(sim_params_constant_harvest, time_range = 12, species = c("Cod"), return_data = TRUE)
cod_abundance_year13 <- plotSpectra(sim_params_constant_harvest, time_range = 13, species = c("Cod"), return_data = TRUE)
cod_abundance_year14 <- plotSpectra(sim_params_constant_harvest, time_range = 14, species = c("Cod"), return_data = TRUE)
cod_abundance_year15 <- plotSpectra(sim_params_constant_harvest, time_range = 15, species = c("Cod"), return_data = TRUE)
cod_abundance_year16 <- plotSpectra(sim_params_constant_harvest, time_range = 16, species = c("Cod"), return_data = TRUE)
cod_abundance_year17 <- plotSpectra(sim_params_constant_harvest, time_range = 17, species = c("Cod"), return_data = TRUE)
cod_abundance_year18 <- plotSpectra(sim_params_constant_harvest, time_range = 18, species = c("Cod"), return_data = TRUE)
cod_abundance_year19 <- plotSpectra(sim_params_constant_harvest, time_range = 19, species = c("Cod"), return_data = TRUE)
cod_abundance_year20 <- plotSpectra(sim_params_constant_harvest, time_range = 20, species = c("Cod"), return_data = TRUE)
cod_abundance_year21 <- plotSpectra(sim_params_constant_harvest, time_range = 21, species = c("Cod"), return_data = TRUE)
cod_abundance_year22 <- plotSpectra(sim_params_constant_harvest, time_range = 22, species = c("Cod"), return_data = TRUE)
cod_abundance_year23 <- plotSpectra(sim_params_constant_harvest, time_range = 23, species = c("Cod"), return_data = TRUE)
cod_abundance_year24 <- plotSpectra(sim_params_constant_harvest, time_range = 24, species = c("Cod"), return_data = TRUE)
cod_abundance_year25 <- plotSpectra(sim_params_constant_harvest, time_range = 25, species = c("Cod"), return_data = TRUE)
cod_abundance_year26 <- plotSpectra(sim_params_constant_harvest, time_range = 26, species = c("Cod"), return_data = TRUE)
cod_abundance_year27 <- plotSpectra(sim_params_constant_harvest, time_range = 27, species = c("Cod"), return_data = TRUE)
cod_abundance_year28 <- plotSpectra(sim_params_constant_harvest, time_range = 28, species = c("Cod"), return_data = TRUE)
cod_abundance_year29 <- plotSpectra(sim_params_constant_harvest, time_range = 29, species = c("Cod"), return_data = TRUE)
cod_abundance_year30 <- plotSpectra(sim_params_constant_harvest, time_range = 30, species = c("Cod"), return_data = TRUE)
cod_abundance_year31 <- plotSpectra(sim_params_constant_harvest, time_range = 31, species = c("Cod"), return_data = TRUE)
cod_abundance_year32 <- plotSpectra(sim_params_constant_harvest, time_range = 32, species = c("Cod"), return_data = TRUE)
cod_abundance_year33 <- plotSpectra(sim_params_constant_harvest, time_range = 33, species = c("Cod"), return_data = TRUE)
cod_abundance_year34 <- plotSpectra(sim_params_constant_harvest, time_range = 34, species = c("Cod"), return_data = TRUE)
cod_abundance_year35 <- plotSpectra(sim_params_constant_harvest, time_range = 35, species = c("Cod"), return_data = TRUE)
cod_abundance_year36 <- plotSpectra(sim_params_constant_harvest, time_range = 36, species = c("Cod"), return_data = TRUE)
cod_abundance_year37 <- plotSpectra(sim_params_constant_harvest, time_range = 37, species = c("Cod"), return_data = TRUE)
cod_abundance_year38 <- plotSpectra(sim_params_constant_harvest, time_range = 38, species = c("Cod"), return_data = TRUE)
cod_abundance_year39 <- plotSpectra(sim_params_constant_harvest, time_range = 39, species = c("Cod"), return_data = TRUE)
cod_abundance_year40 <- plotSpectra(sim_params_constant_harvest, time_range = 40, species = c("Cod"), return_data = TRUE)
cod_abundance_year41 <- plotSpectra(sim_params_constant_harvest, time_range = 41, species = c("Cod"), return_data = TRUE)
cod_abundance_year42 <- plotSpectra(sim_params_constant_harvest, time_range = 42, species = c("Cod"), return_data = TRUE)
cod_abundance_year43 <- plotSpectra(sim_params_constant_harvest, time_range = 43, species = c("Cod"), return_data = TRUE)
cod_abundance_year44 <- plotSpectra(sim_params_constant_harvest, time_range = 44, species = c("Cod"), return_data = TRUE)
cod_abundance_year45 <- plotSpectra(sim_params_constant_harvest, time_range = 45, species = c("Cod"), return_data = TRUE)
cod_abundance_year46 <- plotSpectra(sim_params_constant_harvest, time_range = 46, species = c("Cod"), return_data = TRUE)
cod_abundance_year47 <- plotSpectra(sim_params_constant_harvest, time_range = 47, species = c("Cod"), return_data = TRUE)
cod_abundance_year48 <- plotSpectra(sim_params_constant_harvest, time_range = 48, species = c("Cod"), return_data = TRUE)
cod_abundance_year49 <- plotSpectra(sim_params_constant_harvest, time_range = 49, species = c("Cod"), return_data = TRUE)
cod_abundance_year50 <- plotSpectra(sim_params_constant_harvest, time_range = 50, species = c("Cod"), return_data = TRUE)


cod_abundance_year1$Year <- 1
cod_abundance_year2$Year <- 2
cod_abundance_year3$Year <- 3
cod_abundance_year4$Year <- 4
cod_abundance_year5$Year <- 5
cod_abundance_year6$Year <- 6
cod_abundance_year7$Year <- 7
cod_abundance_year8$Year <- 8
cod_abundance_year9$Year <- 9
cod_abundance_year10$Year <- 10
cod_abundance_year11$Year <- 11
cod_abundance_year12$Year <- 12
cod_abundance_year13$Year <- 13
cod_abundance_year14$Year <- 14
cod_abundance_year15$Year <- 15
cod_abundance_year16$Year <- 16
cod_abundance_year17$Year <- 17
cod_abundance_year18$Year <- 18
cod_abundance_year19$Year <- 19
cod_abundance_year20$Year <- 20
cod_abundance_year21$Year <- 21
cod_abundance_year22$Year <- 22
cod_abundance_year23$Year <- 23
cod_abundance_year24$Year <- 24
cod_abundance_year25$Year <- 25
cod_abundance_year26$Year <- 26
cod_abundance_year27$Year <- 27
cod_abundance_year28$Year <- 28
cod_abundance_year29$Year <- 29
cod_abundance_year30$Year <- 30
cod_abundance_year31$Year <- 31
cod_abundance_year32$Year <- 32
cod_abundance_year33$Year <- 33
cod_abundance_year34$Year <- 34
cod_abundance_year35$Year <- 35
cod_abundance_year36$Year <- 36
cod_abundance_year37$Year <- 37
cod_abundance_year38$Year <- 38
cod_abundance_year39$Year <- 39
cod_abundance_year40$Year <- 40
cod_abundance_year41$Year <- 41
cod_abundance_year42$Year <- 42
cod_abundance_year43$Year <- 43
cod_abundance_year44$Year <- 44
cod_abundance_year45$Year <- 45
cod_abundance_year46$Year <- 46
cod_abundance_year47$Year <- 47
cod_abundance_year48$Year <- 48
cod_abundance_year49$Year <- 49
cod_abundance_year50$Year <- 50


# Combine all the data frames into a single data set
combined_cod_abundance <- rbind(
  cod_abundance_year1,
  cod_abundance_year2,
  cod_abundance_year3,
  cod_abundance_year4,
  cod_abundance_year5,
  cod_abundance_year6,
  cod_abundance_year7,
  cod_abundance_year8,
  cod_abundance_year9,
  cod_abundance_year10,
  cod_abundance_year11,
  cod_abundance_year12,
  cod_abundance_year13,
  cod_abundance_year14,
  cod_abundance_year15,
  cod_abundance_year16,
  cod_abundance_year17,
  cod_abundance_year18,
  cod_abundance_year19,
  cod_abundance_year20,
  cod_abundance_year21,
  cod_abundance_year22,
  cod_abundance_year23,
  cod_abundance_year24,
  cod_abundance_year25,
  cod_abundance_year26,
  cod_abundance_year27,
  cod_abundance_year28,
  cod_abundance_year29,
  cod_abundance_year30,
  cod_abundance_year31,
  cod_abundance_year32,
  cod_abundance_year33,
  cod_abundance_year34,
  cod_abundance_year35,
  cod_abundance_year36,
  cod_abundance_year37,
  cod_abundance_year38,
  cod_abundance_year39,
  cod_abundance_year40,
  cod_abundance_year41,
  cod_abundance_year42,
  cod_abundance_year43,
  cod_abundance_year44,
  cod_abundance_year45,
  cod_abundance_year46,
  cod_abundance_year47,
  cod_abundance_year48,
  cod_abundance_year49,
  cod_abundance_year50
)


combined_cod_abundance <- combined_cod_abundance[combined_cod_abundance$Species != "Resource", ]


write.csv(combined_cod_abundance, file = "cod_psd_constantv3.csv", row.names = FALSE)

constant_to_be_mean_data_cod <- read.csv("cod_diet_size_class_all_yearsV4.csv")
constant_to_be_mean_data_cod


year_1_values <- constant_to_be_mean_data_cod[constant_to_be_mean_data_cod$Year == 1, ]
year_2_values <- constant_to_be_mean_data_cod[constant_to_be_mean_data_cod$Year == 2, ]
year_3_values <- constant_to_be_mean_data_cod[constant_to_be_mean_data_cod$Year == 3, ]
year_4_values <- constant_to_be_mean_data_cod[constant_to_be_mean_data_cod$Year == 4, ]
year_5_values <- constant_to_be_mean_data_cod[constant_to_be_mean_data_cod$Year == 5, ]
year_6_values <- constant_to_be_mean_data_cod[constant_to_be_mean_data_cod$Year == 6, ]
year_7_values <- constant_to_be_mean_data_cod[constant_to_be_mean_data_cod$Year == 7, ]
year_8_values <- constant_to_be_mean_data_cod[constant_to_be_mean_data_cod$Year == 8, ]
year_9_values <- constant_to_be_mean_data_cod[constant_to_be_mean_data_cod$Year == 9, ]
year_10_values <- constant_to_be_mean_data_cod[constant_to_be_mean_data_cod$Year == 10, ]
year_11_values <- constant_to_be_mean_data_cod[constant_to_be_mean_data_cod$Year == 11, ]
year_12_values <- constant_to_be_mean_data_cod[constant_to_be_mean_data_cod$Year == 12, ]
year_13_values <- constant_to_be_mean_data_cod[constant_to_be_mean_data_cod$Year == 13, ]
year_14_values <- constant_to_be_mean_data_cod[constant_to_be_mean_data_cod$Year == 14, ]
year_15_values <- constant_to_be_mean_data_cod[constant_to_be_mean_data_cod$Year == 15, ]
year_16_values <- constant_to_be_mean_data_cod[constant_to_be_mean_data_cod$Year == 16, ]
year_17_values <- constant_to_be_mean_data_cod[constant_to_be_mean_data_cod$Year == 17, ]
year_18_values <- constant_to_be_mean_data_cod[constant_to_be_mean_data_cod$Year == 18, ]
year_19_values <- constant_to_be_mean_data_cod[constant_to_be_mean_data_cod$Year == 19, ]
year_20_values <- constant_to_be_mean_data_cod[constant_to_be_mean_data_cod$Year == 20, ]
year_21_values <- constant_to_be_mean_data_cod[constant_to_be_mean_data_cod$Year == 21, ]
year_22_values <- constant_to_be_mean_data_cod[constant_to_be_mean_data_cod$Year == 22, ]
year_23_values <- constant_to_be_mean_data_cod[constant_to_be_mean_data_cod$Year == 23, ]
year_24_values <- constant_to_be_mean_data_cod[constant_to_be_mean_data_cod$Year == 24, ]
year_25_values <- constant_to_be_mean_data_cod[constant_to_be_mean_data_cod$Year == 25, ]
year_26_values <- constant_to_be_mean_data_cod[constant_to_be_mean_data_cod$Year == 26, ]
year_27_values <- constant_to_be_mean_data_cod[constant_to_be_mean_data_cod$Year == 27, ]
year_28_values <- constant_to_be_mean_data_cod[constant_to_be_mean_data_cod$Year == 28, ]
year_29_values <- constant_to_be_mean_data_cod[constant_to_be_mean_data_cod$Year == 29, ]
year_30_values <- constant_to_be_mean_data_cod[constant_to_be_mean_data_cod$Year == 30, ]
year_31_values <- constant_to_be_mean_data_cod[constant_to_be_mean_data_cod$Year == 31, ]
year_32_values <- constant_to_be_mean_data_cod[constant_to_be_mean_data_cod$Year == 32, ]
year_33_values <- constant_to_be_mean_data_cod[constant_to_be_mean_data_cod$Year == 33, ]
year_34_values <- constant_to_be_mean_data_cod[constant_to_be_mean_data_cod$Year == 34, ]
year_35_values <- constant_to_be_mean_data_cod[constant_to_be_mean_data_cod$Year == 35, ]
year_36_values <- constant_to_be_mean_data_cod[constant_to_be_mean_data_cod$Year == 36, ]
year_37_values <- constant_to_be_mean_data_cod[constant_to_be_mean_data_cod$Year == 37, ]
year_38_values <- constant_to_be_mean_data_cod[constant_to_be_mean_data_cod$Year == 38, ]
year_39_values <- constant_to_be_mean_data_cod[constant_to_be_mean_data_cod$Year == 39, ]
year_40_values <- constant_to_be_mean_data_cod[constant_to_be_mean_data_cod$Year == 40, ]
year_41_values <- constant_to_be_mean_data_cod[constant_to_be_mean_data_cod$Year == 41, ]
year_42_values <- constant_to_be_mean_data_cod[constant_to_be_mean_data_cod$Year == 42, ]
year_43_values <- constant_to_be_mean_data_cod[constant_to_be_mean_data_cod$Year == 43, ]
year_44_values <- constant_to_be_mean_data_cod[constant_to_be_mean_data_cod$Year == 44, ]
year_45_values <- constant_to_be_mean_data_cod[constant_to_be_mean_data_cod$Year == 45, ]
year_46_values <- constant_to_be_mean_data_cod[constant_to_be_mean_data_cod$Year == 46, ]
year_47_values <- constant_to_be_mean_data_cod[constant_to_be_mean_data_cod$Year == 47, ]
year_48_values <- constant_to_be_mean_data_cod[constant_to_be_mean_data_cod$Year == 48, ]
year_49_values <- constant_to_be_mean_data_cod[constant_to_be_mean_data_cod$Year == 49, ]
year_50_values <- constant_to_be_mean_data_cod[constant_to_be_mean_data_cod$Year == 50, ]

 

# 100 w values (size classes)
# 12 species
# 12 mean values produced per year

year_values <- list(year_1_values, year_2_values, year_3_values, 
                    year_4_values, year_5_values, year_6_values, 
                    year_7_values, year_8_values, year_9_values, 
                    year_10_values, year_11_values, year_12_values, 
                    year_13_values, year_14_values, year_15_values, 
                    year_16_values, year_17_values, year_18_values, 
                    year_19_values, year_20_values,
                    year_21_values, year_22_values, year_23_values,
                    year_24_values, year_25_values, year_26_values,
                    year_27_values, year_28_values, year_29_values,
                    year_30_values, year_31_values, year_32_values,
                    year_33_values, year_34_values, year_35_values,
                    year_36_values, year_37_values, year_38_values,
                    year_39_values, year_40_values, year_41_values,
                    year_42_values, year_43_values, year_44_values, 
                    year_45_values, year_46_values, year_47_values, 
                    year_48_values, year_49_values, year_50_values)


# Create a vector of years
years <- 0:49
length(year_values)
# Loop through each year
for (i in seq_along(years)) {
  year <- years[i]
  year_data <- year_values[[i]]
  
  # Generate the variable name prefix based on the year
  year_prefix <- paste0("year_", year, "_constant$")
  
  # Access the PSD values for the current year
  PSD_values <- year_data$PSD
  
  # Loop through each species and calculate the mean diet value
  for (s in species) {
    # Generate the variable name for the current species
    species_variable <- paste0("mean_diet_", s, "_constant_", year)
    
    # Calculate the sum of the product
    species_sum <- sum(year_data[[s]] * PSD_values)
    
    # Calculate the mean diet value
    assign(species_variable, species_sum / sum(PSD_values))
  }
}

# View the mean diet values for each species and each year
for (s in species) {
  print(paste("Species:", s))
  for (y in years) {
    variable_name <- paste0("mean_diet_", s, "_constant_", y)
    mean_diet_value <- get(variable_name)
    print(paste("Year:", y, "| Mean Diet Value:", mean_diet_value))
  }
  cat("\n")
}

# Create an empty data frame to store the results
results_df <- data.frame(Species = character(), Year = numeric(), Mean_Diet_Value = numeric())

# Iterate over species and years
for (s in species) {
  for (y in years) {
    variable_name <- paste0("mean_diet_", s, "_constant_", y)
    mean_diet_value <- get(variable_name)
    
    # Add the data to the results data frame
    results_df <- rbind(results_df, data.frame(Species = s, Year = y, Mean_Diet_Value = mean_diet_value))
  }
}

# Write the results to a CSV file
write.csv(results_df, "mean_diet_results_constant_year1to50.csv", row.names = FALSE)

mean_diet_results_constant_yr1to50 <- read.csv("mean_diet_results_constant_year1to50.csv")

library(ggplot2)

# Filter the data for the selected species
selected_species <- c("Sandeel", "Herring", "Haddock", "Plaice")
filtered_data <- mean_diet_results_constant_yr1to50[mean_diet_results_constant_yr1to50$Species %in% selected_species, ]


ggplot(filtered_data, aes(x = Year, y = Mean_Diet_Value, color = Species)) +
  geom_line(linewidth = 1) +
  geom_rect(aes(xmin = 0, xmax = 49, ymin = -Inf, ymax = Inf), fill = NA, color = "black", alpha = 0.3) +
  geom_label(aes(x = 25, y = Inf, label = "Constant Harvest"), vjust = 1, hjust = 0.5, color = "black", size = 3) +
  labs(x = "Time step (Year)", y = "Mean Diet of Cod (g/year)", title = "C.) Constant Harvest", color = "Prey Species") +  # Change legend title
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, 49, by = 1), labels = seq(0, 49, by = 1)) +  # Change x-axis labels
  scale_y_continuous(breaks = seq(0, ceiling(max(filtered_data$Mean_Diet_Value)), by = 50),
                     limits = c(0, max(filtered_data$Mean_Diet_Value) + 50),
                     labels = function(x) ifelse(x == max(filtered_data$Mean_Diet_Value), "50", as.character(x))) +  # Adjust y-axis breaks, limits, and label
  theme(panel.grid.major = element_line(color = "grey", linetype = "dotted"),  # More gridlines
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),  # Rotate x-axis labels
        axis.text.y = element_text(size = 8))  # Adjust y-axis label size








## Diet cod temporal ##

## Diet cod ##
getDiet(params_temporal_harvest)

# Initialize an empty list to store diet rates for each year
diet_rates_temporal <- list()

# Loop over each year (time step)
for (year in 1:50) {
  # Extract biomass values for the current year
  n <- sim_params_temporal_harvest@n[year,,]
  n_pp <- sim_params_temporal_harvest@n_pp[year,]
  
  # Get diet information for the current year and store it in the list
  diet_rates_temporal[[year]] <- getDiet(params_temporal_harvest, n, n_pp, proportion = FALSE)
}


predator_cod_temporal <- list()

# Loop over each year
for (year in 1:50) {
  # Extract predator Cod data for the current year
  predator_cod_temporal[[year]] <- diet_rates_temporal[[year]][11,,]
}
predator_cod_temporal

# Example of accessing the predator Cod data for year 14
#The first dimension (w) represents different weight categories of the predator Cod.
#The second dimension (prey) represents different prey species that the predator Cod is consuming
year_1_predator_cod_temporal <- predator_cod_temporal[[1]]
year_2_predator_cod_temporal <- predator_cod_temporal[[2]]
year_3_predator_cod_temporal <- predator_cod_temporal[[3]]
year_4_predator_cod_temporal <- predator_cod_temporal[[4]]
year_5_predator_cod_temporal <- predator_cod_temporal[[5]]
year_6_predator_cod_temporal <- predator_cod_temporal[[6]]
year_7_predator_cod_temporal <- predator_cod_temporal[[7]]
year_8_predator_cod_temporal <- predator_cod_temporal[[8]]
year_9_predator_cod_temporal <- predator_cod_temporal[[9]]
year_10_predator_cod_temporal <- predator_cod_temporal[[10]]
year_11_predator_cod_temporal <- predator_cod_temporal[[11]]
year_12_predator_cod_temporal <- predator_cod_temporal[[12]]
year_13_predator_cod_temporal <- predator_cod_temporal[[13]]
year_14_predator_cod_temporal <- predator_cod_temporal[[14]]
year_15_predator_cod_temporal <- predator_cod_temporal[[15]]
year_16_predator_cod_temporal <- predator_cod_temporal[[16]]
year_17_predator_cod_temporal <- predator_cod_temporal[[17]]
year_18_predator_cod_temporal <- predator_cod_temporal[[18]]
year_19_predator_cod_temporal <- predator_cod_temporal[[19]]
year_20_predator_cod_temporal <- predator_cod_temporal[[20]]
year_21_predator_cod_temporal <- predator_cod_temporal[[21]]
year_22_predator_cod_temporal <- predator_cod_temporal[[22]]
year_23_predator_cod_temporal <- predator_cod_temporal[[23]]
year_24_predator_cod_temporal <- predator_cod_temporal[[24]]
year_25_predator_cod_temporal <- predator_cod_temporal[[25]]
year_26_predator_cod_temporal <- predator_cod_temporal[[26]]
year_27_predator_cod_temporal <- predator_cod_temporal[[27]]
year_28_predator_cod_temporal <- predator_cod_temporal[[28]]
year_29_predator_cod_temporal <- predator_cod_temporal[[29]]
year_30_predator_cod_temporal <- predator_cod_temporal[[30]]
year_31_predator_cod_temporal <- predator_cod_temporal[[31]]
year_32_predator_cod_temporal <- predator_cod_temporal[[32]]
year_33_predator_cod_temporal <- predator_cod_temporal[[33]]
year_34_predator_cod_temporal <- predator_cod_temporal[[34]]
year_35_predator_cod_temporal <- predator_cod_temporal[[35]]
year_36_predator_cod_temporal <- predator_cod_temporal[[36]]
year_37_predator_cod_temporal <- predator_cod_temporal[[37]]
year_38_predator_cod_temporal <- predator_cod_temporal[[38]]
year_39_predator_cod_temporal <- predator_cod_temporal[[39]]
year_40_predator_cod_temporal <- predator_cod_temporal[[40]]
year_41_predator_cod_temporal <- predator_cod_temporal[[41]]
year_42_predator_cod_temporal <- predator_cod_temporal[[42]]
year_43_predator_cod_temporal <- predator_cod_temporal[[43]]
year_44_predator_cod_temporal <- predator_cod_temporal[[44]]
year_45_predator_cod_temporal <- predator_cod_temporal[[45]]
year_46_predator_cod_temporal <- predator_cod_temporal[[46]]
year_47_predator_cod_temporal <- predator_cod_temporal[[47]]
year_48_predator_cod_temporal <- predator_cod_temporal[[48]]
year_49_predator_cod_temporal <- predator_cod_temporal[[49]]
year_50_predator_cod_temporal <- predator_cod_temporal[[50]]

# Write data for year 1 to CSV file
write.csv(year_1_predator_cod_temporal, file = "year_1_predator_cod_temporal_v9.csv", row.names = FALSE)

# Write data for year 2 to CSV file
write.csv(year_2_predator_cod_temporal, file = "year_2_predator_cod_temporal_v9.csv", row.names = FALSE)

# Write data for year 3 to CSV file
write.csv(year_3_predator_cod_temporal, file = "year_3_predator_cod_temporal_v9.csv", row.names = FALSE)

# Write data for year 4 to CSV file
write.csv(year_4_predator_cod_temporal, file = "year_4_predator_cod_temporal_v9.csv", row.names = FALSE)

# Write data for year 5 to CSV file
write.csv(year_5_predator_cod_temporal, file = "year_5_predator_cod_temporal_v9.csv", row.names = FALSE)

# Write data for year 6 to CSV file
write.csv(year_6_predator_cod_temporal, file = "year_6_predator_cod_temporal_v9.csv", row.names = FALSE)

# Write data for year 7 to CSV file
write.csv(year_7_predator_cod_temporal, file = "year_7_predator_cod_temporal_v9.csv", row.names = FALSE)

# Write data for year 8 to CSV file
write.csv(year_8_predator_cod_temporal, file = "year_8_predator_cod_temporal_v9.csv", row.names = FALSE)

# Write data for year 9 to CSV file
write.csv(year_9_predator_cod_temporal, file = "year_9_predator_cod_temporal_v9.csv", row.names = FALSE)

# Write data for year 10 to CSV file
write.csv(year_10_predator_cod_temporal, file = "year_10_predator_cod_temporal_v9.csv", row.names = FALSE)

# Write data for year 11 to CSV file
write.csv(year_11_predator_cod_temporal, file = "year_11_predator_cod_temporal_v9.csv", row.names = FALSE)

# Write data for year 12 to CSV file
write.csv(year_12_predator_cod_temporal, file = "year_12_predator_cod_temporal_v9.csv", row.names = FALSE)

# Write data for year 13 to CSV file
write.csv(year_13_predator_cod_temporal, file = "year_13_predator_cod_temporal_v9.csv", row.names = FALSE)

# Write data for year 14 to CSV file
write.csv(year_14_predator_cod_temporal, file = "year_14_predator_cod_temporal_v9.csv", row.names = FALSE)


# Write data for year 14 to CSV file
write.csv(year_14_predator_cod_temporal, file = "year_14_predator_cod_temporal_v9.csv", row.names = FALSE)

# Write data for year 15 to CSV file
write.csv(year_15_predator_cod_temporal, file = "year_15_predator_cod_temporal_v9.csv", row.names = FALSE)

# Write data for year 16 to CSV file
write.csv(year_16_predator_cod_temporal, file = "year_16_predator_cod_temporal_v9.csv", row.names = FALSE)

# Write data for year 17 to CSV file
write.csv(year_17_predator_cod_temporal, file = "year_17_predator_cod_temporal_v9.csv", row.names = FALSE)

# Write data for year 18 to CSV file
write.csv(year_18_predator_cod_temporal, file = "year_18_predator_cod_temporal_v9.csv", row.names = FALSE)

# Write data for year 19 to CSV file
write.csv(year_19_predator_cod_temporal, file = "year_19_predator_cod_temporal_v9.csv", row.names = FALSE)

# Write data for year 20 to CSV file
write.csv(year_20_predator_cod_temporal, file = "year_20_predator_cod_temporal_v9.csv", row.names = FALSE)

# Write data for year 21 to CSV file
write.csv(year_21_predator_cod_temporal, file = "year_21_predator_cod_temporal_v9.csv", row.names = FALSE)

# Write data for year 22 to CSV file
write.csv(year_22_predator_cod_temporal, file = "year_22_predator_cod_temporal_v9.csv", row.names = FALSE)

# Write data for year 23 to CSV file
write.csv(year_23_predator_cod_temporal, file = "year_23_predator_cod_temporal_v9.csv", row.names = FALSE)

# Write data for year 24 to CSV file
write.csv(year_24_predator_cod_temporal, file = "year_24_predator_cod_temporal_v9.csv", row.names = FALSE)

# Write data for year 25 to CSV file
write.csv(year_25_predator_cod_temporal, file = "year_25_predator_cod_temporal_v9.csv", row.names = FALSE)

# Write data for year 26 to CSV file
write.csv(year_26_predator_cod_temporal, file = "year_26_predator_cod_temporal_v9.csv", row.names = FALSE)

# Write data for year 27 to CSV file
write.csv(year_27_predator_cod_temporal, file = "year_27_predator_cod_temporal_v9.csv", row.names = FALSE)

# Write data for year 28 to CSV file
write.csv(year_28_predator_cod_temporal, file = "year_28_predator_cod_temporal_v9.csv", row.names = FALSE)

# Write data for year 29 to CSV file
write.csv(year_29_predator_cod_temporal, file = "year_29_predator_cod_temporal_v9.csv", row.names = FALSE)

# Write data for year 30 to CSV file
write.csv(year_30_predator_cod_temporal, file = "year_30_predator_cod_temporal_v9.csv", row.names = FALSE)

# Write data for year 31 to CSV file
write.csv(year_31_predator_cod_temporal, file = "year_31_predator_cod_temporal_v9.csv", row.names = FALSE)

# Write data for year 32 to CSV file
write.csv(year_32_predator_cod_temporal, file = "year_32_predator_cod_temporal_v9.csv", row.names = FALSE)

# Write data for year 33 to CSV file
write.csv(year_33_predator_cod_temporal, file = "year_33_predator_cod_temporal_v9.csv", row.names = FALSE)

# Write data for year 34 to CSV file
write.csv(year_34_predator_cod_temporal, file = "year_34_predator_cod_temporal_v9.csv", row.names = FALSE)

# Write data for year 35 to CSV file
write.csv(year_35_predator_cod_temporal, file = "year_35_predator_cod_temporal_v9.csv", row.names = FALSE)

# Write data for year 36 to CSV file
write.csv(year_36_predator_cod_temporal, file = "year_36_predator_cod_temporal_v9.csv", row.names = FALSE)

# Write data for year 37 to CSV file
write.csv(year_37_predator_cod_temporal, file = "year_37_predator_cod_temporal_v9.csv", row.names = FALSE)

# Write data for year 38 to CSV file
write.csv(year_38_predator_cod_temporal, file = "year_38_predator_cod_temporal_v9.csv", row.names = FALSE)

# Write data for year 39 to CSV file
write.csv(year_39_predator_cod_temporal, file = "year_39_predator_cod_temporal_v9.csv", row.names = FALSE)

# Write data for year 40 to CSV file
write.csv(year_40_predator_cod_temporal, file = "year_40_predator_cod_temporal_v9.csv", row.names = FALSE)

# Write data for year 41 to CSV file
write.csv(year_41_predator_cod_temporal, file = "year_41_predator_cod_temporal_v9.csv", row.names = FALSE)

# Write data for year 42 to CSV file
write.csv(year_42_predator_cod_temporal, file = "year_42_predator_cod_temporal_v9.csv", row.names = FALSE)

# Write data for year 43 to CSV file
write.csv(year_43_predator_cod_temporal, file = "year_43_predator_cod_temporal_v9.csv", row.names = FALSE)

# Write data for year 44 to CSV file
write.csv(year_44_predator_cod_temporal, file = "year_44_predator_cod_temporal_v9.csv", row.names = FALSE)

# Write data for year 45 to CSV file
write.csv(year_45_predator_cod_temporal, file = "year_45_predator_cod_temporal_v9.csv", row.names = FALSE)

# Write data for year 46 to CSV file
write.csv(year_46_predator_cod_temporal, file = "year_46_predator_cod_temporal_v9.csv", row.names = FALSE)

# Write data for year 47 to CSV file
write.csv(year_47_predator_cod_temporal, file = "year_47_predator_cod_temporal_v9.csv", row.names = FALSE)

# Write data for year 48 to CSV file
write.csv(year_48_predator_cod_temporal, file = "year_48_predator_cod_temporal_v9.csv", row.names = FALSE)

# Write data for year 49 to CSV file
write.csv(year_49_predator_cod_temporal, file = "year_49_predator_cod_temporal_v9.csv", row.names = FALSE)

# Write data for year 50 to CSV file
write.csv(year_50_predator_cod_temporal, file = "year_50_predator_cod_temporal_v9.csv", row.names = FALSE)


## Read data for year 1 from CSV file
year_1_cod_diet_size_class_temporal <- read.csv("year_1_predator_cod_temporal_v9.csv")

## Read data for year 2 from CSV file
year_2_cod_diet_size_class_temporal <- read.csv("year_2_predator_cod_temporal_v9.csv")

## Read data for year 3 from CSV file
year_3_cod_diet_size_class_temporal <- read.csv("year_3_predator_cod_temporal_v9.csv")

## Read data for year 4 from CSV file
year_4_cod_diet_size_class_temporal <- read.csv("year_4_predator_cod_temporal_v9.csv")

## Read data for year 5 from CSV file
year_5_cod_diet_size_class_temporal <- read.csv("year_5_predator_cod_temporal_v9.csv")

## Read data for year 6 from CSV file
year_6_cod_diet_size_class_temporal <- read.csv("year_6_predator_cod_temporal_v9.csv")

## Read data for year 7 from CSV file
year_7_cod_diet_size_class_temporal <- read.csv("year_7_predator_cod_temporal_v9.csv")

## Read data for year 8 from CSV file
year_8_cod_diet_size_class_temporal <- read.csv("year_8_predator_cod_temporal_v9.csv")

## Read data for year 9 from CSV file
year_9_cod_diet_size_class_temporal <- read.csv("year_9_predator_cod_temporal_v9.csv")

## Read data for year 10 from CSV file
year_10_cod_diet_size_class_temporal <- read.csv("year_10_predator_cod_temporal_v9.csv")

## Read data for year 11 from CSV file
year_11_cod_diet_size_class_temporal <- read.csv("year_11_predator_cod_temporal_v9.csv")

## Read data for year 12 from CSV file
year_12_cod_diet_size_class_temporal <- read.csv("year_12_predator_cod_temporal_v9.csv")

## Read data for year 13 from CSV file
year_13_cod_diet_size_class_temporal <- read.csv("year_13_predator_cod_temporal_v9.csv")


## Read data for year 14 from CSV file
year_14_cod_diet_size_class_temporal <- read.csv("year_14_predator_cod_temporal_v9.csv")
year_14_cod_diet_size_class_temporal

## Read data for year 15 from CSV file
year_15_cod_diet_size_class_temporal <- read.csv("year_15_predator_cod_temporal_v9.csv")

## Read data for year 16 from CSV file
year_16_cod_diet_size_class_temporal <- read.csv("year_16_predator_cod_temporal_v9.csv")

## Read data for year 17 from CSV file
year_17_cod_diet_size_class_temporal <- read.csv("year_17_predator_cod_temporal_v9.csv")

## Read data for year 18 from CSV file
year_18_cod_diet_size_class_temporal <- read.csv("year_18_predator_cod_temporal_v9.csv")

## Read data for year 19 from CSV file
year_19_cod_diet_size_class_temporal <- read.csv("year_19_predator_cod_temporal_v9.csv")

## Read data for year 20 from CSV file
year_20_cod_diet_size_class_temporal <- read.csv("year_20_predator_cod_temporal_v9.csv")

## Read data for year 21 from CSV file
year_21_cod_diet_size_class_temporal <- read.csv("year_21_predator_cod_temporal_v9.csv")

## Read data for year 22 from CSV file
year_22_cod_diet_size_class_temporal <- read.csv("year_22_predator_cod_temporal_v9.csv")

## Read data for year 23 from CSV file
year_23_cod_diet_size_class_temporal <- read.csv("year_23_predator_cod_temporal_v9.csv")

## Read data for year 24 from CSV file
year_24_cod_diet_size_class_temporal <- read.csv("year_24_predator_cod_temporal_v9.csv")

## Read data for year 25 from CSV file
year_25_cod_diet_size_class_temporal <- read.csv("year_25_predator_cod_temporal_v9.csv")

## Read data for year 26 from CSV file
year_26_cod_diet_size_class_temporal <- read.csv("year_26_predator_cod_temporal_v9.csv")

## Read data for year 27 from CSV file
year_27_cod_diet_size_class_temporal <- read.csv("year_27_predator_cod_temporal_v9.csv")

## Read data for year 28 from CSV file
year_28_cod_diet_size_class_temporal <- read.csv("year_28_predator_cod_temporal_v9.csv")

## Read data for year 29 from CSV file
year_29_cod_diet_size_class_temporal <- read.csv("year_29_predator_cod_temporal_v9.csv")

## Read data for year 30 from CSV file
year_30_cod_diet_size_class_temporal <- read.csv("year_30_predator_cod_temporal_v9.csv")

## Read data for year 31 from CSV file
year_31_cod_diet_size_class_temporal <- read.csv("year_31_predator_cod_temporal_v9.csv")

## Read data for year 32 from CSV file
year_32_cod_diet_size_class_temporal <- read.csv("year_32_predator_cod_temporal_v9.csv")

## Read data for year 33 from CSV file
year_33_cod_diet_size_class_temporal <- read.csv("year_33_predator_cod_temporal_v9.csv")

## Read data for year 34 from CSV file
year_34_cod_diet_size_class_temporal <- read.csv("year_34_predator_cod_temporal_v9.csv")

## Read data for year 35 from CSV file
year_35_cod_diet_size_class_temporal <- read.csv("year_35_predator_cod_temporal_v9.csv")

## Read data for year 36 from CSV file
year_36_cod_diet_size_class_temporal <- read.csv("year_36_predator_cod_temporal_v9.csv")

## Read data for year 37 from CSV file
year_37_cod_diet_size_class_temporal <- read.csv("year_37_predator_cod_temporal_v9.csv")

## Read data for year 38 from CSV file
year_38_cod_diet_size_class_temporal <- read.csv("year_38_predator_cod_temporal_v9.csv")

## Read data for year 39 from CSV file
year_39_cod_diet_size_class_temporal <- read.csv("year_39_predator_cod_temporal_v9.csv")

## Read data for year 40 from CSV file
year_40_cod_diet_size_class_temporal <- read.csv("year_40_predator_cod_temporal_v9.csv")

## Read data for year 41 from CSV file
year_41_cod_diet_size_class_temporal <- read.csv("year_41_predator_cod_temporal_v9.csv")

## Read data for year 42 from CSV file
year_42_cod_diet_size_class_temporal <- read.csv("year_42_predator_cod_temporal_v9.csv")

## Read data for year 43 from CSV file
year_43_cod_diet_size_class_temporal <- read.csv("year_43_predator_cod_temporal_v9.csv")

## Read data for year 44 from CSV file
year_44_cod_diet_size_class_temporal <- read.csv("year_44_predator_cod_temporal_v9.csv")
## Read data for year 45 from CSV file
year_45_cod_diet_size_class_temporal <- read.csv("year_45_predator_cod_temporal_v9.csv")

## Read data for year 46 from CSV file
year_46_cod_diet_size_class_temporal <- read.csv("year_46_predator_cod_temporal_v9.csv")

## Read data for year 47 from CSV file
year_47_cod_diet_size_class_temporal <- read.csv("year_47_predator_cod_temporal_v9.csv")

## Read data for year 48 from CSV file
year_48_cod_diet_size_class_temporal <- read.csv("year_48_predator_cod_temporal_v9.csv")

## Read data for year 49 from CSV file
year_49_cod_diet_size_class_temporal <- read.csv("year_49_predator_cod_temporal_v9.csv")

## Read data for year 50 from CSV file
year_50_cod_diet_size_class_temporal <- read.csv("year_50_predator_cod_temporal_v9.csv")


year_1_cod_diet_size_class_temporal$Year <- 1
year_2_cod_diet_size_class_temporal$Year <- 2
year_3_cod_diet_size_class_temporal$Year <- 3
year_4_cod_diet_size_class_temporal$Year <- 4
year_5_cod_diet_size_class_temporal$Year <- 5
year_6_cod_diet_size_class_temporal$Year <- 6
year_7_cod_diet_size_class_temporal$Year <- 7
year_8_cod_diet_size_class_temporal$Year <- 8
year_9_cod_diet_size_class_temporal$Year <- 9
year_10_cod_diet_size_class_temporal$Year <- 10
year_11_cod_diet_size_class_temporal$Year <- 11
year_12_cod_diet_size_class_temporal$Year <- 12
year_13_cod_diet_size_class_temporal$Year <- 13
year_14_cod_diet_size_class_temporal$Year <- 14
year_15_cod_diet_size_class_temporal$Year <- 15
year_16_cod_diet_size_class_temporal$Year <- 16
year_17_cod_diet_size_class_temporal$Year <- 17
year_18_cod_diet_size_class_temporal$Year <- 18
year_19_cod_diet_size_class_temporal$Year <- 19
year_20_cod_diet_size_class_temporal$Year <- 20
year_21_cod_diet_size_class_temporal$Year <- 21
year_22_cod_diet_size_class_temporal$Year <- 22
year_23_cod_diet_size_class_temporal$Year <- 23
year_24_cod_diet_size_class_temporal$Year <- 24
year_25_cod_diet_size_class_temporal$Year <- 25
year_26_cod_diet_size_class_temporal$Year <- 26
year_27_cod_diet_size_class_temporal$Year <- 27
year_28_cod_diet_size_class_temporal$Year <- 28
year_29_cod_diet_size_class_temporal$Year <- 29
year_30_cod_diet_size_class_temporal$Year <- 30
year_31_cod_diet_size_class_temporal$Year <- 31
year_32_cod_diet_size_class_temporal$Year <- 32
year_33_cod_diet_size_class_temporal$Year <- 33
year_34_cod_diet_size_class_temporal$Year <- 34
year_35_cod_diet_size_class_temporal$Year <- 35
year_36_cod_diet_size_class_temporal$Year <- 36
year_37_cod_diet_size_class_temporal$Year <- 37
year_38_cod_diet_size_class_temporal$Year <- 38
year_39_cod_diet_size_class_temporal$Year <- 39
year_40_cod_diet_size_class_temporal$Year <- 40
year_41_cod_diet_size_class_temporal$Year <- 41
year_42_cod_diet_size_class_temporal$Year <- 42
year_43_cod_diet_size_class_temporal$Year <- 43
year_44_cod_diet_size_class_temporal$Year <- 44
year_45_cod_diet_size_class_temporal$Year <- 45
year_46_cod_diet_size_class_temporal$Year <- 46
year_47_cod_diet_size_class_temporal$Year <- 47
year_48_cod_diet_size_class_temporal$Year <- 48
year_49_cod_diet_size_class_temporal$Year <- 49
year_50_cod_diet_size_class_temporal$Year <- 50

combined_data_cod_diet_size_class_temporal <- rbind(
  year_1_cod_diet_size_class_temporal,
  year_2_cod_diet_size_class_temporal,
  year_3_cod_diet_size_class_temporal,
  year_4_cod_diet_size_class_temporal,
  year_5_cod_diet_size_class_temporal,
  year_6_cod_diet_size_class_temporal,
  year_7_cod_diet_size_class_temporal,
  year_8_cod_diet_size_class_temporal,
  year_9_cod_diet_size_class_temporal,
  year_10_cod_diet_size_class_temporal,
  year_11_cod_diet_size_class_temporal,
  year_12_cod_diet_size_class_temporal,
  year_13_cod_diet_size_class_temporal,
  year_14_cod_diet_size_class_temporal,
  year_15_cod_diet_size_class_temporal,
  year_16_cod_diet_size_class_temporal,
  year_17_cod_diet_size_class_temporal,
  year_18_cod_diet_size_class_temporal,
  year_19_cod_diet_size_class_temporal,
  year_20_cod_diet_size_class_temporal,
  year_21_cod_diet_size_class_temporal,
  year_22_cod_diet_size_class_temporal,
  year_23_cod_diet_size_class_temporal,
  year_24_cod_diet_size_class_temporal,
  year_25_cod_diet_size_class_temporal,
  year_26_cod_diet_size_class_temporal,
  year_27_cod_diet_size_class_temporal,
  year_28_cod_diet_size_class_temporal,
  year_29_cod_diet_size_class_temporal,
  year_30_cod_diet_size_class_temporal,
  year_31_cod_diet_size_class_temporal,
  year_32_cod_diet_size_class_temporal,
  year_33_cod_diet_size_class_temporal,
  year_34_cod_diet_size_class_temporal,
  year_35_cod_diet_size_class_temporal,
  year_36_cod_diet_size_class_temporal,
  year_37_cod_diet_size_class_temporal,
  year_38_cod_diet_size_class_temporal,
  year_39_cod_diet_size_class_temporal,
  year_40_cod_diet_size_class_temporal,
  year_41_cod_diet_size_class_temporal,
  year_42_cod_diet_size_class_temporal,
  year_43_cod_diet_size_class_temporal,
  year_44_cod_diet_size_class_temporal,
  year_45_cod_diet_size_class_temporal,
  year_46_cod_diet_size_class_temporal,
  year_47_cod_diet_size_class_temporal,
  year_48_cod_diet_size_class_temporal,
  year_49_cod_diet_size_class_temporal,
  year_50_cod_diet_size_class_temporal
)



write.csv(combined_data_cod_diet_size_class_temporal, file = "cod_diet_size_class_all_years_temporalV2.csv", row.names = FALSE)


# add cod size classes excel
cod_diet_size_class_all_years_temporal <- read.csv("cod_diet_size_class_all_years_temporalV2.csv")
cod_diet_size_class_all_years_temporal

cod_abundance_year1_temporal <- plotSpectra(sim_params_temporal_harvest, time_range = 1, species = c("Cod"), return_data = TRUE)
cod_abundance_year2_temporal <- plotSpectra(sim_params_temporal_harvest, time_range = 2, species = c("Cod"), return_data = TRUE)
cod_abundance_year3_temporal <- plotSpectra(sim_params_temporal_harvest, time_range = 3, species = c("Cod"), return_data = TRUE)
cod_abundance_year4_temporal <- plotSpectra(sim_params_temporal_harvest, time_range = 4, species = c("Cod"), return_data = TRUE)
cod_abundance_year5_temporal <- plotSpectra(sim_params_temporal_harvest, time_range = 5, species = c("Cod"), return_data = TRUE)
cod_abundance_year6_temporal <- plotSpectra(sim_params_temporal_harvest, time_range = 6, species = c("Cod"), return_data = TRUE)
cod_abundance_year7_temporal <- plotSpectra(sim_params_temporal_harvest, time_range = 7, species = c("Cod"), return_data = TRUE)
cod_abundance_year8_temporal <- plotSpectra(sim_params_temporal_harvest, time_range = 8, species = c("Cod"), return_data = TRUE)
cod_abundance_year9_temporal <- plotSpectra(sim_params_temporal_harvest, time_range = 9, species = c("Cod"), return_data = TRUE)
cod_abundance_year10_temporal <- plotSpectra(sim_params_temporal_harvest, time_range = 10, species = c("Cod"), return_data = TRUE)
cod_abundance_year11_temporal <- plotSpectra(sim_params_temporal_harvest, time_range = 11, species = c("Cod"), return_data = TRUE)
cod_abundance_year12_temporal <- plotSpectra(sim_params_temporal_harvest, time_range = 12, species = c("Cod"), return_data = TRUE)
cod_abundance_year13_temporal <- plotSpectra(sim_params_temporal_harvest, time_range = 13, species = c("Cod"), return_data = TRUE)
cod_abundance_year14_temporal <- plotSpectra(sim_params_temporal_harvest, time_range = 14, species = c("Cod"), return_data = TRUE)
cod_abundance_year15_temporal <- plotSpectra(sim_params_temporal_harvest, time_range = 15, species = c("Cod"), return_data = TRUE)
cod_abundance_year16_temporal <- plotSpectra(sim_params_temporal_harvest, time_range = 16, species = c("Cod"), return_data = TRUE)
cod_abundance_year17_temporal <- plotSpectra(sim_params_temporal_harvest, time_range = 17, species = c("Cod"), return_data = TRUE)
cod_abundance_year18_temporal <- plotSpectra(sim_params_temporal_harvest, time_range = 18, species = c("Cod"), return_data = TRUE)
cod_abundance_year19_temporal <- plotSpectra(sim_params_temporal_harvest, time_range = 19, species = c("Cod"), return_data = TRUE)
cod_abundance_year20_temporal <- plotSpectra(sim_params_temporal_harvest, time_range = 20, species = c("Cod"), return_data = TRUE)
cod_abundance_year21_temporal <- plotSpectra(sim_params_temporal_harvest, time_range = 21, species = c("Cod"), return_data = TRUE)
cod_abundance_year22_temporal <- plotSpectra(sim_params_temporal_harvest, time_range = 22, species = c("Cod"), return_data = TRUE)
cod_abundance_year23_temporal <- plotSpectra(sim_params_temporal_harvest, time_range = 23, species = c("Cod"), return_data = TRUE)
cod_abundance_year24_temporal <- plotSpectra(sim_params_temporal_harvest, time_range = 24, species = c("Cod"), return_data = TRUE)
cod_abundance_year25_temporal <- plotSpectra(sim_params_temporal_harvest, time_range = 25, species = c("Cod"), return_data = TRUE)
cod_abundance_year26_temporal <- plotSpectra(sim_params_temporal_harvest, time_range = 26, species = c("Cod"), return_data = TRUE)
cod_abundance_year27_temporal <- plotSpectra(sim_params_temporal_harvest, time_range = 27, species = c("Cod"), return_data = TRUE)
cod_abundance_year28_temporal <- plotSpectra(sim_params_temporal_harvest, time_range = 28, species = c("Cod"), return_data = TRUE)
cod_abundance_year29_temporal <- plotSpectra(sim_params_temporal_harvest, time_range = 29, species = c("Cod"), return_data = TRUE)
cod_abundance_year30_temporal <- plotSpectra(sim_params_temporal_harvest, time_range = 30, species = c("Cod"), return_data = TRUE)
cod_abundance_year31_temporal <- plotSpectra(sim_params_temporal_harvest, time_range = 31, species = c("Cod"), return_data = TRUE)
cod_abundance_year32_temporal <- plotSpectra(sim_params_temporal_harvest, time_range = 32, species = c("Cod"), return_data = TRUE)
cod_abundance_year33_temporal <- plotSpectra(sim_params_temporal_harvest, time_range = 33, species = c("Cod"), return_data = TRUE)
cod_abundance_year34_temporal <- plotSpectra(sim_params_temporal_harvest, time_range = 34, species = c("Cod"), return_data = TRUE)
cod_abundance_year35_temporal <- plotSpectra(sim_params_temporal_harvest, time_range = 35, species = c("Cod"), return_data = TRUE)
cod_abundance_year36_temporal <- plotSpectra(sim_params_temporal_harvest, time_range = 36, species = c("Cod"), return_data = TRUE)
cod_abundance_year37_temporal <- plotSpectra(sim_params_temporal_harvest, time_range = 37, species = c("Cod"), return_data = TRUE)
cod_abundance_year38_temporal <- plotSpectra(sim_params_temporal_harvest, time_range = 38, species = c("Cod"), return_data = TRUE)
cod_abundance_year39_temporal <- plotSpectra(sim_params_temporal_harvest, time_range = 39, species = c("Cod"), return_data = TRUE)
cod_abundance_year40_temporal <- plotSpectra(sim_params_temporal_harvest, time_range = 40, species = c("Cod"), return_data = TRUE)
cod_abundance_year41_temporal <- plotSpectra(sim_params_temporal_harvest, time_range = 41, species = c("Cod"), return_data = TRUE)
cod_abundance_year42_temporal <- plotSpectra(sim_params_temporal_harvest, time_range = 42, species = c("Cod"), return_data = TRUE)
cod_abundance_year43_temporal <- plotSpectra(sim_params_temporal_harvest, time_range = 43, species = c("Cod"), return_data = TRUE)
cod_abundance_year44_temporal <- plotSpectra(sim_params_temporal_harvest, time_range = 44, species = c("Cod"), return_data = TRUE)
cod_abundance_year45_temporal <- plotSpectra(sim_params_temporal_harvest, time_range = 45, species = c("Cod"), return_data = TRUE)
cod_abundance_year46_temporal <- plotSpectra(sim_params_temporal_harvest, time_range = 46, species = c("Cod"), return_data = TRUE)
cod_abundance_year47_temporal <- plotSpectra(sim_params_temporal_harvest, time_range = 47, species = c("Cod"), return_data = TRUE)
cod_abundance_year48_temporal <- plotSpectra(sim_params_temporal_harvest, time_range = 48, species = c("Cod"), return_data = TRUE)
cod_abundance_year49_temporal <- plotSpectra(sim_params_temporal_harvest, time_range = 49, species = c("Cod"), return_data = TRUE)
cod_abundance_year50_temporal <- plotSpectra(sim_params_temporal_harvest, time_range = 50, species = c("Cod"), return_data = TRUE)



cod_abundance_year1_temporal$Year <- 1
cod_abundance_year2_temporal$Year <- 2
cod_abundance_year3_temporal$Year <- 3
cod_abundance_year4_temporal$Year <- 4
cod_abundance_year5_temporal$Year <- 5
cod_abundance_year6_temporal$Year <- 6
cod_abundance_year7_temporal$Year <- 7
cod_abundance_year8_temporal$Year <- 8
cod_abundance_year9_temporal$Year <- 9
cod_abundance_year10_temporal$Year <- 10
cod_abundance_year11_temporal$Year <- 11
cod_abundance_year12_temporal$Year <- 12
cod_abundance_year13_temporal$Year <- 13
cod_abundance_year14_temporal$Year <- 14
cod_abundance_year15_temporal$Year <- 15
cod_abundance_year16_temporal$Year <- 16
cod_abundance_year17_temporal$Year <- 17
cod_abundance_year18_temporal$Year <- 18
cod_abundance_year19_temporal$Year <- 19
cod_abundance_year20_temporal$Year <- 20
cod_abundance_year21_temporal$Year <- 21
cod_abundance_year22_temporal$Year <- 22
cod_abundance_year23_temporal$Year <- 23
cod_abundance_year24_temporal$Year <- 24
cod_abundance_year25_temporal$Year <- 25
cod_abundance_year26_temporal$Year <- 26
cod_abundance_year27_temporal$Year <- 27
cod_abundance_year28_temporal$Year <- 28
cod_abundance_year29_temporal$Year <- 29
cod_abundance_year30_temporal$Year <- 30
cod_abundance_year31_temporal$Year <- 31
cod_abundance_year32_temporal$Year <- 32
cod_abundance_year33_temporal$Year <- 33
cod_abundance_year34_temporal$Year <- 34
cod_abundance_year35_temporal$Year <- 35
cod_abundance_year36_temporal$Year <- 36
cod_abundance_year37_temporal$Year <- 37
cod_abundance_year38_temporal$Year <- 38
cod_abundance_year39_temporal$Year <- 39
cod_abundance_year40_temporal$Year <- 40
cod_abundance_year41_temporal$Year <- 41
cod_abundance_year42_temporal$Year <- 42
cod_abundance_year43_temporal$Year <- 43
cod_abundance_year44_temporal$Year <- 44
cod_abundance_year45_temporal$Year <- 45
cod_abundance_year46_temporal$Year <- 46
cod_abundance_year47_temporal$Year <- 47
cod_abundance_year48_temporal$Year <- 48
cod_abundance_year49_temporal$Year <- 49
cod_abundance_year50_temporal$Year <- 50


combined_cod_abundance_temporal <- rbind(
  cod_abundance_year1_temporal,
  cod_abundance_year2_temporal,
  cod_abundance_year3_temporal,
  cod_abundance_year4_temporal,
  cod_abundance_year5_temporal,
  cod_abundance_year6_temporal,
  cod_abundance_year7_temporal,
  cod_abundance_year8_temporal,
  cod_abundance_year9_temporal,
  cod_abundance_year10_temporal,
  cod_abundance_year11_temporal,
  cod_abundance_year12_temporal,
  cod_abundance_year13_temporal,
  cod_abundance_year14_temporal,
  cod_abundance_year15_temporal,
  cod_abundance_year16_temporal,
  cod_abundance_year17_temporal,
  cod_abundance_year18_temporal,
  cod_abundance_year19_temporal,
  cod_abundance_year20_temporal,
  cod_abundance_year21_temporal,
  cod_abundance_year22_temporal,
  cod_abundance_year23_temporal,
  cod_abundance_year24_temporal,
  cod_abundance_year25_temporal,
  cod_abundance_year26_temporal,
  cod_abundance_year27_temporal,
  cod_abundance_year28_temporal,
  cod_abundance_year29_temporal,
  cod_abundance_year30_temporal,
  cod_abundance_year31_temporal,
  cod_abundance_year32_temporal,
  cod_abundance_year33_temporal,
  cod_abundance_year34_temporal,
  cod_abundance_year35_temporal,
  cod_abundance_year36_temporal,
  cod_abundance_year37_temporal,
  cod_abundance_year38_temporal,
  cod_abundance_year39_temporal,
  cod_abundance_year40_temporal,
  cod_abundance_year41_temporal,
  cod_abundance_year42_temporal,
  cod_abundance_year43_temporal,
  cod_abundance_year44_temporal,
  cod_abundance_year45_temporal,
  cod_abundance_year46_temporal,
  cod_abundance_year47_temporal,
  cod_abundance_year48_temporal,
  cod_abundance_year49_temporal,
  cod_abundance_year50_temporal
)


combined_cod_abundance_temporal <- combined_cod_abundance_temporal[combined_cod_abundance_temporal$Species != "Resource", ]

combined_cod_abundance_double


write.csv(combined_cod_abundance_temporal, file = "cod_psd_temporalV3.csv", row.names = FALSE)
# add to excel 

temporal_to_be_mean_data_cod <- read.csv("cod_diet_size_class_all_years_temporalV2.csv")
temporal_to_be_mean_data_cod


year_1_values_temporal <- temporal_to_be_mean_data_cod[temporal_to_be_mean_data_cod$Year == 1, ]
year_2_values_temporal <- temporal_to_be_mean_data_cod[temporal_to_be_mean_data_cod$Year == 2, ]
year_3_values_temporal <- temporal_to_be_mean_data_cod[temporal_to_be_mean_data_cod$Year == 3, ]
year_4_values_temporal <- temporal_to_be_mean_data_cod[temporal_to_be_mean_data_cod$Year == 4, ]
year_5_values_temporal <- temporal_to_be_mean_data_cod[temporal_to_be_mean_data_cod$Year == 5, ]
year_6_values_temporal <- temporal_to_be_mean_data_cod[temporal_to_be_mean_data_cod$Year == 6, ]
year_7_values_temporal <- temporal_to_be_mean_data_cod[temporal_to_be_mean_data_cod$Year == 7, ]
year_8_values_temporal <- temporal_to_be_mean_data_cod[temporal_to_be_mean_data_cod$Year == 8, ]
year_9_values_temporal <- temporal_to_be_mean_data_cod[temporal_to_be_mean_data_cod$Year == 9, ]
year_10_values_temporal <- temporal_to_be_mean_data_cod[temporal_to_be_mean_data_cod$Year == 10, ]
year_11_values_temporal <- temporal_to_be_mean_data_cod[temporal_to_be_mean_data_cod$Year == 11, ]
year_12_values_temporal <- temporal_to_be_mean_data_cod[temporal_to_be_mean_data_cod$Year == 12, ]
year_13_values_temporal <- temporal_to_be_mean_data_cod[temporal_to_be_mean_data_cod$Year == 13, ]
year_14_values_temporal <- temporal_to_be_mean_data_cod[temporal_to_be_mean_data_cod$Year == 14, ]
year_15_values_temporal <- temporal_to_be_mean_data_cod[temporal_to_be_mean_data_cod$Year == 15, ]
year_16_values_temporal <- temporal_to_be_mean_data_cod[temporal_to_be_mean_data_cod$Year == 16, ]
year_17_values_temporal <- temporal_to_be_mean_data_cod[temporal_to_be_mean_data_cod$Year == 17, ]
year_18_values_temporal <- temporal_to_be_mean_data_cod[temporal_to_be_mean_data_cod$Year == 18, ]
year_19_values_temporal <- temporal_to_be_mean_data_cod[temporal_to_be_mean_data_cod$Year == 19, ]
year_20_values_temporal <- temporal_to_be_mean_data_cod[temporal_to_be_mean_data_cod$Year == 20, ]
year_21_values_temporal <- temporal_to_be_mean_data_cod[temporal_to_be_mean_data_cod$Year == 21, ]
year_22_values_temporal <- temporal_to_be_mean_data_cod[temporal_to_be_mean_data_cod$Year == 22, ]
year_23_values_temporal <- temporal_to_be_mean_data_cod[temporal_to_be_mean_data_cod$Year == 23, ]
year_24_values_temporal <- temporal_to_be_mean_data_cod[temporal_to_be_mean_data_cod$Year == 24, ]
year_25_values_temporal <- temporal_to_be_mean_data_cod[temporal_to_be_mean_data_cod$Year == 25, ]
year_26_values_temporal <- temporal_to_be_mean_data_cod[temporal_to_be_mean_data_cod$Year == 26, ]
year_27_values_temporal <- temporal_to_be_mean_data_cod[temporal_to_be_mean_data_cod$Year == 27, ]
year_28_values_temporal <- temporal_to_be_mean_data_cod[temporal_to_be_mean_data_cod$Year == 28, ]
year_29_values_temporal <- temporal_to_be_mean_data_cod[temporal_to_be_mean_data_cod$Year == 29, ]
year_30_values_temporal <- temporal_to_be_mean_data_cod[temporal_to_be_mean_data_cod$Year == 30, ]
year_31_values_temporal <- temporal_to_be_mean_data_cod[temporal_to_be_mean_data_cod$Year == 31, ]
year_32_values_temporal <- temporal_to_be_mean_data_cod[temporal_to_be_mean_data_cod$Year == 32, ]
year_33_values_temporal <- temporal_to_be_mean_data_cod[temporal_to_be_mean_data_cod$Year == 33, ]
year_34_values_temporal <- temporal_to_be_mean_data_cod[temporal_to_be_mean_data_cod$Year == 34, ]
year_35_values_temporal <- temporal_to_be_mean_data_cod[temporal_to_be_mean_data_cod$Year == 35, ]
year_36_values_temporal <- temporal_to_be_mean_data_cod[temporal_to_be_mean_data_cod$Year == 36, ]
year_37_values_temporal <- temporal_to_be_mean_data_cod[temporal_to_be_mean_data_cod$Year == 37, ]
year_38_values_temporal <- temporal_to_be_mean_data_cod[temporal_to_be_mean_data_cod$Year == 38, ]
year_39_values_temporal <- temporal_to_be_mean_data_cod[temporal_to_be_mean_data_cod$Year == 39, ]
year_40_values_temporal <- temporal_to_be_mean_data_cod[temporal_to_be_mean_data_cod$Year == 40, ]
year_41_values_temporal <- temporal_to_be_mean_data_cod[temporal_to_be_mean_data_cod$Year == 41, ]
year_42_values_temporal <- temporal_to_be_mean_data_cod[temporal_to_be_mean_data_cod$Year == 42, ]
year_43_values_temporal <- temporal_to_be_mean_data_cod[temporal_to_be_mean_data_cod$Year == 43, ]
year_44_values_temporal <- temporal_to_be_mean_data_cod[temporal_to_be_mean_data_cod$Year == 44, ]
year_45_values_temporal <- temporal_to_be_mean_data_cod[temporal_to_be_mean_data_cod$Year == 45, ]
year_46_values_temporal <- temporal_to_be_mean_data_cod[temporal_to_be_mean_data_cod$Year == 46, ]
year_47_values_temporal <- temporal_to_be_mean_data_cod[temporal_to_be_mean_data_cod$Year == 47, ]
year_48_values_temporal <- temporal_to_be_mean_data_cod[temporal_to_be_mean_data_cod$Year == 48, ]
year_49_values_temporal <- temporal_to_be_mean_data_cod[temporal_to_be_mean_data_cod$Year == 49, ]
year_50_values_temporal <- temporal_to_be_mean_data_cod[temporal_to_be_mean_data_cod$Year == 50, ]



# Loop

# Create a list to store the values for each year
year_values <- list(year_1_values_temporal, year_2_values_temporal, year_3_values_temporal, 
                    year_4_values_temporal, year_5_values_temporal, year_6_values_temporal, 
                    year_7_values_temporal, year_8_values_temporal, year_9_values_temporal, 
                    year_10_values_temporal, year_11_values_temporal, year_12_values_temporal, 
                    year_13_values_temporal, year_14_values_temporal, year_15_values_temporal, 
                    year_16_values_temporal, year_17_values_temporal, year_18_values_temporal, 
                    year_19_values_temporal, year_20_values_temporal,
                    year_21_values_temporal, year_22_values_temporal, year_23_values_temporal,
                    year_24_values_temporal, year_25_values_temporal, year_26_values_temporal,
                    year_27_values_temporal, year_28_values_temporal, year_29_values_temporal,
                    year_30_values_temporal, year_31_values_temporal, year_32_values_temporal,
                    year_33_values_temporal, year_34_values_temporal, year_35_values_temporal,
                    year_36_values_temporal, year_37_values_temporal, year_38_values_temporal,
                    year_39_values_temporal, year_40_values_temporal, year_41_values_temporal,
                    year_42_values_temporal, year_43_values_temporal, year_44_values_temporal, 
                    year_45_values_temporal, year_46_values_temporal, year_47_values_temporal, 
                    year_48_values_temporal, year_49_values_temporal, year_50_values_temporal)

length(year_values)

# Create a vector of years
years <- 0:49
length(year_values)
# Loop through each year
for (i in seq_along(years)) {
  year <- years[i]
  year_data <- year_values[[i]]
  
  # Generate the variable name prefix based on the year
  year_prefix <- paste0("year_", year, "_temporal$")
  
  # Access the PSD values for the current year
  PSD_values <- year_data$PSD
  
  # Loop through each species and calculate the mean diet value
  for (s in species) {
    # Generate the variable name for the current species
    species_variable <- paste0("mean_diet_", s, "_temporal_", year)
    
    # Calculate the sum of the product
    species_sum <- sum(year_data[[s]] * PSD_values)
    
    # Calculate the mean diet value
    assign(species_variable, species_sum / sum(PSD_values))
  }
}

# View the mean diet values for each species and each year
for (s in species) {
  print(paste("Species:", s))
  for (y in years) {
    variable_name <- paste0("mean_diet_", s, "_temporal_", y)
    mean_diet_value <- get(variable_name)
    print(paste("Year:", y, "| Mean Diet Value:", mean_diet_value))
  }
  cat("\n")
}

# Create an empty data frame to store the results
results_df <- data.frame(Species = character(), Year = numeric(), Mean_Diet_Value = numeric())

# Iterate over species and years
for (s in species) {
  for (y in years) {
    variable_name <- paste0("mean_diet_", s, "_temporal_", y)
    mean_diet_value <- get(variable_name)
    
    # Add the data to the results data frame
    results_df <- rbind(results_df, data.frame(Species = s, Year = y, Mean_Diet_Value = mean_diet_value))
  }
}

# Write the results to a CSV file
write.csv(results_df, "mean_diet_results_temporal_year1to50.csv", row.names = FALSE)

mean_diet_results_temporal_yr1to50 <- read.csv("mean_diet_results_temporal_year1to50.csv")

library(ggplot2)

# Filter the data for the selected species
selected_species <- c("Sandeel", "Herring", "Haddock", "Plaice")
filtered_data <- mean_diet_results_temporal_yr1to50[mean_diet_results_temporal_yr1to50$Species %in% selected_species, ]

# Plot the data


ggplot(filtered_data, aes(x = Year, y = Mean_Diet_Value, color = Species)) +
  geom_line(linewidth = 1) +
  geom_rect(aes(xmin = 15, xmax = 19, ymin = -Inf, ymax = Inf), fill = NA, color = "black", alpha = 0.3) +
  geom_rect(aes(xmin = 25, xmax = 29, ymin = -Inf, ymax = Inf), fill = NA, color = "black", alpha = 0.3) +
  geom_rect(aes(xmin = 35, xmax = 39, ymin = -Inf, ymax = Inf), fill = NA, color = "black", alpha = 0.3) +
  geom_rect(aes(xmin = 45, xmax = 49, ymin = -Inf, ymax = Inf), fill = NA, color = "black", alpha = 0.3) +
  geom_rect(aes(xmin = 5, xmax = 9, ymin = -Inf, ymax = Inf), fill = NA, color = "black", alpha = 0.3) +
  geom_label(aes(x = 17, y = Inf, label = "PH"), vjust = 1, hjust = 0.5, color = "black", size = 3) +
  geom_label(aes(x = 27, y = Inf, label = "PH"), vjust = 1, hjust = 0.5, color = "black", size = 3) +
  geom_label(aes(x = 37, y = Inf, label = "PH"), vjust = 1, hjust = 0.5, color = "black", size = 3) +
  geom_label(aes(x = 47, y = Inf, label = "PH"), vjust = 1, hjust = 0.5, color = "black", size = 3) +
  geom_label(aes(x = 7, y = Inf, label = "PH"), vjust = 1, hjust = 0.5, color = "black", size = 3) +
  labs(x = "Time step (Year)", y = "Mean Diet of Cod (g/year)", title = "E.) Periodic Harvest", color = "Prey Species") +  # Change legend title
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, 49, by = 1), labels = seq(0, 49, by = 1)) +  # Change x-axis labels
  scale_y_continuous(breaks = seq(0, ceiling(max(filtered_data$Mean_Diet_Value)), by = 50),
                     limits = c(0, max(filtered_data$Mean_Diet_Value) + 50),
                     labels = function(x) ifelse(x == max(filtered_data$Mean_Diet_Value), "50", as.character(x))) +  # Adjust y-axis breaks, limits, and label
  theme(panel.grid.major = element_line(color = "grey", linetype = "dotted"),  # More gridlines
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),  # Rotate x-axis labels
        axis.text.y = element_text(size = 8))  # Adjust y-axis label size








### Double intensity ###

## Diet cod ##
getDiet(params_temporal_harvest)

# Initialize an empty list to store diet rates for each year
diet_rates_double <- list()

# Loop over each year (time step)
for (year in 1:50) {
  # Extract biomass values for the current year
  n <- sim_params_temporal_harvest_double@n[year,,]
  n_pp <- sim_params_temporal_harvest_double@n_pp[year,]
  
  # Get diet information for the current year and store it in the list
  diet_rates_double[[year]] <- getDiet(params_temporal_harvest, n, n_pp, proportion = FALSE)
}


predator_cod_double <- list()

# Loop over each year
for (year in 1:50) {
  # Extract predator Cod data for the current year
  predator_cod_double[[year]] <- diet_rates_double[[year]][11,,]
}
predator_cod_double

###########


year_1_predator_cod_double <- predator_cod_double[[1]]
year_2_predator_cod_double <- predator_cod_double[[2]]
year_3_predator_cod_double <- predator_cod_double[[3]]
year_4_predator_cod_double <- predator_cod_double[[4]]
year_5_predator_cod_double <- predator_cod_double[[5]]
year_6_predator_cod_double <- predator_cod_double[[6]]
year_7_predator_cod_double <- predator_cod_double[[7]]
year_8_predator_cod_double <- predator_cod_double[[8]]
year_9_predator_cod_double <- predator_cod_double[[9]]
year_10_predator_cod_double <- predator_cod_double[[10]]
year_11_predator_cod_double <- predator_cod_double[[11]]
year_12_predator_cod_double <- predator_cod_double[[12]]
year_13_predator_cod_double <- predator_cod_double[[13]]
year_14_predator_cod_double <- predator_cod_double[[14]]
year_15_predator_cod_double <- predator_cod_double[[15]]
year_16_predator_cod_double <- predator_cod_double[[16]]
year_17_predator_cod_double <- predator_cod_double[[17]]
year_18_predator_cod_double <- predator_cod_double[[18]]
year_19_predator_cod_double <- predator_cod_double[[19]]
year_20_predator_cod_double <- predator_cod_double[[20]]
year_21_predator_cod_double <- predator_cod_double[[21]]
year_22_predator_cod_double <- predator_cod_double[[22]]
year_23_predator_cod_double <- predator_cod_double[[23]]
year_24_predator_cod_double <- predator_cod_double[[24]]
year_25_predator_cod_double <- predator_cod_double[[25]]
year_26_predator_cod_double <- predator_cod_double[[26]]
year_27_predator_cod_double <- predator_cod_double[[27]]
year_28_predator_cod_double <- predator_cod_double[[28]]
year_29_predator_cod_double <- predator_cod_double[[29]]
year_30_predator_cod_double <- predator_cod_double[[30]]
year_31_predator_cod_double <- predator_cod_double[[31]]
year_32_predator_cod_double <- predator_cod_double[[32]]
year_33_predator_cod_double <- predator_cod_double[[33]]
year_34_predator_cod_double <- predator_cod_double[[34]]
year_35_predator_cod_double <- predator_cod_double[[35]]
year_36_predator_cod_double <- predator_cod_double[[36]]
year_37_predator_cod_double <- predator_cod_double[[37]]
year_38_predator_cod_double <- predator_cod_double[[38]]
year_39_predator_cod_double <- predator_cod_double[[39]]
year_40_predator_cod_double <- predator_cod_double[[40]]
year_41_predator_cod_double <- predator_cod_double[[41]]
year_42_predator_cod_double <- predator_cod_double[[42]]
year_43_predator_cod_double <- predator_cod_double[[43]]
year_44_predator_cod_double <- predator_cod_double[[44]]
year_45_predator_cod_double <- predator_cod_double[[45]]
year_46_predator_cod_double <- predator_cod_double[[46]]
year_47_predator_cod_double <- predator_cod_double[[47]]
year_48_predator_cod_double <- predator_cod_double[[48]]
year_49_predator_cod_double <- predator_cod_double[[49]]
year_50_predator_cod_double <- predator_cod_double[[50]]



# Write data for year 1 to CSV file
write.csv(year_1_predator_cod_double, file = "year_1_predator_cod_double_v9.csv", row.names = FALSE)

# Write data for year 2 to CSV file
write.csv(year_2_predator_cod_double, file = "year_2_predator_cod_double_v9.csv", row.names = FALSE)

# Write data for year 3 to CSV file
write.csv(year_3_predator_cod_double, file = "year_3_predator_cod_double_v9.csv", row.names = FALSE)

# Write data for year 4 to CSV file
write.csv(year_4_predator_cod_double, file = "year_4_predator_cod_double_v9.csv", row.names = FALSE)

# Write data for year 5 to CSV file
write.csv(year_5_predator_cod_double, file = "year_5_predator_cod_double_v9.csv", row.names = FALSE)

# Write data for year 6 to CSV file
write.csv(year_6_predator_cod_double, file = "year_6_predator_cod_double_v9.csv", row.names = FALSE)

# Write data for year 7 to CSV file
write.csv(year_7_predator_cod_double, file = "year_7_predator_cod_double_v9.csv", row.names = FALSE)

# Write data for year 8 to CSV file
write.csv(year_8_predator_cod_double, file = "year_8_predator_cod_double_v9.csv", row.names = FALSE)

# Write data for year 9 to CSV file
write.csv(year_9_predator_cod_double, file = "year_9_predator_cod_double_v9.csv", row.names = FALSE)

# Write data for year 10 to CSV file
write.csv(year_10_predator_cod_double, file = "year_10_predator_cod_double_v9.csv", row.names = FALSE)

# Write data for year 11 to CSV file
write.csv(year_11_predator_cod_double, file = "year_11_predator_cod_double_v9.csv", row.names = FALSE)

# Write data for year 12 to CSV file
write.csv(year_12_predator_cod_double, file = "year_12_predator_cod_double_v9.csv", row.names = FALSE)

# Write data for year 13 to CSV file
write.csv(year_13_predator_cod_double, file = "year_13_predator_cod_double_v9.csv", row.names = FALSE)


# Write data for year 14 to CSV file
write.csv(year_14_predator_cod_double, file = "year_14_predator_cod_double_v9.csv", row.names = FALSE)

# Write data for year 15 to CSV file
write.csv(year_15_predator_cod_double, file = "year_15_predator_cod_double_v9.csv", row.names = FALSE)

# Write data for year 16 to CSV file
write.csv(year_16_predator_cod_double, file = "year_16_predator_cod_double_v9.csv", row.names = FALSE)

# Write data for year 17 to CSV file
write.csv(year_17_predator_cod_double, file = "year_17_predator_cod_double_v9.csv", row.names = FALSE)

# Write data for year 18 to CSV file
write.csv(year_18_predator_cod_double, file = "year_18_predator_cod_double_v9.csv", row.names = FALSE)

# Write data for year 19 to CSV file
write.csv(year_19_predator_cod_double, file = "year_19_predator_cod_double_v9.csv", row.names = FALSE)

# Write data for year 20 to CSV file
write.csv(year_20_predator_cod_double, file = "year_20_predator_cod_double_v9.csv", row.names = FALSE)

# Write data for year 21 to CSV file
write.csv(year_21_predator_cod_double, file = "year_21_predator_cod_double_v9.csv", row.names = FALSE)

# Write data for year 22 to CSV file
write.csv(year_22_predator_cod_double, file = "year_22_predator_cod_double_v9.csv", row.names = FALSE)

# Write data for year 23 to CSV file
write.csv(year_23_predator_cod_double, file = "year_23_predator_cod_double_v9.csv", row.names = FALSE)

# Write data for year 24 to CSV file
write.csv(year_24_predator_cod_double, file = "year_24_predator_cod_double_v9.csv", row.names = FALSE)

# Write data for year 25 to CSV file
write.csv(year_25_predator_cod_double, file = "year_25_predator_cod_double_v9.csv", row.names = FALSE)

# Write data for year 26 to CSV file
write.csv(year_26_predator_cod_double, file = "year_26_predator_cod_double_v9.csv", row.names = FALSE)

# Write data for year 27 to CSV file
write.csv(year_27_predator_cod_double, file = "year_27_predator_cod_double_v9.csv", row.names = FALSE)

# Write data for year 28 to CSV file
write.csv(year_28_predator_cod_double, file = "year_28_predator_cod_double_v9.csv", row.names = FALSE)

# Write data for year 29 to CSV file
write.csv(year_29_predator_cod_double, file = "year_29_predator_cod_double_v9.csv", row.names = FALSE)

# Write data for year 30 to CSV file
write.csv(year_30_predator_cod_double, file = "year_30_predator_cod_double_v9.csv", row.names = FALSE)

# Write data for year 31 to CSV file
write.csv(year_31_predator_cod_double, file = "year_31_predator_cod_double_v9.csv", row.names = FALSE)

# Write data for year 32 to CSV file
write.csv(year_32_predator_cod_double, file = "year_32_predator_cod_double_v9.csv", row.names = FALSE)

# Write data for year 33 to CSV file
write.csv(year_33_predator_cod_double, file = "year_33_predator_cod_double_v9.csv", row.names = FALSE)

# Write data for year 34 to CSV file
write.csv(year_34_predator_cod_double, file = "year_34_predator_cod_double_v9.csv", row.names = FALSE)

# Write data for year 35 to CSV file
write.csv(year_35_predator_cod_double, file = "year_35_predator_cod_double_v9.csv", row.names = FALSE)

# Write data for year 36 to CSV file
write.csv(year_36_predator_cod_double, file = "year_36_predator_cod_double_v9.csv", row.names = FALSE)

# Write data for year 37 to CSV file
write.csv(year_37_predator_cod_double, file = "year_37_predator_cod_double_v9.csv", row.names = FALSE)

# Write data for year 38 to CSV file
write.csv(year_38_predator_cod_double, file = "year_38_predator_cod_double_v9.csv", row.names = FALSE)

# Write data for year 39 to CSV file
write.csv(year_39_predator_cod_double, file = "year_39_predator_cod_double_v9.csv", row.names = FALSE)

# Write data for year 40 to CSV file
write.csv(year_40_predator_cod_double, file = "year_40_predator_cod_double_v9.csv", row.names = FALSE)

# Write data for year 41 to CSV file
write.csv(year_41_predator_cod_double, file = "year_41_predator_cod_double_v9.csv", row.names = FALSE)

# Write data for year 42 to CSV file
write.csv(year_42_predator_cod_double, file = "year_42_predator_cod_double_v9.csv", row.names = FALSE)

# Write data for year 43 to CSV file
write.csv(year_43_predator_cod_double, file = "year_43_predator_cod_double_v9.csv", row.names = FALSE)

# Write data for year 44 to CSV file
write.csv(year_44_predator_cod_double, file = "year_44_predator_cod_double_v9.csv", row.names = FALSE)
# Write data for year 45 to CSV file
write.csv(year_45_predator_cod_double, file = "year_45_predator_cod_double_v9.csv", row.names = FALSE)

# Write data for year 46 to CSV file
write.csv(year_46_predator_cod_double, file = "year_46_predator_cod_double_v9.csv", row.names = FALSE)

# Write data for year 47 to CSV file
write.csv(year_47_predator_cod_double, file = "year_47_predator_cod_double_v9.csv", row.names = FALSE)

# Write data for year 48 to CSV file
write.csv(year_48_predator_cod_double, file = "year_48_predator_cod_double_v9.csv", row.names = FALSE)

# Write data for year 49 to CSV file
write.csv(year_49_predator_cod_double, file = "year_49_predator_cod_double_v9.csv", row.names = FALSE)

# Write data for year 50 to CSV file
write.csv(year_50_predator_cod_double, file = "year_50_predator_cod_double_v9.csv", row.names = FALSE)


year_1_cod_diet_size_class_double <- read.csv("year_1_predator_cod_double_v9.csv")
year_2_cod_diet_size_class_double <- read.csv("year_2_predator_cod_double_v9.csv")
year_3_cod_diet_size_class_double <- read.csv("year_3_predator_cod_double_v9.csv")
year_4_cod_diet_size_class_double <- read.csv("year_4_predator_cod_double_v9.csv")
year_5_cod_diet_size_class_double <- read.csv("year_5_predator_cod_double_v9.csv")
year_6_cod_diet_size_class_double <- read.csv("year_6_predator_cod_double_v9.csv")
year_7_cod_diet_size_class_double <- read.csv("year_7_predator_cod_double_v9.csv")
year_8_cod_diet_size_class_double <- read.csv("year_8_predator_cod_double_v9.csv")
year_9_cod_diet_size_class_double <- read.csv("year_9_predator_cod_double_v9.csv")
year_10_cod_diet_size_class_double <- read.csv("year_10_predator_cod_double_v9.csv")
year_11_cod_diet_size_class_double <- read.csv("year_11_predator_cod_double_v9.csv")
year_12_cod_diet_size_class_double <- read.csv("year_12_predator_cod_double_v9.csv")
year_13_cod_diet_size_class_double <- read.csv("year_13_predator_cod_double_v9.csv")

## Read data for year 14 from CSV file
year_14_cod_diet_size_class_double <- read.csv("year_14_predator_cod_double_v9.csv")
year_14_cod_diet_size_class_double

## Read data for year 15 from CSV file
year_15_cod_diet_size_class_double <- read.csv("year_15_predator_cod_double_v9.csv")

## Read data for year 16 from CSV file
year_16_cod_diet_size_class_double <- read.csv("year_16_predator_cod_double_v9.csv")

## Read data for year 17 from CSV file
year_17_cod_diet_size_class_double <- read.csv("year_17_predator_cod_double_v9.csv")

## Read data for year 18 from CSV file
year_18_cod_diet_size_class_double <- read.csv("year_18_predator_cod_double_v9.csv")

## Read data for year 19 from CSV file
year_19_cod_diet_size_class_double <- read.csv("year_19_predator_cod_double_v9.csv")

## Read data for year 20 from CSV file
year_20_cod_diet_size_class_double <- read.csv("year_20_predator_cod_double_v9.csv")

## Read data for year 21 from CSV file
year_21_cod_diet_size_class_double <- read.csv("year_21_predator_cod_double_v9.csv")

## Read data for year 22 from CSV file
year_22_cod_diet_size_class_double <- read.csv("year_22_predator_cod_double_v9.csv")

## Read data for year 23 from CSV file
year_23_cod_diet_size_class_double <- read.csv("year_23_predator_cod_double_v9.csv")

## Read data for year 24 from CSV file
year_24_cod_diet_size_class_double <- read.csv("year_24_predator_cod_double_v9.csv")

## Read data for year 25 from CSV file
year_25_cod_diet_size_class_double <- read.csv("year_25_predator_cod_double_v9.csv")

## Read data for year 26 from CSV file
year_26_cod_diet_size_class_double <- read.csv("year_26_predator_cod_double_v9.csv")

## Read data for year 27 from CSV file
year_27_cod_diet_size_class_double <- read.csv("year_27_predator_cod_double_v9.csv")

## Read data for year 28 from CSV file
year_28_cod_diet_size_class_double <- read.csv("year_28_predator_cod_double_v9.csv")

## Read data for year 29 from CSV file
year_29_cod_diet_size_class_double <- read.csv("year_29_predator_cod_double_v9.csv")

## Read data for year 30 from CSV file
year_30_cod_diet_size_class_double <- read.csv("year_30_predator_cod_double_v9.csv")

## Read data for year 31 from CSV file
year_31_cod_diet_size_class_double <- read.csv("year_31_predator_cod_double_v9.csv")

## Read data for year 32 from CSV file
year_32_cod_diet_size_class_double <- read.csv("year_32_predator_cod_double_v9.csv")

## Read data for year 33 from CSV file
year_33_cod_diet_size_class_double <- read.csv("year_33_predator_cod_double_v9.csv")

## Read data for year 34 from CSV file
year_34_cod_diet_size_class_double <- read.csv("year_34_predator_cod_double_v9.csv")

## Read data for year 35 from CSV file
year_35_cod_diet_size_class_double <- read.csv("year_35_predator_cod_double_v9.csv")

## Read data for year 36 from CSV file
year_36_cod_diet_size_class_double <- read.csv("year_36_predator_cod_double_v9.csv")

## Read data for year 37 from CSV file
year_37_cod_diet_size_class_double <- read.csv("year_37_predator_cod_double_v9.csv")

## Read data for year 38 from CSV file
year_38_cod_diet_size_class_double <- read.csv("year_38_predator_cod_double_v9.csv")

## Read data for year 39 from CSV file
year_39_cod_diet_size_class_double <- read.csv("year_39_predator_cod_double_v9.csv")

## Read data for year 40 from CSV file
year_40_cod_diet_size_class_double <- read.csv("year_40_predator_cod_double_v9.csv")

## Read data for year 41 from CSV file
year_41_cod_diet_size_class_double <- read.csv("year_41_predator_cod_double_v9.csv")

## Read data for year 42 from CSV file
year_42_cod_diet_size_class_double <- read.csv("year_42_predator_cod_double_v9.csv")

## Read data for year 43 from CSV file
year_43_cod_diet_size_class_double <- read.csv("year_43_predator_cod_double_v9.csv")

## Read data for year 44 from CSV file
year_44_cod_diet_size_class_double <- read.csv("year_44_predator_cod_double_v9.csv")
year_45_cod_diet_size_class_double <- read.csv("year_45_predator_cod_double_v9.csv")
year_46_cod_diet_size_class_double <- read.csv("year_46_predator_cod_double_v9.csv")
year_47_cod_diet_size_class_double <- read.csv("year_47_predator_cod_double_v9.csv")
year_48_cod_diet_size_class_double <- read.csv("year_48_predator_cod_double_v9.csv")
year_49_cod_diet_size_class_double <- read.csv("year_49_predator_cod_double_v9.csv")
year_50_cod_diet_size_class_double <- read.csv("year_50_predator_cod_double_v9.csv")


year_1_cod_diet_size_class_double$Year <- 1
year_2_cod_diet_size_class_double$Year <- 2
year_3_cod_diet_size_class_double$Year <- 3
year_4_cod_diet_size_class_double$Year <- 4
year_5_cod_diet_size_class_double$Year <- 5
year_6_cod_diet_size_class_double$Year <- 6
year_7_cod_diet_size_class_double$Year <- 7
year_8_cod_diet_size_class_double$Year <- 8
year_9_cod_diet_size_class_double$Year <- 9
year_10_cod_diet_size_class_double$Year <- 10
year_11_cod_diet_size_class_double$Year <- 11
year_12_cod_diet_size_class_double$Year <- 12
year_13_cod_diet_size_class_double$Year <- 13
year_14_cod_diet_size_class_double$Year <- 14
year_15_cod_diet_size_class_double$Year <- 15
year_16_cod_diet_size_class_double$Year <- 16
year_17_cod_diet_size_class_double$Year <- 17
year_18_cod_diet_size_class_double$Year <- 18
year_19_cod_diet_size_class_double$Year <- 19
year_20_cod_diet_size_class_double$Year <- 20
year_21_cod_diet_size_class_double$Year <- 21
year_22_cod_diet_size_class_double$Year <- 22
year_23_cod_diet_size_class_double$Year <- 23
year_24_cod_diet_size_class_double$Year <- 24
year_25_cod_diet_size_class_double$Year <- 25
year_26_cod_diet_size_class_double$Year <- 26
year_27_cod_diet_size_class_double$Year <- 27
year_28_cod_diet_size_class_double$Year <- 28
year_29_cod_diet_size_class_double$Year <- 29
year_30_cod_diet_size_class_double$Year <- 30
year_31_cod_diet_size_class_double$Year <- 31
year_32_cod_diet_size_class_double$Year <- 32
year_33_cod_diet_size_class_double$Year <- 33
year_34_cod_diet_size_class_double$Year <- 34
year_35_cod_diet_size_class_double$Year <- 35
year_36_cod_diet_size_class_double$Year <- 36
year_37_cod_diet_size_class_double$Year <- 37
year_38_cod_diet_size_class_double$Year <- 38
year_39_cod_diet_size_class_double$Year <- 39
year_40_cod_diet_size_class_double$Year <- 40
year_41_cod_diet_size_class_double$Year <- 41
year_42_cod_diet_size_class_double$Year <- 42
year_43_cod_diet_size_class_double$Year <- 43
year_44_cod_diet_size_class_double$Year <- 44
year_45_cod_diet_size_class_double$Year <- 45
year_46_cod_diet_size_class_double$Year <- 46
year_47_cod_diet_size_class_double$Year <- 47
year_48_cod_diet_size_class_double$Year <- 48
year_49_cod_diet_size_class_double$Year <- 49
year_50_cod_diet_size_class_double$Year <- 50

combined_data_cod_diet_size_class_temporal



combined_data_cod_diet_size_class_double <- rbind(
  year_1_cod_diet_size_class_double,
  year_2_cod_diet_size_class_double,
  year_3_cod_diet_size_class_double,
  year_4_cod_diet_size_class_double,
  year_5_cod_diet_size_class_double,
  year_6_cod_diet_size_class_double,
  year_7_cod_diet_size_class_double,
  year_8_cod_diet_size_class_double,
  year_9_cod_diet_size_class_double,
  year_10_cod_diet_size_class_double,
  year_11_cod_diet_size_class_double,
  year_12_cod_diet_size_class_double,
  year_13_cod_diet_size_class_double,
  year_14_cod_diet_size_class_double,
  year_15_cod_diet_size_class_double,
  year_16_cod_diet_size_class_double,
  year_17_cod_diet_size_class_double,
  year_18_cod_diet_size_class_double,
  year_19_cod_diet_size_class_double,
  year_20_cod_diet_size_class_double,
  year_21_cod_diet_size_class_double,
  year_22_cod_diet_size_class_double,
  year_23_cod_diet_size_class_double,
  year_24_cod_diet_size_class_double,
  year_25_cod_diet_size_class_double,
  year_26_cod_diet_size_class_double,
  year_27_cod_diet_size_class_double,
  year_28_cod_diet_size_class_double,
  year_29_cod_diet_size_class_double,
  year_30_cod_diet_size_class_double,
  year_31_cod_diet_size_class_double,
  year_32_cod_diet_size_class_double,
  year_33_cod_diet_size_class_double,
  year_34_cod_diet_size_class_double,
  year_35_cod_diet_size_class_double,
  year_36_cod_diet_size_class_double,
  year_37_cod_diet_size_class_double,
  year_38_cod_diet_size_class_double,
  year_39_cod_diet_size_class_double,
  year_40_cod_diet_size_class_double,
  year_41_cod_diet_size_class_double,
  year_42_cod_diet_size_class_double,
  year_43_cod_diet_size_class_double,
  year_44_cod_diet_size_class_double,
  year_45_cod_diet_size_class_double,
  year_46_cod_diet_size_class_double,
  year_47_cod_diet_size_class_double,
  year_48_cod_diet_size_class_double,
  year_49_cod_diet_size_class_double,
  year_50_cod_diet_size_class_double
)


write.csv(combined_data_cod_diet_size_class_double, file = "cod_diet_size_class_all_years_doubleV2.csv", row.names = FALSE)
# add cod size classes excel

cod_diet_size_class_all_years_double <- read.csv("cod_diet_size_class_all_years_doubleV2.csv")
cod_diet_size_class_all_years_double

cod_abundance_year1_double <- plotSpectra(sim_params_temporal_harvest_double, time_range = 1, species = c("Cod"), return_data = TRUE)
cod_abundance_year2_double <- plotSpectra(sim_params_temporal_harvest_double, time_range = 2, species = c("Cod"), return_data = TRUE)
cod_abundance_year3_double <- plotSpectra(sim_params_temporal_harvest_double, time_range = 3, species = c("Cod"), return_data = TRUE)
cod_abundance_year4_double <- plotSpectra(sim_params_temporal_harvest_double, time_range = 4, species = c("Cod"), return_data = TRUE)
cod_abundance_year5_double <- plotSpectra(sim_params_temporal_harvest_double, time_range = 5, species = c("Cod"), return_data = TRUE)
cod_abundance_year6_double <- plotSpectra(sim_params_temporal_harvest_double, time_range = 6, species = c("Cod"), return_data = TRUE)
cod_abundance_year7_double <- plotSpectra(sim_params_temporal_harvest_double, time_range = 7, species = c("Cod"), return_data = TRUE)
cod_abundance_year8_double <- plotSpectra(sim_params_temporal_harvest_double, time_range = 8, species = c("Cod"), return_data = TRUE)
cod_abundance_year9_double <- plotSpectra(sim_params_temporal_harvest_double, time_range = 9, species = c("Cod"), return_data = TRUE)
cod_abundance_year10_double <- plotSpectra(sim_params_temporal_harvest_double, time_range = 10, species = c("Cod"), return_data = TRUE)
cod_abundance_year11_double <- plotSpectra(sim_params_temporal_harvest_double, time_range = 11, species = c("Cod"), return_data = TRUE)
cod_abundance_year12_double <- plotSpectra(sim_params_temporal_harvest_double, time_range = 12, species = c("Cod"), return_data = TRUE)
cod_abundance_year13_double <- plotSpectra(sim_params_temporal_harvest_double, time_range = 13, species = c("Cod"), return_data = TRUE)
cod_abundance_year14_double <- plotSpectra(sim_params_temporal_harvest_double, time_range = 14, species = c("Cod"), return_data = TRUE)
cod_abundance_year15_double <- plotSpectra(sim_params_temporal_harvest_double, time_range = 15, species = c("Cod"), return_data = TRUE)
cod_abundance_year16_double <- plotSpectra(sim_params_temporal_harvest_double, time_range = 16, species = c("Cod"), return_data = TRUE)
cod_abundance_year17_double <- plotSpectra(sim_params_temporal_harvest_double, time_range = 17, species = c("Cod"), return_data = TRUE)
cod_abundance_year18_double <- plotSpectra(sim_params_temporal_harvest_double, time_range = 18, species = c("Cod"), return_data = TRUE)
cod_abundance_year19_double <- plotSpectra(sim_params_temporal_harvest_double, time_range = 19, species = c("Cod"), return_data = TRUE)
cod_abundance_year20_double <- plotSpectra(sim_params_temporal_harvest_double, time_range = 20, species = c("Cod"), return_data = TRUE)
cod_abundance_year21_double <- plotSpectra(sim_params_temporal_harvest_double, time_range = 21, species = c("Cod"), return_data = TRUE)
cod_abundance_year22_double <- plotSpectra(sim_params_temporal_harvest_double, time_range = 22, species = c("Cod"), return_data = TRUE)
cod_abundance_year23_double <- plotSpectra(sim_params_temporal_harvest_double, time_range = 23, species = c("Cod"), return_data = TRUE)
cod_abundance_year24_double <- plotSpectra(sim_params_temporal_harvest_double, time_range = 24, species = c("Cod"), return_data = TRUE)
cod_abundance_year25_double <- plotSpectra(sim_params_temporal_harvest_double, time_range = 25, species = c("Cod"), return_data = TRUE)
cod_abundance_year26_double <- plotSpectra(sim_params_temporal_harvest_double, time_range = 26, species = c("Cod"), return_data = TRUE)
cod_abundance_year27_double <- plotSpectra(sim_params_temporal_harvest_double, time_range = 27, species = c("Cod"), return_data = TRUE)
cod_abundance_year28_double <- plotSpectra(sim_params_temporal_harvest_double, time_range = 28, species = c("Cod"), return_data = TRUE)
cod_abundance_year29_double <- plotSpectra(sim_params_temporal_harvest_double, time_range = 29, species = c("Cod"), return_data = TRUE)
cod_abundance_year30_double <- plotSpectra(sim_params_temporal_harvest_double, time_range = 30, species = c("Cod"), return_data = TRUE)
cod_abundance_year31_double <- plotSpectra(sim_params_temporal_harvest_double, time_range = 31, species = c("Cod"), return_data = TRUE)
cod_abundance_year32_double <- plotSpectra(sim_params_temporal_harvest_double, time_range = 32, species = c("Cod"), return_data = TRUE)
cod_abundance_year33_double <- plotSpectra(sim_params_temporal_harvest_double, time_range = 33, species = c("Cod"), return_data = TRUE)
cod_abundance_year34_double <- plotSpectra(sim_params_temporal_harvest_double, time_range = 34, species = c("Cod"), return_data = TRUE)
cod_abundance_year35_double <- plotSpectra(sim_params_temporal_harvest_double, time_range = 35, species = c("Cod"), return_data = TRUE)
cod_abundance_year36_double <- plotSpectra(sim_params_temporal_harvest_double, time_range = 36, species = c("Cod"), return_data = TRUE)
cod_abundance_year37_double <- plotSpectra(sim_params_temporal_harvest_double, time_range = 37, species = c("Cod"), return_data = TRUE)
cod_abundance_year38_double <- plotSpectra(sim_params_temporal_harvest_double, time_range = 38, species = c("Cod"), return_data = TRUE)
cod_abundance_year39_double <- plotSpectra(sim_params_temporal_harvest_double, time_range = 39, species = c("Cod"), return_data = TRUE)
cod_abundance_year40_double <- plotSpectra(sim_params_temporal_harvest_double, time_range = 40, species = c("Cod"), return_data = TRUE)
cod_abundance_year41_double <- plotSpectra(sim_params_temporal_harvest_double, time_range = 41, species = c("Cod"), return_data = TRUE)
cod_abundance_year42_double <- plotSpectra(sim_params_temporal_harvest_double, time_range = 42, species = c("Cod"), return_data = TRUE)
cod_abundance_year43_double <- plotSpectra(sim_params_temporal_harvest_double, time_range = 43, species = c("Cod"), return_data = TRUE)
cod_abundance_year44_double <- plotSpectra(sim_params_temporal_harvest_double, time_range = 44, species = c("Cod"), return_data = TRUE)
cod_abundance_year45_double <- plotSpectra(sim_params_temporal_harvest_double, time_range = 45, species = c("Cod"), return_data = TRUE)
cod_abundance_year46_double <- plotSpectra(sim_params_temporal_harvest_double, time_range = 46, species = c("Cod"), return_data = TRUE)
cod_abundance_year47_double <- plotSpectra(sim_params_temporal_harvest_double, time_range = 47, species = c("Cod"), return_data = TRUE)
cod_abundance_year48_double <- plotSpectra(sim_params_temporal_harvest_double, time_range = 48, species = c("Cod"), return_data = TRUE)
cod_abundance_year49_double <- plotSpectra(sim_params_temporal_harvest_double, time_range = 49, species = c("Cod"), return_data = TRUE)
cod_abundance_year50_double <- plotSpectra(sim_params_temporal_harvest_double, time_range = 50, species = c("Cod"), return_data = TRUE)


# Assigning the "Year" variable
cod_abundance_year1_double$Year <- 1
cod_abundance_year2_double$Year <- 2
cod_abundance_year3_double$Year <- 3
cod_abundance_year4_double$Year <- 4
cod_abundance_year5_double$Year <- 5
cod_abundance_year6_double$Year <- 6
cod_abundance_year7_double$Year <- 7
cod_abundance_year8_double$Year <- 8
cod_abundance_year9_double$Year <- 9
cod_abundance_year10_double$Year <- 10
cod_abundance_year11_double$Year <- 11
cod_abundance_year12_double$Year <- 12
cod_abundance_year13_double$Year <- 13
cod_abundance_year14_double$Year <- 14
cod_abundance_year15_double$Year <- 15
cod_abundance_year16_double$Year <- 16
cod_abundance_year17_double$Year <- 17
cod_abundance_year18_double$Year <- 18
cod_abundance_year19_double$Year <- 19
cod_abundance_year20_double$Year <- 20
cod_abundance_year21_double$Year <- 21
cod_abundance_year22_double$Year <- 22
cod_abundance_year23_double$Year <- 23
cod_abundance_year24_double$Year <- 24
cod_abundance_year25_double$Year <- 25
cod_abundance_year26_double$Year <- 26
cod_abundance_year27_double$Year <- 27
cod_abundance_year28_double$Year <- 28
cod_abundance_year29_double$Year <- 29
cod_abundance_year30_double$Year <- 30
cod_abundance_year31_double$Year <- 31
cod_abundance_year32_double$Year <- 32
cod_abundance_year33_double$Year <- 33
cod_abundance_year34_double$Year <- 34
cod_abundance_year35_double$Year <- 35
cod_abundance_year36_double$Year <- 36
cod_abundance_year37_double$Year <- 37
cod_abundance_year38_double$Year <- 38
cod_abundance_year39_double$Year <- 39
cod_abundance_year40_double$Year <- 40
cod_abundance_year41_double$Year <- 41
cod_abundance_year42_double$Year <- 42
cod_abundance_year43_double$Year <- 43
cod_abundance_year44_double$Year <- 44
cod_abundance_year45_double$Year <- 45
cod_abundance_year46_double$Year <- 46
cod_abundance_year47_double$Year <- 47
cod_abundance_year48_double$Year <- 48
cod_abundance_year49_double$Year <- 49
cod_abundance_year50_double$Year <- 50


combined_cod_abundance_double <- rbind(
  cod_abundance_year1_double,
  cod_abundance_year2_double,
  cod_abundance_year3_double,
  cod_abundance_year4_double,
  cod_abundance_year5_double,
  cod_abundance_year6_double,
  cod_abundance_year7_double,
  cod_abundance_year8_double,
  cod_abundance_year9_double,
  cod_abundance_year10_double,
  cod_abundance_year11_double,
  cod_abundance_year12_double,
  cod_abundance_year13_double,
  cod_abundance_year14_double,
  cod_abundance_year15_double,
  cod_abundance_year16_double,
  cod_abundance_year17_double,
  cod_abundance_year18_double,
  cod_abundance_year19_double,
  cod_abundance_year20_double,
  cod_abundance_year21_double,
  cod_abundance_year22_double,
  cod_abundance_year23_double,
  cod_abundance_year24_double,
  cod_abundance_year25_double,
  cod_abundance_year26_double,
  cod_abundance_year27_double,
  cod_abundance_year28_double,
  cod_abundance_year29_double,
  cod_abundance_year30_double,
  cod_abundance_year31_double,
  cod_abundance_year32_double,
  cod_abundance_year33_double,
  cod_abundance_year34_double,
  cod_abundance_year35_double,
  cod_abundance_year36_double,
  cod_abundance_year37_double,
  cod_abundance_year38_double,
  cod_abundance_year39_double,
  cod_abundance_year40_double,
  cod_abundance_year41_double,
  cod_abundance_year42_double,
  cod_abundance_year43_double,
  cod_abundance_year44_double,
  cod_abundance_year45_double,
  cod_abundance_year46_double,
  cod_abundance_year47_double,
  cod_abundance_year48_double,
  cod_abundance_year49_double,
  cod_abundance_year50_double
)


combined_cod_abundance_double <- combined_cod_abundance_double[combined_cod_abundance_double$Species != "Resource", ]



write.csv(combined_cod_abundance_double, file = "cod_psd_doubleV2.csv", row.names = FALSE)


double_to_be_mean_data_cod <- read.csv("cod_diet_size_class_all_years_doubleV2.csv")
double_to_be_mean_data_cod

year_1_values_double <- double_to_be_mean_data_cod[double_to_be_mean_data_cod$Year == 1, ]
year_2_values_double <- double_to_be_mean_data_cod[double_to_be_mean_data_cod$Year == 2, ]
year_3_values_double <- double_to_be_mean_data_cod[double_to_be_mean_data_cod$Year == 3, ]
year_4_values_double <- double_to_be_mean_data_cod[double_to_be_mean_data_cod$Year == 4, ]
year_5_values_double <- double_to_be_mean_data_cod[double_to_be_mean_data_cod$Year == 5, ]
year_6_values_double <- double_to_be_mean_data_cod[double_to_be_mean_data_cod$Year == 6, ]
year_7_values_double <- double_to_be_mean_data_cod[double_to_be_mean_data_cod$Year == 7, ]
year_8_values_double <- double_to_be_mean_data_cod[double_to_be_mean_data_cod$Year == 8, ]
year_9_values_double <- double_to_be_mean_data_cod[double_to_be_mean_data_cod$Year == 9, ]
year_10_values_double <- double_to_be_mean_data_cod[double_to_be_mean_data_cod$Year == 10, ]
year_11_values_double <- double_to_be_mean_data_cod[double_to_be_mean_data_cod$Year == 11, ]
year_12_values_double <- double_to_be_mean_data_cod[double_to_be_mean_data_cod$Year == 12, ]
year_13_values_double <- double_to_be_mean_data_cod[double_to_be_mean_data_cod$Year == 13, ]
year_14_values_double <- double_to_be_mean_data_cod[double_to_be_mean_data_cod$Year == 14, ]
year_15_values_double <- double_to_be_mean_data_cod[double_to_be_mean_data_cod$Year == 15, ]
year_16_values_double <- double_to_be_mean_data_cod[double_to_be_mean_data_cod$Year == 16, ]
year_17_values_double <- double_to_be_mean_data_cod[double_to_be_mean_data_cod$Year == 17, ]
year_18_values_double <- double_to_be_mean_data_cod[double_to_be_mean_data_cod$Year == 18, ]
year_19_values_double <- double_to_be_mean_data_cod[double_to_be_mean_data_cod$Year == 19, ]
year_20_values_double <- double_to_be_mean_data_cod[double_to_be_mean_data_cod$Year == 20, ]
year_21_values_double <- double_to_be_mean_data_cod[double_to_be_mean_data_cod$Year == 21, ]
year_22_values_double <- double_to_be_mean_data_cod[double_to_be_mean_data_cod$Year == 22, ]
year_23_values_double <- double_to_be_mean_data_cod[double_to_be_mean_data_cod$Year == 23, ]
year_24_values_double <- double_to_be_mean_data_cod[double_to_be_mean_data_cod$Year == 24, ]
year_25_values_double <- double_to_be_mean_data_cod[double_to_be_mean_data_cod$Year == 25, ]
year_26_values_double <- double_to_be_mean_data_cod[double_to_be_mean_data_cod$Year == 26, ]
year_27_values_double <- double_to_be_mean_data_cod[double_to_be_mean_data_cod$Year == 27, ]
year_28_values_double <- double_to_be_mean_data_cod[double_to_be_mean_data_cod$Year == 28, ]
year_29_values_double <- double_to_be_mean_data_cod[double_to_be_mean_data_cod$Year == 29, ]
year_30_values_double <- double_to_be_mean_data_cod[double_to_be_mean_data_cod$Year == 30, ]
year_31_values_double <- double_to_be_mean_data_cod[double_to_be_mean_data_cod$Year == 31, ]
year_32_values_double <- double_to_be_mean_data_cod[double_to_be_mean_data_cod$Year == 32, ]
year_33_values_double <- double_to_be_mean_data_cod[double_to_be_mean_data_cod$Year == 33, ]
year_34_values_double <- double_to_be_mean_data_cod[double_to_be_mean_data_cod$Year == 34, ]
year_35_values_double <- double_to_be_mean_data_cod[double_to_be_mean_data_cod$Year == 35, ]
year_36_values_double <- double_to_be_mean_data_cod[double_to_be_mean_data_cod$Year == 36, ]
year_37_values_double <- double_to_be_mean_data_cod[double_to_be_mean_data_cod$Year == 37, ]
year_38_values_double <- double_to_be_mean_data_cod[double_to_be_mean_data_cod$Year == 38, ]
year_39_values_double <- double_to_be_mean_data_cod[double_to_be_mean_data_cod$Year == 39, ]
year_40_values_double <- double_to_be_mean_data_cod[double_to_be_mean_data_cod$Year == 40, ]
year_41_values_double <- double_to_be_mean_data_cod[double_to_be_mean_data_cod$Year == 41, ]
year_42_values_double <- double_to_be_mean_data_cod[double_to_be_mean_data_cod$Year == 42, ]
year_43_values_double <- double_to_be_mean_data_cod[double_to_be_mean_data_cod$Year == 43, ]
year_44_values_double <- double_to_be_mean_data_cod[double_to_be_mean_data_cod$Year == 44, ]
year_45_values_double <- double_to_be_mean_data_cod[double_to_be_mean_data_cod$Year == 45, ]
year_46_values_double <- double_to_be_mean_data_cod[double_to_be_mean_data_cod$Year == 46, ]
year_47_values_double <- double_to_be_mean_data_cod[double_to_be_mean_data_cod$Year == 47, ]
year_48_values_double <- double_to_be_mean_data_cod[double_to_be_mean_data_cod$Year == 48, ]
year_49_values_double <- double_to_be_mean_data_cod[double_to_be_mean_data_cod$Year == 49, ]
year_50_values_double <- double_to_be_mean_data_cod[double_to_be_mean_data_cod$Year == 50, ]


# Loop

# Create a list to store the values for each year
year_values <- list(year_1_values_double, year_2_values_double, year_3_values_double, 
                    year_4_values_double, year_5_values_double, year_6_values_double, 
                    year_7_values_double, year_8_values_double, year_9_values_double, 
                    year_10_values_double, year_11_values_double, year_12_values_double, 
                    year_13_values_double, year_14_values_double, year_15_values_double, 
                    year_16_values_double, year_17_values_double, year_18_values_double, 
                    year_19_values_double, year_20_values_double,
                    year_21_values_double, year_22_values_double, year_23_values_double,
                    year_24_values_double, year_25_values_double, year_26_values_double,
                    year_27_values_double, year_28_values_double, year_29_values_double,
                    year_30_values_double, year_31_values_double, year_32_values_double,
                    year_33_values_double, year_34_values_double, year_35_values_double,
                    year_36_values_double, year_37_values_double, year_38_values_double,
                    year_39_values_double, year_40_values_double, year_41_values_double,
                    year_42_values_double, year_43_values_double, year_44_values_double, 
                    year_45_values_double, year_46_values_double, year_47_values_double, 
                    year_48_values_double, year_49_values_double, year_50_values_double)

length(year_values)

# Create a vector of years
years <- 0:49
length(year_values)
# Loop through each year
for (i in seq_along(years)) {
  year <- years[i]
  year_data <- year_values[[i]]
  
  # Generate the variable name prefix based on the year
  year_prefix <- paste0("year_", year, "_values_double$")
  
  # Access the PSD values for the current year
  PSD_values <- year_data$PSD
  
  # Loop through each species and calculate the mean diet value
  for (s in species) {
    # Generate the variable name for the current species
    species_variable <- paste0("mean_diet_", s, "_double_", year)
    
    # Calculate the sum of the product
    species_sum <- sum(year_data[[s]] * PSD_values)
    
    # Calculate the mean diet value
    assign(species_variable, species_sum / sum(PSD_values))
  }
}

# View the mean diet values for each species and each year
for (s in species) {
  print(paste("Species:", s))
  for (y in years) {
    variable_name <- paste0("mean_diet_", s, "_double_", y)
    mean_diet_value <- get(variable_name)
    print(paste("Year:", y, "| Mean Diet Value:", mean_diet_value))
  }
  cat("\n")
}

# Create an empty data frame to store the results
results_df <- data.frame(Species = character(), Year = numeric(), Mean_Diet_Value = numeric())

# Iterate over species and years
for (s in species) {
  for (y in years) {
    variable_name <- paste0("mean_diet_", s, "_double_", y)
    mean_diet_value <- get(variable_name)
    
    # Add the data to the results data frame
    results_df <- rbind(results_df, data.frame(Species = s, Year = y, Mean_Diet_Value = mean_diet_value))
  }
}

# Write the results to a CSV file
write.csv(results_df, "mean_diet_results_double_year1to50.csv", row.names = FALSE)

mean_diet_results_double_yr1to50 <- read.csv("mean_diet_results_double_year1to50.csv")

library(ggplot2)

# Filter the data for the selected species
selected_species <- c("Sandeel", "Herring", "Haddock", "Plaice")
filtered_data <- mean_diet_results_double_yr1to50[mean_diet_results_double_yr1to50$Species %in% selected_species, ]

ggplot(filtered_data, aes(x = Year, y = Mean_Diet_Value, color = Species)) +
  geom_line(linewidth = 1) +
  geom_rect(aes(xmin = 15, xmax = 19, ymin = -Inf, ymax = Inf), fill = NA, color = "black", alpha = 0.3) +
  geom_rect(aes(xmin = 25, xmax = 29, ymin = -Inf, ymax = Inf), fill = NA, color = "black", alpha = 0.3) +
  geom_rect(aes(xmin = 35, xmax = 39, ymin = -Inf, ymax = Inf), fill = NA, color = "black", alpha = 0.3) +
  geom_rect(aes(xmin = 45, xmax = 49, ymin = -Inf, ymax = Inf), fill = NA, color = "black", alpha = 0.3) +
  geom_rect(aes(xmin = 5, xmax = 9, ymin = -Inf, ymax = Inf), fill = NA, color = "black", alpha = 0.3) +
  geom_label(aes(x = 17, y = Inf, label = "PH Double Intensity"), vjust = 1, hjust = 0.5, color = "black", size = 3) +
  geom_label(aes(x = 27, y = Inf, label = "PH Double Intensity"), vjust = 1, hjust = 0.5, color = "black", size = 3) +
  geom_label(aes(x = 37, y = Inf, label = "PH Double Intensity"), vjust = 1, hjust = 0.5, color = "black", size = 3) +
  geom_label(aes(x = 47, y = Inf, label = "PH Double Intensity"), vjust = 1, hjust = 0.5, color = "black", size = 3) +
  geom_label(aes(x = 7, y = Inf, label = "PH Double Intensity"), vjust = 1, hjust = 0.5, color = "black", size = 3) +
  labs(x = "Time step (Year)", y = "Mean Diet of Cod (g/year)", title = "G.) Periodic Harvest Double Fishing Intensity", color = "Prey Species") +  # Change legend title
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, 49, by = 1), labels = seq(0, 49, by = 1)) +  # Change x-axis labels
  scale_y_continuous(breaks = seq(0, ceiling(max(filtered_data$Mean_Diet_Value)), by = 50),
                     limits = c(0, max(filtered_data$Mean_Diet_Value) + 50),
                     labels = function(x) ifelse(x == max(filtered_data$Mean_Diet_Value), "50", as.character(x))) +  # Adjust y-axis breaks, limits, and label
  theme(panel.grid.major = element_line(color = "grey", linetype = "dotted"),  # More gridlines
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),  # Rotate x-axis labels
        axis.text.y = element_text(size = 8))  # Adjust y-axis label size









## No Harvest ##
## Diet cod ##
getDiet(params_constant_harvest)

# Initialize an empty list to store diet rates for each year
diet_rates_no_fishing <- list()
sim_params_no_harvest
# Loop over each year (time step)
for (year in 1:50) {
  # Extract biomass values for the current year
  n <- sim_params_no_harvest@n[year,,]
  n_pp <- sim_params_no_harvest@n_pp[year,]
  
  # Get diet information for the current year and store it in the list
  diet_rates_no_fishing[[year]] <- getDiet(params_constant_harvest, n, n_pp, proportion = FALSE)
}


predator_cod_no_harvest <- list()

# Loop over each year
for (year in 1:50) {
  # Extract predator Cod data for the current year
  predator_cod_no_harvest[[year]] <- diet_rates_no_fishing[[year]][11,,]
}
predator_cod_no_harvest


year_1_predator_cod_no_harvest <- predator_cod_no_harvest[[1]]
year_2_predator_cod_no_harvest <- predator_cod_no_harvest[[2]]
year_3_predator_cod_no_harvest <- predator_cod_no_harvest[[3]]
year_4_predator_cod_no_harvest <- predator_cod_no_harvest[[4]]
year_5_predator_cod_no_harvest <- predator_cod_no_harvest[[5]]
year_6_predator_cod_no_harvest <- predator_cod_no_harvest[[6]]
year_7_predator_cod_no_harvest <- predator_cod_no_harvest[[7]]
year_8_predator_cod_no_harvest <- predator_cod_no_harvest[[8]]
year_9_predator_cod_no_harvest <- predator_cod_no_harvest[[9]]
year_10_predator_cod_no_harvest <- predator_cod_no_harvest[[10]]
year_11_predator_cod_no_harvest <- predator_cod_no_harvest[[11]]
year_12_predator_cod_no_harvest <- predator_cod_no_harvest[[12]]
year_13_predator_cod_no_harvest <- predator_cod_no_harvest[[13]]
year_14_predator_cod_no_harvest <- predator_cod_no_harvest[[14]]
year_15_predator_cod_no_harvest <- predator_cod_no_harvest[[15]]
year_16_predator_cod_no_harvest <- predator_cod_no_harvest[[16]]
year_17_predator_cod_no_harvest <- predator_cod_no_harvest[[17]]
year_18_predator_cod_no_harvest <- predator_cod_no_harvest[[18]]
year_19_predator_cod_no_harvest <- predator_cod_no_harvest[[19]]
year_20_predator_cod_no_harvest <- predator_cod_no_harvest[[20]]
year_21_predator_cod_no_harvest <- predator_cod_no_harvest[[21]]
year_22_predator_cod_no_harvest <- predator_cod_no_harvest[[22]]
year_23_predator_cod_no_harvest <- predator_cod_no_harvest[[23]]
year_24_predator_cod_no_harvest <- predator_cod_no_harvest[[24]]
year_25_predator_cod_no_harvest <- predator_cod_no_harvest[[25]]
year_26_predator_cod_no_harvest <- predator_cod_no_harvest[[26]]
year_27_predator_cod_no_harvest <- predator_cod_no_harvest[[27]]
year_28_predator_cod_no_harvest <- predator_cod_no_harvest[[28]]
year_29_predator_cod_no_harvest <- predator_cod_no_harvest[[29]]
year_30_predator_cod_no_harvest <- predator_cod_no_harvest[[30]]
year_31_predator_cod_no_harvest <- predator_cod_no_harvest[[31]]
year_32_predator_cod_no_harvest <- predator_cod_no_harvest[[32]]
year_33_predator_cod_no_harvest <- predator_cod_no_harvest[[33]]
year_34_predator_cod_no_harvest <- predator_cod_no_harvest[[34]]
year_35_predator_cod_no_harvest <- predator_cod_no_harvest[[35]]
year_36_predator_cod_no_harvest <- predator_cod_no_harvest[[36]]
year_37_predator_cod_no_harvest <- predator_cod_no_harvest[[37]]
year_38_predator_cod_no_harvest <- predator_cod_no_harvest[[38]]
year_39_predator_cod_no_harvest <- predator_cod_no_harvest[[39]]
year_40_predator_cod_no_harvest <- predator_cod_no_harvest[[40]]
year_41_predator_cod_no_harvest <- predator_cod_no_harvest[[41]]
year_42_predator_cod_no_harvest <- predator_cod_no_harvest[[42]]
year_43_predator_cod_no_harvest <- predator_cod_no_harvest[[43]]
year_44_predator_cod_no_harvest <- predator_cod_no_harvest[[44]]
year_45_predator_cod_no_harvest <- predator_cod_no_harvest[[45]]
year_46_predator_cod_no_harvest <- predator_cod_no_harvest[[46]]
year_47_predator_cod_no_harvest <- predator_cod_no_harvest[[47]]
year_48_predator_cod_no_harvest <- predator_cod_no_harvest[[48]]
year_49_predator_cod_no_harvest <- predator_cod_no_harvest[[49]]
year_50_predator_cod_no_harvest <- predator_cod_no_harvest[[50]]



# Write data for year 1 to CSV file
write.csv(year_1_predator_cod_no_harvest, file = "year_1_predator_cod_no_harvest_v9.csv", row.names = FALSE)

# Write data for year 2 to CSV file
write.csv(year_2_predator_cod_no_harvest, file = "year_2_predator_cod_no_harvest_v9.csv", row.names = FALSE)

# Write data for year 3 to CSV file
write.csv(year_3_predator_cod_no_harvest, file = "year_3_predator_cod_no_harvest_v9.csv", row.names = FALSE)

# Write data for year 4 to CSV file
write.csv(year_4_predator_cod_no_harvest, file = "year_4_predator_cod_no_harvest_v9.csv", row.names = FALSE)

# Write data for year 5 to CSV file
write.csv(year_5_predator_cod_no_harvest, file = "year_5_predator_cod_no_harvest_v9.csv", row.names = FALSE)

# Write data for year 6 to CSV file
write.csv(year_6_predator_cod_no_harvest, file = "year_6_predator_cod_no_harvest_v9.csv", row.names = FALSE)

# Write data for year 7 to CSV file
write.csv(year_7_predator_cod_no_harvest, file = "year_7_predator_cod_no_harvest_v9.csv", row.names = FALSE)

# Write data for year 8 to CSV file
write.csv(year_8_predator_cod_no_harvest, file = "year_8_predator_cod_no_harvest_v9.csv", row.names = FALSE)

# Write data for year 9 to CSV file
write.csv(year_9_predator_cod_no_harvest, file = "year_9_predator_cod_no_harvest_v9.csv", row.names = FALSE)

# Write data for year 10 to CSV file
write.csv(year_10_predator_cod_no_harvest, file = "year_10_predator_cod_no_harvest_v9.csv", row.names = FALSE)

# Write data for year 11 to CSV file
write.csv(year_11_predator_cod_no_harvest, file = "year_11_predator_cod_no_harvest_v9.csv", row.names = FALSE)

# Write data for year 12 to CSV file
write.csv(year_12_predator_cod_no_harvest, file = "year_12_predator_cod_no_harvest_v9.csv", row.names = FALSE)

# Write data for year 13 to CSV file
write.csv(year_13_predator_cod_no_harvest, file = "year_13_predator_cod_no_harvest_v9.csv", row.names = FALSE)

# Write data for year 14 to CSV file
write.csv(year_14_predator_cod_no_harvest, file = "year_14_predator_cod_no_harvest_v9.csv", row.names = FALSE)

# Write data for year 15 to CSV file
write.csv(year_15_predator_cod_no_harvest, file = "year_15_predator_cod_no_harvest_v9.csv", row.names = FALSE)

# Write data for year 16 to CSV file
write.csv(year_16_predator_cod_no_harvest, file = "year_16_predator_cod_no_harvest_v9.csv", row.names = FALSE)

# Write data for year 17 to CSV file
write.csv(year_17_predator_cod_no_harvest, file = "year_17_predator_cod_no_harvest_v9.csv", row.names = FALSE)

# Write data for year 18 to CSV file
write.csv(year_18_predator_cod_no_harvest, file = "year_18_predator_cod_no_harvest_v9.csv", row.names = FALSE)

# Write data for year 19 to CSV file
write.csv(year_19_predator_cod_no_harvest, file = "year_19_predator_cod_no_harvest_v9.csv", row.names = FALSE)

# Write data for year 20 to CSV file
write.csv(year_20_predator_cod_no_harvest, file = "year_20_predator_cod_no_harvest_v9.csv", row.names = FALSE)

# Write data for year 21 to CSV file
write.csv(year_21_predator_cod_no_harvest, file = "year_21_predator_cod_no_harvest_v9.csv", row.names = FALSE)

# Write data for year 22 to CSV file
write.csv(year_22_predator_cod_no_harvest, file = "year_22_predator_cod_no_harvest_v9.csv", row.names = FALSE)

# Write data for year 23 to CSV file
write.csv(year_23_predator_cod_no_harvest, file = "year_23_predator_cod_no_harvest_v9.csv", row.names = FALSE)

# Write data for year 24 to CSV file
write.csv(year_24_predator_cod_no_harvest, file = "year_24_predator_cod_no_harvest_v9.csv", row.names = FALSE)

# Write data for year 25 to CSV file
write.csv(year_25_predator_cod_no_harvest, file = "year_25_predator_cod_no_harvest_v9.csv", row.names = FALSE)

# Write data for year 26 to CSV file
write.csv(year_26_predator_cod_no_harvest, file = "year_26_predator_cod_no_harvest_v9.csv", row.names = FALSE)

# Write data for year 27 to CSV file
write.csv(year_27_predator_cod_no_harvest, file = "year_27_predator_cod_no_harvest_v9.csv", row.names = FALSE)

# Write data for year 28 to CSV file
write.csv(year_28_predator_cod_no_harvest, file = "year_28_predator_cod_no_harvest_v9.csv", row.names = FALSE)

# Write data for year 29 to CSV file
write.csv(year_29_predator_cod_no_harvest, file = "year_29_predator_cod_no_harvest_v9.csv", row.names = FALSE)

# Write data for year 30 to CSV file
write.csv(year_30_predator_cod_no_harvest, file = "year_30_predator_cod_no_harvest_v9.csv", row.names = FALSE)

# Write data for year 31 to CSV file
write.csv(year_31_predator_cod_no_harvest, file = "year_31_predator_cod_no_harvest_v9.csv", row.names = FALSE)

# Write data for year 32 to CSV file
write.csv(year_32_predator_cod_no_harvest, file = "year_32_predator_cod_no_harvest_v9.csv", row.names = FALSE)

# Write data for year 33 to CSV file
write.csv(year_33_predator_cod_no_harvest, file = "year_33_predator_cod_no_harvest_v9.csv", row.names = FALSE)

# Write data for year 34 to CSV file
write.csv(year_34_predator_cod_no_harvest, file = "year_34_predator_cod_no_harvest_v9.csv", row.names = FALSE)

# Write data for year 35 to CSV file
write.csv(year_35_predator_cod_no_harvest, file = "year_35_predator_cod_no_harvest_v9.csv", row.names = FALSE)

# Write data for year 36 to CSV file
write.csv(year_36_predator_cod_no_harvest, file = "year_36_predator_cod_no_harvest_v9.csv", row.names = FALSE)

# Write data for year 37 to CSV file
write.csv(year_37_predator_cod_no_harvest, file = "year_37_predator_cod_no_harvest_v9.csv", row.names = FALSE)

# Write data for year 38 to CSV file
write.csv(year_38_predator_cod_no_harvest, file = "year_38_predator_cod_no_harvest_v9.csv", row.names = FALSE)

# Write data for year 39 to CSV file
write.csv(year_39_predator_cod_no_harvest, file = "year_39_predator_cod_no_harvest_v9.csv", row.names = FALSE)

# Write data for year 40 to CSV file
write.csv(year_40_predator_cod_no_harvest, file = "year_40_predator_cod_no_harvest_v9.csv", row.names = FALSE)

# Write data for year 41 to CSV file
write.csv(year_41_predator_cod_no_harvest, file = "year_41_predator_cod_no_harvest_v9.csv", row.names = FALSE)

# Write data for year 42 to CSV file
write.csv(year_42_predator_cod_no_harvest, file = "year_42_predator_cod_no_harvest_v9.csv", row.names = FALSE)

# Write data for year 43 to CSV file
write.csv(year_43_predator_cod_no_harvest, file = "year_43_predator_cod_no_harvest_v9.csv", row.names = FALSE)

# Write data for year 44 to CSV file
write.csv(year_44_predator_cod_no_harvest, file = "year_44_predator_cod_no_harvest_v9.csv", row.names = FALSE)

# Write data for year 45 to CSV file
write.csv(year_45_predator_cod_no_harvest, file = "year_45_predator_cod_no_harvest_v9.csv", row.names = FALSE)

# Write data for year 46 to CSV file
write.csv(year_46_predator_cod_no_harvest, file = "year_46_predator_cod_no_harvest_v9.csv", row.names = FALSE)

# Write data for year 47 to CSV file
write.csv(year_47_predator_cod_no_harvest, file = "year_47_predator_cod_no_harvest_v9.csv", row.names = FALSE)

# Write data for year 48 to CSV file
write.csv(year_48_predator_cod_no_harvest, file = "year_48_predator_cod_no_harvest_v9.csv", row.names = FALSE)

# Write data for year 49 to CSV file
write.csv(year_49_predator_cod_no_harvest, file = "year_49_predator_cod_no_harvest_v9.csv", row.names = FALSE)

# Write data for year 50 to CSV file
write.csv(year_50_predator_cod_no_harvest, file = "year_50_predator_cod_no_harvest_v9.csv", row.names = FALSE)

# Read data for year 1 from CSV file
year_1_cod_diet_size_class_no_harvest <- read.csv("year_1_predator_cod_no_harvest_v9.csv")

# Read data for year 2 from CSV file
year_2_cod_diet_size_class_no_harvest <- read.csv("year_2_predator_cod_no_harvest_v9.csv")

# Read data for year 3 from CSV file
year_3_cod_diet_size_class_no_harvest <- read.csv("year_3_predator_cod_no_harvest_v9.csv")

# Read data for year 4 from CSV file
year_4_cod_diet_size_class_no_harvest <- read.csv("year_4_predator_cod_no_harvest_v9.csv")

# Read data for year 5 from CSV file
year_5_cod_diet_size_class_no_harvest <- read.csv("year_5_predator_cod_no_harvest_v9.csv")

# Read data for year 6 from CSV file
year_6_cod_diet_size_class_no_harvest <- read.csv("year_6_predator_cod_no_harvest_v9.csv")

# Read data for year 7 from CSV file
year_7_cod_diet_size_class_no_harvest <- read.csv("year_7_predator_cod_no_harvest_v9.csv")

# Read data for year 8 from CSV file
year_8_cod_diet_size_class_no_harvest <- read.csv("year_8_predator_cod_no_harvest_v9.csv")

# Read data for year 9 from CSV file
year_9_cod_diet_size_class_no_harvest <- read.csv("year_9_predator_cod_no_harvest_v9.csv")

# Read data for year 10 from CSV file
year_10_cod_diet_size_class_no_harvest <- read.csv("year_10_predator_cod_no_harvest_v9.csv")

# Read data for year 11 from CSV file
year_11_cod_diet_size_class_no_harvest <- read.csv("year_11_predator_cod_no_harvest_v9.csv")

# Read data for year 12 from CSV file
year_12_cod_diet_size_class_no_harvest <- read.csv("year_12_predator_cod_no_harvest_v9.csv")

# Read data for year 13 from CSV file
year_13_cod_diet_size_class_no_harvest <- read.csv("year_13_predator_cod_no_harvest_v9.csv")


# Read data for year 14 from CSV file
year_14_cod_diet_size_class_no_harvest <- read.csv("year_14_predator_cod_no_harvest_v9.csv")

# Read data for year 15 from CSV file
year_15_cod_diet_size_class_no_harvest <- read.csv("year_15_predator_cod_no_harvest_v9.csv")

# Read data for year 16 from CSV file
year_16_cod_diet_size_class_no_harvest <- read.csv("year_16_predator_cod_no_harvest_v9.csv")

# Read data for year 17 from CSV file
year_17_cod_diet_size_class_no_harvest <- read.csv("year_17_predator_cod_no_harvest_v9.csv")

# Read data for year 18 from CSV file
year_18_cod_diet_size_class_no_harvest <- read.csv("year_18_predator_cod_no_harvest_v9.csv")

# Read data for year 19 from CSV file
year_19_cod_diet_size_class_no_harvest <- read.csv("year_19_predator_cod_no_harvest_v9.csv")

# Read data for year 20 from CSV file
year_20_cod_diet_size_class_no_harvest <- read.csv("year_20_predator_cod_no_harvest_v9.csv")

# Read data for year 21 from CSV file
year_21_cod_diet_size_class_no_harvest <- read.csv("year_21_predator_cod_no_harvest_v9.csv")

# Read data for year 22 from CSV file
year_22_cod_diet_size_class_no_harvest <- read.csv("year_22_predator_cod_no_harvest_v9.csv")

# Read data for year 23 from CSV file
year_23_cod_diet_size_class_no_harvest <- read.csv("year_23_predator_cod_no_harvest_v9.csv")

# Read data for year 24 from CSV file
year_24_cod_diet_size_class_no_harvest <- read.csv("year_24_predator_cod_no_harvest_v9.csv")

# Read data for year 25 from CSV file
year_25_cod_diet_size_class_no_harvest <- read.csv("year_25_predator_cod_no_harvest_v9.csv")

# Read data for year 26 from CSV file
year_26_cod_diet_size_class_no_harvest <- read.csv("year_26_predator_cod_no_harvest_v9.csv")

# Read data for year 27 from CSV file
year_27_cod_diet_size_class_no_harvest <- read.csv("year_27_predator_cod_no_harvest_v9.csv")

# Read data for year 28 from CSV file
year_28_cod_diet_size_class_no_harvest <- read.csv("year_28_predator_cod_no_harvest_v9.csv")

# Read data for year 29 from CSV file
year_29_cod_diet_size_class_no_harvest <- read.csv("year_29_predator_cod_no_harvest_v9.csv")

# Read data for year 30 from CSV file
year_30_cod_diet_size_class_no_harvest <- read.csv("year_30_predator_cod_no_harvest_v9.csv")

# Read data for year 31 from CSV file
year_31_cod_diet_size_class_no_harvest <- read.csv("year_31_predator_cod_no_harvest_v9.csv")

# Read data for year 32 from CSV file
year_32_cod_diet_size_class_no_harvest <- read.csv("year_32_predator_cod_no_harvest_v9.csv")

# Read data for year 33 from CSV file
year_33_cod_diet_size_class_no_harvest <- read.csv("year_33_predator_cod_no_harvest_v9.csv")

# Read data for year 34 from CSV file
year_34_cod_diet_size_class_no_harvest <- read.csv("year_34_predator_cod_no_harvest_v9.csv")

# Read data for year 35 from CSV file
year_35_cod_diet_size_class_no_harvest <- read.csv("year_35_predator_cod_no_harvest_v9.csv")

# Read data for year 36 from CSV file
year_36_cod_diet_size_class_no_harvest <- read.csv("year_36_predator_cod_no_harvest_v9.csv")

# Read data for year 37 from CSV file
year_37_cod_diet_size_class_no_harvest <- read.csv("year_37_predator_cod_no_harvest_v9.csv")

# Read data for year 38 from CSV file
year_38_cod_diet_size_class_no_harvest <- read.csv("year_38_predator_cod_no_harvest_v9.csv")

# Read data for year 39 from CSV file
year_39_cod_diet_size_class_no_harvest <- read.csv("year_39_predator_cod_no_harvest_v9.csv")

# Read data for year 40 from CSV file
year_40_cod_diet_size_class_no_harvest <- read.csv("year_40_predator_cod_no_harvest_v9.csv")

# Read data for year 41 from CSV file
year_41_cod_diet_size_class_no_harvest <- read.csv("year_41_predator_cod_no_harvest_v9.csv")

# Read data for year 42 from CSV file
year_42_cod_diet_size_class_no_harvest <- read.csv("year_42_predator_cod_no_harvest_v9.csv")

# Read data for year 43 from CSV file
year_43_cod_diet_size_class_no_harvest <- read.csv("year_43_predator_cod_no_harvest_v9.csv")

# Read data for year 44 from CSV file
year_44_cod_diet_size_class_no_harvest <- read.csv("year_44_predator_cod_no_harvest_v9.csv")

# Read data for year 45 from CSV file
year_45_cod_diet_size_class_no_harvest <- read.csv("year_45_predator_cod_no_harvest_v9.csv")

# Read data for year 46 from CSV file
year_46_cod_diet_size_class_no_harvest <- read.csv("year_46_predator_cod_no_harvest_v9.csv")

# Read data for year 47 from CSV file
year_47_cod_diet_size_class_no_harvest <- read.csv("year_47_predator_cod_no_harvest_v9.csv")

# Read data for year 48 from CSV file
year_48_cod_diet_size_class_no_harvest <- read.csv("year_48_predator_cod_no_harvest_v9.csv")

# Read data for year 49 from CSV file
year_49_cod_diet_size_class_no_harvest <- read.csv("year_49_predator_cod_no_harvest_v9.csv")

# Read data for year 50 from CSV file
year_50_cod_diet_size_class_no_harvest <- read.csv("year_50_predator_cod_no_harvest_v9.csv")




# Apply year values to each dataframe
year_1_cod_diet_size_class_no_harvest$Year <- 1
year_2_cod_diet_size_class_no_harvest$Year <- 2
year_3_cod_diet_size_class_no_harvest$Year <- 3
year_4_cod_diet_size_class_no_harvest$Year <- 4
year_5_cod_diet_size_class_no_harvest$Year <- 5
year_6_cod_diet_size_class_no_harvest$Year <- 6
year_7_cod_diet_size_class_no_harvest$Year <- 7
year_8_cod_diet_size_class_no_harvest$Year <- 8
year_9_cod_diet_size_class_no_harvest$Year <- 9
year_10_cod_diet_size_class_no_harvest$Year <- 10
year_11_cod_diet_size_class_no_harvest$Year <- 11
year_12_cod_diet_size_class_no_harvest$Year <- 12
year_13_cod_diet_size_class_no_harvest$Year <- 13
year_14_cod_diet_size_class_no_harvest$Year <- 14
year_15_cod_diet_size_class_no_harvest$Year <- 15
year_16_cod_diet_size_class_no_harvest$Year <- 16
year_17_cod_diet_size_class_no_harvest$Year <- 17
year_18_cod_diet_size_class_no_harvest$Year <- 18
year_19_cod_diet_size_class_no_harvest$Year <- 19
year_20_cod_diet_size_class_no_harvest$Year <- 20
year_21_cod_diet_size_class_no_harvest$Year <- 21
year_22_cod_diet_size_class_no_harvest$Year <- 22
year_23_cod_diet_size_class_no_harvest$Year <- 23
year_24_cod_diet_size_class_no_harvest$Year <- 24
year_25_cod_diet_size_class_no_harvest$Year <- 25
year_26_cod_diet_size_class_no_harvest$Year <- 26
year_27_cod_diet_size_class_no_harvest$Year <- 27
year_28_cod_diet_size_class_no_harvest$Year <- 28
year_29_cod_diet_size_class_no_harvest$Year <- 29
year_30_cod_diet_size_class_no_harvest$Year <- 30
year_31_cod_diet_size_class_no_harvest$Year <- 31
year_32_cod_diet_size_class_no_harvest$Year <- 32
year_33_cod_diet_size_class_no_harvest$Year <- 33
year_34_cod_diet_size_class_no_harvest$Year <- 34
year_35_cod_diet_size_class_no_harvest$Year <- 35
year_36_cod_diet_size_class_no_harvest$Year <- 36
year_37_cod_diet_size_class_no_harvest$Year <- 37
year_38_cod_diet_size_class_no_harvest$Year <- 38
year_39_cod_diet_size_class_no_harvest$Year <- 39
year_40_cod_diet_size_class_no_harvest$Year <- 40
year_41_cod_diet_size_class_no_harvest$Year <- 41
year_42_cod_diet_size_class_no_harvest$Year <- 42
year_43_cod_diet_size_class_no_harvest$Year <- 43
year_44_cod_diet_size_class_no_harvest$Year <- 44
year_45_cod_diet_size_class_no_harvest$Year <- 45
year_46_cod_diet_size_class_no_harvest$Year <- 46
year_47_cod_diet_size_class_no_harvest$Year <- 47
year_48_cod_diet_size_class_no_harvest$Year <- 48
year_49_cod_diet_size_class_no_harvest$Year <- 49
year_50_cod_diet_size_class_no_harvest$Year <- 50




combined_data_cod_diet_size_class_no_harvest <- rbind(
  year_1_cod_diet_size_class_no_harvest,
  year_2_cod_diet_size_class_no_harvest,
  year_3_cod_diet_size_class_no_harvest,
  year_4_cod_diet_size_class_no_harvest,
  year_5_cod_diet_size_class_no_harvest,
  year_6_cod_diet_size_class_no_harvest,
  year_7_cod_diet_size_class_no_harvest,
  year_8_cod_diet_size_class_no_harvest,
  year_9_cod_diet_size_class_no_harvest,
  year_10_cod_diet_size_class_no_harvest,
  year_11_cod_diet_size_class_no_harvest,
  year_12_cod_diet_size_class_no_harvest,
  year_13_cod_diet_size_class_no_harvest,
  year_14_cod_diet_size_class_no_harvest,
  year_15_cod_diet_size_class_no_harvest,
  year_16_cod_diet_size_class_no_harvest,
  year_17_cod_diet_size_class_no_harvest,
  year_18_cod_diet_size_class_no_harvest,
  year_19_cod_diet_size_class_no_harvest,
  year_20_cod_diet_size_class_no_harvest,
  year_21_cod_diet_size_class_no_harvest,
  year_22_cod_diet_size_class_no_harvest,
  year_23_cod_diet_size_class_no_harvest,
  year_24_cod_diet_size_class_no_harvest,
  year_25_cod_diet_size_class_no_harvest,
  year_26_cod_diet_size_class_no_harvest,
  year_27_cod_diet_size_class_no_harvest,
  year_28_cod_diet_size_class_no_harvest,
  year_29_cod_diet_size_class_no_harvest,
  year_30_cod_diet_size_class_no_harvest,
  year_31_cod_diet_size_class_no_harvest,
  year_32_cod_diet_size_class_no_harvest,
  year_33_cod_diet_size_class_no_harvest,
  year_34_cod_diet_size_class_no_harvest,
  year_35_cod_diet_size_class_no_harvest,
  year_36_cod_diet_size_class_no_harvest,
  year_37_cod_diet_size_class_no_harvest,
  year_38_cod_diet_size_class_no_harvest,
  year_39_cod_diet_size_class_no_harvest,
  year_40_cod_diet_size_class_no_harvest,
  year_41_cod_diet_size_class_no_harvest,
  year_42_cod_diet_size_class_no_harvest,
  year_43_cod_diet_size_class_no_harvest,
  year_44_cod_diet_size_class_no_harvest,
  year_45_cod_diet_size_class_no_harvest,
  year_46_cod_diet_size_class_no_harvest,
  year_47_cod_diet_size_class_no_harvest,
  year_48_cod_diet_size_class_no_harvest,
  year_49_cod_diet_size_class_no_harvest,
  year_50_cod_diet_size_class_no_harvest)

write.csv(combined_data_cod_diet_size_class_no_harvest, file = "cod_diet_size_class_all_years_no_harvestV2.csv", row.names = FALSE)
# add cod size classes excel


cod_diet_size_class_all_years_no_harvest <- read.csv("cod_diet_size_class_all_years_no_harvestV2.csv")
cod_diet_size_class_all_years_no_harvest


cod_abundance_year1_no_harvest <- plotSpectra(sim_params_no_harvest, time_range = 1, species = c("Cod"), return_data = TRUE)
cod_abundance_year2_no_harvest <- plotSpectra(sim_params_no_harvest, time_range = 2, species = c("Cod"), return_data = TRUE)
cod_abundance_year3_no_harvest <- plotSpectra(sim_params_no_harvest, time_range = 3, species = c("Cod"), return_data = TRUE)
cod_abundance_year4_no_harvest <- plotSpectra(sim_params_no_harvest, time_range = 4, species = c("Cod"), return_data = TRUE)
cod_abundance_year5_no_harvest <- plotSpectra(sim_params_no_harvest, time_range = 5, species = c("Cod"), return_data = TRUE)
cod_abundance_year6_no_harvest <- plotSpectra(sim_params_no_harvest, time_range = 6, species = c("Cod"), return_data = TRUE)
cod_abundance_year7_no_harvest <- plotSpectra(sim_params_no_harvest, time_range = 7, species = c("Cod"), return_data = TRUE)
cod_abundance_year8_no_harvest <- plotSpectra(sim_params_no_harvest, time_range = 8, species = c("Cod"), return_data = TRUE)
cod_abundance_year9_no_harvest <- plotSpectra(sim_params_no_harvest, time_range = 9, species = c("Cod"), return_data = TRUE)
cod_abundance_year10_no_harvest <- plotSpectra(sim_params_no_harvest, time_range = 10, species = c("Cod"), return_data = TRUE)
cod_abundance_year11_no_harvest <- plotSpectra(sim_params_no_harvest, time_range = 11, species = c("Cod"), return_data = TRUE)
cod_abundance_year12_no_harvest <- plotSpectra(sim_params_no_harvest, time_range = 12, species = c("Cod"), return_data = TRUE)
cod_abundance_year13_no_harvest <- plotSpectra(sim_params_no_harvest, time_range = 13, species = c("Cod"), return_data = TRUE)
cod_abundance_year14_no_harvest <- plotSpectra(sim_params_no_harvest, time_range = 14, species = c("Cod"), return_data = TRUE)
cod_abundance_year15_no_harvest <- plotSpectra(sim_params_no_harvest, time_range = 15, species = c("Cod"), return_data = TRUE)
cod_abundance_year16_no_harvest <- plotSpectra(sim_params_no_harvest, time_range = 16, species = c("Cod"), return_data = TRUE)
cod_abundance_year17_no_harvest <- plotSpectra(sim_params_no_harvest, time_range = 17, species = c("Cod"), return_data = TRUE)
cod_abundance_year18_no_harvest <- plotSpectra(sim_params_no_harvest, time_range = 18, species = c("Cod"), return_data = TRUE)
cod_abundance_year19_no_harvest <- plotSpectra(sim_params_no_harvest, time_range = 19, species = c("Cod"), return_data = TRUE)
cod_abundance_year20_no_harvest <- plotSpectra(sim_params_no_harvest, time_range = 20, species = c("Cod"), return_data = TRUE)
cod_abundance_year21_no_harvest <- plotSpectra(sim_params_no_harvest, time_range = 21, species = c("Cod"), return_data = TRUE)
cod_abundance_year22_no_harvest <- plotSpectra(sim_params_no_harvest, time_range = 22, species = c("Cod"), return_data = TRUE)
cod_abundance_year23_no_harvest <- plotSpectra(sim_params_no_harvest, time_range = 23, species = c("Cod"), return_data = TRUE)
cod_abundance_year24_no_harvest <- plotSpectra(sim_params_no_harvest, time_range = 24, species = c("Cod"), return_data = TRUE)
cod_abundance_year25_no_harvest <- plotSpectra(sim_params_no_harvest, time_range = 25, species = c("Cod"), return_data = TRUE)
cod_abundance_year26_no_harvest <- plotSpectra(sim_params_no_harvest, time_range = 26, species = c("Cod"), return_data = TRUE)
cod_abundance_year27_no_harvest <- plotSpectra(sim_params_no_harvest, time_range = 27, species = c("Cod"), return_data = TRUE)
cod_abundance_year28_no_harvest <- plotSpectra(sim_params_no_harvest, time_range = 28, species = c("Cod"), return_data = TRUE)
cod_abundance_year29_no_harvest <- plotSpectra(sim_params_no_harvest, time_range = 29, species = c("Cod"), return_data = TRUE)
cod_abundance_year30_no_harvest <- plotSpectra(sim_params_no_harvest, time_range = 30, species = c("Cod"), return_data = TRUE)
cod_abundance_year31_no_harvest <- plotSpectra(sim_params_no_harvest, time_range = 31, species = c("Cod"), return_data = TRUE)
cod_abundance_year32_no_harvest <- plotSpectra(sim_params_no_harvest, time_range = 32, species = c("Cod"), return_data = TRUE)
cod_abundance_year33_no_harvest <- plotSpectra(sim_params_no_harvest, time_range = 33, species = c("Cod"), return_data = TRUE)
cod_abundance_year34_no_harvest <- plotSpectra(sim_params_no_harvest, time_range = 34, species = c("Cod"), return_data = TRUE)
cod_abundance_year35_no_harvest <- plotSpectra(sim_params_no_harvest, time_range = 35, species = c("Cod"), return_data = TRUE)
cod_abundance_year36_no_harvest <- plotSpectra(sim_params_no_harvest, time_range = 36, species = c("Cod"), return_data = TRUE)
cod_abundance_year37_no_harvest <- plotSpectra(sim_params_no_harvest, time_range = 37, species = c("Cod"), return_data = TRUE)
cod_abundance_year38_no_harvest <- plotSpectra(sim_params_no_harvest, time_range = 38, species = c("Cod"), return_data = TRUE)
cod_abundance_year39_no_harvest <- plotSpectra(sim_params_no_harvest, time_range = 39, species = c("Cod"), return_data = TRUE)
cod_abundance_year40_no_harvest <- plotSpectra(sim_params_no_harvest, time_range = 40, species = c("Cod"), return_data = TRUE)
cod_abundance_year41_no_harvest <- plotSpectra(sim_params_no_harvest, time_range = 41, species = c("Cod"), return_data = TRUE)
cod_abundance_year42_no_harvest <- plotSpectra(sim_params_no_harvest, time_range = 42, species = c("Cod"), return_data = TRUE)
cod_abundance_year43_no_harvest <- plotSpectra(sim_params_no_harvest, time_range = 43, species = c("Cod"), return_data = TRUE)
cod_abundance_year44_no_harvest <- plotSpectra(sim_params_no_harvest, time_range = 44, species = c("Cod"), return_data = TRUE)
cod_abundance_year45_no_harvest <- plotSpectra(sim_params_no_harvest, time_range = 45, species = c("Cod"), return_data = TRUE)
cod_abundance_year46_no_harvest <- plotSpectra(sim_params_no_harvest, time_range = 46, species = c("Cod"), return_data = TRUE)
cod_abundance_year47_no_harvest <- plotSpectra(sim_params_no_harvest, time_range = 47, species = c("Cod"), return_data = TRUE)
cod_abundance_year48_no_harvest <- plotSpectra(sim_params_no_harvest, time_range = 48, species = c("Cod"), return_data = TRUE)
cod_abundance_year49_no_harvest <- plotSpectra(sim_params_no_harvest, time_range = 49, species = c("Cod"), return_data = TRUE)
cod_abundance_year50_no_harvest <- plotSpectra(sim_params_no_harvest, time_range = 50, species = c("Cod"), return_data = TRUE)



cod_abundance_year1_no_harvest$Year <- 1
cod_abundance_year2_no_harvest$Year <- 2
cod_abundance_year3_no_harvest$Year <- 3
cod_abundance_year4_no_harvest$Year <- 4
cod_abundance_year5_no_harvest$Year <- 5
cod_abundance_year6_no_harvest$Year <- 6
cod_abundance_year7_no_harvest$Year <- 7
cod_abundance_year8_no_harvest$Year <- 8
cod_abundance_year9_no_harvest$Year <- 9
cod_abundance_year10_no_harvest$Year <- 10
cod_abundance_year11_no_harvest$Year <- 11
cod_abundance_year12_no_harvest$Year <- 12
cod_abundance_year13_no_harvest$Year <- 13
cod_abundance_year14_no_harvest$Year <- 14
cod_abundance_year15_no_harvest$Year <- 15
cod_abundance_year16_no_harvest$Year <- 16
cod_abundance_year17_no_harvest$Year <- 17
cod_abundance_year18_no_harvest$Year <- 18
cod_abundance_year19_no_harvest$Year <- 19
cod_abundance_year20_no_harvest$Year <- 20
cod_abundance_year21_no_harvest$Year <- 21
cod_abundance_year22_no_harvest$Year <- 22
cod_abundance_year23_no_harvest$Year <- 23
cod_abundance_year24_no_harvest$Year <- 24
cod_abundance_year25_no_harvest$Year <- 25
cod_abundance_year26_no_harvest$Year <- 26
cod_abundance_year27_no_harvest$Year <- 27
cod_abundance_year28_no_harvest$Year <- 28
cod_abundance_year29_no_harvest$Year <- 29
cod_abundance_year30_no_harvest$Year <- 30
cod_abundance_year31_no_harvest$Year <- 31
cod_abundance_year32_no_harvest$Year <- 32
cod_abundance_year33_no_harvest$Year <- 33
cod_abundance_year34_no_harvest$Year <- 34
cod_abundance_year35_no_harvest$Year <- 35
cod_abundance_year36_no_harvest$Year <- 36
cod_abundance_year37_no_harvest$Year <- 37
cod_abundance_year38_no_harvest$Year <- 38
cod_abundance_year39_no_harvest$Year <- 39
cod_abundance_year40_no_harvest$Year <- 40
cod_abundance_year41_no_harvest$Year <- 41
cod_abundance_year42_no_harvest$Year <- 42
cod_abundance_year43_no_harvest$Year <- 43
cod_abundance_year44_no_harvest$Year <- 44
cod_abundance_year45_no_harvest$Year <- 45
cod_abundance_year46_no_harvest$Year <- 46
cod_abundance_year47_no_harvest$Year <- 47
cod_abundance_year48_no_harvest$Year <- 48
cod_abundance_year49_no_harvest$Year <- 49
cod_abundance_year50_no_harvest$Year <- 50


combined_cod_abundance_no_harvest <- rbind(
  cod_abundance_year1_no_harvest,
  cod_abundance_year2_no_harvest,
  cod_abundance_year3_no_harvest,
  cod_abundance_year4_no_harvest,
  cod_abundance_year5_no_harvest,
  cod_abundance_year6_no_harvest,
  cod_abundance_year7_no_harvest,
  cod_abundance_year8_no_harvest,
  cod_abundance_year9_no_harvest,
  cod_abundance_year10_no_harvest,
  cod_abundance_year11_no_harvest,
  cod_abundance_year12_no_harvest,
  cod_abundance_year13_no_harvest,
  cod_abundance_year14_no_harvest,
  cod_abundance_year15_no_harvest,
  cod_abundance_year16_no_harvest,
  cod_abundance_year17_no_harvest,
  cod_abundance_year18_no_harvest,
  cod_abundance_year19_no_harvest,
  cod_abundance_year20_no_harvest,
  cod_abundance_year21_no_harvest,
  cod_abundance_year22_no_harvest,
  cod_abundance_year23_no_harvest,
  cod_abundance_year24_no_harvest,
  cod_abundance_year25_no_harvest,
  cod_abundance_year26_no_harvest,
  cod_abundance_year27_no_harvest,
  cod_abundance_year28_no_harvest,
  cod_abundance_year29_no_harvest,
  cod_abundance_year30_no_harvest,
  cod_abundance_year31_no_harvest,
  cod_abundance_year32_no_harvest,
  cod_abundance_year33_no_harvest,
  cod_abundance_year34_no_harvest,
  cod_abundance_year35_no_harvest,
  cod_abundance_year36_no_harvest,
  cod_abundance_year37_no_harvest,
  cod_abundance_year38_no_harvest,
  cod_abundance_year39_no_harvest,
  cod_abundance_year40_no_harvest,
  cod_abundance_year41_no_harvest,
  cod_abundance_year42_no_harvest,
  cod_abundance_year43_no_harvest,
  cod_abundance_year44_no_harvest,
  cod_abundance_year45_no_harvest,
  cod_abundance_year46_no_harvest,
  cod_abundance_year47_no_harvest,
  cod_abundance_year48_no_harvest,
  cod_abundance_year49_no_harvest,
  cod_abundance_year50_no_harvest)


combined_cod_abundance_no_harvest <- combined_cod_abundance_no_harvest[combined_cod_abundance_no_harvest$Species != "Resource", ]

write.csv(combined_cod_abundance_no_harvest, file = "cod_psd_no_harvestV2.csv", row.names = FALSE)

# add to excel
no_harvest_to_be_mean_data_cod <- read.csv("cod_diet_size_class_all_years_no_harvestV2.csv")


year_1_values_no_harvest <- no_harvest_to_be_mean_data_cod[no_harvest_to_be_mean_data_cod$Year == 1, ]
year_2_values_no_harvest <- no_harvest_to_be_mean_data_cod[no_harvest_to_be_mean_data_cod$Year == 2, ]
year_3_values_no_harvest <- no_harvest_to_be_mean_data_cod[no_harvest_to_be_mean_data_cod$Year == 3, ]
year_4_values_no_harvest <- no_harvest_to_be_mean_data_cod[no_harvest_to_be_mean_data_cod$Year == 4, ]
year_5_values_no_harvest <- no_harvest_to_be_mean_data_cod[no_harvest_to_be_mean_data_cod$Year == 5, ]
year_6_values_no_harvest <- no_harvest_to_be_mean_data_cod[no_harvest_to_be_mean_data_cod$Year == 6, ]
year_7_values_no_harvest <- no_harvest_to_be_mean_data_cod[no_harvest_to_be_mean_data_cod$Year == 7, ]
year_8_values_no_harvest <- no_harvest_to_be_mean_data_cod[no_harvest_to_be_mean_data_cod$Year == 8, ]
year_9_values_no_harvest <- no_harvest_to_be_mean_data_cod[no_harvest_to_be_mean_data_cod$Year == 9, ]
year_10_values_no_harvest <- no_harvest_to_be_mean_data_cod[no_harvest_to_be_mean_data_cod$Year == 10, ]
year_11_values_no_harvest <- no_harvest_to_be_mean_data_cod[no_harvest_to_be_mean_data_cod$Year == 11, ]
year_12_values_no_harvest <- no_harvest_to_be_mean_data_cod[no_harvest_to_be_mean_data_cod$Year == 12, ]
year_13_values_no_harvest <- no_harvest_to_be_mean_data_cod[no_harvest_to_be_mean_data_cod$Year == 13, ]
year_14_values_no_harvest <- no_harvest_to_be_mean_data_cod[no_harvest_to_be_mean_data_cod$Year == 14, ]
year_15_values_no_harvest <- no_harvest_to_be_mean_data_cod[no_harvest_to_be_mean_data_cod$Year == 15, ]
year_16_values_no_harvest <- no_harvest_to_be_mean_data_cod[no_harvest_to_be_mean_data_cod$Year == 16, ]
year_17_values_no_harvest <- no_harvest_to_be_mean_data_cod[no_harvest_to_be_mean_data_cod$Year == 17, ]
year_18_values_no_harvest <- no_harvest_to_be_mean_data_cod[no_harvest_to_be_mean_data_cod$Year == 18, ]
year_19_values_no_harvest <- no_harvest_to_be_mean_data_cod[no_harvest_to_be_mean_data_cod$Year == 19, ]
year_20_values_no_harvest <- no_harvest_to_be_mean_data_cod[no_harvest_to_be_mean_data_cod$Year == 20, ]
year_21_values_no_harvest <- no_harvest_to_be_mean_data_cod[no_harvest_to_be_mean_data_cod$Year == 21, ]
year_22_values_no_harvest <- no_harvest_to_be_mean_data_cod[no_harvest_to_be_mean_data_cod$Year == 22, ]
year_23_values_no_harvest <- no_harvest_to_be_mean_data_cod[no_harvest_to_be_mean_data_cod$Year == 23, ]
year_24_values_no_harvest <- no_harvest_to_be_mean_data_cod[no_harvest_to_be_mean_data_cod$Year == 24, ]
year_25_values_no_harvest <- no_harvest_to_be_mean_data_cod[no_harvest_to_be_mean_data_cod$Year == 25, ]
year_26_values_no_harvest <- no_harvest_to_be_mean_data_cod[no_harvest_to_be_mean_data_cod$Year == 26, ]
year_27_values_no_harvest <- no_harvest_to_be_mean_data_cod[no_harvest_to_be_mean_data_cod$Year == 27, ]
year_28_values_no_harvest <- no_harvest_to_be_mean_data_cod[no_harvest_to_be_mean_data_cod$Year == 28, ]
year_29_values_no_harvest <- no_harvest_to_be_mean_data_cod[no_harvest_to_be_mean_data_cod$Year == 29, ]
year_30_values_no_harvest <- no_harvest_to_be_mean_data_cod[no_harvest_to_be_mean_data_cod$Year == 30, ]
year_31_values_no_harvest <- no_harvest_to_be_mean_data_cod[no_harvest_to_be_mean_data_cod$Year == 31, ]
year_32_values_no_harvest <- no_harvest_to_be_mean_data_cod[no_harvest_to_be_mean_data_cod$Year == 32, ]
year_33_values_no_harvest <- no_harvest_to_be_mean_data_cod[no_harvest_to_be_mean_data_cod$Year == 33, ]
year_34_values_no_harvest <- no_harvest_to_be_mean_data_cod[no_harvest_to_be_mean_data_cod$Year == 34, ]
year_35_values_no_harvest <- no_harvest_to_be_mean_data_cod[no_harvest_to_be_mean_data_cod$Year == 35, ]
year_36_values_no_harvest <- no_harvest_to_be_mean_data_cod[no_harvest_to_be_mean_data_cod$Year == 36, ]
year_37_values_no_harvest <- no_harvest_to_be_mean_data_cod[no_harvest_to_be_mean_data_cod$Year == 37, ]
year_38_values_no_harvest <- no_harvest_to_be_mean_data_cod[no_harvest_to_be_mean_data_cod$Year == 38, ]
year_39_values_no_harvest <- no_harvest_to_be_mean_data_cod[no_harvest_to_be_mean_data_cod$Year == 39, ]
year_40_values_no_harvest <- no_harvest_to_be_mean_data_cod[no_harvest_to_be_mean_data_cod$Year == 40, ]
year_41_values_no_harvest <- no_harvest_to_be_mean_data_cod[no_harvest_to_be_mean_data_cod$Year == 41, ]
year_42_values_no_harvest <- no_harvest_to_be_mean_data_cod[no_harvest_to_be_mean_data_cod$Year == 42, ]
year_43_values_no_harvest <- no_harvest_to_be_mean_data_cod[no_harvest_to_be_mean_data_cod$Year == 43, ]
year_44_values_no_harvest <- no_harvest_to_be_mean_data_cod[no_harvest_to_be_mean_data_cod$Year == 44, ]
year_45_values_no_harvest <- no_harvest_to_be_mean_data_cod[no_harvest_to_be_mean_data_cod$Year == 37, ]
year_46_values_no_harvest <- no_harvest_to_be_mean_data_cod[no_harvest_to_be_mean_data_cod$Year == 38, ]
year_47_values_no_harvest <- no_harvest_to_be_mean_data_cod[no_harvest_to_be_mean_data_cod$Year == 39, ]
year_48_values_no_harvest <- no_harvest_to_be_mean_data_cod[no_harvest_to_be_mean_data_cod$Year == 40, ]
year_49_values_no_harvest <- no_harvest_to_be_mean_data_cod[no_harvest_to_be_mean_data_cod$Year == 41, ]
year_50_values_no_harvest <- no_harvest_to_be_mean_data_cod[no_harvest_to_be_mean_data_cod$Year == 42, ]

# Loop

# Create a list to store the values for each year
year_values <- list(year_1_values_no_harvest, year_2_values_no_harvest, year_3_values_no_harvest, 
                    year_4_values_no_harvest, year_5_values_no_harvest, year_6_values_no_harvest, 
                    year_7_values_no_harvest, year_8_values_no_harvest, year_9_values_no_harvest, 
                    year_10_values_no_harvest, year_11_values_no_harvest, year_12_values_no_harvest, 
                    year_13_values_no_harvest, year_14_values_no_harvest, year_15_values_no_harvest, 
                    year_16_values_no_harvest, year_17_values_no_harvest, year_18_values_no_harvest, 
                    year_19_values_no_harvest, year_20_values_no_harvest,
                    year_21_values_no_harvest, year_22_values_no_harvest, year_23_values_no_harvest,
                    year_24_values_no_harvest, year_25_values_no_harvest, year_26_values_no_harvest,
                    year_27_values_no_harvest, year_28_values_no_harvest, year_29_values_no_harvest,
                    year_30_values_no_harvest, year_31_values_no_harvest, year_32_values_no_harvest,
                    year_33_values_no_harvest, year_34_values_no_harvest, year_35_values_no_harvest,
                    year_36_values_no_harvest, year_37_values_no_harvest, year_38_values_no_harvest,
                    year_39_values_no_harvest, year_40_values_no_harvest, year_41_values_no_harvest,
                    year_42_values_no_harvest, year_43_values_no_harvest, year_44_values_no_harvest, 
                    year_45_values_no_harvest, year_46_values_no_harvest, year_47_values_no_harvest, 
                    year_48_values_no_harvest, year_49_values_no_harvest, year_50_values_no_harvest)
length(year_values)

# Create a vector of years
years <- 0:49
length(year_values)
# Loop through each year
for (i in seq_along(years)) {
  year <- years[i]
  year_data <- year_values[[i]]
  
  # Generate the variable name prefix based on the year
  year_prefix <- paste0("year_", year, "_values_no_harvest$")
  
  # Access the PSD values for the current year
  PSD_values <- year_data$PSD
  
  # Loop through each species and calculate the mean diet value
  for (s in species) {
    # Generate the variable name for the current species
    species_variable <- paste0("mean_diet_", s, "_no_harvest_", year)
    
    # Calculate the sum of the product
    species_sum <- sum(year_data[[s]] * PSD_values)
    
    # Calculate the mean diet value
    assign(species_variable, species_sum / sum(PSD_values))
  }
}

# View the mean diet values for each species and each year
for (s in species) {
  print(paste("Species:", s))
  for (y in years) {
    variable_name <- paste0("mean_diet_", s, "_no_harvest_", y)
    mean_diet_value <- get(variable_name)
    print(paste("Year:", y, "| Mean Diet Value:", mean_diet_value))
  }
  cat("\n")
}

# Create an empty data frame to store the results
results_df <- data.frame(Species = character(), Year = numeric(), Mean_Diet_Value = numeric())

# Iterate over species and years
for (s in species) {
  for (y in years) {
    variable_name <- paste0("mean_diet_", s, "_no_harvest_", y)
    mean_diet_value <- get(variable_name)
    
    # Add the data to the results data frame
    results_df <- rbind(results_df, data.frame(Species = s, Year = y, Mean_Diet_Value = mean_diet_value))
  }
}

# Write the results to a CSV file
write.csv(results_df, "mean_diet_results_no_harvest_year1to50.csv", row.names = FALSE)

mean_diet_results_no_harvest_yr1to50 <- read.csv("mean_diet_results_no_harvest_year1to50.csv")

library(ggplot2)

# Filter the data for the selected species
selected_species <- c("Sandeel", "Herring", "Haddock", "Plaice")
filtered_data <- mean_diet_results_no_harvest_yr1to50[mean_diet_results_no_harvest_yr1to50$Species %in% selected_species, ]


ggplot(filtered_data, aes(x = Year, y = Mean_Diet_Value, color = Species)) +
  geom_line(linewidth = 1) +
  labs(x = "Time step (Year)", y = "Mean Diet of Cod (g/year)", title = "A.) No Harvest", color = "Prey Species") +  # Change legend title
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, 49, by = 1), labels = seq(0, 49, by = 1)) +  # Change x-axis labels
  scale_y_continuous(breaks = seq(0, ceiling(max(filtered_data$Mean_Diet_Value)), by = 50),
                     limits = c(0, max(filtered_data$Mean_Diet_Value) + 50),
                     labels = function(x) ifelse(x == max(filtered_data$Mean_Diet_Value), "50", as.character(x))) +  # Adjust y-axis breaks, limits, and label
  theme(panel.grid.major = element_line(color = "grey", linetype = "dotted"),  # More gridlines
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),  # Rotate x-axis labels
        axis.text.y = element_text(size = 8))  # Adjust y-axis label size


## Spectra plots for biomass density ##

# Biomass density of different sizes of Cod across different harvest regimes. 
# One graph different colour lines are different times plot years 4 (no fishing), 9 (fishing), 14, 19, 24, 29, 34, 39, 44, 49



plotSpectra(sim_params_temporal_harvest, species = c("Cod"), time_range = )

# W = size class (g), value = Biomass density

str(combined_cod_abundance_temporal)


library(ggplot2)

# Filter the data for the specified years
years_to_plot <- c(24, 29, 33, 5)
filtered_data <- combined_cod_abundance_temporal[combined_cod_abundance_temporal$Year %in% years_to_plot, ]


# Plot the data with log-transformed x-axis, more frequent x-axis labels, and a title
ggplot(filtered_data, aes(x = w, y = value, color = factor(Year))) +
  geom_line(linewidth = 1) +
  labs(x = "Cod size (log10(g))", y = "Biomass density of Cod", color = "Year", title = "F.) Periodic Harvest") +
  scale_x_log10(breaks = c(0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000), labels = c("0.001", "0.01", "0.1", "1", "10", "100", "1000", "10000")) +  # Log transform the x-axis and set labels
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),
    panel.grid.minor = element_line(color = "gray", linetype = "dotted")
  )




# Filter the data for the specified years
years_to_plot <- c(24, 29, 33, 5)
filtered_data <- combined_cod_abundance_double[combined_cod_abundance_double$Year %in% years_to_plot, ]

ggplot(filtered_data, aes(x = w, y = value, color = factor(Year))) +
  geom_line(linewidth = 1) +
  labs(x = "Cod size (log10(g))", y = "Biomass density of Cod", color = "Year", title = "H.) Periodic Harvest Double Fishing Intensity") +
  scale_x_log10(breaks = c(0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000), labels = c("0.001", "0.01", "0.1", "1", "10", "100", "1000", "10000")) +  # Log transform the x-axis and set labels
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),
    panel.grid.minor = element_line(color = "gray", linetype = "dotted")
  )



years_to_plot <- c(50, 5, 7, 11)
filtered_data <- combined_cod_abundance[combined_cod_abundance$Year %in% years_to_plot, ]

ggplot(filtered_data, aes(x = w, y = value, color = factor(Year))) +
  geom_line(linewidth = 1) +
  labs(x = "Cod size (log10(g))", y = "Biomass density of Cod", color = "Year", title = "D.) Constant Harvest") +
  scale_x_log10(breaks = c(0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000), labels = c("0.001", "0.01", "0.1", "1", "10", "100", "1000", "10000")) +  # Log transform the x-axis and set labels
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),
    panel.grid.minor = element_line(color = "gray", linetype = "dotted")
  )




years_to_plot <- c(50, 5, 7, 11)
filtered_data <- combined_cod_abundance_no_harvest[combined_cod_abundance_no_harvest$Year %in% years_to_plot, ]

ggplot(filtered_data, aes(x = w, y = value, color = factor(Year))) +
  geom_line(linewidth = 1) +
  labs(x = "Cod size (log10(g))", y = "Biomass density of Cod", color = "Year", title = "B.) No Harvest") +
  scale_x_log10(breaks = c(0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000), labels = c("0.001", "0.01", "0.1", "1", "10", "100", "1000", "10000")) +  # Log transform the x-axis and set labels
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),
    panel.grid.minor = element_line(color = "gray", linetype = "dotted")
  )



