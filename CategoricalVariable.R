# Load your dataset
data <- read.csv(file.choose())


# Count unique values in each column
unique_counts <- sapply(data, function(x) length(unique(x)))

# Create a data frame with column names and unique value counts
unique_counts_df <- data.frame(
  ColumnName = names(unique_counts),
  UniqueCount = unique_counts
)

# Display the result
print(unique_counts_df)
}


# ColumnName UniqueCount
# Northing                     Northing          62
# Easting                       Easting          62
# Depth..ft.                 Depth..ft.        1853
# Formation                   Formation          15
# Pore.pressure           Pore.pressure        2197
# Fracture.pressure   Fracture.pressure        2172
# Mud.pressure..psi. Mud.pressure..psi.        2595
# Hole.size..in.         Hole.size..in.          10
# METERAGE                     METERAGE         249
# DRLTIME                       DRLTIME          50
# WOB                               WOB          67
# Pump.flow.rate         Pump.flow.rate         121
# Pump.pressure           Pump.pressure         316
# MFVIS                           MFVIS          60
# RETSOLID                     RETSOLID          60
# FAN600                         FAN600         249
# FAN300                         FAN300         146
# MIN10GEL                     MIN10GEL          31
# RPM                               RPM          62
# MUDLOSSU                     MUDLOSSU         233


#Categorical Variables are Formation and Hole Size as they have very less unique values (15 and 10 respectively)
