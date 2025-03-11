import pandas as pd
import matplotlib.pyplot as plt
import json

# Load data from the JSON file
with open('data.json') as f:
    data = json.load(f)

# Sample DataFrame creation (replace this with your actual data loading step)
df = pd.DataFrame({
    "Date": ["2024-01-01", "2024-01-02", "2024-01-03", "2024-01-01", "2024-01-02", "2024-01-03"],
    "Skier": ["Jonna Sundling", "Jonna Sundling", "Jonna Sundling", "Maja Dahlqvist", "Maja Dahlqvist", "Maja Dahlqvist"],
    "Elo": [1500, 1520, 1510, 1490, 1505, 1515]
})

# Extract form data
skiers = data['skiers']
x_axis = data['x_axis']
y_axis = data['y_axis']
elo_type = data['elo_type']

# Filter DataFrame for selected skiers
df_filtered = df[df['Skier'].isin(skiers)]

# Calculate rolling average
df_filtered['Rolling_Elo'] = df_filtered.groupby('Skier')['Elo'].transform(lambda x: x.rolling(window=5, min_periods=1).mean())

# Create the plot
fig, ax = plt.subplots(figsize=(10, 6))

for skier in skiers:
    skier_df = df_filtered[df_filtered['Skier'] == skier]
    ax.plot(skier_df['Date'], skier_df['Rolling_Elo'], label=skier, marker='o')

ax.set_title("Smoothed Elo Scores Over Time (5-Race Rolling Average)")
ax.set_xlabel(x_axis)
ax.set_ylabel(y_axis)
ax.legend(title="Skier")

# Save the plot
plt.savefig('static/img/graphs/line_graph.png')

# Close the plot
plt.close()