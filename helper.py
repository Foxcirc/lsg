import matplotlib.pyplot as plt

# should be a valid, connected shape
points = [
                [1900, 1900],
                [500, 1900],
                [500, 1300],
                [500, 1300],
                [1900, 1300],
                [1900, 1900],
                [2100, 1300],
                [403, 1100],
                [403, 1100],
                [463, 800],
                [1936, 800],
                [1996, 1100],
                [403, 1100],
                [2100, 1300],
                [2200, 1300],
                [2200, 1100],
                [2100, 600],
                [300, 600],
                [200, 1100],
                [200, 1300],
                [300, 1300],
                [300, 2000],
                [300, 2055],
                [344, 2100],
                [400, 2100],
                [2000, 2100],
                [2055, 2100],
                [2100, 2055],
                [2100, 2000],
                [2100, 1300],
                [2100, 1300],
                [1900, 1900],
                [1400, 1400],
                [600, 1400],
                [600, 1400],
                [600, 1700],
                [1400, 1700],
                [1400, 1400],
                [1900, 1900],
                [2100, 500],
                [2100, 300],
                [300, 300],
                [300, 300],
                [300, 500],
                [2100, 500],
]

points = [[it[0] / 10, it[1] / 10] for it in points]
# points = [[-it[0], -it[1]] for it in points]

# Count occurrences of each point
point_counts = {}
for i, point in enumerate(points):
    if tuple(point) in point_counts:
        point_counts[tuple(point)].append(i)
    else:
        point_counts[tuple(point)] = [i]

# Convert to separate lists for plotting
x_vals, y_vals = zip(*points)

# Create plot
plt.figure(figsize=(8, 6))
plt.scatter(x_vals, y_vals, color='red', marker='o')

# Annotate points with their indices, offsetting if needed
for (x, y), indices in point_counts.items():
    if len(indices) == 1:
        plt.text(x + 2, y, str(indices[0]), fontsize=8, color='blue')
    else:
        # Spread out overlapping indices
        for offset, index in enumerate(indices):
            plt.text(x + 2 + offset * 8, y, f"{index}, ", fontsize=8, color='blue')

# Set limits and labels
plt.xlim(0, 250)
plt.ylim(0, 220)
plt.xlabel('X-axis')
plt.ylabel('Y-axis')
plt.title('2D Points with Offset Indices for Overlapping Points')
plt.grid(True)

# Show plot
plt.show()
