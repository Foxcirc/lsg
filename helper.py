import matplotlib.pyplot as plt

points = [
    [10, 50],
    [10, 190],
    [10, 195],
    [14, 200],
    [20, 200],
    [220, 200],
    [225, 200],
    [230, 195],
    [230, 190],
    [230, 50],
    [230, 44],
    [225, 40],
    [220, 40],
    [20, 40],
    [14, 40],
    [10, 44],
    [10, 50],
    [10, 50],
    [30, 180],
    [30, 180],
    [30, 60],
    [210, 60],
    [210, 180],
    [30, 180],
];

# Define the points
points = [
    [90, 100], [90, 105], [85, 110], [80, 110], [74, 110], [70, 105], [70, 100], [70, 94],
    [74, 90], [80, 90], [85, 90], [90, 94], [90, 100], [90, 100], [110, 100], [30, 180],
    [10, 50], [10, 190], [10, 195], [14, 200], [20, 200], [220, 200], [225, 200], [230, 195],
    [230, 190], [230, 50], [230, 44], [225, 40], [220, 40], [20, 40], [14, 40], [10, 44],
    [10, 50], [10, 50], [30, 180], [30, 180], [30, 60], [210, 60], [210, 180], [30, 180],
    [110, 100], [110, 83], [96, 70], [80, 70], [63, 70], [50, 83], [50, 100], [50, 116],
    [63, 130], [80, 130], [96, 130], [110, 116], [110, 100], [110, 100]
]

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
