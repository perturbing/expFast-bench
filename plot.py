import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from scipy.interpolate import griddata

def plot_3d_from_file(file_name, plot_title):
    # Read the data from the file
    df = pd.read_csv(file_name)

    # Ensure 'Base Size' and 'Exponent Size' are treated as numerical values
    df['Base Size'] = pd.to_numeric(df['Base Size'])
    df['Exponent Size'] = pd.to_numeric(df['Exponent Size'])
    df['Mean'] = pd.to_numeric(df['Mean'])
    

    # Creating the mesh grid with specific steps and range
    x = np.arange(-64, 65, 16)  # X-axis from -64 to 64 with steps of 16
    y = np.arange(-64, 65, 16)  # Y-axis from -64 to 64 with steps of 16
    x, y = np.meshgrid(x, y)

    # Interpolate the data for Z values (Mean)
    z = griddata((df['Base Size'], df['Exponent Size']), df['Mean'], (x, y), method='cubic')

    # Create the 3D plot
    fig = plt.figure(figsize=(10, 7))
    ax = fig.add_subplot(111, projection='3d')
    ax.set_title(plot_title)

    # Plot the surface
    surf = ax.plot_surface(x, y, z * 1e6, cmap='viridis', edgecolor='none') # scale to microseconds

    # Label the axes
    ax.set_xlabel('Base Size (bytes)')
    ax.set_ylabel('Exponent Size (bytes)')
    ax.set_zlabel('Time (microseconds)')

    # Set the ticks for x and y axes
    desired_ticks = np.arange(-64, 65, 16)  # Create an array of desired tick values
    ax.set_xticks(desired_ticks)  # Set x-axis ticks
    ax.set_yticks(desired_ticks)  # Set y-axis ticks

    # Add a color bar which maps values to colors
    fig.colorbar(surf, ax=ax, shrink=0.5, aspect=5)

    plt.show(block=False)

# File name
file_name1 = "128-bit-prime-modulus.csv"
file_name2 = "128-bit-prime-modulus-gmp.csv"
file_name3 = "256-bit-prime-modulus.csv"
file_name4 = "256-bit-prime-modulus-gmp.csv"
file_name5 = "384-bit-prime-modulus.csv"
file_name6 = "384-bit-prime-modulus-gmp.csv"
file_name7 = "512-bit-prime-modulus.csv"
file_name8 = "512-bit-prime-modulus-gmp.csv"

# Call the function with the file names
plot_3d_from_file(file_name1, "Plot for " + file_name1)
plot_3d_from_file(file_name2, "Plot for " + file_name2)
plot_3d_from_file(file_name3, "Plot for " + file_name3)
plot_3d_from_file(file_name4, "Plot for " + file_name4)
plot_3d_from_file(file_name5, "Plot for " + file_name5)
plot_3d_from_file(file_name6, "Plot for " + file_name6)
plot_3d_from_file(file_name7, "Plot for " + file_name7)
plot_3d_from_file(file_name8, "Plot for " + file_name8)

plt.show()