from matplotlib.pyplot import *
import random as rd 
import numpy as np
from mpl_toolkits.mplot3d.axes3d import *


def main():
    fig = figure()
    ax = fig.add_subplot(111, projection='3d')

    with open("data.txt", "r") as dataf:
        n, m = tuple(map(int, dataf.readline().split()))
        print(n, m)
        x = tuple(map(float, dataf.readline().replace(",", ".").split()))
        z = tuple(map(float, dataf.readline().replace(",", ".").split()))
        print(x)
        print(z) 
        y = []
        for i in range(n):
            #y.append(tuple(map(float, dataf.readline().replace(",", ".").split())))
            tmp = tuple(map(float, dataf.readline().replace(",", ".").split()))
            ntmp = [round(x, 5) for x in tmp]
            print(ntmp)
            y.append(ntmp)
    y = tuple(y)
    xgrid, zgrid = np.meshgrid(x, z)
    ygrid = np.array(y, np.float64)
    ax.plot_surface(xgrid, zgrid, ygrid, color='b')
    ax.set_xlabel('X axis')
    ax.set_zlabel('Y axis')
    ax.set_ylabel('Z axis')
    show()

main()