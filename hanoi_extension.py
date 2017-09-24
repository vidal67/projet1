import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import axes3d

file = open("/Users/vidalattias/Desktop/projet1/hanoi2D.csv")

discs = []
picks = []
totalMoves = []

boolHeader = True
for line in file:
    if boolHeader:
        boolHeader = False
    else:
        tabs = line.split(';')
        tabs[2] = tabs[2].replace('\n', '')
        if int(tabs[0]) not in discs:
            discs.append(int(tabs[0]))
        if int(tabs[1]) not in picks:
            picks.append(int(tabs[1]))
        totalMoves.append(int(tabs[2]))
        
moves = [[0 for i in range(len(picks))] for j in range(len(discs))]

for i in range(len(discs)):
    for j in range(len(picks)):
        moves[i][j] = np.log2(totalMoves[i*(len(picks))+j])


d, p = np.meshgrid(discs, picks)

fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')

ax.plot_wireframe(d, p, np.transpose(moves), rstride=1, cstride=1)











