import numpy as np
import matplotlib.pyplot as plt
from scipy.io import loadmat
import pandas as pd
import time

#Define own mvnpdf because no built-in SciPy function
#can vectorize the array
def mvnpdf(x, array, cov):
    mean = np.array(np.hsplit(x.reshape(6,1) - array, N))
    part1 = 1/np.sqrt(((2*np.pi)**len(x))*np.linalg.det(cov))
    part2 = np.exp(-(1/2)*np.matmul(np.matmul(mean.transpose((0,2,1)),np.linalg.inv(cov)), mean))
    part2 = np.concatenate(part2)
    y = part2 * part1
    return y

start = time.time()

#define all variables
N = 10000
sigma = 0.5
delta_t = 0.5
alpha = 0.6
eta = 3
v = 90
epsilon = 1.5

# Set for Z
set_Z = np.array([[0, 0],
                  [3.5, 0],
                  [0, 3.5],
                  [0, -3.5],
                  [-3.5, 0]])

# Matrices for model
phi = np.array([[1, delta_t, (delta_t ** 2) / 2, 0, 0, 0],
                [0, 1, delta_t, 0, 0, 0],
                [0, 0, alpha, 0, 0, 0],
                [0, 0, 0, 1, delta_t, (delta_t ** 2) / 2],
                [0, 0, 0, 0, 1, delta_t],
                [0, 0, 0, 0, 0, alpha]])

psi_z = np.array([[(delta_t ** 2) / 2, 0],
                  [delta_t, 0],
                  [0, 0],
                  [0, (delta_t ** 2) / 2],
                  [0, delta_t],
                  [0, 0]])
psi_w = np.array([[(delta_t ** 2) / 2, 0],
                  [delta_t, 0],
                  [1, 0],
                  [0, (delta_t ** 2) / 2],
                  [0, delta_t],
                  [0, 1]])

# Transition matrix for Z
P = (1 / 20) * np.array([[16, 1, 1, 1, 1],
                         [1, 16, 1, 1, 1],
                         [1, 1, 16, 1, 1],
                         [1, 1, 1, 16, 1],
                         [1, 1, 1, 1, 16]])

#load observation and station positions
Y_obs = loadmat("RSSI-measurements.mat")["Y"] #REAL HW OBS
#Y_obs = np.array(pd.read_csv("Y_artificial_obs.csv")).T
pos_vec = loadmat("stations.mat")["pos_vec"]


Z_0 = np.random.randint(0,4,N)
X_0 = np.random.multivariate_normal(np.zeros(6),np.diag([500, 5, 5, 200, 5, 5]),N).T
#plot first dot
plt.scatter(X_0[0],X_0[3], color = "red")

tau = [] #list to save trajectory
Z_most_probable = [] #list for keeping most probable drivers

#Mean matrix for all particles A
A = v-10*eta*np.log10(np.linalg.norm(np.concatenate(X_0[[0,3]].T[:,None] - pos_vec.T), axis = 1).reshape(6,N,order = "F"))

#Create vector for starting omegas
omega_0 = mvnpdf(Y_obs.T[0], A, (epsilon**2)*np.identity(6)).reshape(N,)

#first point
tau_01 = np.sum(np.multiply(omega_0, X_0[0]))/np.sum(omega_0)
tau_02 = np.sum(np.multiply(omega_0, X_0[3]))/np.sum(omega_0)
tau.append((tau_01,tau_02))

for k in range(0,Y_obs.shape[1]):
    #MOST PROBABLE DRIVER
    Z_most_probable.append(np.argmax(np.bincount(Z_0)))
    #RESAMPLING
    index = np.random.choice(N, N, p=omega_0 / np.sum(omega_0)) #choose with replacement
    X_0 = X_0.T[index].T #above function generate index, extract them here
    #PROPAGATE/MUTATE
    mean_vector = np.matmul(phi,X_0)+np.matmul(psi_z, set_Z[Z_0].T)
    X_next = mean_vector + np.matmul(psi_w,np.random.multivariate_normal(np.zeros(2),(sigma**2)*np.identity(2),N).T)
    X_pos = X_next[[0, 3]]
    #WEIGHT
    A = v-10*eta*np.log10(np.linalg.norm(np.concatenate(X_next[[0,3]].T[:,None] - pos_vec.T), axis = 1).reshape(6,N,order = "F"))
    omega_next = mvnpdf(Y_obs.T[k], A, (epsilon**2)*np.identity(6)).reshape(N,)
    #ESTIMATE:
    tau_1 = np.sum(np.multiply(omega_next,X_pos[0]))/np.sum(omega_next)
    tau_2 = np.sum(np.multiply(omega_next,X_pos[1]))/np.sum(omega_next)
    tau.append((tau_1,tau_2))
    #Make step with Z
    Z_next = []
    for z in Z_0:
        Z_temp = int(np.random.choice(5, 1, p=P[z])) #apply transition matrix
        Z_next.append(Z_temp)
    Z_next = np.array(Z_next)

    #Reset
    X_0 = X_next
    Z_0 = Z_next
    omega_0 = omega_next


#Plot each station position
station = 1
for station_pos in pos_vec.T:
    # plot trajectory and position of stations in this loop
    plt.scatter(station_pos[0], station_pos[1])
    plt.annotate("station "+ str(station), (station_pos[0], station_pos[1]),
                 textcoords = "offset points",
                 xytext=(0,10), ha = "center")
    station += 1

#Plot trajectory
tau = np.array(tau)
plt.scatter(tau.T[0],tau.T[1])
plt.show()

end = time.time()

print(end - start)


