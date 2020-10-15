import pandas as pd
import numpy as np
import matplotlib.pyplot as plt


disasters = pd.read_csv("coal-mine.csv", sep = ".", header = None).fillna(0)

# constants
N = 10000
burn_in = 5000
d = 3 # d-1 breakpoints
rho = 1
v = 1

# define t
t_start = 1851
t_end = 1963
t_endpoints = np.array([t_start, t_end])
# select randomly first breakpoints
t_0 = np.random.randint(t_start,t_end,d-1)
# put together in vector
t = np.sort(np.concatenate([t_endpoints, t_0]))

# list to save acceptance rate
accept = []


# total sum of disasters per year
n_per_year = disasters[0].value_counts().sort_index()
# create intervals
intervals = []
for i in range(1,d+1):
    intervals.append(list(range(t[i-1],t[i])))
# sum over intervals
n = []
for interval in intervals:
    n.append(int(np.sum(n_per_year.loc[n_per_year.index.isin(interval)])))
n = np.array(n)

# parameters
theta = np.random.gamma(2, 1/v)
lam = np.random.gamma(2, 1/theta, d)

# create dictionary to save different breakpoints
accepted_t = {}
for i in range(1,d):
    accepted_t[i] = []

# create list to save lambdas and thetas
accepted_lambda = []
accepted_theta = []
for k in range(burn_in+N):
    # produce new theta
    theta_new = np.random.gamma(2*d+2, 1/(v+np.sum(lam)))
    # produce new lambdas
    lam_new = []
    for i in range(d):
        lam_new.append(np.random.gamma(n[i]+2,1/(t[i+1]-t[i]+theta_new)))
    lam_new = np.array(lam_new)
    # for each iteration we reset t and use a temporary variable called t_temp
    t_temp = np.sort(np.concatenate([t_endpoints, t_0]))
    for i in range(1,d):
        # random walk proposal
        R = rho*(t_temp[i+1]-t_temp[i-1])
        epsilon = np.random.uniform(-R,R,1)
        t_new = int(t_temp[i]+epsilon)
        # check condition
        while t_new >= t_temp[i+1] or t_new <= t_temp[i-1]:
            epsilon = np.random.uniform(-R, R, 1)
            t_new = int(t_temp[i] + epsilon)

        # generate alpha
        t_try = t_temp
        t_try[i] = t_new
        # create new n for t_try
        n_try = []
        intervals_try = []
        for j in range(1, d + 1):
            intervals_try.append(list(range(t_try[j - 1], t_try[j])))
        for interval in intervals_try:
            try:
                n_try.append(int(np.sum(n_per_year.loc[n_per_year.index.isin(interval)])))
            except:
                n_try.append(0)
        n_try = np.array(n_try)

        alpha = np.min([1, np.exp(-np.sum(np.diff(t_try)*lam_new))*np.prod(np.diff(t_try))*np.prod(lam_new**n_try)/(np.exp(-np.sum(np.diff(t_temp)*lam_new))*np.prod(np.diff(t_temp))*np.prod(lam_new**n))])
        u = float(np.random.uniform(0,1,1))
        if u <= alpha:
            t_temp[i] = t_new
            n = n_try
            accept.append(1)
            accepted_t[i].append(t_new)
            accepted_lambda.append(lam_new)
            accepted_theta.append(theta_new)
        else:
            t_temp[i+1] = t_temp[i]
            accept.append(0)

    # overwrite old lambda with new lambda
    lam = lam_new
    # calculate new intervals
    intervals = []
    for j in range(1, d + 1):
        intervals.append(list(range(t_temp[j - 1], t_temp[j])))
    # new n
    n = []
    for interval in intervals:
        try:
            n.append(int(np.sum(n_per_year.loc[n_per_year.index.isin(interval)])))
        except:
            n.append(0)
    n = np.array(n)

# stack lambdas into one array for plot
lambdas = np.vstack(accepted_lambda)
accept = np.array(accept)
accepted_ratio = np.count_nonzero(accept[burn_in:])/((d-1)*(burn_in+N))

# plots
# breakpoints
fig1, ax1 = plt.subplots()
for i in range(1,d):
    ax1.hist(np.array(accepted_t[i])[burn_in:], alpha = 0.5, bins = 30, label = "breakpoint "+ str(i))
ax1.set(xlabel = "Breakpoint", ylabel= "Frequency")
fig1.legend()
fig1.show()

# lambdas
fig2, ax2 = plt.subplots()
for i in range(d):
    ax2.hist(lambdas.T[i][burn_in:], bins = 30, alpha = 0.5, label = "lambda "+ str(i+1))
ax2.set(xlabel = "Lambda", ylabel= "Frequency")
fig2.legend()
fig2.show()

# thetas
plt.hist(np.array(accepted_theta)[burn_in:], bins = 30)
plt.xlabel("Theta")
plt.ylabel("Frequency")
plt.show()









