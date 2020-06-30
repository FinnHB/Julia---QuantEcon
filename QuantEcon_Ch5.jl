using LinearAlgebra, Statistics, QuantEcon, Distributions, Plots, DataStructures

#----------------------#
#     Excercise 1      #
#  (Solving Lyapunov)  #
#----------------------#

#Parameters
A = [0.8 -0.2; -0.1 0.7]
Σ = [0.5 0.4; 0.4 0.6]

plot([yyy[1], yyy[2], yyy[3]],
     labels = ["θ: 0.8", "θ: 0.0"])
plot(yyy[1], yyy[2])


#Function
OurSolution = function(A, Σ, iterations)
    s = I
    for i in 1:iterations
        s = A*s*A' + Σ*Σ'
    end
    return s
end

#Testing Solution
OurSolution(A, Σ, 100) ≈ solve_discrete_lyapunov(A, Σ * Σ')
@time OurSolution(A, Σ, 100000)
@time solve_discrete_lyapunov(A, Σ * Σ')

#------------------------#
#       Excercise 2      #
#  (Stochastic Process)  #
#------------------------#

#Functions
StochasticProcess = function(T, θ; γ = 1, σ = 1, y_0 = 0)
    w = rand(Normal(0,1), T+1)
    y = zeros(T+1)
    y[1] = y_0
    for t in 1:T
        y[t+1] = γ + θ*y[t] + σ*w[t+1]
    end
    return y
end

rollingmean = function(x, τ)
    output = zeros(length(x))
    for t in τ:length(x)
        output[t] = mean(x[t-τ+1:t])
    end
    return output
end

T_Stochastic = function(f, α; N = 150, T=150, hist_min = 0, hist_max = 80,
                        hist_bins = 25, hist_transparancy = 0.7)

    #Initialising variables
    out_dict = OrderedDict()
    h = stephist()
    μ = zeros(length(α))
    σ = zeros(length(α))

    #Looping through alphas
    for A in 1:length(α)
        varname = map(join, zip(["α_"], α[A]))
        push!(out_dict, varname[1] => nothing)

        #Iterating through N stochastic time series
        f_output = zeros(N)
        for n in 1:N
            f_output[n] = f(T, α[A])[T+1]
        end

        #Updating dictionary & calculating mean/Variance
        out_dict[varname[1]] = f_output
        μ[A] = sum(f_output)/N
        σ[A] = sum(f_output.^2/N) - (sum(f_output/N))^2

        #Updating histogram
        stephist!(f_output, label = varname, alpha = hist_transparancy,
                  bins = range(hist_min, stop = hist_max,
                  length = Int(hist_max/(hist_max/hist_bins))))
    end
    return (results = out_dict, μ = μ, σ = σ, hist = h)
end


#Results
x = StochasticProcess.(300, [0.8, 0.9, 0.98])
m = rollingmean.(x, 20)
plot(m, labels = ["θ = 0.80" "θ = 0.90" "θ = 0.98"])

k, μ, σ, h = T_Stochastic(StochasticProcess, [0.8, 0.9, 0.98], hist_bins = 100, N = 2000)

h
σ
μ
k



#------------------------#
#       Excercise 3      #
#  (Stochastic Process)  #
#------------------------#

[f(x) for x in eachrow(M)]

#Function for taking draws of Y
drawy = function(x_1, x_2, w; N=50, a = 0.1, b = 0.2, c = 0.5, d = 1.0, σ = 0.1)
    y = zeros(N)
    for n in 1:N
        y[n] = a*x_1[n] + b*x_1[n]^2 + c*x_2[n] + d + σ*w[n]
    end
    return y
end

#Function for repeated OLS estimation
OLS = function(X; M = 20, y_func = drawy, N=50, a = 0.1, b = 0.2, c = 0.5, d = 1.0, σ = 0.1)
    output = zeros(M, 5)
    for m in 1:M
        w = randn(size(X)[1])
        y = y_func(X[:,1], X[:,3], w, N=N, a = a, b = b, c = c, d = d, σ = σ)
        output[m,:] = inv(X'X)X'y
    end
    ahat, bhat, chat, dhat, σhat = (col for col in eachcol(output))
    return ahat, bhat, chat, dhat, σhat
end

#Creating X matrix
N = 1000
M = 20
x_1, x_2, w = [col for col in eachcol(randn(N, 3))]
constant = ones(N)
X = [x_1 x_1.^2 x_2 constant w] #X Matrix

#Estimation
a, b, c, d, σ = OLS(X, M = M, N = N)

#Plotting results
stephist([a, b, c, d, σ], labels = ["a" "b" "c" "d" "σ"])



#-----------------------#
#       Excercise 4     #
#   (Solving Lyapunov)  #
#-----------------------#
using NLsolve

#Parameters
A = [0.8 -0.2; -0.1 0.7]
Σ = [0.5 0.4; 0.4 0.6]

#Function
OurSolution = function(A, Σ, iterations)
    s = I
    for i in 1:iterations
        s = A*s*A' + Σ*Σ'
    end
    return s
end

#Testing Solution
OurSolution(A, Σ, 100) ≈ solve_discrete_lyapunov(A, Σ * Σ')
@time OurSolution(A, Σ, 100000)
@time solve_discrete_lyapunov(A, Σ * Σ')


x = [1.0, 2.2, 3.2]
eltype(x)
@time ones(length(x))
@time ones(eltype(x), length(x))
