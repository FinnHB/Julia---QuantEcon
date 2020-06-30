#Accessing Github Project#
using InstantiateFromURL
github_project("QuantEcon/quantecon-notebooks-julia", version="0.8.0")

#-----------------------------------#
#           Excercise 1             #
#  (Building a factorial function)  #
#-----------------------------------#

#Building Function
function factorial2(n)
    output = 1
    for i in 1:n
        output = output*i
    end
    return output
end

#Testing if function gives the same results as base Julia
test_var = falses(10)
for i in 1:10
    test_var[i] = (factorial(i) == factorial2(i))
end
print(all(test_var))



#---------------------------------------------#
#                  Excercise 2                #
#  (Number of successes in a binomial trial)  #
#---------------------------------------------#

#Function
function binomial_rv(n, p)
    return sum(rand(n).<=p)
end

#Testing function
for i in 1:10
    output = binomial_rv(10, 0.5)
    print("$output, ")
end



#------------------------------------#
#             Excercise 3            #
#  (Estimating pi with Monte-Carlo)  #
#------------------------------------#

#Estimation Function 1
function estimatepi1(;n)
    hypotenous = function(x, y)
        return √(x^2 + y^2)
    end
    x, y = rand(n), rand(n)
    output = hypotenous.(x, y) .< 1
    return mean(output)*4
end


#Estimation Function 2
function estimatepi2(;n)
    x, y = rand(n), rand(n)
    output = sqrt.(x.^2 .+ y.^2) .< 1
    return mean(output)*4
end

sum

#Estimation Function 3
function estimatepi3(;n)
    count = 0
    for in in 1:n
        u, v = rand(2)
        d = sqrt((u - 0.5)^2 + (v - 0.5)^2)
        if d < 0.5
            count = count + 1
        end
    end
    pi = (count/n)*4
    return pi
end

#Results
@time estimatepi1(n=10000000)
@time estimatepi2(n=10000000)
@time estimatepi3(n=10000000)
@time π


#-------------------------------------#
#             Excercise 4             #
#  (Coin Flipping, 3 in a row = Win)  #
#-------------------------------------#

#Writing Function
function coinflip(n)
    pay = 0
    flips = rand(n).<=0.5
    flips = sum(flips)
    flips > 3 ? pay = 1 : 0
    return (pay = "You need to pay \$$pay", results = "$flips heads")
end

coinflip(10)


histogram(randn(10000000))


#--------------------------------------#
#              Excercise 5             #
#  (Simulated Correlated Time Series)  #
#--------------------------------------#

#Writing Function
function correlatedTS(α = 0.9, n = 200)
    x = zeros(n)
    ϵ = randn(n)
    for t in 1:n-1
        x[t+1] = α*x[t] + ϵ[t+1]
    end
    return x
end

#Plotting Results
x = correlatedTS(0.9, 200)
plot(x)



#------------------------------------#
#             Excercise 6            #
#  (Plotting Correlated Timeseries)  #
#------------------------------------#

x = correlatedTS(0, 200)
y = correlatedTS(0.8, 200)
z = correlatedTS(0.98,200)

plot(x, label = "α = 0")
plot!(y, label = "α = 0.8")
plot!(z, label = "α = 0.98")

plot([x, y, z])



#-----------------#
#   Excercise 7   #
#  (Random Walk)  #
#-----------------#

#Package for this question
using DataStructures #Package for ordered dictionaries

#Functions
RandomWalk = function(α, σ, t_max, a)
    #Initiating variables
    x = ones(t_max+1)
    ϵ = randn(t_max+1)
    num = collect(1:t_max+1) #Variable for indexing boolean

    #Calculating time series
    for t in 1:t_max
        x[t+1] = α*x[t] + σ*ϵ[t+1]
    end

    #Conditional if x[t] < a
    if any(x.<a)
        T = num[x.<a][1]
    else
        T = t_max
    end
    return T
end
plotting_hist = function(λ, α, N; σ = 0.2, t_max = 200, a = 0,
                         plot_transparancy = 0.5)
    #Initiate Variables
    outputs = OrderedDict()
    hist = histogram()

    #Iterating through alphas
    for i in 1:length(α)
        results = zeros(N)
        A = α[i]
        varname = map(join, zip(["α_"], A))
        push!(outputs, varname => nothing)
        for n in 1:N
            results[n] = λ(A, σ, t_max, a)
        end
        #Data
        outputs[varname] = results

        #Histogram
        hist = histogram!(results, label = varname, alpha = plot_transparancy,
                          bins = range(0, stop = t_max+1,
                          length = Int(t_max/10)))
    end
    #Plotting mean value and alpha values
    p = plot(mean.(values(outputs)), α)

    #Returning data, histogram, and plot
    return (data = outputs, histogram = hist, plot = p)
end

#Plotting the results
input_alpha = collect(0.6:0.2:1.2)
x, h, p = plotting_hist(RandomWalk, input_alpha, 3000)

x
h
p


#-----------------#
#   Excercise 8   #
#  (Random Walk)  #
#-----------------#

#== Part A ==#

#Function
fixedpointmap = function(f, f_prime, x_0;
                         tolerance = 1.0E-10, maxiter = 1000000)
    # Initiating Variables
    d = Inf
    i = 1
    x_old = x_0

    #Iterating and stopping when results have converged
    while i < maxiter && d > tolerance
        x_new = x_old - (f(x_old)/f_prime(x_old))
        d = abs(x_new - x_old)
        x_old = x_new
        i += 1
    end
    return (value = x_new, iter = i, distance = d)
end

#Results
f(x) = (x-1)^3
f_prime(x) = 3(x-1)^(2)
fixedpointmap(f, f_prime, 40)


#== Part B ==#
using ForwardDiff

#Function
fixedpointmap = function(f, x_0;
                         tolerance = 1.0E-10, maxiter = 1000000)
    # Initiating Variables
    D(f)=x->ForwardDiff.derivative(f, x)
    f_prime = D(f)
    d = Inf
    i = 1
    x_old = x_0

    #Iterating and stopping when results have converged
    while i < maxiter && d > tolerance
        x_new = x_old - (f(x_old)/f_prime(x_old))
        d = abs(x_new - x_old)
        x_old = x_new
        i += 1
    end
    return (value = x_new, iter = i, distance = d)
end

#Results
f(x) = (x-1)^3
fixedpointmap(f, 40)
