
cd("C:/Users/finn_/OneDrive/Documents/Julia")

#----------------------------#
#        Excercise 1         #
#  (Using the zip function)  #
#----------------------------#

#Part 1
x_vals, y_vals = [1, 2, 3, 4, 5], [5, 4, 3, 2, 1]
sum(x*y for (x, y) in zip(x_vals, y_vals))

#Part 2
x_vals = sum(iseven(i) for i in 0:99)

#Part 3
pairs = ((2, 5), (4, 2), (9, 8), (12, 10))
sum(all(iseven.(pair)) for pair in pairs)


#---------------------------------#
#           Excercise 2           #
#  (Writing Polynomial Function)  #
#---------------------------------#

#Writing Function
p = function(x, coeff)
    output = 0
    for (i, α) in enumerate(coeff)
        output += α*x^(i-1)
    end
    return output
end

p(2, [0, 1, 99, 1])



#------------------------------------#
#            Excercise 3             #
#  (Returning Number of Uppercase)   #
#------------------------------------#

#Function
example_string = "SoMe String WIth CapiTALS"

CountUpper = function(string)
    output = 0
    for letter in string
        output += isuppercase(letter)
    end
    return output
end

#Results
CountUpper(example_string)



#---------------#
#  Excercise 4  #
#   (x in y)    #
#---------------#

#Function
seq_a = [1, 2, 4, 3, 5, "cat"]
seq_b = [1, 2, 3, 4, 5, "cat"]

allin = function(seq_a, seq_b)
    output = falses(length(seq_a))
    for (i, a) in enumerate(seq_a)
        output[i] = a in seq_b
    end
    return all(output)
end

#Results
allin(seq_a, seq_b)



#-----------------------------#
#         Excercise 5         #
#   (Estimating Functions)    #
#-----------------------------#

#Parameters
f(x) = x^2
limits = (a = 0, b = 10)
n = 1000

#Function
interpolate = function(f, limits, n)
    step_value = (limits[2]-limits[1])/n
    x = limits[1]:step_value:limits[2]
    return (x = x, f = f.(x))
end


#Plotting Results
using Plots
x, y = interpolate(f, (-1, 1), 100)
x1, y1 = interpolate(f, (-1, 1), 2)
plot(x, y, label = "True")
plot!(x1, y1, label = "Approximation")

x = [1,2,3]
f

#----------------------#
#      Excercise 6     #
#  (Total Population)  #
#----------------------#

#Reading Data
cities = readlines(open("us_cities.txt"))
cities = split.(cities, ":")

#Function
sumpop = function(cities)
    output = 0
    for i in eachindex(cities)
        output += parse(Int, cities[i][2])
    end
    return output
end

#Results
sumpop(cities)

#Alternative solution
sum(parse(Int, c[2]) for c in cities)
