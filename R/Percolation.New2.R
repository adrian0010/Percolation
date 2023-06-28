###############
# Student: Adrian Ivan
# Candidate BSc CS 2023
# West University, Timisoara

# Supervisors:
# Prof. Dr. Daniela Zaharie
# Dr. med. Leonard Mada (Syonic SRL)

# URL: https://github.com/adrian0010/Percolation

# Based on Previous Projects (2020-2022)


FUN = readLines("Percolation.Analysis.cpp")

FUN = paste0(FUN, collapse="\n")

cppFunction(FUN)


### Examples

x = rgrid.channel(50, 40, d=1, ppore=4, pblock=4)
# x = rgrid.channel(800, 40, d=1, ppore=5, pblock=5)
m = flood.all(x)

plot.rs(m)

table(m[, ncol(m)])

# TODO: enhanced pallet;
tmp = height.channel.all(m)
tmp[tmp > 1] = tmp[tmp > 1] + 2;
plot.rs(tmp)

plot.rs(split.rs(tmp, 6))


id = 6
tmp = height.channel(m, id=id)
plot.rs(tmp)

