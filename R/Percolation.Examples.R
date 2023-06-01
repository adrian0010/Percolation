########################
###
### Leonard Mada
### [the one and only]
###
### Percolation: Examples
###
### draft v.0.1b

### Percolation

# - some experiments in Percolation

### Github:
# https://github.com/discoleo/R/blob/master/Stat/Percolation.R
# https://github.com/discoleo/R/blob/master/Stat/Percolation.Examples.R


####################
####################

### helper Functions

# fast load
source("Percolation.R")

# see file Percolation.R;


#######################
#######################

### Examples
dims = c(80, 80)
p = 0.3

m = sample(c(-1, 0), prod(dims), replace=T, prob=c(p, 1-p))
m = matrix(m, nrow=dims[1])
m[1:10, 1:10]
plot.rs(m, "Percolation")

### REV
# Representative Elementary Volume
m.rev = REV(m, w=10)
plot.rs(m.rev - min(m.rev), "REV")
quantile(m.rev, c(0.1, 0.25, 0.5, 0.75, 0.9))


### Flood Fill
m = flood.all(m)

m[1:10, 1:10]

table(m)
table(m[,dims[2]])

plot.rs(m, "Percolation")


### Diffusion

# - simple: NO Mixing effects;
diffm = diffusion(m)
sum(diffm[m[,dim(m)[2]] > 0, dim(m)[2]])
diffm = norm.flux(diffm);
plot.rs(diffm)

# dynamic Diffusion:
# - with Mixing effects;
# - initial algorithm: takes very LONG!!!
# - new sequential algorithm:
#   BUT converges/advances extremely slow!
# TODO: combine simple + dynamic;
# diffm = diffusion.dynamic.slow(m)
diffm = diffusion.dynamic(m)
sum(diffm[m[,dim(m)[2]] > 0, dim(m)[2]])
apply(diffm, 2, function(x) sum(x[x>0]))
#
diffm = norm.flux(diffm / max(diffm));
plot.rs(diffm)
plot.rs(diffm[,1:30]) # Input
plot.rs(tail.m(diffm, 30)) # Output


# TODO: advective transport;


### Shortest Path
path.m = length.path(m)

id = dim(path.m)[2];
path.m[1:10, seq(id - 10, id)]

table(path.m[,dims[2]])


### Length of Path
# Gradient = distance from origin;
plot.rs(path.m, main="Path Length")


### Stat/Percolation
# - cells not accessible;
sum(m == 0) / prod(dim(m))
# - total contact area
a0 = contact.area(m, -1)
# - liquid contact area
a1 = contact.area(m, max.id(m))
a0; a1; a1 / a0;



###################

### Ex 2:
dims = c(80, 80)
p = 0.45

m = sample(c(-1, 0), prod(dims), replace=T, prob=c(p, 1-p))
m = matrix(m, nrow=dims[1])

m = flood.all(m)

which.percol(m)

table(m)
table(m[,dims[2]])


plot.rs(m, main="Percolation: Multiple Paths")


### Shortest Path
path.m = length.path(m)

id = dim(path.m)[2];
path.m[1:10, seq(id - 10, id)]

table(path.m[,dims[2]])


### Raster
plot.rs(path.m, main="Path Length")
# Note:
# - only "dominant" path is visualized;

# plot.rs(length.path(m, id=5), main="Path Length")


### Stat/Percolation
# - cells not accessible;
sum(m == 0) / prod(dim(m))
# - total contact area
a0 = contact.area(m, -1)
# - liquid contact area
a1 = contact.area(m, max.id(m))
a0; a1; a1 / a0;


#############

### Ex 3:
dims = c(200, 200) # takes long!
p = 0.40

m = sample(c(-1, 0), prod(dims), replace=T, prob=c(p, 1-p))
m = matrix(m, nrow=dims[1])
m[1:10, 1:10]

m = flood.all(m)

m[1:10, 1:10]

table(m)
table(m[,dims[2]])


#################
#################

### Periodic Boundary Condition

# TODO:
# - real Periodic Boundary;

# - takes ~20 s;
dims = c(2000, 80)
p = 0.4

m = sample(c(-1, 0), prod(dims), replace=T, prob=c(p, 1-p))
m = matrix(m, nrow=dims[1])
m[1:10, 1:10]

m = flood.all(m)

m[1:10, 1:10]

table(m)
table(m[,dims[2]])


plot.rs(split.rs(m), main="Percolation: Multiple Paths")

# plot.rs(m, main="Percolation: Multiple Paths")


### Shortest Path
path.m = length.path(m)

id = dim(path.m)[2];
path.m[1:10, seq(id - 10, id)]

table(path.m[,dims[2]])


### Raster
plot.rs(path.m, main="Path Length")
# Note:
# - only "dominant" path is visualized;

# plot.rs(length.path(m, id=5), main="Path Length")

plot.rs(split.rs(shuffle.colors(clean.percol(m))), main="Percolating Paths")

plot.rs(split.rs(clean.percol(m)), main="Percolating Paths")


####################
### Stat/Percolation

### Inaccessible
# - cells that are not accessible;
sum(m == 0) / prod(dim(m))
# ~ 17%;
### TODO: compute over all paths;
# - total contact area
a0 = contact.area(m, -1)
# - liquid contact area
a1 = contact.area(m, max.id(m))
a0; a1; a1 / a0;


### Average Path Height
# - may be useful for Normalization;
h.m = height.m(clean.percol(m))
h.diff.m = h.m[,2,] - h.m[,1,]
plot.rs(h.diff.m, "Height per column")
h.diff.m[1:10, 1:10]
apply(h.diff.m, 1, mean)


### Other:
# - Average Inputs;
# - Average Outputs;
# - Average Length;
# - Flux, Bottleneck;


path.all = length.path(reset.m(m), id=0)
plot.rs(split.rs(path.all), main="All Path Lengths")

### Mean Distance from In
# - includes also non-percolating paths;
mean(path.all[path.all > 0]) / ncol(path.all)
# ~ 0.98; # sum over the entire path length;
path.percol = path.all; path.percol[clean.percol(m) == 0] = 0;
mean(path.percol[path.percol > 0]) / ncol(path.percol)
# ~ 1.096; # sum over the entire path length;
### Mean Out Distance
nc = ncol(path.all)
mean(path.all[path.all[,nc] > 0, nc]) / nc
# ~ 2.02;


#####################
#####################

### Random Blocks
# - NOT uniformly random;

m = rblock.gen(c(450, 10), c(3,3))

plot.rs(split.rs(m))


path.m = flood.all(m)
path.m = shuffle.colors(path.m)
plot.rs(split.rs(path.m), main="Paths")


#############

### Ex 2
# - min = explicit values with number of elements per block which are "closed";
m = rblock.gen(c(450, 10), c(5,5), min=c(1:5, 15:20))

plot.rs(split.rs(m))

# Note: takes a few seconds!
path.m = flood.all(m)
path.m = shuffle.colors(path.m)
plot.rs(split.rs(path.m), main="Paths")

# added blue to "green"-percolating paths;
plot.rs(split.rs(path.m), main="Paths", addBlue=path.m)


#############

### Ex 3:
m = rblock.gen(c(450, 10), c(3,3), min=c(1,7))

plot.rs(split.rs(m))


path.m = flood.all(m)
path.m = shuffle.colors(path.m)
plot.rs(split.rs(path.m), main="Paths")



#############

### Ex 4:
# - foamy structure, little (or no) percolation;
m = rblock.gen(c(450, 10), c(3,3), min=c(4,5), prob=c(0.65, 0.35))

plot.rs(split.rs(m))


path.m = flood.all(m)
path.m = shuffle.colors(path.m)
plot.rs(split.rs(path.m), main="Paths")

table(path.m[,dim(m)[2]])

cfill.m = count.fill(path.m)
plot.rs(split.rs(cfill.m), main="Confluent Areas")

### Filter
# - remove small areas;
cfill.m[(cfill.m > 0) & (cfill.m < 5)] = 0
plot.rs(split.rs(cfill.m), main="Confluent Areas")

# How many inputs & In-widths:
table(path.m[,1])
# Area of inputs:
table(cfill.m[,1])


#############

### Ex 4:
# - foamy structure, little percolation;
m = rblock.gen(c(450, 10), c(3,3), min=c(4,5), prob=c(0.704, 0.296))

plot.rs(split.rs(m))


path.m = flood.all(m)
path.m = shuffle.colors(path.m)
plot.rs(split.rs(path.m), main="Paths")

table(path.m[,dim(m)[2]])

cfill.m = count.fill(path.m)
plot.rs(split.rs(cfill.m), main="Confluent Areas")

### Filter
# - remove small areas;
cfill.m[(cfill.m > 0) & (cfill.m < 5)] = 0
plot.rs(split.rs(cfill.m), main="Confluent Areas")

# How many inputs & In-widths:
table(path.m[,1])
# Area of inputs:
table(cfill.m[,1])

##################
##################

### Linear Channels

m = rliniar.gen(100, 80, d=1)
plot.rs(m)

m.fl = flood.all(m)
m.fl = shuffle.colors(m.fl)
plot.rs(m.fl)


### pBlock
m = rliniar.gen(100, 80, d=1, pblock=1)
plot.rs(m)

m.fl = flood.all(m)
m.fl = shuffle.colors(m.fl)
plot.rs(m.fl)

########################

### Linear Walk Channels

m = rlinwalk.gen(100, 80, 10)
plot.rs(split.rs(m, n=3))

m.fl = flood.all(m)
m.fl = shuffle.colors(m.fl)
plot.rs(split.rs(m.fl, n=3))

##############
### Example 2:
m = rlinwalk.gen(99, 80, 8)
plot.rs(split.rs(m, n=4))

m.fl = flood.all(m)
m.fl = shuffle.colors(m.fl)
plot.rs(split.rs(m.fl, n=4))

##############
### Example 3:
# skewed/tilted channels
m = rlinwalk.gen(99, 80, 8, pwalk=c(1,1,3), first.const=FALSE)
plot.rs(split.rs(m, n=4))

m.fl = flood.all(m)
m.fl = shuffle.colors(m.fl)
plot.rs(split.rs(m.fl, n=4))


### Ex 3b:
# skewed/tilted channels
m = rlinwalk.gen(99, 80, 8, pwalk=c(1,1,3), first.const=FALSE, duplicate.at=4)
plot.rs(split.rs(m, n=4))

m.fl = flood.all(m)
m.fl = shuffle.colors(m.fl)
plot.rs(split.rs(m.fl, n=4))


##############
### Example 4:
# crossing + duplicated channels
m = rlinwalk.gen(99, 80, 8, walk=c(-1,0,1,2), pwalk=c(4,4,2,1),
	duplicate.at=8+5, first.const=FALSE)
plot.rs(split.rs(m, n=4))

# - discontinuities/pores are also created;
# - takes slightly longer to flood-fill (~1 min)!
m.fl = flood.all(m)
m.fl = shuffle.colors(m.fl)
plot.rs(split.rs(m.fl, n=4))

##############
### Example 5:
# crossing channels
m = rlinwalk.gen(99, 80, 5, walk=c(-2,-1,0,1,2), pwalk=c(1,6,4,6,1))
plot.rs(split.rs(m, n=4))

# - discontinuities/pores are also created;
# - with pwalk=c(1,2,2,2,1):
#  -- one huge connected component!
#  -- takes quite long to flood-fill (1-4 mins)!
m.fl = flood.all(m)
m.fl = shuffle.colors(m.fl)
plot.rs(split.rs(m.fl, n=4))


##############
##############

### Test
m = flood(m, c(1,1), max(m)+1)
m[1:10, 1:10]

table(m == 0)


### Test Raster
rs.m = toRaster(path.m);
plot(rs.m)

###################

tapply.old.toDF = function(x) {
	# instead use: tapply.seq();
	n = nrow(x);
	res.l = tapply(seq(n), x, function(x) seq(length(x)));
	dims = attr(res.l, "dim");
	f1.len = dims[1]; # may NOT be robust!
	f2.len = dims[2];
	id1 = sapply(seq(length(res.l)), function(id) {
		id0 = ((id - 1) %% f1.len) + 1;
		rep(id0, length(res.l[[id]]))
	})
	id2 = sapply(seq(length(res.l)), function(id) {
		id2 = ((id - 1) %/% f2.len) + 1;
		rep(id2, length(res.l[[id]]))
	})
	fs = attr(res.l, "dimnames");
	data.frame(
		id1=as.numeric(fs[[1]][unlist(id1)]),
		id2=as.numeric(fs[[2]][unlist(id2)]), n=unlist(res.l));
}
