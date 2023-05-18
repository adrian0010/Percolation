########################
###
### Leonard Mada
### [the one and only]
###
### Percolation
###
### draft v.0.4e

### Percolation

# - some experiments in Percolation
# - for Examples, see file:
#   Percolation.Examples.R;

### GitHub:
# https://github.com/discoleo/R/blob/master/Stat/Percolation.R


####################

###############
### History ###
###############


### draft v.0.4d:
# - [Refactoring] split file;
### draft v.0.4b:
# - added REV: Representative Elementary Volume;
### draft v.0.4a-cleanup:
# - examples have been moved to file:
#   Percolation.Examples.R;


####################
####################

### Helper Functions

# fast load
# source("Percolation.R")


# Basic Helper Functions
source("Percolation.Tools.R")
# Graphic Functions
source("Percolation.Plot.R")
# Analysis
source("Percolation.Analysis.R")


##################
### Generators ###
##################

rgrid = function(dim, ...) {
	UseMethod("rgrid");
}

rgrid.binom = function(dim, prob, val=c(-1,0)) {
	rugrid.gen(dim, p=prob, val=val);
}
rgrid.unif = function(dim) {
	rugrid(dim);
}

### Randomize within blocs
# dim = "dimension" as number of blocks;
rgrid.binblocks = function(dim, block.dim, min=0, max, prob, val=-1) {
	rblock.gen(dim, block.dim, min=min, max=max, prob=prob, val=val);
}

### Linear Channels
# TODO: name?
rgrid.channel = function(dim, w, d, ppore=3, pblock=0.5, val=-1) {
	if(length(dim) == 1) {
		n = dim; w = w; d = d;
	} else {
		if(missing(d)) d = w;
		n = (dim[1] - 1) / d;
		if(n != trunc(n)) warning("Non-integer number of channels!");
		w = dim[2]; 
	}
	rliniar.gen(n=n, w=w, d=d, ppore=ppore, pblock=pblock, val=val);
}


##################

### Random uniform grid
rugrid.gen = function(dims, p, val=c(-1, 0)) {
	m = sample(val, prod(dims), replace=T, prob=c(p, 1-p))
	m = matrix(m, nrow=dims[1])
}

# Probability matrix
# - used to generate a percolation process;
rugrid = function(dims) {
	m = runif(prod(dims));
	m = matrix(m, nrow=dims[1]);
}

as.grid = function(m, p, val=c(-1, 0)) {
	isClosed = m > p;
	m[isClosed] = val[1];
	m[! isClosed] = val[2];
	return(invisible(m));
}

### Randomise within blocks
# n = number of blocks forming the material (height x width);
# min, max = range of "closed" sub-elements in each block;
# - can be also an explicit sequence of values;
# - prob = probability of each of those values;
rblock.gen = function(n, block.dim, min=0, max, prob, val=-1) {
	if(missing(max)) {
		if(length(min) > 2) {
			val.count = min;
		} else {
			if(length(min) == 1) {
				max = prod(block.dim);
			} else {
				max = min[2]; min = min[1];
			}
			val.count = seq(min, max);
		}
	} else {
		val.count = seq(min, max);
	}
	# prod(n) = total number of blocks;
	if(missing(prob)) {
		nn = sample(val.count, prod(n), replace=TRUE);
	} else {
		nn = sample(val.count, prod(n), replace=TRUE, prob=prob);
	}
	# individual blocks:
	blocks.n = prod(block.dim);
	sample.block = function(n) {
		# n = number of "closed" sub-elements;
		bm = array(0L, block.dim);
		npos = sample(seq(blocks.n), n);
		bm[npos] = val; # "close" material;
		bm
	}
	m = lapply(nn, sample.block);
	m = lapply(seq(0, n[2] - 1), function(id) {
		id0 = id*n[1];
		do.call(rbind, m[seq(id0 + 1, id0 + n[1])]);
	})
	m = do.call(cbind, m);
	# Note: old code does NOT preserve blocks!
	# m = do.call(rbind, m);
	# Correct dimensions of Matrix:
	# dims = n * block.dim;
	# dim(m) = dims;
	invisible(m);
}

# ppore, pblock = Poisson distributions;
rliniar.gen = function(n, w, d=5, ppore=3, pblock=0.5, val=-1) {
	# n = no. of channels;
	# w = width of material/grid; d = width of channel;
	nc = d*n+n+1;
	m = matrix(0, nrow=w, ncol=nc);
	# Channel walls
	idChW = seq(1, nc, by=d+1)
	m[, idChW] = val;
	# add Pores
	npores = rpois(length(idChW), ppore)
	xwidth = seq(w);
	for(id in seq(n+1)) {
		xPore = sample(xwidth, npores[id])
		m[xPore, idChW[id]] = 0;
	}
	# block Channels
	nBlock = rpois(n, pblock)
	for(id in seq(n)) {
		if(nBlock[id] == 0) next;
		xBlock = sample(xwidth, nBlock[id]);
		# (id-1)*(d+1)+1
		m[xBlock, seq(idChW[id] + 1, length.out=d)] = -1;
	}
	
	return(t(m));
}


rgrid.channel.poisson = function(n, w, d=3, ppore=6, pBlock=4, 
						type = c("Poisson", "Constant", "0:n", "1:n"), val=-1) {
	# n = no. of channels;
	# w = width of material/grid; d = width of channel;
	type = match.arg(type);
	nc = d*n+n+1;
	m = matrix(0, nrow=w, ncol=nc);
	# Channel walls
	idChW = seq(1, nc, by = d + 1)
	m[, idChW] = val;
	# add Pores
	npores = rpois(length(idChW), ppore)
	xwidth = seq(w);
	for(id in seq(n+1)) {
		xPore = sample(xwidth, npores[id])
		m[xPore, idChW[id]] = runif(npores[id]);
	}
	# block Channels
	if(type == "Poisson"){
		nBlock = rpois(n, pBlock)
	}
	else if (type == "Constant") {
	   	nBlock = rep(pBlock, n)
	}
	else if (type == "0:n") {
	   	nBlock = sample(0:pBlock, n, replace = TRUE)
	}
	else if (type == "1:n") {
	   	nBlock = sample(1:pBlock, n, replace = TRUE)
	}
	for(id in seq(n)) {
		if(nBlock[id] == 0) next;
		xBlock = sample(xwidth, nBlock[id]);
		# (id-1)*(d+1)+1
		m[xBlock, seq(idChW[id] + 1, length.out=d)] = val;
	}
	
	return(t(m));
}

### Linearly (H) Correlated Process
# TODO: types = "Poisson", "Initial";
# - Initial = start always from Initial row;
rgrid.unifCor = function(dim, pChange=1/2, type = c("Constant", "Bernoulli")) {
	type = match.arg(type);
	nr = dim[1]; nc = dim[2];
	m = matrix(0, nrow=nr, ncol=nc);
	# Column 1:
	if(nr == 0 || nc == 0) return(m);
	m[, 1] = runif(nr);
	if(nc == 1) return(m);
	# Subsequent columns
	if(type == "Constant") {
		nCh = round(nr * pChange);
		ids = seq(1, nr);
		tmp = m[, 1];
		for(i in seq(2, nc)) {
			id = sample(ids, nCh);
			tmp[id] = runif(nCh);
			m[, i]  = tmp;
		}
	} else {
		tmp = m[, 1];
		for(i in seq(2, nc)) {
			ids = as.logical(rbinom(nr, 1, pChange));
			tmp[ids] = runif(sum(ids));
			m[, i] = tmp;
		}
	}
	return(m)
}

### Binary Linear Correlation
# - but Non-Linearly, Non-Monotonically Coupled Process;
rgrid.correl = function(dim, pChange=1/3, type = c("Constant", "Bernoulli")) {
	type = match.arg(type);
	nr = dim[1]; nc = dim[2];
	m = matrix(FALSE, nrow=nr, ncol=nc);
	if(nr == 0 || nc == 0) return(m);
	m[,1] = FALSE;
	if(nc == 1) return(m);
	# Subsequent Columns:
	if(type == "Constant") {
		nCh = round(nr * pChange);
		ids = seq(1, nr);
		for(i in seq(2, nc)) {
			tmp = rep(FALSE, nr);
			id  = sample(ids, nCh);
			tmp[id] = ! tmp[id];
			m[, i]  = tmp;
		}
	} else {
		for(i in seq(2, nc)) {
			tmp = rep(FALSE, nr);
			ids = as.logical(rbinom(nr, 1, pChange));
			tmp[ids] = ! tmp[ids];
			m[, i] = tmp;
		}
	}
	return(m);
}

as.grid.correl = function(x, m.cor, p, val=-1) {
	nr = nrow(m.cor); nc = ncol(m.cor);
	if(length(x) != nr) stop("Invalid dimensions of the carry-forward matrix!");
	m = matrix(0, nrow=nr, ncol=nc);
	if(nr == 0 || nc == 0) return(m);
	m[x <= p, 1] = 1;
	as.grid0 = function(m) {
		m[m == 0] = val;
		m[m > 0]  = 0;
		return(m);
	}
	if(nc == 1) {
		return(as.grid0(m));
	}
	# Subsequent Columns:
	tmp = m[, 1];
	for(i in seq(2, nc)) {
		tmp[m.cor[, i]] = 1 - tmp[m.cor[, i]];
		m[, i] = tmp;
	}
	return(as.grid0(m));
}


rlinwalk.gen = function(n, w, d, walk=c(-1,0,1), pwalk=c(1,2,1), ppore=3,
		first.const=TRUE, duplicate.at=NULL, val=-1) {
	# n = no. of channels; w = width of Material;
	# d = diameter of channel;
	nc = d*n+n+1;
	m = matrix(0, nrow=w, ncol=nc);
	# Channel walls:
	# walls start at fixed positions
	walk = walk * w + 1; # walk[walk == 0] = 1;
	nW = if(first.const) n else (n+1);
	wall = sample(walk, nW*(w-1), replace=TRUE, prob=pwalk);
	idChW = seq(1, nc, by=d+1);
	nposChWAbs = (idChW-1)*w + 1;
	wall = matrix(wall, nrow=w-1);
	if(first.const) wall = cbind(1, wall);
	wall = rbind(nposChWAbs, wall);
	wall = apply(wall, 2, cumsum);
	if( ! is.null(duplicate.at)) wall = cbind(wall, wall + duplicate.at*w);
	# unique(sort(...)): common paths may be rare;
	wall = unique(sort(as.vector(wall)));
	wall = wall[wall > 0 & wall <= (w*nc)];
	m[wall] = val;
	invisible(t(m));
	# TODO: add Pores
}
rrect.gen = function(n, dim, w.lim, h.lim, lambda.pores=2, addPores=TRUE,
		type=c("Poisson", "Simple"), aspect.fixed=NULL, prob.dir=c(1,1), val=-1) {
	# n = no. of rectangles;
	HD = 1; VD = 2;
	x0 = round(runif(n, 1, dim[HD]));
	y0 = round(runif(n, 1, dim[VD]));
	dw = round(runif(n, w.lim[1], w.lim[2]));
	if(is.null(aspect.fixed)) {
		dh = round(runif(n, h.lim[1], h.lim[2]));
	} else {
		dh = dw * aspect.fixed;
	}
	xdir.r = sample(c(-1,1), n, replace=TRUE, prob=prob.dir);
	ydir.r = sample(c(-1,1), n, replace=TRUE, prob=prob.dir);
	# OX
	xe = x0 + dw - 1; xs = x0 - dw; # offset: - 1;
	xe[xdir.r < 0] = x0[xdir.r < 0] - 1;
	xs[xdir.r > 0] = x0[xdir.r > 0] - 1;
	hasV2 = ifelse(xdir.r > 0, xe < dim[HD], xs >= 0);
	xe[xe >= dim[HD]] = dim[HD] - 1;
	xs[xs < 0] = 0;
	# OY
	ye = y0 + dh - 1; ys = y0 - dh + 1; # offset: -/+ 1 ???
	ye[ydir.r < 0] = y0[ydir.r < 0];
	ys[ydir.r > 0] = y0[ydir.r > 0];
	hasH2e = (ye <= dim[VD]);
	ye[ ! hasH2e] = dim[VD];
	hasH2s = (ys > 0);
	ys[ ! hasH2s] = 0;
	hasH2 = hasH2e & hasH2s;
	#
	vline.r = function(id) {
		l.seq = (ys[id]:ye[id]);
		if(hasV2[id]) {
			px = xs[id] * dim[VD] + l.seq;
			px = c(px, xe[id] * dim[VD] + l.seq);
		} else if(xdir.r[id] > 0) {
			px = xs[id] * dim[VD] + l.seq;
		} else {
			px = xe[id] * dim[VD] + l.seq;
		}
		return(px);
	}
	hline.r = function(id) {
		l.seq = (xs[id]:xe[id]) * dim[VD];
		if(hasH2[id]) {
			px = ys[id] + l.seq;
			px = c(px, ye[id] + l.seq);
		} else if(ydir.r[id] > 0) {
			px = ys[id] + l.seq;
		} else {
			px = ye[id] + l.seq;
		}
		return(px);
	}
	vl = unlist(lapply(seq(n), vline.r));
	hl = unlist(lapply(seq(n), hline.r));
	### Debug
	if(any(vl < 1)) print("ERROR: V");
	if(any(hl < 1)) print("ERROR: H");
	print(max(vl));
	print(max(hl));
	px = sort(unique(c(vl, hl)));
	m = matrix(0, nrow=dim[VD], ncol=dim[HD]);
	m[px] = val;
	### Pores
	if(addPores) {
		type = match.arg(type);
		if(type == "Simple") {
			nPores = round(n * lambda.pores);
			# nPores = sum(rpois(n, lambda.pores));
			print(paste0("Pores: ", nPores));
			idPores = sample(px, nPores);
			m[idPores] = 0;
		} else {
			pores = rpores(data.frame(hasH2 = hasH2, hasV2 = hasV2), lambda.pores);
		
			pxy = lapply(pores, function(x) {
				hasHV = attr(x, "f");
				xs = xs[x$id]; xe = xe[x$id]; ys = ys[x$id]; ye = ye[x$id];
				# x$Total: is NOT perfect;
				xm = round((xs * (x$Total + 1 - x$seq) + x$seq * xe)/ (x$Total + 1));
				ym = round((ys * (x$Total + 1 - x$seq) + x$seq * ye)/ (x$Total + 1));
				# xdir = xdir.r[x$id]; ydir = ydir.r[id];
				### Categories: OX = 1 & 2; OY = 3 & 4;
				if(all(hasHV[1,])) {
					print("Both")
					p = xm; # valid for both Cat: 1 & 2;
					p[x$cat == 3] = xs[x$cat == 3];
					p[x$cat == 4] = xe[x$cat == 4];
					p = p * dim[VD];
					p[x$cat >= 3] = p[x$cat >= 3] + ym[x$cat >= 3];
					p[x$cat == 1] = p[x$cat == 1] + ys[x$cat == 1];
					p[x$cat == 2] = p[x$cat == 2] + ye[x$cat == 2];
				} else if(hasHV$hasH2[1]) {
					print("H2")
					p = xm; # valid for both Cat: 1 & 2;
					p = p * dim[VD];
					p[x$cat == 1] = p[x$cat == 1] + ys[x$cat == 1];
					p[x$cat == 2] = p[x$cat == 2] + ye[x$cat == 2];
				} else if(hasHV$hasV2[1]) {
					p = ifelse(x$cat == 1, xs, xe);
					p = p * dim[VD] + ym;
				} else {
					# rectangles are in corners;
					# pores added to H1-stub;
					p = xm * dim[VD] + ifelse(ydir.r[x$id] > 0, ys, ye);
				}
				return(p);
			})
			pxy = sort(unique(unlist(pxy)));
			m[pxy] = 0; # set pores
			print(paste("Unique pores: ", length(pxy), collapse=""));
			return(m);
		### TODO: correct number of pores!
		}
	}
	invisible(m);
}
rpores = function(x, lambda) {
	n = nrow(x);
	nPores = rpois(n, lambda);
	hasPores = (nPores > 0);
	nPores = nPores[hasPores];
	### Categories
	# - using columns present in x;
	pores.df  = cbind(x[hasPores,], nPores = nPores);
	pores.cat = rpores.cat(pores.df);
	# by Category
	pores.df$id = seq(n)[hasPores];
	prle = split.cat(pores.df);
	# pl = list(pores=pores.df, hasPores=hasPores, cat=pores.cat, prle=prle);
	pl = lapply(seq(length(prle)), function(id) {
		x.df = prle[[id]];
		ids = rep(x.df$id, x.df$nPores);
		Total = rep(x.df$nPores, x.df$nPores);
		pores.df = data.frame(id=ids, cat = pores.cat$id.cat[[id]], Total=Total);
		p.seq = tapply.seq(pores.df[,c("id", "cat")]);
		pores.df$seq = p.seq;
		attr(pores.df, "f") = attr(x.df, "f");
		return(pores.df);
	})
	print(str(pl)) # Debug
	invisible(pl);
}
rpores.cat = function(pores.df) {
	# nPores per each Category:
	pores.total = aggregate(nPores ~ ., pores.df, sum);
	len = nrow(pores.total); # no. of categories;
	p.cat = data.frame(Any=TRUE, pores.total[ ! names(pores.total) %in% "nPores"]);
	p.cat.m = as.matrix(p.cat);
	id.cat = seq(ncol(p.cat.m)); # the categories;
	rp.cat = function(id) {
		sample(id.cat[p.cat.m[id,]], pores.total$nPores[id], replace=TRUE);
	}
	pores.l = list(p.cat=p.cat, id.cat=lapply(seq(len), rp.cat));
	return(pores.l);
}
split.cat = function(x, var.names=c("id", "nPores"), aggr.var=var.names[1]) {
	names.p = names(x);
	names.p = names.p[ ! names.p %in% var.names];
	# only in R 4.1.0!
	# frml = reformulate(names.p)
	prle = split(x[, var.names], f=x[ , names.p], drop=TRUE)
	var1 = var.names[1];
	aggr = aggregate(
		formula(paste0(aggr.var, "~.")),
		x[,c(aggr.var, names.p)], length)
	names(aggr)[length(names(aggr))] = "len";
	for(id in seq(length(prle))) {
		attr(prle[[id]], "f") = aggr[id,];
	}
	prle;
}
tapply.seq = function(x, id1=1, FUN=seq_along) {
	ave(x[,id1], x, FUN=FUN);
}

TEST = FALSE;
if(TEST) {
	### TODO: multiple pores by category;
	# m = rrect.gen(120, c(40, 200), c(6, 20), c(6, 16), lambda=3, aspect.fixed=2)
	m = rrect.gen(120, c(40, 200), c(6, 20), c(6, 16), lambda=3)
	plot.rs(m)

	m.fl = flood.all(m)
	plot.rs(m.fl)
}


###############
### Geo-Physics

mod = function(K, ...) {
	UseMethod("mod");
}

### Bulk Modulus
mod.bulk = function(phi, K) {
	# Gassman's eq:
	# Ksat/(Kmin - Ksat) = Kdry/(Kmin - Kdry) + Kfl/(phi*(Kmin - Kfl))
	# (K[mineral], K[dry], K[fluid])
	if(length(K) < 3) {
		# Kfl: use Batzle & Wang's eqs;
	}
	dK = (K[1] - K[2:3]);
	Ks = K[2]/dK[2] + K[3]/(dK[3] * phi);
	Ksat = K[1] * Ks / (Ks + 1);
	return(Ksat);
}
mod.dry.bulk = function(phi, K) {
	# Kdry = (Ksat*(phi*Kmin/Kfl + 1 - phi) - Kmin) /
	#        (phi*Kmin/Kfl + Ksat/Kmin - 1 - phi);
	# Ksat[initial] = rho * (vp^2 - 4/3*vs^2);
}
mod.fluid.bulk = function(K, p) {
	# mixture of oil/fluids: p = proportion;
	if(length(K) - length(p) == 1) p = c(p, 1 - sum(p));
	1 / sum(p/K);
}
mod.min.bulk = function(type=c("quartz", "calcite", "dolomite", "muscovite",
		"feldspar", "albite", "halite", "anhydrite", "pyrite", "siderite")) {
	if(missing(type)) stop("Mineral name needed!")
	type = pmatch(type, formals(mod.min.bulk)$type[-1]);
	if(is.na(type)) stop("Mineral name not yet available!")
	
	K = c(36.6, 76.8, 94.9, 61.5, 75.6, 59.5, # albite: simulated 55;
		24.8, 56.1, 147.4, 123.7)
	return(K[type]);
	# albite:
	# https://www.ceramics-silikaty.cz/2015/pdf/2015_04_326.pdf
}


################
################

### Examples:
# - moved to file:
#   Percolation.Examples.R;