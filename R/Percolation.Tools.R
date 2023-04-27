########################
###
### Leonard Mada
### [the one and only]
###
### Percolation
### Basic Helper Functions
###
### draft v.0.4d


# column tail: last n columns;
tail.m = function(m, n=30, print=FALSE) {
	len = dim(m)[2];
	m = m[ , seq(max(1, len - n), len)]
	if(print) return(m);
	invisible(m);
}
reset.m = function(m, id, val=0) {
	if(missing(id)) {
		m[m > 0] = val;
	} else if(id > 0) {
		m[m == id] = val;
	} else {
		m[(m > 0) & (m != id)] = val;
	}
	invisible(m)
}
revcol = function(m) {
	warning("Deprecated!");
	# rev.col can be searched using: methods(rev);
	return(rev.col(m));
}
rev.col = function(m) {
	nc = ncol(m);
	if(nc == 0) return(m);
	m = m[, rev(seq(nc))];
	invisible(m);
}
clean.percol = function(m, val=0) {
	# removes the non-percolating paths;
	pids = unique(m[,ncol(m)]);
	pids = pids[pids > 0];
	ids  = unique(m);
	ids  = ids[(ids > 0) & ! (ids %in% pids)]
	m[m %in% ids] = val;
	invisible(m);
}
shuffle.colors = function(m) {
	m2 = array(0, dim(m))
	m2[m < 0] = m[m < 0]
	vals = unique(as.vector(m))
	vals = vals[vals > 0];
	vnew = sample(vals, length(vals));
	id = 1
	for(val in vals) {
		m2[m == val] = vnew[id];
		id = id + 1;
	}
	invisible(m2)
}
which.percol = function(m) {
	nc = ncol(m);
	id = unique(m[, nc]);
	id = id[id > 0];
	return(id);
}
as.logical.percol = function(m, percolates=TRUE) {
	id = which.percol(m);
	if( ! percolates) {
		tmp = unique(m[, 1]);
		tmp = tmp[tmp > 0];
		isPercol = tmp %in% id;
		id = tmp[ ! isPercol];
	}
	isE = m %in% id;
	dim(isE) = dim(m);
	invisible(isE);
}

### Extract id of Path:
# TODO: new name which.max.outflow;
# - path which percolates and has largest outflow;
max.id = function(m, fail=FALSE) {
	out.m = m[, ncol(m)];
	out = table(out.m[out.m > 0])
	if(dim(out) == 0) {
		print("NO percolation!");
		if(fail) return(NA);
		out = table(m[m > 0]);
	}
	# matches only the 1st path with max outflow;
	id = match(max(out), out);
	id = as.integer(names(out)[id])
	return(id)
}

### Log Transform
norm.flux = function(m, add=0) {
	m[m > 0] = abs(log(add + m[m > 0]))
	invisible(m)
}

select.subgrid = function(x, id, pad.val = -1) {
	nrAll = nrow(x);
	nrStart = 0;
	for(nr in seq(nrAll)) {
		if(any(x[nr,] == id)) {
			nrStart = nr; break;
		}
	}
	if(nrStart == 0) return(0);
	#
	nrEnd = 0;
	for(nr in rev(seq(nrAll))) {
		if(any(x[nr,] == id)) {
			nrEnd = nr; break;
		}
	}
	#
	if(is.null(pad.val)) return(x[seq(nrStart, nrEnd), ]);
	#
	nrStart0 = nrStart; nrEnd0 = nrEnd;
	if(nrStart == 1) {
		padStart = TRUE;
	} else {
		padStart = FALSE;
		nrStart = nrStart - 1;
	}
	if(nrEnd == nr) {
		padEnd = TRUE;
	} else {
		padEnd = FALSE;
		nrEnd = nrEnd + 1;
	}
	m = x[seq(nrStart, nrEnd), ];
	#
	nc = ncol(x);
	if(padStart) m = rbind(rep(pad.val, nc), m);
	if(padEnd) m = rbind(m, rep(pad.val, nc));
	nrSel = nrow(m);
	pad = rep(pad.val, nrSel);
	m = cbind(pad, m, pad);
	attr(m, "pad") = c(padStart, padEnd);
	attr(m, "nr") = c(nrStart0, nrEnd0);
	return(m);
}
expand.channel = function(m, d=3, d0=1) {
	nr = nrow(m);
	id.bl = seq(1, nr, by = d0 + 1);
	last  = length(id.bl);
	id.bn = id.bl[last];
	id.bl = id.bl[ - last];
	id.ch = id.bl + 1;
	id.ch = rep(id.ch, each = d);
	id.ch = matrix(id.ch, nrow = d);
	id = rbind(id.bl, id.ch);
	id = as.vector(id);
	id = c(id, id.bn); # last delimiting wall;
	mr = m[id,];
	invisible(mr);
}
