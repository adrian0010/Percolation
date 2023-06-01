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
	id1 = unique(m[, 1]);
	id2 = unique(m[, nc]);
	id2 = id2[id2 > 0];
	idP = intersect(id2, id1);
	return(idP);
}

# Which Percolates or all channels
which.percolates.orAny = function(x) {
	id = which.percol(x)
	if(length(id) == 0) {
		id = unique(x[,1]);
		id = id[id > 0];
	}
	return(id);
}

which.channels = function(x) {
	nc = ncol(x);	# numar coloane
	id1 = unique(x[, 1]);
	id1 = id1[id1 > 0];
	id3 = unique(x[, nc]);
	id3 = id3[id3 > 0];
	id2 = intersect(id1, id3);
	if(length(id2) > 0) {
		id1 = setdiff(id1, id2);
		id3 = setdiff(id3, id2);
	}
	result = list(L = id1, P = id2, R = id3);
	return (result);
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

as.df.id = function(x) {
	if(length(x$L) > 0) {
		idf = data.frame(Group = "Left", id = x$L);
	} else {
		idf = data.frame(Group = character(0), id = numeric(0));
	}
	if(length(x$P) > 0) {
		idf = rbind(idf, data.frame(Group = "Percolating", id = x$P));
	}
	if(length(x$R) > 0) {
		idf = rbind(idf, data.frame(Group = "Right", id = x$R));
	}
	return(idf);
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

as.graph.percol = function(m, id = 1) {
	rows = nrow(m)
	cols = ncol(m)
	v = integer(0)
	for (nc in seq(cols - 1)){
		for (nr in seq(rows - 1)){
			if(m[nr, nc] != id) next;
			npos = (nc - 1)*rows + nr 
			if(m[nr + 1, nc] == id){
				v = c(v, npos, npos + 1);
			}
			if(m[nr, nc + 1] == id){
				v = c(v, npos, npos + rows);
			}

		}
	}
	for(nc in seq(cols - 1)){
		if(m[rows, nc] != id) next;
		if(m[rows, nc + 1] == id){
			npos = (nc - 1)*rows + nr
			v = c(v, npos, npos + rows);
		}
	}
	for(nr in seq(rows - 1)){
		if(m[nr, cols] != id) next;
		if(m[nr+1, cols] == id){
			npos = (nc - 1)*rows + nr
			v = c(v, npos, npos + 1);
		}
	}
	# v = matrix(v, ncol = 2, byrows = TRUE)
	idMax = max(v)
	nStart = which(m[,1] == id);
	nStart = rbind(idMax + 1, nStart);
	v = c(nStart, v)

	nEnd = which(m[,cols] == id) + (cols - 1)*rows;
	nEnd = rbind(nEnd, idMax + 2);
	v = c(v, nEnd);

	graph = make_undirected_graph(v, n = idMax + 2)

	attr(graph, "entry") = c(idMax + 1, idMax+2);

	return(invisible(graph))

}
