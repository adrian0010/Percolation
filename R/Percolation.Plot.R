########################
###
### Leonard Mada
### [the one and only]
###
### Percolation
### Graphical Functions
###
### draft v.0.4d


#######################
### Graphical Functions

# this file:
# source("Percolation.Plot.R")


### Raster

as.raster.percol = function(x, showVal=0, isBlue = NULL, rgb.cut = 160/255) {
	rs = toRaster(x, showVal=showVal, isBlue=isBlue, rgb.cut=rgb.cut);
	invisible(rs);
}
toRaster = function(m, showVal=0, isBlue = NULL, rgb.cut = 160/255) {
	rs.m = array(0, c(dim(m), 3));
	if( ! is.na(showVal)) {
		isZero = (m == showVal);
		doShow = TRUE;
	} else {
		doShow = FALSE;
	}

	### R
	layer.m = m;
	layer.m[m < 0] = 0
	val.max = max(layer.m);
	if(val.max > 0) layer.m = layer.m / val.max;
	if(doShow) layer.m[isZero] = 1;
	rs.m[,,1] = layer.m;

	### G
	layer.m = 1 - layer.m;
	layer.m[m <= 0] = 0
	if(doShow) layer.m[isZero] = 1;
	rs.m[,,2] = layer.m

	### B
	if(doShow) {
		layer.m = array(0, dim(m));
		layer.m[isZero] = 1;
		if( ! is.null(isBlue)) {
			addBlue = (rs.m[,,1] < rgb.cut) & isBlue;
			tmpBlue = m[addBlue];
			val.max = max(tmpBlue);
			if(val.max > 0) tmpBlue = tmpBlue / val.max;
			layer.m[addBlue] = tmpBlue;
		}
		rs.m[,,3] = layer.m;
	}

	rs.m = as.raster(rs.m)
	return(rs.m);
}
# addBlue = Matrix used for blue bin;
plot.rs = function(m, main, mar, line=0.5, addBlue = NULL) {
	if( ! missing(main) ) hasTitle = TRUE else hasTitle = FALSE;
	if(missing(mar)) mar = c(0,0, if(hasTitle) 2 else 0, 0) + 0.1;
	type = match(class(m), c("raster", "matrix"));
	if(all(is.na(type))) stop("Data NOT supported!")
	if(any(type == 2)) {
		if( ! is.null(addBlue)) {
			# TODO: better concept to handle split images;
			rsBlue = as.logical.percol(addBlue);
			split = attr(m, "split");
			if( ! is.null(split)) {
				rsBlue = do.call(split.rs, c(list(rsBlue), split));
			}
			m = toRaster(m, isBlue = rsBlue);
		} else {
			m = toRaster(m);
		}
	}
	old.par = par(mar=mar);
		plot(m);
		if(hasTitle) mtext(main, line=line)
	par(old.par);
	invisible();
}
split.rs = function(m, n=5, from=1, max.len=5, w=10) {
	# w = width between displayed fragments;
	# n = number of fragments;
	nr.tot = round(nrow(m) / n);
	frg.tot = ceiling(nrow(m) / nr.tot);
	# TODO: nrow(m) %% nr.tot > 0;
	if(from == 0) from = 1;
	if(from > 0) {
		frg.to = min(frg.tot, from + max.len);
	} else {
		# Negative = tail;
		from = max(1, frg.tot + 1 + from - max.len);
		frg.to = min(frg.tot, from + max.len);
	}
	m0 = matrix(0, ncol=w, nrow=nr.tot); # spacer
	m2 = array(0, c(nr.tot, 0));
	for(frg in from:frg.to) {
		r.start = (frg - 1) * nr.tot + 1;
		r.end   = r.start + nr.tot - 1;
		if(r.end > nrow(m)) {
			m1 = matrix(0, ncol=ncol(m), nrow = r.end - nrow(m))
			m2 = cbind(m2, rbind(m[r.start:nrow(m),], m1), m0)
		} else {
			m2 = cbind(m2, m[r.start:r.end,], m0)
		}
	}
	# TODO: Case from < 0; (but may already work)
	attr(m2, "split") = list(n=n, from=from, max.len=max.len, w=w);
	invisible(m2);
}

# Plot border around Channel
plot.surface = function(m, id, col = "#1624C0") {
	csf = as.surface.contact(m, id = id);
	img = toRaster(m);
	img[csf] = col;
	plot.rs(img);
}

plot.minCut = function(m, id, col = "#1624C0", col.part = "#F0F000") {
	npos = minCut(m, id)
	img = toRaster(m);
	img[npos$neighbors] = col;
	if( ! is.null(col.part)){
		img[npos$part] = col.part;
	}
	plot.rs(img);
}
