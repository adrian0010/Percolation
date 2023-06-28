// Student: Adrian Ivan
// Candidate BSc CS 2023
// West University, Timisoara

// Supervisors:
// Prof. Dr. Daniela Zaharie
// Dr. med. Leonard Mada (Syonic SRL)

// URL: https://github.com/adrian0010/Percolation

// Based on Previous Projects (2020-2022)

NumericVector heightChannel(IntegerVector xStart, NumericVector m,
		const int nRows, const int nCols,
		const bool idealPores = false, const bool verbose = false) {
// Ideal Pores: considered to NOT communicate with adjacent pores;
// nRows, nCols = dimensions of the padded-Matrix;
	// m = transposed Matrix, with Rows & Cols padded;
	// xStart = Pore entry {nr, nc}
	std::vector<int> xyPoreEntry; // Pore offset {nr, nc}
	for(int i = 0; i < xStart.length(); i++) {
		xyPoreEntry.push_back(1);
		xyPoreEntry.push_back((int) xStart[i] - 1); // Column offset
	}
	// Start
	std::vector<int> xyNewPores; // Pore entry {x, y}
	double idLevel = 1;
	if(verbose)
		Rcout << "Starting: Inflows = " << xyPoreEntry.size() << ";\\n";
	while(true) {
		for(size_t nv = 0; nv < xyPoreEntry.size(); ) {
			int nr0 = xyPoreEntry[nv]; nv++;
			int nc0 = xyPoreEntry[nv]; nv++;
			int offsetCol = nc0*nRows;
			int valE = m[offsetCol + nr0];
			if(valE != 0) continue;
			// Sub-Channel:
			int nStart = nr0;
			m[offsetCol + nStart] = idLevel;
			while(nStart > 0) {
				if(m[offsetCol + nStart - 1] == 0)  {
					nStart --;
					m[offsetCol + nStart] = idLevel;
				} else break;
			}
			int nEnd = nr0;
			while(nEnd < nRows) {
				if(m[offsetCol + nEnd + 1] == 0)  {
					nEnd ++;
					m[offsetCol + nEnd] = idLevel;
				} else break;
			}
			// search Pores:
			// Previous Column
			int offPrevW  = (nc0 - 1)*nRows;
			int offPrevCh = (nc0 - 2)*nRows;
			int npos = nStart;
			while(npos <= nEnd) {
				int nposNew = offPrevW + npos;
				if(m[nposNew] == 0)  {
					m[nposNew] = idLevel;
					// to check or not to check?
					if(m[offPrevCh + npos] == 0)  {
						// if(verbose) Rcout << "  Up = " << (nc0 - 2) << ",";
						xyNewPores.push_back(npos);
						xyNewPores.push_back(nc0 - 2);
					}
				}
				npos ++;
			}
			if( ! idealPores) {
				int nposNew = offPrevW + nStart - 1;
				if((m[nposNew] == 0) && (m[nposNew + 1] > 0)) {
					m[nposNew] = idLevel;
					xyNewPores.push_back(npos);
					xyNewPores.push_back(nc0 - 2);
				}
				nposNew = offPrevW + nEnd + 1;
				if((m[nposNew] == 0) && (m[nposNew - 1] > 0)) {
					m[nposNew] = idLevel;
					xyNewPores.push_back(npos);
					xyNewPores.push_back(nc0 - 2);
				}
			}
			// Next Column
			int offNextW  = (nc0 + 1)*nRows;
			int offNextCh = (nc0 + 2)*nRows;
			npos = nStart;
			while(npos <= nEnd) {
				int nposNew = offNextW + npos;
				if(m[nposNew] == 0)  {
					m[nposNew] = idLevel;
					if(m[offNextCh + npos] == 0)  {
						xyNewPores.push_back(npos);
						xyNewPores.push_back(nc0 + 2);
					}
				}
				npos ++;
			}
			if( ! idealPores) {
				int nposNew = offNextW + nStart - 1;
				if((m[nposNew] == 0) && (m[nposNew + 1] > 0)) {
					m[nposNew] = idLevel;
					xyNewPores.push_back(npos);
					xyNewPores.push_back(nc0 + 2);
				}
				nposNew = offNextW + nEnd + 1;
				if((m[nposNew] == 0) && (m[nposNew - 1] > 0)) {
					m[nposNew] = idLevel;
					xyNewPores.push_back(npos);
					xyNewPores.push_back(nc0 + 2);
				}
			}
		}
		xyPoreEntry.clear();
		if(xyNewPores.size() == 0) break;
		xyPoreEntry.insert(xyPoreEntry.begin(), xyNewPores.begin(), xyNewPores.end());
		xyNewPores.clear();
		idLevel ++;
	}
	if(verbose) Rcout << "\\n";
	return m;
}

