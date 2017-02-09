/* filledcontour(x, y, z, levels, col) */
SEXP C_filledcontour(SEXP args)
{
    SEXP sx, sy, sz, sc, scol;
    double *x, *y, *z, *c;
    rcolor *col;
    int i, j, k, npt, nx, ny, nc, ncol, colsave, xpdsave;
    double px[8], py[8], pz[8];
    pGEDevDesc dd = GEcurrentDevice();

    GCheckState(dd);

    PrintDefaults(); /* prepare for labelformat */

    args = CDR(args);
    sx = PROTECT(coerceVector(CAR(args), REALSXP));
    nx = LENGTH(sx);
    args = CDR(args);

    sy = PROTECT(coerceVector(CAR(args), REALSXP));
    ny = LENGTH(sy);
    args = CDR(args);
    if (nx < 2 || ny < 2) error(_("insufficient 'x' or 'y' values"));

    // do it this way as coerceVector can lose dims, e.g. for a list matrix
    sz = CAR(args);
    if (nrows(sz) != nx || ncols(sz) != ny) error(_("dimension mismatch"));
    sz = PROTECT(coerceVector(sz, REALSXP));
    args = CDR(args);

    sc = PROTECT(coerceVector(CAR(args), REALSXP)); /* levels */
    nc = length(sc);
    args = CDR(args);

    if (nc < 1) error(_("no contour values"));

    PROTECT(scol = FixupCol(CAR(args), R_TRANWHITE));
    ncol = length(scol);

    /* Shorthand Pointers */

    x = REAL(sx);
    y = REAL(sy);
    z = REAL(sz);
    c = REAL(sc);
    col = (rcolor *) INTEGER(scol);

    /* Check of grid coordinates */
    /* We want them to all be finite */
    /* and in strictly ascending order */

    if (nx < 1 || ny < 1) goto badxy;
    if (!R_FINITE(x[0])) goto badxy;
    if (!R_FINITE(y[0])) goto badxy;
    for (i = 1; i < nx; i++)
	if (!R_FINITE(x[i]) || x[i] <= x[i - 1]) goto badxy;
    for (j = 1; j < ny; j++)
	if (!R_FINITE(y[j]) || y[j] <= y[j - 1]) goto badxy;

    /* Check of the contour levels */

    if (!R_FINITE(c[0])) goto badlev;
    for (k = 1; k < nc; k++)
	if (!R_FINITE(c[k]) || c[k] <= c[k - 1]) goto badlev;

    colsave = gpptr(dd)->col;
    xpdsave = gpptr(dd)->xpd;
    /* override par("xpd") and force clipping to plot region */
    gpptr(dd)->xpd = 0;

    GMode(1, dd);

    for (i = 1; i < nx; i++) {
	for (j = 1; j < ny; j++) {
	    for (k = 1; k < nc ; k++) {
		FindPolygonVertices(c[k - 1], c[k],
				    x[i - 1], x[i],
				    y[j - 1], y[j],
				    z[i - 1 + (j - 1) * nx],
				    z[i + (j - 1) * nx],
				    z[i - 1 + j * nx],
				    z[i + j * nx],
				    px, py, pz, &npt);
		if (npt > 2)
		    GPolygon(npt, px, py, USER, col[(k-1) % ncol],
			     R_TRANWHITE, dd);
	    }
	}
    }
    GMode(0, dd);
    gpptr(dd)->col = colsave;
    gpptr(dd)->xpd = xpdsave;
    UNPROTECT(5);
    return R_NilValue;

 badxy:
    error(_("invalid x / y values or limits"));
 badlev:
    error(_("invalid contour levels: must be strictly increasing"));
    return R_NilValue;  /* never used; to keep -Wall happy */
}


/* FIXME - This could pretty easily be adapted to handle NA */
/* values on the grid.  Just search the diagonals for cutpoints */
/* instead of the cell sides.  Use the same switch idea as in */
/* contour above.  There are 5 cases to handle. */

static void
FindPolygonVertices(double low, double high,
		    double x1, double x2, double y1, double y2,
		    double z11, double z21, double z12, double z22,
		    double *x, double *y, double *z, int *npt)
{
    *npt = 0;
    FindCutPoints(low, high, x1,  y1,  z11, x2,  y1,  z21, x, y, z, npt);
    FindCutPoints(low, high, y1,  x2,  z21, y2,  x2,  z22, y, x, z, npt);
    FindCutPoints(low, high, x2,  y2,  z22, x1,  y2,  z12, x, y, z, npt);
    FindCutPoints(low, high, y2,  x1,  z12, y1,  x1,  z11, y, x, z, npt);
}

static void
FindCutPoints(double low, double high,
	      double x1, double y1, double z1,
	      double x2, double y2, double z2,
	      double *x, double *y, double *z,
	      int *npt)
{
    double c;

    if (z1 > z2 ) {
	if (z2 > high || z1 < low) return;
	if (z1 < high) {
	    x[*npt] = x1;
	    y[*npt] = y1;
	    z[*npt] = z1;
	    ++*npt;
	} else if (z1 == R_PosInf) {
	    x[*npt] = x2;
	    y[*npt] = y1;
	    z[*npt] = z2;
	    ++*npt;
	} else { /* z1 >= high, z2 in range */
	    c = (z1 - high) / (z1 - z2);
	    x[*npt] = x1 + c * (x2 - x1);
	    y[*npt] = y1;
	    z[*npt] = z1 + c * (z2 - z1);
	    ++*npt;
	}
	if (z2 == R_NegInf) {
	    x[*npt] = x1;
	    y[*npt] = y1;
	    z[*npt] = z1;
	    ++*npt;
	} else if (z2 <= low) { /* and z1 in range */
	    c = (z2 -low) / (z2 - z1);
	    x[*npt] = x2 - c * (x2 - x1);
	    y[*npt] = y1;
	    z[*npt] = z2 - c * (z2 - z1);
	    ++*npt;
	}//end
    
    
    } else if (z1 < z2) {
	if (z2 < low || z1 > high) return;
	if (z1 > low) {
	    x[*npt] = x1;
	    y[*npt] = y1;
	    z[*npt] = z1;
	    ++*npt;
	} else if (z1 == R_NegInf) {
	    x[*npt] = x2;
	    y[*npt] = y1;
	    z[*npt] = z2;;
	    ++*npt;
	} else { /* and z2 in range */
	    c = (z1 - low) / (z1 - z2);
	    x[*npt] = x1 + c * (x2 - x1);
	    y[*npt] = y1;
	    z[*npt] = z1 + c * (z2 - z1);
	    ++*npt;
	}// end/
    
	if (z2 < high) {
#ifdef OMIT
	    /* Don't repeat corner vertices */
	    x[*npt] = x2;
	    y[*npt] = y2;
	    z[*npt] = z2;
	    ++*npt;
#endif
	} else if (z2 == R_PosInf) {
	    x[*npt] = x1;
	    y[*npt] = y1;
	    z[*npt] = z1;
	    ++*npt;
	} else { /* z2 high, z1 in range */
	    c = (z2 - high) / (z2 - z1);
	    x[*npt] = x2 - c * (x2 - x1);
	    y[*npt] = y1;
	    z[*npt] = z2 - c * (z2 - z1);
	    ++*npt;
	}
    } else {
	if(low <= z1 && z1 <= high) {
	    x[*npt] = x1;
	    y[*npt] = y1;
	    z[*npt] = z1;
	    ++*npt;
#ifdef OMIT
	    /* Don't repeat corner vertices */
	    x[*npt] = x2;
	    y[*npt] = y2;
	    z[*npt] = z2;
	    ++*npt;
#endif
	}
    }
}
