/* -*-  Mode:C; c-basic-offset:4 -*- */

// ****************************************************************
// Misc.

// ----------------------------------------------------------------
// fprintHeaders()

extern
void
fprintHeaders (FILE *fpo, char *banner, char *inputFileName);

// ----------------------------------------------------------------
// fprintTrailers()

extern
void
fprintTrailers (FILE *fpo);

// ****************************************************************
// Area analysis

// ----------------------------------------------------------------
// Analyse area and print results

extern
void
analyseArea (FILE *fpo, ModuleGraph *mgp);

// ****************************************************************
// Path length analysis

// ----------------------------------------------------------------
// Build paths, analyse them, and print results

extern
void
analysePaths (FILE *fpo, ModuleGraph *mgp, int nPaths);

// ****************************************************************
// Net fanout analysis

// ----------------------------------------------------------------
// Analyse nets and print results

extern
void
analyseNets (FILE *fpo, ModuleGraph *mgp, int nFanouts);

// ****************************************************************
