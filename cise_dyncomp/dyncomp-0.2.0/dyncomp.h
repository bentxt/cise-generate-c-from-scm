/*
 * dyncomp.h
 */

/* Prologue */
#ifndef GAUCHE_DYNCOMP_H
#define GAUCHE_DYNCOMP_H

#include <gauche.h>
#include <gauche/extend.h>
#include <tcc-0.9.24/libtcc.h>
#if defined(WIN32)
#include <windows.h>
#else
#include <dlfcn.h>
#endif
#include "sys_inc_paths.h"

SCM_DECL_BEGIN

extern ScmObj ClearCode(ScmObj mod);
extern ScmObj AddIncludePath(ScmObj mod, const char *path);
extern ScmObj CompileCCode(ScmObj mod, const char *src_code);
extern ScmObj MakeSubr(ScmObj mod, ScmObj info, int req, int opt, const char *sym);
extern ScmObj LoadDynamicLibrary(const char *filename);

/* Epilogue */
SCM_DECL_END

#endif  /* GAUCHE_DYNCOMP_H */
