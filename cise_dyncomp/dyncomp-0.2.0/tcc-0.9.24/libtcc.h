#ifndef LIBTCC_H
#define LIBTCC_H

#ifdef __cplusplus
extern "C" {
#endif

struct TCCState;

typedef struct TCCState TCCState;

/* create a new TCC compilation context */
TCCState *tcc_new(void);

/* free a TCC compilation context */
void tcc_delete(TCCState *s);

/* add debug information in the generated code */
void tcc_enable_debug(TCCState *s);

/* set error/warning display callback */
void tcc_set_error_func(TCCState *s, void *error_opaque,
                        void (*error_func)(void *opaque, const char *msg));

/* set/reset a warning */
int tcc_set_warning(TCCState *s, const char *warning_name, int value);

/*****************************/
/* preprocessor */

/* add include path */
int tcc_add_include_path(TCCState *s, const char *pathname);

/* add in system include path */
int tcc_add_sysinclude_path(TCCState *s, const char *pathname);

/* define preprocessor symbol 'sym'. Can put optional value */
void tcc_define_symbol(TCCState *s, const char *sym, const char *value);

/* undefine preprocess symbol 'sym' */
void tcc_undefine_symbol(TCCState *s, const char *sym);

/*****************************/
/* compiling */

/* compile a string containing a C source. Return non zero if
   error. */
int tcc_compile_string(TCCState *s, const char *buf);

/*****************************/
/* linking commands */

/* add a symbol to the compiled program */
int tcc_add_symbol(TCCState *s, const char *name, unsigned long val);

/* do all relocations (needed before using tcc_get_symbol()). Return
   non zero if link error. */
int tcc_relocate(TCCState *s);

/* return symbol value. return 0 if OK, -1 if symbol not found */
int tcc_get_symbol(TCCState *s, unsigned long *pval, const char *name);

#ifdef __cplusplus
}
#endif

#endif
