/*
 * dyncomp.c
 */

#include "dyncomp.h"

static ScmClass *TCCStateClass;
static ScmSymbol *TCCStateKey;
static ScmSymbol *SubrListKey;

static void tcc_state_print(ScmObj state_obj, ScmPort *sink, ScmWriteContext *mode)
{
    Scm_Printf(sink, "#<tcc-state %p @%p>",
               SCM_FOREIGN_POINTER_REF(TCCState*, state_obj), state_obj);
}

static void error_func(void *opaque, const char *msg)
{
    Scm_Error("C code compile error: %s", msg);
}

static TCCState* FindTCCState(ScmObj mod)
{
    TCCState *state = NULL;
    ScmObj state_obj = Scm_GlobalVariableRef(SCM_MODULE(mod), TCCStateKey, 0);

    if (SCM_UNBOUNDP(state_obj)) {
        int i;
        const char *sys_inc_paths[] = SYS_INCLUDE_PATHS;
        
        state = tcc_new();
        tcc_set_error_func(state, NULL, error_func);
        for (i = 0; i < sizeof(sys_inc_paths)/sizeof(char*); ++i) {
            tcc_add_sysinclude_path(state, sys_inc_paths[i]);
        }
        tcc_compile_string(state, "#include <dyncomp_helper.h>");
        Scm_Define(SCM_MODULE(mod), TCCStateKey, Scm_MakeForeignPointer(TCCStateClass, state));
    } else {
        state = SCM_FOREIGN_POINTER_REF(TCCState*, state_obj);
    }

    return state;
}

static void PurgeTCCState(ScmObj mod)
{
    ScmObj state_obj = Scm_GlobalVariableRef(SCM_MODULE(mod), TCCStateKey, 0);

    if (! SCM_UNBOUNDP(state_obj)) {
        TCCState *state = SCM_FOREIGN_POINTER_REF(TCCState*, state_obj);
        tcc_delete(state);
        
        Scm_Define(SCM_MODULE(mod), TCCStateKey, SCM_UNBOUND);
    }
}

static void RegisterSubr(ScmObj mod, ScmSubr *subr, const char *sym)
{
    ScmObj subr_list = Scm_GlobalVariableRef(SCM_MODULE(mod), SubrListKey, 0);

    if (SCM_UNBOUNDP(subr_list)) {
        subr_list = SCM_NIL;
    }
    subr_list = Scm_Cons(Scm_Cons(SCM_MAKE_STR_IMMUTABLE(sym), SCM_OBJ(subr)), subr_list);
    Scm_Define(SCM_MODULE(mod), SubrListKey, subr_list);
}

static void RelocateSubr(ScmObj mod)
{
    TCCState *state = FindTCCState(mod);
    ScmObj subr_list = Scm_GlobalVariableRef(SCM_MODULE(mod), SubrListKey, 0);
    ScmObj subr_rest;

    if (SCM_UNBOUNDP(subr_list)) {
        /* DO NOTHING */
        return;
    }

    SCM_FOR_EACH(subr_rest, subr_list) {
        ScmObj pair = SCM_CAR(subr_rest);
        ScmObj name = SCM_CAR(pair);
        ScmObj subr = SCM_CDR(pair);
        void *func_ptr;

        if (tcc_get_symbol(state, (unsigned long *)&func_ptr, Scm_GetStringConst(SCM_STRING(name))) != 0) {
            Scm_Error("symbol %S is not found. may be bug.", name);
        }
        SCM_SUBR_FUNC(subr) = func_ptr;
    }
}

static ScmObj InvalidSubr(ScmObj *SCP_FP, int SCM_ARGCNT, void *data_)
{
    Scm_Error("This #<subr> is invalid");

    SCM_RETURN(SCM_UNDEFINED);
}

ScmObj ClearCode(ScmObj mod)
{
    ScmObj subr_list = Scm_GlobalVariableRef(SCM_MODULE(mod), SubrListKey, 0);
    ScmObj subr_rest;

    if (! SCM_UNBOUNDP(subr_list)) {
        SCM_FOR_EACH(subr_rest, subr_list) {
            ScmObj pair = SCM_CAR(subr_rest);
            ScmObj subr = SCM_CDR(pair);

            SCM_SUBR_FUNC(subr) = InvalidSubr;
        }

        Scm_Define(SCM_MODULE(mod), SubrListKey, SCM_UNBOUND);
    }
    PurgeTCCState(mod);

    SCM_RETURN(SCM_UNDEFINED);
}

ScmObj AddIncludePath(ScmObj mod, const char *path)
{
    TCCState *state = FindTCCState(mod);

    tcc_add_include_path(state, path);

    SCM_RETURN(SCM_UNDEFINED);
}

ScmObj CompileCCode(ScmObj mod, const char *src_code)
{
    TCCState *state = FindTCCState(mod);
    tcc_compile_string(state, src_code);
    tcc_relocate(state);
    RelocateSubr(mod);

    SCM_RETURN(SCM_UNDEFINED);
}

ScmObj MakeSubr(ScmObj mod, ScmObj info, int req, int opt, const char *sym)
{
    TCCState *state = FindTCCState(mod);
    ScmSubr *subr = SCM_NEW(ScmSubr);
    void *func_ptr;

    if (tcc_get_symbol(state, (unsigned long *)&func_ptr, sym) != 0) {
        Scm_Error("symbol %S is not found.", SCM_MAKE_STR_IMMUTABLE(sym));
    }
    
    SCM_SET_CLASS(subr, &Scm_ProcedureClass);
    SCM_PROCEDURE_INIT(subr, req, opt, SCM_PROC_SUBR, info);
    SCM_SUBR_FUNC(subr) = func_ptr;
    SCM_SUBR_DATA(subr) = NULL;

    RegisterSubr(mod, subr, sym);
    
    SCM_RETURN(SCM_OBJ(subr));
}

ScmObj LoadDynamicLibrary(const char *filename)
{
#if defined(__MINGW32__)
    HMODULE handle = LoadLibrary(SCM_MBS2WCS(filename));
#else
    void *handle = dlopen(filename, RTLD_NOW | RTLD_GLOBAL);
#endif
    if (handle == NULL) {
        Scm_Error("Can't load %S", filename);
    }
    SCM_RETURN(SCM_UNDEFINED);
}
    
/*
 * Module initialization function.
 */
extern void Scm_Init_dyncomplib(ScmModule*);

void Scm_Init_dyncomp(void)
{
    ScmModule *mod;

    /* Register this DSO to Gauche */
    SCM_INIT_EXTENSION(dyncomp);

    /* Create the module if it doesn't exist yet. */
    mod = SCM_MODULE(SCM_FIND_MODULE("dyncomp", TRUE));

    TCCStateClass = Scm_MakeForeignPointerClass(mod, "<tcc-state>",
                                                tcc_state_print, NULL,
                                                SCM_FOREIGN_POINTER_MAP_NULL);
    TCCStateKey = SCM_SYMBOL(Scm_Gensym(SCM_STRING(SCM_MAKE_STR_IMMUTABLE("dyncomp"))));
    SubrListKey = SCM_SYMBOL(Scm_Gensym(SCM_STRING(SCM_MAKE_STR_IMMUTABLE("dyncomp"))));

    /* Register stub-generated procedures */
    Scm_Init_dyncomplib(mod);
}
