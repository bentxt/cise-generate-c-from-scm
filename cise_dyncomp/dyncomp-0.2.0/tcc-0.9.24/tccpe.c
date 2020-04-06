/*
 *  TCCPE.C - PE file output for the Tiny C Compiler
 *
 *  Copyright (c) 2005-2007 grischka
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#ifdef TCC_TARGET_PE

#define ST_FN static
#define ST_DATA static
#define PUB_FN

#ifndef _WIN32
#define stricmp strcasecmp
#define strnicmp strncasecmp
#endif

#ifndef MAX_PATH
#define MAX_PATH 260
#endif

#define PE_MERGE_DATA

/* ----------------------------------------------------------- */
#ifndef IMAGE_NT_SIGNATURE
/* ----------------------------------------------------------- */
/* definitions below are from winnt.h */

typedef unsigned char BYTE;
typedef unsigned short WORD;
typedef unsigned long DWORD;

/* ----------------------------------------------------------- */
#endif /* ndef IMAGE_NT_SIGNATURE */
/* ----------------------------------------------------------- */

void error_noabort(const char *, ...);

#ifdef _WIN32
void dbg_printf (const char *fmt, ...)
{
    char buffer[4000];
    va_list arg;
    int x;
    va_start(arg, fmt);
    x = vsprintf (buffer, fmt, arg);
    strcpy(buffer+x, "\n");
    OutputDebugString(buffer);
}
#endif

/* --------------------------------------------*/

ST_FN const char* get_alt_symbol(char *buffer, const char *symbol)
{
    const char *p;
    p = strrchr(symbol, '@');
    if (p && isnum(p[1]) && symbol[0] == '_') { /* stdcall decor */
        strcpy(buffer, symbol+1)[p-symbol-1] = 0;
    } else if (symbol[0] != '_') { /* try non-ansi function */
        buffer[0] = '_', strcpy(buffer + 1, symbol);
    } else if (0 == memcmp(symbol, "__imp__", 7)) { /* mingw 2.0 */
        strcpy(buffer, symbol + 6);
    } else if (0 == memcmp(symbol, "_imp___", 7)) { /* mingw 3.7 */
        strcpy(buffer, symbol + 6);
    } else {
        return symbol;
    }
    return buffer;
}

#if defined _WIN32 || defined __CYGWIN__

#ifdef __CYGWIN__
# include <dlfcn.h>
# define LoadLibrary(s) dlopen(s, RTLD_NOW)
# define GetProcAddress(h,s) dlsym(h, s)
#endif

/* for the -run option: dynamically load symbol from dll */
void *resolve_sym(struct TCCState *s1, const char *symbol, int type)
{
    char buffer[100];
    void *hModule, *addr;
    HANDLE hProcess;
    HMODULE *lphModules;
    DWORD needed;
    TCHAR filename[1024];
    BOOL result = 0;
    int i = 0;
        
    hProcess = GetCurrentProcess();
    if (!EnumProcessModules(hProcess, NULL, 0, &needed)) {
      tcc_error("EnumProcessModules failed (1st call).\n");
    }
    lphModules = tcc_malloc(needed);
    if (!EnumProcessModules(hProcess, lphModules, needed, &needed)) {
      tcc_error("EnumProcessModules failed (2nd call).\n");
    }
        
    addr = NULL;
    for (i = 0; i < needed/sizeof(HMODULE); ++i) {
      GetModuleFileName(lphModules[i], filename, 1024);
      addr = GetProcAddress(lphModules[i], get_alt_symbol(buffer, symbol));
      if (addr != NULL) {
	break;
      }
      addr = GetProcAddress(lphModules[i], symbol);
      if (addr != NULL) {
	break;
      }
    }
    tcc_free(lphModules);
        
    return addr;
}
#endif

/* ------------------------------------------------------------- */
#endif /* def TCC_TARGET_PE */
/* ------------------------------------------------------------- */
