(define-cproc crypt-gensalt-ra (prefix::<const-cstring>
                                 count::<ulong>
                                 randomsrc::<u8vector>)
   (let* ([c::char* (crypt_gensalt_ra prefix count
                                      (cast (const char*)
                                            (SCM_U8VECTOR_ELEMENTS randomsrc))
                                      (SCM_U8VECTOR_SIZE randomsrc))])
     (when (== c NULL) (Scm_SysError "crypt_gensalt_ra failed"))
     (let* ([r (SCM_MAKE_STR_COPYING c)])
       (free c)
       (return r))))
