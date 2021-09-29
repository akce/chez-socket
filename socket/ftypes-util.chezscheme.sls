;; ftypes util functions for Chez scheme.
;; Written by Jerry 2019-2021.
;; SPDX-License-Identifier: Unlicense

;; Use the chezscheme reader due to #% primitive access.
#!chezscheme
(library (socket ftypes-util)
  (export
   u8 u8* u8**
   alloc
   bzero
   c-function c-default-function c-enum c-bitmap
   call-procedure/errno
   locate-library-object
   ;; byte/string array handling functions.
   u8*->bv bv->u8*
   u8*->string u8**->string-list u8**->strings/free
   string->u8* string-list->u8**
   free-u8**
   ;; Chez scheme re-exports. Saves client code from having to import these themselves.
   define-ftype foreign-alloc foreign-free foreign-ref
   ftype-pointer-address ftype-&ref ftype-ref ftype-set! ftype-sizeof load-shared-object)
  (import
   (chezscheme))

  (define-ftype u8 unsigned-8)
  (define-ftype u8* (* u8))
  (define-ftype u8** (* u8*))

  ;; [syntax] (alloc ((var varptr type)) ...)
  (define-syntax alloc
    (syntax-rules ()
      [(_ ((var type) ...) first rest ...)
       (let ([var (foreign-alloc (ftype-sizeof type))] ...)
         (let ([r (begin first rest ...)])
           (foreign-free var) ...
           r))]
      [(_ ((var varptr type) ...) first rest ...)
       (let ([var (foreign-alloc (ftype-sizeof type))] ...)
         (let ([varptr (make-ftype-pointer type var)] ...)
           (let ([r (begin first rest ...)])
             ;; make-ftype-pointer implicitly locks var, so manually unlock before free.
             (unlock-object var) ...
             (foreign-free var) ...
             r)))]
      [(_ ((var varptr type num) ...) first rest ...)
       ;; Ensure num is at least 1, that's a requirement of foreign-alloc.
       (let ([var (foreign-alloc (* (if (= num 0) 1 num) (ftype-sizeof type)))] ...)
         (let ([varptr (make-ftype-pointer type var)] ...)
           (let ([r (begin first rest ...)])
             ;; make-ftype-pointer implicitly locks var, so manually unlock before free.
             (unlock-object var) ...
             (foreign-free var) ...
             r)))]))

  ;; [proc] bzero: clears a chunk of foreign memory.
  ;; [returns]: none
  ;; ptr must be a pointer address as returned by foreign-alloc (or similar).
  ;; sizeof is the memory size in bytes.
  (define bzero
    (lambda (ptr sizeof)
      (let ([isize (foreign-sizeof 'int)])
        (let loop ([offset 0] [rem sizeof])
          (cond
           [(>= rem isize)
            (foreign-set! 'int ptr offset 0)
            (loop (fx+ offset isize) (fx- rem isize))]
           [(fx=? rem 0)
            ptr]
           [else
            (for-each
             (lambda (i) (foreign-set! 'unsigned-8 ptr (fx+ offset i) 0))
             (iota rem))
            ptr])))))

  (meta define string-map
        (lambda (func str)
          (list->string (map func (string->list str)))))

  (meta define symbol->function-name-string
        (lambda (sym)
          (string-map (lambda (c)
                        (if (eqv? c #\-)
                            #\_ c))
                      (symbol->string sym))))

  ;; [syntax] c-function: converts scheme-like function names to c-like function names before passing to foreign-procedure.
  ;; ie, word separating hyphens are converted to underscores for c.
  ;; eg,
  ;; (c-function (str-length (string) int) ....)
  ;; is converted to:
  ;; (begin
  ;;   (define str-length (foreign-procedure "str_length" (string) int))
  ;;   ...)
  (define-syntax c-function
    (lambda (stx)
      (syntax-case stx ()
        [(_ (name args return) ...)
         (with-syntax ([(function-string ...)
                        (map (lambda (n)
                               (datum->syntax n
                                 (symbol->function-name-string (syntax->datum n))))
                             #'(name ...))])
            #'(begin
                (define name
                  (foreign-procedure function-string args return)) ...))])))

  ;; [syntax] c-default-function: define c functions that take a default argument.
  ;; This behaves like c-function, except it first takes a (type, instance) pair.
  ;; c-default-function is useful for those c modules that define a bunch of functions that take
  ;; the same struct as the first argument.
  ;;
  ;; The expansion of this definition:
  ;; (c-default-function (type (current-parameter))
  ;;   (func-name1 (arg1) int)
  ;;   ...)
  ;; will look like:
  ;; (begin
  ;;   (define func-name1
  ;;     (let ([ffi-func (foreign-procedure "func_name1" (type arg1) int)])
  ;;       (lambda args (apply ffi-func (current-parameter) args))))
  ;;   ...)
  (define-syntax c-default-function
    (lambda (stx)
      (syntax-case stx ()
        [(_ (type instance) (name (arg ...) return) ...)
         (with-syntax ([(function-string ...)
                        (map (lambda (n)
                               (datum->syntax n
                                 (symbol->function-name-string (syntax->datum n))))
                             #'(name ...))])
            #'(begin
                (define name
                  (let ([ffi-func (foreign-procedure function-string (type arg ...) return)])
                    (lambda args
                      (apply ffi-func instance args)))) ...))])))

  ;; NOTE: Chez GC must be disabled or it could stomp on errno.
  ;; See: https://github.com/cisco/ChezScheme/issues/550
  ;; See: errno(3)
  ;; glibc (and maybe other libc's?) store errno per thread so this might be thread-safe?
  (define-syntax call-procedure/errno
    (syntax-rules ()
      [(_ func args* ...)
       (with-interrupts-disabled
         (let ([rc (func args* ...)])
           ;; Cache ASAP!
           ;; Hopefully this is soon enough? ie, no system calls inbetween here.
           ;; Note: I might be able to use (#%$assembly-output #t) to check?
           ;; (Otherwise we'll have to implement something in the foreign side.)
           (values rc (#%$errno))))]))

  ;; parse-enum-bit-defs: internal function.
  ;; parses enumdefs (for c-enum) and bitdefs (for c-bitmap).
  ;;
  ;; ebdefs is expected to be a mixed list of symbols / (symbol . id-number)...
  ;;
  ;; Return a list containing (syntax symbol) . (syntax id-number) pairs, suitable for use in with-syntax.
  (meta define parse-enum-bit-defs
    (lambda (ebdefs)
      (let loop ([i 0] [ds ebdefs])
        (cond
         [(null? ds) #'()]
         [else
          (syntax-case (car ds) ()
            [(id val)
             (cons (list #'id #'val) (loop (fx+ (syntax->datum #'val) 1) (cdr ds)))]
            [id
             (identifier? #'id)
             (cons (list #'id (datum->syntax #'id i)) (loop (fx+ i 1) (cdr ds)))])]))))

  ;; [syntax] c-enum: creates a function representing the enumeration.
  ;; c-enum will create a function called 'name'.
  ;; enum values are assumed to start from 0 and increase by one from the previous value unless a value is provided.
  ;;
  ;; Without args, the function will return an assoc list of symbol/value pairs.
  ;;
  ;; With one arg, the function will check the type of the arg and return a value accordingly.
  ;; ie, return an identifier (symbol) when arg is a number, and a number if the arg is a symbol.
  ;;
  ;; Error conditions are raised for invalid or unknown input values.
  ;;
  ;; eg, a c-style enum
  ;;    typedef enum { a, b = 3, c, } name;
  ;;
  ;; could be represented as:
  ;; > (c-enum name a (b 3) c)
  ;; > name
  ;; #<procedure name>
  ;; > (name)
  ;; ((a . 0) (b . 3) (c . 4))
  ;; > (name 3)
  ;; b
  ;; > (name 'c)
  ;; 4
  ;; > (name 2)
  ;; Exception in name: identifier not defined for value 2 in enum
  ;; Type (debug) to enter the debugger.
  ;; > (name 'j)
  ;; Exception in name: value not defined for identifier j in enum
  ;; Type (debug) to enter the debugger.
  (define-syntax c-enum
    (lambda (stx)
      (syntax-case stx ()
        [(_ name enumdef1 enumdef* ...)
         (with-syntax
          ([((esym eid) ...) (parse-enum-bit-defs #'(enumdef1 enumdef* ...))])
          #'(define name
              (case-lambda
               [()
                '((esym . eid) ...)]
               [(x)
                (name
                 (cond
                  [(symbol? x)	'get-value]
                  [(number? x)	'get-id]
                  [else x])
                 x)]
               [(cmd arg)
                (case cmd
                  [(get-value)
                   (case arg
                     [(esym) eid] ...
                     [else (error (syntax->datum #'name) (format #f "value not defined for identifier ~s in enum" arg))])]
                  [(get-id)
                   (case arg
                     [(eid) 'esym] ...
                     [else (error (syntax->datum #'name) (format #f "identifier not defined for value ~d in enum" arg))])]
                  [else
                   (error (syntax->datum #'name) (format #f "unknown enum command ~s" cmd))])])))])))

  ;; [syntax] c-bitmap: define a bitmap enumeration.
  ;; Behaves as c-enum, except each field defines a bit. Querying for symbols returns a list.
  ;;
  ;; eg,
  ;; > (c-bitmap flags A B C D)
  ;; > (flags 'A)
  ;; 0
  ;; > (flags 'B)
  ;; 1
  ;; > (flags 'C)
  ;; 2
  ;; > (flags #b110)
  ;; (B C)
  ;; > (flags #b10)
  ;; (B)
  (define-syntax c-bitmap
    (lambda (stx)
      (syntax-case stx ()
        [(_ name bitdef1 bitdef* ...)
         (with-syntax
          ([((esym eid) ...) (parse-enum-bit-defs #'(bitdef1 bitdef* ...))])
          #'(define name
              (case-lambda
               [()
                '((esym . eid) ...)]
               [(x)
                (name
                 (cond
                  [(symbol? x)	'get-value]
                  [(number? x)	'get-symbols]
                  [else x])
                 x)]
               [(cmd arg)
                (case cmd
                  [(get-value)
                   (case arg
                     [(esym) eid] ...
                     [else (error (syntax->datum #'name) (format #f "value not defined for identifier ~s in bitmap" arg))])]
                  [(get-symbols)
                   (let loop ([ids '(eid ...)] [syms '(esym ...)])
                     (cond
                      [(null? ids) '()]
                      [else
                       (if (bitwise-bit-set? arg (car ids))
                           (cons (car syms) (loop (cdr ids) (cdr syms)))
                           (loop (cdr ids) (cdr syms)))]))]
                  [else
                   (error (syntax->datum #'name) (format #f "unknown bitmap command ~s" cmd))])])))])))

  ;; [procedure] locate-library-object: find first instance of filename within (library-directories) object directories.
  ;; Returns full path of located file, including the filename itself. filename only if not found.
  (define locate-library-object
    (lambda (filename)
      (let loop ([fps (map (lambda (d) (string-append (cdr d) "/" filename)) (library-directories))])
        (cond
         [(null? fps)
          filename]
         [(file-exists? (car fps))
          (car fps)]
         [else
          (loop (cdr fps))]))))

  ;; [proc] copy contents of memory buffer into a u8-bytevector.
  (define u8*->bv
    (lambda (ptr len)
      (let ([bv (make-bytevector len)])
        (let loop ([i 0])
          (cond
            [(fx=? i len)
             bv]
            [else
              (bytevector-u8-set! bv i (foreign-ref 'unsigned-8 ptr i))
              (loop (fx+ i 1))])))))

  ;; [proc] write u8 bytevector into a foreign memory address.
  (define bv->u8*
    (case-lambda
      [(bv address)
       (bv->u8* bv address (bytevector-length bv))]
      [(bv address len)
       ;; copy in the bytes.
       (do ([i 0 (+ i 1)])
         ((= i len) address)
         (foreign-set! 'unsigned-8 address i (bytevector-u8-ref bv i)))]))

  ;; [proc] return ftypes (* unsigned-8) as a UTF8 string.
  (define u8*->string
    (lambda (fptr)
      (if (ftype-pointer-null? fptr)
          ""
          (utf8->string
            (let f ([i 0])
              (let ([c (ftype-ref unsigned-8 () fptr i)])
                (if (fx= c 0)
                    (make-bytevector i)
                    (let ([bv (f (fx+ i 1))])
                      (bytevector-u8-set! bv i c)
                      bv))))))))

  ;; u8** = vector of u8*
  (define u8**->string-list
    (lambda (u8** nitems)
      (let ([ptr-size (ftype-sizeof void*)])
        (do ([i 0 (+ i 1)]
             [v (make-vector nitems)
                (let ([saddr (foreign-ref 'void* u8** (* i ptr-size))])
                  (vector-set! v i (u8*->string saddr))
                  v)])
            ((= i nitems) (vector->list v))))))

  ;; [proc] u8**->strings/free: converts ftype (* u8*) to a list of strings, freeing the source memory.
  ;;
  ;; This function requires that the strings, and the list of strings, are all NULL terminated.
  (define u8**->strings/free
    (lambda (ftype-u8**)
      (if ftype-u8**
        (let loop ([i 0])
          (let* ([sptr (ftype-ref u8* () ftype-u8** i)])
            (cond
             [(ftype-pointer-null? sptr)
              (foreign-free (ftype-pointer-address ftype-u8**))
              '()]
             [else
              (let ([str (u8*->string sptr)])
                (foreign-free (ftype-pointer-address sptr))
                (cons str (loop (fx+ i 1))))])))
        '())))

  ;; [proc] return scheme string object as a ftypes u8* memory block.
  (define string->u8*
    (lambda (str)
      ;; foreign-alloc string and copy in the bytes.
      (let* ([bv (string->utf8 str)]
             [len (bytevector-length bv)])
        (let ([ret
               (do ([i 0 (fx+ i 1)]
                    [fv (foreign-alloc (fx+ 1 len))
                        (begin
                          (foreign-set! 'unsigned-8 fv i (bytevector-u8-ref bv i))
                          fv)])
                   ((= i len) fv))])
          (foreign-set! 'unsigned-8 ret len 0)	;; null terminate.
          ret))))

  (define string-list->u8**
    (lambda (str*)
      (define string->u8*/null
        (lambda (str)
          (if str
            (string->u8* str)
            0)))
      (let ([len (length str*)]
            [ptr-sz (ftype-sizeof void*)])
        (do ([i 0 (+ i 1)]
             [v (foreign-alloc (* len ptr-sz))
                (let ([fstr (string->u8*/null (list-ref str* i))])
                  (foreign-set! 'void* v (* i ptr-sz) fstr)
                  v)])
            ((= i len) v)))))

  (define free-u8**
    (case-lambda
     ([u8**]
      (let loop ([i 0])
        (let ([p (foreign-ref 'void* u8** (* i (ftype-sizeof void*)))])
          (cond
           [(fx=? p 0)
            ;; free containing u8** block.
            (foreign-free u8**)]
           [else
            ;; free individual u8 pointers.
            (foreign-free p)
            (loop (fx+ i 1))]))))
     ([u8** len]
      ;; free individual u8 pointers.
      (for-each
       (lambda (i)
         (foreign-free (foreign-ref 'void* u8** (* i (ftype-sizeof void*)))))
       (iota len))
      ;; free containing u8** block.
      (foreign-free u8**)))))
