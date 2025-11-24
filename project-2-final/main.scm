;; Core Data Structures (using closures for encapsulation)

;; Configuration builder - uses closure to accumulate state
(define (make-config-builder)
  (let ((grid-cfg '())
        (functions '())
        (kernels '())
        (lifeforms '())
        (display-cfg '()))
    (lambda (msg . args)
      (cond
        ((eq? msg 'set-grid!) (set! grid-cfg (car args)))
        ((eq? msg 'add-function!) (set! functions (cons (car args) functions)))
        ((eq? msg 'add-kernel!) (set! kernels (cons (car args) kernels)))
        ((eq? msg 'add-lifeform!) (set! lifeforms (cons (car args) lifeforms)))
        ((eq? msg 'set-display!) (set! display-cfg (car args)))
        ((eq? msg 'build)
         (list (cons 'grid grid-cfg)
               (cons 'functions (reverse functions))
               (cons 'kernels (reverse kernels))
               (cons 'lifeforms (reverse lifeforms))
               (cons 'display display-cfg)))
        (else '())))))

(define (make-pair . xs) (cons (car xs) (cadr xs)))
(define (alist-ref al k)
  (let loop ((a al))
    (if (null? a)
        #f
        (if (eq? (caar a) k) (cdar a) (loop (cdr a))))))

(define (ensure-symbol x) (if (symbol? x) x (string->symbol x)))
(define (ensure-string x) (if (string? x) x (symbol->string x)))

;; Hygienic Macros for Declarative Syntax

;; Main configuration macro - establishes the automaton context
;;(define-syntax automaton
;;  (syntax-rules (grid functions kernels lifeforms display) ; simulation
;;    ((_ (grid w h)
;;        (functions f ...)
;;        (kernels k ...)
;;        (lifeforms l ...)
;;        ;(simulation s ...)
;;        (display d ...))
;;     (let ((b (make-config-builder)))
;;       (begin
;;         (b 'set-grid! (list (make-pair 'width w) (make-pair 'height h)))
;;         (begin (b 'add-function! f) ...)
;;         (begin (b 'add-kernel! k) ...)
;;         (begin (b 'add-lifeform! l) ...)
;;         (b 'set-display! (list d ...))
;;         (b 'build))))))

;; NEW VERSION

(define-syntax CREATE
  (syntax-rules (AUTOMATON DEFINE FUNCTION KERNEL LIFEFORM ADD GRID CONFIG)
    ((_ AUTOMATON 
        (GRID w h)
        body ...)
     (let ((b (make-config-builder)))
       (begin
         (b 'set-grid! (list (make-pair 'width w) (make-pair 'height h)))
         (process-automaton-body b body ...)
         (b 'build))))))

(define-syntax process-automaton-body
  (syntax-rules (CONFIG DEFINE FUNCTION KERNEL ADD LIFEFORM)
    ;; Base case - no more clauses
    ((_ builder)
     (begin))
    
    ((_ builder (CONFIG d) rest ...)
     (begin
       (builder 'set-display! (list d))
       (process-automaton-body builder rest ...)))
    
    ((_ builder (DEFINE FUNCTION name def) rest ...)
     (begin
       (builder 'add-function! (define-function name def))
       (process-automaton-body builder rest ...)))
    
    ((_ builder (DEFINE KERNEL name (disk radius: r blur: (sigma: s size: sz))) rest ...)
     (begin
       (builder 'add-kernel! (list (quote name) 'disk r s sz))
       (process-automaton-body builder rest ...)))
    
    ((_ builder (DEFINE KERNEL name (ring outer: out inner: in blur: (sigma: s size: sz))) rest ...)
     (begin
       (builder 'add-kernel! (list (quote name) 'ring out in s sz))
       (process-automaton-body builder rest ...)))
    
    ((_ builder (DEFINE KERNEL name (square side: side blur: (sigma: s size: sz) custom: arr)) rest ...)
     (begin
       (builder 'add-kernel! (list (quote name) 'square side s sz arr))
       (process-automaton-body builder rest ...)))
    
    ((_ builder (DEFINE KERNEL name expr) rest ...)
     (begin
       (builder 'add-kernel! (kernel-assign (quote name) expr))
       (process-automaton-body builder rest ...)))
    
    ((_ builder (ADD LIFEFORM name props ...) rest ...)
     (begin
       (builder 'add-lifeform! (define-lifeform name props ...))
       (process-automaton-body builder rest ...)))))



;; END NEW VERSION


;; Function definition macro with pattern matching
(define-syntax define-function
  (syntax-rules (gaussian step linear conway identity mu: 
                 sigma: growth: threshold: slope: intercept:
                 underpopulation: overpopulation: reproduction:)

    ((_ name (gaussian mu: m sigma: sig amplitude: amp baseline: base))
     (list (quote name) 'gaussian m sig amp base))
    ((_ name (step threshold: t low_value: lv high_value: hv))
     (list (quote name) 'step t lv hv))
    ((_ name (linear slope: sl intercept: int))
     (list (quote name) 'linear sl int))
    ((_ name (conway underpopulation: und overpopulation: over reproduction:rep))
     (list (quote name) 'conway und over rep))
    ((_ name (identity))
     (list (quote name) 'identity))))

;; Kernel definition macro
(define-syntax define-kernel
  (syntax-rules (disk ring square blur: outer: inner: side: sigma: size: custom: radius:)
    ((_ name (disk radius: r blur: (sigma: s size: sz)))
     (list (quote name) 'disk r s sz))
     ((_ (disk radius: r blur: (sigma: s size: sz)))
     (list 'disk r s sz))
    ((_ name (ring outer: out inner: in blur: (sigma: s size: sz)))
     (list (quote name) 'ring out in s sz))
     ((_ (ring outer: out inner: in blur: (sigma: s size: sz)))
     (list 'ring out in s sz))
    ((_ name (square side: side blur: (sigma: s size: sz) custom: arr))
     (list (quote name) 'square side s sz arr))
    ((_ (square side: side blur: (sigma: s size: sz) custom: arr))
     (list 'square side s sz arr))))

;; Kernel Assign
(define (kernel-assign name k)
  (cons name k))

;; Kernel combinator - creates composite kernels using operations
(define (kernel-combinator op)
  (lambda (k1 k2)
    (list 'composite op k1 k2)))

;; Specific kernel operators (curried combinators)
(define kernel+ (kernel-combinator 'add))
(define kernel- (kernel-combinator 'subtract))
(define kernel* (kernel-combinator 'multiply))
(define kernel/ (kernel-combinator 'divide))

;; Scale kernel by constant (higher-order function)
(define (scale-kernel factor)
  (lambda (kernel)
    (kernel* kernel factor)))

;; Kernel reference (for reusing named kernels)
(define (kernel-ref name)
  (list 'ref name))

;; Function reference (for reusing named functions)
(define (function-ref name)
  (list 'ref name))

;; Lifeform definition macro
(define-syntax define-lifeform
  (syntax-rules (color: initial: rules:)
    ((_ name (color: col) (initial: init) (rules: r ...))
     (list (quote name) (ensure-string col) (quote init) (list r ...)))))

(define-syntax rule
  (syntax-rules (-> dt: kernel: function: weight:)
    ((_ target -> (dt: dtv kernel: k function: f weight: w))
     (list (quote target) dtv (quote k) (quote f) w))))

(define-syntax dt
  (syntax-rules ()
    ((_ v) (cons 'dt v))))
(define-syntax steps
  (syntax-rules ()
    ((_ v) (cons 'steps v))))
(define-syntax seed
  (syntax-rules ()
    ((_ v) (cons 'seed v))))
(define-syntax diffusion
  (syntax-rules ()
    ((_ v) (cons 'diffusion v))))

(define-syntax WINDOW
  (syntax-rules ()
    ((_ v) (cons 'window (quote v)))))
(define-syntax FPS
  (syntax-rules ()
    ((_ v) (cons 'fps v))))
(define-syntax QUIT
  (syntax-rules ()
    ((_ v) (cons 'quit (quote v)))))
(define-syntax SCALE
  (syntax-rules ()
    ((_ v) (cons 'scale v))))

(define (yaml-str . xs) (apply string-append xs))
(define (yaml-num n) (number->string n))
(define (yaml-sym s) (symbol->string s))

(define (yaml-indent n) (make-string n #\space))
(define (yaml-kv indent key val)
  (yaml-str (yaml-indent indent) (ensure-string key) ": " val "\n"))

(define (yaml-write-func-params type params)
  (cond
    ((eq? type 'gaussian)
     (yaml-str "    mu: " (yaml-num (list-ref params 0)) "\n"
               "    sigma: " (yaml-num (list-ref params 1)) "\n"
               "    amplitude: " (yaml-num (list-ref params 2)) "\n"
               "    baseline: " (yaml-num (list-ref params 3)) "\n"))
    ((eq? type 'step)
     (yaml-str "    threshold: " (yaml-num (list-ref params 0)) "\n"
               "    low_value: " (yaml-num (list-ref params 1)) "\n"
               "    high_value: " (yaml-num (list-ref params 2)) "\n"))
    ((eq? type 'linear)
     (yaml-str "    slope: " (yaml-num (list-ref params 0)) "\n"
               "    intercept: " (yaml-num (list-ref params 1)) "\n"))
    ((eq? type 'conway)
     (yaml-str "    underpopulation: " (yaml-num (list-ref params 0)) "\n"
               "    overpopulation: " (yaml-num (list-ref params 1)) "\n")
               "    reproduction")
    ((eq? type 'identity))
    (else "")))

(define (yaml-write-functions funcs)
  (if (null? funcs)
      ""
      (apply string-append
             (map (lambda (f)
                    (cond 
                      ((eq? (cadr f) 'identity) 
                        (let ((name (list-ref f 0))
                            (type (list-ref f 1)))
                            (yaml-str "  " (yaml-sym name) ":\n"
                                      "    type: " (yaml-sym type) "\n")))

                      (else (let ((name (list-ref f 0))
                            (type (list-ref f 1))
                            (params (cddr f)))
                            (yaml-str "  " (yaml-sym name) ":\n"
                                  "    type: " (yaml-sym type) "\n"
                                  (yaml-write-func-params type params))))))
                  funcs))))

(define (yaml-write-kernel-body type params)
  (cond
    ((eq? type 'disk)
     (yaml-str "    type: disk\n"
               "    radius: " (yaml-num (list-ref params 0)) "\n"
               "    gaussian_sigma: " (yaml-num (list-ref params 1)) "\n"
               "    gaussian_kernel_size: " (yaml-num (list-ref params 2)) "\n"))
    
    ((eq? type 'ring)
     (yaml-str "    type: ring\n"
               "    outer_diameter: " (yaml-num (list-ref params 0)) "\n"
               "    inner_diameter: " (yaml-num (list-ref params 1)) "\n"
               "    gaussian_sigma: " (yaml-num (list-ref params 2)) "\n"
               "    gaussian_kernel_size: " (yaml-num (list-ref params 3)) "\n"))
    
    ((eq? type 'square)
     (let ((side (list-ref params 0))
           (sigma (list-ref params 1))
           (ksz (list-ref params 2))
           (arr (list-ref params 3)))
       (yaml-str "    type: square\n"
                 "    side: " (yaml-num side) "\n"
                 "    gaussian_sigma: " (yaml-num sigma) "\n"
                 "    gaussian_kernel_size: " (yaml-num ksz) "\n"
                 (if arr (yaml-str "    custom_array: " (ensure-string arr) "\n") ""))))
    
    ((eq? type 'composite)
      (let ((op (car params))
            (left (cadr params))
            (right (caddr params)))
        (yaml-str "    type: composite\n"
                  "    operation: " (yaml-sym op) "\n"
                  "    left:\n" (yaml-write-kernel-sub left 6)
                  "    right:\n" (yaml-write-kernel-sub right 6))))

    (else "")))

(define (yaml-write-kernel-params  type params indent)
  (cond
    ((eq? type 'disk)
     (yaml-str (yaml-indent indent) "type: disk\n"
               (yaml-indent indent) "radius: " (yaml-num (list-ref params 0)) "\n"
               (yaml-indent indent) "gaussian_sigma: " (yaml-num (list-ref params 1)) "\n"
               (yaml-indent indent) "gaussian_kernel_size: " (yaml-num (list-ref params 2)) "\n"))
    
    ((eq? type 'ring)
     (yaml-str (yaml-indent indent) "type: ring\n"
               (yaml-indent indent) "outer_diameter: " (yaml-num (list-ref params 0)) "\n"
               (yaml-indent indent) "inner_diameter: " (yaml-num (list-ref params 1)) "\n"
               (yaml-indent indent) "gaussian_sigma: " (yaml-num (list-ref params 2)) "\n"
               (yaml-indent indent) "gaussian_kernel_size: " (yaml-num (list-ref params 3)) "\n"))
    
    ((eq? type 'square)
     (let ((side (list-ref params 0))
           (sigma (list-ref params 1))
           (ksz (list-ref params 2))
           (arr (list-ref params 3)))
       (yaml-str (yaml-indent indent) "type: square\n"
                 (yaml-indent indent) "side: " (yaml-num side) "\n"
                 (yaml-indent indent) "gaussian_sigma: " (yaml-num sigma) "\n"
                 (yaml-indent indent) "gaussian_kernel_size: " (yaml-num ksz) "\n"
                 (if arr (yaml-str (yaml-indent indent) "    custom_array: " (ensure-string arr) "\n") ""))))

    (else "")))

(define (yaml-write-kernel-sub k indent)
  (cond
    ((not (list? k))
    (cond 
      ((number? k) (yaml-str (yaml-indent indent) (yaml-num k) "\n"))
      (else (yaml-str (yaml-indent indent) "ref: " (yaml-sym k) "\n"))
    ))
    
    ;; Composite kernel recursion
    ((eq? (car k) 'composite)
     (let ((op (cadr k))
           (left (caddr k))
           (right (cadddr k)))
       (yaml-str (yaml-indent indent) "type: composite\n"
                 (yaml-indent indent) "operation: " (yaml-sym op) "\n"
                 (yaml-indent indent) "left:\n" (yaml-write-kernel-sub left (+ indent 4))
                 (yaml-indent indent) "right:\n" (yaml-write-kernel-sub right (+ indent 4)))))
    
    ; atomic kernel fallback (disk/ring/square)
    (else
      (let ((type (car k))
            (params (cdr k)))
            (yaml-str 
                  (yaml-write-kernel-params type params indent))))))

(define (yaml-write-kernels kernels)
  (if (null? kernels)
      ""
      (apply string-append
             (map (lambda (k)
                    (let ((name (list-ref k 0))
                          (type (list-ref k 1))
                          (params (cddr k)))
                            (yaml-str "  " (yaml-sym name) ":\n"
                                (yaml-write-kernel-body type params))))
                  kernels))))

(define (yaml-write-rule r)
  (let ((name (list-ref r 0))
        (dt (list-ref r 1))
        (kern (list-ref r 2))
        (func (list-ref r 3))
        (weight (list-ref r 4)))
    (yaml-str "      " (yaml-sym name) ":\n"
              "        dt: " (yaml-num dt) "\n"
              "        kernel:\n"
              "          ref: " (yaml-sym kern) "\n"
              "        func:\n"
              "          ref: " (yaml-sym func) "\n"
              "        weight: " (yaml-num weight) "\n")))

(define (yaml-write-rules rules)
  (apply string-append (map yaml-write-rule rules)))

(define (yaml-write-lifeforms lifes)
  (if (null? lifes)
      ""
      (apply string-append
             (map (lambda (life)
                    (let ((name (list-ref life 0))
                          (color (list-ref life 1))
                          (initial (list-ref life 2))
                          (rules (list-ref life 3)))
                      (yaml-str "  " (yaml-sym name) ":\n"
                                "    color: \"" color "\"\n"
                                "    initial_state: " (yaml-sym initial) "\n"
                                "    rules:\n"
                                (yaml-write-rules rules)
                                "\n")))
                  lifes))))

(define (yaml-write-grid grid)
  (if (null? grid)
      ""
      (let ((w (cdr (assq 'width grid)))
            (h (cdr (assq 'height grid))))
        (yaml-str "  width: " (yaml-num w) "\n"
                  "  height: " (yaml-num h) "\n"))))

(define (yaml-write-simulation sim)
  (if (null? sim)
      ""
      (let loop ((rest sim) (acc ""))
        (if (null? rest)
            acc
            (let* ((p (car rest))
                   (k (car p))
                   (v (cdr p)))
              (loop (cdr rest)
                    (string-append acc
                      (case k
                        ((dt) (yaml-kv 2 "dt" (yaml-num v)))
                        ((steps) (yaml-kv 2 "steps" (yaml-num v)))
                        ((seed) (yaml-kv 2 "seed" (yaml-num v)))
                        ((diffusion) (yaml-kv 2 "diffusion" (yaml-num v)))
                        (else (yaml-kv 2 (yaml-sym k) (ensure-string v)))))))))))

(define (yaml-write-display d)
  (if (null? d)
      ""
      (let loop ((rest d) (acc ""))
        (if (null? rest)
            acc
            (let* ((p (car rest))
                   (k (car p))
                   (v (cdr p)))
              (loop (cdr rest)
                    (string-append acc
                      (case k
                        ((window) (yaml-kv 2 "window_name" (yaml-sym v)))
                        ((fps)
                            (let ((n (if (list? v) (car v) v)))
                                (yaml-kv 2 "show_fps" (if (> n 0) "true" "false"))))
                        ((quit) (yaml-kv 2 "quit_key" (yaml-sym v)))
                        (else (yaml-kv 2 (yaml-sym k) (ensure-string v)))))))))))

(define (yaml-write-top-section p)
  (let ((section (car p))
        (data (cdr p)))
    (cond
      ((eq? section 'grid) (string-append "grid:\n" (yaml-write-grid data) "\n"))
      ((eq? section 'functions) (string-append "functions:\n" (yaml-write-functions data) "\n"))
      ((eq? section 'kernels) (string-append "kernels:\n" (yaml-write-kernels data) "\n"))
      ((eq? section 'lifeforms) (string-append "lifeforms:\n" (yaml-write-lifeforms data)))
      ((eq? section 'display) (string-append "display:\n" (yaml-write-display data) "\n"))
      (else ""))))

(define (config->yaml config)
  (apply string-append (map yaml-write-top-section config)))

(define (show-config cfg)
  (display (config->yaml cfg)))

(define (write-config-to-file cfg filename)
  (call-with-output-file filename
    (lambda (out)
      (display (config->yaml cfg) out))))

(define (run-simulation cfg)
  (write-config-to-file cfg "temp.yml")
  (system "python3 src/yml_main.py temp.yml")
  (system "rm temp.yml"))

(define cfg
  (CREATE AUTOMATON
    (GRID 1080 720)
    (CONFIG (FPS 60))
    (CONFIG (WINDOW main))
    (CONFIG (QUIT q))
    
    (DEFINE FUNCTION f1
      (gaussian mu: 0.11 sigma: 0.08 amplitude: 2.0 baseline: -1.0))
    
    (DEFINE KERNEL k1
      (ring outer: 41 inner: 31 blur: (sigma: 1.1 size: 13)))
    
    (DEFINE KERNEL k2
      (kernel-
        (define-kernel (ring outer: 21 inner: 15 blur: (sigma: 1.2 size: 15))) 
        (kernel/ 'k1 2)))
    
    (ADD LIFEFORM wanderer
      (color: "#00FF88")
      (initial: random)
      (rules:
        (rule wanderer -> (dt: 0.01 kernel: k2 function: f1 weight: 1.0))))))


(define nuclei
  (CREATE AUTOMATON
    (GRID 1080 720)
    (CONFIG (FPS 60))
    (CONFIG (WINDOW main))
    (CONFIG (QUIT q))
    
    (DEFINE FUNCTION f1
      (gaussian mu: 0.11 sigma: 0.08 amplitude: 2.0 baseline: -1.0))

    (DEFINE FUNCTION f2
      (gaussian mu: 0.9 sigma: 0.05 amplitude: 2.0 baseline: -1.0))
    
    (DEFINE KERNEL k1
      (ring outer: 41 inner: 31 blur: (sigma: 1.1 size: 13)))

    (DEFINE KERNEL k2
      (kernel-
        (define-kernel (ring outer: 21 inner: 15 blur: (sigma: 1.2 size: 15))) 
        (kernel/ 'k1 2)))
    
    (ADD LIFEFORM skin
      (color: "#28ff9bff")
      (initial: random)
      (rules:
        (rule skin -> (dt: 0.001 kernel: k2 function: f1 weight: 1.0))))
    
    (ADD LIFEFORM nucleo
      (color: "#e3b05eff")
      (initial: random)
      (rules:
        (rule nucleo -> (dt: 0.001 kernel: k2 function: f2 weight: 1.0))))))


(define aquarium
  (CREATE AUTOMATON
    (GRID 1200 800)
    (CONFIG (FPS 45))
    (CONFIG (WINDOW aquarium))
    (CONFIG (QUIT q))
    
    ;; Growth functions
    (DEFINE FUNCTION predator-growth
      (gaussian mu: 0.25 sigma: 0.03 amplitude: 1.2 baseline: -0.5))
    
    (DEFINE FUNCTION prey-growth
      (gaussian mu: 0.18 sigma: 0.05 amplitude: 1.5 baseline: -0.7))
    
    (DEFINE FUNCTION algae-growth
      (linear slope: 3.0 intercept: -1.0))
    
    ;; Kernels
    (DEFINE KERNEL small-hunt
      (ring outer: 18 inner: 10 blur: (sigma: 0.8 size: 7)))
    
    (DEFINE KERNEL wide-sense
      (ring outer: 35 inner: 20 blur: (sigma: 1.5 size: 11)))
    
    (DEFINE KERNEL diffuse
      (disk radius: 8 blur: (sigma: 2.0 size: 9)))
    
    (DEFINE KERNEL prey-kernel
      (kernel+ 'wide-sense (kernel* 'diffuse 0.4)))
    
    ;; Species
    (ADD LIFEFORM shark
      (color: "#FF0000")
      (initial: random)
      (rules:
        (rule shark -> (dt: 0.08 kernel: small-hunt function: predator-growth weight: 1.0))))
    
    (ADD LIFEFORM fish
      (color: "#00AAFF")
      (initial: random)
      (rules:
        (rule fish -> (dt: 0.12 kernel: prey-kernel function: prey-growth weight: 1.0))))
    
    (ADD LIFEFORM algae
      (color: "#00FF44")
      (initial: random)
      (rules:
        (rule algae -> (dt: 0.15 kernel: diffuse function: algae-growth weight: 1.0))))))


;; ============================================================
;; 8C. LAVA LAMP v3 - Smooth buoyant blobs (stable version)
;; ============================================================
(define lava-lamp
  (CREATE AUTOMATON
    (GRID 500 900)
    (CONFIG (FPS 60))
    (CONFIG (WINDOW lava-lamp-v3))
    (CONFIG (QUIT q))
    
    ;; ========================================================
    ;; FUNCTIONS
    ;; ========================================================

    ;; Blob cohesion — keeps forms together
    (DEFINE FUNCTION cohesion
      (gaussian mu: 0.18 sigma: 0.045 amplitude: 2.0 baseline: -1.0))
    
    ;; Surface tension for smooth edges
    (DEFINE FUNCTION tension
      (gaussian mu: 0.23 sigma: 0.06 amplitude: 1.2 baseline: -0.4))
    
    ;; Upward buoyancy — slow lift
    (DEFINE FUNCTION buoyancy
      (gaussian mu: 0.15 sigma: 0.03 amplitude: 1.6 baseline: -0.7))
    
    ;; Gentle gravity (downward bias)
    (DEFINE FUNCTION gravity
      (gaussian mu: 0.32 sigma: 0.06 amplitude: -0.6 baseline: 0.3))
    
    ;; Damping for long-term stability
    (DEFINE FUNCTION diffusion
      (linear slope: -0.4 intercept: 0.3))

    ;; ========================================================
    ;; KERNELS
    ;; ========================================================

    (DEFINE KERNEL core
      (disk radius: 16 blur: (sigma: 2.0 size: 9)))
    
    (DEFINE KERNEL shell
      (ring outer: 28 inner: 16 blur: (sigma: 1.8 size: 11)))
    
    (DEFINE KERNEL ambient
      (ring outer: 45 inner: 25 blur: (sigma: 2.5 size: 15)))
    
    ;; Nested kernel combination (binary-safe)
    (DEFINE KERNEL blob-field
      (kernel+ 
        (kernel+ 
          (kernel* 'core 1.0)
          (kernel* 'shell 0.7))
        (kernel* 'ambient -0.3)))

    ;; ========================================================
    ;; LIFEFORM
    ;; ========================================================

    (ADD LIFEFORM blob
      (color: "#FF8844")
      (initial: random)
      (rules:
        ;; Cohesion and surface tension
        (rule blob -> (dt: 0.10 kernel: blob-field function: cohesion weight: 1.0))
        (rule blob -> (dt: 0.08 kernel: shell function: tension weight: 0.8))
        
        ;; Smooth upward drift
        (rule blob -> (dt: 0.12 kernel: ambient function: buoyancy weight: 0.5))
        
        ;; Occasional slow settling
        (rule blob -> (dt: 0.25 kernel: ambient function: gravity weight: 0.3))
        
        ;; Global damping
        (rule blob -> (dt: 0.04 kernel: ambient function: diffusion weight: 0.3))))))


; Example for printing the configuration
; (show-config lava-lamp)
; 
; Example for writing the .yml
; (write-config-to-file nuclei "nuclei.yml")
; (write-config-to-file aquarium "aquarium.yml")
; (write-config-to-file lava-lamp "lava-lamp.yml")

;; Roda a simulaçao definida pela configuração
(run-simulation nuclei)
