# B-FUN-501-PAR-5-1-HAL-damien.bernard

# Dependencies
 - haskeline
   - [Github](https://github.com/judah/haskeline)
   - [Wiki](https://github.com/judah/haskeline/wiki)
   - [Haskell_Package](https://hackage.haskell.org/package/haskeline)
 - containers
   - [Haskell_Package](https://hackage.haskell.org/package/containers)
 - QuickCheck
   - [Tutorial](https://begriffs.com/posts/2017-01-14-design-use-quickcheck.html)
   - [second](http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html)
   
# Useful Links
 - Chez Scheme (Reference).
   - [Release](https://github.com/cisco/ChezScheme/releases/tag/v9.5.4)
   - [Install](https://github.com/cisco/ChezScheme/blob/master/BUILDING)
     - ./configure
     - sudo make install
   - Quick Introduction
     - [basic](https://cisco.github.io/ChezScheme/csug9.5/use.html#./use:h1)
     - Factorial
       ```
        > (define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))
       ```
     - Merge Sort
       ```
        > (define (null? l) (eq? l '()))
        > (define (merge-lists l1 l2) (cond((null? l1) l2) ((null? l2) l1) ((< (car l1) (car l2)) (cons (car l1) (merge-lists (cdr l1) l2))) (#t (cons (car l2) (merge-lists l1 (cdr l2))))))
        > (define (split-half l l1 l2) (cond ((null? l) (cons l1 l2)) ((null? (cdr l)) (split-half (cdr l) (cons (car l) l1) l2)) (#t (split-half (cdr (cdr l)) (cons (car l) l1) (cons (car (cdr l)) l2)))))
        > (define (merge-sort lst) (cond ((null? lst) '()) ((null? (cdr lst)) lst) (#t (let ((lsts (split-half lst '() '()))) (merge-lists (merge-sort (car lsts)) (merge-sort (cdr lsts)))))))
        > (merge-sort '(39 16 22 24 17 29 18 26 27 3 34 25 10 6 7 12 8 30 2 21 13 36 14 38 32 41 40 4 35 19 5 33 23 9 15 31 28 20 42 37 11 1))
            -> (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42)
       ```
       
   - REFERENCE DOCS
    - [Procedures](https://schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html)

 - Bootstrap Links.
   - [S-Expressions](https://en.wikipedia.org/wiki/S-expression)
   
 - Family types
    - https://wiki.haskell.org/GHC/Type_families
    
 - Type Application:
    - https://www.seas.upenn.edu/~sweirich/papers/type-app-extended.pdf
   
# Expressions