;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname wordle) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)
(require 2htdp/image)
(require 2htdp/universe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Wordle Implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; A Wordle game based on https://www.nytimes.com/games/wordle/


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Data (General)
; --------------
;
; - Pair
;   A generalized definition for associating two arbitrary
;   data types; this is used both for LetterStatusPair and
;   in helping to visualize elements of the game by
;   pairing up a game object, such as a guesed letter, with
;   a function used to visualize that type of object
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct pair [first second])

; A [Pair X Y] is a (make-pair X Y)
; Interpretation: A pairing of two values


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Data (Letters)
; --------------
;
; - LetterStatus
;   A categorization of scored letters
;
; - LetterStatusPair
;   An association between a specific letter and its scored
;   status; updated to use Pair
;
; - VizPair
;   An association between a value and a function that can
;   produce an image for it
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; A LetterStatus (LS) is one of:
; - "wrong"
; - "misplaced"
; - "right"
; Interpretation: Status of a guessed letter

(define LS-WRONG "wrong")
(define LS-MISPLACED "misplaced")
(define LS-RIGHT "right")


; A LetterStatusPair (LSP) is a [Pair 1String LetterStatus]
; Interpretation: A guess letter and its associated status


; A [VizPair X] is a [Pair X [X -> Image]]
; Interpretation: A pairing of a value with a function that
; produces a vizualization of it


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Constants (Visualization)
; -------------------------
;
; - Game background (BG-COLOR)
;
; - Current/upcoming guess border (BORDER-COLOR)
;
; - Current/upcoming guess background (GUESS-COLOR)
;
; - Letter size (LT-SIZE)
;
; - Buffer space between game objects (GAP)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define BG-COLOR "white")
(define BORDER-COLOR "dimgray")
(define GUESS-COLOR "black")

(define LT-SIZE 64)

(define GAP (square 5 "solid" BG-COLOR))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Functions (General)
; -------------------
;
; - mymap2
;   Abstraction for capturing the result of applying a
;   function to the elements from two parallel lists
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; mymap2 : (X Y Z) [List-of X] [List-of Y] [X Y -> Z] -> [List-of Z]
; Produces a result list by applying the supplied function
; to parallel elements from the supplied lists (until either
; is empty)

(check-expect
 (mymap2 '() '() *)
 '())

(check-expect
 (mymap2 (list 2 3) (list 4) *)
 (list 8))

(check-expect
 (mymap2 (list "a" "b")
         (list "1" "2" "3")
         string-append)
 (list "a1" "b2"))

(check-expect
 (mymap2 (list "a" "c")
         (list (list "x") (list 3 1 4))
         (λ (s l) (string-append
                   s
                   "-"
                   (number->string (length l)))))
 (list "a-1" "c-3"))

(define (mymap2 l1 l2 f)
  (cond
    [(or (empty? l1) (empty? l2)) '()]
    [(and (cons? l1) (cons? l2))
     (cons (f (first l1) (first l2))
           (mymap2 (rest l1) (rest l2) f))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Functions (Visualizing Letters)
; -------------------------------
;
; - boxed-letter
;   Abstraction for drawing a letter over a box; uses an
;   updated test to reflect that a "blank" box for guesses
;   is now local to a helper
;
; - guess-letter->image
;   Uses boxed-letter to provide a simple way to visualize
;   letters and empty spaces that have yet to be scored;
;   updated to make its background (blank) a local helper
;
; - lsp->image
;   Uses boxed-letter to provide a simple way to visualize
;   scored letters; updated to (a) make its background a
;   set of local functions and (b) utilize pair data
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; boxed-letter : 1String NonNegReal [NonNegReal -> Image] -> Image
; Produces an image of a letter on a background of a particular size

(check-expect
 (boxed-letter "A" 10 (λ (_) empty-image))
 (text "A" 5 BG-COLOR))

(check-expect
 (boxed-letter "B" 64 (λ (s) (overlay (square s "outline" BORDER-COLOR)
                                      (square s "solid" GUESS-COLOR))))
 (overlay
  (text "B" 32 BG-COLOR)
  (square 64 "outline" BORDER-COLOR)
  (square 64 "solid" GUESS-COLOR)))

(define (boxed-letter s size bgf)
  (overlay
   (text s (/ size 2) BG-COLOR)
   (bgf size)))


; guess-letter->image : 1String -> Image
; Visualizes a guessed character

(check-expect
 (guess-letter->image "A")
 (overlay
  (text "A" (/ LT-SIZE 2) BG-COLOR)
  (square LT-SIZE "outline" BORDER-COLOR)
  (square LT-SIZE "solid" GUESS-COLOR)))

(check-expect
 (guess-letter->image "B")
 (overlay
  (text "B" (/ LT-SIZE 2) BG-COLOR)
  (square LT-SIZE "outline" BORDER-COLOR)
  (square LT-SIZE "solid" GUESS-COLOR)))

(define (guess-letter->image s)
  (local [; blank : NonNegReal -> Image
          ; Produces a blank box of the appropriate size
          (define (blank size)
            (overlay (square size "outline" BORDER-COLOR)
                     (square size "solid" GUESS-COLOR)))]
    (boxed-letter s LT-SIZE blank)))


; lsp->image : LetterStatusPair -> Image
; Produces a visualization of a letter with status

(check-expect (lsp->image (make-pair "A" LS-RIGHT))
              (overlay
               (text "A" (/ LT-SIZE 2) BG-COLOR)
               (square LT-SIZE "solid" "darkgreen")))

(check-expect (lsp->image (make-pair "B" LS-WRONG))
              (overlay
               (text "B" (/ LT-SIZE 2) BG-COLOR)
               (square LT-SIZE "solid" "dimgray")))

(check-expect (lsp->image (make-pair "C" LS-MISPLACED))
              (overlay
               (text "C" (/ LT-SIZE 2) BG-COLOR)
               (square LT-SIZE "solid" "goldenrod")))

(define (lsp->image lsp)
  (local [; ls->color : LS -> Color
          ; Produces a background color associated with a letter status
          (define (ls->color ls)
            (cond
              [(string=? ls LS-WRONG) "dimgray"]
              [(string=? ls LS-MISPLACED) "goldenrod"]
              [(string=? ls LS-RIGHT) "darkgreen"]))

          ; ls->color-fn : LetterStatus -> [NonNegReal -> Image]
          ; Produces a function for a status-specific background
          ; of the appropriate size
          (define (ls->color-fn ls)
            (λ (size) (square size "solid" (ls->color ls))))]
    (boxed-letter
     (pair-first lsp)
     LT-SIZE
     (ls->color-fn (pair-second lsp)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Functions (Visualizing Letters)
; -------------------------------
;
; - stack
;   Abstraction for separating a set of images using a
;   pairwise orientation function
;
; - stack/v
;   Uses stack to easily visualize a list of images in
;   a horizontal fashion
; 
; - stack/h
;   Uses stack to easily visualize a list of images in
;   a horizontal fashion
;
; - row->image
;   Uses stack/h to easily visualize a list of objects, each
;   with an associated visualization function (e.g., a list
;   of letters with one of the functions from the preceding
;   section).
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; stack : [List-of Images] [Image Image -> Image] -> Image
; Space separates a list of images via a pairwise orientation function

(check-expect
 (stack '() beside)
 GAP)

(check-expect
 (stack '() above)
 GAP)

(check-expect
 (stack
  (list
   (text "A" 5 "black")
   (text "B" 10 "black")
   (text "C" 50 "black"))
  above)
 (above
  GAP
  (text "A" 5 "black")
  GAP
  (text "B" 10 "black")
  GAP
  (text "C" 50 "black")
  GAP))

(check-expect
 (stack
  (list
   (text "A" 5 "black")
   (text "B" 10 "black")
   (text "C" 50 "black"))
  beside)
 (beside
  GAP
  (text "A" 5 "black")
  GAP
  (text "B" 10 "black")
  GAP
  (text "C" 50 "black")
  GAP))

(define (stack images addf)
  (local [; buffer : Image Image -> Image
          ; Positions the supplied image
          ; relative to the supplied background
          ; with a gap
          (define (buffer i bg)
            (addf GAP i bg))]
    (foldr
     buffer
     GAP
     images)))


; stack/v : [List-of Images] -> Image
; Space-separates a list of images vertically

(check-expect
 (stack/v '())
 GAP)

(check-expect
 (stack/v
  (list
   (text "A" 5 "black")
   (text "B" 10 "black")
   (text "C" 50 "black")))
 (above
  GAP
  (text "A" 5 "black")
  GAP
  (text "B" 10 "black")
  GAP
  (text "C" 50 "black")
  GAP))

(define (stack/v images)
  (stack images above))


; stack/h : [List-of Image] -> Image
; Space-separates a list of images horizontally

(check-expect
 (stack/h '())
 GAP)

(check-expect
 (stack/h
  (list
   (text "A" 5 "black")
   (text "B" 10 "black")
   (text "C" 50 "black")))
 (beside
  GAP
  (text "A" 5 "black")
  GAP
  (text "B" 10 "black")
  GAP
  (text "C" 50 "black")
  GAP))

(define (stack/h images)
  (stack images beside))


; row->image : [List-of [VizPair Any]] -> Image
; Horizontally visualizes a list of elements, each with an
; associated visualization function

(check-expect
 (row->image '())
 GAP)

(check-expect
 (row->image
  (list
   (make-pair "X" guess-letter->image)
   (make-pair "Y" guess-letter->image)
   (make-pair " " guess-letter->image)))
 (beside
  GAP
  (guess-letter->image "X")
  GAP
  (guess-letter->image "Y")
  GAP
  (guess-letter->image " ")
  GAP))

(check-expect
 (row->image
  (list
   (make-pair (make-pair "A" LS-RIGHT) lsp->image)
   (make-pair (make-pair "B" LS-WRONG) lsp->image)
   (make-pair (make-pair "C" LS-MISPLACED) lsp->image)))
 (beside
  GAP
  (lsp->image (make-pair "A" LS-RIGHT))
  GAP
  (lsp->image (make-pair "B" LS-WRONG))
  GAP
  (lsp->image (make-pair "C" LS-MISPLACED))
  GAP))

(check-expect
 (row->image
  (list
   (make-pair "X" guess-letter->image)
   (make-pair (make-pair "A" LS-RIGHT) lsp->image)))
 (beside
  GAP
  (guess-letter->image "X")
  GAP
  (lsp->image (make-pair "A" LS-RIGHT))
  GAP))

(define (row->image ips)
  (stack/h
   (map
    (λ (ip) ((pair-second ip) (pair-first ip)))
    ips)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; value-in-list? : (X) X [List-of X] [X X -> Boolean] -> Boolean
; Determines if the supplied value in the list accords to the supplied predicate

(check-expect (value-in-list? 1 (list) =) #f)
(check-expect (value-in-list? "a" (list) string=?) #f)
(check-expect (value-in-list? 1 (list 1 2 3) =) #t)
(check-expect (value-in-list? 5 (list 1 2 3) =) #f)
(check-expect (value-in-list? "a" (list "a" "b" "c") string=?) #t)
(check-expect (value-in-list? "A" (list "a" "b" "c") string-ci=?) #t)

(define (value-in-list? x lox p?)
  (ormap (λ (lox-x)
           (p? x lox-x)) lox))

; Recursive list-based approach
#|(cond
    [(empty? lox) #f]
    [(cons? lox)
     (or (p? x (first lox))
         (value-in-list? x (rest lox) p?))]))|#


; string-in-list? : String [List-of String] -> Boolean
; Determines if the supplied string is in the list

(check-expect (string-in-list? "a" (list)) #f)
(check-expect (string-in-list? "a" (list "a" "b" "c")) #t)
(check-expect (string-in-list? "A" (list "a" "b" "c")) #f)
(check-expect (string-in-list? "b" (list "a" "b" "c")) #t)
(check-expect (string-in-list? "c" (list "a" "b" "c")) #t)
(check-expect (string-in-list? "d" (list "a" "b" "c")) #f)

(define (string-in-list? s los)
  (value-in-list? s los string=?))


; read-dictionary : String -> [List-of String]
; Reads the words in a supplied file and capitalizes them all

(define LITTLE-WORDS
  (list "ACT"
        "BAD"
        "CAT"
        "DAB"
        "ETA"))

(check-expect
 (read-dictionary "BAD.EVIL")
 '())

(check-expect
 (read-dictionary "little.txt")
 LITTLE-WORDS)

(define (read-dictionary filename)
  (local [; read-data : String -> [List-of String]
          ; Reads data from a file if it exists
          (define (read-data fn)
            (if (file-exists? filename)
                (read-lines filename)
                '()))]
    (map string-upcase (read-data filename))))


; score : String String -> [List-of LetterStatusPair]
; Given a guess and the correct string (assumed to be the same length, and both
; uppercase), produce the resulting pairing of each character and its status

(check-expect (score "ABC" "ABC")
              (list (make-pair "A" LS-RIGHT)
                    (make-pair "B" LS-RIGHT)
                    (make-pair "C" LS-RIGHT)))

(check-expect (score "ABC" "XYZ")
              (list (make-pair "A" LS-WRONG)
                    (make-pair "B" LS-WRONG)
                    (make-pair "C" LS-WRONG)))

(check-expect (score "CBA" "ABC")
              (list (make-pair "C" LS-MISPLACED)
                    (make-pair "B" LS-RIGHT)
                    (make-pair "A" LS-MISPLACED)))

(check-expect (score "AAA" "ABC")
              (list (make-pair "A" LS-RIGHT)
                    (make-pair "A" LS-MISPLACED)
                    (make-pair "A" LS-MISPLACED)))

(check-expect (score "AAAXB" "ABCAD")
              (list (make-pair "A" LS-RIGHT)
                    (make-pair "A" LS-MISPLACED)
                    (make-pair "A" LS-MISPLACED)
                    (make-pair "X" LS-WRONG)
                    (make-pair "B" LS-MISPLACED)))

(check-expect (score "WEARY" "DONOR")
              (list (make-pair "W" LS-WRONG)
                    (make-pair "E" LS-WRONG)
                    (make-pair "A" LS-WRONG)
                    (make-pair "R" LS-MISPLACED)
                    (make-pair "Y" LS-WRONG)))

(check-expect (score "BROIL" "DONOR")
              (list (make-pair "B" LS-WRONG)
                    (make-pair "R" LS-MISPLACED)
                    (make-pair "O" LS-MISPLACED)
                    (make-pair "I" LS-WRONG)
                    (make-pair "L" LS-WRONG)))

(check-expect (score "ROUND" "DONOR")
              (list (make-pair "R" LS-MISPLACED)
                    (make-pair "O" LS-RIGHT)
                    (make-pair "U" LS-WRONG)
                    (make-pair "N" LS-MISPLACED)
                    (make-pair "D" LS-MISPLACED)))

(check-expect (score "DONOR" "DONOR")
              (list (make-pair "D" LS-RIGHT)
                    (make-pair "O" LS-RIGHT)
                    (make-pair "N" LS-RIGHT)
                    (make-pair "O" LS-RIGHT)
                    (make-pair "R" LS-RIGHT)))

(check-expect (score "GOALS" "ATONE")
              (list (make-pair "G" LS-WRONG)
                    (make-pair "O" LS-MISPLACED)
                    (make-pair "A" LS-MISPLACED)
                    (make-pair "L" LS-WRONG)
                    (make-pair "S" LS-WRONG)))

(check-expect (score "AROMA" "ATONE")
              (list (make-pair "A" LS-RIGHT)
                    (make-pair "R" LS-WRONG)
                    (make-pair "O" LS-RIGHT)
                    (make-pair "M" LS-WRONG)
                    (make-pair "A" LS-MISPLACED)))

(check-expect (score "AWOKE" "ATONE")
              (list (make-pair "A" LS-RIGHT)
                    (make-pair "W" LS-WRONG)
                    (make-pair "O" LS-RIGHT)
                    (make-pair "K" LS-WRONG)
                    (make-pair "E" LS-RIGHT)))

(check-expect (score "ABODE" "ATONE")
              (list (make-pair "A" LS-RIGHT)
                    (make-pair "B" LS-WRONG)
                    (make-pair "O" LS-RIGHT)
                    (make-pair "D" LS-WRONG)
                    (make-pair "E" LS-RIGHT)))

(check-expect (score "ATONE" "ATONE")
              (list (make-pair "A" LS-RIGHT)
                    (make-pair "T" LS-RIGHT)
                    (make-pair "O" LS-RIGHT)
                    (make-pair "N" LS-RIGHT)
                    (make-pair "E" LS-RIGHT)))

(define (score guess answer)
  (local [; score/compare : 1String 1String -> Pair
          ; Compares two 1Strings and creates a pair with the LetterStatus
          (define (score/compare s1 s2)
            (cond
              [(string=? s1 s2) (make-pair s1 LS-RIGHT)]
              [(not (string-in-list? s1 (explode answer)))
               (make-pair s1 LS-WRONG)]
              [else (make-pair s1 LS-MISPLACED)]))]
    (mymap2 (explode guess) (explode answer) score/compare)))


; When a player is actively guessing, they may have fewer letters than are
; required of a full Wordle guess (5, in the NYT version).

; partial-guess->full-guess : String Nat -> String
; Expands a guess with spaces to fill up the supplied length

(check-expect (partial-guess->full-guess "" 3) "   ")
(check-expect (partial-guess->full-guess "ABC" 3) "ABC")
(check-expect (partial-guess->full-guess "XYZ" 5) "XYZ  ")

#|
; String-based approach functions)
(define (partial-guess->full-guess/string s len)
  (string-append
   s
   (replicate (- len (string-length s)) " ")))
|#

; List-based approach to partial-guess->full-guess
(define (partial-guess->full-guess str len)
  (implode
   (append
    (explode str)
    (build-list (- len (string-length str))
                (λ (_) " ")))))


; partial-guess->image : String Nat -> Image
; Visualizes a partial guess (up to a supplied length,
; assumed to be at least as long as the supplied string)

(check-expect
 (partial-guess->image "" 3)
 (beside GAP
         (guess-letter->image " ")
         GAP
         (guess-letter->image " ")
         GAP
         (guess-letter->image " ")
         GAP))

(check-expect
 (partial-guess->image "ABC" 3)
 (beside GAP
         (guess-letter->image "A")
         GAP
         (guess-letter->image "B")
         GAP
         (guess-letter->image "C")
         GAP))

(check-expect
 (partial-guess->image "XYZ" 5)
 (beside GAP
         (guess-letter->image "X")
         GAP
         (guess-letter->image "Y")
         GAP
         (guess-letter->image "Z")
         GAP
         (guess-letter->image " ")
         GAP
         (guess-letter->image " ")
         GAP))

(define (partial-guess->image s len)
  (row->image
   (map
    (λ (l) (make-pair l guess-letter->image))
    (explode
     (partial-guess->full-guess s len)))))


(define-struct wordle [past current])

; A WordleState (WS) is a (make-wordle [List-of String] String)
; Interpretation: the previous and current guess in a Wordle game

(define WS-START (make-wordle '() ""))
(define WS-TYPE1 (make-wordle '() "C"))
(define WS-TYPE2 (make-wordle '() "CA"))
(define WS-TYPE3 (make-wordle '() "CAT"))
(define WS-TYPE-BAD (make-wordle '() "CAX"))
(define WS-WORD1 (make-wordle (list "CAT") ""))
(define WS-WORD2 (make-wordle (list "CAT" "DAB") ""))
(define WS-WORD3 (make-wordle (list "CAT" "DAB" "BAD") ""))


; key-wordle : WS KeyEvent [List-of String] Nat -> WS
; Processes a keyboard event given a list of valid words and the required word length

; Backspace tests
(check-expect
 (key-wordle WS-START "\b" LITTLE-WORDS 3)
 WS-START)

(check-expect
 (key-wordle WS-TYPE1 "\b" LITTLE-WORDS 3)
 WS-START)

(check-expect
 (key-wordle WS-TYPE2 "\b" LITTLE-WORDS 3)
 WS-TYPE1)

(check-expect
 (key-wordle WS-TYPE3 "\b" LITTLE-WORDS 3)
 WS-TYPE2)

(check-expect
 (key-wordle WS-TYPE-BAD "\b" LITTLE-WORDS 3)
 WS-TYPE2)

(check-expect
 (key-wordle WS-WORD1 "\b" LITTLE-WORDS 3)
 WS-WORD1)

; Return tests
(check-expect
 (key-wordle WS-START "\r" LITTLE-WORDS 3)
 WS-START)

(check-expect
 (key-wordle WS-TYPE1 "\r" LITTLE-WORDS 3)
 WS-TYPE1)

(check-expect
 (key-wordle WS-TYPE2 "\r" LITTLE-WORDS 3)
 WS-TYPE2)

(check-expect
 (key-wordle WS-TYPE3 "\r" LITTLE-WORDS 3)
 WS-WORD1)

(check-expect
 (key-wordle WS-TYPE-BAD "\r" LITTLE-WORDS 3)
 WS-TYPE-BAD)

(check-expect
 (key-wordle WS-WORD1 "\r" LITTLE-WORDS 3)
 WS-WORD1)

; Other keys
(check-expect
 (key-wordle WS-START "C" LITTLE-WORDS 3)
 WS-TYPE1)

(check-expect
 (key-wordle WS-START "c" LITTLE-WORDS 3)
 WS-TYPE1)

(check-expect
 (key-wordle WS-START "1" LITTLE-WORDS 3)
 WS-START)

(check-expect
 (key-wordle WS-TYPE1 "A" LITTLE-WORDS 3)
 WS-TYPE2)

(check-expect
 (key-wordle WS-TYPE1 "A" LITTLE-WORDS 1)
 WS-TYPE1)

(check-expect
 (key-wordle WS-TYPE1 "2" LITTLE-WORDS 3)
 WS-TYPE1)

(check-expect
 (key-wordle WS-TYPE2 "T" LITTLE-WORDS 3)
 WS-TYPE3)

(check-expect
 (key-wordle WS-TYPE2 "X" LITTLE-WORDS 3)
 WS-TYPE-BAD)

(check-expect
 (key-wordle WS-TYPE3 "S" LITTLE-WORDS 3)
 WS-TYPE3)

(define (key-wordle ws ke los len)
  (cond
    [(key=? ke "\b")
     (make-wordle (wordle-past ws)
                  (if (string=? (wordle-current ws) "")
                      ""
                      (substring
                       (wordle-current ws)
                       0
                       (sub1 (string-length (wordle-current ws))))))]
    [(key=? ke "\r")
     (if (string-in-list? (wordle-current ws) los)
         (make-wordle (cons (wordle-current ws) (wordle-past ws)) "")
         ws)]
    [(or
      (number? (string->number ke))
      (= (string-length (wordle-current ws)) len)) ws]
    [else (make-wordle
           (wordle-past ws)
           (string-append (wordle-current ws) (string-upcase ke)))]))

; end-wordle? : WS String Nat -> Boolean
; Determines if the game is over, either because the
; (supplied) maximum number of guesses has been made,
; or the (supplied) correct word had been previously
; guessed

(check-expect (end-wordle? WS-START "BAD" 5) #false)
(check-expect (end-wordle? WS-TYPE1 "BAD" 5) #false)
(check-expect (end-wordle? WS-TYPE2 "BAD" 5) #false)
(check-expect (end-wordle? WS-TYPE3 "BAD" 5) #false)
(check-expect (end-wordle? WS-TYPE-BAD "BAD" 5) #false)
(check-expect (end-wordle? WS-WORD1 "BAD" 5) #false)
(check-expect (end-wordle? WS-WORD1 "BAD" 1) #true)
(check-expect (end-wordle? WS-WORD1 "CAT" 3) #true)

(check-expect (end-wordle? WS-WORD2 "BAD" 5) #false)
(check-expect (end-wordle? WS-WORD2 "BAD" 2) #true)
(check-expect (end-wordle? WS-WORD2 "DAB" 2) #true)

(check-expect (end-wordle? WS-WORD3 "BAD" 5) #true)
(check-expect (end-wordle? WS-WORD3 "ETA" 5) #false)

(define (end-wordle? ws correct-str cap)
  (or (string-in-list? correct-str (wordle-past ws))
      (= cap (length (wordle-past ws)))))

; draw-end-wordle : WS String Nat -> Image
; Produces the game end screen

(check-expect (draw-end-wordle WS-WORD1 "CAT" 5)
              (overlay
               (text (string-append "You won with " (number->string 1) " guesses!") 40 "seagreen")
               (square 500 "solid" "silver")))
(check-expect (draw-end-wordle WS-WORD2 "NO" 3)
              (overlay
               (text (string-append "You lost!") 40 "crimson")
               (square 500 "solid" "lightsteelblue")))
(check-expect (draw-end-wordle WS-WORD3 "BAD" 2)
              (overlay
               (text (string-append "You won with " (number->string 3) " guesses!") 40 "seagreen")
               (square 500 "solid" "silver")))

(define (draw-end-wordle ws correct-str num-guesses)
  (local [; draw-assist : String Color Color -> Image
          ; Provides drawing assistance with
          ; predetermined base image to avoid redundant code
          (define (draw-assist message c1 c2)
            (overlay
             (text message 40 c1)
             (square 500 "solid" c2)))]
    (if (string-in-list? correct-str (wordle-past ws))
        (draw-assist
         (string-append "You won with " (number->string (length (wordle-past ws))) " guesses!")
         "seagreen" "silver")
        (draw-assist "You lost!" "crimson" "lightsteelblue"))))


; draw-wordle : WS String Nat -> Image
; Visualizes the play state of wordle given the correct answer
; and number of available guesses

(check-expect (draw-wordle WS-TYPE2 "CAT" 3)
              (overlay
               (stack/v (list
                         (row->image
                          (list
                           (make-pair "C" guess-letter->image)
                           (make-pair "A" guess-letter->image)
                           (make-pair " " guess-letter->image)))
                         (row->image
                          (list
                           (make-pair " " guess-letter->image)
                           (make-pair " " guess-letter->image)
                           (make-pair " " guess-letter->image)))
                         (row->image (list
                                      (make-pair " " guess-letter->image)
                                      (make-pair " " guess-letter->image)
                                      (make-pair " " guess-letter->image)))))
               (square 500 "solid" (color 147 176 181))))
(check-expect (draw-wordle WS-WORD2 "DAB" 3)
              (overlay
               (stack/v (list
                         (row->image
                          (list
                           (make-pair (make-pair "D" LS-RIGHT) lsp->image)
                           (make-pair (make-pair "A" LS-RIGHT) lsp->image)
                           (make-pair (make-pair "B" LS-RIGHT) lsp->image)))
                         (row->image
                          (list
                           (make-pair (make-pair "C" LS-WRONG) lsp->image)
                           (make-pair (make-pair "A" LS-RIGHT) lsp->image)
                           (make-pair (make-pair "T" LS-WRONG) lsp->image)))
                         (row->image (list
                                      (make-pair " " guess-letter->image)
                                      (make-pair " " guess-letter->image)
                                      (make-pair " " guess-letter->image)))))
               (square 500 "solid" (color 147 176 181))))
(check-expect (draw-wordle WS-START "WOK" 3)
              (overlay 
               (stack/v
                (list
                 (row->image
                  (list
                   (make-pair " " guess-letter->image)
                   (make-pair " " guess-letter->image)
                   (make-pair " " guess-letter->image)))
                 (row->image
                  (list
                   (make-pair " " guess-letter->image)
                   (make-pair " " guess-letter->image)
                   (make-pair " " guess-letter->image)))
                 (row->image
                  (list
                   (make-pair " " guess-letter->image)
                   (make-pair " " guess-letter->image)
                   (make-pair " " guess-letter->image)))))
               (square 500 "solid" (color 147 176 181))))
(check-expect (draw-wordle WS-WORD1 "POT" 5)
              (overlay 
               (stack/v
                (list (row->image
                       (list
                        (make-pair (make-pair "C" LS-WRONG) lsp->image)
                        (make-pair (make-pair "A" LS-WRONG) lsp->image)
                        (make-pair (make-pair "T" LS-RIGHT) lsp->image)))
                      (row->image
                       (list
                        (make-pair " " guess-letter->image)
                        (make-pair " " guess-letter->image)
                        (make-pair " " guess-letter->image)))
                      (row->image
                       (list
                        (make-pair " " guess-letter->image)
                        (make-pair " " guess-letter->image)
                        (make-pair " " guess-letter->image)))
                      (row->image
                       (list
                        (make-pair " " guess-letter->image)
                        (make-pair " " guess-letter->image)
                        (make-pair " " guess-letter->image)))
                      (row->image
                       (list
                        (make-pair " " guess-letter->image)
                        (make-pair " " guess-letter->image)
                        (make-pair " " guess-letter->image)))))
               (square 500 "solid" (color 147 176 181))))

(define (draw-wordle ws answer num-guesses)
  (overlay
   (stack/v (reverse (append
                      (build-list (- num-guesses (length (wordle-past ws)) 1)
                                  (λ (_) (partial-guess->image "" (string-length answer))))
                      ;(map (λ (s1) (partial-guess->image s1 (string-length answer)))
                      ;     (build-list (- num-guesses (length (wordle-past ws)) 1) (λ (_) ""))) 
                      (cons (partial-guess->image (wordle-current ws) (string-length answer))
                            (map row->image
                                 (map (λ (p1)
                                        (map (λ (p2) (make-pair p2 lsp->image)) p1))
                                      (map (λ (e1) (score e1 answer)) (wordle-past ws))))))))
   (square 500 "solid" (color 147 176 181))))


; play : String Nat String -> [List-of String]
; Starts a game of Wordle given the correct answer,
; Uses the number of guesses, the location of the valid
; and guesses then produces those guesses

(define (play correct num-guesses guesses-file)
  (local [(define VALID (read-dictionary guesses-file))
          (define UP-CORRECT (string-upcase correct))]
    (wordle-past
     (big-bang WS-START
       [to-draw (λ (ws) (draw-wordle ws UP-CORRECT num-guesses))]
       [on-key (λ (ws ke) (key-wordle ws ke VALID (string-length UP-CORRECT)))]
       [stop-when (λ (ws) (end-wordle? ws UP-CORRECT num-guesses))
                  (λ (ws) (draw-end-wordle ws UP-CORRECT num-guesses))]))))



; Grid area = (length of supplied word) * number of guesses
; Example: "good" with 8 guesses means there are 32 squares
 
; Run for a tiny, testing game - 4 guesses
(play "bad" 4 "little.txt")

; Run for full-on prior game from the NYT - 6 guesses
(play "index" 6 "nyt.txt")
