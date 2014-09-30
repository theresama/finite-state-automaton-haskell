#| Assignment 1 - Parsing TESTS (due Oct 11, noon) 

***Write the names and CDF accounts for each of your group members below.***
<Name>, <CDF>
<Name>, <CDF>
|#
#lang racket
(require "parsing.rkt")
(require test-engine/racket-tests)

; TODO: WRITE TESTS!!
(check-expect (parse-html-tag "<html></html>") '("<html>" "</html>")) 
(check-expect (parse-html-tag "<hey><html>") '(error "<hey><html>"))

(define parse-hi (make-text-parser "hi"))
(check-expect (parse-hi "hiya!") '("hi" "ya!"))
(check-expect (parse-hi "goodbye hi") '(error "goodbye hi"))

(check-expect (parse-non-special-char "hi") '(#\h "i")) 
(check-expect (parse-non-special-char "<html>") '(error "<html>"))

(check-expect (parse-plain-char "hi") '(#\h "i")) 
(check-expect (parse-plain-char " hello!") '(error " hello!"))

(check-expect ((either parse-plain-char parse-html-tag) "hello") '(#\h "ello"))
(check-expect ((either parse-plain-char parse-html-tag) "<html>hello") '("<html>" "hello")) 
(check-expect ((either parse-plain-char parse-html-tag) "<xml>hello") '(error "<xml>hello"))

(test)