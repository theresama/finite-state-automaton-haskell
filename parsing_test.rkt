#| Assignment 1 - Parsing TESTS (due Oct 11, noon) 

***Write the names and CDF accounts for each of your group members below.***
<Name>, <CDF>
Theresa Ma 999596343, g2potato
|#
#lang racket
(require "parsing.rkt")
(require test-engine/racket-tests)

; TODO: WRITE TESTS!!
(check-expect (parse-html-tag "") '(error ""))
(check-expect (parse-html-tag 1234) '(error 1234))
(check-expect (parse-html-tag "<html></html>") '("<html>" "</html>")) 
(check-expect (parse-html-tag "<hey><html>") '(error "<hey><html>"))

(define parse-null (make-text-parser ""))
(check-expect (parse-null "hiya!") '("" "hiya!"))

(define parse-hi (make-text-parser "hi"))
(check-expect (parse-hi "hiya!") '("hi" "ya!"))
(check-expect (parse-hi "goodbye hi") '(error "goodbye hi"))

(check-expect (is-special? #\<) #t) 
(check-expect (is-special? "H") #f)
(check-expect (is-special? "") #f)

(check-expect (is-white? #\space) #t) 
(check-expect (is-white? "asdf") #f)
(check-expect (is-white? "") #f)

(check-expect (parse-non-special-char "hi") '(#\h "i")) 
(check-expect (parse-non-special-char 1234) '(error 1234)) 
(check-expect (parse-non-special-char "<html>") '(error "<html>"))
(check-expect (parse-non-special-char "") '(error "")) 

(check-expect (parse-plain-char "hi") '(#\h "i")) 
(check-expect (parse-plain-char " hello!") '(error " hello!"))
(check-expect (parse-plain-char "") '(error ""))
(check-expect (parse-plain-char #\space) '(error #\space))
(check-expect (parse-plain-char " ") '(error " "))

(check-expect ((either parse-plain-char parse-html-tag) "hello") '(#\h "ello"))
(check-expect ((either parse-html-tag parse-html-tag) "<html>") '("<html>" ""))
(check-expect ((either parse-html-tag parse-html-tag) "hello") '(error "hello"))
(check-expect ((either parse-plain-char parse-html-tag) "<html>hello") '("<html>" "hello")) 
(check-expect ((either parse-plain-char parse-html-tag) "<xml>hello") '(error "<xml>hello"))
(check-expect ((either (either parse-non-special-char parse-plain-char) parse-html-tag) "why") '(#\w "hy"))
(check-expect ((either (either parse-html-tag parse-plain-char) parse-non-special-char) " why") '(#\space "why"))

(check-expect ((both parse-html-tag parse-plain-char) "<html>hello") '(("<html>" #\h) "ello"))
(check-expect ((both parse-html-tag parse-non-special-char) "<html> ") '(("<html>" #\space) ""))
(check-expect ((both parse-html-tag parse-plain-char) "<html> ") '(error "<html> "))
(check-expect ((both parse-html-tag parse-non-special-char) "<html> hello") '(("<html>" #\space) "hello"))
(check-expect ((both parse-html-tag parse-plain-char) "<xml>hello") '(error "<xml>hello"))
(check-expect ((both parse-html-tag parse-plain-char) "<html> hello") '(error "<html> hello"))

(check-expect ((star parse-plain-char) "hi") '((#\h #\i) ""))
(check-expect ((star parse-plain-char) "      ") '(() "      "))
(check-expect ((star parse-plain-char) "hi there") '((#\h #\i) " there"))
(check-expect ((star parse-plain-char) "<html>hi") '(() "<html>hi"))
(check-expect ((star (either parse-plain-char parse-html-tag)) "hi") '((#\h #\i) ""))
(check-expect ((star (either parse-plain-char parse-html-tag)) "<html><html><html>no") '(("<html>" "<html>" "<html>" #\n #\o) ""))
(check-expect ((star (either (both parse-html-tag parse-plain-char) parse-html-tag)) "hi") '(() "hi"))
(check-expect ((star (either parse-non-special-char (both parse-html-tag parse-plain-char))) " <html>asdf") '((#\space ("<html>" #\a) #\s #\d #\f) ""))


(test)