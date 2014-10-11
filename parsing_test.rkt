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

(check-expect (parse-html "<body><p>Not good</body></p>") '(error "<body><p>Not good</body></p>"))
(check-expect (parse-html "") '(error ""))
(check-expect (parse-html "<html><body><p id=\"main\" class=\"super\">Hey</p></body></html>") '(("html" () (("body" () ("p" (("id" "main") ("class" "super")) "Hey") "")) "")))

(check-expect (find-error '(1 2 3)) #f)
(check-expect (find-error '(1 2 error)) #t)
(check-expect (find-error '((1 2) ("hey" error))) #t)

(check-expect (parse-opening-tag "<body> hey") '("<body>" " hey"))
(check-expect (parse-opening-tag "body> hey") '(error "body> hey"))
(check-expect (parse-opening-tag "<p id=\"main\" class=\"super\">Hey</p>") '("<p id=\"main\" class=\"super\">" "Hey</p>"))
(check-expect (parse-opening-tag "") '(error ""))
(check-expect (parse-opening-tag "<p><p>hey</p></p>") '("<p>" "<p>hey</p></p>"))

(check-expect (parse-name "<body>") '("body" ""))
(check-expect (parse-name "<p id=\"main\" class=\"super\">") '("p" " id=\"main\" class=\"super\""))
(check-expect (parse-name '(error "<body>")) '(error (error "<body>")))

(check-expect (parse-closing "<span class=\"red\">text goes here</span></p><div></div></body>" "p") '("<span class=\"red\">text goes here</span>" "<div></div></body>")) 
(check-expect (parse-closing "<span class=\"red\">text goes here</span><div></div></body>" "p") '(error "<span class=\"red\">text goes here</span><div></div></body>"))
(check-expect (parse-closing "" "h1") '(error ""))
(check-expect (parse-closing "<span class=\"red\">text goes here</span><div></div></body>" "p") '(error "<span class=\"red\">text goes here</span><div></div></body>"))
(check-expect (parse-closing "<p></p>" "p") '(error "<p></p>"))
(check-expect (parse-closing "<p id=\"help\">Hey</p></p><p><p></p></p>" "p") '("<p id=\"help\">Hey</p>" "<p><p></p></p>"))
(check-expect (parse-closing "</p>" "p") '("" ""))




(test)