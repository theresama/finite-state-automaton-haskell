#| Assignment 1 - Parsing (due Oct 11, noon)



***Write the names and CDF accounts for each of your group members below.***

<Name>, <CDF>

Theresa Ma 999596343, g2potato

|#

#lang racket

(provide parse-html-tag make-text-parser
         
         parse-non-special-char parse-plain-char
         
         either both star
         
         parse-html is-white? is-special?)



#|
(parse-html-tag str)
  If str starts with "<html>", returns a pair (list "<html>" rest), where
  rest is the part of str after "<html>".
  Otherwise, returns (list 'error "hi"), signifying an error.
> (parse-html-tag "<html></html>")
'("<html>" "</html>")
> (parse-html-tag "<hey><html>")
'(error "<hey><html>")

|#

(define (parse-html-tag str) 
  (if (< (string-length str) 6)
      (list 'error str)
      (if (equal? (substring str 0 6) "<html>")
          (list "<html>" (substring str 6))
          (list 'error str))
      ))

#|
(make-text-parser t)
  Return a parser that tries to read *one* occurrence of t at the
  start of its input.
> (define parse-hi (make-text-parser "hi"))
> (parse-hi "hiya!")
'("hi" "ya!")
> (parse-hi "goodbye hi")
'(error "goodbye hi")
|#

(define (make-text-parser t) 
  (lambda (str) 
    (let ([str-l (string-length t)])
      (if (< (string-length str) str-l)
          (list 'error "firsterror")
          (if (equal? (substring str 0 str-l) t)
              (list t (substring str str-l))
              (list 'error str))))))

#|
(parse-non-special-char str)
  Try to parse *one* non-special character at the start of str.
> (parse-non-special-char "hi")
'(#\h "i")
> (parse-non-special-char "<html>")
'(error "<html>")
|#

(define (parse-non-special-char str) 
  (let ([f-letter (string-ref str 0)])
    (if (is-special? f-letter)
        (list 'error str)
        (list f-letter (substring str 1))
        )))
#|

(parse-plain-char str)
  Try to parse *one* non-special, non-white character at the start of str.
> (parse-plain-char "hi")
'(#\h "i")
> (parse-plain-char " hello!")
'(error " hello!")
|#

(define (parse-plain-char str) 
  (if (or (equal? str "") (is-white? (string-ref str 0)))
      (list 'error str)
      (parse-non-special-char str)
      ))

#|
(is-special? char)
    Checks if the char is a special character.
> (is-special? #\<)
#t
> (is-special? "H")
#f
|#

(define (is-special? char)
  (let ([special '(#\< #\> #\" #\/ #\=)])
    (ormap (lambda (x) (equal? x char)) special)
    ))

#|
(is-white? char)
    Checks if the char is a whitespace.
> (is-white? #\space)
#t
> (is-white? "asdf")
#f

|#
(define (is-white? char)
  (if (equal? #\space char)
      #t
      #f))

#| Parsing Combinators |#

#|
(either parser1 parser2)
  Return a new parser that does the following:
    - Try to apply parser 1; if success, return that result
    - Otherwise, return the result of applying parser 2
> ((either parse-plain-char parse-html-tag) "hello")
'(#\h "ello")
> ((either parse-plain-char parse-html-tag) "<html>hello")
'("<html>" "hello")
> ((either parse-plain-char parse-html-tag) "<xml>hello")
'(error "<xml>hello")
|#

(define (either parser1 parser2) 
  (lambda (x) 
    (let ([result-1 (parser1 x)])
      (if (equal? (first result-1) 'error)
          (parser2 x)
          result-1
          ))))

#|
(both parser1 parser2)
  Return a new parser that does the following:
    - Apply parser1; if failure, return failure
    - Otherwise, apply parser2 to the rest of the string
      not parsed by parser1
    - If failure, emit failure, together with *original* string
    - If success, return (list data rest), where data is a *LIST*
      containing the data parsed by parser1 and parser2, in that order,
      and rest is the part of the string not parsed by either
      parser1 or parser2.
> ((both parse-html-tag parse-plain-char) "<html>hello")
'(("<html>" #\h) "ello")
> ((both parse-html-tag parse-plain-char) "<xml>hello")
'(error "<xml>hello")
> ((both parse-html-tag parse-plain-char) "<html> hello")
'(error "<html> hello")
|#

(define (both parser1 parser2)
  (lambda (x) 
    (let ([result-1 (parser1 x)])
      (if (equal? (first result-1) 'error)
          result-1
          (let ([result-2 (parser2 (second result-1))])
            (if (equal? (first result-2) 'error)
                (list 'error x)
                (list (list (first result-1) (first result-2)) (second result-2))
                )
            ))
      )))

#|
(star parser)
  Return a new parser that tries to parse using parser
  0 or more times, returning as its data a list of *all*
  parsed values. This new parser should be *greedy*: it
  always uses the input parser as many times as it can,
  until it reaches the end of the string or gets an error.
  Note that the new parser never returns an error; even if
  the first attempt at parsing fails, the data returned
  is simply '().
> ((star parse-plain-char) "hi")
'((#\h #\i) "")
> ((star parse-plain-char) "hi there")
'((#\h #\i) " there")
> ((star parse-plain-char) "<html>hi")
'(() "<html>hi")
|#


#| HTML Parsing |#
(define (star parser)
  (lambda (x)
    (let ([result (parser x)])
      (if (equal? (first result) 'error)
          (list '() x)
          (list (append (list (first result)) (first ((star parser) (second result)))) (second ((star parser) (second result)))))
  )))

#|
(parse-html str)
  Parse HTML content at the beginning of str, returning (list data rest),
  where data is the tree representation of the parsed HTML specified in the
  assignment handout, and rest is the rest of str that has not been parsed.

  If the string does not start with a valid html string, return
  (list 'error str) instead.

> (parse-html "<html><body class=\"hello\" >Hello, world!</body></html> Other")
'(("html"
   ()
   ("body"
    (("class" "hello"))
    "Hello, world!"))
  " Other")

> (parse-html "<blAh></blAh>")
'(("blAh"
   ()
   "")
  "")

> (parse-html "<body><p>Not good</body></p>")
'(error "<body><p>Not good</body></p>")

|#

(define (parse-html str) (void))


#|
(parse-opening-tag str)
This is an opening tag parser. It parses an opening tag and 
  returns a pair where the fisrt element is
  the tag name (containing no whitespace or special 
  characters) as a string and the second element is the rest
  of str that wasn't parsed

If the tag name is invalid it returns
  (list 'error str) instead.
|#
(define (parse-opening-tag str) (void))

#|
(parse-attributes str)
This is an attribute parser. It parses the attributes
  of a given opening tag. It returns a list of the
  attributes and their values. Each attribute, value 
  pair is a list containing two strings. 

If the attributes are invalid it returns
  (list 'error str) instead.

> (parse-attributes "class="heading" id="special"")
(("class" "heading")("id" "special"))


|#
(define (parse-attributes str) (void))

#|
(parse-one-attribute str)
Helper function for parse-attributes

> (parse-one-attribute "class="heading"")
("class" "heading")

|#
(define (parse-one-attribute str)(void))


#|theresa
(parse-closing-tag str)
This is a closing tag parser. It finds the matching closing tag and 
  returns all the children elements of the given tag as a string

If the string does not start contain valid open and closing tags, return
  (list 'error str) instead.

- recursively search through str, starting from the end to find </tag-name>
let closing-tag = (string-append "</" tag-name "<")
let last-index-to-search = (string-length str)
let first-index-to-search = (last-index-to-search - (string-length closing-tag))

if (substring first-index-to-search last-index-to-search) = closing-tag
  return (substring 0 first-index-to-search)

else
 findTag(last-index-to-search - 1, first-index-to-search - 1)
|#



(define (parse-closing-tag str tag-name)
  (void)  
  )

#|
(find-tag first-index last-index)
Finds the last occurence of a tag

|#
#|(define (find-tag first-index last-index) 
  (lambda (tag html)
    (if (equal? last-index 0)
        '(error)
        (if (equal? (substring html first-index last-index) tag)
            (substring html 0 first-index)
            ((find-tag (- first-index 1) (- last-index 1)) tag html)
    ))))|#
    
(define (find-tag tag html)
  (let* ([tag-length (string-length tag)]
        [html-length (string-length html)]
        [first-index (- html-length tag-length)])
    (if (equal? (substring html first-index html-length) tag)
        (substring html 0 first-index)
        (find-tag tag (substring html 0 (- html-length 1)))
        )))

;gets text
(define (get-text str) (void))



