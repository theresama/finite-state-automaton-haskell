#| Assignment 1 - Parsing (due Oct 11, noon)



***Write the names and CDF accounts for each of your group members below.***

<Name>, <CDF>

Theresa Ma 999596343, g2potato

|#

#lang racket

(provide parse-html-tag make-text-parser        
         parse-non-special-char parse-plain-char   
         either both star parse-list-attribute-value
         parse-html is-white? is-special? parse-closing
         has-children? parse-text parse-attributes parse-name
         parse-opening-tag find-error parse-main)



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
  (if (or (not (string? str)) (< (string-length str) 6))
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
  (if (not (string? str))
      (list 'error str)
      (if (equal? (string-length str) 0)
          (list 'error str)
          (let ([f-letter (string-ref str 0)])
            (if (is-special? f-letter)
                (list 'error str)
                (list f-letter (substring str 1)) 
                )))))
#|

(parse-plain-char str)
  Try to parse *one* non-special, non-white character at the start of str.
> (parse-plain-char "hi")
'(#\h "i")
> (parse-plain-char " hello!")
'(error " hello!")
|#

(define (parse-plain-char str) 
  (if (or (equal? str "") (equal? str #\space) (is-white? (string-ref str 0)) (not (string? str)))
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
  (if (or (equal? #\space char) (equal? " " char))
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

(define (parse-html str)
  (let ([result (parse-main str)])
    (if (find-error result)
        (list 'error str)
        (append(take result (-(length result)1))(list(list-ref result (-(length result)1)))))
    ))

(define (parse-main str)
  (let* ([tag (parse-opening-tag str)]
         [name (parse-name (first tag))]
         [attributes (parse-attributes (second name))]
         [body (parse-closing (second tag) (first name))]
         [first-body (first body)]
         [second-body (second body)])
    (if (or (equal? (first body) 'error) (equal? body ""))
        (list 'error)
        (if (has-children? (first body))
            (let ([parse-first-child (parse-html first-body)])
              (if (equal? parse-first-child 'error)
                  (list 'error str)
                  (list (list (first name) attributes parse-first-child second-body)))) 
            (if (equal? (first body) "")
                (list (list (first name) attributes first-body )  second-body)
                (list (first name) attributes first-body ))
            ))))


#|
(find-error lst)
Returns #t if lst (which could be nested) contains any errors, 
else returns #f/

>(find-error '(1 2 3))
#f
>(find-error '(1 2 error))
#t
>(find-error '((1 2) ("hey" error))
#t

|#
(define (find-error lst)
  (if (> (find-error-helper lst 0) 0)
      #t
      #f
      ))

(define (find-error-helper lst counter)
  (for ([i lst])
    (if (list? i)
        (set! counter (+ (find-error-helper i counter) counter))
        (if (equal? i 'error)
            (set! counter (+ counter 1))
            (set! counter (+ counter 0)))))
  counter)

#|
(parse-opening-tag str)
This is an opening tag parser. It parses an opening tag and 
  returns a pair where the first element is
  the tag name (containing no whitespace or special 
  characters) as a string and the second element is the rest
  of str that wasn't parsed

If the tag name is invalid it returns
  (list 'error str) instead.

>(parse-opening-tag "<body> hey")
'("<body>" " hey")

>(parse-opening-tag "body> hey")
'(error "body> hey")

>(parse-opening-tag "<p id=\"main\" class=\"super\">Hey</p>")
'("<p id=\"main\" class=\"super\">" "Hey</p>")

> (parse-opening-tag "")
'(error "")
|#
(define (parse-opening-tag str)
  (if (> (string-length str) 0)
      (if (equal? (substring str 0 1) "<")
          (let ([html-tag (string-append (first (string-split str ">")) ">")])
            (list html-tag (substring str (string-length html-tag))))
          (list 'error str))
      (list 'error str)))

#|
(parse-name str)
Takes in an opening tag and returns a pair, where the 
first element is the name of the tag, and the second is
a string containing the attributes (not parsed).
Returns (list 'error str) if input is invalid.

>(parse-name "<body>")
'("body" "")

>(parse-name "<p id=\"main\" class=\"super\">")
'("p" " id=\"main\" class=\"super\"")

|#
(define (parse-name str)
  (if (find-error (list str))
      (list 'error str)
      (let ([tag (string-replace (string-replace(first(string-split str " ")) "<" "") ">" "")])
        (list tag (substring str (+ (string-length tag) 1) (- (string-length str) 1)))   
        
        )))



#|
(parse-attributes str)
This is an attribute parser. It parses the attributes
  of a given opening tag. It returns a list of the
  attributes and their values. Each attribute, value 
  pair is a list containing two strings. 

If the attributes are invalid it returns
  (list 'error str) instead.

> (parse-attributes "    name=\"hello  \"    class=\"  no\" ")
'(("name" "hello  ") ("class" "  no"))

> (parse-attributes " id   =\"main\" class=\"super\" ")
'(("id" "main") ("class" "super"))

> (parse-attributes "")
'()
|#
(define (parse-attributes str) 
  (if (find-error (list str))
      (list 'error str)
      (let* ([split-attributes-values (string-split str "\"")] ;returns a list
             [attributes (parse-list-attribute-value split-attributes-values)])
        attributes
        )))

#|
(parse-list-attribute-value lst)
This is a helper function of parse-attributes. It takes the list
  of strings of attributes and values from parse-attributes, splits on 
  quotation marks, and matches attribute/value pairs and returns
  as a list

> (parse-list-attribute-value '("id" "no id" "class" "button"))
'(("id" "no id") ("class" "button"))

> (parse-list-attribute-value '())
'()

|#  

(define (parse-list-attribute-value lst)
  (if (find-error lst)
      (list 'error lst)
      (if (or (empty? lst) (< (length lst) 2))
          '()
          (let ([attribute (string-replace (string-replace (first lst) " " "") "=" "")]
                [value (first (rest lst))])
            (cons (list attribute value) (parse-list-attribute-value (rest (rest lst)))
                  )))))



#|
(parse-closing str tag)
This is a closing tag parser. It's given HTML as a string and 
  the tag name as a string and it finds the matching closing tag and 
  returns all the inner elements of the given tag as a string, as
  the first item of a list, and the second item of the list is the 
  rest of the HTML string that was not parsed.

If the string does not start contain valid open and closing tags, return
  (list 'error str) instead.

> (parse-closing "Hey</p>" "p") 
'("Hey" "")

> (parse-closing "<span id=\"help\">Hey</span></p>" "p")
'("<span id=\"help\">Hey</span>" "")

> (parse-closing "<p id=\"help\">Hey</p></p><p><p></p></p>" "p")
'("<p id=\"help\">Hey</p>" "<p><p></p></p>")

|#

(define (parse-closing str tag)
  (parse-closing-helper str tag 0 0)
  )


#|
(parse-closing-helper str tag counter init)
  A helper function for parse-closing, it takes
  in as a string, the HTML to search through, and
  the name of the tag to search for, a counter 
  to track how many of the same tags 
  have been opened within its body, and init 
  is the first index of str through at which it
  should start searching

  If the correct tag is not found, an error
  is returned along with the HTML that caused the error
|#

(define (parse-closing-helper str tag counter init)
  (if (or (find-error (list str)) (find-error (list tag)) )
      (list 'error str)
      (let* ([closing-tag (string-append "</" tag ">")]
             [opening-tag (string-append "<" tag)]
             [closing-length (string-length closing-tag)]
             [opening-length (string-length opening-tag)]
             )
        (if (< (string-length (substring str init (string-length str))) closing-length)
            (list 'error str)
            (if (equal? (substring str init (+ opening-length init)) opening-tag)
                (parse-closing-helper str tag (+ counter 1) (+ 1 init))
                (if (equal? (substring str init (+ closing-length init)) closing-tag)
                    (if (equal? counter 0)
                        (list (substring str 0 init) (substring str (+ init closing-length)))
                        (parse-closing-helper str tag (- counter 1) (+ 1 init)))
                    (if (= (string-length (substring str init (string-length str))) closing-length)
                        (list 'error str)
                        (parse-closing-helper str tag counter (+ 1 init ))))
                )
            ))
      ))



#|
(parse-body-text str)
Given the body of an HTML element as a string, this function will 
return the body text of the element

> (parse-text "hello")
"hello"

|#
(define (parse-text str) 
  (if (equal? (string-length str) 0)
      str
      (if (equal? (substring (string-replace str " " "") 0 1) "<")
          (list 'error str)
          str
          )
      ))



#|
(parse-body-children str)
Given the body of an HTML element as a string, this function will 
return #t if it has children elements or #f if it only contains text

|#
(define (has-children? str) 
  (let ([html-no-space (string-replace str " " "")])
    (if (equal? (string-length html-no-space) 0)
        #f
        (if (equal? (substring html-no-space 0 1) "<")
            #t
            #f ;this html element only contains text, so call the other function
            ))))
