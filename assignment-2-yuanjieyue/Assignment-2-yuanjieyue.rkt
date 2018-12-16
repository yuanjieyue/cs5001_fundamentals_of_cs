;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Assignment-2-yuanjieyue) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "guess.rkt" "teachpack" "htdp") (lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "guess.rkt" "teachpack" "htdp") (lib "draw.rkt" "teachpack" "htdp")) #f)))
;;> TOTAL: 118/139
;;> Stick to beginner lang! Change it back now. DO NOT USE ADVANCED STUDENT LANG
;;> Reread the style guide and learn when it is appropriate to use aliases.
;;> Watch your grammar and spelling. Are try to use clear simple english in your
;;> data definitions and purpose statements. 

;;;; Assignment 2


;;;; Problem 1

;;> All 3 of the above data definitions are unnecessary. How is Boolean-1,
;;> Boolean-2, or Xor different from a Boolean? Why did you create new types?
;;> You could have used Boolean to represent your inputs and your outputs

;;;; Signature
;; logical-xor: Boolean Boolean -> Boolean

;;;; Purpose
;; GIVEN: two booleans
;;> -1 this is not plain english. What is a boolean-1??
;; RETURN: a boolean 
;;> Try to write purpose statements in your own words

;;;; Examples
;; (logical-xor #true #true) => #false
;; (logical-xor #true #false)=> #true
;; (logical-xor #false #true) => #true
;; (logical-xor #false #false) => #false

;;;; Function Definition
(define (logical-xor b1 b2)
  (or (and b1 (not b2))
      (and (not b1) b2)))

;;> Although your implementation is correct, there is a better way to write this
;;> 1. An 'else true' or 'else false' indicates here that the cond is
;;> unnecessary. Can you think of a way to remove the cond?
;;> 2. This function could be written using only logical operators (not, and,
;;> or, ..). Can you do it?

;;;; Tests
(check-expect (logical-xor #true #true)
              #false)
(check-expect (logical-xor #true #false)
              #true)
(check-expect (logical-xor #false #true)
              #true)
(check-expect (logical-xor #false #false)
              #false)



;;;; Problem 2

;;;; Data Definition
;; A MaybeNumber is one of
;; - Number
;; - 'undefined
;; INTERP: represents a maybe number
;;> INTERPs are for the data you are defining NOT for the future function you
;;> plan to write. The fact that you are going to add MaybeNumbers later has
;;> nothing to do with the data definition.

;;> -9 Missing MaybeNumber deconstructor template
;;;; Deconstructor Template
;; maybenumber-fn : maybernumber -> ???
#; (define (maybenumber-fn maybenumber)
     (cond
       [(number? maybenumber) ...]
       [(and (symbol? maybenumber)
             (symbol=? maybenumber 'undefined)) ...]))

;;;; Signature
;; addition-on-maybenumbers: MaybeNumber MaybeNumber -> MaybeNumber

;;;; Purpose
;; GIVEN: two maybenumbers
;; RETURN: a number if both inputs are numbers
;;         or 'undefined if either of inputs are 'undefined
;;> Could be more clear. "..if *both* inputs are numbers..", ".. if *either of*
;;> the inputs are 'undefined.." 

;;;; Examples
;; (addition-on-maybenumbers 2 3.5) => 5.5
;; (addition-on-maybenumbers 2 'undefined) => 'undefined
;; (addition-on-maybenumbers 'undefined 3.5) => 'undefined
;; (addition-on-maybenumbers 'undefined 'undefined) => 'undefined

;;;; Function Definition
(define (addition-on-maybenumbers maybenumber-1 maybenumber-2)
  (cond
    [(both-number? maybenumber-1 maybenumber-2) 
     (+ maybenumber-1 maybenumber-2)]
    [(either-undefined? maybenumber-1 maybenumber-2)
     'undefined]))

;;> The structure of your code should follow the structure of your data.
;;> You should explicitly check if the input is a symbol and if it is equal to
;;> 'undefined.


;;;; Signature
;; both-number? : MaybeNumbers MaybeNumbers -> Boolean

;;;; Purpose
;; GIVEN: two maybe numbers
;; RETURN: true if the two maybe numbers are nubmers
;;         false if they are 'undefined

;;;; Examples
;; (both-number? 2 3) => #true
;; (both-number? 2 'undefined) => #false
;; (both-number? 'undefined 3) => #false
;; (both-number? 'undefined 'undefined) => #false

;;;; Function Definition
(define (both-number? m1 m2)
  (and (number? m1)
       (number? m2)))

;;;; Tests
(check-expect (both-number? 2 2)
              #true)
(check-expect (both-number? 2 'undefined)
              #false)
(check-expect (both-number? 'undefined 3)
              #false)
(check-expect (both-number? 'undefined 'undefined)
              #false)


;;;; Signature
;; either-undefined? : MaybeNumber MaybeNumber -> Boolean

;;;; Purpose
;; GIVEN: two maybe numbers
;; RETURN: true if either of the two maybe numbers is 'undefined
;;         false otherwise

;;;; Examples
;; (either-undefined? 2 2) => #false
;; (either-undefined? 'undefined 2) => #true
;; (either-undefined? 3 'undefined) => #true
;; (either-undefined? 'undefined 'undefined) => #true


;;;; Function Definition
(define (either-undefined? u1 u2)
  (or (and (symbol? u1)
           (symbol=? u1 'undefined))
      (and (symbol? u2)
           (symbol=? u2 'undefined))))

;;;; Tests
(check-expect (either-undefined? 2 2)
              #false)
(check-expect (either-undefined? 'undefined 2)
              #true)
(check-expect (either-undefined? 3 'undefined)
              #true)
(check-expect (either-undefined? 'undefined 'undefined)
              #true)

;;> One task one function. You could have used helper functions here to check if
;;> both inputs are numbers or if either of the inputs are the symbol 'undefined

;;;; Tests
(check-expect (addition-on-maybenumbers 2 3.5) 5.5)
(check-expect (addition-on-maybenumbers 2 'undefined) 'undefined)
(check-expect (addition-on-maybenumbers 'undefined 3.5) 'undefined)
(check-expect (addition-on-maybenumbers 'undefined 'undefined) 'undefined)


;;;; Problem 3


;;;; Data Definition
;; A Hour is a NonNegInteger
;; WHEER: 0 <= Hour <= 23
;; INTERP: represents the hour of the current time

;; A Minute is a NonNegInteger
;; WHERE: 0 <= Minute <= 59
;; INTERP: represents the minute of the current time

;; A UpdateTime is a String
;;> *String (Capitalize!!)

;; WHERE: the String follows the format "Hour:Minute"

;;> Use plain english. Your where clause could be:
;;> WHERE: the String follows the format "Hour:Minute"
;; INTERP: represents an update time
;;> -1 Again, your data definitions are not dependant on any of your future
;;> functions. The INTERP should describe what this string represents (which is
;;> simply a time). The fact that it may be the result of adding a minute to a
;;> a different time has NOTHING to do with the data definition. 

;;;; Signature
;; update-time: Hour Minute -> UpdateTime

;;;; Purpose
;; GIVEN: the hour and the minute of the current time
;;> Grammar! Only a minute? I am giving you THE hour and THE minute value for
;;> the current time. 
;; RETURN: a string represents the updated time after one minuter hase
;;         been added
;;> -1 Grammar. What is an update time? Use clear language. For example:
;;> RETURN: a string representing the updated time after one minute has been
;;> added

;;;; Examples
;;> -1 missing some cases. What about when either the input hour or minute is 0?
;; (update-time 0 0) => "0:1"
;; (update-time 12 4) => "12:5"
;; (update-time 5 59) => "6:0"
;; (update-time 23 59) => "0:0"



;;;; Function Definition
(define (update-time hour minute)
  (string-append
   (update-hour hour minute)
   ":"
   (update-minute minute)))
;;> Nice use of helper functions; your function looks clean. However, you still
;;> must follow the design recipe for helper functions too. 

;;> -1 Missing Signature, Purpose, Examples, and Tests

(define MAXHOUR 23)
(define MAXMINUTE 59)
;;;; Signature
;; update-minute : Minute -> Minute

;;;; Purpose
;; GIVEN: the minute of the current time 
;; RETURN: the updated minute after one minute added

;;;; Examples
;; (update-minute 0) => "1"
;; (update-minute 35) => "36"
;; (update-minute 59) => "0"
(define (update-minute minute)
  (cond
    [(< minute MAXMINUTE) (number->string (+ minute 1))]
    [(= minute MAXMINUTE) "0"]))

;;;; Tests
(check-expect (update-minute 0)
              "1")
(check-expect (update-minute 35)
              "36")
(check-expect (update-minute 59)
              "0")



;;> -1 Missing Signature, Purpose, Examples, and Tests
;;;; Signature
;; update-hour : Hour Minute -> Hour

;;;; Purpose
;; GIVEN: the hour and the minute of the current time
;; RETURN: the updated hour after one minute added

;;;; Examples
;; (update-hour 0 0) => "0"
;; (update-hour 0 59) => "1"
;; (update-hour 10 23) => "10"
;; (update-hour 10 59) => "11"
;; (update-hour 23 23) => "23"
;; (update-hour 23 59) => "0"

;;;; Function Definition

(define (update-hour hour minute)
  (cond
    [(< minute MAXMINUTE) (number->string hour)]
    [(and (< hour MAXHOUR) (= minute MAXMINUTE)) (number->string (+ hour 1))]
    [(and (= hour MAXHOUR) (= minute MAXMINUTE)) "0"]))

;;;; Tests
(check-expect (update-hour 0 0)
              "0")
(check-expect (update-hour 0 59)
              "1")
(check-expect (update-hour 10 23)
              "10")
(check-expect (update-hour 10 59)
              "11")
(check-expect (update-hour 23 23)
              "23")
(check-expect (update-hour 23 59)
              "0")



;;> the numbers 23 and 59 could have been made constants
   
;;;; Tests
;;> -1 what about the cases when either the hour or minute is 0? Make sure to
;;> test all cases.
(check-expect (update-time 0 0) "0:1")
(check-expect (update-time 12 4) "12:5")
(check-expect (update-time 5 59) "6:0")
(check-expect (update-time 23 59) "0:0")
(check-expect (update-time 12 44) "12:45")


;;;; Problem 4

;;;; Data Definition
;; A LetterGrade is one of
;; - 'A
;; - 'A-
;; - 'B+
;; - 'B
;; - 'B-
;; - 'C+
;; - 'C
;; - 'C-
;; - 'D+
;; - 'D
;; - 'D-
;; - 'F
;; INTERP:represents a letter grade for a course of a student
;;> For all courses?


;;;; Deconstructor Template
;; letter-grade-fn: LetterGrade -> ???
#; (define (letter-grade-fn letter-grade)
     (cond
       [(symbol=? letter-grade 'A) ...]
       [(symbol=? letter-grade 'A-) ...]
       [(symbol=? letter-grade 'B+) ...]
       [(symbol=? letter-grade 'B) ...]
       [(symbol=? letter-grade 'B-) ...]
       [(symbol=? letter-grade 'C+) ...]
       [(symbol=? letter-grade 'C) ...]
       [(symbol=? letter-grade 'C-) ...]
       [(symbol=? letter-grade 'D+) ...]
       [(symbol-? letter-grade 'D) ...]
       [(symbol=? letter-grade 'D-) ...]
       [(symbol=? letter-grade 'F) ...]))

;;A GPA is a one of
;; - "4.000"
;; - "3.667"
;; - "3.333"
;; - "3.000"
;; - "2.667"
;; - "2.333"
;; - "2.000"
;; - "1.667"
;; - "1.333"
;; - "1.000"
;; - "0.667"
;; - "0.000"

;;;; Deconstructor Template
;; gpa-fn : GPA -> ???
#; (define (gpa-fn gpa)
     (cond
       [(string=? "4.000" gpa) ...]
       [(string=? "3.667" gpa) ...]
       [(string=? "3.333" gpa) ...]
       [(string=? "3.000" gpa) ...]
       [(string=? "2.667" gpa) ...]
       [(string=? "2.333" gpa) ...]
       [(string=? "2.000" gpa) ...]
       [(string=? "1.667" gpa) ...]
       [(string=? "1.333" gpa) ...]
       [(string=? "1.000" gpa) ...]
       [(string=? "0.667" gpa) ...]
       [(string=? "0.000" gpa) ...]))
       
;; INTERP: represents the grade-point average of a course of a student
;;> Normally, a GPA is not an itemization. You could have a GPA of 0.01 or
;;> 3.99, or 2.56. It can fall under the category of NonNegReal.
;;> However, since you made it an itemization, you should have included a
;;> deconstructor template.
;;> -1 Incomplete data definition

;;;; Signature
;; letter-grade->GPA: LetterGrade -> GPA
;;> -1 *LetterGrade. Data definitions must be CamelCase

;;;; Purpose
;; GIVEN: a letter-grade of a course
;; RETURN: the corresponding GPA

;;;; Examples
;;> -1 Missing some examples here
;;(letter-grade->GPA 'A) => "4.000"
;;(letter-grade->GPA 'A-) => "3.667"
;;(letter-grade->GPA 'B+) => "3.333"
;;(letter-grade->GPA 'B) => "3.000"
;;(letter-grade->GPA 'B-) => "2.667"
;;(letter-grade->GPA 'C+) => "2.333"
;;(letter-grade->GPA 'C) => "2.000"
;;(letter-grade->GPA 'C-) => "1.667"
;;(letter-grade->GPA 'D+) => "1.333"
;;(letter-grade->GPA 'D) => "1.000"
;;(letter-grade->GPA 'D-) => "0.667"
;;(letter-grade->GPA 'F) => "0.000"

;;;; Function Definition
(define (letter-grade->GPA letter-grade)
  (cond
    [(symbol=? letter-grade 'A) "4.000"]
    [(symbol=? letter-grade 'A-) "3.667"]
    [(symbol=? letter-grade 'B+) "3.333"]
    [(symbol=? letter-grade 'B) "3.000"]
    [(symbol=? letter-grade 'B-) "2.667"]
    [(symbol=? letter-grade 'C+) "2.333"]
    [(symbol=? letter-grade 'C) "2.000"]
    [(symbol=? letter-grade 'C-) "1.667"]
    [(symbol=? letter-grade 'D+) "1.333"]
    [(symbol=? letter-grade 'D) "1.000"]
    [(symbol=? letter-grade 'D-) "0.667"]
    [(symbol=? letter-grade 'F) "0.000"]))


;;;; Tests
(check-expect (letter-grade->GPA 'A) "4.000")
(check-expect (letter-grade->GPA 'A-) "3.667")
(check-expect (letter-grade->GPA 'B+) "3.333")
(check-expect (letter-grade->GPA 'B) "3.000")
(check-expect (letter-grade->GPA 'B-) "2.667")
(check-expect (letter-grade->GPA 'C+) "2.333")
(check-expect (letter-grade->GPA 'C) "2.000")
(check-expect (letter-grade->GPA 'C-) "1.667")
(check-expect (letter-grade->GPA 'D+) "1.333")
(check-expect (letter-grade->GPA 'D) "1.000")
(check-expect (letter-grade->GPA 'D-) "0.667")
(check-expect (letter-grade->GPA 'F) "0.000")

;;;; Problem 5

;;;; Data Definition
;;> Is this data def necessary? How is a FileSize different from a NonNegReal?

;; A Unit is one of
;; - 'B ;;> -1 What is this? And where is 'YB?
;; - 'KB
;; - 'MB
;; - 'GB
;; - 'TB
;; - 'PB
;; - 'EB
;; - 'ZB
;; INTERP: represents the unit that is used to measure a file

;;;; Deconstructor Template
;; unit-fn: Unit -> ???
#; (define (unit-fn unit)
     (cond
       [(symbol=? unit 'KB) ...]
       [(symbol=? unit 'MB) ...]
       [(symbol=? unit 'GB) ...]
       [(symbol=? unit 'TB) ...]
       [(symbol=? unit 'PB) ...]
       [(symbol=? unit 'EB) ...]
       [(symbol=? unit 'ZB) ...]
       [(symbol=? unit 'YB) ...]))

;;> Is this data def necessary? How is a Byte different from a NonNegReal?

;;;; Signature
;; file-size->byte: NonNegReal Unit -> NonNegReal
;;> *FileSize *Unit *Byte
;;> -1 Data defs are always in CamelCase.

;;;; Purpose
;; GIVEN: a non negative real represents the file size and a unit
;; RETURN: a non negative real represents the total file size in bytes

;;> this purpose statement could be more descrptive. Example: RETURN: the total
;;> file size in bytes

;;;; Examples
;; (file-size->byte 12 'KB) => 12288
;; (file-size->byte 12 'MB) => 12582912
;; (file-size->byte 12 'GB) => 12884901888
;; (file-size->byte 12 'TB) => 13194139533312
;; (file-size->byte 12 'PB) => 13510798882111488
;; (file-size->byte 12 'EB) => 13835058055282163712
;; (file-size->byte 12 'ZB) => 14167099448608935641088
;; (file-size->byte 12 'YB) => 14507109835375550096474112


;;;; Function Definition
(define (file-size->byte file-size unit)
  (cond
       [(symbol=? unit 'KB) (* file-size 1024)]
       [(symbol=? unit 'MB) (* file-size 1048576)]
       [(symbol=? unit 'GB) (* file-size 1073741824)]
       [(symbol=? unit 'TB) (* file-size 1099511627776)]
       [(symbol=? unit 'PB) (* file-size 1125899906842624)]
       [(symbol=? unit 'EB) (* file-size 1152921504606846976)]
       [(symbol=? unit 'ZB) (* file-size 1180591620717411303424)]
       [(symbol=? unit 'YB) (* file-size 1208925819614629174706176)]))

;;;; Tests
(check-expect (file-size->byte 12 'KB) 12288)
(check-expect (file-size->byte 12 'MB) 12582912)
(check-expect (file-size->byte 12 'GB) 12884901888)
(check-expect (file-size->byte 12 'TB) 13194139533312)
(check-expect (file-size->byte 12 'PB) 13510798882111488)
(check-expect (file-size->byte 12 'EB) 13835058055282163712)
(check-expect (file-size->byte 12 'ZB) 14167099448608935641088)
(check-expect (file-size->byte 12 'YB) 14507109835375550096474112)







                                 





































