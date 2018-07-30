; Created by Zwe Phone Shein & Adilbek Momunaliev
;
;	~Film Expert System~
;
; This system analyzes your personality and recommends you a movie that you might enjoy.
;
; To run, load the clips file in CLIPS and type (reset). Then type (run).
;
;------------------------------------------------------------------------------------------

; Global variables

(defglobal ?*firstname* = NIL)
(defglobal ?*gender* = NIL)
(defglobal ?*over18* = NIL)
(defglobal ?*conservative* = NIL)
(defglobal ?*tolerate_gore* = NIL)

;Define a class called movie

(defclass MOVIE
    (is-a USER)
    (role concrete)
    (slot moviename)
    (slot over18)
    (slot gender)
    (slot gore)
    (slot is_conservative)
)

; Define a class for the user

(defclass PERSON
    (is-a USER)
    (role concrete)
    (slot firstname)
    (slot person_gender)
    (slot over18)
    (slot conservative)
    (slot tolerate_gore))

; Read the user input first, out of all the other tasks, by declaring salience 1.

(defrule read-input (declare (salience 1))
    =>
    (printout t "What's your name: ")
    (bind ?*firstname* (read))
    (printout t "Male or Female: ")
    (bind ?*gender* (read))
    (printout t "Are you over 18? ")
    (bind ?*over18* (read))
    (printout t "Are you conservative: ")
    (bind ?*conservative* (read))
    (printout t "Can you tolerate blood and gore: ")
    (bind ?*tolerate_gore* (read)))

; Rules for different types of movies.

(defrule suggest_movie
    ?ins <-
    (object (is-a MOVIE)
    (gore yes)
    (is_conservative no)
    (over18 yes)
    (gender male))
    =>
    (send ?ins put-moviename "Red Dragon"))

(defrule suggest_movie2
    ?ins <-
    (object (is-a MOVIE)
    (gore no)
    (is_conservative no)
    (over18 yes)
    (gender male))
    =>
    (send ?ins put-moviename "Fast and the Furious"))

(defrule suggest_movie3
    ?ins <-
    (object (is-a MOVIE)
    (gore no)
    (is_conservative no)
    (over18 no)
    (gender male))
    =>
    (send ?ins put-moviename "The Simpsons"))

(defrule suggest_movie4
    ?ins <-
    (object (is-a MOVIE)
    (gore no)
    (is_conservative yes)
    (over18 yes)
    (gender male))
    =>
    (send ?ins put-moviename "Gran Torino"))

(defrule suggest_movie5
    ?ins <-
    (object (is-a MOVIE)
    (gore no)
    (is_conservative yes)
    (over18 no)
    (gender male))
    =>
    (send ?ins put-moviename "Forrest Gump"))

(defrule suggest_movie6
    ?ins <-
    (object (is-a MOVIE)
    (gore yes)
    (is_conservative yes)
    (over18 yes)
    (gender male))
    =>
    (send ?ins put-moviename "Saving Private Ryan"))

(defrule suggest_movie7
    ?ins <-
    (object (is-a MOVIE)
    (gore yes)
    (is_conservative no)
    (over18 yes)
    (gender female))
    =>
    (send ?ins put-moviename "Silence of the Lambs"))

(defrule suggest_movie8
    ?ins <-
    (object (is-a MOVIE)
    (gore no)
    (is_conservative no)
    (over18 yes)
    (gender female))
    =>
    (send ?ins put-moviename "Sex in the City"))

(defrule suggest_movie9
    ?ins <-
    (object (is-a MOVIE)
    (gore no)
    (is_conservative no)
    (over18 no)
    (gender female))
    =>
    (send ?ins put-moviename "Steven Universe"))

(defrule suggest_movie10
    ?ins <-
    (object (is-a MOVIE)
    (gore no)
    (is_conservative yes)
    (over18 yes)
    (gender female))
    =>
    (send ?ins put-moviename "The Lives of Others"))

(defrule suggest_movie11
    ?ins <-
    (object (is-a MOVIE)
    (gore no)
    (is_conservative yes)
    (over18 no)
    (gender female))
    =>
    (send ?ins put-moviename "Blast from the Past"))

(defrule suggest_movie12
    ?ins <-
    (object (is-a MOVIE)
    (gore yes)
    (is_conservative yes)
    (over18 yes)
    (gender female))
    =>
    (send ?ins put-moviename "Cindrella Man"))

; Create the user with the personality provided.

(defrule create_user
    ?ins <- (object (is-a PERSON))
    =>
    (send ?ins put-firstname ?*firstname*)
    (send ?ins put-person_gender (lowcase ?*gender*))
    (send ?ins put-over18 (lowcase ?*over18*))
    (send ?ins put-conservative (lowcase ?*conservative*))
    (send ?ins put-tolerate_gore (lowcase ?*tolerate_gore*)))

; Create instances/movies of the movie class.

(definstances MOVIE-INSTANCES
    (firstmovie of MOVIE (gore yes)(is_conservative no)(over18 yes)(gender male))
    (secondmovie of MOVIE (gore no)(is_conservative no)(over18 yes)(gender male))
    (thirdmovie of MOVIE (gore no)(is_conservative no)(over18 no)(gender male))
    (fourthmovie of MOVIE (gore no)(is_conservative yes)(over18 yes)(gender male))
    (fifthmovie of MOVIE (gore no)(is_conservative yes)(over18 no)(gender male))
    (sixthmovie of MOVIE (gore yes)(is_conservative yes)(over18 yes)(gender male))
    (seventhmovie of MOVIE (gore yes)(is_conservative no)(over18 yes)(gender female))
    (eighthmovie of MOVIE (gore no)(is_conservative no)(over18 yes)(gender female))
    (ninthmovie of MOVIE (gore no)(is_conservative no)(over18 no)(gender female))
    (tenthmovie of MOVIE (gore no)(is_conservative yes)(over18 yes)(gender female))
    (eleventhmovie of MOVIE (gore no)(is_conservative yes)(over18 no)(gender female))
    (twelvthmovie of MOVIE (gore yes)(is_conservative yes)(over18 yes)(gender female))
)

; Create the person object/instance.

(definstances PERSON-INSTANCES
    (user of PERSON))

; Choose the movie appropriate for the user depending on its personality and rules.

(defrule choose_movie (declare (salience -1))
    (object (is-a PERSON)(firstname ?fn)(over18 ?age)(conservative ?con)(tolerate_gore ?go)(person_gender ?gen))
    (object (is-a MOVIE)(moviename ?mv)(is_conservative ?con)(over18 ?age)(gore ?go)(gender ?gen))
    =>
    (printout t crlf ?fn ", I would recommend the movie " ?mv " based on your personality." crlf)
)
