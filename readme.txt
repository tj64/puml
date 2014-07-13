			  ___________________

				  PUML

			    Thorsten Jolitz
			   tjolitz@gmail.com
			  ___________________


Table of Contents
_________________

1 puml.el --- Emacs Lisp DSL for PlantUML
.. 1.1 MetaData
.. 1.2 Commentary
.. 1.3 Usage
.. 1.4 Known bugs and limitations





1 puml.el --- Emacs Lisp DSL for PlantUML
=========================================

  Author: Thorsten Jolitz <tjolitz AT gmail DOT com>
  Version: 0.9
  URL: [https://github.com/tj64/puml]


1.1 MetaData
~~~~~~~~~~~~

  copyright: Thorsten Jolitz
  copyright-years: 2014+
  version: 0.9
  licence: GPL 3 or later (free software)
  licence-url: http://www.gnu.org/licenses/
  part-of-emacs: no
  author: Thorsten Jolitz
  author_email: tjolitz AT gmail DOT com
  keywords: emacs org-mode org-bandbook plantuml
  git-repo: https://github.com/tj64/puml
  git-clone: git://github.com/tj64/puml.git


1.2 Commentary
~~~~~~~~~~~~~~

  Emacs-lisp domain-specific-language (DSL) for PlantUML.

  This library is meant for creating PlantUML Scripts programmatically
  by calling Emacs Lisp functions with arguments (instead of inserting
  hardcoded strings from a program or directly writing PlantUML
  syntax). 


1.3 Usage
~~~~~~~~~

  Almost all the real work is done by function `puml--generic'. It
  assumes that most PlantUML syntax constructs can be expressed like
  this:

   :pre   prefix   
   :1st   1st-part 
   :2nd   2nd-part 
   :3rd   3rd-part 
   :as    as X     
   :suf   suffix   
   :crlf  crlf     


  This function is then called by almost all the API functions that
  normally implement one PlantUML syntax element each. Here is the
  simple function for implementing stereotypes like '<< human >>':

  ,----
  | (defun* puml-stereotype (code
  | 	&key (ldelim "<<") (rdelim ">>") (crlf "\n") ins)
  |   "Return or insert PlantUML stereotype."
  |   (puml--generic :typ 'generic-nospaces
  | 		 :1st (puml-sym-or-strg ldelim)
  | 		 :2nd (puml-sym-or-strg code)
  | 		 :3rd (puml-sym-or-strg rdelim)
  | 		 :crlf crlf
  | 		 :ins ins))
  `----

  ,----
  | puml-stereotype
  `----

  A call to this function looks like this:

  ,----
  | (puml-stereotype " human ")
  `----

  ,----
  | << human >>
  `----

  Here is a little PlantUML script from the PlantUML Language
  Reference translated to puml.el:

  ,----
  | (puml-pack
  |  (puml-start-activity :crlf nil)
  |  (puml-space (puml-sync-bar "B1") :lead 1)
  |  (puml-activity :nm "Parallel Activity 1")
  |  (puml-activity :crlf nil)
  |  (puml-space (puml-sync-bar "B2") :lead 1)
  |  (puml-newline)
  |  (puml-sync-bar "B1" :crlf nil)
  |  (puml-space
  |   (puml-activity  :nm "Parallel Activity 2") :lead 1)
  |  (puml-activity :crlf nil)
  |  (puml-space (puml-sync-bar "B2") :lead 1)
  |  (puml-newline)
  |  (puml-end-activity))
  `----

  ,----
  | (*) --> ===B1===
  | --> "Parallel Activity 1"
  | --> ===B2===
  | 
  | ===B1=== --> "Parallel Activity 2"
  | --> ===B2===
  | 
  | --> (*)
  `----


1.4 Known bugs and limitations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  Currently this library only implements ACTIVITY DIAGRAMS. Patches
  are welcome to expand this library to all PlantUML diagram types.

  The complete template for an API function that calls
  `puml--generic' looks like this:

  ,----
  | (defun* puml-foo (&key typ fmt pre 1st 2nd 3rd as suf (crlf "\n")
  | ins)
  |   "Return or insert PlantUML foo."
  |   (puml--generic :typ 'generic-nospaces
  | 		 :fmt generic-nospaces  
  | 		 :pre pre
  | 		 :1st 1st
  | 		 :2nd 2nd
  | 		 :3rd 3rd
  | 		 :as as
  | 		 :suf suf
  | 		 :crlf crlf
  | 		 :ins ins))
  `----

  ,----
  | puml-foo
  `----

  Type 'generic' is the default :typ and need not be given. Both
  types 'generic' and 'generic-nospaces' have their associated
  format-strings, thus argument :fmt need not be given for them.

  Argument :as stands for assignment like 'as B1'. Argument :ins
  stands for 'insert', if non-nil result is inserted at point instead
  of returned. In practice it is mostly used for the outermost call
  to `puml-pack' that wraps a PlantUML script:

  ,----
  | (puml-pack
  |  (puml-key-val "foo" "bar")
  |  :ins t)
  `----

  Besides `puml-pack', another very important and versatile function
  is `puml-arrow'. In can be used to create all kinds of arrow syntax
  used by PlantUML.

   arg      meaning      default 
  -------------------------------
   :len     length       2       
   :dir     direction            
   :shaft   arrow shaft  "-"     
   :lhead   leaft head   ""      
   :rhead   right head   ">"     
   :lextra  left extra           
   :rextra  right extra          

  Here are two example:

  ,----
  | (puml-arrow :dir "up" :lhead "<" :rhead nil :lextra "|")
  `----

  ,----
  | <|-up-
  `----

  ,----
  | (puml-arrow :shaft "." :lhead "1" :rhead "*")
  `----

  ,----
  | 1..*
  `----



					Emacs 24.3.1 (Org mode beta_8.3)
