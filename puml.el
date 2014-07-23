;;; puml.el --- Emacs Lisp DSL for PlantUML

;; Author: Thorsten Jolitz <tjolitz AT gmail DOT com>
;; Version: 0.9
;; URL: https://github.com/tj64/puml

;;;; MetaData
;;   :PROPERTIES:
;;   :copyright: Thorsten Jolitz
;;   :copyright-years: 2014+
;;   :version:  0.9
;;   :licence:  GPL 3 or later (free software)
;;   :licence-url: http://www.gnu.org/licenses/
;;   :part-of-emacs: no
;;   :author: Thorsten Jolitz
;;   :author_email: tjolitz AT gmail DOT com
;;   :keywords: emacs org-mode org-bandbook plantuml
;;   :git-repo: https://github.com/tj64/puml
;;   :git-clone: git://github.com/tj64/puml.git
;;   :END:

;;;; Commentary

;; Emacs-lisp domain-specific-language (DSL) for PlantUML.

;; This library is meant for creating PlantUML Scripts programmatically
;; by calling Emacs Lisp functions with arguments (instead of inserting
;; hardcoded strings from a program or directly writing PlantUML
;; syntax). 

;;;; Usage

;; Almost all the real work is done by function `puml--generic'. It
;; assumes that most PlantUML syntax constructs can be expressed like
;; this:

;;    | :pre  | prefix   |
;;    | :1st  | 1st-part |
;;    | :2nd  | 2nd-part |
;;    | :3rd  | 3rd-part |
;;    | :as   | as X     |
;;    | :suf  | suffix   |
;;    | :crlf | crlf     |
  

;; This function is then called by almost all the API functions that
;; normally implement one PlantUML syntax element each. Here is the
;; simple function for implementing stereotypes like '<< human >>':

;; #+begin_src _elisp_
;; (defun* puml-stereotype (code
;;         &key (ldelim "<<") (rdelim ">>") (crlf "\n") ins)
;;   "Return or insert PlantUML stereotype."
;;   (puml--generic :typ 'generic-nospaces
;;                  :1st (puml-sym-or-strg ldelim)
;;                  :2nd (puml-sym-or-strg code)
;;                  :3rd (puml-sym-or-strg rdelim)
;;                  :crlf crlf
;;                  :ins ins))
;; #+end_src

;; A call to this function looks like this:

;; #+begin_src _elisp_
;; (puml-stereotype " human ")
;; #+end_src

;; #+results:
;; : << human >>

;; Here is a little PlantUML script from the PlantUML Language
;; Reference translated to puml.el:

;; #+begin_src _elisp_
;; (puml-pack
;;  (puml-start-activity :crlf nil)
;;  (puml-space (puml-sync-bar "B1") :lead 1)
;;  (puml-activity :nm "Parallel Activity 1")
;;  (puml-activity :crlf nil)
;;  (puml-space (puml-sync-bar "B2") :lead 1)
;;  (puml-newline)
;;  (puml-sync-bar "B1" :crlf nil)
;;  (puml-space
;;   (puml-activity  :nm "Parallel Activity 2") :lead 1)
;;  (puml-activity :crlf nil)
;;  (puml-space (puml-sync-bar "B2") :lead 1)
;;  (puml-newline)
;;  (puml-end-activity))
;; #+end_src

;; #+results:
;; : (*) --> ===B1===
;; : --> "Parallel Activity 1"
;; : --> ===B2===
;; : 
;; : ===B1=== --> "Parallel Activity 2"
;; : --> ===B2===
;; : 
;; : --> (*)

;;;; Known bugs and limitations

;; Currently this library only implements ACTIVITY DIAGRAMS. Patches
;; are welcome to expand this library to all PlantUML diagram types.

;; The complete template for an API function that calls
;; `puml--generic' looks like this:

;; #+begin_src _elisp_

;; (defun* puml-foo (&key typ fmt pre 1st 2nd 3rd as suf (crlf "\n")
;; ins)
;;   "Return or insert PlantUML foo."
;;   (puml--generic :typ 'generic-nospaces
;;                  :fmt generic-nospaces  
;; 		 :pre pre
;; 		 :1st 1st
;; 		 :2nd 2nd
;; 		 :3rd 3rd
;; 		 :as as
;; 		 :suf suf
;; 		 :crlf crlf
;; 		 :ins ins))
;; #+end_src

;; Type 'generic' is the default :typ and need not be given. Both
;; types 'generic' and 'generic-nospaces' have their associated
;; format-strings, thus argument :fmt need not be given for them.

;; Argument :as stands for assignment like 'as B1'. Argument :ins
;; stands for 'insert', if non-nil result is inserted at point instead
;; of returned. In practice it is mostly used for the outermost call
;; to `puml-pack' that wraps a PlantUML script:

;; #+begin_src _elisp_
;;  (puml-pack
;;   (puml-foo ...)
;;   (puml-bar ...)
;;   :ins t)
;; #+end_src

;; Besides `puml-pack', another very important and versatile function
;; is `puml-arrow'. It can be used to create all kinds of arrow syntax
;; used by PlantUML.

;;  | arg     | meaning     | default |
;;  |---------+-------------+---------|
;;  | :len    | length      | 2       |
;;  | :dir    | direction   |         |
;;  | :shaft  | arrow shaft | "-"     |
;;  | :lhead  | leaft head  | ""      |
;;  | :rhead  | right head  | ">"     |
;;  | :lextra | left extra  |         |
;;  | :rextra | right extra |         |

;; Here are two example:

;; #+begin_src _elisp_
;;  (puml-arrow :dir "up" :lhead "<" :rhead nil :lextra "|")
;; #+end_src

;; #+results:
;; : <|-up-

;; #+begin_src _elisp_
;;  (puml-arrow :shaft "." :lhead "1" :rhead "*")
;; #+end_src

;; #+results:
;; : 1..*

;;; Requires

(eval-when-compile (require 'cl))

;;; Variables
;;;; Constants

;;;;; Wrappers

(defconst puml-src-block
  (concat
   "\n#+header: :file %s.%s\n"
   "#+begin_src plantuml\n%s\n#+end_src\n")
  "Plantuml src_block template for use with `format'.
Takes the following string arguments:
 1. Relative filename sans-extension
 2. file extension
 3. scr_block content")

(defconst puml-uml-block
   "@startuml\n%s\n@enduml\n"
  "Plantuml @startuml-@enduml template for use with `format'.
Takes the following string arguments:
 1. UML block content")

;;;;; Common

(defconst puml-title
    "title %s\n"
  "Plantuml title template for use with `format'.
Takes the following string arguments:
 1. Label")

(defconst puml-if-then-else
  (concat
    "if %s then\n"
    "%s"
    "else\n"
    "%s"
    "endif\n")
  "Plantuml if/then/else template for use with `format'.
Takes the following string arguments:
 1. Label
 2. Activities
 3. Activities")

;;;;; Text Markup

(defconst puml-size
    "<size: %s>%s</size>"
  "Plantuml size markup template for use with `format'.
Takes the following string arguments:
 1. Size
 2. Text")

(defconst puml-emph
    "<%s>%s</%s>"
  "Plantuml emphasis markup template for use with `format'.
Takes the following string arguments:
 1. Emphasis type (b,u,i,...)
 2. Text
 3. Emphasis type (b,u,i,...)")

(defconst puml-font
    "<font %s=%s>%s</font>"
  "Plantuml font markup template for use with `format'.
Takes the following string arguments:
 1. Font property (e.g. color)
 2. Property values (e.g. \"#AAAAAA\" or \"colorName\")
 3. Text")

(defconst puml-color
    "<color:%s>%s</color>"
  "Plantuml color markup template for use with `format'.
Takes the following string arguments:
 1. Color (e.g. \"#AAAAAA\" or \"colorName\")
 2. Text")

(defconst puml-image
    "<img:%s>"
  "Plantuml image markup template for use with `format'.
Takes the following string arguments:
 1. Image file (accessible by filesystem)")

;;;;; Generic 

(defconst puml-generic-nospaces
    "%s%s%s"
  "Plantuml generic template with 3 % (no spaces) for `format'.

Usually takes the following string arguments:
 1. <<Object>>
 2. \"\" (empty string)
 3. <<Object>>

but of cource the programmer can pass whatever arguments he wants.")

(defconst puml-generic
    "%s %s %s"
  "Plantuml generic template with 3 % (and spaces) for `format'.

Usually takes the following string arguments:
 1. <<Object>>
 2. Arrow
 3. <<Object>>

but of cource the programmer can pass whatever arguments he wants.")

;;; Functions

;; CL ARGLIST:

;; (var...
;;  &optional (var initform svar)...
;;  &rest var
;;  &key ((keyword var) initform svar)...
;;  &aux (var initform)...)

;; (defun* foo
;;     (a &optional b &rest c d &key e &allow-other-keys)...)

;; (defun* bar (&key a b (c 17)))

;;;; Non-interactive Functions

;;;;; Generic Workhorse
 
(defun* puml--generic (&key (typ 'generic) fmt 1st (2nd (puml-arrow)) 3rd as crlf pre suf ins)
  "Workhorse function for creating PlantUML diagrams."
  (let* ((raw (format
	       ;; format-strg
	       (or fmt (case typ
			 ((generic) puml-generic)
			 ((generic-nospaces) puml-generic-nospaces)
			 (t (error
			     (concat
			      "Argument :fmt required except "
			      "for types generic and "
			      "generic-nospaces")))))
	       ;; objects, e.g. obj1 --> obj2
	       (or 1st "")
	       (or 2nd "")
	       (or 3rd "")))
	 ;; add prefix (needs trailing space)
	 (prefixed (if pre
		       (concat (puml-sym-or-strg pre) raw)
		     raw))
	 ;; add suffix (needs leading space)
	 (suffixed (if suf
		       (concat prefixed (puml-sym-or-strg suf))
		     prefixed))
	 ;; add declaration 'as X'
	 (declared-as (if as
			  (concat suffixed
				  " as "
				  (puml-sym-or-strg as))
			suffixed))
	 ;; maybe add CRLF
	 (final (if crlf
		    (concat declared-as crlf)
		  declared-as)))
    (if ins (insert final) final)))

;;;;; Wrappers and Utilities

(defun puml-wrap (file ext content &optional insert-p)
  "Wrap PlantUML content in Org-mode source-block."
  (let ((src (format puml-src-block file exta content)))
    (if insert-p (insert src) src)))

(defun puml-uml (content &optional insert-p)
  "Wrap PlantUML content in @startuml and @enduml."
  (let ((uml (format puml-uml-block content)))
    (if insert-p (insert uml) uml)))

(defun* puml-pack (&rest args &key ins &allow-other-keys)
  "Pack ARGS into a single string.
Assumes that each (puml-<foo> ...) expression in ARGS returns a
string that ends with a newline \n. If INS is non-nil, insert packed ARGS at point, otherwise return them as string."
  (when (member :ins args) (nbutlast args 2))
  (let ((pack (mapconcat 'identity args "")))
    (if ins (insert pack) pack)))

(defun* puml-space (sym-or-strg &key (lead 0) (trail 0) ins)
  "Return or insert SYM-OR-STRING with LEADing and TRAIL spaces.
With INS, insert results, otherwise return them."
  (let ((strg1 (puml-sym-or-strg sym-or-strg)))
    (dotimes (i lead strg1)
      (setq strg1 (concat " " strg1)))
    (dotimes (i trail strg1)
      (setq strg1 (concat strg1 " ")))
    (if ins (insert strg1) strg1)))

(defun puml-newline (&optional int insert-p)
  "Return or insert 1 or INT newline(s)."
  (let ((count (or int 1))
	newlines)
    (dotimes (i count newlines)
		     (setq newlines
			   (concat "\n" newlines)))
    (if insert-p (insert newlines) newlines)))

;; copied form org-macs.elo
(defun puml-stringp (s)
  "Is S a string with a non-white character?"
  (and (stringp s)
       (save-match-data
	 (string-match "\\S-" s))
       s))

(defun puml-brackets (strg)
  "Wrap STRG in brackets if non-nil."
  (if (puml-stringp strg)
      (concat "[" strg "]")
    strg))

(defun puml-quotes (strg)
  "Wrap STRG in double-quotes if non-nil."
  (if (puml-stringp strg)
      (concat "\"" strg "\"")
    strg))

(defun puml-sym-or-strg (obj &optional as-sym-p)
  "Return OBJ as string if it is a string or sym.
Otherwise return it as is. If optional argument AS-SYM-P is non-nil, return OBJ as symbol instead."
  (cond
   ((puml-stringp obj) (if as-sym-p (intern obj) obj))
   ((symbolp obj) (if as-sym-p obj (symbol-name obj)))
   (t obj)))

;;;;; Text Markup

(defun* puml-size (sym-or-strg &key (size 12) ins)
  "Return or insert SYM-OR-STRING wrapped in SIZE markup.
With INS, insert results, otherwise return them."
  (let* ((strg1 (puml-sym-or-strg sym-or-strg))
	 (res (format puml-size size strg1)))
    (if ins (insert res) res)))

(defun* puml-emph (sym-or-strg &key (type "i") ins)
  "Return or insert SYM-OR-STRING wrapped in emphasis markup.
TYPE determines the type of emphasis. With INS, insert results,
otherwise return them."
  (let* ((strg1 (puml-sym-or-strg sym-or-strg))
	 (res (format puml-emph type strg1 type)))
    (if ins (insert res) res)))

(defun* puml-font (sym-or-strg &key (attr "color") val ins)
  "Return or insert SYM-OR-STRING wrapped in font (color) markup.
With INS, insert results, otherwise return them. ATTR is the font
attribute, VAL is its value."
  (let* ((strg1 (puml-sym-or-strg sym-or-strg))
	 (res (format puml-font attr val strg1)))
    (if ins (insert res) res)))

(defun* puml-color (sym-or-strg &key val ins)
  "Return or insert SYM-OR-STRING wrapped in color markup.
With INS, insert results, otherwise return them. VAL is the color
to use."
  (let* ((strg1 (puml-sym-or-strg sym-or-strg))
	 (res (format puml-color val strg1)))
    (if ins (insert res) res)))

(defun* puml-img (file &key ins)
  "Return or insert FILE wrapped in IMG markup.
With INS, insert results, otherwise return them."
  (let* ((strg1 (puml-sym-or-strg file))
	 (res (format puml-image strg1)))
    (if ins (insert res) res)))

;;;;; Common

(defun* puml-skinparam (key-val-strg &key elem (crlf "\n") ins)
  "Return or insert PlantUML skinparam."
  (puml--generic :typ 'generic-nospaces
		 :pre (puml-space "skinparam" :trail 1)
		 :1st (if elem (puml-sym-or-strg elem) "")
		 :2nd (if elem
			  (puml-space
			   (puml-pack "{\n" key-val-strg "}")
			   :lead 1)
			key-val-strg)
		 :crlf crlf
		 :ins ins))

(defun* puml-key-val (key val &key crlf ins)
  "Return or insert PlantUML key-val pair."
  (puml--generic :typ 'generic-nospaces
		 :1st (puml-sym-or-strg key)
		 :2nd " "
		 :3rd (puml-sym-or-strg val)
		 :crlf crlf
		 :ins ins))

(defun puml-title (label &optional insert-p)
  "Return or insert PlantUML title."
  (let ((title (format puml-title label)))
    (if insert-p (insert title) title)))

(defun puml-if-then-else (label then-activities else-activities &optional insert-p)
  "Return or insert PlantUML if-then-else."
  (let ((branch (format puml-if-then-else
			(puml-quotes label)
			then-activities else-activities)))
    (if insert-p (insert branch) branch)))

(defun* puml-note (text &key (dir "right") of ml pre suf as (crlf "\n") ins)
  "Return or insert PlantUML note."
  (puml--generic :typ 'generic-nospaces
		 :pre (puml-space "note" :trail 1)
		 :1st (if of
			  (puml-pack
			   (puml-sym-or-strg dir)
			   " of "
			   (puml-sym-or-strg of))
			dir)
		 :2nd (if ml
			  (if as
			      (puml-pack
			       " as " (puml-sym-or-strg as) "\n")
			    "\n")
			" : ")
		 :3rd (if ml text (puml-quotes text))
		 :as (unless ml as)
		 :suf (if ml "\nend note")
		 :crlf crlf
		 :ins ins))

(defun* puml-arrow (&key (len 2) dir (shaft "-") (lhead "") (rhead ">") lextra rextra ins)
  "Return or insert PlantUML arrow."
  (cond
   ((not (or (integerp len) (> len 0)))
    (user-error "Argument :len not an integer > 0: %s" len))
   ((and dir (= len 1))
    (user-error "Cannot give :dir for arrow with length 1"))
   (t (let ((arrow (if dir (concat shaft dir) shaft)))
	(dotimes (i (1- len) arrow)
	  (setq arrow (concat arrow shaft)))
	 (when lextra (setq arrow (concat lextra arrow)))
	 (when rextra (setq arrow (concat arrow rextra)))
	 (when lhead (setq arrow (concat lhead arrow)))
	 (when rhead (setq arrow (concat arrow rhead)))
	 (if ins (insert arrow) arrow)))))

(defun puml-left-to-right (&optional insert-p)
  "Change default top-bottom to left-right direction.
To be inserted at top of diagram."
  (let ((strg "left to right direction\n"))
    (if insert-p (insert strg) strg)))

(defun* puml-stereotype (code &key (ldelim "<<") (rdelim ">>") (crlf "\n") ins)
  "Return or insert PlantUML stereotype."
  (puml--generic :typ 'generic-nospaces
		 :1st (puml-sym-or-strg ldelim)
		 :2nd (puml-sym-or-strg code)
		 :3rd (puml-sym-or-strg rdelim)
		 :crlf crlf
		 :ins ins))

(defun* puml-link (from to &key arr lbl pre suf as (crlf "\n") ins)
  "Return or insert PlantUML link."
  (puml--generic :typ 'generic
		 :pre pre
		 :1st from
		 :2nd arr
		 :3rd to
		 :as as
		 :suf (and lbl (concat
				(puml-space ":" :lead 1 :trail 1)
				lbl))
		 :crlf crlf
		 :ins ins))

(defun puml--header-or-footer (text type dir ml crlf ins)
  "Return or insert PlantUML header or footer."
  (puml--generic :typ 'generic-nospaces
		 :pre (let ((dir-strg (when dir
					(puml-sym-or-strg dir))))
			(when (and
			       dir-strg
			       (member
				dir-strg
				(list "center" "left" "right")))
			  (puml-space dir-strg :trail 1)))
		 :1st (puml-pack
		       (if ml
			   (case type
			     ('header "header\n")
			     ('footer "footer\n")
			     (t (error
				 (concat 
				  "Type %s not "
				  "(memq '(header footer)).")
				 type)))
			 (case type
			   ('header (puml-space "header" :trail 1))
			   ('footer (puml-space "footer" :trail 1))
			   (t (error
			       (concat 
				"Type %s not "
				"(memq '(header footer)).")
			       type)))))
		 :2nd (if ml text (puml-quotes text))
		 :3rd (when ml
			(case type
			  ('header "\nendheader")
			  ('footer "\nendfooter")
			  (t (error
			      (concat 
			       "Type %s not "
			       "(memq '(header footer)).")
			      type))))
		 :crlf crlf
		 :ins ins))

(defun* puml-header (text &key dir ml (crlf "\n") ins)
  "Return or insert PlantUML header or footer."
  (puml--header-or-footer text 'header dir ml crlf ins))

(defun* puml-footer (text &key dir ml (crlf "\n") ins)
  "Return or insert PlantUML header or footer."
  (puml--header-or-footer text 'footer dir ml crlf ins))

(defun puml-rotate (&optional ins)
  "Return or insert PlantUML rotate command."
  (if ins (insert "rotate\n") "rotate\n"))

(defun* puml-zoom (&key scl w h ins)
  "Return or insert PlantUML scale command.
W stands for width, H for hight, SCL is either a number or
fraction like 1.5 or 2/3. H and W take precedence of SCL."
  (let ((res (cond
	      ((and w h)
	       (format "scale %s*%s\n" w h))
	      (w (format "scale %s width\n" w))
	      (h (format "scale %s height\n" h))
	      (t (if scl
		     (format "scale %s" scl)
		   (error
		    "You need to specify scale parameters."))))))
    (if ins (insert res) res)))


;;;;; Sequence Diagram
;;;;; Usecase Diagram

(defun* puml-usecase (ucase &key pre suf as (crlf "\n") ins)
  "Return or insert PlantUML stereotype."
  (puml--generic :typ 'generic-nospaces
		 :pre pre
		 :1st 'usecase
		 :2nd " "
		 :3rd ucase
		 :as as
		 :suf suf
		 :crlf crlf
		 :ins ins))

(defun* puml-actor (actr &key pre suf as (crlf "\n") ins)
  "Return or insert PlantUML actor."
  (let ((act (if (and (puml-stringp actr)
		      (> (length (split-string actr " " t)) 1))
		  (format ":%s:" actr)
	       actr))) 
    (puml--generic :typ 'generic-nospaces
		   :pre pre
		   :1st 'actor
		   :2nd " "
		   :3rd act
		   :as as
		   :suf suf
		   :crlf crlf
		   :ins ins)))

(defun* puml-extension (super sub &key pre suf as (crlf "\n") ins)
  "Return or insert PlantUML extension."
  (puml--generic :typ 'generic
		 :pre pre
		 :1st super
		 :2nd (puml-arrow :lhead "<"
				  :rhead nil
				  :lextra "|")
		 :3rd sub
		 :as as
		 :suf suf
		 :crlf crlf
		 :ins ins))

;;;;; Class Diagram
;;;;; Activity Diagram

(defun* puml-start-activity (&key (arr (puml-arrow)) lbl nm as (crlf "\n") (pre "(*)") suf ins)
  "Return or insert PlantUML start-activity."
  (puml--generic :typ 'generic-nospaces
		 :pre (puml-space pre :trail 1)
		 :1st arr
		 :2nd (if lbl
			  (puml-space
			   (puml-brackets (puml-sym-or-strg lbl))
			   :trail 1)
			(if nm " " ""))
		 :3rd (if (symbolp nm) nm (puml-quotes nm))
		 :as as
		 :suf suf
		 :crlf crlf
		 :ins ins))

(defun* puml-activity (&key (arr (puml-arrow)) lbl nm as (crlf "\n") pre suf ins)
  "Return or insert PlantUML activity."
  (puml--generic :typ 'generic-nospaces
		 :pre pre
		 :1st arr
		 :2nd (if lbl
			  (puml-space
			   (puml-brackets (puml-sym-or-strg lbl))
			   :trail 1)
			(if nm " " ""))
		 :3rd (if (symbolp nm) nm (puml-quotes nm))
		 :as as
		 :suf suf
		 :crlf crlf
		 :ins ins))

(defun* puml-end-activity (&key (arr (puml-arrow)) lbl nm as (crlf "\n") pre (suf "(*)") ins)
  "Return or insert PlantUML end-activity."
  (puml--generic :typ 'generic-nospaces
		 :pre pre
		 :1st (if (symbolp nm) nm (puml-quotes nm))
		 :2nd (if nm (puml-space arr :lead 1) arr)
		 :3rd (if lbl
			  (puml-brackets (puml-sym-or-strg lbl))
			"")
		 :as as
		 :suf (puml-space suf :lead 1)
		 :crlf crlf
		 :ins ins))

(defun* puml-sync-bar (code &key (delim "===") (crlf "\n") ins)
  "Return or insert PlantUML synchronization bar."
  (puml--generic :typ 'generic-nospaces
		 :1st (puml-sym-or-strg delim)
		 :2nd (puml-sym-or-strg code)
		 :3rd (puml-sym-or-strg delim)
		 :crlf crlf
		 :ins ins))

(defun* puml-partition (name activities &key bg (crlf "\n") ins)
  "Return or insert PlantUML partition."
  (puml--generic :typ 'generic-nospaces
		 :pre (puml-space "partition" :trail 1)
		 :1st (puml-pack (puml-sym-or-strg name)
				 (if bg " " "\n"))
		 :2nd (when bg
			(puml-pack (puml-sym-or-strg bg) "\n"))
		 :3rd (puml-sym-or-strg activities)
		 :suf "\nend partition"
		 :crlf crlf
		 :ins ins))

;;;;; Component Diagram
;;;;; State Diagram
;;;;; Object Diagram

;;; Run Hooks and Provide

(provide 'puml)

;;; puml.el ends here
