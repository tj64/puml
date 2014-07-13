- [puml.el &#x2014; Emacs Lisp DSL for PlantUML](#puml.el-&#x2014;-emacs-lisp-dsl-for-plantuml)
  - [MetaData](#metadata)
  - [Commentary](#commentary)
  - [Usage](#usage)
  - [Known bugs and limitations](#known-bugs-and-limitations)



# puml.el &#x2014; Emacs Lisp DSL for PlantUML<a id="sec-1"></a>

Author: Thorsten Jolitz <tjolitz AT gmail DOT com>
Version: 0.9
URL: <https://github.com/tj64/puml>

## MetaData<a id="sec-1-1"></a>

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

## Commentary<a id="sec-1-2"></a>

Emacs-lisp domain-specific-language (DSL) for PlantUML.

This library is meant for creating PlantUML Scripts programmatically
by calling Emacs Lisp functions with arguments (instead of inserting
hardcoded strings from a program or directly writing PlantUML
syntax). 

## Usage<a id="sec-1-3"></a>

Almost all the real work is done by function \`puml&#x2013;generic'. It
assumes that most PlantUML syntax constructs can be expressed like
this:

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="left" />

<col  class="left" />
</colgroup>
<tbody>
<tr>
<td class="left">:pre</td>
<td class="left">prefix</td>
</tr>


<tr>
<td class="left">:1st</td>
<td class="left">1st-part</td>
</tr>


<tr>
<td class="left">:2nd</td>
<td class="left">2nd-part</td>
</tr>


<tr>
<td class="left">:3rd</td>
<td class="left">3rd-part</td>
</tr>


<tr>
<td class="left">:as</td>
<td class="left">as X</td>
</tr>


<tr>
<td class="left">:suf</td>
<td class="left">suffix</td>
</tr>


<tr>
<td class="left">:crlf</td>
<td class="left">crlf</td>
</tr>
</tbody>
</table>

This function is then called by almost all the API functions that
normally implement one PlantUML syntax element each. Here is the
simple function for implementing stereotypes like '<< human >>':

```lisp
(defun* puml-stereotype (code
	&key (ldelim "<<") (rdelim ">>") (crlf "\n") ins)
  "Return or insert PlantUML stereotype."
  (puml--generic :typ 'generic-nospaces
		 :1st (puml-sym-or-strg ldelim)
		 :2nd (puml-sym-or-strg code)
		 :3rd (puml-sym-or-strg rdelim)
		 :crlf crlf
		 :ins ins))
```

    puml-stereotype

A call to this function looks like this:

```lisp
(puml-stereotype " human ")
```

    << human >>

Here is a little PlantUML script from the PlantUML Language
Reference translated to puml.el:

```lisp
(puml-pack
 (puml-start-activity :crlf nil)
 (puml-space (puml-sync-bar "B1") :lead 1)
 (puml-activity :nm "Parallel Activity 1")
 (puml-activity :crlf nil)
 (puml-space (puml-sync-bar "B2") :lead 1)
 (puml-newline)
 (puml-sync-bar "B1" :crlf nil)
 (puml-space
  (puml-activity  :nm "Parallel Activity 2") :lead 1)
 (puml-activity :crlf nil)
 (puml-space (puml-sync-bar "B2") :lead 1)
 (puml-newline)
 (puml-end-activity))
```

    (*) --> ===B1===
    --> "Parallel Activity 1"
    --> ===B2===
    
    ===B1=== --> "Parallel Activity 2"
    --> ===B2===
    
    --> (*)

## Known bugs and limitations<a id="sec-1-4"></a>

Currently this library only implements ACTIVITY DIAGRAMS. Patches
are welcome to expand this library to all PlantUML diagram types.

The complete template for an API function that calls
\`puml&#x2013;generic' looks like this:

```lisp
(defun* puml-foo (&key typ fmt pre 1st 2nd 3rd as suf (crlf "\n")
ins)
  "Return or insert PlantUML foo."
  (puml--generic :typ 'generic-nospaces
		 :fmt generic-nospaces  
		 :pre pre
		 :1st 1st
		 :2nd 2nd
		 :3rd 3rd
		 :as as
		 :suf suf
		 :crlf crlf
		 :ins ins))
```

    puml-foo

Type 'generic' is the default :typ and need not be given. Both
types 'generic' and 'generic-nospaces' have their associated
format-strings, thus argument :fmt need not be given for them.

Argument :as stands for assignment like 'as B1'. Argument :ins
stands for 'insert', if non-nil result is inserted at point instead
of returned. In practice it is mostly used for the outermost call
to \`puml-pack' that wraps a PlantUML script:

```lisp
(puml-pack
 (puml-key-val "foo" "bar")
 :ins t)
```

Besides \`puml-pack', another very important and versatile function
is \`puml-arrow'. In can be used to create all kinds of arrow syntax
used by PlantUML.

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="left" />

<col  class="left" />

<col  class="left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="left">arg</th>
<th scope="col" class="left">meaning</th>
<th scope="col" class="left">default</th>
</tr>
</thead>

<tbody>
<tr>
<td class="left">:len</td>
<td class="left">length</td>
<td class="left">2</td>
</tr>


<tr>
<td class="left">:dir</td>
<td class="left">direction</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">:shaft</td>
<td class="left">arrow shaft</td>
<td class="left">"-"</td>
</tr>


<tr>
<td class="left">:lhead</td>
<td class="left">leaft head</td>
<td class="left">""</td>
</tr>


<tr>
<td class="left">:rhead</td>
<td class="left">right head</td>
<td class="left">">"</td>
</tr>


<tr>
<td class="left">:lextra</td>
<td class="left">left extra</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">:rextra</td>
<td class="left">right extra</td>
<td class="left">&#xa0;</td>
</tr>
</tbody>
</table>

Here are two example:

```lisp
(puml-arrow :dir "up" :lhead "<" :rhead nil :lextra "|")
```

    <|-up-

```lisp
(puml-arrow :shaft "." :lhead "1" :rhead "*")
```

    1..*
