;;; pasc-mode.el -- Pascal mode -- Major mode for editing Pascal programs.
;; Copyright (c) 1990 - 2001 Inge Frick

;;; LCD Archive Entry:
;;   pasc-mode|Inge Frick|inge@kth.nada.se|Pascal mode
;;   |$Date 01/11/20 $|$Revision: 1.13 $|

;; Author: Inge Frick (inge@nada.kth.se)
;; Created: June 1990
;; Version: 1.13
;; Keywords: pascal languages
;; Last edited: Tue Nov 20 16:52:57 2001 by Inge Frick <inge@nada.kth.se>

;; This pascal-mode has borrowed and changed a lot from the pascal-mode by
;; Glen J. Ditchfield (gjditchfield@violet.uwaterloo.ca), some ideas come
;; from the pascal-mode by Magnus Hyllander and
;; Bjorn Gronvall (bjorng@nada.kth.se). Things have also been borrowed
;; from the pascal-mode by Espen Skoglund (espensk@stud.cs.uit.no).

;; This file is not part of GNU Emacs (yet).
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  Neither the author nor any distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; this file, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;;; Commentary:
;;
;; Pascal-mode can do the following:
;; Indent Pascal code correctly.  The indentation style can be changed.
;; Movement, marking and deletion by procedure, sentence and expression.
;; Templates for insertion of all syntactic constructs.
;; Some further commands.  See the documentation string for `pascal-mode'.

;;; Change Log:
;;
;; Version 1.13
;;	Minor changes in comments and comment strings.
;;	Switched meaning for pascal-return so that by default
;;	<nl> indents and <cr> doesn't.
;;	Changed custom method for indents. Indents can now be absolute or
;;	a multiple of new variabel pascal-default-indent. Coversion from
;;	from multiple to absolute is done by new function pascal-get-indent.
;;	(pascal-indent-end, pascal-indent-relative, pascal-add-indent,
;;	pascal-indent-in-do, pascal-block-follow) use pascal-get-indent.
;;	(pascal-indent-else, pascal-indent-then, pascal-indent-in-if):
;;	change pascal-match-if to pascal-find-matching-if.
;;	New variable pascal-start-ignored-line.
;;	pascal-white-forward: use pascal-start-ignored-line intead of
;;	calling pascal-ignore-line.
;;	pascal-do-fill: bind fill-column temprorarily instead of setting
;;	and the resetting it.
;;	pascal-transpose-procedures, pascal-transpose-sentences,
;;	pascal-transpose-sexps: argument is now optional.
;;	pascal-upcase-keywords, pascal-capitalize-standardnames are
;;	now marked interactive "*r".
;;	Moved pascal-indent-fn and pascal-do-calculate-indent forward
;;	a bit.
;;	pascal-find-dekl: use regexp-opt.
;;
;; Version 1.12
;;      Make pascal ignore line that start matching pascal-dont-indent.
;;      (pascal-search-ignore-backward, pascal-search-ignore-forward): new
;;      functions that are called instead of re-search-backward and
;;      re-search-forward.
;;      (pascal-beginnify-region, pascal-search-back-skip-general-paren,
;;       pascal-search-backward, pascal-in-paren, pascal-skip-comment-backward,
;;       pascal-point-in-comment-or-string): use pascal-search-ignore-backward
;;      instead of re-search-backward.
;;      (pascal-comment-out-lines, pascal-set-comment-delim, pascal-end-snts,
;;       pascal-search-forward, pascal-set-bracket, pascal-skip-comment-forward
;;       pascal-point-in-comment-or-string): use pascal-search-ignore-forward
;;      instead of re-search-forward.
;;      (pascal-white-forward, pascal-white-backward): ignore dont-indet lines.
;;      (pascal-indent-fn): no indentation for lines starting with linefeed.
;;
;; Version 1.11
;;      Variables are now customizable.
;;      pascal-extra-keywords, pascal-keywords, pascal-extra-standardnames
;;      and pascal-standardnames are now lists of strings and not regular
;;      expressions.
;;      regexp-opt is used to set pascal-relative-indents,
;;      pascal-indent-parens, pascal-font-lock-keywords,
;;      pascal-sentence-left-delim, pascal-contain-sentence-left-delim,
;;      pascal-continued-statement-start, pascal-procedure-forward-regex,
;;      pascal-head-backward-regex and pascal-head-regex.
;;      Use defconst instead of defvar to define pascal-number-regexp.
;;      Define pascal-mode-map entirely by its defvar definition.
;;      Correct key-bindings for pascal-end-sentence and
;;      pascal-beginning-sentence.
;;      Define pascal-syntax-table entirely by its defvar definition.
;;      Make pasc-mode usable for XEmacs:
;;      Define zmacs-activate-region for Emacs and use it in
;;      pascal-mark-procedure, pascal-mark-sentence and pascal-mark-sexp.
;;      Define region-exists-p for Emacs and buffer-substring-no-properties
;;      for Xemacs. Use easy-menu to define the pascal menu.
;;      (pascal-upcase-keywords); use regexp-opt to make regexp from
;;      pascal-standard-keywords and pascal-extra-keywords.
;;      (pascal-capitalize-standardnames); use regexp-opt to make regexp from
;;      pascal-standard-standardnames and pascal-extra-standardnames.
;;      (pascal-uncomment-out-lines): new command.
;;
;; Version 1.10
;;      New commands: pascal-indent-procedure indents lines in the procedure
;;      surrounding point. pascal-transpose-procedures is similar to
;;      transpose-words but transposes procedures or functions instead.
;;      A transpose entry in the pascal menu.
;;      Fixed bugs in pascal-end-sentence and pascal-skip-procedure:
;;      now a semicolon after a sentence is always included in the sentence,
;;      this means that pascal-transpose-sentences now works.
;;
;; Version 1.09
;;      Support for pasc-semicolon.el which is paren.el hacked to show
;;      sentence start and end. Load pasc-semicolon.el and when point is
;;      immediately after a semicolon, then the semicolon and the start of
;;      the sentence ended by that semicolon is highlighted. A new variable
;;      pascal-blink-sentence-start-on-screen that is turned off by
;;      pasc-semicolon.el.
;;      A new command: pascal-run which is similar to pascal-compile, but
;;      runs a command in a shell buffer. This can be used to run pascal
;;      programs in a shell buffer. The error message parsing supporting the
;;      next-error command can be done in this buffer. With the proper
;;      regular expressions, run time errors can also be found. pascal-run
;;      requires compile-in-shell.el and a modified compile.el that supports
;;      the compilation-shell-minor-mode.
;;      A new variable pascal-error-regexp-alist which if not nil is used
;;      instead of compilation-error-regexp-alist for parsing error
;;      messages.
;;      A number of new key bindings and a menu-bar menu for most commands.
;;
;; Version 1.08
;;      To make it easier to adjust for different pascals there are some
;;      new variables.
;;      pascal-number-regexp recognizes a pascal number.
;;      pascal-equal-ops contains a list of single characters like ?+ that
;;      can be combined in C-like constructs like +=. Used in
;;      pascal-electric-equal.
;;      pascal-extra-keywords and pascal-extra-standardnames matches
;;      extra words for pascal-keywords resp pascal-standardnames.
;;      Improved pattern in pascal-font-lock-keywords.
;;      To support a pascal-label-regexp that could recognize labels, change
;;      in pascal-indent-label, pascal-find-declaration, pascal-find-dekl
;;      and pascal-indent-after-semi.
;;      Use buffer-substring-no-properties instead of buffer-substring in
;;      most cases.
;;      Syntax table changed a little. Use word syntax more.
;;      Removed reference to some old emacs18 variables: comment-indent-hook,
;;      auto-fill-hook and screen-width. These references have only been
;;      commented out you can still find them in the code.
;;      The default for pascal-blink-sentence-start and
;;      pascal-comment-default have changed.
;;
;; Version 1.07
;;      A new command pascal-comment-out-lines. See its documentation
;;      string for details.
;;
;; Version 1.06
;;      The help string for pascal-mode now gives the version number.
;;      Changed comments after end in pascal-electric-semi.
;;      The blinking at matching '[' or '(' is now done by the new
;;      function 'pascal-blink-paren'. The only exterior change is
;;      that no blinking is done if 'blink-paren-function' is nil.
;;
;; Version 1.05
;;
;;      Small change in pascal-keywords. Some keywords where not recognized
;;      and therefore not converted to upper case by pascal-upcase-keywords.
;;      A number of spelling errors have been corrected.
;;
;; Version 1.04
;;      Added commands pascal-upcase-keywords and
;;      pascal-capitalize-standardnames.
;;      Added customization variable pascal-ignore-block-after-do. This
;;      replaces pascal-block-indent, that now has no effect.
;;      Fixed bug in pascal-electric-semi.
;;
;; Version 1.03
;;      Extensive changes.

;;; Todo:
;;
;; The code for special abbrevs should be replaced by dmacros using
;; dmacro by Wayne Mesard.
;(require 'dmacro)
;;
;; More intelligent indentation of labels, cases and record fields.
;;
;; Pascal-complete-symbol similar to lisp-complete-symbol (and bound to
;; the same key). This requires parsing of declarations so that a
;; database of symbols can be built and it requires a function that
;; finds out what type of symbol is expected at the current position in
;; the code.
;; To get reasonable speed, the idea is to have a database that is not
;; aware of the pascal block structure, instead all definitions of a
;; symbol are available at the same time. If the database is to know about
;; blockstructure so that only one (or no) definition is available at a
;; time, then the position in the block structure will have to be
;; recomputed every time the database is queried.

;;; Code:

(require 'idcase) ; This defines the minor mode id-case-significant.
(require 'easymenu) ; Define menu items for both emacs 19 and xemacs.

;; Some code for compatibility between emacs 19 and xemacs.
(or (fboundp 'region-exists-p)
    (defun region-exists-p () mark-active))
(or (fboundp 'zmacs-activate-region)
    (defun zmacs-activate-region () (setq mark-active t)))

;; XEmacs apparently call this `buffer-substring-without-properties',
;; sigh.
(or (fboundp 'buffer-substring-no-properties)
    (fset 'buffer-substring-no-properties
	  (if (fboundp 'buffer-substring-without-properties)
              'buffer-substring-without-properties
	    'buffer-substring)))

;; Customization variables:

(defgroup pascal nil
  "Major mode for editing Pascal programs."
  :prefix "pascal-"
  :group 'languages)

(defgroup pascal-indent nil
  "Indentation settings"
  :group 'pascal)

;; The following variables are only used once, most of them when pascal mode
;; is started. If you change them after pascal mode has been started, then you
;; have to execute some command or restart pascal mode (by the pascal-mode
;; command) for the change to take effect.

;; Change the following two properties if you use other names for the
;; distributed template files.

;; File containing default templates for pascal when case is insignificant.
(put 'pascal-nocase-template-table 'file "pasc-1tmpl")

;; File containing default templates for pascal when case is significant.
(put 'pascal-case-template-table 'file "pasc-2tmpl")

;; If you mainly use a pascal where case is significant, set the following
;; variable to t, otherwise set it to nil.  This variable is used to set the
;; minor mode id-case-significant when pascal-mode is started. To change it
;; after pascal-mode has been started, use the command id-case-significant.
(defcustom pascal-case-significant nil
  "Default value for minor-mode id-case-significant in pascal mode.
If nil, map all words to lower case.  Command `id-case-significant' toggles.
If T, BEGIN etc. are not keywords. Also affects which templates are used."
  :type '(choice :tag "Case":value nil
		 (const :tag "Case insignificant" nil)
		 (const :tag "Case significant" t))
  :group 'pascal)

;; This variable is used when pascal-mode is started.
;; The default value of pascal-return is nil, which means that <nl> first
;; indents then gets a new indented line and <cr> only gets a new line.
;; In my opinion the normal action which is indent, should be bound to the
;; easier accessed key <cr>, but the present default is chosen to agree with
;; other language modes like c-mode and lisp-mode.
(defcustom pascal-return nil
  "Determine if <cr> or <nl> will indent current and next line.
nil means <nl> indents and <cr> not.
The command `pascal-return' reverses this."
  :type '(choice :tag "Who indents" :value nil
		 (const :tag "<nl> indents" nil)
		 (const :tag "<cr> indents" t))
   :group 'pascal)

;; The first time comment-start is needed (in indent-for-comment) then the
;; function pascal-set-comment-delim is called automatically and sets
;; comment-start from pascal-comment-start-international or from
;; pascal-comment-start-ascii.  At the same time comment-end is set from
;; pascal-comment-end-international or pascal-comment-end-ascii.  Which
;; alternative that is used depends on the first comment delimiter that is
;; found in the buffer or the value of the variable pascal-comment-default.

;; The following five variables are only used to set comment-start and
;; comment-end once for each buffer. If you want to change comment-start or
;; comment-end after that, then you have to call pascal-set-comment-delim
;; again.

;; The following variable is consulted by the function pascal-set-comment-delim
;; if there is no comment delimiter in the buffer.
(defvar pascal-comment-default 0	; 0 means use { as a delimiter
  "Determine what comment delimiter to use in buffer with no previous comment.
If nil then ask, else if > 0, use (* for comment, else use {.")

;; The following variables define comment delimiters.
(defvar pascal-comment-start-international "(* "
  "Preferred international comment opener.")
(defvar pascal-comment-end-international " *)"
  "Preferred international comment closer.")
(defvar pascal-comment-start-ascii "{ "
  "Preferred ascii comment opener.")
(defvar pascal-comment-end-ascii " }"
  "Preferred ascii comment closer.")

(defvar pascal-mode-hook nil
  "Functions to run when entering pascal mode.")

;; The following variables are referred to many times by pascal mode. A change
;; to a variable any time affects the future of pascal mode.

;; The following string describes the command to compile a file that is used
;; by the pascal-compile command. It is a format string that is given the
;; name of the file to compile as argument.  It can be changed by the user,
;; but should originally contain a default.  Change it to contain a suitable
;; default for your installation.
(defcustom pascal-compile-format "compile %s"
  "*Compile command format."
  :type 'string
  :group 'pascal)

(defcustom pascal-run-format "run %s"
  "*Run command format."
  :type 'string
  :group 'pascal)

(defvar pascal-error-regexp-alist nil "\
Nil or an alist of regular expressions matching compilation or running errors.
If not nil this is used instead of `compilation-error-regexp-alist' in
Pascal Compile or Pascal Run.  See `compilation-error-regexp-alist'.")

(defcustom pascal-put-end-comment nil
  "*Control automatic comment at `end;'.
If nil do nothing, if t put name at procedure end, otherwise put comment at all
`end;'."
  :type '(choice :value nil
		 (const :tag "No comment" nil)
		 (const :tag "Name at procedure end" t)
		 (const :tag "Comment after all end;" 1))
  :group 'pascal)


(defcustom pascal-auto-newline  nil
  "*If non-nil, `;' at the end of a line causes a newline to be inserted."
  :type 'boolean
  :group 'pascal)

(defcustom pascal-blink-sentence-start t
  "*If non-nil, ';' causes matching sentence start to be shown."
  :type 'boolean
  :group 'pascal)

(defcustom pascal-blink-sentence-start-on-screen t
  "*If non-nil, ';' causes matching sentence start to be shown, if on screen."
  :type 'boolean
  :group 'pascal)

(defcustom pascal-auto-string nil
  "*If non-nil, a ' in code will insert '' with point in between."
  :type 'boolean
  :group 'pascal)

(defcustom pascal-auto-paren nil
  "*If non-nil, a ( in code will insert () with point in between."
  :type 'boolean
  :group 'pascal)

(defvar pascal-auto-bracket nil
  "*Determine what will be inserted in code by the keys `[' or `]'.
If nil then insert `[' resp `]'.
If non-nil insert `[' resp `]' or `(.' resp `.)' depending on what has been
used before.
If non-nil and not t, then the key `[' will insert `['`]' or `(.'`.)' with
point in between.")

(defvar pascal-auto-curly nil
  "*Determine what will be inserted in code by the keys `{' or `}'.
If nil then insert `{' resp `}'.
If non-nil insert `{' resp `}' or `(*' resp `*)' depending on what has been
used before.
If non-nil and not t, then the key `{' will insert `{'`}' or `(*'`*)' with
point in between.")

;; Parsing of declarations not implemented yet. Do not change this variable.
(defconst pascal-parse nil)

(defcustom pascal-tab-always-indent t
  "*Non-nil means TAB in pascal mode always re-indents the current line,
regardless of where in the line point is when the TAB command is used.
nil means, TAB indents the current line only if point is at the left margin
or in the line's indentation; otherwise a tab is inserted."
  :type 'boolean
  :group 'pascal)

(defconst pascal-indent-type
  '(choice (const :tag "pascal-default-indent" +)
	   (const :tag "2 * pascal-default-indent" ++)
	   (const :tag "pascal-default-indent / 2" *)
	   (const :tag "- pascal-default-indent" -)
	   (const :tag "-2 * pascal-default-indent" --)
	   (const :tag "- pascal-default-indent / 2" /)
	   (const :tag "no extra indentation" 0)
	   integer))

;; The following variables defines how much to indent.
(defcustom pascal-default-indent 2
"*Default indent.  Other indents can be absolute value or a factor times this."
  :type 'integer
  :group 'pascal-indent)

(defcustom pascal-program-decl-indent 0
"*Indentation of top level LABEL, CONST, TYPE and VAR."
  :type pascal-indent-type
  :group 'pascal-indent)

(defcustom pascal-procedure-decl-indent '+ "\
*Indentation of LABEL, CONST, TYPE and VAR with respect to current procedure."
  :type pascal-indent-type
  :group 'pascal-indent)

(defcustom pascal-decl-decl-indent '+ "\
*Indentation for declarations relative to preceding LABEL, CONST, TYPE or VAR."
  :type pascal-indent-type
  :group 'pascal-indent)

(defcustom pascal-record-field-indent '+
  "*Extra indentation of RECORD fields."
  :type pascal-indent-type
  :group 'pascal-indent)

(defcustom pascal-of-type-indent '+
  "*Indentation of type with relative start of array or set-declaration."
  :type pascal-indent-type
  :group 'pascal-indent)

(defcustom pascal-program-procedure-indent 0
  "*Indentation of top level procedure."
  :type pascal-indent-type
  :group 'pascal-indent)

(defcustom pascal-procedure-procedure-indent '+
  "*Indentation of procedure inside procedure."
  :type pascal-indent-type
  :group 'pascal-indent)

(defcustom pascal-program-block-indent 0
  "*Indentation of top block of program."
  :type pascal-indent-type
  :group 'pascal-indent)

(defcustom pascal-procedure-block-indent '+
  "*Indentation of top block relative to start of procedure."
  :type pascal-indent-type
  :group 'pascal-indent)

(defcustom pascal-block-statement-indent '+
  "*Indentation of statements relative surrounding block."
  :type pascal-indent-type
  :group 'pascal-indent)

(defcustom pascal-block-end-indent 0
   "*Indentation of end or until relative start of block."
  :type pascal-indent-type
  :group 'pascal-indent)

(defcustom pascal-if-then-indent '+
  "*Indentation of consequent relative start of if-statement."
  :type pascal-indent-type
  :group 'pascal-indent)

(defcustom pascal-if-else-indent 0
  "*Indentation of else relative start of if-statement."
  :type pascal-indent-type
  :group 'pascal-indent)

(defcustom pascal-do-indent '+
  "*Indentation of statement relative start of while, for or with-statement."
  :type pascal-indent-type
  :group 'pascal-indent)

(defcustom pascal-continued-list-indent '*
  "*Indentation of list element relative beginning of list."
  :type pascal-indent-type
  :group 'pascal-indent)

(defcustom pascal-continued-statement-indent '+
  "*Extra indentation of other statement continuations."
  :type pascal-indent-type
  :group 'pascal-indent)


(defcustom pascal-ignore-block-after-do nil
"*If true, BEGIN--END have no affect on indentation, after THEN, ELSE or DO."
  :type 'boolean
  :group 'pascal-indent)

;; This is a list of single characters like ?+ that can be combined in
;; C-like constructs like +=.  Add extra characters for your pascal.
(defvar pascal-equal-ops '(?\+ ?\- ?\* ?\/ ?\& ?\! ?\|)
  "List of characters that can be combined in C-like constructs as += .")

;;  Change this for your pascal.  Used in pascal-upcase-keywords.
(defcustom pascal-extra-keywords
  '("extern" "external" "others" "otherwise")
  "*List of extra keywords for this pascal."
  :type '(repeat string)
  :group 'pascal)

;; This recognizes all standard pascal keywords.
(defvar pascal-standard-keywords
  '("program"
    "label" "const" "type" "var"
    "packed" "array" "record" "set" "file"
    "function" "procedure" "forward"
    "begin" "end" "while" "do" "repeat" "until" "for" "to" "downto"
    "if" "then" "else" "case" "of" "goto" "with"
    "not" "and" "or" "in" "div" "mod"
    "nil"))

;;  Change this for your pascal.  Used in pascal-capitalize-standardnames.
(defcustom pascal-extra-standardnames '()
  "*List of strings: extra standard names for this pascal."
  :type '(repeat string)
  :group 'pascal)

;; This recognizes all standard pascal predefined standard names.
(defvar pascal-standard-standardnames
  '("integer" "real" "char" "boolean" "text"
    "true"  "false" "maxint"
    "input" "output"
    "reset" "rewrite" "eof" "eoln" "page"
    "read" "readln" "write" "writeln" "get" "put"
    "chr" "ord" "pred" "succ"
    "trunc" "round" "abs" "sqr" "odd"
    "sqrt" "sin" "cos" "arctan" "ln" "exp"
    "new" "dispose"))

;; The following regular expression recognizes keywords for which
;; indentation is done relative to where they are and not relative to
;; where they would be if there was only white space in front of them
;; on the line.
(defvar pascal-relative-indents
 (concat
  "[([]\\|"
  (regexp-opt
   '("record")
   t)
  "\\>")
"*Regular expression recognizing keywords with relative indentation.")

;; The following regular expression recognizes keywords and characters such
;; that if they are followed on the same line by a sentence, then the
;; following lines are indented so that sentences are placed directly under
;; this first sentence.
(defvar pascal-indent-parens
  (concat
   "[[(:]\\|"
   (regexp-opt
    '("label" "const" "type" "var"
      "function" "procedure" "program"
      "record"
      "begin" "repeat" "of")
    t)
   "\\>")
  "*Regular expression recognizing indentation parenthesis.")

(defcustom pascal-indent-comments-separately t
"*What to do with right margin comments when indenting code.
If nil, move comments with code when indenting, if t try to keep current
column.  If not t or nil, indent comment to `comment column'."
  :type `(choice (const :tag "Move comments with code" nil)
		 (const :tag "Try to keep current column" t)
		 (const :tag "Indent to `comment column'" 1))
  :group 'pascal-indent)

(defcustom pascal-right-margin 78
 "If `pascal-fix-comment' is a number use this as comment margin."
  :type 'integer
  :group 'pascal-indent)

(defcustom pascal-fix-comment 8
  "Determine if to stay within `pascal-right-margin'.
Must be nil or a positive integer N.  If not nil, try to keep the end of the
line within  `pascal-right-margin' by either (if possible) indenting a comment
to a position that is an integer multiple of  `pascal-fix-comment' left of
 `comment-column' , or (if necessary) indent the comment with just one space
after the code."
  :type '(choice (integer :tag "Comment backtab")
		 (const :tag "Don't stay within margin" nil))
  :group 'pascal-indent)

(defcustom pascal-indent-comment-fn 'pascal-calculate-indent-within-comment
  "*Nil or function to call to indent block comments."
  :type '(choice (const nil) function)
  :group 'pascal-indent)

(defcustom pascal-star-starts '(?* ?! ?| ?+)
  "*List of chars that frame block comments."
  :type '(repeat char)
  :group 'pascal)

(defcustom pascal-dont-indent "#"
  "Regular expression used as last argument to `indent-code-rigidly'.
If matched at the beginning of a line, this means don't indent that line
and ignore this line when indenting next line."
  :type '(choice (const nil) regexp)
  :group 'pascal-indent)

(defcustom pascal-start-ignored-line "^#"
  "nil or regular expression matching beginning of an ignored line."
  :type '(choice (const nil) regexp)
  :group 'pascal-indent)

;;If comment-multi-line-ok is false, pascal-point-in-comment-or-string
;; will not expect multi line comments. This will speed things up a little but
;; it means that various routines might be confused by a multi line comment.
(defcustom comment-multi-line-ok t
  "False means don't expect multi-line comments.
This will speed up things a little but it means that various routines
might be confused by a multi line comment."
  :type 'boolean
  :group 'pascal)

(defcustom pascal-keywords-only nil
  "If true only keywords are fontified."
  :type 'boolean
  :group 'pascal)

;; This is modified from the pascal.el by Espen Skoglund.
(defvar pascal-font-lock-keywords
  (list
   '("\\<\\(function\\|pro\\(cedure\\|gram\\)\\)\\>[ \t]*\\(\\w+\\)?"
     (1 font-lock-keyword-face) (3 font-lock-function-name-face nil t))
   (cons (concat
	  "\\<"
	  (regexp-opt
	   '("integer" "real" "char" "boolean" "text"
	     "packed" "array" "record" "set" "file")
	   t)
	  "\\>")
	 'font-lock-type-face)
   '("\\<\\([0-9]+\\)[ \t]*:" 1 font-lock-reference-face)
   (cons (concat
	  "\\<"
	  (regexp-opt
	   '("label" "const" "type" "var"
	     "forward" "extern" "external"
	     "begin" "end" "while" "do" "repeat" "until" "for" "to" "downto"
	     "if" "then" "else" "case" "of" "with"
	     "not" "and" "or" "in" "div" "mod"
	     "nil")
	   t)
	  "\\>")
	 `font-lock-keyword-face)
   '("\\<\\(goto\\)\\>[ \t]*\\([0-9]+\\)?"
     (1 font-lock-keyword-face) (2 font-lock-reference-face nil t)))
  "Additional expressions to highlight in Pascal mode.")

(defcustom pascal-comment-out-start "(*% "
  "\\<pascal-mode-map>String inserted by \\[pascal-comment-out-lines] to start commenting out code.
This string may not appear for any other purpose in the program."
  :type 'string
  :group 'pascal)

(defcustom pascal-comment-out-end " %*)"
  "\\<pascal-mode-map>String inserted by \\[pascal-comment-out-lines] to end commenting out code.
This string may not appear for any other purpose in the program."
  :type 'string
  :group 'pascal)

;; End of customization variables.

;; The rest of the variables here are not intended to be changed.

;; An unsigned integer or floating point number.
;; The first subexpression must recognize an integer.
(defconst pascal-number-regexp
  "\\([0-9]+\\)\\(\\.[0-9]+\\)?\\([eE][-+]?[0-9]+\\)?")

;; Generalized right parenthesis.
(defconst pascal-general-right-paren "[]'})]\\|\\(end\\|until\\)\\>")

;; Pattern for beginning of a sentence. ;
(defconst pascal-sentence-left-delim
  (concat
   "[')};]"
   "\\|\\<"
   (regexp-opt
    '("label" "const" "type" "var"
      "record"
      "begin" "end" "repeat" "until" "case")
    t)
   "\\>"))

;; Pattern for beginning of a composite sentence. ]([
(defconst pascal-contain-sentence-left-delim
  (concat
   "[]')}([]"
   "\\|\\<"
   (regexp-opt
    '("label" "const" "type" "var"
      "record"
      "begin" "end" "repeat" "until" "case")
    t)
   "\\>"))

;; Pattern for position relative which to indent a continued statement
(defconst pascal-continued-statement-start
  (concat
   "[]')}:;[(]"
   "\\|:="
   "\\|(\\."
   "\\|\\<"
   (regexp-opt
  '("program"
    "label" "const" "type" "var"
    "packed" "array" "record" "set" "file"
    "function" "procedure" "forward" "extern" "external"
    "begin" "end" "while" "do" "repeat" "until" "for" "to" "downto"
    "if" "then" "else" "case" "of" "goto" "with")
    t)
   "\\>"))

;; Pattern for beginning of an assignment statement. Must contain '})
(defconst pascal-assignment-start
 "['}:;)]\\|\\<\\(begin\\|repeat\\|do\\|then\\|else\\|otherwise\\)\\>")

;; (defconst pascal-statement-end "['{(;]\\|\\<\\(end\\|until\\|else\\)\\>"
;;  "Regular expression describing delimiters for statement end")

;; Pattern for label
(defconst pascal-label-regexp
  "\\([0-9]+\\)\\([ \t]*:\\)\\([ \t]*\\)")

;; Pattern for skipping procedure forward
(defconst pascal-procedure-forward-regex
  (concat
   "['{(]"
   "\\|\\<"
   (regexp-opt
    '("begin" "extern" "external" "function" "forward" "procedure")
    t)
   "\\>"))

;; Pattern for backward head search
(defconst pascal-head-backward-regex
  (concat
   "['})]"
   "\\|\\<"
   (regexp-opt
    '("begin" "end" "extern" "external" "forward" "function"
      "procedure" "program")
    t)
   "\\>"))

;; Pattern for seeing head
(defconst pascal-head-regex
  (concat (regexp-opt '("program" "procedure" "function") t) "\\>"))

;; Simplified pascal-head-backward-regex
(defconst pascal-head-back-regex
  (concat "['})]\\|\\<" pascal-head-regex))

;; Pattern for finding current function or procedure.
(defconst pascal-proc-head
  (concat "['{(]\\|\\<" pascal-head-regex))

;; Pattern for id declaration
(defconst pascal-id-declare-regex
"[[.,;(]\\|\\(record\\|label\\|const\\|type\\|var\\|procedure\\|function\\)\\>"
)

;; Pattern for parameter types
(defconst pascal-param-type-regex "\\(var\\|procedure\\|function\\)\\>")

;; white space characters
(defconst pascal-white " \t\n\^l\^m")

;; white space characters on a line
(defconst pascal-white-ln " \t")

(defconst pascal-comment-start-regexp "{\\|(\\*")

(defconst pascal-comment-end-regexp   "}\\|\\*)")

(defvar pascal-bracket-start nil)
(defvar pascal-bracket-end nil)

(defvar pascal-comment-start nil)
(defvar pascal-comment-end nil)

;; Last known status (comment, string or code).
(defvar pascal-last-state nil)

;; Position where last known status was computed.
(defvar pascal-state-pos nil)

(defvar pascal-mode-map
  (progn
    (setq pascal-mode-map (make-sparse-keymap))
    ;; The key definitions are printed in reverse order in the help string
    (define-key pascal-mode-map "\e\C-h"	'pascal-mark-procedure)
    (define-key pascal-mode-map "\e\C-e"	'pascal-end-procedure)
    (define-key pascal-mode-map "\e\C-a"	'pascal-beginning-procedure)
    (define-key pascal-mode-map "\C-xt"	'pascal-transpose-sentences)
    (define-key pascal-mode-map "\C-x\C-?"	'pascal-backward-kill-sentence)
    (define-key pascal-mode-map "\ek"	'pascal-kill-sentence)
    (define-key pascal-mode-map "\C-x@"	'pascal-mark-sentence)
    (define-key pascal-mode-map "\ee"	'pascal-end-sentence)
    (define-key pascal-mode-map "\ea"	'pascal-beginning-sentence)
    ;; (define-key pascal-mode-map "\e\C-u" 'pascal-back-up-level)
    (define-key pascal-mode-map "\e\C-t"	'pascal-transpose-sexps)
    (define-key pascal-mode-map "\e\C-k"	'pascal-kill-sexp)
    (define-key pascal-mode-map "\e\C-@"	'pascal-mark-sexp)
    (define-key pascal-mode-map "\e\C-b"	'pascal-backward-sexp)
    (define-key pascal-mode-map "\e\C-f"	'pascal-forward-sexp)
    (define-key pascal-mode-map "\en"	'pascal-find-template-mark)
    (define-key pascal-mode-map "\ep"	'pascal-template-expand)
    (define-key pascal-mode-map "\e;"	'pascal-indent-for-comment)
    (define-key pascal-mode-map "\e\C-q"	'pascal-indent-sentence)
    (define-key pascal-mode-map "\e\C-g"	'pascal-indent-procedure)
    (define-key pascal-mode-map "\C-c("	'pascal-beginnify-region)
    (define-key pascal-mode-map "\C-c;"	'pascal-insert-end-comment)
    (define-key pascal-mode-map "\C-c\}"	'pascal-uncomment-out-lines)
    (define-key pascal-mode-map "\C-c\{"	'pascal-comment-out-lines)
    (define-key pascal-mode-map "\C-c="	'pascal-find-declaration)
    (define-key pascal-mode-map "\C-c^"	'pascal-upcase-keywords)
    (define-key pascal-mode-map "\C-c!"	'pascal-capitalize-standardnames)
    (define-key pascal-mode-map "\C-c\C-i"	'indent-buffer)
    (define-key pascal-mode-map "\C-c\C-n"	'next-error)
    (define-key pascal-mode-map "\C-c\C-r"	'pascal-run)
    (define-key pascal-mode-map "\C-c\C-c"	'pascal-compile)
    (define-key pascal-mode-map ";" 	'pascal-electric-semi)
    (define-key pascal-mode-map "."		'pascal-electric-period)
    (define-key pascal-mode-map "="		'pascal-electric-equal)
    (define-key pascal-mode-map "'"		'pascal-electric-string)
    (define-key pascal-mode-map "("		'pascal-electric-left-paren)
    (define-key pascal-mode-map ")"		'pascal-electric-right-paren)
    (define-key pascal-mode-map "["		'pascal-electric-left-bracket)
    (define-key pascal-mode-map "]" 	'pascal-electric-right-bracket)
    (define-key pascal-mode-map "{" 	'pascal-electric-left-curly)
    (define-key pascal-mode-map "}" 	'pascal-electric-right-curly)
    (define-key pascal-mode-map "\177"	'backward-delete-char-untabify)
    (define-key pascal-mode-map "\C-m"	'newline)
    (define-key pascal-mode-map "\C-j"	'pascal-newline-check-and-indent)
    (define-key pascal-mode-map "\t"	'pascal-indent-command)
    (easy-menu-define pascal-menu pascal-mode-map "Menu for pascal mode"
      '("Pascal"
	("Indent"
	 ["line" pascal-indent-command (not buffer-read-only)]
	 ["sentence" pascal-indent-sentence (not buffer-read-only)]
	 ["procedure" pascal-indent-procedure (not buffer-read-only)]
	 ["region" indent-region (not buffer-read-only)]
	 ["buffer" indent-buffer (not buffer-read-only)])
	("Go beginning of"
	 ["line" beginning-of-line t]
	 ["expression" pascal-backward-sexp t]
	 ["sentence" pascal-beginning-sentence t]
	 ["procedure" pascal-beginning-procedure t])
	("Go end of"
	 ["line" end-of-line t]
	 ["expression" pascal-forward-sexp t]
	 ["sentence" pascal-end-sentence t]
	 ["procedure" pascal-end-procedure t])
	("Mark"
	 ["expression" pascal-mark-sexp t]
	 ["sentence" pascal-mark-sentence t]
	 ["procedure" pascal-mark-procedure t])
	("Transpose"
	 ["expression" pascal-transpose-sexps (not buffer-read-only)]
	 ["sentence" pascal-transpose-sentences (not buffer-read-only)]
	 ["procedure" pascal-transpose-procedures (not buffer-read-only)])
	"--"
	["Compile" pascal-compile t]
	("Error"
	 ["next" next-error
	  (and (featurep 'compile) (compilation-errors-exist-p))]
	 ["previous" previous-error
	  (and (featurep 'compile) (compilation-errors-exist-p))]
	 ["first" first-error
	  (and (featurep 'compile) (compilation-errors-exist-p))])
	"--"
	["Find definition" pascal-find-declaration t]
	["Comment out region" pascal-comment-out-lines
	 (and (not buffer-read-only) (region-exists-p))]
	["Restore commented out region" pascal-uncomment-out-lines
	 (and (not buffer-read-only) (region-exists-p))]
	["Capitalize standardnames" pascal-capitalize-standardnames
	 (and (not buffer-read-only) (region-exists-p))]
	["Upcase keywords" pascal-upcase-keywords
	 (and (not buffer-read-only) (region-exists-p))]
	["Wrap BEGIN  END around region" pascal-beginnify-region
	 (and (not buffer-read-only) (region-exists-p))]))
    pascal-mode-map)
  "Keymap used in Pascal mode.")

(defvar pascal-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\' "\""  table)
    (modify-syntax-entry ?\" "\""  table)
    (modify-syntax-entry ?\( "()1" table)
    (modify-syntax-entry ?\) ")(4" table)
    (modify-syntax-entry ?\* ".23" table)
    (modify-syntax-entry ?\+ "."   table)
    (modify-syntax-entry ?\- "."   table)
    (modify-syntax-entry ?\/ "."   table)
    (modify-syntax-entry ?\< "."   table)
    (modify-syntax-entry ?\= "."   table)
    (modify-syntax-entry ?\> "."   table)
    (modify-syntax-entry ?\% "."   table)
    (modify-syntax-entry ?\[ "(]"  table)
    (modify-syntax-entry ?\] ")["  table)
    (modify-syntax-entry ?\{ "<"   table)
    (modify-syntax-entry ?\} ">"   table)
    table)
  "Syntax table used in Pascal mode.")

(defvar pascal-case-template-table nil
  "Abbrev table in use in Pascal-mode buffers with case significant.")

(defvar pascal-nocase-template-table nil
  "Abbrev table in use in Pascal-mode buffers with case not significant.")

(defvar pascal-mode-abbrev-table nil)

(defvar pascal-template-table nil
  "Abbrev table in use in Pascal-mode buffers.")
(make-variable-buffer-local 'pascal-template-table)

(defvar pascal-token-end nil)

(defun pascal-mode-case ()
  "Start pascal mode with option `id-case-significant' true."
  (interactive)
  (pascal-mode 1))

(defun pascal-mode-no-case ()
  "Start pascal mode with option `id-case-significant' false."
  (interactive)
  (pascal-mode 0))

(defun pascal-mode (&optional case)
  "Major mode for editing Pascal programs. Version 1.12.
If optional argument CASE is not nil, then it must be numeric and
id-case-significant is set to true if CASE > 0 else to false.
\\<pascal-mode-map>
Knows about procedure (program, procedure or function), sentence (delimited at
the end by end, until or ; and at the beginning by label, const, type, var,
record, begin, repeat and ;) and expression (word, string, function or
procedure call, array reference and parenthesis expression).

Indent by line '\\[pascal-indent-command]', sentence '\\[pascal-indent-sentence]', region '\\[indent-region]' and whole
buffer '\\[indent-buffer]'.
'\\[pascal-newline-check-and-indent]' indents current line, does some checking and gets a new indented line.
'\\[newline]' will just get a new line (use this if you don't want an indented line).

If the variable `pascal-indent-comments-separately' is false then the whole
line including comments at the end is moved as a unit when code at the
beginning of the line is indented.
If `pascal-indent-comments-separately' is t (default),comments try to stay
where they are.
If `pascal-indent-comments-separately' is neither false or t, then comments
are moved to comment-column.
If `pascal-fix-comment' and `pascal-indent-comments-separately' are true then
comment position is adjusted in increments of `pascal-fix-comment' to keep
comment end within `pascal-right-margin'.

Move forward and backward by expression '\\[pascal-forward-sexp]' and '\\[pascal-backward-sexp]',
sentence '\\[pascal-beginning-sentence]' and '\\[pascal-end-sentence]', and procedure '\\[pascal-beginning-procedure]' and '\\[pascal-end-procedure]'.

Mark expression '\\[pascal-mark-sexp]', sentence '\\[pascal-mark-sentence]' and procedure '\\[pascal-mark-procedure]'.

Kill to end of expression '\\[pascal-kill-sexp]' and sentence '\\[pascal-kill-sentence]'.

Kill to beginning of sentence '\\[pascal-backward-kill-sentence]'.

Transpose expressions '\\[pascal-transpose-sexps]' and sentences '\\[pascal-transpose-sentences]'.

If the preceding word is end or a top-level begin, '\\[pascal-insert-end-comment]' will insert a
comment indicating what was ended (or begun).

When ';' is inserted the cursor will momentarily move back to the beginning of
the sentence that was ended by the ';'. If pasc-semicolon is loaded then the
the beginning and end of a sentence is highlighted whenever point is just
after a semicolon.
If `pascal-put-end-comment' is true, ';' after 'end' will do a '\\[pascal-insert-end-comment]'. If
`pascal-put-end-comment' is t (default) this is only done when ending a
function or procedure. If it is not false or t, then this is done after each
'end'.
If pascal-auto-newline is non-nil (default nil), ';' at the end of a line
will do a '\\[pascal-newline-check-and-indent]' to insert a new line.

'x+=' is expanded to 'x:=x+' and 'x +=' is expanded to 'x := x + ' where x
is anything and '+' is '^.' or any character in `pascal-equal-ops' like '+'.

'\\[pascal-template-expand]' inserts Pascal templates. '~<..>' is used as a mark to indicate where
to move point in a template.
'\\[pascal-find-template-mark]' moves to the next '~<..>'.
Type '?\\[pascal-template-expand]' to display a list of built in abbreviations for Pascal templates.

'\\[id-case-significant]' toggles the minor-mode id-case-significant which
if true, means that case is significant in identifier and keyword names.  This
mode also decides whether templates using significant or insignificant case
will be loaded.

'\\[backward-delete-char-untabify]' converts tabs to spaces as it moves back.

'\\[pascal-beginnify-region]' surrounds region with BEGIN  END. If region is one line then this
is done in one line, otherwise begin and end are one separate line each.

'\\[pascal-comment-out-lines]' makes comments out of lines in region. '\\[pascal-comment-out-lines]' restores lines.
Lines that have text (not only white space) in the region are included.

'\\[pascal-find-declaration]' asks for an identifier (default: identifier at point or, if ARG is
given, the last identifier used for this command) and looks for its definition.

'\\[pascal-upcase-keywords]' and '\\[pascal-capitalize-standardnames] will upcase all keywords resp. capitalize
all standardnames in the region.

'\\[pascal-indent-for-comment]' creates a comment. If '{' or '(*' comments are created depends on the
first comment character found in the buffer. Insert the first comment character
in a buffer manually, for the rest of them you can use \\[pascal-indent-for-comment].

'\\[pascal-compile]' compiles current buffer. With ARG changes compilation command.
'\\[pascal-run]' runs current buffer. With ARG changes run command.
'\\[next-error]' goes to the next compilation or run-time error. By issuing more
'\\[next-error]' commands all errors can be visited.

\\{pascal-mode-map}

Comments are controlled by
pascal-comment-start-international, pascal-comment-end-international,
pascal-comment-start-ascii   and    pascal-comment-end-ascii.

Indentation is controlled by the variables
   Variable                 Default      Indentation of
pascal-program-decl-indent    0 top level LABEL, CONST, TYPE or VAR
pascal-procedure-decl-indent  2 LABEL, CONST, TYPE or VAR relative procedure
pascal-decl-decl-indent	      2 declaration relative LABEL, CONST, TYPE or VAR
pascal-record-field-indent    2 fields in RECORD
pascal-of-type-indent         2 type after OF in ARRAY or SET declaration
pascal-program-procedure-indent 0 top level procedure
pascal-procedure-procedure-indent 2 procedure relative surrounding procedure
pascal-program-block-indent   0 top BEGIN of program
pascal-procedure-block-indent 2 top BEGIN relative procedure
pascal-block-statement-indent 2 statements inside BEGIN,CASE-END REPEAT-UNTIL
pascal-block-end-indent       0 END or UNTIL relative BEGIN,CASE,RECORD REPEAT
pascal-if-then-indent         2 statement after THEN relative IF
pascal-if-else-indent         0 ELSE relative IF
pascal-do-indent              2 statement after DO relative WHILE,WITH,FOR
pascal-continued-statement-indent 2 other continued statements
pascal-ignore-block-after-do nil If true, BEGIN--END have no affect on
                                 indentation, after THEN, ELSE or DO."
  (interactive "P")
  (kill-all-local-variables)
  (use-local-map pascal-mode-map)
  (setq major-mode 'pascal-mode)
  (setq mode-name "Pascal")
  (easy-menu-add pascal-menu)		; Noop in emacs 19, add menu in xemacs.
  (if (not pascal-mode-abbrev-table)
      (setq pascal-mode-abbrev-table (make-abbrev-table)))
  (setq local-abbrev-table pascal-mode-abbrev-table)
  (if (not (memq 'pascal-check-template-case id-case-change-hook))
      (setq id-case-change-hook
	    (append id-case-change-hook '(pascal-check-template-case))))
  (id-case-significant (or case (if pascal-case-significant 1 0)) t)
  (set-syntax-table pascal-syntax-table)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
	(list 'pascal-font-lock-keywords pascal-keywords-only
	      (not id-case-significant)))
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'pascal-indent-line)
  (make-local-variable 'indent-region-function)
  (setq indent-region-function 'pascal-indent-region)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'pascal-bracket-start)
  (setq pascal-bracket-start nil)
  (make-local-variable 'pascal-bracket-end)
  (setq pascal-bracket-end nil)
  (make-local-variable 'pascal-comment-start)
  (setq pascal-comment-start "{")
  (make-local-variable 'pascal-comment-end)
  (setq pascal-comment-end "}")
  (make-local-variable 'comment-start)
  (setq comment-start nil)
  (make-local-variable 'comment-end)
  (setq comment-end nil)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "{[ \t]*\\|(\\*+[ \t]*")
  (make-local-variable 'comment-column)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start    "^[ \t]*$\\|^^L")
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'comment-multi-line)
;  (setq comment-multi-line nil)		; prefer single line comment
  (make-local-variable 'comment-multi-line-ok)
  (setq comment-multi-line-ok t)	;  but accept block comments
;  (make-local-variable 'comment-indent-hook)
;  (setq comment-indent-hook 'pascal-comment-indent)
  (setq comment-indent-function 'pascal-comment-indent)
  (pascal-return (if pascal-return 1 -1))
  (run-hooks 'pascal-mode-hook)
;  (if (and (boundp 'auto-fill-hook) (eq auto-fill-hook 'do-auto-fill))
;      (setq auto-fill-hook 'pascal-do-fill))
  (if (and (boundp 'auto-fill-function) (eq auto-fill-function 'do-auto-fill))
      (setq auto-fill-function 'pascal-do-fill))
  (if (fboundp 'pascal-show-sentence-start-mode)
      (pascal-show-sentence-start-mode
       (if pascal-show-sentence-start-mode 1 -1)))
  )

;; Indentation commands and hooks

;; This command doesn't really belong here,
;;  but keep it for the time being.
(defun indent-buffer ()
 "Indent each line in the buffer using current `indent-line-function'."
 (interactive "*")
 (indent-region (point-min) (point-max) nil))

;; This function should be bound to tab
(defun pascal-indent-command (&optional whole-exp)
  "Indent current line as pascal code, or in some cases insert a tab character.
If point is in a string, always insert a tab character.
Otherwise if `pascal-tab-always-indent' is non-nil (the default), always
indent current line.
Otherwise, indent the current line only if point is at the left margin or in
the line's indentation; otherwise insert a tab.
An optional prefix argument WHOLE-EXP, means indent rigidly all the lines
of the sentence starting after point so that this line becomes properly
indented.  The relative indentation among the lines of the sentence is
preserved."
  (interactive "*P")
  (let ((pascal-state-pos (make-marker)))
    (cond
     (whole-exp
      ;; If arg, always indent this line as pascal
      ;; and shift remaining lines of sentence the same amount.
      (let ((shift-amt (pascal-do-indent-line))
	    beg end)
	(save-excursion
	  (if pascal-tab-always-indent (beginning-of-line))
	  (setq beg (point))
	  (pascal-end-sentence 1)
	  (setq end (point))
	  (goto-char beg)
	  (forward-line 1)
	  (setq beg (point)))
	  (if (> end beg)
	      (indent-code-rigidly beg end shift-amt pascal-dont-indent))))
     ((eq 1 (pascal-point-in-comment-or-string)) (insert "\t")) ; Must be tab
     ((or pascal-tab-always-indent (pascal-in-indent)) (pascal-do-indent-line))
     (t (insert-tab)))
    (set-marker pascal-state-pos nil)))

;; This function is normally bound to carriage return or newline.
(defun pascal-newline-check-and-indent ()
  "Check for string or comment, insert a newline and indent old and new line.
If in a string complain, if in a comment do `indent-new-comment-line'."
  (interactive "*")
  (let ((state (pascal-point-in-comment-or-string)) (p (point)))
    (if (eq state 1) (error "End of string needed")
      (skip-chars-backward pascal-white-ln)
      (delete-region (point) p)		; Delete trailing white space
      (if (not state) (pascal-newline)
	(if (not comment-start)		; A comment. If necessary,
	    (pascal-set-comment-delim))	; initialize comment delimiter
	(indent-new-comment-line)))))

;; auto-fill-function
;; A fill function that does not break strings across line boundaries.  If
;; the fill column is in a string, and there are no spaces on the line before
;; the string other than the indentation, the line should break at the end
;; of string, but doesn't.  This seems to be a bug with do-auto-fill.
;; Is this still needed??
(defun pascal-do-fill ()
  (if (not comment-start) (pascal-set-comment-delim))
  (if (not (eq (pascal-point-in-comment-or-string) 1))
      (let ((curr-pt (point)))
	(move-to-column fill-column)
	(if (eq (pascal-point-in-comment-or-string) 1)
	    (let ((fill-column
		   (progn (pascal-beginning-string) (current-column))))
	      (goto-char curr-pt)
	      (if (not (do-auto-fill)) (newline-and-indent)))
	  (goto-char curr-pt)
	  (do-auto-fill)))))

;; indent-region-function
(defun pascal-indent-region (pascal-state-pos end)
  "Cleans empty lines and indent others with `pascal-do-indent-line'.
All lines in region from PASCAL-STATE-POS to END are indented"
  (save-excursion
    (goto-char end)
    (setq end (point-marker))
    (goto-char pascal-state-pos)
    (beginning-of-line)
    (setq pascal-state-pos (make-marker))
    (while (< (point) end)
      (if (looking-at "[ \t]*$")
	  (delete-region (match-beginning 0) (match-end 0))
	(pascal-do-indent-line))
      (forward-line 1))
    (set-marker pascal-state-pos nil)
    (set-marker end nil)))

;; indent-line-function
(defun pascal-indent-line ()
  (let ((pascal-state-pos (make-marker)))
    (pascal-do-indent-line)
    (set-marker pascal-state-pos nil)))

(defun pascal-indent-procedure (&optional n)
  "Indent current or following procedure.
With numeric prefix arg N indent N following procedures."
  (interactive "*p")
  (or n (setq n 1))
  (let ((p (point)) pp)
    (pascal-end-procedure (if (> n 0) n 1))
    (setq pp (point))
    (goto-char p)
    (pascal-beginning-procedure (if (> n 0) 1 (- n)))
    (pascal-indent-region (point) p)))

(defun pascal-indent-sentence (&optional n)
  "Indent current or following sentence.
With numeric prefix arg N indent N following sentences."
  (interactive "*p")
  (pascal-end-sentence 1)
  (let ((p (point)))
    (pascal-beginning-snts)
    (beginning-of-line)
    (if (or (not n) (eq n 1)) (pascal-indent-region (point) p)
      (setq p (point))
      (pascal-end-sentence n)
      (pascal-indent-region p (point)))))

(defun pascal-indent-for-comment ()
  "Indent this line's comment to comment column, or insert an empty comment."
  ;;The same as indent-for-comment but first check what character to use.
  (interactive "*")
  (if (not comment-start) (pascal-set-comment-delim))
  (indent-for-comment))

;; comment-indent-function
;; This is used by indent-for-comment to decide how much to indent a comment
;; in pascal code based on its context.
(defun pascal-comment-indent ()
  (if (and (bolp) (looking-at pascal-comment-start-regexp))
      0					; Existing comment at bol stays there.
    (save-excursion
      (skip-chars-backward pascal-white-ln)
      (max (1+ (current-column))	; Else indent at comment column
	   comment-column))))		; except leave at least one space.

;; Commands that move, mark, kill and transpose

;; A procedure in the sense of pascal mode is a procedure, function or program.

(defun pascal-beginning-procedure (&optional n)
  "Go to beginning of current or previous procedure.
With numeric prefix arg N go N procedures backward."
  (interactive "p")
  (let ((p (point)) r)
    (cond
     ((not n) (setq n 1))
     ((< n 1) (pascal-end-procedure (- 1 n)) (setq n 1)))
    (pascal-end-comment-or-string)
    (pascal-white-forward)
    (cond
     ((setq r (and (not (pascal-in-paren 'go)) (pascal-seing-head)))
      (setq n (1+ n)))
     ((progn (skip-chars-backward pascal-white-ln p)
	     (if (= p (point)) (forward-word 1))
	     (eq 0 (setq r (pascal-go-to-header))))
      (while (eq 0 (setq r (pascal-go-to-header t))))))
    (if r (setq r 1))
    (while (and (> (setq n (1- n)) 0)
		(eq 1 (setq r (pascal-go-to-header)))))
    (if (eq r 2) (message "Surrounding procedure"))
    r))

(defun pascal-end-procedure (&optional n)
 "Go to end of current or next procedure.
With numeric prefix arg N go N procedures forward."
  (interactive "p")
  (let ((r nil) (pp nil))
    (cond
     ((not n) (setq n 1))
     ((< n 1) (pascal-beginning-procedure (- 1 n)) (setq n 1)))
    (pascal-end-comment-or-string)
    (pascal-white-forward)
    (skip-chars-forward "a-zA-Z")	; Want to see this keyword below.
    (while (eq (setq r (pascal-go-to-header)) 0) ; Get out of statements
      (setq pp (point)))
    (setq r (if (not pp) (pascal-skip-procedure)
	      (goto-char pp)
	      (pascal-end-snts)))
    (while (and r (> (setq n (1- n)) 0)
		(progn (pascal-white-forward) (pascal-seing-head)))
      (setq r (pascal-skip-procedure)))
    (if (and r (> n 0) (looking-at "begin\\>")) (pascal-end-snts))))

(defun pascal-mark-procedure ()
  "Put region around current or following procedure."
  (interactive)
  (pascal-end-procedure 1)
  (pascal-forward-begin-line)
  (push-mark)
  (pascal-beginning-procedure 1)
  (pascal-backward-begin-line)
  (zmacs-activate-region))

(defun pascal-transpose-procedures (&optional arg)
  "Interchange procedures around point, leaving point at end of them.
With prefix arg ARG, effect is to take procedure before or around point
and drag it forward past ARG other procedures (backward if ARG negative).
If ARG is zero, the procedures around or after point and around or after mark
are interchanged."
  (interactive "*p")
  (transpose-subr (function
		   (lambda (arg)
		     (if (< arg 0) (pascal-beginning-procedure (- arg))
		       (pascal-end-procedure arg))))
		  arg))

;; A pascal sentence is delimited at the end by end, until or ; and
;; at the beginning by label, const, type, var, record, begin, repeat and ;.

(defun pascal-beginning-sentence (&optional n)
  "Go to beginning of sentence.  With prefix argument N go N sentences back."
  (interactive "p")
  (cond
   ((not n) (setq n 1))
   ((< n 1) (pascal-end-sentence (- 1 n)) (setq n 1)))
  (if (pascal-point-in-comment-or-string)
      (backward-sentence n)		; not pascal
    (while (pascal-in-paren 'go))
    (while (and (> n 1) (pascal-token-back)
		(progn
		  (if (not (looking-at ";")) (goto-char pascal-token-end))
		  (pascal-beginning-snts)))
      (setq n (1- n)))
    (pascal-token-back)
    (if (looking-at pascal-general-right-paren) (goto-char (match-end 0)))
    (pascal-beginning-snts)))

(defun pascal-end-sentence (&optional n)
  "Go to end of sentence.  With prefix argument N go N sentences forward."
  (interactive "p")
  (cond
   ((not n) (setq n 1))
   ((< n 1) (pascal-beginning-sentence (- 1 n)) (setq n 1)))
  (if (pascal-point-in-comment-or-string)
      (forward-sentence n)		; not pascal
    (while (pascal-in-paren 'go))
    (pascal-white-forward)
    (if (looking-at pascal-general-right-paren) (goto-char (match-end 0)))
    (while (and (> n 1) (pascal-end-snts)) (setq n (1- n)))
    (pascal-end-snts)))

(defun pascal-mark-sentence ()
  "Put region around current sentence."
  (interactive)
  (pascal-end-sentence 1)
  (pascal-forward-begin-line)
  (push-mark)
  (pascal-beginning-sentence 1)
  (pascal-backward-begin-line)
  (zmacs-activate-region))

(defun pascal-kill-sentence (&optional n)
  "Kill to end of current sentence.
With prefix argument N kill N following sentences."
  (interactive "p")
  (let ((p (point)))
    (pascal-end-sentence n)
    (kill-region p (point))))

(defun pascal-backward-kill-sentence (&optional n)
"Kill to beginning of current sentence.
With prefix argument N kill N preceding sentences."
  (interactive "p")
  (let ((p (point)))
    (pascal-beginning-sentence n)
    (kill-region (point) p)))

(defun pascal-transpose-sentences (&optional arg)
  "Interchange sentences around point, leaving point at end of them.
With prefix arg ARG, effect is to take sentence before or around point
and drag it forward past ARG other sentences (backward if ARG negative).
If ARG is zero, the sentences around or after point and around or after mark
are interchanged."
  (interactive "*p")
  (transpose-subr (function
		   (lambda (arg)
		     (if (< arg 0) (pascal-beginning-sentence (- arg))
		       (pascal-end-sentence arg))))
		  arg))

;; Parts of expressions.

(defvar pascal-left-parens
  '(
    (?\[      . pascal-match-left-bracket)
    (?\(      . pascal-match-left-paren)
    (?\'      . pascal-end-string)
    (?\)      . pascal-sexp-err)
    (?\]      . pascal-sexp-err)
    (?\;      . pascal-sexp-err)
    ("begin"  . pascal-match-begin)
    ("case"   . pascal-match-begin)
    ("record" . pascal-match-begin)
    ("repeat" . pascal-match-repeat)
    ("end"    . pascal-sexp-err)
    ("until"  . pascal-sexp-err)
    ("not"    . pascal-skip-forward-sexp)
    ("and"    . pascal-skip-forward-sexp)
    ("or"     . pascal-skip-forward-sexp)
    ("mod"    . pascal-skip-forward-sexp)
    ("div"    . pascal-skip-forward-sexp)
    ("in"     . pascal-skip-forward-sexp)
    ("to"     . pascal-skip-forward-sexp)
    ("downto" . pascal-skip-forward-sexp)
    ("else"   . pascal-skip-forward-sexp)
    ("then"   . pascal-skip-forward-sexp)
    ("of"     . pascal-skip-forward-sexp)
    ("do"     . pascal-skip-forward-sexp)
    ))

(defvar pascal-right-parens
  '((?\]      . pascal-match-right-bracket-try-prefix)
    (?\)      . pascal-match-right-paren-try-prefix)
    (?\'      . pascal-beginning-string)
    (?\(      . pascal-sexp-err)
    (?\[      . pascal-sexp-err)
    (?\;      . pascal-match-semi)
    ("begin"  . pascal-sexp-err)
    ("case"   . pascal-sexp-err)
    ("record" . pascal-sexp-err)
    ("repeat" . pascal-sexp-err)
    ("for"    . pascal-sexp-err)
    ("while"  . pascal-sexp-err)
    ("with"   . pascal-sexp-err)
    ("if"     . pascal-sexp-err)
    ("end"    . pascal-match-end-or-until)
    ("until"  . pascal-match-end-or-until)
    ("not"    . pascal-unary)
    ("and"    . pascal-backward-sexp)
    ("or"     . pascal-backward-sexp)
    ("mod"    . pascal-backward-sexp)
    ("div"    . pascal-backward-sexp)
    ("in"     . pascal-backward-sexp)
    ("to"     . pascal-backward-sexp)
    ("downto" . pascal-backward-sexp)
    ("then"   . pascal-backward-sexp)
    ("else"   . pascal-backward-sexp)
    ("of"     . pascal-backward-sexp)
    ("do"     . pascal-backward-sexp)
    ))

;; Move to the end of the next sexp (comment, (-) pair, [-] pair, begin-end,
;; case-end, repeat-until, string, or record-end).
(defun pascal-forward-sexp (&optional n)
  "Move forward across one balanced expression.
This includes record type, begin statement, case statement or repeat statement.
With prefix argument N move across N following expressions."
  (interactive "p")
  (if (not n) (setq n 1))
  (if (< n 0) (pascal-backward-sexp (- n))
    (if (or (pascal-point-in-comment-or-string)	; If in comment or string or
	    (and (looking-at pascal-comment-start-regexp) ; at beginning of
		 (goto-char (match-end 0)))) ; comment, get out of comment
	(progn (setq n (1- n)) (pascal-end-comment-or-string)))	; or string.
    (while (> (setq n (1- n)) -1)
      (pascal-internal-forward-sexp))))

(defun pascal-internal-forward-sexp ()
  ;; We are not im a comment or a string.
  (let ((c (pascal-see-token-forward))	; look at next token
	fn p)
    (cond
     ((> c 3)				; operator token
      (if (setq fn (assq c pascal-left-parens)) (funcall (cdr fn))
	(pascal-skip-forward-sexp)))	; default: skip past operator
     ((not (eq c 3)) (goto-char pascal-token-end)) ; number or string token
     ;; it is a symbol or a reserved word
     ((setq fn (pascal-get pascal-left-parens)) (funcall fn)) ; reserved word
     ((progn (goto-char pascal-token-end) ; try for function or procedure
	     (pascal-white-forward)	; call or array reference
	     (memq (setq c (following-char)) '(?\[ ?\()))
      ;; We are not at beinning of comment as pascal-white-forward would
      ;; have skipped that, so this must be a function or procedure call or
      ;; an array reference.
      (pascal-match-part-left-paren c))
     (t (goto-char pascal-token-end))))) ; just a symbol

;; Skip token just seen and move forward.
(defun pascal-skip-forward-sexp ()
  (goto-char pascal-token-end)
  (pascal-internal-forward-sexp))

;; move back to the start of the previous sexp (comment, (-) pair, [-] pair,
;; string, begin-end, repeat-until, case-end, or record-end).
(defun pascal-backward-sexp (&optional n)
  "Move backward across one balanced expression.
This includes record type, begin statement, case statement or repeat statement.
With prefix argument N move across N preceding expressions."
  (interactive "p")
  (if (not n) (setq n 1))
  (if (< n 0) (pascal-forward-sexp (- n))
    (if (or (pascal-point-in-comment-or-string) (pascal-after-comment))
	(progn (setq n (1- n)) (pascal-beginning-comment-or-string)))
    (let (fn c)
      (while (> (setq n (1- n)) -1)
	(cond
	 ((> (setq c (pascal-token-backward)) 3) ; operator
	  (if (setq fn (assq c pascal-right-parens)) (funcall (cdr fn))
	    (setq n (1+ n))))
	 ((and (eq c 3)	(setq fn (pascal-get pascal-right-parens)))
	  (funcall fn)))))))

(defun pascal-mark-sexp (&optional arg)
  "Set mark at end of current balanced expression.
With ARG set mark at end of ARGth following expression."
  (interactive "p")
  (push-mark
   (save-excursion
     (pascal-forward-sexp arg)
     (point)))
  (zmacs-activate-region))

(defun pascal-kill-sexp (&optional n)
  "Kill the syntactic expression following the cursor.
With prefix argument N, kill N expressions after (or before) the cursor."
  (interactive "p")
  (let ((opoint (point)))
    (pascal-forward-sexp n)
    (kill-region opoint (point))))

(defun pascal-transpose-sexps (&optional arg)
  "Interchange sexps around point, leaving point at end of them.
With prefix arg ARG, effect is to take sexp before or around point
and drag it forward past ARG other sexps (backward if ARG negative).
If ARG is zero, the sexps around or after point and around or after mark
are interchanged."
  (interactive "*p")
  (transpose-subr 'pascal-forward-sexp arg))

;; electric commands

(defun pascal-electric-semi ()
  "Insert ; blink at matching sentence start and may-bee do \\[pascal-newline-check-and-indent] ."
  (interactive "*")
  (insert ";")
  (if (pascal-point-in-comment-or-string) ()
    (pascal-do-electric-semi t)
    (if (and pascal-auto-newline (eolp)) (pascal-newline))))

(defun pascal-electric-period ()
  "Insert . and blink at matching sentence start."
  (interactive "*")
  (insert ".")
  (if (not (pascal-point-in-comment-or-string))
      (pascal-do-electric-semi nil)))

(defun pascal-do-electric-semi (semi)
  (let ((p (if pascal-blink-sentence-start
	       (pascal-blink-match
		(function pascal-match-semi)
		(if (not pascal-parse) blink-matching-paren-distance)
		pascal-blink-sentence-start-on-screen)))
	(pp (point))
	str)
;;; (if pascal-parse
;;;     (progn (if p (goto-char p)
;;;		 (goto-char (1- pp))
;;;		 (pascal-match-semi)
;;;		 (setq p (point)))
;;;	       (pascal-parse-snts)))
    (cond
     ((not (and pascal-put-end-comment (eolp)
		(forward-word -1) (looking-at "end\\>")))
      (goto-char pp))
     ((and (if p (goto-char p) (goto-char (1- pp)) (pascal-match-semi))
	   (not (pascal-do-end-comment (if semi pascal-put-end-comment) pp))
	   (not semi))
      (error "You can only put `.' after end matching program start!")))))

(defun pascal-insert-end-comment ()
  "Insert comment after end or top level begin."
  (interactive)
  (let ((pp (point)))
    (forward-word -1)
    (if (looking-at "[;.]") (forward-word -1))
    (cond
     ((looking-at "end\\>")
      (goto-char (match-end 0))
      (pascal-match-semi)
      (pascal-do-end-comment 2 pp))
     ((looking-at "begin\\>") (pascal-do-end-comment t pp))
     (t (goto-char pp)))))

(defun pascal-do-end-comment (semi pp)
  (let* ((p (point))
	 (str (if (or
		   (and (looking-at "begin\\>")
			(eq 2 (pascal-go-to-header t))
			(or semi (looking-at "program"))
			(progn (setq p (point))
			       (forward-word 1))
			(looking-at "[ \t]*\\w*"))
		   (and (not (eq semi t))
			(progn (goto-char p)
			       (looking-at "\\w*"))))
		  (buffer-substring-no-properties p (match-end 0)))))
    (goto-char pp)
    (if (null str) nil
      (if (not comment-start) (pascal-set-comment-delim))
      (insert " " comment-start str comment-end)
      t)))

(defun pascal-electric-string ()
  "Insert ' or ''."
  (interactive "*")
  (let (status)
    (if (or (not pascal-auto-string)
	    (eq 0 (setq status (pascal-point-in-comment-or-string))))
	(insert "'")
      (insert "''")
      (if (not status) (forward-char -1)))))

(defun pascal-electric-left-paren ()
  "Insert ( or ()."
  (interactive "*")
  (if (or (not pascal-auto-paren) (pascal-point-in-comment-or-string))
      (insert "(")
    (insert "()") (forward-char -1)))

(defun pascal-electric-right-paren ()
  "Insert ) and blink at matching ( ."
  (interactive "*")
  (insert ")")
  (pascal-blink-paren))

(defun pascal-electric-left-bracket ()
  "Insert [,  (., [] or (..) ."
  (interactive "*")
  (if (or (not pascal-auto-bracket) (pascal-point-in-comment-or-string))
      (insert "[")
    (if (or pascal-bracket-start (pascal-set-bracket))
	(insert pascal-bracket-start)
      (insert "[")
      (pascal-set-bracket))
    (if (not (eq pascal-auto-bracket t))
	(save-excursion (insert pascal-bracket-end)))))

(defun pascal-electric-right-bracket ()
  "Insert ] or .) and blink at matching [ or (. ."
  (interactive "*")
  (if (pascal-point-in-comment-or-string) (insert "]")
    (if (and pascal-auto-bracket (or pascal-bracket-end (pascal-set-bracket)))
	(insert pascal-bracket-end)
      (insert "]"))
    (pascal-blink-paren)))

(defun pascal-electric-left-curly ()
  "Insert {, (*, {} or (**) ."
  (interactive "*")
  (if (or (not pascal-auto-curly) (pascal-point-in-comment-or-string))
      (insert "{")
    (if (not comment-start) (pascal-set-comment-delim nil t))
    (insert pascal-comment-start)
    (if (not (eq pascal-auto-curly t))
	(save-excursion (insert pascal-comment-end)))))

(defun pascal-electric-right-curly ()
  "Insert } or *) and blink at matching { or (* ."
  (interactive "*")
  (let ((status (pascal-point-in-comment-or-string)))
    (cond
     ((eq 1 status) (insert "}"))
     ((null status)
      (insert "}") (message "Need to insert matching comment start"))
     (t (if (not pascal-auto-curly) (insert "}")
	  (if (not comment-start) (pascal-set-comment-delim nil t))
	  (insert pascal-comment-end))
	(pascal-blink-paren)))))

(defun pascal-electric-equal ()
  "Handle C-like assignments like += ."
  (interactive "*")
  (let ((p (point)) (c (preceding-char)) str)
    (cond
     ((or (pascal-point-in-comment-or-string)
	  (not (or
		(memq c pascal-equal-ops)
		(and (eq c ?.) (eq (char-after (- p 2)) ?^)))))
      (insert "="))
     ((not (setq str (pascal-search-back-skip-general-paren
		      pascal-assignment-start)))
      (error "Can't find beginning of assignment statement"))
     (t (goto-char str)
	(pascal-white-forward)
	(setq str (buffer-substring (point) p))
	(goto-char p)
	(delete-char (if (eq c '?\. ) -2 -1))
	(if (not (memq (setq c (preceding-char)) '(?\ ?\t))) (setq c nil))
	(insert ":=")
	(if c (insert c))
	(insert str)
	(if c (insert c))))))

(defvar pascal-template-mark "~<[^>]*>")

(defun pascal-beginnify-region (start end)
  "Wrap a begin statement around the region limited by START and END."
  (interactive "*r")
  (let ((atstart (= start (point))) one-line str pp ssw)
    (goto-char start)
    (save-excursion
      (beginning-of-line)
      (setq pp (point))
      (goto-char end)
      (setq end (point-marker))
      (if (bolp) (set-marker end (1- end))
	(beginning-of-line))
      (setq one-line (= pp (point)))
      (goto-char end)
      (delete-horizontal-space)
      (pascal-token-back)
      (setq ssw (looking-at ";")))
    (delete-horizontal-space)
    (if (not (bolp)) (insert " "))
    (setq str (buffer-substring (setq start (point)) end))
    (delete-region start end)
    (if (not (looking-at ";"))
	(progn
	  (if ssw (insert ";"))
	  (if (not (eolp)) (insert (if one-line " " "\n")))))
    (goto-char end)
    (insert-before-markers
     (abbrev-expansion (if one-line "be" "bg") pascal-template-table))
    (if (pascal-search-ignore-backward pascal-template-mark start)
	(delete-region (match-beginning 0) (match-end 0)))
    (search-backward "~@" start)
    (delete-char 2)
    (setq start (point-marker))
    (if atstart (insert str) (insert-before-markers str))
    (if one-line (insert " "))
    (goto-char end)
    (beginning-of-line 3)
    (indent-region pp (point) nil)
    (goto-char start)
    (set-marker end nil)
    (set-marker start nil)))

(defvar pascal-old-find-id nil)

(defun pascal-find-declaration (&optional useold)
  "Leave mark at point and go to definition for read id.
With prefix argument USEOLD, use old id as default, otherwise use id at point"
  (interactive "P")
  (let ((p (point)) id)
    (pascal-end-comment-or-string)
    (setq id
	  (read-string
	   "Find definition for id: "
	   (or
	    (and useold pascal-old-find-id)
	    (progn
	      (if (eq (char-syntax (preceding-char)) ?w)
		  (forward-word -1)
		(pascal-white-forward))
	      (and
	       (looking-at "\\w*")
;;;	       (or (looking-at "\\([a-zA-Z_]\\w*\\)")
;;;		   (and
;;;		    (not (pascal-in-paren))
;;;		    (looking-at "\\([0-9]+\\)[ \t]*:")))
;;;	       (buffer-substring-no-properties (point) (match-end 1))
	       (buffer-substring-no-properties (point) (match-end 0))
	       ))
	    pascal-old-find-id)))
    (goto-char p)
    (pascal-find-dekl id)		; error if not found
    (setq pascal-old-find-id id)
    (push-mark p)))

(defun pascal-compile (&optional def)
  "Compile current buffer.
With prefix argument DEF change compilation command."
  (interactive "P")
  (require 'compile)
  (if def
      (setq pascal-compile-format
	    (concat (read-string "Compile command format: ") " %s")))
  (let ((command
	 (read-from-minibuffer
	  "Compile command: "
	  (format pascal-compile-format
		  (file-name-nondirectory buffer-file-name) "")
	  nil nil '(compile-history . 1))))
    (save-some-buffers (not compilation-ask-about-save) nil)
    (compile-internal command "No more errors" "Pascal Compile"
		      nil pascal-error-regexp-alist)))

(defun pascal-run (&optional def)
  "Run a program.
With prefix argument DEF change run program command."
  (interactive "P")
  (require 'compile-in-shell)
  (if def
      (setq pascal-run-format
	    (concat (read-string "Run command format: ") " %s")))
  (let ((command
	 (read-from-minibuffer
	  "Run command: "
	  (format pascal-run-format
		  (file-name-nondirectory buffer-file-name) "")
	  nil nil '(compile-history . 1))))
    (save-some-buffers (not compilation-ask-about-save) nil)
    (compile-run-in-shell command "No more errors" "Pascal Run"
			  nil pascal-error-regexp-alist)))

(defun pascal-upcase-keywords (start end)
  "In region limited by START and END, convert all keywords to upper case.
The words affected are those matched by `pascal-standard-keywords' and
`pascal-extra-keywords'."
  (interactive "*r")
  (save-excursion
    (goto-char start)
    (let ((pascal-keywords
	   (concat
	    "['{]\\|(\\*\\|\\<"
	    (regexp-opt
	     (append pascal-standard-keywords pascal-extra-keywords)
	     t)
	    "\\>")))
      (while (pascal-search-forward pascal-keywords end)
	(upcase-word 1)))))
    
(defun pascal-capitalize-standardnames (start end)
  "In region, capitalize all predefined standard names.
The words affected are those matched by `pascal-standard-standardnames' and
`pascal-extra-standardnames'."
  (interactive "*r")
  (save-excursion
    (let ((pascal-standardnames
	   (concat
	    "['{]\\|(\\*\\|\\<"
	    (regexp-opt
	     (append pascal-standard-standardnames pascal-extra-standardnames)
	     t)
	    "\\>")))
      (goto-char start)
      (while (pascal-search-forward pascal-standardnames end)
	(capitalize-word 1)))))

(defun pascal-comment-out-lines (start end remcomment)
  "In region lines, comment out all code. If prefix argument, restore code.
Lines that have text (not only white space) in the region are included.
Commenting out is done by inserting value of `pascal-comment-out-start' at the
beginning of each line and after each *\) and \} and inserting value of
`pascal-comment-out-end' after each line.
If there is a prefix argument then out-commented code is restored by removing
all `pascal-comment-out-start' and `pascal-comment-out-end' in the lines.
The two strings `pascal-comment-out-start' and `pascal-comment-out-end'
may not appear for any other purpose in the program."
  (interactive "r\nP")
  (goto-char end)
  (skip-chars-backward pascal-white)
  (end-of-line)
  (setq end (point))
  (goto-char start)
  (skip-chars-forward pascal-white)
  (beginning-of-line)
  (save-restriction
    (narrow-to-region (point) end)
    (perform-replace pascal-comment-out-start "" nil nil nil)
    (goto-char (point-min))
    (perform-replace pascal-comment-out-end "" nil nil nil)
    (goto-char (point-max))
    (if (not remcomment)
	(let ((stat (pascal-point-in-comment-or-string))
	      (l (length pascal-comment-out-start))
	      p)
	  (if (eq stat 1) (error "String with no right limit"))
	  (goto-char (point-min))
	  (while (pascal-search-ignore-forward pascal-comment-end-regexp nil t)
	    (insert pascal-comment-out-start))
	  (goto-char (point-min))
	  (while (not (eobp))
	    (insert pascal-comment-out-start)
	    (end-of-line)
	    (setq p (point))
	    (skip-chars-backward pascal-white)
	    (if (search-backward pascal-comment-out-start (- (point) l) t)
		(delete-region (point) p)
	      (if (not (= p (point))) (delete-region (point) p))
	      (insert pascal-comment-out-end))
	    (beginning-of-line 2))
	  (if stat (insert pascal-comment-out-start))))))

;; A separate command to be used for the menu.
(defun pascal-uncomment-out-lines (start end)
  "In region lines, uncomment all code."
  (interactive "r")
  (pascal-comment-out-lines start end t))

;; Customization commands.

(defun pascal-return (&optional sw)
  "Toggle status of <cr> and <nl>.  One of them indent, the other doesn't.
With prefix argument SW, set <cr> to indent iff SW > 0."
  (interactive "P")
  (setq pascal-return
	(if sw (> (prefix-numeric-value sw) 0)
	  (not pascal-return)))
  (if pascal-return
      (progn
	(define-key pascal-mode-map "\C-m" 'pascal-newline-check-and-indent)
	(define-key pascal-mode-map "\C-j" 'newline))
    (define-key pascal-mode-map "\C-j" 'pascal-newline-check-and-indent)
    (define-key pascal-mode-map "\C-m" 'newline)))

(defun pascal-set-comment-delim (&optional sw ss)
  "If arg is nil (or no arg) use the comment delimiters that are used first
in buffer else if arg > 0 use (* and *) else use { and }."
  (interactive "P")
  (setq sw (if sw (> (prefix-numeric-value sw) 0)
	     (save-excursion
	       (goto-char (point-min))
	       (while (and (setq sw (pascal-search-ignore-forward
				     "['{]\\|(\\*" (point-max) t))
			   (eq (preceding-char) ?'))
		 (search-forward "'" (1+ (buffer-size)) t))
	       (cond
		(sw (setq ss nil) (eq (preceding-char) ?*))
		(ss nil)
		(pascal-comment-default (> pascal-comment-default 0))
		(t (y-or-n-p "Use (* for comment? "))))))
  (if ss nil
    (setq pascal-comment-start (if sw "(*" "{"))
    (setq pascal-comment-end (if sw "*)" "}"))
    (setq comment-start
	  (if sw pascal-comment-start-international
	    pascal-comment-start-ascii))
    (setq comment-end
	  (if sw pascal-comment-end-international pascal-comment-end-ascii))))

;; Internal indentation functions.

;; Insert a newline and indent old and new line.
(defun pascal-newline ()
  (let ((pascal-state-pos (make-marker)))
    (skip-chars-backward pascal-white-ln)
    (unwind-protect
	(if (not (bolp))		; If line not empty
	    (save-excursion (pascal-do-indent-line))) ; indent line.
      (newline)
      (pascal-do-indent-line (point) 2)	; Indent new code line.
      (set-marker pascal-state-pos nil))))

;; This is the function that does the indentation. It expects the
;; variable pascal-state-pos to contain a (possibly unset) marker.
;; All functions that call pascal-do-indent-line have to bind
;; pascal-state-pos.
(defun pascal-do-indent-line (&optional state-pos state)
  ;; Indent current line as pascal code.
  ;; Return the amount the indentation changed by.
  (let (indent ind-pos shift-amt cmt-col
;;; re-indent
	(pos (point)))
    (end-of-line)
    (setq ind-pos (point))
    (skip-chars-backward pascal-white-ln)
    (delete-region (point) ind-pos)	; Remove white space at end of line.
    (setq pos (- (point-max) (min pos (point))))
    (beginning-of-line)
    (skip-chars-forward pascal-white-ln)
    (setq indent (pascal-calculate-indent))
    (setq shift-amt (if indent (- indent (current-column)) 0))
    (setq ind-pos (point))
    (setq cmt-col (and (not pascal-last-state)
		       pascal-indent-comments-separately
		       (or pascal-fix-comment
			   (not (zerop shift-amt))
			   (not (eq pascal-indent-comments-separately t)))
		       (save-excursion (pascal-cmt-pos-on-line))))
    (if (zerop shift-amt) nil
      (beginning-of-line)
      (delete-region (point) ind-pos)
      (indent-to indent))
    ;; If initial point was within line's indentation,
    ;; position after the indentation.  Else stay at same point in text.
    (if (> (- (point-max) pos) (point))
	(goto-char (- (point-max) pos)))
    (if cmt-col (save-excursion (pascal-reposition-comment cmt-col)))
;;;    (if re-indent (pascal-re-indent re-indent))
    shift-amt))

(defun pascal-cmt-pos-on-line ()
  (let ((end (save-excursion (end-of-line) (point)))
	next r)
    (if (looking-at pascal-comment-start-regexp) (goto-char (match-end 0)))
    (while (and (setq r (re-search-forward pascal-comment-start-regexp end t))
		(progn
		  (setq next (point))
		  (goto-char (match-beginning 0))
		  (setq pascal-last-state
			(pascal-point-in-comment-or-string))))
      (set-marker pascal-state-pos (point))
      (goto-char next))
    (if (or (not r)
	    (save-excursion (not (and (pascal-skip-comment-forward t)
				      (eolp)))))
	nil			; Return nil if not comment at end of this line
      (set-marker pascal-state-pos (point))
      (current-column))))

(defun pascal-reposition-comment (col)
  (goto-char pascal-state-pos)
  (let (end (ccol (current-column)))
    (if (not (eq pascal-indent-comments-separately t))
	(setq col comment-column))
    (if pascal-fix-comment
	(progn
	  (setq end (+ (save-excursion (end-of-line) (current-column))
		       (- col ccol)))
	  (while (> end pascal-right-margin)
	    (setq col (- col pascal-fix-comment))
	    (setq end (- end pascal-fix-comment)))))
    (if (= col (current-column)) ()
      (skip-chars-backward pascal-white-ln)
      (if (and pascal-fix-comment
	       (> (setq ccol (1+ (current-column))) col))
	  (while (and (> ccol col)
		      (<= (setq end (+ end pascal-fix-comment))
			  pascal-right-margin))
	    (setq col (+ col pascal-fix-comment))))
      (delete-region (point) pascal-state-pos)
      (indent-to col 1))))

(defun pascal-calculate-indent ()
  (setq pascal-last-state (pascal-point-in-comment-or-string))
  (set-marker pascal-state-pos (point))
  (save-excursion
    (cond
     (pascal-last-state
      ;; This must be a comment as strings don't continue past end of line.
      (if pascal-indent-comment-fn (funcall pascal-indent-comment-fn)))
     ((looking-at pascal-label-regexp) (pascal-indent-label))
     (t (pascal-do-calculate-indent)))))

;; pascal-indent-comment-fn
(defun pascal-calculate-indent-within-comment ()
  "Return the indentation amount for line that is part of a block comment."
  (let (end star-start)
    (save-excursion
      (beginning-of-line)
      (skip-chars-forward pascal-white-ln)
      (setq star-start (memq (following-char) pascal-star-starts))
      (skip-chars-backward pascal-white)
      (setq end (point))
      (beginning-of-line)
      (skip-chars-forward pascal-white-ln)
      (and (re-search-forward comment-start-skip end t)
	   star-start
	   (goto-char (1+ (match-beginning 0))))
      (current-column))))

(defun pascal-indent-label ()
  (let ((lab (buffer-substring (point) (match-end 1)))
	(col (buffer-substring (match-beginning 2) (match-end 2)))
	(bl (if (= (match-end 3) (match-end 2)) " "
	      (buffer-substring (match-beginning 3) (match-end 3))))
	(labend (match-end 3))
	indent)
    (if (not (pascal-in-composite)) (pascal-indent-relative)
      (delete-region (point) labend)
      (setq indent (or (pascal-do-calculate-indent) (pascal-indent-relative)))
      (goto-char pascal-state-pos)
      (insert lab)
      (setq indent (max 0 (- indent (length lab))))
      (if (< indent (+ (length col) (length bl)))
	  (progn (setq col ":") (setq bl " ")))
      (insert col bl)
      (max 0 (- indent (length col) (length bl))))))

(defvar pascal-indent-fn
  '(
    ("program"   . pascal-ret-zero)	; No indentation
    ("\f"        . pascal-ret-zero)	; New page, no indentation
    ("#"         . pascal-ret-zero)	; # prefix, no indentation
    ("{"         . pascal-indent-paren)	; Beginning of comment
    ("("         . pascal-indent-paren)	; Possibly beginning of comment
    (")"         . pascal-indent-right-paren)
    ("procedure" . pascal-indent-proc-head)
    ("function"  . pascal-indent-proc-head)
    ("label"     . pascal-indent-decl)
    ("const"     . pascal-indent-decl)
    ("type"      . pascal-indent-decl)
    ("var"   . pascal-indent-var-decl)	; Variable declaration or var parameter
    ("begin"     . pascal-indent-block)
    ("end"       . pascal-indent-end)
    ("until"     . pascal-indent-end)
    ("forward"   . pascal-indent-forward)
    ("external"  . pascal-indent-forward)
    ("extern"    . pascal-indent-forward)
    ("else"      . pascal-indent-else)
    ("then"      . pascal-indent-then)
    ("do"        . pascal-indent-do)
    ("of"        . pascal-indent-of)
    ))

(defun pascal-get-indent (indent)
  ;; INDENT is either an integer or nil or a symbol for a factor times
  ;; pascal-default-indent.  Returns an integer or nil.
  ;; This allows indentation customizing variables to be given in
  ;; terms of pascal-default-indent so that only changing pascal-default-indent
  ;; will change all indentation.  This idea is taken from cc-mode.el.
  (cond
   ((integerp indent) indent)
   ((null indent) nil)
   ((eq indent '+) pascal-default-indent)
   ((eq indent '++) (* 2 pascal-default-indent))
   ((eq indent '*) (/ pascal-default-indent 2))
   ((eq indent '-) (- pascal-default-indent))
   ((eq indent '--) (* -2 pascal-default-indent))
   ((eq indent '/) (/ pascal-default-indent -2))
   (t (error "Unknown indent %s" indent))))

(defun pascal-do-calculate-indent ()
  (let ((fn (pascal-match-get "\\w+\\|\\W" pascal-indent-fn)))
    (if fn (pascal-get-indent (funcall fn))
      (pascal-indent-relative))))

(defun pascal-indent-paren ()
  (if (looking-at pascal-comment-start-regexp)
      nil			; Beginning of comment. Keep current indent.
    (pascal-indent-relative)))
    
(defun pascal-indent-right-paren ()
  (or (pascal-in-paren-indent) (error "Missing left parenthesis")))

(defun pascal-ret-zero () (or 0))

(defun pascal-indent-decl ()
  ;; Indent declarations relative to closest preceding program,
  ;; procedure or function.
  (cond
   ((not (pascal-search-head)) 0)
   ((looking-at "program\\>") pascal-program-decl-indent)
   (t (pascal-add-indent pascal-procedure-decl-indent))))

(defun pascal-indent-var-decl ()
  ;; Either indent var declaration relative to closest preceding program,
  ;; procedure or function, or indent inside function or procedure header.
  (or (pascal-in-paren-indent)
      (pascal-indent-decl)))		;  or a declaration

(defun pascal-indent-proc-head ()
  ;; Indent procedure relative to closest preceding or surrounding procedure.
  (let (r)
    (cond
     ((pascal-in-paren-indent))
     ((null (setq r (pascal-go-to-header))) 0)
     ((eq r 0) (error "Procedure or function inside begin"))
     ((looking-at "program\\>") pascal-program-procedure-indent)
     (t (pascal-add-indent
	 (if (eq r 1) 0 pascal-procedure-procedure-indent))))))

(defun pascal-indent-block ()
  ;; Indent block relative procedure or preceding or surrounding block.
  (let* ((p (point)) (r (pascal-go-to-header t)))
    (cond
     ((null r) 0)
     ((eq r 0) (goto-char p)		; nested begin
      (pascal-indent-relative))
;;      (+ (pascal-indent-relative) pascal-block-indent))
     ((looking-at "program\\>") pascal-program-block-indent)
     (t (pascal-add-indent (if (eq r 2) pascal-procedure-block-indent 0))))))

(defun pascal-indent-forward ()
  (if (or (not (pascal-search-head)) (looking-at "program\\>"))
      pascal-program-block-indent
    (pascal-add-indent pascal-procedure-block-indent)))

(defun pascal-indent-end ()
  ;; Indent end or until relative matching begin, record, case or repeat.
  (cond
   ((not (pascal-match-end-or-until)) 0)
   ((and (looking-at pascal-indent-parens)
	 (> (save-excursion (end-of-line) (point))
	    (save-excursion (forward-word 1) (pascal-white-forward) (point))))
    (+ (current-column) (pascal-get-indent pascal-block-end-indent)))
   (t (pascal-add-indent pascal-block-end-indent))))

(defun pascal-indent-else ()
  ;; Indent else statement with respect to beginning of if statement.
  (cond
   ((save-excursion (and (pascal-token-back) (looking-at "of\\>\\|;")))
    (pascal-indent-relative)) ; default clause in a turbo pascal case statement
   ((pascal-find-matching-if) (pascal-add-indent pascal-if-else-indent))
   (t 0)))

(defun pascal-indent-then ()
  (if (pascal-find-matching-if) (pascal-add-indent pascal-if-then-indent) 0))

(defun pascal-indent-do ()
  (if (pascal-match-do) (pascal-add-indent pascal-do-indent) 0))

(defun pascal-indent-of ()
  (if (pascal-match-of)
      (pascal-add-indent (if (looking-at "case") pascal-block-statement-indent
			   pascal-of-type-indent))
    0))

(defun pascal-indent-relative ()
  ;; Indent current line relative to nearest preceding not ignored line.
  (let ((ep (point)) p)
    (beginning-of-line)
;    (while (and (setq p (point))
;		(zerop (forward-line -1))
;		(looking-at pascal-dont-indent)))
;    (goto-char p)
    (if (not (pascal-token-back)) 0
      (pascal-add-indent (pascal-try-indent ep)))))

(defun pascal-add-indent (ind)
  (let ((p (point)) (stop (if (listp ind) (setq ind (car ind)))) n)
    (setq ind (pascal-get-indent ind))
    (while (and (not stop)
		(not (looking-at pascal-relative-indents))
		(not (pascal-in-indent))
		(pascal-token-back)
		(not (looking-at pascal-indent-parens)))
      (setq n (pascal-try-indent p))
      (if (setq stop (listp n)) (setq n (car n)))
      (setq ind (+ ind (pascal-get-indent n)))
      (setq p (point)))
    (goto-char p)
    (+ ind (current-column))))

(defun pascal-in-indent ()
  (save-excursion (skip-chars-backward pascal-white-ln) (bolp)))

(defvar pascal-indent-relative-fn
  '(
    ("("	 . pascal-ret-one)
    ("["	 . pascal-ret-one)
    ("."	 . pascal-ret-one)
    (","	 . pascal-indent-after-comma)
    (":"         . pascal-indent-after-colon)
    (";"         . pascal-indent-after-semi)
    ("end"       . pascal-indent-after-end)
    ("until"     . pascal-indent-after-until)
    ("else"      . pascal-indent-in-if)
    ("then"      . pascal-indent-in-if)
    ("do"        . pascal-indent-in-do)
    ("program"   . pascal-indent-in-function)
    ("procedure" . pascal-indent-in-function)
    ("function"  . pascal-indent-in-function)
    ("label"     . pascal-indent-in-decl)
    ("const"     . pascal-indent-in-decl)
    ("type"      . pascal-indent-in-decl)
    ("var"       . pascal-indent-in-decl)
    ("begin"     . pascal-indent-in-block)
    ("repeat"    . pascal-indent-in-block)
    ("case"      . pascal-indent-in-block)
    ("of"        . pascal-indent-in-of)
    ("record"    . pascal-indent-in-record)
    ))
       
(defun pascal-try-indent (p)
  (let ((fn (pascal-get pascal-indent-relative-fn)))
    (if fn (funcall fn)
      (goto-char pascal-token-end)
      (pascal-search-back-skip-general-paren
       pascal-continued-statement-start 'case)
      (list pascal-continued-statement-indent))))

(defun pascal-ret-one () (or 1))

(defun pascal-indent-after-comma ()
  (let ((p (pascal-match-comma-backward))
	(ind pascal-continued-list-indent))
    (if (not p) 0
      (if (if (looking-at "[,[]\\|(\\.") (setq p (match-end 0))
	    (and (looking-at "(")
		 (not (progn (pascal-token-back) (pascal-token-back)
			     (pascal-seing-head)))))
	  (setq ind 0))
      (goto-char p)
      (pascal-white-forward)
      ind)))

(defun pascal-indent-after-colon ()
  (pascal-search-back-skip-general-paren
   pascal-continued-statement-start 'case)
  (list pascal-continued-statement-indent))

(defun pascal-indent-after-semi ()
  (if (pascal-in-paren 'go)
      (progn
	(forward-char 1)
	(pascal-white-forward)
	(if (looking-at pascal-param-type-regex)
	    (progn (goto-char (match-end 0))
		   (pascal-white-forward)))
	0)
    (pascal-beginning-snts)
    (cond
     ((looking-at "program\\>") pascal-program-decl-indent)
     ((pascal-seing-head) pascal-procedure-decl-indent)
     (t (if (looking-at pascal-label-regexp)
	    (let ((p (match-end 3)))
	      (if (pascal-in-composite) (goto-char p))))
	0))))

(defun pascal-indent-after-end ()
  (pascal-match-end-or-until)
  (if (pascal-match-end-or-until t) pascal-block-end-indent 0))

(defun pascal-indent-after-until ()
  (pascal-match-end-or-until)
  pascal-block-statement-indent)

(defun pascal-indent-in-of ()
  (pascal-match-of)
  (if (looking-at "case") pascal-block-statement-indent
    pascal-of-type-indent))

(defun pascal-indent-in-if ()
  (let ((ind (pascal-block-follow)))
    (pascal-find-matching-if)
    (- (pascal-get-indent pascal-if-then-indent) ind)))

(defun pascal-indent-in-do ()
  (let ((ind (pascal-block-follow)))
    (pascal-match-do)
    (- (pascal-get-indent pascal-do-indent) ind)))

(defun pascal-block-follow ()
  (if (and pascal-ignore-block-after-do
	   (save-excursion
	     (goto-char pascal-token-end)
             (pascal-white-forward)
             (looking-at "begin\\>")))
      (pascal-get-indent pascal-block-statement-indent)
    0))

(defun pascal-indent-in-function ()
  pascal-continued-statement-indent)

(defun pascal-indent-in-decl ()
  pascal-decl-decl-indent)

(defun pascal-indent-in-block ()
  pascal-block-statement-indent)

(defun pascal-indent-in-record ()
  pascal-record-field-indent)

;; Internal movement functions. They all return true when successful.
;; Some of them take an argument nocp which controls what to do if a search
;; is unsuccessful. If nocp is true, return nil, otherwise complain.

(defun pascal-missing (m) (error "Missing '%s'" m) nil)

(defun pascal-backward-begin-line ()
  ;; Go back to beginning of line skipping space, tab and entire comments.
  ;; Don't move at all if finding code.
  (let ((p (point)))
    (skip-chars-backward " \t")
    (if (or (bolp)
	    (and (forward-char -2)
		 (looking-at "\\*)\\|.}")
		 (pascal-skip-comment-backward t)
		 (pascal-backward-begin-line)))
	t
      (goto-char p)
      nil)))

(defun pascal-forward-begin-line ()
  ;; Go forward to beginning of next line skipping space, tab and entire
  ;; comments. Don't move at all if finding code.
  (let ((p (point)))
    (skip-chars-forward " \t")
    (if (eolp) (progn (beginning-of-line 2) t)
      (if (and (looking-at "{\\|(\\*")
	       (pascal-skip-comment-forward t)
	       (pascal-forward-begin-line))
	  t
	(goto-char p)
	nil))))

(defun pascal-go-to-header (&optional noprec)
  ;; Try go to beginning of header of procedure.
  ;; Return nil if no header, 0 if inside statement, 1 if header of preceding
  ;; procedure and 2 if surrounding procedure.
  ;; If NOPREC is true then preceding procedures will be skipped and the search
  ;; for a surrounding procedure continued, i.e. the answer will never be 1.
  (let ((loop t) r)
    (while (pascal-in-paren 'go))	; Get out of parenthesis
    (while loop
      (setq loop nil)
      (cond
       ((not (pascal-search-backward pascal-head-backward-regex))
	(setq r nil))			; no preceding or surrounding procedure
       ((looking-at "b") (setq r 0))	; Seeing BEGIN
       ((looking-at "ex\\|fo")		; Seeing EXTERN or FORWARD
	(and (pascal-search-head)	; get header
	     (setq r 1)			; preceding procedure
	     (setq loop noprec)))
       ((not (looking-at "en")) (setq r 2)) ; surrounding procedure
       ((progn (pascal-match-end-or-until) ; Seeing END.
	       (setq loop (looking-at "rec")))) ; In declaration (RECORD)
       (t (setq r (pascal-go-to-header t)) ; At BEGIN, skip sub procedures
    	  (if (eq r 2) (progn (setq r 1) (setq loop noprec))))))
    r))

(defun pascal-skip-procedure (&optional nocp)
  ;; Looking at beginning of procedure. Go to end of procedure.
  (let ((p (point)) r c)
    (if (setq r (pascal-end-snts))
	(while (and (setq r (pascal-search-forward
			     pascal-procedure-forward-regex))
		    (looking-at "p\\|fu")
		    (setq r (pascal-skip-procedure nocp)))))
    (if r (pascal-end-snts)
      (goto-char p)
      (if (not nocp) (pascal-missing "end of procedure")))))

(defun pascal-find-dekl (id &optional nocp)
  (let ((case-fold-search (not id-case-significant))
	(pattern (concat "['{]\\|(\\*\\|"
			 "\\<"
			 (regexp-opt
			  (cons id
				'("begin" "label" "const" "type" "var"
				  "procedure" "function"))
			  t)
			 "\\>"))
;;;	(label (eq 0 (string-match "[0-9]" id)))
	(idsearch  (concat "['{(]\\|" id "\\>"))
	(p (point)) (found nil) (forward nil) r pp fp fpe lim)
    (setq id (concat id "\\>"))
    (pascal-end-comment-or-string)
    (setq r (point))
    (while (and r (not found))
      (goto-char r)
      (setq lim r)
      (while (and (setq r (pascal-go-to-header))
		  (not (or (eq r 2)	; surrounding procedure
			   (and forward (eq r 1)))))) ; preceding procedure
      (cond
       ((not r))			; procedure not found
       ((progn (if (eq r 2) (setq forward nil))
	       (setq r (point))
	       (save-excursion
		 (or
		  (not (pascal-end-snts t))
		  (> (setq pp (point)) lim))))) ; pp is end of header.
       (forward
	(if (pascal-proc-is forward)
	    (progn
	      (setq forward nil)
	      (forward-word 1)
	      (pascal-white-forward)
	      (and
	       (looking-at "(")		; there are parameters
	       (setq found (pascal-is-id-declaration idsearch pp))))))
       ((and ; (not label)
	 (setq found (pascal-proc-is id)))) ; name of procedure
       ((or ; label
	    (progn
	      (setq fp (point))
	      (forward-word 1)
	      (setq fpe (point))
	      (pascal-white-forward)
	      (if (looking-at "(")		; there are parameters
		  (not (setq found (pascal-is-id-declaration idsearch pp)))
		;; no parameters. maybe body of forwarded function
		(setq forward (concat (buffer-substring fp fpe) "\\>")))))
	(goto-char pp)
	(setq pp nil)
	(while (and (pascal-search-forward pattern lim 1)
		    (cond
		     ((looking-at id)
		      (if pp (setq found (pascal-is-id-declaration)))
		      nil)
		     ((looking-at "[fp]")
		      (setq pp nil)
		      (if (or ; label
			      (not (setq found (pascal-proc-is id))))
			  (pascal-skip-procedure t)))
		     ((not (looking-at "b"))
		      (setq pp (and ; (if (looking-at "l") label (not label))
				    (point)))
		      (forward-word 1)
		      t)))))))
    (or found
	(progn (goto-char p)
	       (if (not nocp) (error "Can't find definition"))))))

(defun pascal-search-back-skip-general-paren (pattern &optional trycase)
  ;; Search backward for PATTERN while skipping general parens.
  (let (r p c)
    (while (and (setq r (pascal-search-ignore-backward pattern (point-min) 1))
		(progn (setq p (match-end 0))
		       (looking-at pascal-general-right-paren)))
      (if (looking-at "[]'})]")
	  (pascal-match-part-right-paren (following-char))
	(pascal-match-end-or-until)))
    (if (not trycase) (and r p)
      (setq trycase (and (eq trycase 'case) (looking-at "case\\>")))
      ;; OF as a delimiter for sentences gives a problem: it is a delimiter in
      ;; a case statement but not in array, file or set declarations. To avoid
      ;; this problem we search for case instead and then search forward for OF
      (goto-char p)
      (and trycase (pascal-search-forward "[{(']\\|\\<of\\>")
	   (goto-char (match-end 0)))
      (pascal-white-forward))))

(defun pascal-beginning-snts ()
  ;; Search back for sentence start, while skipping parens.
  (pascal-search-back-skip-general-paren
   pascal-sentence-left-delim 'case))

(defun pascal-match-semi ()
  ;; Get to parameter, block or procedure level.
  (if (pascal-in-paren)
      (pascal-search-back-skip-general-paren "[')}(;]" 'case)
    (pascal-beginning-snts)))

(defun pascal-end-snts (&optional nosemi)
  (let (r p)
    (while
	(and
	 (setq r
	   (pascal-search-ignore-forward
       "[;({']\\|\\<\\(end\\|until\\|begin\\|case\\|re\\(cord\\|peat\\)\\)\\>"
	    (point-max) 1))
	 (progn (goto-char (match-beginning 0)) (not (looking-at "[;eu]"))))
      (cond
       ((looking-at "rep\\>") (pascal-match-repeat))
       ((looking-at "[bcr]") (pascal-match-begin))
       (t (pascal-match-part-left-paren (following-char)))))
    (cond
     ((and (not nosemi) (looking-at ";")) (forward-char 1))
     (t (pascal-white-backward)))
    r))

(defun pascal-match-comma-backward ()
  (pascal-search-back-skip-general-paren
   "[]')};,([]\\|\\<\\(end\\|until\\|of\\|label\\|var\\)\\>"))

(defun pascal-sexp-err () (error "Containing expression ends prematurely"))

(defun pascal-match-right-bracket-try-prefix ()
  (pascal-match-right-bracket)
  (pascal-try-prefix))

(defun pascal-match-right-paren-try-prefix ()
  (pascal-match-right-paren)
  (pascal-try-prefix))

(defun pascal-try-prefix ()
  (let ((p (point)))
    (pascal-token-back)
    (if (or (looking-at "\\W")
	    (and (setq fn (pascal-get pascal-right-parens))
		 (not (eq fn 'pascal-unary))))
	(goto-char p))))

(defun pascal-unary () (pascal-sexp-err))

;; Other functions that move

;;find match for ', \[, { or \(
(defun pascal-match-part-left-paren (c)
  (cond
   ((eq c ?\') (pascal-end-string))
   ((eq c ?\{) (pascal-skip-comment-forward))
   ((eq c ?\[) (pascal-match-left-bracket))
   ;; c is ?\( , is it followed by ?\* or ?\. ?
   ((eq (setq c (char-after (1+ (point)))) ?\*) (pascal-skip-comment-forward))
   ((eq c ?.) (pascal-match-left-bracket))
   (t (pascal-match-left-paren))))

(defun pascal-beginning-part-paren-pos ()
  (pascal-match-part-right-paren (following-char )))

;;find match for ', \], } or \)
(defun pascal-match-part-right-paren (c)
  (cond
   ((eq c ?\') (pascal-beginning-string))
   ((eq c ?\]) (pascal-match-right-bracket))
   ((eq c ?\}) (pascal-skip-comment-backward))
   ;; c is ?\) , is it preceded by ?\* or ?\. ?
   ((eq (setq c (preceding-char)) ?\*)
    (forward-char -1) (pascal-skip-comment-backward))
   ((eq c ?\.) (forward-char -1) (pascal-match-right-bracket))
   (t (pascal-match-right-paren))))

(defun pascal-match-begin (&optional nocp)
  ;; Find end matching begin or case.
  (let (r)
    (forward-char 1)			; don't see this begin (or case) again
    (while (and (setq r (pascal-search-forward
			 "['{(]\\|\\<\\(end\\|begin\\|case\\)\\>"))
		(not (looking-at "end")))
      (pascal-match-begin))
    (cond
     (r (goto-char (match-end 0)))
     ((not nocp) (pascal-missing "end")))
    r))

(defun pascal-match-end-or-until (&optional nocp)
  ;; Find repeat matching until or begin, case, record matching end.
  (let ((r (pascal-search-back-skip-general-paren
      "['})]\\|\\<\\(end\\|until\\|begin\\|repeat\\|case\\|record\\)\\>")))
    (cond
     ((looking-at "case")		; Might be in a record
      (let ((p (point)))
	(pascal-match-end-or-until t)
	(if (not (looking-at "record")) (goto-char p)))
      t)
     ((or r nocp) r)
     (t (pascal-missing "begin, case, record or repeat")))))

(defun pascal-match-repeat (&optional nocp)
  ;; Find until matching repeat.
  (let (r)
    (forward-char 1)
    (while (and (setq r (pascal-search-forward
			 "['{(]\\|\\<\\(until\\|repeat\\)\\>"))
		(looking-at "r"))
      (pascal-match-repeat))
    (cond
     (r (goto-char (match-end 0)))
     ((not nocp) (pascal-missing "until")))
    r))

(defun pascal-find-matching-if (&optional nocp)
  ;; Looking at else or then. Find beginning of if statement.
  (let (r p)
    (while (and (setq r (pascal-search-backward
			 "['})]\\|\\<\\(end\\|until\\|if\\|else\\)\\>"))
		(not (looking-at "i")))
      (cond
       ((looking-at "el") (pascal-find-matching-if))
       (t (pascal-match-end-or-until))))
    (if (not r) (if nocp nil (pascal-missing "if"))
      (setq p (point))
      (if (and (pascal-token-back) (looking-at "else\\>"))
	  (pascal-find-matching-if)
	(goto-char p) r))))

(defun pascal-match-do (&optional nocp)
  ;; Looking at do. Find beginning of for, while or with statement.
  (or (pascal-search-backward "['})]\\|\\<\\(while\\|for\\|with\\)\\>")
      (if (not nocp) (pascal-missing "while, for or with"))))

(defun pascal-match-of (&optional nocp)
  ;; Looking at of. Find beginning of case, array, set or file.
  (or (pascal-search-backward "['})]\\|\\<\\(case\\|array\\|set\\|file\\)\\>")
      (if (not nocp) (pascal-missing "case, array, file or set"))))

(defun pascal-white-forward ()
  (let ((c nil))
    (while (progn (skip-chars-forward pascal-white)
		  (or (setq c (looking-at pascal-comment-start-regexp))
		      (and pascal-start-ignored-line
			   (looking-at pascal-start-ignored-line))))
      (if c (pascal-skip-comment-forward)
	(end-of-line)))))

(defun pascal-white-backward ()
  (let ((c nil) p)
    (while (progn (skip-chars-backward pascal-white)
		  (or (setq c (pascal-after-comment))
		      (setq p (pascal-ignore-line))))
      (if (not c) (goto-char p)
	(if (not (pascal-skip-comment-backward)) (setq c nil))))
    c))

(defun pascal-after-comment ()
  (let ((c (preceding-char)))
    (cond
     ((eq c ?\}) (forward-char -1) c)
     ((not (eq c ?\))) nil)
     ((and (> (1- (point)) (point-min)) (eq (char-after (- (point) 2)) ?\*))
      (forward-char -2) c))))

(defun pascal-search-ignore-forward (regexp &optional bound noerror)
  ;; As re-search-forward, but skip lines headed by pascal-dont-indent
  (let ((p (point)) (r (re-search-forward regexp bound noerror)) pp)
    (while (and r (setq pp (pascal-ignore-line)))
      (goto-char pp)
      (setq r (re-search-forward regexp bound noerror)))
    (and (null r) (eq noerror t) (goto-char p))
    r))

(defun pascal-search-ignore-backward (regexp &optional bound noerror)
  ;; As re-search-backward, but skip lines headed by pascal-dont-indent
  (let ((p (point)) (r (re-search-backward regexp bound noerror)) pp)
    (while (and r (setq pp (pascal-ignore-line)))
      (goto-char pp)
      (setq r (re-search-backward regexp bound noerror)))
    (and (null r) (eq noerror t) (goto-char p))
    r))

(defun pascal-search-forward (pattern &optional lim stay)
  ;; pattern must contain ['{]\\|(\\*
  (let (r c)
    (if (not lim) (setq lim (point-max)))
    (if (not stay) (setq stay t))
    (while (and (setq r (pascal-search-ignore-forward pattern lim stay))
		(progn (goto-char (match-beginning 0))
		       ;; keep match data by not using looking-at here
		       (memq (setq c (following-char)) '(?\' ?\{ ?\())))
      (pascal-match-part-left-paren c))
    r))

(defun pascal-search-backward (pattern)
  (let (r c (lim (point-min)))
    (while (and (setq r (pascal-search-ignore-backward pattern lim t))
		;; keep match data by not using looking-at here
		(memq (setq c (following-char)) '(?\' ?\} ?\))))
	(pascal-match-part-right-paren c))
    r))

(defun pascal-match-right-bracket (&optional nocp)
  "Looking at ']' or '.)'. Find matching '[' or '(.'.
If optional NOCP is true, don't complain if no match is found."
  (let (c)
    (while (progn (skip-chars-backward "^([]'})" (point-min))
		  (not (memq (setq c (preceding-char)) '(0 ?\( ?\[))))
      (forward-char -1)
      (pascal-match-part-right-paren c))
    (if (not (or (eq c ?\[) (and (eq c ?\() (eq (following-char) ?\.))))
	(if nocp nil (pascal-missing "[ or (."))
      (forward-char -1)
      c)))
      
(defun pascal-match-left-bracket (&optional nocp)
  "Looking at '[' or '(.'. Find matching ']' or '.)'.
If optional NOCP is true, don't complain if no match is found."
  (let (c)
    (forward-char (if (looking-at "\\[") 1 2))
    (while (progn (skip-chars-forward "^[]'({)" (point-max))
		  (not (memq (setq c (following-char)) '(0 ?\) ?\]))))
      (pascal-match-part-left-paren c))
    (cond
     ((or (eq c ?\]) (and (eq c ?\)) (eq (preceding-char) ?.)))
      (forward-char 1) t)
     (nocp nil)
     (t (pascal-missing "] or .)")))))

(defun pascal-set-bracket ()
  (let (r)
    (save-excursion
      (goto-char (point-min))
      (while (and
	      (setq r (pascal-search-ignore-forward
		       "[['{]\\|(\\*\\|(\\." (point-max) t))
	      (progn (goto-char (match-beginning 0))
		     (looking-at "[{']\\|(\\*")))
	(if (looking-at "'") (pascal-end-string)
	  (pascal-skip-comment-forward)))
      (cond
       ((not r) nil)
       ((looking-at "\\[")
	(setq pascal-bracket-start "[") (setq pascal-bracket-end "]"))
       (t (setq pascal-bracket-start "(.") (setq pascal-bracket-end ".)"))))))

(defun pascal-match-left-paren (&optional nocp)
  "Looking at a left paren.  Find a matching right paren.
If optional NOCP is true, don't complain if no match is found."
  (let (c)
    (forward-char 1)
    (while (progn (skip-chars-forward "^'({)" (point-max))
		  (not (memq (setq c (following-char)) '(0 ?\)))))
	(pascal-match-part-left-paren c))
    (cond
     ((eq c ?\)) (forward-char 1) t)
     (nocp nil)
     (t (pascal-missing ")")))))
      
(defun pascal-match-right-paren (&optional nocp)
  "Find a matching left paren.
If optional NOCP is true, don't complain if no match is found."
  (let (c)
    (while (progn (skip-chars-backward "^('})" (point-min))
		  (not (memq (setq c (preceding-char)) '(0 ?\())))
      (forward-char -1)
      (pascal-match-part-right-paren c))
    (if (not (eq c ?\()) (if nocp nil (pascal-missing "("))
      (forward-char -1)
      c)))
      
(defun pascal-seing-head ()
  (looking-at pascal-head-regex))

(defun pascal-search-head ()
  (pascal-search-back-skip-general-paren pascal-head-back-regex))

(defun pascal-proc-is (id)
    (forward-word 1)
    (pascal-white-forward)
    (looking-at id))

(defun pascal-is-id-declaration (&optional idsearch pp)
  (and
   (or (null pp)
       (progn (forward-char 1)
	      (pascal-search-forward idsearch pp 1)))
   (save-excursion
     (pascal-token-back)
     (looking-at pascal-id-declare-regex))))

(defun pascal-in-composite ()
  ;; Are we at top level in a begin or repeat? I.e. is it reasonable with a
  ;; label here?
  (save-excursion
    (pascal-token-back)
    (if (looking-at ";")
	(pascal-search-back-skip-general-paren
	 pascal-contain-sentence-left-delim))
    (looking-at "begin\\|repeat")))

(defun pascal-in-paren-indent ()
  (if (pascal-in-paren 'go) (1+ (current-column))))

(defun pascal-in-paren (&optional go)
  ;; Without argument (or argument nil) this is a predicate.
  ;; With argument not nil go back to unmatched "(" if found.
  (let ((p (point)) c r)
    ;; Look back for an unmatched "(", some keywords that can't be inside
    ;; parenthesis, or the beginning of the buffer.
    (while (and (setq r (pascal-search-ignore-backward
		 "[)}'(]\\|\\<\\(end\\|const\\|type\\)\\>" 1 1))
		(memq (setq c (following-char)) '(?\' ?\) ?\})))
      (pascal-match-part-right-paren c))
    (if (and (setq r (and r (eq c ?\())) go) t
      (goto-char p)
      r)))

(defun pascal-ignore-line ()
  (save-excursion
    (beginning-of-line)
    (if (looking-at pascal-dont-indent) (point))))

(defun pascal-beginning-string (&optional nocp)
  "Place point just before string.
If optional NOCP is true, don't complain if no match is found."
  (let (r)
    (while (and (setq r (search-backward "'" (point-min) t))
		(eq (preceding-char) ?'))
      (forward-char -1))
    (or r (if nocp nil (pascal-missing "'")))))

(defun pascal-end-string (&optional nocp)
  "Place point just after string.
If optional NOCP is true, don't complain if no match is found."
  (let (r)
    (while (and (progn (forward-char 1)
		       (setq r (search-forward "'" (point-max) t)))
		(eq (following-char) ?')))
    (or r (if nocp nil (pascal-missing "'")))))

(defun pascal-skip-comment-backward (&optional nocp)
  "Skip comment backward.
If optional NOCP is true, don't complain if no match is found."
  (let (r)
    (while (and (setq r (pascal-search-ignore-backward
			 pascal-comment-start-regexp (point-min) t))
		(eq 0 (pascal-point-in-comment-or-string))))
    (or r (if nocp nil (pascal-missing "{ or (*")))))

(defun pascal-skip-comment-forward (&optional nocp)
  "Skip comment forward.
If optional NOCP is true, don't complain if no match is found."
  (let (r)
    (forward-char (if (looking-at "{") 1 2))
    (setq r (pascal-search-ignore-forward
	     pascal-comment-end-regexp (point-max) t))
    (or r (if nocp nil (pascal-missing "} or *)")))))

(defun pascal-beginning-comment-or-string (&optional stayinside)
  "If point is inside a comment or a string, move backward to program text."
  (let ((r t) p)
    (while (and (not (bobp)) (setq r (pascal-point-in-comment-or-string)))
      (re-search-backward (if (eq r 0) pascal-comment-start-regexp "'")
			  (point-min) 1)
      (setq p (match-end 0)))
    (if (and stayinside (not r) (not (= stayinside p))) (goto-char p))
    r))

(defun pascal-end-comment-or-string (&optional stayinside)
  "If point is inside a comment or a string, move forward to program text."
  (let (r p)
    (while (and (setq r (pascal-point-in-comment-or-string)) (not (eobp)))
      (re-search-forward (if (eq r 0) pascal-comment-end-regexp "'")
			 (point-max) 1)
      (setq p (match-beginning 0)))
    (if (and stayinside (not r) (not (= stayinside p))) (goto-char p))
    r))

;; Match keyword and get property associated with it.
(defun pascal-match-get (regex prop)
  (and (looking-at regex)
       (let ((pascal-token-end (match-end 0)))
	 (pascal-get prop))))

;; Get property associated with token just seen.
(defun pascal-get (prop)
  (cdr-safe
   (assoc (id-case (buffer-substring-no-properties (point) pascal-token-end))
	  prop)))

(or (fboundp 'buffer-substring-no-properties)
    (defun buffer-substring-no-properties (b e)
      (let ((str (buffer-substring b e)))
	(or (car-safe str) str))))

(defconst pascal-assign 257)
(defconst pascal-ne 258)
(defconst pascal-le 259)
(defconst pascal-ge 260)
(defconst pascal-dotdot 261)

(defvar pascal-right-diphthong
  (list
   (list ?\: (cons ?\= pascal-assign))
   (list ?\< (cons ?\> pascal-ne) (cons ?\= pascal-le))
   (list ?\> (cons ?\= pascal-ge))
   (list ?\. (cons ?\. pascal-dotdot))
   '(?\( (?\. ?\[) (?\* . ?\{))
   '(?\. (?\) . ?\]))
   '(?\* (?\) . ?}))
   ))

(defvar pascal-left-diphthong
  (list
   (list ?\= (cons ?\: pascal-assign) (cons ?\< pascal-le)
	 (cons ?\> pascal-ge))
   (list ?\> (cons ?\< pascal-ne))
   (list ?\. (cons ?\. pascal-dotdot))
   '(?\) (?\. . ?\]) (?\* . ?\}))
   '(?\. (?\( . ?\[))
   '(?\* (?\( . ?\{))
   ))

;; Move to beginning of next token and put pascal-token-end at end. Returns 0
;; for string, 1 for float, 2 for integer, 3 for name and char for delimiter.
(defun pascal-see-token-forward ()
  (pascal-white-forward)
  (let ((p (point)) r cc)
    (setq r (cond
	     ((looking-at "'") (pascal-end-string) 0)
	     ((looking-at pascal-number-regexp)
	      (goto-char (match-end 0))
	      (if (= (match-end 0) (match-end 1)) 2 1))
	     ((looking-at "\\w+") (goto-char (match-end 0)) 3)
	     ((progn (forward-char 1)
		     (and (setq cc (assq (setq r (preceding-char))
					 pascal-right-diphthong))
			  (setq cc (assq (following-char) (cdr cc)))))
	      (forward-char 1)
	      (cdr cc))
	     (t r)))
    (setq pascal-token-end (point))
    (goto-char p)
    r))
	      
;; A simplified version of pascal-token-backward.
(defun pascal-token-back ()
  "Move backward at least one char but skip over comments.
Quite similar in action to (backward-word 1)."
  (pascal-white-backward)
  (setq pascal-token-end (point))
  (if (bobp) nil
    (forward-char -1)
    (if (looking-at "\\w+")
	(progn (setq pascal-token-end (match-end 0))
	       (forward-char 1)
	       (forward-word -1)))
    t))

(defun pascal-token-backward ()
  "Skipping comments, move backward over one delimiter, name, number or string.
Returns 0 for string, 1 for float, 2 for integer, 3 for name and char for
delimiter."
  (let (p c pp)
    (pascal-white-backward)
    (setq pascal-token-end (point))
    (forward-char -1)
    (cond
     ((eq (setq c (following-char)) ?\')
      (pascal-beginning-string)
      0)				; string
     ((looking-at "\\w+")	 ; name, integer or floating point number.
      (setq pp (match-end 0))
      (forward-char 1)
      (forward-word -1)
      (setq p (point))			; token starts not later than here
      (cond
       ((not (or (and (not (bobp))	; try for larger token
		      (progn
			(forward-char -1)
			(or (looking-at "\\.")
			    (and (looking-at "[-+]") (forward-word -1)
				 (not (bobp))
				 (progn (forward-char -1)
					(looking-at "\\.")))))
		      (forward-word -1)
		      (looking-at pascal-number-regexp)
		      (>= (match-end 0) pascal-token-end))
		 (and (progn (goto-char p) ; not larger token
			     (looking-at pascal-number-regexp))
		      (>= (match-end 0) pascal-token-end))))
	(setq pascal-token-end pp)	; is a name
	3)
       ((progn (setq pascal-token-end (match-end 0)) ; a number
	       (/= (match-end 1) (match-end 0)))
	1)				; float
       (t 2)))				; integer
     ((and (setq pp (assq c pascal-left-diphthong)) ; character operator
	   (setq pp (assq (preceding-char) (cdr pp)))) ; a diphthong
      (forward-char -1) (cdr pp))
     (t c))))

(defun pascal-blink-paren ()
  (if (and blink-matching-paren blink-paren-function)
      (pascal-blink-match (function pascal-beginning-part-paren-pos)
			  blink-matching-paren-distance
			  blink-matching-paren-on-screen)))

(defun pascal-blink-match (blink-pos-fn blink-matching-paren-distance screen)
  (let ((oldpos (point)) (blinkpos nil))
    (save-excursion
      (save-restriction
	(if blink-matching-paren-distance
	    (narrow-to-region
	     (max (point-min) (- (point) blink-matching-paren-distance))
	     oldpos))
	(condition-case ()
	    (progn (forward-char -1)
		   (funcall blink-pos-fn)
		   (setq blinkpos (point)))
	  (error nil)))
      (cond
       (blinkpos
	(goto-char blinkpos)
	(if (pos-visible-in-window-p)
	    (if screen (sit-for 1))
	  (message
	   "Matches %s"
	   (if (not (pascal-in-indent))
	       (buffer-substring (progn (beginning-of-line) (point))
				 (1+ blinkpos))
	     (forward-char 1)
	     (skip-chars-forward pascal-white-ln)
	     (end-of-line)
	     (buffer-substring blinkpos (point)))))
	blinkpos)
       (blink-matching-paren-distance -1)))))

;; Point can be in a comment (state 0), a string (state 1), or program text
;; (state 2). Strings can't span line breaks, and comments may not if
;; comment-multi-line-ok is not t, so the beginning of a line can have
;; state set {2} or {0,2}. The function returns the state it thinks point is
;; in, nil is returned for state 2; it should be accurate if the pascal code
;; is correct. Notice that this function might have to seek back to the
;; beginning of the file if it doesn't find a comment end earlier.
;; The function handles mixed states like {0,2} internally by having states
;; with number higher than 2 according to the following table:
;;
;;   Mix  State  Transformed to by ' and by } and by {
;;  {0,0}  0                       0        2        0
;;  {1,1}  1                       2        1        1
;;  {2,2}  2                       1        t        0       t means illegal
;;  {0,1}  3                       5        8        3
;;  {1,0}  4                       6        7        4
;;  {0,2}  5                       3        2        0
;;  {2,0}  6                       4        2        0
;;  {1,2}  7                       8        1        4
;;  {2,1}  8                       7        1        3

(defun pascal-point-in-comment-or-string ()
  (let ((old-point (point))
	(prev (if pascal-state-pos (marker-position pascal-state-pos)))
	char-found state-set)
    (setq state-set (if (and prev (not (> prev old-point)))
			(progn (goto-char prev) (or pascal-last-state 2))
		      (beginning-of-line)
		      (if comment-multi-line-ok 5 2)))
    ;look for string and comment delimiters
    (while (pascal-search-ignore-forward "['{}]\\|(\\*\\|\\*)" old-point t)
      (setq state-set (elt (cond
         ((eq (setq char-found (preceding-char)) ?\' )
	  '(0 2 1 5 6 3 4 8 7))  ;toggle between program and string states.
	 ((memq char-found '(?\) ?\}))
	  '(2 1 t 8 7 2 2 1 1))  ;change comment states to program states
	 (t	;found comment start
	  '(0 1 0 3 4 0 0 4 3))) ;change program states to comment states
	state-set))
      (if (eq state-set t) (error "Comment end not in comment or string")))
    ;; If state-set contains more than one state, look back for comment
    ;; delimiters to decide whether point is in a block comment.
    ;; This uses the fact that comments in Pascal may not be nested, so
    ;; after a comment end, point can't be in a comment, it must be in
    ;; a string or in program text.
    (if (> state-set 2)
	(setq state-set (elt (cond
           ((and
	     (progn
	       (beginning-of-line)
	       ;; search backward for a comment delimiter
	       (pascal-search-ignore-backward "[{}]\\|(\\*\\|\\*)"
					      (point-min) t))
	     ;; move to end of line to be sure of not being in a string.
	     (progn (end-of-line)
		    ;; The following recursive call might have to make new
		    ;; recursive calls and eventually go all the way back to
		    ;; the beginning of the buffer.
		    (pascal-point-in-comment-or-string)))
	      '(t t t 0 1 0 2 1 2))
	   (t '(t t t 1 0 2 0 2 1)))
	  state-set)))
    (goto-char old-point)
    (cond
     ((not (eq state-set 2)) state-set)
     ((and (eq (preceding-char) ?\') (eq (following-char) ?\')) 1)
     (t nil))))

;; Support for templates
;; Most of the functions here can be removed when eventually the dmacro
;; package is used.

(defun pascal-add-templates (table defs)
  (let (nm expd def)
    (while defs
      (setq def (car defs))
      (setq nm (car def))
      (setq expd (car (cdr def)))
      (if (and (setq def (cdr (cdr def))) (setq def (cdr def)))
	  (setq def (car (car def))))
      (define-abbrev table nm expd def)
      (setq defs (cdr defs)))))

(defun pascal-template-expand (&optional sw)
  "Expand a pascal template.
With ARG define a template with region as template string."
  (interactive "*P")
  (let (beg end)
    (cond
     (sw (pascal-define-template (mark) (point)))
     ((= (preceding-char) ?\?)
      (delete-char -1) (pascal-template-help1 pascal-template-table))
     ((let ((global-abbrev-table pascal-template-table)
	    (local-abbrev-table nil))
	(expand-abbrev))
      (setq end (point-marker))
      (goto-char last-abbrev-location)
      (beginning-of-line)
      (setq beg (point))
      (indent-region beg end nil)
      (goto-char beg)
      (if (search-forward "~@" end 1) (delete-char -2))
      (set-marker end nil))
     ((not (expand-abbrev)) (error "Unknown abbrev")))))

(defun pascal-define-template (from to)
  "Define new pascal-abbreviation."
  (interactive "*r")
  (if (not from) (error "Mark not set")
    (let ((nm (read-string "Abbreviation: "))
	  (expd (buffer-substring-no-properties from to)))
      (if (equal expd "") (setq expd nil))
      (if (if (abbrev-symbol nm)
	      (yes-or-no-p
	       (format "Symbol %s is defined. %s it? "
		       nm (if expd "Redefine" "Delete")))
	    (or expd (progn (message "No expansion") nil)))
	  (progn (define-abbrev pascal-template-table nm expd)
		 (delete-region from to))))))

(defun pascal-template-help ()
  "List the currently defined templates in Pascal mode."
  (interactive)
  (pascal-template-help1 pascal-template-table))

(defun pascal-template-help1 (table)
  "List the currently defined templates in TABLE."
  (message "Listing template table...")
  (with-output-to-temp-buffer "*Help*"
    (save-excursion
      (let ((abbrev-list nil)
	    (wd (frame-width))
;	     (if (fboundp 'screen-width)
;		    (screen-width)
;		  (frame-width)))
	    nm)
	(set-buffer standard-output)
	(mapatoms
	  (function (lambda (abbrev)
		      (setq abbrev-list (cons abbrev abbrev-list))))
	  table)
	(setq abbrev-list (sort abbrev-list 'string-lessp))
	(insert "Abbrev") (indent-to 8) (insert "Template")
	(indent-to 30) (insert "Comment\n\n")
	(while abbrev-list
	  (insert (symbol-name (setq nm (car abbrev-list))))
	  (indent-to 5) (insert "   ")
	  (if (not (string-equal (symbol-value nm) ""))
	      (insert (let ((print-escape-newlines t))
			(prin1-to-string (symbol-value nm)))))
	  (if (and (fboundp nm) (setq nm (symbol-function nm))
		   (setq nm (documentation nm)))
	      (progn
		(indent-to 27)
		(insert "   ")
		(insert nm)
		(while (not (< (current-column) wd)) (forward-word -1))
		(if (not (eobp)) (progn (insert "\n") (indent-to 31)))
		(goto-char (point-max))))
	  (insert "\n")
	  (setq abbrev-list (cdr abbrev-list))))))
  (message "Listing template table...done"))

(defun pascal-find-template-mark (&optional count)
  "Move to and delete the next template mark.
With ARG move to and delete the ARGth following (or preceding) template mark."
  (interactive "*p")
  (if (or (null count) (zerop count)) (setq count 1))
  (if (if (> count 0)
	  (re-search-forward pascal-template-mark (point-max) t count)
	(re-search-backward pascal-template-mark (point-min) t (- count)))
      (delete-region (match-beginning 0) (match-end 0))
    (error "Can't find this template mark")))

;; If the previous statement ends with a `;', remove that `;'. Intended for
;; use when inserting 'else' clauses.
(defun pascal-abbrev-no-prev-semi ()
  "If previous statement ends with `;', remove `;' before inserting `else'."
  (save-excursion
    (goto-char last-abbrev-location)
    (pascal-token-back)
    (if (looking-at ";")
	(progn (delete-char 1)
	       (setq last-abbrev-location (1- last-abbrev-location))))))

;Used by the nm template to insert name of current function or procedure.
(defun pascal-proc-name ()
  "Name of current function or procedure."
  (goto-char last-abbrev-location)
  (insert
   (save-excursion
     (pascal-beginning-procedure)
     (pascal-search-forward pascal-proc-head)
     (goto-char (match-end 0))
     (pascal-white-forward)
     (looking-at "\\w+")
     (buffer-substring-no-properties (match-beginning 0) (match-end 0)))))

(defun pascal-check-template-case ()
  "id-case-change-hook for pascal mode."
  (if (eq major-mode 'pascal-mode)
      (let* ((tab (if id-case-significant 'pascal-case-template-table
		    'pascal-nocase-template-table))
	     (val (symbol-value tab)))

	(if (not (or val
		     (and (load (get tab 'file))
			  (setq val (symbol-value tab)))))
	    (progn
	      (setq val (make-abbrev-table))
	      (set tab val)
	      (message
	"Couldn't find template definitions. Proceeding without templates")
	      ))
	(setq pascal-template-table val))))

(provide 'pascal-mode)
(provide 'pasc-mode)

;;; pasc-mode.el ends here
