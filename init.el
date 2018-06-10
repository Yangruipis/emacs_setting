;;; init.el --- Spacemacs Initialization File
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Without this comment emacs25 adds (package-initialize) here
;; (package-initialize)

;; Increase gc-cons-threshold, depending on your system you may set it back to a
;; lower value in your dotfile (function `dotspacemacs/user-config')
(setq gc-cons-threshold 100000000)
(package-initialize)

(setq dotspacemacs-install-packages 'used-but-keep-unused)

;; (set-language-environment "UTF-8")
;; (set-default-coding-systems 'utf-8)
;; (set-buffer-file-coding-system 'utf-8-dos)
;; (set-clipboard-coding-system 'euc-cn)
;; (set-file-name-coding-system 'utf-8-dos)
;; (set-keyboard-coding-system 'utf-8-dos)
;; (set-next-selection-coding-system 'utf-8-dos)
;; (set-selection-coding-system 'utf-8-dos)
;; (set-terminal-coding-system 'utf-8-dos)
;; (setq locale-coding-system 'utf-8)
;; (prefer-coding-system 'utf-8)

;; 以上编码有问题，导致复制粘贴进来是乱码，下面这个更好
(set-language-environment 'Chinese-GB)
(set-default buffer-file-coding-system 'utf-8-dos)
(set-default-coding-systems 'utf-8-dos)
(setq file-name-coding-system 'euc-cn)
(prefer-coding-system 'cp950)
(prefer-coding-system 'gb2312)
(prefer-coding-system 'utf-8-dos)

;; 解决 linux 中文目录乱码问题
(setq file-name-coding-system 'utf-8)
(setq path-name-coding-system 'utf-8)


(setq package-archives '(("gnu" . "http://elpa.emacs-china.org/gnu/")
			 ("melpa" . "http://elpa.emacs-china.org/melpa/")
			 ("melpa-stable" . "http://elpa.emacs-china.org/melpa-stable/")
			 ;;("elpy" . "http://jorgenschaefer.github.io/packages/")
       ))

;;nyan mode
;;(nyan-mode t);;启动nyan-mode  
;;(nyan-start-animation);;开始舞动吧（会耗cpu资源）  


(desktop-save-mode 1)
(setq dotspacemacs-delete-orphan-packages nil)
(setq dotspacemacs-additional-packages 'nyan-mode)
(setq gc-cons-threshold 100000000)

(defconst spacemacs-version         "0.200.10" "Spacemacs version.")
(defconst spacemacs-emacs-min-version   "24.4" "Minimal version of Emacs.")

(if (not (version<= spacemacs-emacs-min-version emacs-version))
    (error (concat "Your version of Emacs (%s) is too old. "
                   "Spacemacs requires Emacs version %s or above.")
           emacs-version spacemacs-emacs-min-version)
  (load-file (concat (file-name-directory load-file-name)
                     "core/core-load-paths.el"))
  (require 'core-spacemacs)
  (spacemacs/init)
  (configuration-layer/sync)
  (spacemacs-buffer/display-startup-note)
  (spacemacs/setup-startup-hook)
  (require 'server)
  (unless (server-running-p) (server-start)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ORG  MODE                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 函数M-x org-insert-src-block 插入代码块（或者用<s <tab> 输入代码名称
(defun org-insert-src-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
  (interactive
   (let ((src-code-types
          '("emacs-lisp" "python :results output" "C" "sh" "java" "js" "clojure" "C++" "css"
            "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
            "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
            "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby"
            "scheme" "sqlite")))
     (list (ido-completing-read "Source code type: " src-code-types))))
  (progn
    (newline-and-indent)
    (insert (format "#+BEGIN_SRC %s\n" src-code-type))
    (newline-and-indent)
    (insert "#+END_SRC\n")
    (previous-line 2)
    (org-edit-src-code)))

(setq org-src-fontify-natively t)

;; orgmode 自动加载完成状态
(setq org-todo-keywords
    '((sequence "BUG(b!)" "|" "FIXED(f!)")
      (sequence "TODO(t!)" "SOMEDAY(l)" "|" "DONE(d!)" "CANCELED(c!)")
     ))

;; 中文字体的设置，同时解决中英文字体宽度不一致的问题（org-mode的表格可以中英文对齐）。
;; 而且解决了中文字体导致emacs卡的现象。
(dolist (charset '(kana han cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font) charset
                    (font-spec :family "微软雅黑" :size 16)))
;; org-mode 缩进
(setq org-startup-indented t)


;; 到处中文latex-pdf
(setenv "PATH" (concat (getenv "PATH") ":D:/CTEX/MiKTeX/miktex/bin/"))
  (setq exec-path (append exec-path '("D:/CTEX/MiKTeX/miktex/bin/")))
(setq org-latex-pdf-process '("xelatex -interaction nonstopmode %f" 
                                                    "xelatex -interaction nonstopmode %f"))

(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (shell . t)
   (python . t)
   ;; (ipython . t)
   (R . t)
   (ruby . t)
   (ditaa . t)
   (dot . t)
   (octave . t)
   (sqlite . t)
   (perl . t)
   (C . t)
   ))

;; 中文自动换行
(add-hook 'org-mode-hook   
      (lambda () (setq truncate-lines nil)))  

(defun do-org-show-all-inline-images ()
  (interactive)
  (org-display-inline-images t t))
(global-set-key (kbd "C-c C-x C v")
                'do-org-show-all-inline-images)
(setq org-startup-with-inline-images t)


;; all the icons
;;(add-to-list 'load-path "./all-the-icons")
;;(require 'all-the-icons)

;;(load-file "/home/ray/.emacs.d/all-the-icons/all-the-icons.el")
;;(require 'all-the-icons)

;; neo tree
(add-to-list 'load-path "./neotree")
(require 'neotree)
(setq neo-smart-open t)
(setq projectile-switch-project-action 'neotree-projectile-action)
(global-set-key (kbd "C-c n") 'neotree-projectile-action)
(global-set-key (kbd "C-c M-n") 'neotree-toggle)
;;(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

(load-file "/home/ray/.emacs.d/nyan-mode/nyan-mode.el")
(nyan-mode)
(nyan-start-animation)

(load-file "/home/ray/.emacs.d/nyan-mode/pascal-csperr.el")
;;(require 'pascal-csperr)
(add-hook 'pascal-mode-hook 'opascal-mode)

(load-file "/home/ray/.emacs.d/nyan-mode/figlet.el")
;;()

;; 改建，用M-f1作为标记
(global-set-key [M-f3] (quote set-mark-command))

;; 下方状态栏显示时间
(setq display-time-24hr-format t)
(display-time)

;; 用C-<tab> 作为窗口切换按键
(global-set-key [C-tab] (quote other-window))

;; copy region or whole line(当M-w为设置mark时复制当前行)
(global-set-key "\M-w"
(lambda ()
  (interactive)
  (if mark-active
      (kill-ring-save (region-beginning)
      (region-end))
    (progn
     (kill-ring-save (line-beginning-position)
     (line-end-position))
     (message "copied line")))))

;; kill region or whole line（当C-w未设置mark时剪切当前行)
(global-set-key "\C-w"
(lambda ()
  (interactive)
  (if mark-active
      (kill-region (region-beginning)
   (region-end))
    (progn
     (kill-region (line-beginning-position)
  (line-end-position))
     (message "killed line")))))

(load-file "/home/ray/.emacs.d/ace-jump-mode/ace-jump-mode.el")
;; ace jump mode
(global-set-key (kbd "M-p") 'ace-jump-mode)


;; multiple cursors
(global-set-key (kbd "C-c m c") 'mc/edit-lines)


;; 即使所有的子任务都完成，也只是标记上一级任务的完成情况为100%，而不能自动更新上级任务的完成状态。如果需要自动设定为完成，可以在.emacs中增加如下配置：
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;; 选中当前行
(defun my/select-current-line-and-forward-line (arg)
  "Select the current line and move the cursor by ARG lines IF
no region is selected.

If a region is already selected when calling this command, only move
the cursor by ARG lines."
  (interactive "p")
  (when (not (use-region-p))
    (forward-line 0)
    (set-mark-command nil))
  (forward-line arg))
;; Note that I would not recommend binding this command to `C-l'.
;; From my personal experience, the default binding to `C-l' to
;; `recenter-top-bottom' is very useful.
(global-set-key (kbd "C-c l") #'my/select-current-line-and-forward-line)

;; latex 默认 xelatex编译
(add-hook 'LaTeX-mode-hook
              (lambda ()
                  (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex --synctex=1%(mode)%' %t" TeX-run-TeX nil t))))

;; M-x helm-bibtex 引用文献
(setq helm-bibtex-bibliography '("~/Papers/ref.bib"))


(auto-fill-mode -1)


;; auto-complete : Sort results by usage
(setq-default dotspacemacs-configuration-layers
              '((auto-completion :variables
                                 auto-completion-enable-sort-by-usage t)))

;; auto-complete 防止C-f自动补全
(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-f") nil))

(add-hook 'org-mode-hook (lambda ()
                           (spacemacs/toggle-auto-completion-off)
                           ))
(add-hook 'latex-mode-hook (lambda ()
                             (spacemacs/toggle-auto-completion-off)
                             ))



(defvar base-youdao-url "http://fanyi.youdao.com/openapi.do?keyfrom=emacs-yd-pub&key=527593631&type=data&doctype=json&version=1.1&q=")
;; Get yourself an API KEY here: http://fanyi.youdao.com/openapi?path=data-mode
(defun youdao-fanyi ()
  "Translate current word (en->cn, cn->en), prompt to input if no word here"
  (interactive)
  (let* ((word (or (thing-at-point 'word) (read-string "Translate> ")))
         (full-url (concat base-youdao-url word)))
    (with-current-buffer (url-retrieve-synchronously full-url)
      (unwind-protect
          (progn
            (goto-char (point-min))
            (re-search-forward "^$")
            (delete-region (point) (point-min)) ;strip headers
            (message
             (elt (cdar ;we just want the straight one
                   (json-read-from-string
                    (decode-coding-string
                     (buffer-string) 'utf-8)))
                  0)))
        (kill-buffer)))))
(global-set-key "\C-c\ y" 'youdao-fanyi)

;; orgmode显示latex公式
(setq org-latex-compiler "pdflatex")
(setq org-latex-pdf-process
      '("pdflatex -interaction nonstopmode -output-directory %o %f"))

;; orgmode 图标替换
(setq org-latex-create-formula-image-program 'imagemagick)
(setq org-bullets-bullet-list '("◉" "○" "▶" "◆"  "▲"))

;; 由于M-/冲突，替换M-f2为snippet展开
(global-set-key [M-f2] (quote yas-expand))


;; 解决org-mode导出html时latex公式难看且加载缓慢的问题
;; modify path and mathml
(setq org-html-mathjax-options
  '((path "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML")
    (scale "100")
    (align "center")
    (indent "2em")
    (mathml t)))

(setq org-html-mathjax-template
              "
<script type=\"text/javascript\" src=\"%PATH\"></script>
<script type=\"text/javascript\">
<!--/*--><![CDATA[/*><!--*/
    MathJax.Hub.Config({
        jax: [\"input/TeX\", \"output/HTML-CSS\"],
        extensions: [\"tex2jax.js\",\"TeX/AMSmath.js\",\"TeX/AMSsymbols.js\",
                     \"TeX/noUndefined.js\", \"[Contrib]/siunitx/siunitx.js\", \"[Contrib]/mhchem/mhchem.js\"],
        tex2jax: {
            inlineMath: [ [\"\\\\(\",\"\\\\)\"] ],
            displayMath: [ ['$$','$$'], [\"\\\\[\",\"\\\\]\"], [\"\\\\begin{displaymath}\",\"\\\\end{displaymath}\"] ],
            skipTags: [\"script\",\"noscript\",\"style\",\"textarea\",\"pre\",\"code\"],
            ignoreClass: \"tex2jax_ignore\",
            processEscapes: false,
            processEnvironments: true,
            preview: \"TeX\"
        },
        TeX: {extensions: [\"AMSmath.js\",\"AMSsymbols.js\",  \"[Contrib]/siunitx/siunitx.js\", \"[Contrib]/mhchem/mhchem.js\"]},
        showProcessingMessages: true,
        displayAlign: \"%ALIGN\",
        displayIndent: \"%INDENT\",

        \"HTML-CSS\": {
             scale: %SCALE,
             availableFonts: [\"STIX\",\"TeX\"],
             preferredFont: \"TeX\",
             webFont: \"TeX\",
             imageFont: \"TeX\",
             showMathMenu: true,
        },
        MMLorHTML: {
             prefer: {
                 MSIE:    \"MML\",
                 Firefox: \"MML\",
                 Opera:   \"HTML\",
                 other:   \"HTML\"
             }
        }
    });
/*]]>*///-->
</script>")

(add-hook 'org-mode-hook
          (lambda () (linum-mode -1))
          )

(add-hook 'pdf-view-mode-hook
          (lambda () (linum-mode -1))
          )

(add-hook 'python-mode (lambda () (flymake-mode 1)))

(set-foreground-color "white")

;;  ___ _  _ ___  ___ _  _ _____
;; |_ _| \| |   \| __| \| |_   _|
;;  | || .` | |) | _|| .` | | |
;; |___|_|\_|___/|___|_|\_| |_|

;; indent and unindent
(defun my-indent-region (N)
  (interactive "p")
  (if (use-region-p)
      (progn (indent-rigidly (region-beginning) (region-end) (* N 4))
             (setq deactivate-mark nil))
    (self-insert-command N)))

(defun my-unindent-region (N)
  (interactive "p")
  (if (use-region-p)
      (progn (indent-rigidly (region-beginning) (region-end) (* N -4))
             (setq deactivate-mark nil))
    (self-insert-command N)))

(global-set-key (kbd "C-c >") 'my-indent-region)
(global-set-key (kbd "C-c <") 'my-unindent-region)


;;              _
;;  ___ _ _  __| |
;; / -_) ' \/ _` |
;; \___|_||_\__,_|


;; C-backspace 只删除空格，删到上一个单词之后为止，儿 M-backspace 则是删除上一个词
(global-set-key [C-backspace] 'delete-horizontal-space)



;; 更改pomodoro音量
(setq org-pomodoro-audio-player "mplayer")
(setq org-pomodoro-finished-sound-args "-volume 0.7")
(setq org-pomodoro-long-break-sound-args "-volume 0.7")
(setq org-pomodoro-short-break-sound-args "-volume 0.7")
;; ;; (setq org-pomodoro-ticking-sound-args "-volume 0.3")


;; 纵向滑动mode
(pixel-scroll-mode 1)
(global-set-key [up] (quote pixel-scroll-up))
(global-set-key [down] (quote pixel-scroll-down))


;; 修改org-latex公式大小 为原来的1.5倍
(plist-put org-format-latex-options :scale 1.5)
