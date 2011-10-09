(require 'auto-overlays)
(require 'auto-overlay-nested)
(provide 'auto-overlay-tikz)

;; set tikz overlay parsing and suicide functions, and indicate class requires
;; separate start and end regexps
(put 'tikz 'auto-overlay-parse-function 'auto-o-parse-tikz-match)
(put 'tikz 'auto-overlay-suicide-function 'auto-o-tikz-suicide)
;(put 'tikz 'auto-overlay-match-function 'auto-o-tikz-match)
(put 'tikz 'auto-overlay-complex-class t)


(defun debug-prompt (p v &optional m) 
;  ()
  (if m
      (message "%s %s" (prin1-to-string p) (prin1-to-string v)))
)

(defstruct 
  (matcher (:type vector) 
	   (:constructor nil)
	   :named) 
  (name nil :read-only t)
  (match `(lambda () nil) :read-only t))
(make-matcher :name "foo")


(defun matcher-char (char move-point &optional face)
  "Matches a char at the current point and optionally updates the current point.  
If matched, returns (face . (start . end)), otherwise returns nil."
  `(lambda ()
    (if (char-equal (following-char) ,char)
	,(if move-point
	    `(progn
	      (forward-char 1)
	      (list (cons ,face (cons (1- (point)) (point)))))
	  `(list (cons ,face (cons (point) (1+ (point))))))
      nil)))


(defun matcher-regex (regex move-point &optional face)
  "Matches a regex at the current point and updates the current point.  
If matched, returns (face . (start . end)), otherwise returns nil."
  `(lambda ()
     (if (looking-at ,regex)
	 (let ((start (point))
	       (end (match-end 0)))
	   (if ,move-point (goto-char end))
	   (list (cons ,face (cons start end))))
       nil)))

(defun matcher-sequence (whichseq &rest args)
  (lexical-let ((whichseq whichseq)
		(args args))
    (lambda ()
      (debug-prompt "Handling sequence " whichseq)
      (let ((matches nil))
	(catch 'fail
	  (save-excursion
	    (dolist (matcher args)
	      (let ((match (funcall matcher)))
		(cond 
		 ((equalp match (list 't)))
		 ((equalp match nil)
;		(debug-prompt "Failed at " (point))
		  (throw 'fail nil))
		 (t
		  (setq matches (append match matches))
		  (goto-char (cddar match)))
		 )))
	    )
	  (if matches
	      (goto-char (cddar matches)))
	  matches)
	))))

(defun matcher-noop ()
  "A matcher that always succeeds and does nothing to the current point.  
Returns '(t) rather than nil, to indicate a no-op success"
  (lambda ()
    (list 't)))

(defun matcher-alternates (&rest args)
  "Tries each of the alternates in args in succession, returning the first
one that succeeds, or else returns nil"
  (lexical-let ((args args))
    (lambda ()
      (debug-prompt "In matcher-alternates, args are " args)
      (catch 'match
	(dolist (matcher args)
	  (let* ((alt-name (nth 0 matcher))
		 (match-fn (nth 1 matcher))
		 (match-pred (or (nth 2 matcher) (matcher-noop)))
		 (match (and (save-excursion (funcall match-pred))
			     (save-excursion (funcall match-fn)))))
	    (if match
		(progn
		  (debug-prompt "alternate succeeded " alt-name)
		  (debug-prompt "alternate succeeded: match is " match)
		  (if (cdr match)
		      (goto-char (cddar match)))
		  (throw 'match match))
	      (debug-prompt "alternate failed " alt-name))))
	nil))))

(defun matcher-optional (name matcher)
  "Matches matcher if possible, but always succeeds"
;  (debug-prompt "Optional " name)
  (matcher-alternates (list name matcher) (list "noop" (matcher-noop))))

(defun matcher-repeat (matcher)
  "Matches zero or more instances of matcher.  Always succeeds."
  (lexical-let ((matcher matcher))
    (lambda ()
      (let ((matches nil))
	(catch 'done
	  (while t
	    (let ((match (save-excursion (funcall matcher))))
	      (if match
		  (progn
;		    (debug-prompt "XXX Match is " match)
;		    (debug-prompt "XXX cddar Match is " (cddar match))
		    (goto-char (cddar match))
		    (setq matches (append match matches)))
		(progn
		  (debug-prompt "no more repeats" (or matches (list 't)))
		  (throw 'done (or matches (list 't))))))))))))

(defun matcher-list (whichlist start elem sep end &optional trailing-sep leading-sep)
  "Matches <start> (<sep>? <elem>)? (<sep> <elem>)* <sep>? <end>, 
where the leading and trailing optional <sep>s depend on the optional arguments
to matcher-list."
  (lexical-let ((whichlist whichlist)
		(start start)
		(elem elem)
		(sep sep)
		(end end)
		(trailing-sep trailing-sep)
		(leading-sep leading-sep))
    (matcher-alternates
     (list 
      "mainlist"
      (matcher-sequence 
       whichlist
       start
       (if leading-sep 
	   (matcher-optional "leading sep" sep)
	 (matcher-noop))
       elem
       (matcher-repeat (matcher-sequence "sep-elem" sep elem))
       (if trailing-sep
	   (matcher-optional "trailing sep" sep)
	 (matcher-noop))
       end))
     (list "emptylist" (matcher-sequence "empty-list" start end)))
    ))

(defun matcher-key ()
  "Matches names of the form /a/b/c..., or a/b/.c"
  (matcher-repeat
   (matcher-alternates
    (list "keyword" (matcher-regex "/?\\.[^])=,}]+" t 'font-lock-keyword-face))
    (list "plainname" (matcher-regex "/?[^])=,}/]+" t 'font-lock-variable-name-face)))))

(defun matcher-delim-group (face open close &optional eat-trailing-spaces)
  "Matches a balanced brace-delimited group, assuming the point is currently at an open-brace"
  (lexical-let ((face face)
		(open open)
		(close close)
		(eat-trailing-spaces eat-trailing-spaces))
    (lambda ()
      (let ((depth 0)
	    (start (point)))
	(catch 'done
	  (while t
	    (cond 
	     ((char-equal (following-char) open)
	      (setq depth (1+ depth))
	      (forward-char 1))
	     ((char-equal (following-char) close)
	      (if (equalp depth 1)
		  (progn
		    (forward-char 1)
		    (if eat-trailing-spaces
			(while (let ((c (following-char)))
				 (or (char-equal c ? )
				     (char-equal c ?\n)
				     (char-equal c ?\t)))
			  (forward-char 1)))
		    (throw 'done (list `(,face . (,start . ,(point))))))
		(progn
		  (setq depth (1- depth))
		  (forward-char 1))))
	     (t 
	      (if (equalp depth 0)
		  (throw 'done "Characters outside bracegroup"))
	      (forward-char 1)))))))))
    
(defun matcher-bracegroup (face &optional eat-trailing-space)
  (matcher-delim-group face ?{ ?} eat-trailing-space))
(defun matcher-parengroup (face &optional eat-trailing-space)
  (matcher-delim-group face ?\( ?\) eat-trailing-space))


(defun matcher-val (closer face)
;  (debug-prompt "Trying plain val" "")
  (lexical-let ((closer closer)
		(face face))
    (lambda ()
      (let ((start (point)))
	(catch 'done
	  (while t
	    (cond
	     ((char-equal (following-char) ?{)
	      (let ((brace (funcall (matcher-bracegroup face))))
		(if (equalp brace (list 't))
		    (throw 'done nil))
		(goto-char (cddar brace))))
	     ((or (char-equal (following-char) ?,)
		  (save-excursion (funcall closer)))
	      (throw 'done (list `(,face . (,start . ,(point))))))
	     (t (forward-char 1)))))))))

(defun matcher-keyval (closer)
  "Matches either key=val or just val, delimited by closer"
  (matcher-sequence "keyval"
   (matcher-key)
   (match-equal)
   (matcher-alternates
    (list "tikzset" '(lambda () (funcall (matcher-tikzset))))
    (list "plainval" (matcher-val closer 'font-lock-string-face))
    )
   ))

(defun match-open-brace ()
  (matcher-char ?{ t 'font-lock-preprocessor-face))
(defun match-close-brace ()
  (matcher-regex "[[:space:]\n]*}[[:space:]\n]*" t 'font-lock-preprocessor-face))
(defun match-open-bracket ()
  (matcher-char ?\[ t 'font-lock-preprocessor-face))
(defun match-close-bracket ()
  (matcher-regex "[[:space:]\n]*\\][[:space:]\n]*" t 'font-lock-preprocessor-face))
(defun match-open-paren ()
  (matcher-char ?\( t 'font-lock-preprocessor-face))
(defun match-close-paren ()
  (matcher-regex "[[:space:]\n]*[)][[:space:]\n]*" t 'font-lock-preprocessor-face))
(defun match-comma ()
  (matcher-char ?, t 'font-lock-builtin-face))
(defun match-equal ()
  (matcher-char ?= t 'font-lock-builtin-face))

(defun matcher-tikz-options (open close)
  "Matches a comma-separated list of tikz key/value pairs, delimited by open and close"
  (matcher-list 
   "tikz-options"
   open
   (matcher-alternates
    (list "keyval" (matcher-keyval close))
    (list "plainname" (matcher-regex "[^],)}]+" t 'font-lock-variable-name-face)))
   (match-comma)
   close
   t t
   ))

(defun matcher-tikzset ()
  (interactive)
  (matcher-tikz-options (match-open-brace) (match-close-brace)))

(defun matcher-tikz-brackets ()
  (interactive)
  (matcher-tikz-options (match-open-bracket) (match-close-bracket)))

(defun matcher-process-font-locks (list)
  (if list
      (progn
	(dolist (item (reverse list))
	  (if (car item)
	      (let ((o-new (make-overlay (cadr item) (cddr item) nil t nil)))
		(overlay-put o-new 'auto-overlay t)
		(overlay-put o-new 'face (car item)))))
	(goto-char (cddar list))
	)
  ))

(defun matcher-tikz-command ()
  (interactive)
  (matcher-alternates
   (list 
    "tikzset" 
    (matcher-sequence
     "tikzset"
     (matcher-regex "\\\\tikzset" t 'font-lock-keyword-face)
     (matcher-tikzset)))
   (list "tikz command" 
    (matcher-sequence "tikz command" 
     (matcher-regex "\\\\[A-Za-z]+[[:space:]]*" t 'font-lock-keyword-face)
     (matcher-repeat
      (matcher-alternates
       (list "options" (matcher-tikz-brackets) (match-open-bracket))
       (list "nodename" (matcher-parengroup 'font-lock-string-face t) (match-open-paren))
       (list "bracegroup" (matcher-bracegroup nil t) (match-open-brace))))
     (matcher-char ?\; t 'font-lock-preprocessor-face)))
   (list 
    "error case"
    (matcher-sequence 
     "error"
     (matcher-regex "[^;]*" t 'font-lock-warning-face)
     (matcher-char ?\; t 'font-lock-preprocessor-face)))))


(defun simple-setup ()
  (interactive)
  (matcher-process-font-locks (funcall (matcher-repeat (matcher-tikz-command)))))

(defun simple-stop ()
  (interactive)
  (remove-overlays))
	 

;(font-lock-add-keywords nil '("\\tikzset{" (matcher-tikzset (backward-char 1) nil)) t)
(defun setup ()
  (interactive)
  (auto-overlay-load-definition
   'latex
   '(tikz ("\\\\tikzset{"
   	   :edge start
   	   (priority . 1)
   	   ;(face . (background-color . "seagreen4"))
   	   )
   	  ("\\([^\\]\\|^\\){"
   	   :edge start
   	   (priority . 2)
   	   (synthetic . t))
   	  (("\\([^\\]\\|^\\)\\(}\\)" . 2)
   	   :edge end
   	   (priority . 2)
   	   (synthetic . t))
   	  )
   )
   ;; '(tikz ("\\\\begin{tikzpicture}"
   ;; 	   :edge start
   ;; 	   (priority . 1)
   ;; 	   ;(face . (background-color . "seagreen4"))
   ;; 	   )
   ;; 	  ("\\\\end{tikzpicture}"
   ;; 	   :edge end
   ;; 	   (priority . 1))
   ;; 	  )
   ;; )
  (auto-overlay-start 'latex)
  )
(defun stop ()
  (interactive)
  (auto-overlay-stop 'latex)
  (auto-overlay-unload-set 'latex)
  ;(remove-overlays)
  )

(defun auto-o-parse-tikz-match (o-match)
  ;; Perform any necessary updates of auto overlays due to a match for a
  ;; nested regexp.

  (let* ((o-match o-match)
	 (o-new (auto-o-parse-nested-match o-match)))
;    (debug-prompt "In parse-tikz-match, o-match is " o-match 't)
;    (debug-prompt "In parse-tikz-match, o-new is " o-new 't)
    (cond
     ((eq o-new nil)
      (setq o-new (overlay-get o-match 'parent))))
    (cond 
     ((eq o-new nil)
      nil)
     
     ((overlay-get o-new 'children)
      o-new)

     ((overlay-get o-new 'synthetic)
      ;; (message "Skipping overlay %s because it's synthetic: %s" 
      ;; 	       (prin1-to-string o-new)
      ;; 	       (prin1-to-string (overlay-properties o-new)))
      o-new)

     ((not (overlay-get o-new 'start))
      ;; (message "Skipping overlay %s because it has no start: %s" 
      ;; 	       (prin1-to-string o-new)
      ;; 	       (prin1-to-string (overlay-properties o-new)))
      o-new)

     ((overlay-get (overlay-get o-new 'start) 'synthetic)
      ;; (message "Skipping overlay %s because its start is synthetic: %s" 
      ;; 	       (prin1-to-string o-new)
      ;; 	       (prin1-to-string (overlay-properties (overlay-get o-new 'start))))
      o-new)
     
     ((eq (overlay-get o-new 'end) nil)
      ;; (message "Skipping overlay %s because it has nil end: %s" 
      ;; 	       (prin1-to-string o-new)
      ;; 	       (prin1-to-string (overlay-properties o-new)))
      o-new)

     (t
      (message "Processing overlay %s:\n\t%s\n\t" 
	       (prin1-to-string o-new)
	       (prin1-to-string (overlay-properties o-new))
	       (prin1-to-string (auto-o-props o-new)))
      (auto-o-tikz-match o-new)
      o-new)
     )
    )
  )




(defun auto-o-tikz-suicide (o-self)
  ;; Called when match no longer matches. Unmatch the match overlay O-SELF, if
  ;; necessary deleting its parent overlay or cascading the stack.

  (let ((o-parent (overlay-get o-self 'parent)))
    (message "In auto-o-tikz-suicide")
    (debug-prompt "o-self is " o-self 't)
    (debug-prompt "parent is " o-parent 't)
    (debug-prompt "children to delete are " (overlay-get o-parent 'children) 't)
    (mapcar (lambda (o)
	      (debug-prompt "deleting overlay " o 't)
	      (delete-overlay o))
	    (overlay-get o-parent 'children))
    (overlay-put o-parent 'children 'nil)
    (auto-o-nested-suicide o-self)))

(defun auto-o-tikz-match (o-match)
  (move-overlay o-match
		(apply 'min (delq nil (list (point-max) 
					    (overlay-start o-match)
					    (overlay-start (overlay-get o-match 'start)))))
		(apply 'max (delq nil (list (point-min) 
					    (overlay-end o-match) 
					    (overlay-end (overlay-get o-match 'end))))))
  ;; (debug-prompt "o-match " o-match 't)
  ;; (debug-prompt "o-match o-props " (auto-o-props o-match) 't)
  ;; (debug-prompt "o-match props " (overlay-properties o-match) 't)
  ;; (debug-prompt "o-match edge " (auto-o-edge o-match) 't)
  ;; (debug-prompt "o-match synthetic " (plist-get (overlay-properties o-match) 'synthetic) 't)
  (cond 
   ((and 
     (not (plist-get (overlay-properties o-match) 'synthetic))
     (overlay-get o-match 'start)
     (not (plist-get (overlay-properties (overlay-get o-match 'start)) 'synthetic)))
    (debug-prompt "matching " o-match 't)
    (debug-prompt "delim-end " (overlay-get (overlay-get o-match 'start) 'delim-end) 't)
    (let* ((start (1- (overlay-get (overlay-get o-match 'start) 'delim-end)))
	   (end (1+ (overlay-end o-match)))
	   (matches (save-excursion 
		      (goto-char start) 
		      (overlay-put o-match 'delim-end start)
		      (message "Calling matcher-tikzset from %d" start)
		      (let ((temp (reverse (funcall (matcher-tikzset)))))
			(message "Now at position %d" (point))
			temp)))
	   (overlays (mapcar
		      (lambda (match)
			(let (o-new)
			  (save-excursion 
			    (goto-char (cadr match))
			    (if (looking-at (auto-o-regexp (overlay-get o-match 'end)))
			      (setq o-new (make-overlay (cadr match) (1+ (cadr match)) nil t nil))
			    (setq o-new (make-overlay (cadr match) (cddr match) nil t nil))))
			  (overlay-put o-new 'auto-overlay t)
			  (overlay-put o-new 'set-id (overlay-get o-match 'set-id))
			  (overlay-put o-new 'definition-id (overlay-get o-match 'definition-id))
;			  (overlay-put o-new 'regexp-id (overlay-get o-match 'regexp-id))
			  (overlay-put o-new 'face (car match))
			  (move-overlay o-match
					(min (overlay-start o-match) (overlay-start o-new))
					(max (overlay-end o-match) (overlay-end o-new)))
;			  (delete-overlay o-new)
			  (debug-prompt "o-new" o-new )
;			  nil))
			  o-new))
		      matches)))
      (overlay-put o-match 'children overlays))
    ))
)
