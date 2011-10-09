(defun all (pred list)
  "Ensures that every element of LIST matches PREDICATE and returns t, or nil otherwise."
  (cond ((null list) t)
	((not (listp list)) nil)
	((not (funcall pred (car list))) nil)
	(t (all pred (cdr list)))))

(defcustom font-latex-tikz-environments
  '("tikzpicture" "scope" "pgfonlayer")
  "List of TikZ environment names for font locking."
  :type '(repeat string)
  :group 'font-latex)

(defun font-latex-extend-region-backwards-tikz-env (beg end)
  "Return position to extend region backwards for TikZ environments.
Return nil if region does not have to be extended for a multiline
environment to fit in.  The region between the positions BEG and
END marks boundaries for searching for environment ends."
;;  (message "Expanding region backwards (%d -- %d), point is %d" beg end (point))
  (save-excursion
    (goto-char end)
    (catch 'extend
      (while (re-search-backward
	      (concat "\\\\end[ \t]*{"
		      (regexp-opt font-latex-tikz-environments t)
		      "\\*?}") beg t)
;;	(message "match-end 0 is %d" (match-end 0))
	(when (save-match-data (and (re-search-backward (concat  "\\\\begin[ \t]*{"
								 (buffer-substring-no-properties
								  (match-beginning 1)
								  (match-end 0)))
							(- beg font-latex-multiline-boundary) t)
				    (< (point) beg)))
;;	  (message "New region is (%d -- %d)" (point) end)
	  (throw 'extend (cons (point) (match-end 0)))))
      (cons beg end))))


(defun font-latex-match-tikz-env (limit)
  "Match tikz patterns up to LIMIT.
Used for patterns like:
\\begin{tikzpicture}
 fontified stuff
\\end{tikzpicture}"
;;  (message "match-tikz-env called at point %d with limit %d" (point) limit)
  (when (re-search-forward (concat "\\\\begin[ \t]*{"
				   (regexp-opt font-latex-tikz-environments t)
				   "\\*?}")
			   limit t)
    (let ((beg (match-beginning 0)) end)
      (if (re-search-forward (concat "\\\\end[ \t]*{"
				     (regexp-quote
				      (buffer-substring-no-properties
				       (match-beginning 1)
				       (match-end 0))))
			     limit t)
          (setq end (match-end 0))
	(progn
	  (goto-char beg)
	  (setq end beg)))
      (cond ((> end beg)
;;	     (message "Found a tikzpicture environment!")
	     (font-latex-put-multiline-property-maybe beg end)
	     (store-match-data (list beg end))
	     (goto-char end)
	     t))
    )))





;; debugging
(setq eval-expression-print-level nil)


(defun debug-prompt (p v &optional m) 
;  ()
  (if m
      (message "%s %s" (prin1-to-string p) (prin1-to-string v))
)
)

(defun filter-keys (keys target)
  (cond
   ((null target) target)
   ((not (listp target)) target)
   ((not (listp (cdr target))) target)
   ((member (car target) keys) (filter-keys keys (cddr target)))
   (t (cons (car target) (filter-keys keys (cdr target))))))

(defvar match-begin)(defvar match-end)
(defun in-bounds (&optional pos)
  "Returns whether the current point is between match-begin and match-end"
  (let ((p (or pos (point))))
;;    (message "%s in-bounds: %d <= %d <= %d" (make-string debug-depth ?.) match-begin p match-end)
    (and (>= p match-begin) (<= p match-end))))

(defstruct 
  (matcher 
   (:type vector) 
   (:constructor nil)
   :named) 
  (name nil :read-only t)
  (face 'font-lock-warning-face :read-only t)
  (match `(lambda (this) nil) :read-only t))
(defvar debug-depth)
(setq debug-depth 0)
(defun invoke-matcher (matcher)
  (cond
   ((matcher-p matcher)
;;    (message "%s[ %s %s" (make-string (1- (incf debug-depth)) ? ) (elt matcher 0) (matcher-name matcher))
    (let ((ret (funcall (matcher-match matcher) matcher)))
;;      (message "%s] %s %s %s" (make-string (decf debug-depth) ? ) (elt matcher 0) (matcher-name matcher) (if ret "success" "failed"))
      ret))
   (t
    (message "%sERROR: Not a matcher: %s" (make-string debug-depth ?.) (prin1-to-string matcher)))))

(defstruct
  (custom-matcher
   (:type vector)
   (:include matcher)
   (:constructor nil)
   (:constructor make-custom-matcher (&key name face match custom))
   :named)
  "Custom matchers that define their own matching functions and need no other fields.  
Can refer to `(custom-matcher-face this)` if necessary."
  (custom nil))

(defstruct
  (noop-matcher
   (:type vector)
   (:include matcher)
   (:constructor nil)
   (:constructor
    make-noop-matcher (&key (name "noop") 
		       &aux (match (lambda (this) (if (in-bounds) (list 't) nil)))))
   :named)
  "A matcher that always succeeds when the current point is in bounds and does nothing to the current point.  
Returns '(t) rather than nil, to indicate a no-op success")

(defstruct
  (char-matcher 
   (:type vector)
   (:include matcher)
   (:constructor nil)
   (:constructor
    make-char-matcher 
    (&key name face char
     &aux (match
	   `(lambda (this)
	      (if (and (in-bounds) (char-equal (following-char) ,char))
		  (progn
		    (forward-char 1)
		    (list (cons ,face (cons (1- (point)) (point)))))
		nil)))))
   :named)
  "Matches a char at the current point if the point is in bounds and updates the current point.  
If matched, returns (face . (start . end)), otherwise returns nil."
  (char nil :read-only t))

(defstruct
  (regex-matcher
   (:type vector)
   (:include matcher)
   (:constructor nil)
   (:constructor 
    make-regex-matcher
    (&key name face regex
     &aux (match
	   `(lambda (this)
	      (if (and (in-bounds) (looking-at ,regex))
		  (let ((start (point))
			(end (match-end 0)))
		    (if (in-bounds end)
			(progn
			  (goto-char end)
			  (list (cons ,face (cons start end))))
		      nil))
		nil)))))
   :named)
  "Matches a regex at the current point and updates the current point.  
If matched, returns (face . (start . end)), otherwise returns nil."
  (regex nil :read-only t))

(defstruct
  (sequence-matcher
   (:type vector)
   (:include matcher)
   (:constructor nil)
   (:constructor
    make-sequence-matcher
    (&rest argz
     &key name 
     &allow-other-keys
     &aux (arguments (filter-keys (list :name) argz))
          (match
	   (progn
	     (assert (all 'matcher-p arguments) :string "Found a non-matcher in the sequence")
	     (lambda (this)
	       (debug-prompt "Handling sequence " (sequence-matcher-name this))
	       (if (not (in-bounds))
		   nil
		 (let ((matches nil))
		   (catch 'fail
		     (save-excursion
		       (dolist (matcher (sequence-matcher-arguments this))
			 (debug-prompt "matcher is " matcher)
			 (let ((match (invoke-matcher matcher)))
			   (cond 
			    ((equal match (list 't))
			     ;; nothing; ignored
			     )
			    ((null match)
			     (throw 'fail nil))
			    ((not (in-bounds))
			     (throw 'fail nil))
			    (t
			     (setq matches (append match matches))
			     (goto-char (cddar match)))
			    )))
		       )
		     (if matches
			 (goto-char (cddar matches)))
		     matches)
		   )))))))
   :named)
  "Matches a sequence of matchers starting at the current point and updates the current point, 
assuming the current point is in bounds.  If matched, returns a list of the matcher's results, 
otherwise returns nil."
  (arguments nil :read-only t))

(defstruct 
  (repeat-matcher
   (:type vector)
   (:include matcher)
   (:constructor nil)
   (:constructor 
    make-repeat-matcher
    (&key matcher
     &aux (match 
	   (lambda (this)
	     (if (not (in-bounds))
		 nil
	       (let ((matches nil))
		 (catch 'done
		   (while t
		     (debug-prompt "repeat-matcher-matcher this " (repeat-matcher-matcher this))
		     (let ((match (save-excursion (invoke-matcher (repeat-matcher-matcher this)))))
		       (if (and match (in-bounds))
			   (progn
			     (goto-char (cddar match))
			     (setq matches (append match matches)))
			 (progn
			   (debug-prompt "no more repeats" (or matches (list 't)))
			   (throw 'done (or matches (list 't))))))))))))))
   :named)
  "Matches zero or more instances of match.  Always succeeds if the starting point is in-bounds."
  (matcher nil :read-only t))

(defstruct 
  (alternate
   (:type vector)
   (:constructor nil)
   (:constructor make-alternate (&key name matcher pred))
   :named)
  (name "<alternate>")
  (matcher nil)
  (pred (make-noop-matcher :name "noop predicate")))

(defstruct
  (alternates-matcher
   (:type vector)
   (:include matcher)
   (:constructor nil)
   (:constructor
    make-alternates-matcher
    (&rest argz
     &key name
     &allow-other-keys
     &aux (arguments (filter-keys (list :name) argz))
          (match
	   (progn
	     (assert (all 'alternate-p arguments) :string "Found a non-alternate in the alternates list")
	     (lambda (this)
	       (debug-prompt "In matcher-alternates, arguments are " (alternates-matcher-arguments this))
	       (if (not (in-bounds))
		   nil
		 (catch 'match
		   (dolist (alt (alternates-matcher-arguments this))
		     (let* ((match (and (save-excursion (invoke-matcher (alternate-pred alt)))
					(save-excursion (invoke-matcher (alternate-matcher alt))))))
		       (if (and match (listp match))
			   (progn
			     (debug-prompt "alternate succeeded " (alternate-name alt))
			     (debug-prompt "alternate succeeded: match is " match)
			     (if (cdr match)
				 (goto-char (cddar match)))
			     (throw 'match match))
			 (debug-prompt "alternate failed " (alternate-name alt)))))
		 nil)))))))
   :named)
  "Tries each of the alternates in arguments in succession, returning the first
one that succeeds, or else returns nil"
  (arguments nil :read-only t))

(defun make-optional-matcher (match)
  "Matches match if possible, but always succeeds if the point is in bounds"
  (make-alternates-matcher :name (concat "Optional " (matcher-name match))
			   (make-alternate :name (matcher-name match) :matcher match)
			   (make-alternate :name "noop" :matcher (make-noop-matcher))))

(defun matcher-list (whichlist start elem sep end &optional trailing-sep leading-sep)
  "Matches <start> (<sep>? <elem> (<sep> <elem>)* <sep>?)? <end>, 
where the leading and trailing optional <sep>s depend on the optional arguments
to matcher-list."
  (make-alternates-matcher 
   :name whichlist
   (make-alternate 
    :matcher (make-sequence-matcher 
	      :name (concat whichlist " mainlist")
	      start
	      (make-optional-matcher (match-space-or-comment))
	      (if leading-sep 
		  (make-optional-matcher sep)
		(make-noop-matcher))
	      (make-optional-matcher (match-space-or-comment))
	      elem
	      (make-repeat-matcher :matcher (make-sequence-matcher :name "sep-elem" sep elem))
	      (make-optional-matcher (match-space-or-comment))
	      (if trailing-sep
		  (make-optional-matcher sep)
		(make-noop-matcher))
	      (make-optional-matcher (match-space-or-comment))
	      end))
   (make-alternate
    :matcher (make-sequence-matcher
	      :name (concat whichlist " empty-list")
	      start 
	      end)))
    )

(defun matcher-key ()
  "Matches names of the form /a/b/c..., or a/b/.c"
  (make-repeat-matcher :matcher
   (make-alternates-matcher
    (make-alternate :name "keyword" 
		    :matcher (make-regex-matcher :name "keyword" 
						 :regex "/?\\.[^])=,}]+" 
						 :face 'font-lock-keyword-face))
    (make-alternate :name "plainname" 
		    :matcher (make-regex-matcher :name "plainname"
						 :regex "/?[^])=,}/]+"
						 :face 'font-lock-variable-name-face)))))
(defstruct
  (delim-group-matcher
   (:type vector)
   (:include matcher)
   (:constructor nil)
   (:constructor
    make-delim-group-matcher
    (&key face open close eat-trailing-spaces
     &aux (match
	   `(lambda (this)
	     (let ((delim-depth 0)
		   (escaped nil)
		   (comment nil)
		   (start (point)))
	       (catch 'done
		 (while t
		   (cond 
		    ((char-equal (following-char) 0)
		     (throw 'done "End of file reached"))
		    ((not (in-bounds))
		     (throw 'done nil))
		    ((char-equal (following-char) ?\\)
		     (setq escaped (not escaped))
		     (forward-char 1))
		    (escaped
		     (setq escaped (not escaped))
		     (forward-char 1))
		    ((char-equal (following-char) ?%)
		     (setq comment t)
		     (forward-char 1))
		    (comment 
		     (setq comment (not (char-equal (following-char) ?\n)))
		     (setq escaped nil)
		     (forward-char 1))
		    ((char-equal (following-char) ,open)
		     (setq delim-depth (1+ delim-depth))
		     (setq escaped nil)
		     (forward-char 1))
		    ((char-equal (following-char) ,close)
		     (if (= delim-depth 1)
			 (progn
			   (forward-char 1)
			   ,(if eat-trailing-spaces
			       `(while (and (in-bounds)
					    (let ((c (following-char)))
					      (or (char-equal c ? )
						  (char-equal c ?\n)
						  (char-equal c ?\t))))
				  (forward-char 1)))
			   (throw 'done (list (cons ,face (cons start (point))))))
		       (progn
			 (setq delim-depth (1- delim-depth))
			 (forward-char 1))))
		    (t 
		     (if (= delim-depth 0)
			 (throw 'done "Characters outside bracegroup"))
		     (forward-char 1)))))))
	   )))
   :named)
  "Matches a balanced brace-delimited group, assuming the point is currently at an open-brace in bounds"
  (open nil :read-only t)
  (close nil :read-only t)
  (eat-trailing-spaces nil :read-only t))

    
(defun matcher-bracegroup (face &optional eat-trailing-space)
  (make-delim-group-matcher :face face :open ?{ :close ?} :eat-trailing-spaces eat-trailing-space))
(defun matcher-parengroup (face &optional eat-trailing-space)
  (make-delim-group-matcher :face face :open ?\( :close ?\) :eat-trailing-spaces eat-trailing-space))

(defun matcher-val (closer face)
;  (debug-prompt "Trying plain val" "")
  (make-custom-matcher 
   :name "match val"
   :face face
   :custom (cons (matcher-bracegroup face) closer)
   :match
   (lambda (this)
      (let ((start (point))
	    (comment nil)
	    (escaped nil)
	    (closer (cdr (custom-matcher-custom this)))
	    (bracegroup (car (custom-matcher-custom this))))
	(catch 'done
	  (while t
	    (cond
	     ((char-equal (following-char) 0)
	      (throw 'done "End of file reached"))
	     ((not (in-bounds))
	      (throw 'done nil))
	     ((char-equal (following-char) ?\\)
	      (setq escaped (not escaped))
	      (forward-char 1))
	     (escaped
	      (setq escaped (not escaped))
	      (forward-char 1))
	     ((char-equal (following-char) ?%)
	      (setq comment t)
	      (forward-char 1))
	     (comment 
	      (setq comment (not (char-equal (following-char) ?\n)))
	      (setq escaped nil)
	      (forward-char 1))
	     ((char-equal (following-char) ?{)
	      (let ((brace (invoke-matcher bracegroup)))
		(if (or (not (listp brace)) (equal brace (list 't)))
		    (throw 'done nil))
		(goto-char (cddar brace))))
	     ((or (char-equal (following-char) ?,)
		  (save-excursion (invoke-matcher closer)))
	      (throw 'done (list (cons (matcher-face this) (cons start (point))))))
	     (t (forward-char 1)))))))))

(defun matcher-keyval (closer)
  "Matches either key=val or just val, delimited by closer"
  (let* ((alternate-tikzset (make-alternate :name "tikzset" :matcher nil))
	 (custom-matcher (make-custom-matcher 
			   :name "TEMPORARY" 
			   :custom alternate-tikzset
			   :match
			   (lambda (this)
			     (cond 
			      ((null (cdr tikzpicture-matcher))
			       (message "cdr tikzpicture-matcher is still null, calling matcher-tikzset directly")
			       (invoke-matcher (matcher-tikzset)))
			      (t
			       (setf (alternate-matcher (custom-matcher-custom this)) (cdr tikzpicture-matcher))
			       (invoke-matcher (alternate-matcher (custom-matcher-custom this)))))
			     ))))
    (setf (alternate-matcher alternate-tikzset) custom-matcher)
    (make-sequence-matcher 
     :name "keyval"
     (matcher-key)
     (match-equal)
     (make-alternates-matcher 
      :name "tikzset-or-val"
      alternate-tikzset
      (make-alternate :name "plainval" :matcher (matcher-val closer 'font-lock-string-face))
      )
   )))

(defun match-open-brace ()
  (make-char-matcher :name "{" :char ?{ :face 'font-lock-preprocessor-face))
(defun match-close-brace ()
  (make-regex-matcher :name "}" :regex "[[:space:]\n]*}[[:space:]\n]*" :face 'font-lock-preprocessor-face))
(defun match-open-bracket ()
  (make-char-matcher :name "[" :char ?\[ :face 'font-lock-preprocessor-face))
(defun match-close-bracket ()
  (make-regex-matcher :name "]" :regex "[[:space:]\n]*\\][[:space:]\n]*" :face 'font-lock-preprocessor-face))
(defun match-open-paren ()
  (make-char-matcher :name "(" :char ?\( :face 'font-lock-preprocessor-face))
(defun match-close-paren ()
  (make-regex-matcher :name ")" :regex "[[:space:]\n]*[)][[:space:]\n]*" :face 'font-lock-preprocessor-face))
(defun match-comma ()
  (make-char-matcher :name "," :char ?, :face 'font-lock-builtin-face))
(defun match-equal ()
  (make-char-matcher :name "=" :char ?= :face 'font-lock-builtin-face))
(defun match-comment ()
  (make-sequence-matcher :name "comment"
   (make-regex-matcher :name "comment-line" :regex "[[:space:]]*%" :face 'font-lock-comment-delimiter-face)
   (make-regex-matcher :name "after-comment whitespace" :regex "[^\n]*\n[[:space:]\n]*" :face 'font-lock-comment-face)))
(defun match-space-or-comment ()
  (make-alternates-matcher :name "space-or-comment"
   (make-alternate :matcher (match-comment))
   (make-alternate :matcher (make-regex-matcher :name "whitespace" 
						:regex "[[:space:]\n]+" 
						:face 'font-lock-comment-face))))

(defun matcher-tikz-options (open close)
  "Matches a comma-separated list of tikz key/value pairs, delimited by open and close"
  (matcher-list 
   "tikz-options"
   open
   (make-alternates-matcher :name "tikz option entries"
    (make-alternate :name "keyval" :matcher (matcher-keyval close))
    (make-alternate :name "plainname" :matcher (make-regex-matcher :name "plainname" :regex "[^],)}%]+" :face 'font-lock-variable-name-face))
    (make-alternate :name "comment" :matcher (make-optional-matcher (match-space-or-comment)))
    )
   (make-sequence-matcher :name "separator" (match-comma) (make-repeat-matcher :matcher (match-space-or-comment)))
   close
   t t
   ))

(defun matcher-tikzset ()
  (interactive)
  (message "building a matcher-tikzset")
  (matcher-tikz-options (match-open-brace) (match-close-brace)))

(defun matcher-tikz-brackets ()
  (interactive)
  (matcher-tikz-options (match-open-bracket) (match-close-bracket)))

(defun matcher-process-font-locks (list)
  (setq list (delq 't list))
  (if list
      (progn
	(dolist (item (reverse list))
	  (if (car item)
	      (put-text-property (cadr item) (cddr item) 'face (car item))))
	(goto-char (cddar list))
	)
  ))

(defun make-coordinate-matcher ()
  (let ((single-coord
	 (make-sequence-matcher
	  (make-optional-matcher (match-space-or-comment))
	  (make-char-matcher :char ?\( :face 'font-lock-string-face)
	  (make-optional-matcher (match-space-or-comment))
	  (make-optional-matcher (matcher-tikz-brackets))
	  (make-optional-matcher (match-space-or-comment))
	  (make-repeat-matcher :matcher 
			       (make-sequence-matcher
				(make-regex-matcher :name "coord-data" 
						    :regex "[^()]+" 
						    :face 'font-lock-string-face)
				(make-optional-matcher (matcher-parengroup 'font-lock-string-face t))))
	  (make-char-matcher :char ?\) :face 'font-lock-string-face)
	  (make-optional-matcher (match-space-or-comment)))))
    (make-sequence-matcher
     (make-regex-matcher :name "offset ++?" :regex "\\+?\\+?" :face 'font-lock-string-face)	  
     single-coord
     (make-optional-matcher (make-sequence-matcher :name "relative coord"
			     (make-char-matcher :name "relative +?" :char ?+ :\face 'font-lock-string-face)
			     single-coord))
     )))

(defun test ()
  (interactive)
  (invoke-matcher (make-coordinate-matcher)))

(defun matcher-tikz-command ()
  (interactive)
  (if (null (cdr tikzpicture-matcher))
      (setcdr tikzpicture-matcher (matcher-tikzset)))
  (make-alternates-matcher
   (make-alternate 
    :name "tikzset" 
    :matcher (make-sequence-matcher
	      :name "tikzset"
	      (make-regex-matcher :name "\\tikzset" :regex "\\\\tikzset" :face 'font-lock-keyword-face)
	      (cdr tikzpicture-matcher)))
   (make-alternate 
    :name "tikzset error" 
    :matcher (make-sequence-matcher
	      :name "tikzset error"
	      (make-regex-matcher :name "bad \\tikzset" :regex "\\\\tikzset" :face 'font-lock-keyword-face)
	      (matcher-bracegroup 'font-lock-warning-face t)))
   (make-alternate 
    :name "tikz command" 
    :matcher (make-sequence-matcher 
	      :name "tikz command" 
	      (make-regex-matcher :name "tikz command" :regex "\\\\[A-Za-z]+[[:space:]]*" :face 'font-lock-keyword-face)
	      (make-repeat-matcher
	       :matcher (make-alternates-matcher
			 (make-alternate :name "comment" :matcher (match-space-or-comment))
			 (make-alternate 
			  :name "options" 
			  :matcher (matcher-tikz-brackets) 
			  :pred (match-open-bracket))
			 (make-alternate 
			  :name "nodename"
			  :matcher (make-coordinate-matcher))
			 (make-alternate
			  :name "circle"
			  :matcher (make-sequence-matcher
				    (make-regex-matcher :regex "circle" :face 'font-lock-keyword-face)
				    (make-optional-matcher (match-space-or-comment))
				    (make-alternates-matcher
				     (make-alternate :matcher (matcher-tikz-brackets))
				     (make-alternate :matcher (make-regex-matcher 
							       :regex "([^()]*)" 
							       :face 'font-lock-string-face)))))
			 (make-alternate
			  :name "ellipse"
			  :matcher (make-sequence-matcher
				    (make-regex-matcher :regex "ellipse" :face 'font-lock-keyword-face)
				    (make-optional-matcher (match-space-or-comment))
				    (make-alternates-matcher
				     (make-alternate :matcher (matcher-tikz-brackets))
				     (make-alternate 
				      :matcher (make-regex-matcher 
						:regex "([^()%]+[[:space:]]+and[[:space:]][^()%]+)" 
						:face 'font-lock-string-face)))))
			 (make-alternate
			  :name "bracegroup" 
			  :matcher (matcher-bracegroup nil t)
			  :pred (match-open-brace))))
	      (make-char-matcher :char ?\; :face 'font-lock-preprocessor-face)))
   (make-alternate
    :name "command error"
    :matcher (make-sequence-matcher
	      :name "command error"
	      (make-regex-matcher :regex "\\\\[A-Za-z]+[[:space:]]*" :face 'font-lock-keyword-face)
	      (make-regex-matcher :regex "[^;%]*\\(%[^\n]*\n[^;%]*\\)*;" :face 'font-lock-warning-face)))
   (make-alternate
    :name "error"
    :matcher (make-regex-matcher :regex "[^;%]*\\(%[^\n]*\n[^;%]*\\)*;" :face 'font-lock-warning-face))))


(defun matcher-tikzpicture ()
  (message "building a matcher-tikzpicture")
  (make-sequence-matcher 
   :name "tikzpicture"
   (make-regex-matcher :name "begin-tikzpicture" :regex "\\\\begin{tikzpicture}" :face 'font-lock-constant-face)
   (make-optional-matcher (match-space-or-comment))
   (make-optional-matcher (matcher-tikz-brackets))
   (make-optional-matcher (match-space-or-comment))
   (make-repeat-matcher 
    :matcher (make-alternates-matcher 
	      (make-alternate :name "comment" :matcher (match-space-or-comment))
	      (make-alternate :name "command" :matcher (matcher-tikz-command))))
   (make-regex-matcher :name "end-tikzpicture" :regex "\\\\end{tikzpicture}" :face 'font-lock-constant-face)))


(defvar tikzpicture-matcher (cons nil nil))
(defun simple-setup ()
  (interactive)
  (if (null (cdr tikzpicture-matcher))
      (setcar tikzpicture-matcher (matcher-tikzpicture)))
  (matcher-process-font-locks (invoke-matcher (car tikzpicture-matcher))))

(defun simple-stop ()
  (interactive)
  (remove-overlays))
	 


(defun tikz-anchored-matcher (limit)
;;  (message "Anchored match at %d with limit %d" (point) limit)
  ;; (if (font-latex-match-tikz-env (point-max))
  ;;     (let ((beg (match-beginning 0))
  ;; 	     (end (match-end 0)))
  ;; 	 (message "Found tikz match at (%d -- %d)" beg end)
  ;; 	 (funcall pre beg end)
  ;; 	 (funcall post beg end)
  ;; 	 t)
  ;;   (progn 
  ;;     (message "DIDN'T MATCH tikz at %d" (point))
  ;;     nil)))
  (goto-char (match-end 0))
  nil)

(defun tikz-pre-matcher () 
  (let ((beg (match-beginning 0))
	(end (match-end 0)))
;;    (message "PRE Matching the tikz environment from (%s -- %s)!"
;;	     (prin1-to-string beg)
;;	     (prin1-to-string end))
    (goto-char beg) 
    (simple-stop)
    (set-text-properties beg end nil)
    end))
(defun tikz-post-matcher ()
  (let ((beg (match-beginning 0))
	(end (match-end 0)))
;;    (message "POST Matching the tikz environment from (%s -- %s), point is %d"
;;	     (prin1-to-string beg)
;;	     (prin1-to-string end) 
;;	     (point))
    (goto-char beg)
    (simple-setup)
    (goto-char end)
    end
    ))


;; (add-hook 'LaTeX-mode-hook
;; 	  (lambda ()
;; 	    (message "%s" "Adding tikz hooks")
;; 	    (add-to-list 'font-latex-extend-region-functions 'font-latex-extend-region-backwards-tikz-env)
;; 	    (let ((match-tikz (cons 'font-latex-match-tikz-env
;; 				    (list 'tikz-anchored-matcher '(tikz-pre-matcher) '(tikz-post-matcher)))))
;; 	      (message "tikzhook is %s" (prin1-to-string match-tikz))
;; 	      (add-to-list 'font-latex-keywords-2 match-tikz)
;; 	      (add-to-list 'font-latex-keywords-1 match-tikz)
;; 	      (add-to-list 'font-latex-keywords   match-tikz)
;; 	      )))

;; (setq font-latex-keywords (cdr font-latex-keywords))
;; (setq font-latex-keywords-1 (cdr font-latex-keywords-1))
;; (setq font-latex-keywords-2 (cdr font-latex-keywords-2))
;; (setq LaTeX-mode-hook (cdr LaTeX-mode-hook))
;; (setq font-latex-extend-region-functions (cdr font-latex-extend-region-functions))

(add-hook 'LaTeX-mode-hook
	  (lambda ()
;;	    (message "Advising font-latex-fontify-region")
	    (defadvice font-latex-fontify-region (after fontify-tikz (beg end &optional loudly) activate)
	      "Wrapped version of font-latex-fontify-region that handles tikz too"
	      (trigger-tikz-parsing beg end loudly)
)))

(defun trigger-tikz-parsing (beg end &optional loudly)
;;  (message "In advice! (beg -- end) is (%d -- %d)" beg end)
  (save-excursion
    (let ((region (font-latex-extend-region-backwards-tikz-env beg end))
	  (old-eval-depth max-lisp-eval-depth))
      (setq match-begin (car region) 
	    match-end (cdr region) 
	    debug-depth 0
	    max-lisp-eval-depth 2000)
      (message "In advice! (beg -- end) is (%d -- %d), region is (%d -- %d)" beg end match-begin match-end)
      (goto-char match-begin)
      (cond ((font-latex-match-tikz-env match-end)
	     (message "fontifying a tikzpicture (%d -- %d)" match-begin match-end)
	     (goto-char match-begin)
	     (simple-setup)))
      (setq match-begin nil 
	    match-end nil
	    max-lisp-eval-depth old-eval-depth)
      ))
  )

