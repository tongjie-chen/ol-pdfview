;; This file reloads org-link create and open link functions to put a ::N, N for page number
;; This will open a pdf in org-mode if your default is pdf-tools and go to Page N
;; By: Tongjie Chen
;; My mod of org-goto line for pdf-view to go to page number.
(defun org-open-file (path &optional in-emacs line search)
  "Open the file at PATH.
First, this expands any special file name abbreviations.  Then the
configuration variable `org-file-apps' is checked if it contains an
entry for this file type, and if yes, the corresponding command is launched.

If no application is found, Emacs simply visits the file.

With optional prefix argument IN-EMACS, Emacs will visit the file.
With a double \\[universal-argument] \\[universal-argument] \
prefix arg, Org tries to avoid opening in Emacs
and to use an external application to visit the file.

Optional LINE specifies a line to go to, optional SEARCH a string
to search for.  If LINE or SEARCH is given, the file will be
opened in Emacs, unless an entry from `org-file-apps' that makes
use of groups in a regexp matches.

If you want to change the way frames are used when following a
link, please customize `org-link-frame-setup'.

If the file does not exist, throw an error."
  (let* ((file (if (equal path "") buffer-file-name
		 (substitute-in-file-name (expand-file-name path))))
	 (file-apps (append org-file-apps (org--file-default-apps)))
	 (apps (cl-remove-if #'org--file-apps-entry-dlink-p file-apps))
	 (apps-dlink (cl-remove-if-not #'org--file-apps-entry-dlink-p
				       file-apps))
	 (remp (and (assq 'remote apps) (file-remote-p file)))
	 (dirp (unless remp (file-directory-p file)))
	 (file (if (and dirp org-open-directory-means-index-dot-org)
		   (concat (file-name-as-directory file) "index.org")
		 file))
	 (a-m-a-p (assq 'auto-mode apps))
	 (dfile (downcase file))
	 ;; Reconstruct the original link from the PATH, LINE and
	 ;; SEARCH args.
	 (link (cond (line (concat file "::" (number-to-string line)))
		     (search (concat file "::" search))
		     (t file)))
	 (dlink (downcase link))
	 (ext
	  (and (string-match "\\`.*?\\.\\([a-zA-Z0-9]+\\(\\.gz\\)?\\)\\'" dfile)
	       (match-string 1 dfile)))
	 (save-position-maybe
	  (let ((old-buffer (current-buffer))
		(old-pos (point))
		(old-mode major-mode))
	    (lambda ()
	      (and (derived-mode-p 'org-mode)
		   (eq old-mode 'org-mode)
		   (or (not (eq old-buffer (current-buffer)))
		       (not (eq old-pos (point))))
		   (org-mark-ring-push old-pos old-buffer)))))
	 cmd link-match-data)
    (cond
     ((member in-emacs '((16) system))
      (setq cmd (cdr (assq 'system apps))))
     (in-emacs (setq cmd 'emacs))
     (t
      (setq cmd (or (and remp (cdr (assq 'remote apps)))
		    (and dirp (cdr (assq 'directory apps)))
		    ;; First, try matching against apps-dlink if we
		    ;; get a match here, store the match data for
		    ;; later.
		    (let ((match (assoc-default dlink apps-dlink
						'string-match)))
		      (if match
			  (progn (setq link-match-data (match-data))
				 match)
			(progn (setq in-emacs (or in-emacs line search))
			       nil))) ; if we have no match in apps-dlink,
					; always open the file in emacs if line or search
					; is given (for backwards compatibility)
		    (assoc-default dfile
				   (org--file-apps-regexp-alist apps a-m-a-p)
				   'string-match)
		    (cdr (assoc ext apps))
		    (cdr (assq t apps))))))
    (when (eq cmd 'system)
      (setq cmd (cdr (assq 'system apps))))
    (when (eq cmd 'default)
      (setq cmd (cdr (assoc t apps))))
    (when (eq cmd 'mailcap)
      (require 'mailcap)
      (mailcap-parse-mailcaps)
      (let* ((mime-type (mailcap-extension-to-mime (or ext "")))
	     (command (mailcap-mime-info mime-type)))
	(if (stringp command)
	    (setq cmd command)
	  (setq cmd 'emacs))))
    (when (and (not (eq cmd 'emacs)) ; Emacs has no problems with non-ex files
	       (not (file-exists-p file))
	       (not org-open-non-existing-files))
      (user-error "No such file: %s" file))
    (cond
     ((and (stringp cmd) (not (string-match "^\\s-*$" cmd)))
      ;; Remove quotes around the file name - we'll use shell-quote-argument.
      (while (string-match "['\"]%s['\"]" cmd)
	(setq cmd (replace-match "%s" t t cmd)))
      (setq cmd (replace-regexp-in-string
		 "%s"
		 (shell-quote-argument (convert-standard-filename file))
		 cmd
		 nil t))

      ;; Replace "%1", "%2" etc. in command with group matches from regex
      (save-match-data
	(let ((match-index 1)
	      (number-of-groups (- (/ (length link-match-data) 2) 1)))
	  (set-match-data link-match-data)
	  (while (<= match-index number-of-groups)
	    (let ((regex (concat "%" (number-to-string match-index)))
		  (replace-with (match-string match-index dlink)))
	      (while (string-match regex cmd)
		(setq cmd (replace-match replace-with t t cmd))))
	    (setq match-index (+ match-index 1)))))

      (save-window-excursion
	(message "Running %s...done" cmd)
	(start-process-shell-command cmd nil cmd)
	(and (boundp 'org-wait) (numberp org-wait) (sit-for org-wait))))
     ((or (stringp cmd)
	  (eq cmd 'emacs))
      (funcall (cdr (assq 'file org-link-frame-setup)) file)
      (widen)
      (cond (line (if (derived-mode-p 'pdf-view-mode) (pdf-view-goto-page line) (org-goto-line line))
		  (when (derived-mode-p 'org-mode) (org-reveal)))
	    (search (condition-case err
			(org-link-search search)
		      ;; Save position before error-ing out so user
		      ;; can easily move back to the original buffer.
		      (error (funcall save-position-maybe)
			     (error (nth 1 err)))))))
     ((functionp cmd)
      (save-match-data
	(set-match-data link-match-data)
	(condition-case nil
	    (funcall cmd file link)
	  ;; FIXME: Remove this check when most default installations
	  ;; of Emacs have at least Org 9.0.
	  ((debug wrong-number-of-arguments wrong-type-argument
		  invalid-function)
	   (user-error "Please see Org News for version 9.0 about \
`org-file-apps'--Lisp error: %S" cmd)))))
     ((consp cmd)
      ;; FIXME: Remove this check when most default installations of
      ;; Emacs have at least Org 9.0.  Heads-up instead of silently
      ;; fall back to `org-link-frame-setup' for an old usage of
      ;; `org-file-apps' with sexp instead of a function for `cmd'.
      (user-error "Please see Org News for version 9.0 about \
`org-file-apps'--Error: Deprecated usage of %S" cmd))
     (t (funcall (cdr (assq 'file org-link-frame-setup)) file)))
    (funcall save-position-maybe)))

;; For store with pdf-view-mode page.
(defun org-store-link (arg &optional interactive?)
  "Store a link to the current location.
\\<org-mode-map>
This link is added to `org-stored-links' and can later be inserted
into an Org buffer with `org-insert-link' (`\\[org-insert-link]').

For some link types, a `\\[universal-argument]' prefix ARG is interpreted.  \
A single
`\\[universal-argument]' negates `org-context-in-file-links' for file links or
`org-gnus-prefer-web-links' for links to Usenet articles.

A `\\[universal-argument] \\[universal-argument]' prefix ARG forces \
skipping storing functions that are not
part of Org core.

A `\\[universal-argument] \\[universal-argument] \\[universal-argument]' \
prefix ARG forces storing a link for each line in the
active region.

Assume the function is called interactively if INTERACTIVE? is
non-nil."
  (interactive "P\np")
  (org-load-modules-maybe)
  (if (and (equal arg '(64)) (org-region-active-p))
      (save-excursion
	(let ((end (region-end)))
	  (goto-char (region-beginning))
	  (set-mark (point))
	  (while (< (point-at-eol) end)
	    (move-end-of-line 1) (activate-mark)
	    (let (current-prefix-arg)
	      (call-interactively 'org-store-link))
	    (move-beginning-of-line 2)
	    (set-mark (point)))))
    (setq org-store-link-plist nil)
    (let (link cpltxt desc search custom-id agenda-link) ;; description
      (cond
       ;; Store a link using an external link type, if any function is
       ;; available. If more than one can generate a link from current
       ;; location, ask which one to use.
       ((and (not (equal arg '(16)))
	     (let ((results-alist nil))
	       (dolist (f (org-store-link-functions))
		 (when (funcall f)
		   ;; XXX: return value is not link's plist, so we
		   ;; store the new value before it is modified.  It
		   ;; would be cleaner to ask store link functions to
		   ;; return the plist instead.
		   (push (cons f (copy-sequence org-store-link-plist))
			 results-alist)))
	       (pcase results-alist
		 (`nil nil)
		 (`((,_ . ,_)) t)	;single choice: nothing to do
		 (`((,name . ,_) . ,_)
		  ;; Reinstate link plist associated to the chosen
		  ;; function.
		  (apply #'org-link-store-props
			 (cdr (assoc-string
			       (completing-read
                                (format "Store link with (default %s): " name)
                                (mapcar #'car results-alist)
                                nil t nil nil (symbol-name name))
			       results-alist)))
		  t))))
	(setq link (plist-get org-store-link-plist :link))
	(setq desc (or (plist-get org-store-link-plist :description)
		       link)))

       ;; Store a link from a remote editing buffer.
       ((org-src-edit-buffer-p)
	(let ((coderef-format (org-src-coderef-format))
	      (format-link
	       (lambda (label)
		 (if org-src-source-file-name
		     (format "file:%s::(%s)" org-src-source-file-name label)
		   (format "(%s)" label)))))
	  (cond
	   ;; Code references do not exist in this type of buffer.
	   ;; Pretend we're linking from the source buffer directly.
	   ((not (memq (org-src-source-type) '(example-block src-block)))
	    (with-current-buffer (org-src-source-buffer)
	      (org-store-link arg interactive?))
	    (setq link nil))
	   ;; A code reference exists.  Use it.
	   ((save-excursion
	      (beginning-of-line)
	      (re-search-forward (org-src-coderef-regexp coderef-format)
				 (line-end-position)
				 t))
	    (setq link (funcall format-link (match-string-no-properties 3))))
	   ;; No code reference.  Create a new one then store the link
	   ;; to it, but only in the function is called interactively.
	   (interactive?
	    (end-of-line)
	    (let* ((label (read-string "Code line label: "))
		   (reference (format coderef-format label))
		   (gc (- 79 (length reference))))
	      (if (< (current-column) gc)
		  (org-move-to-column gc t)
		(insert " "))
	      (insert reference)
	      (setq link (funcall format-link label))))
	   ;; No code reference, and non-interactive call.  Don't know
	   ;; what to do.  Give up.
	   (t (setq link nil)))))

       ;; We are in the agenda, link to referenced location
       ((equal (bound-and-true-p org-agenda-buffer-name) (buffer-name))
	(let ((m (or (get-text-property (point) 'org-hd-marker)
		     (get-text-property (point) 'org-marker))))
	  (when m
	    (org-with-point-at m
	      (setq agenda-link (org-store-link nil interactive?))))))

       ((eq major-mode 'calendar-mode)
	(let ((cd (calendar-cursor-to-date)))
	  (setq link
		(format-time-string
		 (car org-time-stamp-formats)
		 (apply 'encode-time
			(list 0 0 0 (nth 1 cd) (nth 0 cd) (nth 2 cd)
			      nil nil nil))))
	  (org-link-store-props :type "calendar" :date cd)))

       ((eq major-mode 'help-mode)
	(let ((symbol (replace-regexp-in-string
		       ;; Help mode escapes backquotes and backslashes
		       ;; before displaying them.  E.g., "`" appears
		       ;; as "\'" for reasons.  Work around this.
		       (rx "\\" (group (or "`" "\\"))) "\\1"
		       (save-excursion
			 (goto-char (point-min))
			 (looking-at "^[^ ]+")
			 (match-string 0)))))
	  (setq link (concat "help:" symbol)))
	(org-link-store-props :type "help"))

       ((eq major-mode 'w3-mode)
	(setq cpltxt (if (and (buffer-name)
			      (not (string-match "Untitled" (buffer-name))))
			 (buffer-name)
		       (url-view-url t))
	      link (url-view-url t))
	(org-link-store-props :type "w3" :url (url-view-url t)))

       ((eq major-mode 'image-mode)
	(setq cpltxt (concat "file:"
			     (abbreviate-file-name buffer-file-name))
	      link cpltxt)
	(org-link-store-props :type "image" :file buffer-file-name))

       ;; In dired, store a link to the file of the current line
       ((derived-mode-p 'dired-mode)
	(let ((file (dired-get-filename nil t)))
	  (setq file (if file
			 (abbreviate-file-name
			  (expand-file-name (dired-get-filename nil t)))
		       ;; otherwise, no file so use current directory.
		       default-directory))
	  (setq cpltxt (concat "file:" file)
		link cpltxt)))

       ((derived-mode-p 'pdf-view-mode)
	(setq search (pdf-view-current-page))
	(setq link (concat "file:" (abbreviate-file-name (buffer-file-name (buffer-base-buffer)))
			   "::" (number-to-string search)))
	(setq cpltxt link)) 

       ((setq search (run-hook-with-args-until-success
		      'org-create-file-search-functions))
	(setq link (concat "file:" (abbreviate-file-name buffer-file-name)
			   "::" search))
	(setq cpltxt (or link))) ;; description

       ((and (buffer-file-name (buffer-base-buffer)) (derived-mode-p 'org-mode))
	(org-with-limited-levels
	 (setq custom-id (org-entry-get nil "CUSTOM_ID"))
	 (cond
	  ;; Store a link using the target at point
	  ((org-in-regexp "[^<]<<\\([^<>]+\\)>>[^>]" 1)
	   (setq cpltxt
		 (concat "file:"
			 (abbreviate-file-name
			  (buffer-file-name (buffer-base-buffer)))
			 "::" (match-string 1))
		 link cpltxt))
	  ((and (featurep 'org-id)
		(or (eq org-id-link-to-org-use-id t)
		    (and interactive?
			 (or (eq org-id-link-to-org-use-id 'create-if-interactive)
			     (and (eq org-id-link-to-org-use-id
				      'create-if-interactive-and-no-custom-id)
				  (not custom-id))))
		    (and org-id-link-to-org-use-id (org-entry-get nil "ID"))))
	   ;; Store a link using the ID at point
	   (setq link (condition-case nil
			  (prog1 (org-id-store-link)
			    (setq desc (or (plist-get org-store-link-plist
						      :description)
					   "")))
			(error
			 ;; Probably before first headline, link only to file
			 (concat "file:"
				 (abbreviate-file-name
				  (buffer-file-name (buffer-base-buffer))))))))
	  (t
	   ;; Just link to current headline.
	   (setq cpltxt (concat "file:"
				(abbreviate-file-name
				 (buffer-file-name (buffer-base-buffer)))))
	   ;; Add a context search string.
	   (when (org-xor org-link-context-for-files (equal arg '(4)))
	     (let* ((element (org-element-at-point))
		    (name (org-element-property :name element))
		    (context
		     (cond
		      ((let ((region (org-link--context-from-region)))
			 (and region (org-link--normalize-string region t))))
		      (name)
		      ((org-before-first-heading-p)
		       (org-link--normalize-string (org-current-line-string) t))
		      (t (org-link-heading-search-string)))))
	       (when (org-string-nw-p context)
		 (setq cpltxt (format "%s::%s" cpltxt context))
		 (setq desc
		       (or name
			   ;; Although description is not a search
			   ;; string, use `org-link--normalize-string'
			   ;; to prettify it (contiguous white spaces)
			   ;; and remove volatile contents (statistics
			   ;; cookies).
			   (and (not (org-before-first-heading-p))
				(org-link--normalize-string
				 (org-get-heading t t t t)))
			   "NONE")))))
	   (setq link cpltxt)))))

       ((buffer-file-name (buffer-base-buffer))
	;; Just link to this file here.
	(setq cpltxt (concat "file:"
			     (abbreviate-file-name
			      (buffer-file-name (buffer-base-buffer)))))
	;; Add a context search string.
	(when (org-xor org-link-context-for-files (equal arg '(4)))
	  (let ((context (org-link--normalize-string
			  (or (org-link--context-from-region)
			      (org-current-line-string))
			  t)))
	    ;; Only use search option if there is some text.
	    (when (org-string-nw-p context)
	      (setq cpltxt (format "%s::%s" cpltxt context))
	      (setq desc "NONE"))))
	(setq link cpltxt))

       (interactive?
	(user-error "No method for storing a link from this buffer"))

       (t (setq link nil)))

      ;; We're done setting link and desc, clean up
      (when (consp link) (setq cpltxt (car link) link (cdr link)))
      (setq link (or link cpltxt)
	    desc (or desc cpltxt))
      (cond ((not desc))
	    ((equal desc "NONE") (setq desc nil))
	    (t (setq desc (org-link-display-format desc))))
      ;; Store and return the link
      (if (not (and interactive? link))
	  (or agenda-link (and link (org-link-make-string link desc)))
	(if (member (list link desc) org-stored-links)
	    (message "This link already exists")
	  (push (list link desc) org-stored-links)
	  (message "Stored: %s" (or desc link))
	  (when custom-id
	    (setq link (concat "file:"
			       (abbreviate-file-name
				(buffer-file-name (buffer-base-buffer)))
			       "::#" custom-id))
	    (push (list link desc) org-stored-links)))
	(car org-stored-links)))))
