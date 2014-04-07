;;; fpco-mode --- A minor mode for using FP Complete's IDE functionality

;; Copyright (c) 2013, FP Complete
;; Copyright (c) 2013, Chris Done

;; Author:    Chris Done <chrisdone@fpcomplete.com>
;; Created:   25-Oct-2013
;; Version:   0.0.0
;; Keywords:  development, haskell
;; Stability: unstable

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:

;;     * Redistributions of source code must retain the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer.

;;     * Redistributions in binary form must reproduce the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer in the documentation and/or other materials
;;       provided with the distribution.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
;; COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
;; OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;; A minor mode for adding structured editing to Haskell.


;; Requirements

(require 'json)
(require 'haskell-mode)
(require 'auto-complete)
(require 'flycheck)


;; Groups

(defgroup fpco nil
  "FP Complete library for Haskell"
  :group 'haskell)


;; Mode

(defvar fpco-old-flycheck-checker nil)

(defvar fpco-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-.") 'fpco/goto)
    (define-key map (kbd "C-c C-t") 'fpco/type)
    (define-key map (kbd "C-c C-i") 'fpco/hoogle-type)
    (define-key map (kbd "C-c C-d") 'fpco/hoogle-doc)
    map)
  "Rebinds (and adds) keybindings for common IDE operations for
  Haskell modules.")

(define-minor-mode fpco-mode
  "Mode for using FP Complete functions with projects."
  :lighter " FP"
  :keymap fpco-mode-map
  (if fpco-mode
      (fpco-mode-start)
    (fpco-mode-stop)))

(defun fpco-mode-start ()
  "Start the mode, setup hooks, etc."
  (add-hook 'after-save-hook 'fpco-save-hook nil t)
  (set (make-local-variable 'ac-sources)
       (cons 'ac-source-fpco
             ac-sources))
  (setq fpco-old-flycheck-checker flycheck-checker)
  (flycheck-select-checker 'fpco-check))

(defun fpco-mode-stop ()
  "Stop the mode, kill hooks, etc."
  (remove-hook 'after-save-hook 'fpco-save-hook t)
  (setq ac-sources (remove-if (lambda (source) (eq 'ac-source-fpco source))
                              ac-sources))
  (flycheck-select-checker fpco-old-flycheck-checker))

(defun fpco-save-hook ()
  "Hook to write Haskell modules back to the server."
  (when fpco-mode
    (with-fpco
     (fpco/save-module))))


;; Customizations

(defcustom fpco-program-name "fpco-api"
  "The program name to call for the server."
  :group 'fpco)

(defcustom fpco-port 1990
  "The port to connect to fpco-api on."
  :group 'fpco)

(defcustom fpco-server-buffer-name "*fpco-api*"
  "The buffer name used for running and logging the fpco-api server."
  :group 'fpco)

(defcustom fpco-check-on-save nil
  "Automatically compile on save?"
  :group 'fpco)

(defcustom fpco-type-popup t
  "Popup a help buffer for types."
  :group 'fpco)

(defcustom fpco-jump-to-external-imports t
  "Jump to external imports?"
  :group 'fpco)


;; Interactive commands

(defun fpco/start ()
  "Start the fpco-api."
  (interactive)
  (if (get-buffer fpco-server-buffer-name)
      (switch-to-buffer fpco-server-buffer-name)
    (let ((default-directory "~/")
          (comint-process-echoes nil))
      (with-current-buffer (get-buffer-create fpco-server-buffer-name)
        (add-hook 'comint-output-filter-functions
                  'comint-truncate-buffer)
        (let ((point (point))
              (notice
               "This is the FP Complete fpco-api buffer. To kill the server, just kill this buffer."))
          (insert notice)
          (let ((o (make-overlay point (point))))
            (overlay-put o 'face font-lock-warning-face))
          (insert "\n\n")
          (setq point (point))
          (insert "-- Command: \n-- \n-- " (fpco-launch-string) "\n")
          (let ((o (make-overlay point (point))))
            (overlay-put o 'face font-lock-comment-face)))
        (shell (current-buffer))
        (goto-char (point-max))
        (comint-send-string (get-buffer-process (current-buffer)) (fpco-launch-string))))))

(defun fpco/hoogle-mode (&optional search)
  "Open the hoogle mode buffer."
  (interactive "P")
  (with-fpco
   (let ((the-search (when search
		       (read-from-minibuffer "Enter a search: "))))
     (let ((my-fpco-pid fpco-pid))
       (switch-to-buffer-other-window "*Hoogle*")
       (if (eq major-mode 'fpco-hoogle-mode)
           (set (make-local-variable 'fpco-pid) my-fpco-pid)
         (progn (fpco-hoogle-mode)
                (set (make-local-variable 'fpco-pid) my-fpco-pid)))
       (when search
	 (goto-char (point-min))
	 (let ((inhibit-buffer-read-only t))
	   (delete-region (line-beginning-position)
			  (line-end-position)))
	 (insert the-search))))))

(defun fpco/save-module ()
  "Save the current module to the server."
  (interactive)
  (with-fpco
   (let ((filename
          (file-relative-name (buffer-file-name)
                              fpco-root)))
     (message "Saving...")
     (fpco-run-command
      (list :tag "MsgSaveModule" :contents (list fpco-pid fpco-root filename))
      (lambda (status)
        (if (eq status t) ;; t or :json-false. IF would consider both
            ;; "truthy". That's why the EQ is necessary.
            (progn (message "Changed on server.")
                   (when (y-or-n-p "The file has changed on the server and the local file is updated. Revert this buffer? ")
                     (revert-buffer nil t t)))
          (message "Saved on server.")))))))

(defun fpco/type (region)
  "Get type info of REGION."
  (interactive (list (fpco-ident-region)))
  (with-fpco
   (fpco-with-region
    region
    (lambda (start-line start-col end-line end-col)
      (fpco-run-command
       (list :tag "MsgTypeInfo"
             :contents (list
                        fpco-pid
                        (fpco-relative-name)
                        start-line
                        start-col
                        end-line
                        end-col))
       (lambda (infos)
         (let ((first (car infos)))
           (when first
             (message "%s :: %s"
                      (elt first 4)
                      (elt first 5)))
           (when fpco-type-popup
             (if (null infos)
                 (message "No type info!")
               (fpco-make-type-buffer infos))))))))))

(defun fpco/goto (region)
  "Go to the definition of the identifier in REGION."
  (interactive (list (fpco-ident-region)))
  (with-fpco
   (fpco-with-region
    region
    (lambda (start-line start-col end-line end-col)
      (fpco-run-command
       (list :tag "MsgGetDefinition"
             :contents (list fpco-pid
                             fpco-root
                             (fpco-relative-name)
                             start-line
                             start-col
                             end-line
                             end-col))
       (lambda (source)
         (pcase (cdr (assoc 'tag source))
           (`"DefinitionImport"
            (let ((imported (cdr (assoc 'contents source))))
              (cond
               ((elt imported 3)
                (fpco-goto-loc (elt imported 3))
                (message "Imported `%s`, defined in %s"
                         (elt imported 0)
                         (elt (elt imported 1) 0)))
               (t
                (when fpco-jump-to-external-imports
                  (fpco-goto-loc (elt imported 4)))
                (message "Imported `%s' from %s, defined in %s (%s)"
                         (elt imported 0)
                         (elt (elt imported 2) 1)
                         (elt (elt imported 1) 1)
                         (elt (elt imported 1) 0))))))
           (`"DefinitionLoc"
            (fpco-goto-loc (cdr (assoc 'contents source))))
           (`"DefinitionUseless"
            (message "Definition: %s" (cdr (assoc 'contents source))))
           (t
            (when source
              (message "Imported: %S" source))))))))))

(defun fpco/hoogle-type (region)
  "Hoogle search the identifier at REGION and print the type."
  (interactive (list (fpco-ident-region)))
  (with-fpco
   (let ((name (buffer-substring-no-properties (car region) (cdr region))))
     (fpco-run-command
      (list :tag "MsgHoogleIdent"
            :contents (list fpco-pid
                            (fpco-relative-name)
                            name))
      (lambda (result)
        (if result
            (message "%s"
                     (fpco-html->string (cdr (assoc 'hrTitle result))))
          (message "Unknown identifier!")))))))

(defun fpco/hoogle-doc (search)
  "Hoogle search the identifier at REGION and print the doc."
  (interactive "P")
  (with-fpco
   (let* ((region (fpco-ident-region))
	  (name (buffer-substring-no-properties (car region) (cdr region))))
     (cond
      (search
       (fpco/hoogle-mode t))
      (t
       (fpco-run-command
	(list :tag "MsgHoogleIdent"
	      :contents (list fpco-pid
			      (fpco-relative-name)
			      name))
	(lambda (result)
	  (if result
	      (fpco-make-hoogle-buffer result)
	    (message "Unknown identifier!")))))))))

(defun fpco/completions (region)
  "For debugging purposes, shows the list of completions for the
point/region."
  (interactive (list (fpco-ident-region)))
  (with-fpco
   (let ((string (buffer-substring-no-properties (car region) (cdr region))))
     (fpco-run-command
      (list :tag "MsgAutoComplete"
            :contents (list fpco-pid
                            (fpco-relative-name)
                            string))
      (lambda (results)
        (message "Completions: %S" results))))))


;; Internal mode functions

(defun fpco-make-type-buffer (infos)
  "Make a buffer to display type information."
  (let ((buffer "*Haskell-Type*"))
    (with-help-window buffer
      (with-current-buffer buffer
        (erase-buffer)
        (font-lock-mode -1)
        (mapc (lambda (info)
                (insert (fpco-fontify-as-mode (elt info 4) 'haskell-mode)
                        " :: "
                        (fpco-fontify-as-mode (elt info 5) 'haskell-mode)
                        "\n\n"))
              infos)))))

(defun fpco-make-hoogle-buffer (info)
  "Hoogle information is presented in a pop-upable buffer."
  (let ((buffer "*Hoogle-Haddock*"))
    (with-help-window buffer
      (with-current-buffer buffer
        (erase-buffer)
        (font-lock-mode -1)
        (let ((point (point)))
          (insert (fpco-fontify-as-mode
                   (format "%s\n\n" (fpco-html->string (cdr (assoc 'hrTitle info))))
                   'haskell-mode)))
        (fpco-insert-haddock-sources (cdr (assoc 'hrSources info)) "\n")
        (insert "\n\n")
        (fpco-insert-haddock-url (cdr (assoc 'hrURL info)))
        (insert "\n")
        (let ((point (point)))
          (insert
           (cdr (assoc 'hrBody info)))
          (goto-char point)
          (fpco-format-docs))))))

(defun fpco-insert-haddock-url (url)
  (insert url "\n"))

(defun fpco-insert-haddock-sources (sources separator)
  "Insert the SOURCES of a haddock entry, separated by SEPARATOR."
  (let ((i 1))
    (mapc (lambda (source)
	    (let ((pl (elt source 0))
		  (modules (elt source 1)))
	      (insert (propertize (cdr (assoc 'plName pl))
				  'face 'font-lock-comment-face))
	      (unless (null modules)
		(insert ":")
		(insert (propertize (mapconcat 'identity
					       (mapcar (lambda (module)
							 (cdr (assoc 'mlName module)))
						       modules)
					       "/")
				    'face 'font-lock-comment-face)))
	      (unless (= i (length sources))
		(insert separator))
	      (setq i (1+ i))))
	  sources)))

(defun fpco-format-docs ()
  "Format the documentation in the buffer."
  (let ((tick 0))
    (while (and (< tick 50)
                (not (eobp)))
      (setq tick (1+ tick)) ;; Cautious
      (cond
       ((save-excursion (forward-line 1)
                        (looking-at "^> "))
        (save-excursion
          (let* ((start (progn (forward-line 1)
                               (point)))
                 (end (progn (forward-paragraph)
                             (point)))
                 (string (buffer-substring-no-properties start end)))
            (delete-region start end)
            (insert (fpco-fontify-as-mode string
                                          'literate-haskell-mode))))
	(forward-paragraph))
       ((save-excursion (forward-line 1)
                        (looking-at "^\\* "))
        (forward-line)
        (goto-char (line-end-position))
        (fill-region (line-beginning-position)
                     (line-end-position)))
       (t (fill-paragraph) (forward-paragraph))))))

(defun fpco-fontify-as-mode (text mode)
  "Fontify TEXT as MODE, returning the fontified text."
  (with-temp-buffer
    (funcall mode)
    (insert text)
    (font-lock-fontify-buffer)
    (buffer-substring (point-min) (point-max))))

(defmacro with-fpco (&rest body)
  "Ensure that fpco-pid and fpco-root are in scope before running
the given code."
  `(when (and (boundp 'fpco-pid)
              (boundp 'fpco-root))
     ,@body))

(defmacro with-fpco-pid (&rest body)
  "Ensure that fpco-pid is in scope before running."
  `(when (boundp 'fpco-pid)
     ,@body))

(defun fpco-relative-name ()
  "Get a relative name of the buffer, relative to the project root."
  (with-fpco
   (file-relative-name (buffer-file-name)
                       fpco-root)))

(defun fpco-with-region (region cont)
  "With REGION unpack its components and pass them to CONT."
  (when region
    (let* ((start (car region))
           (end (cdr region)))
      (funcall cont
               (save-excursion (goto-char start)
                               (line-number-at-pos))
               (save-excursion (goto-char start)
                               ;; Columns in the fpco API aren't 0-based.
                               (1+ (current-column)))
               (save-excursion (goto-char end)
                               (line-number-at-pos))
               (save-excursion (goto-char end)
                               (1+ (current-column)))))))

(defun fpco-ident-region ()
  "Get the region if active or the current identifer at point's region."
  (if (region-active-p)
      (cons (region-beginning)
            (region-end))
    (haskell-ident-pos-at-point)))

(defun fpco-html->string (html)
  "Strip out HTML and unconvert trivial attributes."
  (replace-regexp-in-string
   "&amp;"
   "&"
   (replace-regexp-in-string
    "&gt;"
    ">"
    (replace-regexp-in-string
     "&lt;"
     "<"
     (replace-regexp-in-string
      "<[^>]+>"
      ""
      html)))))

(defun fpco-goto-loc (loc)
  "Go to the given location (file, line, col)."
  (find-file (elt loc 0))
  (goto-char (point-min))
  (forward-line (1- (elt loc 1)))
  (goto-char (+ (point)
                (1- (elt loc 2)))))

(defun fpco-launch-string ()
  "The string used to launch the server executable."
  (format "%s start --agent Emacs\n"
          fpco-program-name))


;; Client Communication

(defun fpco-run-command (data handler)
  "Send the given command to the server."
  (let ((process (make-network-process :name "*fpco-client*"
                                       :host "localhost"
                                       :service fpco-port
                                       :nowait nil
                                       :sentinel 'fpco-sentinel
                                       :filter 'fpco-filter)))
    (process-put process :fpco-handler handler)
    (process-put process :fpco-buffer nil)
    (fpco-log "(%S)" data)
    (fpco-log "=> %s" (json-encode-plist data))
    (process-send-string process
                         (concat (json-encode-plist data) "\n"))))

(defun fpco-sentinel (p status)
  "Once the connection's closed, run the handler and clear the handler & buffer."
  (let ((handler (process-get p :fpco-handler))
        (buffer (apply 'concat
                       (nreverse (process-get p :fpco-buffer)))))
    (fpco-log "<= %s" buffer)
    (funcall handler
             (if (string= buffer "")
                 nil
               ;; Drop the newline.
               (let ((result (cdr (assoc 'contents
                                         (json-read-from-string
                                          (substring buffer 0 (1- (length buffer))))))))
                 (if (vectorp result)
                     (mapcar #'identity result)
                   result))))))

(defun fpco-filter (p data)
  "Just append all the data to the process's buffer."
  (process-put p
               :fpco-buffer
               (cons data
                     (process-get p :fpco-buffer))))

(defun fpco-log (&rest args)
  "Log LINE to the log buffer."
  (with-current-buffer (get-buffer-create "*fpco-log*")
    (goto-char (point-max))
    (insert (apply 'format args) "\n")))


;; Hoogle buffer

(defvar fpco-hoogle-buffer-name "*Hoogle*")
(defvar fpco-hoogle-timer nil)
(defvar fpco-hoogle-idle-timeout 0.5)
(defvar fpco-hoogle-last-q "")
(defvar fpco-hoogle-input-length 80)

(defface fpco-hoogle-loading-face
  '((((class color)) :foreground "#999"))
  "Face for quarantines."
  :group 'shm)

(define-derived-mode fpco-hoogle-mode fundamental-mode
  "FP-Hoogle" "Major mode for searching Hoogle results using fpco-api."
  (font-lock-mode -1)
  (erase-buffer)
  (remove-overlays (point-min) (point-max))
  (widget-create 'editable-field
		 :size fpco-hoogle-input-length
		 :format "%v"
		 "")
  (widget-insert "\n\nNo results yet!")
  (setq fpco-hoogle-last-q "")
  (use-local-map widget-keymap)
  (widget-setup)
  (goto-char (point-min))
  (fpco-hoogle-start-timer))

(defun fpco-hoogle-start-timer ()
  (unless fpco-hoogle-timer
    (setq fpco-hoogle-timer
          (run-with-idle-timer fpco-hoogle-idle-timeout t 'fpco-hoogle-refresh))))

(defun fpco-hoogle-refresh ()
  "Refresh the Hoogle results."
  (interactive)
  (when (eq major-mode 'fpco-hoogle-mode)
    (with-fpco-pid
     (let ((q (fpco-chomp
               (buffer-substring-no-properties (point-min) (+ fpco-hoogle-input-length (point-min))))))
       (when (and (not (string= q fpco-hoogle-last-q))
                  (not (string= q "")))
         (setq fpco-hoogle-last-q q)
         (fpco-hoogle-start-query q))))))

(defun fpco-chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                       str)
    (setq str (replace-match "" t t str)))
  str)

(defun fpco-hoogle-start-query (q)
  "Start the query. This should not be called multiple times at once."
  (save-excursion
    (goto-char (point-min))
    (forward-line 2)
    (let ((inhibit-read-only t))
      (put-text-property (point)
                         (point-max)
                         'face
                         'fpco-hoogle-loading-face)))
  (fpco-run-command
   (list :tag "MsgHoogleDb"
         :contents (list fpco-pid q))
   'fpco-hoogle-handle-results))

(defun fpco-hoogle-handle-results (results)
  "Insert the results into the buffer."
  (with-current-buffer fpco-hoogle-buffer-name
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        (forward-line 2)
        (delete-region (point)
                       (point-max))
        (cond
         ((null results)
          (insert "No results for that search!"))
         (t
          (mapc (lambda (result)
                  (insert (fpco-fontify-as-mode
                           (fpco-html->string (cdr (assoc 'hrTitle result)))
                           'haskell-mode)
                          "\n")
                  (fpco-insert-haddock-sources (cdr (assoc 'hrSources result))
					       ", ")
		  (insert "\n\n"))
                results)))))))


;; Auto complete integration

(ac-define-source fpco
                  '((candidates . fpco-ac-candidates)))

(defvar fpco-candidates-cache nil
  "A cache used for the auto-complete candidates.")
(defvar fpco-last-completion ""
  "Remember the last completion, to avoid doing subsearches.")

(defun fpco-ac-candidates ()
  "Generate a list of candidates for the identifier at point."
  (with-fpco
   (let ((candidates-returned nil)
         (completion (haskell-ident-at-point)))
     (if (string= "" completion)
	 '()
       (cond
	;; If the search is an extension of the previous search (fo ->
	;; foo), and if the results of the last search fit within the
	;; auto-complete menu height, then don't bother doing another
	;; search, just re-return those candidates, auto-complete can
	;; handle filtering after that.
	((and (not (string= fpco-last-completion ""))
	      (fpco-is-prefix-of fpco-last-completion completion)
	      (not (null fpco-candidates-cache))
	      (< (length fpco-candidates-cache) ac-menu-height))
	 fpco-candidates-cache)
	;; Otherwise make an async request to the server, waiting for a
	;; reply, but not for long.
	(t
	 (setq fpco-candidates-cache nil)
	 (fpco-run-command
	  (list :tag "MsgAutoComplete"
		:contents (list fpco-pid
				(fpco-relative-name)
				completion))
	  (lambda (results)
	    (setq candidates-returned t)
	    (setq fpco-candidates-cache results)))
	 (let ((attempts 0) (max-attempts 2))
	   ;; There tends to be some latency on this command so we wait a
	   ;; little bit, but if it takes longer than a second that's
	   ;; just too slow.
	   (sit-for 0.2)
	   (while (and (not candidates-returned) (< attempts max-attempts))
	     (setq attempts (1+ attempts))
	     (sit-for 0.5)))
	 (setq fpco-last-completion completion)
	 fpco-candidates-cache))))))

(defun fpco-is-prefix-of (x y)
  "Is x string a prefix of y string?"
  (string= x (substring y 0 (min (length y) (length x)))))


;; Flycheck integration

(flycheck-define-checker fpco-check
                         "Compiles with GHC and checks with HLint, returns errors, warnings, hints."
                         :command ("fpco-api"
                                   "check"
                                   (eval (number-to-string fpco-pid))
                                   (eval fpco-root)
                                   (eval (fpco-relative-name))
                                   source)
                         :error-patterns
                         ((warning line-start (file-name) ":" line ":" column ":"
                                   (or " " "\n    ") (or "Warning:" "Suggestion:") (optional "\n")
                                   (one-or-more " ")
                                   (message (one-or-more not-newline)
                                            (zero-or-more "\n"
                                                          (one-or-more " ")
                                                          (one-or-more not-newline)))
                                   line-end)
                          (error line-start (file-name) ":" line ":" column ":"
                                 (or (message (one-or-more not-newline))
                                     (and "\n" (one-or-more " ")
                                          (message (one-or-more not-newline)
                                                   (zero-or-more "\n"
                                                                 (one-or-more " ")
                                                                 (one-or-more not-newline)))))
                                 line-end))
                         :modes haskell-mode)


;; Provide

(provide 'fpco-mode)

;;; fpco-mode.el ends here
;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:
