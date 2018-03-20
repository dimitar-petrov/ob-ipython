
(require 'js2-mode)

(defvar ob-ipython-client/host nil)
;; TODO will make it into a list
(defvar ob-ipython-client/latest-proc)
;; TODO will make it into a list
(defvar ob-ipython-client/latest-proc-buf)
(defvar ob-ipython-client/current-output)
(defvar ob-ipython-client-program)

(setq ob-ipython-client-program "~/work/python/jcc-ai/scripts/client.py")

(defun ob-ipython-client/input-sender (proc string)
  "Send STRING to PROC"
  (when (string-match "[[:alnum:]]" string)
    (comint-send-string 
     proc
     (format 
      "\
PS2=
python ~/work/python/jcc-ai/scripts/client.py  --conn-file `get_session_file` --execute  <<__IPYTHON_CLIENT_EOF__
%s
__IPYTHON_CLIENT_EOF__
"
     string))))

(defun ob-ipython-client/output-filter-test (&optional string)
  (let ((start (marker-position comint-last-input-end))
        (end   (and comint-last-prompt (cdr comint-last-prompt))))
    
    (message "ob-ipython-client/output-filter-test (%s . %s)" start end)

    (when (and start end (< start end))
      (let ((new-output-chunk (buffer-substring-no-properties start end)))
        (setq ob-ipython-client/current-output new-output-chunk)
        (with-current-buffer (get-buffer-create "*my-debug*")
          (insert new-output-chunk))
        ))))

(defun ob-ipython-client/test (string)
  (interactive "sEnter Input String: ")
  (ob-ipython-client "instance-2")
  (with-current-buffer ob-ipython-client/latest-proc-buf
    (let ((proc (get-buffer-process (current-buffer))))
      (goto-char (point-max))
      (set-marker (process-mark proc) (point))
      (insert string)
      (comint-send-input))))

(define-derived-mode
  ob-ipython-client/mode comint-mode "IPython-client"
  "Connect to a IPython kernel via ssh. 
Communication is based on JSON.
"
  :syntax-table js2-mode-syntax-table
  (setq comint-use-prompt-regexp t)
  (setq comint-prompt-regexp "^lm@instance.*\\$ *")
  (setq comint-input-sender 'ob-ipython-client/input-sender)
  ;;(add-hook 'comint-output-filter-functions 'ob-ipython-client/output-filter-test nil t)

  (unless (comint-check-proc (current-buffer))
    (let ((proc (get-buffer-process (current-buffer))))
      (set-process-query-on-exit-flag proc nil)
      (insert "IPython Client Mode\n")
      (message (current-buffer)))))

(setq ob-ipython-client/startfile
      (concat (or (-when-let (f load-file-name) (f-dirname f)) default-directory)
              "startfile.sh"))

(defun ob-ipython-client (host)
  (interactive "sEnter a host name to ssh to: ")
  (let* ((startfile ob-ipython-client/startfile)
         (buf (make-comint
               (format "ipython-client:%s" host)
               "ssh"
               startfile
               host)))
    (setq  ob-ipython-client/latest-proc-buf buf)
    (setq  ob-ipython-client/latest-proc (get-buffer-process buf))
    
    (with-current-buffer buf
      (make-variable-buffer-local 'ob-ipython-client/host)
      (setq ob-ipython-client/host host)
      (ob-ipython-client/mode))
    (switch-to-buffer buf)))


(defun ob-ipython-client/search-forward-json ()
  (when (search-forward "{" nil t 1)
    (backward-char 1)
    (point)))

(defun ob-ipython-client/search-backward-json ()
  (when (search-backward "}" nil t 1)
    (forward-char 1)
    (point)))

(defun ob-ipython-client/prompt-p ()
  (save-excursion
    (let ((limit (- (point-max) 1024)))
      (goto-char (point-max))
      (looking-back comint-prompt-regexp limit))))

(defun ob-ipython-client/point-before-complete-json ()
  (let ((count (if (ob-ipython-client/prompt-p) 2 1)))
    (save-excursion
      (goto-char (point-max))
      (re-search-backward comint-prompt-regexp nil t count)
      (when (ob-ipython-client/search-forward-json)
        (point)))))
    
(defun ob-ipython-client/point-after-complete-json ()
  (let ((count (if (ob-ipython-client/prompt-p) 1 0)))
    (save-excursion
      (goto-char (point-max))
      (when (> count 0)
        (re-search-backward comint-prompt-regexp nil t count)
        (when (ob-ipython-client/search-backward-json)
          (point))))))

(defun ob-ipython-client/maybe-json-string-at-point ()
  (let* ((start (ob-ipython-client/point-before-complete-json))
         (end   (ob-ipython-client/point-after-complete-json)))
    (when (and start end (< start end))
      (buffer-substring-no-properties start end))))

(defun ob-ipython-client/before-prompt-p (point)
  (save-excursion
    (goto-char point)
    (re-search-forward "[^[:space:]]")
    (looking-at comint-prompt-regexp)))

(defun ob-ipython-client/after-prompt-p (point)
  (save-excursion
    (goto-char point)
    (let ((limit (- point 100)))
      (looking-back comint-prompt-regexp limit))))

(defun ob-ipython-client/filter (string code name callback args)
  (let* ((start (ob-ipython-client/point-before-complete-json))
         (end   (ob-ipython-client/point-after-complete-json))
         (s     (and start end (buffer-substring-no-properties start end)))
         (pipe-broken "Broken pipe$")
         (keyword   "stdout\\|stderr\\|traceback"))

    (assert (equal (buffer-name) "*ipython-client:instance-2*"))

    (when (and
           start
           end
           (< start end)
           (stringp s)
           (ob-ipython-client/after-prompt-p  start)
           (ob-ipython-client/before-prompt-p end))

      (assert (not (string-match pipe-broken s)))
      (assert (string-match keyword s))

      (ob-ipython-client/collect-json start end callback args)
      ;;(condition-case nil
      ;;(error (error "json parse error:%s" s)))

      (message "ob-ipython-client/filter: finished execution"))))
        
(defun ob-ipython-client/collect-json (start end callback args)
  (save-excursion
    (assert end)
    (assert (< start end))

    (goto-char start)
    (search-forward "{")
    (backward-char 1)
    (assert (looking-at "{"))
    (setq start (point))
              
    (narrow-to-region start end)
    (apply callback (-> (ob-ipython--collect-json)
                        ob-ipython--eval
                        (cons args)))
    (widen)
    (ob-ipython--maybe-run-async)))

(defun ob-ipython--run-async-with-client (code name callback args)
  (lexical-let*
      ((code code)
       (name name)
       (callback callback)
       (args args)
       (proc     ob-ipython-client/latest-proc)
       (buf      ob-ipython-client/latest-proc-buf)
       (filter  (lambda (string)
                  (ob-ipython-client/filter string code name callback args))))
        
    (with-current-buffer buf
      ;; Remove all the filter functions in this buffer
      (make-variable-buffer-local 'comint-output-filter-functions)
      (setq comint-output-filter-functions nil)
      (add-hook 'comint-output-filter-functions filter nil t)
      (goto-char (point-max))
      (insert code "\n")
      (comint-send-input))))


(provide 'ob-ipython-client)

(add-to-list 'load-path "~/work/ob-ipython")

