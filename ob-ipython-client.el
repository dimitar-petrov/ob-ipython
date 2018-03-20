
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

(defun ob-ipython-client/previous-prompt ()
  (save-excursion
    (goto-char (point-max))
    (comint-previous-prompt 1)
    (when (not (ob-ipython-client/at-prompt (point)))
      (comint-previous-prompt 1))
    (point)))

(defun ob-ipython-client/maybe-last-prompt ()
  (save-excursion
    (goto-char (point-max))
    (comint-previous-prompt -1)
    (beginning-of-line)
    (if (ob-ipython-client/at-prompt (point))
        (point)
      nil)))

(defun ob-ipython-client/at-prompt (point)
  (save-excursion
    (goto-char point)
    (looking-at "[ \n]*lm@instance-2:~$ ")))

(defun ob-ipython-client/filter (string code name callback args)
  (let* ((start (ob-ipython-client/previous-prompt))
         (end   (ob-ipython-client/maybe-last-prompt))
         (s     (and start end (buffer-substring-no-properties start end)))
         (pipe-broken "Broken pipe$")
         (keyword   "stdout\\|stderr\\|traceback"))

    (assert (equal (buffer-name) "*ipython-client:instance-2*"))
    (assert start)

    (when (and
           start
           end
           (< start end)
           (stringp s)
           (ob-ipython-client/at-prompt start)
           (ob-ipython-client/at-prompt end))

      (assert (not (string-match pipe-broken s)))
      
      (if (string-match keyword s)
          (ob-ipython-client/collect-json start end callback args)
        (message "ob-ipython-client/filter: No output")))))
        

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

