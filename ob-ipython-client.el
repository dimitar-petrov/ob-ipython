
(require 'js2-mode)

;; TODO will make these into  lists
(defvar ob-ipython-client/latest-host nil)
(defvar ob-ipython-client/latest-proc)
(defvar ob-ipython-client/latest-proc-buf)
(defvar ob-ipython-client/current-output)
(defvar ob-ipython-client-program)


;; Broken approach, fix it by including it in the closures
(defvar ob-ipython-client/default-directory)

(setq ob-ipython-client/startfile
      (concat (or (-when-let (f load-file-name) (f-dirname f)) default-directory)
              "startfile.sh"))

(setq ob-ipython-resources-dir "/tmp/obipy-resources/")

(setq ob-ipython-client-program "~/work/python/jcc-ai/scripts/client.py")

(setq ob-ipython-client/send-format   "\
# Suppress continuation prompt
PS2= 
# The json output file for downloading images
OUTFILE='%s'
# Work-around for strange piping bug
read -r -d '' VAR <<EOF
%s
EOF
# Feeding inputs directly via heredoc won't work for some reason
echo \"$VAR\" | python ~/scripts/client.py  --conn-file `get_session_file` --execute | filter \"$OUTFILE\"
")

(defun ob-ipython-client/input-sender (proc string &optional sentinel)
  "Send STRING to PROC"
  (let* ((proc
          (if (process-live-p proc)
              proc
            (ob-ipython-client ob-ipython/latest-host)))
         (remote-out-file
          (if sentinel
              (format "/tmp/obipy-%s.jsonlines" sentinel)
            "/tmp/out.jsonlines")))
    (setq ob-ipython-client/latest-proc proc)

    (when (string-match "[[:alnum:]]" string)
      (comint-send-string 
       proc
       (format ob-ipython-client/send-format remote-out-file string)))))

(defun ob-ipython-client/input-test (string)
  (interactive "sEnter string: ")
  (ob-ipython-client/input-sender ob-ipython-client/latest-proc string))

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
  ;; Disable font-lock for performance
  (font-lock-mode -1)
  (setq comint-use-prompt-regexp t)
  (setq comint-prompt-regexp "^.*\\$ *")
  (setq comint-input-sender 'ob-ipython-client/input-sender)
  ;;(add-hook 'comint-output-filter-functions 'ob-ipython-client/output-filter-test nil t)

  (unless (comint-check-proc (current-buffer))
    (let ((proc (get-buffer-process (current-buffer))))
      (set-process-query-on-exit-flag proc nil)
      (insert "IPython Client Mode\n")
      (message (current-buffer)))))


(defun ob-ipython-client/kill ()
  "TODO: kill all the relevant communication buffers and processes"
  (interactive)
  (kill-buffer ob-ipython-client/latest-proc-buf))

(defun ob-ipython-client/reset (&optional host)
  (interactive)
  (ob-ipython-client/kill)
  (ob-ipython-client (or host ob-ipython-client/latest-host)))


(defun ob-ipython-client (host)
  "The interactive command to create the ssh communication channel to the ipython kernel"
  (interactive "sEnter a host name to ssh to: ")
  (let* ((startfile ob-ipython-client/startfile)
         (buf (make-comint
               (format "ipython-client:%s" host)
               "ssh"
               startfile
               host)))
    (setq  ob-ipython-client/latest-host     host)
    (setq  ob-ipython-client/latest-proc-buf buf)
    (setq  ob-ipython-client/latest-proc     (get-buffer-process buf))
    
    (with-current-buffer buf
      (make-variable-buffer-local 'ob-ipython-client/host)
      (make-variable-buffer-local 'ob-ipython-client/default-directory)
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
  (if (and (not (s-blank? string))
           (ob-ipython-client/prompt-p))
      (ob-ipython-client/final-filter string code name callback args)
    (ob-ipython-client/intermediate-filter string code name callback args)))

(defun ob-ipython-client/final-filter (string code name callback args)
  (let* ((start (ob-ipython-client/point-before-complete-json))
         (end   (ob-ipython-client/point-after-complete-json))
         (s     (and start end (buffer-substring-no-properties start end)))
         (pipe-broken "Broken pipe$")
         (keyword   "stdout\\|stderr\\|traceback"))

    (message "ob-ipython-client/final-filter: finising execution")
    
    (assert (equal (buffer-name) "*ipython-client:instance-2*"))

    (when (and
           start
           end
           (< start end)
           (stringp s)
           (ob-ipython-client/after-prompt-p  start)
           (ob-ipython-client/before-prompt-p end))

      (assert (string-match keyword s))
      (assert (not (string-match pipe-broken s)))

      (if (string-match "image/png" s) 
          (ob-ipython-client/collect-json-file callback args)
        (ob-ipython-client/collect-json-region start end callback args))
      
      (message "ob-ipython-client/final-filter: finished execution"))))

(defun ob-ipython-client/intermediate-filter (string code name callback args)
  (message "ob-ipython-client/intermediate-filter: %s" string))

(defun ob-ipython-client/collect-json-file (callback args)
  (let* ((sentinel (car args))
         (file (format "/ssh:instance-2:/tmp/obipy-%s.jsonlines" sentinel)))
    (with-current-buffer (find-file-noselect file)
      (ob-ipython-client/collect-json (point-min) (point-max) callback args))))

(defun ob-ipython-client/collect-json-region (start end callback args)
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

(defun ob-ipython-client/run-async (code name callback args)
  (message "current-directory is: %s" default-directory)
  (make-variable-buffer-local 'ob-ipython-client/default-directory)
  (setq ob-ipython-client/default-directory default-directory)

  (when (not (process-live-p ob-ipython-client/latest-proc))
    (ob-ipython-client ob-ipython-client/latest-host))
  
  (lexical-let*
      ((code code)
       (name name)
       (callback callback)
       (args args)
       (proc     ob-ipython-client/latest-proc)
       (buf      ob-ipython-client/latest-proc-buf)
       (sentinel (car args))
       (filter  (lambda (string)
                  (ob-ipython-client/filter string code name callback args))))
        
    (with-current-buffer buf
      ;; Remove all the filter functions in this buffer
      (make-variable-buffer-local 'comint-output-filter-functions)
      (setq comint-output-filter-functions nil)
      (add-hook 'comint-output-filter-functions filter nil t)
      (goto-char (point-max))
      (ob-ipython-client/send-input code name sentinel))))

(defun ob-ipython-client/send-input (code name sentinel)
  (lexical-let ((sentinel sentinel))
    (let ((comint-input-sender
           (lambda (proc string)
             (ob-ipython-client/input-sender proc string sentinel))))
      (insert code "\n")
      (comint-send-input))))

(setq ob-ipython-client/default-directory "~/work/python/jcc-ai/jcc/ai")

(defun ob-ipython-client/generate-file-name (suffix)
  (let ((default-directory ob-ipython-client/default-directory))
    (s-concat (make-temp-name ob-ipython-resources-dir) suffix)))

;; I need a better data structure, maybe keyed by sentinels
;; Maybe two, one for tracking json streams,
;; one for tracking asynchronous state of the buffer

(defun ob-ipython-client/enqueue (q x)
  (message "ob-ipython-client/enqueue (len=%d, %s)"
           (length (symbol-value q)) x)
  (set q (append (symbol-value q) (list x))))

(defun ob-ipython-client/dequeue (q)
  (message "ob-ipython-client/dequeue (len=%d)"
           (length (symbol-value q)))
  (let ((ret (car (symbol-value q))))
    (set q (cdr (symbol-value q)))
    ret))

(defun ob-ipython-client/queue-length (q)
  (length (symbol-value q)))

(defun ob-ipython-client/queue-reset (q)
  (interactive)
  (message "ob-ipython-client/queue-reset (len=%s)"
           (ob-ipython-client/queue-length q))
  (set q nil))


;; Overrides
;; TODO use saner mechanism (but not defadvice)

(defun ob-ipython--run-async (code name callback args)
  (ob-ipython-client/run-async code name callback args))

(defun ob-ipython--generate-file-name (suffix)
  (ob-ipython-client/generate-file-name suffix))

(defun ob-ipython--enqueue (q x)
  (ob-ipython-client/enqueue q x))

(defun ob-ipython--dequeue (q)
  (ob-ipython-client/dequeue q))


(add-to-list 'load-path "~/work/ob-ipython")


(provide 'ob-ipython-client)



